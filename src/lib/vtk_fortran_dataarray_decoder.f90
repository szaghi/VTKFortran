!< VTK_Fortran dataarray decoder: the inverse of the encoder, from ASCII and Base64 (optionally zlib compressed) data.
module vtk_fortran_dataarray_decoder
!< VTK_Fortran dataarray decoder: the inverse of the encoder, from ASCII and Base64 (optionally zlib compressed) data.
!<
!< A DataArray is decoded into its bytes, in the native byte order (the reader refuses BigEndian files, so the bytes of the
!< little endian files are native on the supported platforms), then converted into an array of the kind requested by
!< [[bytes_to_array]]. The errors are 0 on success, 5 for a kind that cannot hold the type, 6 for data that do not decode.
use befor64
use penf
use vtk_fortran_zlib, only : zlib_uncompress_blocks

implicit none
private
public :: base64_decoded_size
public :: bytes_to_array
public :: bytes_to_strings
public :: data_type_size
public :: decode_ascii
public :: decode_base64
public :: header_words
public :: strip_spaces

interface bytes_to_array
  !< Convert the bytes of a dataarray of a VTK type into an array.
  !<
  !< The array must have the kind of the type, or a kind that holds all its values: Int8 into I1P...I8P, Int16 into
  !< I2P...I8P, Int32 into I4P or I8P, Int64 into I8P, Float32 into R4P or R8P, Float64 into R8P. The unsigned types go
  !< into the signed kind of the same width, with the same bits (as `write_dataarray_unsigned` writes
  !< them), or into any wider kind, with their values. Other kinds return the error 5.
  module procedure bytes_to_array_R8P, bytes_to_array_R4P, bytes_to_array_I8P, bytes_to_array_I4P, bytes_to_array_I2P, &
                   bytes_to_array_I1P
endinterface bytes_to_array

contains
  elemental function data_type_size(data_type) result(bytes)
  !< Return the size in bytes of a value of a VTK type (0 for an unknown type).
  !<
  !< The type `String` (field data strings) counts the bytes of the strings.
  character(*), intent(in) :: data_type !< VTK type.
  integer(I8P)             :: bytes     !< Size in bytes.

  select case(data_type)
  case('Int8', 'UInt8', 'String')
    bytes = 1_I8P
  case('Int16', 'UInt16')
    bytes = 2_I8P
  case('Int32', 'UInt32', 'Float32')
    bytes = 4_I8P
  case('Int64', 'UInt64', 'Float64')
    bytes = 8_I8P
  case default
    bytes = 0_I8P
  endselect
  endfunction data_type_size

  pure subroutine strip_spaces(string)
  !< Remove the white space (blank, tab, line feed, carriage return) of a string, in place.
  character(len=:), allocatable, intent(inout) :: string !< String.
  integer(I8P)                                 :: i      !< Counter.
  integer(I8P)                                 :: n      !< Length of the stripped string.
  character(len=:), allocatable                :: strip  !< Stripped string.

  n = 0_I8P
  do i=1_I8P, len(string, kind=I8P)
    select case(string(i:i))
    case(' ', achar(9), achar(10), achar(13))
    case default
      n = n + 1_I8P
      string(n:n) = string(i:i)
    endselect
  enddo
  ! copied into a new string: the self assignment string = string(1:n) can be made through a stack temporary (e.g. ifx)
  allocate(character(len=n) :: strip)
  strip(1:n) = string(1:n)
  call move_alloc(from=strip, to=string)
  endsubroutine strip_spaces

  pure function base64_decoded_size(code) result(n)
  !< Return the number of bytes encoded by a base64 code (without white space), 0 if its length is not a multiple of 4.
  character(*), intent(in) :: code !< Base64 code.
  integer(I8P)             :: n    !< Number of bytes.
  integer(I8P)             :: l    !< Length of the code.

  n = 0_I8P
  l = len(code, kind=I8P)
  if (l == 0_I8P .or. mod(l, 4_I8P) /= 0_I8P) return
  n = l / 4_I8P * 3_I8P
  if (code(l:l) == '=') n = n - 1_I8P
  if (code(l-1_I8P:l-1_I8P) == '=') n = n - 1_I8P
  endfunction base64_decoded_size

  pure function header_words(bytes, word_size) result(words)
  !< Return the words of a VTK binary header (UInt32 or UInt64, given by their size, 4 or 8 bytes) from its bytes.
  integer(I1P), intent(in)  :: bytes(1:)  !< Bytes of the header (a multiple of the word size).
  integer(I8P), intent(in)  :: word_size  !< Size of a word: 4 (UInt32) or 8 (UInt64).
  integer(I8P), allocatable :: words(:)   !< Words.
  integer(I8P)              :: i          !< Counter.
  integer(I8P)              :: k          !< First byte of the current word.

  allocate(words(1:size(bytes, kind=I8P)/word_size))
  do i=1_I8P, size(words, kind=I8P)
    k = (i - 1_I8P) * word_size + 1_I8P
    if (word_size == 8_I8P) then
      words(i) = transfer(bytes(k:k+7_I8P), 0_I8P)
    else
      words(i) = iand(int(transfer(bytes(k:k+3_I8P), 0_I4P), I8P), 4294967295_I8P)
    endif
  enddo
  endfunction header_words

  subroutine decode_base64(code, word_size, is_compressed, bytes, error)
  !< Decode the base64 code of a binary DataArray (inline or appended), header included, into its data bytes.
  !<
  !< Uncompressed, the code is one base64 stream: the bytes count (a header word) followed by the data. Compressed, it is
  !< two streams: the VTK header of the compressed data (number of blocks, block size, last block size, compressed size of
  !< each block), then the compressed blocks. The code must be without white space.
  character(*),              intent(in)  :: code          !< Base64 code, without white space.
  integer(I8P),              intent(in)  :: word_size     !< Size of a header word: 4 (UInt32) or 8 (UInt64).
  logical,                   intent(in)  :: is_compressed !< The data are zlib compressed.
  integer(I1P), allocatable, intent(out) :: bytes(:)      !< Data bytes.
  integer(I4P),              intent(out) :: error         !< Error status: 0, or 6 if the code does not decode.
  integer(I1P), allocatable              :: raw(:)        !< Decoded bytes.
  integer(I8P), allocatable              :: header(:)     !< Header words.
  integer(I8P)                           :: n             !< Bytes count.
  integer(I8P)                           :: lh            !< Length of the code of the header.
  integer                                :: zerror        !< Decompression error.

  error = 6
  allocate(bytes(1:0))
  if (.not.is_compressed) then
    n = base64_decoded_size(code)
    if (n < word_size) return
    allocate(raw(1:n))
    call b64_decode(code=code, n=raw)
    header = header_words(raw(1:word_size), word_size)
    if (header(1) < 0_I8P .or. header(1) > n - word_size) return
    deallocate(bytes)
    allocate(bytes(1:header(1)))
    bytes = raw(word_size+1_I8P:word_size+header(1))
  else
    ! the number of blocks, first word of the header, gives the length of the header stream
    lh = ((word_size + 2_I8P) / 3_I8P) * 4_I8P
    if (len(code, kind=I8P) < lh) return
    allocate(raw(1:(lh/4_I8P)*3_I8P))
    call b64_decode(code=code(1:lh), n=raw)
    header = header_words(raw(1:word_size), word_size)
    if (header(1) < 0_I8P) return
    lh = (((3_I8P + header(1)) * word_size + 2_I8P) / 3_I8P) * 4_I8P
    if (len(code, kind=I8P) < lh) return
    n = base64_decoded_size(code(1:lh))
    if (n /= (3_I8P + header(1)) * word_size) return
    deallocate(raw)
    allocate(raw(1:n))
    call b64_decode(code=code(1:lh), n=raw)
    header = header_words(raw, word_size)
    n = base64_decoded_size(code(lh+1_I8P:))
    if (n == 0_I8P .and. len(code, kind=I8P) > lh) return
    deallocate(raw)
    allocate(raw(1:n))
    if (n > 0_I8P) call b64_decode(code=code(lh+1_I8P:), n=raw)
    call zlib_uncompress_blocks(header=header, blocks=raw, bytes=bytes, error=zerror)
    if (zerror /= 0) return
  endif
  error = 0
  endsubroutine decode_base64

  subroutine decode_ascii(text, data_type, bytes, error)
  !< Decode the text of an ASCII DataArray into its bytes (native byte order).
  !<
  !< Values are separated by white space; values written without separation (e.g. `+0.3E+001+0.1E+002`, written by
  !< VTKFortran up to v2.0.10) are split at the sign that follows a digit or a point. The type `String` holds the bytes of
  !< the strings, written as integers.
  character(*),              intent(in)  :: text      !< Text of the DataArray.
  character(*),              intent(in)  :: data_type !< VTK type.
  integer(I1P), allocatable, intent(out) :: bytes(:)  !< Data bytes.
  integer(I4P),              intent(out) :: error     !< Error status: 0, or 6 if the text does not decode.
  integer(I8P)                           :: sz        !< Size of a value.
  integer(I8P)                           :: n         !< Number of values.
  integer(I8P)                           :: i         !< Counter.
  integer(I8P)                           :: s         !< First character of a value.
  integer(I8P)                           :: e         !< Last character of a value.
  integer(I8P)                           :: p         !< Current position.
  integer(I8P)                           :: vi        !< Integer value.
  real(R8P)                              :: vr        !< Real value.
  integer(I1P)                           :: b(8)      !< Bytes of a value.
  integer(I4P)                           :: iostat    !< IO status.

  error = 6
  allocate(bytes(1:0))
  sz = data_type_size(data_type)
  if (sz == 0_I8P) return
  ! count the values, then decode them
  n = 0_I8P
  p = 1_I8P
  do
    call next_value(text=text, p=p, s=s, e=e)
    if (s == 0_I8P) exit
    n = n + 1_I8P
  enddo
  deallocate(bytes)
  allocate(bytes(1:n*sz))
  p = 1_I8P
  do i=1_I8P, n
    call next_value(text=text, p=p, s=s, e=e)
    select case(data_type)
    case('Float32', 'Float64')
      read(text(s:e), *, iostat=iostat) vr
      if (iostat /= 0) return
      if (sz == 4_I8P) then
        b(1:4) = transfer(real(vr, R4P), b(1:4))
      else
        b = transfer(vr, b)
      endif
    case default
      read(text(s:e), *, iostat=iostat) vi
      if (iostat /= 0) return
      b = transfer(vi, b) ! the low bytes come first (little endian)
    endselect
    bytes((i-1_I8P)*sz+1_I8P:i*sz) = b(1:sz)
  enddo
  error = 0
  endsubroutine decode_ascii

  pure subroutine next_value(text, p, s, e)
  !< Return the next value of an ASCII text from a position, and advance the position after it.
  character(*), intent(in)    :: text !< Text.
  integer(I8P), intent(inout) :: p    !< Current position.
  integer(I8P), intent(out)   :: s    !< First character of the value, 0 if there are no more values.
  integer(I8P), intent(out)   :: e    !< Last character of the value.
  integer(I8P)                :: l    !< Length of the text.

  l = len(text, kind=I8P)
  s = 0_I8P
  e = 0_I8P
  do while (p <= l)
    if (.not.is_space(text(p:p))) exit
    p = p + 1_I8P
  enddo
  if (p > l) return
  s = p
  p = p + 1_I8P
  do while (p <= l)
    if (is_space(text(p:p))) exit
    if (text(p:p) == '+' .or. text(p:p) == '-') then
      if (scan(text(p-1_I8P:p-1_I8P), '0123456789.') > 0) exit
    endif
    p = p + 1_I8P
  enddo
  e = p - 1_I8P
  endsubroutine next_value

  elemental function is_space(c) result(is_blank)
  !< Return .true. if a character is white space (blank, tab, line feed, carriage return).
  character(len=1), intent(in) :: c        !< Character.
  logical                      :: is_blank !< Inquire result.

  is_blank = c == ' ' .or. c == achar(9) .or. c == achar(10) .or. c == achar(13)
  endfunction is_space

  subroutine bytes_to_strings(bytes, x, error)
  !< Convert the bytes of a String array (each string terminated by a NUL byte) into an array of strings.
  !<
  !< The strings are blank padded to the length of the longest one; a last string without a NUL byte is kept.
  integer(I1P),                  intent(in)  :: bytes(1:) !< Bytes of the strings.
  character(len=:), allocatable, intent(out) :: x(:)      !< Strings.
  integer(I4P),                  intent(out) :: error     !< Error status: 0 on success.
  integer(I8P)                               :: n         !< Number of strings.
  integer(I8P)                               :: l         !< Length of the current string.
  integer(I8P)                               :: lmax      !< Length of the longest string.
  integer(I8P)                               :: i         !< Counter.
  integer(I8P)                               :: s         !< Current string.

  error = 0
  n = 0_I8P ; l = 0_I8P ; lmax = 0_I8P
  do i=1_I8P, size(bytes, kind=I8P)
    if (bytes(i) == 0_I1P) then
      n = n + 1_I8P
      lmax = max(lmax, l)
      l = 0_I8P
    else
      l = l + 1_I8P
    endif
  enddo
  if (l > 0_I8P) then
    n = n + 1_I8P
    lmax = max(lmax, l)
  endif
  allocate(character(len=lmax) :: x(1:n))
  x(:) = '' ! a section: assigning the whole array would reallocate it with the length of ''
  s = 1_I8P ; l = 0_I8P
  do i=1_I8P, size(bytes, kind=I8P)
    if (bytes(i) == 0_I1P) then
      s = s + 1_I8P
      l = 0_I8P
    else
      l = l + 1_I8P
      x(s)(l:l) = achar(iand(int(bytes(i), I4P), 255_I4P))
    endif
  enddo
  endsubroutine bytes_to_strings

  subroutine bytes_to_array_R8P(bytes, data_type, x, error)
  !< Convert the bytes of a dataarray of a VTK type into an array (R8P): see [[bytes_to_array]] for the conversions allowed.
  integer(I1P),              intent(in)  :: bytes(1:) !< Bytes of the dataarray (native byte order).
  character(*),              intent(in)  :: data_type !< VTK type of the dataarray.
  real(R8P),    allocatable, intent(out) :: x(:)      !< Values.
  integer(I4P),              intent(out) :: error     !< Error status: 0, or 5 if R8P cannot hold the type.
  integer(I8P)                           :: n         !< Number of values.
  integer(I8P)                           :: i         !< Counter.
  integer(I8P)                           :: k         !< First byte of the current value.

  error = 5
  allocate(x(1:0))
  select case(data_type)
  case('Float32', 'Float64')
  case default
    return
  endselect
  n = size(bytes, kind=I8P) / data_type_size(data_type)
  deallocate(x)
  allocate(x(1:n))
  ! element-wise transfer: whole-array transfer results can be placed on the stack (e.g. ifx)
  select case(data_type)
  case('Float32')
    do i=1_I8P, n
      k = (i - 1_I8P) * 4_I8P + 1_I8P
      x(i) = real(transfer(bytes(k:k+3_I8P), 0._R4P), R8P)
    enddo
  case('Float64')
    do i=1_I8P, n
      k = (i - 1_I8P) * 8_I8P + 1_I8P
      x(i) = transfer(bytes(k:k+7_I8P), 0._R8P)
    enddo
  endselect
  error = 0
  endsubroutine bytes_to_array_R8P

  subroutine bytes_to_array_R4P(bytes, data_type, x, error)
  !< Convert the bytes of a dataarray of a VTK type into an array (R4P): see [[bytes_to_array]] for the conversions allowed.
  integer(I1P),              intent(in)  :: bytes(1:) !< Bytes of the dataarray (native byte order).
  character(*),              intent(in)  :: data_type !< VTK type of the dataarray.
  real(R4P),    allocatable, intent(out) :: x(:)      !< Values.
  integer(I4P),              intent(out) :: error     !< Error status: 0, or 5 if R4P cannot hold the type.
  integer(I8P)                           :: n         !< Number of values.
  integer(I8P)                           :: i         !< Counter.
  integer(I8P)                           :: k         !< First byte of the current value.

  error = 5
  allocate(x(1:0))
  select case(data_type)
  case('Float32')
  case default
    return
  endselect
  n = size(bytes, kind=I8P) / data_type_size(data_type)
  deallocate(x)
  allocate(x(1:n))
  ! element-wise transfer: whole-array transfer results can be placed on the stack (e.g. ifx)
  select case(data_type)
  case('Float32')
    do i=1_I8P, n
      k = (i - 1_I8P) * 4_I8P + 1_I8P
      x(i) = transfer(bytes(k:k+3_I8P), 0._R4P)
    enddo
  endselect
  error = 0
  endsubroutine bytes_to_array_R4P

  subroutine bytes_to_array_I8P(bytes, data_type, x, error)
  !< Convert the bytes of a dataarray of a VTK type into an array (I8P): see [[bytes_to_array]] for the conversions allowed.
  integer(I1P),              intent(in)  :: bytes(1:) !< Bytes of the dataarray (native byte order).
  character(*),              intent(in)  :: data_type !< VTK type of the dataarray.
  integer(I8P), allocatable, intent(out) :: x(:)      !< Values.
  integer(I4P),              intent(out) :: error     !< Error status: 0, or 5 if I8P cannot hold the type.
  integer(I8P)                           :: n         !< Number of values.
  integer(I8P)                           :: i         !< Counter.
  integer(I8P)                           :: k         !< First byte of the current value.

  error = 5
  allocate(x(1:0))
  select case(data_type)
  case('Int8', 'UInt8', 'Int16', 'UInt16', 'Int32', 'UInt32', 'Int64', 'UInt64')
  case default
    return
  endselect
  n = size(bytes, kind=I8P) / data_type_size(data_type)
  deallocate(x)
  allocate(x(1:n))
  ! element-wise transfer: whole-array transfer results can be placed on the stack (e.g. ifx)
  select case(data_type)
  case('Int8')
    do i=1_I8P, n
      x(i) = int(bytes(i), I8P)
    enddo
  case('UInt8')
    do i=1_I8P, n
      x(i) = int(iand(int(bytes(i), I4P), 255_I4P), I8P)
    enddo
  case('Int16')
    do i=1_I8P, n
      k = (i - 1_I8P) * 2_I8P + 1_I8P
      x(i) = int(transfer(bytes(k:k+1_I8P), 0_I2P), I8P)
    enddo
  case('UInt16')
    do i=1_I8P, n
      k = (i - 1_I8P) * 2_I8P + 1_I8P
      x(i) = int(iand(int(transfer(bytes(k:k+1_I8P), 0_I2P), I4P), 65535_I4P), I8P)
    enddo
  case('Int32')
    do i=1_I8P, n
      k = (i - 1_I8P) * 4_I8P + 1_I8P
      x(i) = int(transfer(bytes(k:k+3_I8P), 0_I4P), I8P)
    enddo
  case('UInt32')
    do i=1_I8P, n
      k = (i - 1_I8P) * 4_I8P + 1_I8P
      x(i) = iand(int(transfer(bytes(k:k+3_I8P), 0_I4P), I8P), 4294967295_I8P)
    enddo
  case('Int64')
    do i=1_I8P, n
      k = (i - 1_I8P) * 8_I8P + 1_I8P
      x(i) = transfer(bytes(k:k+7_I8P), 0_I8P)
    enddo
  case('UInt64')
    do i=1_I8P, n
      k = (i - 1_I8P) * 8_I8P + 1_I8P
      x(i) = transfer(bytes(k:k+7_I8P), 0_I8P)
    enddo
  endselect
  error = 0
  endsubroutine bytes_to_array_I8P

  subroutine bytes_to_array_I4P(bytes, data_type, x, error)
  !< Convert the bytes of a dataarray of a VTK type into an array (I4P): see [[bytes_to_array]] for the conversions allowed.
  integer(I1P),              intent(in)  :: bytes(1:) !< Bytes of the dataarray (native byte order).
  character(*),              intent(in)  :: data_type !< VTK type of the dataarray.
  integer(I4P), allocatable, intent(out) :: x(:)      !< Values.
  integer(I4P),              intent(out) :: error     !< Error status: 0, or 5 if I4P cannot hold the type.
  integer(I8P)                           :: n         !< Number of values.
  integer(I8P)                           :: i         !< Counter.
  integer(I8P)                           :: k         !< First byte of the current value.

  error = 5
  allocate(x(1:0))
  select case(data_type)
  case('Int8', 'UInt8', 'Int16', 'UInt16', 'Int32', 'UInt32')
  case default
    return
  endselect
  n = size(bytes, kind=I8P) / data_type_size(data_type)
  deallocate(x)
  allocate(x(1:n))
  ! element-wise transfer: whole-array transfer results can be placed on the stack (e.g. ifx)
  select case(data_type)
  case('Int8')
    do i=1_I8P, n
      x(i) = int(bytes(i), I4P)
    enddo
  case('UInt8')
    do i=1_I8P, n
      x(i) = int(iand(int(bytes(i), I4P), 255_I4P), I4P)
    enddo
  case('Int16')
    do i=1_I8P, n
      k = (i - 1_I8P) * 2_I8P + 1_I8P
      x(i) = int(transfer(bytes(k:k+1_I8P), 0_I2P), I4P)
    enddo
  case('UInt16')
    do i=1_I8P, n
      k = (i - 1_I8P) * 2_I8P + 1_I8P
      x(i) = int(iand(int(transfer(bytes(k:k+1_I8P), 0_I2P), I4P), 65535_I4P), I4P)
    enddo
  case('Int32')
    do i=1_I8P, n
      k = (i - 1_I8P) * 4_I8P + 1_I8P
      x(i) = transfer(bytes(k:k+3_I8P), 0_I4P)
    enddo
  case('UInt32')
    do i=1_I8P, n
      k = (i - 1_I8P) * 4_I8P + 1_I8P
      x(i) = transfer(bytes(k:k+3_I8P), 0_I4P)
    enddo
  endselect
  error = 0
  endsubroutine bytes_to_array_I4P

  subroutine bytes_to_array_I2P(bytes, data_type, x, error)
  !< Convert the bytes of a dataarray of a VTK type into an array (I2P): see [[bytes_to_array]] for the conversions allowed.
  integer(I1P),              intent(in)  :: bytes(1:) !< Bytes of the dataarray (native byte order).
  character(*),              intent(in)  :: data_type !< VTK type of the dataarray.
  integer(I2P), allocatable, intent(out) :: x(:)      !< Values.
  integer(I4P),              intent(out) :: error     !< Error status: 0, or 5 if I2P cannot hold the type.
  integer(I8P)                           :: n         !< Number of values.
  integer(I8P)                           :: i         !< Counter.
  integer(I8P)                           :: k         !< First byte of the current value.

  error = 5
  allocate(x(1:0))
  select case(data_type)
  case('Int8', 'UInt8', 'Int16', 'UInt16')
  case default
    return
  endselect
  n = size(bytes, kind=I8P) / data_type_size(data_type)
  deallocate(x)
  allocate(x(1:n))
  ! element-wise transfer: whole-array transfer results can be placed on the stack (e.g. ifx)
  select case(data_type)
  case('Int8')
    do i=1_I8P, n
      x(i) = int(bytes(i), I2P)
    enddo
  case('UInt8')
    do i=1_I8P, n
      x(i) = int(iand(int(bytes(i), I4P), 255_I4P), I2P)
    enddo
  case('Int16')
    do i=1_I8P, n
      k = (i - 1_I8P) * 2_I8P + 1_I8P
      x(i) = transfer(bytes(k:k+1_I8P), 0_I2P)
    enddo
  case('UInt16')
    do i=1_I8P, n
      k = (i - 1_I8P) * 2_I8P + 1_I8P
      x(i) = transfer(bytes(k:k+1_I8P), 0_I2P)
    enddo
  endselect
  error = 0
  endsubroutine bytes_to_array_I2P

  subroutine bytes_to_array_I1P(bytes, data_type, x, error)
  !< Convert the bytes of a dataarray of a VTK type into an array (I1P): see [[bytes_to_array]] for the conversions allowed.
  integer(I1P),              intent(in)  :: bytes(1:) !< Bytes of the dataarray (native byte order).
  character(*),              intent(in)  :: data_type !< VTK type of the dataarray.
  integer(I1P), allocatable, intent(out) :: x(:)      !< Values.
  integer(I4P),              intent(out) :: error     !< Error status: 0, or 5 if I1P cannot hold the type.
  integer(I8P)                           :: n         !< Number of values.
  integer(I8P)                           :: i         !< Counter.

  error = 5
  allocate(x(1:0))
  select case(data_type)
  case('Int8', 'UInt8')
  case default
    return
  endselect
  n = size(bytes, kind=I8P) / data_type_size(data_type)
  deallocate(x)
  allocate(x(1:n))
  ! element-wise transfer: whole-array transfer results can be placed on the stack (e.g. ifx)
  select case(data_type)
  case('Int8')
    do i=1_I8P, n
      x(i) = bytes(i)
    enddo
  case('UInt8')
    do i=1_I8P, n
      x(i) = bytes(i)
    enddo
  endselect
  error = 0
  endsubroutine bytes_to_array_I1P

endmodule vtk_fortran_dataarray_decoder
