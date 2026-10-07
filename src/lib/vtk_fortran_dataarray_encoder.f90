!< DataArray encoder, codecs: "ascii", "base64".
module vtk_fortran_dataarray_encoder
!< VTK file XMl writer, ascii local.
use befor64
use penf
use vtk_fortran_parameters, only : stderr

implicit none
private
public :: encode_ascii_dataarray
public :: encode_binary_dataarray
public :: bytes_count

interface encode_ascii_dataarray
  !< Ascii DataArray encoder.
  module procedure encode_ascii_dataarray1_rank1_R8P, &
                   encode_ascii_dataarray1_rank1_R4P, &
                   encode_ascii_dataarray1_rank1_I8P, &
                   encode_ascii_dataarray1_rank1_I4P, &
                   encode_ascii_dataarray1_rank1_I2P, &
                   encode_ascii_dataarray1_rank1_I1P, &
                   encode_ascii_dataarray1_rank2_R8P, &
                   encode_ascii_dataarray1_rank2_R4P, &
                   encode_ascii_dataarray1_rank2_I8P, &
                   encode_ascii_dataarray1_rank2_I4P, &
                   encode_ascii_dataarray1_rank2_I2P, &
                   encode_ascii_dataarray1_rank2_I1P, &
                   encode_ascii_dataarray1_rank3_R8P, &
                   encode_ascii_dataarray1_rank3_R4P, &
                   encode_ascii_dataarray1_rank3_I8P, &
                   encode_ascii_dataarray1_rank3_I4P, &
                   encode_ascii_dataarray1_rank3_I2P, &
                   encode_ascii_dataarray1_rank3_I1P, &
                   encode_ascii_dataarray1_rank4_R8P, &
                   encode_ascii_dataarray1_rank4_R4P, &
                   encode_ascii_dataarray1_rank4_I8P, &
                   encode_ascii_dataarray1_rank4_I4P, &
                   encode_ascii_dataarray1_rank4_I2P, &
                   encode_ascii_dataarray1_rank4_I1P, &
                   encode_ascii_dataarray3_rank1_R8P, &
                   encode_ascii_dataarray3_rank1_R4P, &
                   encode_ascii_dataarray3_rank1_I8P, &
                   encode_ascii_dataarray3_rank1_I4P, &
                   encode_ascii_dataarray3_rank1_I2P, &
                   encode_ascii_dataarray3_rank1_I1P, &
                   encode_ascii_dataarray3_rank3_R8P, &
                   encode_ascii_dataarray3_rank3_R4P, &
                   encode_ascii_dataarray3_rank3_I8P, &
                   encode_ascii_dataarray3_rank3_I4P, &
                   encode_ascii_dataarray3_rank3_I2P, &
                   encode_ascii_dataarray3_rank3_I1P, &
                   encode_ascii_dataarray6_rank1_R8P, &
                   encode_ascii_dataarray6_rank1_R4P, &
                   encode_ascii_dataarray6_rank1_I8P, &
                   encode_ascii_dataarray6_rank1_I4P, &
                   encode_ascii_dataarray6_rank1_I2P, &
                   encode_ascii_dataarray6_rank1_I1P, &
                   encode_ascii_dataarray6_rank3_R8P, &
                   encode_ascii_dataarray6_rank3_R4P, &
                   encode_ascii_dataarray6_rank3_I8P, &
                   encode_ascii_dataarray6_rank3_I4P, &
                   encode_ascii_dataarray6_rank3_I2P, &
                   encode_ascii_dataarray6_rank3_I1P
endinterface encode_ascii_dataarray
interface encode_payload
  !< Encode (Base64) the bytes count header followed by the data.
  module procedure encode_payload_R8P, encode_payload_R4P, encode_payload_I8P, &
                   encode_payload_I4P, encode_payload_I2P, encode_payload_I1P
endinterface encode_payload

interface encode_binary_dataarray
  !< Binary (base64) DataArray encoder.
  module procedure encode_binary_dataarray1_rank1_R8P, &
                   encode_binary_dataarray1_rank1_R4P, &
                   encode_binary_dataarray1_rank1_I8P, &
                   encode_binary_dataarray1_rank1_I4P, &
                   encode_binary_dataarray1_rank1_I2P, &
                   encode_binary_dataarray1_rank1_I1P, &
                   encode_binary_dataarray1_rank2_R8P, &
                   encode_binary_dataarray1_rank2_R4P, &
                   encode_binary_dataarray1_rank2_I8P, &
                   encode_binary_dataarray1_rank2_I4P, &
                   encode_binary_dataarray1_rank2_I2P, &
                   encode_binary_dataarray1_rank2_I1P, &
                   encode_binary_dataarray1_rank3_R8P, &
                   encode_binary_dataarray1_rank3_R4P, &
                   encode_binary_dataarray1_rank3_I8P, &
                   encode_binary_dataarray1_rank3_I4P, &
                   encode_binary_dataarray1_rank3_I2P, &
                   encode_binary_dataarray1_rank3_I1P, &
                   encode_binary_dataarray1_rank4_R8P, &
                   encode_binary_dataarray1_rank4_R4P, &
                   encode_binary_dataarray1_rank4_I8P, &
                   encode_binary_dataarray1_rank4_I4P, &
                   encode_binary_dataarray1_rank4_I2P, &
                   encode_binary_dataarray1_rank4_I1P, &
                   encode_binary_dataarray3_rank1_R8P, &
                   encode_binary_dataarray3_rank1_R4P, &
                   encode_binary_dataarray3_rank1_I8P, &
                   encode_binary_dataarray3_rank1_I4P, &
                   encode_binary_dataarray3_rank1_I2P, &
                   encode_binary_dataarray3_rank1_I1P, &
                   encode_binary_dataarray3_rank3_R8P, &
                   encode_binary_dataarray3_rank3_R4P, &
                   encode_binary_dataarray3_rank3_I8P, &
                   encode_binary_dataarray3_rank3_I4P, &
                   encode_binary_dataarray3_rank3_I2P, &
                   encode_binary_dataarray3_rank3_I1P, &
                   encode_binary_dataarray6_rank1_R8P, &
                   encode_binary_dataarray6_rank1_R4P, &
                   encode_binary_dataarray6_rank1_I8P, &
                   encode_binary_dataarray6_rank1_I4P, &
                   encode_binary_dataarray6_rank1_I2P, &
                   encode_binary_dataarray6_rank1_I1P, &
                   encode_binary_dataarray6_rank3_R8P, &
                   encode_binary_dataarray6_rank3_R4P, &
                   encode_binary_dataarray6_rank3_I8P, &
                   encode_binary_dataarray6_rank3_I4P, &
                   encode_binary_dataarray6_rank3_I2P, &
                   encode_binary_dataarray6_rank3_I1P
endinterface encode_binary_dataarray
contains
  !< ascii encoder
  function encode_ascii_dataarray1_rank1_R16P(x) result(code)
  !< Encode (Base64) a dataarray with 1 components of rank 1 (R8P).
  real(R16P),      intent(in)   :: x(1:) !< Data variable.
  character(len=:), allocatable :: code  !< Encoded base64 dataarray.
  integer(I4P)                  :: n     !< Counter.
  integer(I8P)                  :: l     !< Length.
  integer(I8P)                  :: sp    !< String pointer.
  integer(I4P)                  :: size_n!< Dimension size.

  size_n = size(x,dim=1)
  l = DR16P+1
  sp = 0
  allocate(character(len=l*size_n) :: code)
  code(:) = ''
  do n = 1,size_n
      code(sp+1:sp+l) = str(n=x(n))
      sp = sp + l
  enddo
  endfunction encode_ascii_dataarray1_rank1_R16P

  function encode_ascii_dataarray1_rank1_R8P(x) result(code)
  !< Encode (Base64) a dataarray with 1 components of rank 1 (R8P).
  real(R8P),       intent(in)   :: x(1:) !< Data variable.
  character(len=:), allocatable :: code  !< Encoded base64 dataarray.
  integer(I4P)                  :: n     !< Counter.
  integer(I8P)                  :: l     !< Length.
  integer(I8P)                  :: sp    !< String pointer.
  integer(I4P)                  :: size_n!< Dimension size.

  size_n = size(x,dim=1)
  l = DR8P+1
  sp = 0
  allocate(character(len=l*size_n) :: code)
  code(:) = ''
  do n = 1,size_n
      code(sp+1:sp+l) = str(n=x(n))
      sp = sp + l
  enddo
  endfunction encode_ascii_dataarray1_rank1_R8P

  function encode_ascii_dataarray1_rank1_R4P(x) result(code)
  !< Encode (Base64) a dataarray with 1 components of rank 1 (R4P).
  real(R4P),       intent(in)   :: x(1:) !< Data variable.
  character(len=:), allocatable :: code  !< Encoded base64 dataarray.
  integer(I4P)                  :: n     !< Counter.
  integer(I8P)                  :: l     !< Length.
  integer(I8P)                  :: sp    !< String pointer.
  integer(I4P)                  :: size_n!< Dimension size.

  size_n = size(x,dim=1)
  l = DR4P+1
  sp = 0
  allocate(character(len=l*size_n) :: code)
  code(:) = ''
  do n = 1,size_n
      code(sp+1:sp+l) = str(n=x(n))
      sp = sp + l
  enddo
  endfunction encode_ascii_dataarray1_rank1_R4P

  function encode_ascii_dataarray1_rank1_I8P(x) result(code)
  !< Encode (Base64) a dataarray with 1 components of rank 1 (I8P).
  integer(I8P),    intent(in)   :: x(1:) !< Data variable.
  character(len=:), allocatable :: code  !< Encoded base64 dataarray.
  integer(I4P)                  :: n     !< Counter.
  integer(I8P)                  :: l     !< Length.
  integer(I8P)                  :: sp    !< String pointer.
  integer(I4P)                  :: size_n!< Dimension size.

  size_n = size(x,dim=1)
  l = DI8P+1
  sp = 0
  allocate(character(len=l*size_n) :: code)
  code(:) = ''
  do n = 1,size_n
      code(sp+1:sp+l) = str(n=x(n))
      sp = sp + l
  enddo
  endfunction encode_ascii_dataarray1_rank1_I8P

  function encode_ascii_dataarray1_rank1_I4P(x) result(code)
  !< Encode (Base64) a dataarray with 1 components of rank 1 (I4P).
  integer(I4P),    intent(in)   :: x(1:) !< Data variable.
  character(len=:), allocatable :: code  !< Encoded base64 dataarray.
  integer(I4P)                  :: n     !< Counter.
  integer(I8P)                  :: l     !< Length.
  integer(I8P)                  :: sp    !< String pointer.
  integer(I4P)                  :: size_n!< Dimension size.

  size_n = size(x,dim=1)
  l = DI4P+1
  sp = 0
  allocate(character(len=l*size_n) :: code)
  code(:) = ''
  do n = 1,size_n
      code(sp+1:sp+l) = str(n=x(n))
      sp = sp + l
  enddo
  endfunction encode_ascii_dataarray1_rank1_I4P

  function encode_ascii_dataarray1_rank1_I2P(x) result(code)
  !< Encode (Base64) a dataarray with 1 components of rank 1 (I2P).
  integer(I2P),    intent(in)   :: x(1:) !< Data variable.
  character(len=:), allocatable :: code  !< Encoded base64 dataarray.
  integer(I4P)                  :: n     !< Counter.
  integer(I8P)                  :: l     !< Length.
  integer(I8P)                  :: sp    !< String pointer.
  integer(I4P)                  :: size_n!< Dimension size.

  size_n = size(x,dim=1)
  l = DI2P+1
  sp = 0
  allocate(character(len=l*size_n) :: code)
  code(:) = ''
  do n = 1,size_n
      code(sp+1:sp+l) = str(n=x(n))
      sp = sp + l
  enddo
  endfunction encode_ascii_dataarray1_rank1_I2P

  function encode_ascii_dataarray1_rank1_I1P(x) result(code)
  !< Encode (Base64) a dataarray with 1 components of rank 1 (I1P).
  integer(I1P),    intent(in)   :: x(1:) !< Data variable.
  character(len=:), allocatable :: code  !< Encoded base64 dataarray.
  integer(I4P)                  :: n     !< Counter.
  integer(I8P)                  :: l     !< Length.
  integer(I8P)                  :: sp    !< String pointer.
  integer(I4P)                  :: size_n!< Dimension size.

  size_n = size(x,dim=1)
  l = DI1P+1
  sp = 0
  allocate(character(len=l*size_n) :: code)
  code(:) = ''
  do n = 1,size_n
      code(sp+1:sp+l) = str(n=x(n))
      sp = sp + l
  enddo
  endfunction encode_ascii_dataarray1_rank1_I1P

  function encode_ascii_dataarray1_rank2_R16P(x) result(code)
  !< Encode (Base64) a dataarray with 1 components of rank 2 (R16P).
  real(R16P),      intent(in)   :: x(1:,1:) !< Data variable
  character(len=:), allocatable :: code     !< Encoded base64 dataarray.
  integer(I4P)                  :: n1       !< Counter.
  integer(I4P)                  :: n2       !< Counter.
  integer(I8P)                  :: l        !< Length.
  integer(I8P)                  :: sp       !< String pointer.
  integer(I4P)                  :: size_n1  !< Dimension 1 size.
  integer(I4P)                  :: size_n2  !< Dimension 2 size.

  size_n1 = size(x, dim=1)
  size_n2 = size(x, dim=2)
  l = DR16P + 1
  sp = 0
  allocate(character(len=l*size_n1*size_n2) :: code)
  code(:) = ''
  do n2=1, size(x, dim=2)
    do n1=1, size(x, dim=1)-1
      code(sp+1:sp+l) = str(n=x(n1, n2))//' '
      sp = sp + l
    enddo
    code(sp+1:sp+l) = ' '//str(n=x(size(x, dim=1), n2))
    sp = sp + l
  enddo
  endfunction encode_ascii_dataarray1_rank2_R16P

  function encode_ascii_dataarray1_rank2_R8P(x) result(code)
  !< Encode (Base64) a dataarray with 1 components of rank 2 (R8P).
  real(R8P),       intent(in)   :: x(1:,1:) !< Data variable
  character(len=:), allocatable :: code     !< Encoded base64 dataarray.
  integer(I4P)                  :: n1       !< Counter.
  integer(I4P)                  :: n2       !< Counter.
  integer(I8P)                  :: l        !< Length.
  integer(I8P)                  :: sp       !< String pointer.
  integer(I4P)                  :: size_n1  !< Dimension 1 size.
  integer(I4P)                  :: size_n2  !< Dimension 2 size.

  size_n1 = size(x, dim=1)
  size_n2 = size(x, dim=2)
  l = DR8P + 1
  sp = 0
  allocate(character(len=l*size_n1*size_n2) :: code)
  code(:) = ''
  do n2=1, size(x, dim=2)
    do n1=1, size(x, dim=1)-1
      code(sp+1:sp+l) = str(n=x(n1, n2))//' '
      sp = sp + l
    enddo
    code(sp+1:sp+l) = ' '//str(n=x(size(x, dim=1), n2))
    sp = sp + l
  enddo
  endfunction encode_ascii_dataarray1_rank2_R8P

  function encode_ascii_dataarray1_rank2_R4P(x) result(code)
  !< Encode (Base64) a dataarray with 1 components of rank 2 (R4P).
  real(R4P),       intent(in)   :: x(1:,1:) !< Data variable
  character(len=:), allocatable :: code     !< Encoded base64 dataarray.
  integer(I4P)                  :: n1       !< Counter.
  integer(I4P)                  :: n2       !< Counter.
  integer(I8P)                  :: l        !< Length.
  integer(I8P)                  :: sp       !< String pointer.
  integer(I4P)                  :: size_n1  !< Dimension 1 size.
  integer(I4P)                  :: size_n2  !< Dimension 2 size.

  size_n1 = size(x, dim=1)
  size_n2 = size(x, dim=2)
  l = DR4P + 1
  sp = 0
  allocate(character(len=l*size_n1*size_n2) :: code)
  code(:) = ''
  do n2=1, size(x, dim=2)
    do n1=1, size(x, dim=1)-1
      code(sp+1:sp+l) = str(n=x(n1, n2))//' '
      sp = sp + l
    enddo
    code(sp+1:sp+l) = ' '//str(n=x(size(x, dim=1), n2))
    sp = sp + l
  enddo
  endfunction encode_ascii_dataarray1_rank2_R4P

  function encode_ascii_dataarray1_rank2_I8P(x) result(code)
  !< Encode (Base64) a dataarray with 1 components of rank 2 (I8P).
  integer(I8P),    intent(in)   :: x(1:,1:) !< Data variable
  character(len=:), allocatable :: code     !< Encoded base64 dataarray.
  integer(I4P)                  :: n1       !< Counter.
  integer(I4P)                  :: n2       !< Counter.
  integer(I8P)                  :: l        !< Length.
  integer(I8P)                  :: sp       !< String pointer.
  integer(I4P)                  :: size_n1  !< Dimension 1 size.
  integer(I4P)                  :: size_n2  !< Dimension 2 size.

  size_n1 = size(x, dim=1)
  size_n2 = size(x, dim=2)
  l = DI8P + 1
  sp = 0
  allocate(character(len=l*size_n1*size_n2) :: code)
  code(:) = ''
  do n2=1, size(x, dim=2)
    do n1=1, size(x, dim=1)-1
      code(sp+1:sp+l) = str(n=x(n1, n2))//' '
      sp = sp + l
    enddo
    code(sp+1:sp+l) = ' '//str(n=x(size(x, dim=1), n2))
    sp = sp + l
  enddo
  endfunction encode_ascii_dataarray1_rank2_I8P

  function encode_ascii_dataarray1_rank2_I4P(x) result(code)
  !< Encode (Base64) a dataarray with 1 components of rank 2 (I4P).
  integer(I4P),    intent(in)   :: x(1:,1:) !< Data variable
  character(len=:), allocatable :: code     !< Encoded base64 dataarray.
  integer(I4P)                  :: n1       !< Counter.
  integer(I4P)                  :: n2       !< Counter.
  integer(I8P)                  :: l        !< Length.
  integer(I8P)                  :: sp       !< String pointer.
  integer(I4P)                  :: size_n1  !< Dimension 1 size.
  integer(I4P)                  :: size_n2  !< Dimension 2 size.

  size_n1 = size(x, dim=1)
  size_n2 = size(x, dim=2)
  l = DI4P + 1
  sp = 0
  allocate(character(len=l*size_n1*size_n2) :: code)
  code(:) = ''
  do n2=1, size(x, dim=2)
    do n1=1, size(x, dim=1)-1
      code(sp+1:sp+l) = str(n=x(n1, n2))//' '
      sp = sp + l
    enddo
    code(sp+1:sp+l) = ' '//str(n=x(size(x, dim=1), n2))
    sp = sp + l
  enddo
  endfunction encode_ascii_dataarray1_rank2_I4P

  function encode_ascii_dataarray1_rank2_I2P(x) result(code)
  !< Encode (Base64) a dataarray with 1 components of rank 2 (I2P).
  integer(I2P),    intent(in)   :: x(1:,1:) !< Data variable
  character(len=:), allocatable :: code     !< Encoded base64 dataarray.
  integer(I4P)                  :: n1       !< Counter.
  integer(I4P)                  :: n2       !< Counter.
  integer(I8P)                  :: l        !< Length.
  integer(I8P)                  :: sp       !< String pointer.
  integer(I4P)                  :: size_n1  !< Dimension 1 size.
  integer(I4P)                  :: size_n2  !< Dimension 2 size.

  size_n1 = size(x, dim=1)
  size_n2 = size(x, dim=2)
  l = DI4P + 1
  sp = 0
  allocate(character(len=l*size_n1*size_n2) :: code)
  code(:) = ''
  do n2=1, size(x, dim=2)
    do n1=1, size(x, dim=1)-1
      code(sp+1:sp+l) = str(n=x(n1, n2))//' '
      sp = sp + l
    enddo
    code(sp+1:sp+l) = ' '//str(n=x(size(x, dim=1), n2))
    sp = sp + l
  enddo
  endfunction encode_ascii_dataarray1_rank2_I2P

  function encode_ascii_dataarray1_rank2_I1P(x) result(code)
  !< Encode (Base64) a dataarray with 1 components of rank 2 (I2P).
  integer(I1P),    intent(in)   :: x(1:,1:) !< Data variable
  character(len=:), allocatable :: code     !< Encoded base64 dataarray.
  integer(I4P)                  :: n1       !< Counter.
  integer(I4P)                  :: n2       !< Counter.
  integer(I8P)                  :: l        !< Length.
  integer(I8P)                  :: sp       !< String pointer.
  integer(I4P)                  :: size_n1  !< Dimension 1 size.
  integer(I4P)                  :: size_n2  !< Dimension 2 size.

  size_n1 = size(x, dim=1)
  size_n2 = size(x, dim=2)
  l = DI1P + 1
  sp = 0
  allocate(character(len=l*size_n1*size_n2) :: code)
  code(:) = ''
  do n2=1, size(x, dim=2)
    do n1=1, size(x, dim=1)-1
      code(sp+1:sp+l) = str(n=x(n1, n2))//' '
      sp = sp + l
    enddo
    code(sp+1:sp+l) = ' '//str(n=x(size(x, dim=1), n2))
    sp = sp + l
  enddo
  endfunction encode_ascii_dataarray1_rank2_I1P

  function encode_ascii_dataarray1_rank3_R16P(x) result(code)
  !< Encode (Base64) a dataarray with 1 components of rank 3 (R16P).
  real(R16P),      intent(in)   :: x(1:,1:,1:) !< Data variable
  character(len=:), allocatable :: code        !< Encoded base64 dataarray.
  integer(I4P)                  :: n1          !< Counter.
  integer(I4P)                  :: n2          !< Counter.
  integer(I4P)                  :: n3          !< Counter.
  integer(I8P)                  :: l           !< Length.
  integer(I8P)                  :: sp          !< String pointer.
  integer(I4P)                  :: size_n1     !< Dimension 1 size.
  integer(I4P)                  :: size_n2     !< Dimension 2 size.
  integer(I4P)                  :: size_n3     !< Dimension 3 size.

  size_n1 = size(x, dim=1)
  size_n2 = size(x, dim=2)
  size_n3 = size(x, dim=3)

  l = DR16P + 1
  sp = 0
  allocate(character(len=l*size_n1*size_n2*size_n3) :: code)
  code(:) = ''
  do n3=1, size(x, dim=3)
    do n2=1, size(x, dim=2)
      do n1=1, size(x, dim=1)-1
        code(sp+1:sp+l) = str(n=x(n1, n2, n3))//' '
        sp = sp + 1
      enddo
      code(sp+1:sp+l) = ' '//str(n=x(size(x, dim=1), n2, n3))
    enddo
  enddo
  endfunction encode_ascii_dataarray1_rank3_R16P

  function encode_ascii_dataarray1_rank3_R8P(x) result(code)
  !< Encode (Base64) a dataarray with 1 components of rank 3 (R8P).
  real(R8P),       intent(in)   :: x(1:,1:,1:) !< Data variable
  character(len=:), allocatable :: code        !< Encoded base64 dataarray.
  integer(I4P)                  :: n1          !< Counter.
  integer(I4P)                  :: n2          !< Counter.
  integer(I4P)                  :: n3          !< Counter.
  integer(I8P)                  :: l           !< Length.
  integer(I8P)                  :: sp          !< String pointer.
  integer(I4P)                  :: size_n1     !< Dimension 1 size.
  integer(I4P)                  :: size_n2     !< Dimension 2 size.
  integer(I4P)                  :: size_n3     !< Dimension 3 size.

  size_n1 = size(x, dim=1)
  size_n2 = size(x, dim=2)
  size_n3 = size(x, dim=3)

  l = DR8P + 1
  sp = 0
  allocate(character(len=l*size_n1*size_n2*size_n3) :: code)
  code(:) = ''
  do n3=1, size(x, dim=3)
    do n2=1, size(x, dim=2)
      do n1=1, size(x, dim=1)-1
        code(sp+1:sp+l) = str(n=x(n1, n2, n3))//' '
        sp = sp + l
      enddo
      code(sp+1:sp+l) = ' '//str(n=x(size(x, dim=1), n2, n3))
      sp = sp + l
    enddo
  enddo
  endfunction encode_ascii_dataarray1_rank3_R8P

  function encode_ascii_dataarray1_rank3_R4P(x) result(code)
  !< Encode (Base64) a dataarray with 1 components of rank 3 (R4P).
  real(R4P),       intent(in)   :: x(1:,1:,1:) !< Data variable
  character(len=:), allocatable :: code        !< Encoded base64 dataarray.
  integer(I4P)                  :: n1          !< Counter.
  integer(I4P)                  :: n2          !< Counter.
  integer(I4P)                  :: n3          !< Counter.
  integer(I8P)                  :: l           !< Length.
  integer(I8P)                  :: sp          !< String pointer.
  integer(I4P)                  :: size_n1     !< Dimension 1 size.
  integer(I4P)                  :: size_n2     !< Dimension 2 size.
  integer(I4P)                  :: size_n3     !< Dimension 3 size.

  size_n1 = size(x, dim=1)
  size_n2 = size(x, dim=2)
  size_n3 = size(x, dim=3)

  l = DR4P + 1
  sp = 0
  allocate(character(len=l*size_n1*size_n2*size_n3) :: code)
  code(:) = ''
  do n3=1, size(x, dim=3)
    do n2=1, size(x, dim=2)
      do n1=1, size(x, dim=1)-1
        code(sp+1:sp+l) = str(n=x(n1, n2, n3))//' '
        sp = sp + l
      enddo
      code(sp+1:sp+l) = ' '//str(n=x(size(x, dim=1), n2, n3))
      sp = sp + l
    enddo
  enddo
  endfunction encode_ascii_dataarray1_rank3_R4P

  function encode_ascii_dataarray1_rank3_I8P(x) result(code)
  !< Encode (Base64) a dataarray with 1 components of rank 3 (I8P).
  integer(I8P),    intent(in)   :: x(1:,1:,1:) !< Data variable
  character(len=:), allocatable :: code        !< Encoded base64 dataarray.
  integer(I4P)                  :: n1          !< Counter.
  integer(I4P)                  :: n2          !< Counter.
  integer(I4P)                  :: n3          !< Counter.
  integer(I8P)                  :: l           !< Length.
  integer(I8P)                  :: sp          !< String pointer.
  integer(I4P)                  :: size_n1     !< Dimension 1 size.
  integer(I4P)                  :: size_n2     !< Dimension 2 size.
  integer(I4P)                  :: size_n3     !< Dimension 3 size.

  size_n1 = size(x, dim=1)
  size_n2 = size(x, dim=2)
  size_n3 = size(x, dim=3)

  l = DI8P + 1
  sp = 0
  allocate(character(len=l*size_n1*size_n2*size_n3) :: code)
  code(:) = ''
  do n3=1, size(x, dim=3)
    do n2=1, size(x, dim=2)
      do n1=1, size(x, dim=1)-1
        code(sp+1:sp+l) = str(n=x(n1, n2, n3))//' '
        sp = sp + l
      enddo
      code(sp+1:sp+l) = ' '//str(n=x(size(x, dim=1), n2, n3))
      sp = sp + l
    enddo
  enddo
  endfunction encode_ascii_dataarray1_rank3_I8P

  function encode_ascii_dataarray1_rank3_I4P(x) result(code)
  !< Encode (Base64) a dataarray with 1 components of rank 3 (I4P).
  integer(I4P),    intent(in)   :: x(1:,1:,1:) !< Data variable
  character(len=:), allocatable :: code        !< Encoded base64 dataarray.
  integer(I4P)                  :: n1          !< Counter.
  integer(I4P)                  :: n2          !< Counter.
  integer(I4P)                  :: n3          !< Counter.
  integer(I8P)                  :: l           !< Length.
  integer(I8P)                  :: sp          !< String pointer.
  integer(I4P)                  :: size_n1     !< Dimension 1 size.
  integer(I4P)                  :: size_n2     !< Dimension 2 size.
  integer(I4P)                  :: size_n3     !< Dimension 3 size.

  size_n1 = size(x, dim=1)
  size_n2 = size(x, dim=2)
  size_n3 = size(x, dim=3)

  l = DI4P + 1
  sp = 0
  allocate(character(len=l*size_n1*size_n2*size_n3) :: code)
  code(:) = ''
  do n3=1, size(x, dim=3)
    do n2=1, size(x, dim=2)
      do n1=1, size(x, dim=1)-1
        code(sp+1:sp+l) = str(n=x(n1, n2, n3))//' '
        sp = sp + l
      enddo
      code(sp+1:sp+l) = ' '//str(n=x(size(x, dim=1), n2, n3))
      sp = sp + l
    enddo
  enddo
  endfunction encode_ascii_dataarray1_rank3_I4P

  function encode_ascii_dataarray1_rank3_I2P(x) result(code)
  !< Encode (Base64) a dataarray with 1 components of rank 3 (I2P).
  integer(I2P),    intent(in)   :: x(1:,1:,1:) !< Data variable
  character(len=:), allocatable :: code        !< Encoded base64 dataarray.
  integer(I4P)                  :: n1          !< Counter.
  integer(I4P)                  :: n2          !< Counter.
  integer(I4P)                  :: n3          !< Counter.
  integer(I8P)                  :: l           !< Length.
  integer(I8P)                  :: sp          !< String pointer.
  integer(I4P)                  :: size_n1     !< Dimension 1 size.
  integer(I4P)                  :: size_n2     !< Dimension 2 size.
  integer(I4P)                  :: size_n3     !< Dimension 3 size.

  size_n1 = size(x, dim=1)
  size_n2 = size(x, dim=2)
  size_n3 = size(x, dim=3)

  l = DI2P + 1
  sp = 0
  allocate(character(len=l*size_n1*size_n2*size_n3) :: code)
  code(:) = ''
  do n3=1, size(x, dim=3)
    do n2=1, size(x, dim=2)
      do n1=1, size(x, dim=1)-1
        code(sp+1:sp+l) = str(n=x(n1, n2, n3))//' '
        sp = sp + l
      enddo
      code(sp+1:sp+l) = ' '//str(n=x(size(x, dim=1), n2, n3))
      sp = sp + l
    enddo
  enddo
  endfunction encode_ascii_dataarray1_rank3_I2P

  function encode_ascii_dataarray1_rank3_I1P(x) result(code)
  !< Encode (Base64) a dataarray with 1 components of rank 3 (I1P).
  integer(I1P),    intent(in)   :: x(1:,1:,1:) !< Data variable
  character(len=:), allocatable :: code        !< Encoded base64 dataarray.
  integer(I4P)                  :: n1          !< Counter.
  integer(I4P)                  :: n2          !< Counter.
  integer(I4P)                  :: n3          !< Counter.
  integer(I8P)                  :: l           !< Length.
  integer(I8P)                  :: sp          !< String pointer.
  integer(I4P)                  :: size_n1     !< Dimension 1 size.
  integer(I4P)                  :: size_n2     !< Dimension 2 size.
  integer(I4P)                  :: size_n3     !< Dimension 3 size.

  size_n1 = size(x, dim=1)
  size_n2 = size(x, dim=2)
  size_n3 = size(x, dim=3)

  l = DI1P + 1
  sp = 0
  allocate(character(len=l*size_n1*size_n2*size_n3) :: code)
  code(:) = ''
  do n3=1, size(x, dim=3)
    do n2=1, size(x, dim=2)
      do n1=1, size(x, dim=1)-1
        code(sp+1:sp+l) = str(n=x(n1, n2, n3))//' '
        sp = sp + l
      enddo
      code(sp+1:sp+l) = ' '//str(n=x(size(x, dim=1), n2, n3))
      sp = sp + l
    enddo
  enddo
  endfunction encode_ascii_dataarray1_rank3_I1P

  function encode_ascii_dataarray1_rank4_R16P(x) result(code)
  !< Encode (Base64) a dataarray with 1 components of rank 4 (R16P).
  real(R16P),      intent(in)   :: x(1:,1:,1:,1:) !< Data variable.
  character(len=:), allocatable :: code           !< Encoded base64 dataarray.
  integer(I4P)                  :: n1             !< Counter.
  integer(I4P)                  :: n2             !< Counter.
  integer(I4P)                  :: n3             !< Counter.
  integer(I4P)                  :: n4             !< Counter.
  integer(I8P)                  :: l              !< Length.
  integer(I8P)                  :: sp             !< String pointer.
  integer(I4P)                  :: size_n1        !< Dimension 1 size.
  integer(I4P)                  :: size_n2        !< Dimension 2 size.
  integer(I4P)                  :: size_n3        !< Dimension 3 size.
  integer(I4P)                  :: size_n4        !< Dimension 4 size.

  size_n1 = size(x, dim=1)
  size_n2 = size(x, dim=2)
  size_n3 = size(x, dim=3)
  size_n4 = size(x, dim=4)

  l = DR16P + 1
  sp = 0
  allocate(character(len=l*size_n1*size_n2*size_n3*size_n4) :: code)
  code(:) = ''
  do n4=1, size(x, dim=4)
    do n3=1, size(x, dim=3)
      do n2=1, size(x, dim=2)
        do n1=1, size(x, dim=1)
          code(sp+1:sp+l) = str(n=x(n1, n2, n3, n4))//' '
          sp = sp + l
        enddo
      enddo
    enddo
  enddo
  endfunction encode_ascii_dataarray1_rank4_R16P

  function encode_ascii_dataarray1_rank4_R8P(x) result(code)
  !< Encode (Base64) a dataarray with 1 components of rank 4 (R8P).
  real(R8P),       intent(in)   :: x(1:,1:,1:,1:) !< Data variable.
  character(len=:), allocatable :: code           !< Encoded base64 dataarray.
  integer(I4P)                  :: n1             !< Counter.
  integer(I4P)                  :: n2             !< Counter.
  integer(I4P)                  :: n3             !< Counter.
  integer(I4P)                  :: n4             !< Counter.
  integer(I8P)                  :: l              !< Length.
  integer(I8P)                  :: sp             !< String pointer.
  integer(I4P)                  :: size_n1        !< Dimension 1 size.
  integer(I4P)                  :: size_n2        !< Dimension 2 size.
  integer(I4P)                  :: size_n3        !< Dimension 3 size.
  integer(I4P)                  :: size_n4        !< Dimension 4 size.

  size_n1 = size(x, dim=1)
  size_n2 = size(x, dim=2)
  size_n3 = size(x, dim=3)
  size_n4 = size(x, dim=4)

  l = DR8P + 1
  sp = 0
  allocate(character(len=l*size_n1*size_n2*size_n3*size_n4) :: code)
  code(:) = ''
  do n4=1, size(x, dim=4)
    do n3=1, size(x, dim=3)
      do n2=1, size(x, dim=2)
        do n1=1, size(x, dim=1)
          code(sp+1:sp+l) = str(n=x(n1, n2, n3, n4))//' '
          sp = sp + l
        enddo
      enddo
    enddo
  enddo
  endfunction encode_ascii_dataarray1_rank4_R8P

  function encode_ascii_dataarray1_rank4_R4P(x) result(code)
  !< Encode (Base64) a dataarray with 1 components of rank 4 (R4P).
  real(R4P),       intent(in)   :: x(1:,1:,1:,1:) !< Data variable.
  character(len=:), allocatable :: code           !< Encoded base64 dataarray.
  integer(I4P)                  :: n1             !< Counter.
  integer(I4P)                  :: n2             !< Counter.
  integer(I4P)                  :: n3             !< Counter.
  integer(I4P)                  :: n4             !< Counter.
  integer(I8P)                  :: l              !< Length.
  integer(I8P)                  :: sp             !< String pointer.
  integer(I4P)                  :: size_n1        !< Dimension 1 size.
  integer(I4P)                  :: size_n2        !< Dimension 2 size.
  integer(I4P)                  :: size_n3        !< Dimension 3 size.
  integer(I4P)                  :: size_n4        !< Dimension 4 size.

  size_n1 = size(x, dim=1)
  size_n2 = size(x, dim=2)
  size_n3 = size(x, dim=3)
  size_n4 = size(x, dim=4)

  l = DR4P + 1
  sp = 0
  allocate(character(len=l*size_n1*size_n2*size_n3*size_n4) :: code)
  code(:) = ''
  do n4=1, size(x, dim=4)
    do n3=1, size(x, dim=3)
      do n2=1, size(x, dim=2)
        do n1=1, size(x, dim=1)
          code(sp+1:sp+l) = str(n=x(n1, n2, n3, n4))//' '
          sp = sp + l
        enddo
      enddo
    enddo
  enddo
  endfunction encode_ascii_dataarray1_rank4_R4P

  function encode_ascii_dataarray1_rank4_I8P(x) result(code)
  !< Encode (Base64) a dataarray with 1 components of rank 4 (I8P).
  integer(I8P),    intent(in)   :: x(1:,1:,1:,1:) !< Data variable.
  character(len=:), allocatable :: code           !< Encoded base64 dataarray.
  integer(I4P)                  :: n1             !< Counter.
  integer(I4P)                  :: n2             !< Counter.
  integer(I4P)                  :: n3             !< Counter.
  integer(I4P)                  :: n4             !< Counter.
  integer(I8P)                  :: l              !< Length.
  integer(I8P)                  :: sp             !< String pointer.
  integer(I4P)                  :: size_n1        !< Dimension 1 size.
  integer(I4P)                  :: size_n2        !< Dimension 2 size.
  integer(I4P)                  :: size_n3        !< Dimension 3 size.
  integer(I4P)                  :: size_n4        !< Dimension 4 size.

  size_n1 = size(x, dim=1)
  size_n2 = size(x, dim=2)
  size_n3 = size(x, dim=3)
  size_n4 = size(x, dim=4)

  l = DI8P + 1
  sp = 0
  allocate(character(len=l*size_n1*size_n2*size_n3*size_n4) :: code)
  code(:) = ''
  do n4=1, size(x, dim=4)
    do n3=1, size(x, dim=3)
      do n2=1, size(x, dim=2)
        do n1=1, size(x, dim=1)
          code(sp+1:sp+l) = str(n=x(n1, n2, n3, n4))//' '
          sp = sp + l
        enddo
      enddo
    enddo
  enddo
  endfunction encode_ascii_dataarray1_rank4_I8P

  function encode_ascii_dataarray1_rank4_I4P(x) result(code)
  !< Encode (Base64) a dataarray with 1 components of rank 4 (I4P).
  integer(I4P),    intent(in)   :: x(1:,1:,1:,1:) !< Data variable.
  character(len=:), allocatable :: code           !< Encoded base64 dataarray.
  integer(I4P)                  :: n1             !< Counter.
  integer(I4P)                  :: n2             !< Counter.
  integer(I4P)                  :: n3             !< Counter.
  integer(I4P)                  :: n4             !< Counter.
  integer(I8P)                  :: l              !< Length.
  integer(I8P)                  :: sp             !< String pointer.
  integer(I4P)                  :: size_n1        !< Dimension 1 size.
  integer(I4P)                  :: size_n2        !< Dimension 2 size.
  integer(I4P)                  :: size_n3        !< Dimension 3 size.
  integer(I4P)                  :: size_n4        !< Dimension 4 size.

  size_n1 = size(x, dim=1)
  size_n2 = size(x, dim=2)
  size_n3 = size(x, dim=3)
  size_n4 = size(x, dim=4)

  l = DI4P + 1
  sp = 0
  allocate(character(len=l*size_n1*size_n2*size_n3*size_n4) :: code)
  code(:) = ''
  do n4=1, size(x, dim=4)
    do n3=1, size(x, dim=3)
      do n2=1, size(x, dim=2)
        do n1=1, size(x, dim=1)
          code(sp+1:sp+l) = str(n=x(n1, n2, n3, n4))//' '
          sp = sp + l
        enddo
      enddo
    enddo
  enddo
  endfunction encode_ascii_dataarray1_rank4_I4P

  function encode_ascii_dataarray1_rank4_I2P(x) result(code)
  !< Encode (Base64) a dataarray with 1 components of rank 4 (I2P).
  integer(I2P),    intent(in)   :: x(1:,1:,1:,1:) !< Data variable.
  character(len=:), allocatable :: code           !< Encoded base64 dataarray.
  integer(I4P)                  :: n1             !< Counter.
  integer(I4P)                  :: n2             !< Counter.
  integer(I4P)                  :: n3             !< Counter.
  integer(I4P)                  :: n4             !< Counter.
  integer(I8P)                  :: l              !< Length.
  integer(I8P)                  :: sp             !< String pointer.
  integer(I4P)                  :: size_n1        !< Dimension 1 size.
  integer(I4P)                  :: size_n2        !< Dimension 2 size.
  integer(I4P)                  :: size_n3        !< Dimension 3 size.
  integer(I4P)                  :: size_n4        !< Dimension 4 size.

  size_n1 = size(x, dim=1)
  size_n2 = size(x, dim=2)
  size_n3 = size(x, dim=3)
  size_n4 = size(x, dim=4)

  l = DI2P + 1
  sp = 0
  allocate(character(len=l*size_n1*size_n2*size_n3*size_n4) :: code)
  code(:) = ''
  do n4=1, size(x, dim=4)
    do n3=1, size(x, dim=3)
      do n2=1, size(x, dim=2)
        do n1=1, size(x, dim=1)
          code(sp+1:sp+l) = str(n=x(n1, n2, n3, n4))//' '
          sp = sp + l
        enddo
      enddo
    enddo
  enddo
  endfunction encode_ascii_dataarray1_rank4_I2P

  function encode_ascii_dataarray1_rank4_I1P(x) result(code)
  !< Encode (Base64) a dataarray with 1 components of rank 4 (I1P).
  integer(I1P),    intent(in)   :: x(1:,1:,1:,1:) !< Data variable.
  character(len=:), allocatable :: code           !< Encoded base64 dataarray.
  integer(I4P)                  :: n1             !< Counter.
  integer(I4P)                  :: n2             !< Counter.
  integer(I4P)                  :: n3             !< Counter.
  integer(I4P)                  :: n4             !< Counter.
  integer(I8P)                  :: l              !< Length.
  integer(I8P)                  :: sp             !< String pointer.
  integer(I4P)                  :: size_n1        !< Dimension 1 size.
  integer(I4P)                  :: size_n2        !< Dimension 2 size.
  integer(I4P)                  :: size_n3        !< Dimension 3 size.
  integer(I4P)                  :: size_n4        !< Dimension 4 size.

  size_n1 = size(x, dim=1)
  size_n2 = size(x, dim=2)
  size_n3 = size(x, dim=3)
  size_n4 = size(x, dim=4)

  l = DI1P + 1
  sp = 0
  allocate(character(len=l*size_n1*size_n2*size_n3*size_n4) :: code)
  code(:) = ''
  do n4=1, size(x, dim=4)
    do n3=1, size(x, dim=3)
      do n2=1, size(x, dim=2)
        do n1=1, size(x, dim=1)
          code(sp+1:sp+l) = str(n=x(n1, n2, n3, n4))//' '
          sp = sp + l
        enddo
      enddo
    enddo
  enddo
  endfunction encode_ascii_dataarray1_rank4_I1P

  function encode_ascii_dataarray3_rank1_R16P(x, y, z) result(code)
  !< Encode (Base64) a dataarray with 3 components of rank 1 (R16P).
  real(R16P),      intent(in)   :: x(1:) !< X component.
  real(R16P),      intent(in)   :: y(1:) !< Y component.
  real(R16P),      intent(in)   :: z(1:) !< Z component.
  character(len=:), allocatable :: code  !< Encoded base64 dataarray.
  integer(I4P)                  :: n     !< Counter.
  integer(I8P)                  :: l     !< Length.
  integer(I8P)                  :: sp    !< String pointer.
  integer(I4P)                  :: size_n!< Dimension 1 size.

  size_n = size(x, dim=1)
  l = DR16P*3 + 3
  sp = 0
  allocate(character(len=l*size_n) :: code)
  code(:) = ''
  do n=1, size_n
    code(sp+1:sp+l) = str(n=x(n))//' '//str(n=y(n))//' '//str(n=z(n))
    sp = sp + l
  enddo
  endfunction encode_ascii_dataarray3_rank1_R16P

  function encode_ascii_dataarray3_rank1_R8P(x, y, z) result(code)
  !< Encode (Base64) a dataarray with 3 components of rank 1 (R8P).
  real(R8P),       intent(in)   :: x(1:) !< X component.
  real(R8P),       intent(in)   :: y(1:) !< Y component.
  real(R8P),       intent(in)   :: z(1:) !< Z component.
  character(len=:), allocatable :: code  !< Encoded base64 dataarray.
  integer(I4P)                  :: n     !< Counter.
  integer(I8P)                  :: l     !< Length.
  integer(I8P)                  :: sp    !< String pointer.
  integer(I4P)                  :: size_n!< Dimension 1 size.

  size_n = size(x, dim=1)
  l = DR8P*3 + 3
  sp = 0
  allocate(character(len=l*size_n) :: code)
  code(:) = ''
  do n=1, size_n
    code(sp+1:sp+l) = str(n=x(n))//' '//str(n=y(n))//' '//str(n=z(n))
    sp = sp + l
  enddo
  endfunction encode_ascii_dataarray3_rank1_R8P

  function encode_ascii_dataarray3_rank1_R4P(x, y, z) result(code)
  !< Encode (Base64) a dataarray with 3 components of rank 1 (R4P).
  real(R4P),       intent(in)   :: x(1:) !< X component.
  real(R4P),       intent(in)   :: y(1:) !< Y component.
  real(R4P),       intent(in)   :: z(1:) !< Z component.
  character(len=:), allocatable :: code  !< Encoded base64 dataarray.
  integer(I4P)                  :: n     !< Counter.
  integer(I8P)                  :: l     !< Length.
  integer(I8P)                  :: sp    !< String pointer.
  integer(I4P)                  :: size_n!< Dimension 1 size.

  size_n = size(x, dim=1)
  l = DR4P*3 + 3
  sp = 0
  allocate(character(len=l*size_n) :: code)
  code(:) = ''
  do n=1, size_n
    code(sp+1:sp+l) = str(n=x(n))//' '//str(n=y(n))//' '//str(n=z(n))
    sp = sp + l
  enddo
  endfunction encode_ascii_dataarray3_rank1_R4P

  function encode_ascii_dataarray3_rank1_I8P(x, y, z) result(code)
  !< Encode (Base64) a dataarray with 3 components of rank 1 (I8P).
  integer(I8P),    intent(in)   :: x(1:) !< X component.
  integer(I8P),    intent(in)   :: y(1:) !< Y component.
  integer(I8P),    intent(in)   :: z(1:) !< Z component.
  character(len=:), allocatable :: code  !< Encoded base64 dataarray.
  integer(I4P)                  :: n     !< Counter.
  integer(I8P)                  :: l     !< Length.
  integer(I8P)                  :: sp    !< String pointer.
  integer(I4P)                  :: size_n!< Dimension 1 size.

  size_n = size(x, dim=1)
  l = DI8P*3 + 3
  sp = 0
  allocate(character(len=l*size_n) :: code)
  code(:) = ''
  do n=1, size_n
    code(sp+1:sp+l) = str(n=x(n))//' '//str(n=y(n))//' '//str(n=z(n))
    sp = sp + l
  enddo
  endfunction encode_ascii_dataarray3_rank1_I8P

  function encode_ascii_dataarray3_rank1_I4P(x, y, z) result(code)
  !< Encode (Base64) a dataarray with 3 components of rank 1 (I4P).
  integer(I4P),    intent(in)   :: x(1:) !< X component.
  integer(I4P),    intent(in)   :: y(1:) !< Y component.
  integer(I4P),    intent(in)   :: z(1:) !< Z component.
  character(len=:), allocatable :: code  !< Encoded base64 dataarray.
  integer(I4P)                  :: n     !< Counter.
  integer(I8P)                  :: l     !< Length.
  integer(I8P)                  :: sp    !< String pointer.
  integer(I4P)                  :: size_n!< Dimension 1 size.

  size_n = size(x, dim=1)
  l = DI4P*3 + 3
  sp = 0
  allocate(character(len=l*size_n) :: code)
  code(:) = ''
  do n=1, size_n
    code(sp+1:sp+l) = str(n=x(n))//' '//str(n=y(n))//' '//str(n=z(n))
    sp = sp + l
  enddo
  endfunction encode_ascii_dataarray3_rank1_I4P

  function encode_ascii_dataarray3_rank1_I2P(x, y, z) result(code)
  !< Encode (Base64) a dataarray with 3 components of rank 1 (I2P).
  integer(I2P),    intent(in)   :: x(1:) !< X component.
  integer(I2P),    intent(in)   :: y(1:) !< Y component.
  integer(I2P),    intent(in)   :: z(1:) !< Z component.
  character(len=:), allocatable :: code  !< Encoded base64 dataarray.
  integer(I4P)                  :: n     !< Counter.
  integer(I8P)                  :: l     !< Length.
  integer(I8P)                  :: sp    !< String pointer.
  integer(I4P)                  :: size_n!< Dimension 1 size.

  size_n = size(x, dim=1)
  l = DI2P*3 + 3
  sp = 0
  allocate(character(len=l*size_n) :: code)
  code(:) = ''
  do n=1, size_n
    code(sp+1:sp+l) = str(n=x(n))//' '//str(n=y(n))//' '//str(n=z(n))
    sp = sp + l
  enddo
  endfunction encode_ascii_dataarray3_rank1_I2P

  function encode_ascii_dataarray3_rank1_I1P(x, y, z) result(code)
  !< Encode (Base64) a dataarray with 3 components of rank 1 (I1P).
  integer(I1P),    intent(in)   :: x(1:) !< X component.
  integer(I1P),    intent(in)   :: y(1:) !< Y component.
  integer(I1P),    intent(in)   :: z(1:) !< Z component.
  character(len=:), allocatable :: code  !< Encoded base64 dataarray.
  integer(I4P)                  :: n     !< Counter.
  integer(I8P)                  :: l     !< Length.
  integer(I8P)                  :: sp    !< String pointer.
  integer(I4P)                  :: size_n!< Dimension 1 size.

  size_n = size(x, dim=1)
  l = DI1P*3 + 3
  sp = 0
  allocate(character(len=l*size_n) :: code)
  code(:) = ''
  do n=1, size_n
    code(sp+1:sp+l) = str(n=x(n))//' '//str(n=y(n))//' '//str(n=z(n))
    sp = sp + l
  enddo
  endfunction encode_ascii_dataarray3_rank1_I1P

  function encode_ascii_dataarray3_rank3_R16P(x, y, z) result(code)
  !< Encode (Base64) a dataarray with 3 components of rank 3 (R8P).
  real(R16P),      intent(in)   :: x(1:,1:,1:) !< X component.
  real(R16P),      intent(in)   :: y(1:,1:,1:) !< Y component.
  real(R16P),      intent(in)   :: z(1:,1:,1:) !< Z component.
  character(len=:), allocatable :: code        !< Encoded base64 dataarray.
  integer(I4P)                  :: n1          !< Counter.
  integer(I4P)                  :: n2          !< Counter.
  integer(I4P)                  :: n3          !< Counter.
  integer(I8P)                  :: l           !< Length.
  integer(I8P)                  :: sp          !< String pointer.
  integer(I4P)                  :: size_n1     !< Dimension 1 size.
  integer(I4P)                  :: size_n2     !< Dimension 2 size.
  integer(I4P)                  :: size_n3     !< Dimension 3 size.

  size_n1 = size(x, dim=1)
  size_n2 = size(x, dim=2)
  size_n3 = size(x, dim=3)

  l = DR16P*3 + 3
  sp = 0
  allocate(character(len=l*size_n1*size_n2*size_n3) :: code)
  code(:) = ''
  do n3=1, size(x, dim=3)
    do n2=1, size(x, dim=2)
      do n1=1, size(x, dim=1)
        code(sp+1:sp+l) = str(n=x(n1, n2, n3))//' '//str(n=y(n1, n2, n3))//' '//str(n=z(n1, n2, n3))
        sp = sp + l
      enddo
    enddo
  enddo
  endfunction encode_ascii_dataarray3_rank3_R16P

  function encode_ascii_dataarray3_rank3_R8P(x, y, z) result(code)
  !< Encode (Base64) a dataarray with 3 components of rank 3 (R8P).
  real(R8P),       intent(in)   :: x(1:,1:,1:) !< X component.
  real(R8P),       intent(in)   :: y(1:,1:,1:) !< Y component.
  real(R8P),       intent(in)   :: z(1:,1:,1:) !< Z component.
  character(len=:), allocatable :: code        !< Encoded base64 dataarray.
  integer(I4P)                  :: n1          !< Counter.
  integer(I4P)                  :: n2          !< Counter.
  integer(I4P)                  :: n3          !< Counter.
  integer(I8P)                  :: l           !< Length.
  integer(I8P)                  :: sp          !< String pointer.
  integer(I4P)                  :: size_n1     !< Dimension 1 size.
  integer(I4P)                  :: size_n2     !< Dimension 2 size.
  integer(I4P)                  :: size_n3     !< Dimension 3 size.

  size_n1 = size(x, dim=1)
  size_n2 = size(x, dim=2)
  size_n3 = size(x, dim=3)

  l = DR8P*3 + 3
  sp = 0
  allocate(character(len=l*size_n1*size_n2*size_n3) :: code)
  code(:) = ''
  do n3=1, size(x, dim=3)
    do n2=1, size(x, dim=2)
      do n1=1, size(x, dim=1)
        code(sp+1:sp+l) = str(n=x(n1, n2, n3))//' '//str(n=y(n1, n2, n3))//' '//str(n=z(n1, n2, n3))
        sp = sp + l
      enddo
    enddo
  enddo
  endfunction encode_ascii_dataarray3_rank3_R8P

  function encode_ascii_dataarray3_rank3_R4P(x, y, z) result(code)
  !< Encode (Base64) a dataarray with 3 components of rank 3 (R4P).
  real(R4P),       intent(in)   :: x(1:,1:,1:) !< X component.
  real(R4P),       intent(in)   :: y(1:,1:,1:) !< Y component.
  real(R4P),       intent(in)   :: z(1:,1:,1:) !< Z component.
  character(len=:), allocatable :: code        !< Encoded base64 dataarray.
  integer(I4P)                  :: n1          !< Counter.
  integer(I4P)                  :: n2          !< Counter.
  integer(I4P)                  :: n3          !< Counter.
  integer(I8P)                  :: l           !< Length.
  integer(I8P)                  :: sp          !< String pointer.
  integer(I4P)                  :: size_n1     !< Dimension 1 size.
  integer(I4P)                  :: size_n2     !< Dimension 2 size.
  integer(I4P)                  :: size_n3     !< Dimension 3 size.

  size_n1 = size(x, dim=1)
  size_n2 = size(x, dim=2)
  size_n3 = size(x, dim=3)

  l = DR4P*3 + 3
  sp = 0
  allocate(character(len=l*size_n1*size_n2*size_n3) :: code)
  code(:) = ''
  do n3=1, size(x, dim=3)
    do n2=1, size(x, dim=2)
      do n1=1, size(x, dim=1)
        code(sp+1:sp+l) = str(n=x(n1, n2, n3))//' '//str(n=y(n1, n2, n3))//' '//str(n=z(n1, n2, n3))
        sp = sp + l
      enddo
    enddo
  enddo
  endfunction encode_ascii_dataarray3_rank3_R4P

  function encode_ascii_dataarray3_rank3_I8P(x, y, z) result(code)
  !< Encode (Base64) a dataarray with 3 components of rank 3 (I8P).
  integer(I8P),    intent(in)   :: x(1:,1:,1:) !< X component.
  integer(I8P),    intent(in)   :: y(1:,1:,1:) !< Y component.
  integer(I8P),    intent(in)   :: z(1:,1:,1:) !< Z component.
  character(len=:), allocatable :: code        !< Encoded base64 dataarray.
  integer(I4P)                  :: n1          !< Counter.
  integer(I4P)                  :: n2          !< Counter.
  integer(I4P)                  :: n3          !< Counter.
  integer(I8P)                  :: l           !< Length.
  integer(I8P)                  :: sp          !< String pointer.
  integer(I4P)                  :: size_n1     !< Dimension 1 size.
  integer(I4P)                  :: size_n2     !< Dimension 2 size.
  integer(I4P)                  :: size_n3     !< Dimension 3 size.

  size_n1 = size(x, dim=1)
  size_n2 = size(x, dim=2)
  size_n3 = size(x, dim=3)

  l = DI8P*3 + 3
  sp = 0
  allocate(character(len=l*size_n1*size_n2*size_n3) :: code)
  code(:) = ''
  do n3=1, size(x, dim=3)
    do n2=1, size(x, dim=2)
      do n1=1, size(x, dim=1)
        code(sp+1:sp+l) = str(n=x(n1, n2, n3))//' '//str(n=y(n1, n2, n3))//' '//str(n=z(n1, n2, n3))
        sp = sp + l
      enddo
    enddo
  enddo
  endfunction encode_ascii_dataarray3_rank3_I8P

  function encode_ascii_dataarray3_rank3_I4P(x, y, z) result(code)
  !< Encode (Base64) a dataarray with 3 components of rank 3 (I4P).
  integer(I4P),    intent(in)   :: x(1:,1:,1:) !< X component.
  integer(I4P),    intent(in)   :: y(1:,1:,1:) !< Y component.
  integer(I4P),    intent(in)   :: z(1:,1:,1:) !< Z component.
  character(len=:), allocatable :: code        !< Encoded base64 dataarray.
  integer(I4P)                  :: n1          !< Counter.
  integer(I4P)                  :: n2          !< Counter.
  integer(I4P)                  :: n3          !< Counter.
  integer(I8P)                  :: l           !< Length.
  integer(I8P)                  :: sp          !< String pointer.
  integer(I4P)                  :: size_n1     !< Dimension 1 size.
  integer(I4P)                  :: size_n2     !< Dimension 2 size.
  integer(I4P)                  :: size_n3     !< Dimension 3 size.

  size_n1 = size(x, dim=1)
  size_n2 = size(x, dim=2)
  size_n3 = size(x, dim=3)

  l = DI4P*3 + 3
  sp = 0
  allocate(character(len=l*size_n1*size_n2*size_n3) :: code)
  code(:) = ''
  do n3=1, size(x, dim=3)
    do n2=1, size(x, dim=2)
      do n1=1, size(x, dim=1)
        code(sp+1:sp+l) = str(n=x(n1, n2, n3))//' '//str(n=y(n1, n2, n3))//' '//str(n=z(n1, n2, n3))
        sp = sp + l
      enddo
    enddo
  enddo
  endfunction encode_ascii_dataarray3_rank3_I4P

  function encode_ascii_dataarray3_rank3_I2P(x, y, z) result(code)
  !< Encode (Base64) a dataarray with 3 components of rank 3 (I2P).
  integer(I2P),    intent(in)   :: x(1:,1:,1:) !< X component.
  integer(I2P),    intent(in)   :: y(1:,1:,1:) !< Y component.
  integer(I2P),    intent(in)   :: z(1:,1:,1:) !< Z component.
  character(len=:), allocatable :: code        !< Encoded base64 dataarray.
  integer(I4P)                  :: n1          !< Counter.
  integer(I4P)                  :: n2          !< Counter.
  integer(I4P)                  :: n3          !< Counter.
  integer(I8P)                  :: l           !< Length.
  integer(I8P)                  :: sp          !< String pointer.
  integer(I4P)                  :: size_n1     !< Dimension 1 size.
  integer(I4P)                  :: size_n2     !< Dimension 2 size.
  integer(I4P)                  :: size_n3     !< Dimension 3 size.

  size_n1 = size(x, dim=1)
  size_n2 = size(x, dim=2)
  size_n3 = size(x, dim=3)

  l = DI2P*3 + 3
  sp = 0
  allocate(character(len=l*size_n1*size_n2*size_n3) :: code)
  code(:) = ''
  do n3=1, size(x, dim=3)
    do n2=1, size(x, dim=2)
      do n1=1, size(x, dim=1)
        code(sp+1:sp+l) = str(n=x(n1, n2, n3))//' '//str(n=y(n1, n2, n3))//' '//str(n=z(n1, n2, n3))
        sp = sp + l
      enddo
    enddo
  enddo
  endfunction encode_ascii_dataarray3_rank3_I2P

  function encode_ascii_dataarray3_rank3_I1P(x, y, z) result(code)
  !< Encode (Base64) a dataarray with 3 components of rank 3 (I1P).
  integer(I1P),    intent(in)   :: x(1:,1:,1:) !< X component.
  integer(I1P),    intent(in)   :: y(1:,1:,1:) !< Y component.
  integer(I1P),    intent(in)   :: z(1:,1:,1:) !< Z component.
  character(len=:), allocatable :: code        !< Encoded base64 dataarray.
  integer(I4P)                  :: n1          !< Counter.
  integer(I4P)                  :: n2          !< Counter.
  integer(I4P)                  :: n3          !< Counter.
  integer(I8P)                  :: l           !< Length.
  integer(I8P)                  :: sp          !< String pointer.
  integer(I4P)                  :: size_n1     !< Dimension 1 size.
  integer(I4P)                  :: size_n2     !< Dimension 2 size.
  integer(I4P)                  :: size_n3     !< Dimension 3 size.

  size_n1 = size(x, dim=1)
  size_n2 = size(x, dim=2)
  size_n3 = size(x, dim=3)

  l = DI1P*3 + 3
  sp = 0
  allocate(character(len=l*size_n1*size_n2*size_n3) :: code)
  code(:) = ''
  do n3=1, size(x, dim=3)
    do n2=1, size(x, dim=2)
      do n1=1, size(x, dim=1)
        code(sp+1:sp+l) = str(n=x(n1, n2, n3))//' '//str(n=y(n1, n2, n3))//' '//str(n=z(n1, n2, n3))
        sp = sp + l
      enddo
    enddo
  enddo
  endfunction encode_ascii_dataarray3_rank3_I1P

  function encode_ascii_dataarray6_rank1_R16P(u, v, w, x, y, z) result(code)
  !< Encode (Base64) a dataarray with 6 components of rank 1 (R16P).
  real(R16P),      intent(in)   :: u(1:) !< U component.
  real(R16P),      intent(in)   :: v(1:) !< V component.
  real(R16P),      intent(in)   :: w(1:) !< W component.
  real(R16P),      intent(in)   :: x(1:) !< X component.
  real(R16P),      intent(in)   :: y(1:) !< Y component.
  real(R16P),      intent(in)   :: z(1:) !< Z component.
  character(len=:), allocatable :: code  !< Encoded base64 dataarray.
  integer(I4P)                  :: n     !< Counter.
  integer(I8P)                  :: l     !< Length.
  integer(I8P)                  :: sp    !< String pointer.
  integer(I4P)                  :: size_n!< Dimension 1 size.

  size_n = size(x, dim=1)
  l = DR16P*6 + 6
  sp = 0
  allocate(character(len=l*size_n) :: code)
  code(:) = ''
  do n=1, size_n
    code(sp+1:sp+l) = str(n=u(n))//' '//str(n=v(n))//' '//str(n=w(n))// &
                str(n=x(n))//' '//str(n=y(n))//' '//str(n=z(n))
    sp = sp + l
  enddo
  endfunction encode_ascii_dataarray6_rank1_R16P

  function encode_ascii_dataarray6_rank1_R8P(u, v, w, x, y, z) result(code)
  !< Encode (Base64) a dataarray with 6 components of rank 1 (R8P).
  real(R8P),       intent(in)   :: u(1:) !< U component.
  real(R8P),       intent(in)   :: v(1:) !< V component.
  real(R8P),       intent(in)   :: w(1:) !< W component.
  real(R8P),       intent(in)   :: x(1:) !< X component.
  real(R8P),       intent(in)   :: y(1:) !< Y component.
  real(R8P),       intent(in)   :: z(1:) !< Z component.
  character(len=:), allocatable :: code  !< Encoded base64 dataarray.
  integer(I4P)                  :: n     !< Counter.
  integer(I8P)                  :: l     !< Length.
  integer(I8P)                  :: sp    !< String pointer.
  integer(I4P)                  :: size_n!< Dimension 1 size.

  size_n = size(x, dim=1)
  l = DR8P*6 + 6
  sp = 0
  allocate(character(len=l*size_n) :: code)
  code(:) = ''
  do n=1, size_n
    code(sp+1:sp+l) = str(n=u(n))//' '//str(n=v(n))//' '//str(n=w(n))// &
                str(n=x(n))//' '//str(n=y(n))//' '//str(n=z(n))
    sp = sp + l
  enddo
  endfunction encode_ascii_dataarray6_rank1_R8P

  function encode_ascii_dataarray6_rank1_R4P(u, v, w, x, y, z) result(code)
  !< Encode (Base64) a dataarray with 6 components of rank 1 (R4P).
  real(R4P),       intent(in)   :: u(1:) !< U component.
  real(R4P),       intent(in)   :: v(1:) !< V component.
  real(R4P),       intent(in)   :: w(1:) !< W component.
  real(R4P),       intent(in)   :: x(1:) !< X component.
  real(R4P),       intent(in)   :: y(1:) !< Y component.
  real(R4P),       intent(in)   :: z(1:) !< Z component.
  character(len=:), allocatable :: code  !< Encoded base64 dataarray.
  integer(I4P)                  :: n     !< Counter.
  integer(I8P)                  :: l     !< Length.
  integer(I8P)                  :: sp    !< String pointer.
  integer(I4P)                  :: size_n!< Dimension 1 size.

  size_n = size(x, dim=1)
  l = DR4P*6 + 6
  sp = 0
  allocate(character(len=l*size_n) :: code)
  code(:) = ''
  do n=1, size_n
    code(sp+1:sp+l) = str(n=u(n))//' '//str(n=v(n))//' '//str(n=w(n))// &
                str(n=x(n))//' '//str(n=y(n))//' '//str(n=z(n))
    sp = sp + l
  enddo
  endfunction encode_ascii_dataarray6_rank1_R4P

  function encode_ascii_dataarray6_rank1_I8P(u, v, w, x, y, z) result(code)
  !< Encode (Base64) a dataarray with 6 components of rank 1 (I8P).
  integer(I8P),    intent(in)   :: u(1:) !< U component.
  integer(I8P),    intent(in)   :: v(1:) !< V component.
  integer(I8P),    intent(in)   :: w(1:) !< W component.
  integer(I8P),    intent(in)   :: x(1:) !< X component.
  integer(I8P),    intent(in)   :: y(1:) !< Y component.
  integer(I8P),    intent(in)   :: z(1:) !< Z component.
  character(len=:), allocatable :: code  !< Encoded base64 dataarray.
  integer(I4P)                  :: n     !< Counter.
  integer(I8P)                  :: l     !< Length.
  integer(I8P)                  :: sp    !< String pointer.
  integer(I4P)                  :: size_n!< Dimension 1 size.

  size_n = size(x, dim=1)
  l = DI8P*6 + 6
  sp = 0
  allocate(character(len=l*size_n) :: code)
  code(:) = ''
  do n=1, size_n
    code(sp+1:sp+l) = str(n=u(n))//' '//str(n=v(n))//' '//str(n=w(n))// &
                str(n=x(n))//' '//str(n=y(n))//' '//str(n=z(n))
    sp = sp + l
  enddo
  endfunction encode_ascii_dataarray6_rank1_I8P

  function encode_ascii_dataarray6_rank1_I4P(u, v, w, x, y, z) result(code)
  !< Encode (Base64) a dataarray with 6 components of rank 1 (I4P).
  integer(I4P),    intent(in)   :: u(1:) !< U component.
  integer(I4P),    intent(in)   :: v(1:) !< V component.
  integer(I4P),    intent(in)   :: w(1:) !< W component.
  integer(I4P),    intent(in)   :: x(1:) !< X component.
  integer(I4P),    intent(in)   :: y(1:) !< Y component.
  integer(I4P),    intent(in)   :: z(1:) !< Z component.
  character(len=:), allocatable :: code  !< Encoded base64 dataarray.
  integer(I4P)                  :: n     !< Counter.
  integer(I8P)                  :: l     !< Length.
  integer(I8P)                  :: sp    !< String pointer.
  integer(I4P)                  :: size_n!< Dimension 1 size.

  size_n = size(x, dim=1)
  l = DI4P*6 + 6
  sp = 0
  allocate(character(len=l*size_n) :: code)
  code(:) = ''
  do n=1, size_n
    code(sp+1:sp+l) = str(n=u(n))//' '//str(n=v(n))//' '//str(n=w(n))// &
                str(n=x(n))//' '//str(n=y(n))//' '//str(n=z(n))
    sp = sp + l
  enddo
  endfunction encode_ascii_dataarray6_rank1_I4P

  function encode_ascii_dataarray6_rank1_I2P(u, v, w, x, y, z) result(code)
  !< Encode (Base64) a dataarray with 6 components of rank 1 (I2P).
  integer(I2P),    intent(in)   :: u(1:) !< U component.
  integer(I2P),    intent(in)   :: v(1:) !< V component.
  integer(I2P),    intent(in)   :: w(1:) !< W component.
  integer(I2P),    intent(in)   :: x(1:) !< X component.
  integer(I2P),    intent(in)   :: y(1:) !< Y component.
  integer(I2P),    intent(in)   :: z(1:) !< Z component.
  character(len=:), allocatable :: code  !< Encoded base64 dataarray.
  integer(I4P)                  :: n     !< Counter.
  integer(I8P)                  :: l     !< Length.
  integer(I8P)                  :: sp    !< String pointer.
  integer(I4P)                  :: size_n!< Dimension 1 size.

  size_n = size(x, dim=1)
  l = DI2P*6 + 6
  sp = 0
  allocate(character(len=l*size_n) :: code)
  code(:) = ''
  do n=1, size_n
    code(sp+1:sp+l) = str(n=u(n))//' '//str(n=v(n))//' '//str(n=w(n))// &
                str(n=x(n))//' '//str(n=y(n))//' '//str(n=z(n))
    sp = sp + l
  enddo
  endfunction encode_ascii_dataarray6_rank1_I2P

  function encode_ascii_dataarray6_rank1_I1P(u, v, w, x, y, z) result(code)
  !< Encode (Base64) a dataarray with 6 components of rank 1 (I1P).
  integer(I1P),    intent(in)   :: u(1:) !< U component.
  integer(I1P),    intent(in)   :: v(1:) !< V component.
  integer(I1P),    intent(in)   :: w(1:) !< W component.
  integer(I1P),    intent(in)   :: x(1:) !< X component.
  integer(I1P),    intent(in)   :: y(1:) !< Y component.
  integer(I1P),    intent(in)   :: z(1:) !< Z component.
  character(len=:), allocatable :: code  !< Encoded base64 dataarray.
  integer(I4P)                  :: n     !< Counter.
  integer(I8P)                  :: l     !< Length.
  integer(I8P)                  :: sp    !< String pointer.
  integer(I4P)                  :: size_n!< Dimension 1 size.

  size_n = size(x, dim=1)
  l = DI1P*6 + 6
  sp = 0
  allocate(character(len=l*size_n) :: code)
  code(:) = ''
  do n=1, size_n
    code(sp+1:sp+l) = str(n=u(n))//' '//str(n=v(n))//' '//str(n=w(n))// &
                str(n=x(n))//' '//str(n=y(n))//' '//str(n=z(n))
    sp = sp + l
  enddo
  endfunction encode_ascii_dataarray6_rank1_I1P

  function encode_ascii_dataarray6_rank3_R16P(u, v, w, x, y, z) result(code)
  !< Encode (Base64) a dataarray with 6 components of rank 3 (R8P).
  real(R16P),      intent(in)   :: u(1:,1:,1:) !< U component.
  real(R16P),      intent(in)   :: v(1:,1:,1:) !< V component.
  real(R16P),      intent(in)   :: w(1:,1:,1:) !< W component.
  real(R16P),      intent(in)   :: x(1:,1:,1:) !< X component.
  real(R16P),      intent(in)   :: y(1:,1:,1:) !< Y component.
  real(R16P),      intent(in)   :: z(1:,1:,1:) !< Z component.
  character(len=:), allocatable :: code        !< Encoded base64 dataarray.
  integer(I4P)                  :: n1          !< Counter.
  integer(I4P)                  :: n2          !< Counter.
  integer(I4P)                  :: n3          !< Counter.
  integer(I8P)                  :: l           !< Length.
  integer(I8P)                  :: sp          !< String pointer.
  integer(I4P)                  :: size_n1     !< Dimension 1 size.
  integer(I4P)                  :: size_n2     !< Dimension 2 size.
  integer(I4P)                  :: size_n3     !< Dimension 3 size.

  size_n1 = size(x, dim=1)
  size_n2 = size(x, dim=2)
  size_n3 = size(x, dim=3)

  l = DR16P*6 + 6
  sp = 0
  allocate(character(len=l*size_n1*size_n2*size_n3) :: code)
  code(:) = ''
  do n3=1, size_n3
    do n2=1, size_n2
      do n1=1, size_n1
        code(sp+1:sp+l) = str(n=u(n1, n2, n3))//' '//str(n=v(n1, n2, n3))//' '//str(n=w(n1, n2, n3))// &
          str(n=x(n1, n2, n3))//' '//str(n=y(n1, n2, n3))//' '//str(n=z(n1, n2, n3))
        sp = sp + l
      enddo
    enddo
  enddo
  endfunction encode_ascii_dataarray6_rank3_R16P

  function encode_ascii_dataarray6_rank3_R8P(u, v, w, x, y, z) result(code)
  !< Encode (Base64) a dataarray with 6 components of rank 3 (R8P).
  real(R8P),       intent(in)   :: u(1:,1:,1:) !< U component.
  real(R8P),       intent(in)   :: v(1:,1:,1:) !< V component.
  real(R8P),       intent(in)   :: w(1:,1:,1:) !< W component.
  real(R8P),       intent(in)   :: x(1:,1:,1:) !< X component.
  real(R8P),       intent(in)   :: y(1:,1:,1:) !< Y component.
  real(R8P),       intent(in)   :: z(1:,1:,1:) !< Z component.
  character(len=:), allocatable :: code        !< Encoded base64 dataarray.
  integer(I4P)                  :: n1          !< Counter.
  integer(I4P)                  :: n2          !< Counter.
  integer(I4P)                  :: n3          !< Counter.
  integer(I8P)                  :: l           !< Length.
  integer(I8P)                  :: sp          !< String pointer.
  integer(I4P)                  :: size_n1     !< Dimension 1 size.
  integer(I4P)                  :: size_n2     !< Dimension 2 size.
  integer(I4P)                  :: size_n3     !< Dimension 3 size.

  size_n1 = size(x, dim=1)
  size_n2 = size(x, dim=2)
  size_n3 = size(x, dim=3)

  l = DR8P*6 + 6
  sp = 0
  allocate(character(len=l*size_n1*size_n2*size_n3) :: code)
  code(:) = ''
  do n3=1, size_n3
    do n2=1, size_n2
      do n1=1, size_n1
        code(sp+1:sp+l) = str(n=u(n1, n2, n3))//' '//str(n=v(n1, n2, n3))//' '//str(n=w(n1, n2, n3))// &
          str(n=x(n1, n2, n3))//' '//str(n=y(n1, n2, n3))//' '//str(n=z(n1, n2, n3))
        sp = sp + l
      enddo
    enddo
  enddo
  endfunction encode_ascii_dataarray6_rank3_R8P

  function encode_ascii_dataarray6_rank3_R4P(u, v, w, x, y, z) result(code)
  !< Encode (Base64) a dataarray with 3 components of rank 3 (R4P).
  real(R4P),       intent(in)   :: u(1:,1:,1:) !< U component.
  real(R4P),       intent(in)   :: v(1:,1:,1:) !< V component.
  real(R4P),       intent(in)   :: w(1:,1:,1:) !< W component.
  real(R4P),       intent(in)   :: x(1:,1:,1:) !< X component.
  real(R4P),       intent(in)   :: y(1:,1:,1:) !< Y component.
  real(R4P),       intent(in)   :: z(1:,1:,1:) !< Z component.
  character(len=:), allocatable :: code        !< Encoded base64 dataarray.
  integer(I4P)                  :: n1          !< Counter.
  integer(I4P)                  :: n2          !< Counter.
  integer(I4P)                  :: n3          !< Counter.
  integer(I8P)                  :: l           !< Length.
  integer(I8P)                  :: sp          !< String pointer.
  integer(I4P)                  :: size_n1     !< Dimension 1 size.
  integer(I4P)                  :: size_n2     !< Dimension 2 size.
  integer(I4P)                  :: size_n3     !< Dimension 3 size.

  size_n1 = size(x, dim=1)
  size_n2 = size(x, dim=2)
  size_n3 = size(x, dim=3)

  l = DR4P*6 + 6
  sp = 0
  allocate(character(len=l*size_n1*size_n2*size_n3) :: code)
  code(:) = ''
  do n3=1, size_n3
    do n2=1, size_n2
      do n1=1, size_n1
        code(sp+1:sp+l) = str(n=u(n1, n2, n3))//' '//str(n=v(n1, n2, n3))//' '//str(n=w(n1, n2, n3))// &
          str(n=x(n1, n2, n3))//' '//str(n=y(n1, n2, n3))//' '//str(n=z(n1, n2, n3))
        sp = sp + l
      enddo
    enddo
  enddo
  endfunction encode_ascii_dataarray6_rank3_R4P

  function encode_ascii_dataarray6_rank3_I8P(u, v, w, x, y, z) result(code)
  !< Encode (Base64) a dataarray with 6 components of rank 3 (I8P).
  integer(I8P),    intent(in)   :: u(1:,1:,1:) !< U component.
  integer(I8P),    intent(in)   :: v(1:,1:,1:) !< V component.
  integer(I8P),    intent(in)   :: w(1:,1:,1:) !< W component.
  integer(I8P),    intent(in)   :: x(1:,1:,1:) !< X component.
  integer(I8P),    intent(in)   :: y(1:,1:,1:) !< Y component.
  integer(I8P),    intent(in)   :: z(1:,1:,1:) !< Z component.
  character(len=:), allocatable :: code        !< Encoded base64 dataarray.
  integer(I4P)                  :: n1          !< Counter.
  integer(I4P)                  :: n2          !< Counter.
  integer(I4P)                  :: n3          !< Counter.
  integer(I8P)                  :: l           !< Length.
  integer(I8P)                  :: sp          !< String pointer.
  integer(I4P)                  :: size_n1     !< Dimension 1 size.
  integer(I4P)                  :: size_n2     !< Dimension 2 size.
  integer(I4P)                  :: size_n3     !< Dimension 3 size.

  size_n1 = size(x, dim=1)
  size_n2 = size(x, dim=2)
  size_n3 = size(x, dim=3)

  l = DI8P*6 + 6
  sp = 0
  allocate(character(len=l*size_n1*size_n2*size_n3) :: code)
  code(:) = ''
  do n3=1, size_n3
    do n2=1, size_n2
      do n1=1, size_n1
        code(sp+1:sp+l) = str(n=u(n1, n2, n3))//' '//str(n=v(n1, n2, n3))//' '//str(n=w(n1, n2, n3))// &
          str(n=x(n1, n2, n3))//' '//str(n=y(n1, n2, n3))//' '//str(n=z(n1, n2, n3))
        sp = sp + l
      enddo
    enddo
  enddo
  endfunction encode_ascii_dataarray6_rank3_I8P

  function encode_ascii_dataarray6_rank3_I4P(u, v, w, x, y, z) result(code)
  !< Encode (Base64) a dataarray with 6 components of rank 3 (I4P).
  integer(I4P),    intent(in)   :: u(1:,1:,1:) !< U component.
  integer(I4P),    intent(in)   :: v(1:,1:,1:) !< V component.
  integer(I4P),    intent(in)   :: w(1:,1:,1:) !< W component.
  integer(I4P),    intent(in)   :: x(1:,1:,1:) !< X component.
  integer(I4P),    intent(in)   :: y(1:,1:,1:) !< Y component.
  integer(I4P),    intent(in)   :: z(1:,1:,1:) !< Z component.
  character(len=:), allocatable :: code        !< Encoded base64 dataarray.
  integer(I4P)                  :: n1          !< Counter.
  integer(I4P)                  :: n2          !< Counter.
  integer(I4P)                  :: n3          !< Counter.
  integer(I8P)                  :: l           !< Length.
  integer(I8P)                  :: sp          !< String pointer.
  integer(I4P)                  :: size_n1     !< Dimension 1 size.
  integer(I4P)                  :: size_n2     !< Dimension 2 size.
  integer(I4P)                  :: size_n3     !< Dimension 3 size.

  size_n1 = size(x, dim=1)
  size_n2 = size(x, dim=2)
  size_n3 = size(x, dim=3)

  l = DI4P*6 + 6
  sp = 0
  allocate(character(len=l*size_n1*size_n2*size_n3) :: code)
  code(:) = ''
  do n3=1, size_n3
    do n2=1, size_n2
      do n1=1, size_n1
        code(sp+1:sp+l) = str(n=u(n1, n2, n3))//' '//str(n=v(n1, n2, n3))//' '//str(n=w(n1, n2, n3))// &
          str(n=x(n1, n2, n3))//' '//str(n=y(n1, n2, n3))//' '//str(n=z(n1, n2, n3))
        sp = sp + l
      enddo
    enddo
  enddo
  endfunction encode_ascii_dataarray6_rank3_I4P

  function encode_ascii_dataarray6_rank3_I2P(u, v, w, x, y, z) result(code)
  !< Encode (Base64) a dataarray with 6 components of rank 3 (I2P).
  integer(I2P),    intent(in)   :: u(1:,1:,1:) !< U component.
  integer(I2P),    intent(in)   :: v(1:,1:,1:) !< V component.
  integer(I2P),    intent(in)   :: w(1:,1:,1:) !< W component.
  integer(I2P),    intent(in)   :: x(1:,1:,1:) !< X component.
  integer(I2P),    intent(in)   :: y(1:,1:,1:) !< Y component.
  integer(I2P),    intent(in)   :: z(1:,1:,1:) !< Z component.
  character(len=:), allocatable :: code        !< Encoded base64 dataarray.
  integer(I4P)                  :: n1          !< Counter.
  integer(I4P)                  :: n2          !< Counter.
  integer(I4P)                  :: n3          !< Counter.
  integer(I8P)                  :: l           !< Length.
  integer(I8P)                  :: sp          !< String pointer.
  integer(I4P)                  :: size_n1     !< Dimension 1 size.
  integer(I4P)                  :: size_n2     !< Dimension 2 size.
  integer(I4P)                  :: size_n3     !< Dimension 3 size.

  size_n1 = size(x, dim=1)
  size_n2 = size(x, dim=2)
  size_n3 = size(x, dim=3)

  l = DI2P*6 + 6
  sp = 0
  allocate(character(len=l*size_n1*size_n2*size_n3) :: code)
  code(:) = ''
  do n3=1, size_n3
    do n2=1, size_n2
      do n1=1, size_n1
        code(sp+1:sp+l) = str(n=u(n1, n2, n3))//' '//str(n=v(n1, n2, n3))//' '//str(n=w(n1, n2, n3))// &
          str(n=x(n1, n2, n3))//' '//str(n=y(n1, n2, n3))//' '//str(n=z(n1, n2, n3))
        sp = sp + l
      enddo
    enddo
  enddo
  endfunction encode_ascii_dataarray6_rank3_I2P

  function encode_ascii_dataarray6_rank3_I1P(u, v, w, x, y, z) result(code)
  !< Encode (Base64) a dataarray with 6 components of rank 3 (I1P).
  integer(I1P),    intent(in)   :: u(1:,1:,1:) !< U component.
  integer(I1P),    intent(in)   :: v(1:,1:,1:) !< V component.
  integer(I1P),    intent(in)   :: w(1:,1:,1:) !< W component.
  integer(I1P),    intent(in)   :: x(1:,1:,1:) !< X component.
  integer(I1P),    intent(in)   :: y(1:,1:,1:) !< Y component.
  integer(I1P),    intent(in)   :: z(1:,1:,1:) !< Z component.
  character(len=:), allocatable :: code        !< Encoded base64 dataarray.
  integer(I4P)                  :: n1          !< Counter.
  integer(I4P)                  :: n2          !< Counter.
  integer(I4P)                  :: n3          !< Counter.
  integer(I8P)                  :: l           !< Length.
  integer(I8P)                  :: sp          !< String pointer.
  integer(I4P)                  :: size_n1     !< Dimension 1 size.
  integer(I4P)                  :: size_n2     !< Dimension 2 size.
  integer(I4P)                  :: size_n3     !< Dimension 3 size.

  size_n1 = size(x, dim=1)
  size_n2 = size(x, dim=2)
  size_n3 = size(x, dim=3)

  l = DI1P*6 + 6
  sp = 0
  allocate(character(len=l*size_n1*size_n2*size_n3) :: code)
  code(:) = ''
  do n3=1, size_n3
    do n2=1, size_n2
      do n1=1, size_n1
        code(sp+1:sp+l) = str(n=u(n1, n2, n3))//' '//str(n=v(n1, n2, n3))//' '//str(n=w(n1, n2, n3))// &
          str(n=x(n1, n2, n3))//' '//str(n=y(n1, n2, n3))//' '//str(n=z(n1, n2, n3))
        sp = sp + l
      enddo
    enddo
  enddo
  endfunction encode_ascii_dataarray6_rank3_I1P

  !< binary encoder
  ! binary dataarray encoders
  !
  ! Data are flattened/interleaved into allocatable buffers by plain assignment before packing: array constructors and
  ! reshape results passed directly as actual arguments are temporaries that some compilers (e.g. ifx) place on the stack,
  ! overflowing it for large dataarrays (issue #70).
  function bytes_count(n_byte) result(header)
  !< Return the bytes count of a dataarray as its `I4P` (UInt32) header, checking that it fits.
  !<
  !< @note The execution is stopped if the bytes count overflows: larger dataarrays need a UInt64 header.
  integer(I8P), intent(in) :: n_byte !< Bytes count, computed in `I8P`.
  integer(I4P)             :: header !< Bytes count header.

  if (n_byte > int(huge(1_I4P), I8P)) then
    write(stderr, '(A)') 'error: VTKFortran dataarray of '//trim(str(n_byte, .true.))//' bytes exceeds the '// &
                         trim(str(huge(1_I4P), .true.))//' bytes limit of its UInt32 header: '// &
                         'initialize the file with header_type="UInt64"'
    error stop
  endif
  header = int(n_byte, I4P)
  endfunction bytes_count

  function encode_binary_dataarray1_rank1_R8P(x, is_uint64) result(code)
  !< Encode (Base64) a dataarray with 1 components of rank 1 (R8P).
  real(R8P), intent(in)         :: x(1:)      !< Data variable.
  logical, intent(in), optional :: is_uint64  !< Use a UInt64 bytes count header (default UInt32).
  character(len=:), allocatable :: code       !< Encoded base64 dataarray.
  integer(I8P)                  :: nn         !< Number of elements.
  logical                       :: is_uint64_ !< Use a UInt64 bytes count header, local variable.

  is_uint64_ = .false. ; if (present(is_uint64)) is_uint64_ = is_uint64
  nn = size(x, kind=I8P)
  code = encode_payload(n_byte=nn*BYR8P, x=x, is_uint64=is_uint64_)
  endfunction encode_binary_dataarray1_rank1_R8P

  function encode_binary_dataarray1_rank1_R4P(x, is_uint64) result(code)
  !< Encode (Base64) a dataarray with 1 components of rank 1 (R4P).
  real(R4P), intent(in)         :: x(1:)      !< Data variable.
  logical, intent(in), optional :: is_uint64  !< Use a UInt64 bytes count header (default UInt32).
  character(len=:), allocatable :: code       !< Encoded base64 dataarray.
  integer(I8P)                  :: nn         !< Number of elements.
  logical                       :: is_uint64_ !< Use a UInt64 bytes count header, local variable.

  is_uint64_ = .false. ; if (present(is_uint64)) is_uint64_ = is_uint64
  nn = size(x, kind=I8P)
  code = encode_payload(n_byte=nn*BYR4P, x=x, is_uint64=is_uint64_)
  endfunction encode_binary_dataarray1_rank1_R4P

  function encode_binary_dataarray1_rank1_I8P(x, is_uint64) result(code)
  !< Encode (Base64) a dataarray with 1 components of rank 1 (I8P).
  integer(I8P), intent(in)      :: x(1:)      !< Data variable.
  logical, intent(in), optional :: is_uint64  !< Use a UInt64 bytes count header (default UInt32).
  character(len=:), allocatable :: code       !< Encoded base64 dataarray.
  integer(I8P)                  :: nn         !< Number of elements.
  logical                       :: is_uint64_ !< Use a UInt64 bytes count header, local variable.

  is_uint64_ = .false. ; if (present(is_uint64)) is_uint64_ = is_uint64
  nn = size(x, kind=I8P)
  code = encode_payload(n_byte=nn*BYI8P, x=x, is_uint64=is_uint64_)
  endfunction encode_binary_dataarray1_rank1_I8P

  function encode_binary_dataarray1_rank1_I4P(x, is_uint64) result(code)
  !< Encode (Base64) a dataarray with 1 components of rank 1 (I4P).
  integer(I4P), intent(in)      :: x(1:)      !< Data variable.
  logical, intent(in), optional :: is_uint64  !< Use a UInt64 bytes count header (default UInt32).
  character(len=:), allocatable :: code       !< Encoded base64 dataarray.
  integer(I8P)                  :: nn         !< Number of elements.
  logical                       :: is_uint64_ !< Use a UInt64 bytes count header, local variable.

  is_uint64_ = .false. ; if (present(is_uint64)) is_uint64_ = is_uint64
  nn = size(x, kind=I8P)
  code = encode_payload(n_byte=nn*BYI4P, x=x, is_uint64=is_uint64_)
  endfunction encode_binary_dataarray1_rank1_I4P

  function encode_binary_dataarray1_rank1_I2P(x, is_uint64) result(code)
  !< Encode (Base64) a dataarray with 1 components of rank 1 (I2P).
  integer(I2P), intent(in)      :: x(1:)      !< Data variable.
  logical, intent(in), optional :: is_uint64  !< Use a UInt64 bytes count header (default UInt32).
  character(len=:), allocatable :: code       !< Encoded base64 dataarray.
  integer(I8P)                  :: nn         !< Number of elements.
  logical                       :: is_uint64_ !< Use a UInt64 bytes count header, local variable.

  is_uint64_ = .false. ; if (present(is_uint64)) is_uint64_ = is_uint64
  nn = size(x, kind=I8P)
  code = encode_payload(n_byte=nn*BYI2P, x=x, is_uint64=is_uint64_)
  endfunction encode_binary_dataarray1_rank1_I2P

  function encode_binary_dataarray1_rank1_I1P(x, is_uint64) result(code)
  !< Encode (Base64) a dataarray with 1 components of rank 1 (I1P).
  integer(I1P), intent(in)      :: x(1:)      !< Data variable.
  logical, intent(in), optional :: is_uint64  !< Use a UInt64 bytes count header (default UInt32).
  character(len=:), allocatable :: code       !< Encoded base64 dataarray.
  integer(I8P)                  :: nn         !< Number of elements.
  logical                       :: is_uint64_ !< Use a UInt64 bytes count header, local variable.

  is_uint64_ = .false. ; if (present(is_uint64)) is_uint64_ = is_uint64
  nn = size(x, kind=I8P)
  code = encode_payload(n_byte=nn*BYI1P, x=x, is_uint64=is_uint64_)
  endfunction encode_binary_dataarray1_rank1_I1P

  function encode_binary_dataarray1_rank2_R8P(x, is_uint64) result(code)
  !< Encode (Base64) a dataarray with 1 components of rank 2 (R8P).
  real(R8P), intent(in)         :: x(1:,1:)   !< Data variable.
  logical, intent(in), optional :: is_uint64  !< Use a UInt64 bytes count header (default UInt32).
  character(len=:), allocatable :: code       !< Encoded base64 dataarray.
  real(R8P),        allocatable :: buf(:)     !< Flattened data.
  integer(I8P)                  :: nn         !< Number of elements.
  logical                       :: is_uint64_ !< Use a UInt64 bytes count header, local variable.

  is_uint64_ = .false. ; if (present(is_uint64)) is_uint64_ = is_uint64
  nn = size(x, kind=I8P)
  buf = reshape(x, [nn])
  code = encode_payload(n_byte=nn*BYR8P, x=buf, is_uint64=is_uint64_)
  endfunction encode_binary_dataarray1_rank2_R8P

  function encode_binary_dataarray1_rank2_R4P(x, is_uint64) result(code)
  !< Encode (Base64) a dataarray with 1 components of rank 2 (R4P).
  real(R4P), intent(in)         :: x(1:,1:)   !< Data variable.
  logical, intent(in), optional :: is_uint64  !< Use a UInt64 bytes count header (default UInt32).
  character(len=:), allocatable :: code       !< Encoded base64 dataarray.
  real(R4P),        allocatable :: buf(:)     !< Flattened data.
  integer(I8P)                  :: nn         !< Number of elements.
  logical                       :: is_uint64_ !< Use a UInt64 bytes count header, local variable.

  is_uint64_ = .false. ; if (present(is_uint64)) is_uint64_ = is_uint64
  nn = size(x, kind=I8P)
  buf = reshape(x, [nn])
  code = encode_payload(n_byte=nn*BYR4P, x=buf, is_uint64=is_uint64_)
  endfunction encode_binary_dataarray1_rank2_R4P

  function encode_binary_dataarray1_rank2_I8P(x, is_uint64) result(code)
  !< Encode (Base64) a dataarray with 1 components of rank 2 (I8P).
  integer(I8P), intent(in)      :: x(1:,1:)   !< Data variable.
  logical, intent(in), optional :: is_uint64  !< Use a UInt64 bytes count header (default UInt32).
  character(len=:), allocatable :: code       !< Encoded base64 dataarray.
  integer(I8P),     allocatable :: buf(:)     !< Flattened data.
  integer(I8P)                  :: nn         !< Number of elements.
  logical                       :: is_uint64_ !< Use a UInt64 bytes count header, local variable.

  is_uint64_ = .false. ; if (present(is_uint64)) is_uint64_ = is_uint64
  nn = size(x, kind=I8P)
  buf = reshape(x, [nn])
  code = encode_payload(n_byte=nn*BYI8P, x=buf, is_uint64=is_uint64_)
  endfunction encode_binary_dataarray1_rank2_I8P

  function encode_binary_dataarray1_rank2_I4P(x, is_uint64) result(code)
  !< Encode (Base64) a dataarray with 1 components of rank 2 (I4P).
  integer(I4P), intent(in)      :: x(1:,1:)   !< Data variable.
  logical, intent(in), optional :: is_uint64  !< Use a UInt64 bytes count header (default UInt32).
  character(len=:), allocatable :: code       !< Encoded base64 dataarray.
  integer(I4P),     allocatable :: buf(:)     !< Flattened data.
  integer(I8P)                  :: nn         !< Number of elements.
  logical                       :: is_uint64_ !< Use a UInt64 bytes count header, local variable.

  is_uint64_ = .false. ; if (present(is_uint64)) is_uint64_ = is_uint64
  nn = size(x, kind=I8P)
  buf = reshape(x, [nn])
  code = encode_payload(n_byte=nn*BYI4P, x=buf, is_uint64=is_uint64_)
  endfunction encode_binary_dataarray1_rank2_I4P

  function encode_binary_dataarray1_rank2_I2P(x, is_uint64) result(code)
  !< Encode (Base64) a dataarray with 1 components of rank 2 (I2P).
  integer(I2P), intent(in)      :: x(1:,1:)   !< Data variable.
  logical, intent(in), optional :: is_uint64  !< Use a UInt64 bytes count header (default UInt32).
  character(len=:), allocatable :: code       !< Encoded base64 dataarray.
  integer(I2P),     allocatable :: buf(:)     !< Flattened data.
  integer(I8P)                  :: nn         !< Number of elements.
  logical                       :: is_uint64_ !< Use a UInt64 bytes count header, local variable.

  is_uint64_ = .false. ; if (present(is_uint64)) is_uint64_ = is_uint64
  nn = size(x, kind=I8P)
  buf = reshape(x, [nn])
  code = encode_payload(n_byte=nn*BYI2P, x=buf, is_uint64=is_uint64_)
  endfunction encode_binary_dataarray1_rank2_I2P

  function encode_binary_dataarray1_rank2_I1P(x, is_uint64) result(code)
  !< Encode (Base64) a dataarray with 1 components of rank 2 (I1P).
  integer(I1P), intent(in)      :: x(1:,1:)   !< Data variable.
  logical, intent(in), optional :: is_uint64  !< Use a UInt64 bytes count header (default UInt32).
  character(len=:), allocatable :: code       !< Encoded base64 dataarray.
  integer(I1P),     allocatable :: buf(:)     !< Flattened data.
  integer(I8P)                  :: nn         !< Number of elements.
  logical                       :: is_uint64_ !< Use a UInt64 bytes count header, local variable.

  is_uint64_ = .false. ; if (present(is_uint64)) is_uint64_ = is_uint64
  nn = size(x, kind=I8P)
  buf = reshape(x, [nn])
  code = encode_payload(n_byte=nn*BYI1P, x=buf, is_uint64=is_uint64_)
  endfunction encode_binary_dataarray1_rank2_I1P

  function encode_binary_dataarray1_rank3_R8P(x, is_uint64) result(code)
  !< Encode (Base64) a dataarray with 1 components of rank 3 (R8P).
  real(R8P), intent(in)         :: x(1:,1:,1:) !< Data variable.
  logical, intent(in), optional :: is_uint64   !< Use a UInt64 bytes count header (default UInt32).
  character(len=:), allocatable :: code        !< Encoded base64 dataarray.
  real(R8P),        allocatable :: buf(:)      !< Flattened data.
  integer(I8P)                  :: nn          !< Number of elements.
  logical                       :: is_uint64_  !< Use a UInt64 bytes count header, local variable.

  is_uint64_ = .false. ; if (present(is_uint64)) is_uint64_ = is_uint64
  nn = size(x, kind=I8P)
  buf = reshape(x, [nn])
  code = encode_payload(n_byte=nn*BYR8P, x=buf, is_uint64=is_uint64_)
  endfunction encode_binary_dataarray1_rank3_R8P

  function encode_binary_dataarray1_rank3_R4P(x, is_uint64) result(code)
  !< Encode (Base64) a dataarray with 1 components of rank 3 (R4P).
  real(R4P), intent(in)         :: x(1:,1:,1:) !< Data variable.
  logical, intent(in), optional :: is_uint64   !< Use a UInt64 bytes count header (default UInt32).
  character(len=:), allocatable :: code        !< Encoded base64 dataarray.
  real(R4P),        allocatable :: buf(:)      !< Flattened data.
  integer(I8P)                  :: nn          !< Number of elements.
  logical                       :: is_uint64_  !< Use a UInt64 bytes count header, local variable.

  is_uint64_ = .false. ; if (present(is_uint64)) is_uint64_ = is_uint64
  nn = size(x, kind=I8P)
  buf = reshape(x, [nn])
  code = encode_payload(n_byte=nn*BYR4P, x=buf, is_uint64=is_uint64_)
  endfunction encode_binary_dataarray1_rank3_R4P

  function encode_binary_dataarray1_rank3_I8P(x, is_uint64) result(code)
  !< Encode (Base64) a dataarray with 1 components of rank 3 (I8P).
  integer(I8P), intent(in)      :: x(1:,1:,1:) !< Data variable.
  logical, intent(in), optional :: is_uint64   !< Use a UInt64 bytes count header (default UInt32).
  character(len=:), allocatable :: code        !< Encoded base64 dataarray.
  integer(I8P),     allocatable :: buf(:)      !< Flattened data.
  integer(I8P)                  :: nn          !< Number of elements.
  logical                       :: is_uint64_  !< Use a UInt64 bytes count header, local variable.

  is_uint64_ = .false. ; if (present(is_uint64)) is_uint64_ = is_uint64
  nn = size(x, kind=I8P)
  buf = reshape(x, [nn])
  code = encode_payload(n_byte=nn*BYI8P, x=buf, is_uint64=is_uint64_)
  endfunction encode_binary_dataarray1_rank3_I8P

  function encode_binary_dataarray1_rank3_I4P(x, is_uint64) result(code)
  !< Encode (Base64) a dataarray with 1 components of rank 3 (I4P).
  integer(I4P), intent(in)      :: x(1:,1:,1:) !< Data variable.
  logical, intent(in), optional :: is_uint64   !< Use a UInt64 bytes count header (default UInt32).
  character(len=:), allocatable :: code        !< Encoded base64 dataarray.
  integer(I4P),     allocatable :: buf(:)      !< Flattened data.
  integer(I8P)                  :: nn          !< Number of elements.
  logical                       :: is_uint64_  !< Use a UInt64 bytes count header, local variable.

  is_uint64_ = .false. ; if (present(is_uint64)) is_uint64_ = is_uint64
  nn = size(x, kind=I8P)
  buf = reshape(x, [nn])
  code = encode_payload(n_byte=nn*BYI4P, x=buf, is_uint64=is_uint64_)
  endfunction encode_binary_dataarray1_rank3_I4P

  function encode_binary_dataarray1_rank3_I2P(x, is_uint64) result(code)
  !< Encode (Base64) a dataarray with 1 components of rank 3 (I2P).
  integer(I2P), intent(in)      :: x(1:,1:,1:) !< Data variable.
  logical, intent(in), optional :: is_uint64   !< Use a UInt64 bytes count header (default UInt32).
  character(len=:), allocatable :: code        !< Encoded base64 dataarray.
  integer(I2P),     allocatable :: buf(:)      !< Flattened data.
  integer(I8P)                  :: nn          !< Number of elements.
  logical                       :: is_uint64_  !< Use a UInt64 bytes count header, local variable.

  is_uint64_ = .false. ; if (present(is_uint64)) is_uint64_ = is_uint64
  nn = size(x, kind=I8P)
  buf = reshape(x, [nn])
  code = encode_payload(n_byte=nn*BYI2P, x=buf, is_uint64=is_uint64_)
  endfunction encode_binary_dataarray1_rank3_I2P

  function encode_binary_dataarray1_rank3_I1P(x, is_uint64) result(code)
  !< Encode (Base64) a dataarray with 1 components of rank 3 (I1P).
  integer(I1P), intent(in)      :: x(1:,1:,1:) !< Data variable.
  logical, intent(in), optional :: is_uint64   !< Use a UInt64 bytes count header (default UInt32).
  character(len=:), allocatable :: code        !< Encoded base64 dataarray.
  integer(I1P),     allocatable :: buf(:)      !< Flattened data.
  integer(I8P)                  :: nn          !< Number of elements.
  logical                       :: is_uint64_  !< Use a UInt64 bytes count header, local variable.

  is_uint64_ = .false. ; if (present(is_uint64)) is_uint64_ = is_uint64
  nn = size(x, kind=I8P)
  buf = reshape(x, [nn])
  code = encode_payload(n_byte=nn*BYI1P, x=buf, is_uint64=is_uint64_)
  endfunction encode_binary_dataarray1_rank3_I1P

  function encode_binary_dataarray1_rank4_R8P(x, is_uint64) result(code)
  !< Encode (Base64) a dataarray with 1 components of rank 4 (R8P).
  real(R8P), intent(in)         :: x(1:,1:,1:,1:) !< Data variable.
  logical, intent(in), optional :: is_uint64      !< Use a UInt64 bytes count header (default UInt32).
  character(len=:), allocatable :: code           !< Encoded base64 dataarray.
  real(R8P),        allocatable :: buf(:)         !< Flattened data.
  integer(I8P)                  :: nn             !< Number of elements.
  logical                       :: is_uint64_     !< Use a UInt64 bytes count header, local variable.

  is_uint64_ = .false. ; if (present(is_uint64)) is_uint64_ = is_uint64
  nn = size(x, kind=I8P)
  buf = reshape(x, [nn])
  code = encode_payload(n_byte=nn*BYR8P, x=buf, is_uint64=is_uint64_)
  endfunction encode_binary_dataarray1_rank4_R8P

  function encode_binary_dataarray1_rank4_R4P(x, is_uint64) result(code)
  !< Encode (Base64) a dataarray with 1 components of rank 4 (R4P).
  real(R4P), intent(in)         :: x(1:,1:,1:,1:) !< Data variable.
  logical, intent(in), optional :: is_uint64      !< Use a UInt64 bytes count header (default UInt32).
  character(len=:), allocatable :: code           !< Encoded base64 dataarray.
  real(R4P),        allocatable :: buf(:)         !< Flattened data.
  integer(I8P)                  :: nn             !< Number of elements.
  logical                       :: is_uint64_     !< Use a UInt64 bytes count header, local variable.

  is_uint64_ = .false. ; if (present(is_uint64)) is_uint64_ = is_uint64
  nn = size(x, kind=I8P)
  buf = reshape(x, [nn])
  code = encode_payload(n_byte=nn*BYR4P, x=buf, is_uint64=is_uint64_)
  endfunction encode_binary_dataarray1_rank4_R4P

  function encode_binary_dataarray1_rank4_I8P(x, is_uint64) result(code)
  !< Encode (Base64) a dataarray with 1 components of rank 4 (I8P).
  integer(I8P), intent(in)      :: x(1:,1:,1:,1:) !< Data variable.
  logical, intent(in), optional :: is_uint64      !< Use a UInt64 bytes count header (default UInt32).
  character(len=:), allocatable :: code           !< Encoded base64 dataarray.
  integer(I8P),     allocatable :: buf(:)         !< Flattened data.
  integer(I8P)                  :: nn             !< Number of elements.
  logical                       :: is_uint64_     !< Use a UInt64 bytes count header, local variable.

  is_uint64_ = .false. ; if (present(is_uint64)) is_uint64_ = is_uint64
  nn = size(x, kind=I8P)
  buf = reshape(x, [nn])
  code = encode_payload(n_byte=nn*BYI8P, x=buf, is_uint64=is_uint64_)
  endfunction encode_binary_dataarray1_rank4_I8P

  function encode_binary_dataarray1_rank4_I4P(x, is_uint64) result(code)
  !< Encode (Base64) a dataarray with 1 components of rank 4 (I4P).
  integer(I4P), intent(in)      :: x(1:,1:,1:,1:) !< Data variable.
  logical, intent(in), optional :: is_uint64      !< Use a UInt64 bytes count header (default UInt32).
  character(len=:), allocatable :: code           !< Encoded base64 dataarray.
  integer(I4P),     allocatable :: buf(:)         !< Flattened data.
  integer(I8P)                  :: nn             !< Number of elements.
  logical                       :: is_uint64_     !< Use a UInt64 bytes count header, local variable.

  is_uint64_ = .false. ; if (present(is_uint64)) is_uint64_ = is_uint64
  nn = size(x, kind=I8P)
  buf = reshape(x, [nn])
  code = encode_payload(n_byte=nn*BYI4P, x=buf, is_uint64=is_uint64_)
  endfunction encode_binary_dataarray1_rank4_I4P

  function encode_binary_dataarray1_rank4_I2P(x, is_uint64) result(code)
  !< Encode (Base64) a dataarray with 1 components of rank 4 (I2P).
  integer(I2P), intent(in)      :: x(1:,1:,1:,1:) !< Data variable.
  logical, intent(in), optional :: is_uint64      !< Use a UInt64 bytes count header (default UInt32).
  character(len=:), allocatable :: code           !< Encoded base64 dataarray.
  integer(I2P),     allocatable :: buf(:)         !< Flattened data.
  integer(I8P)                  :: nn             !< Number of elements.
  logical                       :: is_uint64_     !< Use a UInt64 bytes count header, local variable.

  is_uint64_ = .false. ; if (present(is_uint64)) is_uint64_ = is_uint64
  nn = size(x, kind=I8P)
  buf = reshape(x, [nn])
  code = encode_payload(n_byte=nn*BYI2P, x=buf, is_uint64=is_uint64_)
  endfunction encode_binary_dataarray1_rank4_I2P

  function encode_binary_dataarray1_rank4_I1P(x, is_uint64) result(code)
  !< Encode (Base64) a dataarray with 1 components of rank 4 (I1P).
  integer(I1P), intent(in)      :: x(1:,1:,1:,1:) !< Data variable.
  logical, intent(in), optional :: is_uint64      !< Use a UInt64 bytes count header (default UInt32).
  character(len=:), allocatable :: code           !< Encoded base64 dataarray.
  integer(I1P),     allocatable :: buf(:)         !< Flattened data.
  integer(I8P)                  :: nn             !< Number of elements.
  logical                       :: is_uint64_     !< Use a UInt64 bytes count header, local variable.

  is_uint64_ = .false. ; if (present(is_uint64)) is_uint64_ = is_uint64
  nn = size(x, kind=I8P)
  buf = reshape(x, [nn])
  code = encode_payload(n_byte=nn*BYI1P, x=buf, is_uint64=is_uint64_)
  endfunction encode_binary_dataarray1_rank4_I1P

  function encode_binary_dataarray3_rank1_R8P(x, y, z, is_uint64) result(code)
  !< Encode (Base64) a dataarray with 3 components of rank 1 (R8P).
  real(R8P), intent(in)         :: x(1:)      !< X component.
  real(R8P), intent(in)         :: y(1:)      !< Y component.
  real(R8P), intent(in)         :: z(1:)      !< Z component.
  logical, intent(in), optional :: is_uint64  !< Use a UInt64 bytes count header (default UInt32).
  character(len=:), allocatable :: code       !< Encoded base64 dataarray.
  real(R8P),        allocatable :: buf(:)     !< Interleaved components.
  integer(I8P)                  :: nn         !< Number of elements.
  logical                       :: is_uint64_ !< Use a UInt64 bytes count header, local variable.

  is_uint64_ = .false. ; if (present(is_uint64)) is_uint64_ = is_uint64
  nn = size(x, kind=I8P)
  allocate(buf(1:3*nn))
  buf(1::3) = x
  buf(2::3) = y
  buf(3::3) = z
  code = encode_payload(n_byte=3*nn*BYR8P, x=buf, is_uint64=is_uint64_)
  endfunction encode_binary_dataarray3_rank1_R8P

  function encode_binary_dataarray3_rank1_R4P(x, y, z, is_uint64) result(code)
  !< Encode (Base64) a dataarray with 3 components of rank 1 (R4P).
  real(R4P), intent(in)         :: x(1:)      !< X component.
  real(R4P), intent(in)         :: y(1:)      !< Y component.
  real(R4P), intent(in)         :: z(1:)      !< Z component.
  logical, intent(in), optional :: is_uint64  !< Use a UInt64 bytes count header (default UInt32).
  character(len=:), allocatable :: code       !< Encoded base64 dataarray.
  real(R4P),        allocatable :: buf(:)     !< Interleaved components.
  integer(I8P)                  :: nn         !< Number of elements.
  logical                       :: is_uint64_ !< Use a UInt64 bytes count header, local variable.

  is_uint64_ = .false. ; if (present(is_uint64)) is_uint64_ = is_uint64
  nn = size(x, kind=I8P)
  allocate(buf(1:3*nn))
  buf(1::3) = x
  buf(2::3) = y
  buf(3::3) = z
  code = encode_payload(n_byte=3*nn*BYR4P, x=buf, is_uint64=is_uint64_)
  endfunction encode_binary_dataarray3_rank1_R4P

  function encode_binary_dataarray3_rank1_I8P(x, y, z, is_uint64) result(code)
  !< Encode (Base64) a dataarray with 3 components of rank 1 (I8P).
  integer(I8P), intent(in)      :: x(1:)      !< X component.
  integer(I8P), intent(in)      :: y(1:)      !< Y component.
  integer(I8P), intent(in)      :: z(1:)      !< Z component.
  logical, intent(in), optional :: is_uint64  !< Use a UInt64 bytes count header (default UInt32).
  character(len=:), allocatable :: code       !< Encoded base64 dataarray.
  integer(I8P),     allocatable :: buf(:)     !< Interleaved components.
  integer(I8P)                  :: nn         !< Number of elements.
  logical                       :: is_uint64_ !< Use a UInt64 bytes count header, local variable.

  is_uint64_ = .false. ; if (present(is_uint64)) is_uint64_ = is_uint64
  nn = size(x, kind=I8P)
  allocate(buf(1:3*nn))
  buf(1::3) = x
  buf(2::3) = y
  buf(3::3) = z
  code = encode_payload(n_byte=3*nn*BYI8P, x=buf, is_uint64=is_uint64_)
  endfunction encode_binary_dataarray3_rank1_I8P

  function encode_binary_dataarray3_rank1_I4P(x, y, z, is_uint64) result(code)
  !< Encode (Base64) a dataarray with 3 components of rank 1 (I4P).
  integer(I4P), intent(in)      :: x(1:)      !< X component.
  integer(I4P), intent(in)      :: y(1:)      !< Y component.
  integer(I4P), intent(in)      :: z(1:)      !< Z component.
  logical, intent(in), optional :: is_uint64  !< Use a UInt64 bytes count header (default UInt32).
  character(len=:), allocatable :: code       !< Encoded base64 dataarray.
  integer(I4P),     allocatable :: buf(:)     !< Interleaved components.
  integer(I8P)                  :: nn         !< Number of elements.
  logical                       :: is_uint64_ !< Use a UInt64 bytes count header, local variable.

  is_uint64_ = .false. ; if (present(is_uint64)) is_uint64_ = is_uint64
  nn = size(x, kind=I8P)
  allocate(buf(1:3*nn))
  buf(1::3) = x
  buf(2::3) = y
  buf(3::3) = z
  code = encode_payload(n_byte=3*nn*BYI4P, x=buf, is_uint64=is_uint64_)
  endfunction encode_binary_dataarray3_rank1_I4P

  function encode_binary_dataarray3_rank1_I2P(x, y, z, is_uint64) result(code)
  !< Encode (Base64) a dataarray with 3 components of rank 1 (I2P).
  integer(I2P), intent(in)      :: x(1:)      !< X component.
  integer(I2P), intent(in)      :: y(1:)      !< Y component.
  integer(I2P), intent(in)      :: z(1:)      !< Z component.
  logical, intent(in), optional :: is_uint64  !< Use a UInt64 bytes count header (default UInt32).
  character(len=:), allocatable :: code       !< Encoded base64 dataarray.
  integer(I2P),     allocatable :: buf(:)     !< Interleaved components.
  integer(I8P)                  :: nn         !< Number of elements.
  logical                       :: is_uint64_ !< Use a UInt64 bytes count header, local variable.

  is_uint64_ = .false. ; if (present(is_uint64)) is_uint64_ = is_uint64
  nn = size(x, kind=I8P)
  allocate(buf(1:3*nn))
  buf(1::3) = x
  buf(2::3) = y
  buf(3::3) = z
  code = encode_payload(n_byte=3*nn*BYI2P, x=buf, is_uint64=is_uint64_)
  endfunction encode_binary_dataarray3_rank1_I2P

  function encode_binary_dataarray3_rank1_I1P(x, y, z, is_uint64) result(code)
  !< Encode (Base64) a dataarray with 3 components of rank 1 (I1P).
  integer(I1P), intent(in)      :: x(1:)      !< X component.
  integer(I1P), intent(in)      :: y(1:)      !< Y component.
  integer(I1P), intent(in)      :: z(1:)      !< Z component.
  logical, intent(in), optional :: is_uint64  !< Use a UInt64 bytes count header (default UInt32).
  character(len=:), allocatable :: code       !< Encoded base64 dataarray.
  integer(I1P),     allocatable :: buf(:)     !< Interleaved components.
  integer(I8P)                  :: nn         !< Number of elements.
  logical                       :: is_uint64_ !< Use a UInt64 bytes count header, local variable.

  is_uint64_ = .false. ; if (present(is_uint64)) is_uint64_ = is_uint64
  nn = size(x, kind=I8P)
  allocate(buf(1:3*nn))
  buf(1::3) = x
  buf(2::3) = y
  buf(3::3) = z
  code = encode_payload(n_byte=3*nn*BYI1P, x=buf, is_uint64=is_uint64_)
  endfunction encode_binary_dataarray3_rank1_I1P

  function encode_binary_dataarray3_rank3_R8P(x, y, z, is_uint64) result(code)
  !< Encode (Base64) a dataarray with 3 components of rank 3 (R8P).
  real(R8P), intent(in)         :: x(1:,1:,1:) !< X component.
  real(R8P), intent(in)         :: y(1:,1:,1:) !< Y component.
  real(R8P), intent(in)         :: z(1:,1:,1:) !< Z component.
  logical, intent(in), optional :: is_uint64   !< Use a UInt64 bytes count header (default UInt32).
  character(len=:), allocatable :: code        !< Encoded base64 dataarray.
  real(R8P),        allocatable :: buf(:)      !< Interleaved components.
  integer(I8P)                  :: nn          !< Number of elements.
  logical                       :: is_uint64_  !< Use a UInt64 bytes count header, local variable.

  is_uint64_ = .false. ; if (present(is_uint64)) is_uint64_ = is_uint64
  nn = size(x, kind=I8P)
  allocate(buf(1:3*nn))
  buf(1::3) = reshape(x, [nn])
  buf(2::3) = reshape(y, [nn])
  buf(3::3) = reshape(z, [nn])
  code = encode_payload(n_byte=3*nn*BYR8P, x=buf, is_uint64=is_uint64_)
  endfunction encode_binary_dataarray3_rank3_R8P

  function encode_binary_dataarray3_rank3_R4P(x, y, z, is_uint64) result(code)
  !< Encode (Base64) a dataarray with 3 components of rank 3 (R4P).
  real(R4P), intent(in)         :: x(1:,1:,1:) !< X component.
  real(R4P), intent(in)         :: y(1:,1:,1:) !< Y component.
  real(R4P), intent(in)         :: z(1:,1:,1:) !< Z component.
  logical, intent(in), optional :: is_uint64   !< Use a UInt64 bytes count header (default UInt32).
  character(len=:), allocatable :: code        !< Encoded base64 dataarray.
  real(R4P),        allocatable :: buf(:)      !< Interleaved components.
  integer(I8P)                  :: nn          !< Number of elements.
  logical                       :: is_uint64_  !< Use a UInt64 bytes count header, local variable.

  is_uint64_ = .false. ; if (present(is_uint64)) is_uint64_ = is_uint64
  nn = size(x, kind=I8P)
  allocate(buf(1:3*nn))
  buf(1::3) = reshape(x, [nn])
  buf(2::3) = reshape(y, [nn])
  buf(3::3) = reshape(z, [nn])
  code = encode_payload(n_byte=3*nn*BYR4P, x=buf, is_uint64=is_uint64_)
  endfunction encode_binary_dataarray3_rank3_R4P

  function encode_binary_dataarray3_rank3_I8P(x, y, z, is_uint64) result(code)
  !< Encode (Base64) a dataarray with 3 components of rank 3 (I8P).
  integer(I8P), intent(in)      :: x(1:,1:,1:) !< X component.
  integer(I8P), intent(in)      :: y(1:,1:,1:) !< Y component.
  integer(I8P), intent(in)      :: z(1:,1:,1:) !< Z component.
  logical, intent(in), optional :: is_uint64   !< Use a UInt64 bytes count header (default UInt32).
  character(len=:), allocatable :: code        !< Encoded base64 dataarray.
  integer(I8P),     allocatable :: buf(:)      !< Interleaved components.
  integer(I8P)                  :: nn          !< Number of elements.
  logical                       :: is_uint64_  !< Use a UInt64 bytes count header, local variable.

  is_uint64_ = .false. ; if (present(is_uint64)) is_uint64_ = is_uint64
  nn = size(x, kind=I8P)
  allocate(buf(1:3*nn))
  buf(1::3) = reshape(x, [nn])
  buf(2::3) = reshape(y, [nn])
  buf(3::3) = reshape(z, [nn])
  code = encode_payload(n_byte=3*nn*BYI8P, x=buf, is_uint64=is_uint64_)
  endfunction encode_binary_dataarray3_rank3_I8P

  function encode_binary_dataarray3_rank3_I4P(x, y, z, is_uint64) result(code)
  !< Encode (Base64) a dataarray with 3 components of rank 3 (I4P).
  integer(I4P), intent(in)      :: x(1:,1:,1:) !< X component.
  integer(I4P), intent(in)      :: y(1:,1:,1:) !< Y component.
  integer(I4P), intent(in)      :: z(1:,1:,1:) !< Z component.
  logical, intent(in), optional :: is_uint64   !< Use a UInt64 bytes count header (default UInt32).
  character(len=:), allocatable :: code        !< Encoded base64 dataarray.
  integer(I4P),     allocatable :: buf(:)      !< Interleaved components.
  integer(I8P)                  :: nn          !< Number of elements.
  logical                       :: is_uint64_  !< Use a UInt64 bytes count header, local variable.

  is_uint64_ = .false. ; if (present(is_uint64)) is_uint64_ = is_uint64
  nn = size(x, kind=I8P)
  allocate(buf(1:3*nn))
  buf(1::3) = reshape(x, [nn])
  buf(2::3) = reshape(y, [nn])
  buf(3::3) = reshape(z, [nn])
  code = encode_payload(n_byte=3*nn*BYI4P, x=buf, is_uint64=is_uint64_)
  endfunction encode_binary_dataarray3_rank3_I4P

  function encode_binary_dataarray3_rank3_I2P(x, y, z, is_uint64) result(code)
  !< Encode (Base64) a dataarray with 3 components of rank 3 (I2P).
  integer(I2P), intent(in)      :: x(1:,1:,1:) !< X component.
  integer(I2P), intent(in)      :: y(1:,1:,1:) !< Y component.
  integer(I2P), intent(in)      :: z(1:,1:,1:) !< Z component.
  logical, intent(in), optional :: is_uint64   !< Use a UInt64 bytes count header (default UInt32).
  character(len=:), allocatable :: code        !< Encoded base64 dataarray.
  integer(I2P),     allocatable :: buf(:)      !< Interleaved components.
  integer(I8P)                  :: nn          !< Number of elements.
  logical                       :: is_uint64_  !< Use a UInt64 bytes count header, local variable.

  is_uint64_ = .false. ; if (present(is_uint64)) is_uint64_ = is_uint64
  nn = size(x, kind=I8P)
  allocate(buf(1:3*nn))
  buf(1::3) = reshape(x, [nn])
  buf(2::3) = reshape(y, [nn])
  buf(3::3) = reshape(z, [nn])
  code = encode_payload(n_byte=3*nn*BYI2P, x=buf, is_uint64=is_uint64_)
  endfunction encode_binary_dataarray3_rank3_I2P

  function encode_binary_dataarray3_rank3_I1P(x, y, z, is_uint64) result(code)
  !< Encode (Base64) a dataarray with 3 components of rank 3 (I1P).
  integer(I1P), intent(in)      :: x(1:,1:,1:) !< X component.
  integer(I1P), intent(in)      :: y(1:,1:,1:) !< Y component.
  integer(I1P), intent(in)      :: z(1:,1:,1:) !< Z component.
  logical, intent(in), optional :: is_uint64   !< Use a UInt64 bytes count header (default UInt32).
  character(len=:), allocatable :: code        !< Encoded base64 dataarray.
  integer(I1P),     allocatable :: buf(:)      !< Interleaved components.
  integer(I8P)                  :: nn          !< Number of elements.
  logical                       :: is_uint64_  !< Use a UInt64 bytes count header, local variable.

  is_uint64_ = .false. ; if (present(is_uint64)) is_uint64_ = is_uint64
  nn = size(x, kind=I8P)
  allocate(buf(1:3*nn))
  buf(1::3) = reshape(x, [nn])
  buf(2::3) = reshape(y, [nn])
  buf(3::3) = reshape(z, [nn])
  code = encode_payload(n_byte=3*nn*BYI1P, x=buf, is_uint64=is_uint64_)
  endfunction encode_binary_dataarray3_rank3_I1P

  function encode_binary_dataarray6_rank1_R8P(u, v, w, x, y, z, is_uint64) result(code)
  !< Encode (Base64) a dataarray with 6 components of rank 1 (R8P).
  real(R8P), intent(in)         :: u(1:)      !< U component.
  real(R8P), intent(in)         :: v(1:)      !< V component.
  real(R8P), intent(in)         :: w(1:)      !< W component.
  real(R8P), intent(in)         :: x(1:)      !< X component.
  real(R8P), intent(in)         :: y(1:)      !< Y component.
  real(R8P), intent(in)         :: z(1:)      !< Z component.
  logical, intent(in), optional :: is_uint64  !< Use a UInt64 bytes count header (default UInt32).
  character(len=:), allocatable :: code       !< Encoded base64 dataarray.
  real(R8P),        allocatable :: buf(:)     !< Interleaved components.
  integer(I8P)                  :: nn         !< Number of elements.
  logical                       :: is_uint64_ !< Use a UInt64 bytes count header, local variable.

  is_uint64_ = .false. ; if (present(is_uint64)) is_uint64_ = is_uint64
  nn = size(x, kind=I8P)
  allocate(buf(1:6*nn))
  buf(1::6) = u
  buf(2::6) = v
  buf(3::6) = w
  buf(4::6) = x
  buf(5::6) = y
  buf(6::6) = z
  code = encode_payload(n_byte=6*nn*BYR8P, x=buf, is_uint64=is_uint64_)
  endfunction encode_binary_dataarray6_rank1_R8P

  function encode_binary_dataarray6_rank1_R4P(u, v, w, x, y, z, is_uint64) result(code)
  !< Encode (Base64) a dataarray with 6 components of rank 1 (R4P).
  real(R4P), intent(in)         :: u(1:)      !< U component.
  real(R4P), intent(in)         :: v(1:)      !< V component.
  real(R4P), intent(in)         :: w(1:)      !< W component.
  real(R4P), intent(in)         :: x(1:)      !< X component.
  real(R4P), intent(in)         :: y(1:)      !< Y component.
  real(R4P), intent(in)         :: z(1:)      !< Z component.
  logical, intent(in), optional :: is_uint64  !< Use a UInt64 bytes count header (default UInt32).
  character(len=:), allocatable :: code       !< Encoded base64 dataarray.
  real(R4P),        allocatable :: buf(:)     !< Interleaved components.
  integer(I8P)                  :: nn         !< Number of elements.
  logical                       :: is_uint64_ !< Use a UInt64 bytes count header, local variable.

  is_uint64_ = .false. ; if (present(is_uint64)) is_uint64_ = is_uint64
  nn = size(x, kind=I8P)
  allocate(buf(1:6*nn))
  buf(1::6) = u
  buf(2::6) = v
  buf(3::6) = w
  buf(4::6) = x
  buf(5::6) = y
  buf(6::6) = z
  code = encode_payload(n_byte=6*nn*BYR4P, x=buf, is_uint64=is_uint64_)
  endfunction encode_binary_dataarray6_rank1_R4P

  function encode_binary_dataarray6_rank1_I8P(u, v, w, x, y, z, is_uint64) result(code)
  !< Encode (Base64) a dataarray with 6 components of rank 1 (I8P).
  integer(I8P), intent(in)      :: u(1:)      !< U component.
  integer(I8P), intent(in)      :: v(1:)      !< V component.
  integer(I8P), intent(in)      :: w(1:)      !< W component.
  integer(I8P), intent(in)      :: x(1:)      !< X component.
  integer(I8P), intent(in)      :: y(1:)      !< Y component.
  integer(I8P), intent(in)      :: z(1:)      !< Z component.
  logical, intent(in), optional :: is_uint64  !< Use a UInt64 bytes count header (default UInt32).
  character(len=:), allocatable :: code       !< Encoded base64 dataarray.
  integer(I8P),     allocatable :: buf(:)     !< Interleaved components.
  integer(I8P)                  :: nn         !< Number of elements.
  logical                       :: is_uint64_ !< Use a UInt64 bytes count header, local variable.

  is_uint64_ = .false. ; if (present(is_uint64)) is_uint64_ = is_uint64
  nn = size(x, kind=I8P)
  allocate(buf(1:6*nn))
  buf(1::6) = u
  buf(2::6) = v
  buf(3::6) = w
  buf(4::6) = x
  buf(5::6) = y
  buf(6::6) = z
  code = encode_payload(n_byte=6*nn*BYI8P, x=buf, is_uint64=is_uint64_)
  endfunction encode_binary_dataarray6_rank1_I8P

  function encode_binary_dataarray6_rank1_I4P(u, v, w, x, y, z, is_uint64) result(code)
  !< Encode (Base64) a dataarray with 6 components of rank 1 (I4P).
  integer(I4P), intent(in)      :: u(1:)      !< U component.
  integer(I4P), intent(in)      :: v(1:)      !< V component.
  integer(I4P), intent(in)      :: w(1:)      !< W component.
  integer(I4P), intent(in)      :: x(1:)      !< X component.
  integer(I4P), intent(in)      :: y(1:)      !< Y component.
  integer(I4P), intent(in)      :: z(1:)      !< Z component.
  logical, intent(in), optional :: is_uint64  !< Use a UInt64 bytes count header (default UInt32).
  character(len=:), allocatable :: code       !< Encoded base64 dataarray.
  integer(I4P),     allocatable :: buf(:)     !< Interleaved components.
  integer(I8P)                  :: nn         !< Number of elements.
  logical                       :: is_uint64_ !< Use a UInt64 bytes count header, local variable.

  is_uint64_ = .false. ; if (present(is_uint64)) is_uint64_ = is_uint64
  nn = size(x, kind=I8P)
  allocate(buf(1:6*nn))
  buf(1::6) = u
  buf(2::6) = v
  buf(3::6) = w
  buf(4::6) = x
  buf(5::6) = y
  buf(6::6) = z
  code = encode_payload(n_byte=6*nn*BYI4P, x=buf, is_uint64=is_uint64_)
  endfunction encode_binary_dataarray6_rank1_I4P

  function encode_binary_dataarray6_rank1_I2P(u, v, w, x, y, z, is_uint64) result(code)
  !< Encode (Base64) a dataarray with 6 components of rank 1 (I2P).
  integer(I2P), intent(in)      :: u(1:)      !< U component.
  integer(I2P), intent(in)      :: v(1:)      !< V component.
  integer(I2P), intent(in)      :: w(1:)      !< W component.
  integer(I2P), intent(in)      :: x(1:)      !< X component.
  integer(I2P), intent(in)      :: y(1:)      !< Y component.
  integer(I2P), intent(in)      :: z(1:)      !< Z component.
  logical, intent(in), optional :: is_uint64  !< Use a UInt64 bytes count header (default UInt32).
  character(len=:), allocatable :: code       !< Encoded base64 dataarray.
  integer(I2P),     allocatable :: buf(:)     !< Interleaved components.
  integer(I8P)                  :: nn         !< Number of elements.
  logical                       :: is_uint64_ !< Use a UInt64 bytes count header, local variable.

  is_uint64_ = .false. ; if (present(is_uint64)) is_uint64_ = is_uint64
  nn = size(x, kind=I8P)
  allocate(buf(1:6*nn))
  buf(1::6) = u
  buf(2::6) = v
  buf(3::6) = w
  buf(4::6) = x
  buf(5::6) = y
  buf(6::6) = z
  code = encode_payload(n_byte=6*nn*BYI2P, x=buf, is_uint64=is_uint64_)
  endfunction encode_binary_dataarray6_rank1_I2P

  function encode_binary_dataarray6_rank1_I1P(u, v, w, x, y, z, is_uint64) result(code)
  !< Encode (Base64) a dataarray with 6 components of rank 1 (I1P).
  integer(I1P), intent(in)      :: u(1:)      !< U component.
  integer(I1P), intent(in)      :: v(1:)      !< V component.
  integer(I1P), intent(in)      :: w(1:)      !< W component.
  integer(I1P), intent(in)      :: x(1:)      !< X component.
  integer(I1P), intent(in)      :: y(1:)      !< Y component.
  integer(I1P), intent(in)      :: z(1:)      !< Z component.
  logical, intent(in), optional :: is_uint64  !< Use a UInt64 bytes count header (default UInt32).
  character(len=:), allocatable :: code       !< Encoded base64 dataarray.
  integer(I1P),     allocatable :: buf(:)     !< Interleaved components.
  integer(I8P)                  :: nn         !< Number of elements.
  logical                       :: is_uint64_ !< Use a UInt64 bytes count header, local variable.

  is_uint64_ = .false. ; if (present(is_uint64)) is_uint64_ = is_uint64
  nn = size(x, kind=I8P)
  allocate(buf(1:6*nn))
  buf(1::6) = u
  buf(2::6) = v
  buf(3::6) = w
  buf(4::6) = x
  buf(5::6) = y
  buf(6::6) = z
  code = encode_payload(n_byte=6*nn*BYI1P, x=buf, is_uint64=is_uint64_)
  endfunction encode_binary_dataarray6_rank1_I1P

  function encode_binary_dataarray6_rank3_R8P(u, v, w, x, y, z, is_uint64) result(code)
  !< Encode (Base64) a dataarray with 6 components of rank 3 (R8P).
  real(R8P), intent(in)         :: u(1:,1:,1:) !< U component.
  real(R8P), intent(in)         :: v(1:,1:,1:) !< V component.
  real(R8P), intent(in)         :: w(1:,1:,1:) !< W component.
  real(R8P), intent(in)         :: x(1:,1:,1:) !< X component.
  real(R8P), intent(in)         :: y(1:,1:,1:) !< Y component.
  real(R8P), intent(in)         :: z(1:,1:,1:) !< Z component.
  logical, intent(in), optional :: is_uint64   !< Use a UInt64 bytes count header (default UInt32).
  character(len=:), allocatable :: code        !< Encoded base64 dataarray.
  real(R8P),        allocatable :: buf(:)      !< Interleaved components.
  integer(I8P)                  :: nn          !< Number of elements.
  logical                       :: is_uint64_  !< Use a UInt64 bytes count header, local variable.

  is_uint64_ = .false. ; if (present(is_uint64)) is_uint64_ = is_uint64
  nn = size(x, kind=I8P)
  allocate(buf(1:6*nn))
  buf(1::6) = reshape(u, [nn])
  buf(2::6) = reshape(v, [nn])
  buf(3::6) = reshape(w, [nn])
  buf(4::6) = reshape(x, [nn])
  buf(5::6) = reshape(y, [nn])
  buf(6::6) = reshape(z, [nn])
  code = encode_payload(n_byte=6*nn*BYR8P, x=buf, is_uint64=is_uint64_)
  endfunction encode_binary_dataarray6_rank3_R8P

  function encode_binary_dataarray6_rank3_R4P(u, v, w, x, y, z, is_uint64) result(code)
  !< Encode (Base64) a dataarray with 6 components of rank 3 (R4P).
  real(R4P), intent(in)         :: u(1:,1:,1:) !< U component.
  real(R4P), intent(in)         :: v(1:,1:,1:) !< V component.
  real(R4P), intent(in)         :: w(1:,1:,1:) !< W component.
  real(R4P), intent(in)         :: x(1:,1:,1:) !< X component.
  real(R4P), intent(in)         :: y(1:,1:,1:) !< Y component.
  real(R4P), intent(in)         :: z(1:,1:,1:) !< Z component.
  logical, intent(in), optional :: is_uint64   !< Use a UInt64 bytes count header (default UInt32).
  character(len=:), allocatable :: code        !< Encoded base64 dataarray.
  real(R4P),        allocatable :: buf(:)      !< Interleaved components.
  integer(I8P)                  :: nn          !< Number of elements.
  logical                       :: is_uint64_  !< Use a UInt64 bytes count header, local variable.

  is_uint64_ = .false. ; if (present(is_uint64)) is_uint64_ = is_uint64
  nn = size(x, kind=I8P)
  allocate(buf(1:6*nn))
  buf(1::6) = reshape(u, [nn])
  buf(2::6) = reshape(v, [nn])
  buf(3::6) = reshape(w, [nn])
  buf(4::6) = reshape(x, [nn])
  buf(5::6) = reshape(y, [nn])
  buf(6::6) = reshape(z, [nn])
  code = encode_payload(n_byte=6*nn*BYR4P, x=buf, is_uint64=is_uint64_)
  endfunction encode_binary_dataarray6_rank3_R4P

  function encode_binary_dataarray6_rank3_I8P(u, v, w, x, y, z, is_uint64) result(code)
  !< Encode (Base64) a dataarray with 6 components of rank 3 (I8P).
  integer(I8P), intent(in)      :: u(1:,1:,1:) !< U component.
  integer(I8P), intent(in)      :: v(1:,1:,1:) !< V component.
  integer(I8P), intent(in)      :: w(1:,1:,1:) !< W component.
  integer(I8P), intent(in)      :: x(1:,1:,1:) !< X component.
  integer(I8P), intent(in)      :: y(1:,1:,1:) !< Y component.
  integer(I8P), intent(in)      :: z(1:,1:,1:) !< Z component.
  logical, intent(in), optional :: is_uint64   !< Use a UInt64 bytes count header (default UInt32).
  character(len=:), allocatable :: code        !< Encoded base64 dataarray.
  integer(I8P),     allocatable :: buf(:)      !< Interleaved components.
  integer(I8P)                  :: nn          !< Number of elements.
  logical                       :: is_uint64_  !< Use a UInt64 bytes count header, local variable.

  is_uint64_ = .false. ; if (present(is_uint64)) is_uint64_ = is_uint64
  nn = size(x, kind=I8P)
  allocate(buf(1:6*nn))
  buf(1::6) = reshape(u, [nn])
  buf(2::6) = reshape(v, [nn])
  buf(3::6) = reshape(w, [nn])
  buf(4::6) = reshape(x, [nn])
  buf(5::6) = reshape(y, [nn])
  buf(6::6) = reshape(z, [nn])
  code = encode_payload(n_byte=6*nn*BYI8P, x=buf, is_uint64=is_uint64_)
  endfunction encode_binary_dataarray6_rank3_I8P

  function encode_binary_dataarray6_rank3_I4P(u, v, w, x, y, z, is_uint64) result(code)
  !< Encode (Base64) a dataarray with 6 components of rank 3 (I4P).
  integer(I4P), intent(in)      :: u(1:,1:,1:) !< U component.
  integer(I4P), intent(in)      :: v(1:,1:,1:) !< V component.
  integer(I4P), intent(in)      :: w(1:,1:,1:) !< W component.
  integer(I4P), intent(in)      :: x(1:,1:,1:) !< X component.
  integer(I4P), intent(in)      :: y(1:,1:,1:) !< Y component.
  integer(I4P), intent(in)      :: z(1:,1:,1:) !< Z component.
  logical, intent(in), optional :: is_uint64   !< Use a UInt64 bytes count header (default UInt32).
  character(len=:), allocatable :: code        !< Encoded base64 dataarray.
  integer(I4P),     allocatable :: buf(:)      !< Interleaved components.
  integer(I8P)                  :: nn          !< Number of elements.
  logical                       :: is_uint64_  !< Use a UInt64 bytes count header, local variable.

  is_uint64_ = .false. ; if (present(is_uint64)) is_uint64_ = is_uint64
  nn = size(x, kind=I8P)
  allocate(buf(1:6*nn))
  buf(1::6) = reshape(u, [nn])
  buf(2::6) = reshape(v, [nn])
  buf(3::6) = reshape(w, [nn])
  buf(4::6) = reshape(x, [nn])
  buf(5::6) = reshape(y, [nn])
  buf(6::6) = reshape(z, [nn])
  code = encode_payload(n_byte=6*nn*BYI4P, x=buf, is_uint64=is_uint64_)
  endfunction encode_binary_dataarray6_rank3_I4P

  function encode_binary_dataarray6_rank3_I2P(u, v, w, x, y, z, is_uint64) result(code)
  !< Encode (Base64) a dataarray with 6 components of rank 3 (I2P).
  integer(I2P), intent(in)      :: u(1:,1:,1:) !< U component.
  integer(I2P), intent(in)      :: v(1:,1:,1:) !< V component.
  integer(I2P), intent(in)      :: w(1:,1:,1:) !< W component.
  integer(I2P), intent(in)      :: x(1:,1:,1:) !< X component.
  integer(I2P), intent(in)      :: y(1:,1:,1:) !< Y component.
  integer(I2P), intent(in)      :: z(1:,1:,1:) !< Z component.
  logical, intent(in), optional :: is_uint64   !< Use a UInt64 bytes count header (default UInt32).
  character(len=:), allocatable :: code        !< Encoded base64 dataarray.
  integer(I2P),     allocatable :: buf(:)      !< Interleaved components.
  integer(I8P)                  :: nn          !< Number of elements.
  logical                       :: is_uint64_  !< Use a UInt64 bytes count header, local variable.

  is_uint64_ = .false. ; if (present(is_uint64)) is_uint64_ = is_uint64
  nn = size(x, kind=I8P)
  allocate(buf(1:6*nn))
  buf(1::6) = reshape(u, [nn])
  buf(2::6) = reshape(v, [nn])
  buf(3::6) = reshape(w, [nn])
  buf(4::6) = reshape(x, [nn])
  buf(5::6) = reshape(y, [nn])
  buf(6::6) = reshape(z, [nn])
  code = encode_payload(n_byte=6*nn*BYI2P, x=buf, is_uint64=is_uint64_)
  endfunction encode_binary_dataarray6_rank3_I2P

  function encode_binary_dataarray6_rank3_I1P(u, v, w, x, y, z, is_uint64) result(code)
  !< Encode (Base64) a dataarray with 6 components of rank 3 (I1P).
  integer(I1P), intent(in)      :: u(1:,1:,1:) !< U component.
  integer(I1P), intent(in)      :: v(1:,1:,1:) !< V component.
  integer(I1P), intent(in)      :: w(1:,1:,1:) !< W component.
  integer(I1P), intent(in)      :: x(1:,1:,1:) !< X component.
  integer(I1P), intent(in)      :: y(1:,1:,1:) !< Y component.
  integer(I1P), intent(in)      :: z(1:,1:,1:) !< Z component.
  logical, intent(in), optional :: is_uint64   !< Use a UInt64 bytes count header (default UInt32).
  character(len=:), allocatable :: code        !< Encoded base64 dataarray.
  integer(I1P),     allocatable :: buf(:)      !< Interleaved components.
  integer(I8P)                  :: nn          !< Number of elements.
  logical                       :: is_uint64_  !< Use a UInt64 bytes count header, local variable.

  is_uint64_ = .false. ; if (present(is_uint64)) is_uint64_ = is_uint64
  nn = size(x, kind=I8P)
  allocate(buf(1:6*nn))
  buf(1::6) = reshape(u, [nn])
  buf(2::6) = reshape(v, [nn])
  buf(3::6) = reshape(w, [nn])
  buf(4::6) = reshape(x, [nn])
  buf(5::6) = reshape(y, [nn])
  buf(6::6) = reshape(z, [nn])
  code = encode_payload(n_byte=6*nn*BYI1P, x=buf, is_uint64=is_uint64_)
  endfunction encode_binary_dataarray6_rank3_I1P

  ! payload encoders: bytes count header (UInt32 or UInt64) followed by the data
  function encode_payload_R8P(n_byte, x, is_uint64) result(code)
  !< Encode (Base64) the bytes count header followed by the data (R8P).
  integer(I8P),    intent(in)    :: n_byte    !< Bytes count of data.
  real(R8P)   , intent(in)    :: x(1:)     !< Data (flattened, interleaved).
  logical,         intent(in)    :: is_uint64 !< Use a UInt64 bytes count header (UInt32 otherwise).
  character(len=:), allocatable  :: code      !< Encoded base64 dataarray.
  integer(I1P),    allocatable   :: xp(:)     !< Packed data.

  if (is_uint64) then
     call pack_data(a1=[n_byte], a2=x, packed=xp)
     call b64_encode(n=xp, code=code)
  else
     call pack_data(a1=[bytes_count(n_byte)], a2=x, packed=xp)
     call b64_encode(n=xp, code=code)
  endif
  endfunction encode_payload_R8P

  function encode_payload_R4P(n_byte, x, is_uint64) result(code)
  !< Encode (Base64) the bytes count header followed by the data (R4P).
  integer(I8P),    intent(in)    :: n_byte    !< Bytes count of data.
  real(R4P)   , intent(in)    :: x(1:)     !< Data (flattened, interleaved).
  logical,         intent(in)    :: is_uint64 !< Use a UInt64 bytes count header (UInt32 otherwise).
  character(len=:), allocatable  :: code      !< Encoded base64 dataarray.
  integer(I1P),    allocatable   :: xp(:)     !< Packed data.

  if (is_uint64) then
     call pack_data(a1=[n_byte], a2=x, packed=xp)
     call b64_encode(n=xp, code=code)
  else
     call pack_data(a1=[bytes_count(n_byte)], a2=x, packed=xp)
     call b64_encode(n=xp, code=code)
  endif
  endfunction encode_payload_R4P

  function encode_payload_I8P(n_byte, x, is_uint64) result(code)
  !< Encode (Base64) the bytes count header followed by the data (I8P).
  integer(I8P),    intent(in)    :: n_byte    !< Bytes count of data.
  integer(I8P), intent(in)    :: x(1:)     !< Data (flattened, interleaved).
  logical,         intent(in)    :: is_uint64 !< Use a UInt64 bytes count header (UInt32 otherwise).
  character(len=:), allocatable  :: code      !< Encoded base64 dataarray.
  integer(I8P),   allocatable  :: buf(:)    !< Header and data (header of the same kind of data).
  integer(I1P),    allocatable   :: xp(:)     !< Packed data.

  if (is_uint64) then
     allocate(buf(0:size(x, kind=I8P)))
     buf(0) = n_byte
     buf(1:) = x
     call b64_encode(n=buf, code=code)
  else
     call pack_data(a1=[bytes_count(n_byte)], a2=x, packed=xp)
     call b64_encode(n=xp, code=code)
  endif
  endfunction encode_payload_I8P

  function encode_payload_I4P(n_byte, x, is_uint64) result(code)
  !< Encode (Base64) the bytes count header followed by the data (I4P).
  integer(I8P),    intent(in)    :: n_byte    !< Bytes count of data.
  integer(I4P), intent(in)    :: x(1:)     !< Data (flattened, interleaved).
  logical,         intent(in)    :: is_uint64 !< Use a UInt64 bytes count header (UInt32 otherwise).
  character(len=:), allocatable  :: code      !< Encoded base64 dataarray.
  integer(I4P),   allocatable  :: buf(:)    !< Header and data (header of the same kind of data).
  integer(I1P),    allocatable   :: xp(:)     !< Packed data.

  if (is_uint64) then
     call pack_data(a1=[n_byte], a2=x, packed=xp)
     call b64_encode(n=xp, code=code)
  else
     allocate(buf(0:size(x, kind=I8P)))
     buf(0) = bytes_count(n_byte)
     buf(1:) = x
     call b64_encode(n=buf, code=code)
  endif
  endfunction encode_payload_I4P

  function encode_payload_I2P(n_byte, x, is_uint64) result(code)
  !< Encode (Base64) the bytes count header followed by the data (I2P).
  integer(I8P),    intent(in)    :: n_byte    !< Bytes count of data.
  integer(I2P), intent(in)    :: x(1:)     !< Data (flattened, interleaved).
  logical,         intent(in)    :: is_uint64 !< Use a UInt64 bytes count header (UInt32 otherwise).
  character(len=:), allocatable  :: code      !< Encoded base64 dataarray.
  integer(I1P),    allocatable   :: xp(:)     !< Packed data.

  if (is_uint64) then
     call pack_data(a1=[n_byte], a2=x, packed=xp)
     call b64_encode(n=xp, code=code)
  else
     call pack_data(a1=[bytes_count(n_byte)], a2=x, packed=xp)
     call b64_encode(n=xp, code=code)
  endif
  endfunction encode_payload_I2P

  function encode_payload_I1P(n_byte, x, is_uint64) result(code)
  !< Encode (Base64) the bytes count header followed by the data (I1P).
  integer(I8P),    intent(in)    :: n_byte    !< Bytes count of data.
  integer(I1P), intent(in)    :: x(1:)     !< Data (flattened, interleaved).
  logical,         intent(in)    :: is_uint64 !< Use a UInt64 bytes count header (UInt32 otherwise).
  character(len=:), allocatable  :: code      !< Encoded base64 dataarray.
  integer(I1P),    allocatable   :: xp(:)     !< Packed data.

  if (is_uint64) then
     call pack_data(a1=[n_byte], a2=x, packed=xp)
     call b64_encode(n=xp, code=code)
  else
     call pack_data(a1=[bytes_count(n_byte)], a2=x, packed=xp)
     call b64_encode(n=xp, code=code)
  endif
  endfunction encode_payload_I1P
endmodule vtk_fortran_dataarray_encoder
