!< Minimal zlib bindings used for VTK XML internal compression (vtkZLibDataCompressor).
module vtk_fortran_zlib
!< Minimal zlib bindings used for VTK XML internal compression (vtkZLibDataCompressor), compression and decompression.
!<
!< The module is always compiled: without `VTKFORTRAN_USE_ZLIB` the procedures are stubs returning an error, and
!< `is_zlib_enabled` is `.false.`. With it, the library must be linked with zlib (`-lz`).
use, intrinsic :: iso_c_binding, only : c_int, c_long, c_ptr, c_loc, c_signed_char
use penf, only : I8P
implicit none
private

public :: is_zlib_enabled
public :: zlib_compress_blocks
public :: zlib_compress_bound
public :: zlib_compress2
public :: zlib_uncompress
public :: zlib_uncompress_blocks
public :: Z_DEFAULT_COMPRESSION
public :: Z_BEST_SPEED
public :: Z_BEST_COMPRESSION

integer(c_int), parameter :: Z_DEFAULT_COMPRESSION = -1_c_int !< zlib default compression level (currently 6).
integer(c_int), parameter :: Z_BEST_SPEED         =  1_c_int  !< zlib fastest compression level.
integer(c_int), parameter :: Z_BEST_COMPRESSION   =  9_c_int  !< zlib best compression level.

#ifdef VTKFORTRAN_USE_ZLIB
logical, parameter :: is_zlib_enabled = .true.  !< The library is built with zlib (VTKFORTRAN_USE_ZLIB).
#else
logical, parameter :: is_zlib_enabled = .false. !< The library is built with zlib (VTKFORTRAN_USE_ZLIB).
#endif

#ifdef VTKFORTRAN_USE_ZLIB
interface
  function compressBound(sourceLen) bind(C, name='compressBound') result(bound)
    !< zlib `compressBound`: upper bound of the compressed size of `sourceLen` bytes.
    import :: c_long
    integer(c_long), value :: sourceLen !< Uncompressed size in bytes.
    integer(c_long)        :: bound     !< Upper bound of the compressed size.
  end function compressBound

  function compress2(dest, destLen, source, sourceLen, level) bind(C, name='compress2') result(ret)
    !< zlib `compress2`: compress `sourceLen` bytes of `source` into `dest`, with the given compression level.
    import :: c_ptr, c_int, c_long
    type(c_ptr),     value :: dest      !< Compressed buffer.
    type(c_ptr),     value :: destLen   !< Size of the compressed buffer on input, compressed size on output.
    type(c_ptr),     value :: source    !< Uncompressed buffer.
    integer(c_long), value :: sourceLen !< Uncompressed size in bytes.
    integer(c_int),  value :: level     !< Compression level.
    integer(c_int)         :: ret       !< zlib return code: 0 (Z_OK) on success.
  end function compress2

  function uncompress(dest, destLen, source, sourceLen) bind(C, name='uncompress') result(ret)
    !< zlib `uncompress`: decompress `sourceLen` bytes of `source` into `dest`.
    import :: c_ptr, c_int, c_long
    type(c_ptr),     value :: dest      !< Uncompressed buffer.
    type(c_ptr),     value :: destLen   !< Size of the uncompressed buffer on input, uncompressed size on output.
    type(c_ptr),     value :: source    !< Compressed buffer.
    integer(c_long), value :: sourceLen !< Compressed size in bytes.
    integer(c_int)         :: ret       !< zlib return code: 0 (Z_OK) on success.
  end function uncompress
end interface
#endif

contains

  function zlib_compress_bound(n) result(bound)
  !< Upper bound of the compressed size of `n` bytes (0 without zlib).
  integer(c_long), value :: n     !< Uncompressed size in bytes.
  integer(c_long)        :: bound !< Upper bound of the compressed size.
#ifdef VTKFORTRAN_USE_ZLIB
  bound = compressBound(n)
#else
  bound = 0_c_long
#endif
  end function zlib_compress_bound

  function zlib_compress2(dst, dst_len, src, src_len, level) result(ret)
  !< Compress a byte buffer with zlib compress2 (an error without zlib).
  integer(c_signed_char), intent(inout), target :: dst(:)  !< Compressed buffer.
  integer(c_long),        intent(inout), target :: dst_len !< Size of `dst` on input, compressed size on output.
  integer(c_signed_char), intent(in),    target :: src(:)  !< Uncompressed buffer.
  integer(c_long),        intent(in)            :: src_len !< Uncompressed size in bytes.
  integer(c_int),         intent(in)            :: level   !< Compression level.
  integer(c_int)                                :: ret     !< zlib return code: 0 on success, non-zero on error.

#ifndef VTKFORTRAN_USE_ZLIB
  dst_len = 0_c_long
  ret = -1_c_int
#else
  if (src_len == 0_c_long) then
    dst_len = 0_c_long
    ret = 0_c_int
    return
  endif
  ret = compress2(c_loc(dst(1)), c_loc(dst_len), c_loc(src(1)), src_len, level)
#endif
  end function zlib_compress2

  subroutine zlib_compress_blocks(bytes, block_size, level, header, blocks, error)
  !< Compress a bytes stream into VTK compressed blocks (vtkZLibDataCompressor layout).
  !<
  !< The stream is split into blocks of `block_size` bytes (the last one can be shorter), each compressed independently.
  !< The VTK header of the compressed data is returned as I8P words, its width in the file (UInt32 or UInt64) is chosen by
  !< the caller:
  !<
  !<```
  !< header = [number of blocks, block size, last block size, compressed size of each block]
  !<```
  !<
  !< As VTK writes it, the last block size is the size of a partial last block, 0 when all blocks are full; an empty stream
  !< has no blocks. `blocks` holds the compressed blocks, concatenated.
  integer(c_signed_char),              intent(in)  :: bytes(1:)  !< Uncompressed bytes.
  integer(I8P),                        intent(in)  :: block_size !< Uncompressed block size in bytes.
  integer(c_int),                      intent(in)  :: level      !< zlib compression level.
  integer(I8P),           allocatable, intent(out) :: header(:)  !< VTK header of the compressed data.
  integer(c_signed_char), allocatable, intent(out) :: blocks(:)  !< Compressed blocks.
  integer,                             intent(out) :: error      !< Error status: 0 on success.
  integer(I8P)                                     :: nb         !< Number of blocks.
  integer(I8P)                                     :: last       !< Size of the last block.
  integer(I8P)                                     :: n_in       !< Size of the current block.
  integer(I8P)                                     :: n_out      !< Compressed bytes written so far.
  integer(I8P)                                     :: first      !< First byte of the current block.
  integer(I8P)                                     :: i          !< Counter.
  integer(c_long)                                  :: bound      !< Bound of the compressed size of a block.
  integer(c_long)                                  :: dst_len    !< Compressed size of the current block.
  integer(c_signed_char), allocatable              :: inbuf(:)   !< Uncompressed block.
  integer(c_signed_char), allocatable              :: outbuf(:)  !< Compressed block.
  integer(c_signed_char), allocatable              :: grown(:)   !< Grown blocks buffer.

  error = 0
  nb = (size(bytes, kind=I8P) + block_size - 1_I8P) / block_size
  last = mod(size(bytes, kind=I8P), block_size)
  allocate(header(1:3_I8P+nb))
  header(1:3) = [nb, block_size, last]
  if (.not.is_zlib_enabled) then
    allocate(blocks(1:0))
    error = 1
    return
  endif
  bound = zlib_compress_bound(int(block_size, c_long))
  allocate(inbuf(1:block_size), outbuf(1:int(bound, I8P)))
  ! the compressed data are usually smaller than the input: start from its size and grow when needed
  allocate(blocks(1:max(int(bound, I8P), size(bytes, kind=I8P))))
  n_out = 0_I8P
  do i=1_I8P, nb
    first = (i - 1_I8P) * block_size
    n_in = merge(last, block_size, i == nb .and. last > 0_I8P)
    if (n_in > 0_I8P) inbuf(1:n_in) = bytes(first+1_I8P:first+n_in)
    dst_len = bound
    error = zlib_compress2(dst=outbuf, dst_len=dst_len, src=inbuf, src_len=int(n_in, c_long), level=level)
    if (error /= 0) return
    if (n_out + int(dst_len, I8P) > size(blocks, kind=I8P)) then
      allocate(grown(1:2_I8P*size(blocks, kind=I8P) + int(dst_len, I8P)))
      grown(1:n_out) = blocks(1:n_out)
      call move_alloc(from=grown, to=blocks)
    endif
    blocks(n_out+1_I8P:n_out+int(dst_len, I8P)) = outbuf(1:int(dst_len, I8P))
    n_out = n_out + int(dst_len, I8P)
    header(3_I8P+i) = int(dst_len, I8P)
  enddo
  allocate(grown(1:n_out))
  grown(1:n_out) = blocks(1:n_out)
  call move_alloc(from=grown, to=blocks)
  endsubroutine zlib_compress_blocks

  function zlib_uncompress(dst, dst_len, src, src_len) result(ret)
  !< Decompress a byte buffer with zlib uncompress (an error without zlib).
  integer(c_signed_char), intent(inout), target :: dst(:)  !< Uncompressed buffer.
  integer(c_long),        intent(inout), target :: dst_len !< Size of `dst` on input, uncompressed size on output.
  integer(c_signed_char), intent(in),    target :: src(:)  !< Compressed buffer.
  integer(c_long),        intent(in)            :: src_len !< Compressed size in bytes.
  integer(c_int)                                :: ret     !< zlib return code: 0 on success, non-zero on error.

#ifndef VTKFORTRAN_USE_ZLIB
  dst_len = 0_c_long
  ret = -1_c_int
#else
  if (dst_len == 0_c_long) then
    ! nothing to decompress (an empty block): src can be empty too
    ret = 0_c_int
    return
  endif
  if (src_len <= 0_c_long) then
    dst_len = 0_c_long
    ret = -3_c_int ! Z_DATA_ERROR
    return
  endif
  ret = uncompress(c_loc(dst(1)), c_loc(dst_len), c_loc(src(1)), src_len)
#endif
  end function zlib_uncompress

  subroutine zlib_uncompress_blocks(header, blocks, bytes, error)
  !< Decompress VTK compressed blocks (vtkZLibDataCompressor layout) into a bytes stream: the inverse of
  !< [[zlib_compress_blocks]].
  !<
  !< The header, read from the file as UInt32 or UInt64 words and passed as I8P, is
  !<
  !<```
  !< header = [number of blocks, block size, last block size, compressed size of each block]
  !<```
  !<
  !< where a last block size of 0 means that the last block is full. `blocks` holds the compressed blocks, concatenated (it
  !< can be longer than their total). The header is checked before decompressing: an inconsistent header, a block that
  !< does not decompress to its declared size, or a library built without zlib return a non-zero error.
  integer(I8P),                        intent(in)  :: header(1:) !< VTK header of the compressed data.
  integer(c_signed_char),              intent(in)  :: blocks(1:) !< Compressed blocks.
  integer(c_signed_char), allocatable, intent(out) :: bytes(:)   !< Uncompressed bytes.
  integer,                             intent(out) :: error      !< Error status: 0 on success.
  integer(I8P)                                     :: nb         !< Number of blocks.
  integer(I8P)                                     :: block_size !< Uncompressed block size.
  integer(I8P)                                     :: last       !< Size of the last block.
  integer(I8P)                                     :: n_out      !< Expected size of the current uncompressed block.
  integer(I8P)                                     :: first_in   !< First compressed byte of the current block.
  integer(I8P)                                     :: first_out  !< First uncompressed byte of the current block.
  integer(I8P)                                     :: i          !< Counter.
  integer(c_long)                                  :: dst_len    !< Uncompressed size of the current block.

  error = 1
  allocate(bytes(1:0))
  if (size(header, kind=I8P) < 3_I8P) return
  nb = header(1) ; block_size = header(2) ; last = header(3)
  if (nb < 0_I8P .or. size(header, kind=I8P) < 3_I8P + nb) return
  if (nb == 0_I8P) then
    error = 0
    return
  endif
  if (block_size <= 0_I8P .or. last < 0_I8P .or. last > block_size) return
  if (any(header(4_I8P:3_I8P+nb) < 0_I8P)) return
  if (sum(header(4_I8P:3_I8P+nb)) > size(blocks, kind=I8P)) return
  if (.not.is_zlib_enabled) return
  if (last == 0_I8P) last = block_size
  deallocate(bytes)
  allocate(bytes(1:(nb - 1_I8P) * block_size + last))
  first_in = 1_I8P
  first_out = 1_I8P
  do i=1_I8P, nb
    n_out = merge(last, block_size, i == nb)
    dst_len = int(n_out, c_long)
    ! decompress in place into the output: the sections are contiguous
    error = zlib_uncompress(dst=bytes(first_out:first_out+n_out-1_I8P), dst_len=dst_len,              &
                            src=blocks(first_in:first_in+header(3_I8P+i)-1_I8P), src_len=int(header(3_I8P+i), c_long))
    if (error /= 0) return
    if (int(dst_len, I8P) /= n_out) then
      error = 1
      return
    endif
    first_in = first_in + header(3_I8P+i)
    first_out = first_out + n_out
  enddo
  endsubroutine zlib_uncompress_blocks

end module vtk_fortran_zlib

