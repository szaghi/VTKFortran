!< VTK_Fortran test: compress and decompress VTK zlib blocks (vtkZLibDataCompressor layout).
program vtk_fortran_zlib_blocks
!< VTK_Fortran test: compress and decompress VTK zlib blocks (vtkZLibDataCompressor layout).
!<
!< Bytes streams are compressed into blocks of 8 bytes and decompressed back: an empty stream, a stream of one partial block,
!< one of full blocks only and one ending with a partial block. Inconsistent headers must be refused. Without
!< VTKFORTRAN_USE_ZLIB, the test checks that both procedures return an error.
use, intrinsic :: iso_c_binding, only : c_int, c_signed_char
use penf
use vtk_fortran_zlib, only : zlib_compress_blocks, zlib_uncompress_blocks

implicit none
integer(I8P), parameter             :: block_size=8_I8P              !< Uncompressed block size.
integer(I8P), parameter             :: sizes(4)=[0_I8P, 5_I8P, 16_I8P, 21_I8P] !< Sizes of the streams.
integer(c_signed_char), allocatable :: bytes(:)                      !< Bytes stream.
integer(I8P),           allocatable :: header(:)                     !< VTK header of the compressed data.
integer(c_signed_char), allocatable :: blocks(:)                     !< Compressed blocks.
integer(c_signed_char), allocatable :: back(:)                       !< Decompressed bytes.
integer                             :: error                         !< Status error.
integer(I8P)                        :: s                             !< Counter.
integer(I8P)                        :: i                             !< Counter.
logical                             :: test_passed(6)                !< List of passed tests.

#ifdef VTKFORTRAN_USE_ZLIB
test_passed = .false.
do s=1_I8P, size(sizes, kind=I8P)
  bytes = [(int(mod(i*i, 128_I8P), c_signed_char), i=1_I8P, sizes(s))]
  call zlib_compress_blocks(bytes=bytes, block_size=block_size, level=6_c_int, header=header, blocks=blocks, error=error)
  if (error /= 0) cycle
  call zlib_uncompress_blocks(header=header, blocks=blocks, bytes=back, error=error)
  test_passed(s) = error == 0 .and. size(back, kind=I8P) == sizes(s)
  if (test_passed(s)) test_passed(s) = all(back == bytes)
enddo
! inconsistent headers: compressed size beyond the blocks, last block larger than the block size
call zlib_uncompress_blocks(header=[1_I8P, block_size, 0_I8P, size(blocks, kind=I8P) + 1_I8P], blocks=blocks, &
                            bytes=back, error=error)
test_passed(5) = error /= 0
call zlib_uncompress_blocks(header=[1_I8P, block_size, block_size + 1_I8P, 1_I8P], blocks=blocks, bytes=back, error=error)
test_passed(6) = error /= 0
#else
test_passed(1:4) = .true.
call zlib_compress_blocks(bytes=[1_c_signed_char], block_size=block_size, level=6_c_int, header=header, blocks=blocks, &
                          error=error)
test_passed(5) = error /= 0
call zlib_uncompress_blocks(header=[1_I8P, block_size, 1_I8P, 1_I8P], blocks=[1_c_signed_char], bytes=back, error=error)
test_passed(6) = error /= 0
#endif

print "(A,L1)", new_line('a')//'Are all tests passed? ', all(test_passed)
if (.not.all(test_passed)) error stop 'some tests failed'
stop
endprogram vtk_fortran_zlib_blocks
