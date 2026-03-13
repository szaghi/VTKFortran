!< Minimal zlib bindings used for VTK XML internal compression (vtkZLibDataCompressor).
module vtk_fortran_zlib
use, intrinsic :: iso_c_binding, only : c_int, c_long, c_ptr, c_loc, c_signed_char
implicit none
private

public :: zlib_compress_bound
public :: zlib_compress2
public :: Z_DEFAULT_COMPRESSION
public :: Z_BEST_SPEED
public :: Z_BEST_COMPRESSION

integer(c_int), parameter :: Z_DEFAULT_COMPRESSION = -1_c_int
integer(c_int), parameter :: Z_BEST_SPEED         =  1_c_int
integer(c_int), parameter :: Z_BEST_COMPRESSION   =  9_c_int

#ifdef VTKFORTRAN_USE_ZLIB
interface
  function compressBound(sourceLen) bind(C, name='compressBound') result(bound)
    import :: c_long
    integer(c_long), value :: sourceLen
    integer(c_long)        :: bound
  end function compressBound

  function compress2(dest, destLen, source, sourceLen, level) bind(C, name='compress2') result(ret)
    import :: c_ptr, c_int, c_long
    type(c_ptr),     value :: dest
    type(c_ptr),     value :: destLen
    type(c_ptr),     value :: source
    integer(c_long), value :: sourceLen
    integer(c_int),  value :: level
    integer(c_int)         :: ret
  end function compress2
end interface
#endif

contains

  function zlib_compress_bound(n) result(bound)
  integer(c_long), value :: n
  integer(c_long)        :: bound
#ifdef VTKFORTRAN_USE_ZLIB
  bound = compressBound(n)
#else
  bound = 0_c_long
#endif
  end function zlib_compress_bound

  function zlib_compress2(dst, dst_len, src, src_len, level) result(ret)
  !< Compress a byte buffer with zlib compress2.
  integer(c_signed_char), intent(inout), target :: dst(:)
  integer(c_long),        intent(inout), target :: dst_len
  integer(c_signed_char), intent(in),    target :: src(:)
  integer(c_long),        intent(in)            :: src_len
  integer(c_int),           intent(in)            :: level
  integer(c_int)                                 :: ret

#ifndef VTKFORTRAN_USE_ZLIB
  dst_len = 0_c_long
  ret = -1_c_int
  return
#endif
  if (src_len == 0_c_long) then
    dst_len = 0_c_long
    ret = 0_c_int
    return
  endif
  ret = compress2(c_loc(dst(1)), c_loc(dst_len), c_loc(src(1)), src_len, level)
  end function zlib_compress2

end module vtk_fortran_zlib

