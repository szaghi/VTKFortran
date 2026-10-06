!< VTK_Fortran test: write large VTS files (regression of issue #70).
program vtk_fortran_write_large
!< VTK_Fortran test: write large VTS files (regression of issue #70).
!<
!< Each 3-components dataarray is 24 MB: before the fix of issue #70 its encoding created stack temporaries of (at least)
!< that size, overflowing the default 8 MB stack with compilers placing them on the stack (e.g. ifx).
use penf
use vtk_fortran, only : vtk_file

implicit none
type(vtk_file)          :: a_vtk_file                             !< A VTK file.
integer(I4P), parameter :: nx1=1_I4P                              !< X lower bound extent.
integer(I4P), parameter :: nx2=1000_I4P                           !< X upper bound extent.
integer(I4P), parameter :: ny1=1_I4P                              !< Y lower bound extent.
integer(I4P), parameter :: ny2=1000_I4P                           !< Y upper bound extent.
integer(I4P), parameter :: nz1=1_I4P                              !< Z lower bound extent.
integer(I4P), parameter :: nz2=1_I4P                              !< Z upper bound extent.
integer(I4P), parameter :: nn=(nx2-nx1+1)*(ny2-ny1+1)*(nz2-nz1+1) !< Number of elements.
character(*), parameter :: formats(4)=['ascii          ', &
                                       'binary         ', &
                                       'raw            ', &
                                       'binary-appended']     !< Tested formats.
real(R8P), allocatable  :: x(:,:,:)                               !< X coordinates.
real(R8P), allocatable  :: y(:,:,:)                               !< Y coordinates.
real(R8P), allocatable  :: z(:,:,:)                               !< Z coordinates.
real(R8P), allocatable  :: v(:,:,:)                               !< Variable defined at coordinates.
integer(I4P)            :: i                                      !< Counter.
integer(I4P)            :: j                                      !< Counter.
integer(I4P)            :: f                                      !< Counter.
logical                 :: test_passed(size(formats))             !< List of passed tests.

allocate(x(nx1:nx2,ny1:ny2,nz1:nz2), y(nx1:nx2,ny1:ny2,nz1:nz2), z(nx1:nx2,ny1:ny2,nz1:nz2), v(nx1:nx2,ny1:ny2,nz1:nz2))
do j=ny1, ny2
  do i=nx1, nx2
    x(i, j, nz1) = i*1._R8P
    y(i, j, nz1) = j*1._R8P
    z(i, j, nz1) = 0._R8P
    v(i, j, nz1) = real(i*j, R8P)
  enddo
enddo
do f=1, size(formats)
  test_passed(f) = write_file(format=trim(formats(f)), filename='XML_STRG-large-'//trim(formats(f))//'.vts')
  print "(A,L1)", 'format '//trim(formats(f))//' passed: ', test_passed(f)
enddo

print "(A,L1)", new_line('a')//'Are all tests passed? ', all(test_passed)
stop
contains
  function write_file(format, filename) result(is_passed)
  !< Write a file, check all errors and the file size, then delete the (large) file.
  character(*), intent(in) :: format    !< File format.
  character(*), intent(in) :: filename  !< File name.
  logical                  :: is_passed !< Test result.
  integer(I4P)             :: errors(8) !< Errors of each call.
  integer(I8P)             :: file_size !< File size.
  integer(I4P)             :: u         !< File unit.

  errors(1) = a_vtk_file%initialize(format=format, filename=filename, mesh_topology='StructuredGrid', &
                                    nx1=nx1, nx2=nx2, ny1=ny1, ny2=ny2, nz1=nz1, nz2=nz2)
  errors(2) = a_vtk_file%xml_writer%write_piece(nx1=nx1, nx2=nx2, ny1=ny1, ny2=ny2, nz1=nz1, nz2=nz2)
  errors(3) = a_vtk_file%xml_writer%write_geo(n=nn, x=x, y=y, z=z)
  errors(4) = a_vtk_file%xml_writer%write_dataarray(location='node', action='open')
  errors(5) = a_vtk_file%xml_writer%write_dataarray(data_name='vector', x=v, y=v, z=v)
  errors(6) = a_vtk_file%xml_writer%write_dataarray(location='node', action='close')
  errors(7) = a_vtk_file%xml_writer%write_piece()
  errors(8) = a_vtk_file%finalize()
  inquire(file=filename, size=file_size)
  ! two 3-components Float64 dataarrays: at least 2*3*nn*8 bytes, whatever the encoding
  is_passed = all(errors==0).and.(file_size>=2_I8P*3_I8P*nn*BYR8P)
  open(newunit=u, file=filename)
  close(unit=u, status='delete')
  endfunction write_file
endprogram vtk_fortran_write_large
