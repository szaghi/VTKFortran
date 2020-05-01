!< VTK_Fortran test: write VTS file.
program vtk_fortran_write_vts
!< VTK_Fortran test: write VTS file.
use penf
use vtk_fortran, only : vtk_file

implicit none
type(vtk_file)          :: a_vtk_file                             !< A VTK file.
integer(I4P), parameter :: nx1=0_I4P                              !< X lower bound extent.
integer(I4P), parameter :: nx2=9_I4P                              !< X upper bound extent.
integer(I4P), parameter :: ny1=0_I4P                              !< Y lower bound extent.
integer(I4P), parameter :: ny2=5_I4P                              !< Y upper bound extent.
integer(I4P), parameter :: nz1=0_I4P                              !< Z lower bound extent.
integer(I4P), parameter :: nz2=5_I4P                              !< Z upper bound extent.
integer(I4P), parameter :: nn=(nx2-nx1+1)*(ny2-ny1+1)*(nz2-nz1+1) !< Number of elements.
real(R8P)               :: x(nx1:nx2,ny1:ny2,nz1:nz2)             !< X coordinates.
real(R8P)               :: y(nx1:nx2,ny1:ny2,nz1:nz2)             !< Y coordinates.
real(R8P)               :: z(nx1:nx2,ny1:ny2,nz1:nz2)             !< Z coordinates.
real(R8P)               :: v(nx1:nx2,ny1:ny2,nz1:nz2)             !< Variable defined at coordinates.
integer(I4P)            :: error                                  !< Status error.
integer(I4P)            :: i                                      !< Counter.
integer(I4P)            :: j                                      !< Counter.
integer(I4P)            :: k                                      !< Counter.
logical                 :: test_passed(1)                         !< List of passed tests.

do k=nz1, nz2
  do j=ny1, ny2
    do i=nx1, nx2
      x(i, j, k) = i*1._R8P
      y(i, j, k) = j*1._R8P
      z(i, j, k) = k*1._R8P
      v(i, j, k) = real(i*j*k, R8P)
    enddo
  enddo
enddo
! binary
error = a_vtk_file%initialize(format='binary', filename='XML_STRG-binary.vts', mesh_topology='StructuredGrid', &
                              nx1=nx1, nx2=nx2, ny1=ny1, ny2=ny2, nz1=nz1, nz2=nz2)
call write_data
error = a_vtk_file%finalize()
! raw
error = a_vtk_file%initialize(format='raw', filename='XML_STRG-raw.vts', mesh_topology='StructuredGrid', &
                              nx1=nx1, nx2=nx2, ny1=ny1, ny2=ny2, nz1=nz1, nz2=nz2)
call write_data
error = a_vtk_file%finalize()

test_passed = .true. ! nothing to test yet

print "(A,L1)", new_line('a')//'Are all tests passed? ', all(test_passed)
stop
contains
  subroutine write_data
  !< Write data.

  error = a_vtk_file%xml_writer%write_piece(nx1=nx1, nx2=nx2, ny1=ny1, ny2=ny2, nz1=nz1, nz2=nz2)
  error = a_vtk_file%xml_writer%write_geo(n=nn, x=x, y=y, z=z)
  error = a_vtk_file%xml_writer%write_dataarray(location='node', action='open')
  error = a_vtk_file%xml_writer%write_dataarray(data_name='float64_scalar', x=v, one_component=.true.)
  error = a_vtk_file%xml_writer%write_dataarray(location='node', action='close')
  error = a_vtk_file%xml_writer%write_piece()
  endsubroutine write_data
endprogram vtk_fortran_write_vts
