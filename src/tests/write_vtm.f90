!< VTK_Fortran test.
program write_vtm
!-----------------------------------------------------------------------------------------------------------------------------------
!< VTK_Fortran test.
!-----------------------------------------------------------------------------------------------------------------------------------
use penf
use vtk_fortran, only : vtm_file, vtk_file
!-----------------------------------------------------------------------------------------------------------------------------------

!-----------------------------------------------------------------------------------------------------------------------------------
implicit none
type(vtk_file)          :: a_vtk_file                             !< A VTK file.
type(vtm_file)          :: a_vtm_file                             !< A VTM file.
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
character(15)           :: filenames(4)                           !< File names.
character(1)            :: names(4)                               !< Custom names.
integer(I4P)            :: error                                  !< Status error.
integer(I4P)            :: i                                      !< Counter.
integer(I4P)            :: j                                      !< Counter.
integer(I4P)            :: k                                      !< Counter.
integer(I4P)            :: f                                      !< Counter.
logical                 :: test_passed(1)                         !< List of passed tests.
!-----------------------------------------------------------------------------------------------------------------------------------

!-----------------------------------------------------------------------------------------------------------------------------------
filenames = ['XML_STRG-01.vts', 'XML_STRG-02.vts', 'XML_STRG-03.vts', 'XML_STRG-04.vts']
names = ['1', '2', '3', '4']
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

do f=1, size(filenames, dim=1)
  x = x + nx2
  v = v * 2._R8P
  error = a_vtk_file%initialize(format='raw', filename=filenames(f), mesh_topology='StructuredGrid', &
                                nx1=nx1, nx2=nx2, ny1=ny1, ny2=ny2, nz1=nz1, nz2=nz2)
  call write_data
  error = a_vtk_file%finalize()
enddo

error = a_vtm_file%initialize(filename='XML_STRG.vtm')
error = a_vtm_file%write_block(filenames=filenames(4)//' '//filenames(1), names=names(4)//' '//names(1), name='first block')
error = a_vtm_file%write_block(filenames=[filenames(2), filenames(3)], names=[names(2), names(3)], name='second block')
error = a_vtm_file%finalize()

test_passed = .true. ! nothing to test yet

print "(A,L1)", new_line('a')//'Are all tests passed? ', all(test_passed)
stop
!-----------------------------------------------------------------------------------------------------------------------------------
contains
  subroutine write_data
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Write data.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  error = a_vtk_file%xml_writer%write_piece(nx1=nx1, nx2=nx2, ny1=ny1, ny2=ny2, nz1=nz1, nz2=nz2)
  error = a_vtk_file%xml_writer%write_geo(n=nn, x=x, y=y, z=z)
  error = a_vtk_file%xml_writer%write_dataarray(location='node', action='open')
  error = a_vtk_file%xml_writer%write_dataarray(data_name='float64_scalar', x=v, one_component=.true.)
  error = a_vtk_file%xml_writer%write_dataarray(location='node', action='close')
  error = a_vtk_file%xml_writer%write_piece()
  !---------------------------------------------------------------------------------------------------------------------------------
  endsubroutine write_data
endprogram write_vtm
