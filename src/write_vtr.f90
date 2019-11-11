!< VTK_Fortran test.
program write_vtr
!-----------------------------------------------------------------------------------------------------------------------------------
!< VTK_Fortran test.
!-----------------------------------------------------------------------------------------------------------------------------------
use penf
use vtk_fortran, only : vtk_file
!-----------------------------------------------------------------------------------------------------------------------------------

!-----------------------------------------------------------------------------------------------------------------------------------
implicit none
type(vtk_file)          :: a_vtk_file                             !< A VTK file.
integer(I4P), parameter :: nx1=0_I4P                              !< X lower bound extent.
integer(I4P), parameter :: nx2=16_I4P                             !< X upper bound extent.
integer(I4P), parameter :: ny1=0_I4P                              !< Y lower bound extent.
integer(I4P), parameter :: ny2=16_I4P                             !< Y upper bound extent.
integer(I4P), parameter :: nz1=0_I4P                              !< Z lower bound extent.
integer(I4P), parameter :: nz2=16_I4P                             !< Z upper bound extent.
integer(I4P), parameter :: nn=(nx2-nx1+1)*(ny2-ny1+1)*(nz2-nz1+1) !< Number of elements.
real(R8P)               :: x(nx1:nx2)                             !< X coordinates.
real(R8P)               :: y(ny1:ny2)                             !< Y coordinates.
real(R8P)               :: z(nz1:nz2)                             !< Z coordinates.
integer(I4P)            :: v(1:nn)                                !< Variable defined at coordinates.
integer(I4P)            :: error                                  !< Status error.
integer(I4P)            :: i                                      !< Counter.
integer(I4P)            :: j                                      !< Counter.
integer(I4P)            :: k                                      !< Counter.
integer(I4P)            :: n                                      !< Counter.
real(R8P)               :: x1=-3._R8P                             !< X lower extent.
real(R8P)               :: x2=3._R8P                              !< X upper extent.
real(R8P)               :: y1=-2._R8P                             !< Y lower extent.
real(R8P)               :: y2=0.25_R8P                            !< Y upper extent.
real(R8P)               :: z1=-2._R8P                             !< Z lower extent.
real(R8P)               :: z2=0.16_R8P                            !< Z upper extent.
logical                 :: test_passed(1)                         !< List of passed tests.
!-----------------------------------------------------------------------------------------------------------------------------------

!-----------------------------------------------------------------------------------------------------------------------------------
n = 0
do k=nz1,nz2
 do j=ny1,ny2
   do i=nx1,nx2
     n = n + 1
     v(n) = i*j*k
   enddo
 enddo
enddo
do i=nx1,nx2
  x(i) = x1 + (i - 1) * (x2 - x1)/real(nx2 - nx1, kind=R8P)
enddo
do j=ny1,ny2
  y(j) = y1 + (j - 1) * (y2 - y1)/real(ny2 - ny1, kind=R8P)
enddo
do k=nz1,nz2
  z(k) = z1 + (k - 1) * (z2 - z1)/real(nz2 - nz1, kind=R8P)
enddo
error = a_vtk_file%initialize(format='binary', filename='XML_RECT-binary.vtr', mesh_topology='RectilinearGrid', &
                              nx1=nx1, nx2=nx2, ny1=ny1, ny2=ny2, nz1=nz1, nz2=nz2)
error = a_vtk_file%xml_writer%write_fielddata(action='open')
error = a_vtk_file%xml_writer%write_fielddata(x=0._R8P, data_name='TIME')
error = a_vtk_file%xml_writer%write_fielddata(x=1_I8P, data_name='CYCLE')
error = a_vtk_file%xml_writer%write_fielddata(action='close')
error = a_vtk_file%xml_writer%write_piece(nx1=nx1, nx2=nx2, ny1=ny1, ny2=ny2, nz1=nz1, nz2=nz2)
error = a_vtk_file%xml_writer%write_geo(x=x, y=y, z=z)
error = a_vtk_file%xml_writer%write_dataarray(location='cell', action='open')
error = a_vtk_file%xml_writer%write_dataarray(data_name='cell_value', x=v)
error = a_vtk_file%xml_writer%write_dataarray(location='cell', action='close')
error = a_vtk_file%xml_writer%write_piece()
error = a_vtk_file%finalize()

test_passed = .true. ! nothing to test yet

print "(A,L1)", new_line('a')//'Are all tests passed? ', all(test_passed)
stop
!-----------------------------------------------------------------------------------------------------------------------------------
endprogram write_vtr
