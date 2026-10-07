program curvilinear
!< Write a curvilinear grid (StructuredGrid): every point has its own coordinates, here a quarter of an annulus.
use penf, only : I4P, R8P
use vtk_fortran, only : vtk_file
implicit none
integer(I4P), parameter :: nr=8, nt=24, nz=2
real(R8P),    parameter :: pi=acos(-1._R8P)
real(R8P)               :: x(nr,nt,nz), y(nr,nt,nz), z(nr,nt,nz), r(nr,nt,nz)
type(vtk_file)          :: a_vtk_file
integer(I4P)            :: i, j, k, error

do k=1, nz ; do j=1, nt ; do i=1, nr
  r(i,j,k) = 1 + (i - 1)/real(nr - 1, R8P)
  x(i,j,k) = r(i,j,k)*cos(pi/2*(j - 1)/(nt - 1))
  y(i,j,k) = r(i,j,k)*sin(pi/2*(j - 1)/(nt - 1))
  z(i,j,k) = 0.1_R8P*(k - 1)
enddo ; enddo ; enddo
error = a_vtk_file%initialize(format='binary', filename='annulus.vts', mesh_topology='StructuredGrid', &
                              nx1=1, nx2=nr, ny1=1, ny2=nt, nz1=1, nz2=nz)
error = a_vtk_file%xml_writer%write_piece(nx1=1, nx2=nr, ny1=1, ny2=nt, nz1=1, nz2=nz)
error = a_vtk_file%xml_writer%write_geo(n=nr*nt*nz, x=x, y=y, z=z)
error = a_vtk_file%xml_writer%write_dataarray(location='node', action='open')
error = a_vtk_file%xml_writer%write_dataarray(data_name='radius', x=r, one_component=.true.)
error = a_vtk_file%xml_writer%write_dataarray(location='node', action='close')
error = a_vtk_file%xml_writer%write_piece()
error = a_vtk_file%finalize()
print '(A,I0)', 'annulus.vts written, error ', error
endprogram curvilinear
