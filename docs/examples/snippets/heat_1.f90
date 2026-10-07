program heat
!< Tutorial, chapter 1: the initial temperature of the cube, written as a rectilinear grid in ASCII.
use penf, only : I4P, R8P
use vtk_fortran, only : vtk_file
implicit none
integer(I4P), parameter :: n=24          ! points along each side of the unit cube
real(R8P)               :: x(n)          ! coordinates of the points along each axis
real(R8P)               :: t(n,n,n)      ! temperature at the points
type(vtk_file)          :: a_vtk_file
integer(I4P)            :: i, j, k, error

x = [(real(i - 1, R8P)/(n - 1), i=1, n)]
do k=1, n ; do j=1, n ; do i=1, n
  ! two hot blobs in a cold cube: the walls are kept at 0
  t(i,j,k) = blob(x(i), x(j), x(k), [0.35_R8P, 0.4_R8P, 0.5_R8P]) + 0.6_R8P*blob(x(i), x(j), x(k), [0.7_R8P, 0.65_R8P, 0.45_R8P])
enddo ; enddo ; enddo
t(1,:,:) = 0 ; t(n,:,:) = 0 ; t(:,1,:) = 0 ; t(:,n,:) = 0 ; t(:,:,1) = 0 ; t(:,:,n) = 0

error = a_vtk_file%initialize(format='ascii', filename='heat.vtr', mesh_topology='RectilinearGrid', &
                              nx1=1, nx2=n, ny1=1, ny2=n, nz1=1, nz2=n)
error = a_vtk_file%xml_writer%write_piece(nx1=1, nx2=n, ny1=1, ny2=n, nz1=1, nz2=n)
error = a_vtk_file%xml_writer%write_geo(x=x, y=x, z=x)
error = a_vtk_file%xml_writer%write_dataarray(location='node', action='open')
error = a_vtk_file%xml_writer%write_dataarray(data_name='temperature', x=t, one_component=.true.)
error = a_vtk_file%xml_writer%write_dataarray(location='node', action='close')
error = a_vtk_file%xml_writer%write_piece()
error = a_vtk_file%finalize()
print '(A,I0,A,F6.3)', 'heat.vtr: ', n**3, ' points, maximum temperature ', maxval(t)
contains
  pure function blob(x, y, z, centre) result(t)
  !< A hot blob: a Gaussian bump of temperature 1 at its centre.
  real(R8P), intent(in) :: x, y, z, centre(3)
  real(R8P)             :: t

  t = exp(-((x - centre(1))**2 + (y - centre(2))**2 + (z - centre(3))**2)/0.04_R8P)
  endfunction blob
endprogram heat
