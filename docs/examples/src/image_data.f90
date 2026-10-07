!run image_data image_data
!render image_data wave.vti array=wave size=560x340 zoom=1.05
program image_data
!< Write a regular grid (ImageData): no coordinates, just an origin and a spacing.
use penf, only : I4P, R8P
use vtk_fortran, only : vtk_file
implicit none
integer(I4P), parameter :: nx=40, ny=30, nz=10
real(R8P)               :: wave(0:nx,0:ny,0:nz)
type(vtk_file)          :: a_vtk_file
integer(I4P)            :: i, j, k, error

do k=0, nz ; do j=0, ny ; do i=0, nx
  wave(i,j,k) = sin(0.3_R8P*i)*cos(0.4_R8P*j) + 0.1_R8P*k
enddo ; enddo ; enddo
error = a_vtk_file%initialize(format='raw', filename='wave.vti', mesh_topology='ImageData', &
                              nx1=0, nx2=nx, ny1=0, ny2=ny, nz1=0, nz2=nz,                  &
                              origin=[0._R8P, 0._R8P, 0._R8P], spacing=[0.1_R8P, 0.1_R8P, 0.1_R8P])
error = a_vtk_file%xml_writer%write_piece(nx1=0, nx2=nx, ny1=0, ny2=ny, nz1=0, nz2=nz)
error = a_vtk_file%xml_writer%write_dataarray(location='node', action='open')
error = a_vtk_file%xml_writer%write_dataarray(data_name='wave', x=wave, one_component=.true.)
error = a_vtk_file%xml_writer%write_dataarray(location='node', action='close')
error = a_vtk_file%xml_writer%write_piece()
error = a_vtk_file%finalize()
print '(A,I0)', 'wave.vti written, error ', error
endprogram image_data
