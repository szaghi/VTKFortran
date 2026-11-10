use vtk_fortran, only : vtk_file
use penf,        only : I4P, R8P

type(vtk_file) :: a_vtk_file
integer(I4P)   :: i, j, k, error
real(R8P)      :: phi(0:15,0:15,0:7)   ! point data, one value per grid point

do k=0, 7 ; do j=0, 15 ; do i=0, 15
  phi(i,j,k) = sin(0.4_R8P*i) * cos(0.4_R8P*j) + 0.1_R8P*k
enddo ; enddo ; enddo
error = a_vtk_file%initialize(format='raw', filename='ref_image.vti', mesh_topology='ImageData', &
                              nx1=0, nx2=15, ny1=0, ny2=15, nz1=0, nz2=7,                     &
                              origin=[0._R8P, 0._R8P, 0._R8P], spacing=[0.1_R8P, 0.1_R8P, 0.2_R8P])
error = a_vtk_file%xml_writer%write_piece(nx1=0, nx2=15, ny1=0, ny2=15, nz1=0, nz2=7)
error = a_vtk_file%xml_writer%write_dataarray(location='node', action='open')
error = a_vtk_file%xml_writer%write_dataarray(data_name='phi', x=phi, one_component=.true.)
error = a_vtk_file%xml_writer%write_dataarray(location='node', action='close')
error = a_vtk_file%xml_writer%write_piece()
error = a_vtk_file%finalize()
