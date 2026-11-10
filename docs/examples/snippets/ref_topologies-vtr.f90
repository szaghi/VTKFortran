use vtk_fortran, only : vtk_file
use penf,        only : I4P, I8P, R8P

type(vtk_file)          :: a_vtk_file
integer(I4P), parameter :: nx1=0, nx2=8, ny1=0, ny2=4, nz1=0, nz2=4
integer(I4P), parameter :: nn=(nx2-nx1+1)*(ny2-ny1+1)*(nz2-nz1+1)
real(R8P)               :: x(nx1:nx2), y(ny1:ny2), z(nz1:nz2)
integer(I4P)            :: v(1:nn)   ! one value per point
integer(I4P)            :: i, error

x = [(0.5_R8P*i, i=nx1, nx2)] ; y = [(0.5_R8P*i, i=ny1, ny2)] ; z = [(0.5_R8P*i, i=nz1, nz2)]
v = [(i, i=1, nn)]
error = a_vtk_file%initialize(format='binary', filename='ref_rectilinear.vtr', &
                              mesh_topology='RectilinearGrid',                  &
                              nx1=nx1, nx2=nx2, ny1=ny1, ny2=ny2, nz1=nz1, nz2=nz2)
! optional global field data
error = a_vtk_file%xml_writer%write_fielddata(action='open')
error = a_vtk_file%xml_writer%write_fielddata(x=0._R8P, data_name='TIME')
error = a_vtk_file%xml_writer%write_fielddata(x=1_I8P,  data_name='CYCLE')
error = a_vtk_file%xml_writer%write_fielddata(action='close')

error = a_vtk_file%xml_writer%write_piece(nx1=nx1, nx2=nx2, ny1=ny1, ny2=ny2, nz1=nz1, nz2=nz2)
error = a_vtk_file%xml_writer%write_geo(x=x, y=y, z=z)
error = a_vtk_file%xml_writer%write_dataarray(location='node', action='open')
error = a_vtk_file%xml_writer%write_dataarray(data_name='node_value', x=v)
error = a_vtk_file%xml_writer%write_dataarray(location='node', action='close')
error = a_vtk_file%xml_writer%write_piece()
error = a_vtk_file%finalize()
