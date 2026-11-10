use vtk_fortran, only : vtk_file
use penf,        only : I1P, I4P, I8P, R8P

type(vtk_file)                :: a_vtk_file
character(len=:), allocatable :: topology, names(:)
real(R8P),        allocatable :: x(:), y(:), z(:), pressure(:), velocity(:,:)
integer(I4P),     allocatable :: connectivity(:), offset(:)
integer(I1P),     allocatable :: cell_type(:)
integer(I8P)                  :: np, nc
integer(I4P)                  :: error

error = a_vtk_file%initialize(filename='ref_mesh.vtu', action='read')
error = a_vtk_file%xml_reader%get_info(mesh_topology=topology)
error = a_vtk_file%xml_reader%read_piece(np=np, nc=nc)
error = a_vtk_file%xml_reader%read_geo(x=x, y=y, z=z)
error = a_vtk_file%xml_reader%read_connectivity(connectivity=connectivity, offset=offset, cell_type=cell_type)
error = a_vtk_file%xml_reader%get_dataarray_names(location='node', names=names)
error = a_vtk_file%xml_reader%read_dataarray(location='node', data_name='pressure', x=pressure)
error = a_vtk_file%xml_reader%read_dataarray(location='node', data_name='velocity', x=velocity) ! shape (3, np)
error = a_vtk_file%finalize()
