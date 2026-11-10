use vtk_fortran, only : vtk_file
use penf,        only : I1P, I4P, R4P, R8P

type(vtk_file)          :: a_vtk_file
integer(I4P), parameter :: np = 9    ! number of points: a unit cube and the apex of a pyramid on top of it
integer(I4P), parameter :: nc = 2    ! number of cells: a hexahedron and a pyramid
real(R4P),    parameter :: x(np) = [0, 1, 1, 0, 0, 1, 1, 0, 0]
real(R4P),    parameter :: y(np) = [0, 0, 1, 1, 0, 0, 1, 1, 0]
real(R4P),    parameter :: z(np) = [0, 0, 0, 0, 1, 1, 1, 1, 2]
integer(I4P), parameter :: connect(13)  = [0,1,2,3,4,5,6,7, 4,5,6,7,8]   ! the point ids of each cell, 0-based
integer(I4P), parameter :: offset(nc)   = [8, 13]                        ! the end of each cell in connect
integer(I1P), parameter :: cell_type(nc) = [12_I1P, 14_I1P]               ! VTK_HEXAHEDRON, VTK_PYRAMID
real(R8P)               :: v(np)              ! scalar at the points
real(R8P)               :: vx(np), vy(np), vz(np) ! vector at the points
integer(I4P)            :: error

v = real(z, R8P) ; vx = 0._R8P ; vy = 0._R8P ; vz = 1._R8P
error = a_vtk_file%initialize(format='binary', filename='ref_unstructured.vtu', mesh_topology='UnstructuredGrid')
error = a_vtk_file%xml_writer%write_piece(np=np, nc=nc)
error = a_vtk_file%xml_writer%write_geo(np=np, nc=nc, x=x, y=y, z=z)
error = a_vtk_file%xml_writer%write_connectivity(nc=nc, connectivity=connect, offset=offset, cell_type=cell_type)
error = a_vtk_file%xml_writer%write_dataarray(location='node', action='open')
error = a_vtk_file%xml_writer%write_dataarray(data_name='scalars', x=v)
error = a_vtk_file%xml_writer%write_dataarray(data_name='vector', x=vx, y=vy, z=vz)
error = a_vtk_file%xml_writer%write_dataarray(location='node', action='close')
error = a_vtk_file%xml_writer%write_piece()
error = a_vtk_file%finalize()
