!run vectors_tensors vectors_tensors
!run vectors_tensors-inspect inspect fields.vtu
program vectors_tensors
!< Write scalars, vectors and symmetric tensors at the points of a tetrahedron.
use penf, only : I1P, I4P, R8P
use vtk_fortran, only : vtk_file
implicit none
real(R8P), parameter :: x(4)=[0, 1, 0, 0], y(4)=[0, 0, 1, 0], z(4)=[0, 0, 0, 1]
real(R8P)            :: p(4), u(4), v(4), w(4), s(6,4)
type(vtk_file)       :: a_vtk_file
integer(I4P)         :: error

p = [1, 2, 3, 4]                         ! a scalar
u = 1 ; v = x ; w = -y                   ! a vector, by components
s(:,1) = [1, 2, 3, 0, 0, 0] ; s(:,2) = 2*s(:,1) ; s(:,3) = 3*s(:,1) ; s(:,4) = 4*s(:,1) ! xx yy zz xy yz xz
error = a_vtk_file%initialize(format='raw', filename='fields.vtu', mesh_topology='UnstructuredGrid')
error = a_vtk_file%xml_writer%write_piece(np=4, nc=1)
error = a_vtk_file%xml_writer%write_geo(np=4, nc=1, x=x, y=y, z=z)
error = a_vtk_file%xml_writer%write_connectivity(nc=1, connectivity=[0, 1, 2, 3], offset=[4], cell_type=[10_I1P])
error = a_vtk_file%xml_writer%write_dataarray(location='node', action='open', scalars='pressure', vectors='velocity', &
                                              tensors='stress')
error = a_vtk_file%xml_writer%write_dataarray(data_name='pressure', x=p)
error = a_vtk_file%xml_writer%write_dataarray(data_name='velocity', x=u, y=v, z=w)
error = a_vtk_file%xml_writer%write_dataarray(data_name='stress', x=s)   ! rank 2: (components, points)
error = a_vtk_file%xml_writer%write_dataarray(location='node', action='close')
error = a_vtk_file%xml_writer%write_piece()
error = a_vtk_file%finalize()
endprogram vectors_tensors
