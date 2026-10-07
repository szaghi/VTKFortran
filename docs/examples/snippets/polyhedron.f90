program polyhedron
!< Write an unstructured grid of different cells: a polyhedron (a cube described by its faces), a tetrahedron, a wedge.
use penf, only : I1P, I4P, R8P
use vtk_fortran, only : vtk_file
implicit none
real(R8P),    parameter :: x(18)=[real(R8P) :: 0,1,1,0,0,1,1,0, 1.5,2.5,1.5,1.5, 3,4,3,3,4,3]
real(R8P),    parameter :: y(18)=[real(R8P) :: 0,0,1,1,0,0,1,1, 0,0,1,0, 0,0,1,0,0,1]
real(R8P),    parameter :: z(18)=[real(R8P) :: 0,0,0,0,1,1,1,1, 0,0,0,1, 0,0,0,1,1,1]
integer(I4P), parameter :: connect(18)=[0,1,2,3,4,5,6,7, 8,9,10,11, 12,13,14,15,16,17]
integer(I4P), parameter :: offset(3)=[8, 12, 18]
integer(I1P), parameter :: cell_type(3)=[42_I1P, 10_I1P, 13_I1P] ! polyhedron, tetrahedron, wedge
! the faces of the polyhedron: their number, then for each face its number of points and their ids
integer(I4P), parameter :: face(31)=[6, 4,0,1,2,3, 4,4,5,6,7, 4,0,1,5,4, 4,1,2,6,5, 4,2,3,7,6, 4,3,0,4,7]
integer(I4P), parameter :: faceoffset(3)=[31, -1, -1] ! the end of the faces of each cell, -1 if not a polyhedron
type(vtk_file)          :: a_vtk_file
integer(I4P)            :: error

error = a_vtk_file%initialize(format='ascii', filename='cells.vtu', mesh_topology='UnstructuredGrid')
error = a_vtk_file%xml_writer%write_piece(np=18, nc=3)
error = a_vtk_file%xml_writer%write_geo(np=18, nc=3, x=x, y=y, z=z)
error = a_vtk_file%xml_writer%write_connectivity(nc=3, connectivity=connect, offset=offset, cell_type=cell_type, &
                                                 face=face, faceoffset=faceoffset)
error = a_vtk_file%xml_writer%write_dataarray(location='cell', action='open')
error = a_vtk_file%xml_writer%write_dataarray(data_name='cell_type', x=int(cell_type, I4P))
error = a_vtk_file%xml_writer%write_dataarray(location='cell', action='close')
error = a_vtk_file%xml_writer%write_piece()
error = a_vtk_file%finalize()
print '(A,I0)', 'cells.vtu written, error ', error
endprogram polyhedron
