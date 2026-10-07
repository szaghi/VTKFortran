!run polydata polydata
!render polydata shapes.vtp array=id categorical=1 edges=1 points=10 camera=xy size=560x340 zoom=1.2
program polydata
!< Write polygonal data: vertices, a polyline and two polygons, with one value per cell.
use penf, only : I4P, R8P
use vtk_fortran, only : vtk_file
implicit none
real(R8P)      :: x(12), y(12), z(12)
type(vtk_file) :: a_vtk_file
integer(I4P)   :: error

x = [real(R8P) :: 0, 1, 2,   0, 1, 2, 3,   0, 1, 1,   2, 3]
y = [real(R8P) :: 3, 3, 3,   2, 2.5, 2, 2.5,   0, 0, 1,   0, 1]
z = 0
error = a_vtk_file%initialize(format='raw', filename='shapes.vtp', mesh_topology='PolyData')
error = a_vtk_file%xml_writer%write_piece(np=12, nverts=3, nlines=1, nstrips=0, npolys=2)
error = a_vtk_file%xml_writer%write_geo(np=12, nc=6, x=x, y=y, z=z)
error = a_vtk_file%xml_writer%write_polydata_cells(verts_connectivity=[0, 1, 2], verts_offset=[1, 2, 3],   &
                                                   lines_connectivity=[3, 4, 5, 6], lines_offset=[4],      &
                                                   polys_connectivity=[7, 8, 9, 8, 10, 11, 9], polys_offset=[3, 7])
! one value per cell, in the order vertices, lines, strips, polygons
error = a_vtk_file%xml_writer%write_dataarray(location='cell', action='open')
error = a_vtk_file%xml_writer%write_dataarray(data_name='id', x=[1, 1, 1, 2, 3, 4])
error = a_vtk_file%xml_writer%write_dataarray(location='cell', action='close')
error = a_vtk_file%xml_writer%write_piece()
error = a_vtk_file%finalize()
print '(A,I0)', 'shapes.vtp written, error ', error
endprogram polydata
