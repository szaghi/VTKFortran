use vtk_fortran, only : vtk_file
use penf,        only : I4P, R8P

type(vtk_file) :: a_vtk_file
integer(I4P)   :: error
real(R8P)      :: x(7), y(7), z(7)   ! a square (points 0-3) and a polyline (points 4-6)

x = [0, 1, 1, 0, 0, 1, 2] ; y = [0, 0, 1, 1, 2, 3, 2] ; z = 0
error = a_vtk_file%initialize(format='raw', filename='ref_polydata.vtp', mesh_topology='PolyData')
error = a_vtk_file%xml_writer%write_piece(np=7, nverts=0, nlines=1, nstrips=0, npolys=2)
error = a_vtk_file%xml_writer%write_geo(np=7, nc=3, x=x, y=y, z=z)
error = a_vtk_file%xml_writer%write_polydata_cells(lines_connectivity=[4,5,6], lines_offset=[3],          &
                                                    polys_connectivity=[0,1,2, 0,2,3], polys_offset=[3,6])
error = a_vtk_file%xml_writer%write_dataarray(location='cell', action='open')
error = a_vtk_file%xml_writer%write_dataarray(data_name='id', x=[1,2,3])   ! the polyline, then the two triangles
error = a_vtk_file%xml_writer%write_dataarray(location='cell', action='close')
error = a_vtk_file%xml_writer%write_piece()
error = a_vtk_file%finalize()
