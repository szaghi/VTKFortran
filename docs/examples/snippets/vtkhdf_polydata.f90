program vtkhdf_polydata
!< Write a PolyData in two pieces into one VTKHDF file: each piece a partition, its cells in the four groups Vertices,
!< Lines, Strips and Polygons. Then print the cells of each partition by group, read back with the HDF5 bindings of the
!< library (VTKFortran does not read VTKHDF yet).
use penf, only : I4P, I8P, R8P
use vtk_fortran, only : vtk_file
use vtk_fortran_hdf5
implicit none
real(R8P), parameter      :: pi = acos(-1._R8P)
type(vtk_file)            :: a_vtk_file
real(R8P)                 :: angle(6)
integer(I4P)              :: i, error
integer(hid_kind)         :: file_id, root
integer(I8P), allocatable :: values(:)
character(8), parameter   :: groups(4) = ['Vertices', 'Lines   ', 'Strips  ', 'Polygons']
character(24)             :: label

angle = [(pi / 3 * i, i=0, 5)]
error = a_vtk_file%initialize(format='vtkhdf', filename='shapes.vtkhdf', mesh_topology='PolyData')
! piece 1: a hexagon and a vertex on each corner
error = a_vtk_file%xml_writer%write_piece(np=6, nverts=6, nlines=0, nstrips=0, npolys=1)
error = a_vtk_file%xml_writer%write_geo(np=6, nc=7, x=cos(angle), y=sin(angle), z=[(0._R8P, i=1, 6)])
error = a_vtk_file%xml_writer%write_polydata_cells(verts_connectivity=[(i, i=0, 5)], verts_offset=[(i, i=1, 6)], &
                                                   polys_connectivity=[(i, i=0, 5)], polys_offset=[6])
error = a_vtk_file%xml_writer%write_dataarray(location='cell', action='open')
error = a_vtk_file%xml_writer%write_dataarray(data_name='block', x=[(1, i=1, 7)])
error = a_vtk_file%xml_writer%write_dataarray(location='cell', action='close')
error = a_vtk_file%xml_writer%write_piece()
! piece 2: a ribbon (a triangle strip of 10 points) and a polyline above it (5 points)
error = a_vtk_file%xml_writer%write_piece(np=15, nverts=0, nlines=1, nstrips=1, npolys=0)
error = a_vtk_file%xml_writer%write_geo(np=15, nc=2, x=[(2._R8P + i / 2, i=0, 9), (2._R8P + i, i=0, 4)],          &
                                        y=[(-0.4_R8P + 0.8_R8P * mod(i, 2), i=0, 9), (1.3_R8P, i=0, 4)], &
                                        z=[(0._R8P, i=1, 15)])
error = a_vtk_file%xml_writer%write_polydata_cells(lines_connectivity=[(i, i=10, 14)], lines_offset=[5], &
                                                   strips_connectivity=[(i, i=0, 9)], strips_offset=[10])
error = a_vtk_file%xml_writer%write_dataarray(location='cell', action='open')
error = a_vtk_file%xml_writer%write_dataarray(data_name='block', x=[2, 2])
error = a_vtk_file%xml_writer%write_dataarray(location='cell', action='close')
error = a_vtk_file%xml_writer%write_piece()
error = a_vtk_file%finalize()
if (error /= 0) error stop 'write error'

error = h5_file_open('shapes.vtkhdf', file_id)
if (error == 0) error = h5_group_open(file_id, 'VTKHDF', root)
if (error /= 0) error stop 'read error'
print '(A)', 'shapes.vtkhdf, the cells of each partition by group:'
error = h5_dataset_read(root, 'NumberOfPoints', values)
label = '  NumberOfPoints'
print '(A,*(1X,I2))', label, values
do i=1, 4
  error = h5_dataset_read(root, trim(groups(i))//'/NumberOfCells', values)
  label = '  '//trim(groups(i))//'/NumberOfCells'
  print '(A,*(1X,I2))', label, values
enddo
error = h5_group_close(root)
error = h5_file_close(file_id)
endprogram vtkhdf_polydata
