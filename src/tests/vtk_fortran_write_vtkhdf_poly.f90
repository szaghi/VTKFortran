!< VTK_Fortran test: write PolyData in the VTKHDF format (`format='vtkhdf'`), pieces as partitions.
program vtk_fortran_write_vtkhdf_poly
!< VTK_Fortran test: write PolyData in the VTKHDF format (`format='vtkhdf'`), pieces as partitions.
!<
!< With HDF5 the test writes three meshes, each one also as a binary `.vtp` by the same calls (VTK readers compare the two
!< outside this test):
!<
!<- `vtkfortran_poly_all`: one piece with the four blocks of cells (a vertex and a poly-vertex, a line and a polyline, a
!<  triangle strip, a triangle and a quad), point and cell data, unsigned cell data, active arrays, field data;
!<- `vtkfortran_poly_pieces`: three pieces, each with some blocks only (vertices and polygons, then lines and strips,
!<  then points without cells), the same arrays in all of them;
!<- `vtkfortran_poly_i8p`: I8P counts, the points as `xyz(3,np)` (R4P), polygons with I8P ids and I4P offsets (converted
!<  to I8P), lines with I4P ids, compressed if HDF5 has deflate.
!<
!< The three-piece file is read back with the HDF5 bindings (counts of each block, offsets from 0, connectivity, arrays).
!< Refusals are checked too. Without HDF5 the writer refuses to start.
use penf
use vtk_fortran, only : vtk_file
use vtk_fortran_hdf5

implicit none
type(vtk_file)                :: a_vtk_file                 !< VTK file.
integer(hid_kind)             :: file_id, root              !< HDF5 identifiers.
character(len=:), allocatable :: text                       !< String read back.
integer(I8P),     allocatable :: ivalues(:), dims(:)        !< Values read back.
integer(I4P),     allocatable :: jvalues(:)                 !< Values read back.
integer(I4P)                  :: e(40)                      !< Errors of each call.
integer(I4P)                  :: f                          !< Counter.
logical                       :: test_passed(7)             !< List of passed tests.
character(16), parameter      :: formats(2) = ['vtkhdf          ', 'binary          '] !< Formats of each mesh.
character(7),  parameter      :: extensions(2) = ['.vtkhdf', '.vtp   ']             !< Extensions of each format.

test_passed = .false.
if (.not.is_hdf5_enabled) then
  test_passed = a_vtk_file%initialize(format='vtkhdf', filename='refused.vtkhdf', mesh_topology='PolyData') /= 0
  print '(A)', 'HDF5 not enabled: the VTKHDF writer refuses to start'
else
  e = 0
  do f=1, 2
    e(f) = write_all(trim(formats(f)), 'vtkfortran_poly_all'//trim(extensions(f)))
    e(2+f) = write_pieces(trim(formats(f)), 'vtkfortran_poly_pieces'//trim(extensions(f)))
    e(4+f) = write_i8p(trim(formats(f)), 'vtkfortran_poly_i8p'//trim(extensions(f)))
  enddo
  test_passed(1) = all(e(1:6) == 0)
  if (.not.test_passed(1)) print '(A,6I2)', 'errors of the meshes (vtkhdf, vtp for each): ', e(1:6)
  test_passed(2) = refusals()

  ! read back the three pieces
  e = 0
  e(1) = h5_file_open('vtkfortran_poly_pieces.vtkhdf', file_id)
  e(2) = h5_group_open(file_id, 'VTKHDF', root)
  e(3) = h5_attribute_read(root, 'Type', text)
  e(4) = h5_attribute_read(root, 'Version', ivalues)
  test_passed(3) = text == 'PolyData' .and. all(ivalues == [2_I8P, 0_I8P]) .and. .not.h5_link_exists(root, 'NumberOfCells')
  e(5) = h5_dataset_read(root, 'NumberOfPoints', ivalues)
  test_passed(4) = all(ivalues == [4_I8P, 4_I8P, 2_I8P])
  e(6) = h5_dataset_read(root, 'Vertices/NumberOfCells', ivalues)
  test_passed(4) = test_passed(4) .and. all(ivalues == [1_I8P, 0_I8P, 0_I8P])
  e(7) = h5_dataset_read(root, 'Lines/NumberOfCells', ivalues)
  test_passed(4) = test_passed(4) .and. all(ivalues == [0_I8P, 1_I8P, 0_I8P])
  e(8) = h5_dataset_read(root, 'Strips/NumberOfCells', ivalues)
  test_passed(4) = test_passed(4) .and. all(ivalues == [0_I8P, 1_I8P, 0_I8P])
  e(9) = h5_dataset_read(root, 'Polygons/NumberOfCells', ivalues)
  test_passed(4) = test_passed(4) .and. all(ivalues == [1_I8P, 0_I8P, 0_I8P])
  e(10) = h5_dataset_read(root, 'Strips/NumberOfConnectivityIds', ivalues)
  test_passed(4) = test_passed(4) .and. all(ivalues == [0_I8P, 4_I8P, 0_I8P])
  e(11) = h5_dataset_read(root, 'Vertices/Offsets', ivalues)
  test_passed(5) = all(ivalues == [0_I8P, 1_I8P, 0_I8P, 0_I8P])
  e(12) = h5_dataset_read(root, 'Polygons/Offsets', ivalues)
  test_passed(5) = test_passed(5) .and. all(ivalues == [0_I8P, 3_I8P, 0_I8P, 0_I8P])
  e(13) = h5_dataset_read(root, 'Lines/Offsets', ivalues)
  test_passed(5) = test_passed(5) .and. all(ivalues == [0_I8P, 0_I8P, 3_I8P, 0_I8P])
  e(14) = h5_dataset_read(root, 'Polygons/Connectivity', ivalues)
  test_passed(5) = test_passed(5) .and. all(ivalues == [1_I8P, 2_I8P, 3_I8P])
  e(15) = h5_dataset_read(root, 'Strips/Connectivity', ivalues)
  test_passed(5) = test_passed(5) .and. all(ivalues == [0_I8P, 1_I8P, 2_I8P, 3_I8P])
  e(16) = h5_dataset_dims(root, 'Points', dims)
  test_passed(6) = all(dims == [3_I8P, 10_I8P])
  e(17) = h5_dataset_read(root, 'CellData/cid', jvalues)
  test_passed(6) = test_passed(6) .and. all(jvalues == [1_I4P, 2_I4P, 101_I4P, 102_I4P])
  e(18) = h5_dataset_read(root, 'PointData/pd', jvalues)
  test_passed(6) = test_passed(6) .and. all(jvalues == [1, 2, 3, 4, 101, 102, 103, 104, 201, 202])
  e(19) = h5_group_close(root)
  e(20) = h5_file_close(file_id)
  test_passed(7) = all(e(1:20) == 0)
  print '(A,7L2)', 'VTKHDF PolyData: meshes, refusals, header, counts, cells, points+data, reads:', test_passed
endif

print "(A,L1)", new_line('a')//'Are all tests passed? ', all(test_passed)
if (.not.all(test_passed)) error stop 'some tests failed'
stop
contains
  function write_all(format, filename) result(error)
  !< One piece with the four blocks of cells, on 10 points of the plane z=0.
  character(*), intent(in) :: format   !< Format.
  character(*), intent(in) :: filename !< File name.
  integer(I4P)             :: error    !< Error status.
  integer(I4P), parameter  :: np=10, nc=7
  real(R8P)                :: x(np), y(np), z(np)
  integer(I4P)             :: p
  integer(I1P)             :: ghost(nc)
  real(R4P)                :: pv(3,np)

  x = [0, 1, 2, 0, 1, 2, 0, 1, 0, 1] ; y = [0, 0, 0, 1, 1, 1, 2, 2, 3, 3] ; z = 0._R8P
  ghost = [0_I1P, 0_I1P, 0_I1P, 0_I1P, 0_I1P, 0_I1P, -56_I1P] ! the last one is 200 as unsigned
  do p=1, np
    pv(:,p) = [real(x(p), R4P), real(y(p), R4P), real(p, R4P)]
  enddo
  error = a_vtk_file%initialize(format=format, filename=filename, mesh_topology='PolyData')
  error = max(error, abs(a_vtk_file%xml_writer%write_fielddata(action='open')))
  error = max(error, abs(a_vtk_file%xml_writer%write_fielddata(data_name='time', x=2.5_R8P)))
  error = max(error, abs(a_vtk_file%xml_writer%write_fielddata(action='close')))
  error = max(error, abs(a_vtk_file%xml_writer%write_piece(np=np, nverts=2, nlines=2, nstrips=1, npolys=2)))
  error = max(error, abs(a_vtk_file%xml_writer%write_geo(np=np, nc=nc, x=x, y=y, z=z)))
  error = max(error, abs(a_vtk_file%xml_writer%write_polydata_cells(                               &
                         verts_connectivity=[0, 1, 2], verts_offset=[1, 3],                        & ! vertex, poly-vertex
                         lines_connectivity=[3, 4, 4, 5, 2], lines_offset=[2, 5],                  & ! line, polyline
                         strips_connectivity=[6, 7, 8, 9], strips_offset=[4],                      & ! triangle strip
                         polys_connectivity=[0, 1, 4, 1, 2, 5, 4], polys_offset=[3, 7])))            ! triangle, quad
  error = max(error, abs(a_vtk_file%xml_writer%write_dataarray(location='node', action='open', scalars='pd', vectors='pv')))
  error = max(error, abs(a_vtk_file%xml_writer%write_dataarray(data_name='pd', x=[(real(p, R8P), p=1, np)])))
  error = max(error, abs(a_vtk_file%xml_writer%write_dataarray(data_name='pv', x=pv)))
  error = max(error, abs(a_vtk_file%xml_writer%write_dataarray(location='node', action='close')))
  error = max(error, abs(a_vtk_file%xml_writer%write_dataarray(location='cell', action='open', scalars='cid')))
  error = max(error, abs(a_vtk_file%xml_writer%write_dataarray(data_name='cid', x=[(p, p=1, nc)])))
  error = max(error, abs(a_vtk_file%xml_writer%write_dataarray_unsigned(data_name='ghost', x=ghost)))
  error = max(error, abs(a_vtk_file%xml_writer%write_dataarray(location='cell', action='close')))
  error = max(error, abs(a_vtk_file%xml_writer%write_piece()))
  error = max(error, abs(a_vtk_file%finalize()))
  endfunction write_all

  function write_pieces(format, filename) result(error)
  !< Three pieces: a vertex and a triangle (4 points), a polyline and a triangle strip (4 points), 2 points without cells;
  !< point ids from 0 in each piece.
  character(*), intent(in) :: format   !< Format.
  character(*), intent(in) :: filename !< File name.
  integer(I4P)             :: error    !< Error status.

  error = a_vtk_file%initialize(format=format, filename=filename, mesh_topology='PolyData')
  ! piece 1: vertices and polygons
  error = max(error, abs(a_vtk_file%xml_writer%write_piece(np=4, nverts=1, nlines=0, nstrips=0, npolys=1)))
  error = max(error, abs(a_vtk_file%xml_writer%write_geo(np=4, nc=2, x=[0._R8P, 1._R8P, 2._R8P, 1.5_R8P], &
                                                         y=[0._R8P, 0._R8P, 0._R8P, 1._R8P], z=[0._R8P, 0._R8P, 0._R8P, 0._R8P])))
  error = max(error, abs(a_vtk_file%xml_writer%write_polydata_cells(verts_connectivity=[0], verts_offset=[1], &
                                                                    polys_connectivity=[1, 2, 3], polys_offset=[3])))
  error = max(error, abs(data(4, 2, 0, 0)))
  ! piece 2: lines and strips; nc of write_geo is not used by a PolyData (as in the XML formats): 0 here
  error = max(error, abs(a_vtk_file%xml_writer%write_piece(np=4, nverts=0, nlines=1, nstrips=1, npolys=0)))
  error = max(error, abs(a_vtk_file%xml_writer%write_geo(np=4, nc=0, x=[0._R8P, 1._R8P, 0._R8P, 1._R8P], &
                                                         y=[2._R8P, 2._R8P, 3._R8P, 3._R8P], z=[0._R8P, 0._R8P, 0._R8P, 0._R8P])))
  error = max(error, abs(a_vtk_file%xml_writer%write_polydata_cells(lines_connectivity=[0, 1, 2], lines_offset=[3],      &
                                                                    strips_connectivity=[0, 1, 2, 3], strips_offset=[4])))
  error = max(error, abs(data(4, 2, 100, 100)))
  ! piece 3: points without cells (no write_polydata_cells)
  error = max(error, abs(a_vtk_file%xml_writer%write_piece(np=2, nverts=0, nlines=0, nstrips=0, npolys=0)))
  error = max(error, abs(a_vtk_file%xml_writer%write_geo(np=2, nc=0, x=[3._R8P, 4._R8P], y=[0._R8P, 0._R8P], &
                                                         z=[0._R8P, 0._R8P])))
  error = max(error, abs(data(2, 0, 200, 200)))
  error = max(error, abs(a_vtk_file%finalize()))
  endfunction write_pieces

  function data(np, nc, point_base, cell_base) result(error)
  !< Write the point data pd (point_base + point number) and cell data cid (cell_base + cell number), then end the piece.
  integer(I4P), intent(in) :: np, nc, point_base, cell_base
  integer(I4P)             :: error, p

  error = a_vtk_file%xml_writer%write_dataarray(location='node', action='open')
  error = max(error, abs(a_vtk_file%xml_writer%write_dataarray(data_name='pd', x=[(point_base + p, p=1, np)])))
  error = max(error, abs(a_vtk_file%xml_writer%write_dataarray(location='node', action='close')))
  error = max(error, abs(a_vtk_file%xml_writer%write_dataarray(location='cell', action='open')))
  error = max(error, abs(a_vtk_file%xml_writer%write_dataarray(data_name='cid', x=[(cell_base + p, p=1, nc)])))
  error = max(error, abs(a_vtk_file%xml_writer%write_dataarray(location='cell', action='close')))
  error = max(error, abs(a_vtk_file%xml_writer%write_piece()))
  endfunction data

  function write_i8p(format, filename) result(error)
  !< A polyline and a quad, I8P counts, R4P points as xyz(3,np), polygons with I8P ids and I4P offsets, compressed if
  !< possible.
  character(*), intent(in) :: format   !< Format.
  character(*), intent(in) :: filename !< File name.
  integer(I4P)             :: error    !< Error status.
  integer(I8P), parameter  :: np=5_I8P
  real(R4P)                :: xyz(3,np)

  xyz = reshape([0., 0., 0.,  1., 0., 0.,  1., 1., 0.,  0., 1., 0.,  0.5, 0.5, 1.], [3, 5])
  if (format == 'vtkhdf' .and. h5_deflate_available()) then
    error = a_vtk_file%initialize(format=format, filename=filename, mesh_topology='PolyData', compressor='zlib')
  else
    error = a_vtk_file%initialize(format=format, filename=filename, mesh_topology='PolyData')
  endif
  error = max(error, abs(a_vtk_file%xml_writer%write_piece(np=np, nverts=0_I8P, nlines=1_I8P, nstrips=0_I8P, npolys=1_I8P)))
  error = max(error, abs(a_vtk_file%xml_writer%write_geo(np=np, nc=2_I8P, xyz=xyz)))
  error = max(error, abs(a_vtk_file%xml_writer%write_polydata_cells(lines_connectivity=[0, 4, 2], lines_offset=[3], &
                                                                    polys_connectivity=[0_I8P, 1_I8P, 2_I8P, 3_I8P], &
                                                                    polys_offset=[4])))
  error = max(error, abs(a_vtk_file%xml_writer%write_dataarray(location='node', action='open')))
  error = max(error, abs(a_vtk_file%xml_writer%write_dataarray(data_name='height', x=xyz(3,:))))
  error = max(error, abs(a_vtk_file%xml_writer%write_dataarray(location='node', action='close')))
  error = max(error, abs(a_vtk_file%xml_writer%write_piece()))
  error = max(error, abs(a_vtk_file%finalize()))
  endfunction write_i8p

  logical function refusals()
  !< The refusals of the PolyData writer, each one a non-zero error.
  integer(I4P) :: error
  logical      :: ok(8)
  real(R8P)    :: one_point(1) = [0._R8P]

  ok = .false.
  ! the cells of a PolyData in an UnstructuredGrid
  error = a_vtk_file%initialize(format='vtkhdf', filename='refused_unst.vtkhdf', mesh_topology='UnstructuredGrid')
  error = a_vtk_file%xml_writer%write_piece(np=1, nc=1)
  error = a_vtk_file%xml_writer%write_geo(np=1, nc=1, x=one_point, y=one_point, z=one_point)
  ok(1) = a_vtk_file%xml_writer%write_polydata_cells(verts_connectivity=[0], verts_offset=[1]) /= 0
  error = a_vtk_file%finalize()
  ! a piece of an UnstructuredGrid, then the cells of an UnstructuredGrid, in a PolyData
  error = a_vtk_file%initialize(format='vtkhdf', filename='refused_poly.vtkhdf', mesh_topology='PolyData')
  ok(2) = a_vtk_file%xml_writer%write_piece(np=1, nc=1) /= 0
  error = a_vtk_file%xml_writer%write_piece(np=1, nverts=2, nlines=1, nstrips=0, npolys=0)
  error = a_vtk_file%xml_writer%write_geo(np=1, nc=3, x=one_point, y=one_point, z=one_point)
  ok(3) = a_vtk_file%xml_writer%write_connectivity(nc=1, connectivity=[0], offset=[1], cell_type=[1_I1P]) /= 0
  ! a block with cells not given; a block with one offset short; a connectivity without offsets; real ids
  ok(4) = a_vtk_file%xml_writer%write_polydata_cells(verts_connectivity=[0, 0], verts_offset=[1, 2]) /= 0
  ok(5) = a_vtk_file%xml_writer%write_polydata_cells(verts_connectivity=[0, 0], verts_offset=[1],     &
                                                     lines_connectivity=[0, 0], lines_offset=[2]) /= 0
  ok(6) = a_vtk_file%xml_writer%write_polydata_cells(verts_connectivity=[0, 0],                       &
                                                     lines_connectivity=[0, 0], lines_offset=[2]) /= 0
  ok(7) = a_vtk_file%xml_writer%write_polydata_cells(verts_connectivity=[0._R8P, 0._R8P], verts_offset=[1, 2], &
                                                     lines_connectivity=[0, 0], lines_offset=[2]) /= 0
  ! nothing written by the refused calls: the piece, with cells, ends without them
  ok(8) = a_vtk_file%xml_writer%write_piece() /= 0
  error = a_vtk_file%finalize()
  refusals = all(ok)
  if (.not.refusals) print '(A,8L2)', 'refusals: ', ok
  endfunction refusals
endprogram vtk_fortran_write_vtkhdf_poly
