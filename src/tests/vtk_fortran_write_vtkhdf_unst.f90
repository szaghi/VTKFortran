!< VTK_Fortran test: write UnstructuredGrid in the VTKHDF format (`format='vtkhdf'`), pieces as partitions.
program vtk_fortran_write_vtkhdf_unst
!< VTK_Fortran test: write UnstructuredGrid in the VTKHDF format (`format='vtkhdf'`), pieces as partitions.
!<
!< With HDF5 the test writes three meshes, each one also as a binary `.vtu` by the same calls (VTK readers compare the two
!< outside this test):
!<
!<- `vtkfortran_unst_mixed`: one piece of mixed cells (vertex, line, quadratic edge, triangle, quad, tetra, hexahedron,
!<  wedge, pyramid), point and cell data, unsigned cell data, active arrays, field data;
!<- `vtkfortran_unst_pieces`: two pieces (a hexahedron, then a pyramid and a tetra), the same arrays in both;
!<- `vtkfortran_unst_i8p`: I8P counts and ids, the points as `xyz(3,np)` (R4P), compressed if HDF5 has deflate.
!<
!< The two-piece file is read back with the HDF5 bindings (counts, offsets from 0, connectivity, types, points, arrays).
!< Refusals are checked too: an array missing from a piece, an array new after the first piece, a piece without cells,
!< polyhedra, points of the wrong size. Without HDF5 the writer refuses to start.
use penf
use vtk_fortran, only : vtk_file
use vtk_fortran_hdf5

implicit none
type(vtk_file)                :: a_vtk_file                 !< VTK file.
integer(hid_kind)             :: file_id, root              !< HDF5 identifiers.
character(len=:), allocatable :: text                       !< String read back.
integer(I8P),     allocatable :: ivalues(:), dims(:)        !< Values read back.
integer(I2P),     allocatable :: bvalues(:)                 !< Values read back.
real(R8P),        allocatable :: rvalues(:)                 !< Values read back.
integer(I4P),     allocatable :: jvalues(:)                 !< Values read back.
integer(I4P)                  :: e(30)                      !< Errors of each call.
integer(I4P)                  :: f                          !< Counter.
logical                       :: test_passed(7)             !< List of passed tests.
character(16), parameter      :: formats(2) = ['vtkhdf          ', 'binary          '] !< Formats of each mesh.
character(7),  parameter      :: extensions(2) = ['.vtkhdf', '.vtu   ']             !< Extensions of each format.

test_passed = .false.
if (.not.is_hdf5_enabled) then
  test_passed = a_vtk_file%initialize(format='vtkhdf', filename='refused.vtkhdf', mesh_topology='UnstructuredGrid') /= 0
  print '(A)', 'HDF5 not enabled: the VTKHDF writer refuses to start'
else
  e = 0
  do f=1, 2
    e(f) = write_mixed(trim(formats(f)), 'vtkfortran_unst_mixed'//trim(extensions(f)))
    e(2+f) = write_pieces(trim(formats(f)), 'vtkfortran_unst_pieces'//trim(extensions(f)))
    e(4+f) = write_i8p(trim(formats(f)), 'vtkfortran_unst_i8p'//trim(extensions(f)))
  enddo
  test_passed(1) = all(e(1:6) == 0)
  if (.not.test_passed(1)) print '(A,6I2)', 'errors of the meshes (vtkhdf, vtu for each): ', e(1:6)
  test_passed(2) = refusals()

  ! read back the two pieces
  e = 0
  e(1) = h5_file_open('vtkfortran_unst_pieces.vtkhdf', file_id)
  e(2) = h5_group_open(file_id, 'VTKHDF', root)
  e(3) = h5_attribute_read(root, 'Type', text)
  e(4) = h5_attribute_read(root, 'Version', ivalues)
  test_passed(3) = text == 'UnstructuredGrid' .and. all(ivalues == [2_I8P, 6_I8P])
  e(5) = h5_dataset_read(root, 'NumberOfPoints', ivalues)
  test_passed(4) = all(ivalues == [8_I8P, 5_I8P])
  e(6) = h5_dataset_read(root, 'NumberOfCells', ivalues)
  test_passed(4) = test_passed(4) .and. all(ivalues == [1_I8P, 2_I8P])
  e(7) = h5_dataset_read(root, 'NumberOfConnectivityIds', ivalues)
  test_passed(4) = test_passed(4) .and. all(ivalues == [8_I8P, 9_I8P])
  e(8) = h5_dataset_read(root, 'Offsets', ivalues)
  test_passed(5) = all(ivalues == [0_I8P, 8_I8P, 0_I8P, 5_I8P, 9_I8P])
  e(9) = h5_dataset_read(root, 'Connectivity', ivalues)
  test_passed(5) = test_passed(5) .and. all(ivalues == [0_I8P, 1_I8P, 3_I8P, 2_I8P, 4_I8P, 5_I8P, 7_I8P, 6_I8P, &
                                                        0_I8P, 1_I8P, 3_I8P, 2_I8P, 4_I8P, 0_I8P, 1_I8P, 2_I8P, 4_I8P])
  e(10) = h5_dataset_read(root, 'Types', bvalues)
  test_passed(5) = test_passed(5) .and. all(bvalues == [12_I2P, 14_I2P, 10_I2P])
  e(11) = h5_dataset_dims(root, 'Points', dims)
  test_passed(6) = all(dims == [3_I8P, 13_I8P])
  e(12) = h5_dataset_read(root, 'PointData/pd', rvalues)
  test_passed(6) = test_passed(6) .and. all(rvalues == [(real(f, R8P), f=1, 8), (real(100 + f, R8P), f=1, 5)])
  e(13) = h5_dataset_read(root, 'CellData/cid', jvalues)
  test_passed(6) = test_passed(6) .and. all(jvalues == [1_I4P, 101_I4P, 102_I4P])
  e(14) = h5_dataset_dims(root, 'PointData/pv', dims)
  test_passed(6) = test_passed(6) .and. all(dims == [3_I8P, 13_I8P])
  e(15) = h5_group_close(root)
  e(16) = h5_file_close(file_id)
  test_passed(7) = all(e(1:16) == 0)
  print '(A,7L2)', 'VTKHDF UnstructuredGrid: meshes, refusals, header, counts, cells, points+data, reads:', test_passed
endif

print "(A,L1)", new_line('a')//'Are all tests passed? ', all(test_passed)
if (.not.all(test_passed)) error stop 'some tests failed'
stop
contains
  function write_mixed(format, filename) result(error)
  !< One piece of mixed cells on the 8 corners of a unit cube and the middle of its first edge.
  character(*), intent(in) :: format   !< Format.
  character(*), intent(in) :: filename !< File name.
  integer(I4P)             :: error    !< Error status.
  integer(I4P), parameter  :: np=9, nc=9
  real(R8P)                :: x(np), y(np), z(np)
  integer(I4P)             :: connect(36), offset(nc), p
  integer(I1P)             :: cell_type(nc), ghost(nc)
  real(R4P)                :: pv(3,np)

  x = [0, 1, 0, 1, 0, 1, 0, 1, 0] ; y = [0, 0, 1, 1, 0, 0, 1, 1, 0] ; z = [0, 0, 0, 0, 1, 1, 1, 1, 0]
  x(9) = 0.5_R8P
  connect = [0,                          & ! vertex
             0, 1,                       & ! line
             0, 1, 8,                    & ! quadratic edge (the middle node last)
             0, 1, 2,                    & ! triangle
             0, 1, 3, 2,                 & ! quad
             0, 1, 2, 4,                 & ! tetra
             0, 1, 3, 2, 4, 5, 7, 6,     & ! hexahedron
             0, 2, 1, 4, 6, 5,           & ! wedge
             0, 1, 3, 2, 4]                ! pyramid
  offset = [1, 3, 6, 9, 13, 17, 25, 31, 36]
  cell_type = [1_I1P, 3_I1P, 21_I1P, 5_I1P, 9_I1P, 10_I1P, 12_I1P, 13_I1P, 14_I1P]
  ghost = [0_I1P, 0_I1P, 0_I1P, 0_I1P, 0_I1P, 0_I1P, 0_I1P, 0_I1P, -56_I1P] ! the last one is 200 as unsigned
  do p=1, np
    pv(:,p) = [real(x(p), R4P), real(y(p), R4P), real(z(p), R4P)]
  enddo
  error = a_vtk_file%initialize(format=format, filename=filename, mesh_topology='UnstructuredGrid')
  error = max(error, abs(a_vtk_file%xml_writer%write_fielddata(action='open')))
  error = max(error, abs(a_vtk_file%xml_writer%write_fielddata(data_name='time', x=2.5_R8P)))
  error = max(error, abs(a_vtk_file%xml_writer%write_fielddata(action='close')))
  error = max(error, abs(a_vtk_file%xml_writer%write_piece(np=np, nc=nc)))
  error = max(error, abs(a_vtk_file%xml_writer%write_geo(np=np, nc=nc, x=x, y=y, z=z)))
  error = max(error, abs(a_vtk_file%xml_writer%write_connectivity(nc=nc, connectivity=connect, offset=offset, &
                                                                    cell_type=cell_type)))
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
  endfunction write_mixed

  function write_pieces(format, filename) result(error)
  !< Two pieces: a hexahedron (8 points), then a pyramid and a tetra (5 points); point ids from 0 in each piece.
  character(*), intent(in) :: format   !< Format.
  character(*), intent(in) :: filename !< File name.
  integer(I4P)             :: error    !< Error status.

  error = a_vtk_file%initialize(format=format, filename=filename, mesh_topology='UnstructuredGrid')
  error = max(error, abs(piece(8, 1, [0._R8P, 1._R8P, 0._R8P, 1._R8P, 0._R8P, 1._R8P, 0._R8P, 1._R8P], &
                                     [0._R8P, 0._R8P, 1._R8P, 1._R8P, 0._R8P, 0._R8P, 1._R8P, 1._R8P], &
                                     [0._R8P, 0._R8P, 0._R8P, 0._R8P, 1._R8P, 1._R8P, 1._R8P, 1._R8P], &
                                     [0, 1, 3, 2, 4, 5, 7, 6], [8], [12_I1P], 0, 0)))
  error = max(error, abs(piece(5, 2, [2._R8P, 3._R8P, 2._R8P, 3._R8P, 2.5_R8P],                    &
                                     [0._R8P, 0._R8P, 1._R8P, 1._R8P, 0.5_R8P],                    &
                                     [0._R8P, 0._R8P, 0._R8P, 0._R8P, 1._R8P],                     &
                                     [0, 1, 3, 2, 4, 0, 1, 2, 4], [5, 9], [14_I1P, 10_I1P], 100, 100)))
  error = max(error, abs(a_vtk_file%finalize()))
  endfunction write_pieces

  function piece(np, nc, x, y, z, connect, offset, cell_type, point_base, cell_base) result(error)
  !< Write a piece of the two-piece mesh, with its point data pd (point_base + point number), pv and cell data cid.
  integer(I4P), intent(in) :: np, nc, connect(:), offset(:), point_base, cell_base
  real(R8P),    intent(in) :: x(:), y(:), z(:)
  integer(I1P), intent(in) :: cell_type(:)
  integer(I4P)             :: error, p

  error = a_vtk_file%xml_writer%write_piece(np=np, nc=nc)
  error = max(error, abs(a_vtk_file%xml_writer%write_geo(np=np, nc=nc, x=x, y=y, z=z)))
  error = max(error, abs(a_vtk_file%xml_writer%write_connectivity(nc=nc, connectivity=connect, offset=offset, &
                                                                    cell_type=cell_type)))
  error = max(error, abs(a_vtk_file%xml_writer%write_dataarray(location='node', action='open', scalars='pd')))
  error = max(error, abs(a_vtk_file%xml_writer%write_dataarray(data_name='pd', x=[(real(point_base + p, R8P), p=1, np)])))
  error = max(error, abs(a_vtk_file%xml_writer%write_dataarray(data_name='pv', x=x, y=y, z=z)))
  error = max(error, abs(a_vtk_file%xml_writer%write_dataarray(location='node', action='close')))
  error = max(error, abs(a_vtk_file%xml_writer%write_dataarray(location='cell', action='open')))
  error = max(error, abs(a_vtk_file%xml_writer%write_dataarray(data_name='cid', x=[(cell_base + p, p=1, nc)])))
  error = max(error, abs(a_vtk_file%xml_writer%write_dataarray(location='cell', action='close')))
  error = max(error, abs(a_vtk_file%xml_writer%write_piece()))
  endfunction piece

  function write_i8p(format, filename) result(error)
  !< A tetra and a hexahedron sharing a face, I8P counts and ids, R4P points as xyz(3,np), compressed if possible.
  character(*), intent(in) :: format   !< Format.
  character(*), intent(in) :: filename !< File name.
  integer(I4P)             :: error    !< Error status.
  integer(I8P), parameter  :: np=9_I8P, nc=2_I8P
  real(R4P)                :: xyz(3,np)
  integer(I8P)             :: connect(12), offset(nc)
  integer(I1P)             :: cell_type(nc)

  xyz = reshape([0., 0., 0.,  1., 0., 0.,  0., 1., 0.,  1., 1., 0.,  0., 0., 1.,  1., 0., 1.,  0., 1., 1.,  1., 1., 1., &
                 0.5, 0.5, -1.], [3, 9])
  connect = [0_I8P, 1_I8P, 3_I8P, 2_I8P, 4_I8P, 5_I8P, 7_I8P, 6_I8P, 0_I8P, 1_I8P, 2_I8P, 8_I8P]
  offset = [8_I8P, 12_I8P]
  cell_type = [12_I1P, 10_I1P]
  if (format == 'vtkhdf' .and. h5_deflate_available()) then
    error = a_vtk_file%initialize(format=format, filename=filename, mesh_topology='UnstructuredGrid', compressor='zlib')
  else
    error = a_vtk_file%initialize(format=format, filename=filename, mesh_topology='UnstructuredGrid')
  endif
  error = max(error, abs(a_vtk_file%xml_writer%write_piece(np=np, nc=nc)))
  error = max(error, abs(a_vtk_file%xml_writer%write_geo(np=np, nc=nc, xyz=xyz)))
  error = max(error, abs(a_vtk_file%xml_writer%write_connectivity(nc=nc, connectivity=connect, offset=offset, &
                                                                    cell_type=cell_type)))
  error = max(error, abs(a_vtk_file%xml_writer%write_dataarray(location='node', action='open')))
  error = max(error, abs(a_vtk_file%xml_writer%write_dataarray(data_name='height', x=xyz(3,:))))
  error = max(error, abs(a_vtk_file%xml_writer%write_dataarray(location='node', action='close')))
  error = max(error, abs(a_vtk_file%xml_writer%write_piece()))
  error = max(error, abs(a_vtk_file%finalize()))
  endfunction write_i8p

  logical function refusals()
  !< The refusals of the UnstructuredGrid writer, each one a non-zero error.
  integer(I4P) :: error
  logical      :: ok(5)
  real(R8P)    :: one_point(1) = [0._R8P]

  ok = .false.
  ! an array of the first piece missing from the second: refused at the end of the second piece
  error = a_vtk_file%initialize(format='vtkhdf', filename='refused_missing.vtkhdf', mesh_topology='UnstructuredGrid')
  error = cell1(with_data=.true.)
  ok(1) = error == 0 .and. cell1(with_data=.false.) /= 0
  error = a_vtk_file%finalize()
  ! an array new after the first piece: refused when written
  error = a_vtk_file%initialize(format='vtkhdf', filename='refused_new.vtkhdf', mesh_topology='UnstructuredGrid')
  error = cell1(with_data=.false.)
  error = a_vtk_file%xml_writer%write_piece(np=1, nc=1)
  error = a_vtk_file%xml_writer%write_dataarray(location='node', action='open')
  ok(2) = a_vtk_file%xml_writer%write_dataarray(data_name='late', x=one_point) /= 0
  error = a_vtk_file%xml_writer%write_dataarray(location='node', action='close')
  error = a_vtk_file%finalize()
  ! a piece without cells, polyhedra, points of the wrong size
  error = a_vtk_file%initialize(format='vtkhdf', filename='refused_other.vtkhdf', mesh_topology='UnstructuredGrid')
  error = a_vtk_file%xml_writer%write_piece(np=1, nc=1)
  ok(3) = a_vtk_file%xml_writer%write_geo(np=2, nc=1, x=[0._R8P, 1._R8P], y=[0._R8P, 1._R8P], z=[0._R8P, 1._R8P]) /= 0
  error = a_vtk_file%xml_writer%write_geo(np=1, nc=1, x=one_point, y=one_point, z=one_point)
  ok(4) = a_vtk_file%xml_writer%write_connectivity(nc=1, connectivity=[0], offset=[1], cell_type=[42_I1P], &
                                                     face=[1, 1, 0], faceoffset=[3]) /= 0
  ok(5) = a_vtk_file%xml_writer%write_piece() /= 0 ! no cells written
  error = a_vtk_file%finalize()
  refusals = all(ok)
  if (.not.refusals) print '(A,5L2)', 'refusals: ', ok
  endfunction refusals

  function cell1(with_data) result(error)
  !< A piece of one vertex, with or without its point data.
  logical, intent(in) :: with_data
  integer(I4P)        :: error
  real(R8P)           :: zero(1) = [0._R8P]

  error = a_vtk_file%xml_writer%write_piece(np=1, nc=1)
  error = max(error, abs(a_vtk_file%xml_writer%write_geo(np=1, nc=1, x=zero, y=zero, z=zero)))
  error = max(error, abs(a_vtk_file%xml_writer%write_connectivity(nc=1, connectivity=[0], offset=[1], cell_type=[1_I1P])))
  if (with_data) then
    error = max(error, abs(a_vtk_file%xml_writer%write_dataarray(location='node', action='open')))
    error = max(error, abs(a_vtk_file%xml_writer%write_dataarray(data_name='v', x=zero)))
    error = max(error, abs(a_vtk_file%xml_writer%write_dataarray(location='node', action='close')))
  endif
  error = max(error, abs(a_vtk_file%xml_writer%write_piece()))
  endfunction cell1
endprogram vtk_fortran_write_vtkhdf_unst
