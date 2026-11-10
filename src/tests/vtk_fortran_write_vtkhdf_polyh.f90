!< VTK_Fortran test: write the polyhedra of an UnstructuredGrid in the VTKHDF format (`format='vtkhdf'`).
program vtk_fortran_write_vtkhdf_polyh
!< VTK_Fortran test: write the polyhedra of an UnstructuredGrid in the VTKHDF format (`format='vtkhdf'`).
!<
!< With HDF5 the test writes two meshes, each one also as a binary `.vtu` by the same calls (VTK readers compare the two
!< outside this test):
!<
!<- `vtkfortran_unst_polyh1`: one piece, a cube and a pyramid written as polyhedra (type 42) with a tetrahedron between
!<  them, I4P;
!<- `vtkfortran_unst_polyh2`: three pieces, the polyhedra in the second only (I8P ids there, I4P in the others): the
!<  first piece gets its zero entries when the polyhedra come, the third one when it ends.
!<
!< The three-piece file is read back with the HDF5 bindings (Version 2.5, the counts of faces, the offsets of the faces
!< and of the polyhedra). Refusals are checked too. Without HDF5 the writer refuses to start.
use penf
use vtk_fortran, only : vtk_file
use vtk_fortran_hdf5

implicit none
type(vtk_file)                :: a_vtk_file                 !< VTK file.
integer(hid_kind)             :: file_id, root              !< HDF5 identifiers.
integer(I8P),     allocatable :: ivalues(:)                 !< Values read back.
integer(I4P)                  :: e(30)                      !< Errors of each call.
integer(I4P)                  :: f                          !< Counter.
logical                       :: test_passed(5)             !< List of passed tests.
character(16), parameter      :: formats(2) = ['vtkhdf          ', 'binary          '] !< Formats of each mesh.
character(7),  parameter      :: extensions(2) = ['.vtkhdf', '.vtu   ']             !< Extensions of each format.
!< The faces of a unit cube (points 0-7), counter-clockwise seen from outside: 6 faces, each of 4 points.
integer(I4P), parameter :: cube(31) = [6, 4, 0, 3, 2, 1,  4, 4, 5, 6, 7,  4, 0, 1, 5, 4,  4, 1, 2, 6, 5, &
                                       4, 2, 3, 7, 6,  4, 3, 0, 4, 7]
!< The faces of a pyramid (base points 0-3, apex 4): the base, then 4 triangles.
integer(I4P), parameter :: pyramid(22) = [5, 4, 0, 3, 2, 1,  3, 0, 1, 4,  3, 1, 2, 4,  3, 2, 3, 4,  3, 3, 0, 4]

test_passed = .false.
if (.not.is_hdf5_enabled) then
  test_passed = a_vtk_file%initialize(format='vtkhdf', filename='refused.vtkhdf', mesh_topology='UnstructuredGrid') /= 0
  print '(A)', 'HDF5 not enabled: the VTKHDF writer refuses to start'
else
  e = 0
  do f=1, 2
    e(f) = write_one_piece(trim(formats(f)), 'vtkfortran_unst_polyh1'//trim(extensions(f)))
    e(2+f) = write_three_pieces(trim(formats(f)), 'vtkfortran_unst_polyh2'//trim(extensions(f)))
  enddo
  test_passed(1) = all(e(1:4) == 0)
  if (.not.test_passed(1)) print '(A,4I2)', 'errors of the meshes (vtkhdf, vtu for each): ', e(1:4)
  test_passed(2) = refusals()

  ! read back the three pieces
  e = 0
  e(1) = h5_file_open('vtkfortran_unst_polyh2.vtkhdf', file_id)
  e(2) = h5_group_open(file_id, 'VTKHDF', root)
  e(3) = h5_attribute_read(root, 'Version', ivalues)
  test_passed(3) = all(ivalues == [2_I8P, 5_I8P])
  e(4) = h5_dataset_read(root, 'NumberOfFaces', ivalues)
  test_passed(3) = test_passed(3) .and. all(ivalues == [0_I8P, 6_I8P, 0_I8P])
  e(5) = h5_dataset_read(root, 'NumberOfFaceConnectivityIds', ivalues)
  test_passed(3) = test_passed(3) .and. all(ivalues == [0_I8P, 24_I8P, 0_I8P])
  e(6) = h5_dataset_read(root, 'NumberOfPolyhedronToFaceIds', ivalues)
  test_passed(3) = test_passed(3) .and. all(ivalues == [0_I8P, 6_I8P, 0_I8P])
  e(7) = h5_dataset_read(root, 'PolyhedronOffsets', ivalues)
  test_passed(4) = all(ivalues == [0_I8P, 0_I8P,  0_I8P, 6_I8P, 6_I8P,  0_I8P, 0_I8P])
  e(8) = h5_dataset_read(root, 'FaceOffsets', ivalues)
  test_passed(4) = test_passed(4) .and. all(ivalues == [0_I8P,  0_I8P, 4_I8P, 8_I8P, 12_I8P, 16_I8P, 20_I8P, 24_I8P,  0_I8P])
  e(9) = h5_dataset_read(root, 'PolyhedronToFaces', ivalues)
  test_passed(4) = test_passed(4) .and. all(ivalues == [0_I8P, 1_I8P, 2_I8P, 3_I8P, 4_I8P, 5_I8P])
  e(10) = h5_dataset_read(root, 'FaceConnectivity', ivalues)
  test_passed(4) = test_passed(4) .and. size(ivalues) == 24 .and. all(ivalues(1:8) == [0_I8P, 3_I8P, 2_I8P, 1_I8P, &
                                                                                         4_I8P, 5_I8P, 6_I8P, 7_I8P])
  e(11) = h5_group_close(root)
  e(12) = h5_file_close(file_id)
  test_passed(5) = all(e(1:12) == 0)
  print '(A,5L2)', 'VTKHDF polyhedra: meshes, refusals, counts, faces, reads:', test_passed
endif

print "(A,L1)", new_line('a')//'Are all tests passed? ', all(test_passed)
if (.not.all(test_passed)) error stop 'some tests failed'
stop
contains
  function write_one_piece(format, filename) result(error)
  !< One piece: a cube (polyhedron), a tetrahedron, a pyramid (polyhedron), with cell data.
  character(*), intent(in) :: format   !< Format.
  character(*), intent(in) :: filename !< File name.
  integer(I4P)             :: error    !< Error status.
  real(R8P)                :: x(17), y(17), z(17)

  x = [0, 1, 1, 0, 0, 1, 1, 0,  2, 3, 2, 2,  4, 5, 5, 4, 0] ; x(17) = 4.5_R8P
  y = [0, 0, 1, 1, 0, 0, 1, 1,  0, 0, 1, 0,  0, 0, 1, 1, 0] ; y(17) = 0.5_R8P
  z = [0, 0, 0, 0, 1, 1, 1, 1,  0, 0, 0, 1,  0, 0, 0, 0, 1]
  error = a_vtk_file%initialize(format=format, filename=filename, mesh_topology='UnstructuredGrid')
  error = max(error, abs(a_vtk_file%xml_writer%write_piece(np=17, nc=3)))
  error = max(error, abs(a_vtk_file%xml_writer%write_geo(np=17, nc=3, x=x, y=y, z=z)))
  error = max(error, abs(a_vtk_file%xml_writer%write_connectivity(nc=3, connectivity=[(f, f=0, 16)], offset=[8, 12, 17],     &
                                                                    cell_type=[42_I1P, 10_I1P, 42_I1P],                      &
                                                                    face=[cube, shifted(pyramid, 12)], faceoffset=[31, -1, 53])))
  error = max(error, abs(a_vtk_file%xml_writer%write_dataarray(location='cell', action='open')))
  error = max(error, abs(a_vtk_file%xml_writer%write_dataarray(data_name='cid', x=[1, 2, 3])))
  error = max(error, abs(a_vtk_file%xml_writer%write_dataarray(location='cell', action='close')))
  error = max(error, abs(a_vtk_file%xml_writer%write_piece()))
  error = max(error, abs(a_vtk_file%finalize()))
  endfunction write_one_piece

  function write_three_pieces(format, filename) result(error)
  !< Three pieces: a hexahedron; a cube (polyhedron) and a tetrahedron, I8P ids; a tetrahedron.
  character(*), intent(in) :: format   !< Format.
  character(*), intent(in) :: filename !< File name.
  integer(I4P)             :: error    !< Error status.
  real(R8P)                :: x(12), y(12), z(12)

  x = [0, 1, 1, 0, 0, 1, 1, 0,  2, 3, 2, 2] ; y = [0, 0, 1, 1, 0, 0, 1, 1,  0, 0, 1, 0]
  z = [0, 0, 0, 0, 1, 1, 1, 1,  0, 0, 0, 1]
  error = a_vtk_file%initialize(format=format, filename=filename, mesh_topology='UnstructuredGrid')
  ! piece 1: a hexahedron, no polyhedra
  error = max(error, abs(a_vtk_file%xml_writer%write_piece(np=8, nc=1)))
  error = max(error, abs(a_vtk_file%xml_writer%write_geo(np=8, nc=1, x=x(1:8) - 2, y=y(1:8), z=z(1:8))))
  error = max(error, abs(a_vtk_file%xml_writer%write_connectivity(nc=1, connectivity=[0, 1, 2, 3, 4, 5, 6, 7], offset=[8], &
                                                                    cell_type=[12_I1P])))
  error = max(error, abs(cell_data([1])))
  ! piece 2: a cube as a polyhedron and a tetrahedron, I8P
  error = max(error, abs(a_vtk_file%xml_writer%write_piece(np=12_I8P, nc=2_I8P)))
  error = max(error, abs(a_vtk_file%xml_writer%write_geo(np=12_I8P, nc=2_I8P, x=x, y=y, z=z)))
  error = max(error, abs(a_vtk_file%xml_writer%write_connectivity(nc=2_I8P, connectivity=[(int(f, I8P), f=0, 11)],          &
                                                                    offset=[8_I8P, 12_I8P], cell_type=[42_I1P, 10_I1P],     &
                                                                    face=int(cube, I8P), faceoffset=[31_I8P, -1_I8P])))
  error = max(error, abs(cell_data([2, 3])))
  ! piece 3: a tetrahedron, no polyhedra
  error = max(error, abs(a_vtk_file%xml_writer%write_piece(np=4, nc=1)))
  error = max(error, abs(a_vtk_file%xml_writer%write_geo(np=4, nc=1, x=x(9:12) + 2, y=y(9:12), z=z(9:12))))
  error = max(error, abs(a_vtk_file%xml_writer%write_connectivity(nc=1, connectivity=[0, 1, 2, 3], offset=[4], &
                                                                    cell_type=[10_I1P])))
  error = max(error, abs(cell_data([4])))
  error = max(error, abs(a_vtk_file%finalize()))
  endfunction write_three_pieces

  function cell_data(cid) result(error)
  !< Write the cell data cid, then end the piece.
  integer(I4P), intent(in) :: cid(:)
  integer(I4P)             :: error

  error = a_vtk_file%xml_writer%write_dataarray(location='cell', action='open')
  error = max(error, abs(a_vtk_file%xml_writer%write_dataarray(data_name='cid', x=cid)))
  error = max(error, abs(a_vtk_file%xml_writer%write_dataarray(location='cell', action='close')))
  error = max(error, abs(a_vtk_file%xml_writer%write_piece()))
  endfunction cell_data

  pure function shifted(faces, base) result(moved)
  !< A face stream of one polyhedron with its point ids moved by `base`.
  integer(I4P), intent(in) :: faces(:), base
  integer(I4P)             :: moved(size(faces))
  integer(I4P)             :: i, f, n

  moved = faces
  i = 2
  do f=1, faces(1)
    n = faces(i)
    moved(i+1:i+n) = faces(i+1:i+n) + base
    i = i + n + 1
  enddo
  endfunction shifted

  logical function refusals()
  !< The refusals of the polyhedra, each one a non-zero error; nothing is written by a refused call.
  integer(I4P) :: error
  logical      :: ok(6)
  real(R8P)    :: x(8)

  ok = .false.
  x = 0._R8P
  error = a_vtk_file%initialize(format='vtkhdf', filename='refused_polyh.vtkhdf', mesh_topology='UnstructuredGrid')
  error = a_vtk_file%xml_writer%write_piece(np=8, nc=1)
  error = a_vtk_file%xml_writer%write_geo(np=8, nc=1, x=x, y=x, z=x)
  ok(1) = a_vtk_file%xml_writer%write_connectivity(nc=1, connectivity=[0, 1, 2, 3, 4, 5, 6, 7], offset=[8], &
                                                   cell_type=[42_I1P]) /= 0                       ! no faces
  ok(2) = a_vtk_file%xml_writer%write_connectivity(nc=1, connectivity=[0, 1, 2, 3, 4, 5, 6, 7], offset=[8], &
                                                   cell_type=[42_I1P], face=cube) /= 0            ! no faceoffset
  ok(3) = a_vtk_file%xml_writer%write_connectivity(nc=1, connectivity=[0, 1, 2, 3, 4, 5, 6, 7], offset=[8], &
                                                   cell_type=[12_I1P], face=cube, faceoffset=[31]) /= 0 ! not a polyhedron
  ok(4) = a_vtk_file%xml_writer%write_connectivity(nc=1, connectivity=[0, 1, 2, 3, 4, 5, 6, 7], offset=[8], &
                                                   cell_type=[42_I1P], face=cube, faceoffset=[-1]) /= 0 ! a polyhedron, -1
  ok(5) = a_vtk_file%xml_writer%write_connectivity(nc=1, connectivity=[0, 1, 2, 3, 4, 5, 6, 7], offset=[8], &
                                                   cell_type=[42_I1P], face=cube, faceoffset=[30]) /= 0 ! faces past the end
  ok(6) = a_vtk_file%xml_writer%write_piece() /= 0                                                ! no cells written
  error = a_vtk_file%finalize()
  refusals = all(ok)
  if (.not.refusals) print '(A,6L2)', 'refusals: ', ok
  endfunction refusals
endprogram vtk_fortran_write_vtkhdf_polyh
