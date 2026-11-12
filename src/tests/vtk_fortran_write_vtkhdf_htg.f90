!< VTK_Fortran test: VTKHDF HyperTreeGrid (`mesh_topology='HyperTreeGrid'`), checked with the HDF5 bindings.
program vtk_fortran_write_vtkhdf_htg
!< VTK_Fortran test: VTKHDF HyperTreeGrid (`mesh_topology='HyperTreeGrid'`), checked with the HDF5 bindings.
!<
!< With HDF5 the test writes a HyperTreeGrid of a 2x2 root grid, branch factor 2: tree 0 of depth 3 (its root and its first
!< child refined), tree 1 of depth 2, tree 3 a masked leaf (tree 2 not written), cell data (R8P, I4P); and one of a single
!< tree of branch factor 3 and depth 3. It reads them back with the HDF5 bindings: the attributes, every dataset, the
!< descriptors and the mask unpacked (bits MSB first) and compared with the bits written. VTK readers compare them with
!< the same trees built in VTK outside this test. Then the refusals: a descriptor ending inside a depth or giving the
!< deepest depth, a mask of the wrong size or masking a refined cell, a tree out of the root grid or written twice, point
!< data, field data, a tree after the cell data, cell data of the wrong size, a piece without trees or coordinates, a
!< branch factor of 4, a time series, `write_tree` in another topology. The reader refuses a HyperTreeGrid (error 3).
!< Without HDF5 the writer refuses a HyperTreeGrid; the XML formats refuse it always.
use penf
use vtk_fortran, only : vtk_file
use vtk_fortran_hdf5

implicit none
type(vtk_file) :: w     !< Writer.
logical        :: ok(4) !< Checks.
integer(I4P)   :: e     !< Error.

ok = .false.
! the XML formats have no HyperTreeGrid here (VTKHDF only)
ok(1) = w%initialize(format='ascii', filename='htg.vthtg', mesh_topology='HyperTreeGrid', &
                     nx1=0, nx2=2, ny1=0, ny2=2, nz1=0, nz2=0) /= 0
if (.not.is_hdf5_enabled) then
  e = w%initialize(format='vtkhdf', filename='htg.vtkhdf', mesh_topology='HyperTreeGrid', &
                   nx1=0, nx2=2, ny1=0, ny2=2, nz1=0, nz2=0)
  ok(2:) = e /= 0
  print '(A)', 'HDF5 not enabled: a HyperTreeGrid is refused'
else
  ok(2) = binary_htg()
  ok(3) = ternary_htg()
  ok(4) = refusals()
endif
print '(A,4L2)', 'VTKHDF HyperTreeGrid: XML refused, branch factor 2, branch factor 3, refusals:', ok

print "(A,L1)", new_line('a')//'Are all tests passed? ', all(ok)
if (.not.all(ok)) error stop 'some tests failed'
stop
contains
  function bits_of(bytes, n) result(bits)
  !< The first n bits of bytes (unsigned values), MSB first.
  integer(I8P), intent(in) :: bytes(:) !< Bytes, 0 to 255.
  integer(I8P), intent(in) :: n        !< Bits.
  logical                  :: bits(n)  !< Bits.
  integer(I8P)             :: i        !< Counter.

  do i=1_I8P, n
    bits(i) = btest(bytes((i - 1_I8P) / 8_I8P + 1_I8P), 7_I8P - mod(i - 1_I8P, 8_I8P))
  enddo
  endfunction bits_of

  logical function binary_htg()
  !< The trees of the VTK probe: 2x2 root cells, branch factor 2; checked with the bindings.
  logical, parameter            :: t=.true., f=.false.
  integer(hid_kind)             :: file_id, root
  integer(I8P), allocatable     :: ivalues(:), bytes(:)
  real(R8P),    allocatable     :: rvalues(:)
  character(len=:), allocatable :: text
  type(vtk_file)                :: r
  integer(I4P)                  :: ee(10), i
  logical                       :: c(8)

  ee = 0 ; c = .false.
  ee(1) = w%initialize(format='vtkhdf', filename='htg.vtkhdf', mesh_topology='HyperTreeGrid', &
                       nx1=0, nx2=2, ny1=0, ny2=2, nz1=0, nz2=0, branch_factor=2)
  ee(2) = w%xml_writer%write_piece(nx1=0, nx2=2, ny1=0, ny2=2, nz1=0, nz2=0)
  ee(3) = w%xml_writer%write_geo(x=[0._R8P, 1._R8P, 2._R8P], y=[0._R8P, 1._R8P, 2._R8P], z=[0._R8P])
  ! one call per statement: the order of the calls of an expression is the processor's
  ee(4) = abs(w%xml_writer%write_tree(tree_id=0, descriptor=[t, t, f, f, f]))           ! depths of 1, 4, 4 cells
  ee(4) = ee(4) + abs(w%xml_writer%write_tree(tree_id=1, descriptor=[t]))               ! depths of 1, 4 cells
  ee(4) = ee(4) + abs(w%xml_writer%write_tree(tree_id=3, descriptor=[logical ::], mask=[t])) ! a masked leaf
  ee(5) = w%xml_writer%write_dataarray(location='cell', action='open')
  ee(6) = abs(w%xml_writer%write_dataarray(data_name='cd', x=[(real(i, R8P), i=0, 14)]))
  ee(6) = ee(6) + abs(w%xml_writer%write_dataarray(data_name='depth', x=[0, 1, 1, 1, 1, 2, 2, 2, 2, 0, 1, 1, 1, 1, 0]))
  ee(7) = w%xml_writer%write_dataarray(location='cell', action='close')
  ee(8) = w%xml_writer%write_piece()
  ee(9) = w%finalize()
  ee(10) = h5_file_open('htg.vtkhdf', file_id)
  e = h5_group_open(file_id, 'VTKHDF', root)
  e = h5_attribute_read(root, 'Type', text)
  c(1) = text == 'HyperTreeGrid'
  e = h5_attribute_read(root, 'Dimensions', ivalues)
  c(1) = c(1) .and. all(ivalues == [3, 3, 1])
  e = h5_attribute_read(root, 'BranchFactor', ivalues)
  c(1) = c(1) .and. all(ivalues == [2])
  e = h5_attribute_read(root, 'Version', ivalues)
  c(1) = c(1) .and. all(ivalues == [2, 4])
  e = h5_dataset_read(root, 'Descriptors', bytes)
  c(2) = all(bytes == [196]) ! 110001 00: tree 0 then tree 1, contiguous
  e = h5_dataset_read(root, 'DescriptorsSize', ivalues)
  c(2) = c(2) .and. all(ivalues == [6])
  if (c(2)) c(2) = all(bits_of(bytes, 6_I8P) .eqv. [t, t, f, f, f, t])
  e = h5_dataset_read(root, 'TreeIds', ivalues)
  c(3) = all(ivalues == [0, 1, 3])
  e = h5_dataset_read(root, 'DepthPerTree', ivalues)
  c(3) = c(3) .and. all(ivalues == [3, 2, 1])
  e = h5_dataset_read(root, 'NumberOfCellsPerTreeDepth', ivalues)
  c(4) = all(ivalues == [1, 4, 4, 1, 4, 1])
  e = h5_dataset_read(root, 'NumberOfTrees', ivalues)
  c(4) = c(4) .and. all(ivalues == [3])
  e = h5_dataset_read(root, 'NumberOfDepths', ivalues)
  c(4) = c(4) .and. all(ivalues == [6])
  e = h5_dataset_read(root, 'NumberOfCells', ivalues)
  c(4) = c(4) .and. all(ivalues == [15])
  e = h5_dataset_read(root, 'Mask', bytes)
  c(5) = all(bytes == [0, 2]) ! the cell 15 (the last) masked
  if (c(5)) c(5) = all(bits_of(bytes, 15_I8P) .eqv. [(i == 15, i=1, 15)])
  e = h5_dataset_read(root, 'CellData/cd', rvalues)
  c(6) = all(rvalues == [(real(i, R8P), i=0, 14)])
  e = h5_dataset_read(root, 'XCoordinates', rvalues)
  c(6) = c(6) .and. all(rvalues == [0._R8P, 1._R8P, 2._R8P])
  e = h5_group_close(root)
  e = h5_file_close(file_id)
  c(7) = .not.h5_link_exists_in('htg.vtkhdf', 'VTKHDF/PointData')
  c(8) = r%initialize(filename='htg.vtkhdf', action='read') == 3 ! not read (yet)
  e = r%finalize()
  binary_htg = all(c) .and. all(ee == 0)
  if (.not.binary_htg) print '(A,10I3,A,8L2)', 'HyperTreeGrid: errors ', ee, ' checks ', c
  endfunction binary_htg

  logical function h5_link_exists_in(filename, path)
  !< True if the link path exists in the file.
  character(*), intent(in) :: filename, path
  integer(hid_kind)        :: file_id

  h5_link_exists_in = .false.
  if (h5_file_open(filename, file_id) /= 0) return
  h5_link_exists_in = h5_link_exists(file_id, path)
  e = h5_file_close(file_id)
  endfunction h5_link_exists_in

  logical function ternary_htg()
  !< One tree, branch factor 3 (9 children in 2D): its root refined, then two of its children; checked with the bindings.
  logical, parameter        :: t=.true., f=.false.
  logical                   :: descriptor(10)
  integer(hid_kind)         :: file_id
  integer(I8P), allocatable :: ivalues(:), bytes(:)
  integer(I4P)              :: ee(7), i

  descriptor = [t, f, f, f, f, t, f, f, f, t] ! the root, then the 9 children: the centre and the last refined
  ee = 0
  ee(1) = w%initialize(format='vtkhdf', filename='htg3.vtkhdf', mesh_topology='HyperTreeGrid', &
                       nx1=0, nx2=1, ny1=0, ny2=1, nz1=0, nz2=0, branch_factor=3)
  ee(2) = w%xml_writer%write_piece(nx1=0, nx2=1, ny1=0, ny2=1, nz1=0, nz2=0)
  ee(3) = w%xml_writer%write_geo(x=[0._R8P, 3._R8P], y=[0._R8P, 3._R8P], z=[0._R8P])
  ee(4) = w%xml_writer%write_tree(tree_id=0, descriptor=descriptor)
  ee(5) = abs(w%xml_writer%write_dataarray(location='cell', action='open'))
  ee(5) = ee(5) + abs(w%xml_writer%write_dataarray(data_name='cd', x=[(i, i=0, 27)]))
  ee(5) = ee(5) + abs(w%xml_writer%write_dataarray(location='cell', action='close'))
  ee(6) = abs(w%xml_writer%write_piece())
  ee(6) = ee(6) + abs(w%finalize())
  ee(7) = h5_file_open('htg3.vtkhdf', file_id)
  e = h5_dataset_read(file_id, 'VTKHDF/NumberOfCellsPerTreeDepth', ivalues)
  ternary_htg = all(ivalues == [1, 9, 18])
  e = h5_dataset_read(file_id, 'VTKHDF/Descriptors', bytes)
  ternary_htg = ternary_htg .and. size(bytes) == 2
  if (ternary_htg) ternary_htg = all(bits_of(bytes, 10_I8P) .eqv. descriptor)
  e = h5_attribute_read_in(file_id, 'BranchFactor', ivalues)
  ternary_htg = ternary_htg .and. all(ivalues == [3])
  e = h5_file_close(file_id)
  ternary_htg = ternary_htg .and. all(ee == 0)
  if (.not.ternary_htg) print '(A,7I3)', 'HyperTreeGrid of branch factor 3: errors ', ee
  endfunction ternary_htg

  function h5_attribute_read_in(file_id, name, values) result(error)
  !< An attribute of /VTKHDF.
  integer(hid_kind),         intent(in)  :: file_id
  character(*),              intent(in)  :: name
  integer(I8P), allocatable, intent(out) :: values(:)
  integer(I4P)                           :: error
  integer(hid_kind)                      :: root

  error = h5_group_open(file_id, 'VTKHDF', root)
  if (error == 0) error = h5_attribute_read(root, name, values)
  if (error == 0) error = h5_group_close(root)
  endfunction h5_attribute_read_in

  logical function refusals()
  !< What a HyperTreeGrid refuses (each a non-zero error). One call per statement: the order of the calls of an
  !< expression is the processor's.
  logical, parameter :: t=.true., f=.false.
  logical            :: r(16)

  r = .false.
  e = w%initialize(format='vtkhdf', filename='htg_refused.vtkhdf', mesh_topology='HyperTreeGrid', &
                   nx1=0, nx2=2, ny1=0, ny2=2, nz1=0, nz2=0)
  e = w%xml_writer%write_piece(nx1=0, nx2=2, ny1=0, ny2=2, nz1=0, nz2=0)
  e = w%xml_writer%write_piece()
  r(1) = e /= 0                                                              ! no coordinates
  e = w%finalize()
  e = w%initialize(format='vtkhdf', filename='htg_refused.vtkhdf', mesh_topology='HyperTreeGrid', &
                   nx1=0, nx2=2, ny1=0, ny2=2, nz1=0, nz2=0)
  e = w%xml_writer%write_fielddata(action='open')
  r(2) = e /= 0                                                              ! field data
  e = w%xml_writer%write_step(time=0._R8P)
  r(3) = e /= 0                                                              ! a time series
  e = w%xml_writer%write_piece(nx1=0, nx2=2, ny1=0, ny2=2, nz1=0, nz2=0)
  e = w%xml_writer%write_geo(x=[0._R8P, 1._R8P, 2._R8P], y=[0._R8P, 1._R8P, 2._R8P], z=[0._R8P])
  e = w%xml_writer%write_tree(tree_id=0, descriptor=[t, t, f])
  r(4) = e /= 0                                                              ! ends inside depth 1
  e = w%xml_writer%write_tree(tree_id=0, descriptor=[t, f, f, f, f])
  r(5) = e /= 0                                                              ! the deepest depth given
  e = w%xml_writer%write_tree(tree_id=0, descriptor=[t], mask=[f, f])
  r(6) = e /= 0                                                              ! a mask of the wrong size
  e = w%xml_writer%write_tree(tree_id=0, descriptor=[t], mask=[t, f, f, f, f])
  r(7) = e /= 0                                                              ! a refined cell masked
  e = w%xml_writer%write_tree(tree_id=4, descriptor=[t])
  r(8) = e /= 0                                                              ! not a root cell
  e = w%xml_writer%write_tree(tree_id=0, descriptor=[t])
  e = w%xml_writer%write_tree(tree_id=0, descriptor=[t])
  r(9) = e /= 0                                                              ! tree 0 twice
  e = w%xml_writer%write_dataarray(location='node', action='open')
  r(10) = e /= 0                                                             ! point data
  e = w%xml_writer%write_dataarray(location='cell', action='open')
  e = w%xml_writer%write_dataarray(data_name='cd', x=[1._R8P, 2._R8P])
  r(11) = e /= 0                                                             ! 2 values for 5 cells
  e = w%xml_writer%write_dataarray(location='cell', action='close')
  e = w%xml_writer%write_tree(tree_id=1, descriptor=[t])
  r(12) = e /= 0                                                             ! a tree after the cell data
  e = w%xml_writer%write_piece()
  e = w%finalize()
  e = w%initialize(format='vtkhdf', filename='htg_refused.vtkhdf', mesh_topology='HyperTreeGrid', &
                   nx1=0, nx2=2, ny1=0, ny2=2, nz1=0, nz2=0)
  e = w%xml_writer%write_piece(nx1=0, nx2=2, ny1=0, ny2=2, nz1=0, nz2=0)
  e = w%xml_writer%write_geo(x=[0._R8P, 1._R8P, 2._R8P], y=[0._R8P, 1._R8P, 2._R8P], z=[0._R8P])
  e = w%xml_writer%write_piece()
  r(13) = e /= 0                                                             ! no trees
  e = w%finalize()
  e = w%initialize(format='vtkhdf', filename='htg_refused.vtkhdf', mesh_topology='HyperTreeGrid', &
                   nx1=0, nx2=2, ny1=0, ny2=2, nz1=0, nz2=0, branch_factor=4)
  r(14) = e /= 0                                                             ! branch factor 4
  e = w%initialize(format='vtkhdf', filename='htg_refused.vtkhdf', mesh_topology='RectilinearGrid', &
                   nx1=0, nx2=2, ny1=0, ny2=2, nz1=0, nz2=0)
  e = w%xml_writer%write_tree(tree_id=0, descriptor=[t])
  r(15) = e /= 0                                                             ! not a HyperTreeGrid
  e = w%finalize()
  e = w%initialize(format='vtkhdf', filename='htg_refused.vtkhdf', mesh_topology='HyperTreeGrid', &
                   nx1=0, nx2=2, ny1=0, ny2=2, nz1=0, nz2=0)
  e = w%xml_writer%write_tree(tree_id=0, descriptor=[t])
  r(16) = e /= 0                                                             ! outside the piece
  e = w%finalize()
  refusals = all(r)
  if (.not.refusals) print '(A,16L2)', 'HyperTreeGrid refusals: ', r
  endfunction refusals
endprogram vtk_fortran_write_vtkhdf_htg
