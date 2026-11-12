!< VTK_Fortran test: write a multi-block dataset in one VTKHDF file (`vtm_file`, `format='vtkhdf'`).
program vtk_fortran_write_vtkhdf_vtm
!< VTK_Fortran test: write a multi-block dataset in one VTKHDF file (`vtm_file`, `format='vtkhdf'`).
!<
!< With HDF5 the test writes `.vtkhdf` datasets with `vtk_file`, then builds two VTKHDF multi-block files from them with
!< the calls of the XML `.vtm`, and lists for each one, in `<name>.txt`, the path of each dataset in the tree of the blocks
!< and its file (VTK readers compare each dataset of the multi-block file with its file outside this test):
!<
!<- `vtkfortran_vtm_nested`: the nested assembly of the docs: a block `assembly` with two parts and a nested block `bolts`
!<  of two bolts, and a block `domain` with the fluid (an ImageData), written with every form of `write_block`;
!<- `vtkfortran_vtm_series`: a time series of three steps, two UnstructuredGrid series (the second of two parts) in a
!<  block `solid` and a PolyData series in a block `surface`, with the same times.
!<
!<- `vtkfortran_pdc_nested`, `vtkfortran_pdc_series`: the same two as `PartitionedDataSetCollection`s
!<  (`composite='PartitionedDataSetCollection'`), each with one more block, `checked`, that references two datasets
!<  already copied (`references=`), without a new copy.
!<
!< The files are read back with the HDF5 bindings (the type, the links of the tree; the steps of each block of the
!< series; the copies of a collection named after their datasets, their `Index`, a referenced dataset copied once).
!< Refusals are checked too: a time-series ImageData block, a static block after a series block and the reverse, a
!< series with another number of steps or other times, block and dataset names with '/' or '.'; in a collection a
!< dataset name used twice or `Assembly`, a reference to a dataset not copied; references in a multi-block file or an XML
!< `.vtm`, an unknown composite, a collection in XML; a composite (multi-block, collection) or an OverlappingAMR as the
!< source of a block, in a multi-block file and in a collection. The four composite files are read back with `vtm_file`
!< (`action='read'`: the entries of the Assembly, the dataset of each) and every dataset with `vtk_file%initialize(...,
!< action='read', dataset=)`, compared with its source file step by step; the refusals of `dataset`; a collection with
!< two datasets the Assembly does not reach (added with the bindings, `Index` 2 and 1), listed after the tree in the
!< order of their `Index`; empty datasets (VTK's null blocks: an empty file name) in a multi-block file, read back as
!< datasets without file, refused in a collection. Without HDF5 the
!< VTKHDF format of `vtm_file` is refused.
use penf
use vtk_fortran, only : vtk_file, vtm_file
use vtk_fortran_hdf5

implicit none
type(vtk_file)                :: a_vtk_file                 !< VTK file.
type(vtm_file)                :: a_vtm_file                 !< VTM file.
integer(hid_kind)             :: file_id, root, steps       !< HDF5 identifiers.
character(len=:), allocatable :: text                       !< String read back.
integer(I4P)                  :: e(20)                      !< Errors.
integer(I4P)                  :: u                          !< Unit.
integer(I8P), allocatable     :: n_steps(:)                 !< Steps read back.
real(R8P), allocatable        :: times(:)                   !< Times read back.
integer(I4P)                  :: b                          !< Counter.
logical                       :: test_passed(9)             !< List of passed tests.

test_passed = .false.
if (.not.is_hdf5_enabled) then
  test_passed = a_vtm_file%initialize(filename='refused.vtkhdf', format='vtkhdf') /= 0
  print '(A)', 'HDF5 not enabled: the VTKHDF format of vtm_file is refused'
else
  e = 0
  ! the datasets
  e(1) = tetra('part_1.vtkhdf', 0._R8P, 0, 0._R8P)
  e(2) = tetra('part_2.vtkhdf', 2._R8P, 0, 0._R8P)
  e(3) = tetra('bolt_1.vtkhdf', 0._R8P, 0, 3._R8P)
  e(4) = tetra('bolt_2.vtkhdf', 2._R8P, 0, 3._R8P)
  e(5) = image('fluid.vtkhdf', 0)
  e(6) = tetra('series_tetra.vtkhdf', 0._R8P, 3, 0._R8P)
  e(7) = image('series_fluid.vtkhdf', 3)
  e(9) = tetra('series_tetra_2.vtkhdf', 0._R8P, 3, 2._R8P, parts=2)
  e(10) = triangles('series_skin.vtkhdf', 3, 0.5_R8P)
  e(11) = tetra('series_short.vtkhdf', 0._R8P, 2, 0._R8P)
  e(12) = triangles('series_late.vtkhdf', 3, 0.6_R8P)
  ! the nested assembly
  e(8) = a_vtm_file%initialize(filename='vtkfortran_vtm_nested.vtkhdf', format='vtkhdf')
  e(8) = max(e(8), abs(a_vtm_file%write_block(action='open', name='assembly')))
  e(8) = max(e(8), abs(a_vtm_file%write_block(filenames=['part_1.vtkhdf', 'part_2.vtkhdf'], action='write')))
  e(8) = max(e(8), abs(a_vtm_file%write_block(filenames=['bolt_1.vtkhdf', 'bolt_2.vtkhdf'], names=['bolt-1', 'bolt-2'], &
                                              name='bolts')))
  e(8) = max(e(8), abs(a_vtm_file%write_block(action='close')))
  e(8) = max(e(8), abs(a_vtm_file%write_block(filenames='fluid.vtkhdf', names='fluid', name='domain')))
  e(8) = max(e(8), abs(a_vtm_file%finalize()))
  open(newunit=u, file='vtkfortran_vtm_nested.txt', status='replace')
  write(u, '(A)') 'assembly/part_1 part_1.vtkhdf', 'assembly/part_2 part_2.vtkhdf', 'assembly/bolts/bolt-1 bolt_1.vtkhdf', &
                  'assembly/bolts/bolt-2 bolt_2.vtkhdf', 'domain/fluid fluid.vtkhdf'
  close(u)
  ! the time series
  e(13) = a_vtm_file%initialize(filename='vtkfortran_vtm_series.vtkhdf', format='vtkhdf')
  e(13) = max(e(13), abs(a_vtm_file%write_block(filenames=['series_tetra.vtkhdf  ', 'series_tetra_2.vtkhdf'], &
                                                name='solid')))
  e(13) = max(e(13), abs(a_vtm_file%write_block(filenames='series_skin.vtkhdf', name='surface')))
  e(13) = max(e(13), abs(a_vtm_file%finalize()))
  open(newunit=u, file='vtkfortran_vtm_series.txt', status='replace')
  write(u, '(A)') 'solid/series_tetra series_tetra.vtkhdf', 'solid/series_tetra_2 series_tetra_2.vtkhdf', &
                  'surface/series_skin series_skin.vtkhdf'
  close(u)
  ! the same two as collections, each with a block referencing two datasets already copied
  e(14) = a_vtm_file%initialize(filename='vtkfortran_pdc_nested.vtkhdf', format='vtkhdf', &
                                composite='PartitionedDataSetCollection')
  e(14) = max(e(14), abs(a_vtm_file%write_block(action='open', name='assembly')))
  e(14) = max(e(14), abs(a_vtm_file%write_block(filenames=['part_1.vtkhdf', 'part_2.vtkhdf'], action='write')))
  e(14) = max(e(14), abs(a_vtm_file%write_block(filenames=['bolt_1.vtkhdf', 'bolt_2.vtkhdf'], names=['bolt-1', 'bolt-2'], &
                                                name='bolts')))
  e(14) = max(e(14), abs(a_vtm_file%write_block(action='close')))
  e(14) = max(e(14), abs(a_vtm_file%write_block(filenames='fluid.vtkhdf', names='fluid', name='domain')))
  e(14) = max(e(14), abs(a_vtm_file%write_block(references='bolt-1 part_2', name='checked')))
  e(14) = max(e(14), abs(a_vtm_file%finalize()))
  open(newunit=u, file='vtkfortran_pdc_nested.txt', status='replace')
  write(u, '(A)') 'assembly/part_1 part_1.vtkhdf', 'assembly/part_2 part_2.vtkhdf', 'assembly/bolts/bolt-1 bolt_1.vtkhdf', &
                  'assembly/bolts/bolt-2 bolt_2.vtkhdf', 'domain/fluid fluid.vtkhdf', 'checked/bolt-1 bolt_1.vtkhdf', &
                  'checked/part_2 part_2.vtkhdf'
  close(u)
  e(15) = a_vtm_file%initialize(filename='vtkfortran_pdc_series.vtkhdf', format='vtkhdf', &
                                composite='partitioneddatasetcollection')
  e(15) = max(e(15), abs(a_vtm_file%write_block(filenames=['series_tetra.vtkhdf  ', 'series_tetra_2.vtkhdf'], &
                                                name='solid')))
  e(15) = max(e(15), abs(a_vtm_file%write_block(filenames='series_skin.vtkhdf', name='surface')))
  e(15) = max(e(15), abs(a_vtm_file%write_block(action='open', name='checked')))
  e(15) = max(e(15), abs(a_vtm_file%write_block(action='write', references='series_skin series_tetra')))
  e(15) = max(e(15), abs(a_vtm_file%write_block(action='close')))
  e(15) = max(e(15), abs(a_vtm_file%finalize()))
  open(newunit=u, file='vtkfortran_pdc_series.txt', status='replace')
  write(u, '(A)') 'solid/series_tetra series_tetra.vtkhdf', 'solid/series_tetra_2 series_tetra_2.vtkhdf', &
                  'surface/series_skin series_skin.vtkhdf', 'checked/series_skin series_skin.vtkhdf', &
                  'checked/series_tetra series_tetra.vtkhdf'
  close(u)
  test_passed(1) = all(e(1:15) == 0)
  if (.not.test_passed(1)) print '(A,15I2)', 'errors of the files: ', e(1:15)
  test_passed(2) = refusals()

  ! read back the nested assembly
  e = 0
  e(1) = h5_file_open('vtkfortran_vtm_nested.vtkhdf', file_id)
  e(2) = h5_group_open(file_id, 'VTKHDF', root)
  e(3) = h5_attribute_read(root, 'Type', text)
  test_passed(3) = text == 'MultiBlockDataSet' .and. h5_link_exists(root, 'Block_5')                         .and. &
                   h5_link_exists(root, 'Assembly/assembly/part_1') .and. h5_link_exists(root, 'Assembly/domain/fluid') .and. &
                   h5_link_exists(root, 'Assembly/assembly/bolts/bolt-2')
  e(4) = h5_group_close(root)
  e(5) = h5_file_close(file_id)
  test_passed(4) = all(e(1:5) == 0)

  ! read back the time series: the steps of every block
  e = 0
  e(1) = h5_file_open('vtkfortran_vtm_series.vtkhdf', file_id)
  e(2) = h5_group_open(file_id, 'VTKHDF', root)
  test_passed(5) = h5_link_exists(root, 'Assembly/solid/series_tetra_2') .and. h5_link_exists(root, 'Assembly/surface/series_skin')
  do b=1, 3
    e(3) = max(e(3), abs(h5_group_open(root, 'Block_'//trim(str(n=b, no_sign=.true.))//'/Steps', steps)))
    e(4) = max(e(4), abs(h5_attribute_read(steps, 'NSteps', n_steps)), abs(h5_dataset_read(steps, 'Values', times)))
    e(4) = max(e(4), abs(h5_group_close(steps)))
    test_passed(5) = test_passed(5) .and. all(n_steps == [3_I8P])
    if (test_passed(5)) test_passed(5) = all(times == [0.5_R8P, 1._R8P, 1.5_R8P])
  enddo
  e(5) = h5_group_close(root)
  e(6) = h5_file_close(file_id)
  test_passed(6) = all(e(1:6) == 0)

  ! read back the collections: the type, the copies named after their datasets with their Index, the references
  test_passed(7) = collection('vtkfortran_pdc_nested.vtkhdf', &
                              [character(6) :: 'part_1', 'part_2', 'bolt-1', 'bolt-2', 'fluid'], &
                              [character(32) :: 'Assembly/assembly/bolts/bolt-2', 'Assembly/checked/bolt-1', &
                               'Assembly/checked/part_2'])
  test_passed(8) = collection('vtkfortran_pdc_series.vtkhdf', &
                              [character(14) :: 'series_tetra', 'series_tetra_2', 'series_skin'], &
                              [character(32) :: 'Assembly/surface/series_skin', 'Assembly/checked/series_skin', &
                               'Assembly/checked/series_tetra'])
  if (.not.composite_sources()) test_passed(2) = .false. ! (a call in an expression can be skipped)
  ! read back with the reader: the entries of the Assembly, each dataset compared with its file
  test_passed(9) = read_composite('vtkfortran_vtm_nested.vtkhdf', [1, 2, 2, 2, 3, 3, 1, 2], 'bddbddbd', &
                     [character(8) :: 'assembly', 'part_1', 'part_2', 'bolts', 'bolt-1', 'bolt-2', 'domain', 'fluid'], &
                     [character(7) :: '', 'Block_1', 'Block_2', '', 'Block_3', 'Block_4', '', 'Block_5'], &
                     [character(14) :: '', 'part_1', 'part_2', '', 'bolt_1', 'bolt_2', '', 'fluid'])
  test_passed(9) = read_composite('vtkfortran_pdc_nested.vtkhdf', [1, 2, 2, 2, 3, 3, 1, 2, 1, 2, 2], 'bddbddbdbdd', &
                     [character(8) :: 'assembly', 'part_1', 'part_2', 'bolts', 'bolt-1', 'bolt-2', 'domain', 'fluid', &
                      'checked', 'bolt-1', 'part_2'], &
                     [character(7) :: '', 'part_1', 'part_2', '', 'bolt-1', 'bolt-2', '', 'fluid', '', 'bolt-1', 'part_2'], &
                     [character(14) :: '', 'part_1', 'part_2', '', 'bolt_1', 'bolt_2', '', 'fluid', '', 'bolt_1', &
                      'part_2']) .and. test_passed(9)
  test_passed(9) = read_composite('vtkfortran_vtm_series.vtkhdf', [1, 2, 2, 1, 2], 'bddbd', &
                     [character(14) :: 'solid', 'series_tetra', 'series_tetra_2', 'surface', 'series_skin'], &
                     [character(7) :: '', 'Block_1', 'Block_2', '', 'Block_3'], &
                     [character(14) :: '', 'series_tetra', 'series_tetra_2', '', 'series_skin']) .and. test_passed(9)
  test_passed(9) = read_composite('vtkfortran_pdc_series.vtkhdf', [1, 2, 2, 1, 2, 1, 2, 2], 'bddbdbdd', &
                     [character(14) :: 'solid', 'series_tetra', 'series_tetra_2', 'surface', 'series_skin', 'checked', &
                      'series_skin', 'series_tetra'], &
                     [character(14) :: '', 'series_tetra', 'series_tetra_2', '', 'series_skin', '', 'series_skin', &
                      'series_tetra'], &
                     [character(14) :: '', 'series_tetra', 'series_tetra_2', '', 'series_skin', '', 'series_skin', &
                      'series_tetra']) .and. test_passed(9)
  ! a collection with a dataset the Assembly does not reach (as vtkHDFWriter writes some): listed after the tree
  if (.not.unlinked_dataset()) test_passed(9) = .false. ! its file is read next: called in a statement of its own
  if (.not.empty_datasets()) test_passed(9) = .false.
  test_passed(9) = read_composite('unlinked_pdc.vtkhdf', [1, 2, 1, 1], 'bddd', &
                     [character(6) :: 'a', 'part_1', 'second', 'third'], [character(6) :: '', 'part_1', 'second', 'third'], &
                     [character(6) :: '', 'part_1', 'part_2', 'bolt_1']) .and. test_passed(9)
  test_passed(9) = composite_refusals() .and. test_passed(9)
  print '(A,9L2)', 'VTKHDF multi-block: files, refusals, tree, reads, series steps, series reads, collections, read back:', &
                   test_passed
endif

print "(A,L1)", new_line('a')//'Are all tests passed? ', all(test_passed)
if (.not.all(test_passed)) error stop 'some tests failed'
stop
contains
  logical function read_composite(filename, levels, kinds, names, files, sources)
  !< Read a composite file back with `vtm_file` (`action='read'`): its entries (levels, kinds b/d, names, files: the
  !< dataset under /VTKHDF), then each dataset with `vtk_file%initialize(..., action='read', dataset=)`, compared with its
  !< source `.vtkhdf` (`sources`): type, steps, pieces, points and the arrays of every piece of every step.
  character(*), intent(in)      :: filename  !< Composite file.
  integer(I4P), intent(in)      :: levels(:) !< Levels expected.
  character(*), intent(in)      :: kinds     !< Kinds expected: b block, d dataset.
  character(*), intent(in)      :: names(:)  !< Names expected.
  character(*), intent(in)      :: files(:)  !< Files expected (datasets).
  character(*), intent(in)      :: sources(:) !< Source files of the datasets, without .vtkhdf.
  type(vtm_file)                :: vtm
  integer(I4P), allocatable     :: level(:), index(:)
  character(len=:), allocatable :: kind(:), name(:), file(:)
  integer(I4P)                  :: i, error

  read_composite = .false.
  error = vtm%initialize(filename=filename, action='read')
  error = error + vtm%get_entries(level=level, kind=kind, index=index, name=name, file=file)
  if (error /= 0 .or. size(level) /= size(levels)) then
    print '(A,I0,A,I0)', filename//': entries read with error ', error, ', number ', size(level)
    return
  endif
  read_composite = all(level == levels)
  do i=1, size(levels)
    read_composite = read_composite .and. kind(i) == trim(merge('block  ', 'dataset', kinds(i:i) == 'b')) .and. &
                     name(i) == names(i) .and. file(i) == files(i)
    if (kinds(i:i) == 'd') read_composite = read_composite .and. &
                                            same_dataset(filename, trim(files(i)), trim(sources(i))//'.vtkhdf')
  enddo
  error = vtm%finalize()
  if (.not.read_composite) print '(A)', filename//': entries or datasets read back differ'
  endfunction read_composite

  logical function same_dataset(filename, dataset, source)
  !< The dataset `dataset` of the composite file is its source file: type, steps, pieces, points, the node and cell arrays.
  character(*), intent(in)      :: filename !< Composite file.
  character(*), intent(in)      :: dataset  !< Dataset under /VTKHDF.
  character(*), intent(in)      :: source   !< Source file.
  type(vtk_file)                :: a, b
  character(len=:), allocatable :: ta, tb, na(:), nb(:)
  real(R8P), allocatable        :: ta_(:), tb_(:), xa(:), ya(:), za(:), xb(:), yb(:), zb(:), va(:), vb(:)
  integer(I8P), allocatable     :: ia(:), ib(:)
  integer(I4P)                  :: pa, pb, s, p, l, k, ea, eb, n_steps
  character(4), parameter       :: LOCATIONS(2) = ['node', 'cell']

  ea = a%initialize(filename=filename, action='read', dataset=dataset)
  eb = b%initialize(filename=source, action='read')
  same_dataset = ea == 0 .and. eb == 0
  if (.not.same_dataset) return
  ea = a%xml_reader%get_steps(times=ta_)
  eb = b%xml_reader%get_steps(times=tb_)
  same_dataset = ea == eb .and. size(ta_) == size(tb_)
  if (same_dataset) same_dataset = all(ta_ == tb_)
  n_steps = max(1, size(ta_))
  do s=1, n_steps
    if (.not.same_dataset) exit
    if (size(ta_) > 0) then
      ea = a%xml_reader%set_step(s)
      eb = b%xml_reader%set_step(s)
    endif
    ea = a%xml_reader%get_info(mesh_topology=ta, npieces=pa)
    eb = b%xml_reader%get_info(mesh_topology=tb, npieces=pb)
    same_dataset = same_dataset .and. ea == 0 .and. eb == 0 .and. ta == tb .and. pa == pb
    do p=1, pa
      if (.not.same_dataset) exit
      if (ta /= 'ImageData') then
        ea = a%xml_reader%read_geo(x=xa, y=ya, z=za, piece=p)
        eb = b%xml_reader%read_geo(x=xb, y=yb, z=zb, piece=p)
        same_dataset = ea == 0 .and. eb == 0 .and. size(xa) == size(xb)
        if (same_dataset) same_dataset = all(xa == xb) .and. all(ya == yb) .and. all(za == zb)
      endif
      do l=1, 2
        ea = a%xml_reader%get_dataarray_names(location=LOCATIONS(l), names=na, piece=p)
        eb = b%xml_reader%get_dataarray_names(location=LOCATIONS(l), names=nb, piece=p)
        same_dataset = same_dataset .and. ea == 0 .and. eb == 0 .and. size(na) == size(nb)
        if (.not.same_dataset) exit
        do k=1, size(na)
          ea = a%xml_reader%get_dataarray_info(location=LOCATIONS(l), data_name=trim(na(k)), piece=p, data_type=ta)
          if (ta(1:min(5, len(ta))) == 'Float') then
            ea = ea + a%xml_reader%read_dataarray(location=LOCATIONS(l), data_name=trim(na(k)), x=va, piece=p)
            eb = b%xml_reader%read_dataarray(location=LOCATIONS(l), data_name=trim(na(k)), x=vb, piece=p)
            same_dataset = same_dataset .and. ea == 0 .and. eb == 0 .and. na(k) == nb(k) .and. size(va) == size(vb)
            if (same_dataset) same_dataset = all(va == vb)
          else ! an integer array
            ea = ea + a%xml_reader%read_dataarray(location=LOCATIONS(l), data_name=trim(na(k)), x=ia, piece=p)
            eb = b%xml_reader%read_dataarray(location=LOCATIONS(l), data_name=trim(na(k)), x=ib, piece=p)
            same_dataset = same_dataset .and. ea == 0 .and. eb == 0 .and. na(k) == nb(k) .and. size(ia) == size(ib)
            if (same_dataset) same_dataset = all(ia == ib)
          endif
        enddo
      enddo
    enddo
  enddo
  ea = a%finalize()
  eb = b%finalize()
  if (.not.same_dataset) print '(A)', filename//': '//dataset//' differs from '//source
  endfunction same_dataset

  logical function unlinked_dataset()
  !< A collection of one block, then two datasets copied in with the bindings, with an `Index` (2 for `third`, 1 for
  !< `second`: listed in Index order) and no link from the Assembly.
  integer(hid_kind) :: file_id, root, src, group
  integer(I4P)      :: ee(4)

  ! one call per statement: the order of the calls of an expression is the processor's
  ee = 0
  ee(1) = a_vtm_file%initialize(filename='unlinked_pdc.vtkhdf', format='vtkhdf', composite='PartitionedDataSetCollection')
  ee(1) = ee(1) + abs(a_vtm_file%write_block(filenames='part_1.vtkhdf', name='a'))
  ee(1) = ee(1) + abs(a_vtm_file%finalize())
  ee(2) = h5_file_open('unlinked_pdc.vtkhdf', file_id, readwrite=.true.)
  ee(2) = ee(2) + abs(h5_group_open(file_id, 'VTKHDF', root))
  ee(3) = h5_file_open('bolt_1.vtkhdf', src)
  ee(3) = ee(3) + abs(h5_object_copy(src, 'VTKHDF', root, 'third'))
  ee(3) = ee(3) + abs(h5_file_close(src))
  ee(3) = ee(3) + abs(h5_group_open(root, 'third', group))
  ee(3) = ee(3) + abs(h5_attribute_write(group, 'Index', [2_I8P]))
  ee(3) = ee(3) + abs(h5_group_close(group))
  ee(4) = h5_file_open('part_2.vtkhdf', src)
  ee(4) = ee(4) + abs(h5_object_copy(src, 'VTKHDF', root, 'second'))
  ee(4) = ee(4) + abs(h5_file_close(src))
  ee(4) = ee(4) + abs(h5_group_open(root, 'second', group))
  ee(4) = ee(4) + abs(h5_attribute_write(group, 'Index', [1_I8P]))
  ee(4) = ee(4) + abs(h5_group_close(group))
  ee(4) = ee(4) + abs(h5_group_close(root))
  ee(4) = ee(4) + abs(h5_file_close(file_id))
  unlinked_dataset = all(ee == 0)
  if (.not.unlinked_dataset) print '(A,4I3)', 'collection with unlinked datasets: errors ', ee
  endfunction unlinked_dataset

  logical function empty_datasets()
  !< Empty datasets (an empty file name: VTK's null block, a group without attributes linked as a dataset) in a multi-block
  !< file, nested, read back with vtm_file: datasets with an empty file, the others as written; refused in a collection.
  type(vtm_file)                :: vtm
  integer(I4P), allocatable     :: level(:), index(:)
  character(len=:), allocatable :: kind(:), name(:), file(:)
  integer(I4P)                  :: ee(4), k
  logical                       :: c(2)

  ee = 0 ; c = .false.
  ee(1) = a_vtm_file%initialize(filename='empty_vtm.vtkhdf', format='vtkhdf')
  ee(1) = ee(1) + abs(a_vtm_file%write_block(filenames=[character(13) :: 'part_1.vtkhdf', ''], &
                                             names=[character(6) :: 'part_1', 'hole'], name='parts'))
  ee(1) = ee(1) + abs(a_vtm_file%write_block(filenames=[character(1) :: ''], name='nothing'))
  ee(1) = ee(1) + abs(a_vtm_file%finalize())
  ! refused in a collection (VTK 9.7.2 would read none of its datasets)
  ee(2) = a_vtm_file%initialize(filename='empty_pdc.vtkhdf', format='vtkhdf', composite='PartitionedDataSetCollection')
  c(2) = a_vtm_file%write_block(filenames=[character(1) :: ''], names=[character(4) :: 'void'], name='b') /= 0
  e = a_vtm_file%finalize() ! not 0: the refused file left its block open
  do k=1, 1
    if (k == 1) ee(3) = ee(3) + abs(vtm%initialize(filename='empty_vtm.vtkhdf', action='read'))
    if (k == 2) ee(3) = ee(3) + abs(vtm%initialize(filename='empty_pdc.vtkhdf', action='read'))
    ee(4) = ee(4) + abs(vtm%get_entries(level=level, kind=kind, index=index, name=name, file=file))
    if (all(ee == 0) .and. k == 1) then ! parts: part_1, hole (empty); nothing: Block_3 (empty)
      c(1) = size(level) == 5
      if (c(1)) c(1) = all(level == [1, 2, 2, 1, 2]) .and. all(kind == [character(7) :: 'block', 'dataset', 'dataset', &
                                                                           'block', 'dataset']) .and. &
                       name(3) == 'hole' .and. name(5) == 'Block_3' .and. file(2) == 'Block_1' .and. len_trim(file(3)) == 0 &
                       .and. len_trim(file(5)) == 0
    endif
    e = vtm%finalize()
  enddo
  empty_datasets = all(c) .and. all(ee == 0)
  if (.not.empty_datasets) print '(A,4I3,A,2L2)', 'empty datasets: errors ', ee, ' checks ', c
  endfunction empty_datasets

  logical function composite_refusals()
  !< Reading a composite: a missing dataset (4), a dataset of an XML file (3), dataset without action read (1), a
  !< composite without dataset (3), a .vtkhdf that is not a composite to vtm_file (2), then a composite read by the same
  !< vtm_file.
  type(vtk_file) :: r
  type(vtm_file) :: vtm
  logical        :: ok(6)
  integer(I4P), allocatable :: level(:)
  integer(I4P)   :: err !< Error (e is the array of the program).

  ok(1) = r%initialize(filename='vtkfortran_vtm_nested.vtkhdf', action='read', dataset='Block_9') == 4
  e = r%finalize()
  ok(2) = r%initialize(filename='refused_references.vtm', action='read', dataset='Block_1') == 3
  e = r%finalize()
  ok(3) = r%initialize(format='vtkhdf', filename='refused_dataset.vtkhdf', mesh_topology='UnstructuredGrid', &
                       dataset='Block_1') == 1
  ok(4) = r%initialize(filename='vtkfortran_vtm_nested.vtkhdf', action='read') == 3
  e = r%finalize()
  ok(5) = vtm%initialize(filename='part_1.vtkhdf', action='read') == 2
  e = vtm%finalize()
  ! the same vtm_file reads a composite after the refused read
  ok(6) = vtm%initialize(filename='vtkfortran_vtm_nested.vtkhdf', action='read') == 0
  err = vtm%get_entries(level=level)
  ok(6) = ok(6) .and. err == 0 .and. size(level) == 8
  e = vtm%finalize()
  composite_refusals = all(ok)
  if (.not.composite_refusals) print '(A,6L2)', 'composite refusals: ', ok
  endfunction composite_refusals

  logical function collection(filename, copies, links)
  !< A collection read back: its type, its copies named after their datasets with the Index of the order written, no
  !< other dataset group (a referenced dataset copied once), the links of the tree given.
  character(*), intent(in)      :: filename  !< File name.
  character(*), intent(in)      :: copies(:) !< Datasets, in the order written.
  character(*), intent(in)      :: links(:)  !< Links of the tree.
  character(len=:), allocatable :: members   !< Members of /VTKHDF.
  character(len=:), allocatable :: text      !< Type.
  integer(hid_kind)             :: file_id   !< File.
  integer(hid_kind)             :: root      !< Group /VTKHDF.
  integer(hid_kind)             :: group     !< A copy.
  integer(I8P), allocatable     :: index_(:) !< Index of a copy.
  integer(I4P)                  :: c, ee(4)  !< Counter, errors.

  ee = 0
  ee(1) = h5_file_open(filename, file_id)
  ee(2) = h5_group_open(file_id, 'VTKHDF', root)
  ee(3) = h5_attribute_read(root, 'Type', text)
  collection = all(ee == 0) .and. text == 'PartitionedDataSetCollection'
  do c=1, size(copies)
    collection = collection .and. h5_link_exists(root, trim(copies(c)))
    if (.not.collection) exit
    collection = h5_group_open(root, trim(copies(c)), group) == 0
    if (collection) collection = h5_attribute_read(group, 'Index', index_) == 0
    if (collection) collection = all(index_ == [int(c - 1, I8P)])
    ee(4) = h5_group_close(group)
  enddo
  ! the members of /VTKHDF in creation order: Assembly (created by initialize), the copies
  ee(4) = h5_group_names(file_id, 'VTKHDF', members)
  collection = collection .and. ee(4) == 0 .and. members == 'Assembly'//achar(0)//join(copies)
  do c=1, size(links)
    collection = collection .and. h5_link_exists(root, trim(links(c)))
  enddo
  ee(1) = h5_group_close(root)
  ee(2) = h5_file_close(file_id)
  collection = collection .and. all(ee(1:2) == 0)
  endfunction collection

  pure function join(names) result(list)
  !< The names, each trimmed and followed by NUL.
  character(*), intent(in)      :: names(:) !< Names.
  character(len=:), allocatable :: list     !< List.
  integer(I4P)                  :: n        !< Counter.

  list = ''
  do n=1, size(names)
    list = list//trim(names(n))//achar(0)
  enddo
  endfunction join

  function tetra(filename, shift, n_steps, height, parts) result(error)
  !< A tetrahedron moved by shift along x and height along z, with point data; a time series of n_steps steps if n_steps > 0;
  !< `parts` tetrahedra (pieces) per step, each one 1.5 further along y.
  character(*), intent(in)           :: filename  !< File name.
  real(R8P),    intent(in)           :: shift     !< Shift along x.
  integer(I4P), intent(in)           :: n_steps   !< Steps, 0 for a dataset that is not a time series.
  real(R8P),    intent(in)           :: height    !< Shift along z.
  integer(I4P), intent(in), optional :: parts     !< Pieces per step (default 1).
  integer(I4P)                       :: error     !< Error status.
  integer(I4P)                       :: s, i, p   !< Counters.
  integer(I4P)                       :: n_parts   !< Pieces per step.

  n_parts = 1 ; if (present(parts)) n_parts = parts
  error = a_vtk_file%initialize(format='vtkhdf', filename=filename, mesh_topology='UnstructuredGrid')
  do s=1, max(1, n_steps)
    if (n_steps > 0) error = max(error, abs(a_vtk_file%xml_writer%write_step(time=0.5_R8P * s)))
    do p=1, n_parts
      error = max(error, abs(a_vtk_file%xml_writer%write_piece(np=4, nc=1)))
      error = max(error, abs(a_vtk_file%xml_writer%write_geo(np=4, nc=1, x=[0._R8P, 1._R8P, 0._R8P, 0._R8P] + shift + s, &
                                                             y=[0._R8P, 0._R8P, 1._R8P, 0._R8P] + 1.5_R8P * (p - 1),     &
                                                             z=[0._R8P, 0._R8P, 0._R8P, 1._R8P] + height)))
      error = max(error, abs(a_vtk_file%xml_writer%write_connectivity(nc=1, connectivity=[0, 1, 2, 3], offset=[4], &
                                                                        cell_type=[10_I1P])))
      error = max(error, abs(a_vtk_file%xml_writer%write_dataarray(location='node', action='open')))
      error = max(error, abs(a_vtk_file%xml_writer%write_dataarray(data_name='pd', x=[(real(10 * s + i + p, R8P), i=1, 4)])))
      error = max(error, abs(a_vtk_file%xml_writer%write_dataarray(location='node', action='close')))
      error = max(error, abs(a_vtk_file%xml_writer%write_piece()))
    enddo
  enddo
  error = max(error, abs(a_vtk_file%finalize()))
  endfunction tetra

  function triangles(filename, n_steps, dt) result(error)
  !< A PolyData time series of n_steps steps at the times dt, 2 dt, ...: two triangles rising along z, with cell data.
  character(*), intent(in) :: filename  !< File name.
  integer(I4P), intent(in) :: n_steps   !< Steps.
  real(R8P),    intent(in) :: dt        !< Time of the first step, and between the steps.
  integer(I4P)             :: error     !< Error status.
  integer(I4P)             :: s, i      !< Counters.

  error = a_vtk_file%initialize(format='vtkhdf', filename=filename, mesh_topology='PolyData')
  do s=1, n_steps
    error = max(error, abs(a_vtk_file%xml_writer%write_step(time=dt * s)))
    error = max(error, abs(a_vtk_file%xml_writer%write_piece(np=4, nverts=0, nlines=0, nstrips=0, npolys=2)))
    error = max(error, abs(a_vtk_file%xml_writer%write_geo(np=4, nc=2, x=[0._R8P, 1._R8P, 1._R8P, 0._R8P], &
                                                           y=[0._R8P, 0._R8P, 1._R8P, 1._R8P],           &
                                                           z=[(real(s, R8P), i=1, 4)])))
    error = max(error, abs(a_vtk_file%xml_writer%write_polydata_cells(polys_connectivity=[0, 1, 2, 0, 2, 3], &
                                                                      polys_offset=[3, 6])))
    error = max(error, abs(a_vtk_file%xml_writer%write_dataarray(location='cell', action='open')))
    error = max(error, abs(a_vtk_file%xml_writer%write_dataarray(data_name='cd', x=[10 * s + 1, 10 * s + 2])))
    error = max(error, abs(a_vtk_file%xml_writer%write_dataarray(location='cell', action='close')))
    error = max(error, abs(a_vtk_file%xml_writer%write_piece()))
  enddo
  error = max(error, abs(a_vtk_file%finalize()))
  endfunction triangles

  function image(filename, n_steps) result(error)
  !< A 2D ImageData with point data; a time series of n_steps steps if n_steps > 0.
  character(*), intent(in) :: filename  !< File name.
  integer(I4P), intent(in) :: n_steps   !< Steps, 0 for a dataset that is not a time series.
  integer(I4P)             :: error     !< Error status.
  integer(I4P)             :: s, i      !< Counters.

  error = a_vtk_file%initialize(format='vtkhdf', filename=filename, mesh_topology='ImageData', &
                                nx1=0, nx2=2, ny1=0, ny2=1, nz1=0, nz2=0,                       &
                                origin=[0._R8P, 0._R8P, -1._R8P], spacing=[1._R8P, 1._R8P, 1._R8P])
  do s=1, max(1, n_steps)
    if (n_steps > 0) error = max(error, abs(a_vtk_file%xml_writer%write_step(time=0.5_R8P * s)))
    error = max(error, abs(a_vtk_file%xml_writer%write_piece(nx1=0, nx2=2, ny1=0, ny2=1, nz1=0, nz2=0)))
    error = max(error, abs(a_vtk_file%xml_writer%write_dataarray(location='node', action='open')))
    error = max(error, abs(a_vtk_file%xml_writer%write_dataarray(data_name='pd', x=[(real(10 * s + i, R8P), i=1, 6)])))
    error = max(error, abs(a_vtk_file%xml_writer%write_dataarray(location='node', action='close')))
    error = max(error, abs(a_vtk_file%xml_writer%write_piece()))
  enddo
  error = max(error, abs(a_vtk_file%finalize()))
  endfunction image

  logical function composite_sources()
  !< A composite (MultiBlockDataSet, PartitionedDataSetCollection) or an OverlappingAMR as the source of a block is
  !< refused (the blocks of a composite are not composite, VTKHDF specification), in a multi-block file and in a collection;
  !< a dataset after them is still copied.
  type(vtk_file) :: amr
  integer(I4P)   :: error
  logical        :: ok(9)

  ok = .false.
  error = amr%initialize(format='vtkhdf', filename='amr_block.vtkhdf', mesh_topology='OverlappingAMR')
  error = error + amr%xml_writer%write_level(0, [1._R8P, 1._R8P, 1._R8P])
  error = error + amr%xml_writer%write_piece(nx1=0, nx2=2, ny1=0, ny2=2, nz1=0, nz2=2)
  error = error + amr%xml_writer%write_piece()
  ok(1) = error == 0 .and. amr%finalize() == 0
  error = a_vtm_file%initialize(filename='refused_vtm_composite.vtkhdf', format='vtkhdf')
  ok(2) = a_vtm_file%write_block(filenames='vtkfortran_vtm_nested.vtkhdf', name='a') /= 0 ! a multi-block file
  ok(3) = a_vtm_file%write_block(filenames='vtkfortran_pdc_nested.vtkhdf', name='b') /= 0 ! a collection
  ok(4) = a_vtm_file%write_block(filenames='amr_block.vtkhdf', name='c') /= 0             ! an OverlappingAMR
  ok(5) = a_vtm_file%write_block(filenames='part_1.vtkhdf', name='d') == 0                ! a dataset
  error = a_vtm_file%finalize() ! not 0: each refused file left its new block open
  error = a_vtm_file%initialize(filename='refused_pdc_composite.vtkhdf', format='vtkhdf', &
                                composite='PartitionedDataSetCollection')
  ok(6) = a_vtm_file%write_block(filenames='vtkfortran_vtm_nested.vtkhdf', name='a') /= 0 ! a multi-block file
  ok(7) = a_vtm_file%write_block(filenames='vtkfortran_pdc_nested.vtkhdf', name='b') /= 0 ! a collection
  ok(8) = a_vtm_file%write_block(filenames='amr_block.vtkhdf', name='c') /= 0             ! an OverlappingAMR
  ok(9) = a_vtm_file%write_block(filenames='part_1.vtkhdf', name='d') == 0                ! a dataset
  error = a_vtm_file%finalize()
  composite_sources = all(ok)
  if (.not.composite_sources) print '(A,9L2)', 'composite sources refused: ', ok
  endfunction composite_sources

  logical function refusals()
  !< The refusals of the VTKHDF format of vtm_file, each one a non-zero error.
  integer(I4P) :: error
  logical      :: ok(21)

  ok = .false.
  ! a static file
  error = a_vtm_file%initialize(filename='refused_vtm.vtkhdf', format='vtkhdf')
  ok(1) = a_vtm_file%write_block(filenames=['missing.vtkhdf'], name='a') /= 0             ! a missing file
  error = a_vtm_file%write_block(filenames=['part_1.vtkhdf'], name='a2')
  ok(2) = a_vtm_file%write_block(filenames='series_tetra.vtkhdf', name='b') /= 0          ! a series after a static block
  error = a_vtm_file%write_block(action='open', name='c')
  error = a_vtm_file%write_block(filenames=['part_1.vtkhdf'], names=['p'], action='write')
  ok(3) = a_vtm_file%write_block(filenames=['part_2.vtkhdf'], names=['p'], action='write') /= 0 ! a name used twice
  error = a_vtm_file%write_block(action='close')
  ok(4) = a_vtm_file%write_block(scratch=1, action='open', name='d') /= 0                  ! no scratch units
  error = a_vtm_file%write_block(action='open', name='e')
  ok(5) = a_vtm_file%finalize() /= 0                                                       ! a block left open
  ! a time series
  error = a_vtm_file%initialize(filename='refused_vtm_series.vtkhdf', format='vtkhdf')
  ok(6) = a_vtm_file%write_block(filenames='series_fluid.vtkhdf', name='a') /= 0          ! a time-series grid
  error = a_vtm_file%write_block(filenames='series_tetra.vtkhdf', name='b')
  ok(7) = a_vtm_file%write_block(filenames='fluid.vtkhdf', name='c') /= 0                 ! a static block after a series
  ok(8) = a_vtm_file%write_block(filenames='series_short.vtkhdf', name='d') /= 0          ! another number of steps
  ok(9) = a_vtm_file%write_block(filenames='series_late.vtkhdf', name='e') /= 0           ! other times
  error = a_vtm_file%finalize() ! not 0: each refused file left its new block open
  ! names: a block or a dataset name with '/' or '.' is refused (VTKHDF names), never renamed
  error = a_vtm_file%initialize(filename='refused_vtm_names.vtkhdf', format='vtkhdf')
  ok(10) = a_vtm_file%write_block(filenames='part_1.vtkhdf', name='a.b') /= 0             ! a '.' in a block name
  error = a_vtm_file%write_block(action='open', name='blk')
  ok(11) = a_vtm_file%write_block(filenames=['part_1.vtkhdf'], names=['p/q'], action='write') /= 0 ! a '/' in a name
  ok(12) = a_vtm_file%write_block(filenames=['part_1.vtkhdf'], names=['p_q'], action='write') == 0 ! a valid name
  error = a_vtm_file%write_block(action='close')
  ok(13) = a_vtm_file%finalize() == 0                                                      ! the file is complete
  ! a collection: a dataset name used twice or Assembly, a reference to a dataset not copied or to the Assembly
  error = a_vtm_file%initialize(filename='refused_pdc.vtkhdf', format='vtkhdf', composite='PartitionedDataSetCollection')
  error = a_vtm_file%write_block(filenames='part_1.vtkhdf', name='a')
  ok(14) = a_vtm_file%write_block(filenames='part_2.vtkhdf', names='part_1', name='b') /= 0 ! part_1 copied twice
  ok(15) = a_vtm_file%write_block(filenames='part_2.vtkhdf', names='Assembly', name='c') /= 0 ! the name Assembly
  ok(16) = a_vtm_file%write_block(references='part_2', name='d') /= 0                      ! part_2 never copied
  ok(17) = a_vtm_file%write_block(references='Assembly', name='e') /= 0                    ! not a dataset
  error = a_vtm_file%finalize() ! not 0: each refused call left its new block open
  ! references outside a collection, an unknown composite, a collection in XML
  error = a_vtm_file%initialize(filename='refused_vtm_references.vtkhdf', format='vtkhdf')
  error = a_vtm_file%write_block(filenames='part_1.vtkhdf', name='a')
  ok(18) = a_vtm_file%write_block(references='part_1', name='b') /= 0                      ! a multi-block file
  error = a_vtm_file%finalize()
  error = a_vtm_file%initialize(filename='refused_references.vtm')
  ok(19) = a_vtm_file%write_block(references='part_1', name='b') /= 0                      ! an XML .vtm
  error = a_vtm_file%finalize()
  ok(20) = a_vtm_file%initialize(filename='refused_composite.vtkhdf', format='vtkhdf', composite='Collection') /= 0
  ok(21) = a_vtm_file%initialize(filename='refused_collection.vtm', composite='PartitionedDataSetCollection') /= 0
  refusals = all(ok)
  if (.not.refusals) print '(A,21L2)', 'refusals: ', ok
  endfunction refusals
endprogram vtk_fortran_write_vtkhdf_vtm
