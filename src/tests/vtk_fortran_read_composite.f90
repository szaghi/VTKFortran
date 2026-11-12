!< VTK_Fortran test: read parallel headers, multi-block and time series files (action='read').
program vtk_fortran_read_composite
!< VTK_Fortran test: read parallel headers, multi-block and time series files (action='read').
!<
!< A parallel unstructured grid of two tetrahedra pieces is written and its header read back (information, declared arrays,
!< pieces) and checked against the pieces: a correct header, one declaring a missing array and one declaring a wrong type.
!< The pieces extents of a parallel rectilinear grid header are read. A nested multi-block file and a time series are
!< written and their entries read back. String arrays declared as `PArray`: a header and a piece written by VTK 9.7.2
!< (embedded), and a header of the library over the same piece. XML partitioned composites (`.vtpc`, `.vtpd`) written
!< and read back, and the two written by VTK 9.7.2 (embedded) read. A parallel Table (`.pvtt`) over two `.vtt` pieces
!< (a numeric and a string column) and a `.vtm` with a block of pieces (`action='pieces'`), read back. An OverlappingAMR
!< (`.vthb`: three ImageData boxes on two levels, `write_level`) written and read back, its refusals, and the `.vthb` of
!< VTK 9.7.2 (embedded) read. A `.vtpc` with a refused dataset still written (finalize reports it), the block name
!< `dataset` refused, two unnamed empty datasets; a block of pieces without files refused.
use penf
use vtk_fortran, only : pvd_file, pvtk_file, vtk_file, vtm_file

implicit none
character(*), parameter :: pieces(2)=['vtkfortran_read_composite_01.vtu', 'vtkfortran_read_composite_02.vtu'] !< Pieces.
type(pvtk_file)         :: a_pvtk_file    !< A parallel VTK file.
logical                 :: test_passed(10) !< List of passed tests.
integer(I4P)            :: p              !< Counter.

do p=1, size(pieces)
  call write_piece(part=p, filename=pieces(p))
enddo
test_passed(1) = check_pvtu()
test_passed(2) = check_pvtu_mismatch()
test_passed(3) = check_pvtr()
test_passed(4) = check_vtm()
test_passed(5) = check_pvd()
test_passed(6) = check_parray()
test_passed(7) = check_partitioned()
test_passed(8) = check_ptable_pieces()
test_passed(9) = check_amr()
test_passed(10) = check_part_refusals()

print "(A,L1)", new_line('a')//'Are all tests passed? ', all(test_passed)
if (.not.all(test_passed)) error stop 'some tests failed'
stop
contains
  subroutine write_piece(part, filename)
  !< Write one piece: a tetrahedron with a point and a cell array.
  integer(I4P), intent(in) :: part       !< Piece number.
  character(*), intent(in) :: filename   !< Output file name.
  type(vtk_file)           :: a_vtk_file !< A VTK file.
  integer(I4P)             :: error      !< Status error.

  error = a_vtk_file%initialize(format='raw', filename=filename, mesh_topology='UnstructuredGrid')
  error = a_vtk_file%xml_writer%write_piece(np=4, nc=1)
  error = a_vtk_file%xml_writer%write_geo(np=4, nc=1, x=[0._R8P, 1._R8P, 0._R8P, 0._R8P] + part, &
                                          y=[0._R8P, 0._R8P, 1._R8P, 0._R8P], z=[0._R8P, 0._R8P, 0._R8P, 1._R8P])
  error = a_vtk_file%xml_writer%write_connectivity(nc=1, connectivity=[0_I4P, 1_I4P, 2_I4P, 3_I4P], offset=[4_I4P], &
                                                   cell_type=[10_I1P])
  error = a_vtk_file%xml_writer%write_dataarray(location='node', action='open')
  error = a_vtk_file%xml_writer%write_dataarray(data_name='temperature', x=[1._R8P, 2._R8P, 3._R8P, 4._R8P])
  error = a_vtk_file%xml_writer%write_dataarray(location='node', action='close')
  error = a_vtk_file%xml_writer%write_dataarray(location='cell', action='open')
  error = a_vtk_file%xml_writer%write_dataarray(data_name='part', x=[part])
  error = a_vtk_file%xml_writer%write_dataarray(location='cell', action='close')
  error = a_vtk_file%xml_writer%write_piece()
  error = a_vtk_file%finalize()
  endsubroutine write_piece

  function write_pvtu(filename, node_name, node_type) result(error)
  !< Write the parallel header of the pieces, declaring a point array of the given name and type.
  character(*), intent(in) :: filename  !< Output file name.
  character(*), intent(in) :: node_name !< Name of the declared point array.
  character(*), intent(in) :: node_type !< Type of the declared point array.
  integer(I4P)             :: error     !< Status error: the largest error of all calls.
  integer(I4P)             :: p         !< Counter.

  error = abs(a_pvtk_file%initialize(filename=filename, mesh_topology='PUnstructuredGrid', mesh_kind='Float64', &
                                     ghost_level=1))
  error = max(error, abs(a_pvtk_file%xml_writer%write_dataarray(location='node', action='open')))
  error = max(error, abs(a_pvtk_file%xml_writer%write_parallel_dataarray(data_name=node_name, data_type=node_type, &
                                                                         number_of_components=1)))
  error = max(error, abs(a_pvtk_file%xml_writer%write_dataarray(location='node', action='close')))
  error = max(error, abs(a_pvtk_file%xml_writer%write_dataarray(location='cell', action='open')))
  error = max(error, abs(a_pvtk_file%xml_writer%write_parallel_dataarray(data_name='part', data_type='Int32', &
                                                                         number_of_components=1)))
  error = max(error, abs(a_pvtk_file%xml_writer%write_dataarray(location='cell', action='close')))
  do p=1, size(pieces)
    error = max(error, abs(a_pvtk_file%xml_writer%write_parallel_geo(source=pieces(p))))
  enddo
  error = max(error, abs(a_pvtk_file%finalize()))
  endfunction write_pvtu

  function check_pvtu() result(is_passed)
  !< Read a parallel header and check its pieces.
  logical                       :: is_passed    !< Check result.
  character(len=:), allocatable :: topology     !< Mesh topology.
  character(len=:), allocatable :: sources(:)   !< Files of the pieces.
  character(len=:), allocatable :: names(:)     !< Names of the declared arrays.
  character(len=:), allocatable :: data_type    !< Type of a declared array.
  character(len=:), allocatable :: message      !< Message of the check.
  real(R8P), allocatable        :: r(:)         !< Values read.
  integer(I4P)                  :: npieces      !< Number of pieces.
  integer(I4P)                  :: ghost_level  !< Number of ghost levels.
  integer(I4P)                  :: n_components !< Number of components.
  integer(I4P)                  :: e(8)         !< Errors.

  e(1) = write_pvtu(filename='vtkfortran_read_composite.pvtu', node_name='temperature', node_type='Float64')
  e(2) = a_pvtk_file%initialize(filename='vtkfortran_read_composite.pvtu', action='read')
  e(3) = a_pvtk_file%xml_reader%get_info(mesh_topology=topology, npieces=npieces, ghost_level=ghost_level)
  e(4) = a_pvtk_file%xml_reader%get_sources(sources)
  e(5) = a_pvtk_file%xml_reader%get_dataarray_names(location='node', names=names)
  e(6) = a_pvtk_file%xml_reader%get_dataarray_info(location='cell', data_name='part', data_type=data_type, &
                                                   n_components=n_components)
  e(7) = a_pvtk_file%xml_reader%check_pieces(message=message)
  ! a parallel header holds no data
  e(8) = merge(0, 1, a_pvtk_file%xml_reader%read_dataarray(location='node', data_name='temperature', x=r) == 4)
  e(1) = max(e(1), a_pvtk_file%finalize())
  is_passed = all(e == 0) .and. topology == 'PUnstructuredGrid' .and. npieces == 2 .and. ghost_level == 1 .and. &
              size(sources) == 2 .and. sources(1) == pieces(1) .and. sources(2) == pieces(2) .and.          &
              size(names) == 1 .and. names(1) == 'temperature' .and. data_type == 'Int32' .and.             &
              n_components == 1 .and. message == ''
  if (.not.is_passed) print '(A)', 'failed: vtkfortran_read_composite.pvtu'
  endfunction check_pvtu

  function check_pvtu_mismatch() result(is_passed)
  !< Check parallel headers that do not match their pieces: a missing array, a wrong type.
  logical                       :: is_passed !< Check result.
  character(len=:), allocatable :: message   !< Message of the check.
  integer(I4P)                  :: e(4)      !< Errors.

  e(1) = write_pvtu(filename='vtkfortran_read_composite-missing.pvtu', node_name='pressure', node_type='Float64')
  e(2) = a_pvtk_file%initialize(filename='vtkfortran_read_composite-missing.pvtu', action='read')
  is_passed = a_pvtk_file%xml_reader%check_pieces(message=message) == 7
  is_passed = is_passed .and. index(message, '"pressure" declared but missing') > 0
  e(3) = write_pvtu(filename='vtkfortran_read_composite-type.pvtu', node_name='temperature', node_type='Float32')
  e(4) = a_pvtk_file%initialize(filename='vtkfortran_read_composite-type.pvtu', action='read')
  is_passed = is_passed .and. a_pvtk_file%xml_reader%check_pieces(message=message) == 7
  is_passed = is_passed .and. index(message, 'is Float64 with 1 components, declared Float32 with 1') > 0
  is_passed = is_passed .and. all(e == 0) .and. a_pvtk_file%finalize() == 0
  if (.not.is_passed) print '(A)', 'failed: mismatched pvtu, '//message
  endfunction check_pvtu_mismatch

  function check_pvtr() result(is_passed)
  !< Read the whole extent and the pieces extents of a parallel rectilinear grid header.
  logical      :: is_passed !< Check result.
  integer(I4P) :: ext(6)    !< Whole extent.
  integer(I4P) :: pext(6)   !< Extent of the second piece.
  integer(I4P) :: e(5)      !< Errors.

  e(1) = a_pvtk_file%initialize(filename='vtkfortran_read_composite.pvtr', mesh_topology='PRectilinearGrid', &
                                mesh_kind='Float64', nx1=0, nx2=4, ny1=0, ny2=2, nz1=0, nz2=2)
  e(1) = e(1) + a_pvtk_file%xml_writer%write_parallel_geo(source='a.vtr', nx1=0, nx2=2, ny1=0, ny2=2, nz1=0, nz2=2)
  e(1) = e(1) + a_pvtk_file%xml_writer%write_parallel_geo(source='b.vtr', nx1=2, nx2=4, ny1=0, ny2=2, nz1=0, nz2=2)
  e(1) = e(1) + a_pvtk_file%finalize()
  e(2) = a_pvtk_file%initialize(filename='vtkfortran_read_composite.pvtr', action='read')
  e(3) = a_pvtk_file%xml_reader%get_info(nx1=ext(1), nx2=ext(2), ny1=ext(3), ny2=ext(4), nz1=ext(5), nz2=ext(6))
  e(4) = a_pvtk_file%xml_reader%read_piece(piece=2, nx1=pext(1), nx2=pext(2), ny1=pext(3), ny2=pext(4), nz1=pext(5), &
                                           nz2=pext(6))
  e(5) = a_pvtk_file%finalize()
  is_passed = all(e == 0) .and. all(ext == [0, 4, 0, 2, 0, 2]) .and. all(pext == [2, 4, 0, 2, 0, 2])
  if (.not.is_passed) print '(A)', 'failed: vtkfortran_read_composite.pvtr'
  endfunction check_pvtr

  function check_vtm() result(is_passed)
  !< Write a nested multi-block file and read back its entries.
  logical                       :: is_passed  !< Check result.
  type(vtm_file)                :: a_vtm_file !< A VTM file.
  integer(I4P),     allocatable :: level(:)   !< Nesting level of the entries.
  character(len=:), allocatable :: kind(:)    !< Kind of the entries.
  integer(I4P),     allocatable :: index(:)   !< Index of the entries.
  character(len=:), allocatable :: name(:)    !< Name of the entries.
  character(len=:), allocatable :: file(:)    !< File of the datasets.
  integer(I4P)                  :: e(8)       !< Errors.

  e(1) = a_vtm_file%initialize(filename='vtkfortran_read_composite.vtm')
  e(2) = a_vtm_file%write_block(action='open', name='assembly')
  e(3) = a_vtm_file%write_block(filenames=[pieces(1)], names=['part-a'], action='write')
  e(4) = a_vtm_file%write_block(filenames=pieces, names=['bolt-a', 'bolt-b'], name='bolts')
  e(5) = a_vtm_file%write_block(action='close')
  e(6) = a_vtm_file%finalize()
  e(7) = a_vtm_file%initialize(filename='vtkfortran_read_composite.vtm', action='read')
  e(8) = a_vtm_file%get_entries(level=level, kind=kind, index=index, name=name, file=file)
  e(1) = e(1) + a_vtm_file%finalize()
  is_passed = all(e == 0) .and. size(level) == 5
  if (is_passed) is_passed = all(level == [1, 2, 2, 3, 3]) .and. all(index == [0, 0, 1, 0, 1]) .and.          &
                             kind(1) == 'block' .and. kind(2) == 'dataset' .and. kind(3) == 'block' .and.      &
                             kind(5) == 'dataset' .and. name(1) == 'assembly' .and. name(3) == 'bolts' .and.   &
                             name(5) == 'bolt-b' .and. file(1) == '' .and. file(2) == pieces(1) .and.          &
                             file(5) == pieces(2)
  ! a file of another type is refused
  is_passed = is_passed .and. a_vtm_file%initialize(filename=pieces(1), action='read') == 2
  if (.not.is_passed) print '(A)', 'failed: vtkfortran_read_composite.vtm'
  endfunction check_vtm

  function check_pvd() result(is_passed)
  !< Write a time series and read back its datasets.
  logical                       :: is_passed   !< Check result.
  type(pvd_file)                :: a_pvd_file  !< A PVD file.
  real(R8P),        allocatable :: timestep(:) !< Time step of the datasets.
  integer(I4P),     allocatable :: part(:)     !< Part of the datasets.
  character(len=:), allocatable :: group(:)    !< Group of the datasets.
  character(len=:), allocatable :: file(:)     !< File of the datasets.
  integer(I4P)                  :: e(6)        !< Errors.

  e(1) = a_pvd_file%initialize(filename='vtkfortran_read_composite.pvd')
  e(2) = a_pvd_file%write_dataset(filename=pieces(1), timestep=0.5_R8P)
  e(3) = a_pvd_file%write_dataset(filename=pieces(2), timestep=1.25_R8P, part=1, group='fluid')
  e(4) = a_pvd_file%finalize()
  e(5) = a_pvd_file%initialize(filename='vtkfortran_read_composite.pvd', action='read')
  e(6) = a_pvd_file%get_datasets(timestep=timestep, part=part, group=group, file=file)
  e(1) = e(1) + a_pvd_file%finalize()
  is_passed = all(e == 0) .and. size(timestep) == 2
  if (is_passed) is_passed = all(timestep == [0.5_R8P, 1.25_R8P]) .and. all(part == [0, 1]) .and. group(1) == '' .and. &
                             group(2) == 'fluid' .and. file(1) == pieces(1) .and. file(2) == pieces(2)
  if (.not.is_passed) print '(A)', 'failed: vtkfortran_read_composite.pvd'
  endfunction check_pvd
  function check_parray() result(is_passed)
  !< String arrays declared in a parallel header (`PArray`): a header and its piece written by VTK 9.7.2
  !< (vtkXMLPUnstructuredGridWriter: `lab` strings, `pd` Float64 point data), embedded below; the header read back (the
  !< declared arrays, `PArray` included, checked against the piece), then a header of the library declaring the same
  !< arrays (`write_parallel_dataarray(data_type='String')` writes a `PArray`) over the same piece, read back.
  logical                       :: is_passed !< Check result.
  character(len=:), allocatable :: text      !< File content.
  character(len=:), allocatable :: names(:)  !< Declared point arrays.
  character(len=:), allocatable :: message   !< check_pieces message.
  character(len=:), allocatable :: data_type !< Declared type of lab.
  character(len=1), parameter   :: nl=new_line('a') !< New line.
  integer(I4P)                  :: u, f      !< File unit, counter.
  integer(I4P)                  :: e(6)      !< Errors.

  text = &
    '<?xml version="1.0"?>'//nl//&
    '<VTKFile type="UnstructuredGrid" version="0.1" byte_order="LittleEndian" header_type="UInt32">'//nl//&
    '  <UnstructuredGrid>'//nl//&
    '    <Piece NumberOfPoints="3" NumberOfCells="1">'//nl//&
    '      <PointData>'//nl//&
    '        <Array type="String" Name="lab" format="ascii">'//nl//&
    '          97 0 98 98 0 99 32 99 0'//nl//&
    '        </Array>'//nl//&
    '        <DataArray type="Float64" Name="pd" format="ascii" RangeMin="1.5" RangeMax="3.5">'//nl//&
    '          1.5 2.5 3.5'//nl//&
    '        </DataArray>'//nl//&
    '      </PointData>'//nl//&
    '      <CellData>'//nl//&
    '      </CellData>'//nl//&
    '      <Points>'//nl//&
    '        <DataArray type="Float32" Name="Points" NumberOfComponents="3" format="ascii" RangeMin="0" R'//&
    'angeMax="2">'//nl//&
    '          0 0 0 1 0 0'//nl//&
    '          2 0 0'//nl//&
    '          <InformationKey name="L2_NORM_RANGE" location="vtkDataArray" length="2">'//nl//&
    '            <Value index="0">'//nl//&
    '              0'//nl//&
    '            </Value>'//nl//&
    '            <Value index="1">'//nl//&
    '              2'//nl//&
    '            </Value>'//nl//&
    '          </InformationKey>'//nl//&
    '        </DataArray>'//nl//&
    '      </Points>'//nl//&
    '      <Cells>'//nl//&
    '        <DataArray type="Int64" Name="connectivity" format="ascii" RangeMin="0" RangeMax="2">'//nl//&
    '          0 1 2'//nl//&
    '        </DataArray>'//nl//&
    '        <DataArray type="Int64" Name="offsets" format="ascii" RangeMin="3" RangeMax="3">'//nl//&
    '          3'//nl//&
    '        </DataArray>'//nl//&
    '        <DataArray type="UInt8" Name="types" format="ascii" RangeMin="5" RangeMax="5">'//nl//&
    '          5'//nl//&
    '        </DataArray>'//nl//&
    '      </Cells>'//nl//&
    '    </Piece>'//nl//&
    '  </UnstructuredGrid>'//nl//&
    '</VTKFile>'
  open(newunit=u, file='ps_0.vtu', access='stream', form='unformatted', action='write', status='replace')
  write(u) text//nl
  close(u)
  text = &
    '<?xml version="1.0"?>'//nl//&
    '<VTKFile type="PUnstructuredGrid" version="0.1" byte_order="LittleEndian" header_type="UInt32">'//nl//&
    '  <PUnstructuredGrid GhostLevel="0">'//nl//&
    '    <PPointData>'//nl//&
    '      <PArray type="String" Name="lab"/>'//nl//&
    '      <PDataArray type="Float64" Name="pd"/>'//nl//&
    '    </PPointData>'//nl//&
    '    <PPoints>'//nl//&
    '      <PDataArray type="Float32" Name="Points" NumberOfComponents="3"/>'//nl//&
    '    </PPoints>'//nl//&
    '    <Piece Source="ps_0.vtu"/>'//nl//&
    '  </PUnstructuredGrid>'//nl//&
    '</VTKFile>'
  open(newunit=u, file='vtkfortran_read_composite-vtk-parray.pvtu', access='stream', form='unformatted', action='write', &
       status='replace')
  write(u) text//nl
  close(u)
  e = 0
  e(1) = abs(a_pvtk_file%initialize(filename='vtkfortran_read_composite-parray.pvtu', mesh_topology='PUnstructuredGrid', &
                                    mesh_kind='Float32'))
  e(1) = e(1) + abs(a_pvtk_file%xml_writer%write_dataarray(location='node', action='open'))
  e(1) = e(1) + abs(a_pvtk_file%xml_writer%write_parallel_dataarray(data_name='lab', data_type='String'))
  e(1) = e(1) + abs(a_pvtk_file%xml_writer%write_parallel_dataarray(data_name='pd', data_type='Float64'))
  e(1) = e(1) + abs(a_pvtk_file%xml_writer%write_dataarray(location='node', action='close'))
  e(1) = e(1) + abs(a_pvtk_file%xml_writer%write_parallel_geo(source='ps_0.vtu'))
  e(1) = e(1) + abs(a_pvtk_file%finalize())
  is_passed = e(1) == 0
  do f=1, 2
    if (f == 1) e(2) = a_pvtk_file%initialize(filename='vtkfortran_read_composite-vtk-parray.pvtu', action='read')
    if (f == 2) e(2) = a_pvtk_file%initialize(filename='vtkfortran_read_composite-parray.pvtu', action='read')
    e(3) = a_pvtk_file%xml_reader%get_dataarray_names(location='node', names=names)
    e(4) = a_pvtk_file%xml_reader%get_dataarray_info(location='node', data_name='lab', data_type=data_type)
    e(5) = a_pvtk_file%xml_reader%check_pieces(message=message)
    e(6) = a_pvtk_file%finalize()
    is_passed = is_passed .and. all(e == 0)
    if (is_passed) is_passed = size(names) == 2
    if (is_passed) is_passed = names(1) == 'lab' .and. names(2) == 'pd' .and. data_type == 'String'
    if (.not.is_passed) then
      print '(A,I0,A,*(1X,I0))', 'failed: PArray, header ', f, ': '//message, e
      exit
    endif
  enddo
  endfunction check_parray
  function check_partitioned() result(is_passed)
  !< XML partitioned composites: a PartitionedDataSetCollection (`.vtpc`: blocks `solid` with two datasets and `checked`
  !< referencing one of them, an empty dataset at the top) and a PartitionedDataSet (`.vtpd`) of the two pieces, written
  !< with `vtm_file` and read back (`get_entries`); the `.vtpc` and `.vtpd` written by VTK 9.7.2 (embedded below: a
  !< Partitions of two DataSets, one outside the assembly) read too. Refusals: a block in a `.vtpd`, a block name that
  !< is not an XML name, a dataset name used twice, a reference to an unknown dataset.
  logical                       :: is_passed !< Check result.
  type(vtm_file)                :: vtm       !< Composite file.
  character(len=:), allocatable :: text      !< File content.
  character(len=:), allocatable :: kind(:), name(:), file(:) !< Entries read.
  integer(I4P),     allocatable :: level(:), index(:)       !< Entries read.
  character(len=1), parameter   :: nl=new_line('a') !< New line.
  integer(I4P)                  :: u         !< File unit.
  integer(I4P)                  :: e(12)     !< Errors.
  logical                       :: c(7)      !< Checks.

  e = 0 ; c = .false.
  e(1) = vtm%initialize(filename='vtkfortran_read_composite.vtpc', composite='PartitionedDataSetCollection')
  e(2) = vtm%write_block(filenames=pieces, names=['part_1', 'part_2'], name='solid')
  e(3) = vtm%write_block(references='part_1', name='checked')
  e(4) = vtm%write_block(filenames=[character(1) :: ''], names=['void'], action='write')
  c(1) = vtm%write_block(filenames=[pieces(1)], names=['part_2'], name='again') /= 0 ! part_2 written twice
  e(5) = vtm%write_block(action='close')                                             ! (the block it opened)
  c(1) = c(1) .and. vtm%write_block(action='open', name='not a name') /= 0         ! not an XML element name
  c(1) = c(1) .and. vtm%write_block(references='nothing', name='bad') /= 0          ! an unknown dataset
  e(6) = vtm%write_block(action='close')                                             ! (the block it opened)
  e(7) = vtm%finalize()
  e(8) = vtm%initialize(filename='vtkfortran_read_composite.vtpd', composite='PartitionedDataSet')
  e(9) = vtm%write_block(filenames=pieces, action='write')
  c(1) = c(1) .and. vtm%write_block(action='open', name='blk') /= 0                ! no blocks in a .vtpd
  e(10) = vtm%finalize()
  ! read back
  e(11) = vtm%initialize(filename='vtkfortran_read_composite.vtpc', action='read')
  e(11) = e(11) + vtm%get_entries(level=level, kind=kind, index=index, name=name, file=file)
  e(11) = e(11) + vtm%finalize()
  ! solid (part_1, part_2), checked (part_1, referenced), void (empty, at the top), again and bad (the blocks of the
  ! refused calls, left empty)
  if (e(11) == 0) c(2) = size(level) == 8
  if (c(2)) c(2) = all(level == [1, 2, 2, 1, 2, 1, 1, 1]) .and. &
                   all(kind == [character(7) :: 'block', 'dataset', 'dataset', 'block', 'dataset', 'dataset', 'block', &
                                'block']) .and. &
                   name(1) == 'solid' .and. name(2) == 'part_1' .and. file(2) == pieces(1) .and. file(3) == pieces(2) .and. &
                   name(4) == 'checked' .and. name(5) == 'part_1' .and. file(5) == pieces(1) .and. name(6) == 'void' .and. &
                   len_trim(file(6)) == 0 .and. name(7) == 'again' .and. name(8) == 'bad'
  e(12) = vtm%initialize(filename='vtkfortran_read_composite.vtpd', action='read')
  e(12) = e(12) + vtm%get_entries(level=level, kind=kind, file=file)
  e(12) = e(12) + vtm%finalize()
  if (e(12) == 0) c(3) = size(level) == 2
  if (c(3)) c(3) = all(level == 1) .and. all(kind == 'dataset') .and. file(1) == pieces(1) .and. file(2) == pieces(2)
  ! the files written by VTK 9.7.2
  text = &
    '<?xml version="1.0"?>'//nl//&
    '<VTKFile type="vtkPartitionedDataSetCollection" version="1.0" byte_order="LittleEndian" header_type='//&
    '"UInt32" compressor="vtkZLibDataCompressor">'//nl//&
    '  <vtkPartitionedDataSetCollection>'//nl//&
    '    <Partitions index="0" name="first">'//nl//&
    '      <DataSet index="0" file="pdc/pdc_0_0.vtu"/>'//nl//&
    '      <DataSet index="1" file="pdc/pdc_0_1.vtp"/>'//nl//&
    '    </Partitions>'//nl//&
    '    <Partitions index="1">'//nl//&
    '      <DataSet index="0" file="pdc/pdc_1_0.vti"/>'//nl//&
    '    </Partitions>'//nl//&
    '    <DataAssembly encoding="base64">'//nl//&
    '      PD94bWwgdmVyc2lvbj0iMS4wIj8+Cjxyb290IHR5cGU9InZ0a0RhdGFBc3NlbWJseSIgdmVyc2lvbj0iMS4wIiBpZD0iMC'//&
    'I+CiAgPGdyb3VwIGlkPSIxIj4KICAgIDxkYXRhc2V0IGlkPSIwIiAvPgogIDwvZ3JvdXA+CiAgPGRhdGFzZXQgaWQ9IjEiIC8+Cj'//&
    'wvcm9vdD4K'//nl//&
    '    </DataAssembly>'//nl//&
    '  </vtkPartitionedDataSetCollection>'//nl//&
    '</VTKFile>'
  open(newunit=u, file='vtkfortran_read_composite-vtk.vtpc', access='stream', form='unformatted', action='write', &
       status='replace')
  write(u) text//nl
  close(u)
  text = &
    '<?xml version="1.0"?>'//nl//&
    '<VTKFile type="vtkPartitionedDataSet" version="1.0" byte_order="LittleEndian" header_type="UInt32" c'//&
    'ompressor="vtkZLibDataCompressor">'//nl//&
    '  <vtkPartitionedDataSet>'//nl//&
    '    <DataSet index="0" file="pds/pds_0.vtu"/>'//nl//&
    '    <DataSet index="1" file="pds/pds_1.vtp"/>'//nl//&
    '  </vtkPartitionedDataSet>'//nl//&
    '</VTKFile>'
  open(newunit=u, file='vtkfortran_read_composite-vtk.vtpd', access='stream', form='unformatted', action='write', &
       status='replace')
  write(u) text//nl
  close(u)
  ! .vtpc: group (block, its dataset 0: Partitions first of 2 DataSets), dataset 1 (Partitions of 1 DataSet)
  if (vtm%initialize(filename='vtkfortran_read_composite-vtk.vtpc', action='read') == 0) then
    if (vtm%get_entries(level=level, kind=kind, name=name, file=file) == 0) then
      c(4) = size(level) == 5
      if (c(4)) c(4) = all(level == [1, 2, 3, 3, 1]) .and. &
                       all(kind == [character(7) :: 'block', 'dataset', 'piece', 'piece', 'dataset']) .and. &
                       name(1) == 'group' .and. name(2) == 'first' .and. file(3) == 'pdc/pdc_0_0.vtu' .and. &
                       file(4) == 'pdc/pdc_0_1.vtp' .and. file(5) == 'pdc/pdc_1_0.vti'
    endif
  endif
  e(1) = e(1) + vtm%finalize()
  if (vtm%initialize(filename='vtkfortran_read_composite-vtk.vtpd', action='read') == 0) then
    if (vtm%get_entries(level=level, kind=kind, file=file) == 0) then
      c(5) = size(level) == 2
      if (c(5)) c(5) = file(1) == 'pds/pds_0.vtu' .and. file(2) == 'pds/pds_1.vtp'
    endif
  endif
  e(1) = e(1) + vtm%finalize()
  c(6) = vtm%initialize(filename='vtkfortran_read_composite.vtpd', format='vtkhdf', composite='PartitionedDataSet') /= 0
  c(7) = .true.
  is_passed = all(c) .and. all(e == 0)
  if (.not.is_passed) print '(A,12(1X,I0),A,7L2)', 'failed: partitioned composites, errors', e, ' checks', c
  endfunction check_partitioned
  function check_ptable_pieces() result(is_passed)
  !< A parallel Table (`.pvtt`): two `.vtt` pieces of the columns `a` (Float64) and `s` (strings), the header declaring
  !< them (`PRowData`, `s` a `PArray`), read back and checked against the pieces; a `.vtm` with a block of pieces
  !< (`write_block(..., action='pieces')`: a vtkMultiPieceDataSet), its entries read back.
  logical                       :: is_passed !< Check result.
  type(vtk_file)                :: t         !< A Table.
  type(vtm_file)                :: vtm       !< Multi-block file.
  character(len=:), allocatable :: topology, message, names(:), sources(:) !< Header read.
  character(len=:), allocatable :: kind(:), file(:) !< Entries read.
  integer(I4P),     allocatable :: level(:)  !< Entries read.
  integer(I4P)                  :: k, npieces !< Counter, pieces.
  integer(I4P)                  :: e(10)     !< Errors.

  e = 0
  do k=1, 2
    e(1) = e(1) + abs(t%initialize(format='ascii', filename='vtkfortran_read_composite_t'//trim(str(k, no_sign=.true.))// &
                                   '.vtt', mesh_topology='Table'))
    e(1) = e(1) + abs(t%xml_writer%write_dataarray(location='row', action='open'))
    e(1) = e(1) + abs(t%xml_writer%write_dataarray(data_name='a', x=[1._R8P, 2._R8P] * k))
    e(1) = e(1) + abs(t%xml_writer%write_dataarray(data_name='s', x=[character(2) :: 'x'//achar(48 + k), 'y']))
    e(1) = e(1) + abs(t%xml_writer%write_dataarray(location='row', action='close'))
    e(1) = e(1) + abs(t%finalize())
  enddo
  e(2) = a_pvtk_file%initialize(filename='vtkfortran_read_composite.pvtt', mesh_topology='PTable')
  e(2) = e(2) + abs(a_pvtk_file%xml_writer%write_dataarray(location='row', action='open'))
  e(2) = e(2) + abs(a_pvtk_file%xml_writer%write_parallel_dataarray(data_name='a', data_type='Float64'))
  e(2) = e(2) + abs(a_pvtk_file%xml_writer%write_parallel_dataarray(data_name='s', data_type='String'))
  e(2) = e(2) + abs(a_pvtk_file%xml_writer%write_dataarray(location='row', action='close'))
  e(2) = e(2) + abs(a_pvtk_file%xml_writer%write_parallel_geo(source='vtkfortran_read_composite_t1.vtt'))
  e(2) = e(2) + abs(a_pvtk_file%xml_writer%write_parallel_geo(source='vtkfortran_read_composite_t2.vtt'))
  e(2) = e(2) + abs(a_pvtk_file%finalize())
  e(3) = a_pvtk_file%initialize(filename='vtkfortran_read_composite.pvtt', action='read')
  e(4) = a_pvtk_file%xml_reader%get_info(mesh_topology=topology, npieces=npieces)
  e(5) = a_pvtk_file%xml_reader%get_dataarray_names(location='row', names=names)
  e(6) = a_pvtk_file%xml_reader%get_sources(sources)
  e(7) = a_pvtk_file%xml_reader%check_pieces(message=message)
  e(8) = a_pvtk_file%finalize()
  is_passed = all(e == 0)
  if (is_passed) is_passed = topology == 'PTable' .and. npieces == 2 .and. size(names) == 2 .and. size(sources) == 2
  if (is_passed) is_passed = names(1) == 'a' .and. names(2) == 's' .and. sources(2) == 'vtkfortran_read_composite_t2.vtt'
  if (.not.is_passed) then
    print '(A,10(1X,I0))', 'failed: vtkfortran_read_composite.pvtt '//message, e
    return
  endif
  ! a block of pieces in a .vtm
  e = 0
  e(1) = vtm%initialize(filename='vtkfortran_read_composite-pieces.vtm')
  e(2) = vtm%write_block(filenames=pieces, names=['p1', 'p2'], name='parts', action='pieces')
  e(3) = vtm%write_block(filenames=[pieces(1)], name='solid')
  e(3) = e(3) + vtm%write_block(filenames=pieces(2)//' '//pieces(1), action='pieces', name='more')
  e(4) = vtm%finalize()
  e(5) = vtm%initialize(filename='vtkfortran_read_composite-pieces.vtm', action='read')
  e(6) = vtm%get_entries(level=level, kind=kind, file=file)
  e(7) = vtm%finalize()
  ! refused outside an XML .vtm (a .vtpd here; VTKHDF likewise), array and string forms
  e(8) = vtm%initialize(filename='vtkfortran_read_composite-pieces.vtpd', composite='PartitionedDataSet')
  e(9) = merge(0, 1, vtm%write_block(filenames=pieces, action='pieces') /= 0)                   ! array form
  e(9) = e(9) + merge(0, 1, vtm%write_block(filenames=pieces(1), action='pieces') /= 0)          ! string form
  e(10) = vtm%finalize()
  is_passed = all(e == 0)
  if (is_passed) is_passed = size(level) == 8
  if (is_passed) is_passed = all(level == [1, 2, 2, 1, 2, 1, 2, 2]) .and. &
                             all(kind == [character(7) :: 'piece', 'dataset', 'dataset', 'block', 'dataset', &
                                                          'piece', 'dataset', 'dataset']) .and. &
                             file(2) == pieces(1) .and. file(3) == pieces(2) .and. file(7) == pieces(2)
  if (.not.is_passed) print '(A,10(1X,I0))', 'failed: vtkfortran_read_composite-pieces.vtm', e
  endfunction check_ptable_pieces
  function check_part_refusals() result(is_passed)
  !< A refused dataset in a `.vtpc` (a name used twice) leaves its block open: finalize closes it, writes every dataset
  !< and reports the error; the block name `dataset` (reserved by vtkDataAssembly) is refused; two empty datasets without
  !< names get names of their own; a block of pieces without files is refused.
  logical                       :: is_passed !< Check result.
  type(vtm_file)                :: vtm       !< The composite.
  character(len=:), allocatable :: kind(:), file(:) !< Entries read.
  integer(I4P),     allocatable :: level(:)  !< Entries read.
  integer(I4P)                  :: e(8)      !< Errors.

  e = 0
  e(1) = vtm%initialize(filename='vtkfortran_read_composite_refused.vtpc', composite='PartitionedDataSetCollection')
  e(1) = e(1) + vtm%write_block(filenames=[pieces(1)], names=['a'], name='x')
  e(2) = merge(0, 1, vtm%write_block(filenames=pieces, names=['b', 'a'], name='y') /= 0)  ! a twice: refused
  e(3) = vtm%write_block(filenames=[pieces(2)], names=['c'], name='z')
  e(4) = merge(0, 1, vtm%write_block(filenames=[pieces(2)], names=['d'], name='dataset') /= 0) ! a reserved name
  e(5) = vtm%write_block(filenames=[character(1) :: ' ', ' '], name='e')                  ! two unnamed empty datasets
  e(6) = merge(0, 1, vtm%finalize() /= 0)                                                  ! the open blocks reported
  e(7) = vtm%initialize(filename='vtkfortran_read_composite_refused.vtpc', action='read')
  e(7) = e(7) + vtm%get_entries(level=level, kind=kind, file=file)
  e(7) = e(7) + vtm%finalize()
  if (e(7) == 0) e(7) = merge(0, 1, count(kind == 'dataset') == 5 .and. count(file == pieces(1)) == 2 .and. &
                                    count(file == pieces(2)) == 1)
  e(8) = vtm%initialize(filename='vtkfortran_read_composite_refused.vtm')
  e(8) = e(8) + merge(0, 1, vtm%write_block(action='pieces', name='p') /= 0)                ! no files
  if (vtm%finalize() /= 0) continue
  is_passed = all(e == 0)
  if (.not.is_passed) print '(A,8(1X,I0))', 'failed: the refusals of a .vtpc, pieces without files', e
  endfunction check_part_refusals

  function check_amr() result(is_passed)
  !< An OverlappingAMR (`.vthb`): a 2D level 0 of one box (4x4 cells, spacing 1) and a level 1 of two (spacing 0.5), each
  !< box an ImageData file with the origin of the AMR and its extent in the indices of its level; read back (the levels as
  !< blocks, the boxes as datasets); the refusals (levels out of order, another origin or spacing, not an ImageData, a
  !< VTKHDF composite, write_block, write_level in a multi-block, no levels); the `.vthb` VTK 9.7.2 writes, read.
  logical                       :: is_passed !< Check result.
  type(vtk_file)                :: box       !< A box.
  type(vtm_file)                :: amr       !< The .vthb.
  character(len=:), allocatable :: kind(:), file(:) !< Entries read.
  integer(I4P),     allocatable :: level(:), index(:) !< Entries read.
  integer(I4P)                  :: u, e(12)  !< Unit, errors.

  e = 0
  e(1) = write_box('vtkfortran_read_composite_amr_0.vti', [0, 4, 0, 4], 1._R8P, [0._R8P, 0._R8P, 0._R8P])
  e(1) = e(1) + write_box('vtkfortran_read_composite_amr_1.vti', [0, 4, 0, 4], 0.5_R8P, [0._R8P, 0._R8P, 0._R8P])
  e(1) = e(1) + write_box('vtkfortran_read_composite_amr_2.vti', [4, 8, 4, 8], 0.5_R8P, [0._R8P, 0._R8P, 0._R8P])
  e(1) = e(1) + write_box('vtkfortran_read_composite_amr_shifted.vti', [4, 8, 4, 8], 0.5_R8P, [1._R8P, 0._R8P, 0._R8P])
  e(1) = e(1) + write_box('vtkfortran_read_composite_amr_coarse.vti', [4, 8, 4, 8], 1._R8P, [0._R8P, 0._R8P, 0._R8P])
  e(2) = amr%initialize(filename='vtkfortran_read_composite.vthb', composite='OverlappingAMR')
  e(3) = amr%write_level(level=0, filenames=['vtkfortran_read_composite_amr_0.vti'])
  e(4) = amr%write_level(level=1, filenames=['vtkfortran_read_composite_amr_1.vti', 'vtkfortran_read_composite_amr_2.vti'])
  e(5) = amr%finalize()
  e(6) = amr%initialize(filename='vtkfortran_read_composite.vthb', action='read')
  e(6) = e(6) + amr%get_entries(level=level, kind=kind, index=index, file=file)
  e(6) = e(6) + amr%finalize()
  is_passed = all(e == 0)
  if (is_passed) is_passed = size(level) == 5
  if (is_passed) is_passed = all(level == [1, 2, 1, 2, 2]) .and. all(index == [0, 0, 1, 0, 1]) .and. &
                             all(kind == [character(7) :: 'block', 'dataset', 'block', 'dataset', 'dataset']) .and. &
                             file(5) == 'vtkfortran_read_composite_amr_2.vti'
  if (.not.is_passed) then
    print '(A,12(1X,I0))', 'failed: vtkfortran_read_composite.vthb', e
    return
  endif
  ! refusals
  e = 0
  e(1) = amr%initialize(filename='vtkfortran_read_composite_refused.vthb', composite='OverlappingAMR')
  e(2) = merge(0, 1, amr%write_level(level=1, filenames=['vtkfortran_read_composite_amr_1.vti']) /= 0)  ! level 0 first
  e(3) = merge(0, 1, amr%write_level(level=0, filenames=[pieces(1)]) /= 0)                              ! not an ImageData
  e(4) = merge(0, 1, amr%write_level(level=0, filenames=[character(41) :: 'vtkfortran_read_composite_amr_0.vti', &
                                                         'vtkfortran_read_composite_amr_coarse.vti']) == 0) ! ok: level 0
  e(5) = merge(0, 1, amr%write_level(level=1, filenames=[character(41) :: 'vtkfortran_read_composite_amr_1.vti', &
                                                         'vtkfortran_read_composite_amr_shifted.vti']) /= 0) ! another origin
  e(6) = merge(0, 1, amr%write_level(level=1, filenames=[character(41) :: 'vtkfortran_read_composite_amr_1.vti', &
                                                         'vtkfortran_read_composite_amr_coarse.vti']) /= 0) ! another spacing
  e(7) = merge(0, 1, amr%write_block(filenames=['vtkfortran_read_composite_amr_1.vti'], name='b') /= 0) ! write_block
  e(8) = amr%finalize()                                                                                ! level 0 only
  e(9) = merge(0, 1, amr%initialize(filename='vtkfortran_read_composite_refused.vtkhdf', format='vtkhdf', &
                                    composite='OverlappingAMR') /= 0)                                ! VTKHDF
  e(10) = amr%initialize(filename='vtkfortran_read_composite_refused.vthb', composite='OverlappingAMR')
  e(10) = merge(0, 1, amr%finalize() /= 0)                                                           ! no levels
  e(11) = amr%initialize(filename='vtkfortran_read_composite_refused.vtm')
  e(11) = e(11) + merge(0, 1, amr%write_level(level=0, filenames=['vtkfortran_read_composite_amr_0.vti']) /= 0) ! a .vtm
  e(11) = e(11) + amr%finalize()
  ! the .vthb of VTK 9.7.2 (vtkXMLUniformGridAMRWriter, vtkAMRGaussianPulseSource of 2 levels in 2D)
  open(newunit=u, file='vtkfortran_read_composite_vtk.vthb', status='replace', action='write')
  write(u, '(A)') '<?xml version="1.0"?>', &
    '<VTKFile type="vtkOverlappingAMR" version="1.1" byte_order="LittleEndian" header_type="UInt32" '// &
    'compressor="vtkZLibDataCompressor">', &
    '  <vtkOverlappingAMR origin="-2 -2 0" grid_description="XY">', &
    '    <Block level="0" spacing="0.5 0.5 0.5">', &
    '      <DataSet index="0" amr_box="0 4 0 4 0 -1" file="amr/amr_0.vti"/>', &
    '    </Block>', &
    '    <Block level="1" spacing="0.25 0.25 0.5">', &
    '      <DataSet index="0" amr_box="0 3 0 5 0 -1" file="amr/amr_1.vti"/>', &
    '      <DataSet index="1" amr_box="6 9 4 9 0 -1" file="amr/amr_2.vti"/>', &
    '    </Block>', &
    '  </vtkOverlappingAMR>', &
    '</VTKFile>'
  close(u)
  e(12) = amr%initialize(filename='vtkfortran_read_composite_vtk.vthb', action='read')
  e(12) = e(12) + amr%get_entries(level=level, kind=kind, index=index, file=file)
  e(12) = e(12) + amr%finalize()
  is_passed = all(e == 0)
  if (is_passed) is_passed = size(level) == 5
  if (is_passed) is_passed = all(level == [1, 2, 1, 2, 2]) .and. all(index == [0, 0, 1, 0, 1]) .and. &
                             file(2) == 'amr/amr_0.vti' .and. file(5) == 'amr/amr_2.vti'
  if (.not.is_passed) print '(A,12(1X,I0))', 'failed: the refusals of a .vthb or the .vthb of VTK', e
  endfunction check_amr

  function write_box(filename, ext, spacing, origin) result(error)
  !< A 2D ImageData box: the point extent ext (x1, x2, y1, y2) in the indices of its level, its spacing, the origin; point
  !< data pd (the point index) and cell data cd (the x of the cell centre).
  character(*), intent(in) :: filename   !< File name.
  integer(I4P), intent(in) :: ext(4)     !< Point extent.
  real(R8P),    intent(in) :: spacing    !< Spacing (x and y; z 1).
  real(R8P),    intent(in) :: origin(3)  !< Origin.
  integer(I4P)             :: error      !< Error status.
  type(vtk_file)           :: box        !< The box.
  integer(I4P)             :: np, nc, i, j !< Points, cells, counters.

  np = (ext(2) - ext(1) + 1) * (ext(4) - ext(3) + 1)
  nc = (ext(2) - ext(1)) * (ext(4) - ext(3))
  error = box%initialize(format='ascii', filename=filename, mesh_topology='ImageData', nx1=ext(1), nx2=ext(2), &
                         ny1=ext(3), ny2=ext(4), nz1=0, nz2=0, origin=origin, spacing=[spacing, spacing, 1._R8P])
  error = error + abs(box%xml_writer%write_piece(nx1=ext(1), nx2=ext(2), ny1=ext(3), ny2=ext(4), nz1=0, nz2=0))
  error = error + abs(box%xml_writer%write_dataarray(location='node', action='open'))
  error = error + abs(box%xml_writer%write_dataarray(data_name='pd', x=[(real(i, R8P), i=1, np)]))
  error = error + abs(box%xml_writer%write_dataarray(location='node', action='close'))
  error = error + abs(box%xml_writer%write_dataarray(location='cell', action='open'))
  error = error + abs(box%xml_writer%write_dataarray(data_name='cd', x=[((origin(1) + (i + 0.5_R8P) * spacing, &
                                                     i=ext(1), ext(2) - 1), j=ext(3), ext(4) - 1)]))
  error = error + abs(box%xml_writer%write_dataarray(location='cell', action='close'))
  error = error + abs(box%xml_writer%write_piece())
  error = error + abs(box%finalize())
  if (nc < 0) error = 1
  endfunction write_box
endprogram vtk_fortran_read_composite
