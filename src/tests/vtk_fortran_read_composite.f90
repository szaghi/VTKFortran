!< VTK_Fortran test: read parallel headers, multi-block and time series files (action='read').
program vtk_fortran_read_composite
!< VTK_Fortran test: read parallel headers, multi-block and time series files (action='read').
!<
!< A parallel unstructured grid of two tetrahedra pieces is written and its header read back (information, declared arrays,
!< pieces) and checked against the pieces: a correct header, one declaring a missing array and one declaring a wrong type.
!< The pieces extents of a parallel rectilinear grid header are read. A nested multi-block file and a time series are
!< written and their entries read back.
use penf
use vtk_fortran, only : pvd_file, pvtk_file, vtk_file, vtm_file

implicit none
character(*), parameter :: pieces(2)=['vtkfortran_read_composite_01.vtu', 'vtkfortran_read_composite_02.vtu'] !< Pieces.
type(pvtk_file)         :: a_pvtk_file    !< A parallel VTK file.
logical                 :: test_passed(5) !< List of passed tests.
integer(I4P)            :: p              !< Counter.

do p=1, size(pieces)
  call write_piece(part=p, filename=pieces(p))
enddo
test_passed(1) = check_pvtu()
test_passed(2) = check_pvtu_mismatch()
test_passed(3) = check_pvtr()
test_passed(4) = check_vtm()
test_passed(5) = check_pvd()

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
endprogram vtk_fortran_read_composite
