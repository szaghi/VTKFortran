!< VTK_Fortran test: one VTKHDF file written by the ranks of an MPI communicator (`vtk_file%initialize(..., comm=)`).
program vtk_fortran_write_vtkhdf_ranks
!< VTK_Fortran test: one VTKHDF file written by the ranks of an MPI communicator (`vtk_file%initialize(..., comm=)`).
!<
!< With MPI (`VTKFORTRAN_USE_MPI`, run on 4 ranks: `mpiexec -n 4`) every rank writes its piece of two files, each piece
!< a partition:
!<
!<- `vtkfortran_unst_mpi.vtkhdf`: an UnstructuredGrid, rank r with 1, 2, 0, 3 tetrahedra (rank 2 an empty piece), the
!<  ids I4P on the even ranks and I8P on the odd ones, point data (R8P), cell data (I4P), field data;
!<- `vtkfortran_poly_mpi.vtkhdf`: a PolyData, a vertex and a poly-vertex on rank 0, a polyline on rank 1 (I8P ids), a
!<  triangle and a quad on rank 2, points without cells on rank 3, point data;
!<- `vtkfortran_image_mpi.vtkhdf`, `vtkfortran_rect_mpi.vtkhdf`, `vtkfortran_strg_mpi.vtkhdf`: an ImageData, a
!<  RectilinearGrid and a StructuredGrid of extent 0:6 x 0:3 x 0:1, each rank writing its region: on 4 ranks
!<  0:3 x 0:1 x 0:1, 3:6 x 0:1 x 0:1, 0:6 x 1:3 x 0:1 and an empty one (on other numbers of ranks rank 0 the whole
!<  extent, the others empty regions), with point data (R8P) and cell data (I4P) of the global point and cell indices.
!<
!<- `vtkfortran_series_unst_mpi.vtkhdf`: a time series of an UnstructuredGrid, every rank its piece of each step (the
!<  pieces change size from step to step, one empty), steps 1-2, then the ranks continue it (`action='append'` with
!<  `comm`, a parallel restart) with step 3 and step 4, which reuses the mesh of step 3 (each rank the point data of its
!<  part), field data at every step; `vtkfortran_series_poly_mpi.vtkhdf`: a PolyData series of three steps.
!<
!< `vtkfortran_polyh_mpi.vtkhdf`: a time series of two steps whose polyhedra appear at step 2, on the odd ranks only (a
!< tetrahedron on every rank at step 1, a hexahedron as a polyhedron on the odd ranks at step 2); with an HDF5 that
!< writes deflated datasets from ranks (`h5_parallel_filters_available`), `vtkfortran_unst_zlib_mpi.vtkhdf` and
!< `vtkfortran_poly_zlib_mpi.vtkhdf`, the UnstructuredGrid and the PolyData compressed (skipped with a message
!< otherwise).
!<
!< Rank 0 then writes the same pieces (the same grids, the same series) alone, in rank order: a serial `.vtkhdf` of
!< each, compared here dataset by dataset (values: the parallel file stores the widest kind of ids the ranks give), and
!< a `.vtu`/`.vtp`/`.vti`/`.vtr`/`.vts` of each (VTK readers compare the two outside this test). The refusals of
!< `comm` are checked too, those of the grid regions (outside the whole extent, overlapping, not covering it, two per
!< rank), of the time series (different times on the ranks, a grid) and of a Table included. Without MPI `comm` is
!< refused.
!<
!< Its name has no "mpi": `scripts/run_tests.sh` runs those with `mpirun` in every build, MPI or not. Without MPI it
!< runs alone; with MPI on any number of ranks (CTest: 4).
use penf
use vtk_fortran, only : vtk_file
use vtk_fortran_hdf5
#ifdef VTKFORTRAN_USE_MPI
use mpi_f08
#endif

implicit none
type(vtk_file)          :: a_vtk_file     !< VTK file.
integer(I4P)            :: rank, n_ranks  !< MPI rank, ranks.
integer(I4P)            :: comm           !< MPI communicator (Fortran handle).
integer(I4P)            :: e(16)          !< Errors.
logical                 :: test_passed(4) !< List of passed tests.
character(*), parameter :: GRIDS(3) = ['ImageData      ', 'RectilinearGrid', 'StructuredGrid '] !< Grid topologies.
character(*), parameter :: GRID_NAMES(3) = ['image', 'rect ', 'strg '] !< Their names in the files.
character(*), parameter :: GRID_XML(3) = ['.vti', '.vtr', '.vts']      !< Their XML extensions.
character(*), parameter :: GRID_DATASETS(3) = [character(63) :: 'PointData/pd CellData/cd', &
                                               'XCoordinates YCoordinates ZCoordinates PointData/pd CellData/cd', &
                                               'Points PointData/pd CellData/cd'] !< Datasets compared.
integer(I4P)            :: g              !< Counter.
character(*), parameter :: SERIES_DATASETS = 'Steps/Values Steps/PartOffsets Steps/NumberOfParts Steps/PointOffsets '// &
  'Steps/CellOffsets Steps/ConnectivityIdOffsets Steps/FieldDataOffsets/TIME Steps/FieldDataSizes/TIME FieldData/TIME'
character(*), parameter :: UNST_DATASETS = 'NumberOfPoints NumberOfCells NumberOfConnectivityIds Points Connectivity '// &
                                           'Offsets Types PointData/pd CellData/cd FieldData/TIME'
character(*), parameter :: POLY_DATASETS = 'NumberOfPoints Points PointData/pd '//                                      &
  'Vertices/NumberOfCells Vertices/NumberOfConnectivityIds Vertices/Connectivity Vertices/Offsets '//                 &
  'Lines/NumberOfCells Lines/NumberOfConnectivityIds Lines/Connectivity Lines/Offsets '//                             &
  'Strips/NumberOfCells Strips/NumberOfConnectivityIds Strips/Connectivity Strips/Offsets '//                         &
  'Polygons/NumberOfCells Polygons/NumberOfConnectivityIds Polygons/Connectivity Polygons/Offsets'

test_passed = .false.
#ifdef VTKFORTRAN_USE_MPI
call MPI_Init
call MPI_Comm_rank(MPI_COMM_WORLD, rank)
call MPI_Comm_size(MPI_COMM_WORLD, n_ranks)
comm = MPI_COMM_WORLD%MPI_VAL
#else
rank = 0 ; n_ranks = 1 ; comm = 0
#endif
if (.not.is_mpi_enabled) then
  test_passed = a_vtk_file%initialize(format='vtkhdf', filename='refused.vtkhdf', mesh_topology='UnstructuredGrid', &
                                      comm=comm) /= 0
  print '(A)', 'MPI not enabled: comm is refused'
else
  ! every rank its piece of the two files
  e = 0
  e(1) = a_vtk_file%initialize(format='vtkhdf', filename='vtkfortran_unst_mpi.vtkhdf', mesh_topology='UnstructuredGrid', &
                               comm=comm)
  e(2) = unst_piece(a_vtk_file, rank, field=.true.)
  e(3) = abs(a_vtk_file%finalize())
  e(4) = a_vtk_file%initialize(format='vtkhdf', filename='vtkfortran_poly_mpi.vtkhdf', mesh_topology='PolyData', comm=comm)
  e(5) = poly_piece(a_vtk_file, rank, cells_call=.true.)
  e(6) = abs(a_vtk_file%finalize())
  do g=1, 3 ! the grids, a region per rank
    e(6+g) = grid_file('vtkhdf', GRIDS(g), 'vtkfortran_'//trim(GRID_NAMES(g))//'_mpi.vtkhdf', is_parallel=.true.)
  enddo
  ! the time series, a piece per rank in each step; the UnstructuredGrid continued by the ranks after a restart
  e(10) = series_unst('vtkfortran_series_unst_mpi.vtkhdf', is_parallel=.true., first=1, last=2)
  e(11) = series_unst('vtkfortran_series_unst_mpi.vtkhdf', is_parallel=.true., first=3, last=4)
  e(12) = series_poly('vtkfortran_series_poly_mpi.vtkhdf', is_parallel=.true.)
  e(13) = series_polyh('vtkfortran_polyh_mpi.vtkhdf', is_parallel=.true.)
  if (h5_parallel_filters_available()) then ! the UnstructuredGrid and the PolyData compressed
    e(14) = a_vtk_file%initialize(format='vtkhdf', filename='vtkfortran_unst_zlib_mpi.vtkhdf', &
                                  mesh_topology='UnstructuredGrid', compressor='zlib', comm=comm)
    e(14) = max(e(14), abs(unst_piece(a_vtk_file, rank, field=.true.)), abs(a_vtk_file%finalize()))
    e(15) = a_vtk_file%initialize(format='vtkhdf', filename='vtkfortran_poly_zlib_mpi.vtkhdf', mesh_topology='PolyData', &
                                  compressor='zlib', comm=comm)
    e(15) = max(e(15), abs(poly_piece(a_vtk_file, rank, cells_call=.true.)), abs(a_vtk_file%finalize()))
  elseif (rank == 0) then
    print '(A)', 'HDF5 without parallel filtered writes (deflate): the compressed files from ranks are skipped'
  endif
  test_passed(1) = all(e(1:15) == 0)
  if (.not.test_passed(1)) print '(A,I0,A,15I2)', 'rank ', rank, ' errors of the parallel files: ', e(1:15)
  test_passed(2) = refusals()

  ! rank 0: the same pieces alone, compared with the parallel files
  test_passed(3:4) = .true.
  if (rank == 0) then
    e = 0
    e(1) = serial_unst('vtkhdf', 'serial_unst_mpi.vtkhdf')
    e(2) = serial_unst('raw', 'vtkfortran_unst_mpi.vtu')
    e(3) = serial_poly('vtkhdf', 'serial_poly_mpi.vtkhdf')
    e(4) = serial_poly('raw', 'vtkfortran_poly_mpi.vtp')
    do g=1, 3
      e(4+g) = grid_file('vtkhdf', GRIDS(g), 'serial_'//trim(GRID_NAMES(g))//'_mpi.vtkhdf', is_parallel=.false.)
      e(7+g) = grid_file('raw', GRIDS(g), 'vtkfortran_'//trim(GRID_NAMES(g))//'_mpi'//GRID_XML(g), is_parallel=.false.)
    enddo
    e(11) = series_unst('serial_series_unst_mpi.vtkhdf', is_parallel=.false., first=1, last=4)
    e(12) = series_poly('serial_series_poly_mpi.vtkhdf', is_parallel=.false.)
    e(13) = series_polyh('serial_polyh_mpi.vtkhdf', is_parallel=.false.)
    test_passed(3) = all(e(1:13) == 0)
    if (.not.test_passed(3)) print '(A,13I2)', 'errors of the serial files: ', e(1:13)
    test_passed(4) = same_datasets('vtkfortran_unst_mpi.vtkhdf', 'serial_unst_mpi.vtkhdf', UNST_DATASETS)
    test_passed(4) = same_datasets('vtkfortran_poly_mpi.vtkhdf', 'serial_poly_mpi.vtkhdf', POLY_DATASETS) .and. test_passed(4)
    test_passed(4) = same_datasets('vtkfortran_series_unst_mpi.vtkhdf', 'serial_series_unst_mpi.vtkhdf', &
                                   UNST_DATASETS//' '//SERIES_DATASETS//' Steps/PointDataOffsets/pd Steps/CellDataOffsets/cd') &
                     .and. test_passed(4)
    if (n_ranks > 1) then ! polyhedra on the odd ranks
      test_passed(4) = same_datasets('vtkfortran_polyh_mpi.vtkhdf', 'serial_polyh_mpi.vtkhdf', 'NumberOfPoints '// &
                                     'NumberOfCells NumberOfConnectivityIds Points Connectivity Offsets Types '// &
                                     'FaceConnectivity FaceOffsets PolyhedronToFaces PolyhedronOffsets NumberOfFaces '// &
                                     'NumberOfFaceConnectivityIds NumberOfPolyhedronToFaceIds Steps/PartOffsets '// &
                                     'Steps/FaceConnectivityOffsets Steps/FaceOffsetsOffsets Steps/PolyhedronToFaceIdOffsets') &
                       .and. test_passed(4)
    else ! one rank, even: no polyhedra
      test_passed(4) = same_datasets('vtkfortran_polyh_mpi.vtkhdf', 'serial_polyh_mpi.vtkhdf', 'NumberOfPoints '// &
                                     'NumberOfCells Points Connectivity Offsets Types Steps/PartOffsets') .and. test_passed(4)
    endif
    if (h5_parallel_filters_available()) then
      test_passed(4) = same_datasets('vtkfortran_unst_zlib_mpi.vtkhdf', 'serial_unst_mpi.vtkhdf', UNST_DATASETS) .and. &
                       test_passed(4)
      test_passed(4) = same_datasets('vtkfortran_poly_zlib_mpi.vtkhdf', 'serial_poly_mpi.vtkhdf', POLY_DATASETS) .and. &
                       test_passed(4)
    endif
    test_passed(4) = same_datasets('vtkfortran_series_poly_mpi.vtkhdf', 'serial_series_poly_mpi.vtkhdf', &
                                   POLY_DATASETS//' Steps/Values Steps/PartOffsets Steps/NumberOfParts Steps/PointOffsets '// &
                                   'Steps/CellOffsets Steps/ConnectivityIdOffsets Steps/PointDataOffsets/pd') .and. test_passed(4)
    do g=1, 3
      test_passed(4) = same_datasets('vtkfortran_'//trim(GRID_NAMES(g))//'_mpi.vtkhdf', &
                                     'serial_'//trim(GRID_NAMES(g))//'_mpi.vtkhdf', trim(GRID_DATASETS(g))) .and. test_passed(4)
    enddo
  endif
#ifdef VTKFORTRAN_USE_MPI
  call MPI_Allreduce(MPI_IN_PLACE, test_passed, size(test_passed), MPI_LOGICAL, MPI_LAND, MPI_COMM_WORLD)
#endif
  if (rank == 0) print '(A,I0,A,4L2)', 'VTKHDF from ', n_ranks, ' ranks: parallel files, refusals, serial files, data:', &
                       test_passed
endif

if (rank == 0) print "(A,L1)", new_line('a')//'Are all tests passed? ', all(test_passed)
#ifdef VTKFORTRAN_USE_MPI
call MPI_Finalize
#endif
if (.not.all(test_passed)) error stop 'some tests failed'
stop
contains
  function unst_piece(vtk, r, field) result(error)
  !< The piece of rank r of the UnstructuredGrid: 1, 2, 0, 3 tetrahedra (cycling), 4 points each, moved along x by the
  !< rank, ids I4P on the even ranks, I8P on the odd ones; with `field`, the field data before it.
  type(vtk_file), intent(inout) :: vtk   !< VTK file.
  integer(I4P),   intent(in)    :: r     !< Rank.
  logical,        intent(in)    :: field !< Write the field data.
  integer(I4P)                  :: error !< Error status.
  integer(I4P), parameter       :: TETRAS(0:3) = [1, 2, 0, 3]
  integer(I4P)                  :: nt, np, t, p
  real(R8P),    allocatable     :: x(:), y(:), z(:)
  integer(I4P), allocatable     :: connectivity(:), offset(:)

  error = 0
  if (field) then
    error = vtk%xml_writer%write_fielddata(action='open')
    error = max(error, abs(vtk%xml_writer%write_fielddata(data_name='TIME', x=1.5_R8P)))
    error = max(error, abs(vtk%xml_writer%write_fielddata(action='close')))
  endif
  nt = TETRAS(mod(r, 4))
  np = 4 * nt
  x = [(real(10 * r + 2 * (p / 4) + merge(1, 0, mod(p, 4) == 1), R8P), p=0, np-1)]
  y = [(real(merge(1, 0, mod(p, 4) == 2), R8P), p=0, np-1)]
  z = [(real(merge(1, 0, mod(p, 4) == 3), R8P), p=0, np-1)]
  connectivity = [(p, p=0, np-1)]
  offset = [(4 * t, t=1, nt)]
  error = max(error, abs(vtk%xml_writer%write_piece(np=np, nc=nt)))
  error = max(error, abs(vtk%xml_writer%write_geo(np=np, nc=nt, x=x, y=y, z=z)))
  if (mod(r, 2) == 0) then
    error = max(error, abs(vtk%xml_writer%write_connectivity(nc=nt, connectivity=connectivity, offset=offset, &
                                                            cell_type=[(10_I1P, t=1, nt)])))
  else
    error = max(error, abs(vtk%xml_writer%write_connectivity(nc=int(nt, I8P), connectivity=int(connectivity, I8P), &
                                                            offset=int(offset, I8P), cell_type=[(10_I1P, t=1, nt)])))
  endif
  error = max(error, abs(vtk%xml_writer%write_dataarray(location='node', action='open')))
  error = max(error, abs(vtk%xml_writer%write_dataarray(data_name='pd', x=[(real(100 * r + p, R8P), p=1, np)])))
  error = max(error, abs(vtk%xml_writer%write_dataarray(location='node', action='close')))
  error = max(error, abs(vtk%xml_writer%write_dataarray(location='cell', action='open')))
  error = max(error, abs(vtk%xml_writer%write_dataarray(data_name='cd', x=[(100 * r + t, t=1, nt)])))
  error = max(error, abs(vtk%xml_writer%write_dataarray(location='cell', action='close')))
  error = max(error, abs(vtk%xml_writer%write_piece()))
  endfunction unst_piece

  function poly_piece(vtk, r, cells_call) result(error)
  !< The piece of rank r of the PolyData (cycling): a vertex and a poly-vertex (3 points), a polyline (4 points, I8P ids),
  !< a triangle and a quad (5 points), 2 points without cells (write_polydata_cells without blocks if `cells_call`).
  type(vtk_file), intent(inout) :: vtk        !< VTK file.
  integer(I4P),   intent(in)    :: r          !< Rank.
  logical,        intent(in)    :: cells_call !< Call write_polydata_cells for a piece without cells.
  integer(I4P)                  :: error      !< Error status.
  integer(I4P), parameter       :: NP(0:3) = [3, 4, 5, 2]
  integer(I4P)                  :: n, p

  n = NP(mod(r, 4))
  select case(mod(r, 4))
  case(0)
    error = vtk%xml_writer%write_piece(np=n, nverts=2, nlines=0, nstrips=0, npolys=0)
  case(1)
    error = vtk%xml_writer%write_piece(np=n, nverts=0, nlines=1, nstrips=0, npolys=0)
  case(2)
    error = vtk%xml_writer%write_piece(np=n, nverts=0, nlines=0, nstrips=0, npolys=2)
  case default
    error = vtk%xml_writer%write_piece(np=n, nverts=0, nlines=0, nstrips=0, npolys=0)
  endselect
  error = max(error, abs(vtk%xml_writer%write_geo(np=n, nc=0, x=[(real(10 * r + p, R8P), p=1, n)],         &
                                                  y=[(real(mod(p, 2), R8P), p=1, n)], z=[(0._R8P, p=1, n)])))
  select case(mod(r, 4))
  case(0)
    error = max(error, abs(vtk%xml_writer%write_polydata_cells(verts_connectivity=[0, 1, 2], verts_offset=[1, 3])))
  case(1)
    error = max(error, abs(vtk%xml_writer%write_polydata_cells(lines_connectivity=[0_I8P, 1_I8P, 2_I8P, 3_I8P], &
                                                               lines_offset=[4_I8P])))
  case(2)
    error = max(error, abs(vtk%xml_writer%write_polydata_cells(polys_connectivity=[0, 1, 2, 1, 3, 4, 2], &
                                                               polys_offset=[3, 7])))
  case default
    if (cells_call) error = max(error, abs(vtk%xml_writer%write_polydata_cells()))
  endselect
  error = max(error, abs(vtk%xml_writer%write_dataarray(location='node', action='open')))
  error = max(error, abs(vtk%xml_writer%write_dataarray(data_name='pd', x=[(100 * r + p, p=1, n)])))
  error = max(error, abs(vtk%xml_writer%write_dataarray(location='node', action='close')))
  error = max(error, abs(vtk%xml_writer%write_piece()))
  endfunction poly_piece

  function serial_unst(format, filename) result(error)
  !< The pieces of all the ranks, in rank order, written by one rank.
  character(*), intent(in) :: format   !< Format.
  character(*), intent(in) :: filename !< File name.
  integer(I4P)             :: error    !< Error status.
  integer(I4P)             :: r        !< Counter.

  error = a_vtk_file%initialize(format=format, filename=filename, mesh_topology='UnstructuredGrid')
  do r=0, n_ranks-1
    error = max(error, abs(unst_piece(a_vtk_file, r, field=r == 0)))
  enddo
  error = max(error, abs(a_vtk_file%finalize()))
  endfunction serial_unst

  function serial_poly(format, filename) result(error)
  !< The pieces of all the ranks, in rank order, written by one rank (a .vtp piece without cells has no cells call).
  character(*), intent(in) :: format   !< Format.
  character(*), intent(in) :: filename !< File name.
  integer(I4P)             :: error    !< Error status.
  integer(I4P)             :: r        !< Counter.

  error = a_vtk_file%initialize(format=format, filename=filename, mesh_topology='PolyData')
  do r=0, n_ranks-1
    error = max(error, abs(poly_piece(a_vtk_file, r, cells_call=format == 'vtkhdf')))
  enddo
  error = max(error, abs(a_vtk_file%finalize()))
  endfunction serial_poly

  logical function same_datasets(file_a, file_b, names)
  !< The datasets `names` (blank separated) of the group VTKHDF of the two files hold the same values (read as R8P).
  character(*), intent(in) :: file_a, file_b !< Files.
  character(*), intent(in) :: names          !< Dataset names.
  integer(hid_kind)        :: fa, fb         !< Files.
  real(R8P), allocatable   :: a(:), b(:)     !< Values.
  integer(I4P)             :: first, last    !< Bounds of a name.
  integer(I4P)             :: error          !< Error status.

  same_datasets = h5_file_open(file_a, fa) == 0 .and. h5_file_open(file_b, fb) == 0
  first = 1
  do while (same_datasets .and. first <= len_trim(names))
    last = index(names(first:)//' ', ' ') + first - 2
    error = h5_dataset_read(fa, 'VTKHDF/'//names(first:last), a)
    error = error + h5_dataset_read(fb, 'VTKHDF/'//names(first:last), b)
    if (error == 0) then
      same_datasets = size(a) == size(b)
      if (same_datasets) same_datasets = all(a == b)
    else
      same_datasets = .false.
    endif
    if (.not.same_datasets) print '(A)', 'different dataset: '//names(first:last)//' of '//file_a
    first = last + 2
  enddo
  error = h5_file_close(fa)
  error = h5_file_close(fb)
  endfunction same_datasets

  function region(r) result(extent)
  !< The region of rank r of the grids (extent 0:6 x 0:3 x 0:1): on 4 ranks two regions along x of the rows 0:1, the
  !< rows 1:3 and an empty one; otherwise rank 0 the whole extent, the others empty regions.
  integer(I4P), intent(in) :: r          !< Rank.
  integer(I4P)             :: extent(6)  !< Region.

  extent = [0, -1, 0, -1, 0, -1] ! empty
  if (n_ranks == 4) then
    select case(r)
    case(0) ; extent = [0, 3, 0, 1, 0, 1]
    case(1) ; extent = [3, 6, 0, 1, 0, 1]
    case(2) ; extent = [0, 6, 1, 3, 0, 1]
    endselect
  elseif (r == 0) then
    extent = [0, 6, 0, 3, 0, 1]
  endif
  endfunction region

  function series_unst(filename, is_parallel, first, last) result(error)
  !< The steps `first` to `last` of the UnstructuredGrid series (a new file from step 1, the file continued otherwise):
  !< at step s the piece of rank r is the piece r+s of unst_piece (its size changes), and the field data TIME; step 4
  !< has no pieces: it reuses the mesh of step 3, each rank the point data of its part. By every rank of comm
  !< (`is_parallel`), or by one rank, the pieces of all the ranks in rank order.
  character(*), intent(in) :: filename    !< File name.
  logical,      intent(in) :: is_parallel !< Written by the ranks, otherwise by one rank.
  integer(I4P), intent(in) :: first, last !< Steps written.
  integer(I4P)             :: error       !< Error status.
  integer(I4P)             :: s, r, p     !< Counters.

  error = 0
  if (first == 1 .and. is_parallel) then
    error = a_vtk_file%initialize(format='vtkhdf', filename=filename, mesh_topology='UnstructuredGrid', comm=comm)
  elseif (first == 1) then
    error = a_vtk_file%initialize(format='vtkhdf', filename=filename, mesh_topology='UnstructuredGrid')
  else
    error = a_vtk_file%initialize(format='vtkhdf', filename=filename, action='append', comm=comm)
  endif
  do s=first, last
    error = max(error, abs(a_vtk_file%xml_writer%write_step(time=0.5_R8P * s)))
    if (s <= 3) then
      if (is_parallel) then
        error = max(error, abs(unst_piece(a_vtk_file, rank + s, field=.true.)))
      else
        do r=0, n_ranks-1
          error = max(error, abs(unst_piece(a_vtk_file, r + s, field=r == 0)))
        enddo
      endif
    else ! the mesh of step 3: point data only, those of the part of each rank
      error = max(error, abs(a_vtk_file%xml_writer%write_dataarray(location='node', action='open')))
      if (is_parallel) then
        error = max(error, abs(a_vtk_file%xml_writer%write_dataarray(data_name='pd', &
                                                                     x=[(real(1000 * rank + p, R8P), p=1, part_points(rank))])))
      else
        error = max(error, abs(a_vtk_file%xml_writer%write_dataarray(data_name='pd', &
                               x=[((real(1000 * r + p, R8P), p=1, part_points(r)), r=0, n_ranks-1)])))
      endif
      error = max(error, abs(a_vtk_file%xml_writer%write_dataarray(location='node', action='close')))
    endif
  enddo
  error = max(error, abs(a_vtk_file%finalize()))
  endfunction series_unst

  function series_polyh(filename, is_parallel) result(error)
  !< Two steps: a tetrahedron on every rank at step 1; at step 2 a hexahedron as a polyhedron on the odd ranks, a
  !< tetrahedron on the even ones (the polyhedra start at a later step, on some ranks only). By every rank of comm
  !< (`is_parallel`), or by one rank, the pieces of all the ranks in rank order.
  character(*), intent(in) :: filename    !< File name.
  logical,      intent(in) :: is_parallel !< Written by the ranks, otherwise by one rank.
  integer(I4P)             :: error       !< Error status.
  integer(I4P), parameter  :: cube(31) = [6, 4, 0, 3, 2, 1,  4, 4, 5, 6, 7,  4, 0, 1, 5, 4,  4, 1, 2, 6, 5, &
                                          4, 2, 3, 7, 6,  4, 3, 0, 4, 7] !< Faces of a unit cube.
  integer(I4P)             :: s, r, p     !< Counters.

  if (is_parallel) then
    error = a_vtk_file%initialize(format='vtkhdf', filename=filename, mesh_topology='UnstructuredGrid', comm=comm)
  else
    error = a_vtk_file%initialize(format='vtkhdf', filename=filename, mesh_topology='UnstructuredGrid')
  endif
  do s=1, 2
    error = max(error, abs(a_vtk_file%xml_writer%write_step(time=real(s, R8P))))
    do r=0, n_ranks-1
      if (is_parallel .and. r /= rank) cycle
      if (s == 2 .and. mod(r, 2) == 1) then
        error = max(error, abs(a_vtk_file%xml_writer%write_piece(np=8, nc=1)))
        error = max(error, abs(a_vtk_file%xml_writer%write_geo(np=8, nc=1, x=[(real(mod(p, 2) + 3 * r, R8P), p=0, 7)], &
                               y=[(real(mod(p / 2, 2), R8P), p=0, 7)], z=[(real(p / 4, R8P), p=0, 7)])))
        error = max(error, abs(a_vtk_file%xml_writer%write_connectivity(nc=1, connectivity=[0, 1, 3, 2, 4, 5, 7, 6], &
                               offset=[8], cell_type=[42_I1P], face=cube, faceoffset=[31])))
      else
        error = max(error, abs(a_vtk_file%xml_writer%write_piece(np=4, nc=1)))
        error = max(error, abs(a_vtk_file%xml_writer%write_geo(np=4, nc=1, x=[0._R8P, 1._R8P, 0._R8P, 0._R8P] + 3 * r, &
                               y=[0._R8P, 0._R8P, 1._R8P, 0._R8P], z=[0._R8P, 0._R8P, 0._R8P, 1._R8P])))
        error = max(error, abs(a_vtk_file%xml_writer%write_connectivity(nc=1, connectivity=[0, 1, 2, 3], offset=[4], &
                                                                         cell_type=[10_I1P])))
      endif
      error = max(error, abs(a_vtk_file%xml_writer%write_piece()))
    enddo
  enddo
  error = max(error, abs(a_vtk_file%finalize()))
  endfunction series_polyh

  pure function part_points(r) result(np)
  !< The points of the piece of rank r at step 3 of the UnstructuredGrid series (unst_piece r+3: 4 per tetrahedron).
  integer(I4P), intent(in) :: r  !< Rank.
  integer(I4P)             :: np !< Points.
  integer(I4P), parameter  :: TETRAS(0:3) = [1, 2, 0, 3]

  np = 4 * TETRAS(mod(r + 3, 4))
  endfunction part_points

  function series_poly(filename, is_parallel) result(error)
  !< The PolyData series: at step s the piece of rank r is the piece r+s of poly_piece; by every rank of comm, or by one.
  character(*), intent(in) :: filename    !< File name.
  logical,      intent(in) :: is_parallel !< Written by the ranks, otherwise by one rank.
  integer(I4P)             :: error       !< Error status.
  integer(I4P)             :: s, r        !< Counters.

  if (is_parallel) then
    error = a_vtk_file%initialize(format='vtkhdf', filename=filename, mesh_topology='PolyData', comm=comm)
  else
    error = a_vtk_file%initialize(format='vtkhdf', filename=filename, mesh_topology='PolyData')
  endif
  do s=1, 3
    error = max(error, abs(a_vtk_file%xml_writer%write_step(time=real(s, R8P))))
    if (is_parallel) then
      error = max(error, abs(poly_piece(a_vtk_file, rank + s, cells_call=.true.)))
    else
      do r=0, n_ranks-1
        error = max(error, abs(poly_piece(a_vtk_file, r + s, cells_call=.true.)))
      enddo
    endif
  enddo
  error = max(error, abs(a_vtk_file%finalize()))
  endfunction series_poly

  pure function region_points(extent) result(n)
  !< The points of a region (0 for an empty one).
  integer(I4P), intent(in) :: extent(6) !< Region.
  integer(I4P)             :: n         !< Points.

  n = product(max(extent(2::2) - extent(1::2) + 1, 0))
  endfunction region_points

  function grid_file(format, topology, filename, is_parallel) result(error)
  !< A grid of extent 0:6 x 0:3 x 0:1: written by every rank, its region (`is_parallel`), or by one rank, whole. Point data
  !< pd = 100 k + 10 j + i, cell data cd = 1000 + 100 k + 10 j + i (global indices); coordinates x = i^2/4, y = 1.5 j,
  !< z = 0.5 k (RectilinearGrid); points sheared (StructuredGrid).
  character(*), intent(in)  :: format      !< Format.
  character(*), intent(in)  :: topology    !< Topology.
  character(*), intent(in)  :: filename    !< File name.
  logical,      intent(in)  :: is_parallel !< A region per rank, otherwise the whole grid.
  integer(I4P)              :: error       !< Error status.
  integer(I4P)              :: ext(6)      !< Extent written.
  real(R8P), allocatable    :: pd(:), xyz(:,:)
  integer(I4P), allocatable :: cd(:)
  integer(I4P)              :: i, j, k

  ext = [0, 6, 0, 3, 0, 1]
  if (is_parallel) then
    ext = region(rank)
    error = a_vtk_file%initialize(format=format, filename=filename, mesh_topology=trim(topology), &
                                  nx1=0, nx2=6, ny1=0, ny2=3, nz1=0, nz2=1,                       &
                                  origin=[0._R8P, 0._R8P, 0._R8P], spacing=[1._R8P, 1._R8P, 1._R8P], comm=comm)
  else
    error = a_vtk_file%initialize(format=format, filename=filename, mesh_topology=trim(topology), &
                                  nx1=0, nx2=6, ny1=0, ny2=3, nz1=0, nz2=1,                       &
                                  origin=[0._R8P, 0._R8P, 0._R8P], spacing=[1._R8P, 1._R8P, 1._R8P])
  endif
  pd = [((((100._R8P * k + 10 * j + i), i=ext(1), ext(2)), j=ext(3), ext(4)), k=ext(5), ext(6))]
  cd = [((((1000 + 100 * k + 10 * j + i), i=ext(1), ext(2)-1), j=ext(3), ext(4)-1), k=ext(5), ext(6)-1)]
  allocate(xyz(3, size(pd)))
  if (size(pd) > 0) xyz = reshape([((([real(i, R8P) + 0.1_R8P * j, real(j, R8P) + 0.05_R8P * i, 0.5_R8P * k], &
                                      i=ext(1), ext(2)), j=ext(3), ext(4)), k=ext(5), ext(6))], [3, size(pd)])
  error = max(error, abs(a_vtk_file%xml_writer%write_piece(nx1=ext(1), nx2=ext(2), ny1=ext(3), ny2=ext(4), &
                                                            nz1=ext(5), nz2=ext(6))))
  if (trim(topology) == 'RectilinearGrid') then
    error = max(error, abs(a_vtk_file%xml_writer%write_geo(x=[(real(i, R8P)**2 / 4, i=ext(1), ext(2))], &
                                                           y=[(1.5_R8P * j, j=ext(3), ext(4))],         &
                                                           z=[(0.5_R8P * k, k=ext(5), ext(6))])))
  elseif (trim(topology) == 'StructuredGrid' .and. is_parallel) then ! the layout xyz(3, nx1:nx2, ny1:ny2, nz1:nz2)
    error = max(error, abs(a_vtk_file%xml_writer%write_geo(xyz=reshape(xyz, [3, max(ext(2) - ext(1) + 1, 0),    &
                                                                              max(ext(4) - ext(3) + 1, 0),    &
                                                                              max(ext(6) - ext(5) + 1, 0)]))))
  elseif (trim(topology) == 'StructuredGrid') then
    error = max(error, abs(a_vtk_file%xml_writer%write_geo(n=size(pd), x=xyz(1,:), y=xyz(2,:), z=xyz(3,:))))
  endif
  error = max(error, abs(a_vtk_file%xml_writer%write_dataarray(location='node', action='open')))
  error = max(error, abs(a_vtk_file%xml_writer%write_dataarray(data_name='pd', x=pd)))
  error = max(error, abs(a_vtk_file%xml_writer%write_dataarray(location='node', action='close')))
  error = max(error, abs(a_vtk_file%xml_writer%write_dataarray(location='cell', action='open')))
  error = max(error, abs(a_vtk_file%xml_writer%write_dataarray(data_name='cd', x=cd)))
  error = max(error, abs(a_vtk_file%xml_writer%write_dataarray(location='cell', action='close')))
  error = max(error, abs(a_vtk_file%xml_writer%write_piece()))
  error = max(error, abs(a_vtk_file%finalize()))
  endfunction grid_file

  logical function region_refused_step()
  !< A time series of a grid from the ranks: refused (on every rank).
  integer(I4P) :: error !< Error status.

  error = a_vtk_file%initialize(format='vtkhdf', filename='refused_grid_step.vtkhdf', mesh_topology='ImageData', &
                                nx1=0, nx2=6, ny1=0, ny2=3, nz1=0, nz2=1,                                       &
                                origin=[0._R8P, 0._R8P, 0._R8P], spacing=[1._R8P, 1._R8P, 1._R8P], comm=comm)
  region_refused_step = a_vtk_file%xml_writer%write_step(time=0._R8P) /= 0
  error = a_vtk_file%finalize()
  endfunction region_refused_step

  logical function region_refused(extent, filename, second)
  !< A grid region of this rank refused (on every rank): `extent`, or with `second` a valid region then a second one.
  integer(I4P), intent(in) :: extent(6) !< Region of this rank.
  character(*), intent(in) :: filename  !< File name.
  logical,      intent(in) :: second    !< Write a valid region first, then `extent` again.
  integer(I4P)             :: error     !< Error status.

  error = a_vtk_file%initialize(format='vtkhdf', filename=filename, mesh_topology='ImageData', &
                                nx1=0, nx2=6, ny1=0, ny2=3, nz1=0, nz2=1,                      &
                                origin=[0._R8P, 0._R8P, 0._R8P], spacing=[1._R8P, 1._R8P, 1._R8P], comm=comm)
  if (second) then
    error = a_vtk_file%xml_writer%write_piece(nx1=extent(1), nx2=extent(2), ny1=extent(3), ny2=extent(4), &
                                              nz1=extent(5), nz2=extent(6))
    error = a_vtk_file%xml_writer%write_piece()
  endif
  region_refused = a_vtk_file%xml_writer%write_piece(nx1=extent(1), nx2=extent(2), ny1=extent(3), ny2=extent(4), &
                                                     nz1=extent(5), nz2=extent(6)) /= 0
  error = a_vtk_file%finalize()
  endfunction region_refused

  logical function refusals()
  !< The refusals of comm, by every rank (the files created are finalized by all of them).
  integer(I4P) :: error
  logical      :: ok(15)
  integer(I4P) :: ext(6) !< Region of this rank.

  ok = .false.
  ! the regions of a grid: outside the whole extent, overlapping (on more than one rank), not covering it, two per rank
  ok(1) = region_refused(merge([0, 7, 0, 3, 0, 1], [0, -1, 0, -1, 0, -1], rank == 0), 'refused_outside.vtkhdf', .false.)
  ok(8) = n_ranks == 1
  if (n_ranks > 1) ok(8) = region_refused(merge([0, 6, 0, 3, 0, 1], [0, -1, 0, -1, 0, -1], rank <= 1), &
                                          'refused_overlap.vtkhdf', .false.)
  ok(9) = region_refused(merge([0, 3, 0, 3, 0, 1], [0, -1, 0, -1, 0, -1], rank == 0), 'refused_cover.vtkhdf', .false.)
  ok(10) = region_refused(region(rank), 'refused_second.vtkhdf', .true.)
  ! a time series: different times on the ranks (more than one), a grid
  error = a_vtk_file%initialize(format='vtkhdf', filename='refused_times.vtkhdf', mesh_topology='UnstructuredGrid', &
                                comm=comm)
  ok(5) = n_ranks == 1
  if (n_ranks > 1) ok(5) = a_vtk_file%xml_writer%write_step(time=real(rank, R8P)) /= 0
  error = a_vtk_file%finalize()
  ok(12) = region_refused_step()
  ! an array of the wrong size on rank 0 only: refused on every rank (none left waiting in a collective call)
  error = a_vtk_file%initialize(format='vtkhdf', filename='refused_size.vtkhdf', mesh_topology='ImageData', &
                                nx1=0, nx2=6, ny1=0, ny2=3, nz1=0, nz2=1,                                  &
                                origin=[0._R8P, 0._R8P, 0._R8P], spacing=[1._R8P, 1._R8P, 1._R8P], comm=comm)
  ext = region(rank)
  error = a_vtk_file%xml_writer%write_piece(nx1=ext(1), nx2=ext(2), ny1=ext(3), ny2=ext(4), nz1=ext(5), nz2=ext(6))
  error = a_vtk_file%xml_writer%write_dataarray(location='node', action='open')
  ok(11) = a_vtk_file%xml_writer%write_dataarray(data_name='pd', &
             x=[(1._R8P, g=1, merge(1, region_points(ext), rank == 0))]) /= 0
  error = a_vtk_file%xml_writer%write_dataarray(location='node', action='close')
  error = a_vtk_file%xml_writer%write_piece()
  error = a_vtk_file%finalize()
  ok(2) = a_vtk_file%initialize(format='vtkhdf', filename='refused_zlib.vtkhdf', mesh_topology='ImageData', &
                                nx1=0, nx2=1, ny1=0, ny2=1, nz1=0, nz2=0,                                  &
                                origin=[0._R8P, 0._R8P, 0._R8P], spacing=[1._R8P, 1._R8P, 1._R8P],         &
                                compressor='zlib', comm=comm) /= 0                 ! zlib for a grid from ranks
  ok(15) = a_vtk_file%initialize(format='vtkhdf', filename='refused_table.vtkhdf', mesh_topology='Table', &
                                 comm=comm) /= 0                                   ! a Table: one process
  ok(3) = a_vtk_file%initialize(format='raw', filename='refused.vtu', mesh_topology='UnstructuredGrid', comm=comm) /= 0
  ok(4) = a_vtk_file%initialize(filename='vtkfortran_unst_mpi.vtkhdf', action='read', comm=comm) /= 0
  error = a_vtk_file%initialize(format='vtkhdf', filename='refused_pieces.vtkhdf', mesh_topology='UnstructuredGrid', &
                                comm=comm)
  error = unst_piece(a_vtk_file, rank, field=.false.)
  ok(6) = a_vtk_file%xml_writer%write_piece(np=0, nc=0) /= 0                 ! a second piece of the rank
  error = a_vtk_file%finalize()
  error = a_vtk_file%initialize(format='vtkhdf', filename='refused_polyh.vtkhdf', mesh_topology='UnstructuredGrid', &
                                comm=comm)
  error = a_vtk_file%xml_writer%write_piece(np=4, nc=1)
  error = a_vtk_file%xml_writer%write_geo(np=4, nc=1, x=[0._R8P, 1._R8P, 0._R8P, 0._R8P], y=[0._R8P, 0._R8P, 1._R8P, 0._R8P], &
                                         z=[0._R8P, 0._R8P, 0._R8P, 1._R8P])
  ok(7) = a_vtk_file%xml_writer%write_connectivity(nc=1, connectivity=[0, 1, 2, 3], offset=[4], cell_type=[42_I1P], &
                                                   face=[4, 3, 0, 1, 2, 3, 0, 1, 3, 3, 0, 2, 3, 3, 1, 2, 3],         &
                                                   faceoffset=[merge(16, 17, rank == 0)]) /= 0 ! malformed on rank 0
  error = a_vtk_file%finalize()
  ! wrong arguments on rank 0 only, in a call every rank makes: refused on every rank (none left waiting)
  error = a_vtk_file%initialize(format='vtkhdf', filename='refused_local.vtkhdf', mesh_topology='UnstructuredGrid', &
                                comm=comm)
  ok(13) = a_vtk_file%xml_writer%write_piece(np=merge(-1, 4, rank == 0), nc=1) /= 0          ! negative counts on rank 0
  error = a_vtk_file%xml_writer%write_piece(np=4, nc=1)
  error = a_vtk_file%xml_writer%write_geo(np=4, nc=1, x=[0._R8P, 1._R8P, 0._R8P, 0._R8P], y=[0._R8P, 0._R8P, 1._R8P, 0._R8P], &
                                         z=[0._R8P, 0._R8P, 0._R8P, 1._R8P])
  if (rank == 0) then ! a face stream without its faceoffset on rank 0
    ok(14) = a_vtk_file%xml_writer%write_connectivity(nc=1, connectivity=[0, 1, 2, 3], offset=[4], cell_type=[10_I1P], &
                                                      face=[4, 3, 0, 1, 2]) /= 0
  else
    ok(14) = a_vtk_file%xml_writer%write_connectivity(nc=1, connectivity=[0, 1, 2, 3], offset=[4], cell_type=[10_I1P]) /= 0
  endif
  error = a_vtk_file%finalize()
  refusals = all(ok)
  if (.not.refusals) print '(A,I0,A,15L2)', 'rank ', rank, ' refusals: ', ok
  endfunction refusals
endprogram vtk_fortran_write_vtkhdf_ranks
