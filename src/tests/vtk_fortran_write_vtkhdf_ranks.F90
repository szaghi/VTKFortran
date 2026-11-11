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
!<  triangle and a quad on rank 2, points without cells on rank 3, point data.
!<
!< Rank 0 then writes the same pieces alone, in rank order: a serial `.vtkhdf` of each, compared here dataset by dataset
!< (values: the parallel file stores the widest kind of ids the ranks give), and a `.vtu`/`.vtp` of each (VTK readers
!< compare the two outside this test). The refusals of `comm` are checked too. Without MPI `comm` is refused.
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
integer(I4P)            :: e(8)           !< Errors.
logical                 :: test_passed(4) !< List of passed tests.
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
  test_passed(1) = all(e(1:6) == 0)
  if (.not.test_passed(1)) print '(A,I0,A,6I2)', 'rank ', rank, ' errors of the parallel files: ', e(1:6)
  test_passed(2) = refusals()

  ! rank 0: the same pieces alone, compared with the parallel files
  test_passed(3:4) = .true.
  if (rank == 0) then
    e = 0
    e(1) = serial_unst('vtkhdf', 'serial_unst_mpi.vtkhdf')
    e(2) = serial_unst('raw', 'vtkfortran_unst_mpi.vtu')
    e(3) = serial_poly('vtkhdf', 'serial_poly_mpi.vtkhdf')
    e(4) = serial_poly('raw', 'vtkfortran_poly_mpi.vtp')
    test_passed(3) = all(e(1:4) == 0)
    if (.not.test_passed(3)) print '(A,4I2)', 'errors of the serial files: ', e(1:4)
    test_passed(4) = same_datasets('vtkfortran_unst_mpi.vtkhdf', 'serial_unst_mpi.vtkhdf', UNST_DATASETS)
    test_passed(4) = same_datasets('vtkfortran_poly_mpi.vtkhdf', 'serial_poly_mpi.vtkhdf', POLY_DATASETS) .and. test_passed(4)
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

  logical function refusals()
  !< The refusals of comm, by every rank (the files created are finalized by all of them).
  integer(I4P) :: error
  logical      :: ok(7)

  ok = .false.
  ok(1) = a_vtk_file%initialize(format='vtkhdf', filename='refused_image.vtkhdf', mesh_topology='ImageData', &
                                nx1=0, nx2=1, ny1=0, ny2=1, nz1=0, nz2=1,                                  &
                                origin=[0._R8P, 0._R8P, 0._R8P], spacing=[1._R8P, 1._R8P, 1._R8P], comm=comm) /= 0
  ok(2) = a_vtk_file%initialize(format='vtkhdf', filename='refused_zlib.vtkhdf', mesh_topology='UnstructuredGrid', &
                                compressor='zlib', comm=comm) /= 0
  ok(3) = a_vtk_file%initialize(format='raw', filename='refused.vtu', mesh_topology='UnstructuredGrid', comm=comm) /= 0
  ok(4) = a_vtk_file%initialize(filename='vtkfortran_unst_mpi.vtkhdf', action='read', comm=comm) /= 0
  error = a_vtk_file%initialize(format='vtkhdf', filename='refused_step.vtkhdf', mesh_topology='UnstructuredGrid', &
                                comm=comm)
  ok(5) = a_vtk_file%xml_writer%write_step(time=0._R8P) /= 0                 ! a time series
  error = a_vtk_file%finalize()
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
                                                   faceoffset=[17]) /= 0     ! a polyhedron
  error = a_vtk_file%finalize()
  refusals = all(ok)
  if (.not.refusals) print '(A,I0,A,7L2)', 'rank ', rank, ' refusals: ', ok
  endfunction refusals
endprogram vtk_fortran_write_vtkhdf_ranks
