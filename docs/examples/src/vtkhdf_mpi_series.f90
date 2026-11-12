!mpi
!hdf5
!run vtkhdf_mpi_series mpirun -np 4 vtkhdf_mpi_series
!render vtkhdf_mpi_series series.vtkhdf array=wave range=-1,1 edges=1 size=560x340 zoom=1.1 fps=3 time=1
program vtkhdf_mpi_series
!< Write a time series into one VTKHDF file from MPI ranks: every rank its block of hexahedra, a partition of each step.
!< The first step writes the mesh, the next ones only the point data, each rank those of its block (the mesh is stored
!< once). Then rank 0 prints the Steps of the file, read back with the HDF5 bindings.
use mpi_f08
use penf, only : I1P, I4P, I8P, R8P
use vtk_fortran, only : vtk_file
use vtk_fortran_hdf5
implicit none
integer(I4P), parameter   :: np=18, nc=4 ! each block: 3x3x2 points, 2x2x1 hexahedra
real(R8P),    parameter   :: pi = acos(-1._R8P)
type(vtk_file)            :: a_vtk_file
real(R8P)                 :: x(np), y(np), z(np)
integer(I4P)              :: connect(8*nc), offset(nc), rank, s, error
integer(I1P)              :: cell_type(nc)
integer(hid_kind)         :: file_id
integer(I8P), allocatable :: values(:)

call MPI_Init()
call MPI_Comm_rank(MPI_COMM_WORLD, rank)
call block_mesh(rank, x, y, z, connect, offset, cell_type) ! the hexahedra of this rank, point ids from 0

!region write
! every rank makes the same calls (they are collective): the same time, its own piece or point data
error = a_vtk_file%initialize(format='vtkhdf', filename='series.vtkhdf', mesh_topology='UnstructuredGrid', &
                              comm=MPI_COMM_WORLD%MPI_VAL)
do s=0, 5
  error = a_vtk_file%xml_writer%write_step(time=0.25_R8P * s)
  if (s == 0) then ! the first step: the mesh of each rank
    error = a_vtk_file%xml_writer%write_piece(np=np, nc=nc)
    error = a_vtk_file%xml_writer%write_geo(np=np, nc=nc, x=x, y=y, z=z)
    error = a_vtk_file%xml_writer%write_connectivity(nc=nc, connectivity=connect, offset=offset, cell_type=cell_type)
    error = point_data(time=0._R8P)
    error = a_vtk_file%xml_writer%write_piece()
  else             ! the next steps: no piece, the point data of each rank's block (the mesh of the step before)
    error = point_data(time=0.25_R8P * s)
  endif
enddo
error = a_vtk_file%finalize()
!endregion write
if (error /= 0) error stop 'write error'

if (rank == 0) then
  error = h5_file_open('series.vtkhdf', file_id)
  if (error /= 0) error stop 'read error'
  print '(A)', 'series.vtkhdf, the steps:'
  error = h5_dataset_read(file_id, 'VTKHDF/Steps/NumberOfParts', values)
  print '(A,*(1X,I3))', '  NumberOfParts         ', values
  error = h5_dataset_read(file_id, 'VTKHDF/Steps/PointOffsets', values)
  print '(A,*(1X,I3))', '  PointOffsets          ', values
  error = h5_dataset_read(file_id, 'VTKHDF/Steps/PointDataOffsets/wave', values)
  print '(A,*(1X,I3))', '  PointDataOffsets/wave ', values
  error = h5_file_close(file_id)
endif
call MPI_Finalize()
contains
  function point_data(time) result(error)
  !< The point data of the block of this rank at a time: a wave travelling along x.
  real(R8P), intent(in) :: time  !< Time.
  integer(I4P)          :: error !< Error status.

  error = a_vtk_file%xml_writer%write_dataarray(location='node', action='open')
  error = a_vtk_file%xml_writer%write_dataarray(data_name='wave', x=sin(2 * pi * (x / 10.5_R8P - time)))
  error = a_vtk_file%xml_writer%write_dataarray(location='node', action='close')
  endfunction point_data

  subroutine block_mesh(rank, x, y, z, connect, offset, cell_type)
  !< The points and hexahedra of the block of a rank: 2x2x1 unit cells, moved by 2.5 along x from the previous rank's.
  integer(I4P), intent(in)  :: rank
  real(R8P),    intent(out) :: x(:), y(:), z(:)
  integer(I4P), intent(out) :: connect(:), offset(:)
  integer(I1P), intent(out) :: cell_type(:)
  integer(I4P)              :: i, j, k, p, c, n0

  p = 0
  do k=0, 1 ; do j=0, 2 ; do i=0, 2
    p = p + 1 ; x(p) = 2.5_R8P * rank + i ; y(p) = j ; z(p) = k
  enddo ; enddo ; enddo
  c = 0
  do j=0, 1 ; do i=0, 1
    n0 = i + 3 * j ! the point of the cell at (i, j, 0), from 0
    connect(8*c+1:8*c+8) = [n0, n0 + 1, n0 + 4, n0 + 3, n0 + 9, n0 + 10, n0 + 13, n0 + 12]
    c = c + 1
  enddo ; enddo
  offset = [(8 * c, c=1, size(offset))]
  cell_type = 12_I1P
  endsubroutine block_mesh
endprogram vtkhdf_mpi_series
