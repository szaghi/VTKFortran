!mpi
!hdf5
!run vtkhdf_mpi mpirun -np 4 vtkhdf_mpi
!render vtkhdf_mpi ranks.vtkhdf array=rank categorical=1 edges=1 size=560x340 zoom=1.1
program vtkhdf_mpi
!< Write one VTKHDF file from MPI ranks: every rank its block of hexahedra, a partition of the file, all of them into the
!< same file at the same time (a parallel HDF5). Then rank 0 prints the partitions, read back with the HDF5 bindings.
use mpi_f08
use penf, only : I1P, I4P, I8P, R8P
use vtk_fortran, only : vtk_file
use vtk_fortran_hdf5
implicit none
integer(I4P), parameter   :: np=18, nc=4 ! each block: 3x3x2 points, 2x2x1 hexahedra
type(vtk_file)            :: a_vtk_file
real(R8P)                 :: x(np), y(np), z(np)
integer(I4P)              :: connect(8*nc), offset(nc), c, rank, error
integer(I1P)              :: cell_type(nc)
integer(hid_kind)         :: file_id, root
integer(I8P), allocatable :: values(:)

call MPI_Init()
call MPI_Comm_rank(MPI_COMM_WORLD, rank)

!region write
! every rank makes the same calls (they are collective), each one with its own piece
error = a_vtk_file%initialize(format='vtkhdf', filename='ranks.vtkhdf', mesh_topology='UnstructuredGrid', &
                              comm=MPI_COMM_WORLD%MPI_VAL)
call block_mesh(rank, x, y, z, connect, offset, cell_type) ! the hexahedra of this rank, point ids from 0
error = a_vtk_file%xml_writer%write_piece(np=np, nc=nc)
error = a_vtk_file%xml_writer%write_geo(np=np, nc=nc, x=x, y=y, z=z)
error = a_vtk_file%xml_writer%write_connectivity(nc=nc, connectivity=connect, offset=offset, cell_type=cell_type)
error = a_vtk_file%xml_writer%write_dataarray(location='cell', action='open')
error = a_vtk_file%xml_writer%write_dataarray(data_name='rank', x=[(rank, c=1, nc)])
error = a_vtk_file%xml_writer%write_dataarray(location='cell', action='close')
error = a_vtk_file%xml_writer%write_piece()
error = a_vtk_file%finalize()
!endregion write
if (error /= 0) error stop 'write error'

if (rank == 0) then
  error = h5_file_open('ranks.vtkhdf', file_id)
  if (error == 0) error = h5_group_open(file_id, 'VTKHDF', root)
  if (error /= 0) error stop 'read error'
  print '(A)', 'ranks.vtkhdf, one partition per rank:'
  error = h5_dataset_read(root, 'NumberOfPoints', values)
  print '(A,*(1X,I0))', '  NumberOfPoints', values
  error = h5_dataset_read(root, 'NumberOfCells', values)
  print '(A,*(1X,I0))', '  NumberOfCells ', values
  error = h5_dataset_read(root, 'CellData/rank', values)
  print '(A,*(1X,I0))', '  CellData/rank ', values
  error = h5_group_close(root)
  error = h5_file_close(file_id)
endif
call MPI_Finalize()
contains
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
endprogram vtkhdf_mpi
