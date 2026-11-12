!mpi
!hdf5
!run vtkhdf_mpi_grid mpirun -np 4 vtkhdf_mpi_grid
!render vtkhdf_mpi_grid grid.vtkhdf array=rank categorical=1 edges=1 camera=xy size=560x300 zoom=1.1
program vtkhdf_mpi_grid
!< Write one VTKHDF ImageData from MPI ranks: the grid of 8x4 cells split into 2x2 regions, each rank writing its region
!< (its sub-extent, its point and cell data) into the same file at the same time (a parallel HDF5). Then rank 0 prints
!< the cell data of the whole grid, read back with the HDF5 bindings.
use mpi_f08
use penf, only : I4P, I8P, R8P
use vtk_fortran, only : vtk_file
use vtk_fortran_hdf5
implicit none
type(vtk_file)            :: a_vtk_file
integer(I4P)              :: rank, n_ranks, i, j, nx1, nx2, ny1, ny2, error
real(R8P), allocatable    :: wave(:)
integer(hid_kind)         :: file_id
integer(I8P), allocatable :: dims(:), values(:)

call MPI_Init()
call MPI_Comm_rank(MPI_COMM_WORLD, rank)
call MPI_Comm_size(MPI_COMM_WORLD, n_ranks)
if (n_ranks /= 4) error stop 'run on 4 ranks'

!region write
! the region of this rank: 4x2 cells of the whole extent 0:8 x 0:4 (2D)
nx1 = 4 * mod(rank, 2) ; nx2 = nx1 + 4 ; ny1 = 2 * (rank / 2) ; ny2 = ny1 + 2
wave = [((sin(0.4_R8P * i) * cos(0.6_R8P * j), i=nx1, nx2), j=ny1, ny2)] ! its points, global indices
! every rank makes the same calls (they are collective): the whole extent to initialize, its region to write_piece
error = a_vtk_file%initialize(format='vtkhdf', filename='grid.vtkhdf', mesh_topology='ImageData', &
                              nx1=0, nx2=8, ny1=0, ny2=4, nz1=0, nz2=0,                          &
                              origin=[0._R8P, 0._R8P, 0._R8P], spacing=[1._R8P, 1._R8P, 1._R8P], &
                              comm=MPI_COMM_WORLD%MPI_VAL)
error = a_vtk_file%xml_writer%write_piece(nx1=nx1, nx2=nx2, ny1=ny1, ny2=ny2, nz1=0, nz2=0)
error = a_vtk_file%xml_writer%write_dataarray(location='node', action='open')
error = a_vtk_file%xml_writer%write_dataarray(data_name='wave', x=wave)
error = a_vtk_file%xml_writer%write_dataarray(location='node', action='close')
error = a_vtk_file%xml_writer%write_dataarray(location='cell', action='open')
error = a_vtk_file%xml_writer%write_dataarray(data_name='rank', x=[(rank, i=1, 8)])
error = a_vtk_file%xml_writer%write_dataarray(location='cell', action='close')
error = a_vtk_file%xml_writer%write_piece()
error = a_vtk_file%finalize()
!endregion write
if (error /= 0) error stop 'write error'

if (rank == 0) then
  error = h5_file_open('grid.vtkhdf', file_id)
  if (error /= 0) error stop 'read error'
  error = h5_dataset_dims(file_id, 'VTKHDF/PointData/wave', dims)
  print '(A,2(1X,I0))', 'grid.vtkhdf: PointData/wave, Fortran shape', dims
  error = h5_dataset_read(file_id, 'VTKHDF/CellData/rank', values)
  print '(A)', 'CellData/rank, the 8x4 cells (the row y=0 last):'
  do j=4, 1, -1
    print '(2X,8(1X,I0))', values(8 * (j - 1) + 1:8 * j)
  enddo
  error = h5_file_close(file_id)
endif
call MPI_Finalize()
endprogram vtkhdf_mpi_grid
