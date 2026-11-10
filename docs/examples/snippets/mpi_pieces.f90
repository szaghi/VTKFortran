!mpi
program mpi_pieces
!< Write a partitioned mesh from MPI ranks: each rank writes its own piece, rank 0 the parallel header, then checks it.
use mpi_f08
use penf, only : I1P, I4P, R8P
use vtk_fortran, only : pvtk_file, vtk_file
implicit none
integer, parameter            :: nc=6               ! cells of each piece: a strip of tetrahedra
integer                       :: rank, nranks, r
character(len=32)             :: filename
character(len=:), allocatable :: message
type(pvtk_file)               :: header
integer(I4P)                  :: error

call MPI_Init()
call MPI_Comm_rank(MPI_COMM_WORLD, rank)
call MPI_Comm_size(MPI_COMM_WORLD, nranks)

! every rank: its own piece, in a file named after the rank, with point ids local to the piece
write(filename, '(A,I0,A)') 'mpi_piece_', rank, '.vtu'
error = write_piece(trim(filename), rank)

call MPI_Barrier(MPI_COMM_WORLD)    ! the header is checked against the pieces: wait until all are written
if (rank == 0) then
  error = header%initialize(filename='mpi.pvtu', mesh_topology='PUnstructuredGrid', mesh_kind='Float64')
  error = header%xml_writer%write_dataarray(location='cell', action='open')
  error = header%xml_writer%write_parallel_dataarray(data_name='rank', data_type='Int32', number_of_components=1)
  error = header%xml_writer%write_dataarray(location='cell', action='close')
  do r=0, nranks - 1
    write(filename, '(A,I0,A)') 'mpi_piece_', r, '.vtu'
    error = header%xml_writer%write_parallel_geo(source=trim(filename))
  enddo
  error = header%finalize()
  ! read the header back and check every piece against it
  error = header%initialize(filename='mpi.pvtu', action='read')
  error = header%xml_reader%check_pieces(message=message)
  print '(A,I0,A,I0)', 'mpi.pvtu: ', nranks, ' pieces, check_pieces error ', error
  error = header%finalize()
endif
call MPI_Finalize()
contains
  function write_piece(filename, rank) result(error)
  !< Write the piece of a rank: a strip of nc tetrahedra, shifted along x by the rank, with the cell data "rank".
  character(*), intent(in) :: filename
  integer,      intent(in) :: rank
  integer(I4P)             :: error
  type(vtk_file)           :: a_vtk_file
  real(R8P)                :: x(nc+3), y(nc+3), z(nc+3)
  integer(I4P)             :: connect(4*nc), c, p

  do p=1, nc + 3
    x(p) = rank*nc + p - 1 ; y(p) = mod(p, 2) ; z(p) = mod(p, 3)
  enddo
  connect = [((c - 1 + p, p=0, 3), c=1, nc)]
  error = a_vtk_file%initialize(format='raw', filename=filename, mesh_topology='UnstructuredGrid')
  error = a_vtk_file%xml_writer%write_piece(np=nc+3, nc=nc)
  error = a_vtk_file%xml_writer%write_geo(np=nc+3, nc=nc, x=x, y=y, z=z)
  error = a_vtk_file%xml_writer%write_connectivity(nc=nc, connectivity=connect, offset=[(4*c, c=1, nc)], &
                                                   cell_type=[(10_I1P, c=1, nc)])
  error = a_vtk_file%xml_writer%write_dataarray(location='cell', action='open')
  error = a_vtk_file%xml_writer%write_dataarray(data_name='rank', x=[(int(rank, I4P), c=1, nc)])
  error = a_vtk_file%xml_writer%write_dataarray(location='cell', action='close')
  error = a_vtk_file%xml_writer%write_piece()
  error = a_vtk_file%finalize()
  endfunction write_piece
endprogram mpi_pieces
