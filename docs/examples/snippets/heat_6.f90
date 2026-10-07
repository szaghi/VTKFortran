program heat
!< Tutorial, chapter 6: the hexahedra split in 4 pieces, as 4 processes would write them, with a parallel header.
use penf, only : I1P, I4P, R8P
use vtk_fortran, only : pvtk_file, vtk_file
implicit none
integer(I4P), parameter :: n=24
integer(I4P), parameter :: pieces=4           ! pieces, along x
real(R8P),    parameter :: h=1._R8P/(n - 1)
real(R8P),    parameter :: dt=0.15_R8P*h**2
real(R8P)               :: x(n), t(n,n,n)
character(len=12)       :: sources(pieces)
character(len=:), allocatable :: message
type(pvtk_file)         :: header
integer(I4P)            :: i, j, k, p, cycle, error

x = [(real(i - 1, R8P)/(n - 1), i=1, n)]
do k=1, n ; do j=1, n ; do i=1, n
  t(i,j,k) = blob(x(i), x(j), x(k), [0.35_R8P, 0.4_R8P, 0.5_R8P]) + 0.6_R8P*blob(x(i), x(j), x(k), [0.7_R8P, 0.65_R8P, 0.45_R8P])
enddo ; enddo ; enddo
t(1,:,:) = 0 ; t(n,:,:) = 0 ; t(:,1,:) = 0 ; t(:,n,:) = 0 ; t(:,:,1) = 0 ; t(:,:,n) = 0
do cycle=1, 10
  call step(t)
enddo

! each "process" writes its own piece: the cells it owns, plus one layer of ghost cells of each neighbour
do p=1, pieces
  write(sources(p), '(A,I1,A)') 'heat_', p, '.vtu'
  error = write_piece(p, trim(sources(p)))
enddo

! one process writes the header: the layout of the data and the list of the pieces
error = header%initialize(filename='heat.pvtu', mesh_topology='PUnstructuredGrid', mesh_kind='Float64', ghost_level=1)
error = header%xml_writer%write_dataarray(location='node', action='open')
error = header%xml_writer%write_parallel_dataarray(data_name='temperature', data_type='Float64', number_of_components=1)
error = header%xml_writer%write_dataarray(location='node', action='close')
error = header%xml_writer%write_dataarray(location='cell', action='open')
error = header%xml_writer%write_parallel_dataarray(data_name='piece', data_type='Int32', number_of_components=1)
error = header%xml_writer%write_parallel_dataarray(data_name='vtkGhostType', data_type='UInt8', number_of_components=1)
error = header%xml_writer%write_dataarray(location='cell', action='close')
do p=1, pieces
  error = header%xml_writer%write_parallel_geo(source=trim(sources(p)))
enddo
error = header%finalize()

! read the header back and check that every piece holds what it declares
error = header%initialize(filename='heat.pvtu', action='read')
error = header%xml_reader%check_pieces(message=message)
print '(A,I0,A,A)', 'heat.pvtu: ', pieces, ' pieces, check_pieces: ', merge('OK     ', message, error == 0)
error = header%finalize()
contains
  function write_piece(p, filename) result(error)
  !< Write the piece p: the cells i1...i2 along x it owns and one layer of ghost cells on each side.
  integer(I4P), intent(in)  :: p
  character(*), intent(in)  :: filename
  integer(I4P)              :: error
  type(vtk_file)            :: a_vtk_file
  integer(I4P)              :: i1, i2, g1, g2, ni, np, nc, c, i, j, k, p0
  real(R8P),    allocatable :: px(:), py(:), pz(:), tp(:)
  integer(I4P), allocatable :: connect(:), offset(:), owner(:)
  integer(I1P), allocatable :: cell_type(:), ghost(:)

  i1 = (p - 1)*(n - 1)/pieces + 1 ; i2 = p*(n - 1)/pieces   ! owned cells along x
  g1 = max(1, i1 - 1) ; g2 = min(n - 1, i2 + 1)             ! with the ghost cells
  ni = g2 - g1 + 2 ; np = ni*n*n ; nc = (g2 - g1 + 1)*(n - 1)**2
  allocate(px(np), py(np), pz(np), tp(np), connect(8*nc), offset(nc), owner(nc), cell_type(nc), ghost(nc))
  do k=1, n ; do j=1, n ; do i=g1, g2 + 1
    p0 = id(i,j,k,g1,ni) + 1
    px(p0) = x(i) ; py(p0) = x(j) ; pz(p0) = x(k) ; tp(p0) = t(i,j,k)
  enddo ; enddo ; enddo
  c = 0
  do k=1, n - 1 ; do j=1, n - 1 ; do i=g1, g2
    c = c + 1
    connect(8*c-7:8*c) = id([i, i+1, i+1, i, i, i+1, i+1, i], [j, j, j+1, j+1, j, j, j+1, j+1], &
                            [k, k, k, k, k+1, k+1, k+1, k+1], g1, ni) ! the VTK order of a hexahedron
    offset(c) = 8*c
    ghost(c) = merge(0_I1P, 1_I1P, i >= i1 .and. i <= i2) ! 1: a duplicate (ghost) cell, owned by a neighbour
    owner(c) = merge(p, merge(p - 1, p + 1, i < i1), ghost(c) == 0)
  enddo ; enddo ; enddo
  cell_type = 12_I1P
  error = a_vtk_file%initialize(format='raw', filename=filename, mesh_topology='UnstructuredGrid', compressor='zlib')
  error = a_vtk_file%xml_writer%write_piece(np=np, nc=nc)
  error = a_vtk_file%xml_writer%write_geo(np=np, nc=nc, x=px, y=py, z=pz)
  error = a_vtk_file%xml_writer%write_connectivity(nc=nc, connectivity=connect, offset=offset, cell_type=cell_type)
  error = a_vtk_file%xml_writer%write_dataarray(location='node', action='open')
  error = a_vtk_file%xml_writer%write_dataarray(data_name='temperature', x=tp)
  error = a_vtk_file%xml_writer%write_dataarray(location='node', action='close')
  error = a_vtk_file%xml_writer%write_dataarray(location='cell', action='open')
  error = a_vtk_file%xml_writer%write_dataarray(data_name='piece', x=owner)
  error = a_vtk_file%xml_writer%write_dataarray_unsigned(data_name='vtkGhostType', x=ghost)
  error = a_vtk_file%xml_writer%write_dataarray(location='cell', action='close')
  error = a_vtk_file%xml_writer%write_piece()
  error = a_vtk_file%finalize()
  endfunction write_piece

  elemental function id(i, j, k, g1, ni)
  !< The id of the point (i,j,k) in a piece whose points start at i=g1 and are ni along x, from 0.
  integer(I4P), intent(in) :: i, j, k, g1, ni
  integer(I4P)             :: id

  id = (i - g1) + (j - 1)*ni + (k - 1)*ni*n
  endfunction id

  subroutine step(t)
  !< Advance the temperature by one explicit time step of the heat equation; the walls stay at 0.
  real(R8P), intent(inout) :: t(:,:,:)

  t(2:n-1,2:n-1,2:n-1) = t(2:n-1,2:n-1,2:n-1) + dt/h**2*(t(1:n-2,2:n-1,2:n-1) + t(3:n,2:n-1,2:n-1) + &
                                                          t(2:n-1,1:n-2,2:n-1) + t(2:n-1,3:n,2:n-1) + &
                                                          t(2:n-1,2:n-1,1:n-2) + t(2:n-1,2:n-1,3:n) - 6*t(2:n-1,2:n-1,2:n-1))
  endsubroutine step

  pure function blob(x, y, z, centre) result(t)
  !< A hot blob: a Gaussian bump of temperature 1 at its centre.
  real(R8P), intent(in) :: x, y, z, centre(3)
  real(R8P)             :: t

  t = exp(-((x - centre(1))**2 + (y - centre(2))**2 + (z - centre(3))**2)/0.04_R8P)
  endfunction blob
endprogram heat
