program heat
!< Tutorial, chapter 5: the cube as an unstructured grid of hexahedra, with point and cell data.
use penf, only : I1P, I4P, R8P
use vtk_fortran, only : vtk_file
implicit none
integer(I4P), parameter :: n=24
integer(I4P), parameter :: np=n**3, nc=(n - 1)**3 ! points and cells
real(R8P),    parameter :: h=1._R8P/(n - 1)
real(R8P),    parameter :: dt=0.15_R8P*h**2
real(R8P)               :: x(n), t(n,n,n)
real(R8P)               :: px(np), py(np), pz(np) ! coordinates of each point
real(R8P)               :: tc(nc)                 ! temperature of each cell
integer(I4P)            :: connect(8*nc), offset(nc)
integer(I1P)            :: cell_type(nc)
type(vtk_file)          :: a_vtk_file
integer(I4P)            :: i, j, k, c, cycle, error

x = [(real(i - 1, R8P)/(n - 1), i=1, n)]
do k=1, n ; do j=1, n ; do i=1, n
  t(i,j,k) = blob(x(i), x(j), x(k), [0.35_R8P, 0.4_R8P, 0.5_R8P]) + 0.6_R8P*blob(x(i), x(j), x(k), [0.7_R8P, 0.65_R8P, 0.45_R8P])
enddo ; enddo ; enddo
t(1,:,:) = 0 ; t(n,:,:) = 0 ; t(:,1,:) = 0 ; t(:,n,:) = 0 ; t(:,:,1) = 0 ; t(:,:,n) = 0
do cycle=1, 10
  call step(t)
enddo

! the points, numbered from 0 with i running fastest
do k=1, n ; do j=1, n ; do i=1, n
  px(id(i,j,k) + 1) = x(i) ; py(id(i,j,k) + 1) = x(j) ; pz(id(i,j,k) + 1) = x(k)
enddo ; enddo ; enddo
! the cells: hexahedra (VTK type 12), their 8 points in the VTK order, bottom face then top face
c = 0
do k=1, n - 1 ; do j=1, n - 1 ; do i=1, n - 1
  c = c + 1
  connect(8*c-7:8*c) = [id(i,j,k), id(i+1,j,k), id(i+1,j+1,k), id(i,j+1,k), &
                        id(i,j,k+1), id(i+1,j,k+1), id(i+1,j+1,k+1), id(i,j+1,k+1)]
  offset(c) = 8*c
  tc(c) = sum(t(i:i+1,j:j+1,k:k+1))/8  ! the mean of its points
enddo ; enddo ; enddo
cell_type = 12_I1P

error = a_vtk_file%initialize(format='raw', filename='heat.vtu', mesh_topology='UnstructuredGrid', compressor='zlib')
error = a_vtk_file%xml_writer%write_piece(np=np, nc=nc)
error = a_vtk_file%xml_writer%write_geo(np=np, nc=nc, x=px, y=py, z=pz)
error = a_vtk_file%xml_writer%write_connectivity(nc=nc, connectivity=connect, offset=offset, cell_type=cell_type)
error = a_vtk_file%xml_writer%write_dataarray(location='node', action='open')
error = a_vtk_file%xml_writer%write_dataarray(data_name='temperature', x=reshape(t, [np]))
error = a_vtk_file%xml_writer%write_dataarray(location='node', action='close')
error = a_vtk_file%xml_writer%write_dataarray(location='cell', action='open')
error = a_vtk_file%xml_writer%write_dataarray(data_name='cell_temperature', x=tc)
error = a_vtk_file%xml_writer%write_dataarray(location='cell', action='close')
error = a_vtk_file%xml_writer%write_piece()
error = a_vtk_file%finalize()
print '(A,I0,A,I0,A)', 'heat.vtu: ', np, ' points, ', nc, ' hexahedra'
contains
  elemental function id(i, j, k)
  !< The id of the point (i,j,k), from 0.
  integer(I4P), intent(in) :: i, j, k
  integer(I4P)             :: id

  id = (i - 1) + (j - 1)*n + (k - 1)*n*n
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
