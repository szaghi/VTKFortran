program quickstart
!< Write a torus, a structured grid with a temperature field and a velocity field, ready for ParaView.
use penf, only : I4P, R8P
use vtk_fortran, only : vtk_file
implicit none
integer(I4P), parameter :: nu=48, nv=24, nw=3           ! points around the torus, around the tube, across it
real(R8P),    parameter :: pi=acos(-1._R8P)
real(R8P)               :: x(nu,nv,nw), y(nu,nv,nw), z(nu,nv,nw), t(nu,nv,nw), u(nu,nv,nw), v(nu,nv,nw), w(nu,nv,nw)
real(R8P)               :: a, b, r
type(vtk_file)          :: torus
integer(I4P)            :: i, j, k, error

do k=1, nw ; do j=1, nv ; do i=1, nu
  a = 2*pi*(i - 1)/(nu - 1) ; b = 2*pi*(j - 1)/(nv - 1) ; r = 0.3_R8P + 0.1_R8P*(k - 1)
  x(i,j,k) = (1 + r*cos(b))*cos(a) ; y(i,j,k) = (1 + r*cos(b))*sin(a) ; z(i,j,k) = r*sin(b)
  t(i,j,k) = 300 + 50*sin(3*a)*cos(b)                    ! temperature
  u(i,j,k) = -sin(a) ; v(i,j,k) = cos(a) ; w(i,j,k) = 0  ! velocity, around the torus
enddo ; enddo ; enddo

error = torus%initialize(format='raw', filename='torus.vts', mesh_topology='StructuredGrid', &
                         nx1=1, nx2=nu, ny1=1, ny2=nv, nz1=1, nz2=nw)
error = torus%xml_writer%write_piece(nx1=1, nx2=nu, ny1=1, ny2=nv, nz1=1, nz2=nw)
error = torus%xml_writer%write_geo(n=nu*nv*nw, x=x, y=y, z=z)
error = torus%xml_writer%write_dataarray(location='node', action='open')
error = torus%xml_writer%write_dataarray(data_name='temperature', x=t, one_component=.true.)
error = torus%xml_writer%write_dataarray(data_name='velocity', x=u, y=v, z=w)
error = torus%xml_writer%write_dataarray(location='node', action='close')
error = torus%xml_writer%write_piece()
error = torus%finalize()
print '(A,I0,A,I0,A)', 'torus.vts written: ', nu*nv*nw, ' points, ', (nu-1)*(nv-1)*(nw-1), ' cells'
endprogram quickstart
