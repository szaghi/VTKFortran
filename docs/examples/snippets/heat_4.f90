program heat
!< Tutorial, chapter 4: a time series, one file every 2 time steps, collected in heat.pvd.
use penf, only : I4P, I8P, R8P, strz
use vtk_fortran, only : pvd_file, vtk_file
implicit none
integer(I4P), parameter :: n=24
real(R8P),    parameter :: h=1._R8P/(n - 1)
real(R8P),    parameter :: dt=0.15_R8P*h**2
integer(I4P), parameter :: steps=36          ! time steps of the run
integer(I4P), parameter :: every=2           ! time steps between two outputs
real(R8P)               :: x(n), t(n,n,n)
real(R8P)               :: time
type(pvd_file)          :: series
integer(I4P)            :: i, j, k, cycle, error

x = [(real(i - 1, R8P)/(n - 1), i=1, n)]
do k=1, n ; do j=1, n ; do i=1, n
  t(i,j,k) = blob(x(i), x(j), x(k), [0.35_R8P, 0.4_R8P, 0.5_R8P]) + 0.6_R8P*blob(x(i), x(j), x(k), [0.7_R8P, 0.65_R8P, 0.45_R8P])
enddo ; enddo ; enddo
t(1,:,:) = 0 ; t(n,:,:) = 0 ; t(:,1,:) = 0 ; t(:,n,:) = 0 ; t(:,:,1) = 0 ; t(:,:,n) = 0
time = 0

error = series%initialize(filename='heat.pvd')
do cycle=0, steps
  if (cycle > 0) then
    call step(t)
    time = time + dt
  endif
  if (mod(cycle, every) == 0) then
    error = write_file(filename='heat_'//trim(strz(cycle, 4))//'.vtr', time=time, cycle=cycle)
    ! the collection is valid after each call: a run that stops here still opens in ParaView
    error = series%write_dataset(filename='heat_'//trim(strz(cycle, 4))//'.vtr', timestep=time)
  endif
enddo
error = series%finalize()
print '(A,I0,A,ES10.3,A,F6.3)', 'heat.pvd: ', steps/every + 1, ' files, until time ', time, ', maximum temperature ', &
      maxval(t)
contains
  function write_file(filename, time, cycle) result(error)
  !< Write the temperature at a time step, with its time and cycle.
  character(*), intent(in) :: filename
  real(R8P),    intent(in) :: time
  integer(I4P), intent(in) :: cycle
  integer(I4P)             :: error
  type(vtk_file)           :: a_vtk_file

  error = a_vtk_file%initialize(format='raw', filename=filename, mesh_topology='RectilinearGrid', &
                                nx1=1, nx2=n, ny1=1, ny2=n, nz1=1, nz2=n, compressor='zlib')
  error = a_vtk_file%xml_writer%write_fielddata(action='open')
  error = a_vtk_file%xml_writer%write_fielddata(data_name='TIME', x=time)
  error = a_vtk_file%xml_writer%write_fielddata(data_name='CYCLE', x=int(cycle, I8P))
  error = a_vtk_file%xml_writer%write_fielddata(action='close')
  error = a_vtk_file%xml_writer%write_piece(nx1=1, nx2=n, ny1=1, ny2=n, nz1=1, nz2=n)
  error = a_vtk_file%xml_writer%write_geo(x=x, y=x, z=x)
  error = a_vtk_file%xml_writer%write_dataarray(location='node', action='open', scalars='temperature')
  error = a_vtk_file%xml_writer%write_dataarray(data_name='temperature', x=t, one_component=.true.)
  error = a_vtk_file%xml_writer%write_dataarray(location='node', action='close')
  error = a_vtk_file%xml_writer%write_piece()
  error = a_vtk_file%finalize()
  endfunction write_file

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
