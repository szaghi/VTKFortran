!as heat
!run heat_8 heat
!run heat_8-pvd tail -n 5 heat.pvd
!render heat_8 heat_0072.vtr array=temperature carpet=Z,0.6 edges=1 outline=1 zoom=1.2 range=0,0.99
program heat
!< Tutorial, chapter 8: restart the run of chapter 4 from its last file, and continue its time series.
use penf, only : I4P, I8P, R8P, strz
use vtk_fortran, only : pvd_file, vtk_file
implicit none
integer(I4P), parameter :: steps=36          ! more time steps
integer(I4P), parameter :: every=2
real(R8P),        allocatable :: x(:), y(:), z(:), t(:,:,:), values(:), time_read(:), timestep(:)
integer(I8P),     allocatable :: cycle_read(:)
character(len=:), allocatable :: file(:)
type(pvd_file)          :: series
type(vtk_file)          :: last
real(R8P)               :: time, h, dt
integer(I4P)            :: n, cycle, start, error

!region restart
! the last dataset of the time series
error = series%initialize(filename='heat.pvd', action='read')
error = series%get_datasets(timestep=timestep, file=file)
error = series%finalize()
! its grid, temperature, time and cycle
error = last%initialize(filename=trim(file(size(file))), action='read')
error = last%xml_reader%read_geo(x=x, y=y, z=z)
error = last%xml_reader%read_dataarray(location='node', data_name='temperature', x=values)
error = last%xml_reader%read_dataarray(location='field', data_name='TIME', x=time_read)
error = last%xml_reader%read_dataarray(location='field', data_name='CYCLE', x=cycle_read)
error = last%finalize()
n = size(x)
t = reshape(values, [n, n, n])
time = time_read(1)
start = int(cycle_read(1), I4P)
print '(A,A,A,I0,A,ES10.3)', 'restart from ', trim(file(size(file))), ': cycle ', start, ', time ', time
!endregion restart
h = x(2) - x(1)
dt = 0.15_R8P*h**2

!region continue
! continue the run, appending to the same collection
error = series%initialize(filename='heat.pvd', action='append')
do cycle=start + 1, start + steps
  call step(t)
  time = time + dt
  if (mod(cycle, every) == 0) then
    error = write_file(filename='heat_'//trim(strz(cycle, 4))//'.vtr', time=time, cycle=cycle)
    error = series%write_dataset(filename='heat_'//trim(strz(cycle, 4))//'.vtr', timestep=time)
  endif
enddo
error = series%finalize()
!endregion continue
print '(A,I0,A,ES10.3,A,F6.3)', 'heat.pvd: ', size(file) + steps/every, ' files, until time ', time, &
      ', maximum temperature ', maxval(t)
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
  error = a_vtk_file%xml_writer%write_geo(x=x, y=y, z=z)
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
endprogram heat
