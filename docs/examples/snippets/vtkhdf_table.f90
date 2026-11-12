program vtkhdf_table
!< Tables in VTKHDF files: the time history of a damped oscillator (its time, displacement and velocity at every time
!< step, one row per step) written at the end of the run as the columns of one Table; then three probes of a travelling
!< wave as a time series of Tables, one row per probe at each output. Both read back with the reader of the library.
use penf, only : I4P, R8P
use vtk_fortran, only : vtk_file
implicit none
integer(I4P), parameter :: n_samples=41, n_probes=3
real(R8P),    parameter :: pi = acos(-1._R8P), dt=0.25_R8P
type(vtk_file)          :: a_vtk_file, reader
real(R8P)               :: t(n_samples), x(n_samples), v(n_samples), probe_x(n_probes)
real(R8P), allocatable  :: column(:), times(:)
integer(I4P)            :: i, s, error

! the history of x'' + 0.2 x' + x = 0, x(0) = 1, v(0) = 0 (semi-implicit Euler), one sample per time step
t(1) = 0 ; x(1) = 1 ; v(1) = 0
do i=2, n_samples
  t(i) = t(i-1) + dt
  v(i) = v(i-1) - dt * (0.2_R8P * v(i-1) + x(i-1))
  x(i) = x(i-1) + dt * v(i)
enddo
! one Table: a column per quantity, a row per sample
error = a_vtk_file%initialize(format='vtkhdf', filename='history.vtkhdf', mesh_topology='Table')
error = a_vtk_file%xml_writer%write_dataarray(location='row', action='open')
error = a_vtk_file%xml_writer%write_dataarray(data_name='time', x=t)
error = a_vtk_file%xml_writer%write_dataarray(data_name='displacement', x=x)
error = a_vtk_file%xml_writer%write_dataarray(data_name='velocity', x=v)
error = a_vtk_file%xml_writer%write_dataarray(location='row', action='close')
error = a_vtk_file%finalize()
if (error /= 0) error stop 'write error'

! a time series of Tables: at each output a row per probe, the same columns
probe_x = [1._R8P, 4._R8P, 7._R8P]
error = a_vtk_file%initialize(format='vtkhdf', filename='probes.vtkhdf', mesh_topology='Table')
do s=0, 3
  error = a_vtk_file%xml_writer%write_step(time=0.5_R8P * s)
  error = a_vtk_file%xml_writer%write_dataarray(location='row', action='open')
  error = a_vtk_file%xml_writer%write_dataarray(data_name='probe', x=[(i, i=1, n_probes)])
  error = a_vtk_file%xml_writer%write_dataarray(data_name='x', x=probe_x)
  error = a_vtk_file%xml_writer%write_dataarray(data_name='wave', x=sin(2 * pi * (probe_x - 0.5_R8P * s) / 8))
  error = a_vtk_file%xml_writer%write_dataarray(location='row', action='close')
enddo
error = a_vtk_file%finalize()
if (error /= 0) error stop 'write error'

error = reader%initialize(filename='history.vtkhdf', action='read')
error = reader%xml_reader%read_dataarray(location='row', data_name='displacement', x=column)
print '(A,I0,A)', 'history.vtkhdf: ', size(column), ' rows; the displacement every 2 time units:'
print '(2X,*(F7.3))', column(1::8)
error = reader%finalize()
error = reader%initialize(filename='probes.vtkhdf', action='read')
error = reader%xml_reader%get_steps(times=times)
print '(A,I0,A)', 'probes.vtkhdf: ', size(times), ' steps; the wave at the probes:'
do s=1, size(times)
  error = reader%xml_reader%set_step(s)
  error = reader%xml_reader%read_dataarray(location='row', data_name='wave', x=column)
  print '(A,F4.2,A,*(F7.3))', '  t = ', times(s), ':', column
enddo
error = reader%finalize()
if (error /= 0) error stop 'read error'
endprogram vtkhdf_table
