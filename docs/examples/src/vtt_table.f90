!run vtt_table vtt_table
program vtt_table
!< Tables in XML files (`.vtt`, which ParaView opens): the time history of a damped oscillator written at the end of the
!< run as the columns of one Table; then three probes of a travelling wave, one `.vtt` per output listed in a `.pvd`.
!< The history read back with the reader of the library.
use penf, only : I4P, R8P
use vtk_fortran, only : vtk_file, pvd_file
implicit none
integer(I4P), parameter :: n_samples=41, n_probes=3
real(R8P),    parameter :: pi = acos(-1._R8P), dt=0.25_R8P
type(vtk_file)          :: a_vtk_file, reader
type(pvd_file)          :: pvd
real(R8P)               :: t(n_samples), x(n_samples), v(n_samples), probe_x(n_probes)
real(R8P), allocatable  :: column(:)
character(len=:), allocatable :: names(:)
character(16)           :: filename
integer(I4P)            :: i, s, error

!region history
! the history of x'' + 0.2 x' + x = 0, x(0) = 1, v(0) = 0 (semi-implicit Euler), one sample per time step
t(1) = 0 ; x(1) = 1 ; v(1) = 0
do i=2, n_samples
  t(i) = t(i-1) + dt
  v(i) = v(i-1) - dt * (0.2_R8P * v(i-1) + x(i-1))
  x(i) = x(i-1) + dt * v(i)
enddo
! one Table: a column per quantity, a row per sample (the same calls as for a VTKHDF Table)
error = a_vtk_file%initialize(format='binary', filename='history.vtt', mesh_topology='Table')
error = a_vtk_file%xml_writer%write_dataarray(location='row', action='open')
error = a_vtk_file%xml_writer%write_dataarray(data_name='time', x=t)
error = a_vtk_file%xml_writer%write_dataarray(data_name='displacement', x=x)
error = a_vtk_file%xml_writer%write_dataarray(data_name='velocity', x=v)
error = a_vtk_file%xml_writer%write_dataarray(location='row', action='close')
error = a_vtk_file%finalize()
!endregion history
if (error /= 0) error stop 'write error'

!region probes
! a time series of Tables: a .vtt per output, a row per probe, listed in a .pvd
probe_x = [1._R8P, 4._R8P, 7._R8P]
error = pvd%initialize(filename='probes.pvd')
do s=0, 3
  write(filename, '(A,I0,A)') 'probes_', s, '.vtt'
  error = a_vtk_file%initialize(format='ascii', filename=trim(filename), mesh_topology='Table')
  error = a_vtk_file%xml_writer%write_dataarray(location='row', action='open')
  error = a_vtk_file%xml_writer%write_dataarray(data_name='probe', x=[(i, i=1, n_probes)])
  error = a_vtk_file%xml_writer%write_dataarray(data_name='x', x=probe_x)
  error = a_vtk_file%xml_writer%write_dataarray(data_name='wave', x=sin(2 * pi * (probe_x - 0.5_R8P * s) / 8))
  error = a_vtk_file%xml_writer%write_dataarray(location='row', action='close')
  error = a_vtk_file%finalize()
  error = pvd%write_dataset(filename=trim(filename), timestep=0.5_R8P * s)
enddo
error = pvd%finalize()
!endregion probes
if (error /= 0) error stop 'write error'

!region read
error = reader%initialize(filename='history.vtt', action='read')
error = reader%xml_reader%get_dataarray_names(location='row', names=names)
print '(A,*(1X,A))', 'history.vtt columns:', (trim(names(i)), i=1, size(names))
error = reader%xml_reader%read_dataarray(location='row', data_name='displacement', x=column)
print '(A,I0,A)', 'history.vtt: ', size(column), ' rows; the displacement every 2 time units:'
print '(2X,*(F7.3))', column(1::8)
error = reader%finalize()
!endregion read
if (error /= 0) error stop 'read error'
endprogram vtt_table
