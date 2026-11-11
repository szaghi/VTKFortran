!hdf5
!run vtkhdf_series vtkhdf_series
!render vtkhdf_series wave.vtkhdf array=wave range=-1,1 edges=1 camera=xy size=560x300 zoom=1.1 fps=4 time=1
program vtkhdf_series
!< A time series in one VTKHDF file: a wave on a 2D grid, written in two runs, the second one continuing the file
!< (`action='append'`, as after a restart). The steps are then read back with the HDF5 bindings of the library.
use penf, only : I4P, I8P, R8P
use vtk_fortran, only : vtk_file
use vtk_fortran_hdf5
implicit none
integer(I4P), parameter   :: nx=24, ny=12
real(R8P),    parameter   :: pi = acos(-1._R8P)
type(vtk_file)            :: a_vtk_file
integer(I4P)              :: s, error
integer(hid_kind)         :: file_id, steps
integer(I8P), allocatable :: nsteps(:)
real(R8P),    allocatable :: times(:)

!region write
! first run: the file is created, steps 0 to 3
error = a_vtk_file%initialize(format='vtkhdf', filename='wave.vtkhdf', mesh_topology='ImageData', &
                              nx1=0, nx2=nx, ny1=0, ny2=ny, nz1=0, nz2=0,                       &
                              origin=[0._R8P, 0._R8P, 0._R8P], spacing=[1._R8P, 1._R8P, 1._R8P])
do s=0, 3
  error = output(time=0.25_R8P * s)
enddo
error = a_vtk_file%finalize()
! second run (a restart): the file is reopened, steps 4 to 7 follow
error = a_vtk_file%initialize(format='vtkhdf', filename='wave.vtkhdf', action='append')
do s=4, 7
  error = output(time=0.25_R8P * s)
enddo
error = a_vtk_file%finalize()
!endregion write
if (error /= 0) error stop 'write error'

error = h5_file_open('wave.vtkhdf', file_id)
if (error == 0) error = h5_group_open(file_id, 'VTKHDF/Steps', steps)
if (error == 0) error = h5_attribute_read(steps, 'NSteps', nsteps)
if (error == 0) error = h5_dataset_read(steps, 'Values', times)
if (error /= 0) error stop 'read error'
print '(A,I0,A)', 'wave.vtkhdf: ', nsteps(1), ' steps, at the times'
print '(*(F5.2))', times
error = h5_group_close(steps)
error = h5_file_close(file_id)
contains
  !region output
  function output(time) result(error)
  !< One output: the step, its field data, its piece with the wave at that time.
  real(R8P), intent(in) :: time  !< Time.
  integer(I4P)          :: error !< Error status.
  real(R8P)             :: wave(0:nx,0:ny)
  integer(I4P)          :: i, j

  do j=0, ny ; do i=0, nx
    wave(i,j) = sin(2._R8P * pi * (i / real(nx, R8P) - 0.5_R8P * time)) * cos(pi * (j / real(ny, R8P) - 0.5_R8P))
  enddo ; enddo
  error = a_vtk_file%xml_writer%write_step(time=time)
  error = a_vtk_file%xml_writer%write_fielddata(action='open')
  error = a_vtk_file%xml_writer%write_fielddata(data_name='TIME', x=time)
  error = a_vtk_file%xml_writer%write_fielddata(action='close')
  error = a_vtk_file%xml_writer%write_piece(nx1=0, nx2=nx, ny1=0, ny2=ny, nz1=0, nz2=0)
  error = a_vtk_file%xml_writer%write_dataarray(location='node', action='open')
  error = a_vtk_file%xml_writer%write_dataarray(data_name='wave', x=wave, one_component=.true.)
  error = a_vtk_file%xml_writer%write_dataarray(location='node', action='close')
  error = a_vtk_file%xml_writer%write_piece()
  endfunction output
  !endregion output
endprogram vtkhdf_series
