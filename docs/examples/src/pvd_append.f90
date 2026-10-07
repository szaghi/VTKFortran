!run pvd_append pvd_append
program pvd_append
!< Continue a time series after a restart, then read the collection back.
use penf, only : I4P, R8P
use vtk_fortran, only : pvd_file
implicit none
type(pvd_file)                :: series
real(R8P),        allocatable :: timestep(:)
character(len=:), allocatable :: file(:)
integer(I4P)                  :: d, error

! the first job
error = series%initialize(filename='run.pvd')
error = series%write_dataset(filename='run_0.vtu', timestep=0._R8P)
error = series%write_dataset(filename='run_1.vtu', timestep=0.5_R8P)
error = series%finalize()
! the restarted job: the datasets already listed are kept
error = series%initialize(filename='run.pvd', action='append')
error = series%write_dataset(filename='run_2.vtu', timestep=1._R8P)
error = series%finalize()

error = series%initialize(filename='run.pvd', action='read')
error = series%get_datasets(timestep=timestep, file=file)
error = series%finalize()
do d=1, size(file)
  print '(A,F4.2,2A)', 'time ', timestep(d), ': ', file(d)
enddo
endprogram pvd_append
