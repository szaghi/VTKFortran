use vtk_fortran, only : pvd_file
use penf,        only : I4P, R8P, strz

type(pvd_file)          :: a_pvd_file
integer(I4P), parameter :: nsteps = 3
integer(I4P)            :: step, error
real(R8P)               :: time

error = a_pvd_file%initialize(filename='ref_simulation.pvd')
do step=0, nsteps
  time = step / 10._R8P
  ! ... advance the solution to time, write ref_simulation_NNNN.vtu with a vtk_file ...
  error = a_pvd_file%write_dataset(filename='ref_simulation_'//trim(strz(step, 4))//'.vtu', timestep=time)
enddo
error = a_pvd_file%finalize()
