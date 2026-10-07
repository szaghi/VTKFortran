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
