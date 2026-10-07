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
