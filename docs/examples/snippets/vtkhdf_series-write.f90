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
