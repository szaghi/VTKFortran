! each mesh is a time series of its own, with the same times
error = a_vtk_file%initialize(format='vtkhdf', filename='sea.vtkhdf', mesh_topology='PolyData')
do s=0, n_steps - 1
  error = sea(time=0.25_R8P * s)
enddo
error = a_vtk_file%finalize()
error = a_vtk_file%initialize(format='vtkhdf', filename='buoy.vtkhdf', mesh_topology='UnstructuredGrid')
do s=0, n_steps - 1
  error = buoy(time=0.25_R8P * s)
enddo
error = a_vtk_file%finalize()
! the multi-block file: the two series copied as its blocks
error = a_vtm_file%initialize(filename='harbour.vtkhdf', format='vtkhdf')
error = a_vtm_file%write_block(filenames='sea.vtkhdf', name='water')
error = a_vtm_file%write_block(filenames='buoy.vtkhdf', name='bodies')
error = a_vtm_file%finalize()
