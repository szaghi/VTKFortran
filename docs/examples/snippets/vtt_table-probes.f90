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
