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
