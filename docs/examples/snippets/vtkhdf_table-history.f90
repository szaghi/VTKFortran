! the history of x'' + 0.2 x' + x = 0, x(0) = 1, v(0) = 0 (semi-implicit Euler), one sample per time step
t(1) = 0 ; x(1) = 1 ; v(1) = 0
do i=2, n_samples
  t(i) = t(i-1) + dt
  v(i) = v(i-1) - dt * (0.2_R8P * v(i-1) + x(i-1))
  x(i) = x(i-1) + dt * v(i)
enddo
! one Table: a column per quantity, a row per sample
error = a_vtk_file%initialize(format='vtkhdf', filename='history.vtkhdf', mesh_topology='Table')
error = a_vtk_file%xml_writer%write_dataarray(location='row', action='open')
error = a_vtk_file%xml_writer%write_dataarray(data_name='time', x=t)
error = a_vtk_file%xml_writer%write_dataarray(data_name='displacement', x=x)
error = a_vtk_file%xml_writer%write_dataarray(data_name='velocity', x=v)
error = a_vtk_file%xml_writer%write_dataarray(location='row', action='close')
error = a_vtk_file%finalize()
