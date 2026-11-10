error = a_vtk_file%xml_writer%write_dataarray(location='node', action='open', scalars='pressure', vectors='velocity')
error = a_vtk_file%xml_writer%write_dataarray(data_name='temperature', x=t)              ! not active
error = a_vtk_file%xml_writer%write_dataarray(data_name='pressure', x=p)                 ! active scalars
error = a_vtk_file%xml_writer%write_dataarray(data_name='velocity', x=u, y=v, z=w)       ! active vectors
error = a_vtk_file%xml_writer%write_dataarray(location='node', action='close')
