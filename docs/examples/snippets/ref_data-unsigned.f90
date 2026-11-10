error = a_vtk_file%xml_writer%write_dataarray(location='cell', action='open')
error = a_vtk_file%xml_writer%write_dataarray_unsigned(data_name='vtkGhostType', x=ghost)
error = a_vtk_file%xml_writer%write_dataarray(location='cell', action='close')
