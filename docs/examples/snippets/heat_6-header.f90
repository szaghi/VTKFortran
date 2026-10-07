! one process writes the header: the layout of the data and the list of the pieces
error = header%initialize(filename='heat.pvtu', mesh_topology='PUnstructuredGrid', mesh_kind='Float64', ghost_level=1)
error = header%xml_writer%write_dataarray(location='node', action='open')
error = header%xml_writer%write_parallel_dataarray(data_name='temperature', data_type='Float64', number_of_components=1)
error = header%xml_writer%write_dataarray(location='node', action='close')
error = header%xml_writer%write_dataarray(location='cell', action='open')
error = header%xml_writer%write_parallel_dataarray(data_name='piece', data_type='Int32', number_of_components=1)
error = header%xml_writer%write_parallel_dataarray(data_name='vtkGhostType', data_type='UInt8', number_of_components=1)
error = header%xml_writer%write_dataarray(location='cell', action='close')
do p=1, pieces
  error = header%xml_writer%write_parallel_geo(source=trim(sources(p)))
enddo
error = header%finalize()
