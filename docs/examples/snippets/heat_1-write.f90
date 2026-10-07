error = a_vtk_file%initialize(format='ascii', filename='heat.vtr', mesh_topology='RectilinearGrid', &
                              nx1=1, nx2=n, ny1=1, ny2=n, nz1=1, nz2=n)
error = a_vtk_file%xml_writer%write_piece(nx1=1, nx2=n, ny1=1, ny2=n, nz1=1, nz2=n)
error = a_vtk_file%xml_writer%write_geo(x=x, y=x, z=x)
error = a_vtk_file%xml_writer%write_dataarray(location='node', action='open')
error = a_vtk_file%xml_writer%write_dataarray(data_name='temperature', x=t, one_component=.true.)
error = a_vtk_file%xml_writer%write_dataarray(location='node', action='close')
error = a_vtk_file%xml_writer%write_piece()
error = a_vtk_file%finalize()
