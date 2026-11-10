error = a_vtk_file%initialize(format='vtkhdf', filename='annulus.vtkhdf', mesh_topology='StructuredGrid', &
                              nx1=0, nx2=ni, ny1=0, ny2=nj, nz1=0, nz2=nk)
error = a_vtk_file%xml_writer%write_piece(nx1=0, nx2=ni, ny1=0, ny2=nj, nz1=0, nz2=nk)
error = a_vtk_file%xml_writer%write_geo(xyz=xyz)
error = a_vtk_file%xml_writer%write_dataarray(location='node', action='open')
error = a_vtk_file%xml_writer%write_dataarray(data_name='radius', x=radius, one_component=.true.)
error = a_vtk_file%xml_writer%write_dataarray(location='node', action='close')
error = a_vtk_file%xml_writer%write_piece()
error = a_vtk_file%finalize()
