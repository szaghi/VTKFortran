error = a_vtk_file%initialize(format='vtkhdf', filename='wave.vtkhdf', mesh_topology='ImageData',     &
                              nx1=0, nx2=nx, ny1=0, ny2=ny, nz1=0, nz2=nz, compressor='zlib',      &
                              origin=[0._R8P, 0._R8P, 0._R8P], spacing=[0.1_R8P, 0.1_R8P, 0.1_R8P])
error = a_vtk_file%xml_writer%write_fielddata(action='open')
error = a_vtk_file%xml_writer%write_fielddata(data_name='time', x=0.5_R8P)
error = a_vtk_file%xml_writer%write_fielddata(action='close')
error = a_vtk_file%xml_writer%write_piece(nx1=0, nx2=nx, ny1=0, ny2=ny, nz1=0, nz2=nz)
error = a_vtk_file%xml_writer%write_dataarray(location='node', action='open', scalars='wave', vectors='velocity')
error = a_vtk_file%xml_writer%write_dataarray(data_name='wave', x=wave, one_component=.true.)
error = a_vtk_file%xml_writer%write_dataarray(data_name='velocity', x=velocity)
error = a_vtk_file%xml_writer%write_dataarray(location='node', action='close')
error = a_vtk_file%xml_writer%write_piece()
error = a_vtk_file%finalize()
