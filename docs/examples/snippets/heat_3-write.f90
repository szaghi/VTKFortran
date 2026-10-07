error = a_vtk_file%initialize(format='raw', filename='heat.vtr', mesh_topology='RectilinearGrid', &
                              nx1=1, nx2=n, ny1=1, ny2=n, nz1=1, nz2=n, compressor='zlib')
! global data: the time and the cycle of the solution
error = a_vtk_file%xml_writer%write_fielddata(action='open')
error = a_vtk_file%xml_writer%write_fielddata(data_name='TIME', x=time)
error = a_vtk_file%xml_writer%write_fielddata(data_name='CYCLE', x=int(cycle - 1, I8P))
error = a_vtk_file%xml_writer%write_fielddata(action='close')
error = a_vtk_file%xml_writer%write_piece(nx1=1, nx2=n, ny1=1, ny2=n, nz1=1, nz2=n)
error = a_vtk_file%xml_writer%write_geo(x=x, y=x, z=x)
! the arrays ParaView colours by and draws arrows of, by default
error = a_vtk_file%xml_writer%write_dataarray(location='node', action='open', scalars='temperature', vectors='heat_flux')
error = a_vtk_file%xml_writer%write_dataarray(data_name='temperature', x=t, one_component=.true.)
error = a_vtk_file%xml_writer%write_dataarray(data_name='heat_flux', x=qx, y=qy, z=qz)
error = a_vtk_file%xml_writer%write_dataarray(location='node', action='close')
error = a_vtk_file%xml_writer%write_piece()
error = a_vtk_file%finalize()
