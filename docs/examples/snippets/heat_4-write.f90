function write_file(filename, time, cycle) result(error)
!< Write the temperature at a time step, with its time and cycle.
character(*), intent(in) :: filename
real(R8P),    intent(in) :: time
integer(I4P), intent(in) :: cycle
integer(I4P)             :: error
type(vtk_file)           :: a_vtk_file

error = a_vtk_file%initialize(format='raw', filename=filename, mesh_topology='RectilinearGrid', &
                              nx1=1, nx2=n, ny1=1, ny2=n, nz1=1, nz2=n, compressor='zlib')
error = a_vtk_file%xml_writer%write_fielddata(action='open')
error = a_vtk_file%xml_writer%write_fielddata(data_name='TIME', x=time)
error = a_vtk_file%xml_writer%write_fielddata(data_name='CYCLE', x=int(cycle, I8P))
error = a_vtk_file%xml_writer%write_fielddata(action='close')
error = a_vtk_file%xml_writer%write_piece(nx1=1, nx2=n, ny1=1, ny2=n, nz1=1, nz2=n)
error = a_vtk_file%xml_writer%write_geo(x=x, y=x, z=x)
error = a_vtk_file%xml_writer%write_dataarray(location='node', action='open', scalars='temperature')
error = a_vtk_file%xml_writer%write_dataarray(data_name='temperature', x=t, one_component=.true.)
error = a_vtk_file%xml_writer%write_dataarray(location='node', action='close')
error = a_vtk_file%xml_writer%write_piece()
error = a_vtk_file%finalize()
endfunction write_file
