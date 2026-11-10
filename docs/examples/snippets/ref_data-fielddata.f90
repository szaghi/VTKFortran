error = a_vtk_file%xml_writer%write_fielddata(action='open')
error = a_vtk_file%xml_writer%write_fielddata(data_name='TIME',      x=0.5_R8P)                     ! scalar
error = a_vtk_file%xml_writer%write_fielddata(data_name='CYCLE',     x=7_I8P)                       ! scalar
error = a_vtk_file%xml_writer%write_fielddata(data_name='residuals', x=[1.e-3_R8P, 1.e-4_R8P])      ! rank-1 array
error = a_vtk_file%xml_writer%write_fielddata(data_name='solver',    x='my solver v1.2')            ! string
error = a_vtk_file%xml_writer%write_fielddata(data_name='species',   x=['N2', 'O2'])                ! array of strings
error = a_vtk_file%xml_writer%write_fielddata(action='close')
