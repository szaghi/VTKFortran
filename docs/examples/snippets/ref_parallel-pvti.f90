error = a_pvtk_file%initialize(filename='ref.pvti', mesh_topology='PImageData',          &
                               nx1=0, nx2=15, ny1=0, ny2=15, nz1=0, nz2=7,              &
                               origin=[0._R8P, 0._R8P, 0._R8P], spacing=[0.1_R8P, 0.1_R8P, 0.2_R8P])
error = a_pvtk_file%xml_writer%write_dataarray(location='node', action='open')
error = a_pvtk_file%xml_writer%write_parallel_dataarray(data_name='phi', data_type='Float64', number_of_components=1)
error = a_pvtk_file%xml_writer%write_dataarray(location='node', action='close')
error = a_pvtk_file%xml_writer%write_parallel_geo(source='ref_part_1.vti', nx1=0, nx2=8,  ny1=0, ny2=15, nz1=0, nz2=7)
error = a_pvtk_file%xml_writer%write_parallel_geo(source='ref_part_2.vti', nx1=8, nx2=15, ny1=0, ny2=15, nz1=0, nz2=7)
error = a_pvtk_file%finalize()
