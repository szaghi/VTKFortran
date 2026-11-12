error = a_vtk_file%xml_writer%write_piece(np=4, nc=1)
error = a_vtk_file%xml_writer%write_geo(np=4, nc=1, x=x, y=y, z=z)
error = a_vtk_file%xml_writer%write_connectivity(nc=1, connectivity=[0,1,2,3], offset=[4], cell_type=[10_I1P])
error = a_vtk_file%xml_writer%write_dataarray(location='node', action='open', global_ids='id')
error = a_vtk_file%xml_writer%write_dataarray(data_name='id', x=ids)                       ! the same id in both
error = a_vtk_file%xml_writer%write_dataarray_unsigned(data_name='vtkGhostType', x=ghost)  ! 1: a duplicate
error = a_vtk_file%xml_writer%write_dataarray(location='node', action='close')
error = a_vtk_file%xml_writer%write_dataarray(location='cell', action='open', global_ids='cell_id')
error = a_vtk_file%xml_writer%write_dataarray(data_name='cell_id', x=[cell_id])
error = a_vtk_file%xml_writer%write_dataarray(location='cell', action='close')
error = a_vtk_file%xml_writer%write_piece()
