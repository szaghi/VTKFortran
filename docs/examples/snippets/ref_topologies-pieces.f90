error = a_vtk_file%initialize(format='raw', filename='ref_blocks.vtu', mesh_topology='UnstructuredGrid')
do b=1, nblocks
  x = [0, 1, 0, 0] + 2._R8P*b ; p = b   ! the points and the data of block b
  error = a_vtk_file%xml_writer%write_piece(np=np(b), nc=nc(b))
  error = a_vtk_file%xml_writer%write_geo(np=np(b), nc=nc(b), x=x, y=y, z=z)
  error = a_vtk_file%xml_writer%write_connectivity(nc=nc(b), connectivity=[0,1,2,3], offset=[4], cell_type=[10_I1P])
  error = a_vtk_file%xml_writer%write_dataarray(location='node', action='open')
  error = a_vtk_file%xml_writer%write_dataarray(data_name='pressure', x=p)
  error = a_vtk_file%xml_writer%write_dataarray(location='node', action='close')
  error = a_vtk_file%xml_writer%write_piece()
enddo
error = a_vtk_file%finalize()
