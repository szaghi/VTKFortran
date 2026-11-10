error = a_vtk_file%initialize(format='vtkhdf', filename='blocks.vtkhdf', mesh_topology='UnstructuredGrid')
do b=1, 3
  call block_mesh(b, x, y, z, connect, offset, cell_type) ! the hexahedra of block b, point ids from 0 in the block
  error = a_vtk_file%xml_writer%write_piece(np=np, nc=nc)
  error = a_vtk_file%xml_writer%write_geo(np=np, nc=nc, x=x, y=y, z=z)
  error = a_vtk_file%xml_writer%write_connectivity(nc=nc, connectivity=connect, offset=offset, cell_type=cell_type)
  error = a_vtk_file%xml_writer%write_dataarray(location='cell', action='open')
  error = a_vtk_file%xml_writer%write_dataarray(data_name='block', x=[(b, c=1, nc)])
  error = a_vtk_file%xml_writer%write_dataarray(location='cell', action='close')
  error = a_vtk_file%xml_writer%write_piece()
enddo
error = a_vtk_file%finalize()
