! every rank makes the same calls (they are collective), each one with its own piece
error = a_vtk_file%initialize(format='vtkhdf', filename='ranks.vtkhdf', mesh_topology='UnstructuredGrid', &
                              comm=MPI_COMM_WORLD%MPI_VAL)
call block_mesh(rank, x, y, z, connect, offset, cell_type) ! the hexahedra of this rank, point ids from 0
error = a_vtk_file%xml_writer%write_piece(np=np, nc=nc)
error = a_vtk_file%xml_writer%write_geo(np=np, nc=nc, x=x, y=y, z=z)
error = a_vtk_file%xml_writer%write_connectivity(nc=nc, connectivity=connect, offset=offset, cell_type=cell_type)
error = a_vtk_file%xml_writer%write_dataarray(location='cell', action='open')
error = a_vtk_file%xml_writer%write_dataarray(data_name='rank', x=[(rank, c=1, nc)])
error = a_vtk_file%xml_writer%write_dataarray(location='cell', action='close')
error = a_vtk_file%xml_writer%write_piece()
error = a_vtk_file%finalize()
