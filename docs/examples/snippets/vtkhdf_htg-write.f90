error = a_vtk_file%initialize(format='vtkhdf', filename='quadtree.vtkhdf', mesh_topology='HyperTreeGrid', &
                              nx1=0, nx2=nx, ny1=0, ny2=ny, nz1=0, nz2=0, branch_factor=2)
error = a_vtk_file%xml_writer%write_piece(nx1=0, nx2=nx, ny1=0, ny2=ny, nz1=0, nz2=0)
error = a_vtk_file%xml_writer%write_geo(x=[(real(i, R8P), i=0, nx)], y=[(real(j, R8P), j=0, ny)], z=[0._R8P])
allocate(depths(0))
do j=0, ny - 1 ; do i=0, nx - 1 ! the tree of root cell (i, j): its id i + nx j
  error = a_vtk_file%xml_writer%write_tree(tree_id=i + nx * j, descriptor=tree(real(i, R8P), real(j, R8P)))
enddo ; enddo
error = a_vtk_file%xml_writer%write_dataarray(location='cell', action='open')
error = a_vtk_file%xml_writer%write_dataarray(data_name='depth', x=depths)
error = a_vtk_file%xml_writer%write_dataarray(location='cell', action='close')
error = a_vtk_file%xml_writer%write_piece()
error = a_vtk_file%finalize()
