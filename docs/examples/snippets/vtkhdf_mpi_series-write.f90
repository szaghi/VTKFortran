! every rank makes the same calls (they are collective): the same time, its own piece or point data
error = a_vtk_file%initialize(format='vtkhdf', filename='series.vtkhdf', mesh_topology='UnstructuredGrid', &
                              comm=MPI_COMM_WORLD%MPI_VAL)
do s=0, 5
  error = a_vtk_file%xml_writer%write_step(time=0.25_R8P * s)
  if (s == 0) then ! the first step: the mesh of each rank
    error = a_vtk_file%xml_writer%write_piece(np=np, nc=nc)
    error = a_vtk_file%xml_writer%write_geo(np=np, nc=nc, x=x, y=y, z=z)
    error = a_vtk_file%xml_writer%write_connectivity(nc=nc, connectivity=connect, offset=offset, cell_type=cell_type)
    error = point_data(time=0._R8P)
    error = a_vtk_file%xml_writer%write_piece()
  else             ! the next steps: no piece, the point data of each rank's block (the mesh of the step before)
    error = point_data(time=0.25_R8P * s)
  endif
enddo
error = a_vtk_file%finalize()
