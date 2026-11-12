error = a_vtk_file%initialize(format='vtkhdf', filename='plate.vtkhdf', mesh_topology='UnstructuredGrid')
do s=0, n_steps - 1
  error = a_vtk_file%xml_writer%write_step(time=0.25_R8P * s)
  if (s == 0) then ! the first step: the mesh and the temperature
    error = a_vtk_file%xml_writer%write_piece(np=np, nc=nc)
    error = a_vtk_file%xml_writer%write_geo(np=np, nc=nc, x=x, y=y, z=z)
    error = a_vtk_file%xml_writer%write_connectivity(nc=nc, connectivity=connectivity, offset=offset, cell_type=cell_type)
    error = temperature(time=0._R8P)
    error = a_vtk_file%xml_writer%write_piece()
  else             ! the next steps: no piece, the temperature only (the mesh of the step before)
    error = temperature(time=0.25_R8P * s)
  endif
enddo
error = a_vtk_file%finalize()
