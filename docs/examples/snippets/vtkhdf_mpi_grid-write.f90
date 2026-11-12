! the region of this rank: 4x2 cells of the whole extent 0:8 x 0:4 (2D)
nx1 = 4 * mod(rank, 2) ; nx2 = nx1 + 4 ; ny1 = 2 * (rank / 2) ; ny2 = ny1 + 2
wave = [((sin(0.4_R8P * i) * cos(0.6_R8P * j), i=nx1, nx2), j=ny1, ny2)] ! its points, global indices
! every rank makes the same calls (they are collective): the whole extent to initialize, its region to write_piece
error = a_vtk_file%initialize(format='vtkhdf', filename='grid.vtkhdf', mesh_topology='ImageData', &
                              nx1=0, nx2=8, ny1=0, ny2=4, nz1=0, nz2=0,                          &
                              origin=[0._R8P, 0._R8P, 0._R8P], spacing=[1._R8P, 1._R8P, 1._R8P], &
                              comm=MPI_COMM_WORLD%MPI_VAL)
error = a_vtk_file%xml_writer%write_piece(nx1=nx1, nx2=nx2, ny1=ny1, ny2=ny2, nz1=0, nz2=0)
error = a_vtk_file%xml_writer%write_dataarray(location='node', action='open')
error = a_vtk_file%xml_writer%write_dataarray(data_name='wave', x=wave)
error = a_vtk_file%xml_writer%write_dataarray(location='node', action='close')
error = a_vtk_file%xml_writer%write_dataarray(location='cell', action='open')
error = a_vtk_file%xml_writer%write_dataarray(data_name='rank', x=[(rank, i=1, 8)])
error = a_vtk_file%xml_writer%write_dataarray(location='cell', action='close')
error = a_vtk_file%xml_writer%write_piece()
error = a_vtk_file%finalize()
