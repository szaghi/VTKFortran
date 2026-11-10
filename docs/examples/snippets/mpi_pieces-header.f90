call MPI_Barrier(MPI_COMM_WORLD)    ! the header is checked against the pieces: wait until all are written
if (rank == 0) then
  error = header%initialize(filename='mpi.pvtu', mesh_topology='PUnstructuredGrid', mesh_kind='Float64')
  error = header%xml_writer%write_dataarray(location='cell', action='open')
  error = header%xml_writer%write_parallel_dataarray(data_name='rank', data_type='Int32', number_of_components=1)
  error = header%xml_writer%write_dataarray(location='cell', action='close')
  do r=0, nranks - 1
    write(filename, '(A,I0,A)') 'mpi_piece_', r, '.vtu'
    error = header%xml_writer%write_parallel_geo(source=trim(filename))
  enddo
  error = header%finalize()
  ! read the header back and check every piece against it
  error = header%initialize(filename='mpi.pvtu', action='read')
  error = header%xml_reader%check_pieces(message=message)
  print '(A,I0,A,I0)', 'mpi.pvtu: ', nranks, ' pieces, check_pieces error ', error
  error = header%finalize()
endif
