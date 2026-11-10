! every rank: its own piece, in a file named after the rank, with point ids local to the piece
write(filename, '(A,I0,A)') 'mpi_piece_', rank, '.vtu'
error = write_piece(trim(filename), rank)
