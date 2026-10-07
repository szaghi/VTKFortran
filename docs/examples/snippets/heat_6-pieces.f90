! each "process" writes its own piece: the cells it owns, plus one layer of ghost cells of each neighbour
do p=1, pieces
  write(sources(p), '(A,I1,A)') 'heat_', p, '.vtu'
  error = write_piece(p, trim(sources(p)))
enddo
