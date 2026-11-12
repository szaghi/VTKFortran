error = reader%initialize(filename='amr.vtkhdf', action='read')
error = reader%xml_reader%get_levels(n_levels)
do level=0, n_levels - 1
  error = reader%xml_reader%set_level(level)
  error = reader%xml_reader%get_info(npieces=n_boxes, spacing=spacing)
  print '(A,I0,A,F4.2,A,I0)', 'level ', level, ': spacing ', spacing(1), ', boxes ', n_boxes
  do b=1, n_boxes
    error = reader%xml_reader%read_piece(piece=b, nx1=nx1, nx2=nx2, ny1=ny1, ny2=ny2)
    error = reader%xml_reader%read_dataarray(location='node', data_name='temperature', x=temperature, piece=b)
    print '(A,I0,A,4(1X,I2),A,F6.3)', '  box ', b, ': points', nx1, nx2, ny1, ny2, ', hottest', maxval(temperature)
  enddo
enddo
error = reader%finalize()
