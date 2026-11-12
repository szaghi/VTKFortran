error = reader%initialize(filename='history.vtkhdf', action='read')
error = reader%xml_reader%read_dataarray(location='row', data_name='displacement', x=column)
print '(A,I0,A)', 'history.vtkhdf: ', size(column), ' rows; the displacement every 2 time units:'
print '(2X,*(F7.3))', column(1::8)
error = reader%finalize()
error = reader%initialize(filename='probes.vtkhdf', action='read')
error = reader%xml_reader%get_steps(times=times)
print '(A,I0,A)', 'probes.vtkhdf: ', size(times), ' steps; the wave at the probes:'
do s=1, size(times)
  error = reader%xml_reader%set_step(s)
  error = reader%xml_reader%read_dataarray(location='row', data_name='wave', x=column)
  print '(A,F4.2,A,*(F7.3))', '  t = ', times(s), ':', column
enddo
error = reader%finalize()
