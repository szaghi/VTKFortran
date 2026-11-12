error = reader%initialize(filename='history.vtt', action='read')
error = reader%xml_reader%get_dataarray_names(location='row', names=names)
print '(A,*(1X,A))', 'history.vtt columns:', (trim(names(i)), i=1, size(names))
error = reader%xml_reader%read_dataarray(location='row', data_name='displacement', x=column)
print '(A,I0,A)', 'history.vtt: ', size(column), ' rows; the displacement every 2 time units:'
print '(2X,*(F7.3))', column(1::8)
error = reader%finalize()
