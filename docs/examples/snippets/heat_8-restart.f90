! the last dataset of the time series
error = series%initialize(filename='heat.pvd', action='read')
error = series%get_datasets(timestep=timestep, file=file)
error = series%finalize()
! its grid, temperature, time and cycle
error = last%initialize(filename=trim(file(size(file))), action='read')
error = last%xml_reader%read_geo(x=x, y=y, z=z)
error = last%xml_reader%read_dataarray(location='node', data_name='temperature', x=values)
error = last%xml_reader%read_dataarray(location='field', data_name='TIME', x=time_read)
error = last%xml_reader%read_dataarray(location='field', data_name='CYCLE', x=cycle_read)
error = last%finalize()
n = size(x)
t = reshape(values, [n, n, n])
time = time_read(1)
start = int(cycle_read(1), I4P)
print '(A,A,A,I0,A,ES10.3)', 'restart from ', trim(file(size(file))), ': cycle ', start, ', time ', time
