error = assembly%initialize(filename='heat.vtm')
error = assembly%write_block(action='open', name='solver')
error = assembly%write_block(filenames=['heat_1.vtu', 'heat_2.vtu', 'heat_3.vtu', 'heat_4.vtu'], &
                             names=['piece-1', 'piece-2', 'piece-3', 'piece-4'], name='domain')
error = assembly%write_block(filenames=['probes.vtp'], names=['probes'], name='sensors')
error = assembly%write_block(action='close')
error = assembly%finalize()
