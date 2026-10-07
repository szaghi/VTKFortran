! read the header back and check that every piece holds what it declares
error = header%initialize(filename='heat.pvtu', action='read')
error = header%xml_reader%check_pieces(message=message)
print '(A,I0,A,A)', 'heat.pvtu: ', pieces, ' pieces, check_pieces: ', merge('OK     ', message, error == 0)
error = header%finalize()
