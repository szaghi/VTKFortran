! read the assembly back: its blocks and datasets, depth first
error = assembly%initialize(filename='heat.vtm', action='read')
error = assembly%get_entries(level=level, kind=kind, name=name, file=file)
do e=1, size(level)
  print '(A,A,1X,A,1X,A)', repeat('  ', level(e) - 1), trim(kind(e)), trim(name(e)), trim(file(e))
enddo
error = assembly%finalize()
