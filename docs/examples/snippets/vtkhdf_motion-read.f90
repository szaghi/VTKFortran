! the entries of the multi-block file, then each dataset (its path in the file) read as a .vtkhdf of its own
error = a_vtm_file%initialize(filename='harbour.vtkhdf', action='read')
error = a_vtm_file%get_entries(level=level, kind=kind, name=name, file=file)
error = a_vtm_file%finalize()
print '(A)', 'harbour.vtkhdf, its blocks and datasets:'
do e=1, size(level)
  if (trim(kind(e)) == 'block') then
    print '(A)', repeat('  ', level(e))//'block '//trim(name(e))
  else
    error = a_vtk_file%initialize(filename='harbour.vtkhdf', action='read', dataset=trim(file(e)))
    error = a_vtk_file%xml_reader%get_info(mesh_topology=topology)
    error = a_vtk_file%xml_reader%get_steps(times=times)
    print '(A,I0,A,*(F5.2))', repeat('  ', level(e))//'dataset '//trim(name(e))//' ('//trim(file(e))//', '// &
                              topology//'): ', size(times), ' steps, at', times
    error = a_vtk_file%finalize()
  endif
enddo
