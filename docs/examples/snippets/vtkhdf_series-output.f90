function output(time) result(error)
!< One output: the step, its field data, its piece with the wave at that time.
real(R8P), intent(in) :: time  !< Time.
integer(I4P)          :: error !< Error status.
real(R8P)             :: wave(0:nx,0:ny)
integer(I4P)          :: i, j

do j=0, ny ; do i=0, nx
  wave(i,j) = sin(2._R8P * pi * (i / real(nx, R8P) - 0.5_R8P * time)) * cos(pi * (j / real(ny, R8P) - 0.5_R8P))
enddo ; enddo
error = a_vtk_file%xml_writer%write_step(time=time)
error = a_vtk_file%xml_writer%write_fielddata(action='open')
error = a_vtk_file%xml_writer%write_fielddata(data_name='TIME', x=time)
error = a_vtk_file%xml_writer%write_fielddata(action='close')
error = a_vtk_file%xml_writer%write_piece(nx1=0, nx2=nx, ny1=0, ny2=ny, nz1=0, nz2=0)
error = a_vtk_file%xml_writer%write_dataarray(location='node', action='open')
error = a_vtk_file%xml_writer%write_dataarray(data_name='wave', x=wave, one_component=.true.)
error = a_vtk_file%xml_writer%write_dataarray(location='node', action='close')
error = a_vtk_file%xml_writer%write_piece()
endfunction output
