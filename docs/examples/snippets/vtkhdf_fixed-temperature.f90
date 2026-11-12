function temperature(time) result(error)
!< The temperature of the plate at a time: a hot spot moving along x, as point data.
real(R8P), intent(in) :: time  !< Time.
integer(I4P)          :: error !< Error status.

error = a_vtk_file%xml_writer%write_dataarray(location='node', action='open')
error = a_vtk_file%xml_writer%write_dataarray(data_name='temperature', &
                                              x=exp(-((x - 2 - 8 * time)**2 + (y - ny / 2._R8P)**2) / 8))
error = a_vtk_file%xml_writer%write_dataarray(location='node', action='close')
endfunction temperature
