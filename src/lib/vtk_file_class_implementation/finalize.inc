  ! Implementation of finalize of VTK file class.
  function finalize_write(self) result(error)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Finalize file (exporter).
  !---------------------------------------------------------------------------------------------------------------------------------
  class(vtk_file), intent(inout) :: self  !< VTK file.
  integer(I4P)                   :: error !< Error status.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  call self%write_end_tag(tag_name=self%topology%chars())
  if (self%format==raw.or.self%format==bin_app) call self%write_dataarray_appended
  call self%write_end_tag(tag_name='VTKFile')
  error = self%error
  return
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction finalize_write
