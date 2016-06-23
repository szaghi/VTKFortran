  !< Implementation of geo-rectilinear write of VTK file class.
  function write_geo_rect_data3_rank1_R8P(self, x, y, z) result(error)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Write mesh with **RectilinearGrid** topology (data 3, rank 1, R8P).
  !---------------------------------------------------------------------------------------------------------------------------------
  class(vtk_file), intent(inout) :: self  !< VTK file.
  real(R8P),       intent(in)    :: x(1:) !< X coordinates.
  real(R8P),       intent(in)    :: y(1:) !< Y coordinates.
  real(R8P),       intent(in)    :: z(1:) !< Z coordinates.
  integer(I4P)                   :: error !< Error status.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  call self%write_start_tag(tag_name='Coordinates')
  error = self%write_dataarray(data_name='X', x=x)
  error = self%write_dataarray(data_name='Y', x=y)
  error = self%write_dataarray(data_name='Z', x=z)
  call self%write_end_tag(tag_name='Coordinates')
  error = self%error
  return
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction write_geo_rect_data3_rank1_R8P

  function write_geo_rect_data3_rank1_R4P(self, x, y, z) result(error)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Write mesh with **RectilinearGrid** topology (data 3, rank 1, R4P).
  !---------------------------------------------------------------------------------------------------------------------------------
  class(vtk_file), intent(inout) :: self  !< VTK file.
  real(R4P),       intent(in)    :: x(1:) !< X coordinates.
  real(R4P),       intent(in)    :: y(1:) !< Y coordinates.
  real(R4P),       intent(in)    :: z(1:) !< Z coordinates.
  integer(I4P)                   :: error !< Error status.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  call self%write_start_tag(tag_name='Coordinates')
  error = self%write_dataarray(data_name='X', x=x)
  error = self%write_dataarray(data_name='Y', x=y)
  error = self%write_dataarray(data_name='Z', x=z)
  call self%write_end_tag(tag_name='Coordinates')
  error = self%error
  return
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction write_geo_rect_data3_rank1_R4P
