  !< Implementation of geo-structured write of VTK file class.
  function write_geo_strg_data1_rank2_R8P(self, xyz) result(error)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Write mesh with **StructuredGrid** topology (data 1, rank 2, R8P).
  !---------------------------------------------------------------------------------------------------------------------------------
  class(vtk_file), intent(inout) :: self       !< VTK file.
  real(R8P),       intent(in)    :: xyz(1:,1:) !< X, y, z coordinates [1:3,1:n].
  integer(I4P)                   :: error      !< Error status.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  call self%write_start_tag(tag_name='Points')
  error = self%write_dataarray(data_name='Points', x=xyz)
  call self%write_end_tag(tag_name='Points')
  error = self%error
  return
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction write_geo_strg_data1_rank2_R8P

  function write_geo_strg_data1_rank2_R4P(self, xyz) result(error)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Write mesh with **StructuredGrid** topology (data 1, rank 2, R4P).
  !---------------------------------------------------------------------------------------------------------------------------------
  class(vtk_file), intent(inout) :: self       !< VTK file.
  real(R4P),       intent(in)    :: xyz(1:,1:) !< X, y, z coordinates [1:3,:].
  integer(I4P)                   :: error      !< Error status.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  call self%write_start_tag(tag_name='Points')
  error = self%write_dataarray(data_name='Points', x=xyz)
  call self%write_end_tag(tag_name='Points')
  error = self%error
  return
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction write_geo_strg_data1_rank2_R4P

  function write_geo_strg_data1_rank4_R8P(self, xyz) result(error)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Write mesh with **StructuredGrid** topology (data 1, rank 4, R8P).
  !---------------------------------------------------------------------------------------------------------------------------------
  class(vtk_file), intent(inout) :: self             !< VTK file.
  real(R8P),       intent(in)    :: xyz(1:,1:,1:,1:) !< X, y, z coordinates [1:3,:,:,:].
  integer(I4P)                   :: error            !< Error status.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  call self%write_start_tag(tag_name='Points')
  error = self%write_dataarray(data_name='Points', x=xyz)
  call self%write_end_tag(tag_name='Points')
  error = self%error
  return
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction write_geo_strg_data1_rank4_R8P

  function write_geo_strg_data1_rank4_R4P(self, xyz) result(error)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Write mesh with **StructuredGrid** topology (data 1, rank 4, R4P).
  !---------------------------------------------------------------------------------------------------------------------------------
  class(vtk_file), intent(inout) :: self             !< VTK file.
  real(R4P),       intent(in)    :: xyz(1:,1:,1:,1:) !< X, y, z coordinates [1:3,:,:,:].
  integer(I4P)                   :: error            !< Error status.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  call self%write_start_tag(tag_name='Points')
  error = self%write_dataarray(data_name='Points', x=xyz)
  call self%write_end_tag(tag_name='Points')
  error = self%error
  return
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction write_geo_strg_data1_rank4_R4P

  function write_geo_strg_data3_rank1_R8P(self, n, x, y, z) result(error)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Write mesh with **StructuredGrid** topology (data 3, rank 1, R8P).
  !---------------------------------------------------------------------------------------------------------------------------------
  class(vtk_file), intent(inout) :: self  !< VTK file.
  integer(I4P),    intent(in)    :: n     !< Number of nodes.
  real(R8P),       intent(in)    :: x(1:) !< X coordinates.
  real(R8P),       intent(in)    :: y(1:) !< Y coordinates.
  real(R8P),       intent(in)    :: z(1:) !< Z coordinates.
  integer(I4P)                   :: error !< Error status.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  if ((n/=size(x, dim=1)).or.(n/=size(y, dim=1)).or.(n/=size(z, dim=1))) then
    self%error = 1
    return
  endif
  call self%write_start_tag(tag_name='Points')
  error = self%write_dataarray(data_name='Points', x=x, y=y, z=z)
  call self%write_end_tag(tag_name='Points')
  error = self%error
  return
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction write_geo_strg_data3_rank1_R8P

  function write_geo_strg_data3_rank1_R4P(self, n, x, y, z) result(error)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Write mesh with **StructuredGrid** topology (data 3, rank 1, R4P).
  !---------------------------------------------------------------------------------------------------------------------------------
  class(vtk_file), intent(inout) :: self  !< VTK file.
  integer(I4P),    intent(in)    :: n     !< Number of nodes.
  real(R4P),       intent(in)    :: x(1:) !< X coordinates.
  real(R4P),       intent(in)    :: y(1:) !< Y coordinates.
  real(R4P),       intent(in)    :: z(1:) !< Z coordinates.
  integer(I4P)                   :: error !< Error status.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  if ((n/=size(x, dim=1)).or.(n/=size(y, dim=1)).or.(n/=size(z, dim=1))) then
    self%error = 1
    return
  endif
  call self%write_start_tag(tag_name='Points')
  error = self%write_dataarray(data_name='Points', x=x, y=y, z=z)
  call self%write_end_tag(tag_name='Points')
  error = self%error
  return
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction write_geo_strg_data3_rank1_R4P

  function write_geo_strg_data3_rank3_R8P(self, n, x, y, z) result(error)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Write mesh with **StructuredGrid** topology (data 3, rank 3, R8P).
  !---------------------------------------------------------------------------------------------------------------------------------
  class(vtk_file), intent(inout) :: self        !< VTK file.
  integer(I4P),    intent(in)    :: n           !< Number of nodes.
  real(R8P),       intent(in)    :: x(1:,1:,1:) !< X coordinates.
  real(R8P),       intent(in)    :: y(1:,1:,1:) !< Y coordinates.
  real(R8P),       intent(in)    :: z(1:,1:,1:) !< Z coordinates.
  integer(I4P)                   :: error       !< Error status.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  if ((n/=size(x, dim=1)*size(x, dim=2)*size(x, dim=3)).or.&
      (n/=size(y, dim=1)*size(y, dim=2)*size(y, dim=3)).or.&
      (n/=size(z, dim=1)*size(z, dim=2)*size(z, dim=3))) then
    self%error = 1
    return
  endif
  call self%write_start_tag(tag_name='Points')
  error = self%write_dataarray(data_name='Points', x=x, y=y, z=z)
  call self%write_end_tag(tag_name='Points')
  error = self%error
  return
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction write_geo_strg_data3_rank3_R8P

  function write_geo_strg_data3_rank3_R4P(self, n, x, y, z) result(error)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Write mesh with **StructuredGrid** topology (data 3, rank 3, R4P).
  !---------------------------------------------------------------------------------------------------------------------------------
  class(vtk_file), intent(inout) :: self        !< VTK file.
  integer(I4P),    intent(in)    :: n           !< Number of nodes.
  real(R4P),       intent(in)    :: x(1:,1:,1:) !< X coordinates.
  real(R4P),       intent(in)    :: y(1:,1:,1:) !< Y coordinates.
  real(R4P),       intent(in)    :: z(1:,1:,1:) !< Z coordinates.
  integer(I4P)                   :: error       !< Error status.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  if ((n/=size(x, dim=1)*size(x, dim=2)*size(x, dim=3)).or.&
      (n/=size(y, dim=1)*size(y, dim=2)*size(y, dim=3)).or.&
      (n/=size(z, dim=1)*size(z, dim=2)*size(z, dim=3))) then
    self%error = 1
    return
  endif
  call self%write_start_tag(tag_name='Points')
  error = self%write_dataarray(data_name='Points', x=x, y=y, z=z)
  call self%write_end_tag(tag_name='Points')
  error = self%error
  return
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction write_geo_strg_data3_rank3_R4P
