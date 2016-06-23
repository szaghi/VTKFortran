  !< Implementation of field data write of VTK file class.
  function write_fielddata1_rank0_R8P(self, data_name, x) result(error)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Write `<DataArray... NumberOfTuples="..."...>...</DataArray>` tag (R8P).
  !---------------------------------------------------------------------------------------------------------------------------------
  class(vtk_file), intent(inout) :: self      !< VTK file.
  character(*),    intent(in)    :: data_name !< Data name.
  real(R8P),       intent(in)    :: x         !< Data variable.
  integer(I4P)                   :: error     !< Error status.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  self%error = self%write_dataarray(data_name=data_name, x=[x], is_tuples=.true.)
  error = self%error
  return
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction write_fielddata1_rank0_R8P

  function write_fielddata1_rank0_R4P(self, data_name, x) result(error)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Write `<DataArray... NumberOfTuples="..."...>...</DataArray>` tag (R4P).
  !---------------------------------------------------------------------------------------------------------------------------------
  class(vtk_file), intent(inout) :: self      !< VTK file.
  character(*),    intent(in)    :: data_name !< Data name.
  real(R4P),       intent(in)    :: x         !< Data variable.
  integer(I4P)                   :: error     !< Error status.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  self%error = self%write_dataarray(data_name=data_name, x=[x], is_tuples=.true.)
  error = self%error
  return
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction write_fielddata1_rank0_R4P

  function write_fielddata1_rank0_I8P(self, data_name, x) result(error)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Write `<DataArray... NumberOfTuples="..."...>...</DataArray>` tag (I8P).
  !---------------------------------------------------------------------------------------------------------------------------------
  class(vtk_file), intent(inout) :: self      !< VTK file.
  character(*),    intent(in)    :: data_name !< Data name.
  integer(I8P),    intent(in)    :: x         !< Data variable.
  integer(I4P)                   :: error     !< Error status.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  self%error = self%write_dataarray(data_name=data_name, x=[x], is_tuples=.true.)
  error = self%error
  return
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction write_fielddata1_rank0_I8P

  function write_fielddata1_rank0_I4P(self, data_name, x) result(error)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Write `<DataArray... NumberOfTuples="..."...>...</DataArray>` tag (I4P).
  !---------------------------------------------------------------------------------------------------------------------------------
  class(vtk_file), intent(inout) :: self      !< VTK file.
  character(*),    intent(in)    :: data_name !< Data name.
  integer(I4P),    intent(in)    :: x         !< Data variable.
  integer(I4P)                   :: error     !< Error status.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  self%error = self%write_dataarray(data_name=data_name, x=[x], is_tuples=.true.)
  error = self%error
  return
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction write_fielddata1_rank0_I4P

  function write_fielddata1_rank0_I2P(self, data_name, x) result(error)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Write `<DataArray... NumberOfTuples="..."...>...</DataArray>` tag (I2P).
  !---------------------------------------------------------------------------------------------------------------------------------
  class(vtk_file), intent(inout) :: self      !< VTK file.
  character(*),    intent(in)    :: data_name !< Data name.
  integer(I2P),    intent(in)    :: x         !< Data variable.
  integer(I4P)                   :: error     !< Error status.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  self%error = self%write_dataarray(data_name=data_name, x=[x], is_tuples=.true.)
  error = self%error
  return
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction write_fielddata1_rank0_I2P

  function write_fielddata1_rank0_I1P(self, data_name, x) result(error)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Write `<DataArray... NumberOfTuples="..."...>...</DataArray>` tag (I1P).
  !---------------------------------------------------------------------------------------------------------------------------------
  class(vtk_file), intent(inout) :: self      !< VTK file.
  character(*),    intent(in)    :: data_name !< Data name.
  integer(I1P),    intent(in)    :: x         !< Data variable.
  integer(I4P)                   :: error     !< Error status.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  self%error = self%write_dataarray(data_name=data_name, x=[x], is_tuples=.true.)
  error = self%error
  return
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction write_fielddata1_rank0_I1P

  function write_fielddata_tag(self, action) result(error)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Write `<FieldData>`/`</FieldData>` start/end tag.
  !---------------------------------------------------------------------------------------------------------------------------------
  class(vtk_file), intent(inout) :: self      !< VTK file.
  character(*),    intent(in)    :: action    !< Action: **open** or **close** tag.
  integer(I4P)                   :: error     !< Error status.
  type(string)                   :: action_   !< Action string.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  action_ = trim(adjustl(action)) ; action_ = action_%upper()
  select case(action_%chars())
  case('OPEN')
    call self%write_start_tag(tag_name='FieldData')
  case('CLOSE')
    call self%write_end_tag(tag_name='FieldData')
  endselect
  error = self%error
  return
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction write_fielddata_tag
