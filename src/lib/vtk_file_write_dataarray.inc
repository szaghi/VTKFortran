  !< Implementation of dataarray write of VTK file class.
  function write_dataarray1_rank1_R8P(self, data_name, x, is_tuples) result(error)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Write `<DataArray... NumberOfComponents="1"...>...</DataArray>` tag (R8P).
  !---------------------------------------------------------------------------------------------------------------------------------
  class(vtk_file), intent(inout)        :: self         !< VTK file.
  character(*),    intent(in)           :: data_name    !< Data name.
  real(R8P),       intent(in)           :: x(1:)        !< Data variable.
  logical,         intent(in), optional :: is_tuples    !< Use "NumberOfTuples" instead of "NumberOfComponents" attribute.
  integer(I4P)                          :: error        !< Error status.
  character(len=:), allocatable         :: data_type    !< Data type.
  integer(I4P)                          :: n_components !< Number of components.
  character(len=:), allocatable         :: code         !< Data variable encoded, ascii or Base64 codec.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  data_type = 'Float64'
  n_components = 1
  select case(self%format)
  case(ascii)
    code = self%encode_ascii_dataarray(x=x)
    call self%write_dataarray_tag(data_type=data_type, number_of_components=n_components, data_name=data_name, data_content=code, &
                                  is_tuples=is_tuples)
  case(raw, bin_app)
    call self%write_dataarray_tag_appended(data_type=data_type, number_of_components=n_components, data_name=data_name, &
                                           is_tuples=is_tuples)
    call self%ioffset_update(n_byte=self%write_on_scratch_dataarray(x=x))
  case(binary)
    code = self%encode_base64_dataarray(x=x)
    call self%write_dataarray_tag(data_type=data_type, number_of_components=n_components, data_name=data_name, data_content=code, &
                                  is_tuples=is_tuples)
  endselect
  error = self%error
  return
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction write_dataarray1_rank1_R8P

  function write_dataarray1_rank1_R4P(self, data_name, x, is_tuples) result(error)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Write `<DataArray... NumberOfComponents="1"...>...</DataArray>` tag (R4P).
  !---------------------------------------------------------------------------------------------------------------------------------
  class(vtk_file), intent(inout)        :: self         !< VTK file.
  character(*),    intent(in)           :: data_name    !< Data name.
  real(R4P),       intent(in)           :: x(1:)        !< Data variable.
  logical,         intent(in), optional :: is_tuples    !< Use "NumberOfTuples" instead of "NumberOfComponents" attribute.
  integer(I4P)                          :: error        !< Error status.
  character(len=:), allocatable         :: data_type    !< Data type.
  integer(I4P)                          :: n_components !< Number of components.
  character(len=:), allocatable         :: code         !< Data variable encoded, ascii or Base64 codec.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  data_type = 'Float32'
  n_components = 1
  select case(self%format)
  case(ascii)
    code = self%encode_ascii_dataarray(x=x)
    call self%write_dataarray_tag(data_type=data_type, number_of_components=n_components, data_name=data_name, data_content=code, &
                                  is_tuples=is_tuples)
  case(raw, bin_app)
    call self%write_dataarray_tag_appended(data_type=data_type, number_of_components=n_components, data_name=data_name, &
                                           is_tuples=is_tuples)
    call self%ioffset_update(n_byte=self%write_on_scratch_dataarray(x=x))
  case(binary)
    code = self%encode_base64_dataarray(x=x)
    call self%write_dataarray_tag(data_type=data_type, number_of_components=n_components, data_name=data_name, data_content=code, &
                                  is_tuples=is_tuples)
  endselect
  error = self%error
  return
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction write_dataarray1_rank1_R4P

  function write_dataarray1_rank1_I8P(self, data_name, x, is_tuples) result(error)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Write `<DataArray... NumberOfComponents="1"...>...</DataArray>` tag (I8P).
  !---------------------------------------------------------------------------------------------------------------------------------
  class(vtk_file), intent(inout)        :: self         !< VTK file.
  character(*),    intent(in)           :: data_name    !< Data name.
  integer(I8P),    intent(in)           :: x(1:)        !< Data variable.
  logical,         intent(in), optional :: is_tuples    !< Use "NumberOfTuples" instead of "NumberOfComponents" attribute.
  integer(I4P)                          :: error        !< Error status.
  character(len=:), allocatable         :: data_type    !< Data type.
  integer(I4P)                          :: n_components !< Number of components.
  character(len=:), allocatable         :: code         !< Data variable encoded, ascii or Base64 codec.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  data_type = 'Int64'
  n_components = 1
  select case(self%format)
  case(ascii)
    code = self%encode_ascii_dataarray(x=x)
    call self%write_dataarray_tag(data_type=data_type, number_of_components=n_components, data_name=data_name, data_content=code, &
                                  is_tuples=is_tuples)
  case(raw, bin_app)
    call self%write_dataarray_tag_appended(data_type=data_type, number_of_components=n_components, data_name=data_name, &
                                           is_tuples=is_tuples)
    call self%ioffset_update(n_byte=self%write_on_scratch_dataarray(x=x))
  case(binary)
    code = self%encode_base64_dataarray(x=x)
    call self%write_dataarray_tag(data_type=data_type, number_of_components=n_components, data_name=data_name, data_content=code, &
                                  is_tuples=is_tuples)
  endselect
  error = self%error
  return
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction write_dataarray1_rank1_I8P

  function write_dataarray1_rank1_I4P(self, data_name, x, is_tuples) result(error)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Write `<DataArray... NumberOfComponents="1"...>...</DataArray>` tag (I4P).
  !---------------------------------------------------------------------------------------------------------------------------------
  class(vtk_file), intent(inout)        :: self         !< VTK file.
  character(*),    intent(in)           :: data_name    !< Data name.
  integer(I4P),    intent(in)           :: x(1:)        !< Data variable.
  logical,         intent(in), optional :: is_tuples    !< Use "NumberOfTuples" instead of "NumberOfComponents" attribute.
  integer(I4P)                          :: error        !< Error status.
  character(len=:), allocatable         :: data_type    !< Data type.
  integer(I4P)                          :: n_components !< Number of components.
  character(len=:), allocatable         :: code         !< Data variable encoded, ascii or Base64 codec.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  data_type = 'Int32'
  n_components = 1
  select case(self%format)
  case(ascii)
    code = self%encode_ascii_dataarray(x=x)
    call self%write_dataarray_tag(data_type=data_type, number_of_components=n_components, data_name=data_name, data_content=code, &
                                  is_tuples=is_tuples)
  case(raw, bin_app)
    call self%write_dataarray_tag_appended(data_type=data_type, number_of_components=n_components, data_name=data_name, &
                                           is_tuples=is_tuples)
    call self%ioffset_update(n_byte=self%write_on_scratch_dataarray(x=x))
  case(binary)
    code = self%encode_base64_dataarray(x=x)
    call self%write_dataarray_tag(data_type=data_type, number_of_components=n_components, data_name=data_name, data_content=code, &
                                  is_tuples=is_tuples)
  endselect
  error = self%error
  return
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction write_dataarray1_rank1_I4P

  function write_dataarray1_rank1_I2P(self, data_name, x, is_tuples) result(error)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Write `<DataArray... NumberOfComponents="1"...>...</DataArray>` tag (I2P).
  !---------------------------------------------------------------------------------------------------------------------------------
  class(vtk_file), intent(inout)        :: self         !< VTK file.
  character(*),    intent(in)           :: data_name    !< Data name.
  integer(I2P),    intent(in)           :: x(1:)        !< Data variable.
  logical,         intent(in), optional :: is_tuples    !< Use "NumberOfTuples" instead of "NumberOfComponents" attribute.
  integer(I4P)                          :: error        !< Error status.
  character(len=:), allocatable         :: data_type    !< Data type.
  integer(I4P)                          :: n_components !< Number of components.
  character(len=:), allocatable         :: code         !< Data variable encoded, ascii or Base64 codec.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  data_type = 'Int16'
  n_components = 1
  select case(self%format)
  case(ascii)
    code = self%encode_ascii_dataarray(x=x)
    call self%write_dataarray_tag(data_type=data_type, number_of_components=n_components, data_name=data_name, data_content=code, &
                                  is_tuples=is_tuples)
  case(raw, bin_app)
    call self%write_dataarray_tag_appended(data_type=data_type, number_of_components=n_components, data_name=data_name, &
                                           is_tuples=is_tuples)
    call self%ioffset_update(n_byte=self%write_on_scratch_dataarray(x=x))
  case(binary)
    code = self%encode_base64_dataarray(x=x)
    call self%write_dataarray_tag(data_type=data_type, number_of_components=n_components, data_name=data_name, data_content=code, &
                                  is_tuples=is_tuples)
  endselect
  error = self%error
  return
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction write_dataarray1_rank1_I2P

  function write_dataarray1_rank1_I1P(self, data_name, x, is_tuples) result(error)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Write `<DataArray... NumberOfComponents="1"...>...</DataArray>` tag (I1P).
  !---------------------------------------------------------------------------------------------------------------------------------
  class(vtk_file), intent(inout)        :: self         !< VTK file.
  character(*),    intent(in)           :: data_name    !< Data name.
  integer(I1P),    intent(in)           :: x(1:)        !< Data variable.
  logical,         intent(in), optional :: is_tuples    !< Use "NumberOfTuples" instead of "NumberOfComponents" attribute.
  integer(I4P)                          :: error        !< Error status.
  character(len=:), allocatable         :: data_type    !< Data type.
  integer(I4P)                          :: n_components !< Number of components.
  character(len=:), allocatable         :: code         !< Data variable encoded, ascii or Base64 codec.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  data_type = 'Int8'
  n_components = 1
  select case(self%format)
  case(ascii)
    code = self%encode_ascii_dataarray(x=x)
    call self%write_dataarray_tag(data_type=data_type, number_of_components=n_components, data_name=data_name, data_content=code, &
                                  is_tuples=is_tuples)
  case(raw, bin_app)
    call self%write_dataarray_tag_appended(data_type=data_type, number_of_components=n_components, data_name=data_name, &
                                           is_tuples=is_tuples)
    call self%ioffset_update(n_byte=self%write_on_scratch_dataarray(x=x))
  case(binary)
    code = self%encode_base64_dataarray(x=x)
    call self%write_dataarray_tag(data_type=data_type, number_of_components=n_components, data_name=data_name, data_content=code, &
                                  is_tuples=is_tuples)
  endselect
  error = self%error
  return
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction write_dataarray1_rank1_I1P

  function write_dataarray1_rank2_R8P(self, data_name, x, one_component, is_tuples) result(error)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Write `<DataArray... NumberOfComponents="n"...>...</DataArray>` tag (R8P).
  !---------------------------------------------------------------------------------------------------------------------------------
  class(vtk_file), intent(inout)        :: self          !< VTK file.
  character(*),    intent(in)           :: data_name     !< Data name.
  real(R8P),       intent(in)           :: x(1:,1:)      !< Data variable.
  logical,         intent(in), optional :: one_component !< Force one component instead of inferring from first rank-size.
  logical,         intent(in), optional :: is_tuples     !< Use "NumberOfTuples" instead of "NumberOfComponents" attribute.
  integer(I4P)                          :: error         !< Error status.
  character(len=:), allocatable         :: data_type     !< Data type.
  integer(I4P)                          :: n_components  !< Number of components.
  character(len=:), allocatable         :: code          !< Data variable encoded, ascii or Base64 codec.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  data_type = 'Float64'
  n_components = size(x, dim=1)
  if (present(one_component)) then
    if (one_component) n_components = 1
  endif
  select case(self%format)
  case(ascii)
    code = self%encode_ascii_dataarray(x=x)
    call self%write_dataarray_tag(data_type=data_type, number_of_components=n_components, data_name=data_name, data_content=code, &
                                  is_tuples=is_tuples)
  case(raw, bin_app)
    call self%write_dataarray_tag_appended(data_type=data_type, number_of_components=n_components, data_name=data_name, &
                                           is_tuples=is_tuples)
    call self%ioffset_update(n_byte=self%write_on_scratch_dataarray(x=x))
  case(binary)
    code = self%encode_base64_dataarray(x=x)
    call self%write_dataarray_tag(data_type=data_type, number_of_components=n_components, data_name=data_name, data_content=code, &
                                  is_tuples=is_tuples)
  endselect
  error = self%error
  return
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction write_dataarray1_rank2_R8P

  function write_dataarray1_rank2_R4P(self, data_name, x, one_component, is_tuples) result(error)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Write `<DataArray... NumberOfComponents="n"...>...</DataArray>` tag (R4P).
  !---------------------------------------------------------------------------------------------------------------------------------
  class(vtk_file), intent(inout)        :: self          !< VTK file.
  character(*),    intent(in)           :: data_name     !< Data name.
  real(R4P),       intent(in)           :: x(1:,1:)      !< Data variable.
  logical,         intent(in), optional :: one_component !< Force one component instead of inferring from first rank-size.
  logical,         intent(in), optional :: is_tuples     !< Use "NumberOfTuples" instead of "NumberOfComponents" attribute.
  integer(I4P)                          :: error         !< Error status.
  character(len=:), allocatable         :: data_type     !< Data type.
  integer(I4P)                          :: n_components  !< Number of components.
  character(len=:), allocatable         :: code          !< Data variable encoded, ascii or Base64 codec.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  data_type = 'Float32'
  n_components = size(x, dim=1)
  if (present(one_component)) then
    if (one_component) n_components = 1
  endif
  select case(self%format)
  case(ascii)
    code = self%encode_ascii_dataarray(x=x)
    call self%write_dataarray_tag(data_type=data_type, number_of_components=n_components, data_name=data_name, data_content=code, &
                                  is_tuples=is_tuples)
  case(raw, bin_app)
    call self%write_dataarray_tag_appended(data_type=data_type, number_of_components=n_components, data_name=data_name, &
                                           is_tuples=is_tuples)
    call self%ioffset_update(n_byte=self%write_on_scratch_dataarray(x=x))
  case(binary)
    code = self%encode_base64_dataarray(x=x)
    call self%write_dataarray_tag(data_type=data_type, number_of_components=n_components, data_name=data_name, data_content=code, &
                                  is_tuples=is_tuples)
  endselect
  error = self%error
  return
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction write_dataarray1_rank2_R4P

  function write_dataarray1_rank2_I8P(self, data_name, x, one_component, is_tuples) result(error)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Write `<DataArray... NumberOfComponents="n"...>...</DataArray>` tag (I8P).
  !---------------------------------------------------------------------------------------------------------------------------------
  class(vtk_file), intent(inout)        :: self          !< VTK file.
  character(*),    intent(in)           :: data_name     !< Data name.
  integer(I8P),    intent(in)           :: x(1:,1:)      !< Data variable.
  logical,         intent(in), optional :: one_component !< Force one component instead of inferring from first rank-size.
  logical,         intent(in), optional :: is_tuples     !< Use "NumberOfTuples" instead of "NumberOfComponents" attribute.
  integer(I4P)                          :: error         !< Error status.
  character(len=:), allocatable         :: data_type     !< Data type.
  integer(I4P)                          :: n_components  !< Number of components.
  character(len=:), allocatable         :: code          !< Data variable encoded, ascii or Base64 codec.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  data_type = 'Int64'
  n_components = size(x, dim=1)
  if (present(one_component)) then
    if (one_component) n_components = 1
  endif
  select case(self%format)
  case(ascii)
    code = self%encode_ascii_dataarray(x=x)
    call self%write_dataarray_tag(data_type=data_type, number_of_components=n_components, data_name=data_name, data_content=code, &
                                  is_tuples=is_tuples)
  case(raw, bin_app)
    call self%write_dataarray_tag_appended(data_type=data_type, number_of_components=n_components, data_name=data_name, &
                                           is_tuples=is_tuples)
    call self%ioffset_update(n_byte=self%write_on_scratch_dataarray(x=x))
  case(binary)
    code = self%encode_base64_dataarray(x=x)
    call self%write_dataarray_tag(data_type=data_type, number_of_components=n_components, data_name=data_name, data_content=code, &
                                  is_tuples=is_tuples)
  endselect
  error = self%error
  return
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction write_dataarray1_rank2_I8P

  function write_dataarray1_rank2_I4P(self, data_name, x, one_component, is_tuples) result(error)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Write `<DataArray... NumberOfComponents="n"...>...</DataArray>` tag (I4P).
  !---------------------------------------------------------------------------------------------------------------------------------
  class(vtk_file), intent(inout)        :: self          !< VTK file.
  character(*),    intent(in)           :: data_name     !< Data name.
  integer(I4P),    intent(in)           :: x(1:,1:)      !< Data variable.
  logical,         intent(in), optional :: one_component !< Force one component instead of inferring from first rank-size.
  logical,         intent(in), optional :: is_tuples     !< Use "NumberOfTuples" instead of "NumberOfComponents" attribute.
  integer(I4P)                          :: error         !< Error status.
  character(len=:), allocatable         :: data_type     !< Data type.
  integer(I4P)                          :: n_components  !< Number of components.
  character(len=:), allocatable         :: code          !< Data variable encoded, ascii or Base64 codec.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  data_type = 'Int32'
  n_components = size(x, dim=1)
  if (present(one_component)) then
    if (one_component) n_components = 1
  endif
  select case(self%format)
  case(ascii)
    code = self%encode_ascii_dataarray(x=x)
    call self%write_dataarray_tag(data_type=data_type, number_of_components=n_components, data_name=data_name, data_content=code, &
                                  is_tuples=is_tuples)
  case(raw, bin_app)
    call self%write_dataarray_tag_appended(data_type=data_type, number_of_components=n_components, data_name=data_name, &
                                           is_tuples=is_tuples)
    call self%ioffset_update(n_byte=self%write_on_scratch_dataarray(x=x))
  case(binary)
    code = self%encode_base64_dataarray(x=x)
    call self%write_dataarray_tag(data_type=data_type, number_of_components=n_components, data_name=data_name, data_content=code, &
                                  is_tuples=is_tuples)
  endselect
  error = self%error
  return
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction write_dataarray1_rank2_I4P

  function write_dataarray1_rank2_I2P(self, data_name, x, one_component, is_tuples) result(error)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Write `<DataArray... NumberOfComponents="n"...>...</DataArray>` tag (I2P).
  !---------------------------------------------------------------------------------------------------------------------------------
  class(vtk_file), intent(inout)        :: self          !< VTK file.
  character(*),    intent(in)           :: data_name     !< Data name.
  integer(I2P),    intent(in)           :: x(1:,1:)      !< Data variable.
  logical,         intent(in), optional :: one_component !< Force one component instead of inferring from first rank-size.
  logical,         intent(in), optional :: is_tuples     !< Use "NumberOfTuples" instead of "NumberOfComponents" attribute.
  integer(I4P)                          :: error         !< Error status.
  character(len=:), allocatable         :: data_type     !< Data type.
  integer(I4P)                          :: n_components  !< Number of components.
  character(len=:), allocatable         :: code          !< Data variable encoded, ascii or Base64 codec.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  data_type = 'Int16'
  n_components = size(x, dim=1)
  if (present(one_component)) then
    if (one_component) n_components = 1
  endif
  select case(self%format)
  case(ascii)
    code = self%encode_ascii_dataarray(x=x)
    call self%write_dataarray_tag(data_type=data_type, number_of_components=n_components, data_name=data_name, data_content=code, &
                                  is_tuples=is_tuples)
  case(raw, bin_app)
    call self%write_dataarray_tag_appended(data_type=data_type, number_of_components=n_components, data_name=data_name, &
                                           is_tuples=is_tuples)
    call self%ioffset_update(n_byte=self%write_on_scratch_dataarray(x=x))
  case(binary)
    code = self%encode_base64_dataarray(x=x)
    call self%write_dataarray_tag(data_type=data_type, number_of_components=n_components, data_name=data_name, data_content=code, &
                                  is_tuples=is_tuples)
  endselect
  error = self%error
  return
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction write_dataarray1_rank2_I2P

  function write_dataarray1_rank2_I1P(self, data_name, x, one_component, is_tuples) result(error)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Write `<DataArray... NumberOfComponents="n"...>...</DataArray>` tag (I1P).
  !---------------------------------------------------------------------------------------------------------------------------------
  class(vtk_file), intent(inout)        :: self          !< VTK file.
  character(*),    intent(in)           :: data_name     !< Data name.
  integer(I1P),    intent(in)           :: x(1:,1:)      !< Data variable.
  logical,         intent(in), optional :: one_component !< Force one component instead of inferring from first rank-size.
  logical,         intent(in), optional :: is_tuples     !< Use "NumberOfTuples" instead of "NumberOfComponents" attribute.
  integer(I4P)                          :: error         !< Error status.
  character(len=:), allocatable         :: data_type     !< Data type.
  integer(I4P)                          :: n_components  !< Number of components.
  character(len=:), allocatable         :: code          !< Data variable encoded, ascii or Base64 codec.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  data_type = 'Int8'
  n_components = size(x, dim=1)
  if (present(one_component)) then
    if (one_component) n_components = 1
  endif
  select case(self%format)
  case(ascii)
    code = self%encode_ascii_dataarray(x=x)
    call self%write_dataarray_tag(data_type=data_type, number_of_components=n_components, data_name=data_name, data_content=code, &
                                  is_tuples=is_tuples)
  case(raw, bin_app)
    call self%write_dataarray_tag_appended(data_type=data_type, number_of_components=n_components, data_name=data_name, &
                                           is_tuples=is_tuples)
    call self%ioffset_update(n_byte=self%write_on_scratch_dataarray(x=x))
  case(binary)
    code = self%encode_base64_dataarray(x=x)
    call self%write_dataarray_tag(data_type=data_type, number_of_components=n_components, data_name=data_name, data_content=code, &
                                  is_tuples=is_tuples)
  endselect
  error = self%error
  return
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction write_dataarray1_rank2_I1P

  function write_dataarray1_rank3_R8P(self, data_name, x, one_component, is_tuples) result(error)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Write `<DataArray... NumberOfComponents="n"...>...</DataArray>` tag (R8P).
  !---------------------------------------------------------------------------------------------------------------------------------
  class(vtk_file), intent(inout)        :: self          !< VTK file.
  character(*),    intent(in)           :: data_name     !< Data name.
  real(R8P),       intent(in)           :: x(1:,1:,1:)   !< Data variable.
  logical,         intent(in), optional :: one_component !< Force one component instead of inferring from first rank-size.
  logical,         intent(in), optional :: is_tuples     !< Use "NumberOfTuples" instead of "NumberOfComponents" attribute.
  integer(I4P)                          :: error         !< Error status.
  character(len=:), allocatable         :: data_type     !< Data type.
  integer(I4P)                          :: n_components  !< Number of components.
  character(len=:), allocatable         :: code          !< Data variable encoded, ascii or Base64 codec.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  data_type = 'Float64'
  n_components = size(x, dim=1)
  if (present(one_component)) then
    if (one_component) n_components = 1
  endif
  select case(self%format)
  case(ascii)
    code = self%encode_ascii_dataarray(x=x)
    call self%write_dataarray_tag(data_type=data_type, number_of_components=n_components, data_name=data_name, data_content=code, &
                                  is_tuples=is_tuples)
  case(raw, bin_app)
    call self%write_dataarray_tag_appended(data_type=data_type, number_of_components=n_components, data_name=data_name, &
                                           is_tuples=is_tuples)
    call self%ioffset_update(n_byte=self%write_on_scratch_dataarray(x=x))
  case(binary)
    code = self%encode_base64_dataarray(x=x)
    call self%write_dataarray_tag(data_type=data_type, number_of_components=n_components, data_name=data_name, data_content=code, &
                                  is_tuples=is_tuples)
  endselect
  error = self%error
  return
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction write_dataarray1_rank3_R8P

  function write_dataarray1_rank3_R4P(self, data_name, x, one_component, is_tuples) result(error)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Write `<DataArray... NumberOfComponents="n"...>...</DataArray>` tag (R4P).
  !---------------------------------------------------------------------------------------------------------------------------------
  class(vtk_file), intent(inout)        :: self          !< VTK file.
  character(*),    intent(in)           :: data_name     !< Data name.
  real(R4P),       intent(in)           :: x(1:,1:,1:)   !< Data variable.
  logical,         intent(in), optional :: one_component !< Force one component instead of inferring from first rank-size.
  logical,         intent(in), optional :: is_tuples     !< Use "NumberOfTuples" instead of "NumberOfComponents" attribute.
  integer(I4P)                          :: error         !< Error status.
  character(len=:), allocatable         :: data_type     !< Data type.
  integer(I4P)                          :: n_components  !< Number of components.
  character(len=:), allocatable         :: code          !< Data variable encoded, ascii or Base64 codec.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  data_type = 'Float32'
  n_components = size(x, dim=1)
  if (present(one_component)) then
    if (one_component) n_components = 1
  endif
  select case(self%format)
  case(ascii)
    code = self%encode_ascii_dataarray(x=x)
    call self%write_dataarray_tag(data_type=data_type, number_of_components=n_components, data_name=data_name, data_content=code, &
                                  is_tuples=is_tuples)
  case(raw, bin_app)
    call self%write_dataarray_tag_appended(data_type=data_type, number_of_components=n_components, data_name=data_name, &
                                           is_tuples=is_tuples)
    call self%ioffset_update(n_byte=self%write_on_scratch_dataarray(x=x))
  case(binary)
    code = self%encode_base64_dataarray(x=x)
    call self%write_dataarray_tag(data_type=data_type, number_of_components=n_components, data_name=data_name, data_content=code, &
                                  is_tuples=is_tuples)
  endselect
  error = self%error
  return
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction write_dataarray1_rank3_R4P

  function write_dataarray1_rank3_I8P(self, data_name, x, one_component, is_tuples) result(error)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Write `<DataArray... NumberOfComponents="n"...>...</DataArray>` tag (I8P).
  !---------------------------------------------------------------------------------------------------------------------------------
  class(vtk_file), intent(inout)        :: self          !< VTK file.
  character(*),    intent(in)           :: data_name     !< Data name.
  integer(I8P),    intent(in)           :: x(1:,1:,1:)   !< Data variable.
  logical,         intent(in), optional :: one_component !< Force one component instead of inferring from first rank-size.
  logical,         intent(in), optional :: is_tuples     !< Use "NumberOfTuples" instead of "NumberOfComponents" attribute.
  integer(I4P)                          :: error         !< Error status.
  character(len=:), allocatable         :: data_type     !< Data type.
  integer(I4P)                          :: n_components  !< Number of components.
  character(len=:), allocatable         :: code          !< Data variable encoded, ascii or Base64 codec.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  data_type = 'Int64'
  n_components = size(x, dim=1)
  if (present(one_component)) then
    if (one_component) n_components = 1
  endif
  select case(self%format)
  case(ascii)
    code = self%encode_ascii_dataarray(x=x)
    call self%write_dataarray_tag(data_type=data_type, number_of_components=n_components, data_name=data_name, data_content=code, &
                                  is_tuples=is_tuples)
  case(raw, bin_app)
    call self%write_dataarray_tag_appended(data_type=data_type, number_of_components=n_components, data_name=data_name, &
                                           is_tuples=is_tuples)
    call self%ioffset_update(n_byte=self%write_on_scratch_dataarray(x=x))
  case(binary)
    code = self%encode_base64_dataarray(x=x)
    call self%write_dataarray_tag(data_type=data_type, number_of_components=n_components, data_name=data_name, data_content=code, &
                                  is_tuples=is_tuples)
  endselect
  error = self%error
  return
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction write_dataarray1_rank3_I8P

  function write_dataarray1_rank3_I4P(self, data_name, x, one_component, is_tuples) result(error)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Write `<DataArray... NumberOfComponents="n"...>...</DataArray>` tag (I4P).
  !---------------------------------------------------------------------------------------------------------------------------------
  class(vtk_file), intent(inout)        :: self          !< VTK file.
  character(*),    intent(in)           :: data_name     !< Data name.
  integer(I4P),    intent(in)           :: x(1:,1:,1:)   !< Data variable.
  logical,         intent(in), optional :: one_component !< Force one component instead of inferring from first rank-size.
  logical,         intent(in), optional :: is_tuples     !< Use "NumberOfTuples" instead of "NumberOfComponents" attribute.
  integer(I4P)                          :: error         !< Error status.
  character(len=:), allocatable         :: data_type     !< Data type.
  integer(I4P)                          :: n_components  !< Number of components.
  character(len=:), allocatable         :: code          !< Data variable encoded, ascii or Base64 codec.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  data_type = 'Int32'
  n_components = size(x, dim=1)
  if (present(one_component)) then
    if (one_component) n_components = 1
  endif
  select case(self%format)
  case(ascii)
    code = self%encode_ascii_dataarray(x=x)
    call self%write_dataarray_tag(data_type=data_type, number_of_components=n_components, data_name=data_name, data_content=code, &
                                  is_tuples=is_tuples)
  case(raw, bin_app)
    call self%write_dataarray_tag_appended(data_type=data_type, number_of_components=n_components, data_name=data_name, &
                                           is_tuples=is_tuples)
    call self%ioffset_update(n_byte=self%write_on_scratch_dataarray(x=x))
  case(binary)
    code = self%encode_base64_dataarray(x=x)
    call self%write_dataarray_tag(data_type=data_type, number_of_components=n_components, data_name=data_name, data_content=code, &
                                  is_tuples=is_tuples)
  endselect
  error = self%error
  return
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction write_dataarray1_rank3_I4P

  function write_dataarray1_rank3_I2P(self, data_name, x, one_component, is_tuples) result(error)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Write `<DataArray... NumberOfComponents="n"...>...</DataArray>` tag (I2P).
  !---------------------------------------------------------------------------------------------------------------------------------
  class(vtk_file), intent(inout)        :: self          !< VTK file.
  character(*),    intent(in)           :: data_name     !< Data name.
  integer(I2P),    intent(in)           :: x(1:,1:,1:)   !< Data variable.
  logical,         intent(in), optional :: one_component !< Force one component instead of inferring from first rank-size.
  logical,         intent(in), optional :: is_tuples     !< Use "NumberOfTuples" instead of "NumberOfComponents" attribute.
  integer(I4P)                          :: error         !< Error status.
  character(len=:), allocatable         :: data_type     !< Data type.
  integer(I4P)                          :: n_components  !< Number of components.
  character(len=:), allocatable         :: code          !< Data variable encoded, ascii or Base64 codec.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  data_type = 'Int16'
  n_components = size(x, dim=1)
  if (present(one_component)) then
    if (one_component) n_components = 1
  endif
  select case(self%format)
  case(ascii)
    code = self%encode_ascii_dataarray(x=x)
    call self%write_dataarray_tag(data_type=data_type, number_of_components=n_components, data_name=data_name, data_content=code, &
                                  is_tuples=is_tuples)
  case(raw, bin_app)
    call self%write_dataarray_tag_appended(data_type=data_type, number_of_components=n_components, data_name=data_name, &
                                           is_tuples=is_tuples)
    call self%ioffset_update(n_byte=self%write_on_scratch_dataarray(x=x))
  case(binary)
    code = self%encode_base64_dataarray(x=x)
    call self%write_dataarray_tag(data_type=data_type, number_of_components=n_components, data_name=data_name, data_content=code, &
                                  is_tuples=is_tuples)
  endselect
  error = self%error
  return
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction write_dataarray1_rank3_I2P

  function write_dataarray1_rank3_I1P(self, data_name, x, one_component, is_tuples) result(error)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Write `<DataArray... NumberOfComponents="n"...>...</DataArray>` tag (I1P).
  !---------------------------------------------------------------------------------------------------------------------------------
  class(vtk_file), intent(inout)        :: self          !< VTK file.
  character(*),    intent(in)           :: data_name     !< Data name.
  integer(I1P),    intent(in)           :: x(1:,1:,1:)   !< Data variable.
  logical,         intent(in), optional :: one_component !< Force one component instead of inferring from first rank-size.
  logical,         intent(in), optional :: is_tuples     !< Use "NumberOfTuples" instead of "NumberOfComponents" attribute.
  integer(I4P)                          :: error         !< Error status.
  character(len=:), allocatable         :: data_type     !< Data type.
  integer(I4P)                          :: n_components  !< Number of components.
  character(len=:), allocatable         :: code          !< Data variable encoded, ascii or Base64 codec.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  data_type = 'Int8'
  n_components = size(x, dim=1)
  if (present(one_component)) then
    if (one_component) n_components = 1
  endif
  select case(self%format)
  case(ascii)
    code = self%encode_ascii_dataarray(x=x)
    call self%write_dataarray_tag(data_type=data_type, number_of_components=n_components, data_name=data_name, data_content=code, &
                                  is_tuples=is_tuples)
  case(raw, bin_app)
    call self%write_dataarray_tag_appended(data_type=data_type, number_of_components=n_components, data_name=data_name, &
                                           is_tuples=is_tuples)
    call self%ioffset_update(n_byte=self%write_on_scratch_dataarray(x=x))
  case(binary)
    code = self%encode_base64_dataarray(x=x)
    call self%write_dataarray_tag(data_type=data_type, number_of_components=n_components, data_name=data_name, data_content=code, &
                                  is_tuples=is_tuples)
  endselect
  error = self%error
  return
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction write_dataarray1_rank3_I1P

  function write_dataarray1_rank4_R8P(self, data_name, x, one_component, is_tuples) result(error)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Write `<DataArray... NumberOfComponents="n"...>...</DataArray>` tag (R8P).
  !---------------------------------------------------------------------------------------------------------------------------------
  class(vtk_file), intent(inout)        :: self           !< VTK file.
  character(*),    intent(in)           :: data_name      !< Data name.
  real(R8P),       intent(in)           :: x(1:,1:,1:,1:) !< Data variable.
  logical,         intent(in), optional :: one_component  !< Force one component instead of inferring from first rank-size.
  logical,         intent(in), optional :: is_tuples     !< Use "NumberOfTuples" instead of "NumberOfComponents" attribute.
  integer(I4P)                          :: error          !< Error status.
  character(len=:), allocatable         :: data_type      !< Data type.
  integer(I4P)                          :: n_components   !< Number of components.
  character(len=:), allocatable         :: code           !< Data variable encoded, ascii or Base64 codec.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  data_type = 'Float64'
  n_components = size(x, dim=1)
  if (present(one_component)) then
    if (one_component) n_components = 1
  endif
  select case(self%format)
  case(ascii)
    code = self%encode_ascii_dataarray(x=x)
    call self%write_dataarray_tag(data_type=data_type, number_of_components=n_components, data_name=data_name, data_content=code, &
                                  is_tuples=is_tuples)
  case(raw, bin_app)
    call self%write_dataarray_tag_appended(data_type=data_type, number_of_components=n_components, data_name=data_name, &
                                           is_tuples=is_tuples)
    call self%ioffset_update(n_byte=self%write_on_scratch_dataarray(x=x))
  case(binary)
    code = self%encode_base64_dataarray(x=x)
    call self%write_dataarray_tag(data_type=data_type, number_of_components=n_components, data_name=data_name, data_content=code, &
                                  is_tuples=is_tuples)
  endselect
  error = self%error
  return
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction write_dataarray1_rank4_R8P

  function write_dataarray1_rank4_R4P(self, data_name, x, one_component, is_tuples) result(error)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Write `<DataArray... NumberOfComponents="n"...>...</DataArray>` tag (R4P).
  !---------------------------------------------------------------------------------------------------------------------------------
  class(vtk_file), intent(inout)        :: self           !< VTK file.
  character(*),    intent(in)           :: data_name      !< Data name.
  real(R4P),       intent(in)           :: x(1:,1:,1:,1:) !< Data variable.
  logical,         intent(in), optional :: one_component  !< Force one component instead of inferring from first rank-size.
  logical,         intent(in), optional :: is_tuples      !< Use "NumberOfTuples" instead of "NumberOfComponents" attribute.
  integer(I4P)                          :: error          !< Error status.
  character(len=:), allocatable         :: data_type      !< Data type.
  integer(I4P)                          :: n_components   !< Number of components.
  character(len=:), allocatable         :: code           !< Data variable encoded, ascii or Base64 codec.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  data_type = 'Float32'
  n_components = size(x, dim=1)
  if (present(one_component)) then
    if (one_component) n_components = 1
  endif
  select case(self%format)
  case(ascii)
    code = self%encode_ascii_dataarray(x=x)
    call self%write_dataarray_tag(data_type=data_type, number_of_components=n_components, data_name=data_name, data_content=code, &
                                  is_tuples=is_tuples)
  case(raw, bin_app)
    call self%write_dataarray_tag_appended(data_type=data_type, number_of_components=n_components, data_name=data_name, &
                                           is_tuples=is_tuples)
    call self%ioffset_update(n_byte=self%write_on_scratch_dataarray(x=x))
  case(binary)
    code = self%encode_base64_dataarray(x=x)
    call self%write_dataarray_tag(data_type=data_type, number_of_components=n_components, data_name=data_name, data_content=code, &
                                  is_tuples=is_tuples)
  endselect
  error = self%error
  return
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction write_dataarray1_rank4_R4P

  function write_dataarray1_rank4_I8P(self, data_name, x, one_component, is_tuples) result(error)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Write `<DataArray... NumberOfComponents="n"...>...</DataArray>` tag (I8P).
  !---------------------------------------------------------------------------------------------------------------------------------
  class(vtk_file), intent(inout)        :: self           !< VTK file.
  character(*),    intent(in)           :: data_name      !< Data name.
  integer(I8P),    intent(in)           :: x(1:,1:,1:,1:) !< Data variable.
  logical,         intent(in), optional :: one_component  !< Force one component instead of inferring from first rank-size.
  logical,         intent(in), optional :: is_tuples      !< Use "NumberOfTuples" instead of "NumberOfComponents" attribute.
  integer(I4P)                          :: error          !< Error status.
  character(len=:), allocatable         :: data_type      !< Data type.
  integer(I4P)                          :: n_components   !< Number of components.
  character(len=:), allocatable         :: code           !< Data variable encoded, ascii or Base64 codec.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  data_type = 'Int64'
  n_components = size(x, dim=1)
  if (present(one_component)) then
    if (one_component) n_components = 1
  endif
  select case(self%format)
  case(ascii)
    code = self%encode_ascii_dataarray(x=x)
    call self%write_dataarray_tag(data_type=data_type, number_of_components=n_components, data_name=data_name, data_content=code, &
                                  is_tuples=is_tuples)
  case(raw, bin_app)
    call self%write_dataarray_tag_appended(data_type=data_type, number_of_components=n_components, data_name=data_name, &
                                           is_tuples=is_tuples)
    call self%ioffset_update(n_byte=self%write_on_scratch_dataarray(x=x))
  case(binary)
    code = self%encode_base64_dataarray(x=x)
    call self%write_dataarray_tag(data_type=data_type, number_of_components=n_components, data_name=data_name, data_content=code, &
                                  is_tuples=is_tuples)
  endselect
  error = self%error
  return
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction write_dataarray1_rank4_I8P

  function write_dataarray1_rank4_I4P(self, data_name, x, one_component, is_tuples) result(error)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Write `<DataArray... NumberOfComponents="n"...>...</DataArray>` tag (I4P).
  !---------------------------------------------------------------------------------------------------------------------------------
  class(vtk_file), intent(inout)        :: self           !< VTK file.
  character(*),    intent(in)           :: data_name      !< Data name.
  integer(I4P),    intent(in)           :: x(1:,1:,1:,1:) !< Data variable.
  logical,         intent(in), optional :: one_component  !< Force one component instead of inferring from first rank-size.
  logical,         intent(in), optional :: is_tuples      !< Use "NumberOfTuples" instead of "NumberOfComponents" attribute.
  integer(I4P)                          :: error          !< Error status.
  character(len=:), allocatable         :: data_type      !< Data type.
  integer(I4P)                          :: n_components   !< Number of components.
  character(len=:), allocatable         :: code           !< Data variable encoded, ascii or Base64 codec.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  data_type = 'Int32'
  n_components = size(x, dim=1)
  if (present(one_component)) then
    if (one_component) n_components = 1
  endif
  select case(self%format)
  case(ascii)
    code = self%encode_ascii_dataarray(x=x)
    call self%write_dataarray_tag(data_type=data_type, number_of_components=n_components, data_name=data_name, data_content=code, &
                                  is_tuples=is_tuples)
  case(raw, bin_app)
    call self%write_dataarray_tag_appended(data_type=data_type, number_of_components=n_components, data_name=data_name, &
                                           is_tuples=is_tuples)
    call self%ioffset_update(n_byte=self%write_on_scratch_dataarray(x=x))
  case(binary)
    code = self%encode_base64_dataarray(x=x)
    call self%write_dataarray_tag(data_type=data_type, number_of_components=n_components, data_name=data_name, data_content=code, &
                                  is_tuples=is_tuples)
  endselect
  error = self%error
  return
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction write_dataarray1_rank4_I4P

  function write_dataarray1_rank4_I2P(self, data_name, x, one_component, is_tuples) result(error)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Write `<DataArray... NumberOfComponents="n"...>...</DataArray>` tag (I2P).
  !---------------------------------------------------------------------------------------------------------------------------------
  class(vtk_file), intent(inout)        :: self           !< VTK file.
  character(*),    intent(in)           :: data_name      !< Data name.
  integer(I2P),    intent(in)           :: x(1:,1:,1:,1:) !< Data variable.
  logical,         intent(in), optional :: one_component  !< Force one component instead of inferring from first rank-size.
  logical,         intent(in), optional :: is_tuples      !< Use "NumberOfTuples" instead of "NumberOfComponents" attribute.
  integer(I4P)                          :: error          !< Error status.
  character(len=:), allocatable         :: data_type      !< Data type.
  integer(I4P)                          :: n_components   !< Number of components.
  character(len=:), allocatable         :: code           !< Data variable encoded, ascii or Base64 codec.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  data_type = 'Int16'
  n_components = size(x, dim=1)
  if (present(one_component)) then
    if (one_component) n_components = 1
  endif
  select case(self%format)
  case(ascii)
    code = self%encode_ascii_dataarray(x=x)
    call self%write_dataarray_tag(data_type=data_type, number_of_components=n_components, data_name=data_name, data_content=code, &
                                  is_tuples=is_tuples)
  case(raw, bin_app)
    call self%write_dataarray_tag_appended(data_type=data_type, number_of_components=n_components, data_name=data_name, &
                                           is_tuples=is_tuples)
    call self%ioffset_update(n_byte=self%write_on_scratch_dataarray(x=x))
  case(binary)
    code = self%encode_base64_dataarray(x=x)
    call self%write_dataarray_tag(data_type=data_type, number_of_components=n_components, data_name=data_name, data_content=code, &
                                  is_tuples=is_tuples)
  endselect
  error = self%error
  return
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction write_dataarray1_rank4_I2P

  function write_dataarray1_rank4_I1P(self, data_name, x, one_component, is_tuples) result(error)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Write `<DataArray... NumberOfComponents="n"...>...</DataArray>` tag (I1P).
  !---------------------------------------------------------------------------------------------------------------------------------
  class(vtk_file), intent(inout)        :: self           !< VTK file.
  character(*),    intent(in)           :: data_name      !< Data name.
  integer(I1P),    intent(in)           :: x(1:,1:,1:,1:) !< Data variable.
  logical,         intent(in), optional :: one_component  !< Force one component instead of inferring from first rank-size.
  logical,         intent(in), optional :: is_tuples      !< Use "NumberOfTuples" instead of "NumberOfComponents" attribute.
  integer(I4P)                          :: error          !< Error status.
  character(len=:), allocatable         :: data_type      !< Data type.
  integer(I4P)                          :: n_components   !< Number of components.
  character(len=:), allocatable         :: code           !< Data variable encoded, ascii or Base64 codec.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  data_type = 'Int8'
  n_components = size(x, dim=1)
  if (present(one_component)) then
    if (one_component) n_components = 1
  endif
  select case(self%format)
  case(ascii)
    code = self%encode_ascii_dataarray(x=x)
    call self%write_dataarray_tag(data_type=data_type, number_of_components=n_components, data_name=data_name, data_content=code, &
                                  is_tuples=is_tuples)
  case(raw, bin_app)
    call self%write_dataarray_tag_appended(data_type=data_type, number_of_components=n_components, data_name=data_name, &
                                           is_tuples=is_tuples)
    call self%ioffset_update(n_byte=self%write_on_scratch_dataarray(x=x))
  case(binary)
    code = self%encode_base64_dataarray(x=x)
    call self%write_dataarray_tag(data_type=data_type, number_of_components=n_components, data_name=data_name, data_content=code, &
                                  is_tuples=is_tuples)
  endselect
  error = self%error
  return
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction write_dataarray1_rank4_I1P

  function write_dataarray3_rank1_R8P(self, data_name, x, y, z, is_tuples) result(error)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Write `<DataArray... NumberOfComponents="3"...>...</DataArray>` tag (R8P).
  !---------------------------------------------------------------------------------------------------------------------------------
  class(vtk_file), intent(inout)        :: self         !< VTK file.
  character(*),    intent(in)           :: data_name    !< Data name.
  real(R8P),       intent(in)           :: x(1:)        !< X component of data variable.
  real(R8P),       intent(in)           :: y(1:)        !< Y component of data variable.
  real(R8P),       intent(in)           :: z(1:)        !< Z component of data variable.
  logical,         intent(in), optional :: is_tuples    !< Use "NumberOfTuples" instead of "NumberOfComponents" attribute.
  integer(I4P)                          :: error        !< Error status.
  character(len=:), allocatable         :: data_type    !< Data type.
  integer(I4P)                          :: n_components !< Number of components.
  character(len=:), allocatable         :: code         !< Data variable encoded, ascii or Base64 codec.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  data_type = 'Float64'
  n_components = 3
  select case(self%format)
  case(ascii)
    code = self%encode_ascii_dataarray(x=x, y=y, z=z)
    call self%write_dataarray_tag(data_type=data_type, number_of_components=n_components, data_name=data_name, data_content=code, &
                                  is_tuples=is_tuples)
  case(raw, bin_app)
    call self%write_dataarray_tag_appended(data_type=data_type, number_of_components=n_components, data_name=data_name, &
                                           is_tuples=is_tuples)
    call self%ioffset_update(n_byte=self%write_on_scratch_dataarray(x=x, y=y, z=z))
  case(binary)
    code = self%encode_base64_dataarray(x=x, y=y, z=z)
    call self%write_dataarray_tag(data_type=data_type, number_of_components=n_components, data_name=data_name, data_content=code, &
                                  is_tuples=is_tuples)
  endselect
  error = self%error
  return
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction write_dataarray3_rank1_R8P

  function write_dataarray3_rank1_R4P(self, data_name, x, y, z, is_tuples) result(error)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Write `<DataArray... NumberOfComponents="3"...>...</DataArray>` tag (R4P).
  !---------------------------------------------------------------------------------------------------------------------------------
  class(vtk_file), intent(inout)        :: self         !< VTK file.
  character(*),    intent(in)           :: data_name    !< Data name.
  real(R4P),       intent(in)           :: x(1:)        !< X component of data variable.
  real(R4P),       intent(in)           :: y(1:)        !< Y component of data variable.
  real(R4P),       intent(in)           :: z(1:)        !< Z component of data variable.
  logical,         intent(in), optional :: is_tuples    !< Use "NumberOfTuples" instead of "NumberOfComponents" attribute.
  integer(I4P)                          :: error        !< Error status.
  character(len=:), allocatable         :: data_type    !< Data type.
  integer(I4P)                          :: n_components !< Number of components.
  character(len=:), allocatable         :: code         !< Data variable encoded, ascii or Base64 codec.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  data_type = 'Float32'
  n_components = 3
  select case(self%format)
  case(ascii)
    code = self%encode_ascii_dataarray(x=x, y=y, z=z)
    call self%write_dataarray_tag(data_type=data_type, number_of_components=n_components, data_name=data_name, data_content=code, &
                                  is_tuples=is_tuples)
  case(raw, bin_app)
    call self%write_dataarray_tag_appended(data_type=data_type, number_of_components=n_components, data_name=data_name, &
                                           is_tuples=is_tuples)
    call self%ioffset_update(n_byte=self%write_on_scratch_dataarray(x=x, y=y, z=z))
  case(binary)
    code = self%encode_base64_dataarray(x=x, y=y, z=z)
    call self%write_dataarray_tag(data_type=data_type, number_of_components=n_components, data_name=data_name, data_content=code, &
                                  is_tuples=is_tuples)
  endselect
  error = self%error
  return
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction write_dataarray3_rank1_R4P

  function write_dataarray3_rank1_I8P(self, data_name, x, y, z, is_tuples) result(error)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Write `<DataArray... NumberOfComponents="3"...>...</DataArray>` tag (I8P).
  !---------------------------------------------------------------------------------------------------------------------------------
  class(vtk_file), intent(inout)        :: self         !< VTK file.
  character(*),    intent(in)           :: data_name    !< Data name.
  integer(I8P),    intent(in)           :: x(1:)        !< X component of data variable.
  integer(I8P),    intent(in)           :: y(1:)        !< Y component of data variable.
  integer(I8P),    intent(in)           :: z(1:)        !< Z component of data variable.
  logical,         intent(in), optional :: is_tuples    !< Use "NumberOfTuples" instead of "NumberOfComponents" attribute.
  integer(I4P)                          :: error        !< Error status.
  character(len=:), allocatable         :: data_type    !< Data type.
  integer(I4P)                          :: n_components !< Number of components.
  character(len=:), allocatable         :: code         !< Data variable encoded, ascii or Base64 codec.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  data_type = 'Int64'
  n_components = 3
  select case(self%format)
  case(ascii)
    code = self%encode_ascii_dataarray(x=x, y=y, z=z)
    call self%write_dataarray_tag(data_type=data_type, number_of_components=n_components, data_name=data_name, data_content=code, &
                                  is_tuples=is_tuples)
  case(raw, bin_app)
    call self%write_dataarray_tag_appended(data_type=data_type, number_of_components=n_components, data_name=data_name, &
                                           is_tuples=is_tuples)
    call self%ioffset_update(n_byte=self%write_on_scratch_dataarray(x=x, y=y, z=z))
  case(binary)
    code = self%encode_base64_dataarray(x=x, y=y, z=z)
    call self%write_dataarray_tag(data_type=data_type, number_of_components=n_components, data_name=data_name, data_content=code, &
                                  is_tuples=is_tuples)
  endselect
  error = self%error
  return
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction write_dataarray3_rank1_I8P

  function write_dataarray3_rank1_I4P(self, data_name, x, y, z, is_tuples) result(error)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Write `<DataArray... NumberOfComponents="3"...>...</DataArray>` tag (I4P).
  !---------------------------------------------------------------------------------------------------------------------------------
  class(vtk_file), intent(inout)        :: self         !< VTK file.
  character(*),    intent(in)           :: data_name    !< Data name.
  integer(I4P),    intent(in)           :: x(1:)        !< X component of data variable.
  integer(I4P),    intent(in)           :: y(1:)        !< Y component of data variable.
  integer(I4P),    intent(in)           :: z(1:)        !< Z component of data variable.
  logical,         intent(in), optional :: is_tuples    !< Use "NumberOfTuples" instead of "NumberOfComponents" attribute.
  integer(I4P)                          :: error        !< Error status.
  character(len=:), allocatable         :: data_type    !< Data type.
  integer(I4P)                          :: n_components !< Number of components.
  character(len=:), allocatable         :: code         !< Data variable encoded, ascii or Base64 codec.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  data_type = 'Int32'
  n_components = 3
  select case(self%format)
  case(ascii)
    code = self%encode_ascii_dataarray(x=x, y=y, z=z)
    call self%write_dataarray_tag(data_type=data_type, number_of_components=n_components, data_name=data_name, data_content=code, &
                                  is_tuples=is_tuples)
  case(raw, bin_app)
    call self%write_dataarray_tag_appended(data_type=data_type, number_of_components=n_components, data_name=data_name, &
                                           is_tuples=is_tuples)
    call self%ioffset_update(n_byte=self%write_on_scratch_dataarray(x=x, y=y, z=z))
  case(binary)
    code = self%encode_base64_dataarray(x=x, y=y, z=z)
    call self%write_dataarray_tag(data_type=data_type, number_of_components=n_components, data_name=data_name, data_content=code, &
                                  is_tuples=is_tuples)
  endselect
  error = self%error
  return
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction write_dataarray3_rank1_I4P

  function write_dataarray3_rank1_I2P(self, data_name, x, y, z, is_tuples) result(error)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Write `<DataArray... NumberOfComponents="3"...>...</DataArray>` tag (I2P).
  !---------------------------------------------------------------------------------------------------------------------------------
  class(vtk_file), intent(inout)        :: self         !< VTK file.
  character(*),    intent(in)           :: data_name    !< Data name.
  integer(I2P),    intent(in)           :: x(1:)        !< X component of data variable.
  integer(I2P),    intent(in)           :: y(1:)        !< Y component of data variable.
  integer(I2P),    intent(in)           :: z(1:)        !< Z component of data variable.
  logical,         intent(in), optional :: is_tuples    !< Use "NumberOfTuples" instead of "NumberOfComponents" attribute.
  integer(I4P)                          :: error        !< Error status.
  character(len=:), allocatable         :: data_type    !< Data type.
  integer(I4P)                          :: n_components !< Number of components.
  character(len=:), allocatable         :: code         !< Data variable encoded, ascii or Base64 codec.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  data_type = 'Int16'
  n_components = 3
  select case(self%format)
  case(ascii)
    code = self%encode_ascii_dataarray(x=x, y=y, z=z)
    call self%write_dataarray_tag(data_type=data_type, number_of_components=n_components, data_name=data_name, data_content=code, &
                                  is_tuples=is_tuples)
  case(raw, bin_app)
    call self%write_dataarray_tag_appended(data_type=data_type, number_of_components=n_components, data_name=data_name, &
                                           is_tuples=is_tuples)
    call self%ioffset_update(n_byte=self%write_on_scratch_dataarray(x=x, y=y, z=z))
  case(binary)
    code = self%encode_base64_dataarray(x=x, y=y, z=z)
    call self%write_dataarray_tag(data_type=data_type, number_of_components=n_components, data_name=data_name, data_content=code, &
                                  is_tuples=is_tuples)
  endselect
  error = self%error
  return
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction write_dataarray3_rank1_I2P

  function write_dataarray3_rank1_I1P(self, data_name, x, y, z, is_tuples) result(error)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Write `<DataArray... NumberOfComponents="3"...>...</DataArray>` tag (I1P).
  !---------------------------------------------------------------------------------------------------------------------------------
  class(vtk_file), intent(inout)        :: self         !< VTK file.
  character(*),    intent(in)           :: data_name    !< Data name.
  integer(I1P),    intent(in)           :: x(1:)        !< X component of data variable.
  integer(I1P),    intent(in)           :: y(1:)        !< Y component of data variable.
  integer(I1P),    intent(in)           :: z(1:)        !< Z component of data variable.
  logical,         intent(in), optional :: is_tuples    !< Use "NumberOfTuples" instead of "NumberOfComponents" attribute.
  integer(I4P)                          :: error        !< Error status.
  character(len=:), allocatable         :: data_type    !< Data type.
  integer(I4P)                          :: n_components !< Number of components.
  character(len=:), allocatable         :: code         !< Data variable encoded, ascii or Base64 codec.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  data_type = 'Int8'
  n_components = 3
  select case(self%format)
  case(ascii)
    code = self%encode_ascii_dataarray(x=x, y=y, z=z)
    call self%write_dataarray_tag(data_type=data_type, number_of_components=n_components, data_name=data_name, data_content=code, &
                                  is_tuples=is_tuples)
  case(raw, bin_app)
    call self%write_dataarray_tag_appended(data_type=data_type, number_of_components=n_components, data_name=data_name, &
                                           is_tuples=is_tuples)
    call self%ioffset_update(n_byte=self%write_on_scratch_dataarray(x=x, y=y, z=z))
  case(binary)
    code = self%encode_base64_dataarray(x=x, y=y, z=z)
    call self%write_dataarray_tag(data_type=data_type, number_of_components=n_components, data_name=data_name, data_content=code, &
                                  is_tuples=is_tuples)
  endselect
  error = self%error
  return
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction write_dataarray3_rank1_I1P

  function write_dataarray3_rank3_R8P(self, data_name, x, y, z, is_tuples) result(error)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Write `<DataArray... NumberOfComponents="3"...>...</DataArray>` tag (R8P).
  !---------------------------------------------------------------------------------------------------------------------------------
  class(vtk_file), intent(inout)        :: self         !< VTK file.
  character(*),    intent(in)           :: data_name    !< Data name.
  real(R8P),       intent(in)           :: x(1:,1:,1:)  !< X component of data variable.
  real(R8P),       intent(in)           :: y(1:,1:,1:)  !< Y component of data variable.
  real(R8P),       intent(in)           :: z(1:,1:,1:)  !< Z component of data variable.
  logical,         intent(in), optional :: is_tuples    !< Use "NumberOfTuples" instead of "NumberOfComponents" attribute.
  integer(I4P)                          :: error        !< Error status.
  character(len=:), allocatable         :: data_type    !< Data type.
  integer(I4P)                          :: n_components !< Number of components.
  character(len=:), allocatable         :: code         !< Data variable encoded, ascii or Base64 codec.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  data_type = 'Float64'
  n_components = 3
  select case(self%format)
  case(ascii)
    code = self%encode_ascii_dataarray(x=x, y=y, z=z)
    call self%write_dataarray_tag(data_type=data_type, number_of_components=n_components, data_name=data_name, data_content=code, &
                                  is_tuples=is_tuples)
  case(raw, bin_app)
    call self%write_dataarray_tag_appended(data_type=data_type, number_of_components=n_components, data_name=data_name, &
                                           is_tuples=is_tuples)
    call self%ioffset_update(n_byte=self%write_on_scratch_dataarray(x=x, y=y, z=z))
  case(binary)
    code = self%encode_base64_dataarray(x=x, y=y, z=z)
    call self%write_dataarray_tag(data_type=data_type, number_of_components=n_components, data_name=data_name, data_content=code, &
                                  is_tuples=is_tuples)
  endselect
  error = self%error
  return
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction write_dataarray3_rank3_R8P

  function write_dataarray3_rank3_R4P(self, data_name, x, y, z, is_tuples) result(error)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Write `<DataArray... NumberOfComponents="3"...>...</DataArray>` tag (R4P).
  !---------------------------------------------------------------------------------------------------------------------------------
  class(vtk_file), intent(inout)        :: self         !< VTK file.
  character(*),    intent(in)           :: data_name    !< Data name.
  real(R4P),       intent(in)           :: x(1:,1:,1:)  !< X component of data variable.
  real(R4P),       intent(in)           :: y(1:,1:,1:)  !< Y component of data variable.
  real(R4P),       intent(in)           :: z(1:,1:,1:)  !< Z component of data variable.
  logical,         intent(in), optional :: is_tuples    !< Use "NumberOfTuples" instead of "NumberOfComponents" attribute.
  integer(I4P)                          :: error        !< Error status.
  character(len=:), allocatable         :: data_type    !< Data type.
  integer(I4P)                          :: n_components !< Number of components.
  character(len=:), allocatable         :: code         !< Data variable encoded, ascii or Base64 codec.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  data_type = 'Float32'
  n_components = 3
  select case(self%format)
  case(ascii)
    code = self%encode_ascii_dataarray(x=x, y=y, z=z)
    call self%write_dataarray_tag(data_type=data_type, number_of_components=n_components, data_name=data_name, data_content=code, &
                                  is_tuples=is_tuples)
  case(raw, bin_app)
    call self%write_dataarray_tag_appended(data_type=data_type, number_of_components=n_components, data_name=data_name, &
                                           is_tuples=is_tuples)
    call self%ioffset_update(n_byte=self%write_on_scratch_dataarray(x=x, y=y, z=z))
  case(binary)
    code = self%encode_base64_dataarray(x=x, y=y, z=z)
    call self%write_dataarray_tag(data_type=data_type, number_of_components=n_components, data_name=data_name, data_content=code, &
                                  is_tuples=is_tuples)
  endselect
  error = self%error
  return
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction write_dataarray3_rank3_R4P

  function write_dataarray3_rank3_I8P(self, data_name, x, y, z, is_tuples) result(error)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Write `<DataArray... NumberOfComponents="3"...>...</DataArray>` tag (I8P).
  !---------------------------------------------------------------------------------------------------------------------------------
  class(vtk_file), intent(inout)        :: self         !< VTK file.
  character(*),    intent(in)           :: data_name    !< Data name.
  integer(I8P),    intent(in)           :: x(1:,1:,1:)  !< X component of data variable.
  integer(I8P),    intent(in)           :: y(1:,1:,1:)  !< Y component of data variable.
  integer(I8P),    intent(in)           :: z(1:,1:,1:)  !< Z component of data variable.
  logical,         intent(in), optional :: is_tuples    !< Use "NumberOfTuples" instead of "NumberOfComponents" attribute.
  integer(I4P)                          :: error        !< Error status.
  character(len=:), allocatable         :: data_type    !< Data type.
  integer(I4P)                          :: n_components !< Number of components.
  character(len=:), allocatable         :: code         !< Data variable encoded, ascii or Base64 codec.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  data_type = 'Int64'
  n_components = 3
  select case(self%format)
  case(ascii)
    code = self%encode_ascii_dataarray(x=x, y=y, z=z)
    call self%write_dataarray_tag(data_type=data_type, number_of_components=n_components, data_name=data_name, data_content=code, &
                                  is_tuples=is_tuples)
  case(raw, bin_app)
    call self%write_dataarray_tag_appended(data_type=data_type, number_of_components=n_components, data_name=data_name, &
                                           is_tuples=is_tuples)
    call self%ioffset_update(n_byte=self%write_on_scratch_dataarray(x=x, y=y, z=z))
  case(binary)
    code = self%encode_base64_dataarray(x=x, y=y, z=z)
    call self%write_dataarray_tag(data_type=data_type, number_of_components=n_components, data_name=data_name, data_content=code, &
                                  is_tuples=is_tuples)
  endselect
  error = self%error
  return
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction write_dataarray3_rank3_I8P

  function write_dataarray3_rank3_I4P(self, data_name, x, y, z, is_tuples) result(error)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Write `<DataArray... NumberOfComponents="3"...>...</DataArray>` tag (I4P).
  !---------------------------------------------------------------------------------------------------------------------------------
  class(vtk_file), intent(inout)        :: self         !< VTK file.
  character(*),    intent(in)           :: data_name    !< Data name.
  integer(I4P),    intent(in)           :: x(1:,1:,1:)  !< X component of data variable.
  integer(I4P),    intent(in)           :: y(1:,1:,1:)  !< Y component of data variable.
  integer(I4P),    intent(in)           :: z(1:,1:,1:)  !< Z component of data variable.
  logical,         intent(in), optional :: is_tuples    !< Use "NumberOfTuples" instead of "NumberOfComponents" attribute.
  integer(I4P)                          :: error        !< Error status.
  character(len=:), allocatable         :: data_type    !< Data type.
  integer(I4P)                          :: n_components !< Number of components.
  character(len=:), allocatable         :: code         !< Data variable encoded, ascii or Base64 codec.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  data_type = 'Int32'
  n_components = 3
  select case(self%format)
  case(ascii)
    code = self%encode_ascii_dataarray(x=x, y=y, z=z)
    call self%write_dataarray_tag(data_type=data_type, number_of_components=n_components, data_name=data_name, data_content=code, &
                                  is_tuples=is_tuples)
  case(raw, bin_app)
    call self%write_dataarray_tag_appended(data_type=data_type, number_of_components=n_components, data_name=data_name, &
                                           is_tuples=is_tuples)
    call self%ioffset_update(n_byte=self%write_on_scratch_dataarray(x=x, y=y, z=z))
  case(binary)
    code = self%encode_base64_dataarray(x=x, y=y, z=z)
    call self%write_dataarray_tag(data_type=data_type, number_of_components=n_components, data_name=data_name, data_content=code, &
                                  is_tuples=is_tuples)
  endselect
  error = self%error
  return
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction write_dataarray3_rank3_I4P

  function write_dataarray3_rank3_I2P(self, data_name, x, y, z, is_tuples) result(error)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Write `<DataArray... NumberOfComponents="3"...>...</DataArray>` tag (I2P).
  !---------------------------------------------------------------------------------------------------------------------------------
  class(vtk_file), intent(inout)        :: self         !< VTK file.
  character(*),    intent(in)           :: data_name    !< Data name.
  integer(I2P),    intent(in)           :: x(1:,1:,1:)  !< X component of data variable.
  integer(I2P),    intent(in)           :: y(1:,1:,1:)  !< Y component of data variable.
  integer(I2P),    intent(in)           :: z(1:,1:,1:)  !< Z component of data variable.
  logical,         intent(in), optional :: is_tuples    !< Use "NumberOfTuples" instead of "NumberOfComponents" attribute.
  integer(I4P)                          :: error        !< Error status.
  character(len=:), allocatable         :: data_type    !< Data type.
  integer(I4P)                          :: n_components !< Number of components.
  character(len=:), allocatable         :: code         !< Data variable encoded, ascii or Base64 codec.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  data_type = 'Int16'
  n_components = 3
  select case(self%format)
  case(ascii)
    code = self%encode_ascii_dataarray(x=x, y=y, z=z)
    call self%write_dataarray_tag(data_type=data_type, number_of_components=n_components, data_name=data_name, data_content=code, &
                                  is_tuples=is_tuples)
  case(raw, bin_app)
    call self%write_dataarray_tag_appended(data_type=data_type, number_of_components=n_components, data_name=data_name, &
                                           is_tuples=is_tuples)
    call self%ioffset_update(n_byte=self%write_on_scratch_dataarray(x=x, y=y, z=z))
  case(binary)
    code = self%encode_base64_dataarray(x=x, y=y, z=z)
    call self%write_dataarray_tag(data_type=data_type, number_of_components=n_components, data_name=data_name, data_content=code, &
                                  is_tuples=is_tuples)
  endselect
  error = self%error
  return
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction write_dataarray3_rank3_I2P

  function write_dataarray3_rank3_I1P(self, data_name, x, y, z, is_tuples) result(error)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Write `<DataArray... NumberOfComponents="3"...>...</DataArray>` tag (I1P).
  !---------------------------------------------------------------------------------------------------------------------------------
  class(vtk_file), intent(inout)        :: self         !< VTK file.
  character(*),    intent(in)           :: data_name    !< Data name.
  integer(I1P),    intent(in)           :: x(1:,1:,1:)  !< X component of data variable.
  integer(I1P),    intent(in)           :: y(1:,1:,1:)  !< Y component of data variable.
  integer(I1P),    intent(in)           :: z(1:,1:,1:)  !< Z component of data variable.
  logical,         intent(in), optional :: is_tuples    !< Use "NumberOfTuples" instead of "NumberOfComponents" attribute.
  integer(I4P)                          :: error        !< Error status.
  character(len=:), allocatable         :: data_type    !< Data type.
  integer(I4P)                          :: n_components !< Number of components.
  character(len=:), allocatable         :: code         !< Data variable encoded, ascii or Base64 codec.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  data_type = 'Int8'
  n_components = 3
  select case(self%format)
  case(ascii)
    code = self%encode_ascii_dataarray(x=x, y=y, z=z)
    call self%write_dataarray_tag(data_type=data_type, number_of_components=n_components, data_name=data_name, data_content=code, &
                                  is_tuples=is_tuples)
  case(raw, bin_app)
    call self%write_dataarray_tag_appended(data_type=data_type, number_of_components=n_components, data_name=data_name, &
                                           is_tuples=is_tuples)
    call self%ioffset_update(n_byte=self%write_on_scratch_dataarray(x=x, y=y, z=z))
  case(binary)
    code = self%encode_base64_dataarray(x=x, y=y, z=z)
    call self%write_dataarray_tag(data_type=data_type, number_of_components=n_components, data_name=data_name, data_content=code, &
                                  is_tuples=is_tuples)
  endselect
  error = self%error
  return
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction write_dataarray3_rank3_I1P

  subroutine write_dataarray_tag(self, data_type, number_of_components, data_name, data_content, is_tuples)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Write `<DataArray...>...</DataArray>` tag.
  !---------------------------------------------------------------------------------------------------------------------------------
  class(vtk_file), intent(inout)        :: self                 !< VTK file.
  character(*),    intent(in)           :: data_type            !< Type of dataarray.
  integer(I4P),    intent(in)           :: number_of_components !< Number of dataarray components.
  character(*),    intent(in)           :: data_name            !< Data name.
  character(*),    intent(in), optional :: data_content         !< Data content.
  logical,         intent(in), optional :: is_tuples            !< Use "NumberOfTuples" instead of "NumberOfComponents" attribute.
  type(string)                          :: tag_attributes       !< Tag attributes.
  logical                               :: is_tuples_           !< Use "NumberOfTuples" instead of "NumberOfComponents" attribute.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  is_tuples_ = .false.
  if (present(is_tuples)) is_tuples_ = is_tuples
  if (is_tuples_) then
    tag_attributes = 'type="'//trim(adjustl(data_type))//             &
      '" NumberOfTuples="'//trim(str(number_of_components, .true.))// &
      '" Name="'//trim(adjustl(data_name))//                          &
      '" format="'//self%format_ch//'"'
  else
    tag_attributes = 'type="'//trim(adjustl(data_type))//                 &
      '" NumberOfComponents="'//trim(str(number_of_components, .true.))// &
      '" Name="'//trim(adjustl(data_name))//                              &
      '" format="'//self%format_ch//'"'
  endif
  call self%write_tag(tag_name='DataArray', tag_attributes=tag_attributes%chars(), tag_content=data_content)
  return
  !---------------------------------------------------------------------------------------------------------------------------------
  endsubroutine write_dataarray_tag  

  subroutine write_dataarray_tag_appended(self, data_type, number_of_components, data_name, is_tuples)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Write `<DataArray.../>` tag.
  !---------------------------------------------------------------------------------------------------------------------------------
  class(vtk_file), intent(inout)        :: self                 !< VTK file.
  character(*),    intent(in)           :: data_type            !< Type of dataarray.
  integer(I4P),    intent(in)           :: number_of_components !< Number of dataarray components.
  character(*),    intent(in)           :: data_name            !< Data name.
  logical,         intent(in), optional :: is_tuples            !< Use "NumberOfTuples" instead of "NumberOfComponents" attribute.
  type(string)                          :: tag_attributes       !< Tag attributes.
  logical                               :: is_tuples_           !< Use "NumberOfTuples" instead of "NumberOfComponents" attribute.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  is_tuples_ = .false.
  if (present(is_tuples)) is_tuples_ = is_tuples
  if (is_tuples_) then
    tag_attributes =  'type="'//trim(adjustl(data_type))//            &
      '" NumberOfTuples="'//trim(str(number_of_components, .true.))// &
      '" Name="'//trim(adjustl(data_name))//                          &
      '" format="'//self%format_ch//                                  &
      '" offset="'//trim(str(self%ioffset, .true.))//'"'
  else
    tag_attributes = 'type="'//trim(adjustl(data_type))//                 &
      '" NumberOfComponents="'//trim(str(number_of_components, .true.))// &
      '" Name="'//trim(adjustl(data_name))//                              &
      '" format="'//self%format_ch//                                      &
      '" offset="'//trim(str(self%ioffset, .true.))//'"'
  endif  
  call self%write_self_closing_tag(tag_name='DataArray', tag_attributes=tag_attributes%chars())
  return
  !---------------------------------------------------------------------------------------------------------------------------------
  endsubroutine write_dataarray_tag_appended

  function write_dataarray_location_tag(self, location, action) result(error)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Write `<[/]PointData>` or `<[/]CellData>` open/close tag.
  !<
  !< @note **must** be called before saving the data related to geometric mesh, this function initializes the
  !< saving of data variables indicating the *location* (node or cell centered) of variables that will be saved.
  !<
  !< @note A single file can contain both cell and node centered variables. In this case the VTK_DAT_XML function must be
  !< called two times, before saving cell-centered variables and before saving node-centered variables.
  !<
  !<### Examples of usage
  !<
  !<#### Opening node piece
  !<```fortran
  !< error = vtk%write_data('node','OPeN')
  !<```
  !<
  !<#### Closing node piece
  !<```fortran
  !< error = vtk%write_data('node','Close')
  !<```
  !<
  !<#### Opening cell piece
  !<```fortran
  !< error = vtk%write_data('cell','OPEN')
  !<```
  !<
  !<#### Closing cell piece
  !<```fortran
  !< error = vtk%write_data('cell','close')
  !<```
  !---------------------------------------------------------------------------------------------------------------------------------
  class(vtk_file), intent(inout) :: self      !< VTK file.
  character(*),    intent(in)    :: location  !< Location of variables: **cell** or **node** centered.
  character(*),    intent(in)    :: action    !< Action: **open** or **close** tag.
  integer(I4P)                   :: error     !< Error status.
  type(string)                   :: location_ !< Location string.
  type(string)                   :: action_   !< Action string.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  location_ = trim(adjustl(location)) ; location_ = location_%upper()
  action_ = trim(adjustl(action)) ; action_ = action_%upper()
  select case(location_%chars())
  case('CELL')
    select case(action_%chars())
    case('OPEN')
      call self%write_start_tag(tag_name='CellData')
    case('CLOSE')
      call self%write_end_tag(tag_name='CellData')
    endselect
  case('NODE')
    select case(action_%chars())
    case('OPEN')
      call self%write_start_tag(tag_name='PointData')
    case('CLOSE')
      call self%write_end_tag(tag_name='PointData')
    endselect
  endselect
  error = self%error
  return
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction write_dataarray_location_tag

  subroutine write_dataarray_appended(self)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Write `<AppendedData...>...</AppendedData>` tag.
  !---------------------------------------------------------------------------------------------------------------------------------
  class(vtk_file), intent(inout) :: self              !< VTK file.
  type(string)                   :: tag_attributes    !< Tag attributes.
  integer(I4P)                   :: n_byte            !< Bytes count.
  character(len=2)               :: dataarray_type    !< Dataarray type = R8,R4,I8,I4,I2,I1.
  integer(I4P)                   :: dataarray_dim     !< Dataarray dimension.
  real(R8P),    allocatable      :: dataarray_R8P(:)  !< Dataarray buffer of R8P.
  real(R4P),    allocatable      :: dataarray_R4P(:)  !< Dataarray buffer of R4P.
  integer(I8P), allocatable      :: dataarray_I8P(:)  !< Dataarray buffer of I8P.
  integer(I4P), allocatable      :: dataarray_I4P(:)  !< Dataarray buffer of I4P.
  integer(I2P), allocatable      :: dataarray_I2P(:)  !< Dataarray buffer of I2P.
  integer(I1P), allocatable      :: dataarray_I1P(:)  !< Dataarray buffer of I1P.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  select case(self%format)
  case(raw)
    tag_attributes = 'encoding="raw"'
  case(bin_app)
    tag_attributes = 'encoding="base64"'
  endselect
  call self%write_start_tag(tag_name='AppendedData', tag_attributes=tag_attributes%chars())
  write(unit=self%xml, iostat=self%error)'_'
  endfile(unit=self%scratch, iostat=self%error)
  rewind(unit=self%scratch, iostat=self%error)
  do
    call read_dataarray_from_scratch
    if (self%error==0) call write_dataarray_on_xml
    if (is_iostat_end(self%error)) exit
  enddo
  close(unit=self%scratch, iostat=self%error)
  write(unit=self%xml, iostat=self%error)end_rec
  call self%write_end_tag(tag_name='AppendedData')
  return
  !---------------------------------------------------------------------------------------------------------------------------------
  contains
    subroutine read_dataarray_from_scratch
    !-------------------------------------------------------------------------------------------------------------------------------
    !< Read the current dataaray from scratch file.
    !-------------------------------------------------------------------------------------------------------------------------------

    !-------------------------------------------------------------------------------------------------------------------------------
    read(unit=self%scratch, iostat=self%error, end=10)n_byte, dataarray_type, dataarray_dim
    select case(dataarray_type)
    case('R8')
      if (allocated(dataarray_R8P)) deallocate(dataarray_R8P) ; allocate(dataarray_R8P(1:dataarray_dim))
      read(unit=self%scratch, iostat=self%error)dataarray_R8P
    case('R4')
      if (allocated(dataarray_R4P)) deallocate(dataarray_R4P) ; allocate(dataarray_R4P(1:dataarray_dim))
      read(unit=self%scratch, iostat=self%error)dataarray_R4P
    case('I8')
      if (allocated(dataarray_I8P)) deallocate(dataarray_I8P) ; allocate(dataarray_I8P(1:dataarray_dim))
      read(unit=self%scratch, iostat=self%error)dataarray_I8P
    case('I4')
      if (allocated(dataarray_I4P)) deallocate(dataarray_I4P) ; allocate(dataarray_I4P(1:dataarray_dim))
      read(unit=self%scratch, iostat=self%error)dataarray_I4P
    case('I2')
      if (allocated(dataarray_I2P)) deallocate(dataarray_I2P) ; allocate(dataarray_I2P(1:dataarray_dim))
      read(unit=self%scratch, iostat=self%error)dataarray_I2P
    case('I1')
      if (allocated(dataarray_I1P)) deallocate(dataarray_I1P) ; allocate(dataarray_I1P(1:dataarray_dim))
      read(unit=self%scratch, iostat=self%error)dataarray_I1P
    case default
      self%error = 1
      write (stderr,'(A)')' error: bad dataarray_type = '//dataarray_type
      write (stderr,'(A)')' bytes = '//trim(str(n=n_byte))
      write (stderr,'(A)')' dataarray dimension = '//trim(str(n=dataarray_dim))
    endselect
    10 return
    !-------------------------------------------------------------------------------------------------------------------------------
    endsubroutine read_dataarray_from_scratch

    subroutine write_dataarray_on_xml
    !-------------------------------------------------------------------------------------------------------------------------------
    !< Write the current dataaray on xml file.
    !-------------------------------------------------------------------------------------------------------------------------------
    character(len=:), allocatable  :: code !< Dataarray encoded with Base64 codec.
    !-------------------------------------------------------------------------------------------------------------------------------

    !-------------------------------------------------------------------------------------------------------------------------------
    if (self%format==raw) then
      select case(dataarray_type)
      case('R8')
        write(unit=self%xml, iostat=self%error)n_byte, dataarray_R8P
        deallocate(dataarray_R8P)
      case('R4')
        write(unit=self%xml, iostat=self%error)n_byte, dataarray_R4P
        deallocate(dataarray_R4P)
      case('I8')
        write(unit=self%xml, iostat=self%error)n_byte, dataarray_I8P
        deallocate(dataarray_I8P)
      case('I4')
        write(unit=self%xml, iostat=self%error)n_byte, dataarray_I4P
        deallocate(dataarray_I4P)
      case('I2')
        write(unit=self%xml, iostat=self%error)n_byte, dataarray_I2P
        deallocate(dataarray_I2P)
      case('I1')
        write(unit=self%xml, iostat=self%error)n_byte, dataarray_I1P
        deallocate(dataarray_I1P)
      endselect
    else
      select case(dataarray_type)
      case('R8')
        code = self%encode_base64_dataarray(x=dataarray_R8P)
        write(unit=self%xml, iostat=self%error)code
      case('R4')
        code = self%encode_base64_dataarray(x=dataarray_R4P)
        write(unit=self%xml, iostat=self%error)code
      case('I8')
        code = self%encode_base64_dataarray(x=dataarray_I8P)
        write(unit=self%xml, iostat=self%error)code
      case('I4')
        code = self%encode_base64_dataarray(x=dataarray_I4P)
        write(unit=self%xml, iostat=self%error)code
      case('I2')
        code = self%encode_base64_dataarray(x=dataarray_I2P)
        write(unit=self%xml, iostat=self%error)code
      case('I1')
        code = self%encode_base64_dataarray(x=dataarray_I1P)
        write(unit=self%xml, iostat=self%error)code
      endselect
    endif
    !-------------------------------------------------------------------------------------------------------------------------------
    endsubroutine write_dataarray_on_xml
  endsubroutine write_dataarray_appended
