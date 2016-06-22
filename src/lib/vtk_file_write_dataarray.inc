  !< Implementation of dataarray write of VTK file class.
  function write_dataarray1_rank1_R8P(self, data_name, x) result(error)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Write `<DataArray... NumberOfComponents="1"...>...</DataArray>` tag (R8P).
  !---------------------------------------------------------------------------------------------------------------------------------
  class(vtk_file), intent(inout) :: self         !< VTK file.
  character(*),    intent(in)    :: data_name    !< Data name.
  real(R8P),       intent(in)    :: x(1:)        !< Data variable.
  integer(I4P)                   :: error        !< Error status.
  character(len=:), allocatable  :: data_type    !< Data type.
  integer(I4P)                   :: n_components !< Number of components.
  character(len=:), allocatable  :: code         !< Data variable encoded, ascii or Base64 codec.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  data_type = 'Float64'
  n_components = 1
  select case(self%format)
  case(ascii)
    code = self%encode_ascii_dataarray(x=x)
    call self%write_dataarray_tag(data_type=data_type, number_of_components=n_components, data_name=data_name, data_content=code)
  case(raw, bin_app)
    call self%write_dataarray_tag_appended(data_type=data_type, number_of_components=n_components, data_name=data_name)
    call self%ioffset_update(n_byte=self%write_on_scratch_dataarray(x=x))
  case(binary)
    code = self%encode_base64_dataarray(x=x)
    call self%write_dataarray_tag(data_type=data_type, number_of_components=n_components, data_name=data_name, data_content=code)
  endselect
  error = self%error
  return
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction write_dataarray1_rank1_R8P

  function write_dataarray1_rank1_R4P(self, data_name, x) result(error)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Write `<DataArray... NumberOfComponents="1"...>...</DataArray>` tag (R4P).
  !---------------------------------------------------------------------------------------------------------------------------------
  class(vtk_file), intent(inout) :: self         !< VTK file.
  character(*),    intent(in)    :: data_name    !< Data name.
  real(R4P),       intent(in)    :: x(1:)        !< Data variable.
  integer(I4P)                   :: error        !< Error status.
  character(len=:), allocatable  :: data_type    !< Data type.
  integer(I4P)                   :: n_components !< Number of components.
  character(len=:), allocatable  :: code         !< Data variable encoded, ascii or Base64 codec.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  data_type = 'Float32'
  n_components = 1
  select case(self%format)
  case(ascii)
    code = self%encode_ascii_dataarray(x=x)
    call self%write_dataarray_tag(data_type=data_type, number_of_components=n_components, data_name=data_name, data_content=code)
  case(raw, bin_app)
    call self%write_dataarray_tag_appended(data_type=data_type, number_of_components=n_components, data_name=data_name)
    call self%ioffset_update(n_byte=self%write_on_scratch_dataarray(x=x))
  case(binary)
    code = self%encode_base64_dataarray(x=x)
    call self%write_dataarray_tag(data_type=data_type, number_of_components=n_components, data_name=data_name, data_content=code)
  endselect
  error = self%error
  return
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction write_dataarray1_rank1_R4P

  function write_dataarray1_rank1_I8P(self, data_name, x) result(error)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Write `<DataArray... NumberOfComponents="1"...>...</DataArray>` tag (I8P).
  !---------------------------------------------------------------------------------------------------------------------------------
  class(vtk_file), intent(inout) :: self         !< VTK file.
  character(*),    intent(in)    :: data_name    !< Data name.
  integer(I8P),    intent(in)    :: x(1:)        !< Data variable.
  integer(I4P)                   :: error        !< Error status.
  character(len=:), allocatable  :: data_type    !< Data type.
  integer(I4P)                   :: n_components !< Number of components.
  character(len=:), allocatable  :: code         !< Data variable encoded, ascii or Base64 codec.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  data_type = 'Int64'
  n_components = 1
  select case(self%format)
  case(ascii)
    code = self%encode_ascii_dataarray(x=x)
    call self%write_dataarray_tag(data_type=data_type, number_of_components=n_components, data_name=data_name, data_content=code)
  case(raw, bin_app)
    call self%write_dataarray_tag_appended(data_type=data_type, number_of_components=n_components, data_name=data_name)
    call self%ioffset_update(n_byte=self%write_on_scratch_dataarray(x=x))
  case(binary)
    code = self%encode_base64_dataarray(x=x)
    call self%write_dataarray_tag(data_type=data_type, number_of_components=n_components, data_name=data_name, data_content=code)
  endselect
  error = self%error
  return
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction write_dataarray1_rank1_I8P

  function write_dataarray1_rank1_I4P(self, data_name, x) result(error)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Write `<DataArray... NumberOfComponents="1"...>...</DataArray>` tag (I4P).
  !---------------------------------------------------------------------------------------------------------------------------------
  class(vtk_file), intent(inout) :: self         !< VTK file.
  character(*),    intent(in)    :: data_name    !< Data name.
  integer(I4P),    intent(in)    :: x(1:)        !< Data variable.
  integer(I4P)                   :: error        !< Error status.
  character(len=:), allocatable  :: data_type    !< Data type.
  integer(I4P)                   :: n_components !< Number of components.
  character(len=:), allocatable  :: code         !< Data variable encoded, ascii or Base64 codec.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  data_type = 'Int32'
  n_components = 1
  select case(self%format)
  case(ascii)
    code = self%encode_ascii_dataarray(x=x)
    call self%write_dataarray_tag(data_type=data_type, number_of_components=n_components, data_name=data_name, data_content=code)
  case(raw, bin_app)
    call self%write_dataarray_tag_appended(data_type=data_type, number_of_components=n_components, data_name=data_name)
    call self%ioffset_update(n_byte=self%write_on_scratch_dataarray(x=x))
  case(binary)
    code = self%encode_base64_dataarray(x=x)
    call self%write_dataarray_tag(data_type=data_type, number_of_components=n_components, data_name=data_name, data_content=code)
  endselect
  error = self%error
  return
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction write_dataarray1_rank1_I4P

  function write_dataarray1_rank1_I2P(self, data_name, x) result(error)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Write `<DataArray... NumberOfComponents="1"...>...</DataArray>` tag (I2P).
  !---------------------------------------------------------------------------------------------------------------------------------
  class(vtk_file), intent(inout) :: self         !< VTK file.
  character(*),    intent(in)    :: data_name    !< Data name.
  integer(I2P),    intent(in)    :: x(1:)        !< Data variable.
  integer(I4P)                   :: error        !< Error status.
  character(len=:), allocatable  :: data_type    !< Data type.
  integer(I4P)                   :: n_components !< Number of components.
  character(len=:), allocatable  :: code         !< Data variable encoded, ascii or Base64 codec.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  data_type = 'Int16'
  n_components = 1
  select case(self%format)
  case(ascii)
    code = self%encode_ascii_dataarray(x=x)
    call self%write_dataarray_tag(data_type=data_type, number_of_components=n_components, data_name=data_name, data_content=code)
  case(raw, bin_app)
    call self%write_dataarray_tag_appended(data_type=data_type, number_of_components=n_components, data_name=data_name)
    call self%ioffset_update(n_byte=self%write_on_scratch_dataarray(x=x))
  case(binary)
    code = self%encode_base64_dataarray(x=x)
    call self%write_dataarray_tag(data_type=data_type, number_of_components=n_components, data_name=data_name, data_content=code)
  endselect
  error = self%error
  return
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction write_dataarray1_rank1_I2P

  function write_dataarray1_rank1_I1P(self, data_name, x) result(error)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Write `<DataArray... NumberOfComponents="1"...>...</DataArray>` tag (I1P).
  !---------------------------------------------------------------------------------------------------------------------------------
  class(vtk_file), intent(inout) :: self         !< VTK file.
  character(*),    intent(in)    :: data_name    !< Data name.
  integer(I1P),    intent(in)    :: x(1:)        !< Data variable.
  integer(I4P)                   :: error        !< Error status.
  character(len=:), allocatable  :: data_type    !< Data type.
  integer(I4P)                   :: n_components !< Number of components.
  character(len=:), allocatable  :: code         !< Data variable encoded, ascii or Base64 codec.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  data_type = 'Int8'
  n_components = 1
  select case(self%format)
  case(ascii)
    code = self%encode_ascii_dataarray(x=x)
    call self%write_dataarray_tag(data_type=data_type, number_of_components=n_components, data_name=data_name, data_content=code)
  case(raw, bin_app)
    call self%write_dataarray_tag_appended(data_type=data_type, number_of_components=n_components, data_name=data_name)
    call self%ioffset_update(n_byte=self%write_on_scratch_dataarray(x=x))
  case(binary)
    code = self%encode_base64_dataarray(x=x)
    call self%write_dataarray_tag(data_type=data_type, number_of_components=n_components, data_name=data_name, data_content=code)
  endselect
  error = self%error
  return
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction write_dataarray1_rank1_I1P

  function write_dataarray1_rank2_R8P(self, data_name, x, one_component) result(error)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Write `<DataArray... NumberOfComponents="n"...>...</DataArray>` tag (R8P).
  !---------------------------------------------------------------------------------------------------------------------------------
  class(vtk_file), intent(inout)        :: self          !< VTK file.
  character(*),    intent(in)           :: data_name     !< Data name.
  real(R8P),       intent(in)           :: x(1:,1:)      !< Data variable.
  logical,         intent(in), optional :: one_component !< Force one component instead of inferring from first rank-size.
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
    call self%write_dataarray_tag(data_type=data_type, number_of_components=n_components, data_name=data_name, data_content=code)
  case(raw, bin_app)
    call self%write_dataarray_tag_appended(data_type=data_type, number_of_components=n_components, data_name=data_name)
    call self%ioffset_update(n_byte=self%write_on_scratch_dataarray(x=x))
  case(binary)
    code = self%encode_base64_dataarray(x=x)
    call self%write_dataarray_tag(data_type=data_type, number_of_components=n_components, data_name=data_name, data_content=code)
  endselect
  error = self%error
  return
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction write_dataarray1_rank2_R8P

  function write_dataarray1_rank2_R4P(self, data_name, x, one_component) result(error)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Write `<DataArray... NumberOfComponents="n"...>...</DataArray>` tag (R4P).
  !---------------------------------------------------------------------------------------------------------------------------------
  class(vtk_file), intent(inout)        :: self          !< VTK file.
  character(*),    intent(in)           :: data_name     !< Data name.
  real(R4P),       intent(in)           :: x(1:,1:)      !< Data variable.
  logical,         intent(in), optional :: one_component !< Force one component instead of inferring from first rank-size.
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
    call self%write_dataarray_tag(data_type=data_type, number_of_components=n_components, data_name=data_name, data_content=code)
  case(raw, bin_app)
    call self%write_dataarray_tag_appended(data_type=data_type, number_of_components=n_components, data_name=data_name)
    call self%ioffset_update(n_byte=self%write_on_scratch_dataarray(x=x))
  case(binary)
    code = self%encode_base64_dataarray(x=x)
    call self%write_dataarray_tag(data_type=data_type, number_of_components=n_components, data_name=data_name, data_content=code)
  endselect
  error = self%error
  return
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction write_dataarray1_rank2_R4P

  function write_dataarray1_rank2_I8P(self, data_name, x, one_component) result(error)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Write `<DataArray... NumberOfComponents="n"...>...</DataArray>` tag (I8P).
  !---------------------------------------------------------------------------------------------------------------------------------
  class(vtk_file), intent(inout)        :: self          !< VTK file.
  character(*),    intent(in)           :: data_name     !< Data name.
  integer(I8P),    intent(in)           :: x(1:,1:)      !< Data variable.
  logical,         intent(in), optional :: one_component !< Force one component instead of inferring from first rank-size.
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
    call self%write_dataarray_tag(data_type=data_type, number_of_components=n_components, data_name=data_name, data_content=code)
  case(raw, bin_app)
    call self%write_dataarray_tag_appended(data_type=data_type, number_of_components=n_components, data_name=data_name)
    call self%ioffset_update(n_byte=self%write_on_scratch_dataarray(x=x))
  case(binary)
    code = self%encode_base64_dataarray(x=x)
    call self%write_dataarray_tag(data_type=data_type, number_of_components=n_components, data_name=data_name, data_content=code)
  endselect
  error = self%error
  return
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction write_dataarray1_rank2_I8P

  function write_dataarray1_rank2_I4P(self, data_name, x, one_component) result(error)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Write `<DataArray... NumberOfComponents="n"...>...</DataArray>` tag (I4P).
  !---------------------------------------------------------------------------------------------------------------------------------
  class(vtk_file), intent(inout)        :: self          !< VTK file.
  character(*),    intent(in)           :: data_name     !< Data name.
  integer(I4P),    intent(in)           :: x(1:,1:)      !< Data variable.
  logical,         intent(in), optional :: one_component !< Force one component instead of inferring from first rank-size.
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
    call self%write_dataarray_tag(data_type=data_type, number_of_components=n_components, data_name=data_name, data_content=code)
  case(raw, bin_app)
    call self%write_dataarray_tag_appended(data_type=data_type, number_of_components=n_components, data_name=data_name)
    call self%ioffset_update(n_byte=self%write_on_scratch_dataarray(x=x))
  case(binary)
    code = self%encode_base64_dataarray(x=x)
    call self%write_dataarray_tag(data_type=data_type, number_of_components=n_components, data_name=data_name, data_content=code)
  endselect
  error = self%error
  return
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction write_dataarray1_rank2_I4P

  function write_dataarray1_rank2_I2P(self, data_name, x, one_component) result(error)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Write `<DataArray... NumberOfComponents="n"...>...</DataArray>` tag (I2P).
  !---------------------------------------------------------------------------------------------------------------------------------
  class(vtk_file), intent(inout)        :: self          !< VTK file.
  character(*),    intent(in)           :: data_name     !< Data name.
  integer(I2P),    intent(in)           :: x(1:,1:)      !< Data variable.
  logical,         intent(in), optional :: one_component !< Force one component instead of inferring from first rank-size.
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
    call self%write_dataarray_tag(data_type=data_type, number_of_components=n_components, data_name=data_name, data_content=code)
  case(raw, bin_app)
    call self%write_dataarray_tag_appended(data_type=data_type, number_of_components=n_components, data_name=data_name)
    call self%ioffset_update(n_byte=self%write_on_scratch_dataarray(x=x))
  case(binary)
    code = self%encode_base64_dataarray(x=x)
    call self%write_dataarray_tag(data_type=data_type, number_of_components=n_components, data_name=data_name, data_content=code)
  endselect
  error = self%error
  return
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction write_dataarray1_rank2_I2P

  function write_dataarray1_rank2_I1P(self, data_name, x, one_component) result(error)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Write `<DataArray... NumberOfComponents="n"...>...</DataArray>` tag (I1P).
  !---------------------------------------------------------------------------------------------------------------------------------
  class(vtk_file), intent(inout)        :: self          !< VTK file.
  character(*),    intent(in)           :: data_name     !< Data name.
  integer(I1P),    intent(in)           :: x(1:,1:)      !< Data variable.
  logical,         intent(in), optional :: one_component !< Force one component instead of inferring from first rank-size.
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
    call self%write_dataarray_tag(data_type=data_type, number_of_components=n_components, data_name=data_name, data_content=code)
  case(raw, bin_app)
    call self%write_dataarray_tag_appended(data_type=data_type, number_of_components=n_components, data_name=data_name)
    call self%ioffset_update(n_byte=self%write_on_scratch_dataarray(x=x))
  case(binary)
    code = self%encode_base64_dataarray(x=x)
    call self%write_dataarray_tag(data_type=data_type, number_of_components=n_components, data_name=data_name, data_content=code)
  endselect
  error = self%error
  return
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction write_dataarray1_rank2_I1P

  function write_dataarray1_rank3_R8P(self, data_name, x, one_component) result(error)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Write `<DataArray... NumberOfComponents="n"...>...</DataArray>` tag (R8P).
  !---------------------------------------------------------------------------------------------------------------------------------
  class(vtk_file), intent(inout)        :: self          !< VTK file.
  character(*),    intent(in)           :: data_name     !< Data name.
  real(R8P),       intent(in)           :: x(1:,1:,1:)   !< Data variable.
  logical,         intent(in), optional :: one_component !< Force one component instead of inferring from first rank-size.
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
    call self%write_dataarray_tag(data_type=data_type, number_of_components=n_components, data_name=data_name, data_content=code)
  case(raw, bin_app)
    call self%write_dataarray_tag_appended(data_type=data_type, number_of_components=n_components, data_name=data_name)
    call self%ioffset_update(n_byte=self%write_on_scratch_dataarray(x=x))
  case(binary)
    code = self%encode_base64_dataarray(x=x)
    call self%write_dataarray_tag(data_type=data_type, number_of_components=n_components, data_name=data_name, data_content=code)
  endselect
  error = self%error
  return
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction write_dataarray1_rank3_R8P

  function write_dataarray1_rank3_R4P(self, data_name, x, one_component) result(error)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Write `<DataArray... NumberOfComponents="n"...>...</DataArray>` tag (R4P).
  !---------------------------------------------------------------------------------------------------------------------------------
  class(vtk_file), intent(inout)        :: self          !< VTK file.
  character(*),    intent(in)           :: data_name     !< Data name.
  real(R4P),       intent(in)           :: x(1:,1:,1:)   !< Data variable.
  logical,         intent(in), optional :: one_component !< Force one component instead of inferring from first rank-size.
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
    call self%write_dataarray_tag(data_type=data_type, number_of_components=n_components, data_name=data_name, data_content=code)
  case(raw, bin_app)
    call self%write_dataarray_tag_appended(data_type=data_type, number_of_components=n_components, data_name=data_name)
    call self%ioffset_update(n_byte=self%write_on_scratch_dataarray(x=x))
  case(binary)
    code = self%encode_base64_dataarray(x=x)
    call self%write_dataarray_tag(data_type=data_type, number_of_components=n_components, data_name=data_name, data_content=code)
  endselect
  error = self%error
  return
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction write_dataarray1_rank3_R4P

  function write_dataarray1_rank3_I8P(self, data_name, x, one_component) result(error)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Write `<DataArray... NumberOfComponents="n"...>...</DataArray>` tag (I8P).
  !---------------------------------------------------------------------------------------------------------------------------------
  class(vtk_file), intent(inout)        :: self          !< VTK file.
  character(*),    intent(in)           :: data_name     !< Data name.
  integer(I8P),    intent(in)           :: x(1:,1:,1:)   !< Data variable.
  logical,         intent(in), optional :: one_component !< Force one component instead of inferring from first rank-size.
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
    call self%write_dataarray_tag(data_type=data_type, number_of_components=n_components, data_name=data_name, data_content=code)
  case(raw, bin_app)
    call self%write_dataarray_tag_appended(data_type=data_type, number_of_components=n_components, data_name=data_name)
    call self%ioffset_update(n_byte=self%write_on_scratch_dataarray(x=x))
  case(binary)
    code = self%encode_base64_dataarray(x=x)
    call self%write_dataarray_tag(data_type=data_type, number_of_components=n_components, data_name=data_name, data_content=code)
  endselect
  error = self%error
  return
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction write_dataarray1_rank3_I8P

  function write_dataarray1_rank3_I4P(self, data_name, x, one_component) result(error)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Write `<DataArray... NumberOfComponents="n"...>...</DataArray>` tag (I4P).
  !---------------------------------------------------------------------------------------------------------------------------------
  class(vtk_file), intent(inout)        :: self          !< VTK file.
  character(*),    intent(in)           :: data_name     !< Data name.
  integer(I4P),    intent(in)           :: x(1:,1:,1:)   !< Data variable.
  logical,         intent(in), optional :: one_component !< Force one component instead of inferring from first rank-size.
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
    call self%write_dataarray_tag(data_type=data_type, number_of_components=n_components, data_name=data_name, data_content=code)
  case(raw, bin_app)
    call self%write_dataarray_tag_appended(data_type=data_type, number_of_components=n_components, data_name=data_name)
    call self%ioffset_update(n_byte=self%write_on_scratch_dataarray(x=x))
  case(binary)
    code = self%encode_base64_dataarray(x=x)
    call self%write_dataarray_tag(data_type=data_type, number_of_components=n_components, data_name=data_name, data_content=code)
  endselect
  error = self%error
  return
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction write_dataarray1_rank3_I4P

  function write_dataarray1_rank3_I2P(self, data_name, x, one_component) result(error)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Write `<DataArray... NumberOfComponents="n"...>...</DataArray>` tag (I2P).
  !---------------------------------------------------------------------------------------------------------------------------------
  class(vtk_file), intent(inout)        :: self          !< VTK file.
  character(*),    intent(in)           :: data_name     !< Data name.
  integer(I2P),    intent(in)           :: x(1:,1:,1:)   !< Data variable.
  logical,         intent(in), optional :: one_component !< Force one component instead of inferring from first rank-size.
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
    call self%write_dataarray_tag(data_type=data_type, number_of_components=n_components, data_name=data_name, data_content=code)
  case(raw, bin_app)
    call self%write_dataarray_tag_appended(data_type=data_type, number_of_components=n_components, data_name=data_name)
    call self%ioffset_update(n_byte=self%write_on_scratch_dataarray(x=x))
  case(binary)
    code = self%encode_base64_dataarray(x=x)
    call self%write_dataarray_tag(data_type=data_type, number_of_components=n_components, data_name=data_name, data_content=code)
  endselect
  error = self%error
  return
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction write_dataarray1_rank3_I2P

  function write_dataarray1_rank3_I1P(self, data_name, x, one_component) result(error)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Write `<DataArray... NumberOfComponents="n"...>...</DataArray>` tag (I1P).
  !---------------------------------------------------------------------------------------------------------------------------------
  class(vtk_file), intent(inout)        :: self          !< VTK file.
  character(*),    intent(in)           :: data_name     !< Data name.
  integer(I1P),    intent(in)           :: x(1:,1:,1:)   !< Data variable.
  logical,         intent(in), optional :: one_component !< Force one component instead of inferring from first rank-size.
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
    call self%write_dataarray_tag(data_type=data_type, number_of_components=n_components, data_name=data_name, data_content=code)
  case(raw, bin_app)
    call self%write_dataarray_tag_appended(data_type=data_type, number_of_components=n_components, data_name=data_name)
    call self%ioffset_update(n_byte=self%write_on_scratch_dataarray(x=x))
  case(binary)
    code = self%encode_base64_dataarray(x=x)
    call self%write_dataarray_tag(data_type=data_type, number_of_components=n_components, data_name=data_name, data_content=code)
  endselect
  error = self%error
  return
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction write_dataarray1_rank3_I1P

  function write_dataarray1_rank4_R8P(self, data_name, x, one_component) result(error)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Write `<DataArray... NumberOfComponents="n"...>...</DataArray>` tag (R8P).
  !---------------------------------------------------------------------------------------------------------------------------------
  class(vtk_file), intent(inout)        :: self           !< VTK file.
  character(*),    intent(in)           :: data_name      !< Data name.
  real(R8P),       intent(in)           :: x(1:,1:,1:,1:) !< Data variable.
  logical,         intent(in), optional :: one_component  !< Force one component instead of inferring from first rank-size.
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
    call self%write_dataarray_tag(data_type=data_type, number_of_components=n_components, data_name=data_name, data_content=code)
  case(raw, bin_app)
    call self%write_dataarray_tag_appended(data_type=data_type, number_of_components=n_components, data_name=data_name)
    call self%ioffset_update(n_byte=self%write_on_scratch_dataarray(x=x))
  case(binary)
    code = self%encode_base64_dataarray(x=x)
    call self%write_dataarray_tag(data_type=data_type, number_of_components=n_components, data_name=data_name, data_content=code)
  endselect
  error = self%error
  return
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction write_dataarray1_rank4_R8P

  function write_dataarray1_rank4_R4P(self, data_name, x, one_component) result(error)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Write `<DataArray... NumberOfComponents="n"...>...</DataArray>` tag (R4P).
  !---------------------------------------------------------------------------------------------------------------------------------
  class(vtk_file), intent(inout)        :: self           !< VTK file.
  character(*),    intent(in)           :: data_name      !< Data name.
  real(R4P),       intent(in)           :: x(1:,1:,1:,1:) !< Data variable.
  logical,         intent(in), optional :: one_component  !< Force one component instead of inferring from first rank-size.
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
    call self%write_dataarray_tag(data_type=data_type, number_of_components=n_components, data_name=data_name, data_content=code)
  case(raw, bin_app)
    call self%write_dataarray_tag_appended(data_type=data_type, number_of_components=n_components, data_name=data_name)
    call self%ioffset_update(n_byte=self%write_on_scratch_dataarray(x=x))
  case(binary)
    code = self%encode_base64_dataarray(x=x)
    call self%write_dataarray_tag(data_type=data_type, number_of_components=n_components, data_name=data_name, data_content=code)
  endselect
  error = self%error
  return
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction write_dataarray1_rank4_R4P

  function write_dataarray1_rank4_I8P(self, data_name, x, one_component) result(error)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Write `<DataArray... NumberOfComponents="n"...>...</DataArray>` tag (I8P).
  !---------------------------------------------------------------------------------------------------------------------------------
  class(vtk_file), intent(inout)        :: self           !< VTK file.
  character(*),    intent(in)           :: data_name      !< Data name.
  integer(I8P),    intent(in)           :: x(1:,1:,1:,1:) !< Data variable.
  logical,         intent(in), optional :: one_component  !< Force one component instead of inferring from first rank-size.
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
    call self%write_dataarray_tag(data_type=data_type, number_of_components=n_components, data_name=data_name, data_content=code)
  case(raw, bin_app)
    call self%write_dataarray_tag_appended(data_type=data_type, number_of_components=n_components, data_name=data_name)
    call self%ioffset_update(n_byte=self%write_on_scratch_dataarray(x=x))
  case(binary)
    code = self%encode_base64_dataarray(x=x)
    call self%write_dataarray_tag(data_type=data_type, number_of_components=n_components, data_name=data_name, data_content=code)
  endselect
  error = self%error
  return
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction write_dataarray1_rank4_I8P

  function write_dataarray1_rank4_I4P(self, data_name, x, one_component) result(error)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Write `<DataArray... NumberOfComponents="n"...>...</DataArray>` tag (I4P).
  !---------------------------------------------------------------------------------------------------------------------------------
  class(vtk_file), intent(inout)        :: self           !< VTK file.
  character(*),    intent(in)           :: data_name      !< Data name.
  integer(I4P),    intent(in)           :: x(1:,1:,1:,1:) !< Data variable.
  logical,         intent(in), optional :: one_component  !< Force one component instead of inferring from first rank-size.
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
    call self%write_dataarray_tag(data_type=data_type, number_of_components=n_components, data_name=data_name, data_content=code)
  case(raw, bin_app)
    call self%write_dataarray_tag_appended(data_type=data_type, number_of_components=n_components, data_name=data_name)
    call self%ioffset_update(n_byte=self%write_on_scratch_dataarray(x=x))
  case(binary)
    code = self%encode_base64_dataarray(x=x)
    call self%write_dataarray_tag(data_type=data_type, number_of_components=n_components, data_name=data_name, data_content=code)
  endselect
  error = self%error
  return
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction write_dataarray1_rank4_I4P

  function write_dataarray1_rank4_I2P(self, data_name, x, one_component) result(error)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Write `<DataArray... NumberOfComponents="n"...>...</DataArray>` tag (I2P).
  !---------------------------------------------------------------------------------------------------------------------------------
  class(vtk_file), intent(inout)        :: self           !< VTK file.
  character(*),    intent(in)           :: data_name      !< Data name.
  integer(I2P),    intent(in)           :: x(1:,1:,1:,1:) !< Data variable.
  logical,         intent(in), optional :: one_component  !< Force one component instead of inferring from first rank-size.
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
    call self%write_dataarray_tag(data_type=data_type, number_of_components=n_components, data_name=data_name, data_content=code)
  case(raw, bin_app)
    call self%write_dataarray_tag_appended(data_type=data_type, number_of_components=n_components, data_name=data_name)
    call self%ioffset_update(n_byte=self%write_on_scratch_dataarray(x=x))
  case(binary)
    code = self%encode_base64_dataarray(x=x)
    call self%write_dataarray_tag(data_type=data_type, number_of_components=n_components, data_name=data_name, data_content=code)
  endselect
  error = self%error
  return
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction write_dataarray1_rank4_I2P

  function write_dataarray1_rank4_I1P(self, data_name, x, one_component) result(error)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Write `<DataArray... NumberOfComponents="n"...>...</DataArray>` tag (I1P).
  !---------------------------------------------------------------------------------------------------------------------------------
  class(vtk_file), intent(inout)        :: self           !< VTK file.
  character(*),    intent(in)           :: data_name      !< Data name.
  integer(I1P),    intent(in)           :: x(1:,1:,1:,1:) !< Data variable.
  logical,         intent(in), optional :: one_component  !< Force one component instead of inferring from first rank-size.
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
    call self%write_dataarray_tag(data_type=data_type, number_of_components=n_components, data_name=data_name, data_content=code)
  case(raw, bin_app)
    call self%write_dataarray_tag_appended(data_type=data_type, number_of_components=n_components, data_name=data_name)
    call self%ioffset_update(n_byte=self%write_on_scratch_dataarray(x=x))
  case(binary)
    code = self%encode_base64_dataarray(x=x)
    call self%write_dataarray_tag(data_type=data_type, number_of_components=n_components, data_name=data_name, data_content=code)
  endselect
  error = self%error
  return
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction write_dataarray1_rank4_I1P

  function write_dataarray3_rank1_R8P(self, data_name, x, y, z) result(error)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Write `<DataArray... NumberOfComponents="3"...>...</DataArray>` tag (R8P).
  !---------------------------------------------------------------------------------------------------------------------------------
  class(vtk_file), intent(inout) :: self         !< VTK file.
  character(*),    intent(in)    :: data_name    !< Data name.
  real(R8P),       intent(in)    :: x(1:)        !< X component of data variable.
  real(R8P),       intent(in)    :: y(1:)        !< Y component of data variable.
  real(R8P),       intent(in)    :: z(1:)        !< Z component of data variable.
  integer(I4P)                   :: error        !< Error status.
  character(len=:), allocatable  :: data_type    !< Data type.
  integer(I4P)                   :: n_components !< Number of components.
  character(len=:), allocatable  :: code         !< Data variable encoded, ascii or Base64 codec.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  data_type = 'Float64'
  n_components = 3
  select case(self%format)
  case(ascii)
    code = self%encode_ascii_dataarray(x=x, y=y, z=z)
    call self%write_dataarray_tag(data_type=data_type, number_of_components=n_components, data_name=data_name, data_content=code)
  case(raw, bin_app)
    call self%write_dataarray_tag_appended(data_type=data_type, number_of_components=n_components, data_name=data_name)
    call self%ioffset_update(n_byte=self%write_on_scratch_dataarray(x=x, y=y, z=z))
  case(binary)
    code = self%encode_base64_dataarray(x=x, y=y, z=z)
    call self%write_dataarray_tag(data_type=data_type, number_of_components=n_components, data_name=data_name, data_content=code)
  endselect
  error = self%error
  return
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction write_dataarray3_rank1_R8P

  function write_dataarray3_rank1_R4P(self, data_name, x, y, z) result(error)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Write `<DataArray... NumberOfComponents="3"...>...</DataArray>` tag (R4P).
  !---------------------------------------------------------------------------------------------------------------------------------
  class(vtk_file), intent(inout) :: self         !< VTK file.
  character(*),    intent(in)    :: data_name    !< Data name.
  real(R4P),       intent(in)    :: x(1:)        !< X component of data variable.
  real(R4P),       intent(in)    :: y(1:)        !< Y component of data variable.
  real(R4P),       intent(in)    :: z(1:)        !< Z component of data variable.
  integer(I4P)                   :: error        !< Error status.
  character(len=:), allocatable  :: data_type    !< Data type.
  integer(I4P)                   :: n_components !< Number of components.
  character(len=:), allocatable  :: code         !< Data variable encoded, ascii or Base64 codec.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  data_type = 'Float32'
  n_components = 3
  select case(self%format)
  case(ascii)
    code = self%encode_ascii_dataarray(x=x, y=y, z=z)
    call self%write_dataarray_tag(data_type=data_type, number_of_components=n_components, data_name=data_name, data_content=code)
  case(raw, bin_app)
    call self%write_dataarray_tag_appended(data_type=data_type, number_of_components=n_components, data_name=data_name)
    call self%ioffset_update(n_byte=self%write_on_scratch_dataarray(x=x, y=y, z=z))
  case(binary)
    code = self%encode_base64_dataarray(x=x, y=y, z=z)
    call self%write_dataarray_tag(data_type=data_type, number_of_components=n_components, data_name=data_name, data_content=code)
  endselect
  error = self%error
  return
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction write_dataarray3_rank1_R4P

  function write_dataarray3_rank1_I8P(self, data_name, x, y, z) result(error)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Write `<DataArray... NumberOfComponents="3"...>...</DataArray>` tag (I8P).
  !---------------------------------------------------------------------------------------------------------------------------------
  class(vtk_file), intent(inout) :: self         !< VTK file.
  character(*),    intent(in)    :: data_name    !< Data name.
  integer(I8P),    intent(in)    :: x(1:)        !< X component of data variable.
  integer(I8P),    intent(in)    :: y(1:)        !< Y component of data variable.
  integer(I8P),    intent(in)    :: z(1:)        !< Z component of data variable.
  integer(I4P)                   :: error        !< Error status.
  character(len=:), allocatable  :: data_type    !< Data type.
  integer(I4P)                   :: n_components !< Number of components.
  character(len=:), allocatable  :: code         !< Data variable encoded, ascii or Base64 codec.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  data_type = 'Int64'
  n_components = 3
  select case(self%format)
  case(ascii)
    code = self%encode_ascii_dataarray(x=x, y=y, z=z)
    call self%write_dataarray_tag(data_type=data_type, number_of_components=n_components, data_name=data_name, data_content=code)
  case(raw, bin_app)
    call self%write_dataarray_tag_appended(data_type=data_type, number_of_components=n_components, data_name=data_name)
    call self%ioffset_update(n_byte=self%write_on_scratch_dataarray(x=x, y=y, z=z))
  case(binary)
    code = self%encode_base64_dataarray(x=x, y=y, z=z)
    call self%write_dataarray_tag(data_type=data_type, number_of_components=n_components, data_name=data_name, data_content=code)
  endselect
  error = self%error
  return
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction write_dataarray3_rank1_I8P

  function write_dataarray3_rank1_I4P(self, data_name, x, y, z) result(error)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Write `<DataArray... NumberOfComponents="3"...>...</DataArray>` tag (I4P).
  !---------------------------------------------------------------------------------------------------------------------------------
  class(vtk_file), intent(inout) :: self         !< VTK file.
  character(*),    intent(in)    :: data_name    !< Data name.
  integer(I4P),    intent(in)    :: x(1:)        !< X component of data variable.
  integer(I4P),    intent(in)    :: y(1:)        !< Y component of data variable.
  integer(I4P),    intent(in)    :: z(1:)        !< Z component of data variable.
  integer(I4P)                   :: error        !< Error status.
  character(len=:), allocatable  :: data_type    !< Data type.
  integer(I4P)                   :: n_components !< Number of components.
  character(len=:), allocatable  :: code         !< Data variable encoded, ascii or Base64 codec.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  data_type = 'Int32'
  n_components = 3
  select case(self%format)
  case(ascii)
    code = self%encode_ascii_dataarray(x=x, y=y, z=z)
    call self%write_dataarray_tag(data_type=data_type, number_of_components=n_components, data_name=data_name, data_content=code)
  case(raw, bin_app)
    call self%write_dataarray_tag_appended(data_type=data_type, number_of_components=n_components, data_name=data_name)
    call self%ioffset_update(n_byte=self%write_on_scratch_dataarray(x=x, y=y, z=z))
  case(binary)
    code = self%encode_base64_dataarray(x=x, y=y, z=z)
    call self%write_dataarray_tag(data_type=data_type, number_of_components=n_components, data_name=data_name, data_content=code)
  endselect
  error = self%error
  return
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction write_dataarray3_rank1_I4P

  function write_dataarray3_rank1_I2P(self, data_name, x, y, z) result(error)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Write `<DataArray... NumberOfComponents="3"...>...</DataArray>` tag (I2P).
  !---------------------------------------------------------------------------------------------------------------------------------
  class(vtk_file), intent(inout) :: self         !< VTK file.
  character(*),    intent(in)    :: data_name    !< Data name.
  integer(I2P),    intent(in)    :: x(1:)        !< X component of data variable.
  integer(I2P),    intent(in)    :: y(1:)        !< Y component of data variable.
  integer(I2P),    intent(in)    :: z(1:)        !< Z component of data variable.
  integer(I4P)                   :: error        !< Error status.
  character(len=:), allocatable  :: data_type    !< Data type.
  integer(I4P)                   :: n_components !< Number of components.
  character(len=:), allocatable  :: code         !< Data variable encoded, ascii or Base64 codec.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  data_type = 'Int16'
  n_components = 3
  select case(self%format)
  case(ascii)
    code = self%encode_ascii_dataarray(x=x, y=y, z=z)
    call self%write_dataarray_tag(data_type=data_type, number_of_components=n_components, data_name=data_name, data_content=code)
  case(raw, bin_app)
    call self%write_dataarray_tag_appended(data_type=data_type, number_of_components=n_components, data_name=data_name)
    call self%ioffset_update(n_byte=self%write_on_scratch_dataarray(x=x, y=y, z=z))
  case(binary)
    code = self%encode_base64_dataarray(x=x, y=y, z=z)
    call self%write_dataarray_tag(data_type=data_type, number_of_components=n_components, data_name=data_name, data_content=code)
  endselect
  error = self%error
  return
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction write_dataarray3_rank1_I2P

  function write_dataarray3_rank1_I1P(self, data_name, x, y, z) result(error)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Write `<DataArray... NumberOfComponents="3"...>...</DataArray>` tag (I1P).
  !---------------------------------------------------------------------------------------------------------------------------------
  class(vtk_file), intent(inout) :: self         !< VTK file.
  character(*),    intent(in)    :: data_name    !< Data name.
  integer(I1P),    intent(in)    :: x(1:)        !< X component of data variable.
  integer(I1P),    intent(in)    :: y(1:)        !< Y component of data variable.
  integer(I1P),    intent(in)    :: z(1:)        !< Z component of data variable.
  integer(I4P)                   :: error        !< Error status.
  character(len=:), allocatable  :: data_type    !< Data type.
  integer(I4P)                   :: n_components !< Number of components.
  character(len=:), allocatable  :: code         !< Data variable encoded, ascii or Base64 codec.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  data_type = 'Int8'
  n_components = 3
  select case(self%format)
  case(ascii)
    code = self%encode_ascii_dataarray(x=x, y=y, z=z)
    call self%write_dataarray_tag(data_type=data_type, number_of_components=n_components, data_name=data_name, data_content=code)
  case(raw, bin_app)
    call self%write_dataarray_tag_appended(data_type=data_type, number_of_components=n_components, data_name=data_name)
    call self%ioffset_update(n_byte=self%write_on_scratch_dataarray(x=x, y=y, z=z))
  case(binary)
    code = self%encode_base64_dataarray(x=x, y=y, z=z)
    call self%write_dataarray_tag(data_type=data_type, number_of_components=n_components, data_name=data_name, data_content=code)
  endselect
  error = self%error
  return
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction write_dataarray3_rank1_I1P

  function write_dataarray3_rank3_R8P(self, data_name, x, y, z) result(error)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Write `<DataArray... NumberOfComponents="3"...>...</DataArray>` tag (R8P).
  !---------------------------------------------------------------------------------------------------------------------------------
  class(vtk_file), intent(inout) :: self         !< VTK file.
  character(*),    intent(in)    :: data_name    !< Data name.
  real(R8P),       intent(in)    :: x(1:,1:,1:)  !< X component of data variable.
  real(R8P),       intent(in)    :: y(1:,1:,1:)  !< Y component of data variable.
  real(R8P),       intent(in)    :: z(1:,1:,1:)  !< Z component of data variable.
  integer(I4P)                   :: error        !< Error status.
  character(len=:), allocatable  :: data_type    !< Data type.
  integer(I4P)                   :: n_components !< Number of components.
  character(len=:), allocatable  :: code         !< Data variable encoded, ascii or Base64 codec.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  data_type = 'Float64'
  n_components = 3
  select case(self%format)
  case(ascii)
    code = self%encode_ascii_dataarray(x=x, y=y, z=z)
    call self%write_dataarray_tag(data_type=data_type, number_of_components=n_components, data_name=data_name, data_content=code)
  case(raw, bin_app)
    call self%write_dataarray_tag_appended(data_type=data_type, number_of_components=n_components, data_name=data_name)
    call self%ioffset_update(n_byte=self%write_on_scratch_dataarray(x=x, y=y, z=z))
  case(binary)
    code = self%encode_base64_dataarray(x=x, y=y, z=z)
    call self%write_dataarray_tag(data_type=data_type, number_of_components=n_components, data_name=data_name, data_content=code)
  endselect
  error = self%error
  return
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction write_dataarray3_rank3_R8P

  function write_dataarray3_rank3_R4P(self, data_name, x, y, z) result(error)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Write `<DataArray... NumberOfComponents="3"...>...</DataArray>` tag (R4P).
  !---------------------------------------------------------------------------------------------------------------------------------
  class(vtk_file), intent(inout) :: self         !< VTK file.
  character(*),    intent(in)    :: data_name    !< Data name.
  real(R4P),       intent(in)    :: x(1:,1:,1:)  !< X component of data variable.
  real(R4P),       intent(in)    :: y(1:,1:,1:)  !< Y component of data variable.
  real(R4P),       intent(in)    :: z(1:,1:,1:)  !< Z component of data variable.
  integer(I4P)                   :: error        !< Error status.
  character(len=:), allocatable  :: data_type    !< Data type.
  integer(I4P)                   :: n_components !< Number of components.
  character(len=:), allocatable  :: code         !< Data variable encoded, ascii or Base64 codec.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  data_type = 'Float32'
  n_components = 3
  select case(self%format)
  case(ascii)
    code = self%encode_ascii_dataarray(x=x, y=y, z=z)
    call self%write_dataarray_tag(data_type=data_type, number_of_components=n_components, data_name=data_name, data_content=code)
  case(raw, bin_app)
    call self%write_dataarray_tag_appended(data_type=data_type, number_of_components=n_components, data_name=data_name)
    call self%ioffset_update(n_byte=self%write_on_scratch_dataarray(x=x, y=y, z=z))
  case(binary)
    code = self%encode_base64_dataarray(x=x, y=y, z=z)
    call self%write_dataarray_tag(data_type=data_type, number_of_components=n_components, data_name=data_name, data_content=code)
  endselect
  error = self%error
  return
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction write_dataarray3_rank3_R4P

  function write_dataarray3_rank3_I8P(self, data_name, x, y, z) result(error)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Write `<DataArray... NumberOfComponents="3"...>...</DataArray>` tag (I8P).
  !---------------------------------------------------------------------------------------------------------------------------------
  class(vtk_file), intent(inout) :: self         !< VTK file.
  character(*),    intent(in)    :: data_name    !< Data name.
  integer(I8P),    intent(in)    :: x(1:,1:,1:)  !< X component of data variable.
  integer(I8P),    intent(in)    :: y(1:,1:,1:)  !< Y component of data variable.
  integer(I8P),    intent(in)    :: z(1:,1:,1:)  !< Z component of data variable.
  integer(I4P)                   :: error        !< Error status.
  character(len=:), allocatable  :: data_type    !< Data type.
  integer(I4P)                   :: n_components !< Number of components.
  character(len=:), allocatable  :: code         !< Data variable encoded, ascii or Base64 codec.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  data_type = 'Int64'
  n_components = 3
  select case(self%format)
  case(ascii)
    code = self%encode_ascii_dataarray(x=x, y=y, z=z)
    call self%write_dataarray_tag(data_type=data_type, number_of_components=n_components, data_name=data_name, data_content=code)
  case(raw, bin_app)
    call self%write_dataarray_tag_appended(data_type=data_type, number_of_components=n_components, data_name=data_name)
    call self%ioffset_update(n_byte=self%write_on_scratch_dataarray(x=x, y=y, z=z))
  case(binary)
    code = self%encode_base64_dataarray(x=x, y=y, z=z)
    call self%write_dataarray_tag(data_type=data_type, number_of_components=n_components, data_name=data_name, data_content=code)
  endselect
  error = self%error
  return
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction write_dataarray3_rank3_I8P

  function write_dataarray3_rank3_I4P(self, data_name, x, y, z) result(error)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Write `<DataArray... NumberOfComponents="3"...>...</DataArray>` tag (I4P).
  !---------------------------------------------------------------------------------------------------------------------------------
  class(vtk_file), intent(inout) :: self         !< VTK file.
  character(*),    intent(in)    :: data_name    !< Data name.
  integer(I4P),    intent(in)    :: x(1:,1:,1:)  !< X component of data variable.
  integer(I4P),    intent(in)    :: y(1:,1:,1:)  !< Y component of data variable.
  integer(I4P),    intent(in)    :: z(1:,1:,1:)  !< Z component of data variable.
  integer(I4P)                   :: error        !< Error status.
  character(len=:), allocatable  :: data_type    !< Data type.
  integer(I4P)                   :: n_components !< Number of components.
  character(len=:), allocatable  :: code         !< Data variable encoded, ascii or Base64 codec.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  data_type = 'Int32'
  n_components = 3
  select case(self%format)
  case(ascii)
    code = self%encode_ascii_dataarray(x=x, y=y, z=z)
    call self%write_dataarray_tag(data_type=data_type, number_of_components=n_components, data_name=data_name, data_content=code)
  case(raw, bin_app)
    call self%write_dataarray_tag_appended(data_type=data_type, number_of_components=n_components, data_name=data_name)
    call self%ioffset_update(n_byte=self%write_on_scratch_dataarray(x=x, y=y, z=z))
  case(binary)
    code = self%encode_base64_dataarray(x=x, y=y, z=z)
    call self%write_dataarray_tag(data_type=data_type, number_of_components=n_components, data_name=data_name, data_content=code)
  endselect
  error = self%error
  return
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction write_dataarray3_rank3_I4P

  function write_dataarray3_rank3_I2P(self, data_name, x, y, z) result(error)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Write `<DataArray... NumberOfComponents="3"...>...</DataArray>` tag (I2P).
  !---------------------------------------------------------------------------------------------------------------------------------
  class(vtk_file), intent(inout) :: self         !< VTK file.
  character(*),    intent(in)    :: data_name    !< Data name.
  integer(I2P),    intent(in)    :: x(1:,1:,1:)  !< X component of data variable.
  integer(I2P),    intent(in)    :: y(1:,1:,1:)  !< Y component of data variable.
  integer(I2P),    intent(in)    :: z(1:,1:,1:)  !< Z component of data variable.
  integer(I4P)                   :: error        !< Error status.
  character(len=:), allocatable  :: data_type    !< Data type.
  integer(I4P)                   :: n_components !< Number of components.
  character(len=:), allocatable  :: code         !< Data variable encoded, ascii or Base64 codec.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  data_type = 'Int16'
  n_components = 3
  select case(self%format)
  case(ascii)
    code = self%encode_ascii_dataarray(x=x, y=y, z=z)
    call self%write_dataarray_tag(data_type=data_type, number_of_components=n_components, data_name=data_name, data_content=code)
  case(raw, bin_app)
    call self%write_dataarray_tag_appended(data_type=data_type, number_of_components=n_components, data_name=data_name)
    call self%ioffset_update(n_byte=self%write_on_scratch_dataarray(x=x, y=y, z=z))
  case(binary)
    code = self%encode_base64_dataarray(x=x, y=y, z=z)
    call self%write_dataarray_tag(data_type=data_type, number_of_components=n_components, data_name=data_name, data_content=code)
  endselect
  error = self%error
  return
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction write_dataarray3_rank3_I2P

  function write_dataarray3_rank3_I1P(self, data_name, x, y, z) result(error)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Write `<DataArray... NumberOfComponents="3"...>...</DataArray>` tag (I1P).
  !---------------------------------------------------------------------------------------------------------------------------------
  class(vtk_file), intent(inout) :: self         !< VTK file.
  character(*),    intent(in)    :: data_name    !< Data name.
  integer(I1P),    intent(in)    :: x(1:,1:,1:)  !< X component of data variable.
  integer(I1P),    intent(in)    :: y(1:,1:,1:)  !< Y component of data variable.
  integer(I1P),    intent(in)    :: z(1:,1:,1:)  !< Z component of data variable.
  integer(I4P)                   :: error        !< Error status.
  character(len=:), allocatable  :: data_type    !< Data type.
  integer(I4P)                   :: n_components !< Number of components.
  character(len=:), allocatable  :: code         !< Data variable encoded, ascii or Base64 codec.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  data_type = 'Int8'
  n_components = 3
  select case(self%format)
  case(ascii)
    code = self%encode_ascii_dataarray(x=x, y=y, z=z)
    call self%write_dataarray_tag(data_type=data_type, number_of_components=n_components, data_name=data_name, data_content=code)
  case(raw, bin_app)
    call self%write_dataarray_tag_appended(data_type=data_type, number_of_components=n_components, data_name=data_name)
    call self%ioffset_update(n_byte=self%write_on_scratch_dataarray(x=x, y=y, z=z))
  case(binary)
    code = self%encode_base64_dataarray(x=x, y=y, z=z)
    call self%write_dataarray_tag(data_type=data_type, number_of_components=n_components, data_name=data_name, data_content=code)
  endselect
  error = self%error
  return
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction write_dataarray3_rank3_I1P
