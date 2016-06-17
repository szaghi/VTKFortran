  !< Dataarray write submodule interface for VTK file class.
  module function write_dataarray1_rank1_R8P(self, data_name, x) result(error)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Write `<DataArray... NumberOfComponents="1"...>...</DataArray>` tag (R8P).
  !---------------------------------------------------------------------------------------------------------------------------------
  class(vtk_file), intent(inout) :: self         !< VTK file.
  character(*),    intent(in)    :: data_name    !< Data name.
  real(R8P),       intent(in)    :: x(1:)        !< Data variable.
  integer(I4P)                   :: error        !< Error status.
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction write_dataarray1_rank1_R8P

  module function write_dataarray1_rank1_R4P(self, data_name, x) result(error)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Write `<DataArray... NumberOfComponents="1"...>...</DataArray>` tag (R4P).
  !---------------------------------------------------------------------------------------------------------------------------------
  class(vtk_file), intent(inout) :: self         !< VTK file.
  character(*),    intent(in)    :: data_name    !< Data name.
  real(R4P),       intent(in)    :: x(1:)        !< Data variable.
  integer(I4P)                   :: error        !< Error status.
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction write_dataarray1_rank1_R4P

   module function write_dataarray1_rank1_I8P(self, data_name, x) result(error)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Write `<DataArray... NumberOfComponents="1"...>...</DataArray>` tag (I8P).
  !---------------------------------------------------------------------------------------------------------------------------------
  class(vtk_file), intent(inout) :: self         !< VTK file.
  character(*),    intent(in)    :: data_name    !< Data name.
  integer(I8P),    intent(in)    :: x(1:)        !< Data variable.
  integer(I4P)                   :: error        !< Error status.
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction write_dataarray1_rank1_I8P

  module function write_dataarray1_rank1_I4P(self, data_name, x) result(error)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Write `<DataArray... NumberOfComponents="1"...>...</DataArray>` tag (I4P).
  !---------------------------------------------------------------------------------------------------------------------------------
  class(vtk_file), intent(inout) :: self         !< VTK file.
  character(*),    intent(in)    :: data_name    !< Data name.
  integer(I4P),    intent(in)    :: x(1:)        !< Data variable.
  integer(I4P)                   :: error        !< Error status.
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction write_dataarray1_rank1_I4P

  module function write_dataarray1_rank1_I2P(self, data_name, x) result(error)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Write `<DataArray... NumberOfComponents="1"...>...</DataArray>` tag (I2P).
  !---------------------------------------------------------------------------------------------------------------------------------
  class(vtk_file), intent(inout) :: self         !< VTK file.
  character(*),    intent(in)    :: data_name    !< Data name.
  integer(I2P),    intent(in)    :: x(1:)        !< Data variable.
  integer(I4P)                   :: error        !< Error status.
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction write_dataarray1_rank1_I2P

   module function write_dataarray1_rank1_I1P(self, data_name, x) result(error)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Write `<DataArray... NumberOfComponents="1"...>...</DataArray>` tag (I1P).
  !---------------------------------------------------------------------------------------------------------------------------------
  class(vtk_file), intent(inout) :: self         !< VTK file.
  character(*),    intent(in)    :: data_name    !< Data name.
  integer(I1P),    intent(in)    :: x(1:)        !< Data variable.
  integer(I4P)                   :: error        !< Error status.
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction write_dataarray1_rank1_I1P

  module function write_dataarray1_rank2_R8P(self, data_name, x, one_component) result(error)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Write `<DataArray... NumberOfComponents="n"...>...</DataArray>` tag (R8P).
  !---------------------------------------------------------------------------------------------------------------------------------
  class(vtk_file), intent(inout)        :: self          !< VTK file.
  character(*),    intent(in)           :: data_name     !< Data name.
  real(R8P),       intent(in)           :: x(1:,1:)      !< Data variable.
  logical,         intent(in), optional :: one_component !< Force one component instead of inferring from first rank-size.
  integer(I4P)                          :: error         !< Error status.
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction write_dataarray1_rank2_R8P

  module function write_dataarray1_rank2_R4P(self, data_name, x, one_component) result(error)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Write `<DataArray... NumberOfComponents="n"...>...</DataArray>` tag (R4P).
  !---------------------------------------------------------------------------------------------------------------------------------
  class(vtk_file), intent(inout)        :: self          !< VTK file.
  character(*),    intent(in)           :: data_name     !< Data name.
  real(R4P),       intent(in)           :: x(1:,1:)      !< Data variable.
  logical,         intent(in), optional :: one_component !< Force one component instead of inferring from first rank-size.
  integer(I4P)                          :: error         !< Error status.
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction write_dataarray1_rank2_R4P

  module function write_dataarray1_rank2_I8P(self, data_name, x, one_component) result(error)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Write `<DataArray... NumberOfComponents="n"...>...</DataArray>` tag (I8P).
  !---------------------------------------------------------------------------------------------------------------------------------
  class(vtk_file), intent(inout)        :: self          !< VTK file.
  character(*),    intent(in)           :: data_name     !< Data name.
  integer(I8P),    intent(in)           :: x(1:,1:)      !< Data variable.
  logical,         intent(in), optional :: one_component !< Force one component instead of inferring from first rank-size.
  integer(I4P)                          :: error         !< Error status.
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction write_dataarray1_rank2_I8P

  module function write_dataarray1_rank2_I4P(self, data_name, x, one_component) result(error)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Write `<DataArray... NumberOfComponents="n"...>...</DataArray>` tag (I4P).
  !---------------------------------------------------------------------------------------------------------------------------------
  class(vtk_file), intent(inout)        :: self          !< VTK file.
  character(*),    intent(in)           :: data_name     !< Data name.
  integer(I4P),    intent(in)           :: x(1:,1:)      !< Data variable.
  logical,         intent(in), optional :: one_component !< Force one component instead of inferring from first rank-size.
  integer(I4P)                          :: error         !< Error status.
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction write_dataarray1_rank2_I4P

  module function write_dataarray1_rank2_I2P(self, data_name, x, one_component) result(error)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Write `<DataArray... NumberOfComponents="n"...>...</DataArray>` tag (I2P).
  !---------------------------------------------------------------------------------------------------------------------------------
  class(vtk_file), intent(inout)        :: self          !< VTK file.
  character(*),    intent(in)           :: data_name     !< Data name.
  integer(I2P),    intent(in)           :: x(1:,1:)      !< Data variable.
  logical,         intent(in), optional :: one_component !< Force one component instead of inferring from first rank-size.
  integer(I4P)                          :: error         !< Error status.
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction write_dataarray1_rank2_I2P

  module function write_dataarray1_rank2_I1P(self, data_name, x, one_component) result(error)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Write `<DataArray... NumberOfComponents="n"...>...</DataArray>` tag (I1P).
  !---------------------------------------------------------------------------------------------------------------------------------
  class(vtk_file), intent(inout)        :: self          !< VTK file.
  character(*),    intent(in)           :: data_name     !< Data name.
  integer(I1P),    intent(in)           :: x(1:,1:)      !< Data variable.
  logical,         intent(in), optional :: one_component !< Force one component instead of inferring from first rank-size.
  integer(I4P)                          :: error         !< Error status.
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction write_dataarray1_rank2_I1P

  module function write_dataarray1_rank3_R8P(self, data_name, x, one_component) result(error)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Write `<DataArray... NumberOfComponents="n"...>...</DataArray>` tag (R8P).
  !---------------------------------------------------------------------------------------------------------------------------------
  class(vtk_file), intent(inout)        :: self          !< VTK file.
  character(*),    intent(in)           :: data_name     !< Data name.
  real(R8P),       intent(in)           :: x(1:,1:,1:)   !< Data variable.
  logical,         intent(in), optional :: one_component !< Force one component instead of inferring from first rank-size.
  integer(I4P)                          :: error         !< Error status.
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction write_dataarray1_rank3_R8P

  module function write_dataarray1_rank3_R4P(self, data_name, x, one_component) result(error)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Write `<DataArray... NumberOfComponents="n"...>...</DataArray>` tag (R4P).
  !---------------------------------------------------------------------------------------------------------------------------------
  class(vtk_file), intent(inout)        :: self          !< VTK file.
  character(*),    intent(in)           :: data_name     !< Data name.
  real(R4P),       intent(in)           :: x(1:,1:,1:)   !< Data variable.
  logical,         intent(in), optional :: one_component !< Force one component instead of inferring from first rank-size.
  integer(I4P)                          :: error         !< Error status.
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction write_dataarray1_rank3_R4P

  module function write_dataarray1_rank3_I8P(self, data_name, x, one_component) result(error)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Write `<DataArray... NumberOfComponents="n"...>...</DataArray>` tag (I8P).
  !---------------------------------------------------------------------------------------------------------------------------------
  class(vtk_file), intent(inout)        :: self          !< VTK file.
  character(*),    intent(in)           :: data_name     !< Data name.
  integer(I8P),    intent(in)           :: x(1:,1:,1:)   !< Data variable.
  logical,         intent(in), optional :: one_component !< Force one component instead of inferring from first rank-size.
  integer(I4P)                          :: error         !< Error status.
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction write_dataarray1_rank3_I8P

  module function write_dataarray1_rank3_I4P(self, data_name, x, one_component) result(error)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Write `<DataArray... NumberOfComponents="n"...>...</DataArray>` tag (I4P).
  !---------------------------------------------------------------------------------------------------------------------------------
  class(vtk_file), intent(inout)        :: self          !< VTK file.
  character(*),    intent(in)           :: data_name     !< Data name.
  integer(I4P),    intent(in)           :: x(1:,1:,1:)   !< Data variable.
  logical,         intent(in), optional :: one_component !< Force one component instead of inferring from first rank-size.
  integer(I4P)                          :: error         !< Error status.
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction write_dataarray1_rank3_I4P

  module function write_dataarray1_rank3_I2P(self, data_name, x, one_component) result(error)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Write `<DataArray... NumberOfComponents="n"...>...</DataArray>` tag (I2P).
  !---------------------------------------------------------------------------------------------------------------------------------
  class(vtk_file), intent(inout)        :: self          !< VTK file.
  character(*),    intent(in)           :: data_name     !< Data name.
  integer(I2P),    intent(in)           :: x(1:,1:,1:)   !< Data variable.
  logical,         intent(in), optional :: one_component !< Force one component instead of inferring from first rank-size.
  integer(I4P)                          :: error         !< Error status.
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction write_dataarray1_rank3_I2P

  module function write_dataarray1_rank3_I1P(self, data_name, x, one_component) result(error)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Write `<DataArray... NumberOfComponents="n"...>...</DataArray>` tag (I1P).
  !---------------------------------------------------------------------------------------------------------------------------------
  class(vtk_file), intent(inout)        :: self          !< VTK file.
  character(*),    intent(in)           :: data_name     !< Data name.
  integer(I1P),    intent(in)           :: x(1:,1:,1:)   !< Data variable.
  logical,         intent(in), optional :: one_component !< Force one component instead of inferring from first rank-size.
  integer(I4P)                          :: error         !< Error status.
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction write_dataarray1_rank3_I1P

  module function write_dataarray1_rank4_R8P(self, data_name, x, one_component) result(error)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Write `<DataArray... NumberOfComponents="n"...>...</DataArray>` tag (R8P).
  !---------------------------------------------------------------------------------------------------------------------------------
  class(vtk_file), intent(inout)        :: self           !< VTK file.
  character(*),    intent(in)           :: data_name      !< Data name.
  real(R8P),       intent(in)           :: x(1:,1:,1:,1:) !< Data variable.
  logical,         intent(in), optional :: one_component  !< Force one component instead of inferring from first rank-size.
  integer(I4P)                          :: error          !< Error status.
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction write_dataarray1_rank4_R8P

  module function write_dataarray1_rank4_R4P(self, data_name, x, one_component) result(error)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Write `<DataArray... NumberOfComponents="n"...>...</DataArray>` tag (R4P).
  !---------------------------------------------------------------------------------------------------------------------------------
  class(vtk_file), intent(inout)        :: self           !< VTK file.
  character(*),    intent(in)           :: data_name      !< Data name.
  real(R4P),       intent(in)           :: x(1:,1:,1:,1:) !< Data variable.
  logical,         intent(in), optional :: one_component  !< Force one component instead of inferring from first rank-size.
  integer(I4P)                          :: error          !< Error status.
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction write_dataarray1_rank4_R4P

  module function write_dataarray1_rank4_I8P(self, data_name, x, one_component) result(error)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Write `<DataArray... NumberOfComponents="n"...>...</DataArray>` tag (I8P).
  !---------------------------------------------------------------------------------------------------------------------------------
  class(vtk_file), intent(inout)        :: self           !< VTK file.
  character(*),    intent(in)           :: data_name      !< Data name.
  integer(I8P),    intent(in)           :: x(1:,1:,1:,1:) !< Data variable.
  logical,         intent(in), optional :: one_component  !< Force one component instead of inferring from first rank-size.
  integer(I4P)                          :: error          !< Error status.
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction write_dataarray1_rank4_I8P

  module function write_dataarray1_rank4_I4P(self, data_name, x, one_component) result(error)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Write `<DataArray... NumberOfComponents="n"...>...</DataArray>` tag (I4P).
  !---------------------------------------------------------------------------------------------------------------------------------
  class(vtk_file), intent(inout)        :: self           !< VTK file.
  character(*),    intent(in)           :: data_name      !< Data name.
  integer(I4P),    intent(in)           :: x(1:,1:,1:,1:) !< Data variable.
  logical,         intent(in), optional :: one_component  !< Force one component instead of inferring from first rank-size.
  integer(I4P)                          :: error          !< Error status.
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction write_dataarray1_rank4_I4P

  module function write_dataarray1_rank4_I2P(self, data_name, x, one_component) result(error)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Write `<DataArray... NumberOfComponents="n"...>...</DataArray>` tag (I2P).
  !---------------------------------------------------------------------------------------------------------------------------------
  class(vtk_file), intent(inout)        :: self           !< VTK file.
  character(*),    intent(in)           :: data_name      !< Data name.
  integer(I2P),    intent(in)           :: x(1:,1:,1:,1:) !< Data variable.
  logical,         intent(in), optional :: one_component  !< Force one component instead of inferring from first rank-size.
  integer(I4P)                          :: error          !< Error status.
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction write_dataarray1_rank4_I2P

  module function write_dataarray1_rank4_I1P(self, data_name, x, one_component) result(error)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Write `<DataArray... NumberOfComponents="n"...>...</DataArray>` tag (I1P).
  !---------------------------------------------------------------------------------------------------------------------------------
  class(vtk_file), intent(inout)        :: self           !< VTK file.
  character(*),    intent(in)           :: data_name      !< Data name.
  integer(I1P),    intent(in)           :: x(1:,1:,1:,1:) !< Data variable.
  logical,         intent(in), optional :: one_component  !< Force one component instead of inferring from first rank-size.
  integer(I4P)                          :: error          !< Error status.
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction write_dataarray1_rank4_I1P

  module function write_dataarray3_rank1_R8P(self, data_name, x, y, z) result(error)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Write `<DataArray... NumberOfComponents="3"...>...</DataArray>` tag (R8P).
  !---------------------------------------------------------------------------------------------------------------------------------
  class(vtk_file), intent(inout) :: self         !< VTK file.
  character(*),    intent(in)    :: data_name    !< Data name.
  real(R8P),       intent(in)    :: x(1:)        !< X component of data variable.
  real(R8P),       intent(in)    :: y(1:)        !< Y component of data variable.
  real(R8P),       intent(in)    :: z(1:)        !< Z component of data variable.
  integer(I4P)                   :: error        !< Error status.
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction write_dataarray3_rank1_R8P

  module function write_dataarray3_rank1_R4P(self, data_name, x, y, z) result(error)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Write `<DataArray... NumberOfComponents="3"...>...</DataArray>` tag (R4P).
  !---------------------------------------------------------------------------------------------------------------------------------
  class(vtk_file), intent(inout) :: self         !< VTK file.
  character(*),    intent(in)    :: data_name    !< Data name.
  real(R4P),       intent(in)    :: x(1:)        !< X component of data variable.
  real(R4P),       intent(in)    :: y(1:)        !< Y component of data variable.
  real(R4P),       intent(in)    :: z(1:)        !< Z component of data variable.
  integer(I4P)                   :: error        !< Error status.
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction write_dataarray3_rank1_R4P

  module function write_dataarray3_rank1_I8P(self, data_name, x, y, z) result(error)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Write `<DataArray... NumberOfComponents="3"...>...</DataArray>` tag (I8P).
  !---------------------------------------------------------------------------------------------------------------------------------
  class(vtk_file), intent(inout) :: self         !< VTK file.
  character(*),    intent(in)    :: data_name    !< Data name.
  integer(I8P),    intent(in)    :: x(1:)        !< X component of data variable.
  integer(I8P),    intent(in)    :: y(1:)        !< Y component of data variable.
  integer(I8P),    intent(in)    :: z(1:)        !< Z component of data variable.
  integer(I4P)                   :: error        !< Error status.
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction write_dataarray3_rank1_I8P

  module function write_dataarray3_rank1_I4P(self, data_name, x, y, z) result(error)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Write `<DataArray... NumberOfComponents="3"...>...</DataArray>` tag (I4P).
  !---------------------------------------------------------------------------------------------------------------------------------
  class(vtk_file), intent(inout) :: self         !< VTK file.
  character(*),    intent(in)    :: data_name    !< Data name.
  integer(I4P),    intent(in)    :: x(1:)        !< X component of data variable.
  integer(I4P),    intent(in)    :: y(1:)        !< Y component of data variable.
  integer(I4P),    intent(in)    :: z(1:)        !< Z component of data variable.
  integer(I4P)                   :: error        !< Error status.
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction write_dataarray3_rank1_I4P

  module function write_dataarray3_rank1_I2P(self, data_name, x, y, z) result(error)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Write `<DataArray... NumberOfComponents="3"...>...</DataArray>` tag (I2P).
  !---------------------------------------------------------------------------------------------------------------------------------
  class(vtk_file), intent(inout) :: self         !< VTK file.
  character(*),    intent(in)    :: data_name    !< Data name.
  integer(I2P),    intent(in)    :: x(1:)        !< X component of data variable.
  integer(I2P),    intent(in)    :: y(1:)        !< Y component of data variable.
  integer(I2P),    intent(in)    :: z(1:)        !< Z component of data variable.
  integer(I4P)                   :: error        !< Error status.
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction write_dataarray3_rank1_I2P

  module function write_dataarray3_rank1_I1P(self, data_name, x, y, z) result(error)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Write `<DataArray... NumberOfComponents="3"...>...</DataArray>` tag (I1P).
  !---------------------------------------------------------------------------------------------------------------------------------
  class(vtk_file), intent(inout) :: self         !< VTK file.
  character(*),    intent(in)    :: data_name    !< Data name.
  integer(I1P),    intent(in)    :: x(1:)        !< X component of data variable.
  integer(I1P),    intent(in)    :: y(1:)        !< Y component of data variable.
  integer(I1P),    intent(in)    :: z(1:)        !< Z component of data variable.
  integer(I4P)                   :: error        !< Error status.
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction write_dataarray3_rank1_I1P

  module function write_dataarray3_rank3_R8P(self, data_name, x, y, z) result(error)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Write `<DataArray... NumberOfComponents="3"...>...</DataArray>` tag (R8P).
  !---------------------------------------------------------------------------------------------------------------------------------
  class(vtk_file), intent(inout) :: self         !< VTK file.
  character(*),    intent(in)    :: data_name    !< Data name.
  real(R8P),       intent(in)    :: x(1:,1:,1:)  !< X component of data variable.
  real(R8P),       intent(in)    :: y(1:,1:,1:)  !< Y component of data variable.
  real(R8P),       intent(in)    :: z(1:,1:,1:)  !< Z component of data variable.
  integer(I4P)                   :: error        !< Error status.
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction write_dataarray3_rank3_R8P

  module function write_dataarray3_rank3_R4P(self, data_name, x, y, z) result(error)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Write `<DataArray... NumberOfComponents="3"...>...</DataArray>` tag (R4P).
  !---------------------------------------------------------------------------------------------------------------------------------
  class(vtk_file), intent(inout) :: self         !< VTK file.
  character(*),    intent(in)    :: data_name    !< Data name.
  real(R4P),       intent(in)    :: x(1:,1:,1:)  !< X component of data variable.
  real(R4P),       intent(in)    :: y(1:,1:,1:)  !< Y component of data variable.
  real(R4P),       intent(in)    :: z(1:,1:,1:)  !< Z component of data variable.
  integer(I4P)                   :: error        !< Error status.
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction write_dataarray3_rank3_R4P

  module function write_dataarray3_rank3_I8P(self, data_name, x, y, z) result(error)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Write `<DataArray... NumberOfComponents="3"...>...</DataArray>` tag (I8P).
  !---------------------------------------------------------------------------------------------------------------------------------
  class(vtk_file), intent(inout) :: self         !< VTK file.
  character(*),    intent(in)    :: data_name    !< Data name.
  integer(I8P),    intent(in)    :: x(1:,1:,1:)  !< X component of data variable.
  integer(I8P),    intent(in)    :: y(1:,1:,1:)  !< Y component of data variable.
  integer(I8P),    intent(in)    :: z(1:,1:,1:)  !< Z component of data variable.
  integer(I4P)                   :: error        !< Error status.
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction write_dataarray3_rank3_I8P

  module function write_dataarray3_rank3_I4P(self, data_name, x, y, z) result(error)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Write `<DataArray... NumberOfComponents="3"...>...</DataArray>` tag (I4P).
  !---------------------------------------------------------------------------------------------------------------------------------
  class(vtk_file), intent(inout) :: self         !< VTK file.
  character(*),    intent(in)    :: data_name    !< Data name.
  integer(I4P),    intent(in)    :: x(1:,1:,1:)  !< X component of data variable.
  integer(I4P),    intent(in)    :: y(1:,1:,1:)  !< Y component of data variable.
  integer(I4P),    intent(in)    :: z(1:,1:,1:)  !< Z component of data variable.
  integer(I4P)                   :: error        !< Error status.
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction write_dataarray3_rank3_I4P

  module function write_dataarray3_rank3_I2P(self, data_name, x, y, z) result(error)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Write `<DataArray... NumberOfComponents="3"...>...</DataArray>` tag (I2P).
  !---------------------------------------------------------------------------------------------------------------------------------
  class(vtk_file), intent(inout) :: self         !< VTK file.
  character(*),    intent(in)    :: data_name    !< Data name.
  integer(I2P),    intent(in)    :: x(1:,1:,1:)  !< X component of data variable.
  integer(I2P),    intent(in)    :: y(1:,1:,1:)  !< Y component of data variable.
  integer(I2P),    intent(in)    :: z(1:,1:,1:)  !< Z component of data variable.
  integer(I4P)                   :: error        !< Error status.
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction write_dataarray3_rank3_I2P

  module function write_dataarray3_rank3_I1P(self, data_name, x, y, z) result(error)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Write `<DataArray... NumberOfComponents="3"...>...</DataArray>` tag (I1P).
  !---------------------------------------------------------------------------------------------------------------------------------
  class(vtk_file), intent(inout) :: self         !< VTK file.
  character(*),    intent(in)    :: data_name    !< Data name.
  integer(I1P),    intent(in)    :: x(1:,1:,1:)  !< X component of data variable.
  integer(I1P),    intent(in)    :: y(1:,1:,1:)  !< Y component of data variable.
  integer(I1P),    intent(in)    :: z(1:,1:,1:)  !< Z component of data variable.
  integer(I4P)                   :: error        !< Error status.
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction write_dataarray3_rank3_I1P
