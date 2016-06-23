!< VTK file XMl writer, ascii local.
module vtk_file_xml_writer_ascii_local
!-----------------------------------------------------------------------------------------------------------------------------------
!< VTK file XMl writer, ascii local.
!-----------------------------------------------------------------------------------------------------------------------------------
use befor64
use penf
use stringifor
use vtk_file_xml_writer_abstract
!-----------------------------------------------------------------------------------------------------------------------------------

!-----------------------------------------------------------------------------------------------------------------------------------
implicit none
private
public :: xml_writer_ascii_local
!-----------------------------------------------------------------------------------------------------------------------------------

!-----------------------------------------------------------------------------------------------------------------------------------
type, extends(xml_writer_abstract) :: xml_writer_ascii_local
  !< VTK file XML writer, ascii local.
  contains
    ! deferred methods
    procedure, pass(self) :: initialize                 !< Initialize writer.
    procedure, pass(self) :: write_dataarray1_rank1_R8P !< Write dataarray 1, rank 1, R8P.
    procedure, pass(self) :: write_dataarray1_rank1_R4P !< Write dataarray 1, rank 1, R4P.
    procedure, pass(self) :: write_dataarray1_rank1_I8P !< Write dataarray 1, rank 1, I8P.
    procedure, pass(self) :: write_dataarray1_rank1_I4P !< Write dataarray 1, rank 1, I4P.
    procedure, pass(self) :: write_dataarray1_rank1_I2P !< Write dataarray 1, rank 1, I2P.
    procedure, pass(self) :: write_dataarray1_rank1_I1P !< Write dataarray 1, rank 1, I1P.
    procedure, pass(self) :: write_dataarray1_rank2_R8P !< Write dataarray 1, rank 2, R8P.
    procedure, pass(self) :: write_dataarray1_rank2_R4P !< Write dataarray 1, rank 2, R4P.
    procedure, pass(self) :: write_dataarray1_rank2_I8P !< Write dataarray 1, rank 2, I8P.
    procedure, pass(self) :: write_dataarray1_rank2_I4P !< Write dataarray 1, rank 2, I4P.
    procedure, pass(self) :: write_dataarray1_rank2_I2P !< Write dataarray 1, rank 2, I2P.
    procedure, pass(self) :: write_dataarray1_rank2_I1P !< Write dataarray 1, rank 2, I1P.
    procedure, pass(self) :: write_dataarray1_rank3_R8P !< Write dataarray 1, rank 3, R8P.
    procedure, pass(self) :: write_dataarray1_rank3_R4P !< Write dataarray 1, rank 3, R4P.
    procedure, pass(self) :: write_dataarray1_rank3_I8P !< Write dataarray 1, rank 3, I8P.
    procedure, pass(self) :: write_dataarray1_rank3_I4P !< Write dataarray 1, rank 3, I4P.
    procedure, pass(self) :: write_dataarray1_rank3_I2P !< Write dataarray 1, rank 3, I2P.
    procedure, pass(self) :: write_dataarray1_rank3_I1P !< Write dataarray 1, rank 3, I1P.
    procedure, pass(self) :: write_dataarray1_rank4_R8P !< Write dataarray 1, rank 4, R8P.
    procedure, pass(self) :: write_dataarray1_rank4_R4P !< Write dataarray 1, rank 4, R4P.
    procedure, pass(self) :: write_dataarray1_rank4_I8P !< Write dataarray 1, rank 4, I8P.
    procedure, pass(self) :: write_dataarray1_rank4_I4P !< Write dataarray 1, rank 4, I4P.
    procedure, pass(self) :: write_dataarray1_rank4_I2P !< Write dataarray 1, rank 4, I2P.
    procedure, pass(self) :: write_dataarray1_rank4_I1P !< Write dataarray 1, rank 4, I1P.
    procedure, pass(self) :: write_dataarray3_rank1_R8P !< Write dataarray 3, rank 1, R8P.
    procedure, pass(self) :: write_dataarray3_rank1_R4P !< Write dataarray 3, rank 1, R4P.
    procedure, pass(self) :: write_dataarray3_rank1_I8P !< Write dataarray 3, rank 1, I8P.
    procedure, pass(self) :: write_dataarray3_rank1_I4P !< Write dataarray 3, rank 1, I4P.
    procedure, pass(self) :: write_dataarray3_rank1_I2P !< Write dataarray 3, rank 1, I2P.
    procedure, pass(self) :: write_dataarray3_rank1_I1P !< Write dataarray 3, rank 1, I1P.
    procedure, pass(self) :: write_dataarray3_rank3_R8P !< Write dataarray 3, rank 3, R8P.
    procedure, pass(self) :: write_dataarray3_rank3_R4P !< Write dataarray 3, rank 3, R4P.
    procedure, pass(self) :: write_dataarray3_rank3_I8P !< Write dataarray 3, rank 3, I8P.
    procedure, pass(self) :: write_dataarray3_rank3_I4P !< Write dataarray 3, rank 3, I4P.
    procedure, pass(self) :: write_dataarray3_rank3_I2P !< Write dataarray 3, rank 3, I2P.
    procedure, pass(self) :: write_dataarray3_rank3_I1P !< Write dataarray 3, rank 3, I1P.
    procedure, pass(self) :: write_dataarray_appended   !< Write appended.
    ! private methods
    generic, private           :: encode_ascii_dataarray =>          &
                                  encode_ascii_dataarray1_rank1_R8P, &
                                  encode_ascii_dataarray1_rank1_R4P, &
                                  encode_ascii_dataarray1_rank1_I8P, &
                                  encode_ascii_dataarray1_rank1_I4P, &
                                  encode_ascii_dataarray1_rank1_I2P, &
                                  encode_ascii_dataarray1_rank1_I1P, &
                                  encode_ascii_dataarray1_rank2_R8P, &
                                  encode_ascii_dataarray1_rank2_R4P, &
                                  encode_ascii_dataarray1_rank2_I8P, &
                                  encode_ascii_dataarray1_rank2_I4P, &
                                  encode_ascii_dataarray1_rank2_I2P, &
                                  encode_ascii_dataarray1_rank2_I1P, &
                                  encode_ascii_dataarray1_rank3_R8P, &
                                  encode_ascii_dataarray1_rank3_R4P, &
                                  encode_ascii_dataarray1_rank3_I8P, &
                                  encode_ascii_dataarray1_rank3_I4P, &
                                  encode_ascii_dataarray1_rank3_I2P, &
                                  encode_ascii_dataarray1_rank3_I1P, &
                                  encode_ascii_dataarray1_rank4_R8P, &
                                  encode_ascii_dataarray1_rank4_R4P, &
                                  encode_ascii_dataarray1_rank4_I8P, &
                                  encode_ascii_dataarray1_rank4_I4P, &
                                  encode_ascii_dataarray1_rank4_I2P, &
                                  encode_ascii_dataarray1_rank4_I1P, &
                                  encode_ascii_dataarray3_rank1_R8P, &
                                  encode_ascii_dataarray3_rank1_R4P, &
                                  encode_ascii_dataarray3_rank1_I8P, &
                                  encode_ascii_dataarray3_rank1_I4P, &
                                  encode_ascii_dataarray3_rank1_I2P, &
                                  encode_ascii_dataarray3_rank1_I1P, &
                                  encode_ascii_dataarray3_rank3_R8P, &
                                  encode_ascii_dataarray3_rank3_R4P, &
                                  encode_ascii_dataarray3_rank3_I8P, &
                                  encode_ascii_dataarray3_rank3_I4P, &
                                  encode_ascii_dataarray3_rank3_I2P, &
                                  encode_ascii_dataarray3_rank3_I1P !< Encode ascii dataarray.
    procedure, nopass, private :: encode_ascii_dataarray1_rank1_R8P !< Encode (ascii) dataarray, 1 data of rank 1 (R8P).
    procedure, nopass, private :: encode_ascii_dataarray1_rank1_R4P !< Encode (ascii) dataarray, 1 data of rank 1 (R4P).
    procedure, nopass, private :: encode_ascii_dataarray1_rank1_I8P !< Encode (ascii) dataarray, 1 data of rank 1 (I8P).
    procedure, nopass, private :: encode_ascii_dataarray1_rank1_I4P !< Encode (ascii) dataarray, 1 data of rank 1 (I4P).
    procedure, nopass, private :: encode_ascii_dataarray1_rank1_I2P !< Encode (ascii) dataarray, 1 data of rank 1 (I2P).
    procedure, nopass, private :: encode_ascii_dataarray1_rank1_I1P !< Encode (ascii) dataarray, 1 data of rank 1 (I1P).
    procedure, nopass, private :: encode_ascii_dataarray1_rank2_R8P !< Encode (ascii) dataarray, 1 data of rank 2 (R8P).
    procedure, nopass, private :: encode_ascii_dataarray1_rank2_R4P !< Encode (ascii) dataarray, 1 data of rank 2 (R4P).
    procedure, nopass, private :: encode_ascii_dataarray1_rank2_I8P !< Encode (ascii) dataarray, 1 data of rank 2 (I8P).
    procedure, nopass, private :: encode_ascii_dataarray1_rank2_I4P !< Encode (ascii) dataarray, 1 data of rank 2 (I4P).
    procedure, nopass, private :: encode_ascii_dataarray1_rank2_I2P !< Encode (ascii) dataarray, 1 data of rank 2 (I2P).
    procedure, nopass, private :: encode_ascii_dataarray1_rank2_I1P !< Encode (ascii) dataarray, 1 data of rank 2 (I1P).
    procedure, nopass, private :: encode_ascii_dataarray1_rank3_R8P !< Encode (ascii) dataarray, 1 data of rank 3 (R8P).
    procedure, nopass, private :: encode_ascii_dataarray1_rank3_R4P !< Encode (ascii) dataarray, 1 data of rank 3 (R4P).
    procedure, nopass, private :: encode_ascii_dataarray1_rank3_I8P !< Encode (ascii) dataarray, 1 data of rank 3 (I8P).
    procedure, nopass, private :: encode_ascii_dataarray1_rank3_I4P !< Encode (ascii) dataarray, 1 data of rank 3 (I4P).
    procedure, nopass, private :: encode_ascii_dataarray1_rank3_I2P !< Encode (ascii) dataarray, 1 data of rank 3 (I2P).
    procedure, nopass, private :: encode_ascii_dataarray1_rank3_I1P !< Encode (ascii) dataarray, 1 data of rank 3 (I1P).
    procedure, nopass, private :: encode_ascii_dataarray1_rank4_R8P !< Encode (ascii) dataarray, 1 data of rank 4 (R8P).
    procedure, nopass, private :: encode_ascii_dataarray1_rank4_R4P !< Encode (ascii) dataarray, 1 data of rank 4 (R4P).
    procedure, nopass, private :: encode_ascii_dataarray1_rank4_I8P !< Encode (ascii) dataarray, 1 data of rank 4 (I8P).
    procedure, nopass, private :: encode_ascii_dataarray1_rank4_I4P !< Encode (ascii) dataarray, 1 data of rank 4 (I4P).
    procedure, nopass, private :: encode_ascii_dataarray1_rank4_I2P !< Encode (ascii) dataarray, 1 data of rank 4 (I2P).
    procedure, nopass, private :: encode_ascii_dataarray1_rank4_I1P !< Encode (ascii) dataarray, 1 data of rank 4 (I1P).
    procedure, nopass, private :: encode_ascii_dataarray3_rank1_R8P !< Encode (ascii) dataarray, 3 data of rank 1 (R8P).
    procedure, nopass, private :: encode_ascii_dataarray3_rank1_R4P !< Encode (ascii) dataarray, 3 data of rank 1 (R4P).
    procedure, nopass, private :: encode_ascii_dataarray3_rank1_I8P !< Encode (ascii) dataarray, 3 data of rank 1 (I8P).
    procedure, nopass, private :: encode_ascii_dataarray3_rank1_I4P !< Encode (ascii) dataarray, 3 data of rank 1 (I4P).
    procedure, nopass, private :: encode_ascii_dataarray3_rank1_I2P !< Encode (ascii) dataarray, 3 data of rank 1 (I2P).
    procedure, nopass, private :: encode_ascii_dataarray3_rank1_I1P !< Encode (ascii) dataarray, 3 data of rank 1 (I1P).
    procedure, nopass, private :: encode_ascii_dataarray3_rank3_R8P !< Encode (ascii) dataarray, 3 data of rank 3 (R8P).
    procedure, nopass, private :: encode_ascii_dataarray3_rank3_R4P !< Encode (ascii) dataarray, 3 data of rank 3 (R4P).
    procedure, nopass, private :: encode_ascii_dataarray3_rank3_I8P !< Encode (ascii) dataarray, 3 data of rank 3 (I8P).
    procedure, nopass, private :: encode_ascii_dataarray3_rank3_I4P !< Encode (ascii) dataarray, 3 data of rank 3 (I4P).
    procedure, nopass, private :: encode_ascii_dataarray3_rank3_I2P !< Encode (ascii) dataarray, 3 data of rank 3 (I2P).
    procedure, nopass, private :: encode_ascii_dataarray3_rank3_I1P !< Encode (ascii) dataarray, 3 data of rank 3 (I1P).
endtype xml_writer_ascii_local
!-----------------------------------------------------------------------------------------------------------------------------------
contains
  function initialize(self, format, filename, mesh_topology, nx1, nx2, ny1, ny2, nz1, nz2) result(error)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Initialize writer.
  !---------------------------------------------------------------------------------------------------------------------------------
  class(xml_writer_ascii_local), intent(inout)         :: self          !< Writer.
  character(*),                  intent(in)            :: format        !< File format: ASCII.
  character(*),                  intent(in)            :: filename      !< File name.
  character(*),                  intent(in)            :: mesh_topology !< Mesh topology.
  integer(I4P),                  intent(in),  optional :: nx1           !< Initial node of x axis.
  integer(I4P),                  intent(in),  optional :: nx2           !< Final node of x axis.
  integer(I4P),                  intent(in),  optional :: ny1           !< Initial node of y axis.
  integer(I4P),                  intent(in),  optional :: ny2           !< Final node of y axis.
  integer(I4P),                  intent(in),  optional :: nz1           !< Initial node of z axis.
  integer(I4P),                  intent(in),  optional :: nz2           !< Final node of z axis.
  integer(I4P)                                         :: error         !< Error status.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  if (.not.is_initialized) call penf_init
  if (.not.is_b64_initialized) call b64_init
  self%topology = trim(adjustl(mesh_topology))
  self%format_ch = 'ascii'
  call self%open_xml_file(filename=filename)
  call self%write_header_tag
  call self%write_topology_tag(nx1=nx1, nx2=nx2, ny1=ny1, ny2=ny2, nz1=nz1, nz2=nz2)
  error = self%error
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction initialize

  ! write_dataarray methods
  function write_dataarray1_rank1_R8P(self, data_name, x, is_tuples) result(error)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Write `<DataArray... NumberOfComponents="1"...>...</DataArray>` tag (R8P).
  !---------------------------------------------------------------------------------------------------------------------------------
  class(xml_writer_ascii_local), intent(inout)        :: self         !< Writer.
  character(*),                  intent(in)           :: data_name    !< Data name.
  real(R8P),                     intent(in)           :: x(1:)        !< Data variable.
  logical,                       intent(in), optional :: is_tuples    !< Use "NumberOfTuples" instead of "NumberOfComponents".
  integer(I4P)                                        :: error        !< Error status.
  character(len=:), allocatable                       :: data_type    !< Data type.
  integer(I4P)                                        :: n_components !< Number of components.
  character(len=:), allocatable                       :: code         !< Data variable encoded, ascii or Base64 codec.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  data_type = 'Float64'
  n_components = 1
  code = self%encode_ascii_dataarray(x=x)
  call self%write_dataarray_tag(data_type=data_type, number_of_components=n_components, data_name=data_name, data_content=code, &
                                is_tuples=is_tuples)
  error = self%error
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction write_dataarray1_rank1_R8P

  function write_dataarray1_rank1_R4P(self, data_name, x, is_tuples) result(error)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Write `<DataArray... NumberOfComponents="1"...>...</DataArray>` tag (R4P).
  !---------------------------------------------------------------------------------------------------------------------------------
  class(xml_writer_ascii_local), intent(inout)        :: self         !< Writer.
  character(*),                  intent(in)           :: data_name    !< Data name.
  real(R4P),                     intent(in)           :: x(1:)        !< Data variable.
  logical,                       intent(in), optional :: is_tuples    !< Use "NumberOfTuples" instead of "NumberOfComponents".
  integer(I4P)                                        :: error        !< Error status.
  character(len=:), allocatable                       :: data_type    !< Data type.
  integer(I4P)                                        :: n_components !< Number of components.
  character(len=:), allocatable                       :: code         !< Data variable encoded, ascii or Base64 codec.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  data_type = 'Float32'
  n_components = 1
  code = self%encode_ascii_dataarray(x=x)
  call self%write_dataarray_tag(data_type=data_type, number_of_components=n_components, data_name=data_name, data_content=code, &
                                is_tuples=is_tuples)
  error = self%error
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction write_dataarray1_rank1_R4P

  function write_dataarray1_rank1_I8P(self, data_name, x, is_tuples) result(error)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Write `<DataArray... NumberOfComponents="1"...>...</DataArray>` tag (I8P).
  !---------------------------------------------------------------------------------------------------------------------------------
  class(xml_writer_ascii_local), intent(inout)        :: self         !< Writer.
  character(*),                  intent(in)           :: data_name    !< Data name.
  integer(I8P),                  intent(in)           :: x(1:)        !< Data variable.
  logical,                       intent(in), optional :: is_tuples    !< Use "NumberOfTuples" instead of "NumberOfComponents".
  integer(I4P)                                        :: error        !< Error status.
  character(len=:), allocatable                       :: data_type    !< Data type.
  integer(I4P)                                        :: n_components !< Number of components.
  character(len=:), allocatable                       :: code         !< Data variable encoded, ascii or Base64 codec.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  data_type = 'Int64'
  n_components = 1
  code = self%encode_ascii_dataarray(x=x)
  call self%write_dataarray_tag(data_type=data_type, number_of_components=n_components, data_name=data_name, data_content=code, &
                                is_tuples=is_tuples)
  error = self%error
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction write_dataarray1_rank1_I8P

  function write_dataarray1_rank1_I4P(self, data_name, x, is_tuples) result(error)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Write `<DataArray... NumberOfComponents="1"...>...</DataArray>` tag (I4P).
  !---------------------------------------------------------------------------------------------------------------------------------
  class(xml_writer_ascii_local), intent(inout)        :: self         !< Writer.
  character(*),                  intent(in)           :: data_name    !< Data name.
  integer(I4P),                  intent(in)           :: x(1:)        !< Data variable.
  logical,                       intent(in), optional :: is_tuples    !< Use "NumberOfTuples" instead of "NumberOfComponents".
  integer(I4P)                                        :: error        !< Error status.
  character(len=:), allocatable                       :: data_type    !< Data type.
  integer(I4P)                                        :: n_components !< Number of components.
  character(len=:), allocatable                       :: code         !< Data variable encoded, ascii or Base64 codec.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  data_type = 'Int32'
  n_components = 1
  code = self%encode_ascii_dataarray(x=x)
  call self%write_dataarray_tag(data_type=data_type, number_of_components=n_components, data_name=data_name, data_content=code, &
                                is_tuples=is_tuples)
  error = self%error
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction write_dataarray1_rank1_I4P

  function write_dataarray1_rank1_I2P(self, data_name, x, is_tuples) result(error)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Write `<DataArray... NumberOfComponents="1"...>...</DataArray>` tag (I2P).
  !---------------------------------------------------------------------------------------------------------------------------------
  class(xml_writer_ascii_local), intent(inout)        :: self         !< Writer.
  character(*),                  intent(in)           :: data_name    !< Data name.
  integer(I2P),                  intent(in)           :: x(1:)        !< Data variable.
  logical,                       intent(in), optional :: is_tuples    !< Use "NumberOfTuples" instead of "NumberOfComponents".
  integer(I4P)                                        :: error        !< Error status.
  character(len=:), allocatable                       :: data_type    !< Data type.
  integer(I4P)                                        :: n_components !< Number of components.
  character(len=:), allocatable                       :: code         !< Data variable encoded, ascii or Base64 codec.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  data_type = 'Int16'
  n_components = 1
  code = self%encode_ascii_dataarray(x=x)
  call self%write_dataarray_tag(data_type=data_type, number_of_components=n_components, data_name=data_name, data_content=code, &
                                is_tuples=is_tuples)
  error = self%error
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction write_dataarray1_rank1_I2P

  function write_dataarray1_rank1_I1P(self, data_name, x, is_tuples) result(error)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Write `<DataArray... NumberOfComponents="1"...>...</DataArray>` tag (I1P).
  !---------------------------------------------------------------------------------------------------------------------------------
  class(xml_writer_ascii_local), intent(inout)        :: self         !< Writer.
  character(*),                  intent(in)           :: data_name    !< Data name.
  integer(I1P),                  intent(in)           :: x(1:)        !< Data variable.
  logical,                       intent(in), optional :: is_tuples    !< Use "NumberOfTuples" instead of "NumberOfComponents".
  integer(I4P)                                        :: error        !< Error status.
  character(len=:), allocatable                       :: data_type    !< Data type.
  integer(I4P)                                        :: n_components !< Number of components.
  character(len=:), allocatable                       :: code         !< Data variable encoded, ascii or Base64 codec.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  data_type = 'Int8'
  n_components = 1
  code = self%encode_ascii_dataarray(x=x)
  call self%write_dataarray_tag(data_type=data_type, number_of_components=n_components, data_name=data_name, data_content=code, &
                                is_tuples=is_tuples)
  error = self%error
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction write_dataarray1_rank1_I1P

  function write_dataarray1_rank2_R8P(self, data_name, x, one_component, is_tuples) result(error)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Write `<DataArray... NumberOfComponents="n"...>...</DataArray>` tag (R8P).
  !---------------------------------------------------------------------------------------------------------------------------------
  class(xml_writer_ascii_local), intent(inout)        :: self          !< Writer.
  character(*),                  intent(in)           :: data_name     !< Data name.
  real(R8P),                     intent(in)           :: x(1:,1:)      !< Data variable.
  logical,                       intent(in), optional :: one_component !< Force one component.
  logical,                       intent(in), optional :: is_tuples     !< Use "NumberOfTuples" instead of "NumberOfComponents".
  integer(I4P)                                        :: error         !< Error status.
  character(len=:), allocatable                       :: data_type     !< Data type.
  integer(I4P)                                        :: n_components  !< Number of components.
  character(len=:), allocatable                       :: code          !< Data variable encoded, ascii or Base64 codec.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  data_type = 'Float64'
  n_components = size(x, dim=1)
  if (present(one_component)) then
    if (one_component) n_components = 1
  endif
  code = self%encode_ascii_dataarray(x=x)
  call self%write_dataarray_tag(data_type=data_type, number_of_components=n_components, data_name=data_name, data_content=code, &
                                is_tuples=is_tuples)
  error = self%error
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction write_dataarray1_rank2_R8P

  function write_dataarray1_rank2_R4P(self, data_name, x, one_component, is_tuples) result(error)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Write `<DataArray... NumberOfComponents="n"...>...</DataArray>` tag (R4P).
  !---------------------------------------------------------------------------------------------------------------------------------
  class(xml_writer_ascii_local), intent(inout)        :: self          !< Writer.
  character(*),                  intent(in)           :: data_name     !< Data name.
  real(R4P),                     intent(in)           :: x(1:,1:)      !< Data variable.
  logical,                       intent(in), optional :: one_component !< Force one component.
  logical,                       intent(in), optional :: is_tuples     !< Use "NumberOfTuples" instead of "NumberOfComponents".
  integer(I4P)                                        :: error         !< Error status.
  character(len=:), allocatable                       :: data_type     !< Data type.
  integer(I4P)                                        :: n_components  !< Number of components.
  character(len=:), allocatable                       :: code          !< Data variable encoded, ascii or Base64 codec.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  data_type = 'Float32'
  n_components = size(x, dim=1)
  if (present(one_component)) then
    if (one_component) n_components = 1
  endif
  code = self%encode_ascii_dataarray(x=x)
  call self%write_dataarray_tag(data_type=data_type, number_of_components=n_components, data_name=data_name, data_content=code, &
                                is_tuples=is_tuples)
  error = self%error
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction write_dataarray1_rank2_R4P

  function write_dataarray1_rank2_I8P(self, data_name, x, one_component, is_tuples) result(error)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Write `<DataArray... NumberOfComponents="n"...>...</DataArray>` tag (I8P).
  !---------------------------------------------------------------------------------------------------------------------------------
  class(xml_writer_ascii_local), intent(inout)        :: self          !< Writer.
  character(*),                  intent(in)           :: data_name     !< Data name.
  integer(I8P),                  intent(in)           :: x(1:,1:)      !< Data variable.
  logical,                       intent(in), optional :: one_component !< Force one component.
  logical,                       intent(in), optional :: is_tuples     !< Use "NumberOfTuples" instead of "NumberOfComponents".
  integer(I4P)                                        :: error         !< Error status.
  character(len=:), allocatable                       :: data_type     !< Data type.
  integer(I4P)                                        :: n_components  !< Number of components.
  character(len=:), allocatable                       :: code          !< Data variable encoded, ascii or Base64 codec.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  data_type = 'Int64'
  n_components = size(x, dim=1)
  if (present(one_component)) then
    if (one_component) n_components = 1
  endif
  code = self%encode_ascii_dataarray(x=x)
  call self%write_dataarray_tag(data_type=data_type, number_of_components=n_components, data_name=data_name, data_content=code, &
                                is_tuples=is_tuples)
  error = self%error
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction write_dataarray1_rank2_I8P

  function write_dataarray1_rank2_I4P(self, data_name, x, one_component, is_tuples) result(error)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Write `<DataArray... NumberOfComponents="n"...>...</DataArray>` tag (I4P).
  !---------------------------------------------------------------------------------------------------------------------------------
  class(xml_writer_ascii_local), intent(inout)        :: self          !< Writer.
  character(*),                  intent(in)           :: data_name     !< Data name.
  integer(I4P),                  intent(in)           :: x(1:,1:)      !< Data variable.
  logical,                       intent(in), optional :: one_component !< Force one component.
  logical,                       intent(in), optional :: is_tuples     !< Use "NumberOfTuples" instead of "NumberOfComponents".
  integer(I4P)                                        :: error         !< Error status.
  character(len=:), allocatable                       :: data_type     !< Data type.
  integer(I4P)                                        :: n_components  !< Number of components.
  character(len=:), allocatable                       :: code          !< Data variable encoded, ascii or Base64 codec.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  data_type = 'Int32'
  n_components = size(x, dim=1)
  if (present(one_component)) then
    if (one_component) n_components = 1
  endif
  code = self%encode_ascii_dataarray(x=x)
  call self%write_dataarray_tag(data_type=data_type, number_of_components=n_components, data_name=data_name, data_content=code, &
                                is_tuples=is_tuples)
  error = self%error
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction write_dataarray1_rank2_I4P

  function write_dataarray1_rank2_I2P(self, data_name, x, one_component, is_tuples) result(error)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Write `<DataArray... NumberOfComponents="n"...>...</DataArray>` tag (I2P).
  !---------------------------------------------------------------------------------------------------------------------------------
  class(xml_writer_ascii_local), intent(inout)        :: self          !< Writer.
  character(*),                  intent(in)           :: data_name     !< Data name.
  integer(I2P),                  intent(in)           :: x(1:,1:)      !< Data variable.
  logical,                       intent(in), optional :: one_component !< Force one component.
  logical,                       intent(in), optional :: is_tuples     !< Use "NumberOfTuples" instead of "NumberOfComponents".
  integer(I4P)                                        :: error         !< Error status.
  character(len=:), allocatable                       :: data_type     !< Data type.
  integer(I4P)                                        :: n_components  !< Number of components.
  character(len=:), allocatable                       :: code          !< Data variable encoded, ascii or Base64 codec.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  data_type = 'Int16'
  n_components = size(x, dim=1)
  if (present(one_component)) then
    if (one_component) n_components = 1
  endif
  code = self%encode_ascii_dataarray(x=x)
  call self%write_dataarray_tag(data_type=data_type, number_of_components=n_components, data_name=data_name, data_content=code, &
                                is_tuples=is_tuples)
  error = self%error
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction write_dataarray1_rank2_I2P

  function write_dataarray1_rank2_I1P(self, data_name, x, one_component, is_tuples) result(error)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Write `<DataArray... NumberOfComponents="n"...>...</DataArray>` tag (I1P).
  !---------------------------------------------------------------------------------------------------------------------------------
  class(xml_writer_ascii_local), intent(inout)        :: self          !< Writer.
  character(*),                  intent(in)           :: data_name     !< Data name.
  integer(I1P),                  intent(in)           :: x(1:,1:)      !< Data variable.
  logical,                       intent(in), optional :: one_component !< Force one component.
  logical,                       intent(in), optional :: is_tuples     !< Use "NumberOfTuples" instead of "NumberOfComponents".
  integer(I4P)                                        :: error         !< Error status.
  character(len=:), allocatable                       :: data_type     !< Data type.
  integer(I4P)                                        :: n_components  !< Number of components.
  character(len=:), allocatable                       :: code          !< Data variable encoded, ascii or Base64 codec.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  data_type = 'Int8'
  n_components = size(x, dim=1)
  if (present(one_component)) then
    if (one_component) n_components = 1
  endif
  code = self%encode_ascii_dataarray(x=x)
  call self%write_dataarray_tag(data_type=data_type, number_of_components=n_components, data_name=data_name, data_content=code, &
                                is_tuples=is_tuples)
  error = self%error
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction write_dataarray1_rank2_I1P

  function write_dataarray1_rank3_R8P(self, data_name, x, one_component, is_tuples) result(error)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Write `<DataArray... NumberOfComponents="n"...>...</DataArray>` tag (R8P).
  !---------------------------------------------------------------------------------------------------------------------------------
  class(xml_writer_ascii_local), intent(inout)        :: self          !< Writer.
  character(*),                  intent(in)           :: data_name     !< Data name.
  real(R8P),                     intent(in)           :: x(1:,1:,1:)   !< Data variable.
  logical,                       intent(in), optional :: one_component !< Force one component.
  logical,                       intent(in), optional :: is_tuples     !< Use "NumberOfTuples" instead of "NumberOfComponents".
  integer(I4P)                                        :: error         !< Error status.
  character(len=:), allocatable                       :: data_type     !< Data type.
  integer(I4P)                                        :: n_components  !< Number of components.
  character(len=:), allocatable                       :: code          !< Data variable encoded, ascii or Base64 codec.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  data_type = 'Float64'
  n_components = size(x, dim=1)
  if (present(one_component)) then
    if (one_component) n_components = 1
  endif
  code = self%encode_ascii_dataarray(x=x)
  call self%write_dataarray_tag(data_type=data_type, number_of_components=n_components, data_name=data_name, data_content=code, &
                                is_tuples=is_tuples)
  error = self%error
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction write_dataarray1_rank3_R8P

  function write_dataarray1_rank3_R4P(self, data_name, x, one_component, is_tuples) result(error)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Write `<DataArray... NumberOfComponents="n"...>...</DataArray>` tag (R4P).
  !---------------------------------------------------------------------------------------------------------------------------------
  class(xml_writer_ascii_local), intent(inout)        :: self          !< Writer.
  character(*),                  intent(in)           :: data_name     !< Data name.
  real(R4P),                     intent(in)           :: x(1:,1:,1:)   !< Data variable.
  logical,                       intent(in), optional :: one_component !< Force one component.
  logical,                       intent(in), optional :: is_tuples     !< Use "NumberOfTuples" instead of "NumberOfComponents".
  integer(I4P)                                        :: error         !< Error status.
  character(len=:), allocatable                       :: data_type     !< Data type.
  integer(I4P)                                        :: n_components  !< Number of components.
  character(len=:), allocatable                       :: code          !< Data variable encoded, ascii or Base64 codec.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  data_type = 'Float32'
  n_components = size(x, dim=1)
  if (present(one_component)) then
    if (one_component) n_components = 1
  endif
  code = self%encode_ascii_dataarray(x=x)
  call self%write_dataarray_tag(data_type=data_type, number_of_components=n_components, data_name=data_name, data_content=code, &
                                is_tuples=is_tuples)
  error = self%error
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction write_dataarray1_rank3_R4P

  function write_dataarray1_rank3_I8P(self, data_name, x, one_component, is_tuples) result(error)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Write `<DataArray... NumberOfComponents="n"...>...</DataArray>` tag (I8P).
  !---------------------------------------------------------------------------------------------------------------------------------
  class(xml_writer_ascii_local), intent(inout)        :: self          !< Writer.
  character(*),                  intent(in)           :: data_name     !< Data name.
  integer(I8P),                  intent(in)           :: x(1:,1:,1:)   !< Data variable.
  logical,                       intent(in), optional :: one_component !< Force one component.
  logical,                       intent(in), optional :: is_tuples     !< Use "NumberOfTuples" instead of "NumberOfComponents".
  integer(I4P)                                        :: error         !< Error status.
  character(len=:), allocatable                       :: data_type     !< Data type.
  integer(I4P)                                        :: n_components  !< Number of components.
  character(len=:), allocatable                       :: code          !< Data variable encoded, ascii or Base64 codec.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  data_type = 'Int64'
  n_components = size(x, dim=1)
  if (present(one_component)) then
    if (one_component) n_components = 1
  endif
  code = self%encode_ascii_dataarray(x=x)
  call self%write_dataarray_tag(data_type=data_type, number_of_components=n_components, data_name=data_name, data_content=code, &
                                is_tuples=is_tuples)
  error = self%error
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction write_dataarray1_rank3_I8P

  function write_dataarray1_rank3_I4P(self, data_name, x, one_component, is_tuples) result(error)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Write `<DataArray... NumberOfComponents="n"...>...</DataArray>` tag (I4P).
  !---------------------------------------------------------------------------------------------------------------------------------
  class(xml_writer_ascii_local), intent(inout)        :: self          !< Writer.
  character(*),                  intent(in)           :: data_name     !< Data name.
  integer(I4P),                  intent(in)           :: x(1:,1:,1:)   !< Data variable.
  logical,                       intent(in), optional :: one_component !< Force one component.
  logical,                       intent(in), optional :: is_tuples     !< Use "NumberOfTuples" instead of "NumberOfComponents".
  integer(I4P)                                        :: error         !< Error status.
  character(len=:), allocatable                       :: data_type     !< Data type.
  integer(I4P)                                        :: n_components  !< Number of components.
  character(len=:), allocatable                       :: code          !< Data variable encoded, ascii or Base64 codec.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  data_type = 'Int32'
  n_components = size(x, dim=1)
  if (present(one_component)) then
    if (one_component) n_components = 1
  endif
  code = self%encode_ascii_dataarray(x=x)
  call self%write_dataarray_tag(data_type=data_type, number_of_components=n_components, data_name=data_name, data_content=code, &
                                is_tuples=is_tuples)
  error = self%error
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction write_dataarray1_rank3_I4P

  function write_dataarray1_rank3_I2P(self, data_name, x, one_component, is_tuples) result(error)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Write `<DataArray... NumberOfComponents="n"...>...</DataArray>` tag (I2P).
  !---------------------------------------------------------------------------------------------------------------------------------
  class(xml_writer_ascii_local), intent(inout)        :: self          !< Writer.
  character(*),                  intent(in)           :: data_name     !< Data name.
  integer(I2P),                  intent(in)           :: x(1:,1:,1:)   !< Data variable.
  logical,                       intent(in), optional :: one_component !< Force one component.
  logical,                       intent(in), optional :: is_tuples     !< Use "NumberOfTuples" instead of "NumberOfComponents".
  integer(I4P)                                        :: error         !< Error status.
  character(len=:), allocatable                       :: data_type     !< Data type.
  integer(I4P)                                        :: n_components  !< Number of components.
  character(len=:), allocatable                       :: code          !< Data variable encoded, ascii or Base64 codec.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  data_type = 'Int16'
  n_components = size(x, dim=1)
  if (present(one_component)) then
    if (one_component) n_components = 1
  endif
  code = self%encode_ascii_dataarray(x=x)
  call self%write_dataarray_tag(data_type=data_type, number_of_components=n_components, data_name=data_name, data_content=code, &
                                is_tuples=is_tuples)
  error = self%error
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction write_dataarray1_rank3_I2P

  function write_dataarray1_rank3_I1P(self, data_name, x, one_component, is_tuples) result(error)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Write `<DataArray... NumberOfComponents="n"...>...</DataArray>` tag (I1P).
  !---------------------------------------------------------------------------------------------------------------------------------
  class(xml_writer_ascii_local), intent(inout)        :: self          !< Writer.
  character(*),                  intent(in)           :: data_name     !< Data name.
  integer(I1P),                  intent(in)           :: x(1:,1:,1:)   !< Data variable.
  logical,                       intent(in), optional :: one_component !< Force one component.
  logical,                       intent(in), optional :: is_tuples     !< Use "NumberOfTuples" instead of "NumberOfComponents".
  integer(I4P)                                        :: error         !< Error status.
  character(len=:), allocatable                       :: data_type     !< Data type.
  integer(I4P)                                        :: n_components  !< Number of components.
  character(len=:), allocatable                       :: code          !< Data variable encoded, ascii or Base64 codec.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  data_type = 'Int8'
  n_components = size(x, dim=1)
  if (present(one_component)) then
    if (one_component) n_components = 1
  endif
  code = self%encode_ascii_dataarray(x=x)
  call self%write_dataarray_tag(data_type=data_type, number_of_components=n_components, data_name=data_name, data_content=code, &
                                is_tuples=is_tuples)
  error = self%error
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction write_dataarray1_rank3_I1P

  function write_dataarray1_rank4_R8P(self, data_name, x, one_component, is_tuples) result(error)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Write `<DataArray... NumberOfComponents="n"...>...</DataArray>` tag (R8P).
  !---------------------------------------------------------------------------------------------------------------------------------
  class(xml_writer_ascii_local), intent(inout)        :: self           !< Writer.
  character(*),                  intent(in)           :: data_name      !< Data name.
  real(R8P),                     intent(in)           :: x(1:,1:,1:,1:) !< Data variable.
  logical,                       intent(in), optional :: one_component  !< Force one component.
  logical,                       intent(in), optional :: is_tuples      !< Use "NumberOfTuples" instead of "NumberOfComponents".
  integer(I4P)                                        :: error          !< Error status.
  character(len=:), allocatable                       :: data_type      !< Data type.
  integer(I4P)                                        :: n_components   !< Number of components.
  character(len=:), allocatable                       :: code           !< Data variable encoded, ascii or Base64 codec.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  data_type = 'Float64'
  n_components = size(x, dim=1)
  if (present(one_component)) then
    if (one_component) n_components = 1
  endif
  code = self%encode_ascii_dataarray(x=x)
  call self%write_dataarray_tag(data_type=data_type, number_of_components=n_components, data_name=data_name, data_content=code, &
                                is_tuples=is_tuples)
  error = self%error
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction write_dataarray1_rank4_R8P

  function write_dataarray1_rank4_R4P(self, data_name, x, one_component, is_tuples) result(error)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Write `<DataArray... NumberOfComponents="n"...>...</DataArray>` tag (R4P).
  !---------------------------------------------------------------------------------------------------------------------------------
  class(xml_writer_ascii_local), intent(inout)        :: self           !< Writer.
  character(*),                  intent(in)           :: data_name      !< Data name.
  real(R4P),                     intent(in)           :: x(1:,1:,1:,1:) !< Data variable.
  logical,                       intent(in), optional :: one_component  !< Force one component.
  logical,                       intent(in), optional :: is_tuples      !< Use "NumberOfTuples" instead of "NumberOfComponents".
  integer(I4P)                                        :: error          !< Error status.
  character(len=:), allocatable                       :: data_type      !< Data type.
  integer(I4P)                                        :: n_components   !< Number of components.
  character(len=:), allocatable                       :: code           !< Data variable encoded, ascii or Base64 codec.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  data_type = 'Float32'
  n_components = size(x, dim=1)
  if (present(one_component)) then
    if (one_component) n_components = 1
  endif
  code = self%encode_ascii_dataarray(x=x)
  call self%write_dataarray_tag(data_type=data_type, number_of_components=n_components, data_name=data_name, data_content=code, &
                                is_tuples=is_tuples)
  error = self%error
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction write_dataarray1_rank4_R4P

  function write_dataarray1_rank4_I8P(self, data_name, x, one_component, is_tuples) result(error)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Write `<DataArray... NumberOfComponents="n"...>...</DataArray>` tag (I8P).
  !---------------------------------------------------------------------------------------------------------------------------------
  class(xml_writer_ascii_local), intent(inout)        :: self           !< Writer.
  character(*),                  intent(in)           :: data_name      !< Data name.
  integer(I8P),                  intent(in)           :: x(1:,1:,1:,1:) !< Data variable.
  logical,                       intent(in), optional :: one_component  !< Force one component.
  logical,                       intent(in), optional :: is_tuples      !< Use "NumberOfTuples" instead of "NumberOfComponents".
  integer(I4P)                                        :: error          !< Error status.
  character(len=:), allocatable                       :: data_type      !< Data type.
  integer(I4P)                                        :: n_components   !< Number of components.
  character(len=:), allocatable                       :: code           !< Data variable encoded, ascii or Base64 codec.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  data_type = 'Int64'
  n_components = size(x, dim=1)
  if (present(one_component)) then
    if (one_component) n_components = 1
  endif
  code = self%encode_ascii_dataarray(x=x)
  call self%write_dataarray_tag(data_type=data_type, number_of_components=n_components, data_name=data_name, data_content=code, &
                                is_tuples=is_tuples)
  error = self%error
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction write_dataarray1_rank4_I8P

  function write_dataarray1_rank4_I4P(self, data_name, x, one_component, is_tuples) result(error)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Write `<DataArray... NumberOfComponents="n"...>...</DataArray>` tag (I4P).
  !---------------------------------------------------------------------------------------------------------------------------------
  class(xml_writer_ascii_local), intent(inout)        :: self           !< Writer.
  character(*),                  intent(in)           :: data_name      !< Data name.
  integer(I4P),                  intent(in)           :: x(1:,1:,1:,1:) !< Data variable.
  logical,                       intent(in), optional :: one_component  !< Force one component.
  logical,                       intent(in), optional :: is_tuples      !< Use "NumberOfTuples" instead of "NumberOfComponents".
  integer(I4P)                                        :: error          !< Error status.
  character(len=:), allocatable                       :: data_type      !< Data type.
  integer(I4P)                                        :: n_components   !< Number of components.
  character(len=:), allocatable                       :: code           !< Data variable encoded, ascii or Base64 codec.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  data_type = 'Int32'
  n_components = size(x, dim=1)
  if (present(one_component)) then
    if (one_component) n_components = 1
  endif
  code = self%encode_ascii_dataarray(x=x)
  call self%write_dataarray_tag(data_type=data_type, number_of_components=n_components, data_name=data_name, data_content=code, &
                                is_tuples=is_tuples)
  error = self%error
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction write_dataarray1_rank4_I4P

  function write_dataarray1_rank4_I2P(self, data_name, x, one_component, is_tuples) result(error)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Write `<DataArray... NumberOfComponents="n"...>...</DataArray>` tag (I2P).
  !---------------------------------------------------------------------------------------------------------------------------------
  class(xml_writer_ascii_local), intent(inout)        :: self           !< Writer.
  character(*),                  intent(in)           :: data_name      !< Data name.
  integer(I2P),                  intent(in)           :: x(1:,1:,1:,1:) !< Data variable.
  logical,                       intent(in), optional :: one_component  !< Force one component.
  logical,                       intent(in), optional :: is_tuples      !< Use "NumberOfTuples" instead of "NumberOfComponents".
  integer(I4P)                                        :: error          !< Error status.
  character(len=:), allocatable                       :: data_type      !< Data type.
  integer(I4P)                                        :: n_components   !< Number of components.
  character(len=:), allocatable                       :: code           !< Data variable encoded, ascii or Base64 codec.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  data_type = 'Int16'
  n_components = size(x, dim=1)
  if (present(one_component)) then
    if (one_component) n_components = 1
  endif
  code = self%encode_ascii_dataarray(x=x)
  call self%write_dataarray_tag(data_type=data_type, number_of_components=n_components, data_name=data_name, data_content=code, &
                                is_tuples=is_tuples)
  error = self%error
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction write_dataarray1_rank4_I2P

  function write_dataarray1_rank4_I1P(self, data_name, x, one_component, is_tuples) result(error)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Write `<DataArray... NumberOfComponents="n"...>...</DataArray>` tag (I1P).
  !---------------------------------------------------------------------------------------------------------------------------------
  class(xml_writer_ascii_local), intent(inout)        :: self           !< Writer.
  character(*),                  intent(in)           :: data_name      !< Data name.
  integer(I1P),                  intent(in)           :: x(1:,1:,1:,1:) !< Data variable.
  logical,                       intent(in), optional :: one_component  !< Force one component.
  logical,                       intent(in), optional :: is_tuples      !< Use "NumberOfTuples" instead of "NumberOfComponents".
  integer(I4P)                                        :: error          !< Error status.
  character(len=:), allocatable                       :: data_type      !< Data type.
  integer(I4P)                                        :: n_components   !< Number of components.
  character(len=:), allocatable                       :: code           !< Data variable encoded, ascii or Base64 codec.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  data_type = 'Int8'
  n_components = size(x, dim=1)
  if (present(one_component)) then
    if (one_component) n_components = 1
  endif
  code = self%encode_ascii_dataarray(x=x)
  call self%write_dataarray_tag(data_type=data_type, number_of_components=n_components, data_name=data_name, data_content=code, &
                                is_tuples=is_tuples)
  error = self%error
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction write_dataarray1_rank4_I1P

  function write_dataarray3_rank1_R8P(self, data_name, x, y, z, is_tuples) result(error)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Write `<DataArray... NumberOfComponents="3"...>...</DataArray>` tag (R8P).
  !---------------------------------------------------------------------------------------------------------------------------------
  class(xml_writer_ascii_local), intent(inout)        :: self         !< Writer.
  character(*),                  intent(in)           :: data_name    !< Data name.
  real(R8P),                     intent(in)           :: x(1:)        !< X component of data variable.
  real(R8P),                     intent(in)           :: y(1:)        !< Y component of data variable.
  real(R8P),                     intent(in)           :: z(1:)        !< Z component of data variable.
  logical,                       intent(in), optional :: is_tuples    !< Use "NumberOfTuples" instead of "NumberOfComponents".
  integer(I4P)                                        :: error        !< Error status.
  character(len=:), allocatable                       :: data_type    !< Data type.
  integer(I4P)                                        :: n_components !< Number of components.
  character(len=:), allocatable                       :: code         !< Data variable encoded, ascii or Base64 codec.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  data_type = 'Float64'
  n_components = 3
  code = self%encode_ascii_dataarray(x=x, y=y, z=z)
  call self%write_dataarray_tag(data_type=data_type, number_of_components=n_components, data_name=data_name, data_content=code, &
                                is_tuples=is_tuples)
  error = self%error
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction write_dataarray3_rank1_R8P

  function write_dataarray3_rank1_R4P(self, data_name, x, y, z, is_tuples) result(error)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Write `<DataArray... NumberOfComponents="3"...>...</DataArray>` tag (R4P).
  !---------------------------------------------------------------------------------------------------------------------------------
  class(xml_writer_ascii_local), intent(inout)        :: self         !< Writer.
  character(*),                  intent(in)           :: data_name    !< Data name.
  real(R4P),                     intent(in)           :: x(1:)        !< X component of data variable.
  real(R4P),                     intent(in)           :: y(1:)        !< Y component of data variable.
  real(R4P),                     intent(in)           :: z(1:)        !< Z component of data variable.
  logical,                       intent(in), optional :: is_tuples    !< Use "NumberOfTuples" instead of "NumberOfComponents".
  integer(I4P)                                        :: error        !< Error status.
  character(len=:), allocatable                       :: data_type    !< Data type.
  integer(I4P)                                        :: n_components !< Number of components.
  character(len=:), allocatable                       :: code         !< Data variable encoded, ascii or Base64 codec.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  data_type = 'Float32'
  n_components = 3
  code = self%encode_ascii_dataarray(x=x, y=y, z=z)
  call self%write_dataarray_tag(data_type=data_type, number_of_components=n_components, data_name=data_name, data_content=code, &
                                is_tuples=is_tuples)
  error = self%error
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction write_dataarray3_rank1_R4P

  function write_dataarray3_rank1_I8P(self, data_name, x, y, z, is_tuples) result(error)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Write `<DataArray... NumberOfComponents="3"...>...</DataArray>` tag (I8P).
  !---------------------------------------------------------------------------------------------------------------------------------
  class(xml_writer_ascii_local), intent(inout)        :: self         !< Writer.
  character(*),                  intent(in)           :: data_name    !< Data name.
  integer(I8P),                  intent(in)           :: x(1:)        !< X component of data variable.
  integer(I8P),                  intent(in)           :: y(1:)        !< Y component of data variable.
  integer(I8P),                  intent(in)           :: z(1:)        !< Z component of data variable.
  logical,                       intent(in), optional :: is_tuples    !< Use "NumberOfTuples" instead of "NumberOfComponents".
  integer(I4P)                                        :: error        !< Error status.
  character(len=:), allocatable                       :: data_type    !< Data type.
  integer(I4P)                                        :: n_components !< Number of components.
  character(len=:), allocatable                       :: code         !< Data variable encoded, ascii or Base64 codec.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  data_type = 'Int64'
  n_components = 3
  code = self%encode_ascii_dataarray(x=x, y=y, z=z)
  call self%write_dataarray_tag(data_type=data_type, number_of_components=n_components, data_name=data_name, data_content=code, &
                                is_tuples=is_tuples)
  error = self%error
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction write_dataarray3_rank1_I8P

  function write_dataarray3_rank1_I4P(self, data_name, x, y, z, is_tuples) result(error)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Write `<DataArray... NumberOfComponents="3"...>...</DataArray>` tag (I4P).
  !---------------------------------------------------------------------------------------------------------------------------------
  class(xml_writer_ascii_local), intent(inout)        :: self         !< Writer.
  character(*),                  intent(in)           :: data_name    !< Data name.
  integer(I4P),                  intent(in)           :: x(1:)        !< X component of data variable.
  integer(I4P),                  intent(in)           :: y(1:)        !< Y component of data variable.
  integer(I4P),                  intent(in)           :: z(1:)        !< Z component of data variable.
  logical,                       intent(in), optional :: is_tuples    !< Use "NumberOfTuples" instead of "NumberOfComponents".
  integer(I4P)                                        :: error        !< Error status.
  character(len=:), allocatable                       :: data_type    !< Data type.
  integer(I4P)                                        :: n_components !< Number of components.
  character(len=:), allocatable                       :: code         !< Data variable encoded, ascii or Base64 codec.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  data_type = 'Int32'
  n_components = 3
  code = self%encode_ascii_dataarray(x=x, y=y, z=z)
  call self%write_dataarray_tag(data_type=data_type, number_of_components=n_components, data_name=data_name, data_content=code, &
                                is_tuples=is_tuples)
  error = self%error
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction write_dataarray3_rank1_I4P

  function write_dataarray3_rank1_I2P(self, data_name, x, y, z, is_tuples) result(error)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Write `<DataArray... NumberOfComponents="3"...>...</DataArray>` tag (I2P).
  !---------------------------------------------------------------------------------------------------------------------------------
  class(xml_writer_ascii_local), intent(inout)        :: self         !< Writer.
  character(*),                  intent(in)           :: data_name    !< Data name.
  integer(I2P),                  intent(in)           :: x(1:)        !< X component of data variable.
  integer(I2P),                  intent(in)           :: y(1:)        !< Y component of data variable.
  integer(I2P),                  intent(in)           :: z(1:)        !< Z component of data variable.
  logical,                       intent(in), optional :: is_tuples    !< Use "NumberOfTuples" instead of "NumberOfComponents".
  integer(I4P)                                        :: error        !< Error status.
  character(len=:), allocatable                       :: data_type    !< Data type.
  integer(I4P)                                        :: n_components !< Number of components.
  character(len=:), allocatable                       :: code         !< Data variable encoded, ascii or Base64 codec.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  data_type = 'Int16'
  n_components = 3
  code = self%encode_ascii_dataarray(x=x, y=y, z=z)
  call self%write_dataarray_tag(data_type=data_type, number_of_components=n_components, data_name=data_name, data_content=code, &
                                is_tuples=is_tuples)
  error = self%error
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction write_dataarray3_rank1_I2P

  function write_dataarray3_rank1_I1P(self, data_name, x, y, z, is_tuples) result(error)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Write `<DataArray... NumberOfComponents="3"...>...</DataArray>` tag (I1P).
  !---------------------------------------------------------------------------------------------------------------------------------
  class(xml_writer_ascii_local), intent(inout)        :: self         !< Writer.
  character(*),                  intent(in)           :: data_name    !< Data name.
  integer(I1P),                  intent(in)           :: x(1:)        !< X component of data variable.
  integer(I1P),                  intent(in)           :: y(1:)        !< Y component of data variable.
  integer(I1P),                  intent(in)           :: z(1:)        !< Z component of data variable.
  logical,                       intent(in), optional :: is_tuples    !< Use "NumberOfTuples" instead of "NumberOfComponents".
  integer(I4P)                                        :: error        !< Error status.
  character(len=:), allocatable                       :: data_type    !< Data type.
  integer(I4P)                                        :: n_components !< Number of components.
  character(len=:), allocatable                       :: code         !< Data variable encoded, ascii or Base64 codec.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  data_type = 'Int8'
  n_components = 3
  code = self%encode_ascii_dataarray(x=x, y=y, z=z)
  call self%write_dataarray_tag(data_type=data_type, number_of_components=n_components, data_name=data_name, data_content=code, &
                                is_tuples=is_tuples)
  error = self%error
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction write_dataarray3_rank1_I1P

  function write_dataarray3_rank3_R8P(self, data_name, x, y, z, is_tuples) result(error)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Write `<DataArray... NumberOfComponents="3"...>...</DataArray>` tag (R8P).
  !---------------------------------------------------------------------------------------------------------------------------------
  class(xml_writer_ascii_local), intent(inout)        :: self         !< Writer.
  character(*),                  intent(in)           :: data_name    !< Data name.
  real(R8P),                     intent(in)           :: x(1:,1:,1:)  !< X component of data variable.
  real(R8P),                     intent(in)           :: y(1:,1:,1:)  !< Y component of data variable.
  real(R8P),                     intent(in)           :: z(1:,1:,1:)  !< Z component of data variable.
  logical,                       intent(in), optional :: is_tuples    !< Use "NumberOfTuples" instead of "NumberOfComponents".
  integer(I4P)                                        :: error        !< Error status.
  character(len=:), allocatable                       :: data_type    !< Data type.
  integer(I4P)                                        :: n_components !< Number of components.
  character(len=:), allocatable                       :: code         !< Data variable encoded, ascii or Base64 codec.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  data_type = 'Float64'
  n_components = 3
  code = self%encode_ascii_dataarray(x=x, y=y, z=z)
  call self%write_dataarray_tag(data_type=data_type, number_of_components=n_components, data_name=data_name, data_content=code, &
                                is_tuples=is_tuples)
  error = self%error
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction write_dataarray3_rank3_R8P

  function write_dataarray3_rank3_R4P(self, data_name, x, y, z, is_tuples) result(error)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Write `<DataArray... NumberOfComponents="3"...>...</DataArray>` tag (R4P).
  !---------------------------------------------------------------------------------------------------------------------------------
  class(xml_writer_ascii_local), intent(inout)        :: self         !< Writer.
  character(*),                  intent(in)           :: data_name    !< Data name.
  real(R4P),                     intent(in)           :: x(1:,1:,1:)  !< X component of data variable.
  real(R4P),                     intent(in)           :: y(1:,1:,1:)  !< Y component of data variable.
  real(R4P),                     intent(in)           :: z(1:,1:,1:)  !< Z component of data variable.
  logical,                       intent(in), optional :: is_tuples    !< Use "NumberOfTuples" instead of "NumberOfComponents".
  integer(I4P)                                        :: error        !< Error status.
  character(len=:), allocatable                       :: data_type    !< Data type.
  integer(I4P)                                        :: n_components !< Number of components.
  character(len=:), allocatable                       :: code         !< Data variable encoded, ascii or Base64 codec.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  data_type = 'Float32'
  n_components = 3
  code = self%encode_ascii_dataarray(x=x, y=y, z=z)
  call self%write_dataarray_tag(data_type=data_type, number_of_components=n_components, data_name=data_name, data_content=code, &
                                is_tuples=is_tuples)
  error = self%error
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction write_dataarray3_rank3_R4P

  function write_dataarray3_rank3_I8P(self, data_name, x, y, z, is_tuples) result(error)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Write `<DataArray... NumberOfComponents="3"...>...</DataArray>` tag (I8P).
  !---------------------------------------------------------------------------------------------------------------------------------
  class(xml_writer_ascii_local), intent(inout)        :: self         !< Writer.
  character(*),                  intent(in)           :: data_name    !< Data name.
  integer(I8P),                  intent(in)           :: x(1:,1:,1:)  !< X component of data variable.
  integer(I8P),                  intent(in)           :: y(1:,1:,1:)  !< Y component of data variable.
  integer(I8P),                  intent(in)           :: z(1:,1:,1:)  !< Z component of data variable.
  logical,                       intent(in), optional :: is_tuples    !< Use "NumberOfTuples" instead of "NumberOfComponents".
  integer(I4P)                                        :: error        !< Error status.
  character(len=:), allocatable                       :: data_type    !< Data type.
  integer(I4P)                                        :: n_components !< Number of components.
  character(len=:), allocatable                       :: code         !< Data variable encoded, ascii or Base64 codec.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  data_type = 'Int64'
  n_components = 3
  code = self%encode_ascii_dataarray(x=x, y=y, z=z)
  call self%write_dataarray_tag(data_type=data_type, number_of_components=n_components, data_name=data_name, data_content=code, &
                                is_tuples=is_tuples)
  error = self%error
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction write_dataarray3_rank3_I8P

  function write_dataarray3_rank3_I4P(self, data_name, x, y, z, is_tuples) result(error)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Write `<DataArray... NumberOfComponents="3"...>...</DataArray>` tag (I4P).
  !---------------------------------------------------------------------------------------------------------------------------------
  class(xml_writer_ascii_local), intent(inout)        :: self         !< Writer.
  character(*),                  intent(in)           :: data_name    !< Data name.
  integer(I4P),                  intent(in)           :: x(1:,1:,1:)  !< X component of data variable.
  integer(I4P),                  intent(in)           :: y(1:,1:,1:)  !< Y component of data variable.
  integer(I4P),                  intent(in)           :: z(1:,1:,1:)  !< Z component of data variable.
  logical,                       intent(in), optional :: is_tuples    !< Use "NumberOfTuples" instead of "NumberOfComponents".
  integer(I4P)                                        :: error        !< Error status.
  character(len=:), allocatable                       :: data_type    !< Data type.
  integer(I4P)                                        :: n_components !< Number of components.
  character(len=:), allocatable                       :: code         !< Data variable encoded, ascii or Base64 codec.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  data_type = 'Int32'
  n_components = 3
  code = self%encode_ascii_dataarray(x=x, y=y, z=z)
  call self%write_dataarray_tag(data_type=data_type, number_of_components=n_components, data_name=data_name, data_content=code, &
                                is_tuples=is_tuples)
  error = self%error
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction write_dataarray3_rank3_I4P

  function write_dataarray3_rank3_I2P(self, data_name, x, y, z, is_tuples) result(error)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Write `<DataArray... NumberOfComponents="3"...>...</DataArray>` tag (I2P).
  !---------------------------------------------------------------------------------------------------------------------------------
  class(xml_writer_ascii_local), intent(inout)        :: self         !< Writer.
  character(*),                  intent(in)           :: data_name    !< Data name.
  integer(I2P),                  intent(in)           :: x(1:,1:,1:)  !< X component of data variable.
  integer(I2P),                  intent(in)           :: y(1:,1:,1:)  !< Y component of data variable.
  integer(I2P),                  intent(in)           :: z(1:,1:,1:)  !< Z component of data variable.
  logical,                       intent(in), optional :: is_tuples    !< Use "NumberOfTuples" instead of "NumberOfComponents".
  integer(I4P)                                        :: error        !< Error status.
  character(len=:), allocatable                       :: data_type    !< Data type.
  integer(I4P)                                        :: n_components !< Number of components.
  character(len=:), allocatable                       :: code         !< Data variable encoded, ascii or Base64 codec.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  data_type = 'Int16'
  n_components = 3
  code = self%encode_ascii_dataarray(x=x, y=y, z=z)
  call self%write_dataarray_tag(data_type=data_type, number_of_components=n_components, data_name=data_name, data_content=code, &
                                is_tuples=is_tuples)
  error = self%error
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction write_dataarray3_rank3_I2P

  function write_dataarray3_rank3_I1P(self, data_name, x, y, z, is_tuples) result(error)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Write `<DataArray... NumberOfComponents="3"...>...</DataArray>` tag (I1P).
  !---------------------------------------------------------------------------------------------------------------------------------
  class(xml_writer_ascii_local), intent(inout)        :: self         !< Writer.
  character(*),                  intent(in)           :: data_name    !< Data name.
  integer(I1P),                  intent(in)           :: x(1:,1:,1:)  !< X component of data variable.
  integer(I1P),                  intent(in)           :: y(1:,1:,1:)  !< Y component of data variable.
  integer(I1P),                  intent(in)           :: z(1:,1:,1:)  !< Z component of data variable.
  logical,                       intent(in), optional :: is_tuples    !< Use "NumberOfTuples" instead of "NumberOfComponents".
  integer(I4P)                                        :: error        !< Error status.
  character(len=:), allocatable                       :: data_type    !< Data type.
  integer(I4P)                                        :: n_components !< Number of components.
  character(len=:), allocatable                       :: code         !< Data variable encoded, ascii or Base64 codec.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  data_type = 'Int8'
  n_components = 3
  code = self%encode_ascii_dataarray(x=x, y=y, z=z)
  call self%write_dataarray_tag(data_type=data_type, number_of_components=n_components, data_name=data_name, data_content=code, &
                                is_tuples=is_tuples)
  error = self%error
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction write_dataarray3_rank3_I1P

  subroutine write_dataarray_appended(self)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Do nothing, ascii data cannot be appended.
  !---------------------------------------------------------------------------------------------------------------------------------
  class(xml_writer_ascii_local), intent(inout) :: self !< Writer.
  !---------------------------------------------------------------------------------------------------------------------------------
  endsubroutine write_dataarray_appended

  !< ascii encode
  function encode_ascii_dataarray1_rank1_R16P(x) result(code)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Encode (Base64) a dataarray with 1 components of rank 1 (R8P).
  !---------------------------------------------------------------------------------------------------------------------------------
  real(R16P),      intent(in)   :: x(1:) !< Data variable.
  character(len=:), allocatable :: code  !< Encoded base64 dataarray.
  integer(I4P)                  :: n     !< Counter.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  code = ''
  do n=1, size(x, dim=1)
    code = code//str(n=x(n))
  enddo
  return
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction encode_ascii_dataarray1_rank1_R16P

  function encode_ascii_dataarray1_rank1_R8P(x) result(code)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Encode (Base64) a dataarray with 1 components of rank 1 (R8P).
  !---------------------------------------------------------------------------------------------------------------------------------
  real(R8P),       intent(in)   :: x(1:) !< Data variable.
  character(len=:), allocatable :: code  !< Encoded base64 dataarray.
  integer(I4P)                  :: n     !< Counter.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  code = ''
  do n=1, size(x, dim=1)
    code = code//str(n=x(n))
  enddo
  return
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction encode_ascii_dataarray1_rank1_R8P

  function encode_ascii_dataarray1_rank1_R4P(x) result(code)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Encode (Base64) a dataarray with 1 components of rank 1 (R4P).
  !---------------------------------------------------------------------------------------------------------------------------------
  real(R4P),       intent(in)   :: x(1:) !< Data variable.
  character(len=:), allocatable :: code  !< Encoded base64 dataarray.
  integer(I4P)                  :: n     !< Counter.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  code = ''
  do n=1, size(x, dim=1)
    code = code//str(n=x(n))
  enddo
  return
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction encode_ascii_dataarray1_rank1_R4P

  function encode_ascii_dataarray1_rank1_I8P(x) result(code)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Encode (Base64) a dataarray with 1 components of rank 1 (I8P).
  !---------------------------------------------------------------------------------------------------------------------------------
  integer(I8P),    intent(in)   :: x(1:) !< Data variable.
  character(len=:), allocatable :: code  !< Encoded base64 dataarray.
  integer(I4P)                  :: n     !< Counter.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  code = ''
  do n=1, size(x, dim=1)
    code = code//str(n=x(n))
  enddo
  return
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction encode_ascii_dataarray1_rank1_I8P

  function encode_ascii_dataarray1_rank1_I4P(x) result(code)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Encode (Base64) a dataarray with 1 components of rank 1 (I4P).
  !---------------------------------------------------------------------------------------------------------------------------------
  integer(I4P),    intent(in)   :: x(1:) !< Data variable.
  character(len=:), allocatable :: code  !< Encoded base64 dataarray.
  integer(I4P)                  :: n     !< Counter.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  code = ''
  do n=1, size(x, dim=1)
    code = code//str(n=x(n))
  enddo
  return
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction encode_ascii_dataarray1_rank1_I4P

  function encode_ascii_dataarray1_rank1_I2P(x) result(code)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Encode (Base64) a dataarray with 1 components of rank 1 (I2P).
  !---------------------------------------------------------------------------------------------------------------------------------
  integer(I2P),    intent(in)   :: x(1:) !< Data variable.
  character(len=:), allocatable :: code  !< Encoded base64 dataarray.
  integer(I4P)                  :: n     !< Counter.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  code = ''
  do n=1, size(x, dim=1)
    code = code//str(n=x(n))
  enddo
  return
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction encode_ascii_dataarray1_rank1_I2P

  function encode_ascii_dataarray1_rank1_I1P(x) result(code)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Encode (Base64) a dataarray with 1 components of rank 1 (I1P).
  !---------------------------------------------------------------------------------------------------------------------------------
  integer(I1P),    intent(in)   :: x(1:) !< Data variable.
  character(len=:), allocatable :: code  !< Encoded base64 dataarray.
  integer(I4P)                  :: n     !< Counter.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  code = ''
  do n=1, size(x, dim=1)
    code = code//str(n=x(n))
  enddo
  return
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction encode_ascii_dataarray1_rank1_I1P

  function encode_ascii_dataarray1_rank2_R16P(x) result(code)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Encode (Base64) a dataarray with 1 components of rank 2 (R16P).
  !---------------------------------------------------------------------------------------------------------------------------------
  real(R16P),      intent(in)   :: x(1:,1:) !< Data variable
  character(len=:), allocatable :: code     !< Encoded base64 dataarray.
  integer(I4P)                  :: n1       !< Counter.
  integer(I4P)                  :: n2       !< Counter.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  code = ''
  do n2=1, size(x, dim=2)
    do n1=1, size(x, dim=1)-1
      code = code//str(n=x(n1, n2))//' '
    enddo
    code = code//' '//str(n=x(size(x, dim=1), n2))
  enddo
  return
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction encode_ascii_dataarray1_rank2_R16P

  function encode_ascii_dataarray1_rank2_R8P(x) result(code)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Encode (Base64) a dataarray with 1 components of rank 2 (R8P).
  !---------------------------------------------------------------------------------------------------------------------------------
  real(R8P),       intent(in)   :: x(1:,1:) !< Data variable
  character(len=:), allocatable :: code     !< Encoded base64 dataarray.
  integer(I4P)                  :: n1       !< Counter.
  integer(I4P)                  :: n2       !< Counter.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  code = ''
  do n2=1, size(x, dim=2)
    do n1=1, size(x, dim=1)-1
      code = code//str(n=x(n1, n2))//' '
    enddo
    code = code//' '//str(n=x(size(x, dim=1), n2))
  enddo
  return
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction encode_ascii_dataarray1_rank2_R8P

  function encode_ascii_dataarray1_rank2_R4P(x) result(code)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Encode (Base64) a dataarray with 1 components of rank 2 (R4P).
  !---------------------------------------------------------------------------------------------------------------------------------
  real(R4P),       intent(in)   :: x(1:,1:) !< Data variable
  character(len=:), allocatable :: code     !< Encoded base64 dataarray.
  integer(I4P)                  :: n1       !< Counter.
  integer(I4P)                  :: n2       !< Counter.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  code = ''
  do n2=1, size(x, dim=2)
    do n1=1, size(x, dim=1)
      code = code//str(n=x(n1, n2))//' '
    enddo
  enddo
  return
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction encode_ascii_dataarray1_rank2_R4P

  function encode_ascii_dataarray1_rank2_I8P(x) result(code)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Encode (Base64) a dataarray with 1 components of rank 2 (I8P).
  !---------------------------------------------------------------------------------------------------------------------------------
  integer(I8P),    intent(in)   :: x(1:,1:) !< Data variable
  character(len=:), allocatable :: code     !< Encoded base64 dataarray.
  integer(I4P)                  :: n1       !< Counter.
  integer(I4P)                  :: n2       !< Counter.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  code = ''
  do n2=1, size(x, dim=2)
    do n1=1, size(x, dim=1)-1
      code = code//str(n=x(n1, n2))//' '
    enddo
    code = code//' '//str(n=x(size(x, dim=1), n2))
  enddo
  return
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction encode_ascii_dataarray1_rank2_I8P

  function encode_ascii_dataarray1_rank2_I4P(x) result(code)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Encode (Base64) a dataarray with 1 components of rank 2 (I4P).
  !---------------------------------------------------------------------------------------------------------------------------------
  integer(I4P),    intent(in)   :: x(1:,1:) !< Data variable
  character(len=:), allocatable :: code     !< Encoded base64 dataarray.
  integer(I4P)                  :: n1       !< Counter.
  integer(I4P)                  :: n2       !< Counter.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  code = ''
  do n2=1, size(x, dim=2)
    do n1=1, size(x, dim=1)-1
      code = code//str(n=x(n1, n2))//' '
    enddo
    code = code//' '//str(n=x(size(x, dim=1), n2))
  enddo
  return
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction encode_ascii_dataarray1_rank2_I4P

  function encode_ascii_dataarray1_rank2_I2P(x) result(code)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Encode (Base64) a dataarray with 1 components of rank 2 (I2P).
  !---------------------------------------------------------------------------------------------------------------------------------
  integer(I2P),    intent(in)   :: x(1:,1:) !< Data variable
  character(len=:), allocatable :: code     !< Encoded base64 dataarray.
  integer(I4P)                  :: n1       !< Counter.
  integer(I4P)                  :: n2       !< Counter.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  code = ''
  do n2=1, size(x, dim=2)
    do n1=1, size(x, dim=1)-1
      code = code//str(n=x(n1, n2))//' '
    enddo
    code = code//' '//str(n=x(size(x, dim=1), n2))
  enddo
  return
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction encode_ascii_dataarray1_rank2_I2P

  function encode_ascii_dataarray1_rank2_I1P(x) result(code)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Encode (Base64) a dataarray with 1 components of rank 2 (I2P).
  !---------------------------------------------------------------------------------------------------------------------------------
  integer(I1P),    intent(in)   :: x(1:,1:) !< Data variable
  character(len=:), allocatable :: code     !< Encoded base64 dataarray.
  integer(I4P)                  :: n1       !< Counter.
  integer(I4P)                  :: n2       !< Counter.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  code = ''
  do n2=1, size(x, dim=2)
    do n1=1, size(x, dim=1)-1
      code = code//str(n=x(n1, n2))//' '
    enddo
    code = code//' '//str(n=x(size(x, dim=1), n2))
  enddo
  return
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction encode_ascii_dataarray1_rank2_I1P

  function encode_ascii_dataarray1_rank3_R16P(x) result(code)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Encode (Base64) a dataarray with 1 components of rank 3 (R16P).
  !---------------------------------------------------------------------------------------------------------------------------------
  real(R16P),      intent(in)   :: x(1:,1:,1:) !< Data variable
  character(len=:), allocatable :: code        !< Encoded base64 dataarray.
  integer(I4P)                  :: n1          !< Counter.
  integer(I4P)                  :: n2          !< Counter.
  integer(I4P)                  :: n3          !< Counter.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  code = ''
  do n3=1, size(x, dim=3)
    do n2=1, size(x, dim=2)
      do n1=1, size(x, dim=1)-1
        code = code//str(n=x(n1, n2, n3))//' '
      enddo
      code = code//' '//str(n=x(size(x, dim=1), n2, n3))
    enddo
  enddo
  return
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction encode_ascii_dataarray1_rank3_R16P

  function encode_ascii_dataarray1_rank3_R8P(x) result(code)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Encode (Base64) a dataarray with 1 components of rank 3 (R8P).
  !---------------------------------------------------------------------------------------------------------------------------------
  real(R8P),       intent(in)   :: x(1:,1:,1:) !< Data variable
  character(len=:), allocatable :: code        !< Encoded base64 dataarray.
  integer(I4P)                  :: n1          !< Counter.
  integer(I4P)                  :: n2          !< Counter.
  integer(I4P)                  :: n3          !< Counter.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  code = ''
  do n3=1, size(x, dim=3)
    do n2=1, size(x, dim=2)
      do n1=1, size(x, dim=1)-1
        code = code//str(n=x(n1, n2, n3))//' '
      enddo
      code = code//' '//str(n=x(size(x, dim=1), n2, n3))
    enddo
  enddo
  return
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction encode_ascii_dataarray1_rank3_R8P

  function encode_ascii_dataarray1_rank3_R4P(x) result(code)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Encode (Base64) a dataarray with 1 components of rank 3 (R4P).
  !---------------------------------------------------------------------------------------------------------------------------------
  real(R4P),       intent(in)   :: x(1:,1:,1:) !< Data variable
  character(len=:), allocatable :: code        !< Encoded base64 dataarray.
  integer(I4P)                  :: n1          !< Counter.
  integer(I4P)                  :: n2          !< Counter.
  integer(I4P)                  :: n3          !< Counter.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  code = ''
  do n3=1, size(x, dim=3)
    do n2=1, size(x, dim=2)
      do n1=1, size(x, dim=1)-1
        code = code//str(n=x(n1, n2, n3))//' '
      enddo
      code = code//' '//str(n=x(size(x, dim=1), n2, n3))
    enddo
  enddo
  return
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction encode_ascii_dataarray1_rank3_R4P

  function encode_ascii_dataarray1_rank3_I8P(x) result(code)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Encode (Base64) a dataarray with 1 components of rank 3 (I8P).
  !---------------------------------------------------------------------------------------------------------------------------------
  integer(I8P),    intent(in)   :: x(1:,1:,1:) !< Data variable
  character(len=:), allocatable :: code        !< Encoded base64 dataarray.
  integer(I4P)                  :: n1          !< Counter.
  integer(I4P)                  :: n2          !< Counter.
  integer(I4P)                  :: n3          !< Counter.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  code = ''
  do n3=1, size(x, dim=3)
    do n2=1, size(x, dim=2)
      do n1=1, size(x, dim=1)-1
        code = code//str(n=x(n1, n2, n3))//' '
      enddo
      code = code//' '//str(n=x(size(x, dim=1), n2, n3))
    enddo
  enddo
  return
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction encode_ascii_dataarray1_rank3_I8P

  function encode_ascii_dataarray1_rank3_I4P(x) result(code)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Encode (Base64) a dataarray with 1 components of rank 3 (I4P).
  !---------------------------------------------------------------------------------------------------------------------------------
  integer(I4P),    intent(in)   :: x(1:,1:,1:) !< Data variable
  character(len=:), allocatable :: code        !< Encoded base64 dataarray.
  integer(I4P)                  :: n1          !< Counter.
  integer(I4P)                  :: n2          !< Counter.
  integer(I4P)                  :: n3          !< Counter.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  code = ''
  do n3=1, size(x, dim=3)
    do n2=1, size(x, dim=2)
      do n1=1, size(x, dim=1)-1
        code = code//str(n=x(n1, n2, n3))//' '
      enddo
      code = code//' '//str(n=x(size(x, dim=1), n2, n3))
    enddo
  enddo
  return
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction encode_ascii_dataarray1_rank3_I4P

  function encode_ascii_dataarray1_rank3_I2P(x) result(code)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Encode (Base64) a dataarray with 1 components of rank 3 (I2P).
  !---------------------------------------------------------------------------------------------------------------------------------
  integer(I2P),    intent(in)   :: x(1:,1:,1:) !< Data variable
  character(len=:), allocatable :: code        !< Encoded base64 dataarray.
  integer(I4P)                  :: n1          !< Counter.
  integer(I4P)                  :: n2          !< Counter.
  integer(I4P)                  :: n3          !< Counter.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  code = ''
  do n3=1, size(x, dim=3)
    do n2=1, size(x, dim=2)
      do n1=1, size(x, dim=1)-1
        code = code//str(n=x(n1, n2, n3))//' '
      enddo
      code = code//' '//str(n=x(size(x, dim=1), n2, n3))
    enddo
  enddo
  return
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction encode_ascii_dataarray1_rank3_I2P

  function encode_ascii_dataarray1_rank3_I1P(x) result(code)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Encode (Base64) a dataarray with 1 components of rank 3 (I1P).
  !---------------------------------------------------------------------------------------------------------------------------------
  integer(I1P),    intent(in)   :: x(1:,1:,1:) !< Data variable
  character(len=:), allocatable :: code        !< Encoded base64 dataarray.
  integer(I4P)                  :: n1          !< Counter.
  integer(I4P)                  :: n2          !< Counter.
  integer(I4P)                  :: n3          !< Counter.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  code = ''
  do n3=1, size(x, dim=3)
    do n2=1, size(x, dim=2)
      do n1=1, size(x, dim=1)-1
        code = code//str(n=x(n1, n2, n3))//' '
      enddo
      code = code//' '//str(n=x(size(x, dim=1), n2, n3))
    enddo
  enddo
  return
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction encode_ascii_dataarray1_rank3_I1P

  function encode_ascii_dataarray1_rank4_R16P(x) result(code)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Encode (Base64) a dataarray with 1 components of rank 4 (R16P).
  !---------------------------------------------------------------------------------------------------------------------------------
  real(R16P),      intent(in)   :: x(1:,1:,1:,1:) !< Data variable.
  character(len=:), allocatable :: code           !< Encoded base64 dataarray.
  integer(I4P)                  :: n1             !< Counter.
  integer(I4P)                  :: n2             !< Counter.
  integer(I4P)                  :: n3             !< Counter.
  integer(I4P)                  :: n4             !< Counter.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  code = ''
  do n4=1, size(x, dim=4)
    do n3=1, size(x, dim=3)
      do n2=1, size(x, dim=2)
        do n1=1, size(x, dim=1)
          code = code//str(n=x(n1,n2,n3,n4))//' '
        enddo
      enddo
    enddo
  enddo
  return
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction encode_ascii_dataarray1_rank4_R16P

  function encode_ascii_dataarray1_rank4_R8P(x) result(code)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Encode (Base64) a dataarray with 1 components of rank 4 (R8P).
  !---------------------------------------------------------------------------------------------------------------------------------
  real(R8P),       intent(in)   :: x(1:,1:,1:,1:) !< Data variable.
  character(len=:), allocatable :: code           !< Encoded base64 dataarray.
  integer(I4P)                  :: n1             !< Counter.
  integer(I4P)                  :: n2             !< Counter.
  integer(I4P)                  :: n3             !< Counter.
  integer(I4P)                  :: n4             !< Counter.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  code = ''
  do n4=1, size(x, dim=4)
    do n3=1, size(x, dim=3)
      do n2=1, size(x, dim=2)
        do n1=1, size(x, dim=1)
          code = code//str(n=x(n1,n2,n3,n4))//' '
        enddo
      enddo
    enddo
  enddo
  return
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction encode_ascii_dataarray1_rank4_R8P

  function encode_ascii_dataarray1_rank4_R4P(x) result(code)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Encode (Base64) a dataarray with 1 components of rank 4 (R4P).
  !---------------------------------------------------------------------------------------------------------------------------------
  real(R4P),       intent(in)   :: x(1:,1:,1:,1:) !< Data variable.
  character(len=:), allocatable :: code           !< Encoded base64 dataarray.
  integer(I4P)                  :: n1             !< Counter.
  integer(I4P)                  :: n2             !< Counter.
  integer(I4P)                  :: n3             !< Counter.
  integer(I4P)                  :: n4             !< Counter.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  code = ''
  do n4=1, size(x, dim=4)
    do n3=1, size(x, dim=3)
      do n2=1, size(x, dim=2)
        do n1=1, size(x, dim=1)
          code = code//str(n=x(n1, n2, n3, n4))//' '
        enddo
      enddo
    enddo
  enddo
  return
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction encode_ascii_dataarray1_rank4_R4P

  function encode_ascii_dataarray1_rank4_I8P(x) result(code)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Encode (Base64) a dataarray with 1 components of rank 4 (I8P).
  !---------------------------------------------------------------------------------------------------------------------------------
  integer(I8P),    intent(in)   :: x(1:,1:,1:,1:) !< Data variable.
  character(len=:), allocatable :: code           !< Encoded base64 dataarray.
  integer(I4P)                  :: n1             !< Counter.
  integer(I4P)                  :: n2             !< Counter.
  integer(I4P)                  :: n3             !< Counter.
  integer(I4P)                  :: n4             !< Counter.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  code = ''
  do n4=1, size(x, dim=4)
    do n3=1, size(x, dim=3)
      do n2=1, size(x, dim=2)
        do n1=1, size(x, dim=1)
          code = code//str(n=x(n1,n2,n3,n4))//' '
        enddo
      enddo
    enddo
  enddo
  return
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction encode_ascii_dataarray1_rank4_I8P

  function encode_ascii_dataarray1_rank4_I4P(x) result(code)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Encode (Base64) a dataarray with 1 components of rank 4 (I4P).
  !---------------------------------------------------------------------------------------------------------------------------------
  integer(I4P),    intent(in)   :: x(1:,1:,1:,1:) !< Data variable.
  character(len=:), allocatable :: code           !< Encoded base64 dataarray.
  integer(I4P)                  :: n1             !< Counter.
  integer(I4P)                  :: n2             !< Counter.
  integer(I4P)                  :: n3             !< Counter.
  integer(I4P)                  :: n4             !< Counter.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  code = ''
  do n4=1, size(x, dim=4)
    do n3=1, size(x, dim=3)
      do n2=1, size(x, dim=2)
        do n1=1, size(x, dim=1)
          code = code//str(n=x(n1,n2,n3,n4))//' '
        enddo
      enddo
    enddo
  enddo
  return
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction encode_ascii_dataarray1_rank4_I4P

  function encode_ascii_dataarray1_rank4_I2P(x) result(code)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Encode (Base64) a dataarray with 1 components of rank 4 (I2P).
  !---------------------------------------------------------------------------------------------------------------------------------
  integer(I2P),    intent(in)   :: x(1:,1:,1:,1:) !< Data variable.
  character(len=:), allocatable :: code           !< Encoded base64 dataarray.
  integer(I4P)                  :: n1             !< Counter.
  integer(I4P)                  :: n2             !< Counter.
  integer(I4P)                  :: n3             !< Counter.
  integer(I4P)                  :: n4             !< Counter.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  code = ''
  do n4=1, size(x, dim=4)
    do n3=1, size(x, dim=3)
      do n2=1, size(x, dim=2)
        do n1=1, size(x, dim=1)
          code = code//str(n=x(n1,n2,n3,n4))//' '
        enddo
      enddo
    enddo
  enddo
  return
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction encode_ascii_dataarray1_rank4_I2P

  function encode_ascii_dataarray1_rank4_I1P(x) result(code)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Encode (Base64) a dataarray with 1 components of rank 4 (I1P).
  !---------------------------------------------------------------------------------------------------------------------------------
  integer(I1P),    intent(in)   :: x(1:,1:,1:,1:) !< Data variable.
  character(len=:), allocatable :: code           !< Encoded base64 dataarray.
  integer(I4P)                  :: n1             !< Counter.
  integer(I4P)                  :: n2             !< Counter.
  integer(I4P)                  :: n3             !< Counter.
  integer(I4P)                  :: n4             !< Counter.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  code = ''
  do n4=1, size(x, dim=4)
    do n3=1, size(x, dim=3)
      do n2=1, size(x, dim=2)
        do n1=1, size(x, dim=1)
          code = code//str(n=x(n1,n2,n3,n4))//' '
        enddo
      enddo
    enddo
  enddo
  return
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction encode_ascii_dataarray1_rank4_I1P

  function encode_ascii_dataarray3_rank1_R16P(x, y, z) result(code)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Encode (Base64) a dataarray with 3 components of rank 1 (R16P).
  !---------------------------------------------------------------------------------------------------------------------------------
  real(R16P),      intent(in)   :: x(1:) !< X component.
  real(R16P),      intent(in)   :: y(1:) !< Y component.
  real(R16P),      intent(in)   :: z(1:) !< Z component.
  character(len=:), allocatable :: code  !< Encoded base64 dataarray.
  integer(I4P)                  :: n     !< Counter.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  code = ''
  do n=1, size(x, dim=1)
    code = code//str(n=x(n))//' '//str(n=y(n))//' '//str(n=z(n))
  enddo
  return
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction encode_ascii_dataarray3_rank1_R16P

  function encode_ascii_dataarray3_rank1_R8P(x, y, z) result(code)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Encode (Base64) a dataarray with 3 components of rank 1 (R8P).
  !---------------------------------------------------------------------------------------------------------------------------------
  real(R8P),       intent(in)   :: x(1:) !< X component.
  real(R8P),       intent(in)   :: y(1:) !< Y component.
  real(R8P),       intent(in)   :: z(1:) !< Z component.
  character(len=:), allocatable :: code  !< Encoded base64 dataarray.
  integer(I4P)                  :: n     !< Counter.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  code = ''
  do n=1, size(x, dim=1)
    code = code//str(n=x(n))//' '//str(n=y(n))//' '//str(n=z(n))
  enddo
  return
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction encode_ascii_dataarray3_rank1_R8P

  function encode_ascii_dataarray3_rank1_R4P(x, y, z) result(code)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Encode (Base64) a dataarray with 3 components of rank 1 (R4P).
  !---------------------------------------------------------------------------------------------------------------------------------
  real(R4P),       intent(in)   :: x(1:) !< X component.
  real(R4P),       intent(in)   :: y(1:) !< Y component.
  real(R4P),       intent(in)   :: z(1:) !< Z component.
  character(len=:), allocatable :: code  !< Encoded base64 dataarray.
  integer(I4P)                  :: n     !< Counter.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  code = ''
  do n=1, size(x, dim=1)
    code = code//str(n=x(n))//' '//str(n=y(n))//' '//str(n=z(n))
  enddo
  return
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction encode_ascii_dataarray3_rank1_R4P

  function encode_ascii_dataarray3_rank1_I8P(x, y, z) result(code)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Encode (Base64) a dataarray with 3 components of rank 1 (I8P).
  !---------------------------------------------------------------------------------------------------------------------------------
  integer(I8P),    intent(in)   :: x(1:) !< X component.
  integer(I8P),    intent(in)   :: y(1:) !< Y component.
  integer(I8P),    intent(in)   :: z(1:) !< Z component.
  character(len=:), allocatable :: code  !< Encoded base64 dataarray.
  integer(I4P)                  :: n     !< Counter.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  code = ''
  do n=1, size(x, dim=1)
    code = code//str(n=x(n))//' '//str(n=y(n))//' '//str(n=z(n))
  enddo
  return
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction encode_ascii_dataarray3_rank1_I8P

  function encode_ascii_dataarray3_rank1_I4P(x, y, z) result(code)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Encode (Base64) a dataarray with 3 components of rank 1 (I4P).
  !---------------------------------------------------------------------------------------------------------------------------------
  integer(I4P),    intent(in)   :: x(1:) !< X component.
  integer(I4P),    intent(in)   :: y(1:) !< Y component.
  integer(I4P),    intent(in)   :: z(1:) !< Z component.
  character(len=:), allocatable :: code  !< Encoded base64 dataarray.
  integer(I4P)                  :: n     !< Counter.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  code = ''
  do n=1, size(x, dim=1)
    code = code//str(n=x(n))//' '//str(n=y(n))//' '//str(n=z(n))
  enddo
  return
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction encode_ascii_dataarray3_rank1_I4P

  function encode_ascii_dataarray3_rank1_I2P(x, y, z) result(code)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Encode (Base64) a dataarray with 3 components of rank 1 (I2P).
  !---------------------------------------------------------------------------------------------------------------------------------
  integer(I2P),    intent(in)   :: x(1:) !< X component.
  integer(I2P),    intent(in)   :: y(1:) !< Y component.
  integer(I2P),    intent(in)   :: z(1:) !< Z component.
  character(len=:), allocatable :: code  !< Encoded base64 dataarray.
  integer(I4P)                  :: n     !< Counter.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  code = ''
  do n=1, size(x, dim=1)
    code = code//str(n=x(n))//' '//str(n=y(n))//' '//str(n=z(n))
  enddo
  return
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction encode_ascii_dataarray3_rank1_I2P

  function encode_ascii_dataarray3_rank1_I1P(x, y, z) result(code)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Encode (Base64) a dataarray with 3 components of rank 1 (I1P).
  !---------------------------------------------------------------------------------------------------------------------------------
  integer(I1P),    intent(in)   :: x(1:) !< X component.
  integer(I1P),    intent(in)   :: y(1:) !< Y component.
  integer(I1P),    intent(in)   :: z(1:) !< Z component.
  character(len=:), allocatable :: code  !< Encoded base64 dataarray.
  integer(I4P)                  :: n     !< Counter.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  code = ''
  do n=1, size(x, dim=1)
    code = code//str(n=x(n))//' '//str(n=y(n))//' '//str(n=z(n))
  enddo
  return
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction encode_ascii_dataarray3_rank1_I1P

  function encode_ascii_dataarray3_rank3_R16P(x, y, z) result(code)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Encode (Base64) a dataarray with 3 components of rank 3 (R8P).
  !---------------------------------------------------------------------------------------------------------------------------------
  real(R16P),      intent(in)   :: x(1:,1:,1:) !< X component.
  real(R16P),      intent(in)   :: y(1:,1:,1:) !< Y component.
  real(R16P),      intent(in)   :: z(1:,1:,1:) !< Z component.
  character(len=:), allocatable :: code        !< Encoded base64 dataarray.
  integer(I4P)                  :: n1          !< Counter.
  integer(I4P)                  :: n2          !< Counter.
  integer(I4P)                  :: n3          !< Counter.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  code = ''
  do n3=1, size(x, dim=3)
    do n2=1, size(x, dim=2)
      do n1=1, size(x, dim=1)
        code = code//&
          str(n=x(n1, n2, n3))//' '//str(n=y(n1, n2, n3))//' '//str(n=z(n1, n2, n3))
      enddo
    enddo
  enddo
  return
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction encode_ascii_dataarray3_rank3_R16P

  function encode_ascii_dataarray3_rank3_R8P(x, y, z) result(code)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Encode (Base64) a dataarray with 3 components of rank 3 (R8P).
  !---------------------------------------------------------------------------------------------------------------------------------
  real(R8P),       intent(in)   :: x(1:,1:,1:) !< X component.
  real(R8P),       intent(in)   :: y(1:,1:,1:) !< Y component.
  real(R8P),       intent(in)   :: z(1:,1:,1:) !< Z component.
  character(len=:), allocatable :: code        !< Encoded base64 dataarray.
  integer(I4P)                  :: n1          !< Counter.
  integer(I4P)                  :: n2          !< Counter.
  integer(I4P)                  :: n3          !< Counter.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  code = ''
  do n3=1, size(x, dim=3)
    do n2=1, size(x, dim=2)
      do n1=1, size(x, dim=1)
        code = code//&
          str(n=x(n1, n2, n3))//' '//str(n=y(n1, n2, n3))//' '//str(n=z(n1, n2, n3))
      enddo
    enddo
  enddo
  return
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction encode_ascii_dataarray3_rank3_R8P

  function encode_ascii_dataarray3_rank3_R4P(x, y, z) result(code)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Encode (Base64) a dataarray with 3 components of rank 3 (R4P).
  !---------------------------------------------------------------------------------------------------------------------------------
  real(R4P),       intent(in)   :: x(1:,1:,1:) !< X component.
  real(R4P),       intent(in)   :: y(1:,1:,1:) !< Y component.
  real(R4P),       intent(in)   :: z(1:,1:,1:) !< Z component.
  character(len=:), allocatable :: code        !< Encoded base64 dataarray.
  integer(I4P)                  :: n1          !< Counter.
  integer(I4P)                  :: n2          !< Counter.
  integer(I4P)                  :: n3          !< Counter.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  code = ''
  do n3=1, size(x, dim=3)
    do n2=1, size(x, dim=2)
      do n1=1, size(x, dim=1)
        code = code//&
          str(n=x(n1, n2, n3))//' '//str(n=y(n1, n2, n3))//' '//str(n=z(n1, n2, n3))
      enddo
    enddo
  enddo
  return
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction encode_ascii_dataarray3_rank3_R4P

  function encode_ascii_dataarray3_rank3_I8P(x, y, z) result(code)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Encode (Base64) a dataarray with 3 components of rank 3 (I8P).
  !---------------------------------------------------------------------------------------------------------------------------------
  integer(I8P),    intent(in)   :: x(1:,1:,1:) !< X component.
  integer(I8P),    intent(in)   :: y(1:,1:,1:) !< Y component.
  integer(I8P),    intent(in)   :: z(1:,1:,1:) !< Z component.
  character(len=:), allocatable :: code        !< Encoded base64 dataarray.
  integer(I4P)                  :: n1          !< Counter.
  integer(I4P)                  :: n2          !< Counter.
  integer(I4P)                  :: n3          !< Counter.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  code = ''
  do n3=1, size(x, dim=3)
    do n2=1, size(x, dim=2)
      do n1=1, size(x, dim=1)
        code = code//&
          str(n=x(n1, n2, n3))//' '//str(n=y(n1, n2, n3))//' '//str(n=z(n1, n2, n3))
      enddo
    enddo
  enddo
  return
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction encode_ascii_dataarray3_rank3_I8P

  function encode_ascii_dataarray3_rank3_I4P(x, y, z) result(code)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Encode (Base64) a dataarray with 3 components of rank 3 (I4P).
  !---------------------------------------------------------------------------------------------------------------------------------
  integer(I4P),    intent(in)   :: x(1:,1:,1:) !< X component.
  integer(I4P),    intent(in)   :: y(1:,1:,1:) !< Y component.
  integer(I4P),    intent(in)   :: z(1:,1:,1:) !< Z component.
  character(len=:), allocatable :: code        !< Encoded base64 dataarray.
  integer(I4P)                  :: n1          !< Counter.
  integer(I4P)                  :: n2          !< Counter.
  integer(I4P)                  :: n3          !< Counter.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  code = ''
  do n3=1, size(x, dim=3)
    do n2=1, size(x, dim=2)
      do n1=1, size(x, dim=1)
        code = code//&
          str(n=x(n1, n2, n3))//' '//str(n=y(n1, n2, n3))//' '//str(n=z(n1, n2, n3))
      enddo
    enddo
  enddo
  return
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction encode_ascii_dataarray3_rank3_I4P

  function encode_ascii_dataarray3_rank3_I2P(x, y, z) result(code)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Encode (Base64) a dataarray with 3 components of rank 3 (I2P).
  !---------------------------------------------------------------------------------------------------------------------------------
  integer(I2P),    intent(in)   :: x(1:,1:,1:) !< X component.
  integer(I2P),    intent(in)   :: y(1:,1:,1:) !< Y component.
  integer(I2P),    intent(in)   :: z(1:,1:,1:) !< Z component.
  character(len=:), allocatable :: code        !< Encoded base64 dataarray.
  integer(I4P)                  :: n1          !< Counter.
  integer(I4P)                  :: n2          !< Counter.
  integer(I4P)                  :: n3          !< Counter.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  code = ''
  do n3=1, size(x, dim=3)
    do n2=1, size(x, dim=2)
      do n1=1, size(x, dim=1)
        code = code//&
          str(n=x(n1, n2, n3))//' '//str(n=y(n1, n2, n3))//' '//str(n=z(n1, n2, n3))
      enddo
    enddo
  enddo
  return
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction encode_ascii_dataarray3_rank3_I2P

  function encode_ascii_dataarray3_rank3_I1P(x, y, z) result(code)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Encode (Base64) a dataarray with 3 components of rank 3 (I1P).
  !---------------------------------------------------------------------------------------------------------------------------------
  integer(I1P),    intent(in)   :: x(1:,1:,1:) !< X component.
  integer(I1P),    intent(in)   :: y(1:,1:,1:) !< Y component.
  integer(I1P),    intent(in)   :: z(1:,1:,1:) !< Z component.
  character(len=:), allocatable :: code        !< Encoded base64 dataarray.
  integer(I4P)                  :: n1          !< Counter.
  integer(I4P)                  :: n2          !< Counter.
  integer(I4P)                  :: n3          !< Counter.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  code = ''
  do n3=1, size(x, dim=3)
    do n2=1, size(x, dim=2)
      do n1=1, size(x, dim=1)
        code = code//&
          str(n=x(n1, n2, n3))//' '//str(n=y(n1, n2, n3))//' '//str(n=z(n1, n2, n3))
      enddo
    enddo
  enddo
  return
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction encode_ascii_dataarray3_rank3_I1P
endmodule vtk_file_xml_writer_ascii_local
