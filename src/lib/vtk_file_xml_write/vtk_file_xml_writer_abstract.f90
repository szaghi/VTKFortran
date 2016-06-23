!< VTK file abstract XML writer.
module vtk_file_xml_writer_abstract
!-----------------------------------------------------------------------------------------------------------------------------------
!< VTK file abstract XML writer.
!-----------------------------------------------------------------------------------------------------------------------------------
use penf
use stringifor
use vtk_fortran_parameters
!-----------------------------------------------------------------------------------------------------------------------------------

!-----------------------------------------------------------------------------------------------------------------------------------
implicit none
private
public :: xml_writer_abstract
!-----------------------------------------------------------------------------------------------------------------------------------

!-----------------------------------------------------------------------------------------------------------------------------------
type, abstract :: xml_writer_abstract
  !< VTK file abstract XML writer.
  type(string) :: format_ch     !< Output format, string code.
  type(string) :: topology      !< Mesh topology.
  integer(I4P) :: indent=0_I4P  !< Indent count.
  integer(I8P) :: ioffset=0_I8P !< Offset count.
  integer(I4P) :: xml=0_I4P     !< XML Logical unit.
  integer(I4P) :: scratch=0_I4P !< Scratch logical unit.
  integer(I4P) :: error=0_I4P   !< IO Error status.
  contains
    ! deferred methods
    procedure(initialize_interface), deferred, pass(self) :: initialize !< Initialize writer.
    generic                                               :: write_dataarray =>            &
                                                             write_dataarray_location_tag, &
                                                             write_dataarray1_rank1_R8P,   &
                                                             write_dataarray1_rank1_R4P,   &
                                                             write_dataarray1_rank1_I8P,   &
                                                             write_dataarray1_rank1_I4P,   &
                                                             write_dataarray1_rank1_I2P,   &
                                                             write_dataarray1_rank1_I1P,   &
                                                             write_dataarray1_rank2_R8P,   &
                                                             write_dataarray1_rank2_R4P,   &
                                                             write_dataarray1_rank2_I8P,   &
                                                             write_dataarray1_rank2_I4P,   &
                                                             write_dataarray1_rank2_I2P,   &
                                                             write_dataarray1_rank2_I1P,   &
                                                             write_dataarray1_rank3_R8P,   &
                                                             write_dataarray1_rank3_R4P,   &
                                                             write_dataarray1_rank3_I8P,   &
                                                             write_dataarray1_rank3_I4P,   &
                                                             write_dataarray1_rank3_I2P,   &
                                                             write_dataarray1_rank3_I1P,   &
                                                             write_dataarray1_rank4_R8P,   &
                                                             write_dataarray1_rank4_R4P,   &
                                                             write_dataarray1_rank4_I8P,   &
                                                             write_dataarray1_rank4_I4P,   &
                                                             write_dataarray1_rank4_I2P,   &
                                                             write_dataarray1_rank4_I1P,   &
                                                             write_dataarray3_rank1_R8P,   &
                                                             write_dataarray3_rank1_R4P,   &
                                                             write_dataarray3_rank1_I8P,   &
                                                             write_dataarray3_rank1_I4P,   &
                                                             write_dataarray3_rank1_I2P,   &
                                                             write_dataarray3_rank1_I1P,   &
                                                             write_dataarray3_rank3_R8P,   &
                                                             write_dataarray3_rank3_R4P,   &
                                                             write_dataarray3_rank3_I8P,   &
                                                             write_dataarray3_rank3_I4P,   &
                                                             write_dataarray3_rank3_I2P,   &
                                                             write_dataarray3_rank3_I1P !< Write data (array).
    procedure(write_dataarray1_rank1_R8P_interface), deferred, pass(self) :: write_dataarray1_rank1_R8P !< Data 1, rank 1, R8P.
    procedure(write_dataarray1_rank1_R4P_interface), deferred, pass(self) :: write_dataarray1_rank1_R4P !< Data 1, rank 1, R4P.
    procedure(write_dataarray1_rank1_I8P_interface), deferred, pass(self) :: write_dataarray1_rank1_I8P !< Data 1, rank 1, I8P.
    procedure(write_dataarray1_rank1_I4P_interface), deferred, pass(self) :: write_dataarray1_rank1_I4P !< Data 1, rank 1, I4P.
    procedure(write_dataarray1_rank1_I2P_interface), deferred, pass(self) :: write_dataarray1_rank1_I2P !< Data 1, rank 1, I2P.
    procedure(write_dataarray1_rank1_I1P_interface), deferred, pass(self) :: write_dataarray1_rank1_I1P !< Data 1, rank 1, I1P.
    procedure(write_dataarray1_rank2_R8P_interface), deferred, pass(self) :: write_dataarray1_rank2_R8P !< Data 1, rank 2, R8P.
    procedure(write_dataarray1_rank2_R4P_interface), deferred, pass(self) :: write_dataarray1_rank2_R4P !< Data 1, rank 2, R4P.
    procedure(write_dataarray1_rank2_I8P_interface), deferred, pass(self) :: write_dataarray1_rank2_I8P !< Data 1, rank 2, I8P.
    procedure(write_dataarray1_rank2_I4P_interface), deferred, pass(self) :: write_dataarray1_rank2_I4P !< Data 1, rank 2, I4P.
    procedure(write_dataarray1_rank2_I2P_interface), deferred, pass(self) :: write_dataarray1_rank2_I2P !< Data 1, rank 2, I2P.
    procedure(write_dataarray1_rank2_I1P_interface), deferred, pass(self) :: write_dataarray1_rank2_I1P !< Data 1, rank 2, I1P.
    procedure(write_dataarray1_rank3_R8P_interface), deferred, pass(self) :: write_dataarray1_rank3_R8P !< Data 1, rank 3, R8P.
    procedure(write_dataarray1_rank3_R4P_interface), deferred, pass(self) :: write_dataarray1_rank3_R4P !< Data 1, rank 3, R4P.
    procedure(write_dataarray1_rank3_I8P_interface), deferred, pass(self) :: write_dataarray1_rank3_I8P !< Data 1, rank 3, I8P.
    procedure(write_dataarray1_rank3_I4P_interface), deferred, pass(self) :: write_dataarray1_rank3_I4P !< Data 1, rank 3, I4P.
    procedure(write_dataarray1_rank3_I2P_interface), deferred, pass(self) :: write_dataarray1_rank3_I2P !< Data 1, rank 3, I2P.
    procedure(write_dataarray1_rank3_I1P_interface), deferred, pass(self) :: write_dataarray1_rank3_I1P !< Data 1, rank 3, I1P.
    procedure(write_dataarray1_rank4_R8P_interface), deferred, pass(self) :: write_dataarray1_rank4_R8P !< Data 1, rank 4, R8P.
    procedure(write_dataarray1_rank4_R4P_interface), deferred, pass(self) :: write_dataarray1_rank4_R4P !< Data 1, rank 4, R4P.
    procedure(write_dataarray1_rank4_I8P_interface), deferred, pass(self) :: write_dataarray1_rank4_I8P !< Data 1, rank 4, I8P.
    procedure(write_dataarray1_rank4_I4P_interface), deferred, pass(self) :: write_dataarray1_rank4_I4P !< Data 1, rank 4, I4P.
    procedure(write_dataarray1_rank4_I2P_interface), deferred, pass(self) :: write_dataarray1_rank4_I2P !< Data 1, rank 4, I2P.
    procedure(write_dataarray1_rank4_I1P_interface), deferred, pass(self) :: write_dataarray1_rank4_I1P !< Data 1, rank 4, I1P.
    procedure(write_dataarray3_rank1_R8P_interface), deferred, pass(self) :: write_dataarray3_rank1_R8P !< Data 3, rank 1, R8P.
    procedure(write_dataarray3_rank1_R4P_interface), deferred, pass(self) :: write_dataarray3_rank1_R4P !< Data 3, rank 1, R4P.
    procedure(write_dataarray3_rank1_I8P_interface), deferred, pass(self) :: write_dataarray3_rank1_I8P !< Data 3, rank 1, I8P.
    procedure(write_dataarray3_rank1_I4P_interface), deferred, pass(self) :: write_dataarray3_rank1_I4P !< Data 3, rank 1, I4P.
    procedure(write_dataarray3_rank1_I2P_interface), deferred, pass(self) :: write_dataarray3_rank1_I2P !< Data 3, rank 1, I2P.
    procedure(write_dataarray3_rank1_I1P_interface), deferred, pass(self) :: write_dataarray3_rank1_I1P !< Data 3, rank 1, I1P.
    procedure(write_dataarray3_rank3_R8P_interface), deferred, pass(self) :: write_dataarray3_rank3_R8P !< Data 3, rank 3, R8P.
    procedure(write_dataarray3_rank3_R4P_interface), deferred, pass(self) :: write_dataarray3_rank3_R4P !< Data 3, rank 3, R4P.
    procedure(write_dataarray3_rank3_I8P_interface), deferred, pass(self) :: write_dataarray3_rank3_I8P !< Data 3, rank 3, I8P.
    procedure(write_dataarray3_rank3_I4P_interface), deferred, pass(self) :: write_dataarray3_rank3_I4P !< Data 3, rank 3, I4P.
    procedure(write_dataarray3_rank3_I2P_interface), deferred, pass(self) :: write_dataarray3_rank3_I2P !< Data 3, rank 3, I2P.
    procedure(write_dataarray3_rank3_I1P_interface), deferred, pass(self) :: write_dataarray3_rank3_I1P !< Data 3, rank 3, I1P.
    procedure(write_dataarray_appended_interface),   deferred, pass(self) :: write_dataarray_appended   !< Write appended.
    ! implemented methods
    procedure, pass(self)          :: write_dataarray_location_tag !< Write `<[/]PointData>` or `<[/]CellData>` open/close tag.
    procedure, pass(self)          :: write_dataarray_tag          !< Write `<DataArray...>...</DataArray>` tag.
    procedure, pass(self)          :: write_dataarray_tag_appended !< Write `<DataArray.../>` appended tag.
    procedure, pass(self)          :: open_xml_file                !< Open xml file.
    procedure, pass(self)          :: open_scratch_file            !< Open scratch file.
    procedure, pass(self)          :: self_closing_tag             !< Return `<tag_name.../>` self closing tag.
    procedure, pass(self)          :: tag                          !< Return `<tag_name...>...</tag_name>` tag.
    procedure, pass(self)          :: start_tag                    !< Return `<tag_name...>` start tag.
    procedure, pass(self)          :: end_tag                      !< Return `</tag_name>` end tag.
    procedure, pass(self)          :: write_self_closing_tag       !< Write `<tag_name.../>` self closing tag.
    procedure, pass(self)          :: write_tag                    !< Write `<tag_name...>...</tag_name>` tag.
    procedure, pass(self)          :: write_start_tag              !< Write `<tag_name...>` start tag.
    procedure, pass(self)          :: write_end_tag                !< Write `</tag_name>` end tag.
    procedure, pass(self)          :: write_header_tag             !< Write header tag.
    procedure, pass(self)          :: write_topology_tag           !< Write topology tag.
    generic                        :: write_fielddata =>          &
                                      write_fielddata1_rank0_R8P, &
                                      write_fielddata1_rank0_R4P, &
                                      write_fielddata1_rank0_I8P, &
                                      write_fielddata1_rank0_I4P, &
                                      write_fielddata1_rank0_I2P, &
                                      write_fielddata1_rank0_I1P, &
                                      write_fielddata_tag        !< Write FieldData tag.
    procedure, pass(self), private :: write_fielddata1_rank0_R8P !< Write FieldData tag (data 1, rank 0, R8P).
    procedure, pass(self), private :: write_fielddata1_rank0_R4P !< Write FieldData tag (data 1, rank 0, R4P).
    procedure, pass(self), private :: write_fielddata1_rank0_I8P !< Write FieldData tag (data 1, rank 0, I8P).
    procedure, pass(self), private :: write_fielddata1_rank0_I4P !< Write FieldData tag (data 1, rank 0, I4P).
    procedure, pass(self), private :: write_fielddata1_rank0_I2P !< Write FieldData tag (data 1, rank 0, I2P).
    procedure, pass(self), private :: write_fielddata1_rank0_I1P !< Write FieldData tag (data 1, rank 0, I1P).
    procedure, pass(self), private :: write_fielddata_tag        !< Write FieldData tag.
endtype xml_writer_abstract
!-----------------------------------------------------------------------------------------------------------------------------------

!-----------------------------------------------------------------------------------------------------------------------------------
abstract interface
  function initialize_interface(self, format, filename, mesh_topology, nx1, nx2, ny1, ny2, nz1, nz2) result(error)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Initialize writer.
  !---------------------------------------------------------------------------------------------------------------------------------
  import :: xml_writer_abstract, I4P
  class(xml_writer_abstract), intent(inout)         :: self          !< Writer.
  character(*),               intent(in)            :: format        !< File format: ASCII.
  character(*),               intent(in)            :: filename      !< File name.
  character(*),               intent(in)            :: mesh_topology !< Mesh topology.
  integer(I4P),               intent(in),  optional :: nx1           !< Initial node of x axis.
  integer(I4P),               intent(in),  optional :: nx2           !< Final node of x axis.
  integer(I4P),               intent(in),  optional :: ny1           !< Initial node of y axis.
  integer(I4P),               intent(in),  optional :: ny2           !< Final node of y axis.
  integer(I4P),               intent(in),  optional :: nz1           !< Initial node of z axis.
  integer(I4P),               intent(in),  optional :: nz2           !< Final node of z axis.
  integer(I4P)                                      :: error         !< Error status.
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction initialize_interface

  ! dataarray
  function write_dataarray1_rank1_R8P_interface(self, data_name, x, is_tuples) result(error)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Write `<DataArray... NumberOfComponents="1"...>...</DataArray>` tag (R8P).
  !---------------------------------------------------------------------------------------------------------------------------------
  import :: xml_writer_abstract, I4P, R8P
  class(xml_writer_abstract), intent(inout)        :: self         !< Writer.
  character(*),               intent(in)           :: data_name    !< Data name.
  real(R8P),                  intent(in)           :: x(1:)        !< Data variable.
  logical,                    intent(in), optional :: is_tuples    !< Use "NumberOfTuples" instead of "NumberOfComponents".
  integer(I4P)                                     :: error        !< Error status.
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction write_dataarray1_rank1_R8P_interface

  function write_dataarray1_rank1_R4P_interface(self, data_name, x, is_tuples) result(error)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Write `<DataArray... NumberOfComponents="1"...>...</DataArray>` tag (R4P).
  !---------------------------------------------------------------------------------------------------------------------------------
  import :: xml_writer_abstract, I4P, R4P
  class(xml_writer_abstract), intent(inout)        :: self         !< Writer.
  character(*),               intent(in)           :: data_name    !< Data name.
  real(R4P),                  intent(in)           :: x(1:)        !< Data variable.
  logical,                    intent(in), optional :: is_tuples    !< Use "NumberOfTuples" instead of "NumberOfComponents".
  integer(I4P)                                     :: error        !< Error status.
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction write_dataarray1_rank1_R4P_interface

  function write_dataarray1_rank1_I8P_interface(self, data_name, x, is_tuples) result(error)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Write `<DataArray... NumberOfComponents="1"...>...</DataArray>` tag (I8P).
  !---------------------------------------------------------------------------------------------------------------------------------
  import :: xml_writer_abstract, I4P, I8P
  class(xml_writer_abstract), intent(inout)        :: self         !< Writer.
  character(*),               intent(in)           :: data_name    !< Data name.
  integer(I8P),               intent(in)           :: x(1:)        !< Data variable.
  logical,                    intent(in), optional :: is_tuples    !< Use "NumberOfTuples" instead of "NumberOfComponents".
  integer(I4P)                                     :: error        !< Error status.
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction write_dataarray1_rank1_I8P_interface

  function write_dataarray1_rank1_I4P_interface(self, data_name, x, is_tuples) result(error)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Write `<DataArray... NumberOfComponents="1"...>...</DataArray>` tag (I4P).
  !---------------------------------------------------------------------------------------------------------------------------------
  import :: xml_writer_abstract, I4P
  class(xml_writer_abstract), intent(inout)        :: self         !< Writer.
  character(*),               intent(in)           :: data_name    !< Data name.
  integer(I4P),               intent(in)           :: x(1:)        !< Data variable.
  logical,                    intent(in), optional :: is_tuples    !< Use "NumberOfTuples" instead of "NumberOfComponents".
  integer(I4P)                                     :: error        !< Error status.
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction write_dataarray1_rank1_I4P_interface

  function write_dataarray1_rank1_I2P_interface(self, data_name, x, is_tuples) result(error)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Write `<DataArray... NumberOfComponents="1"...>...</DataArray>` tag (I2P).
  !---------------------------------------------------------------------------------------------------------------------------------
  import :: xml_writer_abstract, I2P, I4P
  class(xml_writer_abstract), intent(inout)        :: self         !< Writer.
  character(*),               intent(in)           :: data_name    !< Data name.
  integer(I2P),               intent(in)           :: x(1:)        !< Data variable.
  logical,                    intent(in), optional :: is_tuples    !< Use "NumberOfTuples" instead of "NumberOfComponents".
  integer(I4P)                                     :: error        !< Error status.
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction write_dataarray1_rank1_I2P_interface

  function write_dataarray1_rank1_I1P_interface(self, data_name, x, is_tuples) result(error)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Write `<DataArray... NumberOfComponents="1"...>...</DataArray>` tag (I1P).
  !---------------------------------------------------------------------------------------------------------------------------------
  import :: xml_writer_abstract, I1P, I4P
  class(xml_writer_abstract), intent(inout)        :: self         !< Writer.
  character(*),               intent(in)           :: data_name    !< Data name.
  integer(I1P),               intent(in)           :: x(1:)        !< Data variable.
  logical,                    intent(in), optional :: is_tuples    !< Use "NumberOfTuples" instead of "NumberOfComponents".
  integer(I4P)                                     :: error        !< Error status.
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction write_dataarray1_rank1_I1P_interface

  function write_dataarray1_rank2_R8P_interface(self, data_name, x, one_component, is_tuples) result(error)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Write `<DataArray... NumberOfComponents="n"...>...</DataArray>` tag (R8P).
  !---------------------------------------------------------------------------------------------------------------------------------
  import :: xml_writer_abstract, I4P, R8P
  class(xml_writer_abstract), intent(inout)        :: self          !< Writer.
  character(*),               intent(in)           :: data_name     !< Data name.
  real(R8P),                  intent(in)           :: x(1:,1:)      !< Data variable.
  logical,                    intent(in), optional :: one_component !< Force one component.
  logical,                    intent(in), optional :: is_tuples     !< Use "NumberOfTuples" instead of "NumberOfComponents".
  integer(I4P)                                     :: error         !< Error status.
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction write_dataarray1_rank2_R8P_interface

  function write_dataarray1_rank2_R4P_interface(self, data_name, x, one_component, is_tuples) result(error)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Write `<DataArray... NumberOfComponents="n"...>...</DataArray>` tag (R4P).
  !---------------------------------------------------------------------------------------------------------------------------------
  import :: xml_writer_abstract, I4P, R4P
  class(xml_writer_abstract), intent(inout)        :: self          !< Writer.
  character(*),               intent(in)           :: data_name     !< Data name.
  real(R4P),                  intent(in)           :: x(1:,1:)      !< Data variable.
  logical,                    intent(in), optional :: one_component !< Force one component.
  logical,                    intent(in), optional :: is_tuples     !< Use "NumberOfTuples" instead of "NumberOfComponents".
  integer(I4P)                                     :: error         !< Error status.
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction write_dataarray1_rank2_R4P_interface

  function write_dataarray1_rank2_I8P_interface(self, data_name, x, one_component, is_tuples) result(error)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Write `<DataArray... NumberOfComponents="n"...>...</DataArray>` tag (I8P).
  !---------------------------------------------------------------------------------------------------------------------------------
  import :: xml_writer_abstract, I4P, I8P
  class(xml_writer_abstract), intent(inout)        :: self          !< Writer.
  character(*),               intent(in)           :: data_name     !< Data name.
  integer(I8P),               intent(in)           :: x(1:,1:)      !< Data variable.
  logical,                    intent(in), optional :: one_component !< Force one component.
  logical,                    intent(in), optional :: is_tuples     !< Use "NumberOfTuples" instead of "NumberOfComponents".
  integer(I4P)                                     :: error         !< Error status.
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction write_dataarray1_rank2_I8P_interface

  function write_dataarray1_rank2_I4P_interface(self, data_name, x, one_component, is_tuples) result(error)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Write `<DataArray... NumberOfComponents="n"...>...</DataArray>` tag (I4P).
  !---------------------------------------------------------------------------------------------------------------------------------
  import :: xml_writer_abstract, I4P
  class(xml_writer_abstract), intent(inout)        :: self          !< Writer.
  character(*),               intent(in)           :: data_name     !< Data name.
  integer(I4P),               intent(in)           :: x(1:,1:)      !< Data variable.
  logical,                    intent(in), optional :: one_component !< Force one component.
  logical,                    intent(in), optional :: is_tuples     !< Use "NumberOfTuples" instead of "NumberOfComponents".
  integer(I4P)                                     :: error         !< Error status.
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction write_dataarray1_rank2_I4P_interface

  function write_dataarray1_rank2_I2P_interface(self, data_name, x, one_component, is_tuples) result(error)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Write `<DataArray... NumberOfComponents="n"...>...</DataArray>` tag (I2P).
  !---------------------------------------------------------------------------------------------------------------------------------
  import :: xml_writer_abstract, I2P, I4P
  class(xml_writer_abstract), intent(inout)        :: self          !< Writer.
  character(*),               intent(in)           :: data_name     !< Data name.
  integer(I2P),               intent(in)           :: x(1:,1:)      !< Data variable.
  logical,                    intent(in), optional :: one_component !< Force one component.
  logical,                    intent(in), optional :: is_tuples     !< Use "NumberOfTuples" instead of "NumberOfComponents".
  integer(I4P)                                     :: error         !< Error status.
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction write_dataarray1_rank2_I2P_interface

  function write_dataarray1_rank2_I1P_interface(self, data_name, x, one_component, is_tuples) result(error)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Write `<DataArray... NumberOfComponents="n"...>...</DataArray>` tag (I1P).
  !---------------------------------------------------------------------------------------------------------------------------------
  import :: xml_writer_abstract, I1P, I4P
  class(xml_writer_abstract), intent(inout)        :: self          !< Writer.
  character(*),               intent(in)           :: data_name     !< Data name.
  integer(I1P),               intent(in)           :: x(1:,1:)      !< Data variable.
  logical,                    intent(in), optional :: one_component !< Force one component.
  logical,                    intent(in), optional :: is_tuples     !< Use "NumberOfTuples" instead of "NumberOfComponents".
  integer(I4P)                                     :: error         !< Error status.
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction write_dataarray1_rank2_I1P_interface

  function write_dataarray1_rank3_R8P_interface(self, data_name, x, one_component, is_tuples) result(error)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Write `<DataArray... NumberOfComponents="n"...>...</DataArray>` tag (R8P).
  !---------------------------------------------------------------------------------------------------------------------------------
  import :: xml_writer_abstract, I4P, R8P
  class(xml_writer_abstract), intent(inout)        :: self          !< Writer.
  character(*),               intent(in)           :: data_name     !< Data name.
  real(R8P),                  intent(in)           :: x(1:,1:,1:)   !< Data variable.
  logical,                    intent(in), optional :: one_component !< Force one component.
  logical,                    intent(in), optional :: is_tuples     !< Use "NumberOfTuples" instead of "NumberOfComponents".
  integer(I4P)                                     :: error         !< Error status.
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction write_dataarray1_rank3_R8P_interface

  function write_dataarray1_rank3_R4P_interface(self, data_name, x, one_component, is_tuples) result(error)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Write `<DataArray... NumberOfComponents="n"...>...</DataArray>` tag (R4P).
  !---------------------------------------------------------------------------------------------------------------------------------
  import :: xml_writer_abstract, I4P, R4P
  class(xml_writer_abstract), intent(inout)        :: self          !< Writer.
  character(*),               intent(in)           :: data_name     !< Data name.
  real(R4P),                  intent(in)           :: x(1:,1:,1:)   !< Data variable.
  logical,                    intent(in), optional :: one_component !< Force one component.
  logical,                    intent(in), optional :: is_tuples     !< Use "NumberOfTuples" instead of "NumberOfComponents".
  integer(I4P)                                     :: error         !< Error status.
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction write_dataarray1_rank3_R4P_interface

  function write_dataarray1_rank3_I8P_interface(self, data_name, x, one_component, is_tuples) result(error)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Write `<DataArray... NumberOfComponents="n"...>...</DataArray>` tag (I8P).
  !---------------------------------------------------------------------------------------------------------------------------------
  import :: xml_writer_abstract, I4P, I8P
  class(xml_writer_abstract), intent(inout)        :: self          !< Writer.
  character(*),               intent(in)           :: data_name     !< Data name.
  integer(I8P),               intent(in)           :: x(1:,1:,1:)   !< Data variable.
  logical,                    intent(in), optional :: one_component !< Force one component.
  logical,                    intent(in), optional :: is_tuples     !< Use "NumberOfTuples" instead of "NumberOfComponents".
  integer(I4P)                                     :: error         !< Error status.
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction write_dataarray1_rank3_I8P_interface

  function write_dataarray1_rank3_I4P_interface(self, data_name, x, one_component, is_tuples) result(error)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Write `<DataArray... NumberOfComponents="n"...>...</DataArray>` tag (I4P).
  !---------------------------------------------------------------------------------------------------------------------------------
  import :: xml_writer_abstract, I4P
  class(xml_writer_abstract), intent(inout)        :: self          !< Writer.
  character(*),               intent(in)           :: data_name     !< Data name.
  integer(I4P),               intent(in)           :: x(1:,1:,1:)   !< Data variable.
  logical,                    intent(in), optional :: one_component !< Force one component.
  logical,                    intent(in), optional :: is_tuples     !< Use "NumberOfTuples" instead of "NumberOfComponents".
  integer(I4P)                                     :: error         !< Error status.
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction write_dataarray1_rank3_I4P_interface

  function write_dataarray1_rank3_I2P_interface(self, data_name, x, one_component, is_tuples) result(error)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Write `<DataArray... NumberOfComponents="n"...>...</DataArray>` tag (I2P).
  !---------------------------------------------------------------------------------------------------------------------------------
  import :: xml_writer_abstract, I2P, I4P
  class(xml_writer_abstract), intent(inout)        :: self          !< Writer.
  character(*),               intent(in)           :: data_name     !< Data name.
  integer(I2P),               intent(in)           :: x(1:,1:,1:)   !< Data variable.
  logical,                    intent(in), optional :: one_component !< Force one component.
  logical,                    intent(in), optional :: is_tuples     !< Use "NumberOfTuples" instead of "NumberOfComponents".
  integer(I4P)                                     :: error         !< Error status.
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction write_dataarray1_rank3_I2P_interface

  function write_dataarray1_rank3_I1P_interface(self, data_name, x, one_component, is_tuples) result(error)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Write `<DataArray... NumberOfComponents="n"...>...</DataArray>` tag (I1P).
  !---------------------------------------------------------------------------------------------------------------------------------
  import :: xml_writer_abstract, I1P, I4P
  class(xml_writer_abstract), intent(inout)        :: self          !< Writer.
  character(*),               intent(in)           :: data_name     !< Data name.
  integer(I1P),               intent(in)           :: x(1:,1:,1:)   !< Data variable.
  logical,                    intent(in), optional :: one_component !< Force one component.
  logical,                    intent(in), optional :: is_tuples     !< Use "NumberOfTuples" instead of "NumberOfComponents".
  integer(I4P)                                     :: error         !< Error status.
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction write_dataarray1_rank3_I1P_interface

  function write_dataarray1_rank4_R8P_interface(self, data_name, x, one_component, is_tuples) result(error)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Write `<DataArray... NumberOfComponents="n"...>...</DataArray>` tag (R8P).
  !---------------------------------------------------------------------------------------------------------------------------------
  import :: xml_writer_abstract, I4P, R8P
  class(xml_writer_abstract), intent(inout)        :: self           !< Writer.
  character(*),               intent(in)           :: data_name      !< Data name.
  real(R8P),                  intent(in)           :: x(1:,1:,1:,1:) !< Data variable.
  logical,                    intent(in), optional :: one_component  !< Force one component.
  logical,                    intent(in), optional :: is_tuples      !< Use "NumberOfTuples" instead of "NumberOfComponents".
  integer(I4P)                                     :: error          !< Error status.
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction write_dataarray1_rank4_R8P_interface

  function write_dataarray1_rank4_R4P_interface(self, data_name, x, one_component, is_tuples) result(error)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Write `<DataArray... NumberOfComponents="n"...>...</DataArray>` tag (R4P).
  !---------------------------------------------------------------------------------------------------------------------------------
  import :: xml_writer_abstract, I4P, R4P
  class(xml_writer_abstract), intent(inout)        :: self           !< Writer.
  character(*),               intent(in)           :: data_name      !< Data name.
  real(R4P),                  intent(in)           :: x(1:,1:,1:,1:) !< Data variable.
  logical,                    intent(in), optional :: one_component  !< Force one component.
  logical,                    intent(in), optional :: is_tuples      !< Use "NumberOfTuples" instead of "NumberOfComponents".
  integer(I4P)                                     :: error          !< Error status.
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction write_dataarray1_rank4_R4P_interface

  function write_dataarray1_rank4_I8P_interface(self, data_name, x, one_component, is_tuples) result(error)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Write `<DataArray... NumberOfComponents="n"...>...</DataArray>` tag (I8P).
  !---------------------------------------------------------------------------------------------------------------------------------
  import :: xml_writer_abstract, I4P, I8P
  class(xml_writer_abstract), intent(inout)        :: self           !< Writer.
  character(*),               intent(in)           :: data_name      !< Data name.
  integer(I8P),               intent(in)           :: x(1:,1:,1:,1:) !< Data variable.
  logical,                    intent(in), optional :: one_component  !< Force one component.
  logical,                    intent(in), optional :: is_tuples      !< Use "NumberOfTuples" instead of "NumberOfComponents".
  integer(I4P)                                     :: error          !< Error status.
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction write_dataarray1_rank4_I8P_interface

  function write_dataarray1_rank4_I4P_interface(self, data_name, x, one_component, is_tuples) result(error)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Write `<DataArray... NumberOfComponents="n"...>...</DataArray>` tag (I4P).
  !---------------------------------------------------------------------------------------------------------------------------------
  import :: xml_writer_abstract, I4P
  class(xml_writer_abstract), intent(inout)        :: self           !< Writer.
  character(*),               intent(in)           :: data_name      !< Data name.
  integer(I4P),               intent(in)           :: x(1:,1:,1:,1:) !< Data variable.
  logical,                    intent(in), optional :: one_component  !< Force one component.
  logical,                    intent(in), optional :: is_tuples      !< Use "NumberOfTuples" instead of "NumberOfComponents".
  integer(I4P)                                     :: error          !< Error status.
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction write_dataarray1_rank4_I4P_interface

  function write_dataarray1_rank4_I2P_interface(self, data_name, x, one_component, is_tuples) result(error)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Write `<DataArray... NumberOfComponents="n"...>...</DataArray>` tag (I2P).
  !---------------------------------------------------------------------------------------------------------------------------------
  import :: xml_writer_abstract, I2P, I4P
  class(xml_writer_abstract), intent(inout)        :: self           !< Writer.
  character(*),               intent(in)           :: data_name      !< Data name.
  integer(I2P),               intent(in)           :: x(1:,1:,1:,1:) !< Data variable.
  logical,                    intent(in), optional :: one_component  !< Force one component.
  logical,                    intent(in), optional :: is_tuples      !< Use "NumberOfTuples" instead of "NumberOfComponents".
  integer(I4P)                                     :: error          !< Error status.
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction write_dataarray1_rank4_I2P_interface

  function write_dataarray1_rank4_I1P_interface(self, data_name, x, one_component, is_tuples) result(error)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Write `<DataArray... NumberOfComponents="n"...>...</DataArray>` tag (I1P).
  !---------------------------------------------------------------------------------------------------------------------------------
  import :: xml_writer_abstract, I1P, I4P
  class(xml_writer_abstract), intent(inout)        :: self           !< Writer.
  character(*),               intent(in)           :: data_name      !< Data name.
  integer(I1P),               intent(in)           :: x(1:,1:,1:,1:) !< Data variable.
  logical,                    intent(in), optional :: one_component  !< Force one component.
  logical,                    intent(in), optional :: is_tuples      !< Use "NumberOfTuples" instead of "NumberOfComponents".
  integer(I4P)                                     :: error          !< Error status.
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction write_dataarray1_rank4_I1P_interface

  function write_dataarray3_rank1_R8P_interface(self, data_name, x, y, z, is_tuples) result(error)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Write `<DataArray... NumberOfComponents="3"...>...</DataArray>` tag (R8P).
  !---------------------------------------------------------------------------------------------------------------------------------
  import :: xml_writer_abstract, I4P, R8P
  class(xml_writer_abstract), intent(inout)        :: self         !< Writer.
  character(*),               intent(in)           :: data_name    !< Data name.
  real(R8P),                  intent(in)           :: x(1:)        !< X component of data variable.
  real(R8P),                  intent(in)           :: y(1:)        !< Y component of data variable.
  real(R8P),                  intent(in)           :: z(1:)        !< Z component of data variable.
  logical,                    intent(in), optional :: is_tuples    !< Use "NumberOfTuples" instead of "NumberOfComponents".
  integer(I4P)                                     :: error        !< Error status.
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction write_dataarray3_rank1_R8P_interface

  function write_dataarray3_rank1_R4P_interface(self, data_name, x, y, z, is_tuples) result(error)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Write `<DataArray... NumberOfComponents="3"...>...</DataArray>` tag (R4P).
  !---------------------------------------------------------------------------------------------------------------------------------
  import :: xml_writer_abstract, I4P, R4P
  class(xml_writer_abstract), intent(inout)        :: self         !< Writer.
  character(*),               intent(in)           :: data_name    !< Data name.
  real(R4P),                  intent(in)           :: x(1:)        !< X component of data variable.
  real(R4P),                  intent(in)           :: y(1:)        !< Y component of data variable.
  real(R4P),                  intent(in)           :: z(1:)        !< Z component of data variable.
  logical,                    intent(in), optional :: is_tuples    !< Use "NumberOfTuples" instead of "NumberOfComponents".
  integer(I4P)                                     :: error        !< Error status.
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction write_dataarray3_rank1_R4P_interface

  function write_dataarray3_rank1_I8P_interface(self, data_name, x, y, z, is_tuples) result(error)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Write `<DataArray... NumberOfComponents="3"...>...</DataArray>` tag (I8P).
  !---------------------------------------------------------------------------------------------------------------------------------
  import :: xml_writer_abstract, I4P, I8P
  class(xml_writer_abstract), intent(inout)        :: self         !< Writer.
  character(*),               intent(in)           :: data_name    !< Data name.
  integer(I8P),               intent(in)           :: x(1:)        !< X component of data variable.
  integer(I8P),               intent(in)           :: y(1:)        !< Y component of data variable.
  integer(I8P),               intent(in)           :: z(1:)        !< Z component of data variable.
  logical,                    intent(in), optional :: is_tuples    !< Use "NumberOfTuples" instead of "NumberOfComponents".
  integer(I4P)                                     :: error        !< Error status.
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction write_dataarray3_rank1_I8P_interface

  function write_dataarray3_rank1_I4P_interface(self, data_name, x, y, z, is_tuples) result(error)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Write `<DataArray... NumberOfComponents="3"...>...</DataArray>` tag (I4P).
  !---------------------------------------------------------------------------------------------------------------------------------
  import :: xml_writer_abstract, I4P
  class(xml_writer_abstract), intent(inout)        :: self         !< Writer.
  character(*),               intent(in)           :: data_name    !< Data name.
  integer(I4P),               intent(in)           :: x(1:)        !< X component of data variable.
  integer(I4P),               intent(in)           :: y(1:)        !< Y component of data variable.
  integer(I4P),               intent(in)           :: z(1:)        !< Z component of data variable.
  logical,                    intent(in), optional :: is_tuples    !< Use "NumberOfTuples" instead of "NumberOfComponents".
  integer(I4P)                                     :: error        !< Error status.
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction write_dataarray3_rank1_I4P_interface

  function write_dataarray3_rank1_I2P_interface(self, data_name, x, y, z, is_tuples) result(error)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Write `<DataArray... NumberOfComponents="3"...>...</DataArray>` tag (I2P).
  !---------------------------------------------------------------------------------------------------------------------------------
  import :: xml_writer_abstract, I2P, I4P
  class(xml_writer_abstract), intent(inout)        :: self         !< Writer.
  character(*),               intent(in)           :: data_name    !< Data name.
  integer(I2P),               intent(in)           :: x(1:)        !< X component of data variable.
  integer(I2P),               intent(in)           :: y(1:)        !< Y component of data variable.
  integer(I2P),               intent(in)           :: z(1:)        !< Z component of data variable.
  logical,                    intent(in), optional :: is_tuples    !< Use "NumberOfTuples" instead of "NumberOfComponents".
  integer(I4P)                                     :: error        !< Error status.
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction write_dataarray3_rank1_I2P_interface

  function write_dataarray3_rank1_I1P_interface(self, data_name, x, y, z, is_tuples) result(error)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Write `<DataArray... NumberOfComponents="3"...>...</DataArray>` tag (I1P).
  !---------------------------------------------------------------------------------------------------------------------------------
  import :: xml_writer_abstract, I1P, I4P
  class(xml_writer_abstract), intent(inout)        :: self         !< Writer.
  character(*),               intent(in)           :: data_name    !< Data name.
  integer(I1P),               intent(in)           :: x(1:)        !< X component of data variable.
  integer(I1P),               intent(in)           :: y(1:)        !< Y component of data variable.
  integer(I1P),               intent(in)           :: z(1:)        !< Z component of data variable.
  logical,                    intent(in), optional :: is_tuples    !< Use "NumberOfTuples" instead of "NumberOfComponents".
  integer(I4P)                                     :: error        !< Error status.
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction write_dataarray3_rank1_I1P_interface

  function write_dataarray3_rank3_R8P_interface(self, data_name, x, y, z, is_tuples) result(error)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Write `<DataArray... NumberOfComponents="3"...>...</DataArray>` tag (R8P).
  !---------------------------------------------------------------------------------------------------------------------------------
  import :: xml_writer_abstract, I4P, R8P
  class(xml_writer_abstract), intent(inout)        :: self         !< Writer.
  character(*),               intent(in)           :: data_name    !< Data name.
  real(R8P),                  intent(in)           :: x(1:,1:,1:)  !< X component of data variable.
  real(R8P),                  intent(in)           :: y(1:,1:,1:)  !< Y component of data variable.
  real(R8P),                  intent(in)           :: z(1:,1:,1:)  !< Z component of data variable.
  logical,                    intent(in), optional :: is_tuples    !< Use "NumberOfTuples" instead of "NumberOfComponents".
  integer(I4P)                                     :: error        !< Error status.
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction write_dataarray3_rank3_R8P_interface

  function write_dataarray3_rank3_R4P_interface(self, data_name, x, y, z, is_tuples) result(error)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Write `<DataArray... NumberOfComponents="3"...>...</DataArray>` tag (R4P).
  !---------------------------------------------------------------------------------------------------------------------------------
  import :: xml_writer_abstract, I4P, R4P
  class(xml_writer_abstract), intent(inout)        :: self         !< Writer.
  character(*),               intent(in)           :: data_name    !< Data name.
  real(R4P),                  intent(in)           :: x(1:,1:,1:)  !< X component of data variable.
  real(R4P),                  intent(in)           :: y(1:,1:,1:)  !< Y component of data variable.
  real(R4P),                  intent(in)           :: z(1:,1:,1:)  !< Z component of data variable.
  logical,                    intent(in), optional :: is_tuples    !< Use "NumberOfTuples" instead of "NumberOfComponents".
  integer(I4P)                                     :: error        !< Error status.
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction write_dataarray3_rank3_R4P_interface

  function write_dataarray3_rank3_I8P_interface(self, data_name, x, y, z, is_tuples) result(error)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Write `<DataArray... NumberOfComponents="3"...>...</DataArray>` tag (I8P).
  !---------------------------------------------------------------------------------------------------------------------------------
  import :: xml_writer_abstract, I4P, I8P
  class(xml_writer_abstract), intent(inout)        :: self         !< Writer.
  character(*),               intent(in)           :: data_name    !< Data name.
  integer(I8P),               intent(in)           :: x(1:,1:,1:)  !< X component of data variable.
  integer(I8P),               intent(in)           :: y(1:,1:,1:)  !< Y component of data variable.
  integer(I8P),               intent(in)           :: z(1:,1:,1:)  !< Z component of data variable.
  logical,                    intent(in), optional :: is_tuples    !< Use "NumberOfTuples" instead of "NumberOfComponents".
  integer(I4P)                                     :: error        !< Error status.
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction write_dataarray3_rank3_I8P_interface

  function write_dataarray3_rank3_I4P_interface(self, data_name, x, y, z, is_tuples) result(error)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Write `<DataArray... NumberOfComponents="3"...>...</DataArray>` tag (I4P).
  !---------------------------------------------------------------------------------------------------------------------------------
  import :: xml_writer_abstract, I4P
  class(xml_writer_abstract), intent(inout)        :: self         !< Writer.
  character(*),               intent(in)           :: data_name    !< Data name.
  integer(I4P),               intent(in)           :: x(1:,1:,1:)  !< X component of data variable.
  integer(I4P),               intent(in)           :: y(1:,1:,1:)  !< Y component of data variable.
  integer(I4P),               intent(in)           :: z(1:,1:,1:)  !< Z component of data variable.
  logical,                    intent(in), optional :: is_tuples    !< Use "NumberOfTuples" instead of "NumberOfComponents".
  integer(I4P)                                     :: error        !< Error status.
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction write_dataarray3_rank3_I4P_interface

  function write_dataarray3_rank3_I2P_interface(self, data_name, x, y, z, is_tuples) result(error)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Write `<DataArray... NumberOfComponents="3"...>...</DataArray>` tag (I2P).
  !---------------------------------------------------------------------------------------------------------------------------------
  import :: xml_writer_abstract, I2P, I4P
  class(xml_writer_abstract), intent(inout)        :: self         !< Writer.
  character(*),               intent(in)           :: data_name    !< Data name.
  integer(I2P),               intent(in)           :: x(1:,1:,1:)  !< X component of data variable.
  integer(I2P),               intent(in)           :: y(1:,1:,1:)  !< Y component of data variable.
  integer(I2P),               intent(in)           :: z(1:,1:,1:)  !< Z component of data variable.
  logical,                    intent(in), optional :: is_tuples    !< Use "NumberOfTuples" instead of "NumberOfComponents".
  integer(I4P)                                     :: error        !< Error status.
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction write_dataarray3_rank3_I2P_interface

  function write_dataarray3_rank3_I1P_interface(self, data_name, x, y, z, is_tuples) result(error)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Write `<DataArray... NumberOfComponents="3"...>...</DataArray>` tag (I1P).
  !---------------------------------------------------------------------------------------------------------------------------------
  import :: xml_writer_abstract, I1P, I4P
  class(xml_writer_abstract), intent(inout)        :: self         !< Writer.
  character(*),               intent(in)           :: data_name    !< Data name.
  integer(I1P),               intent(in)           :: x(1:,1:,1:)  !< X component of data variable.
  integer(I1P),               intent(in)           :: y(1:,1:,1:)  !< Y component of data variable.
  integer(I1P),               intent(in)           :: z(1:,1:,1:)  !< Z component of data variable.
  logical,                    intent(in), optional :: is_tuples    !< Use "NumberOfTuples" instead of "NumberOfComponents".
  integer(I4P)                                     :: error        !< Error status.
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction write_dataarray3_rank3_I1P_interface

  subroutine write_dataarray_appended_interface(self)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Write `<AppendedData...>...</AppendedData>` tag.
  !---------------------------------------------------------------------------------------------------------------------------------
  import :: xml_writer_abstract
  class(xml_writer_abstract), intent(inout) :: self !< Writer.
  !---------------------------------------------------------------------------------------------------------------------------------
  endsubroutine write_dataarray_appended_interface
endinterface
!-----------------------------------------------------------------------------------------------------------------------------------
contains
  ! files methods
  subroutine open_xml_file(self, filename)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Open XML file.
  !---------------------------------------------------------------------------------------------------------------------------------
  class(xml_writer_abstract), intent(inout) :: self     !< Writer.
  character(*),               intent(in)    :: filename !< File name.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  open(newunit=self%xml,             &
       file=trim(adjustl(filename)), &
       form='UNFORMATTED',           &
       access='STREAM',              &
       action='WRITE',               &
       status='REPLACE',             &
       iostat=self%error)
  return
  !---------------------------------------------------------------------------------------------------------------------------------
  endsubroutine open_xml_file

  subroutine open_scratch_file(self)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Open scratch file.
  !---------------------------------------------------------------------------------------------------------------------------------
  class(xml_writer_abstract), intent(inout) :: self !< Writer.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  open(newunit=self%scratch, &
       form='UNFORMATTED',   &
       access='STREAM',      &
       action='READWRITE',   &
       status='SCRATCH',     &
       iostat=self%error)
  return
  !---------------------------------------------------------------------------------------------------------------------------------
  endsubroutine open_scratch_file

  ! tag methods
  elemental function self_closing_tag(self, tag_name, tag_attributes) result(tag_)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Return `<tag_name.../>` self closing tag.
  !---------------------------------------------------------------------------------------------------------------------------------
  class(xml_writer_abstract), intent(in)           :: self           !< Writer.
  character(*),               intent(in)           :: tag_name       !< Tag name.
  character(*),               intent(in), optional :: tag_attributes !< Tag attributes.
  type(string)                                     :: tag_           !< The tag.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  if (present(tag_attributes)) then
    tag_ = repeat(' ', self%indent)//'<'//trim(adjustl(tag_name))//' '//trim(adjustl(tag_attributes))//'/>'//end_rec
  else
    tag_ = repeat(' ', self%indent)//'<'//trim(adjustl(tag_name))//'/>'//end_rec
  endif
  return
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction self_closing_tag

  elemental function tag(self, tag_name, tag_attributes, tag_content) result(tag_)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Return `<tag_name...>...</tag_name>` tag.
  !---------------------------------------------------------------------------------------------------------------------------------
  class(xml_writer_abstract), intent(in)           :: self           !< Writer.
  character(*),               intent(in)           :: tag_name       !< Tag name.
  character(*),               intent(in), optional :: tag_attributes !< Tag attributes.
  character(*),               intent(in), optional :: tag_content    !< Tag content.
  type(string)                                     :: tag_           !< The tag.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  tag_ = self%start_tag(tag_name=tag_name, tag_attributes=tag_attributes)
  if (present(tag_content)) tag_ = tag_//repeat(' ', self%indent+2)//tag_content//end_rec
  tag_ = tag_//self%end_tag(tag_name=tag_name)
  return
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction tag

  elemental function start_tag(self, tag_name, tag_attributes) result(tag_)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Return `<tag_name...>` start tag.
  !---------------------------------------------------------------------------------------------------------------------------------
  class(xml_writer_abstract), intent(in)           :: self           !< Writer.
  character(*),               intent(in)           :: tag_name       !< Tag name.
  character(*),               intent(in), optional :: tag_attributes !< Tag attributes.
  type(string)                                     :: tag_           !< The tag.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  if (present(tag_attributes)) then
    tag_ = repeat(' ', self%indent)//'<'//trim(adjustl(tag_name))//' '//trim(adjustl(tag_attributes))//'>'//end_rec
  else
    tag_ = repeat(' ', self%indent)//'<'//trim(adjustl(tag_name))//'>'//end_rec
  endif
  return
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction start_tag

  elemental function end_tag(self, tag_name) result(tag_)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Return `</tag_name>` end tag.
  !---------------------------------------------------------------------------------------------------------------------------------
  class(xml_writer_abstract), intent(in) :: self     !< Writer.
  character(*),               intent(in) :: tag_name !< Tag name.
  type(string)                           :: tag_     !< The tag.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  tag_ = repeat(' ', self%indent)//'</'//trim(adjustl(tag_name))//'>'//end_rec
  return
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction end_tag

  subroutine write_self_closing_tag(self, tag_name, tag_attributes)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Write `<tag_name.../>` self closing tag.
  !---------------------------------------------------------------------------------------------------------------------------------
  class(xml_writer_abstract), intent(inout)        :: self           !< Writer.
  character(*),               intent(in)           :: tag_name       !< Tag name.
  character(*),               intent(in), optional :: tag_attributes !< Tag attributes.
  type(string)                                     :: tag            !< The tag.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  tag = self%self_closing_tag(tag_name=tag_name, tag_attributes=tag_attributes)
  write(unit=self%xml, iostat=self%error)tag%chars()
  return
  !---------------------------------------------------------------------------------------------------------------------------------
  endsubroutine write_self_closing_tag

  subroutine write_tag(self, tag_name, tag_attributes, tag_content)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Write `<tag_name...>...</tag_name>` tag.
  !---------------------------------------------------------------------------------------------------------------------------------
  class(xml_writer_abstract), intent(inout)        :: self           !< Writer.
  character(*),               intent(in)           :: tag_name       !< Tag name.
  character(*),               intent(in), optional :: tag_attributes !< Tag attributes.
  character(*),               intent(in), optional :: tag_content    !< Tag content.
  type(string)                                     :: tag            !< The tag.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  tag = self%tag(tag_name=tag_name, tag_attributes=tag_attributes, tag_content=tag_content)
  write(unit=self%xml, iostat=self%error)tag%chars()
  return
  !---------------------------------------------------------------------------------------------------------------------------------
  endsubroutine write_tag

  subroutine write_start_tag(self, tag_name, tag_attributes)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Write `<tag_name...>` start tag.
  !---------------------------------------------------------------------------------------------------------------------------------
  class(xml_writer_abstract), intent(inout)        :: self           !< Writer.
  character(*),               intent(in)           :: tag_name       !< Tag name.
  character(*),               intent(in), optional :: tag_attributes !< Tag attributes.
  type(string)                                     :: tag            !< The tag.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  tag = self%start_tag(tag_name=tag_name, tag_attributes=tag_attributes)
  write(unit=self%xml, iostat=self%error)tag%chars()
  self%indent = self%indent + 2
  return
  !---------------------------------------------------------------------------------------------------------------------------------
  endsubroutine write_start_tag

  subroutine write_end_tag(self, tag_name)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Write `</tag_name>` end tag.
  !---------------------------------------------------------------------------------------------------------------------------------
  class(xml_writer_abstract), intent(inout) :: self     !< Writer.
  character(*),               intent(in)    :: tag_name !< Tag name.
  type(string)                              :: tag      !< The tag.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  self%indent = self%indent - 2
  tag = self%end_tag(tag_name=tag_name)
  write(unit=self%xml, iostat=self%error)tag%chars()
  return
  !---------------------------------------------------------------------------------------------------------------------------------
  endsubroutine write_end_tag

  subroutine write_header_tag(self)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Write header tag.
  !---------------------------------------------------------------------------------------------------------------------------------
  class(xml_writer_abstract), intent(inout) :: self   !< Writer.
  type(string)                              :: buffer !< Buffer string.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  buffer = '<?xml version="1.0"?>'//end_rec
  if (endian==endianL) then
    buffer = buffer//'<VTKFile type="'//self%topology//'" version="0.1" byte_order="LittleEndian">'
  else
    buffer = buffer//'<VTKFile type="'//self%topology//'" version="0.1" byte_order="BigEndian">'
  endif
  write(unit=self%xml, iostat=self%error)buffer//end_rec
  self%indent = 2
  return
  !---------------------------------------------------------------------------------------------------------------------------------
  endsubroutine write_header_tag

  subroutine write_topology_tag(self, nx1, nx2, ny1, ny2, nz1, nz2)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Write XML topology tag.
  !---------------------------------------------------------------------------------------------------------------------------------
  class(xml_writer_abstract), intent(inout)         :: self   !< Writer.
  integer(I4P),               intent(in),  optional :: nx1    !< Initial node of x axis.
  integer(I4P),               intent(in),  optional :: nx2    !< Final node of x axis.
  integer(I4P),               intent(in),  optional :: ny1    !< Initial node of y axis.
  integer(I4P),               intent(in),  optional :: ny2    !< Final node of y axis.
  integer(I4P),               intent(in),  optional :: nz1    !< Initial node of z axis.
  integer(I4P),               intent(in),  optional :: nz2    !< Final node of z axis.
  type(string)                                      :: buffer !< Buffer string.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  select case(self%topology%chars())
  case('RectilinearGrid', 'StructuredGrid')
    buffer = repeat(' ', self%indent)//'<'//self%topology//' WholeExtent="'//&
             trim(str(n=nx1))//' '//trim(str(n=nx2))//' '//                  &
             trim(str(n=ny1))//' '//trim(str(n=ny2))//' '//                  &
             trim(str(n=nz1))//' '//trim(str(n=nz2))//'">'
  case('UnstructuredGrid')
    buffer = repeat(' ', self%indent)//'<'//self%topology//'>'
  endselect
  write(unit=self%xml, iostat=self%error)buffer//end_rec
  self%indent = self%indent + 2
  return
  !---------------------------------------------------------------------------------------------------------------------------------
  endsubroutine write_topology_tag

  ! write_dataarray
  subroutine write_dataarray_tag(self, data_type, number_of_components, data_name, data_content, is_tuples)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Write `<DataArray...>...</DataArray>` tag.
  !---------------------------------------------------------------------------------------------------------------------------------
  class(xml_writer_abstract), intent(inout)        :: self                 !< Writer.
  character(*),               intent(in)           :: data_type            !< Type of dataarray.
  integer(I4P),               intent(in)           :: number_of_components !< Number of dataarray components.
  character(*),               intent(in)           :: data_name            !< Data name.
  character(*),               intent(in), optional :: data_content         !< Data content.
  logical,                    intent(in), optional :: is_tuples            !< Use "NumberOfTuples".
  type(string)                                     :: tag_attributes       !< Tag attributes.
  logical                                          :: is_tuples_           !< Use "NumberOfTuples".
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
  class(xml_writer_abstract), intent(inout)        :: self                 !< Writer.
  character(*),               intent(in)           :: data_type            !< Type of dataarray.
  integer(I4P),               intent(in)           :: number_of_components !< Number of dataarray components.
  character(*),               intent(in)           :: data_name            !< Data name.
  logical,                    intent(in), optional :: is_tuples            !< Use "NumberOfTuples".
  type(string)                                     :: tag_attributes       !< Tag attributes.
  logical                                          :: is_tuples_           !< Use "NumberOfTuples".
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
  !< error = vtk%write_dataarray('node','OPeN')
  !<```
  !<
  !<#### Closing node piece
  !<```fortran
  !< error = vtk%write_dataarray('node','Close')
  !<```
  !<
  !<#### Opening cell piece
  !<```fortran
  !< error = vtk%write_dataarray('cell','OPEN')
  !<```
  !<
  !<#### Closing cell piece
  !<```fortran
  !< error = vtk%write_dataarray('cell','close')
  !<```
  !---------------------------------------------------------------------------------------------------------------------------------
  class(xml_writer_abstract), intent(inout) :: self      !< Writer.
  character(*),               intent(in)    :: location  !< Location of variables: **cell** or **node** centered.
  character(*),               intent(in)    :: action    !< Action: **open** or **close** tag.
  integer(I4P)                              :: error     !< Error status.
  type(string)                              :: location_ !< Location string.
  type(string)                              :: action_   !< Action string.
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

  ! write_fielddata methods
  function write_fielddata1_rank0_R8P(self, data_name, x) result(error)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Write `<DataArray... NumberOfTuples="..."...>...</DataArray>` tag (R8P).
  !---------------------------------------------------------------------------------------------------------------------------------
  class(xml_writer_abstract), intent(inout) :: self      !< Writer.
  character(*),               intent(in)    :: data_name !< Data name.
  real(R8P),                  intent(in)    :: x         !< Data variable.
  integer(I4P)                              :: error     !< Error status.
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
  class(xml_writer_abstract), intent(inout) :: self      !< Writer.
  character(*),               intent(in)    :: data_name !< Data name.
  real(R4P),                  intent(in)    :: x         !< Data variable.
  integer(I4P)                              :: error     !< Error status.
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
  class(xml_writer_abstract), intent(inout) :: self      !< Writer.
  character(*),               intent(in)    :: data_name !< Data name.
  integer(I8P),               intent(in)    :: x         !< Data variable.
  integer(I4P)                              :: error     !< Error status.
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
  class(xml_writer_abstract), intent(inout) :: self      !< Writer.
  character(*),               intent(in)    :: data_name !< Data name.
  integer(I4P),               intent(in)    :: x         !< Data variable.
  integer(I4P)                              :: error     !< Error status.
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
  class(xml_writer_abstract), intent(inout) :: self      !< Writer.
  character(*),               intent(in)    :: data_name !< Data name.
  integer(I2P),               intent(in)    :: x         !< Data variable.
  integer(I4P)                              :: error     !< Error status.
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
  class(xml_writer_abstract), intent(inout) :: self      !< Writer.
  character(*),               intent(in)    :: data_name !< Data name.
  integer(I1P),               intent(in)    :: x         !< Data variable.
  integer(I4P)                              :: error     !< Error status.
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
  class(xml_writer_abstract), intent(inout) :: self      !< Writer.
  character(*),               intent(in)    :: action    !< Action: **open** or **close** tag.
  integer(I4P)                              :: error     !< Error status.
  type(string)                              :: action_   !< Action string.
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
endmodule vtk_file_xml_writer_abstract
