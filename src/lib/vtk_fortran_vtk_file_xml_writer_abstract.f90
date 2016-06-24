!< VTK file abstract XML writer.
module vtk_fortran_vtk_file_xml_writer_abstract
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
  integer(I4P) :: error=0_I4P   !< IO Error status.
  contains
    ! public methods (some deferred)
    procedure,                                 pass(self) :: end_tag                      !< Return `</tag_name>` end tag.
    procedure,                                 pass(self) :: open_xml_file                !< Open xml file.
    procedure,                                 pass(self) :: self_closing_tag             !< Return self closing tag.
    procedure,                                 pass(self) :: start_tag                    !< Return start tag.
    procedure,                                 pass(self) :: tag                          !< Return tag.
    procedure,                                 pass(self) :: write_connectivity           !< Write connectivity.
    procedure,                                 pass(self) :: write_dataarray_location_tag !< Write dataarray location tag.
    procedure,                                 pass(self) :: write_dataarray_tag          !< Write dataarray tag.
    procedure,                                 pass(self) :: write_dataarray_tag_appended !< Write dataarray appended tag.
    procedure,                                 pass(self) :: write_end_tag                !< Write `</tag_name>` end tag.
    procedure,                                 pass(self) :: write_header_tag             !< Write header tag.
    procedure,                                 pass(self) :: write_self_closing_tag       !< Write self closing tag.
    procedure,                                 pass(self) :: write_start_tag              !< Write start tag.
    procedure,                                 pass(self) :: write_tag                    !< Write tag.
    procedure,                                 pass(self) :: write_topology_tag           !< Write topology tag.
    procedure(initialize_interface), deferred, pass(self) :: initialize                   !< Initialize writer.
    procedure(finalize_interface),   deferred, pass(self) :: finalize                     !< Finalize writer.
    generic :: write_dataarray =>          &
               write_dataarray1_rank1_R8P, &
               write_dataarray1_rank1_R4P, &
               write_dataarray1_rank1_I8P, &
               write_dataarray1_rank1_I4P, &
               write_dataarray1_rank1_I2P, &
               write_dataarray1_rank1_I1P, &
               write_dataarray1_rank2_R8P, &
               write_dataarray1_rank2_R4P, &
               write_dataarray1_rank2_I8P, &
               write_dataarray1_rank2_I4P, &
               write_dataarray1_rank2_I2P, &
               write_dataarray1_rank2_I1P, &
               write_dataarray1_rank3_R8P, &
               write_dataarray1_rank3_R4P, &
               write_dataarray1_rank3_I8P, &
               write_dataarray1_rank3_I4P, &
               write_dataarray1_rank3_I2P, &
               write_dataarray1_rank3_I1P, &
               write_dataarray1_rank4_R8P, &
               write_dataarray1_rank4_R4P, &
               write_dataarray1_rank4_I8P, &
               write_dataarray1_rank4_I4P, &
               write_dataarray1_rank4_I2P, &
               write_dataarray1_rank4_I1P, &
               write_dataarray3_rank1_R8P, &
               write_dataarray3_rank1_R4P, &
               write_dataarray3_rank1_I8P, &
               write_dataarray3_rank1_I4P, &
               write_dataarray3_rank1_I2P, &
               write_dataarray3_rank1_I1P, &
               write_dataarray3_rank3_R8P, &
               write_dataarray3_rank3_R4P, &
               write_dataarray3_rank3_I8P, &
               write_dataarray3_rank3_I4P, &
               write_dataarray3_rank3_I2P, &
               write_dataarray3_rank3_I1P, &
               write_dataarray_location_tag !< Write data (array).
    generic :: write_fielddata =>      &
               write_fielddata1_rank0, &
               write_fielddata_tag !< Write FieldData tag.
    generic :: write_geo =>                    &
               write_geo_strg_data1_rank2_R8P, &
               write_geo_strg_data1_rank2_R4P, &
               write_geo_strg_data1_rank4_R8P, &
               write_geo_strg_data1_rank4_R4P, &
               write_geo_strg_data3_rank1_R8P, &
               write_geo_strg_data3_rank1_R4P, &
               write_geo_strg_data3_rank3_R8P, &
               write_geo_strg_data3_rank3_R4P, &
               write_geo_rect_data3_rank1_R8P, &
               write_geo_rect_data3_rank1_R4P, &
               write_geo_unst_data1_rank2_R8P, &
               write_geo_unst_data1_rank2_R4P, &
               write_geo_unst_data3_rank1_R8P, &
               write_geo_unst_data3_rank1_R4P !< Write mesh.
    generic :: write_piece =>              &
               write_piece_start_tag,      &
               write_piece_start_tag_unst, &
               write_piece_end_tag !< Write Piece start/end tag.
    ! deferred methods
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
    ! private methods
    procedure, pass(self), private :: write_fielddata1_rank0         !< Write FieldData tag (data 1, rank 0, R8P).
    procedure, pass(self), private :: write_fielddata_tag            !< Write FieldData tag.
    procedure, pass(self), private :: write_geo_strg_data1_rank2_R8P !< Write **StructuredGrid** mesh (data 1, rank 2, R8P).
    procedure, pass(self), private :: write_geo_strg_data1_rank2_R4P !< Write **StructuredGrid** mesh (data 1, rank 2, R4P).
    procedure, pass(self), private :: write_geo_strg_data1_rank4_R8P !< Write **StructuredGrid** mesh (data 1, rank 4, R8P).
    procedure, pass(self), private :: write_geo_strg_data1_rank4_R4P !< Write **StructuredGrid** mesh (data 1, rank 4, R4P).
    procedure, pass(self), private :: write_geo_strg_data3_rank1_R8P !< Write **StructuredGrid** mesh (data 3, rank 1, R8P).
    procedure, pass(self), private :: write_geo_strg_data3_rank1_R4P !< Write **StructuredGrid** mesh (data 3, rank 1, R4P).
    procedure, pass(self), private :: write_geo_strg_data3_rank3_R8P !< Write **StructuredGrid** mesh (data 3, rank 3, R8P).
    procedure, pass(self), private :: write_geo_strg_data3_rank3_R4P !< Write **StructuredGrid** mesh (data 3, rank 3, R4P).
    procedure, pass(self), private :: write_geo_rect_data3_rank1_R8P !< Write **RectilinearGrid** mesh (data 3, rank 1, R8P).
    procedure, pass(self), private :: write_geo_rect_data3_rank1_R4P !< Write **RectilinearGrid** mesh (data 3, rank 1, R4P).
    procedure, pass(self), private :: write_geo_unst_data1_rank2_R8P !< Write **UnstructuredGrid** mesh (data 1, rank 2, R8P).
    procedure, pass(self), private :: write_geo_unst_data1_rank2_R4P !< Write **UnstructuredGrid** mesh (data 1, rank 2, R4P).
    procedure, pass(self), private :: write_geo_unst_data3_rank1_R8P !< Write **UnstructuredGrid** mesh (data 3, rank 1, R8P).
    procedure, pass(self), private :: write_geo_unst_data3_rank1_R4P !< Write **UnstructuredGrid** mesh (data 3, rank 1, R4P).
    procedure, pass(self), private :: write_piece_start_tag          !< Write `<Piece ...>` start tag.
    procedure, pass(self), private :: write_piece_start_tag_unst     !< Write `<Piece ...>` start tag for unstructured topology.
    procedure, pass(self), private :: write_piece_end_tag            !< Write `</Piece>` end tag.
endtype xml_writer_abstract
!-----------------------------------------------------------------------------------------------------------------------------------

!-----------------------------------------------------------------------------------------------------------------------------------
abstract interface
  function initialize_interface(self, format, filename, mesh_topology, nx1, nx2, ny1, ny2, nz1, nz2) result(error)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Initialize writer.
  !---------------------------------------------------------------------------------------------------------------------------------
  import :: xml_writer_abstract, I4P
  class(xml_writer_abstract), intent(inout)        :: self          !< Writer.
  character(*),               intent(in)           :: format        !< File format: ASCII.
  character(*),               intent(in)           :: filename      !< File name.
  character(*),               intent(in)           :: mesh_topology !< Mesh topology.
  integer(I4P),               intent(in), optional :: nx1           !< Initial node of x axis.
  integer(I4P),               intent(in), optional :: nx2           !< Final node of x axis.
  integer(I4P),               intent(in), optional :: ny1           !< Initial node of y axis.
  integer(I4P),               intent(in), optional :: ny2           !< Final node of y axis.
  integer(I4P),               intent(in), optional :: nz1           !< Initial node of z axis.
  integer(I4P),               intent(in), optional :: nz2           !< Final node of z axis.
  integer(I4P)                                     :: error         !< Error status.
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction initialize_interface

  function finalize_interface(self) result(error)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Finalize writer.
  !---------------------------------------------------------------------------------------------------------------------------------
  import :: xml_writer_abstract, I4P
  class(xml_writer_abstract), intent(inout) :: self  !< Writer.
  integer(I4P)                              :: error !< Error status.
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction finalize_interface

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
  function write_fielddata1_rank0(self, data_name, x) result(error)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Write `<DataArray... NumberOfTuples="..."...>...</DataArray>` tag (R8P).
  !---------------------------------------------------------------------------------------------------------------------------------
  class(xml_writer_abstract), intent(inout) :: self      !< Writer.
  character(*),               intent(in)    :: data_name !< Data name.
  class(*),                   intent(in)    :: x         !< Data variable.
  integer(I4P)                              :: error     !< Error status.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  select type(x)
  type is(real(R8P))
    self%error = self%write_dataarray(data_name=data_name, x=[x], is_tuples=.true.)
  type is(real(R4P))
    self%error = self%write_dataarray(data_name=data_name, x=[x], is_tuples=.true.)
  type is(integer(I8P))
    self%error = self%write_dataarray(data_name=data_name, x=[x], is_tuples=.true.)
  type is(integer(I4P))
    self%error = self%write_dataarray(data_name=data_name, x=[x], is_tuples=.true.)
  type is(integer(I2P))
    self%error = self%write_dataarray(data_name=data_name, x=[x], is_tuples=.true.)
  type is(integer(I1P))
    self%error = self%write_dataarray(data_name=data_name, x=[x], is_tuples=.true.)
  endselect
  error = self%error
  return
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction write_fielddata1_rank0

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

  ! write_piece methods
  function write_piece_start_tag(self, nx1, nx2, ny1, ny2, nz1, nz2) result(error)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Write `<Piece ...>` start tag.
  !---------------------------------------------------------------------------------------------------------------------------------
  class(xml_writer_abstract), intent(inout) :: self           !< Writer.
  integer(I4P),               intent(in)    :: nx1            !< Initial node of x axis.
  integer(I4P),               intent(in)    :: nx2            !< Final node of x axis.
  integer(I4P),               intent(in)    :: ny1            !< Initial node of y axis.
  integer(I4P),               intent(in)    :: ny2            !< Final node of y axis.
  integer(I4P),               intent(in)    :: nz1            !< Initial node of z axis.
  integer(I4P),               intent(in)    :: nz2            !< Final node of z axis.
  integer(I4P)                              :: error          !< Error status.
  type(string)                              :: tag_attributes !< Tag attributes.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  tag_attributes = 'Extent="'//trim(str(n=nx1))//' '//trim(str(n=nx2))//' '// &
                               trim(str(n=ny1))//' '//trim(str(n=ny2))//' '// &
                               trim(str(n=nz1))//' '//trim(str(n=nz2))//'"'
  call self%write_start_tag(tag_name='Piece', tag_attributes=tag_attributes%chars())
  error = self%error
  return
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction write_piece_start_tag

  function write_piece_start_tag_unst(self, np, nc) result(error)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Write `<Piece ...>` start tag for unstructured topology.
  !---------------------------------------------------------------------------------------------------------------------------------
  class(xml_writer_abstract), intent(inout) :: self           !< Writer.
  integer(I4P),               intent(in)    :: np             !< Number of points.
  integer(I4P),               intent(in)    :: nc             !< Number of cells.
  integer(I4P)                              :: error          !< Error status.
  type(string)                              :: tag_attributes !< Tag attributes.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  tag_attributes = 'NumberOfPoints="'//trim(str(n=np))//'" NumberOfCells="'//trim(str(n=nc))//'"'
  call self%write_start_tag(tag_name='Piece', tag_attributes=tag_attributes%chars())
  error = self%error
  return
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction write_piece_start_tag_unst

  function write_piece_end_tag(self) result(error)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Write `</Piece>` end tag.
  !---------------------------------------------------------------------------------------------------------------------------------
  class(xml_writer_abstract), intent(inout) :: self  !< Writer.
  integer(I4P)                              :: error !< Error status.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  call self%write_end_tag(tag_name='Piece')
  error = self%error
  return
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction write_piece_end_tag

  ! write_geo_rect methods
  function write_geo_rect_data3_rank1_R8P(self, x, y, z) result(error)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Write mesh with **RectilinearGrid** topology (data 3, rank 1, R8P).
  !---------------------------------------------------------------------------------------------------------------------------------
  class(xml_writer_abstract), intent(inout) :: self  !< Writer.
  real(R8P),                  intent(in)    :: x(1:) !< X coordinates.
  real(R8P),                  intent(in)    :: y(1:) !< Y coordinates.
  real(R8P),                  intent(in)    :: z(1:) !< Z coordinates.
  integer(I4P)                              :: error !< Error status.
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
  class(xml_writer_abstract), intent(inout) :: self  !< Writer.
  real(R4P),                  intent(in)    :: x(1:) !< X coordinates.
  real(R4P),                  intent(in)    :: y(1:) !< Y coordinates.
  real(R4P),                  intent(in)    :: z(1:) !< Z coordinates.
  integer(I4P)                              :: error !< Error status.
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

  ! write_geo_strg methods
  function write_geo_strg_data1_rank2_R8P(self, xyz) result(error)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Write mesh with **StructuredGrid** topology (data 1, rank 2, R8P).
  !---------------------------------------------------------------------------------------------------------------------------------
  class(xml_writer_abstract), intent(inout) :: self       !< Writer.
  real(R8P),                  intent(in)    :: xyz(1:,1:) !< X, y, z coordinates [1:3,1:n].
  integer(I4P)                              :: error      !< Error status.
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
  class(xml_writer_abstract), intent(inout) :: self       !< Writer.
  real(R4P),                  intent(in)    :: xyz(1:,1:) !< X, y, z coordinates [1:3,:].
  integer(I4P)                              :: error      !< Error status.
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
  class(xml_writer_abstract), intent(inout) :: self             !< Writer.
  real(R8P),                  intent(in)    :: xyz(1:,1:,1:,1:) !< X, y, z coordinates [1:3,:,:,:].
  integer(I4P)                              :: error            !< Error status.
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
  class(xml_writer_abstract), intent(inout) :: self             !< Writer.
  real(R4P),                  intent(in)    :: xyz(1:,1:,1:,1:) !< X, y, z coordinates [1:3,:,:,:].
  integer(I4P)                              :: error            !< Error status.
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
  class(xml_writer_abstract), intent(inout) :: self  !< Writer.
  integer(I4P),               intent(in)    :: n     !< Number of nodes.
  real(R8P),                  intent(in)    :: x(1:) !< X coordinates.
  real(R8P),                  intent(in)    :: y(1:) !< Y coordinates.
  real(R8P),                  intent(in)    :: z(1:) !< Z coordinates.
  integer(I4P)                              :: error !< Error status.
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
  class(xml_writer_abstract), intent(inout) :: self  !< Writer.
  integer(I4P),               intent(in)    :: n     !< Number of nodes.
  real(R4P),                  intent(in)    :: x(1:) !< X coordinates.
  real(R4P),                  intent(in)    :: y(1:) !< Y coordinates.
  real(R4P),                  intent(in)    :: z(1:) !< Z coordinates.
  integer(I4P)                              :: error !< Error status.
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
  class(xml_writer_abstract), intent(inout) :: self        !< Writer.
  integer(I4P),               intent(in)    :: n           !< Number of nodes.
  real(R8P),                  intent(in)    :: x(1:,1:,1:) !< X coordinates.
  real(R8P),                  intent(in)    :: y(1:,1:,1:) !< Y coordinates.
  real(R8P),                  intent(in)    :: z(1:,1:,1:) !< Z coordinates.
  integer(I4P)                              :: error       !< Error status.
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
  class(xml_writer_abstract), intent(inout) :: self        !< Writer.
  integer(I4P),               intent(in)    :: n           !< Number of nodes.
  real(R4P),                  intent(in)    :: x(1:,1:,1:) !< X coordinates.
  real(R4P),                  intent(in)    :: y(1:,1:,1:) !< Y coordinates.
  real(R4P),                  intent(in)    :: z(1:,1:,1:) !< Z coordinates.
  integer(I4P)                              :: error       !< Error status.
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

  ! write_geo_unst methods
  function write_geo_unst_data1_rank2_R8P(self, np, nc, xyz) result(error)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Write mesh with **UnstructuredGrid** topology (data 1, rank 2, R8P).
  !---------------------------------------------------------------------------------------------------------------------------------
  class(xml_writer_abstract), intent(inout) :: self       !< Writer.
  integer(I4P),               intent(in)    :: np         !< Number of points.
  integer(I4P),               intent(in)    :: nc         !< Number of cells.
  real(R8P),                  intent(in)    :: xyz(1:,1:) !< X, y, z coordinates [1:3,:].
  integer(I4P)                              :: error      !< Error status.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  if (np/=size(xyz, dim=2)) then
    self%error = 1
    return
  endif
  if (nc>np) then
    self%error = 2
    return
  endif
  call self%write_start_tag(tag_name='Points')
  error = self%write_dataarray(data_name='Points', x=xyz)
  call self%write_end_tag(tag_name='Points')
  error = self%error
  return
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction write_geo_unst_data1_rank2_R8P

  function write_geo_unst_data1_rank2_R4P(self, np, nc, xyz) result(error)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Write mesh with **UnstructuredGrid** topology (data 1, rank 2, R4P).
  !---------------------------------------------------------------------------------------------------------------------------------
  class(xml_writer_abstract), intent(inout) :: self       !< Writer.
  integer(I4P),               intent(in)    :: np         !< Number of points.
  integer(I4P),               intent(in)    :: nc         !< Number of cells.
  real(R4P),                  intent(in)    :: xyz(1:,1:) !< X, y, z coordinates [1:3,:].
  integer(I4P)                              :: error      !< Error status.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  if (np/=size(xyz, dim=2)) then
    self%error = 1
    return
  endif
  if (nc>np) then
    self%error = 2
    return
  endif
  call self%write_start_tag(tag_name='Points')
  error = self%write_dataarray(data_name='Points', x=xyz)
  call self%write_end_tag(tag_name='Points')
  error = self%error
  return
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction write_geo_unst_data1_rank2_R4P

  function write_geo_unst_data3_rank1_R8P(self, np, nc, x, y, z) result(error)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Write mesh with **UnstructuredGrid** topology (data 3, rank 1, R8P).
  !---------------------------------------------------------------------------------------------------------------------------------
  class(xml_writer_abstract), intent(inout) :: self  !< Writer.
  integer(I4P),               intent(in)    :: np    !< Number of points.
  integer(I4P),               intent(in)    :: nc    !< Number of cells.
  real(R8P),                  intent(in)    :: x(1:) !< X coordinates.
  real(R8P),                  intent(in)    :: y(1:) !< Y coordinates.
  real(R8P),                  intent(in)    :: z(1:) !< Z coordinates.
  integer(I4P)                              :: error !< Error status.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  if ((np/=size(x, dim=1)).or.(np/=size(y, dim=1)).or.(np/=size(z, dim=1))) then
    self%error = 1
    return
  endif
  if (nc>np) then
    self%error = 2
    return
  endif
  call self%write_start_tag(tag_name='Points')
  error = self%write_dataarray(data_name='Points', x=x, y=y, z=z)
  call self%write_end_tag(tag_name='Points')
  error = self%error
  return
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction write_geo_unst_data3_rank1_R8P

  function write_geo_unst_data3_rank1_R4P(self, np, nc, x, y, z) result(error)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Write mesh with **UnstructuredGrid** topology (data 3, rank 1, R4P).
  !---------------------------------------------------------------------------------------------------------------------------------
  class(xml_writer_abstract), intent(inout) :: self  !< Writer.
  integer(I4P),               intent(in)    :: np    !< Number of points.
  integer(I4P),               intent(in)    :: nc    !< Number of cells.
  real(R4P),                  intent(in)    :: x(1:) !< X coordinates.
  real(R4P),                  intent(in)    :: y(1:) !< Y coordinates.
  real(R4P),                  intent(in)    :: z(1:) !< Z coordinates.
  integer(I4P)                              :: error !< Error status.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  if ((np/=size(x, dim=1)).or.(np/=size(y, dim=1)).or.(np/=size(z, dim=1))) then
    self%error = 1
    return
  endif
  if (nc>np) then
    self%error = 2
    return
  endif
  call self%write_start_tag(tag_name='Points')
  error = self%write_dataarray(data_name='Points', x=x, y=y, z=z)
  call self%write_end_tag(tag_name='Points')
  error = self%error
  return
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction write_geo_unst_data3_rank1_R4P

  function write_connectivity(self, nc, connectivity, offset, cell_type) result(error)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Write mesh connectivity.
  !<
  !< **Must** be used when unstructured grid is used, it saves the connectivity of the unstructured gird.
  !< @note The vector **connect** must follow the VTK-XML standard. It is passed as *assumed-shape array*
  !< because its dimensions is related to the mesh dimensions in a complex way. Its dimensions can be calculated by the following
  !< equation: \(dc = \sum\limits_{i = 1}^{NC} {nvertex_i }\).
  !< Note that this equation is different from the legacy one. The XML connectivity convention is quite different from the
  !< legacy standard.
  !< As an example suppose we have a mesh composed by 2 cells, one hexahedron (8 vertices) and one pyramid with
  !< square basis (5 vertices) and suppose that the basis of pyramid is constitute by a face of the hexahedron and so the two cells
  !< share 4 vertices. The above equation gives \(dc=8+5=13\). The connectivity vector for this mesh can be:
  !<
  !<##### first cell
  !<+ connect(1)  = 0 identification flag of \(1^\circ\) vertex of first cell
  !<+ connect(2)  = 1 identification flag of \(2^\circ\) vertex of first cell
  !<+ connect(3)  = 2 identification flag of \(3^\circ\) vertex of first cell
  !<+ connect(4)  = 3 identification flag of \(4^\circ\) vertex of first cell
  !<+ connect(5)  = 4 identification flag of \(5^\circ\) vertex of first cell
  !<+ connect(6)  = 5 identification flag of \(6^\circ\) vertex of first cell
  !<+ connect(7)  = 6 identification flag of \(7^\circ\) vertex of first cell
  !<+ connect(8)  = 7 identification flag of \(8^\circ\) vertex of first cell
  !<
  !<##### second cell
  !<+ connect(9 ) = 0 identification flag of \(1^\circ\) vertex of second cell
  !<+ connect(10) = 1 identification flag of \(2^\circ\) vertex of second cell
  !<+ connect(11) = 2 identification flag of \(3^\circ\) vertex of second cell
  !<+ connect(12) = 3 identification flag of \(4^\circ\) vertex of second cell
  !<+ connect(13) = 8 identification flag of \(5^\circ\) vertex of second cell
  !<
  !< Therefore this connectivity vector convention is more simple than the legacy convention, now we must create also the
  !< *offset* vector that contains the data now missing in the *connect* vector. The offset
  !< vector for this mesh can be:
  !<
  !<##### first cell
  !<+ offset(1) = 8  => summ of nodes of \(1^\circ\) cell
  !<
  !<##### second cell
  !<+ offset(2) = 13 => summ of nodes of \(1^\circ\) and \(2^\circ\) cells
  !<
  !< The value of every cell-offset can be calculated by the following equation: \(offset_c=\sum\limits_{i=1}^{c}{nvertex_i}\)
  !< where \(offset_c\) is the value of \(c^{th}\) cell and \(nvertex_i\) is the number of vertices of \(i^{th}\) cell.
  !< The function VTK_CON_XML does not calculate the connectivity and offset vectors: it writes the connectivity and offset
  !< vectors conforming the VTK-XML standard, but does not calculate them.
  !< The vector variable *cell\_type* must conform the VTK-XML standard (see the file VTK-Standard at the
  !< Kitware homepage) that is the same of the legacy standard. It contains the
  !< *type* of each cells. For the above example this vector is:
  !<
  !<##### first cell
  !<+ cell\_type(1) = 12 hexahedron type of first cell
  !<
  !<##### second cell
  !<+ cell\_type(2) = 14 pyramid type of second cell
  !---------------------------------------------------------------------------------------------------------------------------------
  class(xml_writer_abstract), intent(inout) :: self             !< Writer.
  integer(I4P),               intent(in)    :: nc               !< Number of cells.
  integer(I4P),               intent(in)    :: connectivity(1:) !< Mesh connectivity.
  integer(I4P),               intent(in)    :: offset(1:)       !< Cell offset.
  integer(I1P),               intent(in)    :: cell_type(1:)    !< VTK cell type.
  integer(I4P)                              :: error            !< Error status.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  call self%write_start_tag(tag_name='Cells')
  error = self%write_dataarray(data_name='connectivity', x=connectivity)
  error = self%write_dataarray(data_name='offsets', x=offset)
  error = self%write_dataarray(data_name='types', x=cell_type)
  call self%write_end_tag(tag_name='Cells')
  return
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction write_connectivity
endmodule vtk_fortran_vtk_file_xml_writer_abstract
