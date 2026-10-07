!< VTK file abstract XML writer.
module vtk_fortran_vtk_file_xml_writer_abstract
!< VTK file abstract XML writer.
use foxy
use penf
use stringifor
use vtk_fortran_parameters

implicit none
private
public :: xml_writer_abstract

type, abstract :: xml_writer_abstract
  !< VTK file abstract XML writer.
  type(string)  :: format_ch                       !< Output format, string code.
  type(string)  :: topology                        !< Mesh topology.
  integer(I4P)  :: indent=0_I4P                    !< Indent count.
  integer(I4P)  :: ghost_level=0_I4P               !< Ghost level of parallel (P*) topologies.
  type(string)  :: data_type_override              !< If set, type of the next DataArray tag (then unset).
  type(string)  :: tag_name_override               !< If set, element name of the next DataArray tag (then unset).
  integer(I8P)  :: tuples_override=-1_I8P          !< If >= 0, number of tuples of the next DataArray tag (then unset).
  real(R8P)     :: origin(3)=0._R8P                !< Origin of ImageData topologies.
  real(R8P)     :: spacing(3)=1._R8P               !< Spacing of ImageData topologies.
  real(R8P)     :: direction(9)=[1._R8P, 0._R8P, 0._R8P, &
                                 0._R8P, 1._R8P, 0._R8P, &
                                 0._R8P, 0._R8P, 1._R8P] !< Axes directions of ImageData topologies (row-major 3x3).
  logical       :: is_direction_set=.false.        !< Write the Direction of ImageData topologies.
  logical       :: is_uint64=.false.               !< Use UInt64 (instead of UInt32) bytes count headers.
  logical       :: is_compressed=.false.           !< Compress (zlib) the binary data.
  integer(I8P)  :: ioffset=0_I8P                   !< Offset count.
  integer(I4P)  :: xml=0_I4P                       !< XML Logical unit.
  integer(I4P)  :: vtm_block(1:2)=[-1_I4P, -1_I4P] !< Block indexes.
  integer(I4P)  :: error=0_I4P                     !< IO Error status.
  type(xml_tag) :: tag                             !< XML tags handler.
  logical       :: is_volatile=.false.             !< Flag to check volatile writer.
  type(string)  :: xml_volatile                    !< XML file volatile (not a physical file).
  contains
    ! public methods (some deferred)
    procedure,                                 pass(self) :: close_xml_file               !< Close xml file.
    procedure,                                 pass(self) :: open_xml_file                !< Open xml file.
    procedure,                                 pass(self) :: free                         !< Free allocated memory.
    procedure,                                 pass(self) :: get_xml_volatile             !< Return the XML volatile string file.
    procedure,                                 pass(self) :: write_connectivity           !< Write connectivity.
    procedure,                                 pass(self) :: write_polydata_cells         !< Write cell blocks of polydata.
    procedure,                                 pass(self) :: write_dataarray_location_tag !< Write dataarray location tag.
    procedure,                                 pass(self) :: write_dataarray_tag          !< Write dataarray tag.
    procedure,                                 pass(self) :: write_dataarray_tag_appended !< Write dataarray appended tag.
    procedure,                                 pass(self) :: write_end_tag                !< Write `</tag_name>` end tag.
    procedure,                                 pass(self) :: write_header_tag             !< Write header tag.
    procedure,                                 pass(self) :: write_parallel_open_block    !< Write parallel open block.
    procedure,                                 pass(self) :: write_parallel_close_block   !< Write parallel close block.
    procedure,                                 pass(self) :: write_parallel_dataarray     !< Write parallel dataarray.
    procedure,                                 pass(self) :: write_parallel_geo           !< Write parallel geo.
    procedure,                                 pass(self) :: write_self_closing_tag       !< Write self closing tag.
    procedure,                                 pass(self) :: write_start_tag              !< Write start tag.
    procedure,                                 pass(self) :: write_tag                    !< Write tag.
    procedure,                                 pass(self) :: write_topology_tag           !< Write topology tag.
    procedure,                                 pass(self) :: image_attributes             !< Return ImageData attributes.
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
               write_dataarray6_rank1_R8P, &
               write_dataarray6_rank1_R4P, &
               write_dataarray6_rank1_I8P, &
               write_dataarray6_rank1_I4P, &
               write_dataarray6_rank1_I2P, &
               write_dataarray6_rank1_I1P, &
               write_dataarray6_rank3_R8P, &
               write_dataarray6_rank3_R4P, &
               write_dataarray6_rank3_I8P, &
               write_dataarray6_rank3_I4P, &
               write_dataarray6_rank3_I2P, &
               write_dataarray6_rank3_I1P, &
               write_dataarray_location_tag !< Write data (array).
    generic :: write_fielddata =>      &
               write_fielddata1_rank0, &
               write_fielddata1_rank1, &
               write_fielddata_tag !< Write FieldData tag.
    generic :: write_dataarray_unsigned =>      &
               write_dataarray_unsigned_I1P, &
               write_dataarray_unsigned_I2P, &
               write_dataarray_unsigned_I4P, &
               write_dataarray_unsigned_I8P !< Write data (array) as unsigned integers.
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
    generic :: write_parallel_block_files =>     &
               write_parallel_block_file,        &
               write_parallel_block_files_array, &
               write_parallel_block_files_string !< Write block list of files.
    generic :: write_piece =>              &
               write_piece_start_tag,      &
               write_piece_start_tag_unst, &
               write_piece_start_tag_poly, &
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
    procedure(write_dataarray6_rank1_R8P_interface), deferred, pass(self) :: write_dataarray6_rank1_R8P !< Data 3, rank 1, R8P.
    procedure(write_dataarray6_rank1_R4P_interface), deferred, pass(self) :: write_dataarray6_rank1_R4P !< Data 3, rank 1, R4P.
    procedure(write_dataarray6_rank1_I8P_interface), deferred, pass(self) :: write_dataarray6_rank1_I8P !< Data 3, rank 1, I8P.
    procedure(write_dataarray6_rank1_I4P_interface), deferred, pass(self) :: write_dataarray6_rank1_I4P !< Data 3, rank 1, I4P.
    procedure(write_dataarray6_rank1_I2P_interface), deferred, pass(self) :: write_dataarray6_rank1_I2P !< Data 3, rank 1, I2P.
    procedure(write_dataarray6_rank1_I1P_interface), deferred, pass(self) :: write_dataarray6_rank1_I1P !< Data 3, rank 1, I1P.
    procedure(write_dataarray6_rank3_R8P_interface), deferred, pass(self) :: write_dataarray6_rank3_R8P !< Data 3, rank 3, R8P.
    procedure(write_dataarray6_rank3_R4P_interface), deferred, pass(self) :: write_dataarray6_rank3_R4P !< Data 3, rank 3, R4P.
    procedure(write_dataarray6_rank3_I8P_interface), deferred, pass(self) :: write_dataarray6_rank3_I8P !< Data 3, rank 3, I8P.
    procedure(write_dataarray6_rank3_I4P_interface), deferred, pass(self) :: write_dataarray6_rank3_I4P !< Data 3, rank 3, I4P.
    procedure(write_dataarray6_rank3_I2P_interface), deferred, pass(self) :: write_dataarray6_rank3_I2P !< Data 3, rank 3, I2P.
    procedure(write_dataarray6_rank3_I1P_interface), deferred, pass(self) :: write_dataarray6_rank3_I1P !< Data 3, rank 3, I1P.
    procedure(write_dataarray_appended_interface),   deferred, pass(self) :: write_dataarray_appended   !< Write appended.
    ! private methods
    procedure, pass(self), private :: write_dataarray_unsigned_I1P      !< Write data (array) as UInt8.
    procedure, pass(self), private :: write_dataarray_unsigned_I2P      !< Write data (array) as UInt16.
    procedure, pass(self), private :: write_dataarray_unsigned_I4P      !< Write data (array) as UInt32.
    procedure, pass(self), private :: write_dataarray_unsigned_I8P      !< Write data (array) as UInt64.
    procedure, pass(self), private :: write_fielddata1_rank0            !< Write FieldData tag (data 1, rank 0).
    procedure, pass(self), private :: write_fielddata1_rank1            !< Write FieldData tag (data 1, rank 1).
    procedure, pass(self), private :: write_fielddata_strings           !< Write FieldData tag (strings).
    procedure, pass(self), private :: dataarray_tag_overrides           !< Apply the one-shot overrides of DataArray tag.
    procedure, pass(self), private :: write_fielddata_tag               !< Write FieldData tag.
    procedure, pass(self), private :: write_geo_strg_data1_rank2_R8P    !< Write **StructuredGrid** mesh (data 1, rank 2, R8P).
    procedure, pass(self), private :: write_geo_strg_data1_rank2_R4P    !< Write **StructuredGrid** mesh (data 1, rank 2, R4P).
    procedure, pass(self), private :: write_geo_strg_data1_rank4_R8P    !< Write **StructuredGrid** mesh (data 1, rank 4, R8P).
    procedure, pass(self), private :: write_geo_strg_data1_rank4_R4P    !< Write **StructuredGrid** mesh (data 1, rank 4, R4P).
    procedure, pass(self), private :: write_geo_strg_data3_rank1_R8P    !< Write **StructuredGrid** mesh (data 3, rank 1, R8P).
    procedure, pass(self), private :: write_geo_strg_data3_rank1_R4P    !< Write **StructuredGrid** mesh (data 3, rank 1, R4P).
    procedure, pass(self), private :: write_geo_strg_data3_rank3_R8P    !< Write **StructuredGrid** mesh (data 3, rank 3, R8P).
    procedure, pass(self), private :: write_geo_strg_data3_rank3_R4P    !< Write **StructuredGrid** mesh (data 3, rank 3, R4P).
    procedure, pass(self), private :: write_geo_rect_data3_rank1_R8P    !< Write **RectilinearGrid** mesh (data 3, rank 1, R8P).
    procedure, pass(self), private :: write_geo_rect_data3_rank1_R4P    !< Write **RectilinearGrid** mesh (data 3, rank 1, R4P).
    procedure, pass(self), private :: write_geo_unst_data1_rank2_R8P    !< Write **UnstructuredGrid** mesh (data 1, rank 2, R8P).
    procedure, pass(self), private :: write_geo_unst_data1_rank2_R4P    !< Write **UnstructuredGrid** mesh (data 1, rank 2, R4P).
    procedure, pass(self), private :: write_geo_unst_data3_rank1_R8P    !< Write **UnstructuredGrid** mesh (data 3, rank 1, R8P).
    procedure, pass(self), private :: write_geo_unst_data3_rank1_R4P    !< Write **UnstructuredGrid** mesh (data 3, rank 1, R4P).
    procedure, pass(self), private :: write_piece_start_tag             !< Write `<Piece ...>` start tag.
    procedure, pass(self), private :: write_piece_start_tag_unst        !< Write `<Piece ...>` start tag for unstructured topology.
    procedure, pass(self), private :: write_piece_start_tag_poly        !< Write `<Piece ...>` start tag for polydata topology.
    procedure, pass(self), private :: write_piece_end_tag               !< Write `</Piece>` end tag.
    procedure, pass(self), private :: write_parallel_block_file         !< Write single file that belong to the current block.
    procedure, pass(self), private :: write_parallel_block_files_array  !< Write block list of files (array input).
    procedure, pass(self), private :: write_parallel_block_files_string !< Write block list of files (string input).
endtype xml_writer_abstract

abstract interface
  function initialize_interface(self, format, filename, mesh_topology, nx1, nx2, ny1, ny2, nz1, nz2, &
                                is_volatile, mesh_kind) result(error)
  !< Initialize writer.
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
  character(*),               intent(in), optional :: mesh_kind     !< Kind of mesh data: Float64, Float32, ecc.
  logical,                    intent(in), optional :: is_volatile   !< Flag to check volatile writer.
  integer(I4P)                                     :: error         !< Error status.
  endfunction initialize_interface

   function finalize_interface(self) result(error)
   !< Finalize writer.
   import :: xml_writer_abstract, I4P
   class(xml_writer_abstract), intent(inout) :: self  !< Writer.
   integer(I4P)                              :: error !< Error status.
   endfunction finalize_interface

  function write_dataarray1_rank1_R8P_interface(self, data_name, x, is_tuples) result(error)
  !< Write `<DataArray... NumberOfComponents="1"...>...</DataArray>` tag (R8P).
  import :: xml_writer_abstract, I4P, R8P
  class(xml_writer_abstract), intent(inout)        :: self         !< Writer.
  character(*),               intent(in)           :: data_name    !< Data name.
  real(R8P),                  intent(in)           :: x(1:)        !< Data variable.
  logical,                    intent(in), optional :: is_tuples    !< Use "NumberOfTuples" instead of "NumberOfComponents".
  integer(I4P)                                     :: error        !< Error status.
  endfunction write_dataarray1_rank1_R8P_interface

  function write_dataarray1_rank1_R4P_interface(self, data_name, x, is_tuples) result(error)
  !< Write `<DataArray... NumberOfComponents="1"...>...</DataArray>` tag (R4P).
  import :: xml_writer_abstract, I4P, R4P
  class(xml_writer_abstract), intent(inout)        :: self         !< Writer.
  character(*),               intent(in)           :: data_name    !< Data name.
  real(R4P),                  intent(in)           :: x(1:)        !< Data variable.
  logical,                    intent(in), optional :: is_tuples    !< Use "NumberOfTuples" instead of "NumberOfComponents".
  integer(I4P)                                     :: error        !< Error status.
  endfunction write_dataarray1_rank1_R4P_interface

  function write_dataarray1_rank1_I8P_interface(self, data_name, x, is_tuples) result(error)
  !< Write `<DataArray... NumberOfComponents="1"...>...</DataArray>` tag (I8P).
  import :: xml_writer_abstract, I4P, I8P
  class(xml_writer_abstract), intent(inout)        :: self         !< Writer.
  character(*),               intent(in)           :: data_name    !< Data name.
  integer(I8P),               intent(in)           :: x(1:)        !< Data variable.
  logical,                    intent(in), optional :: is_tuples    !< Use "NumberOfTuples" instead of "NumberOfComponents".
  integer(I4P)                                     :: error        !< Error status.
  endfunction write_dataarray1_rank1_I8P_interface

  function write_dataarray1_rank1_I4P_interface(self, data_name, x, is_tuples) result(error)
  !< Write `<DataArray... NumberOfComponents="1"...>...</DataArray>` tag (I4P).
  import :: xml_writer_abstract, I4P
  class(xml_writer_abstract), intent(inout)        :: self         !< Writer.
  character(*),               intent(in)           :: data_name    !< Data name.
  integer(I4P),               intent(in)           :: x(1:)        !< Data variable.
  logical,                    intent(in), optional :: is_tuples    !< Use "NumberOfTuples" instead of "NumberOfComponents".
  integer(I4P)                                     :: error        !< Error status.
  endfunction write_dataarray1_rank1_I4P_interface

  function write_dataarray1_rank1_I2P_interface(self, data_name, x, is_tuples) result(error)
  !< Write `<DataArray... NumberOfComponents="1"...>...</DataArray>` tag (I2P).
  import :: xml_writer_abstract, I2P, I4P
  class(xml_writer_abstract), intent(inout)        :: self         !< Writer.
  character(*),               intent(in)           :: data_name    !< Data name.
  integer(I2P),               intent(in)           :: x(1:)        !< Data variable.
  logical,                    intent(in), optional :: is_tuples    !< Use "NumberOfTuples" instead of "NumberOfComponents".
  integer(I4P)                                     :: error        !< Error status.
  endfunction write_dataarray1_rank1_I2P_interface

  function write_dataarray1_rank1_I1P_interface(self, data_name, x, is_tuples) result(error)
  !< Write `<DataArray... NumberOfComponents="1"...>...</DataArray>` tag (I1P).
  import :: xml_writer_abstract, I1P, I4P
  class(xml_writer_abstract), intent(inout)        :: self         !< Writer.
  character(*),               intent(in)           :: data_name    !< Data name.
  integer(I1P),               intent(in)           :: x(1:)        !< Data variable.
  logical,                    intent(in), optional :: is_tuples    !< Use "NumberOfTuples" instead of "NumberOfComponents".
  integer(I4P)                                     :: error        !< Error status.
  endfunction write_dataarray1_rank1_I1P_interface

  function write_dataarray1_rank2_R8P_interface(self, data_name, x, one_component, is_tuples) result(error)
  !< Write `<DataArray... NumberOfComponents="n"...>...</DataArray>` tag (R8P).
  import :: xml_writer_abstract, I4P, R8P
  class(xml_writer_abstract), intent(inout)        :: self          !< Writer.
  character(*),               intent(in)           :: data_name     !< Data name.
  real(R8P),                  intent(in)           :: x(1:,1:)      !< Data variable.
  logical,                    intent(in), optional :: one_component !< Force one component.
  logical,                    intent(in), optional :: is_tuples     !< Use "NumberOfTuples" instead of "NumberOfComponents".
  integer(I4P)                                     :: error         !< Error status.
  endfunction write_dataarray1_rank2_R8P_interface

  function write_dataarray1_rank2_R4P_interface(self, data_name, x, one_component, is_tuples) result(error)
  !< Write `<DataArray... NumberOfComponents="n"...>...</DataArray>` tag (R4P).
  import :: xml_writer_abstract, I4P, R4P
  class(xml_writer_abstract), intent(inout)        :: self          !< Writer.
  character(*),               intent(in)           :: data_name     !< Data name.
  real(R4P),                  intent(in)           :: x(1:,1:)      !< Data variable.
  logical,                    intent(in), optional :: one_component !< Force one component.
  logical,                    intent(in), optional :: is_tuples     !< Use "NumberOfTuples" instead of "NumberOfComponents".
  integer(I4P)                                     :: error         !< Error status.
  endfunction write_dataarray1_rank2_R4P_interface

  function write_dataarray1_rank2_I8P_interface(self, data_name, x, one_component, is_tuples) result(error)
  !< Write `<DataArray... NumberOfComponents="n"...>...</DataArray>` tag (I8P).
  import :: xml_writer_abstract, I4P, I8P
  class(xml_writer_abstract), intent(inout)        :: self          !< Writer.
  character(*),               intent(in)           :: data_name     !< Data name.
  integer(I8P),               intent(in)           :: x(1:,1:)      !< Data variable.
  logical,                    intent(in), optional :: one_component !< Force one component.
  logical,                    intent(in), optional :: is_tuples     !< Use "NumberOfTuples" instead of "NumberOfComponents".
  integer(I4P)                                     :: error         !< Error status.
  endfunction write_dataarray1_rank2_I8P_interface

  function write_dataarray1_rank2_I4P_interface(self, data_name, x, one_component, is_tuples) result(error)
  !< Write `<DataArray... NumberOfComponents="n"...>...</DataArray>` tag (I4P).
  import :: xml_writer_abstract, I4P
  class(xml_writer_abstract), intent(inout)        :: self          !< Writer.
  character(*),               intent(in)           :: data_name     !< Data name.
  integer(I4P),               intent(in)           :: x(1:,1:)      !< Data variable.
  logical,                    intent(in), optional :: one_component !< Force one component.
  logical,                    intent(in), optional :: is_tuples     !< Use "NumberOfTuples" instead of "NumberOfComponents".
  integer(I4P)                                     :: error         !< Error status.
  endfunction write_dataarray1_rank2_I4P_interface

  function write_dataarray1_rank2_I2P_interface(self, data_name, x, one_component, is_tuples) result(error)
  !< Write `<DataArray... NumberOfComponents="n"...>...</DataArray>` tag (I2P).
  import :: xml_writer_abstract, I2P, I4P
  class(xml_writer_abstract), intent(inout)        :: self          !< Writer.
  character(*),               intent(in)           :: data_name     !< Data name.
  integer(I2P),               intent(in)           :: x(1:,1:)      !< Data variable.
  logical,                    intent(in), optional :: one_component !< Force one component.
  logical,                    intent(in), optional :: is_tuples     !< Use "NumberOfTuples" instead of "NumberOfComponents".
  integer(I4P)                                     :: error         !< Error status.
  endfunction write_dataarray1_rank2_I2P_interface

  function write_dataarray1_rank2_I1P_interface(self, data_name, x, one_component, is_tuples) result(error)
  !< Write `<DataArray... NumberOfComponents="n"...>...</DataArray>` tag (I1P).
  import :: xml_writer_abstract, I1P, I4P
  class(xml_writer_abstract), intent(inout)        :: self          !< Writer.
  character(*),               intent(in)           :: data_name     !< Data name.
  integer(I1P),               intent(in)           :: x(1:,1:)      !< Data variable.
  logical,                    intent(in), optional :: one_component !< Force one component.
  logical,                    intent(in), optional :: is_tuples     !< Use "NumberOfTuples" instead of "NumberOfComponents".
  integer(I4P)                                     :: error         !< Error status.
  endfunction write_dataarray1_rank2_I1P_interface

  function write_dataarray1_rank3_R8P_interface(self, data_name, x, one_component, is_tuples) result(error)
  !< Write `<DataArray... NumberOfComponents="n"...>...</DataArray>` tag (R8P).
  import :: xml_writer_abstract, I4P, R8P
  class(xml_writer_abstract), intent(inout)        :: self          !< Writer.
  character(*),               intent(in)           :: data_name     !< Data name.
  real(R8P),                  intent(in)           :: x(1:,1:,1:)   !< Data variable.
  logical,                    intent(in), optional :: one_component !< Force one component.
  logical,                    intent(in), optional :: is_tuples     !< Use "NumberOfTuples" instead of "NumberOfComponents".
  integer(I4P)                                     :: error         !< Error status.
  endfunction write_dataarray1_rank3_R8P_interface

  function write_dataarray1_rank3_R4P_interface(self, data_name, x, one_component, is_tuples) result(error)
  !< Write `<DataArray... NumberOfComponents="n"...>...</DataArray>` tag (R4P).
  import :: xml_writer_abstract, I4P, R4P
  class(xml_writer_abstract), intent(inout)        :: self          !< Writer.
  character(*),               intent(in)           :: data_name     !< Data name.
  real(R4P),                  intent(in)           :: x(1:,1:,1:)   !< Data variable.
  logical,                    intent(in), optional :: one_component !< Force one component.
  logical,                    intent(in), optional :: is_tuples     !< Use "NumberOfTuples" instead of "NumberOfComponents".
  integer(I4P)                                     :: error         !< Error status.
  endfunction write_dataarray1_rank3_R4P_interface

  function write_dataarray1_rank3_I8P_interface(self, data_name, x, one_component, is_tuples) result(error)
  !< Write `<DataArray... NumberOfComponents="n"...>...</DataArray>` tag (I8P).
  import :: xml_writer_abstract, I4P, I8P
  class(xml_writer_abstract), intent(inout)        :: self          !< Writer.
  character(*),               intent(in)           :: data_name     !< Data name.
  integer(I8P),               intent(in)           :: x(1:,1:,1:)   !< Data variable.
  logical,                    intent(in), optional :: one_component !< Force one component.
  logical,                    intent(in), optional :: is_tuples     !< Use "NumberOfTuples" instead of "NumberOfComponents".
  integer(I4P)                                     :: error         !< Error status.
  endfunction write_dataarray1_rank3_I8P_interface

  function write_dataarray1_rank3_I4P_interface(self, data_name, x, one_component, is_tuples) result(error)
  !< Write `<DataArray... NumberOfComponents="n"...>...</DataArray>` tag (I4P).
  import :: xml_writer_abstract, I4P
  class(xml_writer_abstract), intent(inout)        :: self          !< Writer.
  character(*),               intent(in)           :: data_name     !< Data name.
  integer(I4P),               intent(in)           :: x(1:,1:,1:)   !< Data variable.
  logical,                    intent(in), optional :: one_component !< Force one component.
  logical,                    intent(in), optional :: is_tuples     !< Use "NumberOfTuples" instead of "NumberOfComponents".
  integer(I4P)                                     :: error         !< Error status.
  endfunction write_dataarray1_rank3_I4P_interface

  function write_dataarray1_rank3_I2P_interface(self, data_name, x, one_component, is_tuples) result(error)
  !< Write `<DataArray... NumberOfComponents="n"...>...</DataArray>` tag (I2P).
  import :: xml_writer_abstract, I2P, I4P
  class(xml_writer_abstract), intent(inout)        :: self          !< Writer.
  character(*),               intent(in)           :: data_name     !< Data name.
  integer(I2P),               intent(in)           :: x(1:,1:,1:)   !< Data variable.
  logical,                    intent(in), optional :: one_component !< Force one component.
  logical,                    intent(in), optional :: is_tuples     !< Use "NumberOfTuples" instead of "NumberOfComponents".
  integer(I4P)                                     :: error         !< Error status.
  endfunction write_dataarray1_rank3_I2P_interface

  function write_dataarray1_rank3_I1P_interface(self, data_name, x, one_component, is_tuples) result(error)
  !< Write `<DataArray... NumberOfComponents="n"...>...</DataArray>` tag (I1P).
  import :: xml_writer_abstract, I1P, I4P
  class(xml_writer_abstract), intent(inout)        :: self          !< Writer.
  character(*),               intent(in)           :: data_name     !< Data name.
  integer(I1P),               intent(in)           :: x(1:,1:,1:)   !< Data variable.
  logical,                    intent(in), optional :: one_component !< Force one component.
  logical,                    intent(in), optional :: is_tuples     !< Use "NumberOfTuples" instead of "NumberOfComponents".
  integer(I4P)                                     :: error         !< Error status.
  endfunction write_dataarray1_rank3_I1P_interface

  function write_dataarray1_rank4_R8P_interface(self, data_name, x, one_component, is_tuples) result(error)
  !< Write `<DataArray... NumberOfComponents="n"...>...</DataArray>` tag (R8P).
  import :: xml_writer_abstract, I4P, R8P
  class(xml_writer_abstract), intent(inout)        :: self           !< Writer.
  character(*),               intent(in)           :: data_name      !< Data name.
  real(R8P),                  intent(in)           :: x(1:,1:,1:,1:) !< Data variable.
  logical,                    intent(in), optional :: one_component  !< Force one component.
  logical,                    intent(in), optional :: is_tuples      !< Use "NumberOfTuples" instead of "NumberOfComponents".
  integer(I4P)                                     :: error          !< Error status.
  endfunction write_dataarray1_rank4_R8P_interface

  function write_dataarray1_rank4_R4P_interface(self, data_name, x, one_component, is_tuples) result(error)
  !< Write `<DataArray... NumberOfComponents="n"...>...</DataArray>` tag (R4P).
  import :: xml_writer_abstract, I4P, R4P
  class(xml_writer_abstract), intent(inout)        :: self           !< Writer.
  character(*),               intent(in)           :: data_name      !< Data name.
  real(R4P),                  intent(in)           :: x(1:,1:,1:,1:) !< Data variable.
  logical,                    intent(in), optional :: one_component  !< Force one component.
  logical,                    intent(in), optional :: is_tuples      !< Use "NumberOfTuples" instead of "NumberOfComponents".
  integer(I4P)                                     :: error          !< Error status.
  endfunction write_dataarray1_rank4_R4P_interface

  function write_dataarray1_rank4_I8P_interface(self, data_name, x, one_component, is_tuples) result(error)
  !< Write `<DataArray... NumberOfComponents="n"...>...</DataArray>` tag (I8P).
  import :: xml_writer_abstract, I4P, I8P
  class(xml_writer_abstract), intent(inout)        :: self           !< Writer.
  character(*),               intent(in)           :: data_name      !< Data name.
  integer(I8P),               intent(in)           :: x(1:,1:,1:,1:) !< Data variable.
  logical,                    intent(in), optional :: one_component  !< Force one component.
  logical,                    intent(in), optional :: is_tuples      !< Use "NumberOfTuples" instead of "NumberOfComponents".
  integer(I4P)                                     :: error          !< Error status.
  endfunction write_dataarray1_rank4_I8P_interface

  function write_dataarray1_rank4_I4P_interface(self, data_name, x, one_component, is_tuples) result(error)
  !< Write `<DataArray... NumberOfComponents="n"...>...</DataArray>` tag (I4P).
  import :: xml_writer_abstract, I4P
  class(xml_writer_abstract), intent(inout)        :: self           !< Writer.
  character(*),               intent(in)           :: data_name      !< Data name.
  integer(I4P),               intent(in)           :: x(1:,1:,1:,1:) !< Data variable.
  logical,                    intent(in), optional :: one_component  !< Force one component.
  logical,                    intent(in), optional :: is_tuples      !< Use "NumberOfTuples" instead of "NumberOfComponents".
  integer(I4P)                                     :: error          !< Error status.
  endfunction write_dataarray1_rank4_I4P_interface

  function write_dataarray1_rank4_I2P_interface(self, data_name, x, one_component, is_tuples) result(error)
  !< Write `<DataArray... NumberOfComponents="n"...>...</DataArray>` tag (I2P).
  import :: xml_writer_abstract, I2P, I4P
  class(xml_writer_abstract), intent(inout)        :: self           !< Writer.
  character(*),               intent(in)           :: data_name      !< Data name.
  integer(I2P),               intent(in)           :: x(1:,1:,1:,1:) !< Data variable.
  logical,                    intent(in), optional :: one_component  !< Force one component.
  logical,                    intent(in), optional :: is_tuples      !< Use "NumberOfTuples" instead of "NumberOfComponents".
  integer(I4P)                                     :: error          !< Error status.
  endfunction write_dataarray1_rank4_I2P_interface

  function write_dataarray1_rank4_I1P_interface(self, data_name, x, one_component, is_tuples) result(error)
  !< Write `<DataArray... NumberOfComponents="n"...>...</DataArray>` tag (I1P).
  import :: xml_writer_abstract, I1P, I4P
  class(xml_writer_abstract), intent(inout)        :: self           !< Writer.
  character(*),               intent(in)           :: data_name      !< Data name.
  integer(I1P),               intent(in)           :: x(1:,1:,1:,1:) !< Data variable.
  logical,                    intent(in), optional :: one_component  !< Force one component.
  logical,                    intent(in), optional :: is_tuples      !< Use "NumberOfTuples" instead of "NumberOfComponents".
  integer(I4P)                                     :: error          !< Error status.
  endfunction write_dataarray1_rank4_I1P_interface

  function write_dataarray3_rank1_R8P_interface(self, data_name, x, y, z, is_tuples) result(error)
  !< Write `<DataArray... NumberOfComponents="3"...>...</DataArray>` tag (R8P).
  import :: xml_writer_abstract, I4P, R8P
  class(xml_writer_abstract), intent(inout)        :: self         !< Writer.
  character(*),               intent(in)           :: data_name    !< Data name.
  real(R8P),                  intent(in)           :: x(1:)        !< X component of data variable.
  real(R8P),                  intent(in)           :: y(1:)        !< Y component of data variable.
  real(R8P),                  intent(in)           :: z(1:)        !< Z component of data variable.
  logical,                    intent(in), optional :: is_tuples    !< Use "NumberOfTuples" instead of "NumberOfComponents".
  integer(I4P)                                     :: error        !< Error status.
  endfunction write_dataarray3_rank1_R8P_interface

  function write_dataarray3_rank1_R4P_interface(self, data_name, x, y, z, is_tuples) result(error)
  !< Write `<DataArray... NumberOfComponents="3"...>...</DataArray>` tag (R4P).
  import :: xml_writer_abstract, I4P, R4P
  class(xml_writer_abstract), intent(inout)        :: self         !< Writer.
  character(*),               intent(in)           :: data_name    !< Data name.
  real(R4P),                  intent(in)           :: x(1:)        !< X component of data variable.
  real(R4P),                  intent(in)           :: y(1:)        !< Y component of data variable.
  real(R4P),                  intent(in)           :: z(1:)        !< Z component of data variable.
  logical,                    intent(in), optional :: is_tuples    !< Use "NumberOfTuples" instead of "NumberOfComponents".
  integer(I4P)                                     :: error        !< Error status.
  endfunction write_dataarray3_rank1_R4P_interface

  function write_dataarray3_rank1_I8P_interface(self, data_name, x, y, z, is_tuples) result(error)
  !< Write `<DataArray... NumberOfComponents="3"...>...</DataArray>` tag (I8P).
  import :: xml_writer_abstract, I4P, I8P
  class(xml_writer_abstract), intent(inout)        :: self         !< Writer.
  character(*),               intent(in)           :: data_name    !< Data name.
  integer(I8P),               intent(in)           :: x(1:)        !< X component of data variable.
  integer(I8P),               intent(in)           :: y(1:)        !< Y component of data variable.
  integer(I8P),               intent(in)           :: z(1:)        !< Z component of data variable.
  logical,                    intent(in), optional :: is_tuples    !< Use "NumberOfTuples" instead of "NumberOfComponents".
  integer(I4P)                                     :: error        !< Error status.
  endfunction write_dataarray3_rank1_I8P_interface

  function write_dataarray3_rank1_I4P_interface(self, data_name, x, y, z, is_tuples) result(error)
  !< Write `<DataArray... NumberOfComponents="3"...>...</DataArray>` tag (I4P).
  import :: xml_writer_abstract, I4P
  class(xml_writer_abstract), intent(inout)        :: self         !< Writer.
  character(*),               intent(in)           :: data_name    !< Data name.
  integer(I4P),               intent(in)           :: x(1:)        !< X component of data variable.
  integer(I4P),               intent(in)           :: y(1:)        !< Y component of data variable.
  integer(I4P),               intent(in)           :: z(1:)        !< Z component of data variable.
  logical,                    intent(in), optional :: is_tuples    !< Use "NumberOfTuples" instead of "NumberOfComponents".
  integer(I4P)                                     :: error        !< Error status.
  endfunction write_dataarray3_rank1_I4P_interface

  function write_dataarray3_rank1_I2P_interface(self, data_name, x, y, z, is_tuples) result(error)
  !< Write `<DataArray... NumberOfComponents="3"...>...</DataArray>` tag (I2P).
  import :: xml_writer_abstract, I2P, I4P
  class(xml_writer_abstract), intent(inout)        :: self         !< Writer.
  character(*),               intent(in)           :: data_name    !< Data name.
  integer(I2P),               intent(in)           :: x(1:)        !< X component of data variable.
  integer(I2P),               intent(in)           :: y(1:)        !< Y component of data variable.
  integer(I2P),               intent(in)           :: z(1:)        !< Z component of data variable.
  logical,                    intent(in), optional :: is_tuples    !< Use "NumberOfTuples" instead of "NumberOfComponents".
  integer(I4P)                                     :: error        !< Error status.
  endfunction write_dataarray3_rank1_I2P_interface

  function write_dataarray3_rank1_I1P_interface(self, data_name, x, y, z, is_tuples) result(error)
  !< Write `<DataArray... NumberOfComponents="3"...>...</DataArray>` tag (I1P).
  import :: xml_writer_abstract, I1P, I4P
  class(xml_writer_abstract), intent(inout)        :: self         !< Writer.
  character(*),               intent(in)           :: data_name    !< Data name.
  integer(I1P),               intent(in)           :: x(1:)        !< X component of data variable.
  integer(I1P),               intent(in)           :: y(1:)        !< Y component of data variable.
  integer(I1P),               intent(in)           :: z(1:)        !< Z component of data variable.
  logical,                    intent(in), optional :: is_tuples    !< Use "NumberOfTuples" instead of "NumberOfComponents".
  integer(I4P)                                     :: error        !< Error status.
  endfunction write_dataarray3_rank1_I1P_interface

  function write_dataarray3_rank3_R8P_interface(self, data_name, x, y, z, is_tuples) result(error)
  !< Write `<DataArray... NumberOfComponents="3"...>...</DataArray>` tag (R8P).
  import :: xml_writer_abstract, I4P, R8P
  class(xml_writer_abstract), intent(inout)        :: self         !< Writer.
  character(*),               intent(in)           :: data_name    !< Data name.
  real(R8P),                  intent(in)           :: x(1:,1:,1:)  !< X component of data variable.
  real(R8P),                  intent(in)           :: y(1:,1:,1:)  !< Y component of data variable.
  real(R8P),                  intent(in)           :: z(1:,1:,1:)  !< Z component of data variable.
  logical,                    intent(in), optional :: is_tuples    !< Use "NumberOfTuples" instead of "NumberOfComponents".
  integer(I4P)                                     :: error        !< Error status.
  endfunction write_dataarray3_rank3_R8P_interface

  function write_dataarray3_rank3_R4P_interface(self, data_name, x, y, z, is_tuples) result(error)
  !< Write `<DataArray... NumberOfComponents="3"...>...</DataArray>` tag (R4P).
  import :: xml_writer_abstract, I4P, R4P
  class(xml_writer_abstract), intent(inout)        :: self         !< Writer.
  character(*),               intent(in)           :: data_name    !< Data name.
  real(R4P),                  intent(in)           :: x(1:,1:,1:)  !< X component of data variable.
  real(R4P),                  intent(in)           :: y(1:,1:,1:)  !< Y component of data variable.
  real(R4P),                  intent(in)           :: z(1:,1:,1:)  !< Z component of data variable.
  logical,                    intent(in), optional :: is_tuples    !< Use "NumberOfTuples" instead of "NumberOfComponents".
  integer(I4P)                                     :: error        !< Error status.
  endfunction write_dataarray3_rank3_R4P_interface

  function write_dataarray3_rank3_I8P_interface(self, data_name, x, y, z, is_tuples) result(error)
  !< Write `<DataArray... NumberOfComponents="3"...>...</DataArray>` tag (I8P).
  import :: xml_writer_abstract, I4P, I8P
  class(xml_writer_abstract), intent(inout)        :: self         !< Writer.
  character(*),               intent(in)           :: data_name    !< Data name.
  integer(I8P),               intent(in)           :: x(1:,1:,1:)  !< X component of data variable.
  integer(I8P),               intent(in)           :: y(1:,1:,1:)  !< Y component of data variable.
  integer(I8P),               intent(in)           :: z(1:,1:,1:)  !< Z component of data variable.
  logical,                    intent(in), optional :: is_tuples    !< Use "NumberOfTuples" instead of "NumberOfComponents".
  integer(I4P)                                     :: error        !< Error status.
  endfunction write_dataarray3_rank3_I8P_interface

  function write_dataarray3_rank3_I4P_interface(self, data_name, x, y, z, is_tuples) result(error)
  !< Write `<DataArray... NumberOfComponents="3"...>...</DataArray>` tag (I4P).
  import :: xml_writer_abstract, I4P
  class(xml_writer_abstract), intent(inout)        :: self         !< Writer.
  character(*),               intent(in)           :: data_name    !< Data name.
  integer(I4P),               intent(in)           :: x(1:,1:,1:)  !< X component of data variable.
  integer(I4P),               intent(in)           :: y(1:,1:,1:)  !< Y component of data variable.
  integer(I4P),               intent(in)           :: z(1:,1:,1:)  !< Z component of data variable.
  logical,                    intent(in), optional :: is_tuples    !< Use "NumberOfTuples" instead of "NumberOfComponents".
  integer(I4P)                                     :: error        !< Error status.
  endfunction write_dataarray3_rank3_I4P_interface

  function write_dataarray3_rank3_I2P_interface(self, data_name, x, y, z, is_tuples) result(error)
  !< Write `<DataArray... NumberOfComponents="3"...>...</DataArray>` tag (I2P).
  import :: xml_writer_abstract, I2P, I4P
  class(xml_writer_abstract), intent(inout)        :: self         !< Writer.
  character(*),               intent(in)           :: data_name    !< Data name.
  integer(I2P),               intent(in)           :: x(1:,1:,1:)  !< X component of data variable.
  integer(I2P),               intent(in)           :: y(1:,1:,1:)  !< Y component of data variable.
  integer(I2P),               intent(in)           :: z(1:,1:,1:)  !< Z component of data variable.
  logical,                    intent(in), optional :: is_tuples    !< Use "NumberOfTuples" instead of "NumberOfComponents".
  integer(I4P)                                     :: error        !< Error status.
  endfunction write_dataarray3_rank3_I2P_interface

  function write_dataarray3_rank3_I1P_interface(self, data_name, x, y, z, is_tuples) result(error)
  !< Write `<DataArray... NumberOfComponents="3"...>...</DataArray>` tag (I1P).
  import :: xml_writer_abstract, I1P, I4P
  class(xml_writer_abstract), intent(inout)        :: self         !< Writer.
  character(*),               intent(in)           :: data_name    !< Data name.
  integer(I1P),               intent(in)           :: x(1:,1:,1:)  !< X component of data variable.
  integer(I1P),               intent(in)           :: y(1:,1:,1:)  !< Y component of data variable.
  integer(I1P),               intent(in)           :: z(1:,1:,1:)  !< Z component of data variable.
  logical,                    intent(in), optional :: is_tuples    !< Use "NumberOfTuples" instead of "NumberOfComponents".
  integer(I4P)                                     :: error        !< Error status.
  endfunction write_dataarray3_rank3_I1P_interface

  function write_dataarray6_rank1_R8P_interface(self, data_name, u, v, w, x, y, z, is_tuples) result(error)
  !< Write `<DataArray... NumberOfComponents="6"...>...</DataArray>` tag (R8P).
  import :: xml_writer_abstract, I4P, R8P
  class(xml_writer_abstract), intent(inout)        :: self         !< Writer.
  character(*),               intent(in)           :: data_name    !< Data name.
  real(R8P),                  intent(in)           :: u(1:)        !< U component of data variable.
  real(R8P),                  intent(in)           :: v(1:)        !< V component of data variable.
  real(R8P),                  intent(in)           :: w(1:)        !< W component of data variable.
  real(R8P),                  intent(in)           :: x(1:)        !< X component of data variable.
  real(R8P),                  intent(in)           :: y(1:)        !< Y component of data variable.
  real(R8P),                  intent(in)           :: z(1:)        !< Z component of data variable.
  logical,                    intent(in), optional :: is_tuples    !< Use "NumberOfTuples" instead of "NumberOfComponents".
  integer(I4P)                                     :: error        !< Error status.
  endfunction write_dataarray6_rank1_R8P_interface

  function write_dataarray6_rank1_R4P_interface(self, data_name, u, v, w, x, y, z, is_tuples) result(error)
  !< Write `<DataArray... NumberOfComponents="6"...>...</DataArray>` tag (R4P).
  import :: xml_writer_abstract, I4P, R4P
  class(xml_writer_abstract), intent(inout)        :: self         !< Writer.
  character(*),               intent(in)           :: data_name    !< Data name.
  real(R4P),                  intent(in)           :: u(1:)        !< U component of data variable.
  real(R4P),                  intent(in)           :: v(1:)        !< V component of data variable.
  real(R4P),                  intent(in)           :: w(1:)        !< W component of data variable.
  real(R4P),                  intent(in)           :: x(1:)        !< X component of data variable.
  real(R4P),                  intent(in)           :: y(1:)        !< Y component of data variable.
  real(R4P),                  intent(in)           :: z(1:)        !< Z component of data variable.
  logical,                    intent(in), optional :: is_tuples    !< Use "NumberOfTuples" instead of "NumberOfComponents".
  integer(I4P)                                     :: error        !< Error status.
  endfunction write_dataarray6_rank1_R4P_interface

  function write_dataarray6_rank1_I8P_interface(self, data_name, u, v, w, x, y, z, is_tuples) result(error)
  !< Write `<DataArray... NumberOfComponents="6"...>...</DataArray>` tag (I8P).
  import :: xml_writer_abstract, I4P, I8P
  class(xml_writer_abstract), intent(inout)        :: self         !< Writer.
  character(*),               intent(in)           :: data_name    !< Data name.
  integer(I8P),               intent(in)           :: u(1:)        !< U component of data variable.
  integer(I8P),               intent(in)           :: v(1:)        !< V component of data variable.
  integer(I8P),               intent(in)           :: w(1:)        !< W component of data variable.
  integer(I8P),               intent(in)           :: x(1:)        !< X component of data variable.
  integer(I8P),               intent(in)           :: y(1:)        !< Y component of data variable.
  integer(I8P),               intent(in)           :: z(1:)        !< Z component of data variable.
  logical,                    intent(in), optional :: is_tuples    !< Use "NumberOfTuples" instead of "NumberOfComponents".
  integer(I4P)                                     :: error        !< Error status.
  endfunction write_dataarray6_rank1_I8P_interface

  function write_dataarray6_rank1_I4P_interface(self, data_name, u, v, w, x, y, z, is_tuples) result(error)
  !< Write `<DataArray... NumberOfComponents="6"...>...</DataArray>` tag (I4P).
  import :: xml_writer_abstract, I4P
  class(xml_writer_abstract), intent(inout)        :: self         !< Writer.
  character(*),               intent(in)           :: data_name    !< Data name.
  integer(I4P),               intent(in)           :: u(1:)        !< U component of data variable.
  integer(I4P),               intent(in)           :: v(1:)        !< V component of data variable.
  integer(I4P),               intent(in)           :: w(1:)        !< W component of data variable.
  integer(I4P),               intent(in)           :: x(1:)        !< X component of data variable.
  integer(I4P),               intent(in)           :: y(1:)        !< Y component of data variable.
  integer(I4P),               intent(in)           :: z(1:)        !< Z component of data variable.
  logical,                    intent(in), optional :: is_tuples    !< Use "NumberOfTuples" instead of "NumberOfComponents".
  integer(I4P)                                     :: error        !< Error status.
  endfunction write_dataarray6_rank1_I4P_interface

  function write_dataarray6_rank1_I2P_interface(self, data_name, u, v, w, x, y, z, is_tuples) result(error)
  !< Write `<DataArray... NumberOfComponents="6"...>...</DataArray>` tag (I2P).
  import :: xml_writer_abstract, I2P, I4P
  class(xml_writer_abstract), intent(inout)        :: self         !< Writer.
  character(*),               intent(in)           :: data_name    !< Data name.
  integer(I2P),               intent(in)           :: u(1:)        !< U component of data variable.
  integer(I2P),               intent(in)           :: v(1:)        !< V component of data variable.
  integer(I2P),               intent(in)           :: w(1:)        !< W component of data variable.
  integer(I2P),               intent(in)           :: x(1:)        !< X component of data variable.
  integer(I2P),               intent(in)           :: y(1:)        !< Y component of data variable.
  integer(I2P),               intent(in)           :: z(1:)        !< Z component of data variable.
  logical,                    intent(in), optional :: is_tuples    !< Use "NumberOfTuples" instead of "NumberOfComponents".
  integer(I4P)                                     :: error        !< Error status.
  endfunction write_dataarray6_rank1_I2P_interface

  function write_dataarray6_rank1_I1P_interface(self, data_name, u, v, w, x, y, z, is_tuples) result(error)
  !< Write `<DataArray... NumberOfComponents="6"...>...</DataArray>` tag (I1P).
  import :: xml_writer_abstract, I1P, I4P
  class(xml_writer_abstract), intent(inout)        :: self         !< Writer.
  character(*),               intent(in)           :: data_name    !< Data name.
  integer(I1P),               intent(in)           :: u(1:)        !< U component of data variable.
  integer(I1P),               intent(in)           :: v(1:)        !< V component of data variable.
  integer(I1P),               intent(in)           :: w(1:)        !< W component of data variable.
  integer(I1P),               intent(in)           :: x(1:)        !< X component of data variable.
  integer(I1P),               intent(in)           :: y(1:)        !< Y component of data variable.
  integer(I1P),               intent(in)           :: z(1:)        !< Z component of data variable.
  logical,                    intent(in), optional :: is_tuples    !< Use "NumberOfTuples" instead of "NumberOfComponents".
  integer(I4P)                                     :: error        !< Error status.
  endfunction write_dataarray6_rank1_I1P_interface

  function write_dataarray6_rank3_R8P_interface(self, data_name, u, v, w, x, y, z, is_tuples) result(error)
  !< Write `<DataArray... NumberOfComponents="6"...>...</DataArray>` tag (R8P).
  import :: xml_writer_abstract, I4P, R8P
  class(xml_writer_abstract), intent(inout)        :: self         !< Writer.
  character(*),               intent(in)           :: data_name    !< Data name.
  real(R8P),                  intent(in)           :: u(1:,1:,1:)  !< U component of data variable.
  real(R8P),                  intent(in)           :: v(1:,1:,1:)  !< V component of data variable.
  real(R8P),                  intent(in)           :: w(1:,1:,1:)  !< W component of data variable.
  real(R8P),                  intent(in)           :: x(1:,1:,1:)  !< X component of data variable.
  real(R8P),                  intent(in)           :: y(1:,1:,1:)  !< Y component of data variable.
  real(R8P),                  intent(in)           :: z(1:,1:,1:)  !< Z component of data variable.
  logical,                    intent(in), optional :: is_tuples    !< Use "NumberOfTuples" instead of "NumberOfComponents".
  integer(I4P)                                     :: error        !< Error status.
  endfunction write_dataarray6_rank3_R8P_interface

  function write_dataarray6_rank3_R4P_interface(self, data_name, u, v, w, x, y, z, is_tuples) result(error)
  !< Write `<DataArray... NumberOfComponents="6"...>...</DataArray>` tag (R4P).
  import :: xml_writer_abstract, I4P, R4P
  class(xml_writer_abstract), intent(inout)        :: self         !< Writer.
  character(*),               intent(in)           :: data_name    !< Data name.
  real(R4P),                  intent(in)           :: u(1:,1:,1:)  !< U component of data variable.
  real(R4P),                  intent(in)           :: v(1:,1:,1:)  !< V component of data variable.
  real(R4P),                  intent(in)           :: w(1:,1:,1:)  !< W component of data variable.
  real(R4P),                  intent(in)           :: x(1:,1:,1:)  !< X component of data variable.
  real(R4P),                  intent(in)           :: y(1:,1:,1:)  !< Y component of data variable.
  real(R4P),                  intent(in)           :: z(1:,1:,1:)  !< Z component of data variable.
  logical,                    intent(in), optional :: is_tuples    !< Use "NumberOfTuples" instead of "NumberOfComponents".
  integer(I4P)                                     :: error        !< Error status.
  endfunction write_dataarray6_rank3_R4P_interface

  function write_dataarray6_rank3_I8P_interface(self, data_name, u, v, w, x, y, z, is_tuples) result(error)
  !< Write `<DataArray... NumberOfComponents="6"...>...</DataArray>` tag (I8P).
  import :: xml_writer_abstract, I4P, I8P
  class(xml_writer_abstract), intent(inout)        :: self         !< Writer.
  character(*),               intent(in)           :: data_name    !< Data name.
  integer(I8P),               intent(in)           :: u(1:,1:,1:)  !< U component of data variable.
  integer(I8P),               intent(in)           :: v(1:,1:,1:)  !< V component of data variable.
  integer(I8P),               intent(in)           :: w(1:,1:,1:)  !< W component of data variable.
  integer(I8P),               intent(in)           :: x(1:,1:,1:)  !< X component of data variable.
  integer(I8P),               intent(in)           :: y(1:,1:,1:)  !< Y component of data variable.
  integer(I8P),               intent(in)           :: z(1:,1:,1:)  !< Z component of data variable.
  logical,                    intent(in), optional :: is_tuples    !< Use "NumberOfTuples" instead of "NumberOfComponents".
  integer(I4P)                                     :: error        !< Error status.
  endfunction write_dataarray6_rank3_I8P_interface

  function write_dataarray6_rank3_I4P_interface(self, data_name, u, v, w, x, y, z, is_tuples) result(error)
  !< Write `<DataArray... NumberOfComponents="6"...>...</DataArray>` tag (I4P).
  import :: xml_writer_abstract, I4P
  class(xml_writer_abstract), intent(inout)        :: self         !< Writer.
  character(*),               intent(in)           :: data_name    !< Data name.
  integer(I4P),               intent(in)           :: u(1:,1:,1:)  !< U component of data variable.
  integer(I4P),               intent(in)           :: v(1:,1:,1:)  !< V component of data variable.
  integer(I4P),               intent(in)           :: w(1:,1:,1:)  !< W component of data variable.
  integer(I4P),               intent(in)           :: x(1:,1:,1:)  !< X component of data variable.
  integer(I4P),               intent(in)           :: y(1:,1:,1:)  !< Y component of data variable.
  integer(I4P),               intent(in)           :: z(1:,1:,1:)  !< Z component of data variable.
  logical,                    intent(in), optional :: is_tuples    !< Use "NumberOfTuples" instead of "NumberOfComponents".
  integer(I4P)                                     :: error        !< Error status.
  endfunction write_dataarray6_rank3_I4P_interface

  function write_dataarray6_rank3_I2P_interface(self, data_name, u, v, w, x, y, z, is_tuples) result(error)
  !< Write `<DataArray... NumberOfComponents="6"...>...</DataArray>` tag (I2P).
  import :: xml_writer_abstract, I2P, I4P
  class(xml_writer_abstract), intent(inout)        :: self         !< Writer.
  character(*),               intent(in)           :: data_name    !< Data name.
  integer(I2P),               intent(in)           :: u(1:,1:,1:)  !< U component of data variable.
  integer(I2P),               intent(in)           :: v(1:,1:,1:)  !< V component of data variable.
  integer(I2P),               intent(in)           :: w(1:,1:,1:)  !< W component of data variable.
  integer(I2P),               intent(in)           :: x(1:,1:,1:)  !< X component of data variable.
  integer(I2P),               intent(in)           :: y(1:,1:,1:)  !< Y component of data variable.
  integer(I2P),               intent(in)           :: z(1:,1:,1:)  !< Z component of data variable.
  logical,                    intent(in), optional :: is_tuples    !< Use "NumberOfTuples" instead of "NumberOfComponents".
  integer(I4P)                                     :: error        !< Error status.
  endfunction write_dataarray6_rank3_I2P_interface

  function write_dataarray6_rank3_I1P_interface(self, data_name, u, v, w, x, y, z, is_tuples) result(error)
  !< Write `<DataArray... NumberOfComponents="6"...>...</DataArray>` tag (I1P).
  import :: xml_writer_abstract, I1P, I4P
  class(xml_writer_abstract), intent(inout)        :: self         !< Writer.
  character(*),               intent(in)           :: data_name    !< Data name.
  integer(I1P),               intent(in)           :: u(1:,1:,1:)  !< U component of data variable.
  integer(I1P),               intent(in)           :: v(1:,1:,1:)  !< V component of data variable.
  integer(I1P),               intent(in)           :: w(1:,1:,1:)  !< W component of data variable.
  integer(I1P),               intent(in)           :: x(1:,1:,1:)  !< X component of data variable.
  integer(I1P),               intent(in)           :: y(1:,1:,1:)  !< Y component of data variable.
  integer(I1P),               intent(in)           :: z(1:,1:,1:)  !< Z component of data variable.
  logical,                    intent(in), optional :: is_tuples    !< Use "NumberOfTuples" instead of "NumberOfComponents".
  integer(I4P)                                     :: error        !< Error status.
  endfunction write_dataarray6_rank3_I1P_interface

  subroutine write_dataarray_appended_interface(self)
  !< Write `<AppendedData...>...</AppendedData>` tag.
  import :: xml_writer_abstract
  class(xml_writer_abstract), intent(inout) :: self !< Writer.
  endsubroutine write_dataarray_appended_interface
endinterface
contains
   ! files methods
   subroutine close_xml_file(self)
   !< Close XML file.
   class(xml_writer_abstract), intent(inout) :: self !< Writer.

   if (.not.self%is_volatile) close(unit=self%xml, iostat=self%error)
   endsubroutine close_xml_file

   subroutine open_xml_file(self, filename)
   !< Open XML file.
   class(xml_writer_abstract), intent(inout) :: self     !< Writer.
   character(*),               intent(in)    :: filename !< File name.

   if (.not.self%is_volatile) then
      open(newunit=self%xml,             &
           file=trim(adjustl(filename)), &
           form='UNFORMATTED',           &
           access='STREAM',              &
           action='WRITE',               &
           status='REPLACE',             &
           iostat=self%error)
   else
      self%xml_volatile = ''
   endif
   endsubroutine open_xml_file

   elemental subroutine free(self, error)
   !< Free allocated memory.
   class(xml_writer_abstract), intent(inout)         :: self  !< Writer.
   integer(I4P),               intent(out), optional :: error !< Error status.

   call self%format_ch%free
   call self%topology%free
   self%indent=0_I4P
   self%ioffset=0_I8P
   self%xml=0_I4P
   self%vtm_block(1:2)=[-1_I4P, -1_I4P]
   self%error=0_I4P
   call self%tag%free
   self%is_volatile=.false.
   call self%xml_volatile%free
   endsubroutine free

   pure subroutine get_xml_volatile(self, xml_volatile, error)
   !< Return the eventual XML volatile string file.
   class(xml_writer_abstract), intent(in)               :: self         !< Writer.
   character(len=:),           intent(out), allocatable :: xml_volatile !< XML volatile file.
   integer(I4P),               intent(out), optional    :: error        !< Error status.

   if (self%is_volatile) then
      xml_volatile = self%xml_volatile%raw
   endif
   endsubroutine get_xml_volatile

   ! tag methods
   subroutine write_end_tag(self, name)
   !< Write `</tag_name>` end tag.
   class(xml_writer_abstract), intent(inout) :: self !< Writer.
   character(*),               intent(in)    :: name !< Tag name.

   self%indent = self%indent - 2
   self%tag = xml_tag(name=name, indent=self%indent)
   if (.not.self%is_volatile) then
      call self%tag%write(unit=self%xml, iostat=self%error, is_indented=.true., end_record=end_rec, only_end=.true.)
   else
      self%xml_volatile = self%xml_volatile//self%tag%stringify(is_indented=.true., only_end=.true.)//end_rec
   endif
   endsubroutine write_end_tag

   subroutine write_header_tag(self)
   !< Write header tag.
   !<
   !< The header_type (bytes count width) is always declared; when the binary data are compressed, the compressor is declared
   !< too: compressor="vtkZLibDataCompressor" header_type="UInt32".
   class(xml_writer_abstract), intent(inout) :: self   !< Writer.
   type(string)                              :: buffer !< Buffer string.
   character(len=:), allocatable             :: attrs  !< Compressor and header type attributes.

   attrs = ' header_type="'//trim(merge('UInt64', 'UInt32', self%is_uint64))//'"'
   if (self%is_compressed) attrs = ' compressor="vtkZLibDataCompressor"'//attrs
   buffer = '<?xml version="1.0"?>'//end_rec
   if (endian==endianL) then
      buffer = buffer//'<VTKFile type="'//self%topology//'" version="1.0" byte_order="LittleEndian"'//attrs//'>'
   else
      buffer = buffer//'<VTKFile type="'//self%topology//'" version="1.0" byte_order="BigEndian"'//attrs//'>'
   endif
   if (.not.self%is_volatile) then
      write(unit=self%xml, iostat=self%error)buffer//end_rec
   else
      self%xml_volatile = self%xml_volatile//buffer//end_rec
   endif
   self%indent = 2
   endsubroutine write_header_tag

   subroutine write_self_closing_tag(self, name, attributes)
   !< Write `<tag_name.../>` self closing tag.
   class(xml_writer_abstract), intent(inout)        :: self       !< Writer.
   character(*),               intent(in)           :: name       !< Tag name.
   character(*),               intent(in), optional :: attributes !< Tag attributes.

   self%tag = xml_tag(name=name, attributes_stream=attributes, sanitize_attributes_value=.true., indent=self%indent, &
                      is_self_closing=.true.)
   if (.not.self%is_volatile) then
      call self%tag%write(unit=self%xml, iostat=self%error, is_indented=.true., end_record=end_rec)
   else
      self%xml_volatile = self%xml_volatile//self%tag%stringify(is_indented=.true.)//end_rec
   endif
   endsubroutine write_self_closing_tag

   subroutine write_start_tag(self, name, attributes)
   !< Write `<tag_name...>` start tag.
   class(xml_writer_abstract), intent(inout)        :: self       !< Writer.
   character(*),               intent(in)           :: name       !< Tag name.
   character(*),               intent(in), optional :: attributes !< Tag attributes.

   self%tag = xml_tag(name=name, attributes_stream=attributes, sanitize_attributes_value=.true., indent=self%indent)
   if (.not.self%is_volatile) then
      call self%tag%write(unit=self%xml, iostat=self%error, is_indented=.true., end_record=end_rec, only_start=.true.)
   else
      self%xml_volatile = self%xml_volatile//self%tag%stringify(is_indented=.true., only_start=.true.)//end_rec
   endif
   self%indent = self%indent + 2
   endsubroutine write_start_tag

   subroutine write_tag(self, name, attributes, content)
   !< Write `<tag_name...>...</tag_name>` tag.
   class(xml_writer_abstract), intent(inout)        :: self       !< Writer.
   character(*),               intent(in)           :: name       !< Tag name.
   character(*),               intent(in), optional :: attributes !< Tag attributes.
   character(*),               intent(in), optional :: content    !< Tag content.

   self%tag = xml_tag(name=name, attributes_stream=attributes, sanitize_attributes_value=.true., content=content, &
                      indent=self%indent)
   if (.not.self%is_volatile) then
      call self%tag%write(unit=self%xml, iostat=self%error, is_indented=.true., is_content_indented=.true., end_record=end_rec)
   else
      self%xml_volatile = self%xml_volatile//self%tag%stringify(is_indented=.true., is_content_indented=.true.)//end_rec
   endif
   endsubroutine write_tag

   subroutine write_topology_tag(self, nx1, nx2, ny1, ny2, nz1, nz2, mesh_kind)
   !< Write XML topology tag.
   class(xml_writer_abstract), intent(inout)        :: self      !< Writer.
   integer(I4P),               intent(in), optional :: nx1       !< Initial node of x axis.
   integer(I4P),               intent(in), optional :: nx2       !< Final node of x axis.
   integer(I4P),               intent(in), optional :: ny1       !< Initial node of y axis.
   integer(I4P),               intent(in), optional :: ny2       !< Final node of y axis.
   integer(I4P),               intent(in), optional :: nz1       !< Initial node of z axis.
   integer(I4P),               intent(in), optional :: nz2       !< Final node of z axis.
   character(*),               intent(in), optional :: mesh_kind !< Kind of mesh data: Float64, Float32, ecc.
   type(string)                                     :: buffer    !< Buffer string.

   buffer = ''
   select case(self%topology%chars())
   case('RectilinearGrid', 'StructuredGrid')
      buffer = 'WholeExtent="'//                             &
               trim(str(n=nx1))//' '//trim(str(n=nx2))//' '//&
               trim(str(n=ny1))//' '//trim(str(n=ny2))//' '//&
               trim(str(n=nz1))//' '//trim(str(n=nz2))//'"'
   case('PRectilinearGrid', 'PStructuredGrid')
      buffer = 'WholeExtent="'//                             &
               trim(str(n=nx1))//' '//trim(str(n=nx2))//' '//&
               trim(str(n=ny1))//' '//trim(str(n=ny2))//' '//&
               trim(str(n=nz1))//' '//trim(str(n=nz2))//'" GhostLevel="'//trim(str(self%ghost_level, .true.))//'"'
   case('PUnstructuredGrid', 'PPolyData')
      buffer = 'GhostLevel="'//trim(str(self%ghost_level, .true.))//'"'
   case('ImageData')
      buffer = 'WholeExtent="'//                             &
               trim(str(n=nx1))//' '//trim(str(n=nx2))//' '//&
               trim(str(n=ny1))//' '//trim(str(n=ny2))//' '//&
               trim(str(n=nz1))//' '//trim(str(n=nz2))//'"'//self%image_attributes()
   case('PImageData')
      buffer = 'WholeExtent="'//                             &
               trim(str(n=nx1))//' '//trim(str(n=nx2))//' '//&
               trim(str(n=ny1))//' '//trim(str(n=ny2))//' '//&
               trim(str(n=nz1))//' '//trim(str(n=nz2))//'" GhostLevel="'//trim(str(self%ghost_level, .true.))//'"'// &
               self%image_attributes()
   endselect
   call self%write_start_tag(name=self%topology%chars(), attributes=buffer%chars())
   ! parallel topologies peculiars
   select case(self%topology%chars())
   case('PRectilinearGrid')
      if (.not.present(mesh_kind)) then
         self%error = 1
         return
      endif
      call self%write_start_tag(name='PCoordinates')
      call self%write_self_closing_tag(name='PDataArray', attributes='type="'//trim(mesh_kind)//'"')
      call self%write_self_closing_tag(name='PDataArray', attributes='type="'//trim(mesh_kind)//'"')
      call self%write_self_closing_tag(name='PDataArray', attributes='type="'//trim(mesh_kind)//'"')
      call self%write_end_tag(name='PCoordinates')
   case('PStructuredGrid', 'PUnstructuredGrid', 'PPolyData')
      if (.not.present(mesh_kind)) then
         self%error = 1
         return
      endif
      call self%write_start_tag(name='PPoints')
      call self%write_self_closing_tag(name='PDataArray', &
                                       attributes='type="'//trim(mesh_kind)//'" NumberOfComponents="3" Name="Points"')
      call self%write_end_tag(name='PPoints')
   endselect
   endsubroutine write_topology_tag

   function image_attributes(self) result(attributes)
   !< Return the ` Origin="..." Spacing="..." [Direction="..."]` attributes of ImageData topologies.
   !<
   !< Values are written in the shortest form that reads back exactly.
   class(xml_writer_abstract), intent(in) :: self       !< Writer.
   character(len=:), allocatable          :: attributes !< Attributes, with a leading blank.

   attributes = ' Origin="'//reals(self%origin)//'" Spacing="'//reals(self%spacing)//'"'
   if (self%is_direction_set) attributes = attributes//' Direction="'//reals(self%direction)//'"'
   contains
      function reals(x) result(values)
      !< Return space separated values, without the `+` of positive values (PENF `no_sign` would drop `-` too).
      real(R8P), intent(in)         :: x(:)   !< Values.
      character(len=:), allocatable :: values !< Space separated values.
      character(len=:), allocatable :: value  !< One value.
      integer                       :: i      !< Counter.

      values = ''
      do i=1, size(x)
         value = trim(str(n=x(i), compact=.true.))
         if (value(1:1) == '+') value = value(2:)
         values = values//' '//value
      enddo
      values = values(2:)
      endfunction reals
   endfunction image_attributes

   ! write_dataarray
   subroutine write_dataarray_tag(self, data_type, number_of_components, data_name, data_content, is_tuples)
   !< Write `<DataArray...>...</DataArray>` tag.
   class(xml_writer_abstract), intent(inout)        :: self                 !< Writer.
   character(*),               intent(in)           :: data_type            !< Type of dataarray.
   integer(I4P),               intent(in)           :: number_of_components !< Number of dataarray components.
   character(*),               intent(in)           :: data_name            !< Data name.
   character(*),               intent(in), optional :: data_content         !< Data content.
   logical,                    intent(in), optional :: is_tuples            !< Use "NumberOfTuples".
   type(string)                                     :: tag_attributes       !< Tag attributes.
   logical                                          :: is_tuples_           !< Use "NumberOfTuples".
   character(len=:), allocatable                    :: data_type_           !< Type of dataarray, actually written.
   character(len=:), allocatable                    :: tag_name             !< Element name, actually written.
   character(len=:), allocatable                    :: count                !< Components (or tuples) count, actually written.

   call self%dataarray_tag_overrides(data_type=data_type, number_of_components=number_of_components, &
                                     tag_name=tag_name, data_type_=data_type_, count=count)
   is_tuples_ = .false.
   if (present(is_tuples)) is_tuples_ = is_tuples
   if (is_tuples_) then
      tag_attributes = 'type="'//data_type_//             &
        '" NumberOfTuples="'//count// &
        '" Name="'//trim(adjustl(data_name))//                          &
        '" format="'//self%format_ch//'"'
   else
      tag_attributes = 'type="'//data_type_//                 &
        '" NumberOfComponents="'//count// &
        '" Name="'//trim(adjustl(data_name))//                              &
        '" format="'//self%format_ch//'"'
   endif
   if (present(data_content).and.(.not.self%is_volatile)) then
      ! content written as is between start and end tags (same bytes of write_tag): building the whole tag as a single
      ! string makes full-size copies of the content, some of them stack temporaries with some compilers (issue #70)
      call self%write_start_tag(name=tag_name, attributes=tag_attributes%chars())
      write(unit=self%xml, iostat=self%error)repeat(' ', self%indent), data_content, end_rec
      call self%write_end_tag(name=tag_name)
   else
      call self%write_tag(name=tag_name, attributes=tag_attributes%chars(), content=data_content)
   endif
   endsubroutine write_dataarray_tag

   subroutine write_dataarray_tag_appended(self, data_type, number_of_components, data_name, is_tuples)
   !< Write `<DataArray.../>` tag.
   class(xml_writer_abstract), intent(inout)        :: self                 !< Writer.
   character(*),               intent(in)           :: data_type            !< Type of dataarray.
   integer(I4P),               intent(in)           :: number_of_components !< Number of dataarray components.
   character(*),               intent(in)           :: data_name            !< Data name.
   logical,                    intent(in), optional :: is_tuples            !< Use "NumberOfTuples".
   type(string)                                     :: tag_attributes       !< Tag attributes.
   logical                                          :: is_tuples_           !< Use "NumberOfTuples".
   character(len=:), allocatable                    :: data_type_           !< Type of dataarray, actually written.
   character(len=:), allocatable                    :: tag_name             !< Element name, actually written.
   character(len=:), allocatable                    :: count                !< Components (or tuples) count, actually written.

   call self%dataarray_tag_overrides(data_type=data_type, number_of_components=number_of_components, &
                                     tag_name=tag_name, data_type_=data_type_, count=count)
   is_tuples_ = .false.
   if (present(is_tuples)) is_tuples_ = is_tuples
   if (is_tuples_) then
      tag_attributes =  'type="'//data_type_//            &
        '" NumberOfTuples="'//count// &
        '" Name="'//trim(adjustl(data_name))//                          &
        '" format="'//self%format_ch//                                  &
        '" offset="'//trim(str(self%ioffset, .true.))//'"'
   else
      tag_attributes = 'type="'//data_type_//                 &
        '" NumberOfComponents="'//count// &
        '" Name="'//trim(adjustl(data_name))//                              &
        '" format="'//self%format_ch//                                      &
        '" offset="'//trim(str(self%ioffset, .true.))//'"'
   endif
   call self%write_self_closing_tag(name=tag_name, attributes=tag_attributes%chars())
   endsubroutine write_dataarray_tag_appended

   subroutine dataarray_tag_overrides(self, data_type, number_of_components, tag_name, data_type_, count)
   !< Return element name, type and count of the next DataArray tag, applying (then unsetting) the one-shot overrides.
   !<
   !< The overrides let a caller tag specially one array written through the generic DataArray writers: cell types as UInt8,
   !< strings as `<Array type="String" NumberOfTuples="n">` (bytes written as Int8), field data arrays with their tuples count.
   class(xml_writer_abstract),    intent(inout) :: self                 !< Writer.
   character(*),                  intent(in)    :: data_type            !< Type of dataarray.
   integer(I4P),                  intent(in)    :: number_of_components !< Number of dataarray components.
   character(len=:), allocatable, intent(out)   :: tag_name             !< Element name.
   character(len=:), allocatable, intent(out)   :: data_type_           !< Type of dataarray.
   character(len=:), allocatable, intent(out)   :: count                !< Components (or tuples) count.

   tag_name = 'DataArray'
   if (self%tag_name_override%is_allocated()) then
      tag_name = self%tag_name_override%chars()
      call self%tag_name_override%free
   endif
   data_type_ = trim(adjustl(data_type))
   if (self%data_type_override%is_allocated()) then
      data_type_ = self%data_type_override%chars()
      call self%data_type_override%free
   endif
   count = trim(str(number_of_components, .true.))
   if (self%tuples_override >= 0_I8P) then
      count = trim(str(self%tuples_override, .true.))
      self%tuples_override = -1_I8P
   endif
   endsubroutine dataarray_tag_overrides

   function write_dataarray_location_tag(self, location, action, scalars, vectors, normals, tensors, tcoords) result(error)
   !< Write `<[/]PointData>` or `<[/]CellData>` open/close tag (`<[/]PPointData>` or `<[/]PCellData>` for parallel files).
   !<
   !< @note **must** be called before saving the data related to geometric mesh, this function initializes the
   !< saving of data variables indicating the *location* (node or cell centered) of variables that will be saved.
   !<
   !< @note A single file can contain both cell and node centered variables. In this case this function must be
   !< called two times, before saving cell-centered variables and before saving node-centered variables.
   !<
   !< @note When opening, the optional `scalars`, `vectors`, `normals`, `tensors` and `tcoords` arguments designate the
   !< *active* array of each role, written as the `Scalars`, `Vectors`, `Normals`, `Tensors` and `TCoords` attributes of the
   !< tag: readers (e.g. ParaView) use it as the default array of that role. Each argument is the `data_name` of an array
   !< written (or, for parallel files, declared) inside this tag; the name is not checked. They are ignored when closing.
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
   !<
   !<#### Opening node piece designating the active scalars and vectors
   !<```fortran
   !< error = vtk%write_dataarray(location='node', action='open', scalars='pressure', vectors='velocity')
   !<```
   class(xml_writer_abstract), intent(inout)        :: self       !< Writer.
   character(*),               intent(in)           :: location   !< Location of variables: **cell** or **node** centered.
   character(*),               intent(in)           :: action     !< Action: **open** or **close** tag.
   character(*),               intent(in), optional :: scalars    !< Name of the active scalars array.
   character(*),               intent(in), optional :: vectors    !< Name of the active vectors array.
   character(*),               intent(in), optional :: normals    !< Name of the active normals array.
   character(*),               intent(in), optional :: tensors    !< Name of the active tensors array.
   character(*),               intent(in), optional :: tcoords    !< Name of the active texture coordinates array.
   integer(I4P)                                     :: error      !< Error status.
   type(string)                                     :: location_  !< Location string.
   type(string)                                     :: action_    !< Action string.
   character(len=:), allocatable                    :: attributes !< Active arrays attributes.

   location_ = trim(adjustl(location)) ; location_ = location_%upper()
   action_ = trim(adjustl(action)) ; action_ = action_%upper()
   select case(location_%chars())
   case('CELL')
      location_ = 'CellData'
   case('NODE')
      location_ = 'PointData'
   endselect
   select case(self%topology%chars())
   case('PRectilinearGrid', 'PStructuredGrid', 'PUnstructuredGrid', 'PImageData', 'PPolyData')
      location_ = 'P'//location_
   endselect
   select case(action_%chars())
   case('OPEN')
      attributes = ''
      if (present(scalars)) attributes = attributes//' Scalars="'//trim(adjustl(scalars))//'"'
      if (present(vectors)) attributes = attributes//' Vectors="'//trim(adjustl(vectors))//'"'
      if (present(normals)) attributes = attributes//' Normals="'//trim(adjustl(normals))//'"'
      if (present(tensors)) attributes = attributes//' Tensors="'//trim(adjustl(tensors))//'"'
      if (present(tcoords)) attributes = attributes//' TCoords="'//trim(adjustl(tcoords))//'"'
      if (len(attributes) > 0) then
         call self%write_start_tag(name=location_%chars(), attributes=attributes(2:))
      else
         call self%write_start_tag(name=location_%chars())
      endif
   case('CLOSE')
      call self%write_end_tag(name=location_%chars())
   endselect
   error = self%error
   endfunction write_dataarray_location_tag

   ! write_fielddata methods
   function write_fielddata1_rank0(self, data_name, x) result(error)
   !< Write one FieldData value: a number (any kind) or a string.
   !<
   !< A number is written as `<DataArray ... NumberOfTuples="1">`, a string as `<Array type="String" NumberOfTuples="1">`
   !< (its characters followed by a NUL, as VTK writes strings; trailing blanks are trimmed).
   !<
   !<### Example of usage
   !<
   !<```fortran
   !< error = vtk%xml_writer%write_fielddata(action='open')
   !< error = vtk%xml_writer%write_fielddata(data_name='TIME', x=0.1_R8P)
   !< error = vtk%xml_writer%write_fielddata(data_name='solver', x='my solver v1.2')
   !< error = vtk%xml_writer%write_fielddata(action='close')
   !<```
   class(xml_writer_abstract), intent(inout) :: self      !< Writer.
   character(*),               intent(in)    :: data_name !< Data name.
   class(*),                   intent(in)    :: x         !< Data variable.
   integer(I4P)                              :: error     !< Error status.

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
   type is(character(*))
      self%error = self%write_fielddata_strings(data_name=data_name, x=[x])
   endselect
   error = self%error
   endfunction write_fielddata1_rank0

   function write_fielddata1_rank1(self, data_name, x) result(error)
   !< Write a FieldData array: numbers (any kind) or strings, one tuple per element.
   !<
   !< Numbers are written as `<DataArray ... NumberOfTuples="size(x)">` (one component per tuple), strings as
   !< `<Array type="String" NumberOfTuples="size(x)">` (each string followed by a NUL, as VTK writes strings; trailing blanks
   !< of each element are trimmed).
   !<
   !<### Example of usage
   !<
   !<```fortran
   !< error = vtk%xml_writer%write_fielddata(data_name='residuals', x=[1.e-3_R8P, 1.e-4_R8P, 1.e-5_R8P])
   !< error = vtk%xml_writer%write_fielddata(data_name='species', x=['N2', 'O2'])
   !<```
   class(xml_writer_abstract), intent(inout) :: self      !< Writer.
   character(*),               intent(in)    :: data_name !< Data name.
   class(*),                   intent(in)    :: x(1:)     !< Data variable.
   integer(I4P)                              :: error     !< Error status.

   select type(x)
   type is(character(*))
      self%error = self%write_fielddata_strings(data_name=data_name, x=x)
   class default
      self%tuples_override = size(x, kind=I8P)
      select type(x)
      type is(real(R8P))
         self%error = self%write_dataarray(data_name=data_name, x=x, is_tuples=.true.)
      type is(real(R4P))
         self%error = self%write_dataarray(data_name=data_name, x=x, is_tuples=.true.)
      type is(integer(I8P))
         self%error = self%write_dataarray(data_name=data_name, x=x, is_tuples=.true.)
      type is(integer(I4P))
         self%error = self%write_dataarray(data_name=data_name, x=x, is_tuples=.true.)
      type is(integer(I2P))
         self%error = self%write_dataarray(data_name=data_name, x=x, is_tuples=.true.)
      type is(integer(I1P))
         self%error = self%write_dataarray(data_name=data_name, x=x, is_tuples=.true.)
      class default
         self%tuples_override = -1_I8P
         self%error = 1
      endselect
   endselect
   error = self%error
   endfunction write_fielddata1_rank1

   function write_fielddata_strings(self, data_name, x) result(error)
   !< Write FieldData strings as VTK does: `<Array type="String" NumberOfTuples="size(x)">`, the bytes of each string (trailing
   !< blanks trimmed) followed by a NUL, encoded as any Int8 data in the selected format.
   class(xml_writer_abstract), intent(inout) :: self      !< Writer.
   character(*),               intent(in)    :: data_name !< Data name.
   character(*),               intent(in)    :: x(1:)     !< Strings.
   integer(I4P)                              :: error     !< Error status.
   integer(I1P), allocatable                 :: bytes(:)  !< Strings bytes, NUL terminated.
   integer(I4P)                              :: i         !< Counter.
   integer(I4P)                              :: c         !< Counter.
   integer(I4P)                              :: b         !< Counter.

   allocate(bytes(1:sum(len_trim(x)) + size(x)))
   b = 0
   do i=1, size(x)
      do c=1, len_trim(x(i))
         b = b + 1
         bytes(b) = transfer(x(i)(c:c), 0_I1P)
      enddo
      b = b + 1
      bytes(b) = 0_I1P
   enddo
   self%tag_name_override = 'Array'
   self%data_type_override = 'String'
   self%tuples_override = size(x, kind=I8P)
   error = self%write_dataarray(data_name=data_name, x=bytes, is_tuples=.true.)
   endfunction write_fielddata_strings

   function write_fielddata_tag(self, action) result(error)
   !< Write `<FieldData>`/`</FieldData>` start/end tag.
   class(xml_writer_abstract), intent(inout) :: self      !< Writer.
   character(*),               intent(in)    :: action    !< Action: **open** or **close** tag.
   integer(I4P)                              :: error     !< Error status.
   type(string)                              :: action_   !< Action string.

   action_ = trim(adjustl(action)) ; action_ = action_%upper()
   select case(action_%chars())
   case('OPEN')
      call self%write_start_tag(name='FieldData')
   case('CLOSE')
      call self%write_end_tag(name='FieldData')
   endselect
   error = self%error
   endfunction write_fielddata_tag

   ! write_piece methods
   function write_piece_start_tag(self, nx1, nx2, ny1, ny2, nz1, nz2) result(error)
   !< Write `<Piece ...>` start tag.
   class(xml_writer_abstract), intent(inout) :: self           !< Writer.
   integer(I4P),               intent(in)    :: nx1            !< Initial node of x axis.
   integer(I4P),               intent(in)    :: nx2            !< Final node of x axis.
   integer(I4P),               intent(in)    :: ny1            !< Initial node of y axis.
   integer(I4P),               intent(in)    :: ny2            !< Final node of y axis.
   integer(I4P),               intent(in)    :: nz1            !< Initial node of z axis.
   integer(I4P),               intent(in)    :: nz2            !< Final node of z axis.
   integer(I4P)                              :: error          !< Error status.
   type(string)                              :: tag_attributes !< Tag attributes.

   tag_attributes = 'Extent="'//trim(str(n=nx1))//' '//trim(str(n=nx2))//' '// &
                                trim(str(n=ny1))//' '//trim(str(n=ny2))//' '// &
                                trim(str(n=nz1))//' '//trim(str(n=nz2))//'"'
   call self%write_start_tag(name='Piece', attributes=tag_attributes%chars())
   error = self%error
   endfunction write_piece_start_tag

   function write_piece_start_tag_unst(self, np, nc) result(error)
   !< Write `<Piece ...>` start tag for unstructured topology.
   class(xml_writer_abstract), intent(inout) :: self           !< Writer.
   integer(I4P),               intent(in)    :: np             !< Number of points.
   integer(I4P),               intent(in)    :: nc             !< Number of cells.
   integer(I4P)                              :: error          !< Error status.
   type(string)                              :: tag_attributes !< Tag attributes.

   tag_attributes = 'NumberOfPoints="'//trim(str(n=np))//'" NumberOfCells="'//trim(str(n=nc))//'"'
   call self%write_start_tag(name='Piece', attributes=tag_attributes%chars())
   error = self%error
   endfunction write_piece_start_tag_unst

   function write_piece_start_tag_poly(self, np, nverts, nlines, nstrips, npolys) result(error)
   !< Write `<Piece ...>` start tag for polydata topology: the number of points and of cells of each block.
   class(xml_writer_abstract), intent(inout) :: self           !< Writer.
   integer(I4P),               intent(in)    :: np             !< Number of points.
   integer(I4P),               intent(in)    :: nverts         !< Number of vertex cells (block Verts).
   integer(I4P),               intent(in)    :: nlines         !< Number of line/polyline cells (block Lines).
   integer(I4P),               intent(in)    :: nstrips        !< Number of triangle strip cells (block Strips).
   integer(I4P),               intent(in)    :: npolys         !< Number of polygon cells (block Polys).
   integer(I4P)                              :: error          !< Error status.
   type(string)                              :: tag_attributes !< Tag attributes.

   tag_attributes = 'NumberOfPoints="'//trim(str(n=np, no_sign=.true.))//     &
                    '" NumberOfVerts="'//trim(str(n=nverts, no_sign=.true.))//  &
                    '" NumberOfLines="'//trim(str(n=nlines, no_sign=.true.))//  &
                    '" NumberOfStrips="'//trim(str(n=nstrips, no_sign=.true.))//&
                    '" NumberOfPolys="'//trim(str(n=npolys, no_sign=.true.))//'"'
   call self%write_start_tag(name='Piece', attributes=tag_attributes%chars())
   error = self%error
   endfunction write_piece_start_tag_poly

   function write_piece_end_tag(self) result(error)
   !< Write `</Piece>` end tag.
   class(xml_writer_abstract), intent(inout) :: self  !< Writer.
   integer(I4P)                              :: error !< Error status.

   call self%write_end_tag(name='Piece')
   error = self%error
   endfunction write_piece_end_tag

   ! write_geo_rect methods
   function write_geo_rect_data3_rank1_R8P(self, x, y, z) result(error)
   !< Write mesh with **RectilinearGrid** topology (data 3, rank 1, R8P).
   class(xml_writer_abstract), intent(inout) :: self  !< Writer.
   real(R8P),                  intent(in)    :: x(1:) !< X coordinates.
   real(R8P),                  intent(in)    :: y(1:) !< Y coordinates.
   real(R8P),                  intent(in)    :: z(1:) !< Z coordinates.
   integer(I4P)                              :: error !< Error status.

   call self%write_start_tag(name='Coordinates')
   error = self%write_dataarray(data_name='X', x=x)
   error = self%write_dataarray(data_name='Y', x=y)
   error = self%write_dataarray(data_name='Z', x=z)
   call self%write_end_tag(name='Coordinates')
   error = self%error
   endfunction write_geo_rect_data3_rank1_R8P

   function write_geo_rect_data3_rank1_R4P(self, x, y, z) result(error)
   !< Write mesh with **RectilinearGrid** topology (data 3, rank 1, R4P).
   class(xml_writer_abstract), intent(inout) :: self  !< Writer.
   real(R4P),                  intent(in)    :: x(1:) !< X coordinates.
   real(R4P),                  intent(in)    :: y(1:) !< Y coordinates.
   real(R4P),                  intent(in)    :: z(1:) !< Z coordinates.
   integer(I4P)                              :: error !< Error status.

   call self%write_start_tag(name='Coordinates')
   error = self%write_dataarray(data_name='X', x=x)
   error = self%write_dataarray(data_name='Y', x=y)
   error = self%write_dataarray(data_name='Z', x=z)
   call self%write_end_tag(name='Coordinates')
   error = self%error
   endfunction write_geo_rect_data3_rank1_R4P

   ! write_geo_strg methods
   function write_geo_strg_data1_rank2_R8P(self, xyz) result(error)
   !< Write mesh with **StructuredGrid** topology (data 1, rank 2, R8P).
   class(xml_writer_abstract), intent(inout) :: self       !< Writer.
   real(R8P),                  intent(in)    :: xyz(1:,1:) !< X, y, z coordinates [1:3,1:n].
   integer(I4P)                              :: error      !< Error status.

   call self%write_start_tag(name='Points')
   error = self%write_dataarray(data_name='Points', x=xyz)
   call self%write_end_tag(name='Points')
   error = self%error
   endfunction write_geo_strg_data1_rank2_R8P

   function write_geo_strg_data1_rank2_R4P(self, xyz) result(error)
   !< Write mesh with **StructuredGrid** topology (data 1, rank 2, R4P).
   class(xml_writer_abstract), intent(inout) :: self       !< Writer.
   real(R4P),                  intent(in)    :: xyz(1:,1:) !< X, y, z coordinates [1:3,:].
   integer(I4P)                              :: error      !< Error status.

   call self%write_start_tag(name='Points')
   error = self%write_dataarray(data_name='Points', x=xyz)
   call self%write_end_tag(name='Points')
   error = self%error
   endfunction write_geo_strg_data1_rank2_R4P

   function write_geo_strg_data1_rank4_R8P(self, xyz) result(error)
   !< Write mesh with **StructuredGrid** topology (data 1, rank 4, R8P).
   class(xml_writer_abstract), intent(inout) :: self             !< Writer.
   real(R8P),                  intent(in)    :: xyz(1:,1:,1:,1:) !< X, y, z coordinates [1:3,:,:,:].
   integer(I4P)                              :: error            !< Error status.

   call self%write_start_tag(name='Points')
   error = self%write_dataarray(data_name='Points', x=xyz)
   call self%write_end_tag(name='Points')
   error = self%error
   endfunction write_geo_strg_data1_rank4_R8P

   function write_geo_strg_data1_rank4_R4P(self, xyz) result(error)
   !< Write mesh with **StructuredGrid** topology (data 1, rank 4, R4P).
   class(xml_writer_abstract), intent(inout) :: self             !< Writer.
   real(R4P),                  intent(in)    :: xyz(1:,1:,1:,1:) !< X, y, z coordinates [1:3,:,:,:].
   integer(I4P)                              :: error            !< Error status.

   call self%write_start_tag(name='Points')
   error = self%write_dataarray(data_name='Points', x=xyz)
   call self%write_end_tag(name='Points')
   error = self%error
   endfunction write_geo_strg_data1_rank4_R4P

   function write_geo_strg_data3_rank1_R8P(self, n, x, y, z) result(error)
   !< Write mesh with **StructuredGrid** topology (data 3, rank 1, R8P).
   class(xml_writer_abstract), intent(inout) :: self  !< Writer.
   integer(I4P),               intent(in)    :: n     !< Number of nodes.
   real(R8P),                  intent(in)    :: x(1:) !< X coordinates.
   real(R8P),                  intent(in)    :: y(1:) !< Y coordinates.
   real(R8P),                  intent(in)    :: z(1:) !< Z coordinates.
   integer(I4P)                              :: error !< Error status.

   if ((n/=size(x, dim=1)).or.(n/=size(y, dim=1)).or.(n/=size(z, dim=1))) then
      error = 1 ; self%error = error
      return
   endif
   call self%write_start_tag(name='Points')
   error = self%write_dataarray(data_name='Points', x=x, y=y, z=z)
   call self%write_end_tag(name='Points')
   error = self%error
   endfunction write_geo_strg_data3_rank1_R8P

   function write_geo_strg_data3_rank1_R4P(self, n, x, y, z) result(error)
   !< Write mesh with **StructuredGrid** topology (data 3, rank 1, R4P).
   class(xml_writer_abstract), intent(inout) :: self  !< Writer.
   integer(I4P),               intent(in)    :: n     !< Number of nodes.
   real(R4P),                  intent(in)    :: x(1:) !< X coordinates.
   real(R4P),                  intent(in)    :: y(1:) !< Y coordinates.
   real(R4P),                  intent(in)    :: z(1:) !< Z coordinates.
   integer(I4P)                              :: error !< Error status.

   if ((n/=size(x, dim=1)).or.(n/=size(y, dim=1)).or.(n/=size(z, dim=1))) then
      error = 1 ; self%error = error
      return
   endif
   call self%write_start_tag(name='Points')
   error = self%write_dataarray(data_name='Points', x=x, y=y, z=z)
   call self%write_end_tag(name='Points')
   error = self%error
   endfunction write_geo_strg_data3_rank1_R4P

   function write_geo_strg_data3_rank3_R8P(self, n, x, y, z) result(error)
   !< Write mesh with **StructuredGrid** topology (data 3, rank 3, R8P).
   class(xml_writer_abstract), intent(inout) :: self        !< Writer.
   integer(I4P),               intent(in)    :: n           !< Number of nodes.
   real(R8P),                  intent(in)    :: x(1:,1:,1:) !< X coordinates.
   real(R8P),                  intent(in)    :: y(1:,1:,1:) !< Y coordinates.
   real(R8P),                  intent(in)    :: z(1:,1:,1:) !< Z coordinates.
   integer(I4P)                              :: error       !< Error status.

   if ((n/=size(x, dim=1)*size(x, dim=2)*size(x, dim=3)).or.&
       (n/=size(y, dim=1)*size(y, dim=2)*size(y, dim=3)).or.&
       (n/=size(z, dim=1)*size(z, dim=2)*size(z, dim=3))) then
      error = 1 ; self%error = error
      return
   endif
   call self%write_start_tag(name='Points')
   error = self%write_dataarray(data_name='Points', x=x, y=y, z=z)
   call self%write_end_tag(name='Points')
   error = self%error
   endfunction write_geo_strg_data3_rank3_R8P

   function write_geo_strg_data3_rank3_R4P(self, n, x, y, z) result(error)
   !< Write mesh with **StructuredGrid** topology (data 3, rank 3, R4P).
   class(xml_writer_abstract), intent(inout) :: self        !< Writer.
   integer(I4P),               intent(in)    :: n           !< Number of nodes.
   real(R4P),                  intent(in)    :: x(1:,1:,1:) !< X coordinates.
   real(R4P),                  intent(in)    :: y(1:,1:,1:) !< Y coordinates.
   real(R4P),                  intent(in)    :: z(1:,1:,1:) !< Z coordinates.
   integer(I4P)                              :: error       !< Error status.

   if ((n/=size(x, dim=1)*size(x, dim=2)*size(x, dim=3)).or.&
       (n/=size(y, dim=1)*size(y, dim=2)*size(y, dim=3)).or.&
       (n/=size(z, dim=1)*size(z, dim=2)*size(z, dim=3))) then
      error = 1 ; self%error = error
      return
   endif
   call self%write_start_tag(name='Points')
   error = self%write_dataarray(data_name='Points', x=x, y=y, z=z)
   call self%write_end_tag(name='Points')
   error = self%error
   endfunction write_geo_strg_data3_rank3_R4P

   ! write_geo_unst methods
   function write_geo_unst_data1_rank2_R8P(self, np, nc, xyz) result(error)
   !< Write mesh with **UnstructuredGrid** topology (data 1, rank 2, R8P).
   class(xml_writer_abstract), intent(inout) :: self       !< Writer.
   integer(I4P),               intent(in)    :: np         !< Number of points.
   integer(I4P),               intent(in)    :: nc         !< Number of cells.
   real(R8P),                  intent(in)    :: xyz(1:,1:) !< X, y, z coordinates [1:3,:].
   integer(I4P)                              :: error      !< Error status.

   if (np/=size(xyz, dim=2)) then
      error = 1 ; self%error = error
      return
   endif
   call self%write_start_tag(name='Points')
   error = self%write_dataarray(data_name='Points', x=xyz)
   call self%write_end_tag(name='Points')
   error = self%error
   endfunction write_geo_unst_data1_rank2_R8P

   function write_geo_unst_data1_rank2_R4P(self, np, nc, xyz) result(error)
   !< Write mesh with **UnstructuredGrid** topology (data 1, rank 2, R4P).
   class(xml_writer_abstract), intent(inout) :: self       !< Writer.
   integer(I4P),               intent(in)    :: np         !< Number of points.
   integer(I4P),               intent(in)    :: nc         !< Number of cells.
   real(R4P),                  intent(in)    :: xyz(1:,1:) !< X, y, z coordinates [1:3,:].
   integer(I4P)                              :: error      !< Error status.

   if (np/=size(xyz, dim=2)) then
      error = 1 ; self%error = error
      return
   endif
   call self%write_start_tag(name='Points')
   error = self%write_dataarray(data_name='Points', x=xyz)
   call self%write_end_tag(name='Points')
   error = self%error
   endfunction write_geo_unst_data1_rank2_R4P

   function write_geo_unst_data3_rank1_R8P(self, np, nc, x, y, z) result(error)
   !< Write mesh with **UnstructuredGrid** topology (data 3, rank 1, R8P).
   class(xml_writer_abstract), intent(inout) :: self  !< Writer.
   integer(I4P),               intent(in)    :: np    !< Number of points.
   integer(I4P),               intent(in)    :: nc    !< Number of cells.
   real(R8P),                  intent(in)    :: x(1:) !< X coordinates.
   real(R8P),                  intent(in)    :: y(1:) !< Y coordinates.
   real(R8P),                  intent(in)    :: z(1:) !< Z coordinates.
   integer(I4P)                              :: error !< Error status.

   if ((np/=size(x, dim=1)).or.(np/=size(y, dim=1)).or.(np/=size(z, dim=1))) then
      error = 1 ; self%error = error
      return
   endif
   call self%write_start_tag(name='Points')
   error = self%write_dataarray(data_name='Points', x=x, y=y, z=z)
   call self%write_end_tag(name='Points')
   error = self%error
   endfunction write_geo_unst_data3_rank1_R8P

   function write_geo_unst_data3_rank1_R4P(self, np, nc, x, y, z) result(error)
   !< Write mesh with **UnstructuredGrid** topology (data 3, rank 1, R4P).
   class(xml_writer_abstract), intent(inout) :: self  !< Writer.
   integer(I4P),               intent(in)    :: np    !< Number of points.
   integer(I4P),               intent(in)    :: nc    !< Number of cells.
   real(R4P),                  intent(in)    :: x(1:) !< X coordinates.
   real(R4P),                  intent(in)    :: y(1:) !< Y coordinates.
   real(R4P),                  intent(in)    :: z(1:) !< Z coordinates.
   integer(I4P)                              :: error !< Error status.

   if ((np/=size(x, dim=1)).or.(np/=size(y, dim=1)).or.(np/=size(z, dim=1))) then
      error = 1 ; self%error = error
      return
   endif
   call self%write_start_tag(name='Points')
   error = self%write_dataarray(data_name='Points', x=x, y=y, z=z)
   call self%write_end_tag(name='Points')
   error = self%error
   endfunction write_geo_unst_data3_rank1_R4P

   function write_connectivity(self, nc, connectivity, offset, cell_type, face, faceoffset) result(error)
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
   class(xml_writer_abstract), intent(inout) :: self             !< Writer.
   integer(I4P),               intent(in)    :: nc               !< Number of cells.
   integer(I4P),               intent(in)    :: connectivity(1:) !< Mesh connectivity.
   integer(I4P),               intent(in)    :: offset(1:)       !< Cell offset.
   integer(I4P),   optional,   intent(in)    :: face(1:)         !< face composing the polyhedra.
   integer(I4P),   optional,   intent(in)    :: faceoffset(1:)   !< face offset.
   integer(I1P),               intent(in)    :: cell_type(1:)    !< VTK cell type.
   integer(I4P)                              :: error            !< Error status.

   call self%write_start_tag(name='Cells')
   error = self%write_dataarray(data_name='connectivity', x=connectivity)
   error = self%write_dataarray(data_name='offsets', x=offset)
   ! cell types are written as UInt8, as the VTK XML format specifies (same bytes of Int8, cell type codes are < 128)
   self%data_type_override = 'UInt8'
   error = self%write_dataarray(data_name='types', x=cell_type)
   !< Add faces and faceoffsets to the cell block for polyhedra. If the cell is not a polyhedron, its offset must be set to -1.
   !< They must be children of the Cells element, otherwise readers ignore them (issue #31).
   if(present(face).and. present(faceoffset)) then
        error = self%write_dataarray(data_name='faces', x=face)
        error = self%write_dataarray(data_name='faceoffsets', x=faceoffset)
   endif
   call self%write_end_tag(name='Cells')
   endfunction write_connectivity

   ! write_dataarray_unsigned methods
   !
   ! Fortran has no unsigned integers: the bits of a signed integer are written as the unsigned type of the same width
   ! (I1P as UInt8, I2P as UInt16, I4P as UInt32, I8P as UInt64), e.g. 200 stored as -56_I1P is written (and read) as 200.
   ! The binary formats write the same bytes; the ASCII format prints the unsigned values, widened to the next integer kind
   ! (copied into a local buffer: an expression passed as actual argument can be a stack temporary, see issue #70).
   function write_dataarray_unsigned_I1P(self, data_name, x) result(error)
   !< Write `<DataArray type="UInt8" ...>`: the bits of `x` as unsigned integers of the same width.
   !<
   !<### Example of usage
   !<
   !<```fortran
   !< error = vtk%xml_writer%write_dataarray(location='cell', action='open')
   !< error = vtk%xml_writer%write_dataarray_unsigned(data_name='vtkGhostType', x=ghost) ! ghost: integer(I1P), 0 or 1
   !< error = vtk%xml_writer%write_dataarray(location='cell', action='close')
   !<```
   class(xml_writer_abstract), intent(inout) :: self      !< Writer.
   character(*),               intent(in)    :: data_name !< Data name.
   integer(I1P),               intent(in)    :: x(1:)     !< Data variable (bits of unsigned integers).
   integer(I4P)                              :: error     !< Error status.
   integer(I2P), allocatable                 :: xa(:)     !< Unsigned values, for the ASCII format.

   self%data_type_override = 'UInt8'
   if (self%format_ch == 'ascii') then
      allocate(xa(1:size(x, kind=I8P)))
      xa = iand(int(x, I2P), 255_I2P)
      error = self%write_dataarray(data_name=data_name, x=xa)
   else
      error = self%write_dataarray(data_name=data_name, x=x)
   endif
   endfunction write_dataarray_unsigned_I1P

   function write_dataarray_unsigned_I2P(self, data_name, x) result(error)
   !< Write `<DataArray type="UInt16" ...>`: the bits of `x` as unsigned integers of the same width.
   class(xml_writer_abstract), intent(inout) :: self      !< Writer.
   character(*),               intent(in)    :: data_name !< Data name.
   integer(I2P),               intent(in)    :: x(1:)     !< Data variable (bits of unsigned integers).
   integer(I4P)                              :: error     !< Error status.
   integer(I4P), allocatable                 :: xa(:)     !< Unsigned values, for the ASCII format.

   self%data_type_override = 'UInt16'
   if (self%format_ch == 'ascii') then
      allocate(xa(1:size(x, kind=I8P)))
      xa = iand(int(x, I4P), 65535_I4P)
      error = self%write_dataarray(data_name=data_name, x=xa)
   else
      error = self%write_dataarray(data_name=data_name, x=x)
   endif
   endfunction write_dataarray_unsigned_I2P

   function write_dataarray_unsigned_I4P(self, data_name, x) result(error)
   !< Write `<DataArray type="UInt32" ...>`: the bits of `x` as unsigned integers of the same width.
   class(xml_writer_abstract), intent(inout) :: self      !< Writer.
   character(*),               intent(in)    :: data_name !< Data name.
   integer(I4P),               intent(in)    :: x(1:)     !< Data variable (bits of unsigned integers).
   integer(I4P)                              :: error     !< Error status.
   integer(I8P), allocatable                 :: xa(:)     !< Unsigned values, for the ASCII format.

   self%data_type_override = 'UInt32'
   if (self%format_ch == 'ascii') then
      allocate(xa(1:size(x, kind=I8P)))
      xa = iand(int(x, I8P), 4294967295_I8P)
      error = self%write_dataarray(data_name=data_name, x=xa)
   else
      error = self%write_dataarray(data_name=data_name, x=x)
   endif
   endfunction write_dataarray_unsigned_I4P

   function write_dataarray_unsigned_I8P(self, data_name, x) result(error)
   !< Write `<DataArray type="UInt64" ...>`: the bits of `x` as unsigned integers of the same width.
   !<
   !< @note With the ASCII format, values of 2^63 or more (negative `x`) cannot be printed (there is no wider integer kind):
   !< nothing is written and the error status is 1.
   class(xml_writer_abstract), intent(inout) :: self      !< Writer.
   character(*),               intent(in)    :: data_name !< Data name.
   integer(I8P),               intent(in)    :: x(1:)     !< Data variable (bits of unsigned integers).
   integer(I4P)                              :: error     !< Error status.

   if (self%format_ch == 'ascii' .and. any(x < 0_I8P)) then
      error = 1 ; self%error = error
      return
   endif
   self%data_type_override = 'UInt64'
   error = self%write_dataarray(data_name=data_name, x=x)
   endfunction write_dataarray_unsigned_I8P

   function write_polydata_cells(self, verts_connectivity, verts_offset, lines_connectivity, lines_offset, &
                                 strips_connectivity, strips_offset, polys_connectivity, polys_offset) result(error)
   !< Write the cell blocks of polydata topology: `<Verts>`, `<Lines>`, `<Strips>` and `<Polys>`.
   !<
   !< Each block is given by a pair of arrays, as in `write_connectivity`: the point ids (0-based) of its cells, one cell after
   !< the other, and the cumulative offset of the end of each cell. Only the blocks passed are written (VTK readers treat an
   !< absent block as empty); a block with only one of its two arrays is an error. The number of cells of each block must
   !< match the counts passed to `write_piece(np, nverts, nlines, nstrips, npolys)`.
   !<
   !< @note Cell data of polydata are ordered by block: verts, lines, strips, polys.
   !<
   !<### Example of usage
   !<
   !<```fortran
   !< ! a square made of two triangles and a polyline of 3 points
   !< error = vtk%xml_writer%write_polydata_cells(lines_connectivity=[4,5,6], lines_offset=[3], &
   !<                                            polys_connectivity=[0,1,2, 0,2,3], polys_offset=[3,6])
   !<```
   class(xml_writer_abstract), intent(inout)        :: self                    !< Writer.
   integer(I4P),               intent(in), optional :: verts_connectivity(1:)  !< Vertices connectivity.
   integer(I4P),               intent(in), optional :: verts_offset(1:)        !< Vertices offsets.
   integer(I4P),               intent(in), optional :: lines_connectivity(1:)  !< Lines connectivity.
   integer(I4P),               intent(in), optional :: lines_offset(1:)        !< Lines offsets.
   integer(I4P),               intent(in), optional :: strips_connectivity(1:) !< Triangle strips connectivity.
   integer(I4P),               intent(in), optional :: strips_offset(1:)       !< Triangle strips offsets.
   integer(I4P),               intent(in), optional :: polys_connectivity(1:)  !< Polygons connectivity.
   integer(I4P),               intent(in), optional :: polys_offset(1:)        !< Polygons offsets.
   integer(I4P)                                     :: error                   !< Error status.

   if ((present(verts_connectivity) .neqv. present(verts_offset)) .or. &
       (present(lines_connectivity) .neqv. present(lines_offset)) .or. &
       (present(strips_connectivity) .neqv. present(strips_offset)) .or. &
       (present(polys_connectivity) .neqv. present(polys_offset))) then
      self%error = 1
      error = self%error
      return
   endif
   if (present(verts_connectivity)) call write_block(name='Verts', connectivity=verts_connectivity, offset=verts_offset)
   if (present(lines_connectivity)) call write_block(name='Lines', connectivity=lines_connectivity, offset=lines_offset)
   if (present(strips_connectivity)) call write_block(name='Strips', connectivity=strips_connectivity, offset=strips_offset)
   if (present(polys_connectivity)) call write_block(name='Polys', connectivity=polys_connectivity, offset=polys_offset)
   error = self%error
   contains
      subroutine write_block(name, connectivity, offset)
      !< Write one cell block.
      character(*), intent(in) :: name             !< Block name.
      integer(I4P), intent(in) :: connectivity(1:) !< Block connectivity.
      integer(I4P), intent(in) :: offset(1:)       !< Block offsets.

      call self%write_start_tag(name=name)
      error = self%write_dataarray(data_name='connectivity', x=connectivity)
      error = self%write_dataarray(data_name='offsets', x=offset)
      call self%write_end_tag(name=name)
      endsubroutine write_block
   endfunction write_polydata_cells

   ! write_parallel methods
   function write_parallel_open_block(self, name) result(error)
   !< Write a block (open) container.
   class(xml_writer_abstract), intent(inout)        :: self   !< Writer.
   character(*),               intent(in), optional :: name   !< Block name.
   integer(I4P)                                     :: error  !< Error status.
   type(string)                                     :: buffer !< Buffer string.

   self%vtm_block = self%vtm_block + 1
   if (present(name)) then
      buffer = 'index="'//trim(str((self%vtm_block(1) + self%vtm_block(2)),.true.))//'" name="'//trim(adjustl(name))//'"'
   else
      buffer = 'index="'//trim(str((self%vtm_block(1) + self%vtm_block(2)),.true.))//'"'
   endif
   call self%write_start_tag(name='Block', attributes=buffer%chars())
   error = self%error
   endfunction write_parallel_open_block

   function write_parallel_close_block(self) result(error)
   !< Close a block container.
   class(xml_writer_abstract), intent(inout) :: self  !< Writer.
   integer(I4P)                              :: error !< Error status.

   self%vtm_block(2) = -1
   call self%write_end_tag(name='Block')
   error = self%error
   endfunction write_parallel_close_block

   function write_parallel_dataarray(self, data_name, data_type, number_of_components) result(error)
   !< Write parallel (partitioned) VTK-XML dataarray info.
   class(xml_writer_abstract), intent(inout)        :: self                 !< Writer.
   character(*),               intent(in)           :: data_name            !< Data name.
   character(*),               intent(in)           :: data_type            !< Type of dataarray.
   integer(I4P),               intent(in), optional :: number_of_components !< Number of dataarray components.
   integer(I4P)                                     :: error                !< Error status.
   type(string)                                     :: buffer               !< Buffer string.

   if (present(number_of_components)) then
      buffer = 'type="'//trim(adjustl(data_type))//'" Name="'//trim(adjustl(data_name))//&
               '" NumberOfComponents="'//trim(str(number_of_components, .true.))//'"'
   else
      buffer = 'type="'//trim(adjustl(data_type))//'" Name="'//trim(adjustl(data_name))//'"'
   endif
   call self%write_self_closing_tag(name='PDataArray', attributes=buffer%chars())
   error = self%error
   endfunction write_parallel_dataarray

   function write_parallel_geo(self, source, nx1, nx2, ny1, ny2, nz1, nz2) result(error)
   !< Write parallel (partitioned) VTK-XML geo source file.
   class(xml_writer_abstract), intent(inout)        :: self   !< Writer.
   character(*),               intent(in)           :: source !< Source file name containing the piece data.
   integer(I4P),               intent(in), optional :: nx1    !< Initial node of x axis.
   integer(I4P),               intent(in), optional :: nx2    !< Final node of x axis.
   integer(I4P),               intent(in), optional :: ny1    !< Initial node of y axis.
   integer(I4P),               intent(in), optional :: ny2    !< Final node of y axis.
   integer(I4P),               intent(in), optional :: nz1    !< Initial node of z axis.
   integer(I4P),               intent(in), optional :: nz2    !< Final node of z axis.
   integer(I4P)                                     :: error  !< Error status.
   type(string)                                     :: buffer !< Buffer string.

   select case (self%topology%chars())
   case('PRectilinearGrid', 'PStructuredGrid', 'PImageData')
      buffer = 'Extent="'// &
               trim(str(n=nx1))//' '//trim(str(n=nx2))//' '// &
               trim(str(n=ny1))//' '//trim(str(n=ny2))//' '// &
               trim(str(n=nz1))//' '//trim(str(n=nz2))//'" Source="'//trim(adjustl(source))//'"'
   case('PUnstructuredGrid', 'PPolyData')
      buffer = 'Source="'//trim(adjustl(source))//'"'
   endselect
   call self%write_self_closing_tag(name='Piece', attributes=buffer%chars())
   error = self%error
   endfunction write_parallel_geo

   function write_parallel_block_file(self, file_index, filename, name) result(error)
   !< Write single file that belong to the current block.
   class(xml_writer_abstract), intent(inout)        :: self       !< Writer.
   integer(I4P),               intent(in)           :: file_index !< Index of file in the list.
   character(*),               intent(in)           :: filename   !< Wrapped file names.
   character(*),               intent(in), optional :: name       !< Names attributed to wrapped file.
   integer(I4P)                                     :: error      !< Error status.

   if (present(name)) then
      call self%write_self_closing_tag(name='DataSet',                                      &
                                       attributes='index="'//trim(str(file_index, .true.))//&
                                                 '" file="'//trim(adjustl(filename))//      &
                                                 '" name="'//trim(adjustl(name))//'"')
   else
      call self%write_self_closing_tag(name='DataSet',                                       &
                                       attributes='index="'//trim(str(file_index, .true.))// &
                                                 '" file="'//trim(adjustl(filename))//'"')
   endif
   error = self%error
   endfunction write_parallel_block_file

   function write_parallel_block_files_array(self, filenames, names) result(error)
   !< Write list of files that belong to the current block (list passed as rank 1 array).
   !<
   !<#### Example of usage: 3 files blocks
   !<```fortran
   !< error = vtm%write_files_list_of_block(filenames=['file_1.vts','file_2.vts','file_3.vtu'])
   !<```
   !<
   !<#### Example of usage: 3 files blocks with custom name
   !<```fortran
   !< error = vtm%write_files_list_of_block(filenames=['file_1.vts','file_2.vts','file_3.vtu'],&
   !<                                       names=['block-bar','block-foo','block-baz'])
   !<```
   class(xml_writer_abstract), intent(inout)        :: self         !< Writer.
   character(*),               intent(in)           :: filenames(:) !< List of VTK-XML wrapped file names.
   character(*),               intent(in), optional :: names(:)     !< List names attributed to wrapped files.
   integer(I4P)                                     :: error        !< Error status.
   integer(I4P)                                     :: f            !< File counter.

   if (present(names)) then
      if (size(names, dim=1)==size(filenames, dim=1)) then
         do f=1, size(filenames, dim=1)
            call self%write_self_closing_tag(name='DataSet',                                     &
                                             attributes='index="'//trim(str(f-1, .true.))//      &
                                                       '" file="'//trim(adjustl(filenames(f)))// &
                                                       '" name="'//trim(adjustl(names(f)))//'"')
         enddo
      endif
   else
      do f=1,size(filenames, dim=1)
         call self%write_self_closing_tag(name='DataSet',                                &
                                          attributes='index="'//trim(str(f-1, .true.))// &
                                                    '" file="'//trim(adjustl(filenames(f)))//'"')
      enddo
   endif
   error = self%error
   endfunction write_parallel_block_files_array

   function write_parallel_block_files_string(self, filenames, names, delimiter) result(error)
   !< Write list of files that belong to the current block (list passed as single string).
   !<
   !<#### Example of usage: 3 files blocks
   !<```fortran
   !< error = vtm%write_files_list_of_block(filenames='file_1.vts file_2.vts file_3.vtu')
   !<```
   !<
   !<#### Example of usage: 3 files blocks with custom name
   !<```fortran
   !< error = vtm%write_files_list_of_block(filenames='file_1.vts file_2.vts file_3.vtu',&
   !<                                       names='block-bar block-foo block-baz')
   !<```
   class(xml_writer_abstract), intent(inout)        :: self          !< Writer.
   character(*),               intent(in)           :: filenames     !< List of VTK-XML wrapped file names.
   character(*),               intent(in), optional :: names         !< List names attributed to wrapped files.
   character(*),               intent(in), optional :: delimiter     !< Delimiter character.
   integer(I4P)                                     :: error         !< Error status.
   type(string), allocatable                        :: filenames_(:) !< List of VTK-XML wrapped file names.
   type(string), allocatable                        :: names_(:)     !< List names attributed to wrapped files.
   type(string)                                     :: delimiter_    !< Delimiter character.
   type(string)                                     :: buffer        !< A string buffer.
   integer(I4P)                                     :: f             !< File counter.

   delimiter_ = ' ' ; if (present(delimiter)) delimiter_ = delimiter
   buffer = filenames
   call buffer%split(tokens=filenames_, sep=delimiter_%chars())
   if (present(names)) then
      buffer = names
      call buffer%split(tokens=names_, sep=delimiter_%chars())
      if (size(names_, dim=1)==size(filenames_, dim=1)) then
         do f=1, size(filenames_, dim=1)
            call self%write_self_closing_tag(name='DataSet',                                      &
                                             attributes='index="'//trim(str(f-1, .true.))//       &
                                                       '" file="'//trim(adjustl(filenames_(f)))// &
                                                       '" name="'//trim(adjustl(names_(f)))//'"')
         enddo
      endif
   else
      do f=1,size(filenames_, dim=1)
         call self%write_self_closing_tag(name='DataSet',                               &
                                          attributes='index="'//trim(str(f-1,.true.))// &
                                                    '" file="'//trim(adjustl(filenames_(f)))//'"')
      enddo
   endif
   error = self%error
   endfunction write_parallel_block_files_string
endmodule vtk_fortran_vtk_file_xml_writer_abstract
