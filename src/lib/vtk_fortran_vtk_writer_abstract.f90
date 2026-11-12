!< VTK file abstract writer: the procedures of a dataset writer, whatever the format.
module vtk_fortran_vtk_writer_abstract
!< VTK file abstract writer: the procedures of a dataset writer, whatever the format.
!<
!< `vtk_file%xml_writer` is of this type: the XML writers (`xml_writer_abstract` and its concrete types) extend it, and
!< so will the VTKHDF writer. Every specific procedure is deferred and PUBLIC: an extension defined in another module
!< overrides a private binding with gfortran and nvfortran, but not with ifx (measured with 2025.3), so a private one
!< would silently keep the parent's procedure under ifx.
use penf

implicit none
private
public :: vtk_writer_abstract

type, abstract :: vtk_writer_abstract
  !< Writer of a dataset file, any format: the state set by `vtk_file` and the procedures it exposes.
  real(R8P)     :: origin(3)=0._R8P                !< Origin of ImageData topologies.
  real(R8P)     :: spacing(3)=1._R8P               !< Spacing of ImageData topologies.
  real(R8P)     :: direction(9)=[1._R8P, 0._R8P, 0._R8P, &
                                 0._R8P, 1._R8P, 0._R8P, &
                                 0._R8P, 0._R8P, 1._R8P] !< Axes directions of ImageData topologies (row-major 3x3).
  logical       :: is_direction_set=.false.        !< Write the Direction of ImageData topologies.
  logical       :: is_uint64=.false.               !< Use UInt64 (instead of UInt32) bytes count headers.
  logical       :: is_compressed=.false.           !< Compress (zlib) the binary data.
  contains
    procedure(initialize_interface), deferred, pass(self) :: initialize !< Initialize the writer.
    procedure(finalize_interface),   deferred, pass(self) :: finalize   !< Finalize the writer.
    procedure(write_connectivity_I4P_interface), deferred, pass(self) :: write_connectivity_I4P
    procedure(write_connectivity_I8P_interface), deferred, pass(self) :: write_connectivity_I8P
    procedure(write_dataarray_location_tag_interface), deferred, pass(self) :: write_dataarray_location_tag
    procedure(write_fielddata1_rank0_interface), deferred, pass(self) :: write_fielddata1_rank0
    procedure(write_fielddata1_rank1_interface), deferred, pass(self) :: write_fielddata1_rank1
    procedure(write_fielddata_tag_interface), deferred, pass(self) :: write_fielddata_tag
    procedure(write_dataarray_unsigned_I1P_interface), deferred, pass(self) :: write_dataarray_unsigned_I1P
    procedure(write_dataarray_unsigned_I2P_interface), deferred, pass(self) :: write_dataarray_unsigned_I2P
    procedure(write_dataarray_unsigned_I4P_interface), deferred, pass(self) :: write_dataarray_unsigned_I4P
    procedure(write_dataarray_unsigned_I8P_interface), deferred, pass(self) :: write_dataarray_unsigned_I8P
    procedure(write_geo_strg_data1_rank2_R8P_interface), deferred, pass(self) :: write_geo_strg_data1_rank2_R8P
    procedure(write_geo_strg_data1_rank2_R4P_interface), deferred, pass(self) :: write_geo_strg_data1_rank2_R4P
    procedure(write_geo_strg_data1_rank4_R8P_interface), deferred, pass(self) :: write_geo_strg_data1_rank4_R8P
    procedure(write_geo_strg_data1_rank4_R4P_interface), deferred, pass(self) :: write_geo_strg_data1_rank4_R4P
    procedure(write_geo_strg_data3_rank1_R8P_interface), deferred, pass(self) :: write_geo_strg_data3_rank1_R8P
    procedure(write_geo_strg_data3_rank1_R4P_interface), deferred, pass(self) :: write_geo_strg_data3_rank1_R4P
    procedure(write_geo_strg_data3_rank3_R8P_interface), deferred, pass(self) :: write_geo_strg_data3_rank3_R8P
    procedure(write_geo_strg_data3_rank3_R4P_interface), deferred, pass(self) :: write_geo_strg_data3_rank3_R4P
    procedure(write_geo_rect_data3_rank1_R8P_interface), deferred, pass(self) :: write_geo_rect_data3_rank1_R8P
    procedure(write_geo_rect_data3_rank1_R4P_interface), deferred, pass(self) :: write_geo_rect_data3_rank1_R4P
    procedure(write_geo_unst_data1_rank2_R8P_interface), deferred, pass(self) :: write_geo_unst_data1_rank2_R8P
    procedure(write_geo_unst_data1_rank2_R4P_interface), deferred, pass(self) :: write_geo_unst_data1_rank2_R4P
    procedure(write_geo_unst_data3_rank1_R8P_interface), deferred, pass(self) :: write_geo_unst_data3_rank1_R8P
    procedure(write_geo_unst_data3_rank1_R4P_interface), deferred, pass(self) :: write_geo_unst_data3_rank1_R4P
    procedure(write_geo_unst_data1_rank2_R8P_I8P_interface), deferred, pass(self) :: write_geo_unst_data1_rank2_R8P_I8P
    procedure(write_geo_unst_data1_rank2_R4P_I8P_interface), deferred, pass(self) :: write_geo_unst_data1_rank2_R4P_I8P
    procedure(write_geo_unst_data3_rank1_R8P_I8P_interface), deferred, pass(self) :: write_geo_unst_data3_rank1_R8P_I8P
    procedure(write_geo_unst_data3_rank1_R4P_I8P_interface), deferred, pass(self) :: write_geo_unst_data3_rank1_R4P_I8P
    procedure(write_piece_start_tag_interface), deferred, pass(self) :: write_piece_start_tag
    procedure(write_piece_start_tag_unst_interface), deferred, pass(self) :: write_piece_start_tag_unst
    procedure(write_piece_start_tag_poly_interface), deferred, pass(self) :: write_piece_start_tag_poly
    procedure(write_piece_start_tag_unst_I8P_interface), deferred, pass(self) :: write_piece_start_tag_unst_I8P
    procedure(write_piece_start_tag_poly_I8P_interface), deferred, pass(self) :: write_piece_start_tag_poly_I8P
    procedure(write_piece_end_tag_interface), deferred, pass(self) :: write_piece_end_tag
    procedure(write_polydata_cells_interface), deferred, pass(self) :: write_polydata_cells
    procedure(write_step_interface), deferred, pass(self) :: write_step
    procedure(free_interface), deferred, pass(self) :: free
    procedure(get_xml_volatile_interface), deferred, pass(self) :: get_xml_volatile
    procedure(write_dataarray1_rank1_R8P_interface), deferred, pass(self) :: write_dataarray1_rank1_R8P
    procedure(write_dataarray1_rank1_R4P_interface), deferred, pass(self) :: write_dataarray1_rank1_R4P
    procedure(write_dataarray1_rank1_I8P_interface), deferred, pass(self) :: write_dataarray1_rank1_I8P
    procedure(write_dataarray1_rank1_I4P_interface), deferred, pass(self) :: write_dataarray1_rank1_I4P
    procedure(write_dataarray1_rank1_I2P_interface), deferred, pass(self) :: write_dataarray1_rank1_I2P
    procedure(write_dataarray1_rank1_I1P_interface), deferred, pass(self) :: write_dataarray1_rank1_I1P
    procedure(write_dataarray1_rank2_R8P_interface), deferred, pass(self) :: write_dataarray1_rank2_R8P
    procedure(write_dataarray1_rank2_R4P_interface), deferred, pass(self) :: write_dataarray1_rank2_R4P
    procedure(write_dataarray1_rank2_I8P_interface), deferred, pass(self) :: write_dataarray1_rank2_I8P
    procedure(write_dataarray1_rank2_I4P_interface), deferred, pass(self) :: write_dataarray1_rank2_I4P
    procedure(write_dataarray1_rank2_I2P_interface), deferred, pass(self) :: write_dataarray1_rank2_I2P
    procedure(write_dataarray1_rank2_I1P_interface), deferred, pass(self) :: write_dataarray1_rank2_I1P
    procedure(write_dataarray1_rank3_R8P_interface), deferred, pass(self) :: write_dataarray1_rank3_R8P
    procedure(write_dataarray1_rank3_R4P_interface), deferred, pass(self) :: write_dataarray1_rank3_R4P
    procedure(write_dataarray1_rank3_I8P_interface), deferred, pass(self) :: write_dataarray1_rank3_I8P
    procedure(write_dataarray1_rank3_I4P_interface), deferred, pass(self) :: write_dataarray1_rank3_I4P
    procedure(write_dataarray1_rank3_I2P_interface), deferred, pass(self) :: write_dataarray1_rank3_I2P
    procedure(write_dataarray1_rank3_I1P_interface), deferred, pass(self) :: write_dataarray1_rank3_I1P
    procedure(write_dataarray1_rank4_R8P_interface), deferred, pass(self) :: write_dataarray1_rank4_R8P
    procedure(write_dataarray1_rank4_R4P_interface), deferred, pass(self) :: write_dataarray1_rank4_R4P
    procedure(write_dataarray1_rank4_I8P_interface), deferred, pass(self) :: write_dataarray1_rank4_I8P
    procedure(write_dataarray1_rank4_I4P_interface), deferred, pass(self) :: write_dataarray1_rank4_I4P
    procedure(write_dataarray1_rank4_I2P_interface), deferred, pass(self) :: write_dataarray1_rank4_I2P
    procedure(write_dataarray1_rank4_I1P_interface), deferred, pass(self) :: write_dataarray1_rank4_I1P
    procedure(write_dataarray3_rank1_R8P_interface), deferred, pass(self) :: write_dataarray3_rank1_R8P
    procedure(write_dataarray3_rank1_R4P_interface), deferred, pass(self) :: write_dataarray3_rank1_R4P
    procedure(write_dataarray3_rank1_I8P_interface), deferred, pass(self) :: write_dataarray3_rank1_I8P
    procedure(write_dataarray3_rank1_I4P_interface), deferred, pass(self) :: write_dataarray3_rank1_I4P
    procedure(write_dataarray3_rank1_I2P_interface), deferred, pass(self) :: write_dataarray3_rank1_I2P
    procedure(write_dataarray3_rank1_I1P_interface), deferred, pass(self) :: write_dataarray3_rank1_I1P
    procedure(write_dataarray3_rank3_R8P_interface), deferred, pass(self) :: write_dataarray3_rank3_R8P
    procedure(write_dataarray3_rank3_R4P_interface), deferred, pass(self) :: write_dataarray3_rank3_R4P
    procedure(write_dataarray3_rank3_I8P_interface), deferred, pass(self) :: write_dataarray3_rank3_I8P
    procedure(write_dataarray3_rank3_I4P_interface), deferred, pass(self) :: write_dataarray3_rank3_I4P
    procedure(write_dataarray3_rank3_I2P_interface), deferred, pass(self) :: write_dataarray3_rank3_I2P
    procedure(write_dataarray3_rank3_I1P_interface), deferred, pass(self) :: write_dataarray3_rank3_I1P
    procedure(write_dataarray6_rank1_R8P_interface), deferred, pass(self) :: write_dataarray6_rank1_R8P
    procedure(write_dataarray6_rank1_R4P_interface), deferred, pass(self) :: write_dataarray6_rank1_R4P
    procedure(write_dataarray6_rank1_I8P_interface), deferred, pass(self) :: write_dataarray6_rank1_I8P
    procedure(write_dataarray6_rank1_I4P_interface), deferred, pass(self) :: write_dataarray6_rank1_I4P
    procedure(write_dataarray6_rank1_I2P_interface), deferred, pass(self) :: write_dataarray6_rank1_I2P
    procedure(write_dataarray6_rank1_I1P_interface), deferred, pass(self) :: write_dataarray6_rank1_I1P
    procedure(write_dataarray6_rank3_R8P_interface), deferred, pass(self) :: write_dataarray6_rank3_R8P
    procedure(write_dataarray6_rank3_R4P_interface), deferred, pass(self) :: write_dataarray6_rank3_R4P
    procedure(write_dataarray6_rank3_I8P_interface), deferred, pass(self) :: write_dataarray6_rank3_I8P
    procedure(write_dataarray6_rank3_I4P_interface), deferred, pass(self) :: write_dataarray6_rank3_I4P
    procedure(write_dataarray6_rank3_I2P_interface), deferred, pass(self) :: write_dataarray6_rank3_I2P
    procedure(write_dataarray6_rank3_I1P_interface), deferred, pass(self) :: write_dataarray6_rank3_I1P
    generic :: write_connectivity => &
               write_connectivity_I4P, &
               write_connectivity_I8P
    generic :: write_dataarray => &
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
               write_dataarray_location_tag
    generic :: write_fielddata => &
               write_fielddata1_rank0, &
               write_fielddata1_rank1, &
               write_fielddata_tag
    generic :: write_dataarray_unsigned => &
               write_dataarray_unsigned_I1P, &
               write_dataarray_unsigned_I2P, &
               write_dataarray_unsigned_I4P, &
               write_dataarray_unsigned_I8P
    generic :: write_geo => &
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
               write_geo_unst_data3_rank1_R4P, &
               write_geo_unst_data1_rank2_R8P_I8P, &
               write_geo_unst_data1_rank2_R4P_I8P, &
               write_geo_unst_data3_rank1_R8P_I8P, &
               write_geo_unst_data3_rank1_R4P_I8P
    generic :: write_piece => &
               write_piece_start_tag, &
               write_piece_start_tag_unst, &
               write_piece_start_tag_poly, &
               write_piece_start_tag_unst_I8P, &
               write_piece_start_tag_poly_I8P, &
               write_piece_end_tag
endtype vtk_writer_abstract

abstract interface

  function initialize_interface(self, format, filename, mesh_topology, nx1, nx2, ny1, ny2, nz1, nz2, &
                                is_volatile, mesh_kind) result(error)
  !< Initialize writer.
  import :: vtk_writer_abstract, I4P
  class(vtk_writer_abstract), intent(inout)        :: self          !< Writer.
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
   import :: vtk_writer_abstract, I4P
   class(vtk_writer_abstract), intent(inout) :: self  !< Writer.
   integer(I4P)                              :: error !< Error status.
   endfunction finalize_interface

  function write_dataarray1_rank1_R8P_interface(self, data_name, x, is_tuples) result(error)
  !< Write `<DataArray... NumberOfComponents="1"...>...</DataArray>` tag (R8P).
  import :: vtk_writer_abstract, I4P, R8P
  class(vtk_writer_abstract), intent(inout)        :: self         !< Writer.
  character(*),               intent(in)           :: data_name    !< Data name.
  real(R8P),                  intent(in)           :: x(1:)        !< Data variable.
  logical,                    intent(in), optional :: is_tuples    !< Use "NumberOfTuples" instead of "NumberOfComponents".
  integer(I4P)                                     :: error        !< Error status.
  endfunction write_dataarray1_rank1_R8P_interface

  function write_dataarray1_rank1_R4P_interface(self, data_name, x, is_tuples) result(error)
  !< Write `<DataArray... NumberOfComponents="1"...>...</DataArray>` tag (R4P).
  import :: vtk_writer_abstract, I4P, R4P
  class(vtk_writer_abstract), intent(inout)        :: self         !< Writer.
  character(*),               intent(in)           :: data_name    !< Data name.
  real(R4P),                  intent(in)           :: x(1:)        !< Data variable.
  logical,                    intent(in), optional :: is_tuples    !< Use "NumberOfTuples" instead of "NumberOfComponents".
  integer(I4P)                                     :: error        !< Error status.
  endfunction write_dataarray1_rank1_R4P_interface

  function write_dataarray1_rank1_I8P_interface(self, data_name, x, is_tuples) result(error)
  !< Write `<DataArray... NumberOfComponents="1"...>...</DataArray>` tag (I8P).
  import :: vtk_writer_abstract, I4P, I8P
  class(vtk_writer_abstract), intent(inout)        :: self         !< Writer.
  character(*),               intent(in)           :: data_name    !< Data name.
  integer(I8P),               intent(in)           :: x(1:)        !< Data variable.
  logical,                    intent(in), optional :: is_tuples    !< Use "NumberOfTuples" instead of "NumberOfComponents".
  integer(I4P)                                     :: error        !< Error status.
  endfunction write_dataarray1_rank1_I8P_interface

  function write_dataarray1_rank1_I4P_interface(self, data_name, x, is_tuples) result(error)
  !< Write `<DataArray... NumberOfComponents="1"...>...</DataArray>` tag (I4P).
  import :: vtk_writer_abstract, I4P
  class(vtk_writer_abstract), intent(inout)        :: self         !< Writer.
  character(*),               intent(in)           :: data_name    !< Data name.
  integer(I4P),               intent(in)           :: x(1:)        !< Data variable.
  logical,                    intent(in), optional :: is_tuples    !< Use "NumberOfTuples" instead of "NumberOfComponents".
  integer(I4P)                                     :: error        !< Error status.
  endfunction write_dataarray1_rank1_I4P_interface

  function write_dataarray1_rank1_I2P_interface(self, data_name, x, is_tuples) result(error)
  !< Write `<DataArray... NumberOfComponents="1"...>...</DataArray>` tag (I2P).
  import :: vtk_writer_abstract, I2P, I4P
  class(vtk_writer_abstract), intent(inout)        :: self         !< Writer.
  character(*),               intent(in)           :: data_name    !< Data name.
  integer(I2P),               intent(in)           :: x(1:)        !< Data variable.
  logical,                    intent(in), optional :: is_tuples    !< Use "NumberOfTuples" instead of "NumberOfComponents".
  integer(I4P)                                     :: error        !< Error status.
  endfunction write_dataarray1_rank1_I2P_interface

  function write_dataarray1_rank1_I1P_interface(self, data_name, x, is_tuples) result(error)
  !< Write `<DataArray... NumberOfComponents="1"...>...</DataArray>` tag (I1P).
  import :: vtk_writer_abstract, I1P, I4P
  class(vtk_writer_abstract), intent(inout)        :: self         !< Writer.
  character(*),               intent(in)           :: data_name    !< Data name.
  integer(I1P),               intent(in)           :: x(1:)        !< Data variable.
  logical,                    intent(in), optional :: is_tuples    !< Use "NumberOfTuples" instead of "NumberOfComponents".
  integer(I4P)                                     :: error        !< Error status.
  endfunction write_dataarray1_rank1_I1P_interface

  function write_dataarray1_rank2_R8P_interface(self, data_name, x, one_component, is_tuples) result(error)
  !< Write `<DataArray... NumberOfComponents="n"...>...</DataArray>` tag (R8P).
  import :: vtk_writer_abstract, I4P, R8P
  class(vtk_writer_abstract), intent(inout)        :: self          !< Writer.
  character(*),               intent(in)           :: data_name     !< Data name.
  real(R8P),                  intent(in)           :: x(1:,1:)      !< Data variable.
  logical,                    intent(in), optional :: one_component !< Force one component.
  logical,                    intent(in), optional :: is_tuples     !< Use "NumberOfTuples" instead of "NumberOfComponents".
  integer(I4P)                                     :: error         !< Error status.
  endfunction write_dataarray1_rank2_R8P_interface

  function write_dataarray1_rank2_R4P_interface(self, data_name, x, one_component, is_tuples) result(error)
  !< Write `<DataArray... NumberOfComponents="n"...>...</DataArray>` tag (R4P).
  import :: vtk_writer_abstract, I4P, R4P
  class(vtk_writer_abstract), intent(inout)        :: self          !< Writer.
  character(*),               intent(in)           :: data_name     !< Data name.
  real(R4P),                  intent(in)           :: x(1:,1:)      !< Data variable.
  logical,                    intent(in), optional :: one_component !< Force one component.
  logical,                    intent(in), optional :: is_tuples     !< Use "NumberOfTuples" instead of "NumberOfComponents".
  integer(I4P)                                     :: error         !< Error status.
  endfunction write_dataarray1_rank2_R4P_interface

  function write_dataarray1_rank2_I8P_interface(self, data_name, x, one_component, is_tuples) result(error)
  !< Write `<DataArray... NumberOfComponents="n"...>...</DataArray>` tag (I8P).
  import :: vtk_writer_abstract, I4P, I8P
  class(vtk_writer_abstract), intent(inout)        :: self          !< Writer.
  character(*),               intent(in)           :: data_name     !< Data name.
  integer(I8P),               intent(in)           :: x(1:,1:)      !< Data variable.
  logical,                    intent(in), optional :: one_component !< Force one component.
  logical,                    intent(in), optional :: is_tuples     !< Use "NumberOfTuples" instead of "NumberOfComponents".
  integer(I4P)                                     :: error         !< Error status.
  endfunction write_dataarray1_rank2_I8P_interface

  function write_dataarray1_rank2_I4P_interface(self, data_name, x, one_component, is_tuples) result(error)
  !< Write `<DataArray... NumberOfComponents="n"...>...</DataArray>` tag (I4P).
  import :: vtk_writer_abstract, I4P
  class(vtk_writer_abstract), intent(inout)        :: self          !< Writer.
  character(*),               intent(in)           :: data_name     !< Data name.
  integer(I4P),               intent(in)           :: x(1:,1:)      !< Data variable.
  logical,                    intent(in), optional :: one_component !< Force one component.
  logical,                    intent(in), optional :: is_tuples     !< Use "NumberOfTuples" instead of "NumberOfComponents".
  integer(I4P)                                     :: error         !< Error status.
  endfunction write_dataarray1_rank2_I4P_interface

  function write_dataarray1_rank2_I2P_interface(self, data_name, x, one_component, is_tuples) result(error)
  !< Write `<DataArray... NumberOfComponents="n"...>...</DataArray>` tag (I2P).
  import :: vtk_writer_abstract, I2P, I4P
  class(vtk_writer_abstract), intent(inout)        :: self          !< Writer.
  character(*),               intent(in)           :: data_name     !< Data name.
  integer(I2P),               intent(in)           :: x(1:,1:)      !< Data variable.
  logical,                    intent(in), optional :: one_component !< Force one component.
  logical,                    intent(in), optional :: is_tuples     !< Use "NumberOfTuples" instead of "NumberOfComponents".
  integer(I4P)                                     :: error         !< Error status.
  endfunction write_dataarray1_rank2_I2P_interface

  function write_dataarray1_rank2_I1P_interface(self, data_name, x, one_component, is_tuples) result(error)
  !< Write `<DataArray... NumberOfComponents="n"...>...</DataArray>` tag (I1P).
  import :: vtk_writer_abstract, I1P, I4P
  class(vtk_writer_abstract), intent(inout)        :: self          !< Writer.
  character(*),               intent(in)           :: data_name     !< Data name.
  integer(I1P),               intent(in)           :: x(1:,1:)      !< Data variable.
  logical,                    intent(in), optional :: one_component !< Force one component.
  logical,                    intent(in), optional :: is_tuples     !< Use "NumberOfTuples" instead of "NumberOfComponents".
  integer(I4P)                                     :: error         !< Error status.
  endfunction write_dataarray1_rank2_I1P_interface

  function write_dataarray1_rank3_R8P_interface(self, data_name, x, one_component, is_tuples) result(error)
  !< Write `<DataArray... NumberOfComponents="n"...>...</DataArray>` tag (R8P).
  import :: vtk_writer_abstract, I4P, R8P
  class(vtk_writer_abstract), intent(inout)        :: self          !< Writer.
  character(*),               intent(in)           :: data_name     !< Data name.
  real(R8P),                  intent(in)           :: x(1:,1:,1:)   !< Data variable.
  logical,                    intent(in), optional :: one_component !< Force one component.
  logical,                    intent(in), optional :: is_tuples     !< Use "NumberOfTuples" instead of "NumberOfComponents".
  integer(I4P)                                     :: error         !< Error status.
  endfunction write_dataarray1_rank3_R8P_interface

  function write_dataarray1_rank3_R4P_interface(self, data_name, x, one_component, is_tuples) result(error)
  !< Write `<DataArray... NumberOfComponents="n"...>...</DataArray>` tag (R4P).
  import :: vtk_writer_abstract, I4P, R4P
  class(vtk_writer_abstract), intent(inout)        :: self          !< Writer.
  character(*),               intent(in)           :: data_name     !< Data name.
  real(R4P),                  intent(in)           :: x(1:,1:,1:)   !< Data variable.
  logical,                    intent(in), optional :: one_component !< Force one component.
  logical,                    intent(in), optional :: is_tuples     !< Use "NumberOfTuples" instead of "NumberOfComponents".
  integer(I4P)                                     :: error         !< Error status.
  endfunction write_dataarray1_rank3_R4P_interface

  function write_dataarray1_rank3_I8P_interface(self, data_name, x, one_component, is_tuples) result(error)
  !< Write `<DataArray... NumberOfComponents="n"...>...</DataArray>` tag (I8P).
  import :: vtk_writer_abstract, I4P, I8P
  class(vtk_writer_abstract), intent(inout)        :: self          !< Writer.
  character(*),               intent(in)           :: data_name     !< Data name.
  integer(I8P),               intent(in)           :: x(1:,1:,1:)   !< Data variable.
  logical,                    intent(in), optional :: one_component !< Force one component.
  logical,                    intent(in), optional :: is_tuples     !< Use "NumberOfTuples" instead of "NumberOfComponents".
  integer(I4P)                                     :: error         !< Error status.
  endfunction write_dataarray1_rank3_I8P_interface

  function write_dataarray1_rank3_I4P_interface(self, data_name, x, one_component, is_tuples) result(error)
  !< Write `<DataArray... NumberOfComponents="n"...>...</DataArray>` tag (I4P).
  import :: vtk_writer_abstract, I4P
  class(vtk_writer_abstract), intent(inout)        :: self          !< Writer.
  character(*),               intent(in)           :: data_name     !< Data name.
  integer(I4P),               intent(in)           :: x(1:,1:,1:)   !< Data variable.
  logical,                    intent(in), optional :: one_component !< Force one component.
  logical,                    intent(in), optional :: is_tuples     !< Use "NumberOfTuples" instead of "NumberOfComponents".
  integer(I4P)                                     :: error         !< Error status.
  endfunction write_dataarray1_rank3_I4P_interface

  function write_dataarray1_rank3_I2P_interface(self, data_name, x, one_component, is_tuples) result(error)
  !< Write `<DataArray... NumberOfComponents="n"...>...</DataArray>` tag (I2P).
  import :: vtk_writer_abstract, I2P, I4P
  class(vtk_writer_abstract), intent(inout)        :: self          !< Writer.
  character(*),               intent(in)           :: data_name     !< Data name.
  integer(I2P),               intent(in)           :: x(1:,1:,1:)   !< Data variable.
  logical,                    intent(in), optional :: one_component !< Force one component.
  logical,                    intent(in), optional :: is_tuples     !< Use "NumberOfTuples" instead of "NumberOfComponents".
  integer(I4P)                                     :: error         !< Error status.
  endfunction write_dataarray1_rank3_I2P_interface

  function write_dataarray1_rank3_I1P_interface(self, data_name, x, one_component, is_tuples) result(error)
  !< Write `<DataArray... NumberOfComponents="n"...>...</DataArray>` tag (I1P).
  import :: vtk_writer_abstract, I1P, I4P
  class(vtk_writer_abstract), intent(inout)        :: self          !< Writer.
  character(*),               intent(in)           :: data_name     !< Data name.
  integer(I1P),               intent(in)           :: x(1:,1:,1:)   !< Data variable.
  logical,                    intent(in), optional :: one_component !< Force one component.
  logical,                    intent(in), optional :: is_tuples     !< Use "NumberOfTuples" instead of "NumberOfComponents".
  integer(I4P)                                     :: error         !< Error status.
  endfunction write_dataarray1_rank3_I1P_interface

  function write_dataarray1_rank4_R8P_interface(self, data_name, x, one_component, is_tuples) result(error)
  !< Write `<DataArray... NumberOfComponents="n"...>...</DataArray>` tag (R8P).
  import :: vtk_writer_abstract, I4P, R8P
  class(vtk_writer_abstract), intent(inout)        :: self           !< Writer.
  character(*),               intent(in)           :: data_name      !< Data name.
  real(R8P),                  intent(in)           :: x(1:,1:,1:,1:) !< Data variable.
  logical,                    intent(in), optional :: one_component  !< Force one component.
  logical,                    intent(in), optional :: is_tuples      !< Use "NumberOfTuples" instead of "NumberOfComponents".
  integer(I4P)                                     :: error          !< Error status.
  endfunction write_dataarray1_rank4_R8P_interface

  function write_dataarray1_rank4_R4P_interface(self, data_name, x, one_component, is_tuples) result(error)
  !< Write `<DataArray... NumberOfComponents="n"...>...</DataArray>` tag (R4P).
  import :: vtk_writer_abstract, I4P, R4P
  class(vtk_writer_abstract), intent(inout)        :: self           !< Writer.
  character(*),               intent(in)           :: data_name      !< Data name.
  real(R4P),                  intent(in)           :: x(1:,1:,1:,1:) !< Data variable.
  logical,                    intent(in), optional :: one_component  !< Force one component.
  logical,                    intent(in), optional :: is_tuples      !< Use "NumberOfTuples" instead of "NumberOfComponents".
  integer(I4P)                                     :: error          !< Error status.
  endfunction write_dataarray1_rank4_R4P_interface

  function write_dataarray1_rank4_I8P_interface(self, data_name, x, one_component, is_tuples) result(error)
  !< Write `<DataArray... NumberOfComponents="n"...>...</DataArray>` tag (I8P).
  import :: vtk_writer_abstract, I4P, I8P
  class(vtk_writer_abstract), intent(inout)        :: self           !< Writer.
  character(*),               intent(in)           :: data_name      !< Data name.
  integer(I8P),               intent(in)           :: x(1:,1:,1:,1:) !< Data variable.
  logical,                    intent(in), optional :: one_component  !< Force one component.
  logical,                    intent(in), optional :: is_tuples      !< Use "NumberOfTuples" instead of "NumberOfComponents".
  integer(I4P)                                     :: error          !< Error status.
  endfunction write_dataarray1_rank4_I8P_interface

  function write_dataarray1_rank4_I4P_interface(self, data_name, x, one_component, is_tuples) result(error)
  !< Write `<DataArray... NumberOfComponents="n"...>...</DataArray>` tag (I4P).
  import :: vtk_writer_abstract, I4P
  class(vtk_writer_abstract), intent(inout)        :: self           !< Writer.
  character(*),               intent(in)           :: data_name      !< Data name.
  integer(I4P),               intent(in)           :: x(1:,1:,1:,1:) !< Data variable.
  logical,                    intent(in), optional :: one_component  !< Force one component.
  logical,                    intent(in), optional :: is_tuples      !< Use "NumberOfTuples" instead of "NumberOfComponents".
  integer(I4P)                                     :: error          !< Error status.
  endfunction write_dataarray1_rank4_I4P_interface

  function write_dataarray1_rank4_I2P_interface(self, data_name, x, one_component, is_tuples) result(error)
  !< Write `<DataArray... NumberOfComponents="n"...>...</DataArray>` tag (I2P).
  import :: vtk_writer_abstract, I2P, I4P
  class(vtk_writer_abstract), intent(inout)        :: self           !< Writer.
  character(*),               intent(in)           :: data_name      !< Data name.
  integer(I2P),               intent(in)           :: x(1:,1:,1:,1:) !< Data variable.
  logical,                    intent(in), optional :: one_component  !< Force one component.
  logical,                    intent(in), optional :: is_tuples      !< Use "NumberOfTuples" instead of "NumberOfComponents".
  integer(I4P)                                     :: error          !< Error status.
  endfunction write_dataarray1_rank4_I2P_interface

  function write_dataarray1_rank4_I1P_interface(self, data_name, x, one_component, is_tuples) result(error)
  !< Write `<DataArray... NumberOfComponents="n"...>...</DataArray>` tag (I1P).
  import :: vtk_writer_abstract, I1P, I4P
  class(vtk_writer_abstract), intent(inout)        :: self           !< Writer.
  character(*),               intent(in)           :: data_name      !< Data name.
  integer(I1P),               intent(in)           :: x(1:,1:,1:,1:) !< Data variable.
  logical,                    intent(in), optional :: one_component  !< Force one component.
  logical,                    intent(in), optional :: is_tuples      !< Use "NumberOfTuples" instead of "NumberOfComponents".
  integer(I4P)                                     :: error          !< Error status.
  endfunction write_dataarray1_rank4_I1P_interface

  function write_dataarray3_rank1_R8P_interface(self, data_name, x, y, z, is_tuples) result(error)
  !< Write `<DataArray... NumberOfComponents="3"...>...</DataArray>` tag (R8P).
  import :: vtk_writer_abstract, I4P, R8P
  class(vtk_writer_abstract), intent(inout)        :: self         !< Writer.
  character(*),               intent(in)           :: data_name    !< Data name.
  real(R8P),                  intent(in)           :: x(1:)        !< X component of data variable.
  real(R8P),                  intent(in)           :: y(1:)        !< Y component of data variable.
  real(R8P),                  intent(in)           :: z(1:)        !< Z component of data variable.
  logical,                    intent(in), optional :: is_tuples    !< Use "NumberOfTuples" instead of "NumberOfComponents".
  integer(I4P)                                     :: error        !< Error status.
  endfunction write_dataarray3_rank1_R8P_interface

  function write_dataarray3_rank1_R4P_interface(self, data_name, x, y, z, is_tuples) result(error)
  !< Write `<DataArray... NumberOfComponents="3"...>...</DataArray>` tag (R4P).
  import :: vtk_writer_abstract, I4P, R4P
  class(vtk_writer_abstract), intent(inout)        :: self         !< Writer.
  character(*),               intent(in)           :: data_name    !< Data name.
  real(R4P),                  intent(in)           :: x(1:)        !< X component of data variable.
  real(R4P),                  intent(in)           :: y(1:)        !< Y component of data variable.
  real(R4P),                  intent(in)           :: z(1:)        !< Z component of data variable.
  logical,                    intent(in), optional :: is_tuples    !< Use "NumberOfTuples" instead of "NumberOfComponents".
  integer(I4P)                                     :: error        !< Error status.
  endfunction write_dataarray3_rank1_R4P_interface

  function write_dataarray3_rank1_I8P_interface(self, data_name, x, y, z, is_tuples) result(error)
  !< Write `<DataArray... NumberOfComponents="3"...>...</DataArray>` tag (I8P).
  import :: vtk_writer_abstract, I4P, I8P
  class(vtk_writer_abstract), intent(inout)        :: self         !< Writer.
  character(*),               intent(in)           :: data_name    !< Data name.
  integer(I8P),               intent(in)           :: x(1:)        !< X component of data variable.
  integer(I8P),               intent(in)           :: y(1:)        !< Y component of data variable.
  integer(I8P),               intent(in)           :: z(1:)        !< Z component of data variable.
  logical,                    intent(in), optional :: is_tuples    !< Use "NumberOfTuples" instead of "NumberOfComponents".
  integer(I4P)                                     :: error        !< Error status.
  endfunction write_dataarray3_rank1_I8P_interface

  function write_dataarray3_rank1_I4P_interface(self, data_name, x, y, z, is_tuples) result(error)
  !< Write `<DataArray... NumberOfComponents="3"...>...</DataArray>` tag (I4P).
  import :: vtk_writer_abstract, I4P
  class(vtk_writer_abstract), intent(inout)        :: self         !< Writer.
  character(*),               intent(in)           :: data_name    !< Data name.
  integer(I4P),               intent(in)           :: x(1:)        !< X component of data variable.
  integer(I4P),               intent(in)           :: y(1:)        !< Y component of data variable.
  integer(I4P),               intent(in)           :: z(1:)        !< Z component of data variable.
  logical,                    intent(in), optional :: is_tuples    !< Use "NumberOfTuples" instead of "NumberOfComponents".
  integer(I4P)                                     :: error        !< Error status.
  endfunction write_dataarray3_rank1_I4P_interface

  function write_dataarray3_rank1_I2P_interface(self, data_name, x, y, z, is_tuples) result(error)
  !< Write `<DataArray... NumberOfComponents="3"...>...</DataArray>` tag (I2P).
  import :: vtk_writer_abstract, I2P, I4P
  class(vtk_writer_abstract), intent(inout)        :: self         !< Writer.
  character(*),               intent(in)           :: data_name    !< Data name.
  integer(I2P),               intent(in)           :: x(1:)        !< X component of data variable.
  integer(I2P),               intent(in)           :: y(1:)        !< Y component of data variable.
  integer(I2P),               intent(in)           :: z(1:)        !< Z component of data variable.
  logical,                    intent(in), optional :: is_tuples    !< Use "NumberOfTuples" instead of "NumberOfComponents".
  integer(I4P)                                     :: error        !< Error status.
  endfunction write_dataarray3_rank1_I2P_interface

  function write_dataarray3_rank1_I1P_interface(self, data_name, x, y, z, is_tuples) result(error)
  !< Write `<DataArray... NumberOfComponents="3"...>...</DataArray>` tag (I1P).
  import :: vtk_writer_abstract, I1P, I4P
  class(vtk_writer_abstract), intent(inout)        :: self         !< Writer.
  character(*),               intent(in)           :: data_name    !< Data name.
  integer(I1P),               intent(in)           :: x(1:)        !< X component of data variable.
  integer(I1P),               intent(in)           :: y(1:)        !< Y component of data variable.
  integer(I1P),               intent(in)           :: z(1:)        !< Z component of data variable.
  logical,                    intent(in), optional :: is_tuples    !< Use "NumberOfTuples" instead of "NumberOfComponents".
  integer(I4P)                                     :: error        !< Error status.
  endfunction write_dataarray3_rank1_I1P_interface

  function write_dataarray3_rank3_R8P_interface(self, data_name, x, y, z, is_tuples) result(error)
  !< Write `<DataArray... NumberOfComponents="3"...>...</DataArray>` tag (R8P).
  import :: vtk_writer_abstract, I4P, R8P
  class(vtk_writer_abstract), intent(inout)        :: self         !< Writer.
  character(*),               intent(in)           :: data_name    !< Data name.
  real(R8P),                  intent(in)           :: x(1:,1:,1:)  !< X component of data variable.
  real(R8P),                  intent(in)           :: y(1:,1:,1:)  !< Y component of data variable.
  real(R8P),                  intent(in)           :: z(1:,1:,1:)  !< Z component of data variable.
  logical,                    intent(in), optional :: is_tuples    !< Use "NumberOfTuples" instead of "NumberOfComponents".
  integer(I4P)                                     :: error        !< Error status.
  endfunction write_dataarray3_rank3_R8P_interface

  function write_dataarray3_rank3_R4P_interface(self, data_name, x, y, z, is_tuples) result(error)
  !< Write `<DataArray... NumberOfComponents="3"...>...</DataArray>` tag (R4P).
  import :: vtk_writer_abstract, I4P, R4P
  class(vtk_writer_abstract), intent(inout)        :: self         !< Writer.
  character(*),               intent(in)           :: data_name    !< Data name.
  real(R4P),                  intent(in)           :: x(1:,1:,1:)  !< X component of data variable.
  real(R4P),                  intent(in)           :: y(1:,1:,1:)  !< Y component of data variable.
  real(R4P),                  intent(in)           :: z(1:,1:,1:)  !< Z component of data variable.
  logical,                    intent(in), optional :: is_tuples    !< Use "NumberOfTuples" instead of "NumberOfComponents".
  integer(I4P)                                     :: error        !< Error status.
  endfunction write_dataarray3_rank3_R4P_interface

  function write_dataarray3_rank3_I8P_interface(self, data_name, x, y, z, is_tuples) result(error)
  !< Write `<DataArray... NumberOfComponents="3"...>...</DataArray>` tag (I8P).
  import :: vtk_writer_abstract, I4P, I8P
  class(vtk_writer_abstract), intent(inout)        :: self         !< Writer.
  character(*),               intent(in)           :: data_name    !< Data name.
  integer(I8P),               intent(in)           :: x(1:,1:,1:)  !< X component of data variable.
  integer(I8P),               intent(in)           :: y(1:,1:,1:)  !< Y component of data variable.
  integer(I8P),               intent(in)           :: z(1:,1:,1:)  !< Z component of data variable.
  logical,                    intent(in), optional :: is_tuples    !< Use "NumberOfTuples" instead of "NumberOfComponents".
  integer(I4P)                                     :: error        !< Error status.
  endfunction write_dataarray3_rank3_I8P_interface

  function write_dataarray3_rank3_I4P_interface(self, data_name, x, y, z, is_tuples) result(error)
  !< Write `<DataArray... NumberOfComponents="3"...>...</DataArray>` tag (I4P).
  import :: vtk_writer_abstract, I4P
  class(vtk_writer_abstract), intent(inout)        :: self         !< Writer.
  character(*),               intent(in)           :: data_name    !< Data name.
  integer(I4P),               intent(in)           :: x(1:,1:,1:)  !< X component of data variable.
  integer(I4P),               intent(in)           :: y(1:,1:,1:)  !< Y component of data variable.
  integer(I4P),               intent(in)           :: z(1:,1:,1:)  !< Z component of data variable.
  logical,                    intent(in), optional :: is_tuples    !< Use "NumberOfTuples" instead of "NumberOfComponents".
  integer(I4P)                                     :: error        !< Error status.
  endfunction write_dataarray3_rank3_I4P_interface

  function write_dataarray3_rank3_I2P_interface(self, data_name, x, y, z, is_tuples) result(error)
  !< Write `<DataArray... NumberOfComponents="3"...>...</DataArray>` tag (I2P).
  import :: vtk_writer_abstract, I2P, I4P
  class(vtk_writer_abstract), intent(inout)        :: self         !< Writer.
  character(*),               intent(in)           :: data_name    !< Data name.
  integer(I2P),               intent(in)           :: x(1:,1:,1:)  !< X component of data variable.
  integer(I2P),               intent(in)           :: y(1:,1:,1:)  !< Y component of data variable.
  integer(I2P),               intent(in)           :: z(1:,1:,1:)  !< Z component of data variable.
  logical,                    intent(in), optional :: is_tuples    !< Use "NumberOfTuples" instead of "NumberOfComponents".
  integer(I4P)                                     :: error        !< Error status.
  endfunction write_dataarray3_rank3_I2P_interface

  function write_dataarray3_rank3_I1P_interface(self, data_name, x, y, z, is_tuples) result(error)
  !< Write `<DataArray... NumberOfComponents="3"...>...</DataArray>` tag (I1P).
  import :: vtk_writer_abstract, I1P, I4P
  class(vtk_writer_abstract), intent(inout)        :: self         !< Writer.
  character(*),               intent(in)           :: data_name    !< Data name.
  integer(I1P),               intent(in)           :: x(1:,1:,1:)  !< X component of data variable.
  integer(I1P),               intent(in)           :: y(1:,1:,1:)  !< Y component of data variable.
  integer(I1P),               intent(in)           :: z(1:,1:,1:)  !< Z component of data variable.
  logical,                    intent(in), optional :: is_tuples    !< Use "NumberOfTuples" instead of "NumberOfComponents".
  integer(I4P)                                     :: error        !< Error status.
  endfunction write_dataarray3_rank3_I1P_interface

  function write_dataarray6_rank1_R8P_interface(self, data_name, u, v, w, x, y, z, is_tuples) result(error)
  !< Write `<DataArray... NumberOfComponents="6"...>...</DataArray>` tag (R8P).
  import :: vtk_writer_abstract, I4P, R8P
  class(vtk_writer_abstract), intent(inout)        :: self         !< Writer.
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
  import :: vtk_writer_abstract, I4P, R4P
  class(vtk_writer_abstract), intent(inout)        :: self         !< Writer.
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
  import :: vtk_writer_abstract, I4P, I8P
  class(vtk_writer_abstract), intent(inout)        :: self         !< Writer.
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
  import :: vtk_writer_abstract, I4P
  class(vtk_writer_abstract), intent(inout)        :: self         !< Writer.
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
  import :: vtk_writer_abstract, I2P, I4P
  class(vtk_writer_abstract), intent(inout)        :: self         !< Writer.
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
  import :: vtk_writer_abstract, I1P, I4P
  class(vtk_writer_abstract), intent(inout)        :: self         !< Writer.
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
  import :: vtk_writer_abstract, I4P, R8P
  class(vtk_writer_abstract), intent(inout)        :: self         !< Writer.
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
  import :: vtk_writer_abstract, I4P, R4P
  class(vtk_writer_abstract), intent(inout)        :: self         !< Writer.
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
  import :: vtk_writer_abstract, I4P, I8P
  class(vtk_writer_abstract), intent(inout)        :: self         !< Writer.
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
  import :: vtk_writer_abstract, I4P
  class(vtk_writer_abstract), intent(inout)        :: self         !< Writer.
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
  import :: vtk_writer_abstract, I2P, I4P
  class(vtk_writer_abstract), intent(inout)        :: self         !< Writer.
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
  import :: vtk_writer_abstract, I1P, I4P
  class(vtk_writer_abstract), intent(inout)        :: self         !< Writer.
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

  function write_connectivity_I4P_interface(self, nc, connectivity, offset, cell_type, face, faceoffset) result(error)
  import :: vtk_writer_abstract, I1P, I4P
  class(vtk_writer_abstract), intent(inout) :: self
  integer(I4P),               intent(in)   :: nc
  integer(I4P),               intent(in)   :: connectivity(1:)
  integer(I4P),               intent(in)   :: offset(1:)
  integer(I4P),   optional,   intent(in)   :: face(1:)
  integer(I4P),   optional,   intent(in)   :: faceoffset(1:)
  integer(I1P),               intent(in)   :: cell_type(1:)
  integer(I4P)                             :: error
  endfunction write_connectivity_I4P_interface

  function write_connectivity_I8P_interface(self, nc, connectivity, offset, cell_type, face, faceoffset) result(error)
  import :: vtk_writer_abstract, I1P, I4P, I8P
  class(vtk_writer_abstract), intent(inout) :: self
  integer(I8P),               intent(in)   :: nc
  integer(I8P),               intent(in)   :: connectivity(1:)
  integer(I8P),               intent(in)   :: offset(1:)
  integer(I8P),   optional,   intent(in)   :: face(1:)
  integer(I8P),   optional,   intent(in)   :: faceoffset(1:)
  integer(I1P),               intent(in)   :: cell_type(1:)
  integer(I4P)                             :: error
  endfunction write_connectivity_I8P_interface

  function write_dataarray_location_tag_interface(self, location, action, scalars, vectors, normals, tensors, tcoords, &
                                                  global_ids, pedigree_ids, edge_flags, tangents, rational_weights, &
                                                  higher_order_degrees, process_ids) &
                                                  result(error)
  import :: vtk_writer_abstract, I4P
  class(vtk_writer_abstract), intent(inout) :: self
  character(*),               intent(in)   :: location
  character(*),               intent(in)   :: action
  character(*),               intent(in), optional :: scalars
  character(*),               intent(in), optional :: vectors
  character(*),               intent(in), optional :: normals
  character(*),               intent(in), optional :: tensors
  character(*),               intent(in), optional :: tcoords
  character(*),               intent(in), optional :: global_ids
  character(*),               intent(in), optional :: pedigree_ids
  character(*),               intent(in), optional :: edge_flags
  character(*),               intent(in), optional :: tangents
  character(*),               intent(in), optional :: rational_weights
  character(*),               intent(in), optional :: higher_order_degrees
  character(*),               intent(in), optional :: process_ids
  integer(I4P)                             :: error
  endfunction write_dataarray_location_tag_interface

  function write_fielddata1_rank0_interface(self, data_name, x) result(error)
  import :: vtk_writer_abstract, I4P
  class(vtk_writer_abstract), intent(inout) :: self
  character(*),               intent(in)   :: data_name
  class(*),                   intent(in)   :: x
  integer(I4P)                             :: error
  endfunction write_fielddata1_rank0_interface

  function write_fielddata1_rank1_interface(self, data_name, x) result(error)
  import :: vtk_writer_abstract, I4P
  class(vtk_writer_abstract), intent(inout) :: self
  character(*),               intent(in)   :: data_name
  class(*),                   intent(in)   :: x(1:)
  integer(I4P)                             :: error
  endfunction write_fielddata1_rank1_interface

  function write_fielddata_tag_interface(self, action) result(error)
  import :: vtk_writer_abstract, I4P
  class(vtk_writer_abstract), intent(inout) :: self
  character(*),               intent(in)   :: action
  integer(I4P)                             :: error
  endfunction write_fielddata_tag_interface

  function write_dataarray_unsigned_I1P_interface(self, data_name, x) result(error)
  import :: vtk_writer_abstract, I1P, I4P
  class(vtk_writer_abstract), intent(inout) :: self
  character(*),               intent(in)   :: data_name
  integer(I1P),               intent(in)   :: x(1:)
  integer(I4P)                             :: error
  endfunction write_dataarray_unsigned_I1P_interface

  function write_dataarray_unsigned_I2P_interface(self, data_name, x) result(error)
  import :: vtk_writer_abstract, I2P, I4P
  class(vtk_writer_abstract), intent(inout) :: self
  character(*),               intent(in)   :: data_name
  integer(I2P),               intent(in)   :: x(1:)
  integer(I4P)                             :: error
  endfunction write_dataarray_unsigned_I2P_interface

  function write_dataarray_unsigned_I4P_interface(self, data_name, x) result(error)
  import :: vtk_writer_abstract, I4P
  class(vtk_writer_abstract), intent(inout) :: self
  character(*),               intent(in)   :: data_name
  integer(I4P),               intent(in)   :: x(1:)
  integer(I4P)                             :: error
  endfunction write_dataarray_unsigned_I4P_interface

  function write_dataarray_unsigned_I8P_interface(self, data_name, x) result(error)
  import :: vtk_writer_abstract, I4P, I8P
  class(vtk_writer_abstract), intent(inout) :: self
  character(*),               intent(in)   :: data_name
  integer(I8P),               intent(in)   :: x(1:)
  integer(I4P)                             :: error
  endfunction write_dataarray_unsigned_I8P_interface

  function write_geo_strg_data1_rank2_R8P_interface(self, xyz) result(error)
  import :: vtk_writer_abstract, I4P, R8P
  class(vtk_writer_abstract), intent(inout) :: self
  real(R8P),                  intent(in)   :: xyz(1:,1:)
  integer(I4P)                             :: error
  endfunction write_geo_strg_data1_rank2_R8P_interface

  function write_geo_strg_data1_rank2_R4P_interface(self, xyz) result(error)
  import :: vtk_writer_abstract, I4P, R4P
  class(vtk_writer_abstract), intent(inout) :: self
  real(R4P),                  intent(in)   :: xyz(1:,1:)
  integer(I4P)                             :: error
  endfunction write_geo_strg_data1_rank2_R4P_interface

  function write_geo_strg_data1_rank4_R8P_interface(self, xyz) result(error)
  import :: vtk_writer_abstract, I4P, R8P
  class(vtk_writer_abstract), intent(inout) :: self
  real(R8P),                  intent(in)   :: xyz(1:,1:,1:,1:)
  integer(I4P)                             :: error
  endfunction write_geo_strg_data1_rank4_R8P_interface

  function write_geo_strg_data1_rank4_R4P_interface(self, xyz) result(error)
  import :: vtk_writer_abstract, I4P, R4P
  class(vtk_writer_abstract), intent(inout) :: self
  real(R4P),                  intent(in)   :: xyz(1:,1:,1:,1:)
  integer(I4P)                             :: error
  endfunction write_geo_strg_data1_rank4_R4P_interface

  function write_geo_strg_data3_rank1_R8P_interface(self, n, x, y, z) result(error)
  import :: vtk_writer_abstract, I4P, R8P
  class(vtk_writer_abstract), intent(inout) :: self
  integer(I4P),               intent(in)   :: n
  real(R8P),                  intent(in)   :: x(1:)
  real(R8P),                  intent(in)   :: y(1:)
  real(R8P),                  intent(in)   :: z(1:)
  integer(I4P)                             :: error
  endfunction write_geo_strg_data3_rank1_R8P_interface

  function write_geo_strg_data3_rank1_R4P_interface(self, n, x, y, z) result(error)
  import :: vtk_writer_abstract, I4P, R4P
  class(vtk_writer_abstract), intent(inout) :: self
  integer(I4P),               intent(in)   :: n
  real(R4P),                  intent(in)   :: x(1:)
  real(R4P),                  intent(in)   :: y(1:)
  real(R4P),                  intent(in)   :: z(1:)
  integer(I4P)                             :: error
  endfunction write_geo_strg_data3_rank1_R4P_interface

  function write_geo_strg_data3_rank3_R8P_interface(self, n, x, y, z) result(error)
  import :: vtk_writer_abstract, I4P, R8P
  class(vtk_writer_abstract), intent(inout) :: self
  integer(I4P),               intent(in)   :: n
  real(R8P),                  intent(in)   :: x(1:,1:,1:)
  real(R8P),                  intent(in)   :: y(1:,1:,1:)
  real(R8P),                  intent(in)   :: z(1:,1:,1:)
  integer(I4P)                             :: error
  endfunction write_geo_strg_data3_rank3_R8P_interface

  function write_geo_strg_data3_rank3_R4P_interface(self, n, x, y, z) result(error)
  import :: vtk_writer_abstract, I4P, R4P
  class(vtk_writer_abstract), intent(inout) :: self
  integer(I4P),               intent(in)   :: n
  real(R4P),                  intent(in)   :: x(1:,1:,1:)
  real(R4P),                  intent(in)   :: y(1:,1:,1:)
  real(R4P),                  intent(in)   :: z(1:,1:,1:)
  integer(I4P)                             :: error
  endfunction write_geo_strg_data3_rank3_R4P_interface

  function write_geo_rect_data3_rank1_R8P_interface(self, x, y, z) result(error)
  import :: vtk_writer_abstract, I4P, R8P
  class(vtk_writer_abstract), intent(inout) :: self
  real(R8P),                  intent(in)   :: x(1:)
  real(R8P),                  intent(in)   :: y(1:)
  real(R8P),                  intent(in)   :: z(1:)
  integer(I4P)                             :: error
  endfunction write_geo_rect_data3_rank1_R8P_interface

  function write_geo_rect_data3_rank1_R4P_interface(self, x, y, z) result(error)
  import :: vtk_writer_abstract, I4P, R4P
  class(vtk_writer_abstract), intent(inout) :: self
  real(R4P),                  intent(in)   :: x(1:)
  real(R4P),                  intent(in)   :: y(1:)
  real(R4P),                  intent(in)   :: z(1:)
  integer(I4P)                             :: error
  endfunction write_geo_rect_data3_rank1_R4P_interface

  function write_geo_unst_data1_rank2_R8P_interface(self, np, nc, xyz) result(error)
  import :: vtk_writer_abstract, I4P, R8P
  class(vtk_writer_abstract), intent(inout) :: self
  integer(I4P),               intent(in)   :: np
  integer(I4P),               intent(in)   :: nc
  real(R8P),                  intent(in)   :: xyz(1:,1:)
  integer(I4P)                             :: error
  endfunction write_geo_unst_data1_rank2_R8P_interface

  function write_geo_unst_data1_rank2_R4P_interface(self, np, nc, xyz) result(error)
  import :: vtk_writer_abstract, I4P, R4P
  class(vtk_writer_abstract), intent(inout) :: self
  integer(I4P),               intent(in)   :: np
  integer(I4P),               intent(in)   :: nc
  real(R4P),                  intent(in)   :: xyz(1:,1:)
  integer(I4P)                             :: error
  endfunction write_geo_unst_data1_rank2_R4P_interface

  function write_geo_unst_data3_rank1_R8P_interface(self, np, nc, x, y, z) result(error)
  import :: vtk_writer_abstract, I4P, R8P
  class(vtk_writer_abstract), intent(inout) :: self
  integer(I4P),               intent(in)   :: np
  integer(I4P),               intent(in)   :: nc
  real(R8P),                  intent(in)   :: x(1:)
  real(R8P),                  intent(in)   :: y(1:)
  real(R8P),                  intent(in)   :: z(1:)
  integer(I4P)                             :: error
  endfunction write_geo_unst_data3_rank1_R8P_interface

  function write_geo_unst_data3_rank1_R4P_interface(self, np, nc, x, y, z) result(error)
  import :: vtk_writer_abstract, I4P, R4P
  class(vtk_writer_abstract), intent(inout) :: self
  integer(I4P),               intent(in)   :: np
  integer(I4P),               intent(in)   :: nc
  real(R4P),                  intent(in)   :: x(1:)
  real(R4P),                  intent(in)   :: y(1:)
  real(R4P),                  intent(in)   :: z(1:)
  integer(I4P)                             :: error
  endfunction write_geo_unst_data3_rank1_R4P_interface

  function write_geo_unst_data1_rank2_R8P_I8P_interface(self, np, nc, xyz) result(error)
  import :: vtk_writer_abstract, I4P, I8P, R8P
  class(vtk_writer_abstract), intent(inout) :: self
  integer(I8P),               intent(in)   :: np
  integer(I8P),               intent(in)   :: nc
  real(R8P),                  intent(in)   :: xyz(1:,1:)
  integer(I4P)                             :: error
  endfunction write_geo_unst_data1_rank2_R8P_I8P_interface

  function write_geo_unst_data1_rank2_R4P_I8P_interface(self, np, nc, xyz) result(error)
  import :: vtk_writer_abstract, I4P, I8P, R4P
  class(vtk_writer_abstract), intent(inout) :: self
  integer(I8P),               intent(in)   :: np
  integer(I8P),               intent(in)   :: nc
  real(R4P),                  intent(in)   :: xyz(1:,1:)
  integer(I4P)                             :: error
  endfunction write_geo_unst_data1_rank2_R4P_I8P_interface

  function write_geo_unst_data3_rank1_R8P_I8P_interface(self, np, nc, x, y, z) result(error)
  import :: vtk_writer_abstract, I4P, I8P, R8P
  class(vtk_writer_abstract), intent(inout) :: self
  integer(I8P),               intent(in)   :: np
  integer(I8P),               intent(in)   :: nc
  real(R8P),                  intent(in)   :: x(1:)
  real(R8P),                  intent(in)   :: y(1:)
  real(R8P),                  intent(in)   :: z(1:)
  integer(I4P)                             :: error
  endfunction write_geo_unst_data3_rank1_R8P_I8P_interface

  function write_geo_unst_data3_rank1_R4P_I8P_interface(self, np, nc, x, y, z) result(error)
  import :: vtk_writer_abstract, I4P, I8P, R4P
  class(vtk_writer_abstract), intent(inout) :: self
  integer(I8P),               intent(in)   :: np
  integer(I8P),               intent(in)   :: nc
  real(R4P),                  intent(in)   :: x(1:)
  real(R4P),                  intent(in)   :: y(1:)
  real(R4P),                  intent(in)   :: z(1:)
  integer(I4P)                             :: error
  endfunction write_geo_unst_data3_rank1_R4P_I8P_interface

  function write_piece_start_tag_interface(self, nx1, nx2, ny1, ny2, nz1, nz2) result(error)
  import :: vtk_writer_abstract, I4P
  class(vtk_writer_abstract), intent(inout) :: self
  integer(I4P),               intent(in)   :: nx1
  integer(I4P),               intent(in)   :: nx2
  integer(I4P),               intent(in)   :: ny1
  integer(I4P),               intent(in)   :: ny2
  integer(I4P),               intent(in)   :: nz1
  integer(I4P),               intent(in)   :: nz2
  integer(I4P)                             :: error
  endfunction write_piece_start_tag_interface

  function write_piece_start_tag_unst_interface(self, np, nc) result(error)
  import :: vtk_writer_abstract, I4P
  class(vtk_writer_abstract), intent(inout) :: self
  integer(I4P),               intent(in)   :: np
  integer(I4P),               intent(in)   :: nc
  integer(I4P)                             :: error
  endfunction write_piece_start_tag_unst_interface

  function write_piece_start_tag_poly_interface(self, np, nverts, nlines, nstrips, npolys) result(error)
  import :: vtk_writer_abstract, I4P
  class(vtk_writer_abstract), intent(inout) :: self
  integer(I4P),               intent(in)   :: np
  integer(I4P),               intent(in)   :: nverts
  integer(I4P),               intent(in)   :: nlines
  integer(I4P),               intent(in)   :: nstrips
  integer(I4P),               intent(in)   :: npolys
  integer(I4P)                             :: error
  endfunction write_piece_start_tag_poly_interface

  function write_piece_start_tag_unst_I8P_interface(self, np, nc) result(error)
  import :: vtk_writer_abstract, I4P, I8P
  class(vtk_writer_abstract), intent(inout) :: self
  integer(I8P),               intent(in)   :: np
  integer(I8P),               intent(in)   :: nc
  integer(I4P)                             :: error
  endfunction write_piece_start_tag_unst_I8P_interface

  function write_piece_start_tag_poly_I8P_interface(self, np, nverts, nlines, nstrips, npolys) result(error)
  import :: vtk_writer_abstract, I4P, I8P
  class(vtk_writer_abstract), intent(inout) :: self
  integer(I8P),               intent(in)   :: np
  integer(I8P),               intent(in)   :: nverts
  integer(I8P),               intent(in)   :: nlines
  integer(I8P),               intent(in)   :: nstrips
  integer(I8P),               intent(in)   :: npolys
  integer(I4P)                             :: error
  endfunction write_piece_start_tag_poly_I8P_interface

  function write_piece_end_tag_interface(self) result(error)
  import :: vtk_writer_abstract, I4P
  class(vtk_writer_abstract), intent(inout) :: self
  integer(I4P)                             :: error
  endfunction write_piece_end_tag_interface

  function write_polydata_cells_interface(self, verts_connectivity, verts_offset, lines_connectivity, lines_offset, &
      strips_connectivity, strips_offset, polys_connectivity, polys_offset) result(error)
  import :: vtk_writer_abstract, I4P
  class(vtk_writer_abstract), intent(inout) :: self
  class(*),                   intent(in), optional :: verts_connectivity(1:)
  class(*),                   intent(in), optional :: verts_offset(1:)
  class(*),                   intent(in), optional :: lines_connectivity(1:)
  class(*),                   intent(in), optional :: lines_offset(1:)
  class(*),                   intent(in), optional :: strips_connectivity(1:)
  class(*),                   intent(in), optional :: strips_offset(1:)
  class(*),                   intent(in), optional :: polys_connectivity(1:)
  class(*),                   intent(in), optional :: polys_offset(1:)
  integer(I4P)                             :: error
  endfunction write_polydata_cells_interface

  function write_step_interface(self, time) result(error)
  import :: vtk_writer_abstract, I4P, R8P
  class(vtk_writer_abstract), intent(inout) :: self
  real(R8P),                  intent(in)    :: time
  integer(I4P)                             :: error
  endfunction write_step_interface

  elemental subroutine free_interface(self, error)
  import :: vtk_writer_abstract, I4P
  class(vtk_writer_abstract), intent(inout) :: self
  integer(I4P),               intent(out), optional :: error
  endsubroutine free_interface

  pure subroutine get_xml_volatile_interface(self, xml_volatile, error)
  import :: vtk_writer_abstract, I4P
  class(vtk_writer_abstract), intent(in)   :: self
  character(len=:),           intent(out), allocatable :: xml_volatile
  integer(I4P),               intent(out), optional :: error
  endsubroutine get_xml_volatile_interface
endinterface
endmodule vtk_fortran_vtk_writer_abstract
