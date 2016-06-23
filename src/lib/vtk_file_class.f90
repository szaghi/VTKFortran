!< VTK file class.
module vtk_file_class
!-----------------------------------------------------------------------------------------------------------------------------------
!< VTK file class.
!-----------------------------------------------------------------------------------------------------------------------------------
use befor64
use penf
use stringifor
use vtk_file_xml_writer_abstract
use vtk_file_xml_writer_ascii_local
use vtk_fortran_parameters
!-----------------------------------------------------------------------------------------------------------------------------------

!-----------------------------------------------------------------------------------------------------------------------------------
implicit none
private
save
public :: vtk_file
!-----------------------------------------------------------------------------------------------------------------------------------

!-----------------------------------------------------------------------------------------------------------------------------------
type :: vtk_file
  !< VTK file class.
  private
  integer(I4P)                            :: format=ascii  !< Output format, integer code.
  type(string)                            :: format_ch     !< Output format, string code.
  type(string)                            :: topology      !< Mesh topology.
  integer(I4P)                            :: indent=0_I4P  !< Indent count.
  integer(I8P)                            :: ioffset=0_I8P !< Offset count.
  integer(I4P)                            :: xml=0_I4P     !< XML Logical unit.
  integer(I4P)                            :: scratch=0_I4P !< Scratch logical unit.
  integer(I4P)                            :: error=0_I4P   !< IO Error status.
  class(xml_writer_abstract), allocatable :: xml_writer    !< XML writer facility.
  contains
    procedure, pass(self), private :: strategy_initialize !< Initialize file (exporter).
    include 'vtk_file_initialize_method.inc'                 ! generic                        :: initialize
    include 'vtk_file_finalize_method.inc'                   ! generic                        :: finalize
    include 'vtk_file_write_fielddata_method.inc'            ! generic                        :: write_fielddata
    include 'vtk_file_write_piece_method.inc'                ! generic                        :: write_piece
    include 'vtk_file_write_geo_method.inc'                  ! generic                        :: write_geo
    include 'vtk_file_write_dataarray_method.inc'            ! generic                        :: write_dataarray
    include 'vtk_file_file_methods.inc'
                                                             ! procedure, pass(self), private :: open_xml_file
                                                             ! procedure, pass(self), private :: open_scratch_file
                                                             ! procedure, pass(self), private :: ioffset_update
    include 'vtk_file_tag_methods.inc'
                                                             ! procedure, pass(self), private :: self_closing_tag
                                                             ! procedure, pass(self), private :: tag
                                                             ! procedure, pass(self), private :: start_tag
                                                             ! procedure, pass(self), private :: end_tag
    include 'vtk_file_write_tag_methods.inc'
                                                             ! procedure, pass(self), private :: write_self_closing_tag
                                                             ! procedure, pass(self), private :: write_tag
                                                             ! procedure, pass(self), private :: write_start_tag
                                                             ! procedure, pass(self), private :: write_end_tag
                                                             ! procedure, pass(self), private :: write_header_tag
                                                             ! procedure, pass(self), private :: write_topology_tag
                                                             ! procedure, pass(self), private :: write_dataarray_tag
                                                             ! procedure, pass(self), private :: write_dataarray_tag_appended
                                                             ! procedure, pass(self), private :: write_dataarray_appended
    include 'vtk_file_write_on_scratch_dataarray_method.inc' ! generic,               private :: write_on_scratch_dataarray
    include 'vtk_file_encode_ascii_dataarray_method.inc'     ! generic,               private :: encode_ascii_dataarray
    include 'vtk_file_encode_base64_dataarray_method.inc'    ! generic,               private :: encode_base64_dataarray
endtype vtk_file
!-----------------------------------------------------------------------------------------------------------------------------------
contains
  function strategy_initialize(self, format, filename, mesh_topology, nx1, nx2, ny1, ny2, nz1, nz2) result(error)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Initialize file (exporter).
  !<
  !< @note This function must be the first to be called.
  !<
  !<### Supported output formats are (the passed specifier value is case insensitive):
  !<
  !<- ASCII: data are saved in ASCII format;
  !<- BINARY: data are saved in base64 encoded format;
  !<- RAW: data are saved in raw-binary format in the appended tag of the XML file;
  !<- BINARY-APPENDED: data are saved in base64 encoded format in the appended tag of the XML file.
  !<
  !<### Supported topologies are:
  !<
  !<- RectilinearGrid;
  !<- StructuredGrid;
  !<- UnstructuredGrid.
  !<
  !<### Example of usage
  !<
  !<```fortran
  !< type(vtk_file) :: vtk
  !< integer(I4P)   :: nx1, nx2, ny1, ny2, nz1, nz2
  !< ...
  !< error = vtk%initialize_write('BINARY','XML_RECT_BINARY.vtr','RectilinearGrid',nx1=nx1,nx2=nx2,ny1=ny1,ny2=ny2,nz1=nz1,nz2=nz2)
  !< ...
  !<```
  !< @note The file extension is necessary in the file name. The XML standard has different extensions for each
  !< different topologies (e.g. *vtr* for rectilinear topology). See the VTK-standard file for more information.
  !---------------------------------------------------------------------------------------------------------------------------------
  class(vtk_file), intent(inout)         :: self          !< VTK file.
  character(*),    intent(in)            :: format        !< File format: ASCII, BINARY, RAW or BINARY-APPENDED.
  character(*),    intent(in)            :: filename      !< File name.
  character(*),    intent(in)            :: mesh_topology !< Mesh topology.
  integer(I4P),    intent(in),  optional :: nx1           !< Initial node of x axis.
  integer(I4P),    intent(in),  optional :: nx2           !< Final node of x axis.
  integer(I4P),    intent(in),  optional :: ny1           !< Initial node of y axis.
  integer(I4P),    intent(in),  optional :: ny2           !< Final node of y axis.
  integer(I4P),    intent(in),  optional :: nz1           !< Initial node of z axis.
  integer(I4P),    intent(in),  optional :: nz2           !< Final node of z axis.
  integer(I4P)                           :: error         !< Error status.
  type(string)                           :: fformat       !< File format.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  if (.not.is_initialized) call penf_init
  if (.not.is_b64_initialized) call b64_init
  fformat = trim(adjustl(format))
  fformat = fformat%upper()
  select case(fformat%chars())
  case('ASCII')
    if (allocated(self%xml_writer)) deallocate(self%xml_writer)
    allocate(xml_writer_ascii_local :: self%xml_writer)
  case('RAW')
  case('BINARY-APPENDED')
  case('BINARY')
  endselect
  error = self%xml_writer%initialize(format=format, filename=filename, mesh_topology=mesh_topology, &
                                     nx1=nx1, nx2=nx2, ny1=ny1, ny2=ny2, nz1=nz1, nz2=nz2)
  return
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction strategy_initialize

  include 'vtk_file_initialize.inc'
  include 'vtk_file_finalize.inc'
  include 'vtk_file_write_fielddata.inc'
  include 'vtk_file_write_piece.inc'
  include 'vtk_file_write_geo_strg.inc'
  include 'vtk_file_write_geo_rect.inc'
  include 'vtk_file_write_geo_unst.inc'
  include 'vtk_file_write_dataarray.inc'
  include 'vtk_file_file.inc'
  include 'vtk_file_tag.inc'
  include 'vtk_file_write_tag.inc'
  include 'vtk_file_write_on_scratch_dataarray.inc'
  include 'vtk_file_encode_ascii_dataarray.inc'
  include 'vtk_file_encode_base64_dataarray.inc'
endmodule vtk_file_class
