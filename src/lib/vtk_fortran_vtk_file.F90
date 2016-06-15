!< VTK file class.
module vtk_fortran_vtk_file
!-----------------------------------------------------------------------------------------------------------------------------------
!< VTK file class.
!-----------------------------------------------------------------------------------------------------------------------------------
use befor64
use penf
use stringifor
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
  integer(I4P) :: format=ascii  !< Output format, integer code.
  type(string) :: format_ch     !< Output format, string code.
  type(string) :: topology      !< Mesh topology.
  integer(I4P) :: indent=0_I4P  !< Indent count.
  integer(I8P) :: ioffset=0_I8P !< Offset count.
  integer(I4P) :: xml=0_I4P     !< XML Logical unit.
  integer(I4P) :: scratch=0_I4P !< Scratch logical unit.
  integer(I4P) :: error=0_I4P   !< IO Error status.
  contains
    ! public methods
    generic :: initialize => &
               initialize_write !< Initialize file.
    generic :: finalize => &
               finalize_write   !< Finalize file.
    generic :: write_piece =>         &
               write_piece_start_tag, &
               write_piece_end_tag      !< Write Piece start/end tag.
    generic :: write_geo =>                     &
               write_geo_strg_data1_rank2_R8P,  &
               write_geo_strg_data1_rank2_R4P,  &
               write_geo_strg_data1_rank4_R8P,  &
               write_geo_strg_data1_rank4_R4P,  &
               write_geo_strg_data3_rank1_R8P,  &
               write_geo_strg_data3_rank1_R4P,  &
               write_geo_strg_data3_rank3_R8P,  &
               write_geo_strg_data3_rank3_R4P !< Write mesh.
    generic :: write_data =>            &
               write_data_location_tag, &
               write_data1_rank1_R8P,   &
               write_data1_rank1_R4P,   &
               write_data1_rank2_R8P,   &
               write_data1_rank2_R4P,   &
               write_data1_rank3_R8P,   &
               write_data1_rank3_R4P,   &
               write_data1_rank4_R8P,   &
               write_data1_rank4_R4P,   &
               write_data3_rank1_R8P,   &
               write_data3_rank1_R4P,   &
               write_data3_rank3_R8P,   &
               write_data3_rank3_R4P    !< Write data (array).

    ! private methods
    procedure, pass(self), private :: initialize_write  !< Initialize file (exporter).
    procedure, pass(self), private :: finalize_write    !< Finalize file (exporter).
    procedure, pass(self), private :: open_xml_file     !< Open xml file.
    procedure, pass(self), private :: open_scratch_file !< Open scratch file.
    procedure, pass(self), private :: ioffset_update    !< Update ioffset count.
    ! tags
    procedure, pass(self), private :: self_closing_tag       !< Return `<tag_name.../>` self closing tag.
    procedure, pass(self), private :: tag                    !< Return `<tag_name...>...</tag_name>` tag.
    procedure, pass(self), private :: start_tag              !< Return `<tag_name...>` start tag.
    procedure, pass(self), private :: end_tag                !< Return `</tag_name>` end tag.
    procedure, pass(self), private :: write_self_closing_tag !< Write `<tag_name.../>` self closing tag.
    procedure, pass(self), private :: write_tag              !< Write `<tag_name...>...</tag_name>` tag.
    procedure, pass(self), private :: write_start_tag        !< Write `<tag_name...>` start tag.
    procedure, pass(self), private :: write_end_tag          !< Write `</tag_name>` end tag.
    procedure, pass(self), private :: write_header_tag       !< Write header tag.
    procedure, pass(self), private :: write_topology_tag     !< Write topology tag.
    procedure, pass(self), private :: write_piece_start_tag  !< Write `<Piece ...>` start tag.
    procedure, pass(self), private :: write_piece_end_tag    !< Write `</Piece>` end tag.
    ! geo
    procedure, pass(self), private :: write_geo_strg_data1_rank2_R8P !< Write **StructuredGrid** mesh (data 1, rank 2, R8P).
    procedure, pass(self), private :: write_geo_strg_data1_rank2_R4P !< Write **StructuredGrid** mesh (data 1, rank 2, R4P).
    procedure, pass(self), private :: write_geo_strg_data1_rank4_R8P !< Write **StructuredGrid** mesh (data 1, rank 4, R8P).
    procedure, pass(self), private :: write_geo_strg_data1_rank4_R4P !< Write **StructuredGrid** mesh (data 1, rank 4, R4P).
    procedure, pass(self), private :: write_geo_strg_data3_rank1_R8P !< Write **StructuredGrid** mesh (data 3, rank 1, R8P).
    procedure, pass(self), private :: write_geo_strg_data3_rank1_R4P !< Write **StructuredGrid** mesh (data 3, rank 1, R4P).
    procedure, pass(self), private :: write_geo_strg_data3_rank3_R8P !< Write **StructuredGrid** mesh (data 3, rank 3, R8P).
    procedure, pass(self), private :: write_geo_strg_data3_rank3_R4P !< Write **StructuredGrid** mesh (data 3, rank 3, R4P).
    ! dataarray
    procedure, pass(self), private :: write_dataarray_tag          !< Write `<DataArray...>...</DataArray>` tag.
    procedure, pass(self), private :: write_dataarray_tag_appended !< Write `<DataArray.../>` appended tag.
    procedure, pass(self), private :: write_data_location_tag      !< Write `<[/]PointData>` or `<[/]CellData>` open/close tag.
    procedure, pass(self), private :: write_data1_rank1_R8P        !< Write DataArray (data 1, rank 1, R8P).
    procedure, pass(self), private :: write_data1_rank1_R4P        !< Write DataArray (data 1, rank 1, R4P).
    procedure, pass(self), private :: write_data1_rank2_R8P        !< Write DataArray (data 1, rank 2, R8P).
    procedure, pass(self), private :: write_data1_rank2_R4P        !< Write DataArray (data 1, rank 2, R4P).
    procedure, pass(self), private :: write_data1_rank3_R8P        !< Write DataArray (data 1, rank 3, R8P).
    procedure, pass(self), private :: write_data1_rank3_R4P        !< Write DataArray (data 1, rank 3, R4P).
    procedure, pass(self), private :: write_data1_rank4_R8P        !< Write DataArray (data 1, rank 4, R8P).
    procedure, pass(self), private :: write_data1_rank4_R4P        !< Write DataArray (data 1, rank 4, R4P).
    procedure, pass(self), private :: write_data3_rank1_R8P        !< Write DataArray (data 3, rank 1, R8P).
    procedure, pass(self), private :: write_data3_rank1_R4P        !< Write DataArray (data 3, rank 1, R4P).
    procedure, pass(self), private :: write_data3_rank3_R8P        !< Write DataArray (data 3, rank 3, R8P).
    procedure, pass(self), private :: write_data3_rank3_R4P        !< Write DataArray (data 3, rank 3, R4P).
    generic, private      :: write_on_scratch =>           &
                             write_on_scratch_comp1_rank1, &
                             write_on_scratch_comp1_rank2, &
                             write_on_scratch_comp1_rank3, &
                             write_on_scratch_comp1_rank4, &
                             write_on_scratch_comp3_rank1, &
                             write_on_scratch_comp3_rank2, &
                             write_on_scratch_comp3_rank3          !< Write dataarray.
    generic, private      :: encode_ascii =>               &
                             encode_ascii_comp1_rank1_R8P, &
                             encode_ascii_comp1_rank1_R4P, &
                             encode_ascii_comp1_rank2_R8P, &
                             encode_ascii_comp1_rank2_R4P, &
                             encode_ascii_comp1_rank3_R8P, &
                             encode_ascii_comp1_rank3_R4P, &
                             encode_ascii_comp1_rank4_R8P, &
                             encode_ascii_comp1_rank4_R4P, &
                             encode_ascii_comp3_rank1_R8P, &
                             encode_ascii_comp3_rank3_R8P, &
                             encode_ascii_comp3_rank1_R4P, &
                             encode_ascii_comp3_rank3_R4P          !< Encode ascii dataarray.
    generic, private      :: encode_b64 =>               &
                             encode_b64_comp1_rank1_R8P, &
                             encode_b64_comp1_rank1_R4P, &
                             encode_b64_comp1_rank2_R8P, &
                             encode_b64_comp1_rank2_R4P, &
                             encode_b64_comp1_rank3_R8P, &
                             encode_b64_comp1_rank3_R4P, &
                             encode_b64_comp1_rank4_R8P, &
                             encode_b64_comp1_rank4_R4P, &
                             encode_b64_comp3_rank1_R8P, &
                             encode_b64_comp3_rank3_R8P, &
                             encode_b64_comp3_rank1_R4P, &
                             encode_b64_comp3_rank3_R4P            !< Encode Base64 dataarray.
    procedure, pass(self), private :: write_on_scratch_comp1_rank1 !< Write dataarray.
    procedure, pass(self), private :: write_on_scratch_comp1_rank2 !< Write dataarray.
    procedure, pass(self), private :: write_on_scratch_comp1_rank3 !< Write dataarray.
    procedure, pass(self), private :: write_on_scratch_comp1_rank4 !< Write dataarray.
    procedure, pass(self), private :: write_on_scratch_comp3_rank1 !< Write dataarray.
    procedure, pass(self), private :: write_on_scratch_comp3_rank2 !< Write dataarray.
    procedure, pass(self), private :: write_on_scratch_comp3_rank3 !< Write dataarray.
    procedure, pass(self), private :: encode_ascii_comp1_rank1_R8P !< Encode (ascii) dataarray, 1 comp of rank 1 (R8P).
    procedure, pass(self), private :: encode_ascii_comp1_rank1_R4P !< Encode (ascii) dataarray, 1 comp of rank 1 (R4P).
    procedure, pass(self), private :: encode_ascii_comp1_rank2_R8P !< Encode (ascii) dataarray, 1 comp of rank 2 (R8P).
    procedure, pass(self), private :: encode_ascii_comp1_rank2_R4P !< Encode (ascii) dataarray, 1 comp of rank 2 (R4P).
    procedure, pass(self), private :: encode_ascii_comp1_rank3_R8P !< Encode (ascii) dataarray, 1 comp of rank 3 (R8P).
    procedure, pass(self), private :: encode_ascii_comp1_rank3_R4P !< Encode (ascii) dataarray, 1 comp of rank 3 (R4P).
    procedure, pass(self), private :: encode_ascii_comp1_rank4_R8P !< Encode (ascii) dataarray, 1 comp of rank 4 (R8P).
    procedure, pass(self), private :: encode_ascii_comp1_rank4_R4P !< Encode (ascii) dataarray, 1 comp of rank 4 (R4P).
    procedure, pass(self), private :: encode_ascii_comp3_rank1_R8P !< Encode (ascii) dataarray, 3 comp of rank 1 (R8P).
    procedure, pass(self), private :: encode_ascii_comp3_rank1_R4P !< Encode (ascii) dataarray, 3 comp of rank 1 (R8P).
    procedure, pass(self), private :: encode_ascii_comp3_rank3_R8P !< Encode (ascii) dataarray, 3 comp of rank 3 (R4P).
    procedure, pass(self), private :: encode_ascii_comp3_rank3_R4P !< Encode (ascii) dataarray, 3 comp of rank 3 (R4P).
    procedure, nopass,     private :: encode_b64_comp1_rank1_R8P   !< Encode (Base64) dataarray, 1 comp of rank 1 (R8P).
    procedure, nopass,     private :: encode_b64_comp1_rank1_R4P   !< Encode (Base64) dataarray, 1 comp of rank 1 (R4P).
    procedure, nopass,     private :: encode_b64_comp1_rank2_R8P   !< Encode (Base64) dataarray, 1 comp of rank 2 (R8P).
    procedure, nopass,     private :: encode_b64_comp1_rank2_R4P   !< Encode (Base64) dataarray, 1 comp of rank 2 (R4P).
    procedure, nopass,     private :: encode_b64_comp1_rank3_R8P   !< Encode (Base64) dataarray, 1 comp of rank 3 (R8P).
    procedure, nopass,     private :: encode_b64_comp1_rank3_R4P   !< Encode (Base64) dataarray, 1 comp of rank 3 (R4P).
    procedure, nopass,     private :: encode_b64_comp1_rank4_R8P   !< Encode (Base64) dataarray, 1 comp of rank 4 (R8P).
    procedure, nopass,     private :: encode_b64_comp1_rank4_R4P   !< Encode (Base64) dataarray, 1 comp of rank 4 (R4P).
    procedure, nopass,     private :: encode_b64_comp3_rank1_R8P   !< Encode (Base64) dataarray, 3 comp of rank 1 (R8P).
    procedure, nopass,     private :: encode_b64_comp3_rank1_R4P   !< Encode (Base64) dataarray, 3 comp of rank 1 (R8P).
    procedure, nopass,     private :: encode_b64_comp3_rank3_R8P   !< Encode (Base64) dataarray, 3 comp of rank 3 (R4P).
    procedure, nopass,     private :: encode_b64_comp3_rank3_R4P   !< Encode (Base64) dataarray, 3 comp of rank 3 (R4P).
endtype vtk_file
!-----------------------------------------------------------------------------------------------------------------------------------
contains
  ! public methods
  function initialize_write(self, format, filename, mesh_topology, nx1, nx2, ny1, ny2, nz1, nz2) result(error)
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
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  if (.not.is_initialized) call penf_init
  if (.not.is_b64_initialized) call b64_init
  self%topology = trim(adjustl(mesh_topology))
  self%format_ch = trim(adjustl(format))
  self%format_ch = self%format_ch%upper()
  select case(self%format_ch%chars())
  case('ASCII')
    self%format = ascii
    self%format_ch = 'ascii'
  case('RAW')
    self%format = raw
    self%format_ch = 'appended'
    self%ioffset = 0
    call self%open_scratch_file
  case('BINARY-APPENDED')
    self%format = bin_app
    self%format_ch = 'appended'
    self%ioffset = 0
    call self%open_scratch_file
  case('BINARY')
    self%format = binary
    self%format_ch = 'binary'
  endselect
  call self%open_xml_file(filename=filename)
  call self%write_header_tag
  call self%write_topology_tag(nx1=nx1, nx2=nx2, ny1=ny1, ny2=ny2, nz1=nz1, nz2=nz2)
  error = self%error
  return
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction initialize_write

  function finalize_write(self) result(error)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Finalize file (exporter).
  !---------------------------------------------------------------------------------------------------------------------------------
  class(vtk_file), intent(inout) :: self  !< VTK file.
  integer(I4P)                   :: error !< Error status.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  call self%write_end_tag(tag_name=self%topology%chars())
  call self%write_end_tag(tag_name='VTKFile')
  error = self%error
  return
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction finalize_write

  ! private methods
  subroutine open_xml_file(self, filename)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Open XML file.
  !---------------------------------------------------------------------------------------------------------------------------------
  class(vtk_file), intent(inout) :: self     !< VTK file.
  character(*),    intent(in)    :: filename !< File name.
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
  class(vtk_file), intent(inout) :: self !< File handler.
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

  elemental subroutine ioffset_update(self, n_byte)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Update ioffset count.
  !---------------------------------------------------------------------------------------------------------------------------------
  class(vtk_file), intent(inout) :: self   !< VTK file.
  integer(I4P),    intent(in)    :: n_byte !< Number of bytes saved.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  if (self%format==raw) then
    self%ioffset = self%ioffset + BYI4P + n_byte
  else
    self%ioffset = self%ioffset + ((n_byte + BYI4P + 2_I4P)/3_I4P)*4_I4P
  endif
  return
  !---------------------------------------------------------------------------------------------------------------------------------
  endsubroutine ioffset_update

  ! tags
  elemental function self_closing_tag(self, tag_name, tag_attributes) result(tag_)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Return `<tag_name.../>` self closing tag.
  !---------------------------------------------------------------------------------------------------------------------------------
  class(vtk_file), intent(in)           :: self           !< VTK file.
  character(*),    intent(in)           :: tag_name       !< Tag name.
  character(*),    intent(in), optional :: tag_attributes !< Tag attributes.
  type(string)                          :: tag_           !< The tag.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  tag_ = new_line('a')
  if (present(tag_attributes)) then
    tag_ = tag_//repeat(' ', self%indent)//'<'//trim(adjustl(tag_name))//' '//trim(adjustl(tag_attributes))//'/>'//end_rec
  else
    tag_ = tag_//repeat(' ', self%indent)//'<'//trim(adjustl(tag_name))//'/>'//end_rec
  endif
  return
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction self_closing_tag

  elemental function tag(self, tag_name, tag_attributes, tag_content) result(tag_)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Return `<tag_name...>...</tag_name>` tag.
  !---------------------------------------------------------------------------------------------------------------------------------
  class(vtk_file), intent(in)           :: self           !< VTK file.
  character(*),    intent(in)           :: tag_name       !< Tag name.
  character(*),    intent(in), optional :: tag_attributes !< Tag attributes.
  character(*),    intent(in), optional :: tag_content    !< Tag content.
  type(string)                          :: tag_           !< The tag.
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
  class(vtk_file), intent(in)           :: self           !< VTK file.
  character(*),    intent(in)           :: tag_name       !< Tag name.
  character(*),    intent(in), optional :: tag_attributes !< Tag attributes.
  type(string)                          :: tag_           !< The tag.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  tag_ = ''
  if (present(tag_attributes)) then
    tag_ = tag_//repeat(' ', self%indent)//'<'//trim(adjustl(tag_name))//' '//trim(adjustl(tag_attributes))//'>'//end_rec
  else
    tag_ = tag_//repeat(' ', self%indent)//'<'//trim(adjustl(tag_name))//'>'//end_rec
  endif
  return
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction start_tag

  elemental function end_tag(self, tag_name) result(tag_)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Return `</tag_name>` end tag.
  !---------------------------------------------------------------------------------------------------------------------------------
  class(vtk_file), intent(in) :: self     !< VTK file.
  character(*),    intent(in) :: tag_name !< Tag name.
  type(string)                :: tag_     !< The tag.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  tag_ = ''
  tag_ = tag_//repeat(' ', self%indent)//'</'//trim(adjustl(tag_name))//'>'//end_rec
  return
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction end_tag

  subroutine write_self_closing_tag(self, tag_name, tag_attributes)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Write `<tag_name.../>` self closing tag.
  !---------------------------------------------------------------------------------------------------------------------------------
  class(vtk_file), intent(inout)        :: self           !< VTK file.
  character(*),    intent(in)           :: tag_name       !< Tag name.
  character(*),    intent(in), optional :: tag_attributes !< Tag attributes.
  type(string)                          :: tag            !< The tag.
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
  class(vtk_file), intent(inout)        :: self           !< VTK file.
  character(*),    intent(in)           :: tag_name       !< Tag name.
  character(*),    intent(in), optional :: tag_attributes !< Tag attributes.
  character(*),    intent(in), optional :: tag_content    !< Tag content.
  type(string)                          :: tag            !< The tag.
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
  class(vtk_file), intent(inout)        :: self           !< VTK file.
  character(*),    intent(in)           :: tag_name       !< Tag name.
  character(*),    intent(in), optional :: tag_attributes !< Tag attributes.
  type(string)                          :: tag            !< The tag.
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
  class(vtk_file), intent(inout) :: self     !< VTK file.
  character(*),    intent(in)    :: tag_name !< Tag name.
  type(string)                   :: tag      !< The tag.
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
  class(vtk_file), intent(inout) :: self   !< VTK file.
  type(string)                   :: buffer !< Buffer string.
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
  class(vtk_file), intent(inout)         :: self   !< VTK file.
  integer(I4P),    intent(in),  optional :: nx1    !< Initial node of x axis.
  integer(I4P),    intent(in),  optional :: nx2    !< Final node of x axis.
  integer(I4P),    intent(in),  optional :: ny1    !< Initial node of y axis.
  integer(I4P),    intent(in),  optional :: ny2    !< Final node of y axis.
  integer(I4P),    intent(in),  optional :: nz1    !< Initial node of z axis.
  integer(I4P),    intent(in),  optional :: nz2    !< Final node of z axis.
  type(string)                           :: buffer !< Buffer string.
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

  function write_piece_start_tag(self, nx1, nx2, ny1, ny2, nz1, nz2) result(error)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Write `<Piece ...>` start tag.
  !---------------------------------------------------------------------------------------------------------------------------------
  class(vtk_file), intent(inout) :: self           !< VTK file.
  integer(I4P),    intent(in)    :: nx1            !< Initial node of x axis.
  integer(I4P),    intent(in)    :: nx2            !< Final node of x axis.
  integer(I4P),    intent(in)    :: ny1            !< Initial node of y axis.
  integer(I4P),    intent(in)    :: ny2            !< Final node of y axis.
  integer(I4P),    intent(in)    :: nz1            !< Initial node of z axis.
  integer(I4P),    intent(in)    :: nz2            !< Final node of z axis.
  integer(I4P)                   :: error          !< Error status.
  type(string)                   :: tag_attributes !< Tag attributes.
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

  function write_piece_end_tag(self) result(error)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Write `</Piece>` end tag.
  !---------------------------------------------------------------------------------------------------------------------------------
  class(vtk_file), intent(inout) :: self  !< VTK file.
  integer(I4P)                   :: error !< Error status.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  call self%write_end_tag(tag_name='Piece')
  error = self%error
  return
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction write_piece_end_tag

  ! geo
  function write_geo_strg_data1_rank2_R8P(self, nx1, nx2, ny1, ny2, nz1, nz2, xyz) result(error)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Write mesh with **StructuredGrid** topology (data 1, rank 2, R8P).
  !---------------------------------------------------------------------------------------------------------------------------------
  class(vtk_file), intent(inout) :: self       !< VTK file.
  integer(I4P),    intent(in)    :: nx1        !< Initial node of x axis.
  integer(I4P),    intent(in)    :: nx2        !< Final node of x axis.
  integer(I4P),    intent(in)    :: ny1        !< Initial node of y axis.
  integer(I4P),    intent(in)    :: ny2        !< Final node of y axis.
  integer(I4P),    intent(in)    :: nz1        !< Initial node of z axis.
  integer(I4P),    intent(in)    :: nz2        !< Final node of z axis.
  real(R8P),       intent(in)    :: xyz(1:,1:) !< X, y, z coordinates [1:3,:].
  integer(I4P)                   :: error      !< Error status.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  call self%write_start_tag(tag_name='Points')
  error = self%write_data(data_name='Points', x=xyz)
  call self%write_end_tag(tag_name='Points')
  error = self%error
  return
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction write_geo_strg_data1_rank2_R8P

  function write_geo_strg_data1_rank2_R4P(self, nx1, nx2, ny1, ny2, nz1, nz2, xyz) result(error)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Write mesh with **StructuredGrid** topology (data 1, rank 2, R4P).
  !---------------------------------------------------------------------------------------------------------------------------------
  class(vtk_file), intent(inout) :: self       !< VTK file.
  integer(I4P),    intent(in)    :: nx1        !< Initial node of x axis.
  integer(I4P),    intent(in)    :: nx2        !< Final node of x axis.
  integer(I4P),    intent(in)    :: ny1        !< Initial node of y axis.
  integer(I4P),    intent(in)    :: ny2        !< Final node of y axis.
  integer(I4P),    intent(in)    :: nz1        !< Initial node of z axis.
  integer(I4P),    intent(in)    :: nz2        !< Final node of z axis.
  real(R4P),       intent(in)    :: xyz(1:,1:) !< X, y, z coordinates [1:3,:].
  integer(I4P)                   :: error      !< Error status.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  call self%write_start_tag(tag_name='Points')
  error = self%write_data(data_name='Points', x=xyz)
  call self%write_end_tag(tag_name='Points')
  error = self%error
  return
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction write_geo_strg_data1_rank2_R4P

  function write_geo_strg_data1_rank4_R8P(self, nx1, nx2, ny1, ny2, nz1, nz2, xyz) result(error)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Write mesh with **StructuredGrid** topology (data 1, rank 4, R8P).
  !---------------------------------------------------------------------------------------------------------------------------------
  class(vtk_file), intent(inout) :: self             !< VTK file.
  integer(I4P),    intent(in)    :: nx1              !< Initial node of x axis.
  integer(I4P),    intent(in)    :: nx2              !< Final node of x axis.
  integer(I4P),    intent(in)    :: ny1              !< Initial node of y axis.
  integer(I4P),    intent(in)    :: ny2              !< Final node of y axis.
  integer(I4P),    intent(in)    :: nz1              !< Initial node of z axis.
  integer(I4P),    intent(in)    :: nz2              !< Final node of z axis.
  real(R8P),       intent(in)    :: xyz(1:,1:,1:,1:) !< X, y, z coordinates [1:3,:,:,:].
  integer(I4P)                   :: error            !< Error status.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  call self%write_start_tag(tag_name='Points')
  error = self%write_data(data_name='Points', x=xyz)
  call self%write_end_tag(tag_name='Points')
  error = self%error
  return
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction write_geo_strg_data1_rank4_R8P

  function write_geo_strg_data1_rank4_R4P(self, nx1, nx2, ny1, ny2, nz1, nz2, xyz) result(error)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Write mesh with **StructuredGrid** topology (data 1, rank 4, R4P).
  !---------------------------------------------------------------------------------------------------------------------------------
  class(vtk_file), intent(inout) :: self             !< VTK file.
  integer(I4P),    intent(in)    :: nx1              !< Initial node of x axis.
  integer(I4P),    intent(in)    :: nx2              !< Final node of x axis.
  integer(I4P),    intent(in)    :: ny1              !< Initial node of y axis.
  integer(I4P),    intent(in)    :: ny2              !< Final node of y axis.
  integer(I4P),    intent(in)    :: nz1              !< Initial node of z axis.
  integer(I4P),    intent(in)    :: nz2              !< Final node of z axis.
  real(R4P),       intent(in)    :: xyz(1:,1:,1:,1:) !< X, y, z coordinates [1:3,:,:,:].
  integer(I4P)                   :: error            !< Error status.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  call self%write_start_tag(tag_name='Points')
  error = self%write_data(data_name='Points', x=xyz)
  call self%write_end_tag(tag_name='Points')
  error = self%error
  return
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction write_geo_strg_data1_rank4_R4P

  function write_geo_strg_data3_rank1_R8P(self, nx1, nx2, ny1, ny2, nz1, nz2, x, y, z) result(error)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Write mesh with **StructuredGrid** topology (data 3, rank 1, R8P).
  !---------------------------------------------------------------------------------------------------------------------------------
  class(vtk_file), intent(inout) :: self     !< VTK file.
  integer(I4P),    intent(in)    :: nx1      !< Initial node of x axis.
  integer(I4P),    intent(in)    :: nx2      !< Final node of x axis.
  integer(I4P),    intent(in)    :: ny1      !< Initial node of y axis.
  integer(I4P),    intent(in)    :: ny2      !< Final node of y axis.
  integer(I4P),    intent(in)    :: nz1      !< Initial node of z axis.
  integer(I4P),    intent(in)    :: nz2      !< Final node of z axis.
  real(R8P),       intent(in)    :: x(1:)    !< X coordinates.
  real(R8P),       intent(in)    :: y(1:)    !< Y coordinates.
  real(R8P),       intent(in)    :: z(1:)    !< Z coordinates.
  integer(I4P)                   :: error    !< Error status.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  call self%write_start_tag(tag_name='Points')
  error = self%write_data(data_name='Points', x=x, y=y, z=z)
  call self%write_end_tag(tag_name='Points')
  error = self%error
  return
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction write_geo_strg_data3_rank1_R8P

  function write_geo_strg_data3_rank1_R4P(self, nx1, nx2, ny1, ny2, nz1, nz2, x, y, z) result(error)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Write mesh with **StructuredGrid** topology (data 3, rank 1, R4P).
  !---------------------------------------------------------------------------------------------------------------------------------
  class(vtk_file), intent(inout) :: self     !< VTK file.
  integer(I4P),    intent(in)    :: nx1      !< Initial node of x axis.
  integer(I4P),    intent(in)    :: nx2      !< Final node of x axis.
  integer(I4P),    intent(in)    :: ny1      !< Initial node of y axis.
  integer(I4P),    intent(in)    :: ny2      !< Final node of y axis.
  integer(I4P),    intent(in)    :: nz1      !< Initial node of z axis.
  integer(I4P),    intent(in)    :: nz2      !< Final node of z axis.
  real(R4P),       intent(in)    :: x(1:)    !< X coordinates.
  real(R4P),       intent(in)    :: y(1:)    !< Y coordinates.
  real(R4P),       intent(in)    :: z(1:)    !< Z coordinates.
  integer(I4P)                   :: error    !< Error status.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  call self%write_start_tag(tag_name='Points')
  error = self%write_data(data_name='Points', x=x, y=y, z=z)
  call self%write_end_tag(tag_name='Points')
  error = self%error
  return
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction write_geo_strg_data3_rank1_R4P

  function write_geo_strg_data3_rank3_R8P(self, nx1, nx2, ny1, ny2, nz1, nz2, x, y, z) result(error)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Write mesh with **StructuredGrid** topology (data 3, rank 3, R8P).
  !---------------------------------------------------------------------------------------------------------------------------------
  class(vtk_file), intent(inout) :: self        !< VTK file.
  integer(I4P),    intent(in)    :: nx1         !< Initial node of x axis.
  integer(I4P),    intent(in)    :: nx2         !< Final node of x axis.
  integer(I4P),    intent(in)    :: ny1         !< Initial node of y axis.
  integer(I4P),    intent(in)    :: ny2         !< Final node of y axis.
  integer(I4P),    intent(in)    :: nz1         !< Initial node of z axis.
  integer(I4P),    intent(in)    :: nz2         !< Final node of z axis.
  real(R8P),       intent(in)    :: x(1:,1:,1:) !< X coordinates.
  real(R8P),       intent(in)    :: y(1:,1:,1:) !< Y coordinates.
  real(R8P),       intent(in)    :: z(1:,1:,1:) !< Z coordinates.
  integer(I4P)                   :: error       !< Error status.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  call self%write_start_tag(tag_name='Points')
  error = self%write_data(data_name='Points', x=x, y=y, z=z)
  call self%write_end_tag(tag_name='Points')
  error = self%error
  return
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction write_geo_strg_data3_rank3_R8P

  function write_geo_strg_data3_rank3_R4P(self, nx1, nx2, ny1, ny2, nz1, nz2, x, y, z) result(error)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Write mesh with **StructuredGrid** topology (data 3, rank 3, R4P).
  !---------------------------------------------------------------------------------------------------------------------------------
  class(vtk_file), intent(inout) :: self        !< VTK file.
  integer(I4P),    intent(in)    :: nx1         !< Initial node of x axis.
  integer(I4P),    intent(in)    :: nx2         !< Final node of x axis.
  integer(I4P),    intent(in)    :: ny1         !< Initial node of y axis.
  integer(I4P),    intent(in)    :: ny2         !< Final node of y axis.
  integer(I4P),    intent(in)    :: nz1         !< Initial node of z axis.
  integer(I4P),    intent(in)    :: nz2         !< Final node of z axis.
  real(R4P),       intent(in)    :: x(1:,1:,1:) !< X coordinates.
  real(R4P),       intent(in)    :: y(1:,1:,1:) !< Y coordinates.
  real(R4P),       intent(in)    :: z(1:,1:,1:) !< Z coordinates.
  integer(I4P)                   :: error       !< Error status.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  call self%write_start_tag(tag_name='Points')
  error = self%write_data(data_name='Points', x=x, y=y, z=z)
  call self%write_end_tag(tag_name='Points')
  error = self%error
  return
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction write_geo_strg_data3_rank3_R4P

  ! dataarray
  ! write
  function write_data_location_tag(self, location, action) result(error)
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
  endfunction write_data_location_tag

  subroutine write_dataarray_tag(self, data_type, number_of_components, data_name, data_content)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Write `<DataArray...>...</DataArray>` tag.
  !---------------------------------------------------------------------------------------------------------------------------------
  class(vtk_file), intent(inout)        :: self                 !< VTK file.
  character(*),    intent(in)           :: data_type            !< Type of dataarray.
  integer(I4P),    intent(in)           :: number_of_components !< Number of dataarray components.
  character(*),    intent(in)           :: data_name            !< Data name.
  character(*),    intent(in), optional :: data_content         !< Data content.
  type(string)                          :: tag_attributes       !< Tag attributes.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  tag_attributes = 'type="'//trim(adjustl(data_type))//                 &
    '" NumberOfComponents="'//trim(str(number_of_components, .true.))// &
    '" Name="'//trim(adjustl(data_name))//                              &
    '" format="'//self%format_ch//'"'
  call self%write_tag(tag_name='DataArray', tag_attributes=tag_attributes%chars(), tag_content=data_content)
  return
  !---------------------------------------------------------------------------------------------------------------------------------
  endsubroutine write_dataarray_tag

  subroutine write_dataarray_tag_appended(self, data_type, number_of_components, data_name)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Write `<DataArray.../>` tag.
  !---------------------------------------------------------------------------------------------------------------------------------
  class(vtk_file), intent(inout) :: self                 !< VTK file.
  character(*),    intent(in)    :: data_type            !< Type of dataarray.
  integer(I4P),    intent(in)    :: number_of_components !< Number of dataarray components.
  character(*),    intent(in)    :: data_name            !< Data name.
  type(string)                   :: tag_attributes       !< Tag attributes.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  tag_attributes = 'type="'//trim(adjustl(data_type))//                 &
    '" NumberOfComponents="'//trim(str(number_of_components, .true.))// &
    '" Name="'//trim(adjustl(data_name))//                              &
    '" format="'//self%format_ch//'"'//                                 &
    '" offset="'//trim(str(self%ioffset, .true.))//'"'
  call self%write_self_closing_tag(tag_name='DataArray', tag_attributes=tag_attributes%chars())
  return
  !---------------------------------------------------------------------------------------------------------------------------------
  endsubroutine write_dataarray_tag_appended

  function write_data1_rank1_R8P(self, data_name, x) result(error)
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
    code = self%encode_ascii(x=x)
    call self%write_dataarray_tag(data_type=data_type, number_of_components=n_components, data_name=data_name, data_content=code)
  case(raw, bin_app)
    call self%write_dataarray_tag_appended(data_type=data_type, number_of_components=n_components, data_name=data_name)
    call self%ioffset_update(n_byte=self%write_on_scratch(x=x))
  case(binary)
    code = self%encode_b64(x=x)
    call self%write_dataarray_tag(data_type=data_type, number_of_components=n_components, data_name=data_name, data_content=code)
  endselect
  error = self%error
  return
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction write_data1_rank1_R8P

  function write_data1_rank1_R4P(self, data_name, x) result(error)
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
    code = self%encode_ascii(x=x)
    call self%write_dataarray_tag(data_type=data_type, number_of_components=n_components, data_name=data_name, data_content=code)
  case(raw, bin_app)
    call self%write_dataarray_tag_appended(data_type=data_type, number_of_components=n_components, data_name=data_name)
    call self%ioffset_update(n_byte=self%write_on_scratch(x=x))
  case(binary)
    code = self%encode_b64(x=x)
    call self%write_dataarray_tag(data_type=data_type, number_of_components=n_components, data_name=data_name, data_content=code)
  endselect
  error = self%error
  return
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction write_data1_rank1_R4P

  function write_data1_rank2_R8P(self, data_name, x, one_component) result(error)
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
    code = self%encode_ascii(x=x)
    call self%write_dataarray_tag(data_type=data_type, number_of_components=n_components, data_name=data_name, data_content=code)
  case(raw, bin_app)
    call self%write_dataarray_tag_appended(data_type=data_type, number_of_components=n_components, data_name=data_name)
    call self%ioffset_update(n_byte=self%write_on_scratch(x=x))
  case(binary)
    code = self%encode_b64(x=x)
    call self%write_dataarray_tag(data_type=data_type, number_of_components=n_components, data_name=data_name, data_content=code)
  endselect
  error = self%error
  return
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction write_data1_rank2_R8P

  function write_data1_rank2_R4P(self, data_name, x, one_component) result(error)
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
    code = self%encode_ascii(x=x)
    call self%write_dataarray_tag(data_type=data_type, number_of_components=n_components, data_name=data_name, data_content=code)
  case(raw, bin_app)
    call self%write_dataarray_tag_appended(data_type=data_type, number_of_components=n_components, data_name=data_name)
    call self%ioffset_update(n_byte=self%write_on_scratch(x=x))
  case(binary)
    code = self%encode_b64(x=x)
    call self%write_dataarray_tag(data_type=data_type, number_of_components=n_components, data_name=data_name, data_content=code)
  endselect
  error = self%error
  return
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction write_data1_rank2_R4P

  function write_data1_rank3_R8P(self, data_name, x, one_component) result(error)
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
    code = self%encode_ascii(x=x)
    call self%write_dataarray_tag(data_type=data_type, number_of_components=n_components, data_name=data_name, data_content=code)
  case(raw, bin_app)
    call self%write_dataarray_tag_appended(data_type=data_type, number_of_components=n_components, data_name=data_name)
    call self%ioffset_update(n_byte=self%write_on_scratch(x=x))
  case(binary)
    code = self%encode_b64(x=x)
    call self%write_dataarray_tag(data_type=data_type, number_of_components=n_components, data_name=data_name, data_content=code)
  endselect
  error = self%error
  return
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction write_data1_rank3_R8P

  function write_data1_rank3_R4P(self, data_name, x, one_component) result(error)
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
    code = self%encode_ascii(x=x)
    call self%write_dataarray_tag(data_type=data_type, number_of_components=n_components, data_name=data_name, data_content=code)
  case(raw, bin_app)
    call self%write_dataarray_tag_appended(data_type=data_type, number_of_components=n_components, data_name=data_name)
    call self%ioffset_update(n_byte=self%write_on_scratch(x=x))
  case(binary)
    code = self%encode_b64(x=x)
    call self%write_dataarray_tag(data_type=data_type, number_of_components=n_components, data_name=data_name, data_content=code)
  endselect
  error = self%error
  return
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction write_data1_rank3_R4P

  function write_data1_rank4_R8P(self, data_name, x, one_component) result(error)
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
    code = self%encode_ascii(x=x)
    call self%write_dataarray_tag(data_type=data_type, number_of_components=n_components, data_name=data_name, data_content=code)
  case(raw, bin_app)
    call self%write_dataarray_tag_appended(data_type=data_type, number_of_components=n_components, data_name=data_name)
    call self%ioffset_update(n_byte=self%write_on_scratch(x=x))
  case(binary)
    code = self%encode_b64(x=x)
    call self%write_dataarray_tag(data_type=data_type, number_of_components=n_components, data_name=data_name, data_content=code)
  endselect
  error = self%error
  return
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction write_data1_rank4_R8P

  function write_data1_rank4_R4P(self, data_name, x, one_component) result(error)
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
    code = self%encode_ascii(x=x)
    call self%write_dataarray_tag(data_type=data_type, number_of_components=n_components, data_name=data_name, data_content=code)
  case(raw, bin_app)
    call self%write_dataarray_tag_appended(data_type=data_type, number_of_components=n_components, data_name=data_name)
    call self%ioffset_update(n_byte=self%write_on_scratch(x=x))
  case(binary)
    code = self%encode_b64(x=x)
    call self%write_dataarray_tag(data_type=data_type, number_of_components=n_components, data_name=data_name, data_content=code)
  endselect
  error = self%error
  return
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction write_data1_rank4_R4P

  function write_data3_rank1_R8P(self, data_name, x, y, z) result(error)
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
    code = self%encode_ascii(x=x, y=y, z=z)
    call self%write_dataarray_tag(data_type=data_type, number_of_components=n_components, data_name=data_name, data_content=code)
  case(raw, bin_app)
    call self%write_dataarray_tag_appended(data_type=data_type, number_of_components=n_components, data_name=data_name)
    call self%ioffset_update(n_byte=self%write_on_scratch(x=x, y=y, z=z))
  case(binary)
    code = self%encode_b64(x=x, y=y, z=z)
    call self%write_dataarray_tag(data_type=data_type, number_of_components=n_components, data_name=data_name, data_content=code)
  endselect
  error = self%error
  return
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction write_data3_rank1_R8P

  function write_data3_rank1_R4P(self, data_name, x, y, z) result(error)
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
    code = self%encode_ascii(x=x, y=y, z=z)
    call self%write_dataarray_tag(data_type=data_type, number_of_components=n_components, data_name=data_name, data_content=code)
  case(raw, bin_app)
    call self%write_dataarray_tag_appended(data_type=data_type, number_of_components=n_components, data_name=data_name)
    call self%ioffset_update(n_byte=self%write_on_scratch(x=x, y=y, z=z))
  case(binary)
    code = self%encode_b64(x=x, y=y, z=z)
    call self%write_dataarray_tag(data_type=data_type, number_of_components=n_components, data_name=data_name, data_content=code)
  endselect
  error = self%error
  return
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction write_data3_rank1_R4P

  function write_data3_rank3_R8P(self, data_name, x, y, z) result(error)
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
    code = self%encode_ascii(x=x, y=y, z=z)
    call self%write_dataarray_tag(data_type=data_type, number_of_components=n_components, data_name=data_name, data_content=code)
  case(raw, bin_app)
    call self%write_dataarray_tag_appended(data_type=data_type, number_of_components=n_components, data_name=data_name)
    call self%ioffset_update(n_byte=self%write_on_scratch(x=x, y=y, z=z))
  case(binary)
    code = self%encode_b64(x=x, y=y, z=z)
    call self%write_dataarray_tag(data_type=data_type, number_of_components=n_components, data_name=data_name, data_content=code)
  endselect
  error = self%error
  return
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction write_data3_rank3_R8P

  function write_data3_rank3_R4P(self, data_name, x, y, z) result(error)
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
    code = self%encode_ascii(x=x, y=y, z=z)
    call self%write_dataarray_tag(data_type=data_type, number_of_components=n_components, data_name=data_name, data_content=code)
  case(raw, bin_app)
    call self%write_dataarray_tag_appended(data_type=data_type, number_of_components=n_components, data_name=data_name)
    call self%ioffset_update(n_byte=self%write_on_scratch(x=x, y=y, z=z))
  case(binary)
    code = self%encode_b64(x=x, y=y, z=z)
    call self%write_dataarray_tag(data_type=data_type, number_of_components=n_components, data_name=data_name, data_content=code)
  endselect
  error = self%error
  return
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction write_data3_rank3_R4P

  ! write on scratch
  function write_on_scratch_comp1_rank1(self, x) result(n_byte)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Write a dataarray with 1 components of rank 1.
  !---------------------------------------------------------------------------------------------------------------------------------
  class(vtk_file), intent(inout) :: self   !< VTK file.
  class(*),        intent(in)    :: x(1:)  !< Data variable.
  integer(I4P)                   :: n_byte !< Number of bytes
  integer(I4P)                   :: nn     !< Number of elements.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  nn = size(x, dim=1)
  select type(x)
  type is(real(R8P))
    n_byte = nn*BYR8P
    write(unit=self%scratch, iostat=self%error)n_byte, 'R8', nn
    write(unit=self%scratch, iostat=self%error)x
  type is(real(R4P))
    n_byte = nn*BYR4P
    write(unit=self%scratch, iostat=self%error)n_byte, 'R4', nn
    write(unit=self%scratch, iostat=self%error)x
  endselect
  return
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction write_on_scratch_comp1_rank1

  function write_on_scratch_comp1_rank2(self, x) result(n_byte)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Write a dataarray with 1 components of rank 2.
  !---------------------------------------------------------------------------------------------------------------------------------
  class(vtk_file), intent(inout) :: self     !< VTK file.
  class(*),        intent(in)    :: x(1:,1:) !< Data variable.
  integer(I4P)                   :: n_byte   !< Number of bytes
  integer(I4P)                   :: nn       !< Number of elements.
  !------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  nn = size(x, dim=1)*size(x, dim=2)
  select type(x)
  type is(real(R8P))
    n_byte = nn*BYR8P
    write(unit=self%scratch, iostat=self%error)n_byte, 'R8', nn
    write(unit=self%scratch, iostat=self%error)x
  type is(real(R4P))
    n_byte = nn*BYR4P
    write(unit=self%scratch, iostat=self%error)n_byte, 'R4', nn
    write(unit=self%scratch, iostat=self%error)x
  endselect
  return
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction write_on_scratch_comp1_rank2

  function write_on_scratch_comp1_rank3(self, x) result(n_byte)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Write a dataarray with 1 components of rank 3.
  !---------------------------------------------------------------------------------------------------------------------------------
  class(vtk_file), intent(inout) :: self        !< VTK file.
  class(*),        intent(in)    :: x(1:,1:,1:) !< Data variable.
  integer(I4P)                   :: n_byte      !< Number of bytes
  integer(I4P)                   :: nn          !< Number of elements.
  !------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  nn = size(x, dim=1)*size(x, dim=2)*size(x, dim=3)
  select type(x)
  type is(real(R8P))
    n_byte = nn*BYR8P
    write(unit=self%scratch, iostat=self%error)n_byte, 'R8', nn
    write(unit=self%scratch, iostat=self%error)x
  type is(real(R4P))
    n_byte = nn*BYR4P
    write(unit=self%scratch, iostat=self%error)n_byte, 'R4', nn
    write(unit=self%scratch, iostat=self%error)x
  endselect
  return
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction write_on_scratch_comp1_rank3

  function write_on_scratch_comp1_rank4(self, x) result(n_byte)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Write a dataarray with 1 components of rank 4.
  !---------------------------------------------------------------------------------------------------------------------------------
  class(vtk_file), intent(inout) :: self           !< VTK file.
  class(*),        intent(in)    :: x(1:,1:,1:,1:) !< Data variable.
  integer(I4P)                   :: n_byte         !< Number of bytes
  integer(I4P)                   :: nn             !< Number of elements.
  !------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  nn = size(x, dim=1)*size(x, dim=2)*size(x, dim=3)*size(x, dim=4)
  select type(x)
  type is(real(R8P))
    n_byte = nn*BYR8P
    write(unit=self%scratch, iostat=self%error)n_byte, 'R8', nn
    write(unit=self%scratch, iostat=self%error)x
  type is(real(R4P))
    n_byte = nn*BYR4P
    write(unit=self%scratch, iostat=self%error)n_byte, 'R4', nn
    write(unit=self%scratch, iostat=self%error)x
  endselect
  return
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction write_on_scratch_comp1_rank4

  function write_on_scratch_comp3_rank1(self, x, y, z) result(n_byte)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Write a dataarray with 3 components of rank 1.
  !---------------------------------------------------------------------------------------------------------------------------------
  class(vtk_file), intent(inout) :: self   !< VTK file.
  class(*),        intent(in)    :: x(1:)  !< X component.
  class(*),        intent(in)    :: y(1:)  !< Y component.
  class(*),        intent(in)    :: z(1:)  !< Z component.
  integer(I4P)                   :: n_byte !< Number of bytes
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  n_byte = self%write_on_scratch(x=x) + self%write_on_scratch(x=y) + self%write_on_scratch(x=z)
  return
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction write_on_scratch_comp3_rank1

  function write_on_scratch_comp3_rank2(self, x, y, z) result(n_byte)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Write a dataarray with 3 components of rank 2.
  !---------------------------------------------------------------------------------------------------------------------------------
  class(vtk_file), intent(inout) :: self     !< VTK file.
  class(*),        intent(in)    :: x(1:,1:) !< X component.
  class(*),        intent(in)    :: y(1:,1:) !< Y component.
  class(*),        intent(in)    :: z(1:,1:) !< Z component.
  integer(I4P)                   :: n_byte   !< Number of bytes
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  n_byte = self%write_on_scratch(x=x) + self%write_on_scratch(x=y) + self%write_on_scratch(x=z)
  return
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction write_on_scratch_comp3_rank2

  function write_on_scratch_comp3_rank3(self, x, y, z) result(n_byte)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Write a dataarray with 3 components of rank 3.
  !---------------------------------------------------------------------------------------------------------------------------------
  class(vtk_file), intent(inout) :: self        !< VTK file.
  class(*),        intent(in)    :: x(1:,1:,1:) !< X component.
  class(*),        intent(in)    :: y(1:,1:,1:) !< Y component.
  class(*),        intent(in)    :: z(1:,1:,1:) !< Z component.
  integer(I4P)                   :: n_byte      !< Number of bytes
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  n_byte = self%write_on_scratch(x=x) + self%write_on_scratch(x=y) + self%write_on_scratch(x=z)
  return
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction write_on_scratch_comp3_rank3

  ! encode ascii
  function encode_ascii_comp1_rank1_R8P(self, x) result(code)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Encode (Base64) a dataarray with 1 components of rank 1 (R8P).
  !---------------------------------------------------------------------------------------------------------------------------------
  class(vtk_file), intent(in)   :: self  !< VTK file.
  real(R8P),       intent(in)   :: x(1:) !< Data variable.
  character(len=:), allocatable :: code  !< Encoded base64 dataarray.
  integer(I4P)                  :: n     !< Counter.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  code = ''
  do n=1, size(x, dim=1)
    code = code//new_line('a')//repeat(' ', self%indent)//str(n=x(n))//end_rec
  enddo
  return
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction encode_ascii_comp1_rank1_R8P

  function encode_ascii_comp1_rank1_R4P(self, x) result(code)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Encode (Base64) a dataarray with 1 components of rank 1 (R4P).
  !---------------------------------------------------------------------------------------------------------------------------------
  class(vtk_file), intent(in)   :: self  !< VTK file.
  real(R4P),       intent(in)   :: x(1:) !< Data variable.
  character(len=:), allocatable :: code  !< Encoded base64 dataarray.
  integer(I4P)                  :: n     !< Counter.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  code = ''
  do n=1, size(x, dim=1)
    code = code//new_line('a')//repeat(' ', self%indent)//str(n=x(n))//end_rec
  enddo
  return
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction encode_ascii_comp1_rank1_R4P

  function encode_ascii_comp1_rank2_R8P(self, x) result(code)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Encode (Base64) a dataarray with 1 components of rank 2 (R8P).
  !---------------------------------------------------------------------------------------------------------------------------------
  class(vtk_file), intent(in)   :: self     !< VTK file.
  real(R8P),       intent(in)   :: x(1:,1:) !< Data variable
  character(len=:), allocatable :: code     !< Encoded base64 dataarray.
  integer(I4P)                  :: n1       !< Counter.
  integer(I4P)                  :: n2       !< Counter.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  code = ''
  do n2=1, size(x, dim=2)
    code = code//new_line('a')//repeat(' ', self%indent)
    do n1=1, size(x, dim=1)-1
      code = code//str(n=x(n1, n2))//' '
    enddo
    code = code//' '//str(n=x(size(x, dim=1), n2))//end_rec
  enddo
  return
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction encode_ascii_comp1_rank2_R8P

  function encode_ascii_comp1_rank2_R4P(self, x) result(code)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Encode (Base64) a dataarray with 1 components of rank 2 (R4P).
  !---------------------------------------------------------------------------------------------------------------------------------
  class(vtk_file), intent(in)   :: self     !< VTK file.
  real(R4P),       intent(in)   :: x(1:,1:) !< Data variable
  character(len=:), allocatable :: code     !< Encoded base64 dataarray.
  integer(I4P)                  :: n1       !< Counter.
  integer(I4P)                  :: n2       !< Counter.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  code = ''
  do n2=1, size(x, dim=2)
    code = code//new_line('a')//repeat(' ', self%indent)
    do n1=1, size(x, dim=1)
      code = code//str(n=x(n1, n2))//' '
    enddo
    code = code//end_rec
  enddo
  return
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction encode_ascii_comp1_rank2_R4P

  function encode_ascii_comp1_rank3_R8P(self, x) result(code)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Encode (Base64) a dataarray with 1 components of rank 3 (R8P).
  !---------------------------------------------------------------------------------------------------------------------------------
  class(vtk_file), intent(in)   :: self        !< VTK file.
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
      code = code//new_line('a')//repeat(' ', self%indent)
      do n1=1, size(x, dim=1)-1
        code = code//str(n=x(n1, n2, n3))//' '
      enddo
      code = code//' '//str(n=x(size(x, dim=1), n2, n3))//end_rec
    enddo
  enddo
  return
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction encode_ascii_comp1_rank3_R8P

  function encode_ascii_comp1_rank3_R4P(self, x) result(code)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Encode (Base64) a dataarray with 1 components of rank 3 (R4P).
  !---------------------------------------------------------------------------------------------------------------------------------
  class(vtk_file), intent(in)   :: self        !< VTK file.
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
      code = code//new_line('a')//repeat(' ', self%indent)
      do n1=1, size(x, dim=1)-1
        code = code//str(n=x(n1, n2, n3))//' '
      enddo
      code = code//' '//str(n=x(size(x, dim=1), n2, n3))//end_rec
    enddo
  enddo
  return
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction encode_ascii_comp1_rank3_R4P

  function encode_ascii_comp1_rank4_R8P(self, x) result(code)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Encode (Base64) a dataarray with 1 components of rank 4 (R8P).
  !---------------------------------------------------------------------------------------------------------------------------------
  class(vtk_file), intent(in)   :: self           !< VTK file.
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
        code = code//new_line('a')//repeat(' ', self%indent)
        do n1=1, size(x, dim=1)
          code = code//str(n=x(n1,n2,n3,n4))//' '
        enddo
        code = code//end_rec
      enddo
    enddo
  enddo
  return
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction encode_ascii_comp1_rank4_R8P

  function encode_ascii_comp1_rank4_R4P(self, x) result(code)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Encode (Base64) a dataarray with 1 components of rank 4 (R4P).
  !---------------------------------------------------------------------------------------------------------------------------------
  class(vtk_file), intent(in)   :: self           !< VTK file.
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
        code = code//new_line('a')//repeat(' ', self%indent)
        do n1=1, size(x, dim=1)
          code = code//str(n=x(n1, n2, n3, n4))//' '
        enddo
        code = code//end_rec
      enddo
    enddo
  enddo
  return
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction encode_ascii_comp1_rank4_R4P

  function encode_ascii_comp3_rank1_R8P(self, x, y, z) result(code)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Encode (Base64) a dataarray with 3 components of rank 1 (R8P).
  !---------------------------------------------------------------------------------------------------------------------------------
  class(vtk_file), intent(in)   :: self  !< VTK file.
  real(R8P),       intent(in)   :: x(1:) !< X component.
  real(R8P),       intent(in)   :: y(1:) !< Y component.
  real(R8P),       intent(in)   :: z(1:) !< Z component.
  character(len=:), allocatable :: code  !< Encoded base64 dataarray.
  integer(I4P)                  :: n     !< Counter.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  code = ''
  do n=1, size(x, dim=1)
    code = code//new_line('a')//repeat(' ', self%indent)//str(n=x(n))//' '//str(n=y(n))//' '//str(n=z(n))//end_rec
  enddo
  return
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction encode_ascii_comp3_rank1_R8P

  function encode_ascii_comp3_rank1_R4P(self, x, y, z) result(code)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Encode (Base64) a dataarray with 3 components of rank 1 (R4P).
  !---------------------------------------------------------------------------------------------------------------------------------
  class(vtk_file), intent(in)   :: self  !< VTK file.
  real(R4P),       intent(in)   :: x(1:) !< X component.
  real(R4P),       intent(in)   :: y(1:) !< Y component.
  real(R4P),       intent(in)   :: z(1:) !< Z component.
  character(len=:), allocatable :: code  !< Encoded base64 dataarray.
  integer(I4P)                  :: n     !< Counter.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  code = ''
  do n=1, size(x, dim=1)
    code = code//new_line('a')//repeat(' ', self%indent)//str(n=x(n))//' '//str(n=y(n))//' '//str(n=z(n))//end_rec
  enddo
  return
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction encode_ascii_comp3_rank1_R4P

  function encode_ascii_comp3_rank3_R8P(self, x, y, z) result(code)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Encode (Base64) a dataarray with 3 components of rank 3 (R8P).
  !---------------------------------------------------------------------------------------------------------------------------------
  class(vtk_file), intent(in)   :: self        !< VTK file.
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
        code = code//new_line('a')//repeat(' ', self%indent)//&
          str(n=x(n1, n2, n3))//' '//str(n=y(n1, n2, n3))//' '//str(n=z(n1, n2, n3))//end_rec
      enddo
    enddo
  enddo
  return
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction encode_ascii_comp3_rank3_R8P

  function encode_ascii_comp3_rank3_R4P(self, x, y, z) result(code)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Encode (Base64) a dataarray with 3 components of rank 3 (R4P).
  !---------------------------------------------------------------------------------------------------------------------------------
  class(vtk_file), intent(in)   :: self        !< VTK file.
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
        code = code//new_line('a')//repeat(' ', self%indent)//&
          str(n=x(n1, n2, n3))//' '//str(n=y(n1, n2, n3))//' '//str(n=z(n1, n2, n3))//end_rec
      enddo
    enddo
  enddo
  return
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction encode_ascii_comp3_rank3_R4P

  ! encode Base64
  function encode_b64_comp1_rank1_R8P(x) result(code)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Encode (Base64) a dataarray with 1 components of rank 1 (R8P).
  !---------------------------------------------------------------------------------------------------------------------------------
  real(R8P), intent(in)         :: x(1:) !< Data variable.
  character(len=:), allocatable :: code  !< Encoded base64 dataarray.
  integer(I1P),     allocatable :: xp(:) !< Packed data.
  integer(I4P)                  :: nn    !< Number of elements.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  nn = size(x, dim=1)
  call pack_data(a1=[int(nn*BYR8P, I4P)], a2=reshape(x, [nn]), packed=xp)
  call b64_encode(n=xp, code=code)
  return
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction encode_b64_comp1_rank1_R8P

  function encode_b64_comp1_rank1_R4P(x) result(code)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Encode (Base64) a dataarray with 1 components of rank 1 (R4P).
  !---------------------------------------------------------------------------------------------------------------------------------
  real(R4P), intent(in)         :: x(1:) !< Data variable.
  character(len=:), allocatable :: code  !< Encoded base64 dataarray.
  integer(I1P),     allocatable :: xp(:) !< Packed data.
  integer(I4P)                  :: nn    !< Number of elements.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  nn = size(x, dim=1)
  call pack_data(a1=[int(nn*BYR4P, I4P)], a2=reshape(x, [nn]), packed=xp)
  call b64_encode(n=xp, code=code)
  return
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction encode_b64_comp1_rank1_R4P

  function encode_b64_comp1_rank2_R8P(x) result(code)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Encode (Base64) a dataarray with 1 components of rank 2 (R8P).
  !---------------------------------------------------------------------------------------------------------------------------------
  real(R8P), intent(in)         :: x(1:,1:) !< Data variable
  character(len=:), allocatable :: code     !< Encoded base64 dataarray.
  integer(I1P),     allocatable :: xp(:)    !< Packed data.
  integer(I4P)                  :: nn       !< Number of elements.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  nn = size(x, dim=1)*size(x, dim=2)
  call pack_data(a1=[int(nn*BYR8P, I4P)], a2=reshape(x, [nn]), packed=xp)
  call b64_encode(n=xp, code=code)
  return
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction encode_b64_comp1_rank2_R8P

  function encode_b64_comp1_rank2_R4P(x) result(code)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Encode (Base64) a dataarray with 1 components of rank 2 (R4P).
  !---------------------------------------------------------------------------------------------------------------------------------
  real(R4P), intent(in)         :: x(1:,1:) !< Data variable
  character(len=:), allocatable :: code     !< Encoded base64 dataarray.
  integer(I1P),     allocatable :: xp(:)    !< Packed data.
  integer(I4P)                  :: nn       !< Number of elements.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  nn = size(x, dim=1)*size(x, dim=2)
  call pack_data(a1=[int(nn*BYR4P, I4P)], a2=reshape(x, [nn]), packed=xp)
  call b64_encode(n=xp, code=code)
  return
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction encode_b64_comp1_rank2_R4P

  function encode_b64_comp1_rank4_R8P(x) result(code)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Encode (Base64) a dataarray with 1 components of rank 4 (R8P).
  !---------------------------------------------------------------------------------------------------------------------------------
  real(R8P), intent(in)         :: x(1:,1:,1:,1:) !< Data variable.
  character(len=:), allocatable :: code           !< Encoded base64 dataarray.
  integer(I1P),     allocatable :: xp(:)          !< Packed data.
  integer(I4P)                  :: nn             !< Number of elements.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  nn = size(x, dim=1)*size(x, dim=2)*size(x, dim=3)*size(x, dim=4)
  call pack_data(a1=[int(nn*BYR8P, I4P)], a2=reshape(x, [nn]), packed=xp)
  call b64_encode(n=xp, code=code)
  return
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction encode_b64_comp1_rank4_R8P

  function encode_b64_comp1_rank3_R8P(x) result(code)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Encode (Base64) a dataarray with 1 components of rank 3 (R8P).
  !---------------------------------------------------------------------------------------------------------------------------------
  real(R8P), intent(in)         :: x(1:,1:,1:) !< Data variable
  character(len=:), allocatable :: code        !< Encoded base64 dataarray.
  integer(I1P),     allocatable :: xp(:)       !< Packed data.
  integer(I4P)                  :: nn          !< Number of elements.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  nn = size(x, dim=1)*size(x, dim=2)*size(x, dim=3)
  call pack_data(a1=[int(nn*BYR8P, I4P)], a2=reshape(x, [nn]), packed=xp)
  call b64_encode(n=xp, code=code)
  return
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction encode_b64_comp1_rank3_R8P

  function encode_b64_comp1_rank3_R4P(x) result(code)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Encode (Base64) a dataarray with 1 components of rank 3 (R4P).
  !---------------------------------------------------------------------------------------------------------------------------------
  real(R4P), intent(in)         :: x(1:,1:,1:) !< Data variable
  character(len=:), allocatable :: code        !< Encoded base64 dataarray.
  integer(I1P),     allocatable :: xp(:)       !< Packed data.
  integer(I4P)                  :: nn          !< Number of elements.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  nn = size(x, dim=1)*size(x, dim=2)*size(x, dim=3)
  call pack_data(a1=[int(nn*BYR4P, I4P)], a2=reshape(x, [nn]), packed=xp)
  call b64_encode(n=xp, code=code)
  return
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction encode_b64_comp1_rank3_R4P

  function encode_b64_comp1_rank4_R4P(x) result(code)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Encode (Base64) a dataarray with 1 components of rank 4 (R4P).
  !---------------------------------------------------------------------------------------------------------------------------------
  real(R4P), intent(in)         :: x(1:,1:,1:,1:) !< Data variable.
  character(len=:), allocatable :: code           !< Encoded base64 dataarray.
  integer(I1P),     allocatable :: xp(:)          !< Packed data.
  integer(I4P)                  :: nn             !< Number of elements.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  nn = size(x, dim=1)*size(x, dim=2)*size(x, dim=3)*size(x, dim=4)
  call pack_data(a1=[int(nn*BYR4P, I4P)], a2=reshape(x, [nn]), packed=xp)
  call b64_encode(n=xp, code=code)
  return
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction encode_b64_comp1_rank4_R4P

  function encode_b64_comp3_rank1_R8P(x, y, z) result(code)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Encode (Base64) a dataarray with 3 components of rank 1 (R8P).
  !---------------------------------------------------------------------------------------------------------------------------------
  real(R8P),    intent(in)      :: x(1:)  !< X component.
  real(R8P),    intent(in)      :: y(1:)  !< Y component.
  real(R8P),    intent(in)      :: z(1:)  !< Z component.
  character(len=:), allocatable :: code   !< Encoded base64 dataarray.
  integer(I1P),     allocatable :: xyz(:) !< Packed data.
  integer(I4P)                  :: nn     !< Number of elements.
  integer(I4P)                  :: n      !< Counter.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  nn = size(x, dim=1)
  call pack_data(a1=[int(3*nn*BYR8P, I4P)], a2=[(x(n), y(n), z(n), n=1, nn)], packed=xyz)
  call b64_encode(n=xyz, code=code)
  return
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction encode_b64_comp3_rank1_R8P

  function encode_b64_comp3_rank1_R4P(x, y, z) result(code)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Encode (Base64) a dataarray with 3 components of rank 1 (R4P).
  !---------------------------------------------------------------------------------------------------------------------------------
  real(R4P),    intent(in)      :: x(1:)  !< X component.
  real(R4P),    intent(in)      :: y(1:)  !< Y component.
  real(R4P),    intent(in)      :: z(1:)  !< Z component.
  character(len=:), allocatable :: code   !< Encoded base64 dataarray.
  integer(I1P),     allocatable :: xyz(:) !< Packed data.
  integer(I4P)                  :: nn     !< Number of elements.
  integer(I4P)                  :: n      !< Counter.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  nn = size(x, dim=1)
  call pack_data(a1=[int(3*nn*BYR4P, I4P)], a2=[(x(n), y(n), z(n), n=1, nn)], packed=xyz)
  call b64_encode(n=xyz, code=code)
  return
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction encode_b64_comp3_rank1_R4P

  function encode_b64_comp3_rank3_R8P(x, y, z) result(code)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Encode (Base64) a dataarray with 3 components of rank 3 (R8P).
  !---------------------------------------------------------------------------------------------------------------------------------
  real(R8P),    intent(in)      :: x(1:,1:,1:) !< X component.
  real(R8P),    intent(in)      :: y(1:,1:,1:) !< Y component.
  real(R8P),    intent(in)      :: z(1:,1:,1:) !< Z component.
  character(len=:), allocatable :: code        !< Encoded base64 dataarray.
  integer(I1P),     allocatable :: xyz(:)      !< Packed data.
  integer(I4P)                  :: nn1         !< Number of elements along dim 1.
  integer(I4P)                  :: nn2         !< Number of elements along dim 2.
  integer(I4P)                  :: nn3         !< Number of elements along dim 3.
  integer(I4P)                  :: nn          !< Number of elements.
  integer(I4P)                  :: n1          !< Counter.
  integer(I4P)                  :: n2          !< Counter.
  integer(I4P)                  :: n3          !< Counter.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  nn1 = size(x, dim=1)
  nn2 = size(x, dim=2)
  nn3 = size(x, dim=3)
  nn = nn1*nn2*nn3
  call pack_data(a1=[int(3*nn*BYR8P, I4P)], a2=[(((x(n1, n2, n3), y(n1, n2, n3), z(n1, n2, n3), n1=1, nn1),  &
                                                                                                n2=1, nn2),  &
                                                                                                n3=1, nn3)], &
                 packed=xyz)
  call b64_encode(n=xyz,code=code)
  return
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction encode_b64_comp3_rank3_R8P

  function encode_b64_comp3_rank3_R4P(x, y, z) result(code)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Encode (Base64) a dataarray with 3 components of rank 3 (R4P).
  !---------------------------------------------------------------------------------------------------------------------------------
  real(R4P),    intent(in)      :: x(1:,1:,1:) !< X component.
  real(R4P),    intent(in)      :: y(1:,1:,1:) !< Y component.
  real(R4P),    intent(in)      :: z(1:,1:,1:) !< Z component.
  character(len=:), allocatable :: code        !< Encoded base64 dataarray.
  integer(I1P),     allocatable :: xyz(:)      !< Packed data.
  integer(I4P)                  :: nn1         !< Number of elements along dim 1.
  integer(I4P)                  :: nn2         !< Number of elements along dim 2.
  integer(I4P)                  :: nn3         !< Number of elements along dim 3.
  integer(I4P)                  :: nn          !< Number of elements.
  integer(I4P)                  :: n1          !< Counter.
  integer(I4P)                  :: n2          !< Counter.
  integer(I4P)                  :: n3          !< Counter.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  nn1 = size(x, dim=1)
  nn2 = size(x, dim=2)
  nn3 = size(x, dim=3)
  nn = nn1*nn2*nn3
  call pack_data(a1=[int(3*nn*BYR4P, I4P)], a2=[(((x(n1, n2, n3), y(n1, n2, n3), z(n1, n2, n3), n1=1, nn1),  &
                                                                                                n2=1, nn2),  &
                                                                                                n3=1, nn3)], &
                 packed=xyz)
  call b64_encode(n=xyz,code=code)
  return
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction encode_b64_comp3_rank3_R4P
endmodule vtk_fortran_vtk_file
