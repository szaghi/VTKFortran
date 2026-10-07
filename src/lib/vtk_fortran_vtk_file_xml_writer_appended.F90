!< VTK file XMl writer, appended.
module vtk_fortran_vtk_file_xml_writer_appended
!< VTK file XMl writer, appended.
use, intrinsic :: iso_c_binding, only : c_int, c_loc, c_long, c_signed_char
use penf
use stringifor
use vtk_fortran_dataarray_encoder
use vtk_fortran_parameters
use vtk_fortran_vtk_file_xml_writer_abstract
#ifdef VTKFORTRAN_USE_ZLIB
use vtk_fortran_zlib, only : zlib_compress_bound, zlib_compress2, Z_DEFAULT_COMPRESSION
#endif

implicit none
private
public :: xml_writer_appended

#ifdef VTKFORTRAN_USE_ZLIB
interface to_bytes
  !< Copy a dataarray into a bytes stream, element by element (a whole-array transfer result can be placed on the stack).
  module procedure to_bytes_R8P, to_bytes_R4P, to_bytes_I8P, to_bytes_I4P, to_bytes_I2P, to_bytes_I1P
endinterface to_bytes
#endif

type, extends(xml_writer_abstract) :: xml_writer_appended
  !< VTK file XML writer, appended.
  type(string) :: encoding      !< Appended data encoding: "raw" or "base64".
  integer(I4P) :: scratch=0_I4P !< Scratch logical unit.
  logical      :: is_compressed = .false.     !< Enable VTK internal zlib compression for appended raw data.
  integer(I4P) :: compression_level = 6_I4P   !< zlib compression level [1..9], 6 is a reasonable default.
  integer(I4P) :: compression_block_size = 32768_I4P !< Uncompressed block size in bytes (VTK compressed blocks).
  contains
    ! deferred methods
    procedure, pass(self) :: initialize                 !< Initialize writer.
    procedure, pass(self) :: finalize                   !< Finalize writer.
    procedure, pass(self) :: write_header_tag           !< Write header tag (override to add compression attributes).
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
    procedure, pass(self) :: write_dataarray6_rank1_R8P !< Write dataarray 6, rank 1, R8P.
    procedure, pass(self) :: write_dataarray6_rank1_R4P !< Write dataarray 6, rank 1, R4P.
    procedure, pass(self) :: write_dataarray6_rank1_I8P !< Write dataarray 6, rank 1, I8P.
    procedure, pass(self) :: write_dataarray6_rank1_I4P !< Write dataarray 6, rank 1, I4P.
    procedure, pass(self) :: write_dataarray6_rank1_I2P !< Write dataarray 6, rank 1, I2P.
    procedure, pass(self) :: write_dataarray6_rank1_I1P !< Write dataarray 6, rank 1, I1P.
    procedure, pass(self) :: write_dataarray6_rank3_R8P !< Write dataarray 6, rank 3, R8P.
    procedure, pass(self) :: write_dataarray6_rank3_R4P !< Write dataarray 6, rank 3, R4P.
    procedure, pass(self) :: write_dataarray6_rank3_I8P !< Write dataarray 6, rank 3, I8P.
    procedure, pass(self) :: write_dataarray6_rank3_I4P !< Write dataarray 6, rank 3, I4P.
    procedure, pass(self) :: write_dataarray6_rank3_I2P !< Write dataarray 6, rank 3, I2P.
    procedure, pass(self) :: write_dataarray6_rank3_I1P !< Write dataarray 6, rank 3, I1P.
    procedure, pass(self) :: write_dataarray_appended   !< Write appended.
    ! private methods
    procedure, pass(self), private :: ioffset_update     !< Update ioffset count.
    procedure, pass(self), private :: n_bytes            !< Return the checked bytes count of a dataarray.
    procedure, pass(self), private :: open_scratch_file  !< Open scratch file.
    procedure, pass(self), private :: close_scratch_file !< Close scratch file.
    generic, private :: write_on_scratch_dataarray =>          &
                        write_on_scratch_dataarray1_rank1,     &
                        write_on_scratch_dataarray1_rank2,     &
                        write_on_scratch_dataarray1_rank3,     &
                        write_on_scratch_dataarray1_rank4,     &
                        write_on_scratch_dataarray3_rank1_R8P, &
                        write_on_scratch_dataarray3_rank1_R4P, &
                        write_on_scratch_dataarray3_rank1_I8P, &
                        write_on_scratch_dataarray3_rank1_I4P, &
                        write_on_scratch_dataarray3_rank1_I2P, &
                        write_on_scratch_dataarray3_rank1_I1P, &
                        write_on_scratch_dataarray3_rank2_R8P, &
                        write_on_scratch_dataarray3_rank2_R4P, &
                        write_on_scratch_dataarray3_rank2_I8P, &
                        write_on_scratch_dataarray3_rank2_I4P, &
                        write_on_scratch_dataarray3_rank2_I2P, &
                        write_on_scratch_dataarray3_rank2_I1P, &
                        write_on_scratch_dataarray3_rank3_R8P, &
                        write_on_scratch_dataarray3_rank3_R4P, &
                        write_on_scratch_dataarray3_rank3_I8P, &
                        write_on_scratch_dataarray3_rank3_I4P, &
                        write_on_scratch_dataarray3_rank3_I2P, &
                        write_on_scratch_dataarray3_rank3_I1P, &
                        write_on_scratch_dataarray6_rank1_R8P, &
                        write_on_scratch_dataarray6_rank1_R4P, &
                        write_on_scratch_dataarray6_rank1_I8P, &
                        write_on_scratch_dataarray6_rank1_I4P, &
                        write_on_scratch_dataarray6_rank1_I2P, &
                        write_on_scratch_dataarray6_rank1_I1P, &
                        write_on_scratch_dataarray6_rank2_R8P, &
                        write_on_scratch_dataarray6_rank2_R4P, &
                        write_on_scratch_dataarray6_rank2_I8P, &
                        write_on_scratch_dataarray6_rank2_I4P, &
                        write_on_scratch_dataarray6_rank2_I2P, &
                        write_on_scratch_dataarray6_rank2_I1P, &
                        write_on_scratch_dataarray6_rank3_R8P, &
                        write_on_scratch_dataarray6_rank3_R4P, &
                        write_on_scratch_dataarray6_rank3_I8P, &
                        write_on_scratch_dataarray6_rank3_I4P, &
                        write_on_scratch_dataarray6_rank3_I2P, &
                        write_on_scratch_dataarray6_rank3_I1P !< Write dataarray.
    procedure, pass(self), private :: write_on_scratch_dataarray1_rank1     !< Write dataarray, data 1 rank 1.
    procedure, pass(self), private :: write_on_scratch_dataarray1_rank2     !< Write dataarray, data 1 rank 2.
    procedure, pass(self), private :: write_on_scratch_dataarray1_rank3     !< Write dataarray, data 1 rank 3.
    procedure, pass(self), private :: write_on_scratch_dataarray1_rank4     !< Write dataarray, data 1 rank 4.
    procedure, pass(self), private :: write_on_scratch_dataarray3_rank1_R8P !< Write dataarray, comp 3 rank 1, R8P.
    procedure, pass(self), private :: write_on_scratch_dataarray3_rank1_R4P !< Write dataarray, comp 3 rank 1, R4P.
    procedure, pass(self), private :: write_on_scratch_dataarray3_rank1_I8P !< Write dataarray, comp 3 rank 1, I8P.
    procedure, pass(self), private :: write_on_scratch_dataarray3_rank1_I4P !< Write dataarray, comp 3 rank 1, I4P.
    procedure, pass(self), private :: write_on_scratch_dataarray3_rank1_I2P !< Write dataarray, comp 3 rank 1, I2P.
    procedure, pass(self), private :: write_on_scratch_dataarray3_rank1_I1P !< Write dataarray, comp 3 rank 1, I1P.
    procedure, pass(self), private :: write_on_scratch_dataarray3_rank2_R8P !< Write dataarray, comp 3 rank 2, R8P.
    procedure, pass(self), private :: write_on_scratch_dataarray3_rank2_R4P !< Write dataarray, comp 3 rank 2, R4P.
    procedure, pass(self), private :: write_on_scratch_dataarray3_rank2_I8P !< Write dataarray, comp 3 rank 2, I8P.
    procedure, pass(self), private :: write_on_scratch_dataarray3_rank2_I4P !< Write dataarray, comp 3 rank 2, I4P.
    procedure, pass(self), private :: write_on_scratch_dataarray3_rank2_I2P !< Write dataarray, comp 3 rank 2, I2P.
    procedure, pass(self), private :: write_on_scratch_dataarray3_rank2_I1P !< Write dataarray, comp 3 rank 2, I1P.
    procedure, pass(self), private :: write_on_scratch_dataarray3_rank3_R8P !< Write dataarray, comp 3 rank 3, R8P.
    procedure, pass(self), private :: write_on_scratch_dataarray3_rank3_R4P !< Write dataarray, comp 3 rank 3, R4P.
    procedure, pass(self), private :: write_on_scratch_dataarray3_rank3_I8P !< Write dataarray, comp 3 rank 3, I8P.
    procedure, pass(self), private :: write_on_scratch_dataarray3_rank3_I4P !< Write dataarray, comp 3 rank 3, I4P.
    procedure, pass(self), private :: write_on_scratch_dataarray3_rank3_I2P !< Write dataarray, comp 3 rank 3, I2P.
    procedure, pass(self), private :: write_on_scratch_dataarray3_rank3_I1P !< Write dataarray, comp 3 rank 3, I1P.
    procedure, pass(self), private :: write_on_scratch_dataarray6_rank1_R8P !< Write dataarray, comp 6 rank 1, R8P.
    procedure, pass(self), private :: write_on_scratch_dataarray6_rank1_R4P !< Write dataarray, comp 6 rank 1, R4P.
    procedure, pass(self), private :: write_on_scratch_dataarray6_rank1_I8P !< Write dataarray, comp 6 rank 1, I8P.
    procedure, pass(self), private :: write_on_scratch_dataarray6_rank1_I4P !< Write dataarray, comp 6 rank 1, I4P.
    procedure, pass(self), private :: write_on_scratch_dataarray6_rank1_I2P !< Write dataarray, comp 6 rank 1, I2P.
    procedure, pass(self), private :: write_on_scratch_dataarray6_rank1_I1P !< Write dataarray, comp 6 rank 1, I1P.
    procedure, pass(self), private :: write_on_scratch_dataarray6_rank2_R8P !< Write dataarray, comp 6 rank 2, R8P.
    procedure, pass(self), private :: write_on_scratch_dataarray6_rank2_R4P !< Write dataarray, comp 6 rank 2, R4P.
    procedure, pass(self), private :: write_on_scratch_dataarray6_rank2_I8P !< Write dataarray, comp 6 rank 2, I8P.
    procedure, pass(self), private :: write_on_scratch_dataarray6_rank2_I4P !< Write dataarray, comp 6 rank 2, I4P.
    procedure, pass(self), private :: write_on_scratch_dataarray6_rank2_I2P !< Write dataarray, comp 6 rank 2, I2P.
    procedure, pass(self), private :: write_on_scratch_dataarray6_rank2_I1P !< Write dataarray, comp 6 rank 2, I1P.
    procedure, pass(self), private :: write_on_scratch_dataarray6_rank3_R8P !< Write dataarray, comp 6 rank 3, R8P.
    procedure, pass(self), private :: write_on_scratch_dataarray6_rank3_R4P !< Write dataarray, comp 6 rank 3, R4P.
    procedure, pass(self), private :: write_on_scratch_dataarray6_rank3_I8P !< Write dataarray, comp 6 rank 3, I8P.
    procedure, pass(self), private :: write_on_scratch_dataarray6_rank3_I4P !< Write dataarray, comp 6 rank 3, I4P.
    procedure, pass(self), private :: write_on_scratch_dataarray6_rank3_I2P !< Write dataarray, comp 6 rank 3, I2P.
    procedure, pass(self), private :: write_on_scratch_dataarray6_rank3_I1P !< Write dataarray, comp 6 rank 3, I1P.
endtype xml_writer_appended
contains
  function initialize(self, format, filename, mesh_topology, nx1, nx2, ny1, ny2, nz1, nz2, &
                      is_volatile, mesh_kind) result(error)
  !< Initialize writer.
  class(xml_writer_appended), intent(inout)        :: self          !< Writer.
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

  self%topology = trim(adjustl(mesh_topology))
  self%format_ch = 'appended'
  self%encoding = format
  self%encoding = self%encoding%upper()
  self%is_compressed = .false.
  select case(self%encoding%chars())
  case('RAW')
    self%encoding = 'raw'
  case('BINARY-APPENDED')
    self%encoding = 'base64'
  case('RAW-ZLIB')
#ifdef VTKFORTRAN_USE_ZLIB
    self%encoding = 'raw'
    self%is_compressed = .true.
#else
    self%error = 1
    error = self%error
    return
#endif
  endselect
  call self%open_xml_file(filename=filename)
  call self%write_header_tag
  call self%write_topology_tag(nx1=nx1, nx2=nx2, ny1=ny1, ny2=ny2, nz1=nz1, nz2=nz2, mesh_kind=mesh_kind)
  self%ioffset = 0
  call self%open_scratch_file
  error = self%error
  endfunction initialize

  subroutine write_header_tag(self)
  !< Write header tag.
  !<
  !< The header_type (bytes count width) is always declared; when VTK internal compression is enabled for appended raw data,
  !< the compressor is declared too: compressor="vtkZLibDataCompressor" header_type="UInt32"
  class(xml_writer_appended), intent(inout) :: self   !< Writer.
  type(string)                              :: buffer !< Buffer string.
  character(len=:), allocatable             :: attrs  !< Extra attributes.

  buffer = '<?xml version="1.0"?>'//end_rec
  attrs = ' header_type="'//trim(merge('UInt64', 'UInt32', self%is_uint64))//'"'
  if (self%is_compressed) attrs = ' compressor="vtkZLibDataCompressor"'//attrs
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

   function finalize(self) result(error)
   !< Finalize writer.
   class(xml_writer_appended), intent(inout) :: self  !< Writer.
   integer(I4P)                              :: error !< Error status.

   call self%write_end_tag(name=self%topology%chars())
   call self%write_dataarray_appended
   call self%write_end_tag(name='VTKFile')
   call self%close_xml_file
   call self%close_scratch_file
   error = self%error
   endfunction finalize

  function n_bytes(self, n_byte) result(n)
  !< Return the bytes count of a dataarray, checked against the bytes count header of the file (UInt32 or UInt64).
  class(xml_writer_appended), intent(in) :: self   !< Writer.
  integer(I8P),               intent(in) :: n_byte !< Bytes count, computed in I8P.
  integer(I8P)                           :: n      !< Checked bytes count.

  if (self%is_uint64) then
    n = n_byte
  else
    n = int(bytes_count(n_byte), I8P) ! stops with an explicit error beyond the UInt32 header limit
  endif
  endfunction n_bytes

  elemental subroutine ioffset_update(self, n_byte)
  !< Update ioffset count.
  class(xml_writer_appended), intent(inout) :: self  !< Writer.
  integer(I8P),               intent(in)    :: n_byte !< Number of bytes saved.
  integer(I8P)                              :: hb     !< Bytes of the bytes count header (4 for UInt32, 8 for UInt64).

  hb = merge(int(BYI8P, I8P), int(BYI4P, I8P), self%is_uint64)
  if (self%is_compressed) then
    ! n_byte is already the exact payload byte-size for this DataArray in the <AppendedData> section
    ! (VTK "compressed blocks" header + compressed data).
    self%ioffset = self%ioffset + n_byte
  elseif (self%encoding=='raw') then
    self%ioffset = self%ioffset + hb + n_byte
  else
    self%ioffset = self%ioffset + ((n_byte + hb + 2_I8P)/3_I8P)*4_I8P
  endif
  endsubroutine ioffset_update

  subroutine open_scratch_file(self)
  !< Open scratch file.
  class(xml_writer_appended), intent(inout) :: self  !< Writer.

  open(newunit=self%scratch, &
       form='UNFORMATTED',   &
       access='STREAM',      &
       action='READWRITE',   &
       status='SCRATCH',     &
       iostat=self%error)
  endsubroutine open_scratch_file

  subroutine close_scratch_file(self)
  !< Close scratch file.
  class(xml_writer_appended), intent(inout) :: self  !< Writer.

  close(unit=self%scratch, iostat=self%error)
  endsubroutine close_scratch_file

  ! write_dataarray methods
  function write_dataarray1_rank1_R8P(self, data_name, x, is_tuples) result(error)
  !< Write `<DataArray... NumberOfComponents="1"...>...</DataArray>` tag (R8P).
  class(xml_writer_appended), intent(inout)        :: self         !< Writer.
  character(*),               intent(in)           :: data_name    !< Data name.
  real(R8P),                  intent(in)           :: x(1:)        !< Data variable.
  logical,                    intent(in), optional :: is_tuples    !< Use "NumberOfTuples".
  integer(I4P)                                     :: error        !< Error status.
  character(len=:), allocatable                    :: data_type    !< Data type.
  integer(I4P)                                     :: n_components !< Number of components.

  data_type = 'Float64'
  n_components = 1
  call self%write_dataarray_tag_appended(data_type=data_type, number_of_components=n_components, data_name=data_name, &
                                         is_tuples=is_tuples)
  call self%ioffset_update(n_byte=self%write_on_scratch_dataarray(x=x))
  error = self%error
  endfunction write_dataarray1_rank1_R8P

  function write_dataarray1_rank1_R4P(self, data_name, x, is_tuples) result(error)
  !< Write `<DataArray... NumberOfComponents="1"...>...</DataArray>` tag (R4P).
  class(xml_writer_appended), intent(inout)        :: self         !< Writer.
  character(*),               intent(in)           :: data_name    !< Data name.
  real(R4P),                  intent(in)           :: x(1:)        !< Data variable.
  logical,                    intent(in), optional :: is_tuples    !< Use "NumberOfTuples".
  integer(I4P)                                     :: error        !< Error status.
  character(len=:), allocatable                    :: data_type    !< Data type.
  integer(I4P)                                     :: n_components !< Number of components.

  data_type = 'Float32'
  n_components = 1
  call self%write_dataarray_tag_appended(data_type=data_type, number_of_components=n_components, data_name=data_name, &
                                         is_tuples=is_tuples)
  call self%ioffset_update(n_byte=self%write_on_scratch_dataarray(x=x))
  error = self%error
  endfunction write_dataarray1_rank1_R4P

  function write_dataarray1_rank1_I8P(self, data_name, x, is_tuples) result(error)
  !< Write `<DataArray... NumberOfComponents="1"...>...</DataArray>` tag (I8P).
  class(xml_writer_appended), intent(inout)        :: self         !< Writer.
  character(*),               intent(in)           :: data_name    !< Data name.
  integer(I8P),               intent(in)           :: x(1:)        !< Data variable.
  logical,                    intent(in), optional :: is_tuples    !< Use "NumberOfTuples".
  integer(I4P)                                     :: error        !< Error status.
  character(len=:), allocatable                    :: data_type    !< Data type.
  integer(I4P)                                     :: n_components !< Number of components.

  data_type = 'Int64'
  n_components = 1
  call self%write_dataarray_tag_appended(data_type=data_type, number_of_components=n_components, data_name=data_name, &
                                         is_tuples=is_tuples)
  call self%ioffset_update(n_byte=self%write_on_scratch_dataarray(x=x))
  error = self%error
  endfunction write_dataarray1_rank1_I8P

  function write_dataarray1_rank1_I4P(self, data_name, x, is_tuples) result(error)
  !< Write `<DataArray... NumberOfComponents="1"...>...</DataArray>` tag (I4P).
  class(xml_writer_appended), intent(inout)        :: self         !< Writer.
  character(*),               intent(in)           :: data_name    !< Data name.
  integer(I4P),               intent(in)           :: x(1:)        !< Data variable.
  logical,                    intent(in), optional :: is_tuples    !< Use "NumberOfTuples".
  integer(I4P)                                     :: error        !< Error status.
  character(len=:), allocatable                    :: data_type    !< Data type.
  integer(I4P)                                     :: n_components !< Number of components.

  data_type = 'Int32'
  n_components = 1
  call self%write_dataarray_tag_appended(data_type=data_type, number_of_components=n_components, data_name=data_name, &
                                         is_tuples=is_tuples)
  call self%ioffset_update(n_byte=self%write_on_scratch_dataarray(x=x))
  error = self%error
  endfunction write_dataarray1_rank1_I4P

  function write_dataarray1_rank1_I2P(self, data_name, x, is_tuples) result(error)
  !< Write `<DataArray... NumberOfComponents="1"...>...</DataArray>` tag (I2P).
  class(xml_writer_appended), intent(inout)        :: self         !< Writer.
  character(*),               intent(in)           :: data_name    !< Data name.
  integer(I2P),               intent(in)           :: x(1:)        !< Data variable.
  logical,                    intent(in), optional :: is_tuples    !< Use "NumberOfTuples".
  integer(I4P)                                     :: error        !< Error status.
  character(len=:), allocatable                    :: data_type    !< Data type.
  integer(I4P)                                     :: n_components !< Number of components.

  data_type = 'Int16'
  n_components = 1
  call self%write_dataarray_tag_appended(data_type=data_type, number_of_components=n_components, data_name=data_name, &
                                         is_tuples=is_tuples)
  call self%ioffset_update(n_byte=self%write_on_scratch_dataarray(x=x))
  error = self%error
  endfunction write_dataarray1_rank1_I2P

  function write_dataarray1_rank1_I1P(self, data_name, x, is_tuples) result(error)
  !< Write `<DataArray... NumberOfComponents="1"...>...</DataArray>` tag (I1P).
  class(xml_writer_appended), intent(inout)        :: self         !< Writer.
  character(*),               intent(in)           :: data_name    !< Data name.
  integer(I1P),               intent(in)           :: x(1:)        !< Data variable.
  logical,                    intent(in), optional :: is_tuples    !< Use "NumberOfTuples".
  integer(I4P)                                     :: error        !< Error status.
  character(len=:), allocatable                    :: data_type    !< Data type.
  integer(I4P)                                     :: n_components !< Number of components.

  data_type = 'Int8'
  n_components = 1
  call self%write_dataarray_tag_appended(data_type=data_type, number_of_components=n_components, data_name=data_name, &
                                         is_tuples=is_tuples)
  call self%ioffset_update(n_byte=self%write_on_scratch_dataarray(x=x))
  error = self%error
  endfunction write_dataarray1_rank1_I1P

  function write_dataarray1_rank2_R8P(self, data_name, x, one_component, is_tuples) result(error)
  !< Write `<DataArray... NumberOfComponents="n"...>...</DataArray>` tag (R8P).
  class(xml_writer_appended), intent(inout)        :: self          !< Writer.
  character(*),               intent(in)           :: data_name     !< Data name.
  real(R8P),                  intent(in)           :: x(1:,1:)      !< Data variable.
  logical,                    intent(in), optional :: one_component !< Force one component.
  logical,                    intent(in), optional :: is_tuples     !< Use "NumberOfTuples".
  integer(I4P)                                     :: error         !< Error status.
  character(len=:), allocatable                    :: data_type     !< Data type.
  integer(I4P)                                     :: n_components  !< Number of components.

  data_type = 'Float64'
  n_components = size(x, dim=1)
  if (present(one_component)) then
    if (one_component) n_components = 1
  endif
  call self%write_dataarray_tag_appended(data_type=data_type, number_of_components=n_components, data_name=data_name, &
                                         is_tuples=is_tuples)
  call self%ioffset_update(n_byte=self%write_on_scratch_dataarray(x=x))
  error = self%error
  endfunction write_dataarray1_rank2_R8P

  function write_dataarray1_rank2_R4P(self, data_name, x, one_component, is_tuples) result(error)
  !< Write `<DataArray... NumberOfComponents="n"...>...</DataArray>` tag (R4P).
  class(xml_writer_appended), intent(inout)        :: self          !< Writer.
  character(*),               intent(in)           :: data_name     !< Data name.
  real(R4P),                  intent(in)           :: x(1:,1:)      !< Data variable.
  logical,                    intent(in), optional :: one_component !< Force one component.
  logical,                    intent(in), optional :: is_tuples     !< Use "NumberOfTuples".
  integer(I4P)                                     :: error         !< Error status.
  character(len=:), allocatable                    :: data_type     !< Data type.
  integer(I4P)                                     :: n_components  !< Number of components.

  data_type = 'Float32'
  n_components = size(x, dim=1)
  if (present(one_component)) then
    if (one_component) n_components = 1
  endif
  call self%write_dataarray_tag_appended(data_type=data_type, number_of_components=n_components, data_name=data_name, &
                                         is_tuples=is_tuples)
  call self%ioffset_update(n_byte=self%write_on_scratch_dataarray(x=x))
  error = self%error
  endfunction write_dataarray1_rank2_R4P

  function write_dataarray1_rank2_I8P(self, data_name, x, one_component, is_tuples) result(error)
  !< Write `<DataArray... NumberOfComponents="n"...>...</DataArray>` tag (I8P).
  class(xml_writer_appended), intent(inout)        :: self          !< Writer.
  character(*),               intent(in)           :: data_name     !< Data name.
  integer(I8P),               intent(in)           :: x(1:,1:)      !< Data variable.
  logical,                    intent(in), optional :: one_component !< Force one component.
  logical,                    intent(in), optional :: is_tuples     !< Use "NumberOfTuples".
  integer(I4P)                                     :: error         !< Error status.
  character(len=:), allocatable                    :: data_type     !< Data type.
  integer(I4P)                                     :: n_components  !< Number of components.

  data_type = 'Int64'
  n_components = size(x, dim=1)
  if (present(one_component)) then
    if (one_component) n_components = 1
  endif
  call self%write_dataarray_tag_appended(data_type=data_type, number_of_components=n_components, data_name=data_name, &
                                         is_tuples=is_tuples)
  call self%ioffset_update(n_byte=self%write_on_scratch_dataarray(x=x))
  error = self%error
  endfunction write_dataarray1_rank2_I8P

  function write_dataarray1_rank2_I4P(self, data_name, x, one_component, is_tuples) result(error)
  !< Write `<DataArray... NumberOfComponents="n"...>...</DataArray>` tag (I4P).
  class(xml_writer_appended), intent(inout)        :: self          !< Writer.
  character(*),               intent(in)           :: data_name     !< Data name.
  integer(I4P),               intent(in)           :: x(1:,1:)      !< Data variable.
  logical,                    intent(in), optional :: one_component !< Force one component.
  logical,                    intent(in), optional :: is_tuples     !< Use "NumberOfTuples".
  integer(I4P)                                     :: error         !< Error status.
  character(len=:), allocatable                    :: data_type     !< Data type.
  integer(I4P)                                     :: n_components  !< Number of components.

  data_type = 'Int32'
  n_components = size(x, dim=1)
  if (present(one_component)) then
    if (one_component) n_components = 1
  endif
  call self%write_dataarray_tag_appended(data_type=data_type, number_of_components=n_components, data_name=data_name, &
                                         is_tuples=is_tuples)
  call self%ioffset_update(n_byte=self%write_on_scratch_dataarray(x=x))
  error = self%error
  endfunction write_dataarray1_rank2_I4P

  function write_dataarray1_rank2_I2P(self, data_name, x, one_component, is_tuples) result(error)
  !< Write `<DataArray... NumberOfComponents="n"...>...</DataArray>` tag (I2P).
  class(xml_writer_appended), intent(inout)        :: self          !< Writer.
  character(*),               intent(in)           :: data_name     !< Data name.
  integer(I2P),               intent(in)           :: x(1:,1:)      !< Data variable.
  logical,                    intent(in), optional :: one_component !< Force one component.
  logical,                    intent(in), optional :: is_tuples     !< Use "NumberOfTuples".
  integer(I4P)                                     :: error         !< Error status.
  character(len=:), allocatable                    :: data_type     !< Data type.
  integer(I4P)                                     :: n_components  !< Number of components.

  data_type = 'Int16'
  n_components = size(x, dim=1)
  if (present(one_component)) then
    if (one_component) n_components = 1
  endif
  call self%write_dataarray_tag_appended(data_type=data_type, number_of_components=n_components, data_name=data_name, &
                                         is_tuples=is_tuples)
  call self%ioffset_update(n_byte=self%write_on_scratch_dataarray(x=x))
  error = self%error
  endfunction write_dataarray1_rank2_I2P

  function write_dataarray1_rank2_I1P(self, data_name, x, one_component, is_tuples) result(error)
  !< Write `<DataArray... NumberOfComponents="n"...>...</DataArray>` tag (I1P).
  class(xml_writer_appended), intent(inout)        :: self          !< Writer.
  character(*),               intent(in)           :: data_name     !< Data name.
  integer(I1P),               intent(in)           :: x(1:,1:)      !< Data variable.
  logical,                    intent(in), optional :: one_component !< Force one component.
  logical,                    intent(in), optional :: is_tuples     !< Use "NumberOfTuples".
  integer(I4P)                                     :: error         !< Error status.
  character(len=:), allocatable                    :: data_type     !< Data type.
  integer(I4P)                                     :: n_components  !< Number of components.

  data_type = 'Int8'
  n_components = size(x, dim=1)
  if (present(one_component)) then
    if (one_component) n_components = 1
  endif
  call self%write_dataarray_tag_appended(data_type=data_type, number_of_components=n_components, data_name=data_name, &
                                         is_tuples=is_tuples)
  call self%ioffset_update(n_byte=self%write_on_scratch_dataarray(x=x))
  error = self%error
  endfunction write_dataarray1_rank2_I1P

  function write_dataarray1_rank3_R8P(self, data_name, x, one_component, is_tuples) result(error)
  !< Write `<DataArray... NumberOfComponents="n"...>...</DataArray>` tag (R8P).
  class(xml_writer_appended), intent(inout)        :: self          !< Writer.
  character(*),               intent(in)           :: data_name     !< Data name.
  real(R8P),                  intent(in)           :: x(1:,1:,1:)   !< Data variable.
  logical,                    intent(in), optional :: one_component !< Force one component.
  logical,                    intent(in), optional :: is_tuples     !< Use "NumberOfTuples".
  integer(I4P)                                     :: error         !< Error status.
  character(len=:), allocatable                    :: data_type     !< Data type.
  integer(I4P)                                     :: n_components  !< Number of components.

  data_type = 'Float64'
  n_components = size(x, dim=1)
  if (present(one_component)) then
    if (one_component) n_components = 1
  endif
  call self%write_dataarray_tag_appended(data_type=data_type, number_of_components=n_components, data_name=data_name, &
                                         is_tuples=is_tuples)
  call self%ioffset_update(n_byte=self%write_on_scratch_dataarray(x=x))
  error = self%error
  endfunction write_dataarray1_rank3_R8P

  function write_dataarray1_rank3_R4P(self, data_name, x, one_component, is_tuples) result(error)
  !< Write `<DataArray... NumberOfComponents="n"...>...</DataArray>` tag (R4P).
  class(xml_writer_appended), intent(inout)        :: self          !< Writer.
  character(*),               intent(in)           :: data_name     !< Data name.
  real(R4P),                  intent(in)           :: x(1:,1:,1:)   !< Data variable.
  logical,                    intent(in), optional :: one_component !< Force one component.
  logical,                    intent(in), optional :: is_tuples     !< Use "NumberOfTuples".
  integer(I4P)                                     :: error         !< Error status.
  character(len=:), allocatable                    :: data_type     !< Data type.
  integer(I4P)                                     :: n_components  !< Number of components.

  data_type = 'Float32'
  n_components = size(x, dim=1)
  if (present(one_component)) then
    if (one_component) n_components = 1
  endif
  call self%write_dataarray_tag_appended(data_type=data_type, number_of_components=n_components, data_name=data_name, &
                                         is_tuples=is_tuples)
  call self%ioffset_update(n_byte=self%write_on_scratch_dataarray(x=x))
  error = self%error
  endfunction write_dataarray1_rank3_R4P

  function write_dataarray1_rank3_I8P(self, data_name, x, one_component, is_tuples) result(error)
  !< Write `<DataArray... NumberOfComponents="n"...>...</DataArray>` tag (I8P).
  class(xml_writer_appended), intent(inout)        :: self          !< Writer.
  character(*),               intent(in)           :: data_name     !< Data name.
  integer(I8P),               intent(in)           :: x(1:,1:,1:)   !< Data variable.
  logical,                    intent(in), optional :: one_component !< Force one component.
  logical,                    intent(in), optional :: is_tuples     !< Use "NumberOfTuples".
  integer(I4P)                                     :: error         !< Error status.
  character(len=:), allocatable                    :: data_type     !< Data type.
  integer(I4P)                                     :: n_components  !< Number of components.

  data_type = 'Int64'
  n_components = size(x, dim=1)
  if (present(one_component)) then
    if (one_component) n_components = 1
  endif
  call self%write_dataarray_tag_appended(data_type=data_type, number_of_components=n_components, data_name=data_name, &
                                         is_tuples=is_tuples)
  call self%ioffset_update(n_byte=self%write_on_scratch_dataarray(x=x))
  error = self%error
  endfunction write_dataarray1_rank3_I8P

  function write_dataarray1_rank3_I4P(self, data_name, x, one_component, is_tuples) result(error)
  !< Write `<DataArray... NumberOfComponents="n"...>...</DataArray>` tag (I4P).
  class(xml_writer_appended), intent(inout)        :: self          !< Writer.
  character(*),               intent(in)           :: data_name     !< Data name.
  integer(I4P),               intent(in)           :: x(1:,1:,1:)   !< Data variable.
  logical,                    intent(in), optional :: one_component !< Force one component.
  logical,                    intent(in), optional :: is_tuples     !< Use "NumberOfTuples".
  integer(I4P)                                     :: error         !< Error status.
  character(len=:), allocatable                    :: data_type     !< Data type.
  integer(I4P)                                     :: n_components  !< Number of components.

  data_type = 'Int32'
  n_components = size(x, dim=1)
  if (present(one_component)) then
    if (one_component) n_components = 1
  endif
  call self%write_dataarray_tag_appended(data_type=data_type, number_of_components=n_components, data_name=data_name, &
                                         is_tuples=is_tuples)
  call self%ioffset_update(n_byte=self%write_on_scratch_dataarray(x=x))
  error = self%error
  endfunction write_dataarray1_rank3_I4P

  function write_dataarray1_rank3_I2P(self, data_name, x, one_component, is_tuples) result(error)
  !< Write `<DataArray... NumberOfComponents="n"...>...</DataArray>` tag (I2P).
  class(xml_writer_appended), intent(inout)        :: self          !< Writer.
  character(*),               intent(in)           :: data_name     !< Data name.
  integer(I2P),               intent(in)           :: x(1:,1:,1:)   !< Data variable.
  logical,                    intent(in), optional :: one_component !< Force one component.
  logical,                    intent(in), optional :: is_tuples     !< Use "NumberOfTuples".
  integer(I4P)                                     :: error         !< Error status.
  character(len=:), allocatable                    :: data_type     !< Data type.
  integer(I4P)                                     :: n_components  !< Number of components.

  data_type = 'Int16'
  n_components = size(x, dim=1)
  if (present(one_component)) then
    if (one_component) n_components = 1
  endif
  call self%write_dataarray_tag_appended(data_type=data_type, number_of_components=n_components, data_name=data_name, &
                                         is_tuples=is_tuples)
  call self%ioffset_update(n_byte=self%write_on_scratch_dataarray(x=x))
  error = self%error
  endfunction write_dataarray1_rank3_I2P

  function write_dataarray1_rank3_I1P(self, data_name, x, one_component, is_tuples) result(error)
  !< Write `<DataArray... NumberOfComponents="n"...>...</DataArray>` tag (I1P).
  class(xml_writer_appended), intent(inout)        :: self          !< Writer.
  character(*),               intent(in)           :: data_name     !< Data name.
  integer(I1P),               intent(in)           :: x(1:,1:,1:)   !< Data variable.
  logical,                    intent(in), optional :: one_component !< Force one component.
  logical,                    intent(in), optional :: is_tuples     !< Use "NumberOfTuples".
  integer(I4P)                                     :: error         !< Error status.
  character(len=:), allocatable                    :: data_type     !< Data type.
  integer(I4P)                                     :: n_components  !< Number of components.

  data_type = 'Int8'
  n_components = size(x, dim=1)
  if (present(one_component)) then
    if (one_component) n_components = 1
  endif
  call self%write_dataarray_tag_appended(data_type=data_type, number_of_components=n_components, data_name=data_name, &
                                         is_tuples=is_tuples)
  call self%ioffset_update(n_byte=self%write_on_scratch_dataarray(x=x))
  error = self%error
  endfunction write_dataarray1_rank3_I1P

  function write_dataarray1_rank4_R8P(self, data_name, x, one_component, is_tuples) result(error)
  !< Write `<DataArray... NumberOfComponents="n"...>...</DataArray>` tag (R8P).
  class(xml_writer_appended), intent(inout)        :: self           !< Writer.
  character(*),               intent(in)           :: data_name      !< Data name.
  real(R8P),                  intent(in)           :: x(1:,1:,1:,1:) !< Data variable.
  logical,                    intent(in), optional :: one_component  !< Force one component.
  logical,                    intent(in), optional :: is_tuples      !< Use "NumberOfTuples".
  integer(I4P)                                     :: error          !< Error status.
  character(len=:), allocatable                    :: data_type      !< Data type.
  integer(I4P)                                     :: n_components   !< Number of components.

  data_type = 'Float64'
  n_components = size(x, dim=1)
  if (present(one_component)) then
    if (one_component) n_components = 1
  endif
  call self%write_dataarray_tag_appended(data_type=data_type, number_of_components=n_components, data_name=data_name, &
                                         is_tuples=is_tuples)
  call self%ioffset_update(n_byte=self%write_on_scratch_dataarray(x=x))
  error = self%error
  endfunction write_dataarray1_rank4_R8P

  function write_dataarray1_rank4_R4P(self, data_name, x, one_component, is_tuples) result(error)
  !< Write `<DataArray... NumberOfComponents="n"...>...</DataArray>` tag (R4P).
  class(xml_writer_appended), intent(inout)        :: self           !< Writer.
  character(*),               intent(in)           :: data_name      !< Data name.
  real(R4P),                  intent(in)           :: x(1:,1:,1:,1:) !< Data variable.
  logical,                    intent(in), optional :: one_component  !< Force one component.
  logical,                    intent(in), optional :: is_tuples      !< Use "NumberOfTuples".
  integer(I4P)                                     :: error          !< Error status.
  character(len=:), allocatable                    :: data_type      !< Data type.
  integer(I4P)                                     :: n_components   !< Number of components.

  data_type = 'Float32'
  n_components = size(x, dim=1)
  if (present(one_component)) then
    if (one_component) n_components = 1
  endif
  call self%write_dataarray_tag_appended(data_type=data_type, number_of_components=n_components, data_name=data_name, &
                                         is_tuples=is_tuples)
  call self%ioffset_update(n_byte=self%write_on_scratch_dataarray(x=x))
  error = self%error
  endfunction write_dataarray1_rank4_R4P

  function write_dataarray1_rank4_I8P(self, data_name, x, one_component, is_tuples) result(error)
  !< Write `<DataArray... NumberOfComponents="n"...>...</DataArray>` tag (I8P).
  class(xml_writer_appended), intent(inout)        :: self           !< Writer.
  character(*),               intent(in)           :: data_name      !< Data name.
  integer(I8P),               intent(in)           :: x(1:,1:,1:,1:) !< Data variable.
  logical,                    intent(in), optional :: one_component  !< Force one component.
  logical,                    intent(in), optional :: is_tuples      !< Use "NumberOfTuples".
  integer(I4P)                                     :: error          !< Error status.
  character(len=:), allocatable                    :: data_type      !< Data type.
  integer(I4P)                                     :: n_components   !< Number of components.

  data_type = 'Int64'
  n_components = size(x, dim=1)
  if (present(one_component)) then
    if (one_component) n_components = 1
  endif
  call self%write_dataarray_tag_appended(data_type=data_type, number_of_components=n_components, data_name=data_name, &
                                         is_tuples=is_tuples)
  call self%ioffset_update(n_byte=self%write_on_scratch_dataarray(x=x))
  error = self%error
  endfunction write_dataarray1_rank4_I8P

  function write_dataarray1_rank4_I4P(self, data_name, x, one_component, is_tuples) result(error)
  !< Write `<DataArray... NumberOfComponents="n"...>...</DataArray>` tag (I4P).
  class(xml_writer_appended), intent(inout)        :: self           !< Writer.
  character(*),               intent(in)           :: data_name      !< Data name.
  integer(I4P),               intent(in)           :: x(1:,1:,1:,1:) !< Data variable.
  logical,                    intent(in), optional :: one_component  !< Force one component.
  logical,                    intent(in), optional :: is_tuples      !< Use "NumberOfTuples".
  integer(I4P)                                     :: error          !< Error status.
  character(len=:), allocatable                    :: data_type      !< Data type.
  integer(I4P)                                     :: n_components   !< Number of components.

  data_type = 'Int32'
  n_components = size(x, dim=1)
  if (present(one_component)) then
    if (one_component) n_components = 1
  endif
  call self%write_dataarray_tag_appended(data_type=data_type, number_of_components=n_components, data_name=data_name, &
                                         is_tuples=is_tuples)
  call self%ioffset_update(n_byte=self%write_on_scratch_dataarray(x=x))
  error = self%error
  endfunction write_dataarray1_rank4_I4P

  function write_dataarray1_rank4_I2P(self, data_name, x, one_component, is_tuples) result(error)
  !< Write `<DataArray... NumberOfComponents="n"...>...</DataArray>` tag (I2P).
  class(xml_writer_appended), intent(inout)        :: self           !< Writer.
  character(*),               intent(in)           :: data_name      !< Data name.
  integer(I2P),               intent(in)           :: x(1:,1:,1:,1:) !< Data variable.
  logical,                    intent(in), optional :: one_component  !< Force one component.
  logical,                    intent(in), optional :: is_tuples      !< Use "NumberOfTuples".
  integer(I4P)                                     :: error          !< Error status.
  character(len=:), allocatable                    :: data_type      !< Data type.
  integer(I4P)                                     :: n_components   !< Number of components.

  data_type = 'Int16'
  n_components = size(x, dim=1)
  if (present(one_component)) then
    if (one_component) n_components = 1
  endif
  call self%write_dataarray_tag_appended(data_type=data_type, number_of_components=n_components, data_name=data_name, &
                                         is_tuples=is_tuples)
  call self%ioffset_update(n_byte=self%write_on_scratch_dataarray(x=x))
  error = self%error
  endfunction write_dataarray1_rank4_I2P

  function write_dataarray1_rank4_I1P(self, data_name, x, one_component, is_tuples) result(error)
  !< Write `<DataArray... NumberOfComponents="n"...>...</DataArray>` tag (I1P).
  class(xml_writer_appended), intent(inout)        :: self           !< Writer.
  character(*),               intent(in)           :: data_name      !< Data name.
  integer(I1P),               intent(in)           :: x(1:,1:,1:,1:) !< Data variable.
  logical,                    intent(in), optional :: one_component  !< Force one component.
  logical,                    intent(in), optional :: is_tuples      !< Use "NumberOfTuples".
  integer(I4P)                                     :: error          !< Error status.
  character(len=:), allocatable                    :: data_type      !< Data type.
  integer(I4P)                                     :: n_components   !< Number of components.

  data_type = 'Int8'
  n_components = size(x, dim=1)
  if (present(one_component)) then
    if (one_component) n_components = 1
  endif
  call self%write_dataarray_tag_appended(data_type=data_type, number_of_components=n_components, data_name=data_name, &
                                         is_tuples=is_tuples)
  call self%ioffset_update(n_byte=self%write_on_scratch_dataarray(x=x))
  error = self%error
  endfunction write_dataarray1_rank4_I1P

  function write_dataarray3_rank1_R8P(self, data_name, x, y, z, is_tuples) result(error)
  !< Write `<DataArray... NumberOfComponents="3"...>...</DataArray>` tag (R8P).
  class(xml_writer_appended), intent(inout)        :: self         !< Writer.
  character(*),               intent(in)           :: data_name    !< Data name.
  real(R8P),                  intent(in)           :: x(1:)        !< X component of data variable.
  real(R8P),                  intent(in)           :: y(1:)        !< Y component of data variable.
  real(R8P),                  intent(in)           :: z(1:)        !< Z component of data variable.
  logical,                    intent(in), optional :: is_tuples    !< Use "NumberOfTuples" instead "NumberOfComponents" attribute.
  integer(I4P)                                     :: error        !< Error status.
  character(len=:), allocatable                    :: data_type    !< Data type.
  integer(I4P)                                     :: n_components !< Number of components.

  data_type = 'Float64'
  n_components = 3
  call self%write_dataarray_tag_appended(data_type=data_type, number_of_components=n_components, data_name=data_name, &
                                         is_tuples=is_tuples)
  call self%ioffset_update(n_byte=self%write_on_scratch_dataarray(x=x, y=y, z=z))
  error = self%error
  endfunction write_dataarray3_rank1_R8P

  function write_dataarray3_rank1_R4P(self, data_name, x, y, z, is_tuples) result(error)
  !< Write `<DataArray... NumberOfComponents="3"...>...</DataArray>` tag (R4P).
  class(xml_writer_appended), intent(inout)        :: self         !< Writer.
  character(*),               intent(in)           :: data_name    !< Data name.
  real(R4P),                  intent(in)           :: x(1:)        !< X component of data variable.
  real(R4P),                  intent(in)           :: y(1:)        !< Y component of data variable.
  real(R4P),                  intent(in)           :: z(1:)        !< Z component of data variable.
  logical,                    intent(in), optional :: is_tuples    !< Use "NumberOfTuples".
  integer(I4P)                                     :: error        !< Error status.
  character(len=:), allocatable                    :: data_type    !< Data type.
  integer(I4P)                                     :: n_components !< Number of components.

  data_type = 'Float32'
  n_components = 3
  call self%write_dataarray_tag_appended(data_type=data_type, number_of_components=n_components, data_name=data_name, &
                                         is_tuples=is_tuples)
  call self%ioffset_update(n_byte=self%write_on_scratch_dataarray(x=x, y=y, z=z))
  error = self%error
  endfunction write_dataarray3_rank1_R4P

  function write_dataarray3_rank1_I8P(self, data_name, x, y, z, is_tuples) result(error)
  !< Write `<DataArray... NumberOfComponents="3"...>...</DataArray>` tag (I8P).
  class(xml_writer_appended), intent(inout)        :: self         !< Writer.
  character(*),               intent(in)           :: data_name    !< Data name.
  integer(I8P),               intent(in)           :: x(1:)        !< X component of data variable.
  integer(I8P),               intent(in)           :: y(1:)        !< Y component of data variable.
  integer(I8P),               intent(in)           :: z(1:)        !< Z component of data variable.
  logical,                    intent(in), optional :: is_tuples    !< Use "NumberOfTuples".
  integer(I4P)                                     :: error        !< Error status.
  character(len=:), allocatable                    :: data_type    !< Data type.
  integer(I4P)                                     :: n_components !< Number of components.

  data_type = 'Int64'
  n_components = 3
  call self%write_dataarray_tag_appended(data_type=data_type, number_of_components=n_components, data_name=data_name, &
                                         is_tuples=is_tuples)
  call self%ioffset_update(n_byte=self%write_on_scratch_dataarray(x=x, y=y, z=z))
  error = self%error
  endfunction write_dataarray3_rank1_I8P

  function write_dataarray3_rank1_I4P(self, data_name, x, y, z, is_tuples) result(error)
  !< Write `<DataArray... NumberOfComponents="3"...>...</DataArray>` tag (I4P).
  class(xml_writer_appended), intent(inout)        :: self         !< Writer.
  character(*),               intent(in)           :: data_name    !< Data name.
  integer(I4P),               intent(in)           :: x(1:)        !< X component of data variable.
  integer(I4P),               intent(in)           :: y(1:)        !< Y component of data variable.
  integer(I4P),               intent(in)           :: z(1:)        !< Z component of data variable.
  logical,                    intent(in), optional :: is_tuples    !< Use "NumberOfTuples".
  integer(I4P)                                     :: error        !< Error status.
  character(len=:), allocatable                    :: data_type    !< Data type.
  integer(I4P)                                     :: n_components !< Number of components.

  data_type = 'Int32'
  n_components = 3
  call self%write_dataarray_tag_appended(data_type=data_type, number_of_components=n_components, data_name=data_name, &
                                         is_tuples=is_tuples)
  call self%ioffset_update(n_byte=self%write_on_scratch_dataarray(x=x, y=y, z=z))
  error = self%error
  endfunction write_dataarray3_rank1_I4P

  function write_dataarray3_rank1_I2P(self, data_name, x, y, z, is_tuples) result(error)
  !< Write `<DataArray... NumberOfComponents="3"...>...</DataArray>` tag (I2P).
  class(xml_writer_appended), intent(inout)        :: self         !< Writer.
  character(*),               intent(in)           :: data_name    !< Data name.
  integer(I2P),               intent(in)           :: x(1:)        !< X component of data variable.
  integer(I2P),               intent(in)           :: y(1:)        !< Y component of data variable.
  integer(I2P),               intent(in)           :: z(1:)        !< Z component of data variable.
  logical,                    intent(in), optional :: is_tuples    !< Use "NumberOfTuples".
  integer(I4P)                                     :: error        !< Error status.
  character(len=:), allocatable                    :: data_type    !< Data type.
  integer(I4P)                                     :: n_components !< Number of components.

  data_type = 'Int16'
  n_components = 3
  call self%write_dataarray_tag_appended(data_type=data_type, number_of_components=n_components, data_name=data_name, &
                                         is_tuples=is_tuples)
  call self%ioffset_update(n_byte=self%write_on_scratch_dataarray(x=x, y=y, z=z))
  error = self%error
  endfunction write_dataarray3_rank1_I2P

  function write_dataarray3_rank1_I1P(self, data_name, x, y, z, is_tuples) result(error)
  !< Write `<DataArray... NumberOfComponents="3"...>...</DataArray>` tag (I1P).
  class(xml_writer_appended), intent(inout)        :: self         !< Writer.
  character(*),               intent(in)           :: data_name    !< Data name.
  integer(I1P),               intent(in)           :: x(1:)        !< X component of data variable.
  integer(I1P),               intent(in)           :: y(1:)        !< Y component of data variable.
  integer(I1P),               intent(in)           :: z(1:)        !< Z component of data variable.
  logical,                    intent(in), optional :: is_tuples    !< Use "NumberOfTuples".
  integer(I4P)                                     :: error        !< Error status.
  character(len=:), allocatable                    :: data_type    !< Data type.
  integer(I4P)                                     :: n_components !< Number of components.

  data_type = 'Int8'
  n_components = 3
  call self%write_dataarray_tag_appended(data_type=data_type, number_of_components=n_components, data_name=data_name, &
                                         is_tuples=is_tuples)
  call self%ioffset_update(n_byte=self%write_on_scratch_dataarray(x=x, y=y, z=z))
  error = self%error
  endfunction write_dataarray3_rank1_I1P

  function write_dataarray3_rank3_R8P(self, data_name, x, y, z, is_tuples) result(error)
  !< Write `<DataArray... NumberOfComponents="3"...>...</DataArray>` tag (R8P).
  class(xml_writer_appended), intent(inout)        :: self         !< Writer.
  character(*),               intent(in)           :: data_name    !< Data name.
  real(R8P),                  intent(in)           :: x(1:,1:,1:)  !< X component of data variable.
  real(R8P),                  intent(in)           :: y(1:,1:,1:)  !< Y component of data variable.
  real(R8P),                  intent(in)           :: z(1:,1:,1:)  !< Z component of data variable.
  logical,                    intent(in), optional :: is_tuples    !< Use "NumberOfTuples".
  integer(I4P)                                     :: error        !< Error status.
  character(len=:), allocatable                    :: data_type    !< Data type.
  integer(I4P)                                     :: n_components !< Number of components.

  data_type = 'Float64'
  n_components = 3
  call self%write_dataarray_tag_appended(data_type=data_type, number_of_components=n_components, data_name=data_name, &
                                         is_tuples=is_tuples)
  call self%ioffset_update(n_byte=self%write_on_scratch_dataarray(x=x, y=y, z=z))
  error = self%error
  endfunction write_dataarray3_rank3_R8P

  function write_dataarray3_rank3_R4P(self, data_name, x, y, z, is_tuples) result(error)
  !< Write `<DataArray... NumberOfComponents="3"...>...</DataArray>` tag (R4P).
  class(xml_writer_appended), intent(inout)        :: self         !< Writer.
  character(*),               intent(in)           :: data_name    !< Data name.
  real(R4P),                  intent(in)           :: x(1:,1:,1:)  !< X component of data variable.
  real(R4P),                  intent(in)           :: y(1:,1:,1:)  !< Y component of data variable.
  real(R4P),                  intent(in)           :: z(1:,1:,1:)  !< Z component of data variable.
  logical,                    intent(in), optional :: is_tuples    !< Use "NumberOfTuples".
  integer(I4P)                                     :: error        !< Error status.
  character(len=:), allocatable                    :: data_type    !< Data type.
  integer(I4P)                                     :: n_components !< Number of components.

  data_type = 'Float32'
  n_components = 3
  call self%write_dataarray_tag_appended(data_type=data_type, number_of_components=n_components, data_name=data_name, &
                                         is_tuples=is_tuples)
  call self%ioffset_update(n_byte=self%write_on_scratch_dataarray(x=x, y=y, z=z))
  error = self%error
  endfunction write_dataarray3_rank3_R4P

  function write_dataarray3_rank3_I8P(self, data_name, x, y, z, is_tuples) result(error)
  !< Write `<DataArray... NumberOfComponents="3"...>...</DataArray>` tag (I8P).
  class(xml_writer_appended), intent(inout)        :: self         !< Writer.
  character(*),               intent(in)           :: data_name    !< Data name.
  integer(I8P),               intent(in)           :: x(1:,1:,1:)  !< X component of data variable.
  integer(I8P),               intent(in)           :: y(1:,1:,1:)  !< Y component of data variable.
  integer(I8P),               intent(in)           :: z(1:,1:,1:)  !< Z component of data variable.
  logical,                    intent(in), optional :: is_tuples    !< Use "NumberOfTuples".
  integer(I4P)                                     :: error        !< Error status.
  character(len=:), allocatable                    :: data_type    !< Data type.
  integer(I4P)                                     :: n_components !< Number of components.

  data_type = 'Int64'
  n_components = 3
  call self%write_dataarray_tag_appended(data_type=data_type, number_of_components=n_components, data_name=data_name, &
                                         is_tuples=is_tuples)
  call self%ioffset_update(n_byte=self%write_on_scratch_dataarray(x=x, y=y, z=z))
  error = self%error
  endfunction write_dataarray3_rank3_I8P

  function write_dataarray3_rank3_I4P(self, data_name, x, y, z, is_tuples) result(error)
  !< Write `<DataArray... NumberOfComponents="3"...>...</DataArray>` tag (I4P).
  class(xml_writer_appended), intent(inout)        :: self         !< Writer.
  character(*),               intent(in)           :: data_name    !< Data name.
  integer(I4P),               intent(in)           :: x(1:,1:,1:)  !< X component of data variable.
  integer(I4P),               intent(in)           :: y(1:,1:,1:)  !< Y component of data variable.
  integer(I4P),               intent(in)           :: z(1:,1:,1:)  !< Z component of data variable.
  logical,                    intent(in), optional :: is_tuples    !< Use "NumberOfTuples".
  integer(I4P)                                     :: error        !< Error status.
  character(len=:), allocatable                    :: data_type    !< Data type.
  integer(I4P)                                     :: n_components !< Number of components.

  data_type = 'Int32'
  n_components = 3
  call self%write_dataarray_tag_appended(data_type=data_type, number_of_components=n_components, data_name=data_name, &
                                         is_tuples=is_tuples)
  call self%ioffset_update(n_byte=self%write_on_scratch_dataarray(x=x, y=y, z=z))
  error = self%error
  endfunction write_dataarray3_rank3_I4P

  function write_dataarray3_rank3_I2P(self, data_name, x, y, z, is_tuples) result(error)
  !< Write `<DataArray... NumberOfComponents="3"...>...</DataArray>` tag (I2P).
  class(xml_writer_appended), intent(inout)        :: self         !< Writer.
  character(*),               intent(in)           :: data_name    !< Data name.
  integer(I2P),               intent(in)           :: x(1:,1:,1:)  !< X component of data variable.
  integer(I2P),               intent(in)           :: y(1:,1:,1:)  !< Y component of data variable.
  integer(I2P),               intent(in)           :: z(1:,1:,1:)  !< Z component of data variable.
  logical,                    intent(in), optional :: is_tuples    !< Use "NumberOfTuples".
  integer(I4P)                                     :: error        !< Error status.
  character(len=:), allocatable                    :: data_type    !< Data type.
  integer(I4P)                                     :: n_components !< Number of components.

  data_type = 'Int16'
  n_components = 3
  call self%write_dataarray_tag_appended(data_type=data_type, number_of_components=n_components, data_name=data_name, &
                                         is_tuples=is_tuples)
  call self%ioffset_update(n_byte=self%write_on_scratch_dataarray(x=x, y=y, z=z))
  error = self%error
  endfunction write_dataarray3_rank3_I2P

  function write_dataarray3_rank3_I1P(self, data_name, x, y, z, is_tuples) result(error)
  !< Write `<DataArray... NumberOfComponents="3"...>...</DataArray>` tag (I1P).
  class(xml_writer_appended), intent(inout)        :: self         !< Writer.
  character(*),               intent(in)           :: data_name    !< Data name.
  integer(I1P),               intent(in)           :: x(1:,1:,1:)  !< X component of data variable.
  integer(I1P),               intent(in)           :: y(1:,1:,1:)  !< Y component of data variable.
  integer(I1P),               intent(in)           :: z(1:,1:,1:)  !< Z component of data variable.
  logical,                    intent(in), optional :: is_tuples    !< Use "NumberOfTuples".
  integer(I4P)                                     :: error        !< Error status.
  character(len=:), allocatable                    :: data_type    !< Data type.
  integer(I4P)                                     :: n_components !< Number of components.

  data_type = 'Int8'
  n_components = 3
  call self%write_dataarray_tag_appended(data_type=data_type, number_of_components=n_components, data_name=data_name, &
                                         is_tuples=is_tuples)
  call self%ioffset_update(n_byte=self%write_on_scratch_dataarray(x=x, y=y, z=z))
  error = self%error
  endfunction write_dataarray3_rank3_I1P
  
  function write_dataarray6_rank1_R8P(self, data_name, u, v, w, x, y, z, is_tuples) result(error)
  !< Write `<DataArray... NumberOfComponents="3"...>...</DataArray>` tag (R8P).
  class(xml_writer_appended), intent(inout)        :: self         !< Writer.
  character(*),               intent(in)           :: data_name    !< Data name.
  real(R8P),                  intent(in)           :: u(1:)        !< U component of data variable.
  real(R8P),                  intent(in)           :: v(1:)        !< V component of data variable.
  real(R8P),                  intent(in)           :: w(1:)        !< W component of data variable.
  real(R8P),                  intent(in)           :: x(1:)        !< X component of data variable.
  real(R8P),                  intent(in)           :: y(1:)        !< Y component of data variable.
  real(R8P),                  intent(in)           :: z(1:)        !< Z component of data variable.
  logical,                    intent(in), optional :: is_tuples    !< Use "NumberOfTuples" instead "NumberOfComponents" attribute.
  integer(I4P)                                     :: error        !< Error status.
  character(len=:), allocatable                    :: data_type    !< Data type.
  integer(I4P)                                     :: n_components !< Number of components.

  data_type = 'Float64'
  n_components = 6
  call self%write_dataarray_tag_appended(data_type=data_type, number_of_components=n_components, data_name=data_name, &
                                         is_tuples=is_tuples)
  call self%ioffset_update(n_byte=self%write_on_scratch_dataarray(u=u, v=v, w=w, x=x, y=y, z=z))
  error = self%error
  endfunction write_dataarray6_rank1_R8P

  function write_dataarray6_rank1_R4P(self, data_name, u, v, w, x, y, z, is_tuples) result(error)
  !< Write `<DataArray... NumberOfComponents="3"...>...</DataArray>` tag (R4P).
  class(xml_writer_appended), intent(inout)        :: self         !< Writer.
  character(*),               intent(in)           :: data_name    !< Data name.
  real(R4P),                  intent(in)           :: u(1:)        !< U component of data variable.
  real(R4P),                  intent(in)           :: v(1:)        !< V component of data variable.
  real(R4P),                  intent(in)           :: w(1:)        !< W component of data variable.
  real(R4P),                  intent(in)           :: x(1:)        !< X component of data variable.
  real(R4P),                  intent(in)           :: y(1:)        !< Y component of data variable.
  real(R4P),                  intent(in)           :: z(1:)        !< Z component of data variable.
  logical,                    intent(in), optional :: is_tuples    !< Use "NumberOfTuples".
  integer(I4P)                                     :: error        !< Error status.
  character(len=:), allocatable                    :: data_type    !< Data type.
  integer(I4P)                                     :: n_components !< Number of components.

  data_type = 'Float32'
  n_components = 6
  call self%write_dataarray_tag_appended(data_type=data_type, number_of_components=n_components, data_name=data_name, &
                                         is_tuples=is_tuples)
  call self%ioffset_update(n_byte=self%write_on_scratch_dataarray(u=u, v=v, w=w, x=x, y=y, z=z))
  error = self%error
  endfunction write_dataarray6_rank1_R4P

  function write_dataarray6_rank1_I8P(self, data_name, u, v, w, x, y, z, is_tuples) result(error)
  !< Write `<DataArray... NumberOfComponents="6"...>...</DataArray>` tag (I8P).
  class(xml_writer_appended), intent(inout)        :: self         !< Writer.
  character(*),               intent(in)           :: data_name    !< Data name.
  integer(I8P),               intent(in)           :: u(1:)        !< U component of data variable.
  integer(I8P),               intent(in)           :: v(1:)        !< V component of data variable.
  integer(I8P),               intent(in)           :: w(1:)        !< W component of data variable.
  integer(I8P),               intent(in)           :: x(1:)        !< X component of data variable.
  integer(I8P),               intent(in)           :: y(1:)        !< Y component of data variable.
  integer(I8P),               intent(in)           :: z(1:)        !< Z component of data variable.
  logical,                    intent(in), optional :: is_tuples    !< Use "NumberOfTuples".
  integer(I4P)                                     :: error        !< Error status.
  character(len=:), allocatable                    :: data_type    !< Data type.
  integer(I4P)                                     :: n_components !< Number of components.

  data_type = 'Int64'
  n_components = 6
  call self%write_dataarray_tag_appended(data_type=data_type, number_of_components=n_components, data_name=data_name, &
                                         is_tuples=is_tuples)
  call self%ioffset_update(n_byte=self%write_on_scratch_dataarray(u=u, v=v, w=w, x=x, y=y, z=z))
  error = self%error
  endfunction write_dataarray6_rank1_I8P

  function write_dataarray6_rank1_I4P(self, data_name, u, v, w, x, y, z, is_tuples) result(error)
  !< Write `<DataArray... NumberOfComponents="6"...>...</DataArray>` tag (I4P).
  class(xml_writer_appended), intent(inout)        :: self         !< Writer.
  character(*),               intent(in)           :: data_name    !< Data name.
  integer(I4P),               intent(in)           :: u(1:)        !< U component of data variable.
  integer(I4P),               intent(in)           :: v(1:)        !< V component of data variable.
  integer(I4P),               intent(in)           :: w(1:)        !< W component of data variable.
  integer(I4P),               intent(in)           :: x(1:)        !< X component of data variable.
  integer(I4P),               intent(in)           :: y(1:)        !< Y component of data variable.
  integer(I4P),               intent(in)           :: z(1:)        !< Z component of data variable.
  logical,                    intent(in), optional :: is_tuples    !< Use "NumberOfTuples".
  integer(I4P)                                     :: error        !< Error status.
  character(len=:), allocatable                    :: data_type    !< Data type.
  integer(I4P)                                     :: n_components !< Number of components.

  data_type = 'Int32'
  n_components = 6
  call self%write_dataarray_tag_appended(data_type=data_type, number_of_components=n_components, data_name=data_name, &
                                         is_tuples=is_tuples)
  call self%ioffset_update(n_byte=self%write_on_scratch_dataarray(u=u, v=v, w=w, x=x, y=y, z=z))
  error = self%error
  endfunction write_dataarray6_rank1_I4P

  function write_dataarray6_rank1_I2P(self, data_name, u, v, w, x, y, z, is_tuples) result(error)
  !< Write `<DataArray... NumberOfComponents="6"...>...</DataArray>` tag (I2P).
  class(xml_writer_appended), intent(inout)        :: self         !< Writer.
  character(*),               intent(in)           :: data_name    !< Data name.
  integer(I2P),               intent(in)           :: u(1:)        !< U component of data variable.
  integer(I2P),               intent(in)           :: v(1:)        !< V component of data variable.
  integer(I2P),               intent(in)           :: w(1:)        !< W component of data variable.
  integer(I2P),               intent(in)           :: x(1:)        !< X component of data variable.
  integer(I2P),               intent(in)           :: y(1:)        !< Y component of data variable.
  integer(I2P),               intent(in)           :: z(1:)        !< Z component of data variable.
  logical,                    intent(in), optional :: is_tuples    !< Use "NumberOfTuples".
  integer(I4P)                                     :: error        !< Error status.
  character(len=:), allocatable                    :: data_type    !< Data type.
  integer(I4P)                                     :: n_components !< Number of components.

  data_type = 'Int16'
  n_components = 6
  call self%write_dataarray_tag_appended(data_type=data_type, number_of_components=n_components, data_name=data_name, &
                                         is_tuples=is_tuples)
  call self%ioffset_update(n_byte=self%write_on_scratch_dataarray(u=u, v=v, w=w, x=x, y=y, z=z))
  error = self%error
  endfunction write_dataarray6_rank1_I2P

  function write_dataarray6_rank1_I1P(self, data_name, u, v, w, x, y, z, is_tuples) result(error)
  !< Write `<DataArray... NumberOfComponents="6"...>...</DataArray>` tag (I1P).
  class(xml_writer_appended), intent(inout)        :: self         !< Writer.
  character(*),               intent(in)           :: data_name    !< Data name.
  integer(I1P),               intent(in)           :: u(1:)        !< U component of data variable.
  integer(I1P),               intent(in)           :: v(1:)        !< V component of data variable.
  integer(I1P),               intent(in)           :: w(1:)        !< W component of data variable.
  integer(I1P),               intent(in)           :: x(1:)        !< X component of data variable.
  integer(I1P),               intent(in)           :: y(1:)        !< Y component of data variable.
  integer(I1P),               intent(in)           :: z(1:)        !< Z component of data variable.
  logical,                    intent(in), optional :: is_tuples    !< Use "NumberOfTuples".
  integer(I4P)                                     :: error        !< Error status.
  character(len=:), allocatable                    :: data_type    !< Data type.
  integer(I4P)                                     :: n_components !< Number of components.

  data_type = 'Int8'
  n_components = 6
  call self%write_dataarray_tag_appended(data_type=data_type, number_of_components=n_components, data_name=data_name, &
                                         is_tuples=is_tuples)
  call self%ioffset_update(n_byte=self%write_on_scratch_dataarray(u=u, v=v, w=w, x=x, y=y, z=z))
  error = self%error
  endfunction write_dataarray6_rank1_I1P

  function write_dataarray6_rank3_R8P(self, data_name, u, v, w, x, y, z, is_tuples) result(error)
  !< Write `<DataArray... NumberOfComponents="6"...>...</DataArray>` tag (R8P).
  class(xml_writer_appended), intent(inout)        :: self         !< Writer.
  character(*),               intent(in)           :: data_name    !< Data name.
  real(R8P),                  intent(in)           :: u(1:,1:,1:)  !< U component of data variable.
  real(R8P),                  intent(in)           :: v(1:,1:,1:)  !< V component of data variable.
  real(R8P),                  intent(in)           :: w(1:,1:,1:)  !< W component of data variable.
  real(R8P),                  intent(in)           :: x(1:,1:,1:)  !< X component of data variable.
  real(R8P),                  intent(in)           :: y(1:,1:,1:)  !< Y component of data variable.
  real(R8P),                  intent(in)           :: z(1:,1:,1:)  !< Z component of data variable.
  logical,                    intent(in), optional :: is_tuples    !< Use "NumberOfTuples".
  integer(I4P)                                     :: error        !< Error status.
  character(len=:), allocatable                    :: data_type    !< Data type.
  integer(I4P)                                     :: n_components !< Number of components.

  data_type = 'Float64'
  n_components = 6
  call self%write_dataarray_tag_appended(data_type=data_type, number_of_components=n_components, data_name=data_name, &
                                         is_tuples=is_tuples)
  call self%ioffset_update(n_byte=self%write_on_scratch_dataarray(u=u, v=v, w=w, x=x, y=y, z=z))
  error = self%error
  endfunction write_dataarray6_rank3_R8P

  function write_dataarray6_rank3_R4P(self, data_name, u, v, w, x, y, z, is_tuples) result(error)
  !< Write `<DataArray... NumberOfComponents="6"...>...</DataArray>` tag (R4P).
  class(xml_writer_appended), intent(inout)        :: self         !< Writer.
  character(*),               intent(in)           :: data_name    !< Data name.
  real(R4P),                  intent(in)           :: u(1:,1:,1:)  !< U component of data variable.
  real(R4P),                  intent(in)           :: v(1:,1:,1:)  !< V component of data variable.
  real(R4P),                  intent(in)           :: w(1:,1:,1:)  !< W component of data variable.
  real(R4P),                  intent(in)           :: x(1:,1:,1:)  !< X component of data variable.
  real(R4P),                  intent(in)           :: y(1:,1:,1:)  !< Y component of data variable.
  real(R4P),                  intent(in)           :: z(1:,1:,1:)  !< Z component of data variable.
  logical,                    intent(in), optional :: is_tuples    !< Use "NumberOfTuples".
  integer(I4P)                                     :: error        !< Error status.
  character(len=:), allocatable                    :: data_type    !< Data type.
  integer(I4P)                                     :: n_components !< Number of components.

  data_type = 'Float32'
  n_components = 6
  call self%write_dataarray_tag_appended(data_type=data_type, number_of_components=n_components, data_name=data_name, &
                                         is_tuples=is_tuples)
  call self%ioffset_update(n_byte=self%write_on_scratch_dataarray(u=u, v=v, w=w, x=x, y=y, z=z))
  error = self%error
  endfunction write_dataarray6_rank3_R4P

  function write_dataarray6_rank3_I8P(self, data_name, u, v, w, x, y, z, is_tuples) result(error)
  !< Write `<DataArray... NumberOfComponents="3"...>...</DataArray>` tag (I8P).
  class(xml_writer_appended), intent(inout)        :: self         !< Writer.
  character(*),               intent(in)           :: data_name    !< Data name.
  integer(I8P),               intent(in)           :: u(1:,1:,1:)  !< U component of data variable.
  integer(I8P),               intent(in)           :: v(1:,1:,1:)  !< V component of data variable.
  integer(I8P),               intent(in)           :: w(1:,1:,1:)  !< W component of data variable.
  integer(I8P),               intent(in)           :: x(1:,1:,1:)  !< X component of data variable.
  integer(I8P),               intent(in)           :: y(1:,1:,1:)  !< Y component of data variable.
  integer(I8P),               intent(in)           :: z(1:,1:,1:)  !< Z component of data variable.
  logical,                    intent(in), optional :: is_tuples    !< Use "NumberOfTuples".
  integer(I4P)                                     :: error        !< Error status.
  character(len=:), allocatable                    :: data_type    !< Data type.
  integer(I4P)                                     :: n_components !< Number of components.

  data_type = 'Int64'
  n_components = 6
  call self%write_dataarray_tag_appended(data_type=data_type, number_of_components=n_components, data_name=data_name, &
                                         is_tuples=is_tuples)
  call self%ioffset_update(n_byte=self%write_on_scratch_dataarray(u=u, v=v, w=w, x=x, y=y, z=z))
  error = self%error
  endfunction write_dataarray6_rank3_I8P

  function write_dataarray6_rank3_I4P(self, data_name, u, v, w, x, y, z, is_tuples) result(error)
  !< Write `<DataArray... NumberOfComponents="6"...>...</DataArray>` tag (I4P).
  class(xml_writer_appended), intent(inout)        :: self         !< Writer.
  character(*),               intent(in)           :: data_name    !< Data name.
  integer(I4P),               intent(in)           :: u(1:,1:,1:)  !< U component of data variable.
  integer(I4P),               intent(in)           :: v(1:,1:,1:)  !< V component of data variable.
  integer(I4P),               intent(in)           :: w(1:,1:,1:)  !< W component of data variable.
  integer(I4P),               intent(in)           :: x(1:,1:,1:)  !< X component of data variable.
  integer(I4P),               intent(in)           :: y(1:,1:,1:)  !< Y component of data variable.
  integer(I4P),               intent(in)           :: z(1:,1:,1:)  !< Z component of data variable.
  logical,                    intent(in), optional :: is_tuples    !< Use "NumberOfTuples".
  integer(I4P)                                     :: error        !< Error status.
  character(len=:), allocatable                    :: data_type    !< Data type.
  integer(I4P)                                     :: n_components !< Number of components.

  data_type = 'Int32'
  n_components = 6
  call self%write_dataarray_tag_appended(data_type=data_type, number_of_components=n_components, data_name=data_name, &
                                         is_tuples=is_tuples)
  call self%ioffset_update(n_byte=self%write_on_scratch_dataarray(u=u, v=v, w=w, x=x, y=y, z=z))
  error = self%error
  endfunction write_dataarray6_rank3_I4P

  function write_dataarray6_rank3_I2P(self, data_name, u, v, w, x, y, z, is_tuples) result(error)
  !< Write `<DataArray... NumberOfComponents="6"...>...</DataArray>` tag (I2P).
  class(xml_writer_appended), intent(inout)        :: self         !< Writer.
  character(*),               intent(in)           :: data_name    !< Data name.
  integer(I2P),               intent(in)           :: u(1:,1:,1:)  !< U component of data variable.
  integer(I2P),               intent(in)           :: v(1:,1:,1:)  !< V component of data variable.
  integer(I2P),               intent(in)           :: w(1:,1:,1:)  !< W component of data variable.
  integer(I2P),               intent(in)           :: x(1:,1:,1:)  !< X component of data variable.
  integer(I2P),               intent(in)           :: y(1:,1:,1:)  !< Y component of data variable.
  integer(I2P),               intent(in)           :: z(1:,1:,1:)  !< Z component of data variable.
  logical,                    intent(in), optional :: is_tuples    !< Use "NumberOfTuples".
  integer(I4P)                                     :: error        !< Error status.
  character(len=:), allocatable                    :: data_type    !< Data type.
  integer(I4P)                                     :: n_components !< Number of components.

  data_type = 'Int16'
  n_components = 6
  call self%write_dataarray_tag_appended(data_type=data_type, number_of_components=n_components, data_name=data_name, &
                                         is_tuples=is_tuples)
  call self%ioffset_update(n_byte=self%write_on_scratch_dataarray(u=u, v=v, w=w, x=x, y=y, z=z))
  error = self%error
  endfunction write_dataarray6_rank3_I2P

  function write_dataarray6_rank3_I1P(self, data_name, u, v, w, x, y, z, is_tuples) result(error)
  !< Write `<DataArray... NumberOfComponents="6"...>...</DataArray>` tag (I1P).
  class(xml_writer_appended), intent(inout)        :: self         !< Writer.
  character(*),               intent(in)           :: data_name    !< Data name.
  integer(I1P),               intent(in)           :: u(1:,1:,1:)  !< U component of data variable.
  integer(I1P),               intent(in)           :: v(1:,1:,1:)  !< V component of data variable.
  integer(I1P),               intent(in)           :: w(1:,1:,1:)  !< W component of data variable.
  integer(I1P),               intent(in)           :: x(1:,1:,1:)  !< X component of data variable.
  integer(I1P),               intent(in)           :: y(1:,1:,1:)  !< Y component of data variable.
  integer(I1P),               intent(in)           :: z(1:,1:,1:)  !< Z component of data variable.
  logical,                    intent(in), optional :: is_tuples    !< Use "NumberOfTuples".
  integer(I4P)                                     :: error        !< Error status.
  character(len=:), allocatable                    :: data_type    !< Data type.
  integer(I4P)                                     :: n_components !< Number of components.

  data_type = 'Int8'
  n_components = 6
  call self%write_dataarray_tag_appended(data_type=data_type, number_of_components=n_components, data_name=data_name, &
                                         is_tuples=is_tuples)
  call self%ioffset_update(n_byte=self%write_on_scratch_dataarray(u=u, v=v, w=w, x=x, y=y, z=z))
  error = self%error
  endfunction write_dataarray6_rank3_I1P

  subroutine write_dataarray_appended(self)
  !< Do nothing, ascii data cannot be appended.
  class(xml_writer_appended), intent(inout) :: self              !< Writer.
  type(string)                              :: tag_attributes    !< Tag attributes.
  integer(I8P)                              :: n_byte            !< Bytes count.
  character(len=2)                          :: dataarray_type    !< Dataarray type = R8,R4,I8,I4,I2,I1.
  integer(I4P)                              :: dataarray_dim     !< Dataarray dimension.
  real(R8P),    allocatable                 :: dataarray_R8P(:)  !< Dataarray buffer of R8P.
  real(R4P),    allocatable                 :: dataarray_R4P(:)  !< Dataarray buffer of R4P.
  integer(I8P), allocatable                 :: dataarray_I8P(:)  !< Dataarray buffer of I8P.
  integer(I4P), allocatable                 :: dataarray_I4P(:)  !< Dataarray buffer of I4P.
  integer(I2P), allocatable                 :: dataarray_I2P(:)  !< Dataarray buffer of I2P.
  integer(I1P), allocatable                 :: dataarray_I1P(:)  !< Dataarray buffer of I1P.

  if (self%is_compressed) then
    ! In compressed mode, scratch contains a sequence of VTK compressed-block payloads:
    !   UInt32 numBlocks, blockSize, lastBlockSize
    !   UInt32 compressedSize[numBlocks]
    !   Byte  compressedBlockData...
    ! We stream them to the XML file preserving binary representation by reading/writing
    ! the same types.
    block
      integer(I8P)                    :: nb, bs, last, i
      integer(I8P), allocatable       :: comp_sizes(:)
      integer(c_signed_char), allocatable :: buf(:)

      call self%write_start_tag(name='AppendedData', attributes='encoding="raw"')
      write(unit=self%xml, iostat=self%error)'_'
      endfile(unit=self%scratch, iostat=self%error)
      rewind(unit=self%scratch, iostat=self%error)
      do
        read(unit=self%scratch, iostat=self%error) nb
        if (is_iostat_end(self%error)) exit
        if (self%error /= 0) exit
        read(unit=self%scratch, iostat=self%error) bs
        if (self%error /= 0) exit
        read(unit=self%scratch, iostat=self%error) last
        if (self%error /= 0) exit
        if (allocated(comp_sizes)) deallocate(comp_sizes)
        allocate(comp_sizes(1:nb))
        read(unit=self%scratch, iostat=self%error) comp_sizes
        if (self%error /= 0) exit

        if (self%is_uint64) then
          write(unit=self%xml, iostat=self%error) nb, bs, last, comp_sizes
        else
          write(unit=self%xml, iostat=self%error) int(nb, I4P), int(bs, I4P), int(last, I4P), int(comp_sizes, I4P)
        endif
        if (self%error /= 0) exit
        do i = 1, nb
          if (allocated(buf)) deallocate(buf)
          allocate(buf(1:comp_sizes(i)))
          read(unit=self%scratch, iostat=self%error) buf
          if (self%error /= 0) exit
          write(unit=self%xml, iostat=self%error) buf
          if (self%error /= 0) exit
        enddo
        if (self%error /= 0) exit
      enddo
      if (allocated(comp_sizes)) deallocate(comp_sizes)
      if (allocated(buf)) deallocate(buf)
      close(unit=self%scratch, iostat=self%error)
      write(unit=self%xml, iostat=self%error)end_rec
      call self%write_end_tag(name='AppendedData')
    endblock
    return
  endif

  call self%write_start_tag(name='AppendedData', attributes='encoding="'//self%encoding%chars()//'"')
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
  call self%write_end_tag(name='AppendedData')
  contains
    subroutine read_dataarray_from_scratch
    !< Read the current dataaray from scratch file.

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
    endsubroutine read_dataarray_from_scratch

    subroutine write_dataarray_on_xml
    !< Write the current dataaray on xml file.
    character(len=:), allocatable  :: code !< Dataarray encoded with Base64 codec.

    if (self%encoding=='raw') then
      select case(dataarray_type)
      case('R8')
        call write_n_byte
        write(unit=self%xml, iostat=self%error)dataarray_R8P
        deallocate(dataarray_R8P)
      case('R4')
        call write_n_byte
        write(unit=self%xml, iostat=self%error)dataarray_R4P
        deallocate(dataarray_R4P)
      case('I8')
        call write_n_byte
        write(unit=self%xml, iostat=self%error)dataarray_I8P
        deallocate(dataarray_I8P)
      case('I4')
        call write_n_byte
        write(unit=self%xml, iostat=self%error)dataarray_I4P
        deallocate(dataarray_I4P)
      case('I2')
        call write_n_byte
        write(unit=self%xml, iostat=self%error)dataarray_I2P
        deallocate(dataarray_I2P)
      case('I1')
        call write_n_byte
        write(unit=self%xml, iostat=self%error)dataarray_I1P
        deallocate(dataarray_I1P)
      endselect
    else
      select case(dataarray_type)
      case('R8')
        code = encode_binary_dataarray(x=dataarray_R8P, is_uint64=self%is_uint64)
        write(unit=self%xml, iostat=self%error)code
      case('R4')
        code = encode_binary_dataarray(x=dataarray_R4P, is_uint64=self%is_uint64)
        write(unit=self%xml, iostat=self%error)code
      case('I8')
        code = encode_binary_dataarray(x=dataarray_I8P, is_uint64=self%is_uint64)
        write(unit=self%xml, iostat=self%error)code
      case('I4')
        code = encode_binary_dataarray(x=dataarray_I4P, is_uint64=self%is_uint64)
        write(unit=self%xml, iostat=self%error)code
      case('I2')
        code = encode_binary_dataarray(x=dataarray_I2P, is_uint64=self%is_uint64)
        write(unit=self%xml, iostat=self%error)code
      case('I1')
        code = encode_binary_dataarray(x=dataarray_I1P, is_uint64=self%is_uint64)
        write(unit=self%xml, iostat=self%error)code
      endselect
    endif
    endsubroutine write_dataarray_on_xml

    subroutine write_n_byte
    !< Write the bytes count header of the current raw dataarray, UInt32 or UInt64.

    if (self%is_uint64) then
      write(unit=self%xml, iostat=self%error)n_byte
    else
      write(unit=self%xml, iostat=self%error)int(n_byte, I4P)
    endif
    endsubroutine write_n_byte
  endsubroutine write_dataarray_appended

#ifdef VTKFORTRAN_USE_ZLIB
  function write_zlib_compressed_payload_from_bytes(self, bytes) result(n_written)
  !< Write a VTK "compressed blocks" payload to the main scratch stream.
  !<
  !< Payload layout (UIntXX is UInt32 or UInt64, as the header_type of the file):
  !<   UIntXX numBlocks
  !<   UIntXX blockSize
  !<   UIntXX lastBlockSize
  !<   UIntXX compressedSize[numBlocks]
  !<   Byte   compressedBlockData...
  !<
  !< The header values are stored on the scratch file as I8P and written with the width of the file header when the appended
  !< section is written; offsets are 64-bit, so payloads larger than 2 GiB are handled.
  class(xml_writer_appended), intent(inout)   :: self          !< Writer.
  integer(c_signed_char),     intent(in)      :: bytes(1:)     !< Uncompressed payload bytes.
  integer(I8P)                                :: n_written     !< Total payload bytes written.
  integer(I8P)                                :: bs            !< Block size.
  integer(I8P)                                :: nb            !< Number of blocks.
  integer(I8P)                                :: last          !< Size of the last block.
  integer(I8P)                                :: i             !< Counter.
  integer(I8P)                                :: n_read        !< Bytes of the current block.
  integer(I8P), allocatable                   :: comp_sizes(:) !< Compressed size of each block.
  integer(c_signed_char), allocatable, target :: inbuf(:)      !< Uncompressed block.
  integer(c_signed_char), allocatable, target :: outbuf(:)     !< Compressed block.
  integer(c_long)                             :: bound         !< Bound of compressed block size.
  integer(c_long), target                     :: destLen       !< Compressed block size.
  integer(c_int)                              :: zret          !< zlib return code.

  bs = int(self%compression_block_size, I8P)
  if (bs <= 0_I8P) bs = 32768_I8P
  nb = (size(bytes, dim=1, kind=I8P) + bs - 1_I8P) / bs
  last = size(bytes, dim=1, kind=I8P) - (nb - 1_I8P) * bs
  if (nb < 1_I8P) nb = 1_I8P
  if (last < 0_I8P) last = 0_I8P

  allocate(comp_sizes(1:nb))
  allocate(inbuf(1:bs))
  bound = zlib_compress_bound(int(bs, c_long))
  allocate(outbuf(1:int(bound, I8P)))

  ! Pass 1: compute compressed sizes per block
  do i = 1_I8P, nb
    n_read = merge(bs, last, i < nb)
    if (n_read <= 0_I8P) n_read = 0_I8P
    if (n_read > 0_I8P) inbuf(1:n_read) = bytes((i-1_I8P)*bs + 1_I8P : (i-1_I8P)*bs + n_read)
    destLen = int(size(outbuf), c_long)
    zret = zlib_compress2(dst=outbuf, dst_len=destLen, src=inbuf, src_len=int(n_read, c_long), level=self%compression_level)
    if (zret /= 0) then
      self%error = 1
      exit
    endif
    comp_sizes(i) = int(destLen, I8P)
  enddo

  ! Header
  write(unit=self%scratch, iostat=self%error) nb
  write(unit=self%scratch, iostat=self%error) bs
  write(unit=self%scratch, iostat=self%error) last
  write(unit=self%scratch, iostat=self%error) comp_sizes

  ! Pass 2: write compressed blocks
  do i = 1_I8P, nb
    n_read = merge(bs, last, i < nb)
    if (n_read <= 0_I8P) n_read = 0_I8P
    if (n_read > 0_I8P) inbuf(1:n_read) = bytes((i-1_I8P)*bs + 1_I8P : (i-1_I8P)*bs + n_read)
    destLen = int(size(outbuf), c_long)
    zret = zlib_compress2(dst=outbuf, dst_len=destLen, src=inbuf, src_len=int(n_read, c_long), level=self%compression_level)
    if (zret /= 0) then
      self%error = 1
      exit
    endif
    write(unit=self%scratch, iostat=self%error) outbuf(1:int(destLen, I8P))
    if (self%error /= 0) exit
  enddo

  n_written = (3_I8P + nb) * merge(int(BYI8P, I8P), int(BYI4P, I8P), self%is_uint64) + sum(comp_sizes)
  deallocate(comp_sizes, inbuf, outbuf)
  endfunction write_zlib_compressed_payload_from_bytes
#endif

  ! write_on_scratch_dataarray methods
  function write_on_scratch_dataarray1_rank1(self, x) result(n_byte)
  !< Write a dataarray with 1 components of rank 1.
  class(xml_writer_appended), intent(inout) :: self   !< Writer.
  class(*),                   intent(in)    :: x(1:)  !< Data variable.
  integer(I8P)                              :: n_byte !< Number of bytes
  integer(I4P)                              :: nn     !< Number of elements.
  integer(I4P)                              :: tmp    !< Temporary stream unit.

  nn = size(x, dim=1)
  select type(x)
  type is(real(R8P))
    n_byte = self%n_bytes(size(x, kind=I8P)*BYR8P)
    if (self%is_compressed) then
#ifdef VTKFORTRAN_USE_ZLIB
      block
        integer(c_signed_char), allocatable :: bytes(:)
        allocate(bytes(1:n_byte))
        call to_bytes(x=x, bytes=bytes)
        n_byte = write_zlib_compressed_payload_from_bytes(self=self, bytes=bytes)
        deallocate(bytes)
      endblock
#else
      self%error = 1
      n_byte = 0
#endif
    else
      write(unit=self%scratch, iostat=self%error)n_byte, 'R8', nn
      write(unit=self%scratch, iostat=self%error)x
    endif
  type is(real(R4P))
    n_byte = self%n_bytes(size(x, kind=I8P)*BYR4P)
    if (self%is_compressed) then
#ifdef VTKFORTRAN_USE_ZLIB
      block
        integer(c_signed_char), allocatable :: bytes(:)
        allocate(bytes(1:n_byte))
        call to_bytes(x=x, bytes=bytes)
        n_byte = write_zlib_compressed_payload_from_bytes(self=self, bytes=bytes)
        deallocate(bytes)
      endblock
#else
      self%error = 1
      n_byte = 0
#endif
    else
      write(unit=self%scratch, iostat=self%error)n_byte, 'R4', nn
      write(unit=self%scratch, iostat=self%error)x
    endif
  type is(integer(I8P))
    n_byte = self%n_bytes(size(x, kind=I8P)*BYI8P)
    if (self%is_compressed) then
#ifdef VTKFORTRAN_USE_ZLIB
      block
        integer(c_signed_char), allocatable :: bytes(:)
        allocate(bytes(1:n_byte))
        call to_bytes(x=x, bytes=bytes)
        n_byte = write_zlib_compressed_payload_from_bytes(self=self, bytes=bytes)
        deallocate(bytes)
      endblock
#else
      self%error = 1
      n_byte = 0
#endif
    else
      write(unit=self%scratch, iostat=self%error)n_byte, 'I8', nn
      write(unit=self%scratch, iostat=self%error)x
    endif
  type is(integer(I4P))
    n_byte = self%n_bytes(size(x, kind=I8P)*BYI4P)
    if (self%is_compressed) then
#ifdef VTKFORTRAN_USE_ZLIB
      block
        integer(c_signed_char), allocatable :: bytes(:)
        allocate(bytes(1:n_byte))
        call to_bytes(x=x, bytes=bytes)
        n_byte = write_zlib_compressed_payload_from_bytes(self=self, bytes=bytes)
        deallocate(bytes)
      endblock
#else
      self%error = 1
      n_byte = 0
#endif
    else
      write(unit=self%scratch, iostat=self%error)n_byte, 'I4', nn
      write(unit=self%scratch, iostat=self%error)x
    endif
  type is(integer(I2P))
    n_byte = self%n_bytes(size(x, kind=I8P)*BYI2P)
    if (self%is_compressed) then
#ifdef VTKFORTRAN_USE_ZLIB
      block
        integer(c_signed_char), allocatable :: bytes(:)
        allocate(bytes(1:n_byte))
        call to_bytes(x=x, bytes=bytes)
        n_byte = write_zlib_compressed_payload_from_bytes(self=self, bytes=bytes)
        deallocate(bytes)
      endblock
#else
      self%error = 1
      n_byte = 0
#endif
    else
      write(unit=self%scratch, iostat=self%error)n_byte, 'I2', nn
      write(unit=self%scratch, iostat=self%error)x
    endif
  type is(integer(I1P))
    n_byte = self%n_bytes(size(x, kind=I8P)*BYI1P)
    if (self%is_compressed) then
#ifdef VTKFORTRAN_USE_ZLIB
      block
        integer(c_signed_char), allocatable :: bytes(:)
        allocate(bytes(1:n_byte))
        call to_bytes(x=x, bytes=bytes)
        n_byte = write_zlib_compressed_payload_from_bytes(self=self, bytes=bytes)
        deallocate(bytes)
      endblock
#else
      self%error = 1
      n_byte = 0
#endif
    else
      write(unit=self%scratch, iostat=self%error)n_byte, 'I1', nn
      write(unit=self%scratch, iostat=self%error)x
    endif
  endselect
  endfunction write_on_scratch_dataarray1_rank1

  function write_on_scratch_dataarray1_rank2(self, x) result(n_byte)
  !< Write a dataarray with 1 components of rank 2.
  class(xml_writer_appended), intent(inout) :: self     !< Writer.
  class(*),                   intent(in)    :: x(1:,1:) !< Data variable.
  integer(I8P)                              :: n_byte   !< Number of bytes
  integer(I4P)                              :: nn       !< Number of elements.
  integer(I4P)                              :: tmp      !< Temporary stream unit.

  nn = size(x, dim=1)*size(x, dim=2)
  select type(x)
  type is(real(R8P))
    n_byte = self%n_bytes(size(x, kind=I8P)*BYR8P)
    if (self%is_compressed) then
#ifdef VTKFORTRAN_USE_ZLIB
      block
        integer(c_signed_char), allocatable :: bytes(:)
        real(R8P), allocatable :: xx(:)
        allocate(bytes(1:n_byte))
        xx = reshape(x, [size(x, kind=I8P)])
        call to_bytes(x=xx, bytes=bytes)
        n_byte = write_zlib_compressed_payload_from_bytes(self=self, bytes=bytes)
        deallocate(bytes)
      endblock
#else
      self%error = 1 ; n_byte = 0
#endif
    else
      write(unit=self%scratch, iostat=self%error)n_byte, 'R8', nn
      write(unit=self%scratch, iostat=self%error)x
    endif
  type is(real(R4P))
    n_byte = self%n_bytes(size(x, kind=I8P)*BYR4P)
    if (self%is_compressed) then
#ifdef VTKFORTRAN_USE_ZLIB
      block
        integer(c_signed_char), allocatable :: bytes(:)
        real(R4P), allocatable :: xx(:)
        allocate(bytes(1:n_byte))
        xx = reshape(x, [size(x, kind=I8P)])
        call to_bytes(x=xx, bytes=bytes)
        n_byte = write_zlib_compressed_payload_from_bytes(self=self, bytes=bytes)
        deallocate(bytes)
      endblock
#else
      self%error = 1 ; n_byte = 0
#endif
    else
      write(unit=self%scratch, iostat=self%error)n_byte, 'R4', nn
      write(unit=self%scratch, iostat=self%error)x
    endif
  type is(integer(I8P))
    n_byte = self%n_bytes(size(x, kind=I8P)*BYI8P)
    if (self%is_compressed) then
#ifdef VTKFORTRAN_USE_ZLIB
      block
        integer(c_signed_char), allocatable :: bytes(:)
        integer(I8P), allocatable :: xx(:)
        allocate(bytes(1:n_byte))
        xx = reshape(x, [size(x, kind=I8P)])
        call to_bytes(x=xx, bytes=bytes)
        n_byte = write_zlib_compressed_payload_from_bytes(self=self, bytes=bytes)
        deallocate(bytes)
      endblock
#else
      self%error = 1 ; n_byte = 0
#endif
    else
      write(unit=self%scratch, iostat=self%error)n_byte, 'I8', nn
      write(unit=self%scratch, iostat=self%error)x
    endif
  type is(integer(I4P))
    n_byte = self%n_bytes(size(x, kind=I8P)*BYI4P)
    if (self%is_compressed) then
#ifdef VTKFORTRAN_USE_ZLIB
      block
        integer(c_signed_char), allocatable :: bytes(:)
        integer(I4P), allocatable :: xx(:)
        allocate(bytes(1:n_byte))
        xx = reshape(x, [size(x, kind=I8P)])
        call to_bytes(x=xx, bytes=bytes)
        n_byte = write_zlib_compressed_payload_from_bytes(self=self, bytes=bytes)
        deallocate(bytes)
      endblock
#else
      self%error = 1 ; n_byte = 0
#endif
    else
      write(unit=self%scratch, iostat=self%error)n_byte, 'I4', nn
      write(unit=self%scratch, iostat=self%error)x
    endif
  type is(integer(I2P))
    n_byte = self%n_bytes(size(x, kind=I8P)*BYI2P)
    if (self%is_compressed) then
#ifdef VTKFORTRAN_USE_ZLIB
      block
        integer(c_signed_char), allocatable :: bytes(:)
        integer(I2P), allocatable :: xx(:)
        allocate(bytes(1:n_byte))
        xx = reshape(x, [size(x, kind=I8P)])
        call to_bytes(x=xx, bytes=bytes)
        n_byte = write_zlib_compressed_payload_from_bytes(self=self, bytes=bytes)
        deallocate(bytes)
      endblock
#else
      self%error = 1 ; n_byte = 0
#endif
    else
      write(unit=self%scratch, iostat=self%error)n_byte, 'I2', nn
      write(unit=self%scratch, iostat=self%error)x
    endif
  type is(integer(I1P))
    n_byte = self%n_bytes(size(x, kind=I8P)*BYI1P)
    if (self%is_compressed) then
#ifdef VTKFORTRAN_USE_ZLIB
      block
        integer(c_signed_char), allocatable :: bytes(:)
        integer(I1P), allocatable :: xx(:)
        allocate(bytes(1:n_byte))
        xx = reshape(x, [size(x, kind=I8P)])
        call to_bytes(x=xx, bytes=bytes)
        n_byte = write_zlib_compressed_payload_from_bytes(self=self, bytes=bytes)
        deallocate(bytes)
      endblock
#else
      self%error = 1 ; n_byte = 0
#endif
    else
      write(unit=self%scratch, iostat=self%error)n_byte, 'I1', nn
      write(unit=self%scratch, iostat=self%error)x
    endif
  endselect
  endfunction write_on_scratch_dataarray1_rank2

  function write_on_scratch_dataarray1_rank3(self, x) result(n_byte)
  !< Write a dataarray with 1 components of rank 3.
  class(xml_writer_appended), intent(inout) :: self        !< Writer.
  class(*),                   intent(in)    :: x(1:,1:,1:) !< Data variable.
  integer(I8P)                              :: n_byte      !< Number of bytes
  integer(I4P)                              :: nn          !< Number of elements.
  integer(I4P)                              :: tmp         !< Temporary stream unit.

  nn = size(x, dim=1)*size(x, dim=2)*size(x, dim=3)
  select type(x)
  type is(real(R8P))
    n_byte = self%n_bytes(size(x, kind=I8P)*BYR8P)
    if (self%is_compressed) then
#ifdef VTKFORTRAN_USE_ZLIB
      block
        integer(c_signed_char), allocatable :: bytes(:)
        real(R8P), allocatable :: xx(:)
        allocate(bytes(1:n_byte))
        xx = reshape(x, [size(x, kind=I8P)])
        call to_bytes(x=xx, bytes=bytes)
        n_byte = write_zlib_compressed_payload_from_bytes(self=self, bytes=bytes)
        deallocate(bytes)
      endblock
#else
      self%error = 1 ; n_byte = 0
#endif
    else
      write(unit=self%scratch, iostat=self%error)n_byte, 'R8', nn
      write(unit=self%scratch, iostat=self%error)x
    endif
  type is(real(R4P))
    n_byte = self%n_bytes(size(x, kind=I8P)*BYR4P)
    if (self%is_compressed) then
#ifdef VTKFORTRAN_USE_ZLIB
      block
        integer(c_signed_char), allocatable :: bytes(:)
        real(R4P), allocatable :: xx(:)
        allocate(bytes(1:n_byte))
        xx = reshape(x, [size(x, kind=I8P)])
        call to_bytes(x=xx, bytes=bytes)
        n_byte = write_zlib_compressed_payload_from_bytes(self=self, bytes=bytes)
        deallocate(bytes)
      endblock
#else
      self%error = 1 ; n_byte = 0
#endif
    else
      write(unit=self%scratch, iostat=self%error)n_byte, 'R4', nn
      write(unit=self%scratch, iostat=self%error)x
    endif
  type is(integer(I8P))
    n_byte = self%n_bytes(size(x, kind=I8P)*BYI8P)
    if (self%is_compressed) then
#ifdef VTKFORTRAN_USE_ZLIB
      block
        integer(c_signed_char), allocatable :: bytes(:)
        integer(I8P), allocatable :: xx(:)
        allocate(bytes(1:n_byte))
        xx = reshape(x, [size(x, kind=I8P)])
        call to_bytes(x=xx, bytes=bytes)
        n_byte = write_zlib_compressed_payload_from_bytes(self=self, bytes=bytes)
        deallocate(bytes)
      endblock
#else
      self%error = 1 ; n_byte = 0
#endif
    else
      write(unit=self%scratch, iostat=self%error)n_byte, 'I8', nn
      write(unit=self%scratch, iostat=self%error)x
    endif
  type is(integer(I4P))
    n_byte = self%n_bytes(size(x, kind=I8P)*BYI4P)
    if (self%is_compressed) then
#ifdef VTKFORTRAN_USE_ZLIB
      block
        integer(c_signed_char), allocatable :: bytes(:)
        integer(I4P), allocatable :: xx(:)
        allocate(bytes(1:n_byte))
        xx = reshape(x, [size(x, kind=I8P)])
        call to_bytes(x=xx, bytes=bytes)
        n_byte = write_zlib_compressed_payload_from_bytes(self=self, bytes=bytes)
        deallocate(bytes)
      endblock
#else
      self%error = 1 ; n_byte = 0
#endif
    else
      write(unit=self%scratch, iostat=self%error)n_byte, 'I4', nn
      write(unit=self%scratch, iostat=self%error)x
    endif
  type is(integer(I2P))
    n_byte = self%n_bytes(size(x, kind=I8P)*BYI2P)
    if (self%is_compressed) then
#ifdef VTKFORTRAN_USE_ZLIB
      block
        integer(c_signed_char), allocatable :: bytes(:)
        integer(I2P), allocatable :: xx(:)
        allocate(bytes(1:n_byte))
        xx = reshape(x, [size(x, kind=I8P)])
        call to_bytes(x=xx, bytes=bytes)
        n_byte = write_zlib_compressed_payload_from_bytes(self=self, bytes=bytes)
        deallocate(bytes)
      endblock
#else
      self%error = 1 ; n_byte = 0
#endif
    else
      write(unit=self%scratch, iostat=self%error)n_byte, 'I2', nn
      write(unit=self%scratch, iostat=self%error)x
    endif
  type is(integer(I1P))
    n_byte = self%n_bytes(size(x, kind=I8P)*BYI1P)
    if (self%is_compressed) then
#ifdef VTKFORTRAN_USE_ZLIB
      block
        integer(c_signed_char), allocatable :: bytes(:)
        integer(I1P), allocatable :: xx(:)
        allocate(bytes(1:n_byte))
        xx = reshape(x, [size(x, kind=I8P)])
        call to_bytes(x=xx, bytes=bytes)
        n_byte = write_zlib_compressed_payload_from_bytes(self=self, bytes=bytes)
        deallocate(bytes)
      endblock
#else
      self%error = 1 ; n_byte = 0
#endif
    else
      write(unit=self%scratch, iostat=self%error)n_byte, 'I1', nn
      write(unit=self%scratch, iostat=self%error)x
    endif
  endselect
  endfunction write_on_scratch_dataarray1_rank3

  function write_on_scratch_dataarray1_rank4(self, x) result(n_byte)
  !< Write a dataarray with 1 components of rank 4.
  class(xml_writer_appended), intent(inout) :: self           !< Writer.
  class(*),                   intent(in)    :: x(1:,1:,1:,1:) !< Data variable.
  integer(I8P)                              :: n_byte         !< Number of bytes
  integer(I4P)                              :: nn             !< Number of elements.
  integer(I4P)                              :: tmp            !< Temporary stream unit.

  nn = size(x, dim=1)*size(x, dim=2)*size(x, dim=3)*size(x, dim=4)
  select type(x)
  type is(real(R8P))
    n_byte = self%n_bytes(size(x, kind=I8P)*BYR8P)
    if (self%is_compressed) then
#ifdef VTKFORTRAN_USE_ZLIB
      block
        integer(c_signed_char), allocatable :: bytes(:)
        real(R8P), allocatable :: xx(:)
        allocate(bytes(1:n_byte))
        xx = reshape(x, [size(x, kind=I8P)])
        call to_bytes(x=xx, bytes=bytes)
        n_byte = write_zlib_compressed_payload_from_bytes(self=self, bytes=bytes)
        deallocate(bytes)
      endblock
#else
      self%error = 1 ; n_byte = 0
#endif
    else
      write(unit=self%scratch, iostat=self%error)n_byte, 'R8', nn
      write(unit=self%scratch, iostat=self%error)x
    endif
  type is(real(R4P))
    n_byte = self%n_bytes(size(x, kind=I8P)*BYR4P)
    if (self%is_compressed) then
#ifdef VTKFORTRAN_USE_ZLIB
      block
        integer(c_signed_char), allocatable :: bytes(:)
        real(R4P), allocatable :: xx(:)
        allocate(bytes(1:n_byte))
        xx = reshape(x, [size(x, kind=I8P)])
        call to_bytes(x=xx, bytes=bytes)
        n_byte = write_zlib_compressed_payload_from_bytes(self=self, bytes=bytes)
        deallocate(bytes)
      endblock
#else
      self%error = 1 ; n_byte = 0
#endif
    else
      write(unit=self%scratch, iostat=self%error)n_byte, 'R4', nn
      write(unit=self%scratch, iostat=self%error)x
    endif
  type is(integer(I8P))
    n_byte = self%n_bytes(size(x, kind=I8P)*BYI8P)
    if (self%is_compressed) then
#ifdef VTKFORTRAN_USE_ZLIB
      block
        integer(c_signed_char), allocatable :: bytes(:)
        integer(I8P), allocatable :: xx(:)
        allocate(bytes(1:n_byte))
        xx = reshape(x, [size(x, kind=I8P)])
        call to_bytes(x=xx, bytes=bytes)
        n_byte = write_zlib_compressed_payload_from_bytes(self=self, bytes=bytes)
        deallocate(bytes)
      endblock
#else
      self%error = 1 ; n_byte = 0
#endif
    else
      write(unit=self%scratch, iostat=self%error)n_byte, 'I8', nn
      write(unit=self%scratch, iostat=self%error)x
    endif
  type is(integer(I4P))
    n_byte = self%n_bytes(size(x, kind=I8P)*BYI4P)
    if (self%is_compressed) then
#ifdef VTKFORTRAN_USE_ZLIB
      block
        integer(c_signed_char), allocatable :: bytes(:)
        integer(I4P), allocatable :: xx(:)
        allocate(bytes(1:n_byte))
        xx = reshape(x, [size(x, kind=I8P)])
        call to_bytes(x=xx, bytes=bytes)
        n_byte = write_zlib_compressed_payload_from_bytes(self=self, bytes=bytes)
        deallocate(bytes)
      endblock
#else
      self%error = 1 ; n_byte = 0
#endif
    else
      write(unit=self%scratch, iostat=self%error)n_byte, 'I4', nn
      write(unit=self%scratch, iostat=self%error)x
    endif
  type is(integer(I2P))
    n_byte = self%n_bytes(size(x, kind=I8P)*BYI2P)
    if (self%is_compressed) then
#ifdef VTKFORTRAN_USE_ZLIB
      block
        integer(c_signed_char), allocatable :: bytes(:)
        integer(I2P), allocatable :: xx(:)
        allocate(bytes(1:n_byte))
        xx = reshape(x, [size(x, kind=I8P)])
        call to_bytes(x=xx, bytes=bytes)
        n_byte = write_zlib_compressed_payload_from_bytes(self=self, bytes=bytes)
        deallocate(bytes)
      endblock
#else
      self%error = 1 ; n_byte = 0
#endif
    else
      write(unit=self%scratch, iostat=self%error)n_byte, 'I2', nn
      write(unit=self%scratch, iostat=self%error)x
    endif
  type is(integer(I1P))
    n_byte = self%n_bytes(size(x, kind=I8P)*BYI1P)
    if (self%is_compressed) then
#ifdef VTKFORTRAN_USE_ZLIB
      block
        integer(c_signed_char), allocatable :: bytes(:)
        integer(I1P), allocatable :: xx(:)
        allocate(bytes(1:n_byte))
        xx = reshape(x, [size(x, kind=I8P)])
        call to_bytes(x=xx, bytes=bytes)
        n_byte = write_zlib_compressed_payload_from_bytes(self=self, bytes=bytes)
        deallocate(bytes)
      endblock
#else
      self%error = 1 ; n_byte = 0
#endif
    else
      write(unit=self%scratch, iostat=self%error)n_byte, 'I1', nn
      write(unit=self%scratch, iostat=self%error)x
    endif
  endselect
  endfunction write_on_scratch_dataarray1_rank4

  function write_on_scratch_dataarray3_rank1_R8P(self, x, y, z) result(n_byte)
  !< Write a dataarray with 3 components of rank 1 (R8P).
  class(xml_writer_appended), intent(inout) :: self   !< Writer.
  real(R8P),                  intent(in)    :: x(1:)  !< X component.
  real(R8P),                  intent(in)    :: y(1:)  !< Y component.
  real(R8P),                  intent(in)    :: z(1:)  !< Z component.
  integer(I8P)                              :: n_byte !< Number of bytes
  real(R8P), allocatable                    :: buf(:) !< Interleaved components.
  integer(I8P)                              :: nn     !< Number of elements.

  nn = size(x, kind=I8P)
  allocate(buf(1:3*nn))
  buf(1::3) = x
  buf(2::3) = y
  buf(3::3) = z
  n_byte = self%write_on_scratch_dataarray(x=buf)
  endfunction write_on_scratch_dataarray3_rank1_R8P

  function write_on_scratch_dataarray3_rank1_R4P(self, x, y, z) result(n_byte)
  !< Write a dataarray with 3 components of rank 1 (R4P).
  class(xml_writer_appended), intent(inout) :: self   !< Writer.
  real(R4P),                  intent(in)    :: x(1:)  !< X component.
  real(R4P),                  intent(in)    :: y(1:)  !< Y component.
  real(R4P),                  intent(in)    :: z(1:)  !< Z component.
  integer(I8P)                              :: n_byte !< Number of bytes
  real(R4P), allocatable                    :: buf(:) !< Interleaved components.
  integer(I8P)                              :: nn     !< Number of elements.

  nn = size(x, kind=I8P)
  allocate(buf(1:3*nn))
  buf(1::3) = x
  buf(2::3) = y
  buf(3::3) = z
  n_byte = self%write_on_scratch_dataarray(x=buf)
  endfunction write_on_scratch_dataarray3_rank1_R4P

  function write_on_scratch_dataarray3_rank1_I8P(self, x, y, z) result(n_byte)
  !< Write a dataarray with 3 components of rank 1 (I8P).
  class(xml_writer_appended), intent(inout) :: self   !< Writer.
  integer(I8P),               intent(in)    :: x(1:)  !< X component.
  integer(I8P),               intent(in)    :: y(1:)  !< Y component.
  integer(I8P),               intent(in)    :: z(1:)  !< Z component.
  integer(I8P)                              :: n_byte !< Number of bytes
  integer(I8P), allocatable                 :: buf(:) !< Interleaved components.
  integer(I8P)                              :: nn     !< Number of elements.

  nn = size(x, kind=I8P)
  allocate(buf(1:3*nn))
  buf(1::3) = x
  buf(2::3) = y
  buf(3::3) = z
  n_byte = self%write_on_scratch_dataarray(x=buf)
  endfunction write_on_scratch_dataarray3_rank1_I8P

  function write_on_scratch_dataarray3_rank1_I4P(self, x, y, z) result(n_byte)
  !< Write a dataarray with 3 components of rank 1 (I4P).
  class(xml_writer_appended), intent(inout) :: self   !< Writer.
  integer(I4P),               intent(in)    :: x(1:)  !< X component.
  integer(I4P),               intent(in)    :: y(1:)  !< Y component.
  integer(I4P),               intent(in)    :: z(1:)  !< Z component.
  integer(I8P)                              :: n_byte !< Number of bytes
  integer(I4P), allocatable                 :: buf(:) !< Interleaved components.
  integer(I8P)                              :: nn     !< Number of elements.

  nn = size(x, kind=I8P)
  allocate(buf(1:3*nn))
  buf(1::3) = x
  buf(2::3) = y
  buf(3::3) = z
  n_byte = self%write_on_scratch_dataarray(x=buf)
  endfunction write_on_scratch_dataarray3_rank1_I4P

  function write_on_scratch_dataarray3_rank1_I2P(self, x, y, z) result(n_byte)
  !< Write a dataarray with 3 components of rank 1 (I2P).
  class(xml_writer_appended), intent(inout) :: self   !< Writer.
  integer(I2P),               intent(in)    :: x(1:)  !< X component.
  integer(I2P),               intent(in)    :: y(1:)  !< Y component.
  integer(I2P),               intent(in)    :: z(1:)  !< Z component.
  integer(I8P)                              :: n_byte !< Number of bytes
  integer(I2P), allocatable                 :: buf(:) !< Interleaved components.
  integer(I8P)                              :: nn     !< Number of elements.

  nn = size(x, kind=I8P)
  allocate(buf(1:3*nn))
  buf(1::3) = x
  buf(2::3) = y
  buf(3::3) = z
  n_byte = self%write_on_scratch_dataarray(x=buf)
  endfunction write_on_scratch_dataarray3_rank1_I2P

  function write_on_scratch_dataarray3_rank1_I1P(self, x, y, z) result(n_byte)
  !< Write a dataarray with 3 components of rank 1 (I1P).
  class(xml_writer_appended), intent(inout) :: self   !< Writer.
  integer(I1P),               intent(in)    :: x(1:)  !< X component.
  integer(I1P),               intent(in)    :: y(1:)  !< Y component.
  integer(I1P),               intent(in)    :: z(1:)  !< Z component.
  integer(I8P)                              :: n_byte !< Number of bytes
  integer(I1P), allocatable                 :: buf(:) !< Interleaved components.
  integer(I8P)                              :: nn     !< Number of elements.

  nn = size(x, kind=I8P)
  allocate(buf(1:3*nn))
  buf(1::3) = x
  buf(2::3) = y
  buf(3::3) = z
  n_byte = self%write_on_scratch_dataarray(x=buf)
  endfunction write_on_scratch_dataarray3_rank1_I1P

  function write_on_scratch_dataarray3_rank2_R8P(self, x, y, z) result(n_byte)
  !< Write a dataarray with 3 components of rank 2 (R8P).
  class(xml_writer_appended), intent(inout) :: self     !< Writer.
  real(R8P),                  intent(in)    :: x(1:,1:) !< X component.
  real(R8P),                  intent(in)    :: y(1:,1:) !< Y component.
  real(R8P),                  intent(in)    :: z(1:,1:) !< Z component.
  integer(I8P)                              :: n_byte   !< Number of bytes
  real(R8P), allocatable                    :: buf(:)   !< Interleaved components.
  integer(I8P)                              :: nn       !< Number of elements.

  nn = size(x, kind=I8P)
  allocate(buf(1:3*nn))
  buf(1::3) = reshape(x, [nn])
  buf(2::3) = reshape(y, [nn])
  buf(3::3) = reshape(z, [nn])
  n_byte = self%write_on_scratch_dataarray(x=buf)
  endfunction write_on_scratch_dataarray3_rank2_R8P

  function write_on_scratch_dataarray3_rank2_R4P(self, x, y, z) result(n_byte)
  !< Write a dataarray with 3 components of rank 2 (R4P).
  class(xml_writer_appended), intent(inout) :: self     !< Writer.
  real(R4P),                  intent(in)    :: x(1:,1:) !< X component.
  real(R4P),                  intent(in)    :: y(1:,1:) !< Y component.
  real(R4P),                  intent(in)    :: z(1:,1:) !< Z component.
  integer(I8P)                              :: n_byte   !< Number of bytes
  real(R4P), allocatable                    :: buf(:)   !< Interleaved components.
  integer(I8P)                              :: nn       !< Number of elements.

  nn = size(x, kind=I8P)
  allocate(buf(1:3*nn))
  buf(1::3) = reshape(x, [nn])
  buf(2::3) = reshape(y, [nn])
  buf(3::3) = reshape(z, [nn])
  n_byte = self%write_on_scratch_dataarray(x=buf)
  endfunction write_on_scratch_dataarray3_rank2_R4P

  function write_on_scratch_dataarray3_rank2_I8P(self, x, y, z) result(n_byte)
  !< Write a dataarray with 3 components of rank 2 (I8P).
  class(xml_writer_appended), intent(inout) :: self     !< Writer.
  integer(I8P),               intent(in)    :: x(1:,1:) !< X component.
  integer(I8P),               intent(in)    :: y(1:,1:) !< Y component.
  integer(I8P),               intent(in)    :: z(1:,1:) !< Z component.
  integer(I8P)                              :: n_byte   !< Number of bytes
  integer(I8P), allocatable                 :: buf(:)   !< Interleaved components.
  integer(I8P)                              :: nn       !< Number of elements.

  nn = size(x, kind=I8P)
  allocate(buf(1:3*nn))
  buf(1::3) = reshape(x, [nn])
  buf(2::3) = reshape(y, [nn])
  buf(3::3) = reshape(z, [nn])
  n_byte = self%write_on_scratch_dataarray(x=buf)
  endfunction write_on_scratch_dataarray3_rank2_I8P

  function write_on_scratch_dataarray3_rank2_I4P(self, x, y, z) result(n_byte)
  !< Write a dataarray with 3 components of rank 2 (I4P).
  class(xml_writer_appended), intent(inout) :: self     !< Writer.
  integer(I4P),               intent(in)    :: x(1:,1:) !< X component.
  integer(I4P),               intent(in)    :: y(1:,1:) !< Y component.
  integer(I4P),               intent(in)    :: z(1:,1:) !< Z component.
  integer(I8P)                              :: n_byte   !< Number of bytes
  integer(I4P), allocatable                 :: buf(:)   !< Interleaved components.
  integer(I8P)                              :: nn       !< Number of elements.

  nn = size(x, kind=I8P)
  allocate(buf(1:3*nn))
  buf(1::3) = reshape(x, [nn])
  buf(2::3) = reshape(y, [nn])
  buf(3::3) = reshape(z, [nn])
  n_byte = self%write_on_scratch_dataarray(x=buf)
  endfunction write_on_scratch_dataarray3_rank2_I4P

  function write_on_scratch_dataarray3_rank2_I2P(self, x, y, z) result(n_byte)
  !< Write a dataarray with 3 components of rank 2 (I2P).
  class(xml_writer_appended), intent(inout) :: self     !< Writer.
  integer(I2P),               intent(in)    :: x(1:,1:) !< X component.
  integer(I2P),               intent(in)    :: y(1:,1:) !< Y component.
  integer(I2P),               intent(in)    :: z(1:,1:) !< Z component.
  integer(I8P)                              :: n_byte   !< Number of bytes
  integer(I2P), allocatable                 :: buf(:)   !< Interleaved components.
  integer(I8P)                              :: nn       !< Number of elements.

  nn = size(x, kind=I8P)
  allocate(buf(1:3*nn))
  buf(1::3) = reshape(x, [nn])
  buf(2::3) = reshape(y, [nn])
  buf(3::3) = reshape(z, [nn])
  n_byte = self%write_on_scratch_dataarray(x=buf)
  endfunction write_on_scratch_dataarray3_rank2_I2P

  function write_on_scratch_dataarray3_rank2_I1P(self, x, y, z) result(n_byte)
  !< Write a dataarray with 3 components of rank 2 (I1P).
  class(xml_writer_appended), intent(inout) :: self     !< Writer.
  integer(I1P),               intent(in)    :: x(1:,1:) !< X component.
  integer(I1P),               intent(in)    :: y(1:,1:) !< Y component.
  integer(I1P),               intent(in)    :: z(1:,1:) !< Z component.
  integer(I8P)                              :: n_byte   !< Number of bytes
  integer(I1P), allocatable                 :: buf(:)   !< Interleaved components.
  integer(I8P)                              :: nn       !< Number of elements.

  nn = size(x, kind=I8P)
  allocate(buf(1:3*nn))
  buf(1::3) = reshape(x, [nn])
  buf(2::3) = reshape(y, [nn])
  buf(3::3) = reshape(z, [nn])
  n_byte = self%write_on_scratch_dataarray(x=buf)
  endfunction write_on_scratch_dataarray3_rank2_I1P

  function write_on_scratch_dataarray3_rank3_R8P(self, x, y, z) result(n_byte)
  !< Write a dataarray with 3 components of rank 3 (R8P).
  class(xml_writer_appended), intent(inout) :: self        !< Writer.
  real(R8P),                  intent(in)    :: x(1:,1:,1:) !< X component.
  real(R8P),                  intent(in)    :: y(1:,1:,1:) !< Y component.
  real(R8P),                  intent(in)    :: z(1:,1:,1:) !< Z component.
  integer(I8P)                              :: n_byte      !< Number of bytes
  real(R8P), allocatable                    :: buf(:)      !< Interleaved components.
  integer(I8P)                              :: nn          !< Number of elements.

  nn = size(x, kind=I8P)
  allocate(buf(1:3*nn))
  buf(1::3) = reshape(x, [nn])
  buf(2::3) = reshape(y, [nn])
  buf(3::3) = reshape(z, [nn])
  n_byte = self%write_on_scratch_dataarray(x=buf)
  endfunction write_on_scratch_dataarray3_rank3_R8P

  function write_on_scratch_dataarray3_rank3_R4P(self, x, y, z) result(n_byte)
  !< Write a dataarray with 3 components of rank 3 (R4P).
  class(xml_writer_appended), intent(inout) :: self        !< Writer.
  real(R4P),                  intent(in)    :: x(1:,1:,1:) !< X component.
  real(R4P),                  intent(in)    :: y(1:,1:,1:) !< Y component.
  real(R4P),                  intent(in)    :: z(1:,1:,1:) !< Z component.
  integer(I8P)                              :: n_byte      !< Number of bytes
  real(R4P), allocatable                    :: buf(:)      !< Interleaved components.
  integer(I8P)                              :: nn          !< Number of elements.

  nn = size(x, kind=I8P)
  allocate(buf(1:3*nn))
  buf(1::3) = reshape(x, [nn])
  buf(2::3) = reshape(y, [nn])
  buf(3::3) = reshape(z, [nn])
  n_byte = self%write_on_scratch_dataarray(x=buf)
  endfunction write_on_scratch_dataarray3_rank3_R4P

  function write_on_scratch_dataarray3_rank3_I8P(self, x, y, z) result(n_byte)
  !< Write a dataarray with 3 components of rank 3 (I8P).
  class(xml_writer_appended), intent(inout) :: self        !< Writer.
  integer(I8P),               intent(in)    :: x(1:,1:,1:) !< X component.
  integer(I8P),               intent(in)    :: y(1:,1:,1:) !< Y component.
  integer(I8P),               intent(in)    :: z(1:,1:,1:) !< Z component.
  integer(I8P)                              :: n_byte      !< Number of bytes
  integer(I8P), allocatable                 :: buf(:)      !< Interleaved components.
  integer(I8P)                              :: nn          !< Number of elements.

  nn = size(x, kind=I8P)
  allocate(buf(1:3*nn))
  buf(1::3) = reshape(x, [nn])
  buf(2::3) = reshape(y, [nn])
  buf(3::3) = reshape(z, [nn])
  n_byte = self%write_on_scratch_dataarray(x=buf)
  endfunction write_on_scratch_dataarray3_rank3_I8P

  function write_on_scratch_dataarray3_rank3_I4P(self, x, y, z) result(n_byte)
  !< Write a dataarray with 3 components of rank 3 (I4P).
  class(xml_writer_appended), intent(inout) :: self        !< Writer.
  integer(I4P),               intent(in)    :: x(1:,1:,1:) !< X component.
  integer(I4P),               intent(in)    :: y(1:,1:,1:) !< Y component.
  integer(I4P),               intent(in)    :: z(1:,1:,1:) !< Z component.
  integer(I8P)                              :: n_byte      !< Number of bytes
  integer(I4P), allocatable                 :: buf(:)      !< Interleaved components.
  integer(I8P)                              :: nn          !< Number of elements.

  nn = size(x, kind=I8P)
  allocate(buf(1:3*nn))
  buf(1::3) = reshape(x, [nn])
  buf(2::3) = reshape(y, [nn])
  buf(3::3) = reshape(z, [nn])
  n_byte = self%write_on_scratch_dataarray(x=buf)
  endfunction write_on_scratch_dataarray3_rank3_I4P

  function write_on_scratch_dataarray3_rank3_I2P(self, x, y, z) result(n_byte)
  !< Write a dataarray with 3 components of rank 3 (I2P).
  class(xml_writer_appended), intent(inout) :: self        !< Writer.
  integer(I2P),               intent(in)    :: x(1:,1:,1:) !< X component.
  integer(I2P),               intent(in)    :: y(1:,1:,1:) !< Y component.
  integer(I2P),               intent(in)    :: z(1:,1:,1:) !< Z component.
  integer(I8P)                              :: n_byte      !< Number of bytes
  integer(I2P), allocatable                 :: buf(:)      !< Interleaved components.
  integer(I8P)                              :: nn          !< Number of elements.

  nn = size(x, kind=I8P)
  allocate(buf(1:3*nn))
  buf(1::3) = reshape(x, [nn])
  buf(2::3) = reshape(y, [nn])
  buf(3::3) = reshape(z, [nn])
  n_byte = self%write_on_scratch_dataarray(x=buf)
  endfunction write_on_scratch_dataarray3_rank3_I2P

  function write_on_scratch_dataarray3_rank3_I1P(self, x, y, z) result(n_byte)
  !< Write a dataarray with 3 components of rank 3 (I1P).
  class(xml_writer_appended), intent(inout) :: self        !< Writer.
  integer(I1P),               intent(in)    :: x(1:,1:,1:) !< X component.
  integer(I1P),               intent(in)    :: y(1:,1:,1:) !< Y component.
  integer(I1P),               intent(in)    :: z(1:,1:,1:) !< Z component.
  integer(I8P)                              :: n_byte      !< Number of bytes
  integer(I1P), allocatable                 :: buf(:)      !< Interleaved components.
  integer(I8P)                              :: nn          !< Number of elements.

  nn = size(x, kind=I8P)
  allocate(buf(1:3*nn))
  buf(1::3) = reshape(x, [nn])
  buf(2::3) = reshape(y, [nn])
  buf(3::3) = reshape(z, [nn])
  n_byte = self%write_on_scratch_dataarray(x=buf)
  endfunction write_on_scratch_dataarray3_rank3_I1P
  
  function write_on_scratch_dataarray6_rank1_R8P(self, u, v, w, x, y, z) result(n_byte)
  !< Write a dataarray with 6 components of rank 1 (R8P).
  class(xml_writer_appended), intent(inout) :: self   !< Writer.
  real(R8P),                  intent(in)    :: u(1:)  !< U component.
  real(R8P),                  intent(in)    :: v(1:)  !< V component.
  real(R8P),                  intent(in)    :: w(1:)  !< W component.
  real(R8P),                  intent(in)    :: x(1:)  !< X component.
  real(R8P),                  intent(in)    :: y(1:)  !< Y component.
  real(R8P),                  intent(in)    :: z(1:)  !< Z component.
  integer(I8P)                              :: n_byte !< Number of bytes
  real(R8P), allocatable                    :: buf(:) !< Interleaved components.
  integer(I8P)                              :: nn     !< Number of elements.

  nn = size(x, kind=I8P)
  allocate(buf(1:6*nn))
  buf(1::6) = u
  buf(2::6) = v
  buf(3::6) = w
  buf(4::6) = x
  buf(5::6) = y
  buf(6::6) = z
  n_byte = self%write_on_scratch_dataarray(x=buf)
  endfunction write_on_scratch_dataarray6_rank1_R8P

  function write_on_scratch_dataarray6_rank1_R4P(self, u, v, w, x, y, z) result(n_byte)
  !< Write a dataarray with 6 components of rank 1 (R4P).
  class(xml_writer_appended), intent(inout) :: self   !< Writer.
  real(R4P),                  intent(in)    :: u(1:)  !< U component.
  real(R4P),                  intent(in)    :: v(1:)  !< V component.
  real(R4P),                  intent(in)    :: w(1:)  !< W component.
  real(R4P),                  intent(in)    :: x(1:)  !< X component.
  real(R4P),                  intent(in)    :: y(1:)  !< Y component.
  real(R4P),                  intent(in)    :: z(1:)  !< Z component.
  integer(I8P)                              :: n_byte !< Number of bytes
  real(R4P), allocatable                    :: buf(:) !< Interleaved components.
  integer(I8P)                              :: nn     !< Number of elements.

  nn = size(x, kind=I8P)
  allocate(buf(1:6*nn))
  buf(1::6) = u
  buf(2::6) = v
  buf(3::6) = w
  buf(4::6) = x
  buf(5::6) = y
  buf(6::6) = z
  n_byte = self%write_on_scratch_dataarray(x=buf)
  endfunction write_on_scratch_dataarray6_rank1_R4P

  function write_on_scratch_dataarray6_rank1_I8P(self, u, v, w, x, y, z) result(n_byte)
  !< Write a dataarray with 6 components of rank 1 (I8P).
  class(xml_writer_appended), intent(inout) :: self   !< Writer.
  integer(I8P),               intent(in)    :: u(1:)  !< U component.
  integer(I8P),               intent(in)    :: v(1:)  !< V component.
  integer(I8P),               intent(in)    :: w(1:)  !< W component.
  integer(I8P),               intent(in)    :: x(1:)  !< X component.
  integer(I8P),               intent(in)    :: y(1:)  !< Y component.
  integer(I8P),               intent(in)    :: z(1:)  !< Z component.
  integer(I8P)                              :: n_byte !< Number of bytes
  integer(I8P), allocatable                 :: buf(:) !< Interleaved components.
  integer(I8P)                              :: nn     !< Number of elements.

  nn = size(x, kind=I8P)
  allocate(buf(1:6*nn))
  buf(1::6) = u
  buf(2::6) = v
  buf(3::6) = w
  buf(4::6) = x
  buf(5::6) = y
  buf(6::6) = z
  n_byte = self%write_on_scratch_dataarray(x=buf)
  endfunction write_on_scratch_dataarray6_rank1_I8P

  function write_on_scratch_dataarray6_rank1_I4P(self, u, v, w, x, y, z) result(n_byte)
  !< Write a dataarray with 6 components of rank 1 (I4P).
  class(xml_writer_appended), intent(inout) :: self   !< Writer.
  integer(I4P),               intent(in)    :: u(1:)  !< U component.
  integer(I4P),               intent(in)    :: v(1:)  !< V component.
  integer(I4P),               intent(in)    :: w(1:)  !< W component.
  integer(I4P),               intent(in)    :: x(1:)  !< X component.
  integer(I4P),               intent(in)    :: y(1:)  !< Y component.
  integer(I4P),               intent(in)    :: z(1:)  !< Z component.
  integer(I8P)                              :: n_byte !< Number of bytes
  integer(I4P), allocatable                 :: buf(:) !< Interleaved components.
  integer(I8P)                              :: nn     !< Number of elements.

  nn = size(x, kind=I8P)
  allocate(buf(1:6*nn))
  buf(1::6) = u
  buf(2::6) = v
  buf(3::6) = w
  buf(4::6) = x
  buf(5::6) = y
  buf(6::6) = z
  n_byte = self%write_on_scratch_dataarray(x=buf)
  endfunction write_on_scratch_dataarray6_rank1_I4P

  function write_on_scratch_dataarray6_rank1_I2P(self, u, v, w, x, y, z) result(n_byte)
  !< Write a dataarray with 6 components of rank 1 (I2P).
  class(xml_writer_appended), intent(inout) :: self   !< Writer.
  integer(I2P),               intent(in)    :: u(1:)  !< U component.
  integer(I2P),               intent(in)    :: v(1:)  !< V component.
  integer(I2P),               intent(in)    :: w(1:)  !< W component.
  integer(I2P),               intent(in)    :: x(1:)  !< X component.
  integer(I2P),               intent(in)    :: y(1:)  !< Y component.
  integer(I2P),               intent(in)    :: z(1:)  !< Z component.
  integer(I8P)                              :: n_byte !< Number of bytes
  integer(I2P), allocatable                 :: buf(:) !< Interleaved components.
  integer(I8P)                              :: nn     !< Number of elements.

  nn = size(x, kind=I8P)
  allocate(buf(1:6*nn))
  buf(1::6) = u
  buf(2::6) = v
  buf(3::6) = w
  buf(4::6) = x
  buf(5::6) = y
  buf(6::6) = z
  n_byte = self%write_on_scratch_dataarray(x=buf)
  endfunction write_on_scratch_dataarray6_rank1_I2P

  function write_on_scratch_dataarray6_rank1_I1P(self, u, v, w, x, y, z) result(n_byte)
  !< Write a dataarray with 6 components of rank 1 (I1P).
  class(xml_writer_appended), intent(inout) :: self   !< Writer.
  integer(I1P),               intent(in)    :: u(1:)  !< U component.
  integer(I1P),               intent(in)    :: v(1:)  !< V component.
  integer(I1P),               intent(in)    :: w(1:)  !< W component.
  integer(I1P),               intent(in)    :: x(1:)  !< X component.
  integer(I1P),               intent(in)    :: y(1:)  !< Y component.
  integer(I1P),               intent(in)    :: z(1:)  !< Z component.
  integer(I8P)                              :: n_byte !< Number of bytes
  integer(I1P), allocatable                 :: buf(:) !< Interleaved components.
  integer(I8P)                              :: nn     !< Number of elements.

  nn = size(x, kind=I8P)
  allocate(buf(1:6*nn))
  buf(1::6) = u
  buf(2::6) = v
  buf(3::6) = w
  buf(4::6) = x
  buf(5::6) = y
  buf(6::6) = z
  n_byte = self%write_on_scratch_dataarray(x=buf)
  endfunction write_on_scratch_dataarray6_rank1_I1P

  function write_on_scratch_dataarray6_rank2_R8P(self, u, v, w, x, y, z) result(n_byte)
  !< Write a dataarray with 6 components of rank 2 (R8P).
  class(xml_writer_appended), intent(inout) :: self     !< Writer.
  real(R8P),                  intent(in)    :: u(1:,1:) !< U component.
  real(R8P),                  intent(in)    :: v(1:,1:) !< V component.
  real(R8P),                  intent(in)    :: w(1:,1:) !< W component.
  real(R8P),                  intent(in)    :: x(1:,1:) !< X component.
  real(R8P),                  intent(in)    :: y(1:,1:) !< Y component.
  real(R8P),                  intent(in)    :: z(1:,1:) !< Z component.
  integer(I8P)                              :: n_byte   !< Number of bytes
  real(R8P), allocatable                    :: buf(:)   !< Interleaved components.
  integer(I8P)                              :: nn       !< Number of elements.

  nn = size(x, kind=I8P)
  allocate(buf(1:6*nn))
  buf(1::6) = reshape(u, [nn])
  buf(2::6) = reshape(v, [nn])
  buf(3::6) = reshape(w, [nn])
  buf(4::6) = reshape(x, [nn])
  buf(5::6) = reshape(y, [nn])
  buf(6::6) = reshape(z, [nn])
  n_byte = self%write_on_scratch_dataarray(x=buf)
  endfunction write_on_scratch_dataarray6_rank2_R8P

  function write_on_scratch_dataarray6_rank2_R4P(self, u, v, w, x, y, z) result(n_byte)
  !< Write a dataarray with 6 components of rank 2 (R4P).
  class(xml_writer_appended), intent(inout) :: self     !< Writer.
  real(R4P),                  intent(in)    :: u(1:,1:) !< U component.
  real(R4P),                  intent(in)    :: v(1:,1:) !< V component.
  real(R4P),                  intent(in)    :: w(1:,1:) !< W component.
  real(R4P),                  intent(in)    :: x(1:,1:) !< X component.
  real(R4P),                  intent(in)    :: y(1:,1:) !< Y component.
  real(R4P),                  intent(in)    :: z(1:,1:) !< Z component.
  integer(I8P)                              :: n_byte   !< Number of bytes
  real(R4P), allocatable                    :: buf(:)   !< Interleaved components.
  integer(I8P)                              :: nn       !< Number of elements.

  nn = size(x, kind=I8P)
  allocate(buf(1:6*nn))
  buf(1::6) = reshape(u, [nn])
  buf(2::6) = reshape(v, [nn])
  buf(3::6) = reshape(w, [nn])
  buf(4::6) = reshape(x, [nn])
  buf(5::6) = reshape(y, [nn])
  buf(6::6) = reshape(z, [nn])
  n_byte = self%write_on_scratch_dataarray(x=buf)
  endfunction write_on_scratch_dataarray6_rank2_R4P

  function write_on_scratch_dataarray6_rank2_I8P(self, u, v, w, x, y, z) result(n_byte)
  !< Write a dataarray with 6 components of rank 2 (I8P).
  class(xml_writer_appended), intent(inout) :: self     !< Writer.
  integer(I8P),               intent(in)    :: u(1:,1:) !< U component.
  integer(I8P),               intent(in)    :: v(1:,1:) !< V component.
  integer(I8P),               intent(in)    :: w(1:,1:) !< W component.
  integer(I8P),               intent(in)    :: x(1:,1:) !< X component.
  integer(I8P),               intent(in)    :: y(1:,1:) !< Y component.
  integer(I8P),               intent(in)    :: z(1:,1:) !< Z component.
  integer(I8P)                              :: n_byte   !< Number of bytes
  integer(I8P), allocatable                 :: buf(:)   !< Interleaved components.
  integer(I8P)                              :: nn       !< Number of elements.

  nn = size(x, kind=I8P)
  allocate(buf(1:6*nn))
  buf(1::6) = reshape(u, [nn])
  buf(2::6) = reshape(v, [nn])
  buf(3::6) = reshape(w, [nn])
  buf(4::6) = reshape(x, [nn])
  buf(5::6) = reshape(y, [nn])
  buf(6::6) = reshape(z, [nn])
  n_byte = self%write_on_scratch_dataarray(x=buf)
  endfunction write_on_scratch_dataarray6_rank2_I8P

  function write_on_scratch_dataarray6_rank2_I4P(self, u, v, w, x, y, z) result(n_byte)
  !< Write a dataarray with 6 components of rank 2 (I4P).
  class(xml_writer_appended), intent(inout) :: self     !< Writer.
  integer(I4P),               intent(in)    :: u(1:,1:) !< U component.
  integer(I4P),               intent(in)    :: v(1:,1:) !< V component.
  integer(I4P),               intent(in)    :: w(1:,1:) !< W component.
  integer(I4P),               intent(in)    :: x(1:,1:) !< X component.
  integer(I4P),               intent(in)    :: y(1:,1:) !< Y component.
  integer(I4P),               intent(in)    :: z(1:,1:) !< Z component.
  integer(I8P)                              :: n_byte   !< Number of bytes
  integer(I4P), allocatable                 :: buf(:)   !< Interleaved components.
  integer(I8P)                              :: nn       !< Number of elements.

  nn = size(x, kind=I8P)
  allocate(buf(1:6*nn))
  buf(1::6) = reshape(u, [nn])
  buf(2::6) = reshape(v, [nn])
  buf(3::6) = reshape(w, [nn])
  buf(4::6) = reshape(x, [nn])
  buf(5::6) = reshape(y, [nn])
  buf(6::6) = reshape(z, [nn])
  n_byte = self%write_on_scratch_dataarray(x=buf)
  endfunction write_on_scratch_dataarray6_rank2_I4P

  function write_on_scratch_dataarray6_rank2_I2P(self, u, v, w, x, y, z) result(n_byte)
  !< Write a dataarray with 6 components of rank 2 (I2P).
  class(xml_writer_appended), intent(inout) :: self     !< Writer.
  integer(I2P),               intent(in)    :: u(1:,1:) !< U component.
  integer(I2P),               intent(in)    :: v(1:,1:) !< V component.
  integer(I2P),               intent(in)    :: w(1:,1:) !< W component.
  integer(I2P),               intent(in)    :: x(1:,1:) !< X component.
  integer(I2P),               intent(in)    :: y(1:,1:) !< Y component.
  integer(I2P),               intent(in)    :: z(1:,1:) !< Z component.
  integer(I8P)                              :: n_byte   !< Number of bytes
  integer(I2P), allocatable                 :: buf(:)   !< Interleaved components.
  integer(I8P)                              :: nn       !< Number of elements.

  nn = size(x, kind=I8P)
  allocate(buf(1:6*nn))
  buf(1::6) = reshape(u, [nn])
  buf(2::6) = reshape(v, [nn])
  buf(3::6) = reshape(w, [nn])
  buf(4::6) = reshape(x, [nn])
  buf(5::6) = reshape(y, [nn])
  buf(6::6) = reshape(z, [nn])
  n_byte = self%write_on_scratch_dataarray(x=buf)
  endfunction write_on_scratch_dataarray6_rank2_I2P

  function write_on_scratch_dataarray6_rank2_I1P(self, u, v, w, x, y, z) result(n_byte)
  !< Write a dataarray with 6 components of rank 2 (I1P).
  class(xml_writer_appended), intent(inout) :: self     !< Writer.
  integer(I1P),               intent(in)    :: u(1:,1:) !< U component.
  integer(I1P),               intent(in)    :: v(1:,1:) !< V component.
  integer(I1P),               intent(in)    :: w(1:,1:) !< W component.
  integer(I1P),               intent(in)    :: x(1:,1:) !< X component.
  integer(I1P),               intent(in)    :: y(1:,1:) !< Y component.
  integer(I1P),               intent(in)    :: z(1:,1:) !< Z component.
  integer(I8P)                              :: n_byte   !< Number of bytes
  integer(I1P), allocatable                 :: buf(:)   !< Interleaved components.
  integer(I8P)                              :: nn       !< Number of elements.

  nn = size(x, kind=I8P)
  allocate(buf(1:6*nn))
  buf(1::6) = reshape(u, [nn])
  buf(2::6) = reshape(v, [nn])
  buf(3::6) = reshape(w, [nn])
  buf(4::6) = reshape(x, [nn])
  buf(5::6) = reshape(y, [nn])
  buf(6::6) = reshape(z, [nn])
  n_byte = self%write_on_scratch_dataarray(x=buf)
  endfunction write_on_scratch_dataarray6_rank2_I1P

  function write_on_scratch_dataarray6_rank3_R8P(self, u, v, w, x, y, z) result(n_byte)
  !< Write a dataarray with 6 components of rank 3 (R8P).
  class(xml_writer_appended), intent(inout) :: self        !< Writer.
  real(R8P),                  intent(in)    :: u(1:,1:,1:) !< U component.
  real(R8P),                  intent(in)    :: v(1:,1:,1:) !< V component.
  real(R8P),                  intent(in)    :: w(1:,1:,1:) !< W component.
  real(R8P),                  intent(in)    :: x(1:,1:,1:) !< X component.
  real(R8P),                  intent(in)    :: y(1:,1:,1:) !< Y component.
  real(R8P),                  intent(in)    :: z(1:,1:,1:) !< Z component.
  integer(I8P)                              :: n_byte      !< Number of bytes
  real(R8P), allocatable                    :: buf(:)      !< Interleaved components.
  integer(I8P)                              :: nn          !< Number of elements.

  nn = size(x, kind=I8P)
  allocate(buf(1:6*nn))
  buf(1::6) = reshape(u, [nn])
  buf(2::6) = reshape(v, [nn])
  buf(3::6) = reshape(w, [nn])
  buf(4::6) = reshape(x, [nn])
  buf(5::6) = reshape(y, [nn])
  buf(6::6) = reshape(z, [nn])
  n_byte = self%write_on_scratch_dataarray(x=buf)
  endfunction write_on_scratch_dataarray6_rank3_R8P

  function write_on_scratch_dataarray6_rank3_R4P(self, u, v, w, x, y, z) result(n_byte)
  !< Write a dataarray with 6 components of rank 3 (R4P).
  class(xml_writer_appended), intent(inout) :: self        !< Writer.
  real(R4P),                  intent(in)    :: u(1:,1:,1:) !< U component.
  real(R4P),                  intent(in)    :: v(1:,1:,1:) !< V component.
  real(R4P),                  intent(in)    :: w(1:,1:,1:) !< W component.
  real(R4P),                  intent(in)    :: x(1:,1:,1:) !< X component.
  real(R4P),                  intent(in)    :: y(1:,1:,1:) !< Y component.
  real(R4P),                  intent(in)    :: z(1:,1:,1:) !< Z component.
  integer(I8P)                              :: n_byte      !< Number of bytes
  real(R4P), allocatable                    :: buf(:)      !< Interleaved components.
  integer(I8P)                              :: nn          !< Number of elements.

  nn = size(x, kind=I8P)
  allocate(buf(1:6*nn))
  buf(1::6) = reshape(u, [nn])
  buf(2::6) = reshape(v, [nn])
  buf(3::6) = reshape(w, [nn])
  buf(4::6) = reshape(x, [nn])
  buf(5::6) = reshape(y, [nn])
  buf(6::6) = reshape(z, [nn])
  n_byte = self%write_on_scratch_dataarray(x=buf)
  endfunction write_on_scratch_dataarray6_rank3_R4P

  function write_on_scratch_dataarray6_rank3_I8P(self, u, v, w, x, y, z) result(n_byte)
  !< Write a dataarray with 6 components of rank 3 (I8P).
  class(xml_writer_appended), intent(inout) :: self        !< Writer.
  integer(I8P),               intent(in)    :: u(1:,1:,1:) !< U component.
  integer(I8P),               intent(in)    :: v(1:,1:,1:) !< V component.
  integer(I8P),               intent(in)    :: w(1:,1:,1:) !< W component.
  integer(I8P),               intent(in)    :: x(1:,1:,1:) !< X component.
  integer(I8P),               intent(in)    :: y(1:,1:,1:) !< Y component.
  integer(I8P),               intent(in)    :: z(1:,1:,1:) !< Z component.
  integer(I8P)                              :: n_byte      !< Number of bytes
  integer(I8P), allocatable                 :: buf(:)      !< Interleaved components.
  integer(I8P)                              :: nn          !< Number of elements.

  nn = size(x, kind=I8P)
  allocate(buf(1:6*nn))
  buf(1::6) = reshape(u, [nn])
  buf(2::6) = reshape(v, [nn])
  buf(3::6) = reshape(w, [nn])
  buf(4::6) = reshape(x, [nn])
  buf(5::6) = reshape(y, [nn])
  buf(6::6) = reshape(z, [nn])
  n_byte = self%write_on_scratch_dataarray(x=buf)
  endfunction write_on_scratch_dataarray6_rank3_I8P

  function write_on_scratch_dataarray6_rank3_I4P(self, u, v, w, x, y, z) result(n_byte)
  !< Write a dataarray with 6 components of rank 3 (I4P).
  class(xml_writer_appended), intent(inout) :: self        !< Writer.
  integer(I4P),               intent(in)    :: u(1:,1:,1:) !< U component.
  integer(I4P),               intent(in)    :: v(1:,1:,1:) !< V component.
  integer(I4P),               intent(in)    :: w(1:,1:,1:) !< W component.
  integer(I4P),               intent(in)    :: x(1:,1:,1:) !< X component.
  integer(I4P),               intent(in)    :: y(1:,1:,1:) !< Y component.
  integer(I4P),               intent(in)    :: z(1:,1:,1:) !< Z component.
  integer(I8P)                              :: n_byte      !< Number of bytes
  integer(I4P), allocatable                 :: buf(:)      !< Interleaved components.
  integer(I8P)                              :: nn          !< Number of elements.

  nn = size(x, kind=I8P)
  allocate(buf(1:6*nn))
  buf(1::6) = reshape(u, [nn])
  buf(2::6) = reshape(v, [nn])
  buf(3::6) = reshape(w, [nn])
  buf(4::6) = reshape(x, [nn])
  buf(5::6) = reshape(y, [nn])
  buf(6::6) = reshape(z, [nn])
  n_byte = self%write_on_scratch_dataarray(x=buf)
  endfunction write_on_scratch_dataarray6_rank3_I4P

  function write_on_scratch_dataarray6_rank3_I2P(self, u, v, w, x, y, z) result(n_byte)
  !< Write a dataarray with 6 components of rank 3 (I2P).
  class(xml_writer_appended), intent(inout) :: self        !< Writer.
  integer(I2P),               intent(in)    :: u(1:,1:,1:) !< U component.
  integer(I2P),               intent(in)    :: v(1:,1:,1:) !< V component.
  integer(I2P),               intent(in)    :: w(1:,1:,1:) !< W component.
  integer(I2P),               intent(in)    :: x(1:,1:,1:) !< X component.
  integer(I2P),               intent(in)    :: y(1:,1:,1:) !< Y component.
  integer(I2P),               intent(in)    :: z(1:,1:,1:) !< Z component.
  integer(I8P)                              :: n_byte      !< Number of bytes
  integer(I2P), allocatable                 :: buf(:)      !< Interleaved components.
  integer(I8P)                              :: nn          !< Number of elements.

  nn = size(x, kind=I8P)
  allocate(buf(1:6*nn))
  buf(1::6) = reshape(u, [nn])
  buf(2::6) = reshape(v, [nn])
  buf(3::6) = reshape(w, [nn])
  buf(4::6) = reshape(x, [nn])
  buf(5::6) = reshape(y, [nn])
  buf(6::6) = reshape(z, [nn])
  n_byte = self%write_on_scratch_dataarray(x=buf)
  endfunction write_on_scratch_dataarray6_rank3_I2P

  function write_on_scratch_dataarray6_rank3_I1P(self, u, v, w, x, y, z) result(n_byte)
  !< Write a dataarray with 6 components of rank 3 (I1P).
  class(xml_writer_appended), intent(inout) :: self        !< Writer.
  integer(I1P),               intent(in)    :: u(1:,1:,1:) !< U component.
  integer(I1P),               intent(in)    :: v(1:,1:,1:) !< V component.
  integer(I1P),               intent(in)    :: w(1:,1:,1:) !< W component.
  integer(I1P),               intent(in)    :: x(1:,1:,1:) !< X component.
  integer(I1P),               intent(in)    :: y(1:,1:,1:) !< Y component.
  integer(I1P),               intent(in)    :: z(1:,1:,1:) !< Z component.
  integer(I8P)                              :: n_byte      !< Number of bytes
  integer(I1P), allocatable                 :: buf(:)      !< Interleaved components.
  integer(I8P)                              :: nn          !< Number of elements.

  nn = size(x, kind=I8P)
  allocate(buf(1:6*nn))
  buf(1::6) = reshape(u, [nn])
  buf(2::6) = reshape(v, [nn])
  buf(3::6) = reshape(w, [nn])
  buf(4::6) = reshape(x, [nn])
  buf(5::6) = reshape(y, [nn])
  buf(6::6) = reshape(z, [nn])
  n_byte = self%write_on_scratch_dataarray(x=buf)
  endfunction write_on_scratch_dataarray6_rank3_I1P

#ifdef VTKFORTRAN_USE_ZLIB
  ! to_bytes methods
  pure subroutine to_bytes_R8P(x, bytes)
  !< Copy a dataarray into a bytes stream (R8P).
  real(R8P),              intent(in)  :: x(1:)     !< Dataarray.
  integer(c_signed_char), intent(out) :: bytes(1:) !< Bytes stream, at least of size(x)*BYR8P elements.
  integer(I8P)                        :: n         !< Counter.

  do n=1_I8P, size(x, kind=I8P)
    bytes((n-1_I8P)*BYR8P+1_I8P:n*BYR8P) = transfer(x(n), bytes)
  enddo
  endsubroutine to_bytes_R8P

  pure subroutine to_bytes_R4P(x, bytes)
  !< Copy a dataarray into a bytes stream (R4P).
  real(R4P),              intent(in)  :: x(1:)     !< Dataarray.
  integer(c_signed_char), intent(out) :: bytes(1:) !< Bytes stream, at least of size(x)*BYR4P elements.
  integer(I8P)                        :: n         !< Counter.

  do n=1_I8P, size(x, kind=I8P)
    bytes((n-1_I8P)*BYR4P+1_I8P:n*BYR4P) = transfer(x(n), bytes)
  enddo
  endsubroutine to_bytes_R4P

  pure subroutine to_bytes_I8P(x, bytes)
  !< Copy a dataarray into a bytes stream (I8P).
  integer(I8P),           intent(in)  :: x(1:)     !< Dataarray.
  integer(c_signed_char), intent(out) :: bytes(1:) !< Bytes stream, at least of size(x)*BYI8P elements.
  integer(I8P)                        :: n         !< Counter.

  do n=1_I8P, size(x, kind=I8P)
    bytes((n-1_I8P)*BYI8P+1_I8P:n*BYI8P) = transfer(x(n), bytes)
  enddo
  endsubroutine to_bytes_I8P

  pure subroutine to_bytes_I4P(x, bytes)
  !< Copy a dataarray into a bytes stream (I4P).
  integer(I4P),           intent(in)  :: x(1:)     !< Dataarray.
  integer(c_signed_char), intent(out) :: bytes(1:) !< Bytes stream, at least of size(x)*BYI4P elements.
  integer(I8P)                        :: n         !< Counter.

  do n=1_I8P, size(x, kind=I8P)
    bytes((n-1_I8P)*BYI4P+1_I8P:n*BYI4P) = transfer(x(n), bytes)
  enddo
  endsubroutine to_bytes_I4P

  pure subroutine to_bytes_I2P(x, bytes)
  !< Copy a dataarray into a bytes stream (I2P).
  integer(I2P),           intent(in)  :: x(1:)     !< Dataarray.
  integer(c_signed_char), intent(out) :: bytes(1:) !< Bytes stream, at least of size(x)*BYI2P elements.
  integer(I8P)                        :: n         !< Counter.

  do n=1_I8P, size(x, kind=I8P)
    bytes((n-1_I8P)*BYI2P+1_I8P:n*BYI2P) = transfer(x(n), bytes)
  enddo
  endsubroutine to_bytes_I2P

  pure subroutine to_bytes_I1P(x, bytes)
  !< Copy a dataarray into a bytes stream (I1P).
  integer(I1P),           intent(in)  :: x(1:)     !< Dataarray.
  integer(c_signed_char), intent(out) :: bytes(1:) !< Bytes stream, at least of size(x)*BYI1P elements.
  integer(I8P)                        :: n         !< Counter.

  do n=1_I8P, size(x, kind=I8P)
    bytes((n-1_I8P)*BYI1P+1_I8P:n*BYI1P) = transfer(x(n), bytes)
  enddo
  endsubroutine to_bytes_I1P
#endif
endmodule vtk_fortran_vtk_file_xml_writer_appended
