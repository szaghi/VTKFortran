!< VTK file class.
module vtk_fortran_vtk_file
!< VTK file class.
use befor64
use penf
use stringifor
use vtk_fortran_vtk_file_xml_writer_abstract
use vtk_fortran_vtk_file_xml_writer_appended
use vtk_fortran_vtk_file_xml_writer_ascii_local
use vtk_fortran_vtk_file_xml_writer_binary_local
use vtk_fortran_zlib, only : is_zlib_enabled

implicit none
private
public :: vtk_file

type :: vtk_file
  !< VTK file class.
  private
  class(xml_writer_abstract), allocatable, public :: xml_writer !< XML writer.
  contains
    procedure, pass(self) :: get_xml_volatile !< Return the eventual XML volatile string file.
    procedure, pass(self) :: initialize       !< Initialize file.
    procedure, pass(self) :: finalize         !< Finalize file.
    procedure, pass(self) :: free             !< Free allocated memory.
endtype vtk_file
contains
   pure subroutine get_xml_volatile(self, xml_volatile, error)
   !< Return the eventual XML volatile string file.
   class(vtk_file),  intent(in)               :: self         !< VTK file.
   character(len=:), intent(out), allocatable :: xml_volatile !< XML volatile file.
   integer(I4P),     intent(out), optional    :: error !< Error status.

   call self%xml_writer%get_xml_volatile(xml_volatile=xml_volatile, error=error)
   endsubroutine get_xml_volatile

   function initialize(self, format, filename, mesh_topology, is_volatile, nx1, nx2, ny1, ny2, nz1, nz2, &
                       origin, spacing, direction, header_type, compressor) result(error)
   !< Initialize file (writer).
   !<
   !< @note This function must be the first to be called.
   !<
   !<### Supported output formats are (the passed specifier value is case insensitive):
   !<
   !<- ASCII: data are saved in ASCII format;
   !<- BINARY: data are saved in base64 encoded format;
   !<- RAW: data are saved in raw-binary format in the appended tag of the XML file;
   !<- RAW-ZLIB: shorthand of RAW with `compressor='zlib'`;
   !<- BINARY-APPENDED: data are saved in base64 encoded format in the appended tag of the XML file.
   !<
   !<### Compression of binary data
   !<
   !< The optional `compressor` compresses the data of the binary formats (BINARY, RAW, BINARY-APPENDED) as VTK does
   !< (`vtkZLibDataCompressor`, blocks of 32 KiB): **none** (default) or **zlib**, case insensitive; it is ignored by the ASCII
   !< format. zlib needs the library built with `VTKFORTRAN_USE_ZLIB`: otherwise, as for an unknown compressor, `initialize`
   !< returns a non-zero error. RAW-ZLIB with `compressor='none'` is an error too.
   !<
   !<### Bytes count header of binary data
   !<
   !< Each binary DataArray (formats BINARY, RAW, RAW-ZLIB, BINARY-APPENDED) is prefixed by its bytes count (by the header of
   !< its compressed blocks, when compressed). The optional
   !< `header_type` selects its width: **UInt32** (default) limits each DataArray to 2 GiB (larger ones stop the execution with
   !< an explicit error), **UInt64** lifts the limit; it is case insensitive and ignored by the ASCII format.
   !<
   !<### Supported topologies are:
   !<
   !<- RectilinearGrid;
   !<- StructuredGrid;
   !<- UnstructuredGrid;
   !<- ImageData: a regular grid, defined by the extents and the `origin`, `spacing` (both required) and `direction`
   !<  (optional) arguments; the point of indexes (i,j,k) is at `origin + direction . ([i,j,k] * spacing)`. No geometry is
   !<  written (`write_geo` is not used).
   !<- PolyData: points (`write_geo(np, nc, x, y, z)`) and up to four cell blocks (vertices, lines, triangle strips, polygons)
   !<  written with `write_polydata_cells`; the piece is opened with `write_piece(np, nverts, nlines, nstrips, npolys)`.
   !<
   !<### Example of usage
   !<
   !<```fortran
   !< type(vtk_file) :: vtk
   !< integer(I4P)   :: nx1, nx2, ny1, ny2, nz1, nz2
   !< ...
   !< error = vtk%initialize('BINARY','XML_RECT_BINARY.vtr','RectilinearGrid',nx1=nx1,nx2=nx2,ny1=ny1,ny2=ny2,nz1=nz1,nz2=nz2)
   !< ...
   !< error = vtk%initialize('BINARY','XML_UNST_ZLIB.vtu','UnstructuredGrid',compressor='zlib')
   !< ...
   !<```
   !< @note The file extension is necessary in the file name. The XML standard has different extensions for each
   !< different topologies (e.g. *vtr* for rectilinear topology). See the VTK-standard file for more information.
   class(vtk_file), intent(inout)        :: self          !< VTK file.
   character(*),    intent(in)           :: format        !< File format: ASCII, BINARY, RAW, RAW-ZLIB or BINARY-APPENDED.
   character(*),    intent(in)           :: filename      !< File name.
   character(*),    intent(in)           :: mesh_topology !< Mesh topology.
   logical,         intent(in), optional :: is_volatile   !< Flag to check volatile writer.
   integer(I4P),    intent(in), optional :: nx1           !< Initial node of x axis.
   integer(I4P),    intent(in), optional :: nx2           !< Final node of x axis.
   integer(I4P),    intent(in), optional :: ny1           !< Initial node of y axis.
   integer(I4P),    intent(in), optional :: ny2           !< Final node of y axis.
   integer(I4P),    intent(in), optional :: nz1           !< Initial node of z axis.
   integer(I4P),    intent(in), optional :: nz2           !< Final node of z axis.
   real(R8P),       intent(in), optional :: origin(3)      !< Origin of ImageData: coordinates of the point of indexes (0,0,0).
   real(R8P),       intent(in), optional :: spacing(3)     !< Spacing of ImageData along each axis.
   real(R8P),       intent(in), optional :: direction(9)   !< Axes directions of ImageData, row-major 3x3 matrix (default identity).
   character(*),    intent(in), optional :: header_type    !< Bytes count header of binary data: UInt32 (default) or UInt64.
   character(*),    intent(in), optional :: compressor     !< Compressor of binary data: none (default) or zlib.
   integer(I4P)                          :: error         !< Error status.
   type(string)                          :: fformat       !< File format.
   logical                               :: is_compressed !< Compress the binary data.

   if (.not.is_initialized) call penf_init
   if (.not.is_b64_initialized) call b64_init
   fformat = trim(adjustl(format))
   fformat = fformat%upper()
   if (allocated(self%xml_writer)) deallocate(self%xml_writer)
   error = 0_I4P
   select case(fformat%chars())
   case('ASCII')
      allocate(xml_writer_ascii_local :: self%xml_writer)
   case('BINARY-APPENDED', 'RAW', 'RAW-ZLIB')
      allocate(xml_writer_appended :: self%xml_writer)
   case('BINARY')
      allocate(xml_writer_binary_local :: self%xml_writer)
   case default
      error = 1
   endselect
   if (error /= 0_I4P) return
   if (present(header_type)) then
      select case(upper_case(trim(adjustl(header_type))))
      case('UINT32')
         self%xml_writer%is_uint64 = .false.
      case('UINT64')
         self%xml_writer%is_uint64 = .true.
      case default
         error = 1_I4P
         return
      endselect
   endif
   is_compressed = fformat == 'RAW-ZLIB'
   if (present(compressor)) then
      select case(upper_case(trim(adjustl(compressor))))
      case('NONE')
         if (is_compressed) error = 1_I4P ! RAW-ZLIB is always compressed
      case('ZLIB')
         is_compressed = .true.
      case default
         error = 1_I4P
      endselect
      if (error /= 0_I4P) return
   endif
   if (fformat == 'ASCII') is_compressed = .false.
   if (is_compressed .and. .not. is_zlib_enabled) then
      error = 1_I4P
      return
   endif
   self%xml_writer%is_compressed = is_compressed
   if (index(mesh_topology, 'ImageData') > 0) then
      ! ImageData grids are defined by extents, origin and spacing (direction is optional)
      if (.not.(present(origin).and.present(spacing))) then
         error = 1_I4P
         return
      endif
   endif
   if (present(origin)) self%xml_writer%origin = origin
   if (present(spacing)) self%xml_writer%spacing = spacing
   if (present(direction)) then
      self%xml_writer%direction = direction
      self%xml_writer%is_direction_set = .true.
   endif
   error = self%xml_writer%initialize(format=format, filename=filename, mesh_topology=mesh_topology, &
                                      is_volatile=is_volatile,                                       &
                                      nx1=nx1, nx2=nx2, ny1=ny1, ny2=ny2, nz1=nz1, nz2=nz2)
   endfunction initialize

   pure function upper_case(string) result(upper)
   !< Return a string in upper case (ASCII letters only).
   character(*), intent(in) :: string !< Input string.
   character(len(string))   :: upper  !< Upper case string.
   integer                  :: i      !< Counter.

   upper = string
   do i=1, len(string)
      if (string(i:i) >= 'a' .and. string(i:i) <= 'z') upper(i:i) = achar(iachar(string(i:i)) - 32)
   enddo
   endfunction upper_case

   function finalize(self) result(error)
   !< Finalize file (writer).
   class(vtk_file), intent(inout)  :: self  !< VTK file.
   integer(I4P)                    :: error !< Error status.
   character(len=:),           allocatable :: xml_volatile !< XML volatile file.

   error = 1
   if (allocated(self%xml_writer)) error = self%xml_writer%finalize()
   endfunction finalize

   elemental subroutine free(self, error)
   !< Free allocated memory.
   class(vtk_file), intent(inout)         :: self  !< VTK file.
   integer(I4P),    intent(out), optional :: error !< Error status.

   if (allocated(self%xml_writer)) call self%xml_writer%free(error=error)
   endsubroutine free
endmodule vtk_fortran_vtk_file
