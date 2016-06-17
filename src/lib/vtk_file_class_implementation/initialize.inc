  ! Implementation of initialize of VTK file class.
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
