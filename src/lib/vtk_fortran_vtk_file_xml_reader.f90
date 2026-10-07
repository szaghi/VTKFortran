!< VTK file XML reader.
module vtk_fortran_vtk_file_xml_reader
!< VTK file XML reader.
!<
!< The reader indexes the file when it is initialized: one pass over the XML that records the elements and attributes and
!< the positions of the data, without loading them (the scan stops at the appended data). Each read then decodes only the
!< data asked for: the memory used is about the size of the dataarray read, not of the file.
!<
!< It reads the serial XML formats (ImageData, RectilinearGrid, StructuredGrid, UnstructuredGrid, PolyData) written by
!< VTKFortran or by VTK, in every format (ascii, binary, raw and base64 appended), uncompressed or zlib compressed
!< (`vtkZLibDataCompressor`), with UInt32 or UInt64 headers. BigEndian files and other compressors are not supported.
!<
!< It reads the parallel headers too (PImageData, PRectilinearGrid, PStructuredGrid, PUnstructuredGrid, PPolyData): their
!< information, the declared arrays (`get_dataarray_names`, `get_dataarray_info`), the pieces (`get_sources`, `read_piece`
!< for their extents) and the check of the pieces against the header (`check_pieces`). They hold no data.
!<
!< All procedures return an error status:
!<
!<| Error | Meaning                                                                                  |
!<|-------|------------------------------------------------------------------------------------------|
!<| 0     | success                                                                                  |
!<| 1     | the file cannot be read                                                                  |
!<| 2     | the file is not a VTK XML file (malformed XML, no VTKFile element or dataset element)    |
!<| 3     | unsupported feature (BigEndian, compressor, parallel or unknown dataset type, encoding)  |
!<| 4     | not found (piece, data array, geometry or cells absent), or the reader is not initialized |
!<| 5     | the output kind cannot hold the values of the data array                                 |
!<| 6     | the data do not decode (inconsistent sizes, malformed base64, zlib failure)              |
!<| 7     | the pieces of a parallel header do not match it (see `check_pieces`)                     |
use penf
use vtk_fortran_dataarray_decoder
use vtk_fortran_xml_scanner
use vtk_fortran_zlib, only : zlib_uncompress_blocks

implicit none
private
public :: xml_reader

type :: xml_reader
  !< VTK file XML reader.
  private
  character(len=:), allocatable :: filename                !< File name.
  type(xml_scanner)             :: xml                     !< Index of the XML elements.
  character(len=:), allocatable :: mesh_topology           !< Dataset type.
  integer(I8P)                  :: word_size=4_I8P         !< Size of the binary header words: 4 (UInt32) or 8 (UInt64).
  logical                       :: is_compressed=.false.   !< The binary data are zlib compressed.
  logical                       :: is_initialized=.false.  !< The reader is initialized.
  integer(I4P)                  :: dataset=0               !< Index of the dataset element.
  integer(I4P)                  :: pieces_number=0         !< Number of pieces.
  logical                       :: is_appended_raw=.true.  !< The appended data are raw (base64 otherwise).
  integer(I8P)                  :: appended_start=0_I8P    !< Position of the first byte of the appended data, 0 if none.
  logical                       :: is_parallel=.false.     !< The file is a parallel header (P* dataset type).
  contains
    ! public methods
    procedure, pass(self) :: check_pieces         !< Check the pieces of a parallel header against it.
    procedure, pass(self) :: finalize             !< Finalize the reader (free memory).
    procedure, pass(self) :: get_dataarray_info   !< Return the type, components and tuples of a dataarray.
    procedure, pass(self) :: get_dataarray_names  !< Return the names of the dataarrays of a location.
    procedure, pass(self) :: get_info             !< Return the information of the dataset.
    procedure, pass(self) :: get_sources          !< Return the files of the pieces of a parallel header.
    procedure, pass(self) :: initialize           !< Initialize the reader: index the file.
    generic               :: read_connectivity => read_connectivity_I4P, read_connectivity_I8P !< Read the cells.
    generic               :: read_dataarray => read_dataarray_rank1_R8P, read_dataarray_rank1_R4P, &
                                               read_dataarray_rank1_I8P, read_dataarray_rank1_I4P, &
                                               read_dataarray_rank1_I2P, read_dataarray_rank1_I1P, &
                                               read_dataarray_rank2_R8P, read_dataarray_rank2_R4P, &
                                               read_dataarray_rank2_I8P, read_dataarray_rank2_I4P, &
                                               read_dataarray_rank2_I2P, read_dataarray_rank2_I1P, &
                                               read_dataarray_strings !< Read a dataarray.
    generic               :: read_geo => read_geo_R8P, read_geo_R4P !< Read the geometry.
    procedure, pass(self) :: read_piece           !< Read the counts and extent of a piece.
    generic               :: read_polydata_cells => read_polydata_cells_I4P, read_polydata_cells_I8P !< Read polydata cells.
    ! private methods
    procedure, pass(self), private :: dataarray_bytes         !< Return the bytes of a dataarray element.
    procedure, pass(self), private :: find_dataarray          !< Return the index of a named dataarray element.
    procedure, pass(self), private :: geometry_dataarrays     !< Return the indexes of the geometry dataarray elements.
    procedure, pass(self), private :: piece_element           !< Return the index of a piece element.
    procedure, pass(self), private :: read_connectivity_I4P   !< Read the cells (I4P).
    procedure, pass(self), private :: read_connectivity_I8P   !< Read the cells (I8P).
    procedure, pass(self), private :: read_dataarray_rank1_R8P !< Read a dataarray (rank 1, R8P).
    procedure, pass(self), private :: read_dataarray_rank1_R4P !< Read a dataarray (rank 1, R4P).
    procedure, pass(self), private :: read_dataarray_rank1_I8P !< Read a dataarray (rank 1, I8P).
    procedure, pass(self), private :: read_dataarray_rank1_I4P !< Read a dataarray (rank 1, I4P).
    procedure, pass(self), private :: read_dataarray_rank1_I2P !< Read a dataarray (rank 1, I2P).
    procedure, pass(self), private :: read_dataarray_rank1_I1P !< Read a dataarray (rank 1, I1P).
    procedure, pass(self), private :: read_dataarray_rank2_R8P !< Read a dataarray (rank 2, R8P).
    procedure, pass(self), private :: read_dataarray_rank2_R4P !< Read a dataarray (rank 2, R4P).
    procedure, pass(self), private :: read_dataarray_rank2_I8P !< Read a dataarray (rank 2, I8P).
    procedure, pass(self), private :: read_dataarray_rank2_I4P !< Read a dataarray (rank 2, I4P).
    procedure, pass(self), private :: read_dataarray_rank2_I2P !< Read a dataarray (rank 2, I2P).
    procedure, pass(self), private :: read_dataarray_rank2_I1P !< Read a dataarray (rank 2, I1P).
    procedure, pass(self), private :: read_dataarray_strings   !< Read a dataarray of strings.
    procedure, pass(self), private :: read_geo_R8P             !< Read the geometry (R8P).
    procedure, pass(self), private :: read_geo_R4P             !< Read the geometry (R4P).
    procedure, pass(self), private :: read_ids                 !< Read a dataarray of ids (I8P) of a piece section.
    procedure, pass(self), private :: read_named_dataarray     !< Return the bytes of a named dataarray.
    procedure, pass(self), private :: read_polydata_cells_I4P  !< Read polydata cells (I4P).
    procedure, pass(self), private :: read_polydata_cells_I8P  !< Read polydata cells (I8P).
endtype xml_reader

contains
  ! public methods
  function initialize(self, filename) result(error)
  !< Initialize the reader: index the file.
  class(xml_reader), intent(inout) :: self      !< XML reader.
  character(*),      intent(in)    :: filename  !< File name.
  integer(I4P)                     :: error     !< Error status.
  character(len=:), allocatable    :: value     !< Attribute value.
  integer(I4P)                     :: root      !< Index of the VTKFile element.
  integer(I4P)                     :: appended  !< Index of the AppendedData element.

  call self%finalize
  call self%xml%scan(filename=filename, error=error, stop_at='AppendedData')
  if (error /= 0) return
  error = 2
  root = self%xml%find_child(parent=0, name='VTKFile')
  if (root == 0) return
  call self%xml%element(root)%get_attribute(name='type', value=value)
  self%mesh_topology = value
  self%dataset = self%xml%find_child(parent=root, name=self%mesh_topology)
  if (self%dataset == 0) return
  error = 3
  select case(self%mesh_topology)
  case('ImageData', 'RectilinearGrid', 'StructuredGrid', 'UnstructuredGrid', 'PolyData')
    self%is_parallel = .false.
  case('PImageData', 'PRectilinearGrid', 'PStructuredGrid', 'PUnstructuredGrid', 'PPolyData')
    self%is_parallel = .true.
  case default
    return
  endselect
  call self%xml%element(root)%get_attribute(name='byte_order', value=value)
  if (value == 'BigEndian') return
  call self%xml%element(root)%get_attribute(name='header_type', value=value)
  select case(value)
  case('', 'UInt32')
    self%word_size = 4_I8P
  case('UInt64')
    self%word_size = 8_I8P
  case default
    return
  endselect
  call self%xml%element(root)%get_attribute(name='compressor', value=value)
  select case(value)
  case('')
    self%is_compressed = .false.
  case('vtkZLibDataCompressor')
    self%is_compressed = .true.
  case default
    return
  endselect
  appended = self%xml%find_child(parent=root, name='AppendedData')
  if (appended > 0) then
    call self%xml%element(appended)%get_attribute(name='encoding', value=value)
    select case(value)
    case('raw')
      self%is_appended_raw = .true.
    case('base64')
      self%is_appended_raw = .false.
    case default
      return
    endselect
    ! the appended data start after the '_' marker
    self%appended_start = find_marker(filename=filename, pos=self%xml%element(appended)%content_start)
    if (self%appended_start == 0_I8P) then
      error = 2
      return
    endif
  endif
  self%pieces_number = 0
  do while (self%xml%find_child(parent=self%dataset, name='Piece', n=self%pieces_number+1) > 0)
    self%pieces_number = self%pieces_number + 1
  enddo
  self%filename = filename
  self%is_initialized = .true.
  error = 0
  endfunction initialize

  elemental subroutine finalize(self)
  !< Finalize the reader (free memory).
  class(xml_reader), intent(inout) :: self !< XML reader.

  call self%xml%free
  if (allocated(self%filename)) deallocate(self%filename)
  if (allocated(self%mesh_topology)) deallocate(self%mesh_topology)
  self%word_size = 4_I8P
  self%is_compressed = .false.
  self%is_initialized = .false.
  self%dataset = 0
  self%pieces_number = 0
  self%is_appended_raw = .true.
  self%appended_start = 0_I8P
  self%is_parallel = .false.
  endsubroutine finalize

  function get_info(self, mesh_topology, npieces, header_type, compressor, nx1, nx2, ny1, ny2, nz1, nz2, &
                    origin, spacing, direction, ghost_level) result(error)
  !< Return the information of the dataset.
  !<
  !< The whole extent (`nx1...nz2`) is returned for the structured topologies (ImageData, RectilinearGrid, StructuredGrid);
  !< `origin`, `spacing` and `direction` for ImageData (`direction` is the identity when the file has none); `ghost_level`
  !< for parallel headers (0 when the file has none).
  class(xml_reader),             intent(in)            :: self          !< XML reader.
  character(len=:), allocatable, intent(out), optional :: mesh_topology !< Dataset type, e.g. UnstructuredGrid.
  integer(I4P),                  intent(out), optional :: npieces       !< Number of pieces.
  character(len=:), allocatable, intent(out), optional :: header_type   !< Header of the binary data: UInt32 or UInt64.
  character(len=:), allocatable, intent(out), optional :: compressor    !< Compressor of the binary data: none or zlib.
  integer(I4P),                  intent(out), optional :: nx1           !< Initial node of x axis.
  integer(I4P),                  intent(out), optional :: nx2           !< Final node of x axis.
  integer(I4P),                  intent(out), optional :: ny1           !< Initial node of y axis.
  integer(I4P),                  intent(out), optional :: ny2           !< Final node of y axis.
  integer(I4P),                  intent(out), optional :: nz1           !< Initial node of z axis.
  integer(I4P),                  intent(out), optional :: nz2           !< Final node of z axis.
  real(R8P),                     intent(out), optional :: origin(3)     !< Origin of ImageData.
  real(R8P),                     intent(out), optional :: spacing(3)    !< Spacing of ImageData.
  real(R8P),                     intent(out), optional :: direction(9)  !< Axes directions of ImageData (row-major).
  integer(I4P),                  intent(out), optional :: ghost_level   !< Number of ghost levels of a parallel header.
  integer(I4P)                                         :: error         !< Error status.
  character(len=:), allocatable                        :: value         !< Attribute value.
  integer(I4P)                                         :: extent(6)     !< Extent.
  integer(I4P)                                         :: iostat        !< IO status.

  error = 4
  if (.not.self%is_initialized) return
  error = 0
  iostat = 0
  if (present(mesh_topology)) mesh_topology = self%mesh_topology
  if (present(npieces)) npieces = self%pieces_number
  if (present(header_type)) header_type = trim(merge('UInt64', 'UInt32', self%word_size == 8_I8P))
  if (present(compressor)) compressor = trim(merge('zlib', 'none', self%is_compressed))
  extent = 0
  call self%xml%element(self%dataset)%get_attribute(name='WholeExtent', value=value)
  if (len(value) > 0) then
    read(value, *, iostat=iostat) extent
    if (iostat /= 0) error = 6
  endif
  if (present(nx1)) nx1 = extent(1)
  if (present(nx2)) nx2 = extent(2)
  if (present(ny1)) ny1 = extent(3)
  if (present(ny2)) ny2 = extent(4)
  if (present(nz1)) nz1 = extent(5)
  if (present(nz2)) nz2 = extent(6)
  if (present(origin)) then
    origin = 0._R8P
    call self%xml%element(self%dataset)%get_attribute(name='Origin', value=value)
    if (len(value) > 0) read(value, *, iostat=iostat) origin
    if (iostat /= 0) error = 6
  endif
  if (present(spacing)) then
    spacing = 1._R8P
    call self%xml%element(self%dataset)%get_attribute(name='Spacing', value=value)
    if (len(value) > 0) read(value, *, iostat=iostat) spacing
    if (iostat /= 0) error = 6
  endif
  if (present(direction)) then
    direction = [1._R8P, 0._R8P, 0._R8P, 0._R8P, 1._R8P, 0._R8P, 0._R8P, 0._R8P, 1._R8P]
    call self%xml%element(self%dataset)%get_attribute(name='Direction', value=value)
    if (len(value) > 0) read(value, *, iostat=iostat) direction
    if (iostat /= 0) error = 6
  endif
  if (present(ghost_level)) then
    ghost_level = 0
    call self%xml%element(self%dataset)%get_attribute(name='GhostLevel', value=value)
    if (len(value) > 0) read(value, *, iostat=iostat) ghost_level
    if (iostat /= 0) error = 6
  endif
  endfunction get_info

  function read_piece(self, piece, np, nc, nx1, nx2, ny1, ny2, nz1, nz2, nverts, nlines, nstrips, npolys) result(error)
  !< Read the counts and the extent of a piece.
  !<
  !< For the structured topologies the counts are computed from the extent of the piece. For PolyData `nc` is the number of
  !< all the cells (vertices, lines, strips and polygons).
  class(xml_reader), intent(in)            :: self      !< XML reader.
  integer(I4P),      intent(in),  optional :: piece     !< Piece, from 1 (default 1).
  integer(I8P),      intent(out), optional :: np        !< Number of points.
  integer(I8P),      intent(out), optional :: nc        !< Number of cells.
  integer(I4P),      intent(out), optional :: nx1       !< Initial node of x axis.
  integer(I4P),      intent(out), optional :: nx2       !< Final node of x axis.
  integer(I4P),      intent(out), optional :: ny1       !< Initial node of y axis.
  integer(I4P),      intent(out), optional :: ny2       !< Final node of y axis.
  integer(I4P),      intent(out), optional :: nz1       !< Initial node of z axis.
  integer(I4P),      intent(out), optional :: nz2       !< Final node of z axis.
  integer(I8P),      intent(out), optional :: nverts    !< Number of vertices (PolyData).
  integer(I8P),      intent(out), optional :: nlines    !< Number of lines (PolyData).
  integer(I8P),      intent(out), optional :: nstrips   !< Number of strips (PolyData).
  integer(I8P),      intent(out), optional :: npolys    !< Number of polygons (PolyData).
  integer(I4P)                             :: error     !< Error status.
  integer(I4P)                             :: p         !< Index of the piece element.
  integer(I8P)                             :: counts(6) !< Points, cells, verts, lines, strips, polys.
  integer(I4P)                             :: extent(6) !< Extent.

  error = self%piece_element(piece=piece, id=p)
  if (error /= 0) return
  call piece_counts(element=self%xml%element(p), mesh_topology=self%mesh_topology, counts=counts, extent=extent, error=error)
  if (present(np)) np = counts(1)
  if (present(nc)) nc = counts(2)
  if (present(nverts)) nverts = counts(3)
  if (present(nlines)) nlines = counts(4)
  if (present(nstrips)) nstrips = counts(5)
  if (present(npolys)) npolys = counts(6)
  if (present(nx1)) nx1 = extent(1)
  if (present(nx2)) nx2 = extent(2)
  if (present(ny1)) ny1 = extent(3)
  if (present(ny2)) ny2 = extent(4)
  if (present(nz1)) nz1 = extent(5)
  if (present(nz2)) nz2 = extent(6)
  endfunction read_piece

  function get_dataarray_names(self, location, names, piece) result(error)
  !< Return the names of the dataarrays of a location (node, cell or field), in the order of the file.
  !<
  !< The names are blank padded to the length of the longest one; a location without dataarrays returns no names.
  class(xml_reader),             intent(in)           :: self     !< XML reader.
  character(*),                  intent(in)           :: location !< Location: node, cell or field.
  character(len=:), allocatable, intent(out)          :: names(:) !< Names of the dataarrays.
  integer(I4P),                  intent(in), optional :: piece    !< Piece, from 1 (default 1, ignored for field data).
  integer(I4P)                                        :: error    !< Error status.
  integer(I4P)                                        :: section  !< Index of the location element.
  integer(I4P)                                        :: c        !< Counter.
  integer(I4P)                                        :: n        !< Number of dataarrays.
  integer(I4P)                                        :: l        !< Length of the longest name.
  integer(I4P)                                        :: id       !< Index of a dataarray element.
  character(len=:), allocatable                       :: value    !< Attribute value.

  allocate(character(len=0) :: names(1:0))
  ! a missing piece or an unknown location is an error, a location without arrays is not
  error = 4
  select case(lower_case(trim(adjustl(location))))
  case('node', 'cell')
    if (.not.self%is_parallel) then
      error = self%piece_element(piece=piece, id=id) ; if (error /= 0) return
    endif
    if (.not.self%is_initialized) return
  case('field')
    if (.not.self%is_initialized) return
  case default
    return
  endselect
  error = self%find_dataarray(location=location, data_name='', piece=piece, id=id, section=section)
  error = 0
  if (section == 0) return
  n = 0 ; l = 0
  do c=1, self%xml%element(section)%children_number
    id = self%xml%element(section)%child(c)
    if (.not.is_dataarray(self%xml%element(id)%name)) cycle
    call self%xml%element(id)%get_attribute(name='Name', value=value)
    n = n + 1
    l = max(l, len(value))
  enddo
  deallocate(names)
  allocate(character(len=l) :: names(1:n))
  n = 0
  do c=1, self%xml%element(section)%children_number
    id = self%xml%element(section)%child(c)
    if (.not.is_dataarray(self%xml%element(id)%name)) cycle
    call self%xml%element(id)%get_attribute(name='Name', value=value)
    n = n + 1
    names(n) = value
  enddo
  endfunction get_dataarray_names

  function get_dataarray_info(self, location, data_name, piece, data_type, n_components, n_tuples) result(error)
  !< Return the VTK type, the number of components and the number of tuples of a dataarray, without reading it.
  !<
  !< The number of tuples is the `NumberOfTuples` attribute when present, otherwise the number of points or cells of the
  !< piece; -1 when unknown (field data without `NumberOfTuples`, arrays declared by a parallel header).
  class(xml_reader),             intent(in)            :: self         !< XML reader.
  character(*),                  intent(in)            :: location     !< Location: node, cell or field.
  character(*),                  intent(in)            :: data_name    !< Name of the dataarray.
  integer(I4P),                  intent(in),  optional :: piece        !< Piece, from 1 (default 1, ignored for field data).
  character(len=:), allocatable, intent(out), optional :: data_type    !< VTK type, e.g. Float64, Int32, UInt8, String.
  integer(I4P),                  intent(out), optional :: n_components !< Number of components.
  integer(I8P),                  intent(out), optional :: n_tuples     !< Number of tuples.
  integer(I4P)                                         :: error        !< Error status.
  integer(I4P)                                         :: id           !< Index of the dataarray element.
  integer(I4P)                                         :: section      !< Index of the location element.
  integer(I4P)                                         :: p            !< Index of the piece element.
  integer(I8P)                                         :: counts(6)    !< Points, cells, verts, lines, strips, polys.
  integer(I4P)                                         :: extent(6)    !< Extent.
  character(len=:), allocatable                        :: value        !< Attribute value.
  integer(I4P)                                         :: iostat       !< IO status.

  error = self%find_dataarray(location=location, data_name=data_name, piece=piece, id=id, section=section)
  if (error /= 0) return
  iostat = 0
  if (present(data_type)) call self%xml%element(id)%get_attribute(name='type', value=data_type)
  if (present(n_components)) then
    n_components = 1
    call self%xml%element(id)%get_attribute(name='NumberOfComponents', value=value)
    if (len(value) > 0) read(value, *, iostat=iostat) n_components
  endif
  if (present(n_tuples)) then
    n_tuples = -1_I8P
    call self%xml%element(id)%get_attribute(name='NumberOfTuples', value=value)
    if (len(value) > 0) then
      read(value, *, iostat=iostat) n_tuples
    elseif (self%xml%element(section)%name /= 'FieldData' .and. .not.self%is_parallel) then
      p = self%xml%element(section)%parent
      call piece_counts(element=self%xml%element(p), mesh_topology=self%mesh_topology, counts=counts, extent=extent, &
                        error=error)
      n_tuples = merge(counts(1), counts(2), self%xml%element(section)%name == 'PointData')
    endif
  endif
  endfunction get_dataarray_info

  function read_dataarray_strings(self, location, data_name, x, piece) result(error)
  !< Read a dataarray of strings (type String, as field data strings are written).
  class(xml_reader),             intent(in)           :: self      !< XML reader.
  character(*),                  intent(in)           :: location  !< Location: node, cell or field.
  character(*),                  intent(in)           :: data_name !< Name of the dataarray.
  character(len=:), allocatable, intent(out)          :: x(:)      !< Strings, blank padded to the longest one.
  integer(I4P),                  intent(in), optional :: piece     !< Piece, from 1 (default 1, ignored for field data).
  integer(I4P)                                        :: error     !< Error status.
  integer(I1P), allocatable                           :: bytes(:)  !< Bytes of the dataarray.
  character(len=:), allocatable                       :: data_type !< VTK type of the dataarray.
  integer(I4P)                                        :: nc        !< Number of components.

  allocate(character(len=0) :: x(1:0))
  error = self%read_named_dataarray(location=location, data_name=data_name, piece=piece, bytes=bytes, data_type=data_type, &
                                    n_components=nc)
  if (error /= 0) return
  error = 5
  if (data_type /= 'String') return
  call bytes_to_strings(bytes=bytes, x=x, error=error)
  endfunction read_dataarray_strings

  function read_geo_R8P(self, x, y, z, piece) result(error)
  !< Read the geometry of a piece (R8P): the coordinates of the points, or the coordinates along each axis of a
  !< RectilinearGrid. ImageData have no stored geometry (see `get_info`): the error is 4.
  class(xml_reader),      intent(in)           :: self   !< XML reader.
  real(R8P), allocatable, intent(out)          :: x(:)   !< X coordinates.
  real(R8P), allocatable, intent(out)          :: y(:)   !< Y coordinates.
  real(R8P), allocatable, intent(out)          :: z(:)   !< Z coordinates.
  integer(I4P),           intent(in), optional :: piece  !< Piece, from 1 (default 1).
  integer(I4P)                                 :: error  !< Error status.
  real(R8P), allocatable                       :: xyz(:) !< Coordinates of the points, interleaved.
  integer(I4P)                                 :: id(3)  !< Indexes of the geometry dataarray elements.
  integer(I4P)                                 :: n      !< Number of geometry dataarrays.

  allocate(x(1:0), y(1:0), z(1:0))
  error = self%geometry_dataarrays(piece=piece, id=id, n=n)
  if (error /= 0) return
  if (n == 3) then
    error = read_coordinate_R8P(self, id(1), x) ; if (error /= 0) return
    error = read_coordinate_R8P(self, id(2), y) ; if (error /= 0) return
    error = read_coordinate_R8P(self, id(3), z)
  else
    error = read_coordinate_R8P(self, id(1), xyz, n_components=3)
    if (error /= 0) return
    x = xyz(1::3) ; y = xyz(2::3) ; z = xyz(3::3)
  endif
  endfunction read_geo_R8P

  function read_geo_R4P(self, x, y, z, piece) result(error)
  !< Read the geometry of a piece (R4P): the coordinates of the points, or the coordinates along each axis of a
  !< RectilinearGrid. ImageData have no stored geometry (see `get_info`): the error is 4.
  class(xml_reader),      intent(in)           :: self   !< XML reader.
  real(R4P), allocatable, intent(out)          :: x(:)   !< X coordinates.
  real(R4P), allocatable, intent(out)          :: y(:)   !< Y coordinates.
  real(R4P), allocatable, intent(out)          :: z(:)   !< Z coordinates.
  integer(I4P),           intent(in), optional :: piece  !< Piece, from 1 (default 1).
  integer(I4P)                                 :: error  !< Error status.
  real(R4P), allocatable                       :: xyz(:) !< Coordinates of the points, interleaved.
  integer(I4P)                                 :: id(3)  !< Indexes of the geometry dataarray elements.
  integer(I4P)                                 :: n      !< Number of geometry dataarrays.

  allocate(x(1:0), y(1:0), z(1:0))
  error = self%geometry_dataarrays(piece=piece, id=id, n=n)
  if (error /= 0) return
  if (n == 3) then
    error = read_coordinate_R4P(self, id(1), x) ; if (error /= 0) return
    error = read_coordinate_R4P(self, id(2), y) ; if (error /= 0) return
    error = read_coordinate_R4P(self, id(3), z)
  else
    error = read_coordinate_R4P(self, id(1), xyz, n_components=3)
    if (error /= 0) return
    x = xyz(1::3) ; y = xyz(2::3) ; z = xyz(3::3)
  endif
  endfunction read_geo_R4P

  function read_connectivity_I8P(self, connectivity, offset, cell_type, face, faceoffset, piece) result(error)
  !< Read the cells of an UnstructuredGrid piece (I8P): connectivity, offsets, types and, if present, polyhedra faces.
  !<
  !< The ids can be of any integer type in the file. Asking for `face`/`faceoffset` when the file has none is an error 4.
  class(xml_reader),         intent(in)            :: self            !< XML reader.
  integer(I8P), allocatable, intent(out)           :: connectivity(:) !< Connectivity.
  integer(I8P), allocatable, intent(out)           :: offset(:)       !< Offsets: end of each cell in the connectivity.
  integer(I1P), allocatable, intent(out), optional :: cell_type(:)    !< VTK cell types.
  integer(I8P), allocatable, intent(out), optional :: face(:)         !< Faces stream of the polyhedra.
  integer(I8P), allocatable, intent(out), optional :: faceoffset(:)   !< End of the faces of each cell, -1 if none.
  integer(I4P),              intent(in),  optional :: piece           !< Piece, from 1 (default 1).
  integer(I4P)                                     :: error           !< Error status.
  integer(I8P), allocatable                        :: ids(:)          !< Cell types, as read.

  allocate(connectivity(1:0), offset(1:0))
  error = 4
  if (.not.self%is_initialized) return
  if (self%mesh_topology /= 'UnstructuredGrid') return
  error = self%read_ids(piece=piece, section='Cells', data_name='connectivity', ids=connectivity) ; if (error /= 0) return
  error = self%read_ids(piece=piece, section='Cells', data_name='offsets', ids=offset) ; if (error /= 0) return
  if (present(cell_type)) then
    error = self%read_ids(piece=piece, section='Cells', data_name='types', ids=ids) ; if (error /= 0) return
    cell_type = int(ids, I1P)
  endif
  if (present(face)) then
    error = self%read_ids(piece=piece, section='Cells', data_name='faces', ids=face) ; if (error /= 0) return
  endif
  if (present(faceoffset)) then
    error = self%read_ids(piece=piece, section='Cells', data_name='faceoffsets', ids=faceoffset) ; if (error /= 0) return
  endif
  endfunction read_connectivity_I8P

  function read_connectivity_I4P(self, connectivity, offset, cell_type, face, faceoffset, piece) result(error)
  !< Read the cells of an UnstructuredGrid piece (I4P): connectivity, offsets, types and, if present, polyhedra faces.
  !<
  !< The ids can be of any integer type in the file (VTK writes Int64): values that do not fit I4P are an error 5.
  class(xml_reader),         intent(in)            :: self            !< XML reader.
  integer(I4P), allocatable, intent(out)           :: connectivity(:) !< Connectivity.
  integer(I4P), allocatable, intent(out)           :: offset(:)       !< Offsets: end of each cell in the connectivity.
  integer(I1P), allocatable, intent(out), optional :: cell_type(:)    !< VTK cell types.
  integer(I4P), allocatable, intent(out), optional :: face(:)         !< Faces stream of the polyhedra.
  integer(I4P), allocatable, intent(out), optional :: faceoffset(:)   !< End of the faces of each cell, -1 if none.
  integer(I4P),              intent(in),  optional :: piece           !< Piece, from 1 (default 1).
  integer(I4P)                                     :: error           !< Error status.
  integer(I8P), allocatable                        :: ids(:)          !< Ids, as read.

  allocate(connectivity(1:0), offset(1:0))
  error = 4
  if (.not.self%is_initialized) return
  if (self%mesh_topology /= 'UnstructuredGrid') return
  error = self%read_ids(piece=piece, section='Cells', data_name='connectivity', ids=ids) ; if (error /= 0) return
  error = to_I4P(ids, connectivity) ; if (error /= 0) return
  error = self%read_ids(piece=piece, section='Cells', data_name='offsets', ids=ids) ; if (error /= 0) return
  error = to_I4P(ids, offset) ; if (error /= 0) return
  if (present(cell_type)) then
    error = self%read_ids(piece=piece, section='Cells', data_name='types', ids=ids) ; if (error /= 0) return
    cell_type = int(ids, I1P)
  endif
  if (present(face)) then
    error = self%read_ids(piece=piece, section='Cells', data_name='faces', ids=ids) ; if (error /= 0) return
    error = to_I4P(ids, face) ; if (error /= 0) return
  endif
  if (present(faceoffset)) then
    error = self%read_ids(piece=piece, section='Cells', data_name='faceoffsets', ids=ids) ; if (error /= 0) return
    error = to_I4P(ids, faceoffset) ; if (error /= 0) return
  endif
  endfunction read_connectivity_I4P

  function read_polydata_cells_I8P(self, block, connectivity, offset, piece) result(error)
  !< Read one block of cells of a PolyData piece (I8P): `block` is verts, lines, strips or polys (case insensitive).
  !<
  !< A block absent from the file, with no cells in the piece, returns empty arrays.
  class(xml_reader),         intent(in)           :: self            !< XML reader.
  character(*),              intent(in)           :: block           !< Block of cells: verts, lines, strips or polys.
  integer(I8P), allocatable, intent(out)          :: connectivity(:) !< Connectivity.
  integer(I8P), allocatable, intent(out)          :: offset(:)       !< Offsets: end of each cell in the connectivity.
  integer(I4P),              intent(in), optional :: piece           !< Piece, from 1 (default 1).
  integer(I4P)                                    :: error           !< Error status.
  character(len=:), allocatable                   :: section         !< Name of the block element.

  allocate(connectivity(1:0), offset(1:0))
  error = polydata_section(self=self, block=block, piece=piece, section=section)
  if (error /= 0 .or. len(section) == 0) return
  error = self%read_ids(piece=piece, section=section, data_name='connectivity', ids=connectivity) ; if (error /= 0) return
  error = self%read_ids(piece=piece, section=section, data_name='offsets', ids=offset)
  endfunction read_polydata_cells_I8P

  function read_polydata_cells_I4P(self, block, connectivity, offset, piece) result(error)
  !< Read one block of cells of a PolyData piece (I4P): `block` is verts, lines, strips or polys (case insensitive).
  !<
  !< A block absent from the file, with no cells in the piece, returns empty arrays. Values that do not fit I4P are an
  !< error 5.
  class(xml_reader),         intent(in)           :: self            !< XML reader.
  character(*),              intent(in)           :: block           !< Block of cells: verts, lines, strips or polys.
  integer(I4P), allocatable, intent(out)          :: connectivity(:) !< Connectivity.
  integer(I4P), allocatable, intent(out)          :: offset(:)       !< Offsets: end of each cell in the connectivity.
  integer(I4P),              intent(in), optional :: piece           !< Piece, from 1 (default 1).
  integer(I4P)                                    :: error           !< Error status.
  character(len=:), allocatable                   :: section         !< Name of the block element.
  integer(I8P), allocatable                       :: ids(:)          !< Ids, as read.

  allocate(connectivity(1:0), offset(1:0))
  error = polydata_section(self=self, block=block, piece=piece, section=section)
  if (error /= 0 .or. len(section) == 0) return
  error = self%read_ids(piece=piece, section=section, data_name='connectivity', ids=ids) ; if (error /= 0) return
  error = to_I4P(ids, connectivity) ; if (error /= 0) return
  error = self%read_ids(piece=piece, section=section, data_name='offsets', ids=ids) ; if (error /= 0) return
  error = to_I4P(ids, offset)
  endfunction read_polydata_cells_I4P

  function get_sources(self, sources) result(error)
  !< Return the files of the pieces of a parallel header (the `Source` attributes), as written in the file.
  !<
  !< Relative paths are relative to the directory of the header. The names are blank padded to the length of the longest
  !< one. The extents of the pieces of structured grids are returned by `read_piece(piece, nx1, ...)`.
  class(xml_reader),             intent(in)  :: self       !< XML reader.
  character(len=:), allocatable, intent(out) :: sources(:) !< Files of the pieces.
  integer(I4P)                               :: error      !< Error status.
  character(len=:), allocatable              :: value      !< Attribute value.
  integer(I4P)                               :: p          !< Counter.
  integer(I4P)                               :: l          !< Length of the longest source.

  allocate(character(len=0) :: sources(1:0))
  error = 4
  if (.not.self%is_initialized .or. .not.self%is_parallel) return
  l = 0
  do p=1, self%pieces_number
    call self%xml%element(self%xml%find_child(parent=self%dataset, name='Piece', n=p))%get_attribute(name='Source', &
                                                                                                    value=value)
    l = max(l, len(value))
  enddo
  deallocate(sources)
  allocate(character(len=l) :: sources(1:self%pieces_number))
  do p=1, self%pieces_number
    call self%xml%element(self%xml%find_child(parent=self%dataset, name='Piece', n=p))%get_attribute(name='Source', &
                                                                                                    value=value)
    sources(p) = value
  enddo
  error = 0
  endfunction get_sources

  function check_pieces(self, message) result(error)
  !< Check the pieces of a parallel header against it.
  !<
  !< Each piece file is read (its index only, not its data) and must be a dataset of the type of the header (e.g. an
  !< UnstructuredGrid for a PUnstructuredGrid), with points or coordinates of the declared type (`PPoints`, `PCoordinates`)
  !< and, in every one of its pieces, every array declared by `PPointData` and `PCellData` with the same type and number of
  !< components (a piece can hold more arrays). VTK readers otherwise drop arrays, fill them with zeros or fail.
  !<
  !< The error is 0 when all the pieces match, 7 when one does not, or the error of reading a piece (1, 2, 3); `message`
  !< then describes the first mismatch found.
  class(xml_reader),             intent(in)            :: self      !< XML reader.
  character(len=:), allocatable, intent(out), optional :: message   !< Description of the first mismatch, empty if none.
  integer(I4P)                                         :: error     !< Error status.
  type(xml_reader)                                     :: piece     !< Reader of a piece.
  character(len=:), allocatable                        :: header    !< File name of the header.
  character(len=:), allocatable                        :: source    !< File of a piece, as written in the header.
  character(len=:), allocatable                        :: path      !< File of a piece.
  character(len=:), allocatable                        :: msg       !< Description of the mismatch.
  character(len=:), allocatable                        :: name      !< Name of a declared array.
  character(len=:), allocatable                        :: dtype     !< Type of a declared array.
  character(len=:), allocatable                        :: ptype     !< Type of an array of a piece.
  character(len=:), allocatable                        :: value     !< Attribute value.
  integer(I4P)                                         :: p         !< Counter.
  integer(I4P)                                         :: q         !< Counter.
  integer(I4P)                                         :: l         !< Counter.
  integer(I4P)                                         :: c         !< Counter.
  integer(I4P)                                         :: s         !< Index of a section element.
  integer(I4P)                                         :: a         !< Index of a declared array element.
  integer(I4P)                                         :: nc        !< Number of components of a declared array.
  integer(I4P)                                         :: pnc       !< Number of components of an array of a piece.
  integer(I4P)                                         :: id(3)     !< Indexes of the geometry elements of a piece.
  integer(I4P)                                         :: n         !< Number of geometry elements of a piece.
  integer(I4P)                                         :: iostat    !< IO status.
  character(len=10), parameter                         :: sections(2)=['PPointData', 'PCellData '] !< Data sections.
  character(len=4),  parameter                         :: locations(2)=['node', 'cell'] !< Locations of the sections.

  msg = ''
  error = 4
  if (.not.self%is_initialized .or. .not.self%is_parallel) then
    msg = 'the file is not a parallel header open for reading'
    if (present(message)) message = msg
    return
  endif
  header = self%filename
  pieces_loop: do p=1, self%pieces_number
    call self%xml%element(self%xml%find_child(parent=self%dataset, name='Piece', n=p))%get_attribute(name='Source', &
                                                                                                    value=source)
    path = source
    if (source(1:min(1, len(source))) /= '/') path = header(1:index(header, '/', back=.true.))//source
    error = piece%initialize(filename=path)
    if (error /= 0) then
      msg = 'piece '//trim(str(p, no_sign=.true.))//' ('//source//') cannot be read'
      exit pieces_loop
    endif
    error = 7
    if (piece%mesh_topology /= self%mesh_topology(2:)) then
      msg = 'piece '//trim(str(p, no_sign=.true.))//' ('//source//') is a '//piece%mesh_topology//', not a '// &
            self%mesh_topology(2:)
      exit pieces_loop
    endif
    ! type of the points or coordinates
    s = self%xml%find_child(parent=self%dataset, name='PPoints')
    if (s == 0) s = self%xml%find_child(parent=self%dataset, name='PCoordinates')
    if (s > 0) then
      a = self%xml%find_child(parent=s, name='PDataArray')
      if (a > 0) then
        call self%xml%element(a)%get_attribute(name='type', value=dtype)
        do q=1, piece%pieces_number
          if (piece%geometry_dataarrays(piece=q, id=id, n=n) /= 0) cycle
          do c=1, n
            call piece%xml%element(id(c))%get_attribute(name='type', value=ptype)
            if (ptype /= dtype) then
              msg = 'piece '//trim(str(p, no_sign=.true.))//' ('//source//'): coordinates of type '//ptype// &
                    ', declared '//dtype
              exit pieces_loop
            endif
          enddo
        enddo
      endif
    endif
    ! declared arrays
    do l=1, size(sections)
      s = self%xml%find_child(parent=self%dataset, name=trim(sections(l)))
      if (s == 0) cycle
      do c=1, self%xml%element(s)%children_number
        a = self%xml%element(s)%child(c)
        if (.not.is_dataarray(self%xml%element(a)%name)) cycle
        call self%xml%element(a)%get_attribute(name='Name', value=name)
        call self%xml%element(a)%get_attribute(name='type', value=dtype)
        nc = 1
        call self%xml%element(a)%get_attribute(name='NumberOfComponents', value=value)
        if (len(value) > 0) read(value, *, iostat=iostat) nc
        do q=1, piece%pieces_number
          if (piece%get_dataarray_info(location=trim(locations(l)), data_name=name, piece=q, data_type=ptype, &
                                       n_components=pnc) /= 0) then
            msg = 'piece '//trim(str(p, no_sign=.true.))//' ('//source//'): '//trim(locations(l))//' array "'//name// &
                  '" declared but missing'
            exit pieces_loop
          endif
          if (ptype /= dtype .or. pnc /= nc) then
            msg = 'piece '//trim(str(p, no_sign=.true.))//' ('//source//'): '//trim(locations(l))//' array "'//name// &
                  '" is '//ptype//' with '//trim(str(pnc, no_sign=.true.))//' components, declared '//dtype//' with '// &
                  trim(str(nc, no_sign=.true.))
            exit pieces_loop
          endif
        enddo
      enddo
    enddo
    call piece%finalize
    error = 0
  enddo pieces_loop
  call piece%finalize
  if (present(message)) message = msg
  endfunction check_pieces

  ! private methods
  function dataarray_bytes(self, id, bytes, data_type, n_components) result(error)
  !< Return the bytes (native byte order), the VTK type and the number of components of a dataarray element.
  class(xml_reader),             intent(in)  :: self         !< XML reader.
  integer(I4P),                  intent(in)  :: id           !< Index of the dataarray element.
  integer(I1P),     allocatable, intent(out) :: bytes(:)     !< Bytes of the dataarray.
  character(len=:), allocatable, intent(out) :: data_type    !< VTK type.
  integer(I4P),                  intent(out) :: n_components !< Number of components.
  integer(I4P)                               :: error        !< Error status.
  character(len=:), allocatable              :: value        !< Attribute value.
  character(len=:), allocatable              :: code         !< Text of the dataarray.
  integer(I8P)                               :: offset       !< Offset of the appended data.
  integer(I8P)                               :: n_tuples     !< Number of tuples declared.
  integer(I8P)                               :: sz           !< Size of a value.
  integer(I4P)                               :: iostat       !< IO status.

  allocate(bytes(1:0))
  data_type = ''
  n_components = 1
  error = 6
  call self%xml%element(id)%get_attribute(name='type', value=data_type)
  sz = data_type_size(data_type)
  if (sz == 0_I8P) then
    error = 3
    return
  endif
  call self%xml%element(id)%get_attribute(name='NumberOfComponents', value=value)
  if (len(value) > 0) then
    read(value, *, iostat=iostat) n_components
    if (iostat /= 0 .or. n_components < 1) return
  endif
  call self%xml%element(id)%get_attribute(name='format', value=value)
  select case(value)
  case('ascii')
    error = read_chars(filename=self%filename, first=self%xml%element(id)%content_start, &
                       last=self%xml%element(id)%text_end, chars=code)
    if (error /= 0) return
    call decode_ascii(text=code, data_type=data_type, bytes=bytes, error=error)
  case('binary')
    error = read_chars(filename=self%filename, first=self%xml%element(id)%content_start, &
                       last=self%xml%element(id)%text_end, chars=code)
    if (error /= 0) return
    call strip_spaces(code)
    call decode_base64(code=code, word_size=self%word_size, is_compressed=self%is_compressed, bytes=bytes, error=error)
  case('appended')
    if (self%appended_start == 0_I8P) return
    call self%xml%element(id)%get_attribute(name='offset', value=value)
    read(value, *, iostat=iostat) offset
    if (iostat /= 0 .or. offset < 0_I8P) return
    if (self%is_appended_raw) then
      error = read_appended_raw(self=self, pos=self%appended_start+offset, bytes=bytes)
    else
      error = read_appended_base64(self=self, pos=self%appended_start+offset, bytes=bytes)
    endif
  case default
    error = 3
  endselect
  if (error /= 0) return
  ! consistency of the size with the type, the components and the declared tuples
  error = 6
  if (mod(size(bytes, kind=I8P), sz) /= 0_I8P) return
  if (data_type /= 'String') then
    if (mod(size(bytes, kind=I8P)/sz, int(n_components, I8P)) /= 0_I8P) return
    call self%xml%element(id)%get_attribute(name='NumberOfTuples', value=value)
    if (len(value) > 0) then
      read(value, *, iostat=iostat) n_tuples
      if (iostat /= 0 .or. n_tuples * n_components /= size(bytes, kind=I8P)/sz) return
    endif
  endif
  error = 0
  endfunction dataarray_bytes

  function find_dataarray(self, location, data_name, piece, id, section) result(error)
  !< Return the index of a named dataarray element of a location, and of the location element (0 if absent).
  class(xml_reader), intent(in)           :: self      !< XML reader.
  character(*),      intent(in)           :: location  !< Location: node, cell or field.
  character(*),      intent(in)           :: data_name !< Name of the dataarray.
  integer(I4P),      intent(in), optional :: piece     !< Piece, from 1 (default 1, ignored for field data).
  integer(I4P),      intent(out)          :: id        !< Index of the dataarray element, 0 if not found.
  integer(I4P),      intent(out)          :: section   !< Index of the location element, 0 if not found.
  integer(I4P)                            :: error     !< Error status.
  integer(I4P)                            :: p         !< Index of the piece element.
  integer(I4P)                            :: c         !< Counter.
  character(len=:), allocatable           :: value     !< Attribute value.

  id = 0
  section = 0
  error = 4
  if (.not.self%is_initialized) return
  select case(lower_case(trim(adjustl(location))))
  case('node')
    if (self%is_parallel) then
      section = self%xml%find_child(parent=self%dataset, name='PPointData')
    else
      error = self%piece_element(piece=piece, id=p) ; if (error /= 0) return
      section = self%xml%find_child(parent=p, name='PointData')
    endif
  case('cell')
    if (self%is_parallel) then
      section = self%xml%find_child(parent=self%dataset, name='PCellData')
    else
      error = self%piece_element(piece=piece, id=p) ; if (error /= 0) return
      section = self%xml%find_child(parent=p, name='CellData')
    endif
  case('field')
    section = self%xml%find_child(parent=self%dataset, name='FieldData')
  endselect
  error = 4
  if (section == 0) return
  do c=1, self%xml%element(section)%children_number
    if (.not.is_dataarray(self%xml%element(self%xml%element(section)%child(c))%name)) cycle
    call self%xml%element(self%xml%element(section)%child(c))%get_attribute(name='Name', value=value)
    if (value == data_name) then
      id = self%xml%element(section)%child(c)
      error = 0
      return
    endif
  enddo
  endfunction find_dataarray

  function geometry_dataarrays(self, piece, id, n) result(error)
  !< Return the indexes of the geometry dataarray elements of a piece: Points (n=1) or the 3 Coordinates (n=3).
  class(xml_reader), intent(in)           :: self  !< XML reader.
  integer(I4P),      intent(in), optional :: piece !< Piece, from 1 (default 1).
  integer(I4P),      intent(out)          :: id(3) !< Indexes of the geometry dataarray elements.
  integer(I4P),      intent(out)          :: n     !< Number of geometry dataarray elements.
  integer(I4P)                            :: error !< Error status.
  integer(I4P)                            :: p     !< Index of the piece element.
  integer(I4P)                            :: g     !< Index of the geometry element.
  integer(I4P)                            :: c     !< Counter.

  id = 0
  n = 0
  error = self%piece_element(piece=piece, id=p) ; if (error /= 0) return
  error = 4
  select case(self%mesh_topology)
  case('RectilinearGrid')
    g = self%xml%find_child(parent=p, name='Coordinates')
  case('StructuredGrid', 'UnstructuredGrid', 'PolyData')
    g = self%xml%find_child(parent=p, name='Points')
  case default
    return
  endselect
  if (g == 0) return
  do c=1, self%xml%element(g)%children_number
    if (.not.is_dataarray(self%xml%element(self%xml%element(g)%child(c))%name)) cycle
    n = n + 1
    id(n) = self%xml%element(g)%child(c)
    if (n == 3) exit
  enddo
  if ((self%mesh_topology == 'RectilinearGrid' .and. n == 3) .or. (self%mesh_topology /= 'RectilinearGrid' .and. n >= 1)) then
    if (self%mesh_topology /= 'RectilinearGrid') n = 1
    error = 0
  endif
  endfunction geometry_dataarrays

  function piece_element(self, piece, id) result(error)
  !< Return the index of a piece element.
  class(xml_reader), intent(in)           :: self   !< XML reader.
  integer(I4P),      intent(in), optional :: piece  !< Piece, from 1 (default 1).
  integer(I4P),      intent(out)          :: id     !< Index of the piece element.
  integer(I4P)                            :: error  !< Error status.
  integer(I4P)                            :: piece_ !< Piece, local variable.

  id = 0
  error = 4
  if (.not.self%is_initialized) return
  piece_ = 1 ; if (present(piece)) piece_ = piece
  if (piece_ < 1 .or. piece_ > self%pieces_number) return
  id = self%xml%find_child(parent=self%dataset, name='Piece', n=piece_)
  if (id > 0) error = 0
  endfunction piece_element

  function read_ids(self, piece, section, data_name, ids) result(error)
  !< Read a dataarray of ids (any integer type) of a section of a piece (e.g. Cells, Polys) into I8P.
  class(xml_reader),         intent(in)           :: self      !< XML reader.
  integer(I4P),              intent(in), optional :: piece     !< Piece, from 1 (default 1).
  character(*),              intent(in)           :: section   !< Name of the section element.
  character(*),              intent(in)           :: data_name !< Name of the dataarray.
  integer(I8P), allocatable, intent(out)          :: ids(:)    !< Ids.
  integer(I4P)                                    :: error     !< Error status.
  integer(I1P), allocatable                       :: bytes(:)  !< Bytes of the dataarray.
  character(len=:), allocatable                   :: data_type !< VTK type of the dataarray.
  character(len=:), allocatable                   :: value     !< Attribute value.
  integer(I4P)                                    :: p         !< Index of the piece element.
  integer(I4P)                                    :: s         !< Index of the section element.
  integer(I4P)                                    :: c         !< Counter.
  integer(I4P)                                    :: id        !< Index of the dataarray element.
  integer(I4P)                                    :: nc        !< Number of components.

  allocate(ids(1:0))
  error = self%piece_element(piece=piece, id=p) ; if (error /= 0) return
  error = 4
  s = self%xml%find_child(parent=p, name=section)
  if (s == 0) return
  id = 0
  do c=1, self%xml%element(s)%children_number
    if (.not.is_dataarray(self%xml%element(self%xml%element(s)%child(c))%name)) cycle
    call self%xml%element(self%xml%element(s)%child(c))%get_attribute(name='Name', value=value)
    if (value == data_name) then
      id = self%xml%element(s)%child(c)
      exit
    endif
  enddo
  if (id == 0) return
  error = self%dataarray_bytes(id=id, bytes=bytes, data_type=data_type, n_components=nc)
  if (error /= 0) return
  call bytes_to_array(bytes=bytes, data_type=data_type, x=ids, error=error)
  endfunction read_ids

  function read_named_dataarray(self, location, data_name, piece, bytes, data_type, n_components) result(error)
  !< Return the bytes, the VTK type and the number of components of a named dataarray of a location.
  class(xml_reader),             intent(in)           :: self         !< XML reader.
  character(*),                  intent(in)           :: location     !< Location: node, cell or field.
  character(*),                  intent(in)           :: data_name    !< Name of the dataarray.
  integer(I4P),                  intent(in), optional :: piece        !< Piece, from 1 (default 1, ignored for field data).
  integer(I1P),     allocatable, intent(out)          :: bytes(:)     !< Bytes of the dataarray.
  character(len=:), allocatable, intent(out)          :: data_type    !< VTK type.
  integer(I4P),                  intent(out)          :: n_components !< Number of components.
  integer(I4P)                                        :: error        !< Error status.
  integer(I4P)                                        :: id           !< Index of the dataarray element.
  integer(I4P)                                        :: section      !< Index of the location element.

  allocate(bytes(1:0))
  data_type = ''
  n_components = 1
  error = self%find_dataarray(location=location, data_name=data_name, piece=piece, id=id, section=section)
  if (error /= 0) return
  ! the arrays of a parallel header hold no data
  error = 4
  if (self%is_parallel) return
  error = self%dataarray_bytes(id=id, bytes=bytes, data_type=data_type, n_components=n_components)
  endfunction read_named_dataarray

  function read_dataarray_rank1_R8P(self, location, data_name, x, piece) result(error)
  !< Read a dataarray, flattened (components interleaved), into an array (R8P).
  class(xml_reader),        intent(in)           :: self      !< XML reader.
  character(*),             intent(in)           :: location  !< Location: node, cell or field.
  character(*),             intent(in)           :: data_name !< Name of the dataarray.
  real(R8P),    allocatable, intent(out)          :: x(:)      !< Values.
  integer(I4P),             intent(in), optional :: piece     !< Piece, from 1 (default 1, ignored for field data).
  integer(I4P)                                   :: error     !< Error status.
  integer(I1P), allocatable                      :: bytes(:)  !< Bytes of the dataarray.
  character(len=:), allocatable                  :: data_type !< VTK type of the dataarray.
  integer(I4P)                                   :: nc        !< Number of components.

  allocate(x(1:0))
  error = self%read_named_dataarray(location=location, data_name=data_name, piece=piece, bytes=bytes, data_type=data_type, &
                                    n_components=nc)
  if (error /= 0) return
  call bytes_to_array(bytes=bytes, data_type=data_type, x=x, error=error)
  endfunction read_dataarray_rank1_R8P

  function read_dataarray_rank1_R4P(self, location, data_name, x, piece) result(error)
  !< Read a dataarray, flattened (components interleaved), into an array (R4P).
  class(xml_reader),        intent(in)           :: self      !< XML reader.
  character(*),             intent(in)           :: location  !< Location: node, cell or field.
  character(*),             intent(in)           :: data_name !< Name of the dataarray.
  real(R4P),    allocatable, intent(out)          :: x(:)      !< Values.
  integer(I4P),             intent(in), optional :: piece     !< Piece, from 1 (default 1, ignored for field data).
  integer(I4P)                                   :: error     !< Error status.
  integer(I1P), allocatable                      :: bytes(:)  !< Bytes of the dataarray.
  character(len=:), allocatable                  :: data_type !< VTK type of the dataarray.
  integer(I4P)                                   :: nc        !< Number of components.

  allocate(x(1:0))
  error = self%read_named_dataarray(location=location, data_name=data_name, piece=piece, bytes=bytes, data_type=data_type, &
                                    n_components=nc)
  if (error /= 0) return
  call bytes_to_array(bytes=bytes, data_type=data_type, x=x, error=error)
  endfunction read_dataarray_rank1_R4P

  function read_dataarray_rank1_I8P(self, location, data_name, x, piece) result(error)
  !< Read a dataarray, flattened (components interleaved), into an array (I8P).
  class(xml_reader),        intent(in)           :: self      !< XML reader.
  character(*),             intent(in)           :: location  !< Location: node, cell or field.
  character(*),             intent(in)           :: data_name !< Name of the dataarray.
  integer(I8P), allocatable, intent(out)          :: x(:)      !< Values.
  integer(I4P),             intent(in), optional :: piece     !< Piece, from 1 (default 1, ignored for field data).
  integer(I4P)                                   :: error     !< Error status.
  integer(I1P), allocatable                      :: bytes(:)  !< Bytes of the dataarray.
  character(len=:), allocatable                  :: data_type !< VTK type of the dataarray.
  integer(I4P)                                   :: nc        !< Number of components.

  allocate(x(1:0))
  error = self%read_named_dataarray(location=location, data_name=data_name, piece=piece, bytes=bytes, data_type=data_type, &
                                    n_components=nc)
  if (error /= 0) return
  call bytes_to_array(bytes=bytes, data_type=data_type, x=x, error=error)
  endfunction read_dataarray_rank1_I8P

  function read_dataarray_rank1_I4P(self, location, data_name, x, piece) result(error)
  !< Read a dataarray, flattened (components interleaved), into an array (I4P).
  class(xml_reader),        intent(in)           :: self      !< XML reader.
  character(*),             intent(in)           :: location  !< Location: node, cell or field.
  character(*),             intent(in)           :: data_name !< Name of the dataarray.
  integer(I4P), allocatable, intent(out)          :: x(:)      !< Values.
  integer(I4P),             intent(in), optional :: piece     !< Piece, from 1 (default 1, ignored for field data).
  integer(I4P)                                   :: error     !< Error status.
  integer(I1P), allocatable                      :: bytes(:)  !< Bytes of the dataarray.
  character(len=:), allocatable                  :: data_type !< VTK type of the dataarray.
  integer(I4P)                                   :: nc        !< Number of components.

  allocate(x(1:0))
  error = self%read_named_dataarray(location=location, data_name=data_name, piece=piece, bytes=bytes, data_type=data_type, &
                                    n_components=nc)
  if (error /= 0) return
  call bytes_to_array(bytes=bytes, data_type=data_type, x=x, error=error)
  endfunction read_dataarray_rank1_I4P

  function read_dataarray_rank1_I2P(self, location, data_name, x, piece) result(error)
  !< Read a dataarray, flattened (components interleaved), into an array (I2P).
  class(xml_reader),        intent(in)           :: self      !< XML reader.
  character(*),             intent(in)           :: location  !< Location: node, cell or field.
  character(*),             intent(in)           :: data_name !< Name of the dataarray.
  integer(I2P), allocatable, intent(out)          :: x(:)      !< Values.
  integer(I4P),             intent(in), optional :: piece     !< Piece, from 1 (default 1, ignored for field data).
  integer(I4P)                                   :: error     !< Error status.
  integer(I1P), allocatable                      :: bytes(:)  !< Bytes of the dataarray.
  character(len=:), allocatable                  :: data_type !< VTK type of the dataarray.
  integer(I4P)                                   :: nc        !< Number of components.

  allocate(x(1:0))
  error = self%read_named_dataarray(location=location, data_name=data_name, piece=piece, bytes=bytes, data_type=data_type, &
                                    n_components=nc)
  if (error /= 0) return
  call bytes_to_array(bytes=bytes, data_type=data_type, x=x, error=error)
  endfunction read_dataarray_rank1_I2P

  function read_dataarray_rank1_I1P(self, location, data_name, x, piece) result(error)
  !< Read a dataarray, flattened (components interleaved), into an array (I1P).
  class(xml_reader),        intent(in)           :: self      !< XML reader.
  character(*),             intent(in)           :: location  !< Location: node, cell or field.
  character(*),             intent(in)           :: data_name !< Name of the dataarray.
  integer(I1P), allocatable, intent(out)          :: x(:)      !< Values.
  integer(I4P),             intent(in), optional :: piece     !< Piece, from 1 (default 1, ignored for field data).
  integer(I4P)                                   :: error     !< Error status.
  integer(I1P), allocatable                      :: bytes(:)  !< Bytes of the dataarray.
  character(len=:), allocatable                  :: data_type !< VTK type of the dataarray.
  integer(I4P)                                   :: nc        !< Number of components.

  allocate(x(1:0))
  error = self%read_named_dataarray(location=location, data_name=data_name, piece=piece, bytes=bytes, data_type=data_type, &
                                    n_components=nc)
  if (error /= 0) return
  call bytes_to_array(bytes=bytes, data_type=data_type, x=x, error=error)
  endfunction read_dataarray_rank1_I1P

  function read_dataarray_rank2_R8P(self, location, data_name, x, piece) result(error)
  !< Read a dataarray into an array of shape (number of components, number of tuples) (R8P).
  class(xml_reader),        intent(in)           :: self      !< XML reader.
  character(*),             intent(in)           :: location  !< Location: node, cell or field.
  character(*),             intent(in)           :: data_name !< Name of the dataarray.
  real(R8P),    allocatable, intent(out)          :: x(:,:)    !< Values.
  integer(I4P),             intent(in), optional :: piece     !< Piece, from 1 (default 1, ignored for field data).
  integer(I4P)                                   :: error     !< Error status.
  integer(I1P), allocatable                      :: bytes(:)  !< Bytes of the dataarray.
  character(len=:), allocatable                  :: data_type !< VTK type of the dataarray.
  real(R8P),    allocatable                       :: x1(:)     !< Values, flattened.
  integer(I4P)                                   :: nc        !< Number of components.
  integer(I8P)                                   :: t         !< Counter.

  allocate(x(1:0,1:0))
  error = self%read_named_dataarray(location=location, data_name=data_name, piece=piece, bytes=bytes, data_type=data_type, &
                                    n_components=nc)
  if (error /= 0) return
  call bytes_to_array(bytes=bytes, data_type=data_type, x=x1, error=error)
  if (error /= 0) return
  deallocate(x)
  allocate(x(1:nc,1:size(x1, kind=I8P)/nc))
  do t=1_I8P, size(x, dim=2, kind=I8P)
    x(:,t) = x1((t-1_I8P)*nc+1_I8P:t*nc)
  enddo
  endfunction read_dataarray_rank2_R8P

  function read_dataarray_rank2_R4P(self, location, data_name, x, piece) result(error)
  !< Read a dataarray into an array of shape (number of components, number of tuples) (R4P).
  class(xml_reader),        intent(in)           :: self      !< XML reader.
  character(*),             intent(in)           :: location  !< Location: node, cell or field.
  character(*),             intent(in)           :: data_name !< Name of the dataarray.
  real(R4P),    allocatable, intent(out)          :: x(:,:)    !< Values.
  integer(I4P),             intent(in), optional :: piece     !< Piece, from 1 (default 1, ignored for field data).
  integer(I4P)                                   :: error     !< Error status.
  integer(I1P), allocatable                      :: bytes(:)  !< Bytes of the dataarray.
  character(len=:), allocatable                  :: data_type !< VTK type of the dataarray.
  real(R4P),    allocatable                       :: x1(:)     !< Values, flattened.
  integer(I4P)                                   :: nc        !< Number of components.
  integer(I8P)                                   :: t         !< Counter.

  allocate(x(1:0,1:0))
  error = self%read_named_dataarray(location=location, data_name=data_name, piece=piece, bytes=bytes, data_type=data_type, &
                                    n_components=nc)
  if (error /= 0) return
  call bytes_to_array(bytes=bytes, data_type=data_type, x=x1, error=error)
  if (error /= 0) return
  deallocate(x)
  allocate(x(1:nc,1:size(x1, kind=I8P)/nc))
  do t=1_I8P, size(x, dim=2, kind=I8P)
    x(:,t) = x1((t-1_I8P)*nc+1_I8P:t*nc)
  enddo
  endfunction read_dataarray_rank2_R4P

  function read_dataarray_rank2_I8P(self, location, data_name, x, piece) result(error)
  !< Read a dataarray into an array of shape (number of components, number of tuples) (I8P).
  class(xml_reader),        intent(in)           :: self      !< XML reader.
  character(*),             intent(in)           :: location  !< Location: node, cell or field.
  character(*),             intent(in)           :: data_name !< Name of the dataarray.
  integer(I8P), allocatable, intent(out)          :: x(:,:)    !< Values.
  integer(I4P),             intent(in), optional :: piece     !< Piece, from 1 (default 1, ignored for field data).
  integer(I4P)                                   :: error     !< Error status.
  integer(I1P), allocatable                      :: bytes(:)  !< Bytes of the dataarray.
  character(len=:), allocatable                  :: data_type !< VTK type of the dataarray.
  integer(I8P), allocatable                       :: x1(:)     !< Values, flattened.
  integer(I4P)                                   :: nc        !< Number of components.
  integer(I8P)                                   :: t         !< Counter.

  allocate(x(1:0,1:0))
  error = self%read_named_dataarray(location=location, data_name=data_name, piece=piece, bytes=bytes, data_type=data_type, &
                                    n_components=nc)
  if (error /= 0) return
  call bytes_to_array(bytes=bytes, data_type=data_type, x=x1, error=error)
  if (error /= 0) return
  deallocate(x)
  allocate(x(1:nc,1:size(x1, kind=I8P)/nc))
  do t=1_I8P, size(x, dim=2, kind=I8P)
    x(:,t) = x1((t-1_I8P)*nc+1_I8P:t*nc)
  enddo
  endfunction read_dataarray_rank2_I8P

  function read_dataarray_rank2_I4P(self, location, data_name, x, piece) result(error)
  !< Read a dataarray into an array of shape (number of components, number of tuples) (I4P).
  class(xml_reader),        intent(in)           :: self      !< XML reader.
  character(*),             intent(in)           :: location  !< Location: node, cell or field.
  character(*),             intent(in)           :: data_name !< Name of the dataarray.
  integer(I4P), allocatable, intent(out)          :: x(:,:)    !< Values.
  integer(I4P),             intent(in), optional :: piece     !< Piece, from 1 (default 1, ignored for field data).
  integer(I4P)                                   :: error     !< Error status.
  integer(I1P), allocatable                      :: bytes(:)  !< Bytes of the dataarray.
  character(len=:), allocatable                  :: data_type !< VTK type of the dataarray.
  integer(I4P), allocatable                       :: x1(:)     !< Values, flattened.
  integer(I4P)                                   :: nc        !< Number of components.
  integer(I8P)                                   :: t         !< Counter.

  allocate(x(1:0,1:0))
  error = self%read_named_dataarray(location=location, data_name=data_name, piece=piece, bytes=bytes, data_type=data_type, &
                                    n_components=nc)
  if (error /= 0) return
  call bytes_to_array(bytes=bytes, data_type=data_type, x=x1, error=error)
  if (error /= 0) return
  deallocate(x)
  allocate(x(1:nc,1:size(x1, kind=I8P)/nc))
  do t=1_I8P, size(x, dim=2, kind=I8P)
    x(:,t) = x1((t-1_I8P)*nc+1_I8P:t*nc)
  enddo
  endfunction read_dataarray_rank2_I4P

  function read_dataarray_rank2_I2P(self, location, data_name, x, piece) result(error)
  !< Read a dataarray into an array of shape (number of components, number of tuples) (I2P).
  class(xml_reader),        intent(in)           :: self      !< XML reader.
  character(*),             intent(in)           :: location  !< Location: node, cell or field.
  character(*),             intent(in)           :: data_name !< Name of the dataarray.
  integer(I2P), allocatable, intent(out)          :: x(:,:)    !< Values.
  integer(I4P),             intent(in), optional :: piece     !< Piece, from 1 (default 1, ignored for field data).
  integer(I4P)                                   :: error     !< Error status.
  integer(I1P), allocatable                      :: bytes(:)  !< Bytes of the dataarray.
  character(len=:), allocatable                  :: data_type !< VTK type of the dataarray.
  integer(I2P), allocatable                       :: x1(:)     !< Values, flattened.
  integer(I4P)                                   :: nc        !< Number of components.
  integer(I8P)                                   :: t         !< Counter.

  allocate(x(1:0,1:0))
  error = self%read_named_dataarray(location=location, data_name=data_name, piece=piece, bytes=bytes, data_type=data_type, &
                                    n_components=nc)
  if (error /= 0) return
  call bytes_to_array(bytes=bytes, data_type=data_type, x=x1, error=error)
  if (error /= 0) return
  deallocate(x)
  allocate(x(1:nc,1:size(x1, kind=I8P)/nc))
  do t=1_I8P, size(x, dim=2, kind=I8P)
    x(:,t) = x1((t-1_I8P)*nc+1_I8P:t*nc)
  enddo
  endfunction read_dataarray_rank2_I2P

  function read_dataarray_rank2_I1P(self, location, data_name, x, piece) result(error)
  !< Read a dataarray into an array of shape (number of components, number of tuples) (I1P).
  class(xml_reader),        intent(in)           :: self      !< XML reader.
  character(*),             intent(in)           :: location  !< Location: node, cell or field.
  character(*),             intent(in)           :: data_name !< Name of the dataarray.
  integer(I1P), allocatable, intent(out)          :: x(:,:)    !< Values.
  integer(I4P),             intent(in), optional :: piece     !< Piece, from 1 (default 1, ignored for field data).
  integer(I4P)                                   :: error     !< Error status.
  integer(I1P), allocatable                      :: bytes(:)  !< Bytes of the dataarray.
  character(len=:), allocatable                  :: data_type !< VTK type of the dataarray.
  integer(I1P), allocatable                       :: x1(:)     !< Values, flattened.
  integer(I4P)                                   :: nc        !< Number of components.
  integer(I8P)                                   :: t         !< Counter.

  allocate(x(1:0,1:0))
  error = self%read_named_dataarray(location=location, data_name=data_name, piece=piece, bytes=bytes, data_type=data_type, &
                                    n_components=nc)
  if (error /= 0) return
  call bytes_to_array(bytes=bytes, data_type=data_type, x=x1, error=error)
  if (error /= 0) return
  deallocate(x)
  allocate(x(1:nc,1:size(x1, kind=I8P)/nc))
  do t=1_I8P, size(x, dim=2, kind=I8P)
    x(:,t) = x1((t-1_I8P)*nc+1_I8P:t*nc)
  enddo
  endfunction read_dataarray_rank2_I1P


  ! private non type-bound procedures
  function read_coordinate_R8P(self, id, x, n_components) result(error)
  !< Read a geometry dataarray element into R8P, checking its number of components (default 1).
  class(xml_reader),      intent(in)           :: self         !< XML reader.
  integer(I4P),           intent(in)           :: id           !< Index of the dataarray element.
  real(R8P), allocatable, intent(out)          :: x(:)         !< Values.
  integer(I4P),           intent(in), optional :: n_components !< Expected number of components.
  integer(I4P)                                 :: error        !< Error status.
  integer(I1P), allocatable                    :: bytes(:)     !< Bytes of the dataarray.
  character(len=:), allocatable                :: data_type    !< VTK type of the dataarray.
  integer(I4P)                                 :: nc           !< Number of components.

  allocate(x(1:0))
  error = self%dataarray_bytes(id=id, bytes=bytes, data_type=data_type, n_components=nc)
  if (error /= 0) return
  error = 6
  if (nc /= 1 .and. .not.present(n_components)) return
  if (present(n_components)) then
    if (nc /= n_components) return
  endif
  call bytes_to_array(bytes=bytes, data_type=data_type, x=x, error=error)
  endfunction read_coordinate_R8P

  function read_coordinate_R4P(self, id, x, n_components) result(error)
  !< Read a geometry dataarray element into R4P, checking its number of components (default 1).
  class(xml_reader),      intent(in)           :: self         !< XML reader.
  integer(I4P),           intent(in)           :: id           !< Index of the dataarray element.
  real(R4P), allocatable, intent(out)          :: x(:)         !< Values.
  integer(I4P),           intent(in), optional :: n_components !< Expected number of components.
  integer(I4P)                                 :: error        !< Error status.
  integer(I1P), allocatable                    :: bytes(:)     !< Bytes of the dataarray.
  character(len=:), allocatable                :: data_type    !< VTK type of the dataarray.
  integer(I4P)                                 :: nc           !< Number of components.

  allocate(x(1:0))
  error = self%dataarray_bytes(id=id, bytes=bytes, data_type=data_type, n_components=nc)
  if (error /= 0) return
  error = 6
  if (nc /= 1 .and. .not.present(n_components)) return
  if (present(n_components)) then
    if (nc /= n_components) return
  endif
  call bytes_to_array(bytes=bytes, data_type=data_type, x=x, error=error)
  endfunction read_coordinate_R4P

  function read_appended_raw(self, pos, bytes) result(error)
  !< Read the data of a dataarray from raw appended data: the bytes count (or the compressed data header) and the data.
  class(xml_reader),         intent(in)  :: self     !< XML reader.
  integer(I8P),              intent(in)  :: pos      !< Position of the dataarray in the file.
  integer(I1P), allocatable, intent(out) :: bytes(:) !< Data bytes.
  integer(I4P)                           :: error    !< Error status.
  integer(I1P), allocatable              :: raw(:)   !< Bytes read.
  integer(I8P), allocatable              :: header(:) !< Header words.
  integer(I8P)                           :: w        !< Size of a header word.
  integer                                :: zerror   !< Decompression error.

  allocate(bytes(1:0))
  w = self%word_size
  error = read_bytes(filename=self%filename, pos=pos, n=w, bytes=raw) ; if (error /= 0) return
  header = header_words(raw, w)
  error = 6
  if (header(1) < 0_I8P) return
  if (.not.self%is_compressed) then
    error = read_bytes(filename=self%filename, pos=pos+w, n=header(1), bytes=bytes)
  else
    ! [number of blocks, block size, last block size, compressed sizes], then the blocks
    error = read_bytes(filename=self%filename, pos=pos, n=(3_I8P+header(1))*w, bytes=raw) ; if (error /= 0) return
    header = header_words(raw, w)
    error = 6
    if (any(header(4:) < 0_I8P)) return
    error = read_bytes(filename=self%filename, pos=pos+(3_I8P+header(1))*w, n=sum(header(4:)), bytes=raw)
    if (error /= 0) return
    call zlib_uncompress_blocks(header=header, blocks=raw, bytes=bytes, error=zerror)
    if (zerror /= 0) error = 6
  endif
  endfunction read_appended_raw

  function read_appended_base64(self, pos, bytes) result(error)
  !< Read the data of a dataarray from base64 appended data (offsets count characters).
  !<
  !< The length of the code of a dataarray is computed from its header: the bytes count (uncompressed, one base64 stream
  !< with the data) or the header of the compressed data (a base64 stream of its own, followed by the blocks).
  class(xml_reader),         intent(in)  :: self      !< XML reader.
  integer(I8P),              intent(in)  :: pos       !< Position of the dataarray in the file.
  integer(I1P), allocatable, intent(out) :: bytes(:)  !< Data bytes.
  integer(I4P)                           :: error     !< Error status.
  character(len=:), allocatable          :: code      !< Base64 code.
  integer(I1P), allocatable              :: raw(:)    !< Decoded bytes.
  integer(I8P), allocatable              :: header(:) !< Header words.
  integer(I8P)                           :: w         !< Size of a header word.
  integer(I8P)                           :: lh        !< Length of the code of the header.
  integer(I8P)                           :: lc        !< Length of the code of the dataarray.

  allocate(bytes(1:0))
  w = self%word_size
  lh = ((w + 2_I8P) / 3_I8P) * 4_I8P
  error = read_chars(filename=self%filename, first=pos, last=pos+lh-1_I8P, chars=code) ; if (error /= 0) return
  error = 6
  allocate(raw(1:(lh/4_I8P)*3_I8P))
  call b64_decode_bytes(code=code, bytes=raw)
  header = header_words(raw(1:w), w)
  if (header(1) < 0_I8P) return
  if (.not.self%is_compressed) then
    lc = ((w + header(1) + 2_I8P) / 3_I8P) * 4_I8P
  else
    lh = (((3_I8P + header(1)) * w + 2_I8P) / 3_I8P) * 4_I8P
    error = read_chars(filename=self%filename, first=pos, last=pos+lh-1_I8P, chars=code) ; if (error /= 0) return
    error = 6
    deallocate(raw)
    allocate(raw(1:base64_decoded_size(code)))
    if (size(raw, kind=I8P) /= (3_I8P + header(1)) * w) return
    call b64_decode_bytes(code=code, bytes=raw)
    header = header_words(raw, w)
    if (any(header(4:) < 0_I8P)) return
    lc = lh + ((sum(header(4:)) + 2_I8P) / 3_I8P) * 4_I8P
  endif
  error = read_chars(filename=self%filename, first=pos, last=pos+lc-1_I8P, chars=code) ; if (error /= 0) return
  call decode_base64(code=code, word_size=w, is_compressed=self%is_compressed, bytes=bytes, error=error)
  endfunction read_appended_base64

  subroutine b64_decode_bytes(code, bytes)
  !< Decode a base64 code into bytes.
  use befor64, only : b64_decode
  character(*), intent(in)    :: code      !< Base64 code.
  integer(I1P), intent(inout) :: bytes(1:) !< Bytes, of the decoded size.

  call b64_decode(code=code, n=bytes)
  endsubroutine b64_decode_bytes

  function polydata_section(self, block, piece, section) result(error)
  !< Return the name of the element of a block of polydata cells, empty if the block is absent and has no cells.
  class(xml_reader),             intent(in)           :: self    !< XML reader.
  character(*),                  intent(in)           :: block   !< Block of cells: verts, lines, strips or polys.
  integer(I4P),                  intent(in), optional :: piece   !< Piece, from 1 (default 1).
  character(len=:), allocatable, intent(out)          :: section !< Name of the block element.
  integer(I4P)                                        :: error   !< Error status.
  integer(I4P)                                        :: p       !< Index of the piece element.
  integer(I8P)                                        :: counts(6) !< Points, cells, verts, lines, strips, polys.
  integer(I4P)                                        :: extent(6) !< Extent.
  integer(I4P)                                        :: b       !< Index of the block in the counts.

  section = ''
  error = 4
  if (.not.self%is_initialized) return
  if (self%mesh_topology /= 'PolyData') return
  select case(lower_case(trim(adjustl(block))))
  case('verts')
    section = 'Verts' ; b = 3
  case('lines')
    section = 'Lines' ; b = 4
  case('strips')
    section = 'Strips' ; b = 5
  case('polys')
    section = 'Polys' ; b = 6
  case default
    return
  endselect
  error = self%piece_element(piece=piece, id=p) ; if (error /= 0) return
  if (self%xml%find_child(parent=p, name=section) == 0) then
    call piece_counts(element=self%xml%element(p), mesh_topology=self%mesh_topology, counts=counts, extent=extent, &
                      error=error)
    if (error /= 0) return
    if (counts(b) > 0_I8P) then
      error = 4
    else
      section = ''
    endif
  endif
  endfunction polydata_section

  subroutine piece_counts(element, mesh_topology, counts, extent, error)
  !< Return the counts (points, cells, verts, lines, strips, polys) and the extent of a piece element.
  type(xml_element), intent(in)  :: element       !< Piece element.
  character(*),      intent(in)  :: mesh_topology !< Dataset type.
  integer(I8P),      intent(out) :: counts(6)     !< Points, cells, verts, lines, strips, polys.
  integer(I4P),      intent(out) :: extent(6)     !< Extent.
  integer(I4P),      intent(out) :: error         !< Error status.
  character(len=:), allocatable  :: value         !< Attribute value.
  integer(I4P)                   :: iostat        !< IO status.
  integer(I4P)                   :: a             !< Counter.
  character(len=14), parameter   :: names(6)=['NumberOfPoints', 'NumberOfCells ', 'NumberOfVerts ', 'NumberOfLines ', &
                                              'NumberOfStrips', 'NumberOfPolys '] !< Counts attributes.

  counts = 0_I8P
  extent = 0
  error = 0
  select case(mesh_topology)
  case('ImageData', 'RectilinearGrid', 'StructuredGrid', 'PImageData', 'PRectilinearGrid', 'PStructuredGrid')
    call element%get_attribute(name='Extent', value=value)
    read(value, *, iostat=iostat) extent
    if (iostat /= 0) error = 6
    counts(1) = product(int(extent(2::2) - extent(1::2), I8P) + 1_I8P)
    counts(2) = product(max(int(extent(2::2) - extent(1::2), I8P), 1_I8P))
  case default
    do a=1, size(names)
      call element%get_attribute(name=trim(names(a)), value=value)
      if (len(value) == 0) cycle
      read(value, *, iostat=iostat) counts(a)
      if (iostat /= 0) error = 6
    enddo
    if (mesh_topology == 'PolyData') counts(2) = sum(counts(3:6))
  endselect
  endsubroutine piece_counts

  function find_marker(filename, pos) result(start)
  !< Return the position following the '_' marker of the appended data, searched from a position (0 if not found).
  character(*), intent(in) :: filename !< File name.
  integer(I8P), intent(in) :: pos      !< Position where the search starts.
  integer(I8P)             :: start    !< Position of the first byte of the appended data.
  character(len=256)       :: chunk    !< Characters read.
  integer(I4P)             :: unit     !< File unit.
  integer(I4P)             :: iostat   !< IO status.
  integer(I8P)             :: file_size !< File size.
  integer(I8P)             :: n        !< Characters read.
  integer                  :: m        !< Position of the marker in the chunk.

  start = 0_I8P
  open(newunit=unit, file=filename, access='stream', form='unformatted', action='read', status='old', iostat=iostat)
  if (iostat /= 0) return
  inquire(unit=unit, size=file_size)
  n = min(int(len(chunk), I8P), file_size - pos + 1_I8P)
  if (n > 0_I8P) then
    read(unit, pos=pos, iostat=iostat) chunk(1:n)
    if (iostat == 0) then
      m = index(chunk(1:n), '_')
      if (m > 0) start = pos + m
    endif
  endif
  close(unit)
  endfunction find_marker

  function read_chars(filename, first, last, chars) result(error)
  !< Read the characters of a file between two positions (none if last < first).
  character(*),                  intent(in)  :: filename !< File name.
  integer(I8P),                  intent(in)  :: first    !< First position.
  integer(I8P),                  intent(in)  :: last     !< Last position.
  character(len=:), allocatable, intent(out) :: chars    !< Characters read.
  integer(I4P)                               :: error    !< Error status.
  integer(I4P)                               :: unit     !< File unit.

  chars = ''
  error = 0
  if (last < first) return
  error = 1
  open(newunit=unit, file=filename, access='stream', form='unformatted', action='read', status='old', iostat=error)
  if (error /= 0) then
    error = 1
    return
  endif
  deallocate(chars)
  allocate(character(len=last-first+1_I8P) :: chars)
  read(unit, pos=first, iostat=error) chars
  if (error /= 0) error = 6
  close(unit)
  endfunction read_chars

  function read_bytes(filename, pos, n, bytes) result(error)
  !< Read bytes of a file from a position.
  character(*),              intent(in)  :: filename !< File name.
  integer(I8P),              intent(in)  :: pos      !< First position.
  integer(I8P),              intent(in)  :: n        !< Number of bytes.
  integer(I1P), allocatable, intent(out) :: bytes(:) !< Bytes read.
  integer(I4P)                           :: error    !< Error status.
  integer(I4P)                           :: unit     !< File unit.

  allocate(bytes(1:max(n, 0_I8P)))
  error = 0
  if (n <= 0_I8P) return
  open(newunit=unit, file=filename, access='stream', form='unformatted', action='read', status='old', iostat=error)
  if (error /= 0) then
    error = 1
    return
  endif
  read(unit, pos=pos, iostat=error) bytes
  if (error /= 0) error = 6
  close(unit)
  endfunction read_bytes

  function to_I4P(ids, x) result(error)
  !< Convert ids into I4P, checking that they fit.
  integer(I8P),              intent(in)  :: ids(:) !< Ids.
  integer(I4P), allocatable, intent(out) :: x(:)   !< Ids, I4P.
  integer(I4P)                           :: error  !< Error status: 0, or 5 if an id does not fit I4P.
  integer(I8P)                           :: i      !< Counter.

  allocate(x(1:0))
  error = 5
  do i=1_I8P, size(ids, kind=I8P)
    if (ids(i) > int(huge(1_I4P), I8P) .or. ids(i) < -int(huge(1_I4P), I8P) - 1_I8P) return
  enddo
  deallocate(x)
  allocate(x(1:size(ids, kind=I8P)))
  do i=1_I8P, size(ids, kind=I8P)
    x(i) = int(ids(i), I4P)
  enddo
  error = 0
  endfunction to_I4P

  elemental function is_dataarray(name) result(is_array)
  !< Return .true. if an element name is a data array: DataArray, Array (field data strings) or PDataArray (parallel).
  character(*), intent(in) :: name     !< Element name.
  logical                  :: is_array !< Inquire result.

  is_array = name == 'DataArray' .or. name == 'Array' .or. name == 'PDataArray'
  endfunction is_dataarray

  pure function lower_case(string) result(lower)
  !< Return a string in lower case (ASCII letters only).
  character(*), intent(in) :: string !< Input string.
  character(len(string))   :: lower  !< Lower case string.
  integer                  :: i      !< Counter.

  lower = string
  do i=1, len(string)
    if (string(i:i) >= 'A' .and. string(i:i) <= 'Z') lower(i:i) = achar(iachar(string(i:i)) + 32)
  enddo
  endfunction lower_case
endmodule vtk_fortran_vtk_file_xml_reader
