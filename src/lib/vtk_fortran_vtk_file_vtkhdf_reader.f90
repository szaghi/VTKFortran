!< VTK file VTKHDF reader.
module vtk_fortran_vtk_file_vtkhdf_reader
!< VTK file VTKHDF reader: the reading of `.vtkhdf` files behind `xml_reader` (`vtk_file%initialize(filename,
!< action='read')`), which dispatches its calls here when the file is an HDF5 one.
!<
!< It reads every dataset the VTKHDF writer writes, and the same layouts written by VTK: ImageData, RectilinearGrid,
!< StructuredGrid (one piece), UnstructuredGrid (polyhedra included) and PolyData (each partition a piece), static or time
!< series (`Steps`: `get_steps`, `set_step`; the step 1 by default). The values are read as HDF5 hyperslabs of the piece
!< and step asked for, as the bytes of their type in the file (`read_dataarray_bytes`), then converted by the caller: the
!< memory used is about the size of the array read. The offsets are returned as VTKFortran writes them (the end of each
!< cell), the faces of the polyhedra as the face stream of `write_connectivity`: a face shared by two polyhedra (VTKHDF
!< 2.9, a negative id in `PolyhedronToFaces` for a face traversed in reverse) is given in the stream of each polyhedron,
!< reversed where marked.
!<
!< The error codes are the ones of `xml_reader`: 1 the file cannot be read, 2 not a VTKHDF file, 3 unsupported (composite
!< files, an unknown type), 4 not found, 5 the kind cannot hold the values, 6 the data do not decode (inconsistent).
use penf
use vtk_fortran_dataarray_decoder, only : bytes_to_array
use vtk_fortran_hdf5

implicit none
private
public :: vtkhdf_reader
public :: is_hdf5_file

character(*), parameter :: BLOCKS(4) = ['Vertices', 'Lines   ', 'Strips  ', 'Polygons'] !< Groups of the PolyData cells.
integer(I4P), parameter :: STEP_COLUMN(4) = [1, 2, 4, 3] !< Column of each block in the [NSteps, 4] offsets of Steps.

type :: vtkhdf_reader
  !< VTK file VTKHDF reader.
  private
  integer(hid_kind)             :: file_id=-1_hid_kind    !< File.
  integer(hid_kind)             :: root=-1_hid_kind       !< Group /VTKHDF.
  character(len=:), allocatable :: mesh_topology          !< Dataset type.
  logical                       :: is_initialized=.false. !< The reader is initialized.
  integer(I8P)                  :: n_steps=0_I8P          !< Steps, 0 for a dataset that is not a time series.
  integer(I8P)                  :: step=1_I8P             !< Step read.
  integer(I4P)                  :: whole_extent(6)=0_I4P  !< Whole extent of a grid.
  contains
    ! public methods
    procedure, pass(self) :: close                !< Close the file and reset the reader.
    procedure, pass(self) :: forget               !< Reset the reader without closing the file (pure).
    procedure, pass(self) :: get_dataarray_info   !< Return the type, components and tuples of a dataarray.
    procedure, pass(self) :: get_dataarray_names  !< Return the names of the dataarrays of a location.
    procedure, pass(self) :: get_info             !< Return the information of the dataset.
    procedure, pass(self) :: get_steps            !< Return the times of the steps.
    procedure, pass(self) :: initialize           !< Initialize the reader: open the file.
    procedure, pass(self) :: read_dataarray_bytes !< Return the bytes, type and components of a dataarray.
    generic               :: read_geo => read_geo_R8P, read_geo_R4P !< Read the geometry.
    procedure, pass(self) :: read_geo_R8P         !< Read the geometry (R8P).
    procedure, pass(self) :: read_geo_R4P         !< Read the geometry (R4P).
    procedure, pass(self) :: read_ids             !< Read ids of the cells (connectivity, offsets, types, faces).
    procedure, pass(self) :: read_piece           !< Read the counts and extent of a piece.
    procedure, pass(self) :: set_step             !< Select the step read.
    ! private methods
    procedure, pass(self), private :: grid_points  !< Points of the grid along each axis.
    procedure, pass(self), private :: is_grid      !< The dataset is a grid (one piece).
    procedure, pass(self), private :: part         !< The partition of a piece of the step, and where it starts.
    procedure, pass(self), private :: step_value   !< An entry of a dataset of Steps for the step read.
endtype vtkhdf_reader

contains
  ! public methods
  function initialize(self, filename) result(error)
  !< Initialize the reader: open the file, read the type and extent of its dataset and its steps.
  class(vtkhdf_reader), intent(inout) :: self     !< Reader.
  character(*),         intent(in)    :: filename !< File name.
  integer(I4P)                        :: error    !< Error status.
  character(len=:), allocatable       :: text     !< Type of the dataset.
  integer(I8P), allocatable           :: ivalues(:) !< Values read.
  integer(hid_kind)                   :: steps    !< Group Steps.

  call self%close
  error = 1
  if (.not.is_hdf5_enabled) return
  if (h5_file_open(filename, self%file_id) /= 0_I4P) then
    self%file_id = -1_hid_kind
    return
  endif
  error = 2
  if (h5_group_open(self%file_id, 'VTKHDF', self%root) /= 0_I4P) then
    self%root = -1_hid_kind
    return
  endif
  if (h5_attribute_read(self%root, 'Type', text) /= 0_I4P) return
  error = 3
  select case(text)
  case('ImageData', 'RectilinearGrid', 'StructuredGrid', 'UnstructuredGrid', 'PolyData')
    self%mesh_topology = text
  case default
    return
  endselect
  error = 6
  if (self%mesh_topology == 'ImageData') then
    if (h5_attribute_read(self%root, 'WholeExtent', ivalues) /= 0_I4P) return
    if (size(ivalues) /= 6) return
    self%whole_extent = int(ivalues, I4P)
  elseif (self%is_grid()) then
    if (h5_attribute_read(self%root, 'Dimensions', ivalues) /= 0_I4P) return
    if (size(ivalues) /= 3) return
    self%whole_extent(1::2) = 0_I4P
    self%whole_extent(2::2) = int(ivalues - 1_I8P, I4P)
  endif
  self%n_steps = 0_I8P
  if (h5_link_exists(self%root, 'Steps')) then
    if (h5_group_open(self%root, 'Steps', steps) /= 0_I4P) return
    error = h5_attribute_read(steps, 'NSteps', ivalues)
    if (h5_group_close(steps) /= 0_I4P) error = 6
    if (error /= 0_I4P) then
      error = 6
      return
    endif
    self%n_steps = ivalues(1)
  endif
  self%step = 1_I8P
  self%is_initialized = .true.
  error = 0
  endfunction initialize

  subroutine close(self)
  !< Close the file and reset the reader.
  class(vtkhdf_reader), intent(inout) :: self  !< Reader.
  integer(I4P)                        :: error !< Error status.

  if (self%root >= 0_hid_kind) error = h5_group_close(self%root)
  if (self%file_id >= 0_hid_kind) error = h5_file_close(self%file_id)
  call self%forget
  endsubroutine close

  elemental subroutine forget(self)
  !< Reset the reader without closing the file (for the pure `free` of `vtk_file`: `close` closes it).
  class(vtkhdf_reader), intent(inout) :: self !< Reader.

  self%root = -1_hid_kind
  self%file_id = -1_hid_kind
  if (allocated(self%mesh_topology)) deallocate(self%mesh_topology)
  self%is_initialized = .false.
  self%n_steps = 0_I8P
  self%step = 1_I8P
  self%whole_extent = 0_I4P
  endsubroutine forget

  function get_info(self, mesh_topology, npieces, header_type, compressor, nx1, nx2, ny1, ny2, nz1, nz2, &
                    origin, spacing, direction, ghost_level) result(error)
  !< Return the information of the dataset, as `xml_reader%get_info`: the pieces are those of the step read; the header
  !< type is `HDF5` and the compressor `none` (HDF5 decompresses its datasets itself).
  class(vtkhdf_reader),          intent(in)            :: self          !< Reader.
  character(len=:), allocatable, intent(out), optional :: mesh_topology !< Dataset type, e.g. UnstructuredGrid.
  integer(I4P),                  intent(out), optional :: npieces       !< Number of pieces (of the step read).
  character(len=:), allocatable, intent(out), optional :: header_type   !< HDF5.
  character(len=:), allocatable, intent(out), optional :: compressor    !< none.
  integer(I4P),                  intent(out), optional :: nx1           !< Initial node of x axis.
  integer(I4P),                  intent(out), optional :: nx2           !< Final node of x axis.
  integer(I4P),                  intent(out), optional :: ny1           !< Initial node of y axis.
  integer(I4P),                  intent(out), optional :: ny2           !< Final node of y axis.
  integer(I4P),                  intent(out), optional :: nz1           !< Initial node of z axis.
  integer(I4P),                  intent(out), optional :: nz2           !< Final node of z axis.
  real(R8P),                     intent(out), optional :: origin(3)     !< Origin of ImageData.
  real(R8P),                     intent(out), optional :: spacing(3)    !< Spacing of ImageData.
  real(R8P),                     intent(out), optional :: direction(9)  !< Axes directions of ImageData (row-major).
  integer(I4P),                  intent(out), optional :: ghost_level   !< 0.
  integer(I4P)                                         :: error         !< Error status.
  real(R8P), allocatable                               :: rvalues(:)    !< Values read.
  integer(I8P)                                         :: g, first(9), n_parts !< Partition, starts, parts of the step.

  error = 4
  if (.not.self%is_initialized) return
  error = 0
  if (present(mesh_topology)) mesh_topology = self%mesh_topology
  if (present(npieces)) then
    npieces = 1
    if (.not.self%is_grid()) then
      error = self%part(1, g, first, n_parts) ! n_parts even when the step has no part
      npieces = int(n_parts, I4P)
      error = 0
    endif
  endif
  if (present(header_type)) header_type = 'HDF5'
  if (present(compressor)) compressor = 'none'
  if (present(nx1)) nx1 = self%whole_extent(1)
  if (present(nx2)) nx2 = self%whole_extent(2)
  if (present(ny1)) ny1 = self%whole_extent(3)
  if (present(ny2)) ny2 = self%whole_extent(4)
  if (present(nz1)) nz1 = self%whole_extent(5)
  if (present(nz2)) nz2 = self%whole_extent(6)
  if (present(origin)) then
    origin = 0._R8P
    if (h5_attribute_exists(self%root, 'Origin')) then
      if (h5_attribute_read(self%root, 'Origin', rvalues) == 0_I4P) origin = rvalues(1:3)
    endif
  endif
  if (present(spacing)) then
    spacing = 1._R8P
    if (h5_attribute_exists(self%root, 'Spacing')) then
      if (h5_attribute_read(self%root, 'Spacing', rvalues) == 0_I4P) spacing = rvalues(1:3)
    endif
  endif
  if (present(direction)) then
    direction = [1._R8P, 0._R8P, 0._R8P, 0._R8P, 1._R8P, 0._R8P, 0._R8P, 0._R8P, 1._R8P]
    if (h5_attribute_exists(self%root, 'Direction')) then
      if (h5_attribute_read(self%root, 'Direction', rvalues) == 0_I4P) direction = rvalues(1:9)
    endif
  endif
  if (present(ghost_level)) ghost_level = 0
  endfunction get_info

  function get_steps(self, times) result(error)
  !< Return the times of the steps (none when the dataset is not a time series).
  class(vtkhdf_reader),   intent(in)  :: self     !< Reader.
  real(R8P), allocatable, intent(out) :: times(:) !< Times of the steps.
  integer(I4P)                        :: error    !< Error status.

  allocate(times(0))
  error = 4
  if (.not.self%is_initialized) return
  error = 0
  if (self%n_steps == 0_I8P) return
  error = h5_dataset_read(self%root, 'Steps/Values', times)
  if (error /= 0) then
    error = 6
  elseif (size(times, kind=I8P) < self%n_steps) then
    error = 6
  else
    times = times(1:self%n_steps)
  endif
  endfunction get_steps

  function set_step(self, step) result(error)
  !< Select the step read, from 1 (a dataset that is not a time series has only the step 1).
  class(vtkhdf_reader), intent(inout) :: self  !< Reader.
  integer(I4P),         intent(in)    :: step  !< Step, from 1.
  integer(I4P)                        :: error !< Error status.

  error = 4
  if (.not.self%is_initialized) return
  if (step < 1 .or. int(step, I8P) > max(1_I8P, self%n_steps)) return
  self%step = int(step, I8P)
  error = 0
  endfunction set_step

  function read_piece(self, piece, np, nc, nx1, nx2, ny1, ny2, nz1, nz2, nverts, nlines, nstrips, npolys) result(error)
  !< Read the counts and the extent of a piece of the step read (a grid has one piece, its extent the whole extent).
  class(vtkhdf_reader), intent(in)            :: self    !< Reader.
  integer(I4P),         intent(in),  optional :: piece   !< Piece, from 1 (default 1).
  integer(I8P),         intent(out), optional :: np      !< Number of points.
  integer(I8P),         intent(out), optional :: nc      !< Number of cells.
  integer(I4P),         intent(out), optional :: nx1     !< Initial node of x axis.
  integer(I4P),         intent(out), optional :: nx2     !< Final node of x axis.
  integer(I4P),         intent(out), optional :: ny1     !< Initial node of y axis.
  integer(I4P),         intent(out), optional :: ny2     !< Final node of y axis.
  integer(I4P),         intent(out), optional :: nz1     !< Initial node of z axis.
  integer(I4P),         intent(out), optional :: nz2     !< Final node of z axis.
  integer(I8P),         intent(out), optional :: nverts  !< Number of vertices (PolyData).
  integer(I8P),         intent(out), optional :: nlines  !< Number of lines (PolyData).
  integer(I8P),         intent(out), optional :: nstrips !< Number of strips (PolyData).
  integer(I8P),         intent(out), optional :: npolys  !< Number of polygons (PolyData).
  integer(I4P)                                :: error   !< Error status.
  integer(I8P)                                :: counts(6) !< Points, cells, verts, lines, strips, polys.
  integer(I8P)                                :: grid(3) !< Points along each axis.
  integer(I8P)                                :: g, first(9), n_parts !< Partition, starts, parts of the step.
  integer(I4P)                                :: b       !< Counter.

  counts = 0_I8P
  error = 4
  if (.not.self%is_initialized) return
  if (self%is_grid()) then
    if (present(piece)) then
      if (piece /= 1) return
    endif
    grid = self%grid_points()
    counts(1) = product(grid)
    counts(2) = product(max(grid - 1_I8P, 1_I8P))
    error = 0
  else
    error = self%part(piece, g, first, n_parts)
    if (error /= 0) return
    counts(1) = count_of(self%root, 'NumberOfPoints', g, error)
    if (self%mesh_topology == 'UnstructuredGrid') then
      if (error == 0) counts(2) = count_of(self%root, 'NumberOfCells', g, error)
    else
      do b=1, 4
        if (error == 0) counts(2+b) = count_of(self%root, trim(BLOCKS(b))//'/NumberOfCells', g, error)
      enddo
      counts(2) = sum(counts(3:6))
    endif
  endif
  if (present(np)) np = counts(1)
  if (present(nc)) nc = counts(2)
  if (present(nverts)) nverts = counts(3)
  if (present(nlines)) nlines = counts(4)
  if (present(nstrips)) nstrips = counts(5)
  if (present(npolys)) npolys = counts(6)
  if (present(nx1)) nx1 = self%whole_extent(1)
  if (present(nx2)) nx2 = self%whole_extent(2)
  if (present(ny1)) ny1 = self%whole_extent(3)
  if (present(ny2)) ny2 = self%whole_extent(4)
  if (present(nz1)) nz1 = self%whole_extent(5)
  if (present(nz2)) nz2 = self%whole_extent(6)
  endfunction read_piece

  function get_dataarray_names(self, location, names, piece) result(error)
  !< Return the names of the dataarrays of a location (node, cell or field), in the order they were written (their names'
  !< order if the file does not track it), blank padded to the longest one.
  class(vtkhdf_reader),          intent(in)           :: self     !< Reader.
  character(*),                  intent(in)           :: location !< Location: node, cell or field.
  character(len=:), allocatable, intent(out)          :: names(:) !< Names of the dataarrays.
  integer(I4P),                  intent(in), optional :: piece    !< Piece, from 1 (default 1, ignored for field data).
  integer(I4P)                                        :: error    !< Error status.
  character(len=:), allocatable                       :: group    !< Group of the location.
  character(len=:), allocatable                       :: list     !< Names, each followed by NUL.
  integer(I8P)                                        :: g, first(9), n_parts !< Partition, starts, parts of the step.
  integer(I4P)                                        :: n, l, i, j !< Counters, lengths.

  allocate(character(len=0) :: names(1:0))
  error = 4
  if (.not.self%is_initialized) return
  group = location_group(location)
  if (len(group) == 0) return
  if (group /= 'FieldData' .and. .not.self%is_grid()) then
    error = self%part(piece, g, first, n_parts)
    if (error /= 0) return
  endif
  error = 0
  if (.not.h5_link_exists(self%root, group)) return
  if (h5_group_names(self%root, group, list) /= 0_I4P) then
    error = 6
    return
  endif
  n = 0 ; l = 0 ; j = 0
  do i=1, len(list)
    if (list(i:i) == achar(0)) then
      n = n + 1
      l = max(l, i - j - 1)
      j = i
    endif
  enddo
  deallocate(names)
  allocate(character(len=l) :: names(1:n))
  n = 0 ; j = 0
  do i=1, len(list)
    if (list(i:i) == achar(0)) then
      n = n + 1
      names(n) = list(j+1:i-1)
      j = i
    endif
  enddo
  endfunction get_dataarray_names

  function get_dataarray_info(self, location, data_name, piece, data_type, n_components, n_tuples) result(error)
  !< Return the VTK type, the number of components and the number of tuples (of the piece and step read) of a dataarray.
  class(vtkhdf_reader),          intent(in)            :: self         !< Reader.
  character(*),                  intent(in)            :: location     !< Location: node, cell or field.
  character(*),                  intent(in)            :: data_name    !< Name of the dataarray.
  integer(I4P),                  intent(in),  optional :: piece        !< Piece, from 1 (default 1, ignored for field data).
  character(len=:), allocatable, intent(out), optional :: data_type    !< VTK type, e.g. Float64, Int32, UInt8.
  integer(I4P),                  intent(out), optional :: n_components !< Number of components.
  integer(I8P),                  intent(out), optional :: n_tuples     !< Number of tuples.
  integer(I4P)                                         :: error        !< Error status.
  character(len=:), allocatable                        :: path         !< Dataset.
  character(len=:), allocatable                        :: dtype        !< VTK type.
  integer(I8P)                                         :: first        !< Rows skipped.
  integer(I8P)                                         :: count        !< Rows read.
  integer(I4P)                                         :: nc           !< Components.
  integer(I8P)                                         :: nt           !< Tuples.

  error = array_slice(self, location, data_name, piece, path, dtype, first, count, nc, nt)
  if (error /= 0) return
  if (present(data_type)) data_type = dtype
  if (present(n_components)) n_components = nc
  if (present(n_tuples)) n_tuples = nt
  endfunction get_dataarray_info

  function read_dataarray_bytes(self, location, data_name, piece, bytes, data_type, n_components) result(error)
  !< Return the bytes (of the piece and step read), the VTK type and the number of components of a named dataarray.
  class(vtkhdf_reader),          intent(in)           :: self         !< Reader.
  character(*),                  intent(in)           :: location     !< Location: node, cell or field.
  character(*),                  intent(in)           :: data_name    !< Name of the dataarray.
  integer(I4P),                  intent(in), optional :: piece        !< Piece, from 1 (default 1, ignored for field data).
  integer(I1P),     allocatable, intent(out)          :: bytes(:)     !< Bytes of the dataarray.
  character(len=:), allocatable, intent(out)          :: data_type    !< VTK type.
  integer(I4P),                  intent(out)          :: n_components !< Number of components.
  integer(I4P)                                        :: error        !< Error status.
  character(len=:), allocatable                       :: path         !< Dataset.
  integer(I8P)                                        :: first        !< Rows skipped.
  integer(I8P)                                        :: count        !< Rows read.
  integer(I8P)                                        :: nt           !< Tuples.

  allocate(bytes(0))
  error = array_slice(self, location, data_name, piece, path, data_type, first, count, n_components, nt)
  if (error /= 0) return
  if (h5_dataset_read_bytes(self%root, path, first, count, bytes) /= 0_I4P) error = 6
  endfunction read_dataarray_bytes

  function read_geo_R8P(self, x, y, z, piece) result(error)
  !< Read the geometry of a piece (R8P): the points (UnstructuredGrid, PolyData, StructuredGrid) or the coordinates along
  !< each axis (RectilinearGrid). An ImageData has no stored geometry: the error is 4.
  class(vtkhdf_reader),   intent(in)           :: self  !< Reader.
  real(R8P), allocatable, intent(out)          :: x(:)  !< X coordinates.
  real(R8P), allocatable, intent(out)          :: y(:)  !< Y coordinates.
  real(R8P), allocatable, intent(out)          :: z(:)  !< Z coordinates.
  integer(I4P),           intent(in), optional :: piece !< Piece, from 1 (default 1).
  integer(I4P)                                 :: error !< Error status.
  integer(I1P), allocatable                    :: bytes(:) !< Bytes read.
  character(len=:), allocatable                :: dtype    !< Their VTK type.
  real(R8P), allocatable                       :: xyz(:)   !< Coordinates of the points, interleaved.

  allocate(x(0), y(0), z(0))
  error = 4
  if (.not.self%is_initialized) return
  if (self%mesh_topology == 'ImageData') return
  if (self%mesh_topology == 'RectilinearGrid') then
    error = axis_bytes(self, 1, piece, bytes, dtype) ; if (error /= 0) return
    error = coordinates_R8P(bytes, dtype, x) ; if (error /= 0) return
    error = axis_bytes(self, 2, piece, bytes, dtype) ; if (error /= 0) return
    error = coordinates_R8P(bytes, dtype, y) ; if (error /= 0) return
    error = axis_bytes(self, 3, piece, bytes, dtype) ; if (error /= 0) return
    error = coordinates_R8P(bytes, dtype, z)
  else
    error = points_bytes(self, piece, bytes, dtype) ; if (error /= 0) return
    error = coordinates_R8P(bytes, dtype, xyz) ; if (error /= 0) return
    x = xyz(1::3) ; y = xyz(2::3) ; z = xyz(3::3)
  endif
  endfunction read_geo_R8P

  function read_geo_R4P(self, x, y, z, piece) result(error)
  !< Read the geometry of a piece (R4P): the points (UnstructuredGrid, PolyData, StructuredGrid) or the coordinates along
  !< each axis (RectilinearGrid). An ImageData has no stored geometry: the error is 4.
  class(vtkhdf_reader),   intent(in)           :: self  !< Reader.
  real(R4P), allocatable, intent(out)          :: x(:)  !< X coordinates.
  real(R4P), allocatable, intent(out)          :: y(:)  !< Y coordinates.
  real(R4P), allocatable, intent(out)          :: z(:)  !< Z coordinates.
  integer(I4P),           intent(in), optional :: piece !< Piece, from 1 (default 1).
  integer(I4P)                                 :: error !< Error status.
  integer(I1P), allocatable                    :: bytes(:) !< Bytes read.
  character(len=:), allocatable                :: dtype    !< Their VTK type.
  real(R4P), allocatable                       :: xyz(:)   !< Coordinates of the points, interleaved.

  allocate(x(0), y(0), z(0))
  error = 4
  if (.not.self%is_initialized) return
  if (self%mesh_topology == 'ImageData') return
  if (self%mesh_topology == 'RectilinearGrid') then
    error = axis_bytes(self, 1, piece, bytes, dtype) ; if (error /= 0) return
    error = coordinates_R4P(bytes, dtype, x) ; if (error /= 0) return
    error = axis_bytes(self, 2, piece, bytes, dtype) ; if (error /= 0) return
    error = coordinates_R4P(bytes, dtype, y) ; if (error /= 0) return
    error = axis_bytes(self, 3, piece, bytes, dtype) ; if (error /= 0) return
    error = coordinates_R4P(bytes, dtype, z)
  else
    error = points_bytes(self, piece, bytes, dtype) ; if (error /= 0) return
    error = coordinates_R4P(bytes, dtype, xyz) ; if (error /= 0) return
    x = xyz(1::3) ; y = xyz(2::3) ; z = xyz(3::3)
  endif
  endfunction read_geo_R4P

  function read_ids(self, piece, section, data_name, ids) result(error)
  !< Read ids of the cells of a piece, as VTKFortran writes them: `section` Cells (UnstructuredGrid: `data_name`
  !< connectivity, offsets, types, faces, faceoffsets) or a PolyData block (Verts, Lines, Strips, Polys: connectivity,
  !< offsets). The offsets are the end of each cell; the faces are the face stream of the polyhedra, `faceoffsets` the end
  !< of the faces of each cell, -1 if it is not a polyhedron (error 4 if the file has no polyhedra). A face shared by two
  !< polyhedra (VTKHDF 2.9: stored once, `-(n+1)` in `PolyhedronToFaces` for the face `n` traversed in reverse) is given
  !< in the stream of each polyhedron, its points reversed where marked; a face id out of the faces of the partition, or
  !< offsets out of their dataset, are error 6.
  class(vtkhdf_reader),      intent(in)  :: self      !< Reader.
  integer(I4P),              intent(in)  :: piece     !< Piece, from 1.
  character(*),              intent(in)  :: section   !< Cells, Verts, Lines, Strips or Polys.
  character(*),              intent(in)  :: data_name !< connectivity, offsets, types, faces or faceoffsets.
  integer(I8P), allocatable, intent(out) :: ids(:)    !< Ids.
  integer(I4P)                           :: error     !< Error status.
  integer(I8P)                           :: g, first(9), n_parts !< Partition, starts, parts of the step.
  integer(I8P), allocatable              :: offsets(:), faces(:), face_offsets(:), polyhedra(:) !< Polyhedra datasets.
  logical,      allocatable              :: reversed(:) !< The face of a polyhedron is traversed in reverse (VTKHDF 2.9).
  character(len=:), allocatable          :: group     !< Group of a PolyData block, with the separator.
  integer(I8P)                           :: nc, n, b, c, f, i, k, m, nf, nids !< Counts, counters.

  allocate(ids(0))
  error = 4
  if (.not.self%is_initialized .or. self%is_grid()) return
  error = self%part(piece, g, first, n_parts)
  if (error /= 0) return
  if (section == 'Cells') then
    error = 4
    if (self%mesh_topology /= 'UnstructuredGrid') return
    error = 0
    nc = count_of(self%root, 'NumberOfCells', g, error) ; if (error /= 0) return
    select case(data_name)
    case('connectivity')
      n = count_of(self%root, 'NumberOfConnectivityIds', g, error) ; if (error /= 0) return
      error = read_I8P(self%root, 'Connectivity', first(4), n, ids)
    case('offsets')
      error = read_I8P(self%root, 'Offsets', first(3) + g - 1_I8P, nc + 1_I8P, offsets) ; if (error /= 0) return
      ids = offsets(2:) - offsets(1)
    case('types')
      error = read_I8P(self%root, 'Types', first(3), nc, ids)
    case('faces', 'faceoffsets')
      error = 4
      if (.not.h5_link_exists(self%root, 'FaceConnectivity')) return
      ! the faces of each cell (ids in the piece), then the points of each face
      error = read_I8P(self%root, 'PolyhedronOffsets', first(3) + g - 1_I8P, nc + 1_I8P, offsets) ; if (error /= 0) return
      m = count_of(self%root, 'NumberOfPolyhedronToFaceIds', g, error) ; if (error /= 0) return
      error = read_I8P(self%root, 'PolyhedronToFaces', first(7), m, polyhedra) ; if (error /= 0) return
      nf = count_of(self%root, 'NumberOfFaces', g, error) ; if (error /= 0) return
      error = read_I8P(self%root, 'FaceOffsets', first(6) + g - 1_I8P, nf + 1_I8P, face_offsets) ; if (error /= 0) return
      nids = count_of(self%root, 'NumberOfFaceConnectivityIds', g, error) ; if (error /= 0) return
      error = read_I8P(self%root, 'FaceConnectivity', first(5), nids, faces) ; if (error /= 0) return
      offsets = offsets - offsets(1) ; face_offsets = face_offsets - face_offsets(1)
      error = 6
      ! the faces of the polyhedra, from 1: since VTKHDF 2.9 a face shared by two polyhedra is stored once, -(n+1) in
      ! PolyhedronToFaces for the face n traversed in reverse; any id out of the faces of the partition is an error
      if (offsets(nc+1) > m .or. any(offsets(2:) < offsets(:nc))) return
      reversed = polyhedra < 0_I8P
      where (reversed) polyhedra = -polyhedra - 1_I8P
      polyhedra = polyhedra + 1_I8P
      if (any(polyhedra < 1_I8P .or. polyhedra > nf)) return
      if (any(face_offsets(2:) < face_offsets(:nf)) .or. face_offsets(nf+1) > nids) return
      ! the stream: for each polyhedron its faces, each one its points; faceoffsets its end, -1 if not a polyhedron
      n = 0_I8P
      do c=1_I8P, nc
        k = offsets(c+1) - offsets(c)
        if (k > 0_I8P) n = n + 1_I8P + sum(face_offsets(polyhedra(offsets(c)+1:offsets(c+1))+1) - &
                                           face_offsets(polyhedra(offsets(c)+1:offsets(c+1))) + 1_I8P)
      enddo
      deallocate(ids)
      if (data_name == 'faceoffsets') then
        allocate(ids(nc))
      else
        allocate(ids(n))
      endif
      i = 0_I8P
      do c=1_I8P, nc
        k = offsets(c+1) - offsets(c)
        if (k == 0_I8P) then
          if (data_name == 'faceoffsets') ids(c) = -1_I8P
          cycle
        endif
        if (data_name == 'faces') ids(i+1) = k
        i = i + 1_I8P
        do f=offsets(c)+1_I8P, offsets(c+1)
          b = polyhedra(f) ! the face, from 1
          m = face_offsets(b+1) - face_offsets(b)
          if (data_name == 'faces') then
            ids(i+1) = m
            if (reversed(f)) then ! its points in reverse order
              ids(i+2:i+1+m) = faces(face_offsets(b+1):face_offsets(b)+1:-1)
            else
              ids(i+2:i+1+m) = faces(face_offsets(b)+1:face_offsets(b+1))
            endif
          endif
          i = i + 1_I8P + m
        enddo
        if (data_name == 'faceoffsets') ids(c) = i
      enddo
      error = 0
    case default
      error = 4
    endselect
  else
    error = 4
    if (self%mesh_topology /= 'PolyData') return
    select case(section)
    case('Verts')
      b = 1
    case('Lines')
      b = 2
    case('Strips')
      b = 3
    case('Polys')
      b = 4
    case default
      return
    endselect
    group = trim(BLOCKS(b))//'/'
    error = 0
    nc = count_of(self%root, group//'NumberOfCells', g, error) ; if (error /= 0) return
    select case(data_name)
    case('connectivity')
      n = count_of(self%root, group//'NumberOfConnectivityIds', g, error) ; if (error /= 0) return
      error = read_I8P(self%root, group//'Connectivity', block_start(self, g, b, 'NumberOfConnectivityIds', &
                                                                      'ConnectivityIdOffsets'), n, ids)
    case('offsets')
      error = read_I8P(self%root, group//'Offsets', block_start(self, g, b, 'NumberOfCells', 'CellOffsets') + g - 1_I8P, &
                       nc + 1_I8P, offsets)
      if (error /= 0) return
      ids = offsets(2:) - offsets(1)
    case default
      error = 4
    endselect
  endif
  endfunction read_ids

  ! private methods
  pure function grid_points(self) result(grid)
  !< Points of the grid along each axis.
  class(vtkhdf_reader), intent(in) :: self    !< Reader.
  integer(I8P)                     :: grid(3) !< Points along x, y, z.

  grid = int(self%whole_extent(2::2) - self%whole_extent(1::2), I8P) + 1_I8P
  endfunction grid_points

  elemental function is_grid(self) result(is_grid_)
  !< Return true if the dataset is a grid (ImageData, RectilinearGrid, StructuredGrid: one piece).
  class(vtkhdf_reader), intent(in) :: self     !< Reader.
  logical                          :: is_grid_ !< Check result.

  is_grid_ = .false.
  if (allocated(self%mesh_topology)) is_grid_ = self%mesh_topology == 'ImageData' .or. &
                                                self%mesh_topology == 'RectilinearGrid' .or. &
                                                self%mesh_topology == 'StructuredGrid'
  endfunction is_grid

  function part(self, piece, g, first, n_parts) result(error)
  !< The partition `g` (from 1, in the whole file) of the piece `piece` of the step read, the parts of the step, and where
  !< the partition starts in the datasets the parts share: `first` = points, (unused), cells, connectivity ids, face point
  !< ids, faces, polyhedra's faces (UnstructuredGrid), each counted from the start of the step (Steps offsets) plus the
  !< counts of the parts of the step before it.
  class(vtkhdf_reader), intent(in)           :: self     !< Reader.
  integer(I4P),         intent(in), optional :: piece    !< Piece, from 1 (default 1).
  integer(I8P),         intent(out)          :: g        !< Partition, from 1.
  integer(I8P),         intent(out)          :: first(9) !< Starts of the partition.
  integer(I8P),         intent(out)          :: n_parts  !< Parts of the step.
  integer(I4P)                               :: error    !< Error status.
  integer(I8P), allocatable                  :: counts(:) !< Counts of the parts.
  integer(I8P)                               :: p0       !< Parts before the step.
  integer(I8P)                               :: p        !< Piece, from 1.
  integer(I4P)                               :: k        !< Counter.
  character(*), parameter :: COUNTS_OF(7) = ['NumberOfPoints             ', '                           ', &
                                             'NumberOfCells              ', 'NumberOfConnectivityIds    ', &
                                             'NumberOfFaceConnectivityIds', 'NumberOfFaces              ', &
                                             'NumberOfPolyhedronToFaceIds']
  character(*), parameter :: OFFSETS_OF(7) = ['PointOffsets             ', '                         ', &
                                              'CellOffsets              ', 'ConnectivityIdOffsets    ', &
                                              'FaceConnectivityOffsets  ', 'FaceOffsetsOffsets       ', &
                                              'PolyhedronToFaceIdOffsets']

  g = 0_I8P ; first = 0_I8P ; n_parts = 0_I8P
  p = 1_I8P
  if (present(piece)) p = int(piece, I8P)
  error = 6
  if (h5_dataset_read(self%root, 'NumberOfPoints', counts) /= 0_I4P) return
  if (self%n_steps > 0_I8P) then
    p0 = self%step_value('PartOffsets', 1_I4P, error) ; if (error /= 0) return
    if (h5_link_exists(self%root, 'Steps/NumberOfParts')) then
      n_parts = self%step_value('NumberOfParts', 1_I4P, error) ; if (error /= 0) return
    else
      n_parts = size(counts, kind=I8P) / self%n_steps
    endif
  else
    p0 = 0_I8P
    n_parts = size(counts, kind=I8P)
  endif
  error = 4
  if (p < 1_I8P .or. p > n_parts) return
  g = p0 + p
  error = 6
  if (g > size(counts, kind=I8P)) return
  error = 0
  do k=1, 7
    if (len_trim(COUNTS_OF(k)) == 0) cycle
    if (.not.h5_link_exists(self%root, trim(COUNTS_OF(k)))) cycle
    if (self%mesh_topology /= 'UnstructuredGrid' .and. k > 1) cycle
    if (h5_dataset_read(self%root, trim(COUNTS_OF(k)), counts) /= 0_I4P) then
      error = 6
      return
    endif
    if (size(counts, kind=I8P) < g) then
      error = 6
      return
    endif
    first(k) = sum(counts(p0+1:g-1))
    if (self%n_steps > 0_I8P) then
      if (h5_link_exists(self%root, 'Steps/'//trim(OFFSETS_OF(k)))) then
        first(k) = first(k) + self%step_value(trim(OFFSETS_OF(k)), 1_I4P, error)
        if (error /= 0) return
      endif
    else
      first(k) = sum(counts(1:g-1))
    endif
  enddo
  endfunction part

  function step_value(self, name, column, error) result(value)
  !< The entry of the dataset `Steps/name` for the step read (the column `column` of a [NSteps, n] dataset).
  class(vtkhdf_reader), intent(in)  :: self   !< Reader.
  character(*),         intent(in)  :: name   !< Dataset of Steps.
  integer(I4P),         intent(in)  :: column !< Column, from 1.
  integer(I4P),         intent(out) :: error  !< Error status.
  integer(I8P)                      :: value  !< Entry.
  integer(I8P), allocatable         :: values(:), dims(:) !< Values, shape.
  character(len=:), allocatable     :: dtype  !< Type.

  value = 0_I8P
  error = 6
  if (h5_dataset_info(self%root, 'Steps/'//name, dtype, dims) /= 0_I4P) return
  if (h5_dataset_read(self%root, 'Steps/'//name, values) /= 0_I4P) return
  if (size(dims) == 1) then
    if (size(values, kind=I8P) < self%step) return
    value = values(self%step)
  else
    if (size(values, kind=I8P) < dims(1) * self%step .or. column > dims(1)) return
    value = values(dims(1) * (self%step - 1_I8P) + column)
  endif
  error = 0
  endfunction step_value

  ! non type-bound helpers
  function array_slice(self, location, data_name, piece, path, data_type, first, count, n_components, n_tuples) result(error)
  !< Where a dataarray of the piece and step read is: its dataset, the rows to skip and to read along its last dimension
  !< (tuples of a partition, a time slice of a grid, the tuples of a step of field data), its type, components, tuples.
  class(vtkhdf_reader),          intent(in)           :: self         !< Reader.
  character(*),                  intent(in)           :: location     !< Location: node, cell or field.
  character(*),                  intent(in)           :: data_name    !< Name of the dataarray.
  integer(I4P),                  intent(in), optional :: piece        !< Piece, from 1 (default 1, ignored for field data).
  character(len=:), allocatable, intent(out)          :: path         !< Dataset.
  character(len=:), allocatable, intent(out)          :: data_type    !< VTK type.
  integer(I8P),                  intent(out)          :: first        !< Rows skipped.
  integer(I8P),                  intent(out)          :: count        !< Rows read.
  integer(I4P),                  intent(out)          :: n_components !< Components.
  integer(I8P),                  intent(out)          :: n_tuples     !< Tuples.
  integer(I4P)                                        :: error        !< Error status.
  character(len=:), allocatable                       :: group        !< Group of the location.
  integer(I8P), allocatable                           :: dims(:)      !< Shape of the dataset, Fortran order.
  integer(I8P)                                        :: g, starts(9), n_parts, b !< Partition, starts, parts, counter.
  integer(I8P)                                        :: grid(3)      !< Points of a grid along each axis.
  integer(I4P)                                        :: n_axes       !< Axes of the grid in the arrays.

  path = '' ; data_type = '' ; first = 0_I8P ; count = 0_I8P ; n_components = 1 ; n_tuples = 0_I8P
  error = 4
  if (.not.self%is_initialized) return
  group = location_group(location)
  if (len(group) == 0) return
  path = group//'/'//trim(data_name)
  if (.not.h5_link_exists(self%root, group)) return
  if (.not.h5_link_exists(self%root, path)) return
  error = 6
  if (h5_dataset_info(self%root, path, data_type, dims) /= 0_I4P) return
  if (len(data_type) == 0 .or. size(dims) == 0) then
    error = 5
    return
  endif
  if (group == 'FieldData') then
    count = dims(size(dims))
    if (size(dims) > 1) n_components = int(product(dims(1:size(dims)-1)), I4P)
    if (self%n_steps > 0_I8P .and. h5_link_exists(self%root, 'Steps/FieldDataOffsets/'//trim(data_name))) then
      first = self%step_value('FieldDataOffsets/'//trim(data_name), 1_I4P, error) ; if (error /= 0) return
      count = self%step_value('FieldDataSizes/'//trim(data_name), 2_I4P, error) ; if (error /= 0) return
    endif
    n_tuples = count
  elseif (self%is_grid()) then
    if (present(piece)) then
      error = 4
      if (piece /= 1) return
    endif
    grid = self%grid_points()
    n_axes = max(1, size(pack(grid, grid > 1_I8P))) ! (count is a dummy here)
    if (self%n_steps > 0_I8P) then ! the time axis last: one slice
      first = self%step - 1_I8P
      count = 1_I8P
      if (size(dims) > n_axes + 1) n_components = int(dims(1), I4P)
    else
      first = 0_I8P
      count = dims(size(dims))
      if (size(dims) > n_axes) n_components = int(dims(1), I4P)
    endif
    if (group == 'PointData') then
      n_tuples = product(grid)
    else
      n_tuples = product(max(grid - 1_I8P, 1_I8P))
    endif
  else
    error = self%part(piece, g, starts, n_parts) ; if (error /= 0) return
    if (size(dims) > 1) n_components = int(dims(1), I4P)
    if (group == 'PointData') then
      first = starts(1)
      count = count_of(self%root, 'NumberOfPoints', g, error) ; if (error /= 0) return
    elseif (self%mesh_topology == 'UnstructuredGrid') then
      first = starts(3)
      count = count_of(self%root, 'NumberOfCells', g, error) ; if (error /= 0) return
    else ! PolyData: the cells of the four blocks
      first = 0_I8P ; count = 0_I8P
      do b=1_I8P, 4_I8P
        first = first + block_start(self, g, b, 'NumberOfCells', 'CellOffsets')
        count = count + count_of(self%root, trim(BLOCKS(b))//'/NumberOfCells', g, error) ; if (error /= 0) return
      enddo
    endif
    if (self%n_steps > 0_I8P) then
      if (h5_link_exists(self%root, 'Steps/'//trim(merge('PointDataOffsets/', 'CellDataOffsets/ ', group == 'PointData'))// &
                                    trim(data_name))) then
        ! an explicit offset of the step for this array, plus the parts of the step before the piece
        b = first
        first = self%step_value(trim(merge('PointDataOffsets/', 'CellDataOffsets/ ', group == 'PointData'))// &
                                trim(data_name), 1_I4P, error)
        if (error /= 0) return
        first = first + b - step_start(self, group)
      endif
    endif
    n_tuples = count
  endif
  error = 6
  if (first < 0_I8P .or. first + count > dims(size(dims))) return
  error = 0
  endfunction array_slice

  function step_start(self, group) result(start)
  !< Where the step read starts in the points (PointData) or cells (CellData) given by the geometry offsets of Steps.
  class(vtkhdf_reader), intent(in) :: self  !< Reader.
  character(*),         intent(in) :: group !< PointData or CellData.
  integer(I8P)                     :: start !< Start.
  integer(I4P)                     :: error !< Error status.
  integer(I4P)                     :: b     !< Counter.

  start = 0_I8P
  if (group == 'PointData') then
    if (h5_link_exists(self%root, 'Steps/PointOffsets')) start = self%step_value('PointOffsets', 1_I4P, error)
  elseif (self%mesh_topology == 'UnstructuredGrid') then
    if (h5_link_exists(self%root, 'Steps/CellOffsets')) start = self%step_value('CellOffsets', 1_I4P, error)
  else
    do b=1, 4
      if (h5_link_exists(self%root, 'Steps/CellOffsets')) start = start + self%step_value('CellOffsets', STEP_COLUMN(b), error)
    enddo
  endif
  endfunction step_start

  function block_start(self, g, b, counts_name, offsets_name) result(start)
  !< Where the partition `g` starts in the cells (`NumberOfCells`, `CellOffsets`) or the ids (`NumberOfConnectivityIds`,
  !< `ConnectivityIdOffsets`) of the PolyData block `b`.
  class(vtkhdf_reader), intent(in) :: self         !< Reader.
  integer(I8P),         intent(in) :: g            !< Partition, from 1.
  integer(I8P),         intent(in) :: b            !< Block, from 1.
  character(*),         intent(in) :: counts_name  !< Counts of the block.
  character(*),         intent(in) :: offsets_name !< Offsets of Steps.
  integer(I8P)                     :: start        !< Start.
  integer(I8P), allocatable        :: counts(:)    !< Counts of the parts.
  integer(I8P)                     :: p0           !< Parts before the step.
  integer(I4P)                     :: error        !< Error status.

  start = 0_I8P
  if (h5_dataset_read(self%root, trim(BLOCKS(b))//'/'//counts_name, counts) /= 0_I4P) return
  if (self%n_steps > 0_I8P) then
    p0 = self%step_value('PartOffsets', 1_I4P, error)
    start = sum(counts(p0+1:g-1))
    if (h5_link_exists(self%root, 'Steps/'//offsets_name)) &
      start = start + self%step_value(offsets_name, STEP_COLUMN(b), error)
  else
    start = sum(counts(1:g-1))
  endif
  endfunction block_start

  function count_of(root, name, g, error) result(n)
  !< The count of the partition `g` in the dataset of counts `name`.
  integer(hid_kind), intent(in)    :: root   !< Group /VTKHDF.
  character(*),      intent(in)    :: name   !< Dataset of counts.
  integer(I8P),      intent(in)    :: g      !< Partition, from 1.
  integer(I4P),      intent(inout) :: error  !< Error status: 6 if the count cannot be read.
  integer(I8P)                     :: n      !< Count.
  integer(I8P), allocatable        :: counts(:) !< Counts.

  n = 0_I8P
  if (h5_dataset_read(root, name, counts) /= 0_I4P) then
    error = 6
  elseif (size(counts, kind=I8P) < g) then
    error = 6
  else
    n = counts(g)
  endif
  endfunction count_of

  function axis_bytes(self, a, piece, bytes, data_type) result(error)
  !< The bytes of the coordinates along the axis `a` (1 x, 2 y, 3 z) of a RectilinearGrid (of the step read).
  class(vtkhdf_reader),          intent(in)           :: self      !< Reader.
  integer(I4P),                  intent(in)           :: a         !< Axis.
  integer(I4P),                  intent(in), optional :: piece     !< Piece, from 1 (default 1).
  integer(I1P),     allocatable, intent(out)          :: bytes(:)  !< Bytes of the coordinates.
  character(len=:), allocatable, intent(out)          :: data_type !< Their VTK type.
  integer(I4P)                                        :: error     !< Error status.
  integer(I8P), allocatable                           :: dims(:)   !< Shape.
  integer(I8P)                                        :: grid(3), start !< Points along each axis, offset of the step.
  character(*), parameter                             :: AXES(3) = ['X', 'Y', 'Z'] !< Axes.

  allocate(bytes(0))
  data_type = ''
  error = 4
  if (present(piece)) then
    if (piece /= 1) return
  endif
  grid = self%grid_points()
  start = 0_I8P
  if (self%n_steps > 0_I8P) then
    if (h5_link_exists(self%root, 'Steps/'//AXES(a)//'CoordinatesOffsets')) then
      start = self%step_value(AXES(a)//'CoordinatesOffsets', 1_I4P, error) ; if (error /= 0) return
    endif
  endif
  error = 6
  if (h5_dataset_info(self%root, AXES(a)//'Coordinates', data_type, dims) /= 0_I4P) return
  if (h5_dataset_read_bytes(self%root, AXES(a)//'Coordinates', start, grid(a), bytes) /= 0_I4P) return
  error = 0
  endfunction axis_bytes

  function points_bytes(self, piece, bytes, data_type) result(error)
  !< The bytes of the points of a piece (of the step read), interleaved: UnstructuredGrid, PolyData, StructuredGrid.
  class(vtkhdf_reader),          intent(in)           :: self      !< Reader.
  integer(I4P),                  intent(in), optional :: piece     !< Piece, from 1 (default 1).
  integer(I1P),     allocatable, intent(out)          :: bytes(:)  !< Bytes of the points.
  character(len=:), allocatable, intent(out)          :: data_type !< Their VTK type.
  integer(I4P)                                        :: error     !< Error status.
  integer(I8P), allocatable                           :: dims(:)   !< Shape.
  integer(I8P)                                        :: g, first(9), n_parts, start, n !< Partition, sizes.

  allocate(bytes(0))
  data_type = ''
  error = 6
  if (h5_dataset_info(self%root, 'Points', data_type, dims) /= 0_I4P) return
  if (self%mesh_topology == 'StructuredGrid') then
    error = 4
    if (present(piece)) then
      if (piece /= 1) return
    endif
    start = 0_I8P
    n = dims(size(dims))
    if (self%n_steps > 0_I8P .and. size(dims) == 5) then ! a time slice
      start = self%step - 1_I8P
      if (h5_link_exists(self%root, 'Steps/PointOffsets')) then
        start = self%step_value('PointOffsets', 1_I4P, error) ; if (error /= 0) return
      endif
      n = 1_I8P
    endif
  else
    error = self%part(piece, g, first, n_parts) ; if (error /= 0) return
    n = count_of(self%root, 'NumberOfPoints', g, error) ; if (error /= 0) return
    start = first(1)
  endif
  error = 6
  if (h5_dataset_read_bytes(self%root, 'Points', start, n, bytes) /= 0_I4P) return
  error = 0
  endfunction points_bytes

  function coordinates_R8P(bytes, data_type, x) result(error)
  !< Coordinates (R8P) from their bytes: real, or integer (VTK writes the coordinates of some datasets as Int64).
  integer(I1P),           intent(in)  :: bytes(:)  !< Bytes.
  character(*),           intent(in)  :: data_type !< VTK type.
  real(R8P), allocatable, intent(out) :: x(:)      !< Coordinates.
  integer(I4P)                        :: error     !< Error status.
  integer(I8P), allocatable           :: ix(:)     !< Integer coordinates.

  if (index(data_type, 'Float') > 0) then
    call bytes_to_array(bytes=bytes, data_type=data_type, x=x, error=error)
  else
    allocate(x(0))
    call bytes_to_array(bytes=bytes, data_type=data_type, x=ix, error=error)
    if (error == 0) x = real(ix, R8P)
  endif
  endfunction coordinates_R8P

  function coordinates_R4P(bytes, data_type, x) result(error)
  !< Coordinates (R4P) from their bytes: real, or integer (VTK writes the coordinates of some datasets as Int64).
  integer(I1P),           intent(in)  :: bytes(:)  !< Bytes.
  character(*),           intent(in)  :: data_type !< VTK type.
  real(R4P), allocatable, intent(out) :: x(:)      !< Coordinates.
  integer(I4P)                        :: error     !< Error status.
  integer(I8P), allocatable           :: ix(:)     !< Integer coordinates.

  if (index(data_type, 'Float') > 0) then
    call bytes_to_array(bytes=bytes, data_type=data_type, x=x, error=error)
  else
    allocate(x(0))
    call bytes_to_array(bytes=bytes, data_type=data_type, x=ix, error=error)
    if (error == 0) x = real(ix, R4P)
  endif
  endfunction coordinates_R4P

  function read_I8P(root, name, first, count, ids) result(error)
  !< Read the slice `first+1 ... first+count` of an integer dataset into I8P values.
  integer(hid_kind),         intent(in)  :: root     !< Group /VTKHDF.
  character(*),              intent(in)  :: name     !< Dataset.
  integer(I8P),              intent(in)  :: first    !< Values skipped.
  integer(I8P),              intent(in)  :: count    !< Values read.
  integer(I8P), allocatable, intent(out) :: ids(:)   !< Values.
  integer(I4P)                           :: error    !< Error status.
  integer(I1P), allocatable              :: bytes(:) !< Bytes read.
  integer(I8P), allocatable              :: dims(:)  !< Shape.
  character(len=:), allocatable          :: dtype    !< Type.

  allocate(ids(0))
  error = 6
  if (h5_dataset_info(root, name, dtype, dims) /= 0_I4P) return
  if (h5_dataset_read_bytes(root, name, first, count, bytes) /= 0_I4P) return
  call bytes_to_array(bytes=bytes, data_type=dtype, x=ids, error=error)
  endfunction read_I8P

  function location_group(location) result(group)
  !< The group of a location: node PointData, cell CellData, field FieldData (case insensitive), empty otherwise.
  character(*), intent(in)      :: location !< Location.
  character(len=:), allocatable :: group    !< Group.
  character(len=:), allocatable :: l        !< Location, lower case.
  integer                       :: i        !< Counter.

  l = trim(adjustl(location))
  do i=1, len(l)
    if (l(i:i) >= 'A' .and. l(i:i) <= 'Z') l(i:i) = achar(iachar(l(i:i)) + 32)
  enddo
  select case(l)
  case('node')
    group = 'PointData'
  case('cell')
    group = 'CellData'
  case('field')
    group = 'FieldData'
  case default
    group = ''
  endselect
  endfunction location_group

  function is_hdf5_file(filename) result(is_hdf5)
  !< Return true if the file starts with the HDF5 signature (`\\211HDF\\r\\n\\032\\n`).
  character(*), intent(in) :: filename  !< File name.
  logical                  :: is_hdf5   !< Check result.
  character(len=8)         :: signature !< First bytes of the file.
  integer                  :: unit      !< Unit.
  integer                  :: iostat    !< IO status.

  is_hdf5 = .false.
  open(newunit=unit, file=filename, access='stream', form='unformatted', action='read', status='old', iostat=iostat)
  if (iostat /= 0) return
  read(unit, iostat=iostat) signature
  close(unit)
  if (iostat /= 0) return
  is_hdf5 = signature == achar(137)//'HDF'//achar(13)//achar(10)//achar(26)//achar(10)
  endfunction is_hdf5_file
endmodule vtk_fortran_vtk_file_vtkhdf_reader
