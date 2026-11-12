!< VTK file VTKHDF reader.
module vtk_fortran_vtk_file_vtkhdf_reader
!< VTK file VTKHDF reader: the reading of `.vtkhdf` files behind `xml_reader` (`vtk_file%initialize(filename,
!< action='read')`), which dispatches its calls here when the file is an HDF5 one.
!<
!< It reads every dataset the VTKHDF writer writes, and the same layouts written by VTK: ImageData, RectilinearGrid,
!< StructuredGrid (one piece), UnstructuredGrid (polyhedra included), PolyData (each partition a piece), Table (no
!< pieces: its columns are read with `location='row'`, the rows of the step read; `NumberOfRows`) and OverlappingAMR
!< (`get_levels`, `set_level`: the boxes of the level read, from level 0, are its pieces, `read_piece` giving their point
!< extents, `get_info` its spacing and the origin), HyperTreeGrid (one piece: its root grid, `read_geo` its coordinates as
!< for a RectilinearGrid, `get_trees` the ids of its trees and the branch factor, `read_tree` the descriptor and mask of a
!< tree as `write_tree` takes them, unpacked from the bits, MSB first; cell data the cells of the trees in their order,
!< breadth first in each), static or time
!< series (`Steps`: `get_steps`, `set_step`; the step 1 by default; through the offsets of `Steps`, so that a step reusing
!< the mesh or an array of the step before reads it as written in full). The values are read as HDF5 hyperslabs of the piece
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
use vtk_fortran_parameters, only : ACTIVE_ROLES, active_role

implicit none
private
public :: vtkhdf_reader
public :: is_hdf5_file
public :: polyhedra_stream

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
  integer(I4P)                  :: n_levels=0_I4P         !< Levels of an OverlappingAMR.
  integer(I4P)                  :: level=0_I4P            !< Level read (from 0).
  integer(I8P)                  :: version(2)=[2_I8P, 0_I8P] !< Version of the file (of the composite if the dataset has none).
  contains
    ! public methods
    procedure, pass(self) :: close                !< Close the file and reset the reader.
    procedure, pass(self) :: forget               !< Reset the reader without closing the file (pure).
    procedure, pass(self) :: get_dataarray_info   !< Return the type, components and tuples of a dataarray.
    procedure, pass(self) :: get_active           !< Return the name of the active array of a role.
    procedure, pass(self) :: get_dataarray_names  !< Return the names of the dataarrays of a location.
    procedure, pass(self) :: get_info             !< Return the information of the dataset.
    procedure, pass(self) :: get_levels           !< Return the number of levels of an OverlappingAMR.
    procedure, pass(self) :: get_steps            !< Return the times of the steps.
    procedure, pass(self) :: initialize           !< Initialize the reader: open the file.
    procedure, pass(self) :: read_dataarray_bytes !< Return the bytes, type and components of a dataarray.
    generic               :: read_geo => read_geo_R8P, read_geo_R4P !< Read the geometry.
    procedure, pass(self) :: read_geo_R8P         !< Read the geometry (R8P).
    procedure, pass(self) :: read_geo_R4P         !< Read the geometry (R4P).
    procedure, pass(self) :: read_ids             !< Read ids of the cells (connectivity, offsets, types, faces).
    procedure, pass(self) :: read_piece           !< Read the counts and extent of a piece.
    procedure, pass(self) :: get_trees            !< Return the trees of a HyperTreeGrid (of the step read).
    procedure, pass(self) :: read_tree            !< Read a tree of a HyperTreeGrid: its descriptor and mask.
    procedure, pass(self) :: set_level            !< Select the level read of an OverlappingAMR.
    procedure, pass(self) :: set_step             !< Select the step read.
    ! private methods
    procedure, pass(self), private :: amr_box      !< A box of the level and step read of an OverlappingAMR.
    procedure, pass(self), private :: is_v23       !< The file is VTKHDF 2.3 or later (OverlappingAMR Steps names).
    procedure, pass(self), private :: grid_points  !< Points of the grid along each axis.
    procedure, pass(self), private :: prefix       !< Group of the point and cell data of the level read (AMR).
    procedure, pass(self), private :: is_grid      !< The dataset is a grid (one piece).
    procedure, pass(self), private :: part         !< The partition of a piece of the step, and where it starts.
    procedure, pass(self), private :: step_value   !< An entry of a dataset of Steps for the step read.
    procedure, pass(self), private :: step_entry   !< The entry of the step read of a dataset with one per step.
    procedure, pass(self), private :: tree_layout  !< Where the trees of a HyperTreeGrid (of the step read) are.
endtype vtkhdf_reader

contains
  ! public methods
  function initialize(self, filename, dataset) result(error)
  !< Initialize the reader: open the file, read the type and extent of its dataset and its steps. With `dataset`, the
  !< dataset is the group `/VTKHDF/<dataset>` of a composite file (a `MultiBlockDataSet` or `PartitionedDataSetCollection`:
  !< `Block_3`, a dataset name, or a path through its `Assembly`, as `vtm_file%get_entries` gives it).
  class(vtkhdf_reader), intent(inout)        :: self     !< Reader.
  character(*),         intent(in)           :: filename !< File name.
  character(*),         intent(in), optional :: dataset  !< A dataset of a composite file.
  character(len=:), allocatable              :: path     !< Group of the dataset.
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
  path = 'VTKHDF'
  if (present(dataset)) then ! a dataset of a composite file (error 4 if there is none of that name)
    error = 4
    if (len_trim(dataset) == 0) return
    path = 'VTKHDF/'//trim(adjustl(dataset))
    if (.not.h5_link_exists(self%file_id, path)) return
  endif
  if (h5_group_open(self%file_id, path, self%root) /= 0_I4P) then
    self%root = -1_hid_kind
    return
  endif
  if (h5_attribute_read(self%root, 'Type', text) /= 0_I4P) return
  ! the Version (of the composite file for a dataset without one): VTK chooses the Steps names of an OverlappingAMR by it
  self%version = [2_I8P, 0_I8P]
  if (h5_attribute_read(self%root, 'Version', ivalues) == 0_I4P) then
    if (size(ivalues) == 2) self%version = ivalues
  elseif (present(dataset)) then
    if (h5_group_open(self%file_id, 'VTKHDF', steps) == 0_I4P) then
      if (h5_attribute_read(steps, 'Version', ivalues) == 0_I4P) then
        if (size(ivalues) == 2) self%version = ivalues
      endif
      if (h5_group_close(steps) /= 0_I4P) return
    endif
  endif
  error = 3
  select case(text)
  case('ImageData', 'RectilinearGrid', 'StructuredGrid', 'UnstructuredGrid', 'PolyData', 'Table', 'OverlappingAMR', &
       'HyperTreeGrid')
    self%mesh_topology = text
  case default
    return
  endselect
  self%n_levels = 0
  self%level = 0
  if (self%mesh_topology == 'OverlappingAMR') then ! the levels Level0, Level1, ...
    do while (h5_link_exists(self%root, 'Level'//trim(str(n=self%n_levels, no_sign=.true.))))
      self%n_levels = self%n_levels + 1
    enddo
  endif
  error = 6
  if (self%mesh_topology == 'ImageData') then
    if (h5_attribute_read(self%root, 'WholeExtent', ivalues) /= 0_I4P) return
    if (size(ivalues) /= 6) return
    self%whole_extent = int(ivalues, I4P)
  elseif (self%is_grid() .or. self%mesh_topology == 'HyperTreeGrid') then ! (a HyperTreeGrid: the points of its root grid)
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
  self%n_levels = 0
  self%level = 0
  self%version = [2_I8P, 0_I8P]
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
  integer(I4P)                                         :: ext(6)        !< Extent of a box (AMR).
  integer(I8P)                                         :: starts(2)     !< Points, cells before a box (AMR).
  integer(I8P)                                         :: n_boxes       !< Boxes of the level read (AMR).
  integer(hid_kind)                                    :: dset          !< Group of the level read (AMR).
  integer(I4P)                                         :: e             !< Error of a closing.

  error = 4
  if (.not.self%is_initialized) return
  error = 0
  if (present(mesh_topology)) mesh_topology = self%mesh_topology
  if (present(npieces)) then
    npieces = 1
    if (self%mesh_topology == 'Table') then ! no pieces: its columns are read with location='row'
      npieces = 0
    elseif (self%mesh_topology == 'HyperTreeGrid') then ! one piece: its root grid and trees
      npieces = 1
    elseif (self%mesh_topology == 'OverlappingAMR') then ! the boxes of the level read
      error = self%amr_box(1, ext, starts, n_boxes) ! n_boxes even when the level has no box in the step
      npieces = int(n_boxes, I4P)
      if (error == 4) error = 0 ! no box in the step; Steps entries missing or inconsistent stay an error
    elseif (.not.self%is_grid()) then
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
      if (h5_attribute_read(self%root, 'Origin', rvalues) == 0_I4P) then
        if (size(rvalues) >= 3) origin = rvalues(1:3)
      endif
    endif
  endif
  if (present(spacing) .and. self%mesh_topology == 'OverlappingAMR') then ! the spacing of the level read
    spacing = 1._R8P
    if (h5_group_open(self%root, 'Level'//trim(str(n=self%level, no_sign=.true.)), dset) == 0_I4P) then
      if (h5_attribute_read(dset, 'Spacing', rvalues) == 0_I4P) then
        if (size(rvalues) >= 3) then
          spacing = rvalues(1:3)
        else
          error = 6 ! a malformed level
        endif
      endif
      e = h5_group_close(dset)
    endif
  elseif (present(spacing)) then
    spacing = 1._R8P
    if (h5_attribute_exists(self%root, 'Spacing')) then
      if (h5_attribute_read(self%root, 'Spacing', rvalues) == 0_I4P) then
        if (size(rvalues) >= 3) spacing = rvalues(1:3)
      endif
    endif
  endif
  if (present(direction)) then
    direction = [1._R8P, 0._R8P, 0._R8P, 0._R8P, 1._R8P, 0._R8P, 0._R8P, 0._R8P, 1._R8P]
    if (h5_attribute_exists(self%root, 'Direction')) then
      if (h5_attribute_read(self%root, 'Direction', rvalues) == 0_I4P) then
        if (size(rvalues) >= 9) direction = rvalues(1:9)
      endif
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

  function get_levels(self, n_levels) result(error)
  !< Return the number of levels of an OverlappingAMR (0 for another dataset).
  class(vtkhdf_reader), intent(in)  :: self     !< Reader.
  integer(I4P),         intent(out) :: n_levels !< Levels.
  integer(I4P)                      :: error    !< Error status.

  n_levels = 0
  error = 4
  if (.not.self%is_initialized) return
  n_levels = self%n_levels
  error = 0
  endfunction get_levels

  function set_level(self, level) result(error)
  !< Select the level read of an OverlappingAMR, from 0 (level 0 by default): its boxes are the pieces of the following
  !< calls.
  class(vtkhdf_reader), intent(inout) :: self  !< Reader.
  integer(I4P),         intent(in)    :: level !< Level, from 0.
  integer(I4P)                        :: error !< Error status.

  error = 4
  if (.not.self%is_initialized .or. level < 0 .or. level >= self%n_levels) return
  self%level = level
  error = 0
  endfunction set_level

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
  integer(I4P)                                :: ext(6)  !< Extent of a box (AMR).
  integer(I8P)                                :: starts(2) !< Points, cells before a box (AMR).
  integer(I8P)                                :: n_boxes !< Boxes of the level read (AMR).

  counts = 0_I8P
  error = 4
  if (.not.self%is_initialized .or. self%mesh_topology == 'Table') return ! a Table has no pieces
  if (self%mesh_topology == 'OverlappingAMR') then ! a box of the level read: its point extent
    error = self%amr_box(piece, ext, starts, n_boxes)
    if (error /= 0) return
    counts(1) = product(int(ext(2::2) - ext(1::2), I8P) + 1_I8P)
    counts(2) = product(int(ext(2::2) - ext(1::2), I8P))
    if (present(nx1)) nx1 = ext(1)
    if (present(nx2)) nx2 = ext(2)
    if (present(ny1)) ny1 = ext(3)
    if (present(ny2)) ny2 = ext(4)
    if (present(nz1)) nz1 = ext(5)
    if (present(nz2)) nz2 = ext(6)
    if (present(np)) np = counts(1)
    if (present(nc)) nc = counts(2)
    return
  elseif (self%is_grid()) then
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

  function get_active(self, location, role, data_name, piece) result(error)
  !< Return the name of the active array of a role (see `xml_reader%get_active`) of a location (node or cell): the
  !< dataset of the group whose `Attribute` attribute is the role (case insensitive); empty if none.
  class(vtkhdf_reader),          intent(in)           :: self      !< Reader.
  character(*),                  intent(in)           :: location  !< Location: node or cell.
  character(*),                  intent(in)           :: role      !< Role.
  character(len=:), allocatable, intent(out)          :: data_name !< Name of the active array, empty if none.
  integer(I4P),                  intent(in), optional :: piece     !< Piece, from 1 (default 1).
  integer(I4P)                                        :: error     !< Error status.
  character(len=:), allocatable                       :: group     !< Group of the location.
  character(len=:), allocatable                       :: list      !< Names, each followed by NUL.
  character(len=:), allocatable                       :: text      !< Attribute of a dataset.
  integer(hid_kind)                                   :: dset      !< A dataset.
  integer(I8P)                                        :: g, first(9), n_parts !< Partition, starts, parts of the step.
  integer(I4P)                                        :: i, j, e   !< Counters, error of a closing.

  data_name = ''
  error = 4
  if (.not.self%is_initialized .or. active_role(role) == 0) return
  group = location_group(location)
  if (group /= 'PointData' .and. group /= 'CellData') return
  if (.not.(self%is_grid() .or. self%mesh_topology == 'OverlappingAMR')) then
    error = self%part(piece, g, first, n_parts)
    if (error /= 0) return
  endif
  error = 0
  group = self%prefix(group)//group
  if (.not.h5_link_exists(self%root, group)) return
  if (h5_group_names(self%root, group, list) /= 0_I4P) then
    error = 6
    return
  endif
  j = 0
  do i=1, len(list)
    if (list(i:i) /= achar(0)) cycle
    if (h5_dataset_open(self%root, group//'/'//list(j+1:i-1), dset) == 0_I4P) then
      if (h5_attribute_exists(dset, 'Attribute')) then
        if (h5_attribute_read(dset, 'Attribute', text) == 0_I4P) then
          if (active_role(text) == active_role(role)) data_name = list(j+1:i-1)
        endif
      endif
      e = h5_dataset_close(dset)
    endif
    if (len(data_name) > 0) return
    j = i
  enddo
  endfunction get_active

  function get_dataarray_names(self, location, names, piece) result(error)
  !< Return the names of the dataarrays of a location (node, cell, field, or row: the columns of a Table), in the order
  !< they were written (their names' order if the file does not track it), blank padded to the longest one.
  class(vtkhdf_reader),          intent(in)           :: self     !< Reader.
  character(*),                  intent(in)           :: location !< Location: node, cell, field or row (Table).
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
  if (group /= 'FieldData' .and. group /= 'RowData' .and. &
      .not.(self%is_grid() .or. self%mesh_topology == 'Table' .or. self%mesh_topology == 'OverlappingAMR')) then
    error = self%part(piece, g, first, n_parts)
    if (error /= 0) return
  endif
  error = 0
  group = self%prefix(group)//group
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
  character(*),                  intent(in)            :: location     !< Location: node, cell, field or row (Table).
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
  character(*),                  intent(in)           :: location     !< Location: node, cell, field or row (Table).
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
  if (self%mesh_topology == 'RectilinearGrid' .or. self%mesh_topology == 'HyperTreeGrid') then ! coordinates per axis
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
  if (self%mesh_topology == 'RectilinearGrid' .or. self%mesh_topology == 'HyperTreeGrid') then ! coordinates per axis
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
      error = polyhedra_stream(offsets, polyhedra, face_offsets, faces, data_name == 'faceoffsets', ids)
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
  function get_trees(self, tree_ids, branch_factor) result(error)
  !< Return the trees of a HyperTreeGrid, of the step read: the ids of their root cells (i + (nx-1)(j + (ny-1)k), from 0),
  !< in the order of the file (that of their cells in the cell data), and its branch factor. Error 4 for another dataset.
  class(vtkhdf_reader),      intent(in)            :: self          !< Reader.
  integer(I8P), allocatable, intent(out)           :: tree_ids(:)   !< Ids of the trees.
  integer(I4P),              intent(out), optional :: branch_factor !< Branch factor: 2 or 3.
  integer(I4P)                                     :: error         !< Error status.
  integer(I8P), allocatable                        :: depths(:), counts(:), ivalues(:) !< Layout of the trees.
  integer(I8P)                                     :: first_bit, first_mask !< Bits of the step (unused here).

  allocate(tree_ids(0))
  error = self%tree_layout(tree_ids, depths, counts, first_bit, first_mask)
  if (error /= 0 .or. .not.present(branch_factor)) return
  error = 6
  if (h5_attribute_read(self%root, 'BranchFactor', ivalues) /= 0_I4P) return
  if (size(ivalues) < 1) return
  branch_factor = int(ivalues(1), I4P)
  error = 0
  endfunction get_trees

  function read_tree(self, tree_id, descriptor, mask) result(error)
  !< Read the tree `tree_id` of a HyperTreeGrid (of the step read), as `write_tree` takes it: `descriptor` the refinement
  !< of its cells breadth first, every depth but the deepest (empty for a tree of one leaf); `mask` one value per cell,
  !< breadth first (all false when the file has no mask). The bits are unpacked from `Descriptors` and `Mask` (MSB
  !< first). Error 4 for another dataset or a tree not in the file (a root cell without tree), 6 for an inconsistent file.
  class(vtkhdf_reader), intent(in)            :: self          !< Reader.
  integer(I8P),         intent(in)            :: tree_id       !< Id of the tree (its root cell).
  logical, allocatable, intent(out)           :: descriptor(:) !< Refinement of its cells.
  logical, allocatable, intent(out), optional :: mask(:)       !< Masked cells.
  integer(I4P)                                :: error         !< Error status.
  integer(I8P), allocatable                   :: tree_ids(:), depths(:), counts(:) !< Layout of the trees.
  integer(I8P)                                :: first_bit     !< First byte of the descriptors of the step.
  integer(I8P)                                :: first_mask    !< First byte of the mask of the step.
  integer(I8P)                                :: bits_before   !< Descriptor bits of the trees before.
  integer(I8P)                                :: cells_before  !< Cells of the trees before.
  integer(I8P)                                :: c0            !< First entry of the tree in counts.
  integer(I8P)                                :: n_bits, n_cells !< Descriptor bits and cells of the tree.
  integer(I8P)                                :: t, u          !< Counters.

  allocate(descriptor(0))
  if (present(mask)) allocate(mask(0))
  error = self%tree_layout(tree_ids, depths, counts, first_bit, first_mask) ; if (error /= 0) return
  error = 4
  t = findloc(tree_ids, tree_id, dim=1, kind=I8P)
  if (t == 0_I8P) return
  error = 6
  bits_before = 0_I8P ; cells_before = 0_I8P ; c0 = 0_I8P
  do u=1_I8P, t - 1_I8P
    bits_before = bits_before + sum(counts(c0 + 1_I8P:c0 + depths(u) - 1_I8P))
    cells_before = cells_before + sum(counts(c0 + 1_I8P:c0 + depths(u)))
    c0 = c0 + depths(u)
  enddo
  if (depths(t) < 1_I8P .or. c0 + depths(t) > size(counts, kind=I8P)) return
  n_bits = sum(counts(c0 + 1_I8P:c0 + depths(t) - 1_I8P))
  n_cells = sum(counts(c0 + 1_I8P:c0 + depths(t)))
  error = read_bits(self%root, 'Descriptors', first_bit, bits_before, n_bits, descriptor) ; if (error /= 0) return
  if (.not.present(mask)) return
  if (h5_link_exists(self%root, 'Mask')) then
    error = read_bits(self%root, 'Mask', first_mask, cells_before, n_cells, mask)
  else
    deallocate(mask)
    allocate(mask(n_cells))
    mask = .false.
  endif
  endfunction read_tree

  function tree_layout(self, tree_ids, depths, counts, first_bit, first_mask) result(error)
  !< Where the trees of a HyperTreeGrid of the step read are: their ids, depths, cells of each depth (tree after tree),
  !< the first byte of their descriptors and of their mask (a time series: each step from a new byte).
  class(vtkhdf_reader),      intent(in)  :: self        !< Reader.
  integer(I8P), allocatable, intent(out) :: tree_ids(:) !< Ids of the trees.
  integer(I8P), allocatable, intent(out) :: depths(:)   !< Depths of each tree.
  integer(I8P), allocatable, intent(out) :: counts(:)   !< Cells of each depth of each tree.
  integer(I8P),              intent(out) :: first_bit   !< First byte of the descriptors.
  integer(I8P),              intent(out) :: first_mask  !< First byte of the mask.
  integer(I4P)                           :: error       !< Error status.
  integer(I8P)                           :: n_trees, n_depths, first_tree, first_count !< Counts, starts of the step.

  allocate(tree_ids(0), depths(0), counts(0))
  first_bit = 0_I8P ; first_mask = 0_I8P
  error = 4
  if (.not.self%is_initialized) return
  if (self%mesh_topology /= 'HyperTreeGrid') return
  n_trees = self%step_entry('NumberOfTrees', error) ; if (error /= 0) return
  n_depths = self%step_entry('NumberOfDepths', error) ; if (error /= 0) return
  first_tree = 0_I8P ; first_count = 0_I8P
  if (self%n_steps > 0_I8P) then
    first_tree = self%step_value('TreeIdsOffsets', 1_I4P, error) ; if (error /= 0) return
    first_count = self%step_value('NumberOfCellsPerTreeDepthOffsets', 1_I4P, error) ; if (error /= 0) return
    first_bit = self%step_value('DescriptorsOffsets', 1_I4P, error) ; if (error /= 0) return
    if (h5_link_exists(self%root, 'Steps/MaskOffsets')) then
      first_mask = self%step_value('MaskOffsets', 1_I4P, error) ; if (error /= 0) return
    endif
  endif
  error = read_I8P(self%root, 'TreeIds', first_tree, n_trees, tree_ids) ; if (error /= 0) return
  error = read_I8P(self%root, 'DepthPerTree', first_tree, n_trees, depths) ; if (error /= 0) return
  error = read_I8P(self%root, 'NumberOfCellsPerTreeDepth', first_count, n_depths, counts) ; if (error /= 0) return
  error = 6
  if (size(tree_ids) /= n_trees .or. size(depths) /= n_trees .or. size(counts) /= n_depths) return
  if (any(depths < 1_I8P) .or. sum(depths) /= n_depths .or. any(counts < 1_I8P)) return
  error = 0
  endfunction tree_layout

  function step_entry(self, name, error) result(value)
  !< The entry of the step read of a dataset with one entry per step (one for a dataset that is not a time series):
  !< `NumberOfTrees`, `NumberOfCells`, ... of a HyperTreeGrid.
  class(vtkhdf_reader), intent(in)  :: self   !< Reader.
  character(*),         intent(in)  :: name   !< Dataset.
  integer(I4P),         intent(out) :: error  !< Error status.
  integer(I8P)                      :: value  !< Entry.
  integer(I8P), allocatable         :: values(:) !< Values.

  value = 0_I8P
  error = 6
  if (h5_dataset_read(self%root, name, values) /= 0_I4P) return
  if (size(values, kind=I8P) < max(self%step, 1_I8P)) return
  value = values(max(self%step, 1_I8P))
  if (value < 0_I8P) return
  error = 0
  endfunction step_entry

  pure function grid_points(self) result(grid)
  !< Points of the grid along each axis.
  class(vtkhdf_reader), intent(in) :: self    !< Reader.
  integer(I8P)                     :: grid(3) !< Points along x, y, z.

  grid = int(self%whole_extent(2::2) - self%whole_extent(1::2), I8P) + 1_I8P
  endfunction grid_points

  function amr_box(self, piece, extent, starts, n_boxes) result(error)
  !< The box `piece` (from 1, default 1) of the level and step read of an OverlappingAMR: its point extent (the cells of
  !< its `AMRBox` row, `imin, imax, ...`, are the points `imin, imax+1, ...`), the points and cells of the boxes of the
  !< level and step before it (where its data start), and the boxes of the level in the step.
  class(vtkhdf_reader), intent(in)           :: self      !< Reader.
  integer(I4P),         intent(in), optional :: piece     !< Box, from 1 (default 1).
  integer(I4P),         intent(out)          :: extent(6) !< Point extent of the box.
  integer(I8P),         intent(out)          :: starts(2) !< Points and cells of the boxes before it.
  integer(I8P),         intent(out)          :: n_boxes   !< Boxes of the level in the step.
  integer(I4P)                               :: error     !< Error status.
  integer(I8P), allocatable                  :: boxes(:)  !< AMRBox, 6 values per box.
  character(len=:), allocatable              :: level     !< Group of the level.
  integer(I8P)                               :: first     !< Boxes of the level before the step.
  integer(I8P)                               :: p         !< Box, from 1.
  integer(I8P)                               :: q         !< Counter.
  integer(I8P)                               :: cells(3)  !< Cells of a box along each axis.

  extent = 0 ; starts = 0_I8P ; n_boxes = 0_I8P
  error = 6
  level = 'Level'//trim(str(n=self%level, no_sign=.true.))
  if (h5_dataset_read(self%root, level//'/AMRBox', boxes) /= 0_I4P) return
  if (mod(size(boxes, kind=I8P), 6_I8P) /= 0_I8P) return
  first = 0_I8P
  if (self%n_steps > 0_I8P) then ! the names of VTKHDF 2.3 (plural), singular before, as VTK reads them
    first = self%step_value(level//'/AMRBoxOffset'//trim(merge('s', ' ', self%is_v23())), 1_I4P, error)
    if (error /= 0) return
    n_boxes = self%step_value(level//'/NumberOfAMRBox'//trim(merge('es', '  ', self%is_v23())), 1_I4P, error)
    if (error /= 0) then
      n_boxes = 0_I8P
      return
    endif
    error = 6
    if (first < 0_I8P .or. n_boxes < 0_I8P .or. 6_I8P * (first + n_boxes) > size(boxes, kind=I8P)) then
      n_boxes = 0_I8P
      return
    endif
  else
    n_boxes = size(boxes, kind=I8P) / 6_I8P
  endif
  p = 1_I8P
  if (present(piece)) p = int(piece, I8P)
  error = 4
  if (p < 1_I8P .or. p > n_boxes) return
  error = 6
  do q=first + 1_I8P, first + p
    cells = boxes(6_I8P * q - 4_I8P:6_I8P * q:2) - boxes(6_I8P * q - 5_I8P:6_I8P * q - 1_I8P:2) + 1_I8P
    if (any(cells < 1_I8P)) return ! a malformed box: at least one cell along every axis
    if (q < first + p) starts = starts + [product(cells + 1_I8P), product(cells)]
  enddo
  q = first + p
  extent(1::2) = int(boxes(6_I8P * q - 5_I8P:6_I8P * q - 1_I8P:2), I4P)
  extent(2::2) = int(boxes(6_I8P * q - 4_I8P:6_I8P * q:2), I4P) + 1
  error = 0
  endfunction amr_box

  pure function prefix(self, group) result(path)
  !< The path of the data groups of the level read of an OverlappingAMR (`Level<n>/`, for PointData and CellData), empty
  !< for the other datasets.
  class(vtkhdf_reader), intent(in) :: self  !< Reader.
  character(*),         intent(in) :: group !< Data group.
  character(len=:), allocatable    :: path  !< Path of the level, with the separator.

  path = ''
  if (.not.allocated(self%mesh_topology)) return
  if (self%mesh_topology == 'OverlappingAMR' .and. (group == 'PointData' .or. group == 'CellData')) &
    path = 'Level'//trim(str(n=self%level, no_sign=.true.))//'/'
  endfunction prefix

  elemental function is_v23(self) result(is_v23_)
  !< Return true if the file is VTKHDF 2.3 or later: the OverlappingAMR Steps names are plural (`AMRBoxOffsets`,
  !< `NumberOfAMRBoxes`, `PointDataOffsets`, `CellDataOffsets`), singular before, as VTK 9.7.2 reads them.
  class(vtkhdf_reader), intent(in) :: self    !< Reader.
  logical                          :: is_v23_ !< Check result.

  is_v23_ = self%version(1) > 2_I8P .or. (self%version(1) == 2_I8P .and. self%version(2) >= 3_I8P)
  endfunction is_v23

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
  !< (tuples of a partition, a time slice of a grid, the tuples of a step of field data, the rows of a step of a Table
  !< column), its type, components, tuples. A column of a time series needs its `Steps/RowDataOffsets` entry (as VTK).
  class(vtkhdf_reader),          intent(in)           :: self         !< Reader.
  character(*),                  intent(in)           :: location     !< Location: node, cell, field or row (Table).
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
  integer(I8P), allocatable                           :: counts(:)    !< Rows of the steps of a Table.
  integer(I4P)                                        :: ext(6)       !< Extent of a box (AMR).
  integer(I8P)                                        :: box_starts(2) !< Points, cells before a box (AMR).
  integer(I8P)                                        :: n_boxes      !< Boxes of the level read (AMR).
  integer(I8P)                                        :: g, starts(9), n_parts, b !< Partition, starts, parts, counter.
  integer(I8P)                                        :: grid(3)      !< Points of a grid along each axis.
  integer(I4P)                                        :: n_axes       !< Axes of the grid in the arrays.

  path = '' ; data_type = '' ; first = 0_I8P ; count = 0_I8P ; n_components = 1 ; n_tuples = 0_I8P
  error = 4
  if (.not.self%is_initialized) return
  group = location_group(location)
  if (len(group) == 0) return
  path = self%prefix(group)//group//'/'//trim(data_name)
  if (.not.h5_link_exists(self%root, self%prefix(group)//group)) return
  if (.not.h5_link_exists(self%root, path)) return
  error = 6
  if (h5_dataset_info(self%root, path, data_type, dims) /= 0_I4P) return
  if (len(data_type) == 0 .or. size(dims) == 0) then
    error = 5
    return
  endif
  if (self%mesh_topology == 'OverlappingAMR' .and. group /= 'FieldData') then ! a box of the level and step read
    error = self%amr_box(piece, ext, box_starts, n_boxes) ; if (error /= 0) return
    error = 6
    if (size(dims) > 1) n_components = int(dims(1), I4P)
    if (group == 'PointData') then
      count = product(int(ext(2::2) - ext(1::2), I8P) + 1_I8P)
      first = box_starts(1)
    else
      count = product(int(ext(2::2) - ext(1::2), I8P))
      first = box_starts(2)
    endif
    if (self%n_steps > 0_I8P) then ! where the arrays of the step start
      b = first
      first = self%step_value(self%prefix(group)//trim(merge('PointDataOffset', 'CellDataOffset ', group == 'PointData'))// &
                              trim(merge('s', ' ', self%is_v23()))//'/'//trim(data_name), 1_I4P, error)
      if (error /= 0) return
      first = first + b
    endif
    n_tuples = count
  elseif (self%mesh_topology == 'HyperTreeGrid' .and. group /= 'FieldData') then ! the cells of the trees of the step
    error = 4
    if (group /= 'CellData') return ! a HyperTreeGrid has cell data only
    if (present(piece)) then
      if (piece /= 1) return
    endif
    if (size(dims) > 1) n_components = int(dims(1), I4P)
    count = self%step_entry('NumberOfCells', error) ; if (error /= 0) return
    if (self%n_steps > 0_I8P) then
      first = self%step_value('CellDataOffsets/'//trim(data_name), 1_I4P, error) ; if (error /= 0) return
    endif
    n_tuples = count
  elseif (group == 'RowData') then ! a column of a Table: the rows of the step read (NumberOfRows, one entry per step)
    if (size(dims) > 1) n_components = int(product(dims(1:size(dims)-1)), I4P)
    if (h5_dataset_read(self%root, 'NumberOfRows', counts) /= 0_I4P) return
    if (size(counts, kind=I8P) < max(self%step, 1_I8P)) return
    count = counts(self%step)
    if (self%n_steps > 0_I8P) then
      if (.not.h5_link_exists(self%root, 'Steps/RowDataOffsets/'//trim(data_name))) return
      first = self%step_value('RowDataOffsets/'//trim(data_name), 1_I4P, error) ; if (error /= 0) return
    endif
    n_tuples = count
  elseif (group == 'FieldData') then
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
    if (self%n_steps > 0_I8P) then ! the time axis last: one slice, the one of the step or of the array's offset
      first = self%step - 1_I8P
      count = 1_I8P
      if (h5_link_exists(self%root, 'Steps/'//group//'Offsets/'//trim(data_name))) then
        first = self%step_value(group//'Offsets/'//trim(data_name), 1_I4P, error) ; if (error /= 0) return
      endif
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

  function read_bits(root, name, first_byte, skip, n, bits) result(error)
  !< Read `n` bits of the dataset of packed bits `name` (bytes, MSB first), after `skip` bits from the byte `first_byte`.
  integer(hid_kind),    intent(in)  :: root       !< Group /VTKHDF.
  character(*),         intent(in)  :: name       !< Dataset.
  integer(I8P),         intent(in)  :: first_byte !< First byte.
  integer(I8P),         intent(in)  :: skip       !< Bits skipped from it.
  integer(I8P),         intent(in)  :: n          !< Bits read.
  logical, allocatable, intent(out) :: bits(:)    !< Bits.
  integer(I4P)                      :: error      !< Error status.
  integer(I1P), allocatable         :: bytes(:)   !< Bytes read.
  integer(I8P)                      :: b0, i      !< First byte read, counter.

  allocate(bits(n))
  error = 0
  if (n == 0_I8P) return
  error = 6
  b0 = first_byte + skip / 8_I8P
  if (h5_dataset_read_bytes(root, name, b0, (skip + n + 7_I8P) / 8_I8P - skip / 8_I8P, bytes) /= 0_I4P) return
  do i=0_I8P, n - 1_I8P ! bit skip+i of the step, mod 8 counted from the most significant bit of its byte
    bits(i + 1_I8P) = btest(bytes((mod(skip, 8_I8P) + i) / 8_I8P + 1_I8P), 7 - int(mod(mod(skip, 8_I8P) + i, 8_I8P)))
  enddo
  error = 0
  endfunction read_bits

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
  !< The group of a location: node PointData, cell CellData, field FieldData, row RowData (case insensitive), empty
  !< otherwise.
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
  case('row')
    group = 'RowData'
  case default
    group = ''
  endselect
  endfunction location_group

  function polyhedra_stream(cell_offsets, cell_faces, face_offsets, faces, want_offsets, ids) result(error)
  !< The face stream of the polyhedra of a piece (`write_connectivity`'s `face`) or, `want_offsets`, the end of the faces
  !< of each cell in it (`faceoffset`, -1 for a cell that is not a polyhedron), from the four arrays of
  !< `vtkPolyhedronCellArray` (VTKHDF `PolyhedronOffsets`, `PolyhedronToFaces`, `FaceOffsets`, `FaceConnectivity`; XML
  !< 2.3+ `polyhedron_offsets`, `polyhedron_to_faces`, `face_offsets`, `face_connectivity`).
  !<
  !< The offsets are given with their start (n+1 entries, any origin). A face shared by two polyhedra (VTKHDF 2.9, XML
  !< 2.5: stored once, `-(n+1)` for the face `n` traversed in reverse) is given in the stream of each polyhedron, its points
  !< reversed where marked. Offsets not increasing or out of their arrays, or a face id out of the faces, are error 6.
  integer(I8P),              intent(in)  :: cell_offsets(:) !< Start of the faces of each cell, then their end.
  integer(I8P),              intent(in)  :: cell_faces(:)   !< Face ids of the cells, from 0; -(n+1) reversed.
  integer(I8P),              intent(in)  :: face_offsets(:) !< Start of the points of each face, then their end.
  integer(I8P),              intent(in)  :: faces(:)        !< Point ids of the faces.
  logical,                   intent(in)  :: want_offsets    !< Return the end of the faces of each cell, not the stream.
  integer(I8P), allocatable, intent(out) :: ids(:)          !< Face stream, or end of the faces of each cell.
  integer(I4P)                           :: error           !< Error status.
  integer(I8P), allocatable              :: offsets(:), first(:), polyhedra(:) !< From 0; face starts; face ids from 1.
  logical,      allocatable              :: reversed(:)     !< The face of a polyhedron is traversed in reverse.
  integer(I8P)                           :: nc, nf, n, b, c, f, i, k, m !< Counts, counters.

  allocate(ids(0))
  error = 6
  nc = size(cell_offsets, kind=I8P) - 1_I8P
  nf = size(face_offsets, kind=I8P) - 1_I8P
  if (nc < 0_I8P .or. nf < 0_I8P) return
  offsets = cell_offsets - cell_offsets(1)
  first = face_offsets - face_offsets(1)
  if (any(offsets(2:) < offsets(:nc)) .or. offsets(nc+1) > size(cell_faces, kind=I8P)) return
  if (any(first(2:) < first(:nf)) .or. first(nf+1) > size(faces, kind=I8P)) return
  polyhedra = cell_faces
  reversed = polyhedra < 0_I8P
  where (reversed) polyhedra = -polyhedra - 1_I8P
  polyhedra = polyhedra + 1_I8P
  if (any(polyhedra < 1_I8P .or. polyhedra > nf)) return
  ! the stream: for each polyhedron its faces, each one its points; the end of each cell in it, -1 if not a polyhedron
  n = 0_I8P
  do c=1_I8P, nc
    if (offsets(c+1) > offsets(c)) n = n + 1_I8P + sum(first(polyhedra(offsets(c)+1:offsets(c+1))+1) - &
                                                       first(polyhedra(offsets(c)+1:offsets(c+1))) + 1_I8P)
  enddo
  deallocate(ids)
  if (want_offsets) then
    allocate(ids(nc))
  else
    allocate(ids(n))
  endif
  i = 0_I8P
  do c=1_I8P, nc
    k = offsets(c+1) - offsets(c)
    if (k == 0_I8P) then
      if (want_offsets) ids(c) = -1_I8P
      cycle
    endif
    if (.not.want_offsets) ids(i+1) = k
    i = i + 1_I8P
    do f=offsets(c)+1_I8P, offsets(c+1)
      b = polyhedra(f) ! the face, from 1
      m = first(b+1) - first(b)
      if (.not.want_offsets) then
        ids(i+1) = m
        if (reversed(f)) then ! its points in reverse order
          ids(i+2:i+1+m) = faces(first(b+1):first(b)+1:-1)
        else
          ids(i+2:i+1+m) = faces(first(b)+1:first(b+1))
        endif
      endif
      i = i + 1_I8P + m
    enddo
    if (want_offsets) ids(c) = i
  enddo
  error = 0
  endfunction polyhedra_stream

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
