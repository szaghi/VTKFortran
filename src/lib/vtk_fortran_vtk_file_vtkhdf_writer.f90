!< VTK file VTKHDF writer: an HDF5 file in the VTKHDF layout (every topology of `vtk_file`).
module vtk_fortran_vtk_file_vtkhdf_writer
!< VTK file VTKHDF writer: an HDF5 file in the VTKHDF layout (every topology of `vtk_file`).
!<
!< The writer of `vtk_file%initialize(format='VTKHDF', ...)`: the same calls as the XML formats write the group `/VTKHDF`
!< of an HDF5 file (VTKHDF specification 2.x, read by VTK >= 9.1 and ParaView): the attributes `Version`, `Type`,
!< `WholeExtent`, `Origin`, `Spacing`, `Direction`, the groups `PointData`, `CellData` and `FieldData`, one dataset per
!< data array. A Fortran array `x(n_components, i, j, k)` is stored as it is in memory: it is the VTKHDF dataset of shape
!< `[k, j, i, n_components]` (C order), so nothing is transposed. With `compressor='zlib'` the datasets are deflated.
!<
!< Supported: ImageData, RectilinearGrid and StructuredGrid, one piece each (its extent is the whole extent; the
!< coordinates of a RectilinearGrid are `XCoordinates`, `YCoordinates`, `ZCoordinates`, the points of a StructuredGrid the
!< dataset `Points` of shape `[k, j, i, 3]`, both with the attribute `Dimensions`, VTKHDF 2.7); UnstructuredGrid, each
!< piece a partition (its counts in `NumberOfPoints`, `NumberOfCells`, `NumberOfConnectivityIds`, its points, cells and
!< arrays appended to datasets shared by the pieces; the offsets written from 0, VTKFortran's end offsets converted), ids
!< and counts I4P or I8P; polyhedra (`face`, `faceoffset` of `write_connectivity`) as `FaceConnectivity`, `FaceOffsets`,
!< `PolyhedronToFaces`, `PolyhedronOffsets` with their counts per piece (VTKHDF 2.5; zero entries for the pieces without
!< polyhedra, the earlier ones written when the first polyhedra come);
!< PolyData, each piece a partition as well, its cells in the four groups `Vertices`, `Lines`, `Strips`, `Polygons` (each
!< with its own counts, connectivity and offsets; a block not given has no cells, and its group gets zero entries);
!< data arrays of every kind and layout, unsigned arrays, active arrays (`Attribute`, VTKHDF 2.6), numeric field data.
!< Refused (non-zero error, message on stderr): a volatile file, string field
!< data (the VTK reader crashes on them), zlib without the HDF5 deflate filter, an array missing from a piece or new
!< after the first one, and every call when the library is built without HDF5 (`VTKFORTRAN_USE_HDF5`).
!<
!< Time series (VTKHDF `Steps`): a file whose first output starts with `write_step(time)` is one; each output then starts
!< with `write_step`, its pieces appended as for a single dataset (the grids get a time axis, their arrays `[t, k, j, i]`),
!< and the `Steps` entries of the open step are rewritten at the end of every piece (and of every data group of a step
!< without pieces), with `NSteps` and `Version`, and the file flushed: the file is valid after every piece.
!<
!< What does not change is stored once, the `Steps` offsets of a step repeating those of the step before:
!<- **the mesh**: a step after the first one without pieces (UnstructuredGrid, PolyData: `write_step`, then its data
!<  groups opened directly, `write_dataarray(location=..., action='open')`) reuses all the parts of the step before; a
!<  RectilinearGrid or StructuredGrid piece without `write_geo` reuses the coordinates or points of the step before (an
!<  ImageData has no geometry to store);
!<- **an array**: an array not written in a step reuses its values of the step before; that is possible in a step of a
!<  grid and in a step without pieces; a step with pieces writes every point and cell array (its parts may differ from
!<  those of the arrays' step). The arrays of a step without pieces hold the points (cells) of all the parts reused, in
!<  their order. A field data array not written in a step reuses its values too.
!< The first step writes everything; an array first written at a later step is refused. The offsets of the point and cell
!< data are written for every array (`PointDataOffsets`, `CellDataOffsets`): tuples, time slices for a grid.
!<
!< From MPI ranks (`set_comm`): an UnstructuredGrid or PolyData piece per rank (a partition, appended after the ranks
!< before), or a region per rank of one ImageData, RectilinearGrid or StructuredGrid (`write_piece(nx1, ..., nz2)` with
!< the rank's sub-extent): the datasets are created with the whole extent, by every rank, and each rank writes the block
!< of its region, the points of a boundary shared with the next region excluded (that region writes them), the
!< coordinates of a RectilinearGrid entries no rank before has. The regions are checked when they start (gathered on
!< every rank): inside the whole extent, cells not overlapping, covering it; an empty region (`nx2 < nx1`, ...) writes
!< nothing. A refusal by one rank of its own values (their size, say) is agreed by all the ranks (`agree`) before the
!< collective calls: every rank returns the error, none is left waiting.
!<
!< `reopen` (`vtk_file%initialize(..., action='append')`) continues a time series written before (a restart): the state is
!< read from the file, and every dataset is trimmed to the pieces recorded by the last complete piece (a run interrupted
!< in a piece leaves the data of that piece, dropped).
!<
!< `Version` is written by `finalize` (and after every piece of a time series): 2.0, 2.5 with polyhedra, 2.6 when an
!< active array is marked, 2.7 for a RectilinearGrid or a StructuredGrid.
use penf
use vtk_fortran_hdf5
use vtk_fortran_parameters, only : stderr
use vtk_fortran_vtk_writer_abstract

implicit none
private
public :: vtkhdf_writer

character(*), parameter :: ROLES(5) = ['Scalars', 'Vectors', 'Normals', 'Tensors', 'TCoords'] !< Roles of active arrays.
integer(I4P), parameter :: DEFLATE_LEVEL_ZLIB = 6_I4P !< Deflate level of compressed datasets (the zlib default).
integer(I4P), parameter :: TOPOLOGY_IMAGE = 1_I4P        !< ImageData.
integer(I4P), parameter :: TOPOLOGY_UNSTRUCTURED = 2_I4P !< UnstructuredGrid.
integer(I4P), parameter :: TOPOLOGY_POLY = 3_I4P         !< PolyData.
integer(I4P), parameter :: TOPOLOGY_RECT = 4_I4P         !< RectilinearGrid.
integer(I4P), parameter :: TOPOLOGY_STRG = 5_I4P         !< StructuredGrid.
integer(I1P), parameter :: CELL_POLYHEDRON = 42_I1P     !< VTK type of a polyhedron.
character(*), parameter :: STEP_GROUPS(4) = ['PointDataOffsets', 'CellDataOffsets ', 'FieldDataOffsets', & !< Groups of
                                              'FieldDataSizes  ']                                       !< Steps.
character(*), parameter :: BLOCKS(4) = ['Vertices', 'Lines   ', 'Strips  ', 'Polygons'] !< Groups of the PolyData cells.
integer(I8P), parameter :: VTK_COLUMN(4) = [1_I8P, 2_I8P, 4_I8P, 3_I8P] !< Column of each block in the rows of Steps.
character(*), parameter :: NAMES_SEPARATOR = achar(0)    !< Separator of the names of a list (a name has no NUL).

type :: active_array
  !< Name of the active array of a role.
  character(len=:), allocatable :: name !< Array name.
endtype active_array

type, extends(vtk_writer_abstract) :: vtkhdf_writer
  !< VTKHDF writer (every topology of `vtk_file`).
  private
  integer(hid_kind)             :: file_id=-1_hid_kind     !< File.
  integer(hid_kind)             :: root=-1_hid_kind        !< Group /VTKHDF.
  integer(hid_kind)             :: group=-1_hid_kind       !< Open data group (PointData, CellData, FieldData).
  character(len=:), allocatable :: group_name              !< Name of the open data group, empty if none.
  integer(I4P)                  :: whole_extent(6)=0_I4P   !< Whole extent: nx1, nx2, ny1, ny2, nz1, nz2.
  integer(I4P)                  :: n_pieces=0_I4P          !< Pieces started.
  logical                       :: is_piece_open=.false.   !< A piece is open.
  integer(I8P)                  :: version_minor=0_I8P     !< Minor version needed by the features written.
  type(active_array)            :: active(5)               !< Active arrays of the open group, one per role.
  integer(I4P)                  :: topology=0_I4P          !< Topology: one of the TOPOLOGY_* constants.
  integer(I8P)                  :: np_piece=0_I8P          !< Points of the open piece (UnstructuredGrid, PolyData).
  integer(I8P)                  :: nc_piece=0_I8P          !< Cells of the open piece (UnstructuredGrid, PolyData).
  integer(I8P)                  :: block_cells(4)=0_I8P    !< Cells of each block of the open piece (PolyData).
  integer(I8P)                  :: total_points=0_I8P      !< Points of the pieces started (UnstructuredGrid, PolyData).
  integer(I8P)                  :: total_cells=0_I8P       !< Cells of the pieces started (UnstructuredGrid, PolyData).
  logical                       :: is_geo_written=.false.  !< The points of the open piece are written.
  logical                       :: is_connectivity_written=.false. !< The cells of the open piece are written.
  logical                       :: has_polyhedra=.false.   !< The file has polyhedra (UnstructuredGrid).
  logical                       :: is_faces_written=.false. !< The faces of the polyhedra of the open piece are written.
  logical                       :: is_temporal=.false.     !< The file is a time series (`write_step`).
  integer(I8P)                  :: n_steps=0_I8P           !< Steps started.
  real(R8P)                     :: step_time=0._R8P        !< Time of the open step.
  integer(I8P)                  :: step_pieces=0_I8P       !< Pieces of the open step.
  integer(I8P)                  :: step_start(9)=0_I8P     !< Totals at the start of the open step: parts, points, cells,
                                                           !< connectivity ids, faces' point ids, faces, polyhedra's faces.
  integer(I8P)                  :: step_blocks(8)=0_I8P    !< PolyData totals at the start of the step: cells, ids per block.
  integer(I8P)                  :: totals(9)=0_I8P         !< Running totals, as `step_start`.
  integer(I8P)                  :: block_totals(8)=0_I8P   !< Running PolyData totals: cells of each block, then ids.
  integer(I8P)                  :: mesh_start(9)=0_I8P     !< Entries of the last step with pieces (the mesh that a step
                                                           !< without pieces reuses), as `step_start`.
  integer(I8P)                  :: mesh_blocks(8)=0_I8P    !< PolyData entries of that step, as `step_blocks`.
  integer(I8P)                  :: mesh_pieces=0_I8P       !< Parts of that step.
  integer(I8P)                  :: geo_slices=0_I8P        !< Steps that wrote the geometry of a RectilinearGrid or
                                                           !< StructuredGrid series (its time slices).
  integer(I8P), allocatable     :: point_start(:)          !< Tuples (slices for a grid) of each point array at the start
                                                           !< of the open step, in the order of `point_arrays`.
  integer(I8P), allocatable     :: point_prev(:)           !< Offset of each point array at the step before.
  integer(I8P), allocatable     :: cell_start(:)           !< As `point_start`, for the cell arrays.
  integer(I8P), allocatable     :: cell_prev(:)            !< As `point_prev`, for the cell arrays.
  character(len=:), allocatable :: field_arrays            !< Names of the field data arrays of a time series.
  logical                       :: is_reopened=.false.     !< A time series reopened (`reopen`): the extent start unknown.
  character(len=:), allocatable :: point_arrays            !< Names of the point data arrays, NUL terminated.
  character(len=:), allocatable :: cell_arrays             !< Names of the cell data arrays, NUL terminated.
  logical                       :: is_parallel=.false.     !< The file is written by the ranks of an MPI communicator.
  integer(I4P)                  :: region(6)=0_I4P         !< Region of this rank of a grid written from ranks: its extent.
  integer(I4P)                  :: rank=0_I4P              !< Rank of this process in `comm`.
  integer(I8P), allocatable     :: regions(:,:)            !< Regions of all the ranks, `regions(:, r)` of rank r-1.
  integer(I4P)                  :: comm=0_I4P              !< MPI communicator (Fortran handle), if `is_parallel`.
  contains
    procedure, pass(self) :: initialize
    procedure, pass(self) :: finalize
    procedure, pass(self) :: write_piece_start_tag
    procedure, pass(self) :: write_piece_end_tag
    procedure, pass(self) :: write_step
    procedure, pass(self) :: write_dataarray_location_tag
    procedure, pass(self) :: write_fielddata_tag
    procedure, pass(self) :: write_fielddata1_rank0
    procedure, pass(self) :: write_fielddata1_rank1
    procedure, pass(self) :: free
    procedure, pass(self) :: get_xml_volatile
    procedure, pass(self) :: write_dataarray1_rank1_R8P
    procedure, pass(self) :: write_dataarray1_rank1_R4P
    procedure, pass(self) :: write_dataarray1_rank1_I8P
    procedure, pass(self) :: write_dataarray1_rank1_I4P
    procedure, pass(self) :: write_dataarray1_rank1_I2P
    procedure, pass(self) :: write_dataarray1_rank1_I1P
    procedure, pass(self) :: write_dataarray1_rank2_R8P
    procedure, pass(self) :: write_dataarray1_rank2_R4P
    procedure, pass(self) :: write_dataarray1_rank2_I8P
    procedure, pass(self) :: write_dataarray1_rank2_I4P
    procedure, pass(self) :: write_dataarray1_rank2_I2P
    procedure, pass(self) :: write_dataarray1_rank2_I1P
    procedure, pass(self) :: write_dataarray1_rank3_R8P
    procedure, pass(self) :: write_dataarray1_rank3_R4P
    procedure, pass(self) :: write_dataarray1_rank3_I8P
    procedure, pass(self) :: write_dataarray1_rank3_I4P
    procedure, pass(self) :: write_dataarray1_rank3_I2P
    procedure, pass(self) :: write_dataarray1_rank3_I1P
    procedure, pass(self) :: write_dataarray1_rank4_R8P
    procedure, pass(self) :: write_dataarray1_rank4_R4P
    procedure, pass(self) :: write_dataarray1_rank4_I8P
    procedure, pass(self) :: write_dataarray1_rank4_I4P
    procedure, pass(self) :: write_dataarray1_rank4_I2P
    procedure, pass(self) :: write_dataarray1_rank4_I1P
    procedure, pass(self) :: write_dataarray3_rank1_R8P
    procedure, pass(self) :: write_dataarray3_rank1_R4P
    procedure, pass(self) :: write_dataarray3_rank1_I8P
    procedure, pass(self) :: write_dataarray3_rank1_I4P
    procedure, pass(self) :: write_dataarray3_rank1_I2P
    procedure, pass(self) :: write_dataarray3_rank1_I1P
    procedure, pass(self) :: write_dataarray3_rank3_R8P
    procedure, pass(self) :: write_dataarray3_rank3_R4P
    procedure, pass(self) :: write_dataarray3_rank3_I8P
    procedure, pass(self) :: write_dataarray3_rank3_I4P
    procedure, pass(self) :: write_dataarray3_rank3_I2P
    procedure, pass(self) :: write_dataarray3_rank3_I1P
    procedure, pass(self) :: write_dataarray6_rank1_R8P
    procedure, pass(self) :: write_dataarray6_rank1_R4P
    procedure, pass(self) :: write_dataarray6_rank1_I8P
    procedure, pass(self) :: write_dataarray6_rank1_I4P
    procedure, pass(self) :: write_dataarray6_rank1_I2P
    procedure, pass(self) :: write_dataarray6_rank1_I1P
    procedure, pass(self) :: write_dataarray6_rank3_R8P
    procedure, pass(self) :: write_dataarray6_rank3_R4P
    procedure, pass(self) :: write_dataarray6_rank3_I8P
    procedure, pass(self) :: write_dataarray6_rank3_I4P
    procedure, pass(self) :: write_dataarray6_rank3_I2P
    procedure, pass(self) :: write_dataarray6_rank3_I1P
    procedure, pass(self) :: write_dataarray_unsigned_I8P
    procedure, pass(self) :: write_dataarray_unsigned_I4P
    procedure, pass(self) :: write_dataarray_unsigned_I2P
    procedure, pass(self) :: write_dataarray_unsigned_I1P
    procedure, pass(self) :: write_geo_unst_data1_rank2_R8P
    procedure, pass(self) :: write_geo_unst_data3_rank1_R8P
    procedure, pass(self) :: write_geo_unst_data1_rank2_R8P_I8P
    procedure, pass(self) :: write_geo_unst_data3_rank1_R8P_I8P
    procedure, pass(self) :: write_geo_unst_data1_rank2_R4P
    procedure, pass(self) :: write_geo_unst_data3_rank1_R4P
    procedure, pass(self) :: write_geo_unst_data1_rank2_R4P_I8P
    procedure, pass(self) :: write_geo_unst_data3_rank1_R4P_I8P
    procedure, pass(self) :: write_connectivity_I4P
    procedure, pass(self) :: write_connectivity_I8P
    procedure, pass(self) :: write_piece_start_tag_unst
    procedure, pass(self) :: write_piece_start_tag_unst_I8P
    procedure, pass(self) :: write_piece_start_tag_poly
    procedure, pass(self) :: write_piece_start_tag_poly_I8P
    procedure, pass(self) :: write_polydata_cells
    procedure, pass(self) :: write_geo_rect_data3_rank1_R8P
    procedure, pass(self) :: write_geo_strg_data1_rank2_R8P
    procedure, pass(self) :: write_geo_strg_data1_rank4_R8P
    procedure, pass(self) :: write_geo_strg_data3_rank1_R8P
    procedure, pass(self) :: write_geo_strg_data3_rank3_R8P
    procedure, pass(self) :: write_geo_rect_data3_rank1_R4P
    procedure, pass(self) :: write_geo_strg_data1_rank2_R4P
    procedure, pass(self) :: write_geo_strg_data1_rank4_R4P
    procedure, pass(self) :: write_geo_strg_data3_rank1_R4P
    procedure, pass(self) :: write_geo_strg_data3_rank3_R4P
    ! private helpers
    procedure, pass(self)          :: reopen        !< Reopen a time series to append steps (`action='append'`).
    procedure, pass(self)          :: set_comm      !< Write the file from the ranks of an MPI communicator.
    procedure, pass(self), private :: check_arrays  !< Check the tuples written in the open step by each array.
    procedure, pass(self), private :: check_cells   !< Check a write_connectivity call of an UnstructuredGrid piece.
    procedure, pass(self), private :: check_geo     !< Check a write_geo call of an UnstructuredGrid or PolyData piece.
    procedure, pass(self), private :: close_group   !< Close the open data group.
    procedure, pass(self), private :: dataset_dims  !< Shape of the dataset of a data array.
    procedure, pass(self), private :: deflate_level !< Deflate level of the datasets.
    procedure, pass(self), private :: check_grid_geo !< Check a write_geo call of a RectilinearGrid or StructuredGrid.
    procedure, pass(self), private :: end_partition !< End a piece of an UnstructuredGrid or PolyData: check it.
    procedure, pass(self), private :: grid_points   !< Points of the grid along each axis (whole extent).
    procedure, pass(self), private :: is_partitioned !< The pieces are partitions (UnstructuredGrid, PolyData).
    procedure, pass(self), private :: mark_active   !< Mark a dataset as active array.
    procedure, pass(self), private :: open_group    !< Open (or create) a data group.
    procedure, pass(self), private :: no_faces      !< Write the zero entries of a piece without polyhedra.
    procedure, pass(self), private :: poly_block    !< Check or write a block of cells of a PolyData piece.
    procedure, pass(self), private :: poly_blocks   !< Check or write the four blocks of cells of a PolyData piece.
    procedure, pass(self), private :: end_reusing   !< Check the arrays of a step without pieces, write its entries.
    procedure, pass(self), private :: is_reusing    !< The open step has no piece: it reuses the mesh of the step before.
    procedure, pass(self), private :: is_written_in_step !< A point or cell array is written in the open step.
    procedure, pass(self), private :: record_array  !< Record the name of a point or cell data array.
    procedure, pass(self), private :: agree         !< With comm, the largest error of the ranks.
    procedure, pass(self), private :: axis_runs     !< The coordinates of an axis that this rank writes (RectilinearGrid).
    procedure, pass(self), private :: points_block  !< The block of the points of a StructuredGrid that this rank writes.
    procedure, pass(self), private :: region_block  !< The block of a grid array that this rank writes.
    procedure, pass(self), private :: start_region  !< Start the region of this rank of a grid written from ranks.
    procedure, pass(self), private :: roll_arrays   !< Start the bookkeeping of the arrays for a new step.
    procedure, pass(self), private :: reset         !< Reset the state of the writer.
    procedure, pass(self), private :: start_partition !< Start a piece of an UnstructuredGrid or PolyData.
    procedure, pass(self), private :: start_polyhedra !< Start the polyhedra of the file: zero entries for the earlier pieces.
    procedure, pass(self), private :: step_entry    !< Write the entry of the open step of a dataset of `Steps`.
    procedure, pass(self), private :: write_steps   !< Write the `Steps` entries of the open step, NSteps, Version; flush.
endtype vtkhdf_writer

contains
  function initialize(self, format, filename, mesh_topology, nx1, nx2, ny1, ny2, nz1, nz2, &
      is_volatile, mesh_kind) result(error)
  !< Initialize the writer: create the file and write the header of the topology.
  !<
  !< `format` is VTKHDF (not checked here), `mesh_topology` ImageData, RectilinearGrid or StructuredGrid, with the whole
  !< extent `nx1`...`nz2` (origin, spacing and direction of an ImageData are set by `vtk_file%initialize`),
  !< UnstructuredGrid or PolyData (its four groups of cells
  !< created here: VTK reads none without all of them). `is_volatile` is refused, `mesh_kind` unused (the points keep
  !< the kind they are given).
  class(vtkhdf_writer),       intent(inout)        :: self          !< Writer.
  character(*),               intent(in)           :: format        !< File format: VTKHDF.
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
  integer(hid_kind)                                :: block_group   !< Group of a block of PolyData cells.
  integer(I4P)                                     :: b             !< Counter.

  self%file_id = -1_hid_kind ; self%root = -1_hid_kind ; self%group = -1_hid_kind
  call self%reset
  if (.not.is_hdf5_enabled) then
    error = refuse('the library is built without HDF5 (VTKFORTRAN_USE_HDF5)')
    return
  endif
  if (present(is_volatile)) then
    if (is_volatile) then
      error = refuse('a volatile (in memory) file is not supported')
      return
    endif
  endif
  select case(trim(adjustl(mesh_topology)))
  case('ImageData')
    self%topology = TOPOLOGY_IMAGE
  case('UnstructuredGrid')
    self%topology = TOPOLOGY_UNSTRUCTURED
  case('PolyData')
    self%topology = TOPOLOGY_POLY
  case('RectilinearGrid')
    self%topology = TOPOLOGY_RECT
    self%version_minor = 7_I8P
  case('StructuredGrid')
    self%topology = TOPOLOGY_STRG
    self%version_minor = 7_I8P
  case default
    error = refuse('topology '//trim(adjustl(mesh_topology))//' is not supported')
    return
  endselect
  if (.not.self%is_partitioned()) then
    if (.not.(present(nx1).and.present(nx2).and.present(ny1).and.present(ny2).and.present(nz1).and.present(nz2))) then
      error = refuse(trim(adjustl(mesh_topology))//' needs its whole extent: nx1, nx2, ny1, ny2, nz1, nz2')
      return
    endif
    self%whole_extent = [nx1, nx2, ny1, ny2, nz1, nz2]
    if (any(self%whole_extent(2::2) < self%whole_extent(1::2))) then
      error = refuse('the whole extent is empty: each final node must not precede the initial one')
      return
    endif
  endif
  if (self%is_compressed .and. .not.h5_deflate_available()) then
    error = refuse('compressor zlib: the HDF5 library has no deflate filter')
    return
  endif
  if (self%is_parallel) then ! the ranks of comm write one file: a partition or a grid region each, every call collective
    error = 0_I4P
    if (.not.is_mpi_enabled) then
      error = refuse('comm: the library is built without MPI (VTKFORTRAN_USE_MPI)')
    elseif (self%is_compressed) then
      error = refuse('comm: compressor zlib is not supported (a deflated dataset needs collective writes)')
    endif
    if (error /= 0_I4P) return
    error = h5_file_create(filename, self%file_id, comm=self%comm)
  else
    error = h5_file_create(filename, self%file_id)
  endif
  if (error /= 0_I4P) error = refuse('cannot create '//filename)
  ! every group with its creation order tracked and indexed, as VTK writes them (its reader of a multi-block file
  ! lists the groups of each block by creation order)
  if (error == 0_I4P) error = h5_group_create(self%file_id, 'VTKHDF', self%root, track_order=.true.)
  if (self%topology == TOPOLOGY_IMAGE) then
    if (error == 0_I4P) error = h5_attribute_write(self%root, 'Type', 'ImageData')
    if (error == 0_I4P) error = h5_attribute_write(self%root, 'WholeExtent', int(self%whole_extent, I8P))
    if (error == 0_I4P) error = h5_attribute_write(self%root, 'Origin', self%origin)
    if (error == 0_I4P) error = h5_attribute_write(self%root, 'Spacing', self%spacing)
    if (error == 0_I4P) error = h5_attribute_write(self%root, 'Direction', self%direction)
  elseif (self%topology == TOPOLOGY_RECT .or. self%topology == TOPOLOGY_STRG) then
    if (error == 0_I4P) error = h5_attribute_write(self%root, 'Type', trim(adjustl(mesh_topology)))
    if (error == 0_I4P) error = h5_attribute_write(self%root, 'Dimensions', self%grid_points())
  elseif (self%topology == TOPOLOGY_UNSTRUCTURED) then
    if (error == 0_I4P) error = h5_attribute_write(self%root, 'Type', 'UnstructuredGrid')
  else
    if (error == 0_I4P) error = h5_attribute_write(self%root, 'Type', 'PolyData')
    do b=1, size(BLOCKS)
      if (error == 0_I4P) error = h5_group_create(self%root, trim(BLOCKS(b)), block_group, track_order=.true.)
      if (error == 0_I4P) error = h5_group_close(block_group)
    enddo
  endif
  endfunction initialize

  function finalize(self) result(error)
  !< Finalize the writer: write the version (the lowest one with the features used) and close the file.
  !<
  !< A group or a piece left open is closed. The last step of a time series can be a step without pieces (it reuses the
  !< mesh of the step before), not the first one.
  class(vtkhdf_writer),       intent(inout) :: self  !< Writer.
  integer(I4P)                              :: error !< Error status.

  error = 1_I4P
  if (self%file_id < 0_hid_kind) return
  error = 0_I4P
  if (self%is_temporal .and. self%step_pieces == 0_I8P .and. self%n_steps == 1_I8P) then
    error = refuse('the first step has no piece: the file holds no step')
  elseif (self%is_reusing()) then ! a step without pieces: the arrays written, the entries
    if (self%group >= 0_hid_kind) then
      if (self%close_group() /= 0_I4P) error = 1_I4P
    endif
    if (error == 0_I4P) error = self%end_reusing()
  endif
  if (self%group >= 0_hid_kind) then
    if (h5_group_close(self%group) /= 0_I4P) error = 1_I4P
  endif
  if (h5_attribute_write(self%root, 'Version', [2_I8P, self%version_minor]) /= 0_I4P) error = 1_I4P
  if (h5_group_close(self%root) /= 0_I4P) error = 1_I4P
  if (h5_file_close(self%file_id) /= 0_I4P) error = 1_I4P
  self%file_id = -1_hid_kind ; self%root = -1_hid_kind ; self%group = -1_hid_kind
  call self%reset
  self%is_parallel = .false.
  endfunction finalize

  function write_piece_start_tag(self, nx1, nx2, ny1, ny2, nz1, nz2) result(error)
  !< Start the piece of an ImageData, RectilinearGrid or StructuredGrid: VTKHDF stores one piece, whose extent is the
  !< whole extent. From the ranks of `comm`, the region of this rank: its sub-extent (empty: an upper bound before its
  !< lower one), the regions of all the ranks inside the whole extent, not overlapping in cells, covering it.
  class(vtkhdf_writer),       intent(inout) :: self
  integer(I4P),               intent(in)    :: nx1
  integer(I4P),               intent(in)    :: nx2
  integer(I4P),               intent(in)    :: ny1
  integer(I4P),               intent(in)    :: ny2
  integer(I4P),               intent(in)    :: nz1
  integer(I4P),               intent(in)    :: nz2
  integer(I4P)                              :: error

  error = 1_I4P
  if (self%file_id < 0_hid_kind .or. self%is_piece_open .or. self%group >= 0_hid_kind) return
  if (self%is_partitioned()) then
    error = refuse('write_piece(nx1, ..., nz2) is for a grid: the piece of an UnstructuredGrid or a PolyData is '// &
                   'write_piece(np, ...)')
  elseif (self%is_temporal .and. self%step_pieces > 0_I8P) then
    error = refuse('a grid (ImageData, RectilinearGrid, StructuredGrid) has one piece per step')
  elseif (self%is_reusing() .and. self%is_written_in_step()) then
    error = refuse('write_piece: arrays are already written in this step without its piece')
  elseif (self%is_parallel) then ! the region of this rank (the regions of all checked on every rank)
    error = self%start_region([nx1, nx2, ny1, ny2, nz1, nz2])
    if (error == 0_I4P) then
      self%n_pieces = self%n_pieces + 1_I4P
      self%step_pieces = self%step_pieces + 1_I8P
      self%is_geo_written = .false.
      self%is_piece_open = .true.
    endif
  elseif (.not.self%is_temporal .and. self%n_pieces > 0_I4P) then
    error = refuse('a grid (ImageData, RectilinearGrid, StructuredGrid) has one piece in VTKHDF')
  ! a RectilinearGrid or StructuredGrid reopened (action append): its extent start is not in the file, its size is
  elseif ((.not.(self%is_reopened .and. self%topology /= TOPOLOGY_IMAGE) .and. &
           any([nx1, nx2, ny1, ny2, nz1, nz2] /= self%whole_extent)) .or. &
          any([nx2 - nx1, ny2 - ny1, nz2 - nz1] /= self%whole_extent(2::2) - self%whole_extent(1::2))) then
    error = refuse('the extent of the piece must be the whole extent (one piece in VTKHDF)')
  else
    self%n_pieces = self%n_pieces + 1_I4P
    self%step_pieces = self%step_pieces + 1_I8P
    self%is_geo_written = .false.
    self%is_piece_open = .true.
    error = 0_I4P
  endif
  endfunction write_piece_start_tag

  function write_piece_end_tag(self) result(error)
  !< End the piece. A piece of an UnstructuredGrid or PolyData needs its points and cells, and every array of the first
  !< piece; the piece of a RectilinearGrid or StructuredGrid needs its geometry (in a time series, in the first step only:
  !< a later piece without it reuses the geometry of the step before). In a time series, the arrays of a grid are checked
  !< (one time slice, or none after the first step: the values of the step before) and the `Steps` entries of the step
  !< written (the file is valid after it).
  class(vtkhdf_writer),       intent(inout) :: self
  integer(I4P)                              :: error

  error = 1_I4P
  if (.not.self%is_piece_open .or. self%group >= 0_hid_kind) return
  self%is_piece_open = .false.
  error = 0_I4P
  if (self%is_partitioned()) then
    error = self%end_partition()
  elseif ((self%topology == TOPOLOGY_RECT .or. self%topology == TOPOLOGY_STRG) .and. .not.self%is_geo_written &
          .and. .not.(self%is_temporal .and. self%geo_slices > 0_I8P)) then ! a later step reuses the geometry
    error = refuse('the piece of a RectilinearGrid or StructuredGrid needs write_geo (in the first step)')
  endif
  if (error == 0_I4P .and. self%is_temporal) then
    if (.not.self%is_partitioned()) then ! the arrays of a grid: one time slice, or none after the first step
      error = self%check_arrays('PointData', 1_I8P, self%n_steps > 1_I8P)
      if (error == 0_I4P) error = self%check_arrays('CellData', 1_I8P, self%n_steps > 1_I8P)
    endif
    if (error == 0_I4P) error = self%write_steps()
  endif
  endfunction write_piece_end_tag

  function write_step(self, time) result(error)
  !< Start a time step: a file whose first output starts with `write_step` is a time series (VTKHDF `Steps`), each of
  !< its outputs then starts with `write_step(time)`, times increasing, followed by its field data and its pieces (one
  !< for a grid). The `Steps` entries are written at the end of every piece.
  !<
  !< After the first step, what does not change is stored once: a step without pieces reuses the mesh of the step before
  !< (its point and cell data written in data groups opened directly); a grid piece without `write_geo` reuses the
  !< geometry; an array not written reuses its values (see the module header for the rules).
  class(vtkhdf_writer),       intent(inout) :: self
  real(R8P),                  intent(in)    :: time
  integer(I4P)                              :: error
  integer(hid_kind)                         :: group_id !< A group of Steps.
  integer(I4P)                              :: g        !< Counter.

  error = 1_I4P
  if (self%file_id < 0_hid_kind) return
  if (self%is_parallel) then
    error = refuse('write_step: a time series is not written by the ranks of comm')
  elseif (self%is_piece_open .or. self%group >= 0_hid_kind) then
    error = refuse('write_step: outside the pieces and the data groups')
  elseif (.not.self%is_temporal .and. (self%n_pieces > 0_I4P .or. h5_link_exists(self%root, 'FieldData'))) then
    error = refuse('write_step: a time series starts with write_step, before its first piece and field data')
  elseif (self%is_temporal .and. self%step_pieces == 0_I8P .and. self%n_steps == 1_I8P) then
    error = refuse('write_step: the first step has no piece')
  elseif (self%is_temporal .and. time <= self%step_time) then
    error = refuse('write_step: the times must increase')
  else
    error = 0_I4P
    if (.not.self%is_temporal) then
      error = h5_group_create(self%root, 'Steps', group_id, track_order=.true.)
      if (error == 0_I4P) error = h5_group_close(group_id)
      do g=1, size(STEP_GROUPS)
        if (error == 0_I4P) error = h5_group_create(self%root, 'Steps/'//trim(STEP_GROUPS(g)), group_id, track_order=.true.)
        if (error == 0_I4P) error = h5_group_close(group_id)
      enddo
      if (error /= 0_I4P) error = refuse('cannot create the group Steps')
    endif
    if (self%is_reusing()) error = self%end_reusing() ! the step before, without pieces
    if (error == 0_I4P) then
      self%is_temporal = .true.
      self%n_steps = self%n_steps + 1_I8P
      self%step_time = time
      self%step_pieces = 0_I8P
      self%step_start = [int(self%n_pieces, I8P), self%total_points, self%total_cells, self%totals(4:)]
      self%step_blocks = self%block_totals
      error = self%roll_arrays()
    endif
  endif
  endfunction write_step

  function write_dataarray_location_tag(self, location, action, scalars, vectors, normals, tensors, tcoords) result(error)
  !< Open or close the group of the point (`node`) or cell (`cell`) data, inside the piece (or in a step without pieces,
  !< which reuses the mesh of the step before: its arrays hold the points or cells of all the parts reused).
  !<
  !< When opening, `scalars`, `vectors`, `normals`, `tensors` and `tcoords` name the active array of each role: the
  !< dataset of that name, when written, gets the `Attribute` attribute (VTKHDF 2.6) holding the role.
  class(vtkhdf_writer),       intent(inout)        :: self
  character(*),               intent(in)           :: location
  character(*),               intent(in)           :: action
  character(*),               intent(in), optional :: scalars
  character(*),               intent(in), optional :: vectors
  character(*),               intent(in), optional :: normals
  character(*),               intent(in), optional :: tensors
  character(*),               intent(in), optional :: tcoords
  integer(I4P)                                     :: error
  character(len=:), allocatable                    :: location_ !< Location, upper case.
  character(len=:), allocatable                    :: action_   !< Action, upper case.
  character(len=:), allocatable                    :: name      !< Group name.

  location_ = upper(location) ; action_ = upper(action)
  error = 1_I4P
  if (location_ == 'NODE') then
    name = 'PointData'
  elseif (location_ == 'CELL') then
    name = 'CellData'
  else
    return
  endif
  if (action_ == 'OPEN') then
    ! inside the piece, or in a step without pieces (it reuses the mesh of the step before)
    if (.not.(self%is_piece_open .or. self%is_reusing()) .or. self%group >= 0_hid_kind) return
    error = self%open_group(name)
    if (present(scalars)) self%active(1)%name = scalars
    if (present(vectors)) self%active(2)%name = vectors
    if (present(normals)) self%active(3)%name = normals
    if (present(tensors)) self%active(4)%name = tensors
    if (present(tcoords)) self%active(5)%name = tcoords
  elseif (action_ == 'CLOSE') then
    if (self%group < 0_hid_kind) return
    if (self%group_name /= name) return
    error = self%close_group()
    if (error == 0_I4P .and. self%is_reusing()) error = self%end_reusing() ! the file valid after the group
  endif
  endfunction write_dataarray_location_tag

  function write_fielddata_tag(self, action) result(error)
  !< Open or close the group of the field data (outside the piece).
  class(vtkhdf_writer),       intent(inout) :: self
  character(*),               intent(in)    :: action
  integer(I4P)                              :: error
  character(len=:), allocatable             :: action_ !< Action, upper case.

  action_ = upper(action)
  error = 1_I4P
  if (action_ == 'OPEN') then
    if (self%file_id < 0_hid_kind .or. self%is_piece_open .or. self%group >= 0_hid_kind) return
    error = self%open_group('FieldData')
  elseif (action_ == 'CLOSE') then
    if (self%group < 0_hid_kind) return
    if (self%group_name /= 'FieldData') return
    error = self%close_group()
  endif
  endfunction write_fielddata_tag

  function write_fielddata1_rank0(self, data_name, x) result(error)
  !< Write a field data array (one value, numeric): a dataset of FieldData.
  class(vtkhdf_writer),       intent(inout) :: self
  character(*),               intent(in)    :: data_name
  class(*),                   intent(in)    :: x
  integer(I4P)                              :: error

  error = 1_I4P
  if (self%group < 0_hid_kind) return
  if (self%group_name /= 'FieldData') return
  select type(x)
  type is(real(R8P))
    error = put_R8P(self, data_name, [x], 1_I8P, 1_I4P)
  type is(real(R4P))
    error = put_R4P(self, data_name, [x], 1_I8P, 1_I4P)
  type is(integer(I8P))
    error = put_I8P(self, data_name, [x], 1_I8P, 1_I4P)
  type is(integer(I4P))
    error = put_I4P(self, data_name, [x], 1_I8P, 1_I4P)
  type is(integer(I2P))
    error = put_I2P(self, data_name, [x], 1_I8P, 1_I4P)
  type is(integer(I1P))
    error = put_I1P(self, data_name, [x], 1_I8P, 1_I4P)
  type is(character(*))
    error = refuse('string field data are not supported (the VTK reader of VTKHDF crashes on them)')
  class default
    error = refuse('field data of this type are not supported')
  endselect
  endfunction write_fielddata1_rank0

  function write_fielddata1_rank1(self, data_name, x) result(error)
  !< Write a field data array (rank 1, numeric): a dataset of FieldData.
  class(vtkhdf_writer),       intent(inout) :: self
  character(*),               intent(in)    :: data_name
  class(*),                   intent(in)    :: x(1:)
  integer(I4P)                              :: error

  error = 1_I4P
  if (self%group < 0_hid_kind) return
  if (self%group_name /= 'FieldData') return
  select type(x)
  type is(real(R8P))
    error = put_R8P(self, data_name, x, size(x, kind=I8P), 1_I4P)
  type is(real(R4P))
    error = put_R4P(self, data_name, x, size(x, kind=I8P), 1_I4P)
  type is(integer(I8P))
    error = put_I8P(self, data_name, x, size(x, kind=I8P), 1_I4P)
  type is(integer(I4P))
    error = put_I4P(self, data_name, x, size(x, kind=I8P), 1_I4P)
  type is(integer(I2P))
    error = put_I2P(self, data_name, x, size(x, kind=I8P), 1_I4P)
  type is(integer(I1P))
    error = put_I1P(self, data_name, x, size(x, kind=I8P), 1_I4P)
  type is(character(*))
    error = refuse('string field data are not supported (the VTK reader of VTKHDF crashes on them)')
  class default
    error = refuse('field data of this type are not supported')
  endselect
  endfunction write_fielddata1_rank1

  elemental subroutine free(self, error)
  !< Free the writer state (the file is closed by `finalize`).
  class(vtkhdf_writer),       intent(inout)         :: self
  integer(I4P),               intent(out), optional :: error

  self%file_id = -1_hid_kind ; self%root = -1_hid_kind ; self%group = -1_hid_kind
  call self%reset
  self%is_parallel = .false.
  if (present(error)) error = 0_I4P
  endsubroutine free

  pure subroutine get_xml_volatile(self, xml_volatile, error)
  !< No volatile (in memory) file in VTKHDF: an error.
  class(vtkhdf_writer),       intent(in)               :: self
  character(len=:),           intent(out), allocatable :: xml_volatile
  integer(I4P),               intent(out), optional    :: error

  xml_volatile = ''
  if (present(error)) error = 1_I4P
  endsubroutine get_xml_volatile

  function write_dataarray1_rank1_R8P(self, data_name, x, is_tuples) result(error)
  !< Write a data array (R8P), one component: a dataset of the open group.
  class(vtkhdf_writer),       intent(inout)        :: self      !< Writer.
  character(*),               intent(in)           :: data_name !< Data name.
  real(R8P),                  intent(in)           :: x(1:)     !< Data variable.
  logical,                    intent(in), optional :: is_tuples !< Use "NumberOfTuples" instead of "NumberOfComponents".
  integer(I4P)                                     :: error     !< Error status.

  error = put_R8P(self, data_name, x, size(x, kind=I8P), 1_I4P)
  endfunction write_dataarray1_rank1_R8P

  function write_dataarray1_rank1_R4P(self, data_name, x, is_tuples) result(error)
  !< Write a data array (R4P), one component: a dataset of the open group.
  class(vtkhdf_writer),       intent(inout)        :: self      !< Writer.
  character(*),               intent(in)           :: data_name !< Data name.
  real(R4P),                  intent(in)           :: x(1:)     !< Data variable.
  logical,                    intent(in), optional :: is_tuples !< Use "NumberOfTuples" instead of "NumberOfComponents".
  integer(I4P)                                     :: error     !< Error status.

  error = put_R4P(self, data_name, x, size(x, kind=I8P), 1_I4P)
  endfunction write_dataarray1_rank1_R4P

  function write_dataarray1_rank1_I8P(self, data_name, x, is_tuples) result(error)
  !< Write a data array (I8P), one component: a dataset of the open group.
  class(vtkhdf_writer),       intent(inout)        :: self      !< Writer.
  character(*),               intent(in)           :: data_name !< Data name.
  integer(I8P),               intent(in)           :: x(1:)     !< Data variable.
  logical,                    intent(in), optional :: is_tuples !< Use "NumberOfTuples" instead of "NumberOfComponents".
  integer(I4P)                                     :: error     !< Error status.

  error = put_I8P(self, data_name, x, size(x, kind=I8P), 1_I4P)
  endfunction write_dataarray1_rank1_I8P

  function write_dataarray1_rank1_I4P(self, data_name, x, is_tuples) result(error)
  !< Write a data array (I4P), one component: a dataset of the open group.
  class(vtkhdf_writer),       intent(inout)        :: self      !< Writer.
  character(*),               intent(in)           :: data_name !< Data name.
  integer(I4P),               intent(in)           :: x(1:)     !< Data variable.
  logical,                    intent(in), optional :: is_tuples !< Use "NumberOfTuples" instead of "NumberOfComponents".
  integer(I4P)                                     :: error     !< Error status.

  error = put_I4P(self, data_name, x, size(x, kind=I8P), 1_I4P)
  endfunction write_dataarray1_rank1_I4P

  function write_dataarray1_rank1_I2P(self, data_name, x, is_tuples) result(error)
  !< Write a data array (I2P), one component: a dataset of the open group.
  class(vtkhdf_writer),       intent(inout)        :: self      !< Writer.
  character(*),               intent(in)           :: data_name !< Data name.
  integer(I2P),               intent(in)           :: x(1:)     !< Data variable.
  logical,                    intent(in), optional :: is_tuples !< Use "NumberOfTuples" instead of "NumberOfComponents".
  integer(I4P)                                     :: error     !< Error status.

  error = put_I2P(self, data_name, x, size(x, kind=I8P), 1_I4P)
  endfunction write_dataarray1_rank1_I2P

  function write_dataarray1_rank1_I1P(self, data_name, x, is_tuples) result(error)
  !< Write a data array (I1P), one component: a dataset of the open group.
  class(vtkhdf_writer),       intent(inout)        :: self      !< Writer.
  character(*),               intent(in)           :: data_name !< Data name.
  integer(I1P),               intent(in)           :: x(1:)     !< Data variable.
  logical,                    intent(in), optional :: is_tuples !< Use "NumberOfTuples" instead of "NumberOfComponents".
  integer(I4P)                                     :: error     !< Error status.

  error = put_I1P(self, data_name, x, size(x, kind=I8P), 1_I4P)
  endfunction write_dataarray1_rank1_I1P

  function write_dataarray1_rank2_R8P(self, data_name, x, one_component, is_tuples) result(error)
  !< Write a data array (R8P), `size(x, 1)` components (one with `one_component`): a dataset of the open group.
  class(vtkhdf_writer),       intent(inout)        :: self          !< Writer.
  character(*),               intent(in)           :: data_name     !< Data name.
  real(R8P),                  intent(in)           :: x(1:,1:)      !< Data variable.
  logical,                    intent(in), optional :: one_component !< Force one component.
  logical,                    intent(in), optional :: is_tuples     !< Use "NumberOfTuples" instead of "NumberOfComponents".
  integer(I4P)                                     :: error         !< Error status.
  integer(I4P)                                     :: n_components  !< Number of components.

  n_components = size(x, dim=1)
  if (present(one_component)) then
    if (one_component) n_components = 1_I4P
  endif
  error = put_R8P(self, data_name, x, size(x, kind=I8P), n_components)
  endfunction write_dataarray1_rank2_R8P

  function write_dataarray1_rank2_R4P(self, data_name, x, one_component, is_tuples) result(error)
  !< Write a data array (R4P), `size(x, 1)` components (one with `one_component`): a dataset of the open group.
  class(vtkhdf_writer),       intent(inout)        :: self          !< Writer.
  character(*),               intent(in)           :: data_name     !< Data name.
  real(R4P),                  intent(in)           :: x(1:,1:)      !< Data variable.
  logical,                    intent(in), optional :: one_component !< Force one component.
  logical,                    intent(in), optional :: is_tuples     !< Use "NumberOfTuples" instead of "NumberOfComponents".
  integer(I4P)                                     :: error         !< Error status.
  integer(I4P)                                     :: n_components  !< Number of components.

  n_components = size(x, dim=1)
  if (present(one_component)) then
    if (one_component) n_components = 1_I4P
  endif
  error = put_R4P(self, data_name, x, size(x, kind=I8P), n_components)
  endfunction write_dataarray1_rank2_R4P

  function write_dataarray1_rank2_I8P(self, data_name, x, one_component, is_tuples) result(error)
  !< Write a data array (I8P), `size(x, 1)` components (one with `one_component`): a dataset of the open group.
  class(vtkhdf_writer),       intent(inout)        :: self          !< Writer.
  character(*),               intent(in)           :: data_name     !< Data name.
  integer(I8P),               intent(in)           :: x(1:,1:)      !< Data variable.
  logical,                    intent(in), optional :: one_component !< Force one component.
  logical,                    intent(in), optional :: is_tuples     !< Use "NumberOfTuples" instead of "NumberOfComponents".
  integer(I4P)                                     :: error         !< Error status.
  integer(I4P)                                     :: n_components  !< Number of components.

  n_components = size(x, dim=1)
  if (present(one_component)) then
    if (one_component) n_components = 1_I4P
  endif
  error = put_I8P(self, data_name, x, size(x, kind=I8P), n_components)
  endfunction write_dataarray1_rank2_I8P

  function write_dataarray1_rank2_I4P(self, data_name, x, one_component, is_tuples) result(error)
  !< Write a data array (I4P), `size(x, 1)` components (one with `one_component`): a dataset of the open group.
  class(vtkhdf_writer),       intent(inout)        :: self          !< Writer.
  character(*),               intent(in)           :: data_name     !< Data name.
  integer(I4P),               intent(in)           :: x(1:,1:)      !< Data variable.
  logical,                    intent(in), optional :: one_component !< Force one component.
  logical,                    intent(in), optional :: is_tuples     !< Use "NumberOfTuples" instead of "NumberOfComponents".
  integer(I4P)                                     :: error         !< Error status.
  integer(I4P)                                     :: n_components  !< Number of components.

  n_components = size(x, dim=1)
  if (present(one_component)) then
    if (one_component) n_components = 1_I4P
  endif
  error = put_I4P(self, data_name, x, size(x, kind=I8P), n_components)
  endfunction write_dataarray1_rank2_I4P

  function write_dataarray1_rank2_I2P(self, data_name, x, one_component, is_tuples) result(error)
  !< Write a data array (I2P), `size(x, 1)` components (one with `one_component`): a dataset of the open group.
  class(vtkhdf_writer),       intent(inout)        :: self          !< Writer.
  character(*),               intent(in)           :: data_name     !< Data name.
  integer(I2P),               intent(in)           :: x(1:,1:)      !< Data variable.
  logical,                    intent(in), optional :: one_component !< Force one component.
  logical,                    intent(in), optional :: is_tuples     !< Use "NumberOfTuples" instead of "NumberOfComponents".
  integer(I4P)                                     :: error         !< Error status.
  integer(I4P)                                     :: n_components  !< Number of components.

  n_components = size(x, dim=1)
  if (present(one_component)) then
    if (one_component) n_components = 1_I4P
  endif
  error = put_I2P(self, data_name, x, size(x, kind=I8P), n_components)
  endfunction write_dataarray1_rank2_I2P

  function write_dataarray1_rank2_I1P(self, data_name, x, one_component, is_tuples) result(error)
  !< Write a data array (I1P), `size(x, 1)` components (one with `one_component`): a dataset of the open group.
  class(vtkhdf_writer),       intent(inout)        :: self          !< Writer.
  character(*),               intent(in)           :: data_name     !< Data name.
  integer(I1P),               intent(in)           :: x(1:,1:)      !< Data variable.
  logical,                    intent(in), optional :: one_component !< Force one component.
  logical,                    intent(in), optional :: is_tuples     !< Use "NumberOfTuples" instead of "NumberOfComponents".
  integer(I4P)                                     :: error         !< Error status.
  integer(I4P)                                     :: n_components  !< Number of components.

  n_components = size(x, dim=1)
  if (present(one_component)) then
    if (one_component) n_components = 1_I4P
  endif
  error = put_I1P(self, data_name, x, size(x, kind=I8P), n_components)
  endfunction write_dataarray1_rank2_I1P

  function write_dataarray1_rank3_R8P(self, data_name, x, one_component, is_tuples) result(error)
  !< Write a data array (R8P), `size(x, 1)` components (one with `one_component`): a dataset of the open group.
  class(vtkhdf_writer),       intent(inout)        :: self          !< Writer.
  character(*),               intent(in)           :: data_name     !< Data name.
  real(R8P),                  intent(in)           :: x(1:,1:,1:)   !< Data variable.
  logical,                    intent(in), optional :: one_component !< Force one component.
  logical,                    intent(in), optional :: is_tuples     !< Use "NumberOfTuples" instead of "NumberOfComponents".
  integer(I4P)                                     :: error         !< Error status.
  integer(I4P)                                     :: n_components  !< Number of components.

  n_components = size(x, dim=1)
  if (present(one_component)) then
    if (one_component) n_components = 1_I4P
  endif
  error = put_R8P(self, data_name, x, size(x, kind=I8P), n_components)
  endfunction write_dataarray1_rank3_R8P

  function write_dataarray1_rank3_R4P(self, data_name, x, one_component, is_tuples) result(error)
  !< Write a data array (R4P), `size(x, 1)` components (one with `one_component`): a dataset of the open group.
  class(vtkhdf_writer),       intent(inout)        :: self          !< Writer.
  character(*),               intent(in)           :: data_name     !< Data name.
  real(R4P),                  intent(in)           :: x(1:,1:,1:)   !< Data variable.
  logical,                    intent(in), optional :: one_component !< Force one component.
  logical,                    intent(in), optional :: is_tuples     !< Use "NumberOfTuples" instead of "NumberOfComponents".
  integer(I4P)                                     :: error         !< Error status.
  integer(I4P)                                     :: n_components  !< Number of components.

  n_components = size(x, dim=1)
  if (present(one_component)) then
    if (one_component) n_components = 1_I4P
  endif
  error = put_R4P(self, data_name, x, size(x, kind=I8P), n_components)
  endfunction write_dataarray1_rank3_R4P

  function write_dataarray1_rank3_I8P(self, data_name, x, one_component, is_tuples) result(error)
  !< Write a data array (I8P), `size(x, 1)` components (one with `one_component`): a dataset of the open group.
  class(vtkhdf_writer),       intent(inout)        :: self          !< Writer.
  character(*),               intent(in)           :: data_name     !< Data name.
  integer(I8P),               intent(in)           :: x(1:,1:,1:)   !< Data variable.
  logical,                    intent(in), optional :: one_component !< Force one component.
  logical,                    intent(in), optional :: is_tuples     !< Use "NumberOfTuples" instead of "NumberOfComponents".
  integer(I4P)                                     :: error         !< Error status.
  integer(I4P)                                     :: n_components  !< Number of components.

  n_components = size(x, dim=1)
  if (present(one_component)) then
    if (one_component) n_components = 1_I4P
  endif
  error = put_I8P(self, data_name, x, size(x, kind=I8P), n_components)
  endfunction write_dataarray1_rank3_I8P

  function write_dataarray1_rank3_I4P(self, data_name, x, one_component, is_tuples) result(error)
  !< Write a data array (I4P), `size(x, 1)` components (one with `one_component`): a dataset of the open group.
  class(vtkhdf_writer),       intent(inout)        :: self          !< Writer.
  character(*),               intent(in)           :: data_name     !< Data name.
  integer(I4P),               intent(in)           :: x(1:,1:,1:)   !< Data variable.
  logical,                    intent(in), optional :: one_component !< Force one component.
  logical,                    intent(in), optional :: is_tuples     !< Use "NumberOfTuples" instead of "NumberOfComponents".
  integer(I4P)                                     :: error         !< Error status.
  integer(I4P)                                     :: n_components  !< Number of components.

  n_components = size(x, dim=1)
  if (present(one_component)) then
    if (one_component) n_components = 1_I4P
  endif
  error = put_I4P(self, data_name, x, size(x, kind=I8P), n_components)
  endfunction write_dataarray1_rank3_I4P

  function write_dataarray1_rank3_I2P(self, data_name, x, one_component, is_tuples) result(error)
  !< Write a data array (I2P), `size(x, 1)` components (one with `one_component`): a dataset of the open group.
  class(vtkhdf_writer),       intent(inout)        :: self          !< Writer.
  character(*),               intent(in)           :: data_name     !< Data name.
  integer(I2P),               intent(in)           :: x(1:,1:,1:)   !< Data variable.
  logical,                    intent(in), optional :: one_component !< Force one component.
  logical,                    intent(in), optional :: is_tuples     !< Use "NumberOfTuples" instead of "NumberOfComponents".
  integer(I4P)                                     :: error         !< Error status.
  integer(I4P)                                     :: n_components  !< Number of components.

  n_components = size(x, dim=1)
  if (present(one_component)) then
    if (one_component) n_components = 1_I4P
  endif
  error = put_I2P(self, data_name, x, size(x, kind=I8P), n_components)
  endfunction write_dataarray1_rank3_I2P

  function write_dataarray1_rank3_I1P(self, data_name, x, one_component, is_tuples) result(error)
  !< Write a data array (I1P), `size(x, 1)` components (one with `one_component`): a dataset of the open group.
  class(vtkhdf_writer),       intent(inout)        :: self          !< Writer.
  character(*),               intent(in)           :: data_name     !< Data name.
  integer(I1P),               intent(in)           :: x(1:,1:,1:)   !< Data variable.
  logical,                    intent(in), optional :: one_component !< Force one component.
  logical,                    intent(in), optional :: is_tuples     !< Use "NumberOfTuples" instead of "NumberOfComponents".
  integer(I4P)                                     :: error         !< Error status.
  integer(I4P)                                     :: n_components  !< Number of components.

  n_components = size(x, dim=1)
  if (present(one_component)) then
    if (one_component) n_components = 1_I4P
  endif
  error = put_I1P(self, data_name, x, size(x, kind=I8P), n_components)
  endfunction write_dataarray1_rank3_I1P

  function write_dataarray1_rank4_R8P(self, data_name, x, one_component, is_tuples) result(error)
  !< Write a data array (R8P), `size(x, 1)` components (one with `one_component`): a dataset of the open group.
  class(vtkhdf_writer),       intent(inout)        :: self           !< Writer.
  character(*),               intent(in)           :: data_name      !< Data name.
  real(R8P),                  intent(in)           :: x(1:,1:,1:,1:) !< Data variable.
  logical,                    intent(in), optional :: one_component  !< Force one component.
  logical,                    intent(in), optional :: is_tuples      !< Use "NumberOfTuples" instead of "NumberOfComponents".
  integer(I4P)                                     :: error          !< Error status.
  integer(I4P)                                     :: n_components   !< Number of components.

  n_components = size(x, dim=1)
  if (present(one_component)) then
    if (one_component) n_components = 1_I4P
  endif
  error = put_R8P(self, data_name, x, size(x, kind=I8P), n_components)
  endfunction write_dataarray1_rank4_R8P

  function write_dataarray1_rank4_R4P(self, data_name, x, one_component, is_tuples) result(error)
  !< Write a data array (R4P), `size(x, 1)` components (one with `one_component`): a dataset of the open group.
  class(vtkhdf_writer),       intent(inout)        :: self           !< Writer.
  character(*),               intent(in)           :: data_name      !< Data name.
  real(R4P),                  intent(in)           :: x(1:,1:,1:,1:) !< Data variable.
  logical,                    intent(in), optional :: one_component  !< Force one component.
  logical,                    intent(in), optional :: is_tuples      !< Use "NumberOfTuples" instead of "NumberOfComponents".
  integer(I4P)                                     :: error          !< Error status.
  integer(I4P)                                     :: n_components   !< Number of components.

  n_components = size(x, dim=1)
  if (present(one_component)) then
    if (one_component) n_components = 1_I4P
  endif
  error = put_R4P(self, data_name, x, size(x, kind=I8P), n_components)
  endfunction write_dataarray1_rank4_R4P

  function write_dataarray1_rank4_I8P(self, data_name, x, one_component, is_tuples) result(error)
  !< Write a data array (I8P), `size(x, 1)` components (one with `one_component`): a dataset of the open group.
  class(vtkhdf_writer),       intent(inout)        :: self           !< Writer.
  character(*),               intent(in)           :: data_name      !< Data name.
  integer(I8P),               intent(in)           :: x(1:,1:,1:,1:) !< Data variable.
  logical,                    intent(in), optional :: one_component  !< Force one component.
  logical,                    intent(in), optional :: is_tuples      !< Use "NumberOfTuples" instead of "NumberOfComponents".
  integer(I4P)                                     :: error          !< Error status.
  integer(I4P)                                     :: n_components   !< Number of components.

  n_components = size(x, dim=1)
  if (present(one_component)) then
    if (one_component) n_components = 1_I4P
  endif
  error = put_I8P(self, data_name, x, size(x, kind=I8P), n_components)
  endfunction write_dataarray1_rank4_I8P

  function write_dataarray1_rank4_I4P(self, data_name, x, one_component, is_tuples) result(error)
  !< Write a data array (I4P), `size(x, 1)` components (one with `one_component`): a dataset of the open group.
  class(vtkhdf_writer),       intent(inout)        :: self           !< Writer.
  character(*),               intent(in)           :: data_name      !< Data name.
  integer(I4P),               intent(in)           :: x(1:,1:,1:,1:) !< Data variable.
  logical,                    intent(in), optional :: one_component  !< Force one component.
  logical,                    intent(in), optional :: is_tuples      !< Use "NumberOfTuples" instead of "NumberOfComponents".
  integer(I4P)                                     :: error          !< Error status.
  integer(I4P)                                     :: n_components   !< Number of components.

  n_components = size(x, dim=1)
  if (present(one_component)) then
    if (one_component) n_components = 1_I4P
  endif
  error = put_I4P(self, data_name, x, size(x, kind=I8P), n_components)
  endfunction write_dataarray1_rank4_I4P

  function write_dataarray1_rank4_I2P(self, data_name, x, one_component, is_tuples) result(error)
  !< Write a data array (I2P), `size(x, 1)` components (one with `one_component`): a dataset of the open group.
  class(vtkhdf_writer),       intent(inout)        :: self           !< Writer.
  character(*),               intent(in)           :: data_name      !< Data name.
  integer(I2P),               intent(in)           :: x(1:,1:,1:,1:) !< Data variable.
  logical,                    intent(in), optional :: one_component  !< Force one component.
  logical,                    intent(in), optional :: is_tuples      !< Use "NumberOfTuples" instead of "NumberOfComponents".
  integer(I4P)                                     :: error          !< Error status.
  integer(I4P)                                     :: n_components   !< Number of components.

  n_components = size(x, dim=1)
  if (present(one_component)) then
    if (one_component) n_components = 1_I4P
  endif
  error = put_I2P(self, data_name, x, size(x, kind=I8P), n_components)
  endfunction write_dataarray1_rank4_I2P

  function write_dataarray1_rank4_I1P(self, data_name, x, one_component, is_tuples) result(error)
  !< Write a data array (I1P), `size(x, 1)` components (one with `one_component`): a dataset of the open group.
  class(vtkhdf_writer),       intent(inout)        :: self           !< Writer.
  character(*),               intent(in)           :: data_name      !< Data name.
  integer(I1P),               intent(in)           :: x(1:,1:,1:,1:) !< Data variable.
  logical,                    intent(in), optional :: one_component  !< Force one component.
  logical,                    intent(in), optional :: is_tuples      !< Use "NumberOfTuples" instead of "NumberOfComponents".
  integer(I4P)                                     :: error          !< Error status.
  integer(I4P)                                     :: n_components   !< Number of components.

  n_components = size(x, dim=1)
  if (present(one_component)) then
    if (one_component) n_components = 1_I4P
  endif
  error = put_I1P(self, data_name, x, size(x, kind=I8P), n_components)
  endfunction write_dataarray1_rank4_I1P

  function write_dataarray3_rank1_R8P(self, data_name, x, y, z, is_tuples) result(error)
  !< Write a data array (R8P) of 3 components given one by one: a dataset of the open group.
  class(vtkhdf_writer),       intent(inout)        :: self        !< Writer.
  character(*),               intent(in)           :: data_name   !< Data name.
  real(R8P),                  intent(in)           :: x(1:)       !< X component of data variable.
  real(R8P),                  intent(in)           :: y(1:)       !< Y component of data variable.
  real(R8P),                  intent(in)           :: z(1:)       !< Z component of data variable.
  logical,                    intent(in), optional :: is_tuples   !< Use "NumberOfTuples" instead of "NumberOfComponents".
  integer(I4P)                                     :: error       !< Error status.
  real(R8P), allocatable                           :: buffer(:,:) !< Components interleaved.

  if (size(y) /= size(x) .or. size(z) /= size(x)) then
    error = refuse('the components of '//data_name//' have different sizes')
    return
  endif
  allocate(buffer(3, size(x)))
  buffer(1,:) = x
  buffer(2,:) = y
  buffer(3,:) = z
  error = put_R8P(self, data_name, buffer, size(buffer, kind=I8P), 3_I4P)
  endfunction write_dataarray3_rank1_R8P

  function write_dataarray3_rank1_R4P(self, data_name, x, y, z, is_tuples) result(error)
  !< Write a data array (R4P) of 3 components given one by one: a dataset of the open group.
  class(vtkhdf_writer),       intent(inout)        :: self        !< Writer.
  character(*),               intent(in)           :: data_name   !< Data name.
  real(R4P),                  intent(in)           :: x(1:)       !< X component of data variable.
  real(R4P),                  intent(in)           :: y(1:)       !< Y component of data variable.
  real(R4P),                  intent(in)           :: z(1:)       !< Z component of data variable.
  logical,                    intent(in), optional :: is_tuples   !< Use "NumberOfTuples" instead of "NumberOfComponents".
  integer(I4P)                                     :: error       !< Error status.
  real(R4P), allocatable                           :: buffer(:,:) !< Components interleaved.

  if (size(y) /= size(x) .or. size(z) /= size(x)) then
    error = refuse('the components of '//data_name//' have different sizes')
    return
  endif
  allocate(buffer(3, size(x)))
  buffer(1,:) = x
  buffer(2,:) = y
  buffer(3,:) = z
  error = put_R4P(self, data_name, buffer, size(buffer, kind=I8P), 3_I4P)
  endfunction write_dataarray3_rank1_R4P

  function write_dataarray3_rank1_I8P(self, data_name, x, y, z, is_tuples) result(error)
  !< Write a data array (I8P) of 3 components given one by one: a dataset of the open group.
  class(vtkhdf_writer),       intent(inout)        :: self        !< Writer.
  character(*),               intent(in)           :: data_name   !< Data name.
  integer(I8P),               intent(in)           :: x(1:)       !< X component of data variable.
  integer(I8P),               intent(in)           :: y(1:)       !< Y component of data variable.
  integer(I8P),               intent(in)           :: z(1:)       !< Z component of data variable.
  logical,                    intent(in), optional :: is_tuples   !< Use "NumberOfTuples" instead of "NumberOfComponents".
  integer(I4P)                                     :: error       !< Error status.
  integer(I8P), allocatable                        :: buffer(:,:) !< Components interleaved.

  if (size(y) /= size(x) .or. size(z) /= size(x)) then
    error = refuse('the components of '//data_name//' have different sizes')
    return
  endif
  allocate(buffer(3, size(x)))
  buffer(1,:) = x
  buffer(2,:) = y
  buffer(3,:) = z
  error = put_I8P(self, data_name, buffer, size(buffer, kind=I8P), 3_I4P)
  endfunction write_dataarray3_rank1_I8P

  function write_dataarray3_rank1_I4P(self, data_name, x, y, z, is_tuples) result(error)
  !< Write a data array (I4P) of 3 components given one by one: a dataset of the open group.
  class(vtkhdf_writer),       intent(inout)        :: self        !< Writer.
  character(*),               intent(in)           :: data_name   !< Data name.
  integer(I4P),               intent(in)           :: x(1:)       !< X component of data variable.
  integer(I4P),               intent(in)           :: y(1:)       !< Y component of data variable.
  integer(I4P),               intent(in)           :: z(1:)       !< Z component of data variable.
  logical,                    intent(in), optional :: is_tuples   !< Use "NumberOfTuples" instead of "NumberOfComponents".
  integer(I4P)                                     :: error       !< Error status.
  integer(I4P), allocatable                        :: buffer(:,:) !< Components interleaved.

  if (size(y) /= size(x) .or. size(z) /= size(x)) then
    error = refuse('the components of '//data_name//' have different sizes')
    return
  endif
  allocate(buffer(3, size(x)))
  buffer(1,:) = x
  buffer(2,:) = y
  buffer(3,:) = z
  error = put_I4P(self, data_name, buffer, size(buffer, kind=I8P), 3_I4P)
  endfunction write_dataarray3_rank1_I4P

  function write_dataarray3_rank1_I2P(self, data_name, x, y, z, is_tuples) result(error)
  !< Write a data array (I2P) of 3 components given one by one: a dataset of the open group.
  class(vtkhdf_writer),       intent(inout)        :: self        !< Writer.
  character(*),               intent(in)           :: data_name   !< Data name.
  integer(I2P),               intent(in)           :: x(1:)       !< X component of data variable.
  integer(I2P),               intent(in)           :: y(1:)       !< Y component of data variable.
  integer(I2P),               intent(in)           :: z(1:)       !< Z component of data variable.
  logical,                    intent(in), optional :: is_tuples   !< Use "NumberOfTuples" instead of "NumberOfComponents".
  integer(I4P)                                     :: error       !< Error status.
  integer(I2P), allocatable                        :: buffer(:,:) !< Components interleaved.

  if (size(y) /= size(x) .or. size(z) /= size(x)) then
    error = refuse('the components of '//data_name//' have different sizes')
    return
  endif
  allocate(buffer(3, size(x)))
  buffer(1,:) = x
  buffer(2,:) = y
  buffer(3,:) = z
  error = put_I2P(self, data_name, buffer, size(buffer, kind=I8P), 3_I4P)
  endfunction write_dataarray3_rank1_I2P

  function write_dataarray3_rank1_I1P(self, data_name, x, y, z, is_tuples) result(error)
  !< Write a data array (I1P) of 3 components given one by one: a dataset of the open group.
  class(vtkhdf_writer),       intent(inout)        :: self        !< Writer.
  character(*),               intent(in)           :: data_name   !< Data name.
  integer(I1P),               intent(in)           :: x(1:)       !< X component of data variable.
  integer(I1P),               intent(in)           :: y(1:)       !< Y component of data variable.
  integer(I1P),               intent(in)           :: z(1:)       !< Z component of data variable.
  logical,                    intent(in), optional :: is_tuples   !< Use "NumberOfTuples" instead of "NumberOfComponents".
  integer(I4P)                                     :: error       !< Error status.
  integer(I1P), allocatable                        :: buffer(:,:) !< Components interleaved.

  if (size(y) /= size(x) .or. size(z) /= size(x)) then
    error = refuse('the components of '//data_name//' have different sizes')
    return
  endif
  allocate(buffer(3, size(x)))
  buffer(1,:) = x
  buffer(2,:) = y
  buffer(3,:) = z
  error = put_I1P(self, data_name, buffer, size(buffer, kind=I8P), 3_I4P)
  endfunction write_dataarray3_rank1_I1P

  function write_dataarray3_rank3_R8P(self, data_name, x, y, z, is_tuples) result(error)
  !< Write a data array (R8P) of 3 components given one by one: a dataset of the open group.
  class(vtkhdf_writer),       intent(inout)        :: self        !< Writer.
  character(*),               intent(in)           :: data_name   !< Data name.
  real(R8P),                  intent(in)           :: x(1:,1:,1:) !< X component of data variable.
  real(R8P),                  intent(in)           :: y(1:,1:,1:) !< Y component of data variable.
  real(R8P),                  intent(in)           :: z(1:,1:,1:) !< Z component of data variable.
  logical,                    intent(in), optional :: is_tuples   !< Use "NumberOfTuples" instead of "NumberOfComponents".
  integer(I4P)                                     :: error       !< Error status.
  real(R8P), allocatable                           :: buffer(:,:) !< Components interleaved.

  if (size(y) /= size(x) .or. size(z) /= size(x)) then
    error = refuse('the components of '//data_name//' have different sizes')
    return
  endif
  allocate(buffer(3, size(x)))
  buffer(1,:) = reshape(x, [size(x)])
  buffer(2,:) = reshape(y, [size(y)])
  buffer(3,:) = reshape(z, [size(z)])
  error = put_R8P(self, data_name, buffer, size(buffer, kind=I8P), 3_I4P)
  endfunction write_dataarray3_rank3_R8P

  function write_dataarray3_rank3_R4P(self, data_name, x, y, z, is_tuples) result(error)
  !< Write a data array (R4P) of 3 components given one by one: a dataset of the open group.
  class(vtkhdf_writer),       intent(inout)        :: self        !< Writer.
  character(*),               intent(in)           :: data_name   !< Data name.
  real(R4P),                  intent(in)           :: x(1:,1:,1:) !< X component of data variable.
  real(R4P),                  intent(in)           :: y(1:,1:,1:) !< Y component of data variable.
  real(R4P),                  intent(in)           :: z(1:,1:,1:) !< Z component of data variable.
  logical,                    intent(in), optional :: is_tuples   !< Use "NumberOfTuples" instead of "NumberOfComponents".
  integer(I4P)                                     :: error       !< Error status.
  real(R4P), allocatable                           :: buffer(:,:) !< Components interleaved.

  if (size(y) /= size(x) .or. size(z) /= size(x)) then
    error = refuse('the components of '//data_name//' have different sizes')
    return
  endif
  allocate(buffer(3, size(x)))
  buffer(1,:) = reshape(x, [size(x)])
  buffer(2,:) = reshape(y, [size(y)])
  buffer(3,:) = reshape(z, [size(z)])
  error = put_R4P(self, data_name, buffer, size(buffer, kind=I8P), 3_I4P)
  endfunction write_dataarray3_rank3_R4P

  function write_dataarray3_rank3_I8P(self, data_name, x, y, z, is_tuples) result(error)
  !< Write a data array (I8P) of 3 components given one by one: a dataset of the open group.
  class(vtkhdf_writer),       intent(inout)        :: self        !< Writer.
  character(*),               intent(in)           :: data_name   !< Data name.
  integer(I8P),               intent(in)           :: x(1:,1:,1:) !< X component of data variable.
  integer(I8P),               intent(in)           :: y(1:,1:,1:) !< Y component of data variable.
  integer(I8P),               intent(in)           :: z(1:,1:,1:) !< Z component of data variable.
  logical,                    intent(in), optional :: is_tuples   !< Use "NumberOfTuples" instead of "NumberOfComponents".
  integer(I4P)                                     :: error       !< Error status.
  integer(I8P), allocatable                        :: buffer(:,:) !< Components interleaved.

  if (size(y) /= size(x) .or. size(z) /= size(x)) then
    error = refuse('the components of '//data_name//' have different sizes')
    return
  endif
  allocate(buffer(3, size(x)))
  buffer(1,:) = reshape(x, [size(x)])
  buffer(2,:) = reshape(y, [size(y)])
  buffer(3,:) = reshape(z, [size(z)])
  error = put_I8P(self, data_name, buffer, size(buffer, kind=I8P), 3_I4P)
  endfunction write_dataarray3_rank3_I8P

  function write_dataarray3_rank3_I4P(self, data_name, x, y, z, is_tuples) result(error)
  !< Write a data array (I4P) of 3 components given one by one: a dataset of the open group.
  class(vtkhdf_writer),       intent(inout)        :: self        !< Writer.
  character(*),               intent(in)           :: data_name   !< Data name.
  integer(I4P),               intent(in)           :: x(1:,1:,1:) !< X component of data variable.
  integer(I4P),               intent(in)           :: y(1:,1:,1:) !< Y component of data variable.
  integer(I4P),               intent(in)           :: z(1:,1:,1:) !< Z component of data variable.
  logical,                    intent(in), optional :: is_tuples   !< Use "NumberOfTuples" instead of "NumberOfComponents".
  integer(I4P)                                     :: error       !< Error status.
  integer(I4P), allocatable                        :: buffer(:,:) !< Components interleaved.

  if (size(y) /= size(x) .or. size(z) /= size(x)) then
    error = refuse('the components of '//data_name//' have different sizes')
    return
  endif
  allocate(buffer(3, size(x)))
  buffer(1,:) = reshape(x, [size(x)])
  buffer(2,:) = reshape(y, [size(y)])
  buffer(3,:) = reshape(z, [size(z)])
  error = put_I4P(self, data_name, buffer, size(buffer, kind=I8P), 3_I4P)
  endfunction write_dataarray3_rank3_I4P

  function write_dataarray3_rank3_I2P(self, data_name, x, y, z, is_tuples) result(error)
  !< Write a data array (I2P) of 3 components given one by one: a dataset of the open group.
  class(vtkhdf_writer),       intent(inout)        :: self        !< Writer.
  character(*),               intent(in)           :: data_name   !< Data name.
  integer(I2P),               intent(in)           :: x(1:,1:,1:) !< X component of data variable.
  integer(I2P),               intent(in)           :: y(1:,1:,1:) !< Y component of data variable.
  integer(I2P),               intent(in)           :: z(1:,1:,1:) !< Z component of data variable.
  logical,                    intent(in), optional :: is_tuples   !< Use "NumberOfTuples" instead of "NumberOfComponents".
  integer(I4P)                                     :: error       !< Error status.
  integer(I2P), allocatable                        :: buffer(:,:) !< Components interleaved.

  if (size(y) /= size(x) .or. size(z) /= size(x)) then
    error = refuse('the components of '//data_name//' have different sizes')
    return
  endif
  allocate(buffer(3, size(x)))
  buffer(1,:) = reshape(x, [size(x)])
  buffer(2,:) = reshape(y, [size(y)])
  buffer(3,:) = reshape(z, [size(z)])
  error = put_I2P(self, data_name, buffer, size(buffer, kind=I8P), 3_I4P)
  endfunction write_dataarray3_rank3_I2P

  function write_dataarray3_rank3_I1P(self, data_name, x, y, z, is_tuples) result(error)
  !< Write a data array (I1P) of 3 components given one by one: a dataset of the open group.
  class(vtkhdf_writer),       intent(inout)        :: self        !< Writer.
  character(*),               intent(in)           :: data_name   !< Data name.
  integer(I1P),               intent(in)           :: x(1:,1:,1:) !< X component of data variable.
  integer(I1P),               intent(in)           :: y(1:,1:,1:) !< Y component of data variable.
  integer(I1P),               intent(in)           :: z(1:,1:,1:) !< Z component of data variable.
  logical,                    intent(in), optional :: is_tuples   !< Use "NumberOfTuples" instead of "NumberOfComponents".
  integer(I4P)                                     :: error       !< Error status.
  integer(I1P), allocatable                        :: buffer(:,:) !< Components interleaved.

  if (size(y) /= size(x) .or. size(z) /= size(x)) then
    error = refuse('the components of '//data_name//' have different sizes')
    return
  endif
  allocate(buffer(3, size(x)))
  buffer(1,:) = reshape(x, [size(x)])
  buffer(2,:) = reshape(y, [size(y)])
  buffer(3,:) = reshape(z, [size(z)])
  error = put_I1P(self, data_name, buffer, size(buffer, kind=I8P), 3_I4P)
  endfunction write_dataarray3_rank3_I1P

  function write_dataarray6_rank1_R8P(self, data_name, u, v, w, x, y, z, is_tuples) result(error)
  !< Write a data array (R8P) of 6 components given one by one: a dataset of the open group.
  class(vtkhdf_writer),       intent(inout)        :: self        !< Writer.
  character(*),               intent(in)           :: data_name   !< Data name.
  real(R8P),                  intent(in)           :: u(1:)       !< U component of data variable.
  real(R8P),                  intent(in)           :: v(1:)       !< V component of data variable.
  real(R8P),                  intent(in)           :: w(1:)       !< W component of data variable.
  real(R8P),                  intent(in)           :: x(1:)       !< X component of data variable.
  real(R8P),                  intent(in)           :: y(1:)       !< Y component of data variable.
  real(R8P),                  intent(in)           :: z(1:)       !< Z component of data variable.
  logical,                    intent(in), optional :: is_tuples   !< Use "NumberOfTuples" instead of "NumberOfComponents".
  integer(I4P)                                     :: error       !< Error status.
  real(R8P), allocatable                           :: buffer(:,:) !< Components interleaved.

  if (size(u) /= size(x) .or. size(v) /= size(x) .or. size(w) /= size(x) .or. size(y) /= size(x) .or. size(z) /= size(x)) then
    error = refuse('the components of '//data_name//' have different sizes')
    return
  endif
  allocate(buffer(6, size(x)))
  buffer(1,:) = u
  buffer(2,:) = v
  buffer(3,:) = w
  buffer(4,:) = x
  buffer(5,:) = y
  buffer(6,:) = z
  error = put_R8P(self, data_name, buffer, size(buffer, kind=I8P), 6_I4P)
  endfunction write_dataarray6_rank1_R8P

  function write_dataarray6_rank1_R4P(self, data_name, u, v, w, x, y, z, is_tuples) result(error)
  !< Write a data array (R4P) of 6 components given one by one: a dataset of the open group.
  class(vtkhdf_writer),       intent(inout)        :: self        !< Writer.
  character(*),               intent(in)           :: data_name   !< Data name.
  real(R4P),                  intent(in)           :: u(1:)       !< U component of data variable.
  real(R4P),                  intent(in)           :: v(1:)       !< V component of data variable.
  real(R4P),                  intent(in)           :: w(1:)       !< W component of data variable.
  real(R4P),                  intent(in)           :: x(1:)       !< X component of data variable.
  real(R4P),                  intent(in)           :: y(1:)       !< Y component of data variable.
  real(R4P),                  intent(in)           :: z(1:)       !< Z component of data variable.
  logical,                    intent(in), optional :: is_tuples   !< Use "NumberOfTuples" instead of "NumberOfComponents".
  integer(I4P)                                     :: error       !< Error status.
  real(R4P), allocatable                           :: buffer(:,:) !< Components interleaved.

  if (size(u) /= size(x) .or. size(v) /= size(x) .or. size(w) /= size(x) .or. size(y) /= size(x) .or. size(z) /= size(x)) then
    error = refuse('the components of '//data_name//' have different sizes')
    return
  endif
  allocate(buffer(6, size(x)))
  buffer(1,:) = u
  buffer(2,:) = v
  buffer(3,:) = w
  buffer(4,:) = x
  buffer(5,:) = y
  buffer(6,:) = z
  error = put_R4P(self, data_name, buffer, size(buffer, kind=I8P), 6_I4P)
  endfunction write_dataarray6_rank1_R4P

  function write_dataarray6_rank1_I8P(self, data_name, u, v, w, x, y, z, is_tuples) result(error)
  !< Write a data array (I8P) of 6 components given one by one: a dataset of the open group.
  class(vtkhdf_writer),       intent(inout)        :: self        !< Writer.
  character(*),               intent(in)           :: data_name   !< Data name.
  integer(I8P),               intent(in)           :: u(1:)       !< U component of data variable.
  integer(I8P),               intent(in)           :: v(1:)       !< V component of data variable.
  integer(I8P),               intent(in)           :: w(1:)       !< W component of data variable.
  integer(I8P),               intent(in)           :: x(1:)       !< X component of data variable.
  integer(I8P),               intent(in)           :: y(1:)       !< Y component of data variable.
  integer(I8P),               intent(in)           :: z(1:)       !< Z component of data variable.
  logical,                    intent(in), optional :: is_tuples   !< Use "NumberOfTuples" instead of "NumberOfComponents".
  integer(I4P)                                     :: error       !< Error status.
  integer(I8P), allocatable                        :: buffer(:,:) !< Components interleaved.

  if (size(u) /= size(x) .or. size(v) /= size(x) .or. size(w) /= size(x) .or. size(y) /= size(x) .or. size(z) /= size(x)) then
    error = refuse('the components of '//data_name//' have different sizes')
    return
  endif
  allocate(buffer(6, size(x)))
  buffer(1,:) = u
  buffer(2,:) = v
  buffer(3,:) = w
  buffer(4,:) = x
  buffer(5,:) = y
  buffer(6,:) = z
  error = put_I8P(self, data_name, buffer, size(buffer, kind=I8P), 6_I4P)
  endfunction write_dataarray6_rank1_I8P

  function write_dataarray6_rank1_I4P(self, data_name, u, v, w, x, y, z, is_tuples) result(error)
  !< Write a data array (I4P) of 6 components given one by one: a dataset of the open group.
  class(vtkhdf_writer),       intent(inout)        :: self        !< Writer.
  character(*),               intent(in)           :: data_name   !< Data name.
  integer(I4P),               intent(in)           :: u(1:)       !< U component of data variable.
  integer(I4P),               intent(in)           :: v(1:)       !< V component of data variable.
  integer(I4P),               intent(in)           :: w(1:)       !< W component of data variable.
  integer(I4P),               intent(in)           :: x(1:)       !< X component of data variable.
  integer(I4P),               intent(in)           :: y(1:)       !< Y component of data variable.
  integer(I4P),               intent(in)           :: z(1:)       !< Z component of data variable.
  logical,                    intent(in), optional :: is_tuples   !< Use "NumberOfTuples" instead of "NumberOfComponents".
  integer(I4P)                                     :: error       !< Error status.
  integer(I4P), allocatable                        :: buffer(:,:) !< Components interleaved.

  if (size(u) /= size(x) .or. size(v) /= size(x) .or. size(w) /= size(x) .or. size(y) /= size(x) .or. size(z) /= size(x)) then
    error = refuse('the components of '//data_name//' have different sizes')
    return
  endif
  allocate(buffer(6, size(x)))
  buffer(1,:) = u
  buffer(2,:) = v
  buffer(3,:) = w
  buffer(4,:) = x
  buffer(5,:) = y
  buffer(6,:) = z
  error = put_I4P(self, data_name, buffer, size(buffer, kind=I8P), 6_I4P)
  endfunction write_dataarray6_rank1_I4P

  function write_dataarray6_rank1_I2P(self, data_name, u, v, w, x, y, z, is_tuples) result(error)
  !< Write a data array (I2P) of 6 components given one by one: a dataset of the open group.
  class(vtkhdf_writer),       intent(inout)        :: self        !< Writer.
  character(*),               intent(in)           :: data_name   !< Data name.
  integer(I2P),               intent(in)           :: u(1:)       !< U component of data variable.
  integer(I2P),               intent(in)           :: v(1:)       !< V component of data variable.
  integer(I2P),               intent(in)           :: w(1:)       !< W component of data variable.
  integer(I2P),               intent(in)           :: x(1:)       !< X component of data variable.
  integer(I2P),               intent(in)           :: y(1:)       !< Y component of data variable.
  integer(I2P),               intent(in)           :: z(1:)       !< Z component of data variable.
  logical,                    intent(in), optional :: is_tuples   !< Use "NumberOfTuples" instead of "NumberOfComponents".
  integer(I4P)                                     :: error       !< Error status.
  integer(I2P), allocatable                        :: buffer(:,:) !< Components interleaved.

  if (size(u) /= size(x) .or. size(v) /= size(x) .or. size(w) /= size(x) .or. size(y) /= size(x) .or. size(z) /= size(x)) then
    error = refuse('the components of '//data_name//' have different sizes')
    return
  endif
  allocate(buffer(6, size(x)))
  buffer(1,:) = u
  buffer(2,:) = v
  buffer(3,:) = w
  buffer(4,:) = x
  buffer(5,:) = y
  buffer(6,:) = z
  error = put_I2P(self, data_name, buffer, size(buffer, kind=I8P), 6_I4P)
  endfunction write_dataarray6_rank1_I2P

  function write_dataarray6_rank1_I1P(self, data_name, u, v, w, x, y, z, is_tuples) result(error)
  !< Write a data array (I1P) of 6 components given one by one: a dataset of the open group.
  class(vtkhdf_writer),       intent(inout)        :: self        !< Writer.
  character(*),               intent(in)           :: data_name   !< Data name.
  integer(I1P),               intent(in)           :: u(1:)       !< U component of data variable.
  integer(I1P),               intent(in)           :: v(1:)       !< V component of data variable.
  integer(I1P),               intent(in)           :: w(1:)       !< W component of data variable.
  integer(I1P),               intent(in)           :: x(1:)       !< X component of data variable.
  integer(I1P),               intent(in)           :: y(1:)       !< Y component of data variable.
  integer(I1P),               intent(in)           :: z(1:)       !< Z component of data variable.
  logical,                    intent(in), optional :: is_tuples   !< Use "NumberOfTuples" instead of "NumberOfComponents".
  integer(I4P)                                     :: error       !< Error status.
  integer(I1P), allocatable                        :: buffer(:,:) !< Components interleaved.

  if (size(u) /= size(x) .or. size(v) /= size(x) .or. size(w) /= size(x) .or. size(y) /= size(x) .or. size(z) /= size(x)) then
    error = refuse('the components of '//data_name//' have different sizes')
    return
  endif
  allocate(buffer(6, size(x)))
  buffer(1,:) = u
  buffer(2,:) = v
  buffer(3,:) = w
  buffer(4,:) = x
  buffer(5,:) = y
  buffer(6,:) = z
  error = put_I1P(self, data_name, buffer, size(buffer, kind=I8P), 6_I4P)
  endfunction write_dataarray6_rank1_I1P

  function write_dataarray6_rank3_R8P(self, data_name, u, v, w, x, y, z, is_tuples) result(error)
  !< Write a data array (R8P) of 6 components given one by one: a dataset of the open group.
  class(vtkhdf_writer),       intent(inout)        :: self        !< Writer.
  character(*),               intent(in)           :: data_name   !< Data name.
  real(R8P),                  intent(in)           :: u(1:,1:,1:) !< U component of data variable.
  real(R8P),                  intent(in)           :: v(1:,1:,1:) !< V component of data variable.
  real(R8P),                  intent(in)           :: w(1:,1:,1:) !< W component of data variable.
  real(R8P),                  intent(in)           :: x(1:,1:,1:) !< X component of data variable.
  real(R8P),                  intent(in)           :: y(1:,1:,1:) !< Y component of data variable.
  real(R8P),                  intent(in)           :: z(1:,1:,1:) !< Z component of data variable.
  logical,                    intent(in), optional :: is_tuples   !< Use "NumberOfTuples" instead of "NumberOfComponents".
  integer(I4P)                                     :: error       !< Error status.
  real(R8P), allocatable                           :: buffer(:,:) !< Components interleaved.

  if (size(u) /= size(x) .or. size(v) /= size(x) .or. size(w) /= size(x) .or. size(y) /= size(x) .or. size(z) /= size(x)) then
    error = refuse('the components of '//data_name//' have different sizes')
    return
  endif
  allocate(buffer(6, size(x)))
  buffer(1,:) = reshape(u, [size(u)])
  buffer(2,:) = reshape(v, [size(v)])
  buffer(3,:) = reshape(w, [size(w)])
  buffer(4,:) = reshape(x, [size(x)])
  buffer(5,:) = reshape(y, [size(y)])
  buffer(6,:) = reshape(z, [size(z)])
  error = put_R8P(self, data_name, buffer, size(buffer, kind=I8P), 6_I4P)
  endfunction write_dataarray6_rank3_R8P

  function write_dataarray6_rank3_R4P(self, data_name, u, v, w, x, y, z, is_tuples) result(error)
  !< Write a data array (R4P) of 6 components given one by one: a dataset of the open group.
  class(vtkhdf_writer),       intent(inout)        :: self        !< Writer.
  character(*),               intent(in)           :: data_name   !< Data name.
  real(R4P),                  intent(in)           :: u(1:,1:,1:) !< U component of data variable.
  real(R4P),                  intent(in)           :: v(1:,1:,1:) !< V component of data variable.
  real(R4P),                  intent(in)           :: w(1:,1:,1:) !< W component of data variable.
  real(R4P),                  intent(in)           :: x(1:,1:,1:) !< X component of data variable.
  real(R4P),                  intent(in)           :: y(1:,1:,1:) !< Y component of data variable.
  real(R4P),                  intent(in)           :: z(1:,1:,1:) !< Z component of data variable.
  logical,                    intent(in), optional :: is_tuples   !< Use "NumberOfTuples" instead of "NumberOfComponents".
  integer(I4P)                                     :: error       !< Error status.
  real(R4P), allocatable                           :: buffer(:,:) !< Components interleaved.

  if (size(u) /= size(x) .or. size(v) /= size(x) .or. size(w) /= size(x) .or. size(y) /= size(x) .or. size(z) /= size(x)) then
    error = refuse('the components of '//data_name//' have different sizes')
    return
  endif
  allocate(buffer(6, size(x)))
  buffer(1,:) = reshape(u, [size(u)])
  buffer(2,:) = reshape(v, [size(v)])
  buffer(3,:) = reshape(w, [size(w)])
  buffer(4,:) = reshape(x, [size(x)])
  buffer(5,:) = reshape(y, [size(y)])
  buffer(6,:) = reshape(z, [size(z)])
  error = put_R4P(self, data_name, buffer, size(buffer, kind=I8P), 6_I4P)
  endfunction write_dataarray6_rank3_R4P

  function write_dataarray6_rank3_I8P(self, data_name, u, v, w, x, y, z, is_tuples) result(error)
  !< Write a data array (I8P) of 6 components given one by one: a dataset of the open group.
  class(vtkhdf_writer),       intent(inout)        :: self        !< Writer.
  character(*),               intent(in)           :: data_name   !< Data name.
  integer(I8P),               intent(in)           :: u(1:,1:,1:) !< U component of data variable.
  integer(I8P),               intent(in)           :: v(1:,1:,1:) !< V component of data variable.
  integer(I8P),               intent(in)           :: w(1:,1:,1:) !< W component of data variable.
  integer(I8P),               intent(in)           :: x(1:,1:,1:) !< X component of data variable.
  integer(I8P),               intent(in)           :: y(1:,1:,1:) !< Y component of data variable.
  integer(I8P),               intent(in)           :: z(1:,1:,1:) !< Z component of data variable.
  logical,                    intent(in), optional :: is_tuples   !< Use "NumberOfTuples" instead of "NumberOfComponents".
  integer(I4P)                                     :: error       !< Error status.
  integer(I8P), allocatable                        :: buffer(:,:) !< Components interleaved.

  if (size(u) /= size(x) .or. size(v) /= size(x) .or. size(w) /= size(x) .or. size(y) /= size(x) .or. size(z) /= size(x)) then
    error = refuse('the components of '//data_name//' have different sizes')
    return
  endif
  allocate(buffer(6, size(x)))
  buffer(1,:) = reshape(u, [size(u)])
  buffer(2,:) = reshape(v, [size(v)])
  buffer(3,:) = reshape(w, [size(w)])
  buffer(4,:) = reshape(x, [size(x)])
  buffer(5,:) = reshape(y, [size(y)])
  buffer(6,:) = reshape(z, [size(z)])
  error = put_I8P(self, data_name, buffer, size(buffer, kind=I8P), 6_I4P)
  endfunction write_dataarray6_rank3_I8P

  function write_dataarray6_rank3_I4P(self, data_name, u, v, w, x, y, z, is_tuples) result(error)
  !< Write a data array (I4P) of 6 components given one by one: a dataset of the open group.
  class(vtkhdf_writer),       intent(inout)        :: self        !< Writer.
  character(*),               intent(in)           :: data_name   !< Data name.
  integer(I4P),               intent(in)           :: u(1:,1:,1:) !< U component of data variable.
  integer(I4P),               intent(in)           :: v(1:,1:,1:) !< V component of data variable.
  integer(I4P),               intent(in)           :: w(1:,1:,1:) !< W component of data variable.
  integer(I4P),               intent(in)           :: x(1:,1:,1:) !< X component of data variable.
  integer(I4P),               intent(in)           :: y(1:,1:,1:) !< Y component of data variable.
  integer(I4P),               intent(in)           :: z(1:,1:,1:) !< Z component of data variable.
  logical,                    intent(in), optional :: is_tuples   !< Use "NumberOfTuples" instead of "NumberOfComponents".
  integer(I4P)                                     :: error       !< Error status.
  integer(I4P), allocatable                        :: buffer(:,:) !< Components interleaved.

  if (size(u) /= size(x) .or. size(v) /= size(x) .or. size(w) /= size(x) .or. size(y) /= size(x) .or. size(z) /= size(x)) then
    error = refuse('the components of '//data_name//' have different sizes')
    return
  endif
  allocate(buffer(6, size(x)))
  buffer(1,:) = reshape(u, [size(u)])
  buffer(2,:) = reshape(v, [size(v)])
  buffer(3,:) = reshape(w, [size(w)])
  buffer(4,:) = reshape(x, [size(x)])
  buffer(5,:) = reshape(y, [size(y)])
  buffer(6,:) = reshape(z, [size(z)])
  error = put_I4P(self, data_name, buffer, size(buffer, kind=I8P), 6_I4P)
  endfunction write_dataarray6_rank3_I4P

  function write_dataarray6_rank3_I2P(self, data_name, u, v, w, x, y, z, is_tuples) result(error)
  !< Write a data array (I2P) of 6 components given one by one: a dataset of the open group.
  class(vtkhdf_writer),       intent(inout)        :: self        !< Writer.
  character(*),               intent(in)           :: data_name   !< Data name.
  integer(I2P),               intent(in)           :: u(1:,1:,1:) !< U component of data variable.
  integer(I2P),               intent(in)           :: v(1:,1:,1:) !< V component of data variable.
  integer(I2P),               intent(in)           :: w(1:,1:,1:) !< W component of data variable.
  integer(I2P),               intent(in)           :: x(1:,1:,1:) !< X component of data variable.
  integer(I2P),               intent(in)           :: y(1:,1:,1:) !< Y component of data variable.
  integer(I2P),               intent(in)           :: z(1:,1:,1:) !< Z component of data variable.
  logical,                    intent(in), optional :: is_tuples   !< Use "NumberOfTuples" instead of "NumberOfComponents".
  integer(I4P)                                     :: error       !< Error status.
  integer(I2P), allocatable                        :: buffer(:,:) !< Components interleaved.

  if (size(u) /= size(x) .or. size(v) /= size(x) .or. size(w) /= size(x) .or. size(y) /= size(x) .or. size(z) /= size(x)) then
    error = refuse('the components of '//data_name//' have different sizes')
    return
  endif
  allocate(buffer(6, size(x)))
  buffer(1,:) = reshape(u, [size(u)])
  buffer(2,:) = reshape(v, [size(v)])
  buffer(3,:) = reshape(w, [size(w)])
  buffer(4,:) = reshape(x, [size(x)])
  buffer(5,:) = reshape(y, [size(y)])
  buffer(6,:) = reshape(z, [size(z)])
  error = put_I2P(self, data_name, buffer, size(buffer, kind=I8P), 6_I4P)
  endfunction write_dataarray6_rank3_I2P

  function write_dataarray6_rank3_I1P(self, data_name, u, v, w, x, y, z, is_tuples) result(error)
  !< Write a data array (I1P) of 6 components given one by one: a dataset of the open group.
  class(vtkhdf_writer),       intent(inout)        :: self        !< Writer.
  character(*),               intent(in)           :: data_name   !< Data name.
  integer(I1P),               intent(in)           :: u(1:,1:,1:) !< U component of data variable.
  integer(I1P),               intent(in)           :: v(1:,1:,1:) !< V component of data variable.
  integer(I1P),               intent(in)           :: w(1:,1:,1:) !< W component of data variable.
  integer(I1P),               intent(in)           :: x(1:,1:,1:) !< X component of data variable.
  integer(I1P),               intent(in)           :: y(1:,1:,1:) !< Y component of data variable.
  integer(I1P),               intent(in)           :: z(1:,1:,1:) !< Z component of data variable.
  logical,                    intent(in), optional :: is_tuples   !< Use "NumberOfTuples" instead of "NumberOfComponents".
  integer(I4P)                                     :: error       !< Error status.
  integer(I1P), allocatable                        :: buffer(:,:) !< Components interleaved.

  if (size(u) /= size(x) .or. size(v) /= size(x) .or. size(w) /= size(x) .or. size(y) /= size(x) .or. size(z) /= size(x)) then
    error = refuse('the components of '//data_name//' have different sizes')
    return
  endif
  allocate(buffer(6, size(x)))
  buffer(1,:) = reshape(u, [size(u)])
  buffer(2,:) = reshape(v, [size(v)])
  buffer(3,:) = reshape(w, [size(w)])
  buffer(4,:) = reshape(x, [size(x)])
  buffer(5,:) = reshape(y, [size(y)])
  buffer(6,:) = reshape(z, [size(z)])
  error = put_I1P(self, data_name, buffer, size(buffer, kind=I8P), 6_I4P)
  endfunction write_dataarray6_rank3_I1P

  function write_dataarray_unsigned_I8P(self, data_name, x) result(error)
  !< Write a data array of unsigned values (the bits of I8P): a dataset of the open group.
  class(vtkhdf_writer),       intent(inout) :: self
  character(*),               intent(in)    :: data_name
  integer(I8P),               intent(in)    :: x(1:)
  integer(I4P)                              :: error

  error = put_I8P(self, data_name, x, size(x, kind=I8P), 1_I4P, unsigned=.true.)
  endfunction write_dataarray_unsigned_I8P

  function write_dataarray_unsigned_I4P(self, data_name, x) result(error)
  !< Write a data array of unsigned values (the bits of I4P): a dataset of the open group.
  class(vtkhdf_writer),       intent(inout) :: self
  character(*),               intent(in)    :: data_name
  integer(I4P),               intent(in)    :: x(1:)
  integer(I4P)                              :: error

  error = put_I4P(self, data_name, x, size(x, kind=I8P), 1_I4P, unsigned=.true.)
  endfunction write_dataarray_unsigned_I4P

  function write_dataarray_unsigned_I2P(self, data_name, x) result(error)
  !< Write a data array of unsigned values (the bits of I2P): a dataset of the open group.
  class(vtkhdf_writer),       intent(inout) :: self
  character(*),               intent(in)    :: data_name
  integer(I2P),               intent(in)    :: x(1:)
  integer(I4P)                              :: error

  error = put_I2P(self, data_name, x, size(x, kind=I8P), 1_I4P, unsigned=.true.)
  endfunction write_dataarray_unsigned_I2P

  function write_dataarray_unsigned_I1P(self, data_name, x) result(error)
  !< Write a data array of unsigned values (the bits of I1P): a dataset of the open group.
  class(vtkhdf_writer),       intent(inout) :: self
  character(*),               intent(in)    :: data_name
  integer(I1P),               intent(in)    :: x(1:)
  integer(I4P)                              :: error

  error = put_I1P(self, data_name, x, size(x, kind=I8P), 1_I4P, unsigned=.true.)
  endfunction write_dataarray_unsigned_I1P

  function write_geo_unst_data1_rank2_R8P(self, np, nc, xyz) result(error)
  !< Write the points of the piece of an UnstructuredGrid or PolyData (R8P), `xyz(1:3,1:np)`.
  class(vtkhdf_writer),       intent(inout) :: self
  integer(I4P),               intent(in)    :: np
  integer(I4P),               intent(in)    :: nc
  real(R8P),                  intent(in)    :: xyz(1:,1:)
  integer(I4P)                              :: error

  error = put_points_R8P(self, int(np, I8P), int(nc, I8P), xyz, size(xyz, dim=1, kind=I8P), size(xyz, dim=2, kind=I8P))
  endfunction write_geo_unst_data1_rank2_R8P

  function write_geo_unst_data3_rank1_R8P(self, np, nc, x, y, z) result(error)
  !< Write the points of the piece of an UnstructuredGrid or PolyData (R8P), by coordinate.
  class(vtkhdf_writer),       intent(inout) :: self
  integer(I4P),               intent(in)    :: np
  integer(I4P),               intent(in)    :: nc
  real(R8P),                  intent(in)    :: x(1:)
  real(R8P),                  intent(in)    :: y(1:)
  real(R8P),                  intent(in)    :: z(1:)
  integer(I4P)                              :: error
  real(R8P), allocatable                    :: buffer(:,:) !< Coordinates interleaved.

  if (size(y) /= size(x) .or. size(z) /= size(x)) then
    error = refuse('write_geo: x, y and z have different sizes')
    return
  endif
  allocate(buffer(3, size(x)))
  buffer(1,:) = x
  buffer(2,:) = y
  buffer(3,:) = z
  error = put_points_R8P(self, int(np, I8P), int(nc, I8P), buffer, 3_I8P, size(x, kind=I8P))
  endfunction write_geo_unst_data3_rank1_R8P

  function write_geo_unst_data1_rank2_R8P_I8P(self, np, nc, xyz) result(error)
  !< Write the points of the piece of an UnstructuredGrid or PolyData (R8P), `xyz(1:3,1:np)`.
  class(vtkhdf_writer),       intent(inout) :: self
  integer(I8P),               intent(in)    :: np
  integer(I8P),               intent(in)    :: nc
  real(R8P),                  intent(in)    :: xyz(1:,1:)
  integer(I4P)                              :: error

  error = put_points_R8P(self, np, nc, xyz, size(xyz, dim=1, kind=I8P), size(xyz, dim=2, kind=I8P))
  endfunction write_geo_unst_data1_rank2_R8P_I8P

  function write_geo_unst_data3_rank1_R8P_I8P(self, np, nc, x, y, z) result(error)
  !< Write the points of the piece of an UnstructuredGrid or PolyData (R8P), by coordinate.
  class(vtkhdf_writer),       intent(inout) :: self
  integer(I8P),               intent(in)    :: np
  integer(I8P),               intent(in)    :: nc
  real(R8P),                  intent(in)    :: x(1:)
  real(R8P),                  intent(in)    :: y(1:)
  real(R8P),                  intent(in)    :: z(1:)
  integer(I4P)                              :: error
  real(R8P), allocatable                    :: buffer(:,:) !< Coordinates interleaved.

  if (size(y) /= size(x) .or. size(z) /= size(x)) then
    error = refuse('write_geo: x, y and z have different sizes')
    return
  endif
  allocate(buffer(3, size(x)))
  buffer(1,:) = x
  buffer(2,:) = y
  buffer(3,:) = z
  error = put_points_R8P(self, np, nc, buffer, 3_I8P, size(x, kind=I8P))
  endfunction write_geo_unst_data3_rank1_R8P_I8P

  function write_geo_unst_data1_rank2_R4P(self, np, nc, xyz) result(error)
  !< Write the points of the piece of an UnstructuredGrid or PolyData (R4P), `xyz(1:3,1:np)`.
  class(vtkhdf_writer),       intent(inout) :: self
  integer(I4P),               intent(in)    :: np
  integer(I4P),               intent(in)    :: nc
  real(R4P),                  intent(in)    :: xyz(1:,1:)
  integer(I4P)                              :: error

  error = put_points_R4P(self, int(np, I8P), int(nc, I8P), xyz, size(xyz, dim=1, kind=I8P), size(xyz, dim=2, kind=I8P))
  endfunction write_geo_unst_data1_rank2_R4P

  function write_geo_unst_data3_rank1_R4P(self, np, nc, x, y, z) result(error)
  !< Write the points of the piece of an UnstructuredGrid or PolyData (R4P), by coordinate.
  class(vtkhdf_writer),       intent(inout) :: self
  integer(I4P),               intent(in)    :: np
  integer(I4P),               intent(in)    :: nc
  real(R4P),                  intent(in)    :: x(1:)
  real(R4P),                  intent(in)    :: y(1:)
  real(R4P),                  intent(in)    :: z(1:)
  integer(I4P)                              :: error
  real(R4P), allocatable                    :: buffer(:,:) !< Coordinates interleaved.

  if (size(y) /= size(x) .or. size(z) /= size(x)) then
    error = refuse('write_geo: x, y and z have different sizes')
    return
  endif
  allocate(buffer(3, size(x)))
  buffer(1,:) = x
  buffer(2,:) = y
  buffer(3,:) = z
  error = put_points_R4P(self, int(np, I8P), int(nc, I8P), buffer, 3_I8P, size(x, kind=I8P))
  endfunction write_geo_unst_data3_rank1_R4P

  function write_geo_unst_data1_rank2_R4P_I8P(self, np, nc, xyz) result(error)
  !< Write the points of the piece of an UnstructuredGrid or PolyData (R4P), `xyz(1:3,1:np)`.
  class(vtkhdf_writer),       intent(inout) :: self
  integer(I8P),               intent(in)    :: np
  integer(I8P),               intent(in)    :: nc
  real(R4P),                  intent(in)    :: xyz(1:,1:)
  integer(I4P)                              :: error

  error = put_points_R4P(self, np, nc, xyz, size(xyz, dim=1, kind=I8P), size(xyz, dim=2, kind=I8P))
  endfunction write_geo_unst_data1_rank2_R4P_I8P

  function write_geo_unst_data3_rank1_R4P_I8P(self, np, nc, x, y, z) result(error)
  !< Write the points of the piece of an UnstructuredGrid or PolyData (R4P), by coordinate.
  class(vtkhdf_writer),       intent(inout) :: self
  integer(I8P),               intent(in)    :: np
  integer(I8P),               intent(in)    :: nc
  real(R4P),                  intent(in)    :: x(1:)
  real(R4P),                  intent(in)    :: y(1:)
  real(R4P),                  intent(in)    :: z(1:)
  integer(I4P)                              :: error
  real(R4P), allocatable                    :: buffer(:,:) !< Coordinates interleaved.

  if (size(y) /= size(x) .or. size(z) /= size(x)) then
    error = refuse('write_geo: x, y and z have different sizes')
    return
  endif
  allocate(buffer(3, size(x)))
  buffer(1,:) = x
  buffer(2,:) = y
  buffer(3,:) = z
  error = put_points_R4P(self, np, nc, buffer, 3_I8P, size(x, kind=I8P))
  endfunction write_geo_unst_data3_rank1_R4P_I8P

  function write_connectivity_I4P(self, nc, connectivity, offset, cell_type, face, faceoffset) result(error)
  !< Write the cells of the piece of an UnstructuredGrid (I4P ids): connectivity, offsets (VTKFortran end offsets,
  !< written from 0 as VTKHDF wants them), types, and the faces of its polyhedra (`face`, `faceoffset`, checked before
  !< anything is written).
  class(vtkhdf_writer),       intent(inout) :: self
  integer(I4P),               intent(in)    :: nc
  integer(I4P),               intent(in)    :: connectivity(1:)
  integer(I4P),               intent(in)    :: offset(1:)
  integer(I4P),   optional,   intent(in)    :: face(1:)
  integer(I4P),   optional,   intent(in)    :: faceoffset(1:)
  integer(I1P),               intent(in)    :: cell_type(1:)
  integer(I4P)                              :: error

  if (present(face) .neqv. present(faceoffset)) then
    error = refuse('write_connectivity: face and faceoffset go together')
    return
  endif
  if (present(face)) then
    error = put_faces_I4P(self, int(nc, I8P), face, faceoffset, cell_type, .true.)
    if (error /= 0_I4P) return
  elseif (any(cell_type == CELL_POLYHEDRON)) then
    error = refuse('write_connectivity: a polyhedron (cell type 42) needs face and faceoffset')
    return
  endif
  error = put_cells_I4P(self, int(nc, I8P), connectivity, offset, cell_type)
  if (error /= 0_I4P) return
  if (present(face)) error = put_faces_I4P(self, int(nc, I8P), face, faceoffset, cell_type, .false.)
  endfunction write_connectivity_I4P

  function write_connectivity_I8P(self, nc, connectivity, offset, cell_type, face, faceoffset) result(error)
  !< Write the cells of the piece of an UnstructuredGrid (I8P ids): connectivity, offsets (VTKFortran end offsets,
  !< written from 0 as VTKHDF wants them), types, and the faces of its polyhedra (`face`, `faceoffset`, checked before
  !< anything is written).
  class(vtkhdf_writer),       intent(inout) :: self
  integer(I8P),               intent(in)    :: nc
  integer(I8P),               intent(in)    :: connectivity(1:)
  integer(I8P),               intent(in)    :: offset(1:)
  integer(I8P),   optional,   intent(in)    :: face(1:)
  integer(I8P),   optional,   intent(in)    :: faceoffset(1:)
  integer(I1P),               intent(in)    :: cell_type(1:)
  integer(I4P)                              :: error

  if (present(face) .neqv. present(faceoffset)) then
    error = refuse('write_connectivity: face and faceoffset go together')
    return
  endif
  if (present(face)) then
    error = put_faces_I8P(self, int(nc, I8P), face, faceoffset, cell_type, .true.)
    if (error /= 0_I4P) return
  elseif (any(cell_type == CELL_POLYHEDRON)) then
    error = refuse('write_connectivity: a polyhedron (cell type 42) needs face and faceoffset')
    return
  endif
  error = put_cells_I8P(self, int(nc, I8P), connectivity, offset, cell_type)
  if (error /= 0_I4P) return
  if (present(face)) error = put_faces_I8P(self, int(nc, I8P), face, faceoffset, cell_type, .false.)
  endfunction write_connectivity_I8P

  function write_piece_start_tag_unst(self, np, nc) result(error)
  !< Start a piece of an UnstructuredGrid: a partition of the file.
  class(vtkhdf_writer),       intent(inout) :: self
  integer(I4P),               intent(in)    :: np
  integer(I4P),               intent(in)    :: nc
  integer(I4P)                              :: error

  error = self%start_partition(int(np, I8P), int(nc, I8P))
  endfunction write_piece_start_tag_unst

  function write_piece_start_tag_unst_I8P(self, np, nc) result(error)
  !< Start a piece of an UnstructuredGrid (I8P counts): a partition of the file.
  class(vtkhdf_writer),       intent(inout) :: self
  integer(I8P),               intent(in)    :: np
  integer(I8P),               intent(in)    :: nc
  integer(I4P)                              :: error

  error = self%start_partition(np, nc)
  endfunction write_piece_start_tag_unst_I8P

  function write_piece_start_tag_poly(self, np, nverts, nlines, nstrips, npolys) result(error)
  !< Start a piece of a PolyData: a partition of the file, with the cells of each block.
  class(vtkhdf_writer),       intent(inout) :: self
  integer(I4P),               intent(in)    :: np
  integer(I4P),               intent(in)    :: nverts
  integer(I4P),               intent(in)    :: nlines
  integer(I4P),               intent(in)    :: nstrips
  integer(I4P),               intent(in)    :: npolys
  integer(I4P)                              :: error

  error = self%start_partition(int(np, I8P), int(nverts, I8P) + int(nlines, I8P) + int(nstrips, I8P) + int(npolys, I8P), &
                               [int(nverts, I8P), int(nlines, I8P), int(nstrips, I8P), int(npolys, I8P)])
  endfunction write_piece_start_tag_poly

  function write_piece_start_tag_poly_I8P(self, np, nverts, nlines, nstrips, npolys) result(error)
  !< Start a piece of a PolyData (I8P counts): a partition of the file, with the cells of each block.
  class(vtkhdf_writer),       intent(inout) :: self
  integer(I8P),               intent(in)    :: np
  integer(I8P),               intent(in)    :: nverts
  integer(I8P),               intent(in)    :: nlines
  integer(I8P),               intent(in)    :: nstrips
  integer(I8P),               intent(in)    :: npolys
  integer(I4P)                              :: error

  error = self%start_partition(np, nverts + nlines + nstrips + npolys, [nverts, nlines, nstrips, npolys])
  endfunction write_piece_start_tag_poly_I8P

  function write_polydata_cells(self, verts_connectivity, verts_offset, lines_connectivity, lines_offset, &
      strips_connectivity, strips_offset, polys_connectivity, polys_offset) result(error)
  !< Write the cells of the piece of a PolyData: each block given (connectivity and end offsets, I4P or I8P) in its
  !< group (`Vertices`, `Lines`, `Strips`, `Polygons`), offsets from 0; a block not given has no cells (zero entries).
  !< Every block is checked before any is written.
  class(vtkhdf_writer),       intent(inout)        :: self
  class(*),                   intent(in), optional :: verts_connectivity(1:)
  class(*),                   intent(in), optional :: verts_offset(1:)
  class(*),                   intent(in), optional :: lines_connectivity(1:)
  class(*),                   intent(in), optional :: lines_offset(1:)
  class(*),                   intent(in), optional :: strips_connectivity(1:)
  class(*),                   intent(in), optional :: strips_offset(1:)
  class(*),                   intent(in), optional :: polys_connectivity(1:)
  class(*),                   intent(in), optional :: polys_offset(1:)
  integer(I4P)                                     :: error

  error = 1_I4P
  if (self%topology /= TOPOLOGY_POLY) then
    error = refuse('write_polydata_cells is for a PolyData')
  elseif (.not.self%is_piece_open .or. self%group >= 0_hid_kind) then
    error = refuse('write_polydata_cells: open the piece first, outside the point and cell data')
  elseif (self%is_connectivity_written) then
    error = refuse('write_polydata_cells: the cells of this piece are already written')
  elseif ((present(verts_connectivity) .neqv. present(verts_offset)) .or. &
          (present(lines_connectivity) .neqv. present(lines_offset)) .or. &
          (present(strips_connectivity) .neqv. present(strips_offset)) .or. &
          (present(polys_connectivity) .neqv. present(polys_offset))) then
    error = refuse('write_polydata_cells: each block needs its connectivity and its offsets')
  else
    error = self%poly_blocks(.true., verts_connectivity, verts_offset, lines_connectivity, lines_offset, &
                            strips_connectivity, strips_offset, polys_connectivity, polys_offset)
    error = self%agree(error) ! with comm, a refusal on every rank before the collective writes
    if (error == 0_I4P) error = self%poly_blocks(.false., verts_connectivity, verts_offset, lines_connectivity, lines_offset, &
                                                 strips_connectivity, strips_offset, polys_connectivity, polys_offset)
    if (error /= 0_I4P) error = refuse('cannot write the cells of the PolyData piece')
    if (error == 0_I4P) self%is_connectivity_written = .true.
  endif
  endfunction write_polydata_cells

  function write_geo_rect_data3_rank1_R8P(self, x, y, z) result(error)
  !< Write the coordinates of a RectilinearGrid (R8P): XCoordinates, YCoordinates, ZCoordinates.
  class(vtkhdf_writer),       intent(inout) :: self
  real(R8P),                  intent(in)    :: x(1:)
  real(R8P),                  intent(in)    :: y(1:)
  real(R8P),                  intent(in)    :: z(1:)
  integer(I4P)                              :: error

  error = put_coordinates_R8P(self, x, y, z)
  endfunction write_geo_rect_data3_rank1_R8P

  function write_geo_strg_data1_rank2_R8P(self, xyz) result(error)
  !< Write the points of a StructuredGrid (R8P), `xyz(1:3,1:n)`, x fastest: the dataset Points.
  class(vtkhdf_writer),       intent(inout) :: self
  real(R8P),                  intent(in)    :: xyz(1:,1:)
  integer(I4P)                              :: error

  error = put_grid_points_R8P(self, xyz, size(xyz, dim=1, kind=I8P), size(xyz, dim=2, kind=I8P))
  endfunction write_geo_strg_data1_rank2_R8P

  function write_geo_strg_data1_rank4_R8P(self, xyz) result(error)
  !< Write the points of a StructuredGrid (R8P), `xyz(1:3,nx1:nx2,ny1:ny2,nz1:nz2)`: the dataset Points.
  class(vtkhdf_writer),       intent(inout) :: self
  real(R8P),                  intent(in)    :: xyz(1:,1:,1:,1:)
  integer(I4P)                              :: error
  integer(I8P)                              :: grid(3)          !< Points along x, y, z (of the region of this rank, with comm).

  grid = self%grid_points()
  if (self%is_parallel) then ! the points of the region of this rank
    grid = max(int(self%region(2::2) - self%region(1::2), I8P) + 1_I8P, 0_I8P)
    if (any(self%region(2::2) < self%region(1::2))) grid = 0_I8P
  endif
  error = 0_I4P
  if (any(shape(xyz, kind=I8P) /= [3_I8P, grid])) then
    error = refuse('write_geo: xyz is not 3 by the points of the '//trim(merge('region', 'grid  ', self%is_parallel))// &
                   ' along x, y and z')
  endif
  error = self%agree(error) ! with comm, a refusal on every rank before the collective writes
  if (error /= 0_I4P) return
  error = put_grid_points_R8P(self, xyz, size(xyz, dim=1, kind=I8P), size(xyz, kind=I8P) / 3_I8P)
  endfunction write_geo_strg_data1_rank4_R8P

  function write_geo_strg_data3_rank1_R8P(self, n, x, y, z) result(error)
  !< Write the points of a StructuredGrid (R8P), by coordinate, x fastest: the dataset Points.
  class(vtkhdf_writer),       intent(inout) :: self
  integer(I4P),               intent(in)    :: n
  real(R8P),                  intent(in)    :: x(1:)
  real(R8P),                  intent(in)    :: y(1:)
  real(R8P),                  intent(in)    :: z(1:)
  integer(I4P)                              :: error
  real(R8P), allocatable                    :: buffer(:,:) !< Coordinates interleaved.

  if (size(y) /= size(x) .or. size(z) /= size(x) .or. int(n, I8P) /= size(x, kind=I8P)) then
    error = refuse('write_geo: x, y and z must have n values each')
    return
  endif
  allocate(buffer(3, size(x)))
  buffer(1,:) = x
  buffer(2,:) = y
  buffer(3,:) = z
  error = put_grid_points_R8P(self, buffer, 3_I8P, size(x, kind=I8P))
  endfunction write_geo_strg_data3_rank1_R8P

  function write_geo_strg_data3_rank3_R8P(self, n, x, y, z) result(error)
  !< Write the points of a StructuredGrid (R8P), by coordinate, x fastest: the dataset Points.
  class(vtkhdf_writer),       intent(inout) :: self
  integer(I4P),               intent(in)    :: n
  real(R8P),                  intent(in)    :: x(1:,1:,1:)
  real(R8P),                  intent(in)    :: y(1:,1:,1:)
  real(R8P),                  intent(in)    :: z(1:,1:,1:)
  integer(I4P)                              :: error
  real(R8P), allocatable                    :: buffer(:,:) !< Coordinates interleaved.

  if (size(y) /= size(x) .or. size(z) /= size(x) .or. int(n, I8P) /= size(x, kind=I8P)) then
    error = refuse('write_geo: x, y and z must have n values each')
    return
  endif
  allocate(buffer(3, size(x)))
  buffer(1,:) = reshape(x, [size(x)])
  buffer(2,:) = reshape(y, [size(y)])
  buffer(3,:) = reshape(z, [size(z)])
  error = put_grid_points_R8P(self, buffer, 3_I8P, size(x, kind=I8P))
  endfunction write_geo_strg_data3_rank3_R8P

  function write_geo_rect_data3_rank1_R4P(self, x, y, z) result(error)
  !< Write the coordinates of a RectilinearGrid (R4P): XCoordinates, YCoordinates, ZCoordinates.
  class(vtkhdf_writer),       intent(inout) :: self
  real(R4P),                  intent(in)    :: x(1:)
  real(R4P),                  intent(in)    :: y(1:)
  real(R4P),                  intent(in)    :: z(1:)
  integer(I4P)                              :: error

  error = put_coordinates_R4P(self, x, y, z)
  endfunction write_geo_rect_data3_rank1_R4P

  function write_geo_strg_data1_rank2_R4P(self, xyz) result(error)
  !< Write the points of a StructuredGrid (R4P), `xyz(1:3,1:n)`, x fastest: the dataset Points.
  class(vtkhdf_writer),       intent(inout) :: self
  real(R4P),                  intent(in)    :: xyz(1:,1:)
  integer(I4P)                              :: error

  error = put_grid_points_R4P(self, xyz, size(xyz, dim=1, kind=I8P), size(xyz, dim=2, kind=I8P))
  endfunction write_geo_strg_data1_rank2_R4P

  function write_geo_strg_data1_rank4_R4P(self, xyz) result(error)
  !< Write the points of a StructuredGrid (R4P), `xyz(1:3,nx1:nx2,ny1:ny2,nz1:nz2)`: the dataset Points.
  class(vtkhdf_writer),       intent(inout) :: self
  real(R4P),                  intent(in)    :: xyz(1:,1:,1:,1:)
  integer(I4P)                              :: error
  integer(I8P)                              :: grid(3)          !< Points along x, y, z (of the region of this rank, with comm).

  grid = self%grid_points()
  if (self%is_parallel) then ! the points of the region of this rank
    grid = max(int(self%region(2::2) - self%region(1::2), I8P) + 1_I8P, 0_I8P)
    if (any(self%region(2::2) < self%region(1::2))) grid = 0_I8P
  endif
  error = 0_I4P
  if (any(shape(xyz, kind=I8P) /= [3_I8P, grid])) then
    error = refuse('write_geo: xyz is not 3 by the points of the '//trim(merge('region', 'grid  ', self%is_parallel))// &
                   ' along x, y and z')
  endif
  error = self%agree(error) ! with comm, a refusal on every rank before the collective writes
  if (error /= 0_I4P) return
  error = put_grid_points_R4P(self, xyz, size(xyz, dim=1, kind=I8P), size(xyz, kind=I8P) / 3_I8P)
  endfunction write_geo_strg_data1_rank4_R4P

  function write_geo_strg_data3_rank1_R4P(self, n, x, y, z) result(error)
  !< Write the points of a StructuredGrid (R4P), by coordinate, x fastest: the dataset Points.
  class(vtkhdf_writer),       intent(inout) :: self
  integer(I4P),               intent(in)    :: n
  real(R4P),                  intent(in)    :: x(1:)
  real(R4P),                  intent(in)    :: y(1:)
  real(R4P),                  intent(in)    :: z(1:)
  integer(I4P)                              :: error
  real(R4P), allocatable                    :: buffer(:,:) !< Coordinates interleaved.

  if (size(y) /= size(x) .or. size(z) /= size(x) .or. int(n, I8P) /= size(x, kind=I8P)) then
    error = refuse('write_geo: x, y and z must have n values each')
    return
  endif
  allocate(buffer(3, size(x)))
  buffer(1,:) = x
  buffer(2,:) = y
  buffer(3,:) = z
  error = put_grid_points_R4P(self, buffer, 3_I8P, size(x, kind=I8P))
  endfunction write_geo_strg_data3_rank1_R4P

  function write_geo_strg_data3_rank3_R4P(self, n, x, y, z) result(error)
  !< Write the points of a StructuredGrid (R4P), by coordinate, x fastest: the dataset Points.
  class(vtkhdf_writer),       intent(inout) :: self
  integer(I4P),               intent(in)    :: n
  real(R4P),                  intent(in)    :: x(1:,1:,1:)
  real(R4P),                  intent(in)    :: y(1:,1:,1:)
  real(R4P),                  intent(in)    :: z(1:,1:,1:)
  integer(I4P)                              :: error
  real(R4P), allocatable                    :: buffer(:,:) !< Coordinates interleaved.

  if (size(y) /= size(x) .or. size(z) /= size(x) .or. int(n, I8P) /= size(x, kind=I8P)) then
    error = refuse('write_geo: x, y and z must have n values each')
    return
  endif
  allocate(buffer(3, size(x)))
  buffer(1,:) = reshape(x, [size(x)])
  buffer(2,:) = reshape(y, [size(y)])
  buffer(3,:) = reshape(z, [size(z)])
  error = put_grid_points_R4P(self, buffer, 3_I8P, size(x, kind=I8P))
  endfunction write_geo_strg_data3_rank3_R4P

  ! private helpers
  function put_R8P(self, data_name, x, n, n_components) result(error)
  !< Write `n` values (R8P) as the dataset `data_name` of the open group: in an UnstructuredGrid or PolyData, created by
  !< the first piece and appended to by the others.
  class(vtkhdf_writer), intent(inout) :: self                      !< Writer.
  character(*), intent(in)            :: data_name                 !< Data name.
  real(R8P), intent(in)               :: x(*)                      !< Values.
  integer(I8P), intent(in)            :: n                         !< Number of values.
  integer(I4P), intent(in)            :: n_components              !< Number of components.
  integer(I4P)                        :: error                     !< Error status.
  integer(I8P), allocatable           :: dims(:)                   !< Shape of the dataset, Fortran order.
  logical                             :: is_new                    !< The dataset is created (not appended to).
  integer(I8P), allocatable           :: mem_dims(:), mem_start(:) !< Grid region: values, block start.
  integer(I8P), allocatable           :: file_start(:), count(:)   !< Grid region: block start, shape.

  error = self%dataset_dims(data_name, n, n_components, dims)
  error = self%agree(error) ! with comm, a refusal on every rank before the collective writes
  if (error /= 0_I4P) return
  is_new = .not.h5_link_exists(self%group, data_name)
  if (self%is_temporal .and. is_new .and. self%n_steps > 1_I8P) then
    error = refuse('the array '//data_name//' is not in the first step: every step writes the same arrays')
    return
  endif
  if (self%is_partitioned() .and. self%group_name /= 'FieldData') then
    if (is_new .and. self%n_pieces > 1_I4P) then
      error = refuse('the array '//data_name//' is not in the first piece: every piece writes the same arrays')
      return
    endif
    error = store_R8P(self, self%group, data_name, x, dims)
  elseif (self%is_parallel .and. self%group_name /= 'FieldData') then ! a grid: the block of this rank
    call self%region_block(self%group_name == 'CellData', n_components, dims, mem_dims, mem_start, &
                           file_start, count)
    error = h5_dataset_slab_R8P(self%group, data_name, x, dims, mem_dims, mem_start, file_start, count, .true.)
  elseif (self%is_temporal .and. self%group_name /= 'FieldData') then ! a grid: the time axis last
    error = store_R8P(self, self%group, data_name, x, [dims, 1_I8P])
  elseif (self%is_temporal) then
    error = store_R8P(self, self%group, data_name, x, dims)
  else
    error = h5_dataset_write_R8P(self%group, data_name, x, dims, deflate=self%deflate_level())
  endif
  if (error /= 0_I4P) error = refuse('cannot write the dataset '//data_name)
  if (error == 0_I4P .and. is_new) then
    error = self%mark_active(data_name)
    call self%record_array(data_name)
  endif
  endfunction put_R8P

  function put_R4P(self, data_name, x, n, n_components) result(error)
  !< Write `n` values (R4P) as the dataset `data_name` of the open group: in an UnstructuredGrid or PolyData, created by
  !< the first piece and appended to by the others.
  class(vtkhdf_writer), intent(inout) :: self                      !< Writer.
  character(*), intent(in)            :: data_name                 !< Data name.
  real(R4P), intent(in)               :: x(*)                      !< Values.
  integer(I8P), intent(in)            :: n                         !< Number of values.
  integer(I4P), intent(in)            :: n_components              !< Number of components.
  integer(I4P)                        :: error                     !< Error status.
  integer(I8P), allocatable           :: dims(:)                   !< Shape of the dataset, Fortran order.
  logical                             :: is_new                    !< The dataset is created (not appended to).
  integer(I8P), allocatable           :: mem_dims(:), mem_start(:) !< Grid region: values, block start.
  integer(I8P), allocatable           :: file_start(:), count(:)   !< Grid region: block start, shape.

  error = self%dataset_dims(data_name, n, n_components, dims)
  error = self%agree(error) ! with comm, a refusal on every rank before the collective writes
  if (error /= 0_I4P) return
  is_new = .not.h5_link_exists(self%group, data_name)
  if (self%is_temporal .and. is_new .and. self%n_steps > 1_I8P) then
    error = refuse('the array '//data_name//' is not in the first step: every step writes the same arrays')
    return
  endif
  if (self%is_partitioned() .and. self%group_name /= 'FieldData') then
    if (is_new .and. self%n_pieces > 1_I4P) then
      error = refuse('the array '//data_name//' is not in the first piece: every piece writes the same arrays')
      return
    endif
    error = store_R4P(self, self%group, data_name, x, dims)
  elseif (self%is_parallel .and. self%group_name /= 'FieldData') then ! a grid: the block of this rank
    call self%region_block(self%group_name == 'CellData', n_components, dims, mem_dims, mem_start, &
                           file_start, count)
    error = h5_dataset_slab_R4P(self%group, data_name, x, dims, mem_dims, mem_start, file_start, count, .true.)
  elseif (self%is_temporal .and. self%group_name /= 'FieldData') then ! a grid: the time axis last
    error = store_R4P(self, self%group, data_name, x, [dims, 1_I8P])
  elseif (self%is_temporal) then
    error = store_R4P(self, self%group, data_name, x, dims)
  else
    error = h5_dataset_write_R4P(self%group, data_name, x, dims, deflate=self%deflate_level())
  endif
  if (error /= 0_I4P) error = refuse('cannot write the dataset '//data_name)
  if (error == 0_I4P .and. is_new) then
    error = self%mark_active(data_name)
    call self%record_array(data_name)
  endif
  endfunction put_R4P

  function put_I8P(self, data_name, x, n, n_components, unsigned) result(error)
  !< Write `n` values (I8P) as the dataset `data_name` of the open group: in an UnstructuredGrid or PolyData, created by
  !< the first piece and appended to by the others.
  class(vtkhdf_writer), intent(inout) :: self                      !< Writer.
  character(*), intent(in)            :: data_name                 !< Data name.
  integer(I8P), intent(in)            :: x(*)                      !< Values.
  integer(I8P), intent(in)            :: n                         !< Number of values.
  integer(I4P), intent(in)            :: n_components              !< Number of components.
  logical, intent(in), optional       :: unsigned                  !< Unsigned values.
  integer(I4P)                        :: error                     !< Error status.
  integer(I8P), allocatable           :: dims(:)                   !< Shape of the dataset, Fortran order.
  logical                             :: is_new                    !< The dataset is created (not appended to).
  integer(I8P), allocatable           :: mem_dims(:), mem_start(:) !< Grid region: values, block start.
  integer(I8P), allocatable           :: file_start(:), count(:)   !< Grid region: block start, shape.

  error = self%dataset_dims(data_name, n, n_components, dims)
  error = self%agree(error) ! with comm, a refusal on every rank before the collective writes
  if (error /= 0_I4P) return
  is_new = .not.h5_link_exists(self%group, data_name)
  if (self%is_temporal .and. is_new .and. self%n_steps > 1_I8P) then
    error = refuse('the array '//data_name//' is not in the first step: every step writes the same arrays')
    return
  endif
  if (self%is_partitioned() .and. self%group_name /= 'FieldData') then
    if (is_new .and. self%n_pieces > 1_I4P) then
      error = refuse('the array '//data_name//' is not in the first piece: every piece writes the same arrays')
      return
    endif
    error = store_I8P(self, self%group, data_name, x, dims, unsigned=unsigned)
  elseif (self%is_parallel .and. self%group_name /= 'FieldData') then ! a grid: the block of this rank
    call self%region_block(self%group_name == 'CellData', n_components, dims, mem_dims, mem_start, &
                           file_start, count)
    error = h5_dataset_slab_I8P(self%group, data_name, x, dims, mem_dims, mem_start, file_start, count, .true., unsigned=unsigned)
  elseif (self%is_temporal .and. self%group_name /= 'FieldData') then ! a grid: the time axis last
    error = store_I8P(self, self%group, data_name, x, [dims, 1_I8P], unsigned=unsigned)
  elseif (self%is_temporal) then
    error = store_I8P(self, self%group, data_name, x, dims, unsigned=unsigned)
  else
    error = h5_dataset_write_I8P(self%group, data_name, x, dims, deflate=self%deflate_level(), unsigned=unsigned)
  endif
  if (error /= 0_I4P) error = refuse('cannot write the dataset '//data_name)
  if (error == 0_I4P .and. is_new) then
    error = self%mark_active(data_name)
    call self%record_array(data_name)
  endif
  endfunction put_I8P

  function put_I4P(self, data_name, x, n, n_components, unsigned) result(error)
  !< Write `n` values (I4P) as the dataset `data_name` of the open group: in an UnstructuredGrid or PolyData, created by
  !< the first piece and appended to by the others.
  class(vtkhdf_writer), intent(inout) :: self                      !< Writer.
  character(*), intent(in)            :: data_name                 !< Data name.
  integer(I4P), intent(in)            :: x(*)                      !< Values.
  integer(I8P), intent(in)            :: n                         !< Number of values.
  integer(I4P), intent(in)            :: n_components              !< Number of components.
  logical, intent(in), optional       :: unsigned                  !< Unsigned values.
  integer(I4P)                        :: error                     !< Error status.
  integer(I8P), allocatable           :: dims(:)                   !< Shape of the dataset, Fortran order.
  logical                             :: is_new                    !< The dataset is created (not appended to).
  integer(I8P), allocatable           :: mem_dims(:), mem_start(:) !< Grid region: values, block start.
  integer(I8P), allocatable           :: file_start(:), count(:)   !< Grid region: block start, shape.

  error = self%dataset_dims(data_name, n, n_components, dims)
  error = self%agree(error) ! with comm, a refusal on every rank before the collective writes
  if (error /= 0_I4P) return
  is_new = .not.h5_link_exists(self%group, data_name)
  if (self%is_temporal .and. is_new .and. self%n_steps > 1_I8P) then
    error = refuse('the array '//data_name//' is not in the first step: every step writes the same arrays')
    return
  endif
  if (self%is_partitioned() .and. self%group_name /= 'FieldData') then
    if (is_new .and. self%n_pieces > 1_I4P) then
      error = refuse('the array '//data_name//' is not in the first piece: every piece writes the same arrays')
      return
    endif
    error = store_I4P(self, self%group, data_name, x, dims, unsigned=unsigned)
  elseif (self%is_parallel .and. self%group_name /= 'FieldData') then ! a grid: the block of this rank
    call self%region_block(self%group_name == 'CellData', n_components, dims, mem_dims, mem_start, &
                           file_start, count)
    error = h5_dataset_slab_I4P(self%group, data_name, x, dims, mem_dims, mem_start, file_start, count, .true., unsigned=unsigned)
  elseif (self%is_temporal .and. self%group_name /= 'FieldData') then ! a grid: the time axis last
    error = store_I4P(self, self%group, data_name, x, [dims, 1_I8P], unsigned=unsigned)
  elseif (self%is_temporal) then
    error = store_I4P(self, self%group, data_name, x, dims, unsigned=unsigned)
  else
    error = h5_dataset_write_I4P(self%group, data_name, x, dims, deflate=self%deflate_level(), unsigned=unsigned)
  endif
  if (error /= 0_I4P) error = refuse('cannot write the dataset '//data_name)
  if (error == 0_I4P .and. is_new) then
    error = self%mark_active(data_name)
    call self%record_array(data_name)
  endif
  endfunction put_I4P

  function put_I2P(self, data_name, x, n, n_components, unsigned) result(error)
  !< Write `n` values (I2P) as the dataset `data_name` of the open group: in an UnstructuredGrid or PolyData, created by
  !< the first piece and appended to by the others.
  class(vtkhdf_writer), intent(inout) :: self                      !< Writer.
  character(*), intent(in)            :: data_name                 !< Data name.
  integer(I2P), intent(in)            :: x(*)                      !< Values.
  integer(I8P), intent(in)            :: n                         !< Number of values.
  integer(I4P), intent(in)            :: n_components              !< Number of components.
  logical, intent(in), optional       :: unsigned                  !< Unsigned values.
  integer(I4P)                        :: error                     !< Error status.
  integer(I8P), allocatable           :: dims(:)                   !< Shape of the dataset, Fortran order.
  logical                             :: is_new                    !< The dataset is created (not appended to).
  integer(I8P), allocatable           :: mem_dims(:), mem_start(:) !< Grid region: values, block start.
  integer(I8P), allocatable           :: file_start(:), count(:)   !< Grid region: block start, shape.

  error = self%dataset_dims(data_name, n, n_components, dims)
  error = self%agree(error) ! with comm, a refusal on every rank before the collective writes
  if (error /= 0_I4P) return
  is_new = .not.h5_link_exists(self%group, data_name)
  if (self%is_temporal .and. is_new .and. self%n_steps > 1_I8P) then
    error = refuse('the array '//data_name//' is not in the first step: every step writes the same arrays')
    return
  endif
  if (self%is_partitioned() .and. self%group_name /= 'FieldData') then
    if (is_new .and. self%n_pieces > 1_I4P) then
      error = refuse('the array '//data_name//' is not in the first piece: every piece writes the same arrays')
      return
    endif
    error = store_I2P(self, self%group, data_name, x, dims, unsigned=unsigned)
  elseif (self%is_parallel .and. self%group_name /= 'FieldData') then ! a grid: the block of this rank
    call self%region_block(self%group_name == 'CellData', n_components, dims, mem_dims, mem_start, &
                           file_start, count)
    error = h5_dataset_slab_I2P(self%group, data_name, x, dims, mem_dims, mem_start, file_start, count, .true., unsigned=unsigned)
  elseif (self%is_temporal .and. self%group_name /= 'FieldData') then ! a grid: the time axis last
    error = store_I2P(self, self%group, data_name, x, [dims, 1_I8P], unsigned=unsigned)
  elseif (self%is_temporal) then
    error = store_I2P(self, self%group, data_name, x, dims, unsigned=unsigned)
  else
    error = h5_dataset_write_I2P(self%group, data_name, x, dims, deflate=self%deflate_level(), unsigned=unsigned)
  endif
  if (error /= 0_I4P) error = refuse('cannot write the dataset '//data_name)
  if (error == 0_I4P .and. is_new) then
    error = self%mark_active(data_name)
    call self%record_array(data_name)
  endif
  endfunction put_I2P

  function put_I1P(self, data_name, x, n, n_components, unsigned) result(error)
  !< Write `n` values (I1P) as the dataset `data_name` of the open group: in an UnstructuredGrid or PolyData, created by
  !< the first piece and appended to by the others.
  class(vtkhdf_writer), intent(inout) :: self                      !< Writer.
  character(*), intent(in)            :: data_name                 !< Data name.
  integer(I1P), intent(in)            :: x(*)                      !< Values.
  integer(I8P), intent(in)            :: n                         !< Number of values.
  integer(I4P), intent(in)            :: n_components              !< Number of components.
  logical, intent(in), optional       :: unsigned                  !< Unsigned values.
  integer(I4P)                        :: error                     !< Error status.
  integer(I8P), allocatable           :: dims(:)                   !< Shape of the dataset, Fortran order.
  logical                             :: is_new                    !< The dataset is created (not appended to).
  integer(I8P), allocatable           :: mem_dims(:), mem_start(:) !< Grid region: values, block start.
  integer(I8P), allocatable           :: file_start(:), count(:)   !< Grid region: block start, shape.

  error = self%dataset_dims(data_name, n, n_components, dims)
  error = self%agree(error) ! with comm, a refusal on every rank before the collective writes
  if (error /= 0_I4P) return
  is_new = .not.h5_link_exists(self%group, data_name)
  if (self%is_temporal .and. is_new .and. self%n_steps > 1_I8P) then
    error = refuse('the array '//data_name//' is not in the first step: every step writes the same arrays')
    return
  endif
  if (self%is_partitioned() .and. self%group_name /= 'FieldData') then
    if (is_new .and. self%n_pieces > 1_I4P) then
      error = refuse('the array '//data_name//' is not in the first piece: every piece writes the same arrays')
      return
    endif
    error = store_I1P(self, self%group, data_name, x, dims, unsigned=unsigned)
  elseif (self%is_parallel .and. self%group_name /= 'FieldData') then ! a grid: the block of this rank
    call self%region_block(self%group_name == 'CellData', n_components, dims, mem_dims, mem_start, &
                           file_start, count)
    error = h5_dataset_slab_I1P(self%group, data_name, x, dims, mem_dims, mem_start, file_start, count, .true., unsigned=unsigned)
  elseif (self%is_temporal .and. self%group_name /= 'FieldData') then ! a grid: the time axis last
    error = store_I1P(self, self%group, data_name, x, [dims, 1_I8P], unsigned=unsigned)
  elseif (self%is_temporal) then
    error = store_I1P(self, self%group, data_name, x, dims, unsigned=unsigned)
  else
    error = h5_dataset_write_I1P(self%group, data_name, x, dims, deflate=self%deflate_level(), unsigned=unsigned)
  endif
  if (error /= 0_I4P) error = refuse('cannot write the dataset '//data_name)
  if (error == 0_I4P .and. is_new) then
    error = self%mark_active(data_name)
    call self%record_array(data_name)
  endif
  endfunction put_I1P

  function store_R8P(self, loc_id, name, x, dims) result(error)
  !< Create the appendable dataset `name` (R8P) with `x`, or append `x` to it if it exists; with a communicator, collective.
  class(vtkhdf_writer), intent(in) :: self    !< Writer.
  integer(hid_kind), intent(in)    :: loc_id  !< Parent group.
  character(*), intent(in)         :: name    !< Dataset name.
  real(R8P), intent(in)            :: x(*)    !< Values.
  integer(I8P), intent(in)         :: dims(:) !< Shape of the values, Fortran order (the last dimension grows).
  integer(I4P)                     :: error   !< Error status.

  if (self%is_parallel) then ! every rank, its rows after those of the ranks before
    if (h5_link_exists(loc_id, name)) then
      error = h5_dataset_append_R8P(loc_id, name, x, dims, comm=self%comm)
    else
      error = h5_dataset_write_R8P(loc_id, name, x, dims, appendable=.true., comm=self%comm)
    endif
  elseif (h5_link_exists(loc_id, name)) then
    error = h5_dataset_append_R8P(loc_id, name, x, dims)
  else
    error = h5_dataset_write_R8P(loc_id, name, x, dims, appendable=.true., deflate=self%deflate_level())
  endif
  endfunction store_R8P

  function store_R4P(self, loc_id, name, x, dims) result(error)
  !< Create the appendable dataset `name` (R4P) with `x`, or append `x` to it if it exists; with a communicator, collective.
  class(vtkhdf_writer), intent(in) :: self    !< Writer.
  integer(hid_kind), intent(in)    :: loc_id  !< Parent group.
  character(*), intent(in)         :: name    !< Dataset name.
  real(R4P), intent(in)            :: x(*)    !< Values.
  integer(I8P), intent(in)         :: dims(:) !< Shape of the values, Fortran order (the last dimension grows).
  integer(I4P)                     :: error   !< Error status.

  if (self%is_parallel) then ! every rank, its rows after those of the ranks before
    if (h5_link_exists(loc_id, name)) then
      error = h5_dataset_append_R4P(loc_id, name, x, dims, comm=self%comm)
    else
      error = h5_dataset_write_R4P(loc_id, name, x, dims, appendable=.true., comm=self%comm)
    endif
  elseif (h5_link_exists(loc_id, name)) then
    error = h5_dataset_append_R4P(loc_id, name, x, dims)
  else
    error = h5_dataset_write_R4P(loc_id, name, x, dims, appendable=.true., deflate=self%deflate_level())
  endif
  endfunction store_R4P

  function store_I8P(self, loc_id, name, x, dims, unsigned) result(error)
  !< Create the appendable dataset `name` (I8P) with `x`, or append `x` to it if it exists; with a communicator, collective.
  class(vtkhdf_writer), intent(in) :: self     !< Writer.
  integer(hid_kind), intent(in)    :: loc_id   !< Parent group.
  character(*), intent(in)         :: name     !< Dataset name.
  integer(I8P), intent(in)         :: x(*)     !< Values.
  integer(I8P), intent(in)         :: dims(:)  !< Shape of the values, Fortran order (the last dimension grows).
  logical, intent(in), optional    :: unsigned !< Unsigned values.
  integer(I4P)                     :: error    !< Error status.

  if (self%is_parallel) then ! every rank, its rows after those of the ranks before
    if (h5_link_exists(loc_id, name)) then
      error = h5_dataset_append_I8P(loc_id, name, x, dims, unsigned=unsigned, comm=self%comm)
    else
      error = h5_dataset_write_I8P(loc_id, name, x, dims, appendable=.true., unsigned=unsigned, comm=self%comm)
    endif
  elseif (h5_link_exists(loc_id, name)) then
    error = h5_dataset_append_I8P(loc_id, name, x, dims, unsigned=unsigned)
  else
    error = h5_dataset_write_I8P(loc_id, name, x, dims, appendable=.true., deflate=self%deflate_level(), unsigned=unsigned)
  endif
  endfunction store_I8P

  function store_I4P(self, loc_id, name, x, dims, unsigned) result(error)
  !< Create the appendable dataset `name` (I4P) with `x`, or append `x` to it if it exists; with a communicator, collective.
  class(vtkhdf_writer), intent(in) :: self     !< Writer.
  integer(hid_kind), intent(in)    :: loc_id   !< Parent group.
  character(*), intent(in)         :: name     !< Dataset name.
  integer(I4P), intent(in)         :: x(*)     !< Values.
  integer(I8P), intent(in)         :: dims(:)  !< Shape of the values, Fortran order (the last dimension grows).
  logical, intent(in), optional    :: unsigned !< Unsigned values.
  integer(I4P)                     :: error    !< Error status.

  if (self%is_parallel) then ! every rank, its rows after those of the ranks before
    if (h5_link_exists(loc_id, name)) then
      error = h5_dataset_append_I4P(loc_id, name, x, dims, unsigned=unsigned, comm=self%comm)
    else
      error = h5_dataset_write_I4P(loc_id, name, x, dims, appendable=.true., unsigned=unsigned, comm=self%comm)
    endif
  elseif (h5_link_exists(loc_id, name)) then
    error = h5_dataset_append_I4P(loc_id, name, x, dims, unsigned=unsigned)
  else
    error = h5_dataset_write_I4P(loc_id, name, x, dims, appendable=.true., deflate=self%deflate_level(), unsigned=unsigned)
  endif
  endfunction store_I4P

  function store_I2P(self, loc_id, name, x, dims, unsigned) result(error)
  !< Create the appendable dataset `name` (I2P) with `x`, or append `x` to it if it exists; with a communicator, collective.
  class(vtkhdf_writer), intent(in) :: self     !< Writer.
  integer(hid_kind), intent(in)    :: loc_id   !< Parent group.
  character(*), intent(in)         :: name     !< Dataset name.
  integer(I2P), intent(in)         :: x(*)     !< Values.
  integer(I8P), intent(in)         :: dims(:)  !< Shape of the values, Fortran order (the last dimension grows).
  logical, intent(in), optional    :: unsigned !< Unsigned values.
  integer(I4P)                     :: error    !< Error status.

  if (self%is_parallel) then ! every rank, its rows after those of the ranks before
    if (h5_link_exists(loc_id, name)) then
      error = h5_dataset_append_I2P(loc_id, name, x, dims, unsigned=unsigned, comm=self%comm)
    else
      error = h5_dataset_write_I2P(loc_id, name, x, dims, appendable=.true., unsigned=unsigned, comm=self%comm)
    endif
  elseif (h5_link_exists(loc_id, name)) then
    error = h5_dataset_append_I2P(loc_id, name, x, dims, unsigned=unsigned)
  else
    error = h5_dataset_write_I2P(loc_id, name, x, dims, appendable=.true., deflate=self%deflate_level(), unsigned=unsigned)
  endif
  endfunction store_I2P

  function store_I1P(self, loc_id, name, x, dims, unsigned) result(error)
  !< Create the appendable dataset `name` (I1P) with `x`, or append `x` to it if it exists; with a communicator, collective.
  class(vtkhdf_writer), intent(in) :: self     !< Writer.
  integer(hid_kind), intent(in)    :: loc_id   !< Parent group.
  character(*), intent(in)         :: name     !< Dataset name.
  integer(I1P), intent(in)         :: x(*)     !< Values.
  integer(I8P), intent(in)         :: dims(:)  !< Shape of the values, Fortran order (the last dimension grows).
  logical, intent(in), optional    :: unsigned !< Unsigned values.
  integer(I4P)                     :: error    !< Error status.

  if (self%is_parallel) then ! every rank, its rows after those of the ranks before
    if (h5_link_exists(loc_id, name)) then
      error = h5_dataset_append_I1P(loc_id, name, x, dims, unsigned=unsigned, comm=self%comm)
    else
      error = h5_dataset_write_I1P(loc_id, name, x, dims, appendable=.true., unsigned=unsigned, comm=self%comm)
    endif
  elseif (h5_link_exists(loc_id, name)) then
    error = h5_dataset_append_I1P(loc_id, name, x, dims, unsigned=unsigned)
  else
    error = h5_dataset_write_I1P(loc_id, name, x, dims, appendable=.true., deflate=self%deflate_level(), unsigned=unsigned)
  endif
  endfunction store_I1P

  function put_points_R8P(self, np, nc, xyz, n1, n2) result(error)
  !< Append the points (R8P) of the piece to the dataset Points ([np, 3] in C order).
  class(vtkhdf_writer), intent(inout) :: self   !< Writer.
  integer(I8P), intent(in)            :: np     !< Number of points.
  integer(I8P), intent(in)            :: nc     !< Number of cells.
  real(R8P), intent(in)               :: xyz(*) !< Coordinates, interleaved (x, y, z of each point).
  integer(I8P), intent(in)            :: n1     !< First dimension of the coordinates (3).
  integer(I8P), intent(in)            :: n2     !< Second dimension of the coordinates (np).
  integer(I4P)                        :: error  !< Error status.

  error = self%check_geo(np, nc, n1, n2)
  error = self%agree(error) ! with comm, a refusal on every rank before the collective writes
  if (error /= 0_I4P) return
  error = store_R8P(self, self%root, 'Points', xyz, [3_I8P, np])
  if (error /= 0_I4P) error = refuse('cannot write the points')
  if (error == 0_I4P) self%is_geo_written = .true.
  endfunction put_points_R8P

  function put_points_R4P(self, np, nc, xyz, n1, n2) result(error)
  !< Append the points (R4P) of the piece to the dataset Points ([np, 3] in C order).
  class(vtkhdf_writer), intent(inout) :: self   !< Writer.
  integer(I8P), intent(in)            :: np     !< Number of points.
  integer(I8P), intent(in)            :: nc     !< Number of cells.
  real(R4P), intent(in)               :: xyz(*) !< Coordinates, interleaved (x, y, z of each point).
  integer(I8P), intent(in)            :: n1     !< First dimension of the coordinates (3).
  integer(I8P), intent(in)            :: n2     !< Second dimension of the coordinates (np).
  integer(I4P)                        :: error  !< Error status.

  error = self%check_geo(np, nc, n1, n2)
  error = self%agree(error) ! with comm, a refusal on every rank before the collective writes
  if (error /= 0_I4P) return
  error = store_R4P(self, self%root, 'Points', xyz, [3_I8P, np])
  if (error /= 0_I4P) error = refuse('cannot write the points')
  if (error == 0_I4P) self%is_geo_written = .true.
  endfunction put_points_R4P

  function put_cells_I4P(self, nc, connectivity, offset, cell_type) result(error)
  !< Append the cells (I4P ids) of the piece: Connectivity, Offsets (from 0), Types, NumberOfConnectivityIds.
  class(vtkhdf_writer), intent(inout) :: self            !< Writer.
  integer(I8P), intent(in)            :: nc              !< Number of cells.
  integer(I4P), intent(in)            :: connectivity(:) !< Point ids of the cells, from 0 in the piece.
  integer(I4P), intent(in)            :: offset(:)       !< End offset of each cell in the connectivity.
  integer(I1P), intent(in)            :: cell_type(:)    !< VTK cell types.
  integer(I4P)                        :: error           !< Error status.
  integer(I4P), allocatable           :: offsets(:)      !< Offsets from 0 (nc+1 values).
  integer(I8P)                        :: n_ids           !< Ids used by the cells.

  error = self%check_cells(nc, size(offset, kind=I8P), size(cell_type, kind=I8P))
  n_ids = 0_I8P
  if (error == 0_I4P .and. nc > 0_I8P) n_ids = int(offset(nc), I8P)
  if (error == 0_I4P .and. (n_ids < 0_I8P .or. n_ids > size(connectivity, kind=I8P))) &
    error = refuse('write_connectivity: the last offset is not within the connectivity')
  error = self%agree(error) ! with comm, a refusal on every rank before the collective writes
  if (error /= 0_I4P) return
  allocate(offsets(nc+1_I8P))
  offsets(1) = 0_I4P
  if (nc > 0_I8P) offsets(2:) = offset(1:nc)
  error = store_I4P(self, self%root, 'Connectivity', connectivity, [n_ids])
  if (error == 0_I4P) error = store_I4P(self, self%root, 'Offsets', offsets, [nc+1_I8P])
  if (error == 0_I4P) error = store_I1P(self, self%root, 'Types', cell_type, [nc], unsigned=.true.)
  if (error == 0_I4P) error = store_I8P(self, self%root, 'NumberOfConnectivityIds', [n_ids], [1_I8P])
  if (error /= 0_I4P) error = refuse('cannot write the cells')
  if (error == 0_I4P) self%is_connectivity_written = .true.
  if (error == 0_I4P) self%totals(4) = self%totals(4) + n_ids
  endfunction put_cells_I4P

  function put_cells_I8P(self, nc, connectivity, offset, cell_type) result(error)
  !< Append the cells (I8P ids) of the piece: Connectivity, Offsets (from 0), Types, NumberOfConnectivityIds.
  class(vtkhdf_writer), intent(inout) :: self            !< Writer.
  integer(I8P), intent(in)            :: nc              !< Number of cells.
  integer(I8P), intent(in)            :: connectivity(:) !< Point ids of the cells, from 0 in the piece.
  integer(I8P), intent(in)            :: offset(:)       !< End offset of each cell in the connectivity.
  integer(I1P), intent(in)            :: cell_type(:)    !< VTK cell types.
  integer(I4P)                        :: error           !< Error status.
  integer(I8P), allocatable           :: offsets(:)      !< Offsets from 0 (nc+1 values).
  integer(I8P)                        :: n_ids           !< Ids used by the cells.

  error = self%check_cells(nc, size(offset, kind=I8P), size(cell_type, kind=I8P))
  n_ids = 0_I8P
  if (error == 0_I4P .and. nc > 0_I8P) n_ids = int(offset(nc), I8P)
  if (error == 0_I4P .and. (n_ids < 0_I8P .or. n_ids > size(connectivity, kind=I8P))) &
    error = refuse('write_connectivity: the last offset is not within the connectivity')
  error = self%agree(error) ! with comm, a refusal on every rank before the collective writes
  if (error /= 0_I4P) return
  allocate(offsets(nc+1_I8P))
  offsets(1) = 0_I8P
  if (nc > 0_I8P) offsets(2:) = offset(1:nc)
  error = store_I8P(self, self%root, 'Connectivity', connectivity, [n_ids])
  if (error == 0_I4P) error = store_I8P(self, self%root, 'Offsets', offsets, [nc+1_I8P])
  if (error == 0_I4P) error = store_I1P(self, self%root, 'Types', cell_type, [nc], unsigned=.true.)
  if (error == 0_I4P) error = store_I8P(self, self%root, 'NumberOfConnectivityIds', [n_ids], [1_I8P])
  if (error /= 0_I4P) error = refuse('cannot write the cells')
  if (error == 0_I4P) self%is_connectivity_written = .true.
  if (error == 0_I4P) self%totals(4) = self%totals(4) + n_ids
  endfunction put_cells_I8P

  function put_coordinates_R8P(self, x, y, z) result(error)
  !< Write the coordinates (R8P) of a RectilinearGrid: XCoordinates, YCoordinates, ZCoordinates.
  class(vtkhdf_writer), intent(inout) :: self    !< Writer.
  real(R8P), intent(in)               :: x(:)    !< X coordinates.
  real(R8P), intent(in)               :: y(:)    !< Y coordinates.
  real(R8P), intent(in)               :: z(:)    !< Z coordinates.
  integer(I4P)                        :: error   !< Error status.
  integer(I8P)                        :: grid(3) !< Points along x, y, z.

  error = self%check_grid_geo(TOPOLOGY_RECT)
  if (error /= 0_I4P) return
  grid = self%grid_points()
  if (self%is_parallel) then ! the coordinates of the region of this rank
    grid = max(int(self%region(2::2) - self%region(1::2), I8P) + 1_I8P, 0_I8P)
    if (any(self%region(2::2) < self%region(1::2))) grid = 0_I8P
  endif
  if (any([size(x, kind=I8P), size(y, kind=I8P), size(z, kind=I8P)] /= grid)) then
    error = refuse('write_geo: x, y and z need one value per point of the '// &
                   trim(merge('region', 'grid  ', self%is_parallel))//' along each axis')
  endif
  error = self%agree(error) ! with comm, a refusal on every rank before the collective writes
  if (error /= 0_I4P) return
  if (self%is_parallel) then ! every rank creates the datasets, writes the entries no rank before has
    error = put_axis_R8P(self, 'XCoordinates', 1_I4P, x)
    if (error == 0_I4P) error = put_axis_R8P(self, 'YCoordinates', 2_I4P, y)
    if (error == 0_I4P) error = put_axis_R8P(self, 'ZCoordinates', 3_I4P, z)
  elseif (self%is_temporal) then ! the coordinates of each step after those of the steps before
    error = store_R8P(self, self%root, 'XCoordinates', x, [grid(1)])
    if (error == 0_I4P) error = store_R8P(self, self%root, 'YCoordinates', y, [grid(2)])
    if (error == 0_I4P) error = store_R8P(self, self%root, 'ZCoordinates', z, [grid(3)])
  else
    error = h5_dataset_write_R8P(self%root, 'XCoordinates', x, [grid(1)], deflate=self%deflate_level())
    if (error == 0_I4P) error = h5_dataset_write_R8P(self%root, 'YCoordinates', y, [grid(2)], &
                                         deflate=self%deflate_level())
    if (error == 0_I4P) error = h5_dataset_write_R8P(self%root, 'ZCoordinates', z, [grid(3)], &
                                         deflate=self%deflate_level())
  endif
  if (error /= 0_I4P) error = refuse('cannot write the coordinates')
  if (error == 0_I4P) self%is_geo_written = .true.
  if (error == 0_I4P .and. self%is_temporal) self%geo_slices = self%geo_slices + 1_I8P
  endfunction put_coordinates_R8P

  function put_axis_R8P(self, name, a, values) result(error)
  !< Write the coordinates (R8P) of the axis `a` of a RectilinearGrid from the ranks of `comm`: the dataset created by
  !< every rank, then the entries of the region of this rank that no rank before has (collective).
  class(vtkhdf_writer), intent(in) :: self      !< Writer.
  character(*), intent(in)         :: name      !< Dataset of the coordinates.
  integer(I4P), intent(in)         :: a         !< Axis: 1 x, 2 y, 3 z.
  real(R8P), intent(in)            :: values(:) !< Coordinates of the region of this rank along the axis.
  integer(I4P)                     :: error     !< Error status.
  integer(I8P), allocatable        :: runs(:,:) !< Runs of entries written by this rank.
  integer(I8P)                     :: grid(3)   !< Points along x, y, z.
  integer                          :: r         !< Counter.

  grid = self%grid_points()
  error = h5_dataset_slab_R8P(self%root, name, values, [grid(a)], [size(values, kind=I8P)], [0_I8P], [0_I8P], &
                              [0_I8P], .true.)
  call self%axis_runs(a, runs)
  ! as many writes on every rank (the largest number of runs; empty ones beyond its own): the dataset is opened
  ! the same number of times by every rank (H5Dopen is collective with collective metadata operations)
  do r=1, int(h5_mpi_max(self%comm, size(runs, dim=2, kind=I8P)))
    if (r <= size(runs, dim=2)) then
      if (error == 0_I4P) error = h5_dataset_slab_R8P(self%root, name, values, [grid(a)], [size(values, kind=I8P)], &
                                                    runs(1:1, r), runs(2:2, r), runs(3:3, r), .false.)
    else
      if (error == 0_I4P) error = h5_dataset_slab_R8P(self%root, name, values, [grid(a)], [size(values, kind=I8P)], &
                                                    [0_I8P], [0_I8P], [0_I8P], .false.)
    endif
  enddo
  endfunction put_axis_R8P

  function put_grid_points_R8P(self, xyz, n1, n) result(error)
  !< Write the points (R8P) of a StructuredGrid: the dataset Points, `[k, j, i, 3]` in C order (always 4D, as VTK
  !< requires).
  class(vtkhdf_writer), intent(inout) :: self                    !< Writer.
  real(R8P), intent(in)               :: xyz(*)                  !< Coordinates, interleaved (x, y, z of each point), x fastest.
  integer(I8P), intent(in)            :: n1                      !< First dimension of the coordinates (3).
  integer(I8P), intent(in)            :: n                       !< Number of points.
  integer(I4P)                        :: error                   !< Error status.
  integer(I8P)                        :: grid(3)                 !< Points along x, y, z.
  integer(I8P)                        :: mem_dims(4)             !< Points of the region.
  integer(I8P)                        :: file_start(4), count(4) !< Block of the region.

  error = self%check_grid_geo(TOPOLOGY_STRG)
  if (error /= 0_I4P) return
  grid = self%grid_points()
  if (self%is_parallel) then ! the points of the region of this rank
    call self%points_block(mem_dims, file_start, count)
    grid = mem_dims(2:)
  endif
  if (n1 /= 3_I8P .or. n /= product(grid)) then
    error = refuse('write_geo: the points are not 3 coordinates by the '// &
                   trim(str(n=product(grid), no_sign=.true.))//' points of the '// &
                   trim(merge('region', 'grid  ', self%is_parallel)))
  endif
  error = self%agree(error) ! with comm, a refusal on every rank before the collective writes
  if (error /= 0_I4P) return
  if (self%is_parallel) then ! every rank creates Points, writes its block
    error = h5_dataset_slab_R8P(self%root, 'Points', xyz, [3_I8P, self%grid_points()], mem_dims, &
                                [0_I8P, 0_I8P, 0_I8P, 0_I8P], file_start, count, .true.)
  elseif (self%is_temporal) then ! the time axis last: Points [t, k, j, i, 3] in C order
    error = store_R8P(self, self%root, 'Points', xyz, [3_I8P, grid, 1_I8P])
  else
    error = h5_dataset_write_R8P(self%root, 'Points', xyz, [3_I8P, grid], deflate=self%deflate_level())
  endif
  if (error /= 0_I4P) error = refuse('cannot write the points')
  if (error == 0_I4P) self%is_geo_written = .true.
  if (error == 0_I4P .and. self%is_temporal) self%geo_slices = self%geo_slices + 1_I8P
  endfunction put_grid_points_R8P

  function put_coordinates_R4P(self, x, y, z) result(error)
  !< Write the coordinates (R4P) of a RectilinearGrid: XCoordinates, YCoordinates, ZCoordinates.
  class(vtkhdf_writer), intent(inout) :: self    !< Writer.
  real(R4P), intent(in)               :: x(:)    !< X coordinates.
  real(R4P), intent(in)               :: y(:)    !< Y coordinates.
  real(R4P), intent(in)               :: z(:)    !< Z coordinates.
  integer(I4P)                        :: error   !< Error status.
  integer(I8P)                        :: grid(3) !< Points along x, y, z.

  error = self%check_grid_geo(TOPOLOGY_RECT)
  if (error /= 0_I4P) return
  grid = self%grid_points()
  if (self%is_parallel) then ! the coordinates of the region of this rank
    grid = max(int(self%region(2::2) - self%region(1::2), I8P) + 1_I8P, 0_I8P)
    if (any(self%region(2::2) < self%region(1::2))) grid = 0_I8P
  endif
  if (any([size(x, kind=I8P), size(y, kind=I8P), size(z, kind=I8P)] /= grid)) then
    error = refuse('write_geo: x, y and z need one value per point of the '// &
                   trim(merge('region', 'grid  ', self%is_parallel))//' along each axis')
  endif
  error = self%agree(error) ! with comm, a refusal on every rank before the collective writes
  if (error /= 0_I4P) return
  if (self%is_parallel) then ! every rank creates the datasets, writes the entries no rank before has
    error = put_axis_R4P(self, 'XCoordinates', 1_I4P, x)
    if (error == 0_I4P) error = put_axis_R4P(self, 'YCoordinates', 2_I4P, y)
    if (error == 0_I4P) error = put_axis_R4P(self, 'ZCoordinates', 3_I4P, z)
  elseif (self%is_temporal) then ! the coordinates of each step after those of the steps before
    error = store_R4P(self, self%root, 'XCoordinates', x, [grid(1)])
    if (error == 0_I4P) error = store_R4P(self, self%root, 'YCoordinates', y, [grid(2)])
    if (error == 0_I4P) error = store_R4P(self, self%root, 'ZCoordinates', z, [grid(3)])
  else
    error = h5_dataset_write_R4P(self%root, 'XCoordinates', x, [grid(1)], deflate=self%deflate_level())
    if (error == 0_I4P) error = h5_dataset_write_R4P(self%root, 'YCoordinates', y, [grid(2)], &
                                         deflate=self%deflate_level())
    if (error == 0_I4P) error = h5_dataset_write_R4P(self%root, 'ZCoordinates', z, [grid(3)], &
                                         deflate=self%deflate_level())
  endif
  if (error /= 0_I4P) error = refuse('cannot write the coordinates')
  if (error == 0_I4P) self%is_geo_written = .true.
  if (error == 0_I4P .and. self%is_temporal) self%geo_slices = self%geo_slices + 1_I8P
  endfunction put_coordinates_R4P

  function put_axis_R4P(self, name, a, values) result(error)
  !< Write the coordinates (R4P) of the axis `a` of a RectilinearGrid from the ranks of `comm`: the dataset created by
  !< every rank, then the entries of the region of this rank that no rank before has (collective).
  class(vtkhdf_writer), intent(in) :: self      !< Writer.
  character(*), intent(in)         :: name      !< Dataset of the coordinates.
  integer(I4P), intent(in)         :: a         !< Axis: 1 x, 2 y, 3 z.
  real(R4P), intent(in)            :: values(:) !< Coordinates of the region of this rank along the axis.
  integer(I4P)                     :: error     !< Error status.
  integer(I8P), allocatable        :: runs(:,:) !< Runs of entries written by this rank.
  integer(I8P)                     :: grid(3)   !< Points along x, y, z.
  integer                          :: r         !< Counter.

  grid = self%grid_points()
  error = h5_dataset_slab_R4P(self%root, name, values, [grid(a)], [size(values, kind=I8P)], [0_I8P], [0_I8P], &
                              [0_I8P], .true.)
  call self%axis_runs(a, runs)
  ! as many writes on every rank (the largest number of runs; empty ones beyond its own): the dataset is opened
  ! the same number of times by every rank (H5Dopen is collective with collective metadata operations)
  do r=1, int(h5_mpi_max(self%comm, size(runs, dim=2, kind=I8P)))
    if (r <= size(runs, dim=2)) then
      if (error == 0_I4P) error = h5_dataset_slab_R4P(self%root, name, values, [grid(a)], [size(values, kind=I8P)], &
                                                    runs(1:1, r), runs(2:2, r), runs(3:3, r), .false.)
    else
      if (error == 0_I4P) error = h5_dataset_slab_R4P(self%root, name, values, [grid(a)], [size(values, kind=I8P)], &
                                                    [0_I8P], [0_I8P], [0_I8P], .false.)
    endif
  enddo
  endfunction put_axis_R4P

  function put_grid_points_R4P(self, xyz, n1, n) result(error)
  !< Write the points (R4P) of a StructuredGrid: the dataset Points, `[k, j, i, 3]` in C order (always 4D, as VTK
  !< requires).
  class(vtkhdf_writer), intent(inout) :: self                    !< Writer.
  real(R4P), intent(in)               :: xyz(*)                  !< Coordinates, interleaved (x, y, z of each point), x fastest.
  integer(I8P), intent(in)            :: n1                      !< First dimension of the coordinates (3).
  integer(I8P), intent(in)            :: n                       !< Number of points.
  integer(I4P)                        :: error                   !< Error status.
  integer(I8P)                        :: grid(3)                 !< Points along x, y, z.
  integer(I8P)                        :: mem_dims(4)             !< Points of the region.
  integer(I8P)                        :: file_start(4), count(4) !< Block of the region.

  error = self%check_grid_geo(TOPOLOGY_STRG)
  if (error /= 0_I4P) return
  grid = self%grid_points()
  if (self%is_parallel) then ! the points of the region of this rank
    call self%points_block(mem_dims, file_start, count)
    grid = mem_dims(2:)
  endif
  if (n1 /= 3_I8P .or. n /= product(grid)) then
    error = refuse('write_geo: the points are not 3 coordinates by the '// &
                   trim(str(n=product(grid), no_sign=.true.))//' points of the '// &
                   trim(merge('region', 'grid  ', self%is_parallel)))
  endif
  error = self%agree(error) ! with comm, a refusal on every rank before the collective writes
  if (error /= 0_I4P) return
  if (self%is_parallel) then ! every rank creates Points, writes its block
    error = h5_dataset_slab_R4P(self%root, 'Points', xyz, [3_I8P, self%grid_points()], mem_dims, &
                                [0_I8P, 0_I8P, 0_I8P, 0_I8P], file_start, count, .true.)
  elseif (self%is_temporal) then ! the time axis last: Points [t, k, j, i, 3] in C order
    error = store_R4P(self, self%root, 'Points', xyz, [3_I8P, grid, 1_I8P])
  else
    error = h5_dataset_write_R4P(self%root, 'Points', xyz, [3_I8P, grid], deflate=self%deflate_level())
  endif
  if (error /= 0_I4P) error = refuse('cannot write the points')
  if (error == 0_I4P) self%is_geo_written = .true.
  if (error == 0_I4P .and. self%is_temporal) self%geo_slices = self%geo_slices + 1_I8P
  endfunction put_grid_points_R4P

  function put_faces_I4P(self, nc, face, faceoffset, cell_type, check_only) result(error)
  !< Check (`check_only`) or append the faces (I4P ids) of the polyhedra of an UnstructuredGrid piece:
  !< VTKFortran's stream (for each polyhedron the number of its faces, then for each face the number of its
  !< points and their ids) becomes FaceConnectivity, FaceOffsets, PolyhedronToFaces, PolyhedronOffsets and the
  !< counts of the piece (VTKHDF 2.5). A cell is a polyhedron (type 42) if and only if its faceoffset is not -1.
  class(vtkhdf_writer), intent(inout) :: self                   !< Writer.
  integer(I8P), intent(in)            :: nc                     !< Number of cells.
  integer(I4P), intent(in)            :: face(:)                !< Faces of each polyhedron with their points (VTKFortran stream).
  integer(I4P), intent(in)            :: faceoffset(:)          !< End of the faces of each cell in `face`, -1 if not a polyhedron.
  integer(I1P), intent(in)            :: cell_type(:)           !< VTK cell types.
  logical, intent(in)                 :: check_only             !< Check the faces, write nothing.
  integer(I4P)                        :: error                  !< Error status.
  integer(I4P), allocatable           :: face_connectivity(:)   !< Point ids of the faces.
  integer(I4P), allocatable           :: face_offsets(:)        !< Offsets of the faces, from 0 (n_faces+1 values).
  integer(I4P), allocatable           :: polyhedron_to_faces(:) !< Faces of each polyhedron, from 0 in the piece.
  integer(I4P), allocatable           :: polyhedron_offsets(:)  !< Offsets of the faces of each cell, from 0 (nc+1 values).
  integer(I8P)                        :: n_faces                !< Faces of the polyhedra of the piece.
  integer(I8P)                        :: n_face_ids             !< Point ids of those faces.
  integer(I8P)                        :: start                  !< End of the faces of the previous polyhedron in `face`.
  integer(I8P)                        :: last                   !< End of the faces of the cell in `face`.
  integer(I8P)                        :: c, f, i, n, nf         !< Counters, sizes.

  error = 0_I4P
  if (self%is_parallel) then
    error = refuse('write_connectivity: polyhedra are not written by the ranks of comm')
    return
  endif
  if (size(faceoffset, kind=I8P) /= nc .or. size(cell_type, kind=I8P) < nc) then
    error = refuse('write_connectivity: faceoffset needs one value per cell')
    return
  endif
  ! count and check
  n_faces = 0_I8P ; n_face_ids = 0_I8P ; start = 0_I8P
  do c=1_I8P, nc
    if (faceoffset(c) < 0_I4P) then
      if (cell_type(c) == CELL_POLYHEDRON) then
        error = refuse('write_connectivity: the cell '//trim(str(n=c, no_sign=.true.))//' is a polyhedron, '// &
                       'its faceoffset is -1')
        return
      endif
      cycle
    endif
    if (cell_type(c) /= CELL_POLYHEDRON) then
      error = refuse('write_connectivity: the cell '//trim(str(n=c, no_sign=.true.))//' is not a polyhedron, '// &
                     'its faceoffset must be -1')
      return
    endif
    last = int(faceoffset(c), I8P)
    i = start + 1_I8P
    nf = 0_I8P
    if (last > start .and. last <= size(face, kind=I8P)) nf = int(face(i), I8P)
    i = i + 1_I8P
    do f=1_I8P, nf
      if (i > last) exit
      n = int(face(i), I8P)
      if (n < 3_I8P .or. i + n > last) exit
      n_face_ids = n_face_ids + n
      i = i + n + 1_I8P
    enddo
    if (nf < 1_I8P .or. f <= nf .or. i - 1_I8P /= last) then
      error = refuse('write_connectivity: the faces of the cell '//trim(str(n=c, no_sign=.true.))// &
                     ' do not end at its faceoffset (each face: its number of points, at least 3, then their ids)')
      return
    endif
    n_faces = n_faces + nf
    start = last
  enddo
  if (check_only) return
  ! convert
  allocate(face_connectivity(n_face_ids), face_offsets(n_faces+1_I8P), polyhedron_to_faces(n_faces))
  allocate(polyhedron_offsets(nc+1_I8P))
  face_offsets(1) = 0_I4P ; polyhedron_offsets(1) = 0_I4P
  n_faces = 0_I8P ; n_face_ids = 0_I8P ; start = 0_I8P
  do c=1_I8P, nc
    if (faceoffset(c) >= 0_I4P) then
      i = start + 1_I8P
      nf = int(face(i), I8P)
      i = i + 1_I8P
      do f=1_I8P, nf
        n = int(face(i), I8P)
        face_connectivity(n_face_ids+1_I8P:n_face_ids+n) = face(i+1_I8P:i+n)
        n_face_ids = n_face_ids + n
        face_offsets(n_faces+f+1_I8P) = int(n_face_ids, I4P)
        polyhedron_to_faces(n_faces+f) = int(n_faces + f - 1_I8P, I4P)
        i = i + n + 1_I8P
      enddo
      n_faces = n_faces + nf
      start = int(faceoffset(c), I8P)
    endif
    polyhedron_offsets(c+1_I8P) = int(n_faces, I4P)
  enddo
  error = self%start_polyhedra()
  if (error /= 0_I4P) return
  error = store_I4P(self, self%root, 'FaceConnectivity', face_connectivity, [n_face_ids])
  if (error == 0_I4P) error = store_I4P(self, self%root, 'FaceOffsets', face_offsets, [n_faces+1_I8P])
  if (error == 0_I4P) error = store_I4P(self, self%root, 'PolyhedronToFaces', polyhedron_to_faces, [n_faces])
  if (error == 0_I4P) error = store_I4P(self, self%root, 'PolyhedronOffsets', polyhedron_offsets, [nc+1_I8P])
  if (error == 0_I4P) error = store_I8P(self, self%root, 'NumberOfFaces', [n_faces], [1_I8P])
  if (error == 0_I4P) error = store_I8P(self, self%root, 'NumberOfFaceConnectivityIds', [n_face_ids], [1_I8P])
  if (error == 0_I4P) error = store_I8P(self, self%root, 'NumberOfPolyhedronToFaceIds', [n_faces], [1_I8P])
  if (error /= 0_I4P) error = refuse('cannot write the faces of the polyhedra')
  if (error == 0_I4P) then
    self%is_faces_written = .true.
    self%totals(5:7) = self%totals(5:7) + [n_face_ids, n_faces, n_faces]
  endif
  endfunction put_faces_I4P

  function put_faces_I8P(self, nc, face, faceoffset, cell_type, check_only) result(error)
  !< Check (`check_only`) or append the faces (I8P ids) of the polyhedra of an UnstructuredGrid piece:
  !< VTKFortran's stream (for each polyhedron the number of its faces, then for each face the number of its
  !< points and their ids) becomes FaceConnectivity, FaceOffsets, PolyhedronToFaces, PolyhedronOffsets and the
  !< counts of the piece (VTKHDF 2.5). A cell is a polyhedron (type 42) if and only if its faceoffset is not -1.
  class(vtkhdf_writer), intent(inout) :: self                   !< Writer.
  integer(I8P), intent(in)            :: nc                     !< Number of cells.
  integer(I8P), intent(in)            :: face(:)                !< Faces of each polyhedron with their points (VTKFortran stream).
  integer(I8P), intent(in)            :: faceoffset(:)          !< End of the faces of each cell in `face`, -1 if not a polyhedron.
  integer(I1P), intent(in)            :: cell_type(:)           !< VTK cell types.
  logical, intent(in)                 :: check_only             !< Check the faces, write nothing.
  integer(I4P)                        :: error                  !< Error status.
  integer(I8P), allocatable           :: face_connectivity(:)   !< Point ids of the faces.
  integer(I8P), allocatable           :: face_offsets(:)        !< Offsets of the faces, from 0 (n_faces+1 values).
  integer(I8P), allocatable           :: polyhedron_to_faces(:) !< Faces of each polyhedron, from 0 in the piece.
  integer(I8P), allocatable           :: polyhedron_offsets(:)  !< Offsets of the faces of each cell, from 0 (nc+1 values).
  integer(I8P)                        :: n_faces                !< Faces of the polyhedra of the piece.
  integer(I8P)                        :: n_face_ids             !< Point ids of those faces.
  integer(I8P)                        :: start                  !< End of the faces of the previous polyhedron in `face`.
  integer(I8P)                        :: last                   !< End of the faces of the cell in `face`.
  integer(I8P)                        :: c, f, i, n, nf         !< Counters, sizes.

  error = 0_I4P
  if (self%is_parallel) then
    error = refuse('write_connectivity: polyhedra are not written by the ranks of comm')
    return
  endif
  if (size(faceoffset, kind=I8P) /= nc .or. size(cell_type, kind=I8P) < nc) then
    error = refuse('write_connectivity: faceoffset needs one value per cell')
    return
  endif
  ! count and check
  n_faces = 0_I8P ; n_face_ids = 0_I8P ; start = 0_I8P
  do c=1_I8P, nc
    if (faceoffset(c) < 0_I8P) then
      if (cell_type(c) == CELL_POLYHEDRON) then
        error = refuse('write_connectivity: the cell '//trim(str(n=c, no_sign=.true.))//' is a polyhedron, '// &
                       'its faceoffset is -1')
        return
      endif
      cycle
    endif
    if (cell_type(c) /= CELL_POLYHEDRON) then
      error = refuse('write_connectivity: the cell '//trim(str(n=c, no_sign=.true.))//' is not a polyhedron, '// &
                     'its faceoffset must be -1')
      return
    endif
    last = int(faceoffset(c), I8P)
    i = start + 1_I8P
    nf = 0_I8P
    if (last > start .and. last <= size(face, kind=I8P)) nf = int(face(i), I8P)
    i = i + 1_I8P
    do f=1_I8P, nf
      if (i > last) exit
      n = int(face(i), I8P)
      if (n < 3_I8P .or. i + n > last) exit
      n_face_ids = n_face_ids + n
      i = i + n + 1_I8P
    enddo
    if (nf < 1_I8P .or. f <= nf .or. i - 1_I8P /= last) then
      error = refuse('write_connectivity: the faces of the cell '//trim(str(n=c, no_sign=.true.))// &
                     ' do not end at its faceoffset (each face: its number of points, at least 3, then their ids)')
      return
    endif
    n_faces = n_faces + nf
    start = last
  enddo
  if (check_only) return
  ! convert
  allocate(face_connectivity(n_face_ids), face_offsets(n_faces+1_I8P), polyhedron_to_faces(n_faces))
  allocate(polyhedron_offsets(nc+1_I8P))
  face_offsets(1) = 0_I8P ; polyhedron_offsets(1) = 0_I8P
  n_faces = 0_I8P ; n_face_ids = 0_I8P ; start = 0_I8P
  do c=1_I8P, nc
    if (faceoffset(c) >= 0_I8P) then
      i = start + 1_I8P
      nf = int(face(i), I8P)
      i = i + 1_I8P
      do f=1_I8P, nf
        n = int(face(i), I8P)
        face_connectivity(n_face_ids+1_I8P:n_face_ids+n) = face(i+1_I8P:i+n)
        n_face_ids = n_face_ids + n
        face_offsets(n_faces+f+1_I8P) = int(n_face_ids, I8P)
        polyhedron_to_faces(n_faces+f) = int(n_faces + f - 1_I8P, I8P)
        i = i + n + 1_I8P
      enddo
      n_faces = n_faces + nf
      start = int(faceoffset(c), I8P)
    endif
    polyhedron_offsets(c+1_I8P) = int(n_faces, I8P)
  enddo
  error = self%start_polyhedra()
  if (error /= 0_I4P) return
  error = store_I8P(self, self%root, 'FaceConnectivity', face_connectivity, [n_face_ids])
  if (error == 0_I4P) error = store_I8P(self, self%root, 'FaceOffsets', face_offsets, [n_faces+1_I8P])
  if (error == 0_I4P) error = store_I8P(self, self%root, 'PolyhedronToFaces', polyhedron_to_faces, [n_faces])
  if (error == 0_I4P) error = store_I8P(self, self%root, 'PolyhedronOffsets', polyhedron_offsets, [nc+1_I8P])
  if (error == 0_I4P) error = store_I8P(self, self%root, 'NumberOfFaces', [n_faces], [1_I8P])
  if (error == 0_I4P) error = store_I8P(self, self%root, 'NumberOfFaceConnectivityIds', [n_face_ids], [1_I8P])
  if (error == 0_I4P) error = store_I8P(self, self%root, 'NumberOfPolyhedronToFaceIds', [n_faces], [1_I8P])
  if (error /= 0_I4P) error = refuse('cannot write the faces of the polyhedra')
  if (error == 0_I4P) then
    self%is_faces_written = .true.
    self%totals(5:7) = self%totals(5:7) + [n_face_ids, n_faces, n_faces]
  endif
  endfunction put_faces_I8P

  function put_block_I4P(self, b, check_only, connectivity, offset) result(error)
  !< Check (`check_only`) or append the block `b` (I4P ids) of the cells of a PolyData piece to its group:
  !< Connectivity, Offsets (from 0), NumberOfCells, NumberOfConnectivityIds.
  class(vtkhdf_writer), intent(inout) :: self            !< Writer.
  integer(I4P), intent(in)            :: b               !< Block: 1 vertices, 2 lines, 3 strips, 4 polygons.
  logical, intent(in)                 :: check_only      !< Check the block, write nothing.
  integer(I4P), intent(in)            :: connectivity(:) !< Point ids of the cells, from 0 in the piece.
  integer(I4P), intent(in)            :: offset(:)       !< End offset of each cell in the connectivity.
  integer(I4P)                        :: error           !< Error status.
  integer(I4P), allocatable           :: offsets(:)      !< Offsets from 0 (nc+1 values).
  character(len=:), allocatable       :: group           !< Group of the block, with the separator.
  integer(I8P)                        :: nc              !< Cells of the block.
  integer(I8P)                        :: n_ids           !< Ids used by the cells.

  error = 0_I4P
  group = trim(BLOCKS(b))//'/'
  nc = self%block_cells(b)
  if (size(offset, kind=I8P) /= nc) then
    error = refuse('write_polydata_cells: '//trim(BLOCKS(b))//' needs one offset per cell, '// &
                   trim(str(n=nc, no_sign=.true.))//' as given to write_piece')
    return
  endif
  n_ids = 0_I8P
  if (nc > 0_I8P) n_ids = int(offset(nc), I8P)
  if (n_ids < 0_I8P .or. n_ids > size(connectivity, kind=I8P)) then
    error = refuse('write_polydata_cells: the last offset of '//trim(BLOCKS(b))//' is not within its connectivity')
    return
  endif
  if (check_only) return
  allocate(offsets(nc+1_I8P))
  offsets(1) = 0_I4P
  if (nc > 0_I8P) offsets(2:) = offset(1:nc)
  error = store_I4P(self, self%root, group//'Connectivity', connectivity, [n_ids])
  if (error == 0_I4P) error = store_I4P(self, self%root, group//'Offsets', offsets, [nc+1_I8P])
  if (error == 0_I4P) error = store_I8P(self, self%root, group//'NumberOfCells', [nc], [1_I8P])
  if (error == 0_I4P) error = store_I8P(self, self%root, group//'NumberOfConnectivityIds', [n_ids], [1_I8P])
  if (error == 0_I4P) then
    self%block_totals(b) = self%block_totals(b) + nc
    self%block_totals(4+b) = self%block_totals(4+b) + n_ids
  endif
  endfunction put_block_I4P

  function put_block_I8P(self, b, check_only, connectivity, offset) result(error)
  !< Check (`check_only`) or append the block `b` (I8P ids) of the cells of a PolyData piece to its group:
  !< Connectivity, Offsets (from 0), NumberOfCells, NumberOfConnectivityIds.
  class(vtkhdf_writer), intent(inout) :: self            !< Writer.
  integer(I4P), intent(in)            :: b               !< Block: 1 vertices, 2 lines, 3 strips, 4 polygons.
  logical, intent(in)                 :: check_only      !< Check the block, write nothing.
  integer(I8P), intent(in)            :: connectivity(:) !< Point ids of the cells, from 0 in the piece.
  integer(I8P), intent(in)            :: offset(:)       !< End offset of each cell in the connectivity.
  integer(I4P)                        :: error           !< Error status.
  integer(I8P), allocatable           :: offsets(:)      !< Offsets from 0 (nc+1 values).
  character(len=:), allocatable       :: group           !< Group of the block, with the separator.
  integer(I8P)                        :: nc              !< Cells of the block.
  integer(I8P)                        :: n_ids           !< Ids used by the cells.

  error = 0_I4P
  group = trim(BLOCKS(b))//'/'
  nc = self%block_cells(b)
  if (size(offset, kind=I8P) /= nc) then
    error = refuse('write_polydata_cells: '//trim(BLOCKS(b))//' needs one offset per cell, '// &
                   trim(str(n=nc, no_sign=.true.))//' as given to write_piece')
    return
  endif
  n_ids = 0_I8P
  if (nc > 0_I8P) n_ids = int(offset(nc), I8P)
  if (n_ids < 0_I8P .or. n_ids > size(connectivity, kind=I8P)) then
    error = refuse('write_polydata_cells: the last offset of '//trim(BLOCKS(b))//' is not within its connectivity')
    return
  endif
  if (check_only) return
  allocate(offsets(nc+1_I8P))
  offsets(1) = 0_I8P
  if (nc > 0_I8P) offsets(2:) = offset(1:nc)
  error = store_I8P(self, self%root, group//'Connectivity', connectivity, [n_ids])
  if (error == 0_I4P) error = store_I8P(self, self%root, group//'Offsets', offsets, [nc+1_I8P])
  if (error == 0_I4P) error = store_I8P(self, self%root, group//'NumberOfCells', [nc], [1_I8P])
  if (error == 0_I4P) error = store_I8P(self, self%root, group//'NumberOfConnectivityIds', [n_ids], [1_I8P])
  if (error == 0_I4P) then
    self%block_totals(b) = self%block_totals(b) + nc
    self%block_totals(4+b) = self%block_totals(4+b) + n_ids
  endif
  endfunction put_block_I8P

  function check_cells(self, nc, n_offset, n_types) result(error)
  !< Check a write_connectivity call: an UnstructuredGrid piece open (no data group), its cells once, sizes.
  class(vtkhdf_writer), intent(in) :: self     !< Writer.
  integer(I8P),         intent(in) :: nc       !< Number of cells.
  integer(I8P),         intent(in) :: n_offset !< Size of the offsets.
  integer(I8P),         intent(in) :: n_types  !< Size of the cell types.
  integer(I4P)                     :: error    !< Error status.

  error = 1_I4P
  if (self%topology /= TOPOLOGY_UNSTRUCTURED) then
    error = refuse('write_connectivity is for an UnstructuredGrid')
  elseif (.not.self%is_piece_open .or. self%group >= 0_hid_kind) then
    error = refuse('write_connectivity: open the piece first, outside the point and cell data')
  elseif (self%is_connectivity_written) then
    error = refuse('write_connectivity: the cells of this piece are already written')
  elseif (nc /= self%nc_piece) then
    error = refuse('write_connectivity: nc is not the number of cells of the piece')
  elseif (n_offset /= nc .or. n_types /= nc) then
    error = refuse('write_connectivity: offset and cell_type need one value per cell')
  else
    error = 0_I4P
  endif
  endfunction check_cells

  function check_geo(self, np, nc, n1, n2) result(error)
  !< Check a write_geo call: an UnstructuredGrid or PolyData piece open (no data group), its points once, sizes. The
  !< number of cells is checked for an UnstructuredGrid only: for a PolyData it is not used, as in the XML formats.
  class(vtkhdf_writer), intent(in) :: self  !< Writer.
  integer(I8P),         intent(in) :: np    !< Number of points.
  integer(I8P),         intent(in) :: nc    !< Number of cells.
  integer(I8P),         intent(in) :: n1    !< First dimension of the coordinates.
  integer(I8P),         intent(in) :: n2    !< Second dimension of the coordinates.
  integer(I4P)                     :: error !< Error status.

  error = 1_I4P
  if (self%topology /= TOPOLOGY_UNSTRUCTURED .and. self%topology /= TOPOLOGY_POLY) then
    error = refuse('write_geo(np, nc, ...) is for an UnstructuredGrid or a PolyData (an ImageData has no geometry)')
  elseif (.not.self%is_piece_open .or. self%group >= 0_hid_kind) then
    error = refuse('write_geo: open the piece first, outside the point and cell data')
  elseif (self%is_geo_written) then
    error = refuse('write_geo: the points of this piece are already written')
  elseif (np /= self%np_piece) then
    error = refuse('write_geo: np is not the number of points of the piece')
  elseif (self%topology == TOPOLOGY_UNSTRUCTURED .and. nc /= self%nc_piece) then
    error = refuse('write_geo: nc is not the number of cells of the piece')
  elseif (n1 /= 3_I8P .or. n2 /= np) then
    error = refuse('write_geo: the coordinates are not 3 by np')
  else
    error = 0_I4P
  endif
  endfunction check_geo

  function check_grid_geo(self, topology) result(error)
  !< Check a write_geo call of a RectilinearGrid (`topology` TOPOLOGY_RECT) or a StructuredGrid (TOPOLOGY_STRG): its piece
  !< open (no data group), its geometry once.
  class(vtkhdf_writer), intent(in) :: self     !< Writer.
  integer(I4P),         intent(in) :: topology !< Topology of the call.
  integer(I4P)                     :: error    !< Error status.

  error = 1_I4P
  if (self%topology /= topology) then
    if (topology == TOPOLOGY_RECT) then
      error = refuse('write_geo(x, y, z) is for a RectilinearGrid')
    else
      error = refuse('write_geo(xyz) and write_geo(n, x, y, z) are for a StructuredGrid')
    endif
  elseif (.not.self%is_piece_open .or. self%group >= 0_hid_kind) then
    error = refuse('write_geo: open the piece first, outside the point and cell data')
  elseif (self%is_geo_written) then
    error = refuse('write_geo: the geometry of this piece is already written')
  else
    error = 0_I4P
  endif
  endfunction check_grid_geo

  function close_group(self) result(error)
  !< Close the open data group (and forget its active arrays).
  class(vtkhdf_writer), intent(inout) :: self  !< Writer.
  integer(I4P)                        :: error !< Error status.
  integer(I4P)                        :: r     !< Counter.

  error = h5_group_close(self%group)
  self%group = -1_hid_kind
  self%group_name = ''
  do r=1, size(ROLES)
    if (allocated(self%active(r)%name)) deallocate(self%active(r)%name)
  enddo
  endfunction close_group

  function dataset_dims(self, data_name, n, n_components, dims) result(error)
  !< Shape (Fortran order) of the dataset of `n` values of `n_components` components in the open group.
  !<
  !< Point and cell data are shaped as the grid, `[n_components, ni, nj, nk]`: ni points along x for point data, ni-1 cells
  !< for cell data (ImageData, RectilinearGrid, StructuredGrid). The axes of one point (a 2D or 1D grid) are dropped, as VTK
  !< writes and requires (VTK 9.7 reads neither a size-1 axis nor a flat array), and so is the first dimension with one
  !< component. Field data are
  !< `[n_components, n_tuples]`. In an UnstructuredGrid or a PolyData, the tuples are the points or cells of the piece,
  !< `[n_components, n]` (the cells of a PolyData in the order of the blocks: vertices, lines, strips, polygons).
  class(vtkhdf_writer),      intent(in)  :: self         !< Writer.
  character(*),              intent(in)  :: data_name    !< Data name (for the messages).
  integer(I8P),              intent(in)  :: n            !< Number of values.
  integer(I4P),              intent(in)  :: n_components !< Number of components.
  integer(I8P), allocatable, intent(out) :: dims(:)      !< Shape of the dataset.
  integer(I4P)                           :: error        !< Error status.
  integer(I8P)                           :: grid(3)      !< Points or cells along each axis.
  logical                                :: is_axis(3)   !< The axis has more than one point.
  integer(I8P)                           :: expected     !< Values expected.
  integer(I8P), allocatable              :: mem_dims(:), mem_start(:), file_start(:), count(:) !< Block of a region.

  error = 1_I4P
  if (self%group < 0_hid_kind .or. n_components < 1_I4P) return
  if (self%group_name == 'FieldData') then
    if (mod(n, int(n_components, I8P)) /= 0_I8P) then
      error = refuse('the field data '//data_name//' has '//trim(str(n=n, no_sign=.true.))// &
                     ' values, not a multiple of its '//trim(str(n=n_components, no_sign=.true.))//' components')
      return
    endif
    grid = [n / n_components, 1_I8P, 1_I8P]
    is_axis = [.true., .false., .false.]
  elseif (self%is_partitioned()) then
    grid = [self%np_piece, 1_I8P, 1_I8P]
    if (self%group_name == 'CellData') grid(1) = self%nc_piece
    if (self%is_reusing()) then ! a step without pieces: the points or cells of all the parts of the mesh reused
      grid(1) = self%total_points - self%mesh_start(2)
      if (self%group_name == 'CellData') then
        if (self%topology == TOPOLOGY_UNSTRUCTURED) then
          grid(1) = self%total_cells - self%mesh_start(3)
        else
          grid(1) = self%total_cells - sum(self%mesh_blocks(1:4))
        endif
      endif
    endif
    is_axis = [.true., .false., .false.]
    if (n /= grid(1) * n_components) then
      error = refuse('the array '//data_name//' has '//trim(str(n=n, no_sign=.true.))//' values, the '// &
                     trim(merge('mesh reused', 'piece      ', self%is_reusing()))//' needs '// &
                     trim(str(n=grid(1) * n_components, no_sign=.true.))//' ('//                                   &
                     trim(str(n=n_components, no_sign=.true.))//' components)')
      return
    endif
  else
    grid = self%grid_points()
    is_axis = grid > 1_I8P
    if (.not.any(is_axis)) is_axis(1) = .true. ! one point: one value
    if (self%group_name == 'CellData') grid = max(grid - 1_I8P, 1_I8P)
    expected = product(grid) * n_components
    if (self%is_parallel) then ! the values of the region of this rank
      call self%region_block(self%group_name == 'CellData', n_components, dims, mem_dims, mem_start, file_start, count)
      expected = product(mem_dims)
    endif
    if (n /= expected) then
      error = refuse('the array '//data_name//' has '//trim(str(n=n, no_sign=.true.))//' values, the '// &
                     trim(merge('region', 'grid  ', self%is_parallel))//' needs '// &
                     trim(str(n=expected, no_sign=.true.))//' ('//                              &
                     trim(str(n=n_components, no_sign=.true.))//' components)')
      return
    endif
  endif
  ! one expression, no shrinking of dims in place (nvfortran 26.9 corrupts `dims = dims(2:)`)
  if (n_components == 1_I4P) then
    dims = pack(grid, is_axis)
  else
    dims = [int(n_components, I8P), pack(grid, is_axis)]
  endif
  error = 0_I4P
  endfunction dataset_dims

  function end_partition(self) result(error)
  !< End a piece of an UnstructuredGrid or PolyData: its points and cells written, every array of the first piece written
  !< once. A PolyData piece without cells needs no write_polydata_cells: its four blocks get zero entries.
  class(vtkhdf_writer), intent(inout) :: self  !< Writer.
  integer(I4P)                        :: error !< Error status.

  error = 0_I4P
  if (self%topology == TOPOLOGY_POLY .and. .not.self%is_connectivity_written .and. self%nc_piece == 0_I8P &
      .and. .not.self%is_parallel) then ! with comm the blocks are written by write_polydata_cells, by every rank
    error = self%poly_blocks(check_only=.false.)
    if (error /= 0_I4P) return
    self%is_connectivity_written = .true.
  endif
  if (.not.(self%is_geo_written .and. self%is_connectivity_written)) then
    if (self%topology == TOPOLOGY_POLY) then
      error = refuse('a piece of a PolyData needs write_geo and write_polydata_cells')
    else
      error = refuse('a piece of an UnstructuredGrid needs write_geo and write_connectivity')
    endif
    return
  endif
  if (self%has_polyhedra .and. .not.self%is_faces_written) then
    error = self%no_faces(self%nc_piece)
    if (error /= 0_I4P) return
  endif
  ! every array written in every piece: the tuples of the pieces of the step (of the file, without steps)
  error = self%check_arrays('PointData', self%total_points - self%step_start(2), .false.)
  if (error == 0_I4P) error = self%check_arrays('CellData', self%total_cells - self%step_start(3), .false.)
  endfunction end_partition

  pure function grid_points(self) result(grid)
  !< Points of the grid along each axis, from the whole extent (ImageData, RectilinearGrid, StructuredGrid).
  class(vtkhdf_writer), intent(in) :: self    !< Writer.
  integer(I8P)                     :: grid(3) !< Points along x, y, z.

  grid = int(self%whole_extent(2::2) - self%whole_extent(1::2), I8P) + 1_I8P
  endfunction grid_points

  pure function deflate_level(self) result(level)
  !< Deflate level of the datasets: 0 (none) unless compressed.
  class(vtkhdf_writer), intent(in) :: self  !< Writer.
  integer(I4P)                     :: level !< Deflate level.

  level = 0_I4P
  if (self%is_compressed) level = DEFLATE_LEVEL_ZLIB
  endfunction deflate_level

  elemental function is_partitioned(self) result(is_partitioned_)
  !< Return true if the pieces are partitions of the file (UnstructuredGrid, PolyData), false for a grid of one piece.
  class(vtkhdf_writer), intent(in) :: self             !< Writer.
  logical                          :: is_partitioned_  !< Check result.

  is_partitioned_ = self%topology == TOPOLOGY_UNSTRUCTURED .or. self%topology == TOPOLOGY_POLY
  endfunction is_partitioned

  function mark_active(self, data_name) result(error)
  !< Mark the dataset `data_name` of the open point or cell data group as the active array of its role, if it is one.
  class(vtkhdf_writer), intent(inout) :: self      !< Writer.
  character(*),         intent(in)    :: data_name !< Data name.
  integer(I4P)                        :: error     !< Error status.
  integer(hid_kind)                   :: dset      !< Dataset.
  integer(I4P)                        :: r         !< Counter.

  error = 0_I4P
  if (self%group_name == 'FieldData') return
  do r=1, size(ROLES)
    if (.not.allocated(self%active(r)%name)) cycle
    if (self%active(r)%name /= data_name) cycle
    ! a dataset has one role: the first one naming it
    error = h5_dataset_open(self%group, data_name, dset)
    if (error == 0_I4P) error = h5_attribute_write(dset, 'Attribute', trim(ROLES(r)))
    if (error == 0_I4P) error = h5_dataset_close(dset)
    if (error /= 0_I4P) error = refuse('cannot mark '//data_name//' as active '//trim(ROLES(r)))
    self%version_minor = max(self%version_minor, 6_I8P)
    exit
  enddo
  endfunction mark_active

  function open_group(self, name) result(error)
  !< Open the data group `name` of /VTKHDF, creating it (creation order tracked: the arrays keep the order written).
  class(vtkhdf_writer), intent(inout) :: self  !< Writer.
  character(*),         intent(in)    :: name  !< Group name.
  integer(I4P)                        :: error !< Error status.
  integer(I4P)                        :: r     !< Counter.

  if (h5_link_exists(self%root, name)) then
    error = h5_group_open(self%root, name, self%group)
  else
    error = h5_group_create(self%root, name, self%group, track_order=.true.)
  endif
  if (error /= 0_I4P) then
    self%group = -1_hid_kind
    error = refuse('cannot open the group '//name)
    return
  endif
  self%group_name = name
  do r=1, size(ROLES)
    if (allocated(self%active(r)%name)) deallocate(self%active(r)%name)
  enddo
  endfunction open_group

  function no_faces(self, nc) result(error)
  !< Write the zero entries of a piece without polyhedra, of `nc` cells, in a file with polyhedra: no faces, and the
  !< `nc+1` polyhedron offsets all 0.
  class(vtkhdf_writer), intent(inout) :: self      !< Writer.
  integer(I8P),         intent(in)    :: nc        !< Cells of the piece.
  integer(I4P)                        :: error     !< Error status.
  integer(I8P), allocatable           :: zeros(:)  !< Polyhedron offsets of the piece.

  allocate(zeros(nc+1_I8P))
  zeros = 0_I8P
  error = store_I8P(self, self%root, 'FaceConnectivity', [integer(I8P) ::], [0_I8P])
  if (error == 0_I4P) error = store_I8P(self, self%root, 'FaceOffsets', [0_I8P], [1_I8P])
  if (error == 0_I4P) error = store_I8P(self, self%root, 'PolyhedronToFaces', [integer(I8P) ::], [0_I8P])
  if (error == 0_I4P) error = store_I8P(self, self%root, 'PolyhedronOffsets', zeros, [nc+1_I8P])
  if (error == 0_I4P) error = store_I8P(self, self%root, 'NumberOfFaces', [0_I8P], [1_I8P])
  if (error == 0_I4P) error = store_I8P(self, self%root, 'NumberOfFaceConnectivityIds', [0_I8P], [1_I8P])
  if (error == 0_I4P) error = store_I8P(self, self%root, 'NumberOfPolyhedronToFaceIds', [0_I8P], [1_I8P])
  if (error /= 0_I4P) error = refuse('cannot write the (empty) polyhedra of a piece')
  endfunction no_faces

  function poly_block(self, b, check_only, connectivity, offset) result(error)
  !< Check (`check_only`) or write the block `b` of the cells of a PolyData piece, given by its connectivity and its end
  !< offsets (I4P or I8P each; both converted to I8P when their kinds differ). A block not given must have no cells in
  !< the piece: its group gets zero entries.
  class(vtkhdf_writer), intent(inout)        :: self            !< Writer.
  integer(I4P),         intent(in)           :: b               !< Block: 1 vertices, 2 lines, 3 strips, 4 polygons.
  logical,              intent(in)           :: check_only      !< Check the block, write nothing.
  class(*),             intent(in), optional :: connectivity(:) !< Point ids of the cells, from 0 in the piece.
  class(*),             intent(in), optional :: offset(:)       !< End offset of each cell in the connectivity.
  integer(I4P)                               :: error           !< Error status.

  error = 0_I4P
  if (.not.(present(connectivity) .and. present(offset))) then
    if (self%block_cells(b) > 0_I8P) then
      error = refuse('write_polydata_cells: the piece has '//trim(str(n=self%block_cells(b), no_sign=.true.))// &
                     ' cells in '//trim(BLOCKS(b))//', but their block is not given')
    elseif (.not.check_only) then
      error = put_block_I8P(self, b, check_only, [integer(I8P) ::], [integer(I8P) ::])
    endif
    return
  endif
  select type(connectivity)
  type is(integer(I4P))
    select type(offset)
    type is(integer(I4P))
      error = put_block_I4P(self, b, check_only, connectivity, offset)
    type is(integer(I8P))
      error = put_block_I8P(self, b, check_only, int(connectivity, I8P), offset)
    class default
      error = refuse('write_polydata_cells: the offsets of '//trim(BLOCKS(b))//' are not I4P or I8P')
    endselect
  type is(integer(I8P))
    select type(offset)
    type is(integer(I4P))
      error = put_block_I8P(self, b, check_only, connectivity, int(offset, I8P))
    type is(integer(I8P))
      error = put_block_I8P(self, b, check_only, connectivity, offset)
    class default
      error = refuse('write_polydata_cells: the offsets of '//trim(BLOCKS(b))//' are not I4P or I8P')
    endselect
  class default
    error = refuse('write_polydata_cells: the connectivity of '//trim(BLOCKS(b))//' is not I4P or I8P')
  endselect
  endfunction poly_block

  function poly_blocks(self, check_only, verts_connectivity, verts_offset, lines_connectivity, lines_offset, &
                       strips_connectivity, strips_offset, polys_connectivity, polys_offset) result(error)
  !< Check (`check_only`) or write the four blocks of the cells of a PolyData piece, in the order of their groups.
  class(vtkhdf_writer), intent(inout)        :: self                   !< Writer.
  logical,              intent(in)           :: check_only             !< Check the blocks, write nothing.
  class(*),             intent(in), optional :: verts_connectivity(:)  !< Vertices connectivity.
  class(*),             intent(in), optional :: verts_offset(:)        !< Vertices offsets.
  class(*),             intent(in), optional :: lines_connectivity(:)  !< Lines connectivity.
  class(*),             intent(in), optional :: lines_offset(:)        !< Lines offsets.
  class(*),             intent(in), optional :: strips_connectivity(:) !< Triangle strips connectivity.
  class(*),             intent(in), optional :: strips_offset(:)       !< Triangle strips offsets.
  class(*),             intent(in), optional :: polys_connectivity(:)  !< Polygons connectivity.
  class(*),             intent(in), optional :: polys_offset(:)        !< Polygons offsets.
  integer(I4P)                               :: error                  !< Error status.

  error = self%poly_block(1_I4P, check_only, verts_connectivity, verts_offset)
  if (error == 0_I4P) error = self%poly_block(2_I4P, check_only, lines_connectivity, lines_offset)
  if (error == 0_I4P) error = self%poly_block(3_I4P, check_only, strips_connectivity, strips_offset)
  if (error == 0_I4P) error = self%poly_block(4_I4P, check_only, polys_connectivity, polys_offset)
  endfunction poly_blocks

  subroutine record_array(self, data_name)
  !< Record the name of an array of the open data group: point and cell data of an UnstructuredGrid or PolyData (checked
  !< at the end of the pieces), and of a grid in a time series (checked at the end of the steps); field data of a time
  !< series (their `Steps` entries).
  class(vtkhdf_writer), intent(inout) :: self      !< Writer.
  character(*),         intent(in)    :: data_name !< Data name.
  character(len=:), allocatable       :: names     !< Names, updated (not an assignment of a component to itself).

  if (.not.(self%is_partitioned() .or. self%is_temporal)) return
  if (self%group_name == 'FieldData') then
    if (self%is_temporal) then
      names = self%field_arrays//data_name//NAMES_SEPARATOR
      self%field_arrays = names
    endif
  elseif (self%group_name == 'PointData') then
    names = self%point_arrays//data_name//NAMES_SEPARATOR
    self%point_arrays = names
    call grow(self%point_start) ; call grow(self%point_prev)
  elseif (self%group_name == 'CellData') then
    names = self%cell_arrays//data_name//NAMES_SEPARATOR
    self%cell_arrays = names
    call grow(self%cell_start) ; call grow(self%cell_prev)
  endif
  contains
    subroutine grow(values)
    !< One more value, 0: a new array starts at the start of the datasets (first step, or a file without steps).
    integer(I8P), allocatable, intent(inout) :: values(:) !< Values.
    integer(I8P), allocatable                :: more(:)   !< Values, one more.

    if (.not.allocated(values)) allocate(values(0))
    allocate(more(size(values) + 1))
    more(1:size(values)) = values
    more(size(more)) = 0_I8P
    call move_alloc(more, values)
    endsubroutine grow
  endsubroutine record_array

  function check_arrays(self, group, rows, may_reuse) result(error)
  !< Check that every point (`group` PointData) or cell (CellData) array holds `rows` tuples (time slices for a grid) more
  !< than at the start of the open step, or none when `may_reuse` (it reuses its values of the step before).
  class(vtkhdf_writer), intent(in) :: self      !< Writer.
  character(*),         intent(in) :: group     !< PointData or CellData.
  integer(I8P),         intent(in) :: rows      !< Tuples (slices) written in the open step by an array written in it.
  logical,              intent(in) :: may_reuse !< An array may be left out of the open step.
  integer(I4P)                     :: error     !< Error status.
  character(len=:), allocatable    :: names     !< Names of the arrays.
  integer(I8P), allocatable        :: start(:)  !< Tuples of each array at the start of the step.
  integer(I8P), allocatable        :: dims(:)   !< Shape of a dataset, Fortran order.
  integer(I8P)                     :: first     !< First character of a name.
  integer(I8P)                     :: last      !< Separator after a name.
  integer(I8P)                     :: a         !< Counter.
  integer(I8P)                     :: written   !< Tuples written in the step.

  if (group == 'PointData') then
    names = self%point_arrays ; start = self%point_start
  else
    names = self%cell_arrays ; start = self%cell_start
  endif
  error = 0_I4P
  first = 1_I8P
  a = 0_I8P
  do while (first <= len(names, kind=I8P))
    last = first - 1_I8P + index(names(first:), NAMES_SEPARATOR, kind=I8P)
    a = a + 1_I8P
    error = h5_dataset_dims(self%root, group//'/'//names(first:last-1), dims)
    if (error == 0_I4P) then
      written = dims(size(dims)) - start(a)
      if (written /= rows .and. .not.(may_reuse .and. written == 0_I8P)) error = 1_I4P
    endif
    if (error /= 0_I4P) then
      if (self%is_temporal .and. may_reuse) then
        error = refuse('the array '//names(first:last-1)//' of '//group//' is written once in a step, for all its '// &
                       trim(str(n=rows, no_sign=.true.))//' tuples, or not at all (its values of the step before)')
      elseif (self%is_temporal .and. self%is_partitioned()) then
        error = refuse('the array '//names(first:last-1)//' of '//group//' is not written once in every piece: a '// &
                       'step with pieces writes every array')
      else
        error = refuse('the array '//names(first:last-1)//' of '//group//' is not written once in every piece')
      endif
      return
    endif
    first = last + 1_I8P
  enddo
  endfunction check_arrays

  elemental function is_reusing(self) result(is_reusing_)
  !< Return true if the open step is a step after the first one without pieces: it reuses the mesh of the step before.
  class(vtkhdf_writer), intent(in) :: self        !< Writer.
  logical                          :: is_reusing_ !< Check result.

  is_reusing_ = self%is_temporal .and. self%n_steps > 1_I8P .and. self%step_pieces == 0_I8P
  endfunction is_reusing

  subroutine region_block(self, cells, n_components, dims, mem_dims, mem_start, file_start, count)
  !< The block of a point (`cells` false) or cell array of a grid that this rank writes, its region of the whole extent:
  !< the shape of the dataset `dims` and of the values of the region `mem_dims`, where the block starts in each, its
  !< shape `count` (Fortran order, the components first if more than one; the axes of one point dropped, as
  !< `dataset_dims`). The points of an upper boundary of the region inside the whole extent are left to the region that
  !< starts there; an empty region has an empty block.
  class(vtkhdf_writer),      intent(in)  :: self          !< Writer.
  logical,                   intent(in)  :: cells         !< Cell data, otherwise point data.
  integer(I4P),              intent(in)  :: n_components  !< Components.
  integer(I8P), allocatable, intent(out) :: dims(:)       !< Shape of the dataset.
  integer(I8P), allocatable, intent(out) :: mem_dims(:)   !< Shape of the values of the region.
  integer(I8P), allocatable, intent(out) :: mem_start(:)  !< Start of the block in the values.
  integer(I8P), allocatable, intent(out) :: file_start(:) !< Start of the block in the dataset.
  integer(I8P), allocatable, intent(out) :: count(:)      !< Shape of the block.
  integer(I8P)                           :: grid(3), lo(3), hi(3), whole(3), n(3), owned(3), first(3) !< Per axis.
  logical                                :: is_axis(3)    !< The axis is kept (more than one point, or the only one).
  logical                                :: is_multi(3)   !< The axis has more than one point.

  grid = self%grid_points()
  is_multi = grid > 1_I8P
  is_axis = is_multi
  if (.not.any(is_axis)) is_axis(1) = .true.
  lo = int(self%region(1::2), I8P) ; hi = int(self%region(2::2), I8P) ; whole = int(self%whole_extent(1::2), I8P)
  first = lo - whole
  if (cells) then
    grid = max(grid - 1_I8P, 1_I8P)
    n = merge(hi - lo, 1_I8P, is_multi)
    owned = n
  else
    n = hi - lo + 1_I8P
    owned = merge(hi - lo, n, hi < int(self%whole_extent(2::2), I8P)) ! the upper boundary to the next region
  endif
  if (any(hi < lo)) then ! an empty region
    n = 0_I8P ; owned = 0_I8P ; first = 0_I8P
  endif
  if (n_components == 1_I4P) then
    dims = pack(grid, is_axis) ; mem_dims = pack(n, is_axis) ; file_start = pack(first, is_axis)
    count = pack(owned, is_axis)
  else
    dims = [int(n_components, I8P), pack(grid, is_axis)] ; mem_dims = [int(n_components, I8P), pack(n, is_axis)]
    file_start = [0_I8P, pack(first, is_axis)] ; count = [int(n_components, I8P), pack(owned, is_axis)]
    if (product(owned) == 0_I8P) count(1) = 0_I8P
  endif
  mem_start = 0_I8P * dims
  endsubroutine region_block

  function start_region(self, extent) result(error)
  !< Start the region of this rank of a grid written from the ranks of `comm`: the regions of all the ranks are gathered
  !< and checked on every rank (the same refusal on each): inside the whole extent, with cells along every axis of more
  !< than one point (or empty: an upper bound before its lower one), not overlapping, covering the whole extent.
  class(vtkhdf_writer), intent(inout) :: self      !< Writer.
  integer(I4P),         intent(in)    :: extent(6) !< Region of this rank: nx1, nx2, ny1, ny2, nz1, nz2.
  integer(I4P)                        :: error     !< Error status.
  integer(I8P)                        :: lo(3), hi(3), wlo(3), whi(3), clo(3), chi(3), olo(3), ohi(3) !< Bounds.
  integer(I8P)                        :: covered   !< Cells of all the regions.
  logical                             :: is_multi(3) !< Axes of more than one point.
  integer                             :: r, q      !< Ranks.

  error = 1_I4P
  if (self%n_pieces > 0_I4P) then
    error = refuse('write_piece: one region per rank of comm')
    return
  endif
  if (h5_mpi_allgather(self%comm, int(extent, I8P), self%regions) /= 0_I4P) then
    error = refuse('write_piece: the regions of the ranks cannot be gathered')
    return
  endif
  wlo = int(self%whole_extent(1::2), I8P) ; whi = int(self%whole_extent(2::2), I8P)
  is_multi = whi > wlo
  covered = 0_I8P
  do r=1, size(self%regions, dim=2)
    lo = self%regions(1::2, r) ; hi = self%regions(2::2, r)
    if (any(hi < lo)) cycle ! an empty region
    if (any(lo < wlo .or. hi > whi)) then
      error = refuse('write_piece: the region of rank '//trim(str(n=r - 1, no_sign=.true.))//' is not inside the whole '// &
                     'extent')
      return
    elseif (any(is_multi .and. hi == lo)) then
      error = refuse('write_piece: the region of rank '//trim(str(n=r - 1, no_sign=.true.))//' has no cells along an '// &
                     'axis (an empty region has an upper bound before its lower one)')
      return
    endif
    clo = lo ; chi = merge(hi - 1_I8P, hi, is_multi) ! its cells
    covered = covered + product(chi - clo + 1_I8P)
    do q=1, r - 1
      olo = self%regions(1::2, q) ; ohi = self%regions(2::2, q)
      if (any(ohi < olo)) cycle
      ohi = merge(ohi - 1_I8P, ohi, is_multi)
      if (all(max(clo, olo) <= min(chi, ohi))) then
        error = refuse('write_piece: the regions of ranks '//trim(str(n=q - 1, no_sign=.true.))//' and '// &
                       trim(str(n=r - 1, no_sign=.true.))//' have cells in common')
        return
      endif
    enddo
  enddo
  if (covered /= product(merge(whi - wlo, 1_I8P, is_multi))) then
    error = refuse('write_piece: the regions of the ranks do not cover the whole extent')
    return
  endif
  self%region = extent
  self%rank = h5_mpi_rank(self%comm)
  error = 0_I4P
  endfunction start_region

  function agree(self, error) result(agreed)
  !< With comm, the largest error of the ranks: a refusal by one rank (a size of its own values, say) is a refusal by
  !< every rank, before the collective calls that would leave the others waiting; the error itself without comm.
  class(vtkhdf_writer), intent(in) :: self   !< Writer.
  integer(I4P),         intent(in) :: error  !< Error of this rank.
  integer(I4P)                     :: agreed !< Error of the ranks.

  agreed = error
  if (self%is_parallel) agreed = int(h5_mpi_max(self%comm, int(error, I8P)), I4P)
  endfunction agree

  subroutine axis_runs(self, a, runs)
  !< The coordinates of the axis `a` (1 x, 2 y, 3 z) of a RectilinearGrid that this rank writes: those of its region that
  !< no rank before has in its own (the coordinates shared by regions are equal; the lowest rank writes them), as runs of
  !< consecutive entries: `runs(:, r)` = start in the values of the region, start in the dataset (from 0), length.
  class(vtkhdf_writer),      intent(in)  :: self      !< Writer.
  integer(I4P),              intent(in)  :: a         !< Axis.
  integer(I8P), allocatable, intent(out) :: runs(:,:) !< Runs.
  integer(I8P), allocatable              :: more(:,:) !< Runs, one more.
  integer(I8P)                           :: lo, hi, i !< Bounds, entry.
  integer                                :: q         !< Rank.
  logical                                :: is_taken  !< A rank before has the entry.

  allocate(runs(3, 0))
  lo = int(self%region(2 * a - 1), I8P) ; hi = int(self%region(2 * a), I8P)
  if (any(self%region(2::2) < self%region(1::2))) return ! an empty region
  do i=lo, hi
    is_taken = .false.
    do q=1, int(self%rank)
      if (any(self%regions(2::2, q) < self%regions(1::2, q))) cycle
      if (self%regions(2 * a - 1, q) <= i .and. i <= self%regions(2 * a, q)) is_taken = .true.
    enddo
    if (is_taken) cycle
    if (size(runs, dim=2) > 0) then
      if (runs(1, size(runs, dim=2)) + runs(3, size(runs, dim=2)) == i - lo) then ! the run goes on
        runs(3, size(runs, dim=2)) = runs(3, size(runs, dim=2)) + 1_I8P
        cycle
      endif
    endif
    allocate(more(3, size(runs, dim=2) + 1))
    more(:, 1:size(runs, dim=2)) = runs
    more(:, size(more, dim=2)) = [i - lo, i - int(self%whole_extent(2 * a - 1), I8P), 1_I8P]
    call move_alloc(more, runs)
  enddo
  endsubroutine axis_runs

  subroutine points_block(self, mem_dims, file_start, count)
  !< The block of the points of a StructuredGrid that this rank writes (`Points`, `[3, ni, nj, nk]` in Fortran order,
  !< every axis kept): the shape of the points of its region, where its block starts in the dataset, its shape (the points
  !< of an upper boundary inside the whole extent left to the region that starts there).
  class(vtkhdf_writer), intent(in)  :: self          !< Writer.
  integer(I8P),         intent(out) :: mem_dims(4)   !< Shape of the points of the region.
  integer(I8P),         intent(out) :: file_start(4) !< Start of the block in the dataset.
  integer(I8P),         intent(out) :: count(4)      !< Shape of the block.
  integer(I8P)                      :: lo(3), hi(3)  !< Bounds of the region.

  lo = int(self%region(1::2), I8P) ; hi = int(self%region(2::2), I8P)
  mem_dims = [3_I8P, max(hi - lo + 1_I8P, 0_I8P)]
  file_start = [0_I8P, lo - int(self%whole_extent(1::2), I8P)]
  count = [3_I8P, merge(hi - lo, hi - lo + 1_I8P, hi < int(self%whole_extent(2::2), I8P))]
  if (any(hi < lo)) then
    file_start = 0_I8P ; count = 0_I8P
  endif
  endsubroutine points_block

  function end_reusing(self) result(error)
  !< Check the arrays of a step without pieces (each one written for all the points or cells of the mesh reused, or
  !< not at all) and write the `Steps` entries of the step.
  class(vtkhdf_writer), intent(inout) :: self     !< Writer.
  integer(I4P)                        :: error    !< Error status.
  integer(I8P)                        :: rows(2)  !< Tuples of a point and of a cell array written in the step.

  rows = 1_I8P ! a grid: one time slice
  if (self%topology == TOPOLOGY_UNSTRUCTURED) then
    rows = [self%total_points - self%mesh_start(2), self%total_cells - self%mesh_start(3)]
  elseif (self%topology == TOPOLOGY_POLY) then
    rows = [self%total_points - self%mesh_start(2), self%total_cells - sum(self%mesh_blocks(1:4))]
  endif
  error = self%check_arrays('PointData', rows(1), .true.)
  if (error == 0_I4P) error = self%check_arrays('CellData', rows(2), .true.)
  if (error == 0_I4P) error = self%write_steps()
  endfunction end_reusing

  function is_written_in_step(self) result(is_written)
  !< Return true if a point or cell array is written in the open step (or its shape cannot be read).
  class(vtkhdf_writer), intent(in) :: self       !< Writer.
  logical                          :: is_written !< Check result.
  integer(I8P), allocatable        :: tuples(:)  !< Tuples of each array.

  is_written = .true.
  if (array_tuples(self%root, 'PointData', self%point_arrays, tuples) /= 0_I4P) return
  if (any(tuples > self%point_start)) return
  if (array_tuples(self%root, 'CellData', self%cell_arrays, tuples) /= 0_I4P) return
  if (any(tuples > self%cell_start)) return
  is_written = .false.
  endfunction is_written_in_step

  function roll_arrays(self) result(error)
  !< Start the bookkeeping of the point and cell arrays for a new step: the offset of the step before (where the array
  !< was written in it, or its offset before when it was not) and the tuples (slices) of the array at the new start.
  class(vtkhdf_writer), intent(inout) :: self  !< Writer.
  integer(I4P)                        :: error !< Error status.
  integer(I8P), allocatable           :: tuples(:) !< Tuples of each array.

  error = array_tuples(self%root, 'PointData', self%point_arrays, tuples)
  if (error /= 0_I4P) return
  where (tuples > self%point_start) self%point_prev = self%point_start
  self%point_start = tuples
  error = array_tuples(self%root, 'CellData', self%cell_arrays, tuples)
  if (error /= 0_I4P) return
  where (tuples > self%cell_start) self%cell_prev = self%cell_start
  self%cell_start = tuples
  endfunction roll_arrays

  subroutine set_comm(self, comm)
  !< Write the next file from the ranks of the MPI communicator `comm` (Fortran handle: `MPI_Comm%MPI_VAL` of mpi_f08):
  !< set before `initialize` (`vtk_file%initialize(..., comm=)`), forgotten by `finalize`.
  !<
  !< Every rank calls every procedure, with the same arrays in the same order (the HDF5 calls are collective): its piece is
  !< one partition of the UnstructuredGrid or PolyData, appended after the pieces of the ranks before. One piece per rank;
  !< refused: the grids, time series, polyhedra, compressor zlib.
  class(vtkhdf_writer), intent(inout) :: self !< Writer.
  integer(I4P),         intent(in)    :: comm !< MPI communicator (Fortran handle).

  self%is_parallel = .true.
  self%comm = comm
  endsubroutine set_comm

  function reopen(self, filename) result(error)
  !< Reopen a VTKHDF time series to append steps (`vtk_file%initialize(format='vtkhdf', filename=..., action='append')`):
  !< its topology, extent, steps, the totals of its pieces and its arrays are read from the file, and its datasets trimmed to
  !< the pieces recorded by the last complete piece (the `Steps` entries): what an interrupted run left after it is dropped.
  !< The next `write_step` needs a time after the last one of the file.
  class(vtkhdf_writer), intent(inout) :: self      !< Writer.
  character(*),         intent(in)    :: filename  !< File name.
  integer(I4P)                        :: error     !< Error status.
  character(len=:), allocatable       :: text      !< Type of the dataset.
  character(len=:), allocatable       :: names     !< Names of a group's members.
  integer(I8P), allocatable           :: ivalues(:), counts(:), more(:) !< Values read.
  real(R8P),    allocatable           :: times(:)  !< Times of the steps.
  integer(I8P)                        :: k         !< Steps.
  integer(I8P)                        :: n_parts   !< Parts of the complete steps.
  integer(I8P)                        :: b         !< Counter.
  integer(I8P)                        :: grid(3)   !< Points of a grid along each axis.
  integer(hid_kind)                   :: steps     !< Group Steps.
  integer(I8P)                        :: rows(2)   !< Points and cells of the mesh of the last step.

  self%file_id = -1_hid_kind ; self%root = -1_hid_kind ; self%group = -1_hid_kind
  call self%reset
  if (.not.is_hdf5_enabled) then
    error = refuse('the library is built without HDF5 (VTKFORTRAN_USE_HDF5)')
    return
  endif
  error = h5_file_open(filename, self%file_id, readwrite=.true.)
  if (error == 0_I4P) error = h5_group_open(self%file_id, 'VTKHDF', self%root)
  if (error /= 0_I4P) then
    error = refuse('action append: cannot open '//filename//' as a VTKHDF file')
    return
  endif
  error = h5_attribute_read(self%root, 'Type', text)
  select case(text)
  case('ImageData')
    self%topology = TOPOLOGY_IMAGE
  case('RectilinearGrid')
    self%topology = TOPOLOGY_RECT
  case('StructuredGrid')
    self%topology = TOPOLOGY_STRG
  case('UnstructuredGrid')
    self%topology = TOPOLOGY_UNSTRUCTURED
  case('PolyData')
    self%topology = TOPOLOGY_POLY
  case default
    error = refuse('action append: '//filename//' holds no dataset VTKFortran writes')
    return
  endselect
  if (.not.h5_link_exists(self%root, 'Steps')) then
    error = refuse('action append: '//filename//' is not a time series (no Steps): it cannot be continued')
    return
  endif
  error = h5_attribute_read(self%root, 'Version', ivalues)
  if (error == 0_I4P) self%version_minor = ivalues(2)
  if (error == 0_I4P) error = h5_group_open(self%root, 'Steps', steps)
  if (error == 0_I4P) then
    error = h5_attribute_read(steps, 'NSteps', ivalues)
    if (h5_group_close(steps) /= 0_I4P) error = 1_I4P
  endif
  k = 0_I8P
  if (error == 0_I4P) k = ivalues(1)
  if (error == 0_I4P) error = h5_dataset_read(self%root, 'Steps/Values', times)
  if (error /= 0_I4P .or. k < 1_I8P) then
    error = refuse('action append: the steps of '//filename//' cannot be read, or there are none')
    return
  endif
  self%is_temporal = .true.
  self%is_reopened = .true.
  self%n_steps = k
  self%step_time = times(k)
  self%step_pieces = 1_I8P
  if (self%is_partitioned()) then
    ! the parts of the complete steps, their totals; every dataset trimmed to them
    error = h5_dataset_read(self%root, 'Steps/PartOffsets', ivalues)
    if (error == 0_I4P) error = h5_dataset_read(self%root, 'Steps/NumberOfParts', counts)
    if (error /= 0_I4P) return
    n_parts = ivalues(k) + counts(k)
    self%n_pieces = int(n_parts, I4P)
    self%step_pieces = counts(k)
    error = trimmed_counts(self%root, n_parts, 'NumberOfPoints', self%total_points)
    if (error == 0_I4P) error = trim_to(self%root, 'Points', self%total_points)
    if (self%topology == TOPOLOGY_UNSTRUCTURED) then
      if (error == 0_I4P) error = trimmed_counts(self%root, n_parts, 'NumberOfCells', self%total_cells)
      if (error == 0_I4P) error = trimmed_counts(self%root, n_parts, 'NumberOfConnectivityIds', self%totals(4))
      if (error == 0_I4P) error = trim_to(self%root, 'Connectivity', self%totals(4))
      if (error == 0_I4P) error = trim_to(self%root, 'Offsets', self%total_cells + n_parts)
      if (error == 0_I4P) error = trim_to(self%root, 'Types', self%total_cells)
      if (error == 0_I4P .and. h5_link_exists(self%root, 'FaceConnectivity')) then
        self%has_polyhedra = .true.
        error = trimmed_counts(self%root, n_parts, 'NumberOfFaceConnectivityIds', self%totals(5))
        if (error == 0_I4P) error = trimmed_counts(self%root, n_parts, 'NumberOfFaces', self%totals(6))
        if (error == 0_I4P) error = trimmed_counts(self%root, n_parts, 'NumberOfPolyhedronToFaceIds', self%totals(7))
        if (error == 0_I4P) error = trim_to(self%root, 'FaceConnectivity', self%totals(5))
        if (error == 0_I4P) error = trim_to(self%root, 'FaceOffsets', self%totals(6) + n_parts)
        if (error == 0_I4P) error = trim_to(self%root, 'PolyhedronToFaces', self%totals(7))
        if (error == 0_I4P) error = trim_to(self%root, 'PolyhedronOffsets', self%total_cells + n_parts)
      endif
    else
      do b=1_I8P, size(BLOCKS, kind=I8P)
        if (error == 0_I4P) error = trimmed_counts(self%root, n_parts, trim(BLOCKS(b))//'/NumberOfCells', self%block_totals(b))
        if (error == 0_I4P) error = trimmed_counts(self%root, n_parts, trim(BLOCKS(b))//'/NumberOfConnectivityIds', &
                                                   self%block_totals(4+b))
        if (error == 0_I4P) error = trim_to(self%root, trim(BLOCKS(b))//'/Connectivity', self%block_totals(4+b))
        if (error == 0_I4P) error = trim_to(self%root, trim(BLOCKS(b))//'/Offsets', self%block_totals(b) + n_parts)
      enddo
      self%total_cells = sum(self%block_totals(1:4))
    endif
    ! the mesh of the last step (the one a step without pieces would reuse): its entries
    self%mesh_pieces = counts(k)
    self%mesh_start(1) = ivalues(k)
    if (error == 0_I4P) error = step_value(self%root, 'PointOffsets', k, 1_I8P, self%mesh_start(2))
    if (self%topology == TOPOLOGY_UNSTRUCTURED) then
      if (error == 0_I4P) error = step_value(self%root, 'CellOffsets', k, 1_I8P, self%mesh_start(3))
      if (error == 0_I4P) error = step_value(self%root, 'ConnectivityIdOffsets', k, 1_I8P, self%mesh_start(4))
      if (self%has_polyhedra) then
        if (error == 0_I4P) error = step_value(self%root, 'FaceConnectivityOffsets', k, 1_I8P, self%mesh_start(5))
        if (error == 0_I4P) error = step_value(self%root, 'FaceOffsetsOffsets', k, 1_I8P, self%mesh_start(6))
        if (error == 0_I4P) error = step_value(self%root, 'PolyhedronToFaceIdOffsets', k, 1_I8P, self%mesh_start(7))
      endif
      rows = [self%total_points - self%mesh_start(2), self%total_cells - self%mesh_start(3)]
    else
      do b=1_I8P, size(BLOCKS, kind=I8P) ! the blocks in the order of VTKHDF: Vertices, Lines, Polygons, Strips
        if (error == 0_I4P) error = step_value(self%root, 'CellOffsets', k, VTK_COLUMN(b), self%mesh_blocks(b))
        if (error == 0_I4P) error = step_value(self%root, 'ConnectivityIdOffsets', k, VTK_COLUMN(b), self%mesh_blocks(4+b))
      enddo
      rows = [self%total_points - self%mesh_start(2), self%total_cells - sum(self%mesh_blocks(1:4))]
    endif
    ! the arrays: the tuples of the mesh from their offset of the last step (repeated by the steps that reused them)
    if (error == 0_I4P) error = trimmed_arrays(self%root, 'PointData', k, rows(1), self%mesh_start(2), self%point_arrays, &
                                               self%point_start, self%point_prev)
    if (error == 0_I4P) error = trimmed_arrays(self%root, 'CellData', k, rows(2), self%total_cells - rows(2), &
                                               self%cell_arrays, self%cell_start, self%cell_prev)
  else
    ! a grid: its extent (the start of a RectilinearGrid or StructuredGrid one is not stored), a time slice per step
    if (self%topology == TOPOLOGY_IMAGE) then
      error = h5_attribute_read(self%root, 'WholeExtent', ivalues)
      if (error == 0_I4P) self%whole_extent = int(ivalues, I4P)
    else
      error = h5_attribute_read(self%root, 'Dimensions', ivalues)
      if (error == 0_I4P) then
        self%whole_extent(1::2) = 0_I4P
        self%whole_extent(2::2) = int(ivalues - 1_I8P, I4P)
      endif
    endif
    self%n_pieces = int(k, I4P)
    grid = self%grid_points()
    ! the geometry of the last step that wrote it (its offset at the last step), one time slice per such step
    if (error == 0_I4P .and. self%topology == TOPOLOGY_RECT) then
      error = step_value(self%root, 'XCoordinatesOffsets', k, 1_I8P, self%geo_slices)
      self%geo_slices = self%geo_slices / grid(1) + 1_I8P
      if (error == 0_I4P) error = trim_to(self%root, 'XCoordinates', self%geo_slices * grid(1))
      if (error == 0_I4P) error = trim_to(self%root, 'YCoordinates', self%geo_slices * grid(2))
      if (error == 0_I4P) error = trim_to(self%root, 'ZCoordinates', self%geo_slices * grid(3))
    elseif (error == 0_I4P .and. self%topology == TOPOLOGY_STRG) then
      error = step_value(self%root, 'PointOffsets', k, 1_I8P, self%geo_slices)
      self%geo_slices = self%geo_slices + 1_I8P
      if (error == 0_I4P) error = trim_to(self%root, 'Points', self%geo_slices)
    endif
    ! the arrays: one time slice from their offset of the last step (k - 1 for a file without data offsets)
    if (error == 0_I4P) error = trimmed_arrays(self%root, 'PointData', k, 1_I8P, k - 1_I8P, self%point_arrays, &
                                               self%point_start, self%point_prev)
    if (error == 0_I4P) error = trimmed_arrays(self%root, 'CellData', k, 1_I8P, k - 1_I8P, self%cell_arrays, &
                                               self%cell_start, self%cell_prev)
  endif
  ! field data: the tuples of the complete steps (a group not written is empty)
  names = ''
  if (error == 0_I4P .and. h5_link_exists(self%root, 'FieldData')) error = h5_group_names(self%root, 'FieldData', names)
  self%field_arrays = names
  if (error == 0_I4P) then
    b = 1_I8P
    do while (b <= len(names, kind=I8P) .and. error == 0_I4P)
      text = names(b:b-2_I8P+index(names(b:), NAMES_SEPARATOR, kind=I8P))
      error = h5_dataset_read(self%root, 'Steps/FieldDataOffsets/'//text, ivalues)
      if (error == 0_I4P) error = h5_dataset_read(self%root, 'Steps/FieldDataSizes/'//text, more)
      if (error == 0_I4P) error = trim_to(self%root, 'FieldData/'//text, ivalues(k) + more(2_I8P * k))
      b = b + len(text, kind=I8P) + 1_I8P
    enddo
  endif
  ! the Steps entries of the complete steps
  if (error == 0_I4P) error = trimmed_steps(self%root, 'Steps', k)
  do b=1_I8P, size(STEP_GROUPS, kind=I8P)
    if (error == 0_I4P .and. h5_link_exists(self%root, 'Steps/'//trim(STEP_GROUPS(b)))) &
      error = trimmed_steps(self%root, 'Steps/'//trim(STEP_GROUPS(b)), k)
  enddo
  if (error /= 0_I4P) then
    error = refuse('action append: cannot read or trim the datasets of '//filename)
    return
  endif
  self%step_start = [int(self%n_pieces, I8P), self%total_points, self%total_cells, self%totals(4:)]
  self%step_blocks = self%block_totals
  contains
    function trim_to(root, name, n) result(error)
    !< Trim the dataset `name` of /VTKHDF to `n` along its last dimension (root and sizes passed: nvfortran internal
    !< procedures can read stale host values).
    integer(hid_kind), intent(in) :: root !< Group /VTKHDF.
    character(*), intent(in) :: name  !< Dataset.
    integer(I8P), intent(in) :: n     !< Size kept.
    integer(I4P)             :: error !< Error status.

    error = h5_dataset_resize(root, name, n)
    endfunction trim_to

    function trimmed_counts(root, n_parts, name, total) result(error)
    !< Trim the counts `name` (one per part) to the parts of the complete steps, and return their sum.
    integer(hid_kind), intent(in) :: root    !< Group /VTKHDF.
    integer(I8P), intent(in)  :: n_parts !< Parts of the complete steps.
    character(*), intent(in)  :: name  !< Dataset of counts.
    integer(I8P), intent(out) :: total !< Sum of the counts kept.
    integer(I4P)              :: error !< Error status.
    integer(I8P), allocatable :: values(:) !< Counts.

    total = 0_I8P
    error = h5_dataset_read(root, name, values)
    if (error /= 0_I4P) return
    if (size(values, kind=I8P) < n_parts) then
      error = 1_I4P
      return
    endif
    total = sum(values(1:n_parts))
    error = trim_to(root, name, n_parts)
    endfunction trimmed_counts

    function trimmed_arrays(root, group, k, rows, default, list, start, prev) result(error)
    !< Trim every array of `group` to the `rows` tuples (slices) of the last complete step `k` from its offset at that step
    !< (`Steps/<group>Offsets/<name>`, `default` without it), list their names (none if the group was never written), and
    !< start their bookkeeping: tuples at the start of the next step, offset of the last one.
    integer(hid_kind),             intent(in)  :: root     !< Group /VTKHDF.
    character(*),                  intent(in)  :: group    !< PointData or CellData.
    integer(I8P),                  intent(in)  :: k        !< Last complete step (NSteps).
    integer(I8P),                  intent(in)  :: rows     !< Tuples (slices) of the last step.
    integer(I8P),                  intent(in)  :: default  !< Offset of an array without its offsets dataset.
    character(len=:), allocatable, intent(out) :: list     !< Names, each followed by NAMES_SEPARATOR.
    integer(I8P), allocatable,     intent(out) :: start(:) !< Tuples of each array kept.
    integer(I8P), allocatable,     intent(out) :: prev(:)  !< Offset of each array at the last step.
    integer(I4P)                               :: error    !< Error status.
    integer(I8P), allocatable                  :: offsets(:) !< Offsets of an array.
    integer(I8P)                               :: first, last !< Bounds of a name.
    integer(I8P)                               :: offset   !< Offset of an array at the last step.

    list = ''
    allocate(start(0), prev(0))
    error = 0_I4P
    if (h5_link_exists(root, group)) error = h5_group_names(root, group, list)
    first = 1_I8P
    do while (first <= len(list, kind=I8P) .and. error == 0_I4P)
      last = first - 1_I8P + index(list(first:), NAMES_SEPARATOR, kind=I8P)
      offset = default
      if (h5_link_exists(root, 'Steps/'//group//'Offsets/'//list(first:last-1))) then
        error = h5_dataset_read(root, 'Steps/'//group//'Offsets/'//list(first:last-1), offsets)
        ! the entry of step k: an interrupted run may have left one more, of a step not complete
        if (error == 0_I4P) then
          if (size(offsets, kind=I8P) >= k) then
            offset = offsets(k)
          else
            error = 1_I4P
          endif
        endif
      endif
      if (error == 0_I4P) error = trim_to(root, group//'/'//list(first:last-1), offset + rows)
      start = [start, offset + rows]
      prev = [prev, offset]
      first = last + 1_I8P
    enddo
    endfunction trimmed_arrays

    function step_value(root, name, k, column, value) result(error)
    !< The entry of the step k of `Steps/name`: its value, or its value of the column `column` (VTK order of the blocks:
    !< Vertices, Lines, Polygons, Strips) for a PolyData, whose datasets have a row per step.
    integer(hid_kind), intent(in)  :: root      !< Group /VTKHDF.
    character(*),      intent(in)  :: name      !< Dataset of Steps.
    integer(I8P),      intent(in)  :: k         !< Step.
    integer(I8P),      intent(in)  :: column    !< Column; 1 for a dataset of one value per step.
    integer(I8P),      intent(out) :: value     !< Value.
    integer(I4P)                   :: error     !< Error status.
    integer(I8P), allocatable      :: values(:) !< Values of the steps.
    integer(I8P), allocatable      :: dims(:)   !< Shape, Fortran order.
    integer(I8P)                   :: n         !< Values per step.

    value = 0_I8P
    error = h5_dataset_dims(root, 'Steps/'//name, dims)
    if (error == 0_I4P) error = h5_dataset_read(root, 'Steps/'//name, values)
    if (error /= 0_I4P) return
    n = 1_I8P
    if (size(dims) > 1) n = dims(1)
    value = values((k - 1_I8P) * n + column)
    endfunction step_value

    function trimmed_steps(root, group, n_steps) result(error)
    !< Trim every dataset of the group `group` (Steps or one of its groups) to the complete steps.
    integer(hid_kind), intent(in) :: root   !< Group /VTKHDF.
    character(*), intent(in)      :: group  !< Group.
    integer(I8P), intent(in)      :: n_steps !< Complete steps.
    integer(I4P)                  :: error  !< Error status.
    character(len=:), allocatable :: list   !< Names of its members.
    integer(I8P)                  :: first, last !< Bounds of a name.
    integer(I4P)                  :: g      !< Counter.
    logical                       :: is_group !< The member is a group of Steps.

    error = h5_group_names(root, group, list)
    first = 1_I8P
    do while (first <= len(list, kind=I8P) .and. error == 0_I4P)
      last = first - 1_I8P + index(list(first:), NAMES_SEPARATOR, kind=I8P)
      is_group = .false.
      do g=1, size(STEP_GROUPS)
        if (list(first:last-1) == trim(STEP_GROUPS(g))) is_group = .true.
      enddo
      if (.not.is_group) error = trim_to(root, group//'/'//list(first:last-1), n_steps)
      first = last + 1_I8P
    enddo
    endfunction trimmed_steps
  endfunction reopen

  elemental subroutine reset(self)
  !< Reset the state of the writer, but the HDF5 identifiers.
  class(vtkhdf_writer), intent(inout) :: self !< Writer.
  integer(I4P)                        :: r    !< Counter.

  self%group_name = ''
  self%whole_extent = 0_I4P
  self%n_pieces = 0_I4P
  self%is_piece_open = .false.
  self%version_minor = 0_I8P
  self%topology = 0_I4P
  self%np_piece = 0_I8P
  self%nc_piece = 0_I8P
  self%block_cells = 0_I8P
  self%total_points = 0_I8P
  self%total_cells = 0_I8P
  self%is_geo_written = .false.
  self%is_connectivity_written = .false.
  self%has_polyhedra = .false.
  self%is_faces_written = .false.
  self%is_temporal = .false.
  self%n_steps = 0_I8P
  self%step_time = 0._R8P
  self%step_pieces = 0_I8P
  self%step_start = 0_I8P
  self%step_blocks = 0_I8P
  self%totals = 0_I8P
  self%block_totals = 0_I8P
  self%mesh_start = 0_I8P
  self%mesh_blocks = 0_I8P
  self%mesh_pieces = 0_I8P
  self%geo_slices = 0_I8P
  self%region = 0_I4P
  if (allocated(self%regions)) deallocate(self%regions)
  self%point_start = [integer(I8P) ::] ; self%point_prev = [integer(I8P) ::]
  self%cell_start = [integer(I8P) ::] ; self%cell_prev = [integer(I8P) ::]
  self%field_arrays = ''
  self%is_reopened = .false.
  self%point_arrays = ''
  self%cell_arrays = ''
  do r=1, size(ROLES)
    if (allocated(self%active(r)%name)) deallocate(self%active(r)%name)
  enddo
  endsubroutine reset

  function start_partition(self, np, nc, block_cells) result(error)
  !< Start a piece of an UnstructuredGrid or PolyData: a partition, its counts appended to NumberOfPoints (and
  !< NumberOfCells for an UnstructuredGrid; the blocks of a PolyData get theirs with their cells).
  class(vtkhdf_writer), intent(inout)        :: self           !< Writer.
  integer(I8P),         intent(in)           :: np             !< Number of points.
  integer(I8P),         intent(in)           :: nc             !< Number of cells.
  integer(I8P),         intent(in), optional :: block_cells(4) !< Cells of each block (PolyData).
  integer(I4P)                               :: error          !< Error status.

  error = 1_I4P
  if (self%topology == TOPOLOGY_UNSTRUCTURED .and. present(block_cells)) then
    error = refuse('write_piece(np, nverts, nlines, nstrips, npolys) is for a PolyData: an UnstructuredGrid piece is '// &
                   'write_piece(np, nc)')
    return
  elseif (self%topology == TOPOLOGY_POLY .and. .not.present(block_cells)) then
    error = refuse('write_piece(np, nc) is for an UnstructuredGrid: a PolyData piece is '// &
                   'write_piece(np, nverts, nlines, nstrips, npolys)')
    return
  elseif (self%topology /= TOPOLOGY_UNSTRUCTURED .and. self%topology /= TOPOLOGY_POLY) then
    error = refuse('write_piece(np, ...) is for an UnstructuredGrid or a PolyData: the piece of a grid is '// &
                   'write_piece(nx1, ..., nz2)')
    return
  endif
  if (self%file_id < 0_hid_kind .or. self%is_piece_open .or. self%group >= 0_hid_kind) return
  if (np < 0_I8P .or. nc < 0_I8P) then
    error = refuse('write_piece: negative counts')
    return
  endif
  if (present(block_cells)) then
    if (any(block_cells < 0_I8P)) then
      error = refuse('write_piece: negative counts')
      return
    endif
    self%block_cells = block_cells
  endif
  if (self%is_parallel .and. self%n_pieces > 0_I4P) then
    error = refuse('write_piece: one piece per rank of comm')
    return
  endif
  if (self%is_reusing()) then
    if (self%is_written_in_step()) then
      error = refuse('write_piece: arrays are already written in this step without its pieces')
      return
    endif
  endif
  self%n_pieces = self%n_pieces + 1_I4P
  self%step_pieces = self%step_pieces + 1_I8P
  self%np_piece = np
  self%nc_piece = nc
  if (self%is_parallel) then ! the datasets hold the pieces of all the ranks
    self%total_points = self%total_points + h5_mpi_sum(self%comm, np)
    self%total_cells = self%total_cells + h5_mpi_sum(self%comm, nc)
  else
    self%total_points = self%total_points + np
    self%total_cells = self%total_cells + nc
  endif
  self%is_geo_written = .false.
  self%is_connectivity_written = .false.
  self%is_faces_written = .false.
  error = store_I8P(self, self%root, 'NumberOfPoints', [np], [1_I8P])
  if (error == 0_I4P .and. self%topology == TOPOLOGY_UNSTRUCTURED) &
    error = store_I8P(self, self%root, 'NumberOfCells', [nc], [1_I8P])
  if (error /= 0_I4P) then
    error = refuse('cannot write the counts of the piece')
    return
  endif
  self%is_piece_open = .true.
  endfunction start_partition

  function start_polyhedra(self) result(error)
  !< Start the polyhedra of the file, at the first piece that has some: the earlier pieces get their zero entries (from
  !< their counts of cells), and the version is at least 2.5.
  class(vtkhdf_writer), intent(inout) :: self   !< Writer.
  integer(I4P)                        :: error  !< Error status.
  integer(I8P), allocatable           :: nc(:)  !< Cells of each piece started.
  integer(I8P)                        :: p      !< Counter.

  error = 0_I4P
  if (self%has_polyhedra) return
  error = h5_dataset_read(self%root, 'NumberOfCells', nc)
  if (error /= 0_I4P) then
    error = refuse('cannot read the counts of cells of the pieces')
    return
  endif
  do p=1_I8P, size(nc, kind=I8P) - 1_I8P ! the open piece is the last one
    error = self%no_faces(nc(p))
    if (error /= 0_I4P) return
  enddo
  self%has_polyhedra = .true.
  self%version_minor = max(self%version_minor, 5_I8P)
  endfunction start_polyhedra

  function step_entry(self, name, values) result(error)
  !< Write the entry of the open step (the last one, `n_steps`) of the dataset `Steps/name`: the dataset is trimmed to
  !< the steps before (an entry written at the end of an earlier piece of the step is replaced), or created with zero
  !< entries for them; `values` is one value, or one row (`[NSteps, size(values)]` in C order).
  class(vtkhdf_writer), intent(inout) :: self      !< Writer.
  character(*),         intent(in)    :: name      !< Dataset name in Steps.
  integer(I8P),         intent(in)    :: values(:) !< Values of the step.
  integer(I4P)                        :: error     !< Error status.
  integer(I8P), allocatable           :: zeros(:)  !< Entries of the steps before, for a dataset new at a later step.
  integer(I8P)                        :: n         !< Values per step.

  n = size(values, kind=I8P)
  error = 0_I4P
  if (h5_link_exists(self%root, 'Steps/'//name)) then
    error = h5_dataset_resize(self%root, 'Steps/'//name, self%n_steps - 1_I8P)
  elseif (self%n_steps > 1_I8P) then
    allocate(zeros(n * (self%n_steps - 1_I8P)))
    zeros = 0_I8P
    if (n == 1_I8P) then
      error = store_I8P(self, self%root, 'Steps/'//name, zeros, [self%n_steps - 1_I8P])
    else
      error = store_I8P(self, self%root, 'Steps/'//name, zeros, [n, self%n_steps - 1_I8P])
    endif
  endif
  if (error == 0_I4P) then
    if (n == 1_I8P) then
      error = store_I8P(self, self%root, 'Steps/'//name, values, [1_I8P])
    else
      error = store_I8P(self, self%root, 'Steps/'//name, values, [n, 1_I8P])
    endif
  endif
  endfunction step_entry

  function write_steps(self) result(error)
  !< Write the `Steps` entries of the open step (time, offsets of the parts, points, cells, ids, faces, coordinates and
  !< field data), `NSteps` and `Version`, and flush the file: called at the end of every piece of a time series, it
  !< leaves the file valid, with the steps written so far.
  class(vtkhdf_writer), intent(inout) :: self      !< Writer.
  integer(I4P)                        :: error     !< Error status.
  integer(I8P), allocatable           :: offsets(:) !< Field data offsets of the steps written.
  integer(I8P), allocatable           :: sizes(:)  !< Field data sizes of the steps written (components, tuples).
  integer(I8P), allocatable           :: dims(:)   !< Shape of a field data array.
  integer(I8P)                        :: k         !< The open step.
  integer(I8P)                        :: offset    !< Field data tuples before the open step.
  integer(I8P)                        :: grid(3)   !< Points of a grid along each axis.
  integer(hid_kind)                   :: steps     !< Group Steps.
  integer(I8P)                        :: first     !< First character of a name.
  integer(I8P)                        :: last      !< Separator after a name.
  character(len=:), allocatable       :: name      !< An array name.
  logical                             :: reused    !< A field data array not written in the step.

  k = self%n_steps
  if (h5_link_exists(self%root, 'Steps/Values')) then
    error = h5_dataset_resize(self%root, 'Steps/Values', k - 1_I8P)
    if (error /= 0_I4P) return
  endif
  error = store_R8P(self, self%root, 'Steps/Values', [self%step_time], [1_I8P])
  if (self%is_partitioned()) then
    ! the parts of the step, or those of the last step with pieces (a step without pieces reuses its mesh)
    if (self%step_pieces > 0_I8P) then
      self%mesh_start = self%step_start
      self%mesh_blocks = self%step_blocks
      self%mesh_pieces = self%step_pieces
    endif
    if (error == 0_I4P) error = self%step_entry('PartOffsets', [self%mesh_start(1)])
    if (error == 0_I4P) error = self%step_entry('NumberOfParts', [self%mesh_pieces])
    if (error == 0_I4P) error = self%step_entry('PointOffsets', [self%mesh_start(2)])
    if (self%topology == TOPOLOGY_UNSTRUCTURED) then
      if (error == 0_I4P) error = self%step_entry('CellOffsets', [self%mesh_start(3)])
      if (error == 0_I4P) error = self%step_entry('ConnectivityIdOffsets', [self%mesh_start(4)])
      if (self%has_polyhedra) then
        if (error == 0_I4P) error = self%step_entry('FaceConnectivityOffsets', [self%mesh_start(5)])
        if (error == 0_I4P) error = self%step_entry('FaceOffsetsOffsets', [self%mesh_start(6)])
        if (error == 0_I4P) error = self%step_entry('PolyhedronToFaceIdOffsets', [self%mesh_start(7)])
      endif
    else ! PolyData: the blocks in the order of VTKHDF, Vertices, Lines, Polygons, Strips
      if (error == 0_I4P) error = self%step_entry('CellOffsets', self%mesh_blocks([1, 2, 4, 3]))
      if (error == 0_I4P) error = self%step_entry('ConnectivityIdOffsets', self%mesh_blocks([5, 6, 8, 7]))
    endif
  elseif (self%topology == TOPOLOGY_RECT) then ! the coordinates of the last step that wrote them
    grid = self%grid_points()
    if (error == 0_I4P) error = self%step_entry('XCoordinatesOffsets', [(self%geo_slices - 1_I8P) * grid(1)])
    if (error == 0_I4P) error = self%step_entry('YCoordinatesOffsets', [(self%geo_slices - 1_I8P) * grid(2)])
    if (error == 0_I4P) error = self%step_entry('ZCoordinatesOffsets', [(self%geo_slices - 1_I8P) * grid(3)])
  elseif (self%topology == TOPOLOGY_STRG) then ! in time slices (required by VTK)
    if (error == 0_I4P) error = self%step_entry('PointOffsets', [self%geo_slices - 1_I8P])
  endif
  ! point and cell data: where each array was written in the step, or its offset of the step before (reused); tuples,
  ! time slices for a grid
  if (error == 0_I4P) error = array_entries('PointData', 'PointDataOffsets', self%point_arrays, self%point_start, &
                                            self%point_prev)
  if (error == 0_I4P) error = array_entries('CellData', 'CellDataOffsets', self%cell_arrays, self%cell_start, &
                                            self%cell_prev)
  ! field data: one component, their tuples of the step after those of the steps before
  first = 1_I8P
  do while (first <= len(self%field_arrays, kind=I8P) .and. error == 0_I4P)
    last = first - 1_I8P + index(self%field_arrays(first:), NAMES_SEPARATOR, kind=I8P)
    name = self%field_arrays(first:last-1)
    offset = 0_I8P
    reused = .false.
    if (error == 0_I4P) error = h5_dataset_dims(self%root, 'FieldData/'//name, dims)
    if (k > 1_I8P .and. h5_link_exists(self%root, 'Steps/FieldDataOffsets/'//name)) then
      error = h5_dataset_read(self%root, 'Steps/FieldDataOffsets/'//name, offsets)
      if (error == 0_I4P) error = h5_dataset_read(self%root, 'Steps/FieldDataSizes/'//name, sizes)
      if (error == 0_I4P) then
        offset = offsets(k - 1_I8P) + sizes(2_I8P * (k - 1_I8P))
        ! not written in the step: its values of the step before
        reused = dims(size(dims)) == offset .and. sizes(2_I8P * (k - 1_I8P)) > 0_I8P
        if (reused) offset = offsets(k - 1_I8P)
      endif
    endif
    if (error == 0_I4P) error = self%step_entry('FieldDataOffsets/'//name, [offset])
    if (error == 0_I4P .and. reused) then
      error = self%step_entry('FieldDataSizes/'//name, sizes(2_I8P * k - 3_I8P:2_I8P * k - 2_I8P))
    elseif (error == 0_I4P) then
      error = self%step_entry('FieldDataSizes/'//name, [1_I8P, dims(size(dims)) - offset])
    endif
    first = last + 1_I8P
  enddo
  if (error == 0_I4P) error = h5_group_open(self%root, 'Steps', steps)
  if (error == 0_I4P) then
    error = h5_attribute_write(steps, 'NSteps', [k])
    if (h5_group_close(steps) /= 0_I4P) error = 1_I4P
  endif
  if (error == 0_I4P) error = h5_attribute_write(self%root, 'Version', [2_I8P, self%version_minor])
  if (error == 0_I4P) error = h5_file_flush(self%file_id)
  if (error /= 0_I4P) error = refuse('cannot write the entries of the step')
  contains
    function array_entries(group, steps_group, names, start, prev) result(error)
    !< Write the offsets of the arrays of `group` for the open step: `start` if the array was written in it, `prev` if
    !< not (its values of the step before).
    character(*), intent(in)  :: group       !< PointData or CellData.
    character(*), intent(in)  :: steps_group !< PointDataOffsets or CellDataOffsets.
    character(*), intent(in)  :: names       !< Names, each followed by NAMES_SEPARATOR.
    integer(I8P), intent(in)  :: start(:)    !< Tuples of each array at the start of the step.
    integer(I8P), intent(in)  :: prev(:)     !< Offset of each array at the step before.
    integer(I4P)              :: error       !< Error status.
    integer(I8P), allocatable :: tuples(:)   !< Tuples of each array.
    integer(I8P)              :: first, last !< Bounds of a name.
    integer(I8P)              :: a           !< Counter.

    error = array_tuples(self%root, group, names, tuples)
    first = 1_I8P
    a = 0_I8P
    do while (first <= len(names, kind=I8P) .and. error == 0_I4P)
      last = first - 1_I8P + index(names(first:), NAMES_SEPARATOR, kind=I8P)
      a = a + 1_I8P
      error = self%step_entry(steps_group//'/'//names(first:last-1), [merge(start(a), prev(a), tuples(a) > start(a))])
      first = last + 1_I8P
    enddo
    endfunction array_entries
  endfunction write_steps

  ! non type-bound helpers
  function array_tuples(root, group, names, tuples) result(error)
  !< Return the tuples (time slices for a grid of a time series), the last dimension, of every array of the list.
  integer(hid_kind),         intent(in)  :: root      !< Group /VTKHDF.
  character(*),              intent(in)  :: group     !< Group of the arrays.
  character(*),              intent(in)  :: names     !< Names, each followed by NAMES_SEPARATOR.
  integer(I8P), allocatable, intent(out) :: tuples(:) !< Tuples of each array.
  integer(I4P)                           :: error     !< Error status.
  integer(I8P), allocatable              :: dims(:)   !< Shape of a dataset, Fortran order.
  integer(I8P)                           :: first     !< First character of a name.
  integer(I8P)                           :: last      !< Separator after a name.
  integer(I8P)                           :: a         !< Counter.

  a = 0_I8P
  do first=1_I8P, len(names, kind=I8P)
    if (names(first:first) == NAMES_SEPARATOR) a = a + 1_I8P
  enddo
  allocate(tuples(a))
  error = 0_I4P
  first = 1_I8P
  a = 0_I8P
  do while (first <= len(names, kind=I8P) .and. error == 0_I4P)
    last = first - 1_I8P + index(names(first:), NAMES_SEPARATOR, kind=I8P)
    a = a + 1_I8P
    error = h5_dataset_dims(root, group//'/'//names(first:last-1), dims)
    if (error == 0_I4P) tuples(a) = dims(size(dims))
    first = last + 1_I8P
  enddo
  if (error /= 0_I4P) error = refuse('cannot read the shape of the arrays of '//group)
  endfunction array_tuples

  function refuse(message) result(error)
  !< Print an error message on stderr and return a non-zero error.
  character(*), intent(in) :: message !< Message.
  integer(I4P)             :: error   !< Error status.

  write(stderr, '(A)') 'error: VTKFortran VTKHDF writer: '//message
  error = 1_I4P
  endfunction refuse

  pure function upper(string) result(upper_string)
  !< Return a string left-adjusted, trimmed and in upper case (ASCII letters only).
  character(*), intent(in)      :: string       !< Input string.
  character(len=:), allocatable :: upper_string !< Upper case string.
  integer                       :: i            !< Counter.

  upper_string = trim(adjustl(string))
  do i=1, len(upper_string)
    if (upper_string(i:i) >= 'a' .and. upper_string(i:i) <= 'z') upper_string(i:i) = achar(iachar(upper_string(i:i)) - 32)
  enddo
  endfunction upper
endmodule vtk_fortran_vtk_file_vtkhdf_writer
