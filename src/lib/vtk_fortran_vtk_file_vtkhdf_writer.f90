!< VTK file VTKHDF writer: an HDF5 file in the VTKHDF layout (every topology of `vtk_file`).
module vtk_fortran_vtk_file_vtkhdf_writer
!< VTK file VTKHDF writer: an HDF5 file in the VTKHDF layout (every topology of `vtk_file`).
!<
!< The writer of `vtk_file%initialize(format='VTKHDF', ...)`: the same calls as the XML formats write the group `/VTKHDF`
!< of an HDF5 file (VTKHDF specification 2.x, read by VTK >= 9.1 and ParaView): the attributes `Version`, `Type`,
!< `WholeExtent`, `Origin`, `Spacing`, `Direction`, the groups `PointData`, `CellData` and `FieldData`, one dataset per
!< data array. A Fortran array `x(n_components, i, j, k)` is stored as it is in memory: it is the VTKHDF dataset of shape
!< `[k, j, i, n_components]` (C order), so nothing is transposed. With `compressor='zlib'` the datasets are deflated.
!< The datasets that grow or are deflated are chunked by whole rows, at most `chunk_bytes` (the writer state set by
!< `vtk_file%initialize(..., chunk_bytes=)`, default 1 MiB) and the rows of the first write, at least one row
!< (`dataset_write` of `vtk_fortran_hdf5`); every call creating one passes `chunk_bytes=self%chunk_bytes`.
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
!< after the first one, a data name (point, cell or field data) empty or containing `/` or `.` (VTKHDF names cannot:
!< no renaming, the code reading the file back would not find the array), and every call when the library is built
!< without HDF5 (`VTKFORTRAN_USE_HDF5`).
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
!< before; in a time series a piece per rank in each step, every rank calling `write_step` with the same time, the
!< `Steps` entries the totals of all the ranks, written once; `reopen` by the ranks for a parallel restart; polyhedra on
!< any ranks; compressed datasets with an HDF5 of parallel filtered writes, see `set_comm`), or a region
!< per rank of one ImageData, RectilinearGrid or StructuredGrid (`write_piece(nx1, ..., nz2)` with
!< the rank's sub-extent): the datasets are created with the whole extent, by every rank, and each rank writes the block
!< of its region, the points of a boundary shared with the next region excluded (that region writes them), the
!< coordinates of a RectilinearGrid entries no rank before has. The regions are checked when they start (gathered on
!< every rank): inside the whole extent, cells not overlapping, covering it; an empty region (`nx2 < nx1`, ...) writes
!< nothing. A refusal by one rank of its own values (their size, say) is agreed by all the ranks (`agree`) before the
!< collective calls: every rank returns the error, none is left waiting.
!<
!< `reopen` (`vtk_file%initialize(..., action='append')`; a refused one leaves the file as it was) continues a time series
!< written before (a restart): the state is
!< read from the file, and every dataset is trimmed to the pieces recorded by the last complete piece (a run interrupted
!< in a piece leaves the data of that piece, dropped).
!<
!< Table (`mesh_topology='Table'`, VTKHDF 2.8): no pieces; its columns are written with `write_dataarray(location='row',
!< action='open')`, `write_dataarray(data_name, x)` (any kind, components as for the other data arrays: `RowData/<name>`,
!< `[rows, components]` in C order), `write_dataarray(location='row', action='close')`: every column the same rows, and
!< `NumberOfRows`. In a time series every step writes every column (the same columns as the first step), the rows of
!< each step appended (`NumberOfRows` one entry per step, `Steps/RowDataOffsets/<column>`, the spelling VTK 9.7.2
!< writes and reads, not the `RowDataOffets` of the specification page), the file valid after each step.
!<
!< OverlappingAMR (`mesh_topology='OverlappingAMR'`, `origin` of `vtk_file%initialize` its origin): each level is
!< started by `write_level(level, spacing)` (0, 1, ... in order; a level can be started again, with its spacing), its boxes
!< are pieces, `write_piece(nx1, nx2, ny1, ny2, nz1, nz2)` (the point extent of the box in the indices of the level, at least
!< one cell along every axis, a box of a finer level inside the bounds of the boxes of level 0), their point and cell data
!< (`[n]` or `[n, components]`, x fastest), `write_piece()`. The layout, as VTK 9.7.2 reads it: `Origin`, `GridDescription`
!< `XYZ`, the groups `Level<n>` with `Spacing`, `AMRBox` (one row per box: the cells `nx1, nx2-1, ny1, ny2-1, nz1, nz2-1`)
!< and the data of the boxes appended in `PointData`, `CellData`; every box of every level writes the same arrays. In a
!< time series every step writes its levels and boxes again (`Steps/Level<n>/AMRBoxOffsets`, `NumberOfAMRBoxes`,
!< `PointDataOffsets/<name>`, `CellDataOffsets/<name>`: the names of the specification since VTKHDF 2.3, so `Version`
!< is at least 2.3: VTK 9.7.2 reads the singular names of 2.2 only below 2.3), the file valid after each box. Refused:
!< field data, `comm`, a step without boxes, the XML formats.
!<
!< HyperTreeGrid (`mesh_topology='HyperTreeGrid'`, `nx1`...`nz2` the points of its root grid, `branch_factor` of
!< `vtk_file%initialize` 2 or 3): one piece, `write_piece(nx1, ..., nz2)`, its root coordinates `write_geo(x, y, z)` (as a
!< RectilinearGrid: `XCoordinates`, `YCoordinates`, `ZCoordinates`), its trees `write_tree(tree_id, descriptor, mask)`
!< (the tree of root cell `tree_id` = i + (nx-1)(j + (ny-1)k), from 0; `descriptor` the refinement of its cells, breadth
!< first, the children of a cell x fastest, every depth but the deepest, whose cells are all leaves; `mask` one bit per
!< cell, breadth first, a refined cell not masked), then its cell data (one value per cell of the trees, in the order of
!< the trees written, breadth first in each), `write_piece()`. The layout, as VTK 9.7.2 writes it: `Dimensions`,
!< `BranchFactor`, `Descriptors` and `Mask` (bits packed MSB first, contiguous across the trees), `DescriptorsSize` (bits),
!< `TreeIds`, `DepthPerTree`, `NumberOfCellsPerTreeDepth`, `NumberOfTrees`, `NumberOfDepths`, `NumberOfCells`, `CellData`.
!< A time series (`write_step`): every step writes its piece (its coordinates, trees and cell data), appended to the
!< datasets of the steps before, each step's bits from a new byte, the mask always; `Steps` holds `PartOffsets`,
!< `NumberOfParts`, `X/Y/ZCoordinatesOffsets`, `TreeIdsOffsets`, `DepthPerTreeOffsets` (trees),
!< `NumberOfCellsPerTreeDepthOffsets`, `DescriptorsOffsets`, `MaskOffsets` (bytes), `CellDataOffsets/<name>`, as VTK
!< 9.7.2 writes them; the file is valid after each step.
!< Refused: point data, field data, a step without its piece, `comm`, `action='append'`, the XML formats.
!<
!< `Version` is written by `finalize` (and after every piece of a time series): 2.0, 2.5 with polyhedra, 2.6 when an
!< active array is marked, 2.7 for a RectilinearGrid or a StructuredGrid, 2.8 for a Table, 2.3 for an OverlappingAMR time
!< series, 2.4 for a HyperTreeGrid (the highest of the features written).
use penf
use vtk_fortran_hdf5
use vtk_fortran_parameters, only : stderr
use vtk_fortran_vtk_writer_abstract

implicit none
private
public :: vtkhdf_writer

character(*), parameter :: ROLES(12) = [character(18) :: 'Scalars', 'Vectors', 'Normals', 'Tensors', 'TCoords',      &
                                        'GlobalIds', 'PedigreeIds', 'EdgeFlag', 'Tangents', 'RationalWeights', &
                                        'HigherOrderDegrees', 'ProcessIds'] !< Roles of active arrays (VTK's names).
integer(I4P), parameter :: DEFLATE_LEVEL_ZLIB = 6_I4P !< Deflate level of compressed datasets (the zlib default).
integer(I4P), parameter :: TOPOLOGY_IMAGE = 1_I4P        !< ImageData.
integer(I4P), parameter :: TOPOLOGY_UNSTRUCTURED = 2_I4P !< UnstructuredGrid.
integer(I4P), parameter :: TOPOLOGY_POLY = 3_I4P         !< PolyData.
integer(I4P), parameter :: TOPOLOGY_RECT = 4_I4P         !< RectilinearGrid.
integer(I4P), parameter :: TOPOLOGY_STRG = 5_I4P         !< StructuredGrid.
integer(I4P), parameter :: TOPOLOGY_TABLE = 6_I4P        !< Table.
integer(I4P), parameter :: TOPOLOGY_AMR = 7_I4P          !< OverlappingAMR.
integer(I4P), parameter :: TOPOLOGY_HTG = 8_I4P          !< HyperTreeGrid.
integer(I1P), parameter :: CELL_POLYHEDRON = 42_I1P     !< VTK type of a polyhedron.
character(*), parameter :: STEP_GROUPS(4) = ['PointDataOffsets', 'CellDataOffsets ', 'FieldDataOffsets', & !< Groups of
                                              'FieldDataSizes  ']                                       !< Steps.
character(*), parameter :: BLOCKS(4) = ['Vertices', 'Lines   ', 'Strips  ', 'Polygons'] !< Groups of the PolyData cells.
integer(I8P), parameter :: VTK_COLUMN(4) = [1_I8P, 2_I8P, 4_I8P, 3_I8P] !< Column of each block in the rows of Steps.
character(*), parameter :: NAMES_SEPARATOR = achar(0)    !< Separator of the names of a list (a name has no NUL).
integer(I8P), parameter :: ZERO_BLOCK = 1048576_I8P     !< Zeros appended at once (polyhedra starting late, with comm).

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
  integer(I4P)                  :: n_pieces=0_I4P          !< Pieces started (with comm, the partitions of all the ranks).
  integer(I4P)                  :: local_pieces=0_I4P      !< Pieces started by this process.
  integer(I4P)                  :: step_local=0_I4P        !< Pieces started by this process in the open step.
  logical                       :: is_piece_open=.false.   !< A piece is open.
  integer(I8P)                  :: version_minor=0_I8P     !< Minor version needed by the features written.
  type(active_array)            :: active(12)              !< Active arrays of the open group, one per role.
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
  character(len=:), allocatable :: row_arrays              !< Names of the columns of a Table, NUL terminated.
  integer(I8P)                  :: table_rows=-1_I8P       !< Rows of the columns of the open step (-1: no column yet).
  integer(I8P)                  :: row_total=0_I8P         !< Rows of the steps before the open one (a Table).
  integer(hid_kind)             :: level_group=-1_hid_kind !< Group of the open level of an OverlappingAMR.
  integer(I4P)                  :: level=-1_I4P            !< Open level (from 0), -1 if none.
  integer(I4P)                  :: n_levels=0_I4P          !< Levels written.
  real(R8P), allocatable        :: level_spacing(:,:)      !< Spacing of each level, `(3, n_levels)`.
  integer(I8P), allocatable     :: level_counts(:,:)       !< Of each level: boxes, points, cells written, and the same
                                                           !< at the start of the open step, `(6, n_levels)`.
  logical                       :: has_domain=.false.      !< A box of level 0 is written (in the open step).
  real(R8P)                     :: domain(6)=0._R8P        !< Bounds of the boxes of level 0: x, y, z min and max.
  logical, allocatable          :: tree_bits(:)            !< Refinement bits of the trees written (HyperTreeGrid).
  logical, allocatable          :: tree_mask(:)            !< Mask bits of the cells of the trees written.
  logical                       :: has_mask=.false.        !< A tree written has a mask.
  integer(I8P), allocatable     :: tree_ids(:)             !< Trees written.
  integer(I8P), allocatable     :: tree_depths(:)          !< Depths of the trees written.
  integer(I8P), allocatable     :: tree_counts(:)          !< Cells of each depth of each tree written.
  integer(I8P)                  :: tree_cells=0_I8P        !< Cells of the trees written (the mask bits used).
  integer(I8P)                  :: n_tree_ids=0_I8P        !< Trees written (`tree_ids`, `tree_depths` used).
  integer(I8P)                  :: n_tree_counts=0_I8P     !< Entries of `tree_counts` used.
  integer(I8P)                  :: n_tree_bits=0_I8P       !< Refinement bits used.
  logical                       :: is_tree_data=.false.    !< The cell data of the trees are started.
  integer(I8P)                  :: tree_totals(5)=0_I8P    !< Of the steps before (HyperTreeGrid series): trees, depth
                                                           !< entries, descriptor bytes, mask bytes, cells.
  logical                       :: is_parallel=.false.     !< The file is written by the ranks of an MPI communicator.
  integer(I4P)                  :: region(6)=0_I4P         !< Region of this rank of a grid written from ranks: its extent.
  logical                       :: has_region=.false.      !< The region is known (set by write_piece, not after a reopen).
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
    procedure, pass(self) :: write_level
    procedure, pass(self) :: write_tree
    procedure, pass(self) :: write_fielddata_tag
    procedure, pass(self) :: write_fielddata1_rank0
    procedure, pass(self) :: write_fielddata1_rank1
    procedure, pass(self) :: free
    procedure, pass(self) :: write_dataarray_strings
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
    procedure, pass(self), private :: reopen_file   !< Read the state of a time series reopened, trim its datasets.
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
    procedure, pass(self), private :: add_face_totals !< Add the faces of a piece to the totals (with comm, of the ranks).
    procedure, pass(self), private :: poly_block    !< Check or write a block of cells of a PolyData piece.
    procedure, pass(self), private :: poly_blocks   !< Check or write the four blocks of cells of a PolyData piece.
    procedure, pass(self), private :: end_reusing   !< Check the arrays of a step without pieces, write its entries.
    procedure, pass(self), private :: end_table     !< End the columns of a Table (static, or a step).
    procedure, pass(self), private :: end_box       !< End a box of an OverlappingAMR: check its arrays.
    procedure, pass(self), private :: is_table_complete !< A Table has the NumberOfRows entry of every step.
    procedure, pass(self), private :: end_trees     !< End the piece of a HyperTreeGrid: write its trees.
    procedure, pass(self), private :: is_recorded   !< A data array name is recorded (point or cell data).
    procedure, pass(self), private :: open_level    !< Open (or create) a level of an OverlappingAMR.
    procedure, pass(self), private :: start_box     !< Start a box of an OverlappingAMR (write_piece of the level).
    procedure, pass(self), private :: is_reusing    !< The open step has no piece: it reuses the mesh of the step before.
    procedure, pass(self), private :: is_written_in_step !< A point or cell array is written in the open step.
    procedure, pass(self), private :: record_array  !< Record the name of a point or cell data array.
    procedure, pass(self), private :: agree         !< With comm, the largest error of the ranks.
    procedure, pass(self), private :: agree_name    !< With comm, refuse an array that is not the same on every rank.
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

  self%file_id = -1_hid_kind ; self%root = -1_hid_kind ; self%group = -1_hid_kind ; self%level_group = -1_hid_kind
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
  case('Table')
    self%topology = TOPOLOGY_TABLE
    self%version_minor = 8_I8P
  case('OverlappingAMR')
    self%topology = TOPOLOGY_AMR
  case('HyperTreeGrid')
    self%topology = TOPOLOGY_HTG
    self%version_minor = 4_I8P
  case default
    error = refuse('topology '//trim(adjustl(mesh_topology))//' is not supported')
    return
  endselect
  if (self%topology == TOPOLOGY_HTG .and. self%branch_factor /= 2_I4P .and. self%branch_factor /= 3_I4P) then
    error = refuse('the branch factor of a HyperTreeGrid is 2 or 3')
    return
  elseif (any(self%topology == [TOPOLOGY_TABLE, TOPOLOGY_AMR, TOPOLOGY_HTG]) .and. self%is_parallel) then
    error = refuse('comm: a '//trim(adjustl(mesh_topology))//' is written by one process')
    return
  elseif (.not.self%is_partitioned() .and. self%topology /= TOPOLOGY_TABLE .and. self%topology /= TOPOLOGY_AMR) then
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
    elseif (self%is_compressed .and. .not.self%is_partitioned()) then
      error = refuse('comm: compressor zlib is for an UnstructuredGrid or a PolyData (the regions of a grid are '// &
                     'written by each rank alone)')
    elseif (self%is_compressed .and. .not.h5_parallel_filters_available()) then
      error = refuse('comm: compressor zlib needs an HDF5 1.10.2 or later with the deflate filter (parallel filtered '// &
                     'writes)')
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
  elseif (self%topology == TOPOLOGY_TABLE) then
    if (error == 0_I4P) error = h5_attribute_write(self%root, 'Type', 'Table')
  elseif (self%topology == TOPOLOGY_HTG) then
    if (error == 0_I4P) error = h5_attribute_write(self%root, 'Type', 'HyperTreeGrid')
    if (error == 0_I4P) error = h5_attribute_write(self%root, 'Dimensions', self%grid_points())
    if (error == 0_I4P) error = h5_attribute_write(self%root, 'BranchFactor', [int(self%branch_factor, I8P)])
  elseif (self%topology == TOPOLOGY_AMR) then
    if (error == 0_I4P) error = h5_attribute_write(self%root, 'Type', 'OverlappingAMR')
    if (error == 0_I4P) error = h5_attribute_write(self%root, 'Origin', self%origin)
    if (error == 0_I4P) error = h5_attribute_write(self%root, 'GridDescription', 'XYZ')
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
  !< A group left open is closed (the columns of a Table ended). A piece left open is refused for an OverlappingAMR and
  !< a HyperTreeGrid, whose boxes and trees it writes. The last step of a time series can be a step without pieces (it
  !< reuses the mesh of the step before), not the first one.
  class(vtkhdf_writer),       intent(inout) :: self  !< Writer.
  integer(I4P)                              :: error !< Error status.

  error = 1_I4P
  if (self%file_id < 0_hid_kind) return
  error = 0_I4P
  if (self%topology == TOPOLOGY_TABLE) then ! the columns left open closed, ended; every step with its columns
    if (self%group >= 0_hid_kind .and. self%group_name == 'RowData') then
      error = self%close_group()
      if (error == 0_I4P) error = self%end_table()
    elseif (self%step_pieces == 0_I8P) then
      error = refuse('the Table, or its last step, has no columns')
    elseif (.not.self%is_table_complete()) then ! the close of its columns refused
      error = refuse('the columns of the Table (of its last step) were refused: the file is not complete')
    endif
  elseif (self%topology == TOPOLOGY_HTG .and. self%is_piece_open) then
    error = refuse('the piece of the HyperTreeGrid is not closed (write_piece()): its trees are not written')
  elseif (self%topology == TOPOLOGY_HTG .and. self%is_temporal .and. self%step_pieces == 0_I8P) then
    error = refuse('the last step of the HyperTreeGrid has no piece')
  elseif (self%topology == TOPOLOGY_AMR) then ! every box closed; every step with boxes
    if (self%is_piece_open) then
      error = refuse('the last box of the OverlappingAMR is not closed (write_piece())')
    elseif (self%step_pieces == 0_I8P) then
      error = refuse('the OverlappingAMR, or its last step, has no box')
    endif
  elseif (self%is_temporal .and. self%step_pieces == 0_I8P .and. self%n_steps == 1_I8P) then
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
  if (self%level_group >= 0_hid_kind) then
    if (h5_group_close(self%level_group) /= 0_I4P) error = 1_I4P
  endif
  if (h5_attribute_write(self%root, 'Version', [2_I8P, self%version_minor]) /= 0_I4P) error = 1_I4P
  if (h5_group_close(self%root) /= 0_I4P) error = 1_I4P
  if (h5_file_close(self%file_id) /= 0_I4P) error = 1_I4P
  self%file_id = -1_hid_kind ; self%root = -1_hid_kind ; self%group = -1_hid_kind ; self%level_group = -1_hid_kind
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
  if (self%topology == TOPOLOGY_TABLE) then
    error = refuse('a Table has no pieces: its columns are written with write_dataarray(location=row, ...)')
  elseif (self%topology == TOPOLOGY_AMR) then ! a box of the open level
    error = self%start_box([nx1, nx2, ny1, ny2, nz1, nz2])
  elseif (self%is_partitioned()) then
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
  elseif (self%topology == TOPOLOGY_AMR) then
    error = self%end_box()
  elseif (self%topology == TOPOLOGY_HTG) then ! its root coordinates, its trees
    if (.not.self%is_geo_written) then
      error = refuse('the piece of a HyperTreeGrid needs write_geo (its root coordinates)')
    else
      error = self%end_trees()
      if (error == 0_I4P .and. self%is_temporal) error = self%check_arrays('CellData', self%tree_cells, .false.)
    endif
  elseif ((self%topology == TOPOLOGY_RECT .or. self%topology == TOPOLOGY_STRG) .and. .not.self%is_geo_written &
          .and. .not.(self%is_temporal .and. self%geo_slices > 0_I8P)) then ! a later step reuses the geometry
    error = refuse('the piece of a RectilinearGrid or StructuredGrid needs write_geo (in the first step)')
  endif
  if (error == 0_I4P .and. self%is_temporal) then
    if (.not.self%is_partitioned() .and. self%topology /= TOPOLOGY_AMR .and. self%topology /= TOPOLOGY_HTG) then ! a grid
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
  integer(hid_kind)                         :: group_id     !< A group of Steps.
  integer(I4P)                              :: g            !< Counter.
  logical                                   :: is_same_time !< The ranks give the same time.

  error = 1_I4P
  if (self%file_id < 0_hid_kind) return
  is_same_time = .true. ! with comm, the same time on every rank (no short-circuit: the collective call apart)
  if (self%is_parallel) is_same_time = h5_mpi_max(self%comm, transfer(time, 0_I8P)) == &
                                       -h5_mpi_max(self%comm, -transfer(time, 0_I8P))
  if (.not.is_same_time) then
    error = refuse('write_step: the ranks of comm give different times')
  elseif (self%is_piece_open .or. self%group >= 0_hid_kind) then
    error = refuse('write_step: outside the pieces and the data groups')
  elseif (.not.self%is_temporal .and. (self%n_pieces > 0_I4P .or. h5_link_exists(self%root, 'FieldData'))) then
    error = refuse('write_step: a time series starts with write_step, before its first piece and field data')
  elseif (self%topology == TOPOLOGY_TABLE .and. (self%step_pieces > 0_I8P .neqv. self%is_temporal)) then
    error = refuse('write_step: a time series of a Table starts with write_step, every step with its columns')
  elseif (self%topology == TOPOLOGY_HTG .and. self%is_temporal .and. self%step_pieces == 0_I8P) then
    error = refuse('write_step: every step of a HyperTreeGrid writes its piece')
  elseif (self%topology == TOPOLOGY_AMR .and. self%is_temporal .and. self%step_pieces == 0_I8P) then
    error = refuse('write_step: every step of an OverlappingAMR writes its boxes')
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
      self%step_local = 0_I4P
      if (self%topology == TOPOLOGY_HTG) then ! the totals of the steps before; the trees of the new step
        self%tree_totals = self%tree_totals + [self%n_tree_ids, self%n_tree_counts, (self%n_tree_bits + 7_I8P) / 8_I8P, &
                                               (self%tree_cells + 7_I8P) / 8_I8P, self%tree_cells]
        self%n_tree_ids = 0_I8P ; self%n_tree_counts = 0_I8P ; self%n_tree_bits = 0_I8P ; self%tree_cells = 0_I8P
        self%has_mask = .false. ; self%is_tree_data = .false.
      endif
      if (self%topology == TOPOLOGY_AMR) then ! the levels are started again; the counts at the start of the step
        ! 2.3: the Steps names of the specification (plural); VTK reads them by Version, singular before 2.3
        self%version_minor = max(self%version_minor, 3_I8P)
        if (allocated(self%level_counts)) self%level_counts(4:6, :) = self%level_counts(1:3, :)
        self%has_domain = .false.
        if (self%level_group >= 0_hid_kind) error = h5_group_close(self%level_group)
        self%level_group = -1_hid_kind
        self%level = -1_I4P
      endif
      if (self%topology == TOPOLOGY_TABLE) then ! the rows of the steps before; the offsets group
        self%row_total = self%row_total + max(self%table_rows, 0_I8P)
        self%table_rows = -1_I8P
        if (.not.h5_link_exists(self%root, 'Steps/RowDataOffsets')) then
          error = h5_group_create(self%root, 'Steps/RowDataOffsets', group_id, track_order=.true.)
          if (error == 0_I4P) error = h5_group_close(group_id)
        endif
      endif
      self%step_start = [int(self%n_pieces, I8P), self%total_points, self%total_cells, self%totals(4:)]
      self%step_blocks = self%block_totals
      if (error == 0_I4P .and. self%topology /= TOPOLOGY_AMR) error = self%roll_arrays()
    endif
  endif
  endfunction write_step

  function write_dataarray_location_tag(self, location, action, scalars, vectors, normals, tensors, tcoords, &
      global_ids, pedigree_ids, edge_flags, tangents, rational_weights, &
      higher_order_degrees, process_ids) &
      result(error)
  !< Open or close the group of the point (`node`) or cell (`cell`) data, inside the piece (or in a step without pieces,
  !< which reuses the mesh of the step before: its arrays hold the points or cells of all the parts reused), or the
  !< group of the columns (`row`) of a Table (no piece; once, or once in each step of a time series).
  !<
  !< When opening, `scalars`, `vectors`, `normals`, `tensors`, `tcoords`, `global_ids`, `pedigree_ids`, `edge_flags`,
  !< `tangents`, `rational_weights`, `higher_order_degrees` and `process_ids` name the active array of each role of VTK:
  !< the dataset of that name, when written, gets the `Attribute` attribute (VTKHDF 2.6) holding the role (`Scalars`,
  !< ..., `ProcessIds`, the names VTK writes).
  class(vtkhdf_writer),       intent(inout)        :: self
  character(*),               intent(in)           :: location
  character(*),               intent(in)           :: action
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
  integer(I4P)                                     :: error
  character(len=:), allocatable                    :: location_            !< Location, upper case.
  character(len=:), allocatable                    :: action_              !< Action, upper case.
  character(len=:), allocatable                    :: name                 !< Group name.

  location_ = upper(location) ; action_ = upper(action)
  error = 1_I4P
  if (location_ == 'NODE') then
    name = 'PointData'
  elseif (location_ == 'CELL') then
    name = 'CellData'
  elseif (location_ == 'ROW') then
    name = 'RowData'
  else
    return
  endif
  if (name == 'PointData' .and. self%topology == TOPOLOGY_HTG) then
    error = refuse('write_dataarray: a HyperTreeGrid has cell data only')
    return
  endif
  if ((name == 'RowData') .neqv. (self%topology == TOPOLOGY_TABLE)) then
    error = refuse('write_dataarray: the columns of a Table (location row) go in a Table, and only them')
    return
  endif
  if (action_ == 'OPEN') then
    ! inside the piece, or in a step without pieces (it reuses the mesh of the step before)
    if (name == 'RowData') then ! the columns of a Table (no piece): once per step
      if (self%group >= 0_hid_kind .or. self%step_pieces > 0_I8P) then
        error = refuse('write_dataarray: the columns of a Table are written once (in each step)')
        return
      endif
      self%step_pieces = 1_I8P ! the columns of the Table (of the step) opened
    elseif (.not.(self%is_piece_open .or. self%is_reusing()) .or. self%group >= 0_hid_kind) then
      return
    elseif (self%is_reusing() .and. self%is_parallel .and. .not.self%is_partitioned() .and. .not.self%has_region) then
      ! a grid reopened by the ranks: the region of each rank is not in the file (every rank refuses: the same state)
      error = refuse('write_dataarray: after a restart by the ranks of comm, the first step of a grid writes its '// &
                     'piece (write_piece with the region of the rank): the regions are not in the file')
      return
    endif
    error = self%open_group(name)
    if (error == 0_I4P .and. name == 'CellData' .and. self%topology == TOPOLOGY_HTG) self%is_tree_data = .true.
    if (present(scalars)) self%active(1)%name = scalars
    if (present(vectors)) self%active(2)%name = vectors
    if (present(normals)) self%active(3)%name = normals
    if (present(tensors)) self%active(4)%name = tensors
    if (present(tcoords)) self%active(5)%name = tcoords
    if (present(global_ids)) self%active(6)%name = global_ids
    if (present(pedigree_ids)) self%active(7)%name = pedigree_ids
    if (present(edge_flags)) self%active(8)%name = edge_flags
    if (present(tangents)) self%active(9)%name = tangents
    if (present(rational_weights)) self%active(10)%name = rational_weights
    if (present(higher_order_degrees)) self%active(11)%name = higher_order_degrees
    if (present(process_ids)) self%active(12)%name = process_ids
  elseif (action_ == 'CLOSE') then
    if (self%group < 0_hid_kind) return
    if (self%group_name /= name) return
    error = self%close_group()
    if (error == 0_I4P .and. self%is_reusing()) error = self%end_reusing() ! the file valid after the group
    if (error == 0_I4P .and. name == 'RowData') error = self%end_table() ! the rows checked, written
  endif
  endfunction write_dataarray_location_tag

  function write_level(self, level, spacing) result(error)
  !< Start the level `level` (0, 1, ... in order; a level started before can be started again, with its spacing) of an
  !< OverlappingAMR, of spacing `spacing`: its boxes follow, each a `write_piece(nx1, ..., nz2)` (its point extent in the
  !< indices of the level) with its point and cell data, closed by `write_piece()`. In a time series every step starts
  !< its levels again.
  class(vtkhdf_writer),       intent(inout) :: self
  integer(I4P),               intent(in)    :: level
  real(R8P),                  intent(in)    :: spacing(3)
  integer(I4P)                              :: error

  error = 1_I4P
  if (self%file_id < 0_hid_kind) return
  if (self%topology /= TOPOLOGY_AMR) then
    error = refuse('write_level is for an OverlappingAMR')
  elseif (self%is_piece_open .or. self%group >= 0_hid_kind) then
    error = refuse('write_level: outside the boxes and the data groups')
  elseif (level < 0_I4P .or. level > self%n_levels) then
    error = refuse('write_level: the levels are 0, 1, ... in order (the next new one is '// &
                   trim(str(n=self%n_levels, no_sign=.true.))//')')
  elseif (.not.all(spacing > 0._R8P)) then
    error = refuse('write_level: the spacing must be positive')
  elseif (self%is_temporal .and. self%n_steps == 0_I8P) then
    error = refuse('write_level: a time series starts with write_step')
  else
    error = 0_I4P
    if (level < self%n_levels) then
      if (any(spacing /= self%level_spacing(:, level + 1_I4P))) error = 1_I4P
    endif
    if (error /= 0_I4P) then
      error = refuse('write_level: the spacing of a level is fixed')
    else
      error = self%open_level(level, spacing)
    endif
  endif
  endfunction write_level

  function write_tree(self, tree_id, descriptor, mask) result(error)
  !< Write the tree `tree_id` (its root cell, from 0: i + (nx-1)(j + (ny-1)k) of the root grid) of a HyperTreeGrid, in its
  !< piece, after its coordinates and before its cell data: `descriptor` the refinement (true) of its cells breadth first,
  !< the children of a cell x fastest, for every depth but the deepest (all its cells are leaves: an empty descriptor is a
  !< tree of one leaf); `mask` (optional) one value per cell of the tree, breadth first, a refined cell not masked.
  class(vtkhdf_writer),       intent(inout)        :: self
  integer(I4P),               intent(in)           :: tree_id
  logical,                    intent(in)           :: descriptor(:)
  logical,                    intent(in), optional :: mask(:)
  integer(I4P)                                     :: error
  integer(I8P), allocatable                        :: counts(:)     !< Cells of each depth.
  integer(I8P)                                     :: children      !< Children of a refined cell.
  integer(I8P)                                     :: n_trees       !< Trees of the root grid.
  integer(I8P)                                     :: first         !< Bits of the descriptor used.
  integer(I8P)                                     :: n             !< Cells of a depth.
  integer(I8P)                                     :: refined       !< Cells refined at a depth.
  integer(I8P)                                     :: cells         !< Cells of the tree.

  error = 1_I4P
  if (self%file_id < 0_hid_kind) return
  children = int(self%branch_factor, I8P)**count(self%grid_points() > 1_I8P)
  n_trees = product(max(self%grid_points() - 1_I8P, 1_I8P))
  if (self%topology /= TOPOLOGY_HTG) then
    error = refuse('write_tree is for a HyperTreeGrid')
    return
  elseif (.not.self%is_piece_open .or. self%group >= 0_hid_kind .or. self%is_tree_data) then
    error = refuse('write_tree: in the piece, outside and before its cell data')
    return
  elseif (tree_id < 0_I4P .or. int(tree_id, I8P) >= n_trees) then
    error = refuse('write_tree: the tree '//trim(str(n=tree_id, no_sign=.true.))//' is not a root cell (0 to '// &
                   trim(str(n=n_trees - 1_I8P, no_sign=.true.))//')')
    return
  endif
  if (self%n_tree_ids > 0_I8P) then
    if (any(self%tree_ids(1:self%n_tree_ids) == int(tree_id, I8P))) then
      error = refuse('write_tree: the tree '//trim(str(n=tree_id, no_sign=.true.))//' is already written')
      return
    endif
  endif
  ! the cells of each depth: one root, then the children of the cells refined at the depth before
  counts = [1_I8P]
  first = 0_I8P
  do while (first < size(descriptor, kind=I8P))
    n = counts(size(counts))
    if (first + n > size(descriptor, kind=I8P)) then
      error = refuse('write_tree: the descriptor ends inside a depth (the cells of a depth are all given)')
      return
    endif
    refined = count(descriptor(first + 1_I8P:first + n), kind=I8P)
    if (refined == 0_I8P) then
      error = refuse('write_tree: the descriptor gives the deepest depth, whose cells are all leaves: leave it out')
      return
    endif
    first = first + n
    counts = [counts, refined * children]
  enddo
  cells = sum(counts)
  if (present(mask)) then
    if (size(mask, kind=I8P) /= cells) then
      error = refuse('write_tree: the mask needs one value per cell of the tree, '//trim(str(n=cells, no_sign=.true.)))
      return
    elseif (any(mask(1:size(descriptor)) .and. descriptor)) then
      error = refuse('write_tree: a refined cell cannot be masked')
      return
    endif
  endif
  n = self%n_tree_ids
  call append_I8P(self%tree_ids, self%n_tree_ids, [int(tree_id, I8P)])
  call append_I8P(self%tree_depths, n, [size(counts, kind=I8P)])
  call append_I8P(self%tree_counts, self%n_tree_counts, counts)
  call append_bits(self%tree_bits, self%n_tree_bits, descriptor, size(descriptor, kind=I8P))
  call append_bits(self%tree_mask, self%tree_cells, mask, cells) ! false without a mask
  if (present(mask)) self%has_mask = .true.
  error = 0_I4P
  endfunction write_tree

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
    if (self%topology == TOPOLOGY_AMR .or. self%topology == TOPOLOGY_HTG) then
      error = refuse('field data of '//trim(merge('an OverlappingAMR', 'a HyperTreeGrid  ', self%topology == TOPOLOGY_AMR))// &
                     ' are not supported')
      return
    endif
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
    error = put_strings(self, data_name, [x])
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
    error = put_strings(self, data_name, x)
  class default
    error = refuse('field data of this type are not supported')
  endselect
  endfunction write_fielddata1_rank1

  elemental subroutine free(self, error)
  !< Free the writer state (the file is closed by `finalize`).
  class(vtkhdf_writer),       intent(inout)         :: self
  integer(I4P),               intent(out), optional :: error

  self%file_id = -1_hid_kind ; self%root = -1_hid_kind ; self%group = -1_hid_kind ; self%level_group = -1_hid_kind
  call self%reset
  self%is_parallel = .false.
  if (present(error)) error = 0_I4P
  endsubroutine free

  function write_dataarray_strings(self, data_name, x) result(error)
  !< A string array of point, cell data or a Table column: refused (VTK 9.7.2 reads the strings of a
  !< VTKHDF file as field data only: give them with `write_fielddata`).
  class(vtkhdf_writer),       intent(inout) :: self
  character(*),               intent(in)    :: data_name
  character(*),               intent(in)    :: x(1:)
  integer(I4P)                              :: error

  error = 1_I4P
  if (self%group < 0_hid_kind) return
  error = refuse('the string array '//data_name//' of '//self%group_name//' is not written: VTK 9.7.2 reads the '// &
                 'strings of a VTKHDF file as field data only (write_fielddata)')
  endfunction write_dataarray_strings

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

  error = 0_I4P ! the checks of this rank, agreed by the ranks of comm before their collective calls
  if (present(face) .neqv. present(faceoffset)) then
    error = refuse('write_connectivity: face and faceoffset go together')
  elseif (present(face)) then
    error = put_faces_I4P(self, int(nc, I8P), face, faceoffset, cell_type, .true.)
  elseif (any(cell_type == CELL_POLYHEDRON)) then
    error = refuse('write_connectivity: a polyhedron (cell type 42) needs face and faceoffset')
  endif
  error = self%agree(error) ! with comm, a refusal on every rank before the collective writes
  if (error /= 0_I4P) return
  error = put_cells_I4P(self, int(nc, I8P), connectivity, offset, cell_type)
  if (error /= 0_I4P) return
  if (self%is_parallel) then ! the file has polyhedra if a rank has some: every rank writes its faces, or none
    if (h5_mpi_max(self%comm, merge(1_I8P, 0_I8P, present(face))) > 0_I8P) then
      error = self%start_polyhedra()
      if (error == 0_I4P .and. present(face)) then
        error = put_faces_I4P(self, int(nc, I8P), face, faceoffset, cell_type, .false.)
      elseif (error == 0_I4P) then
        error = self%no_faces(int(nc, I8P))
      endif
      if (error == 0_I4P) self%is_faces_written = .true.
    endif
  elseif (present(face)) then
    error = put_faces_I4P(self, int(nc, I8P), face, faceoffset, cell_type, .false.)
  endif
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

  error = 0_I4P ! the checks of this rank, agreed by the ranks of comm before their collective calls
  if (present(face) .neqv. present(faceoffset)) then
    error = refuse('write_connectivity: face and faceoffset go together')
  elseif (present(face)) then
    error = put_faces_I8P(self, int(nc, I8P), face, faceoffset, cell_type, .true.)
  elseif (any(cell_type == CELL_POLYHEDRON)) then
    error = refuse('write_connectivity: a polyhedron (cell type 42) needs face and faceoffset')
  endif
  error = self%agree(error) ! with comm, a refusal on every rank before the collective writes
  if (error /= 0_I4P) return
  error = put_cells_I8P(self, int(nc, I8P), connectivity, offset, cell_type)
  if (error /= 0_I4P) return
  if (self%is_parallel) then ! the file has polyhedra if a rank has some: every rank writes its faces, or none
    if (h5_mpi_max(self%comm, merge(1_I8P, 0_I8P, present(face))) > 0_I8P) then
      error = self%start_polyhedra()
      if (error == 0_I4P .and. present(face)) then
        error = put_faces_I8P(self, int(nc, I8P), face, faceoffset, cell_type, .false.)
      elseif (error == 0_I4P) then
        error = self%no_faces(int(nc, I8P))
      endif
      if (error == 0_I4P) self%is_faces_written = .true.
    endif
  elseif (present(face)) then
    error = put_faces_I8P(self, int(nc, I8P), face, faceoffset, cell_type, .false.)
  endif
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

  if (len(h5_name_invalid(data_name)) > 0) then ! a VTKHDF name: no / nor .
    error = refuse('the array name "'//data_name//'" '//h5_name_invalid(data_name))
  else
    error = self%dataset_dims(data_name, n, n_components, dims)
  endif
  error = self%agree(error) ! with comm, a refusal on every rank before the collective writes
  if (error /= 0_I4P) return
  if (self%is_parallel) error = self%agree_name(data_name) ! the same array on every rank
  if (error /= 0_I4P) return
  is_new = .not.h5_link_exists(self%group, data_name)
  if (self%is_temporal .and. is_new .and. self%n_steps > 1_I8P .and. self%topology /= TOPOLOGY_AMR) then
    error = refuse('the array '//data_name//' is not in the first step: every step writes the same arrays')
    return
  endif
  if (self%group_name == 'RowData') then ! a column of a Table: the rows of the others
    if (self%table_rows >= 0_I8P .and. dims(size(dims)) /= self%table_rows) then
      error = refuse('the column '//data_name//' has '//trim(str(n=dims(size(dims)), no_sign=.true.))// &
                     ' rows, the other columns '//trim(str(n=self%table_rows, no_sign=.true.)))
      return
    endif
    self%table_rows = dims(size(dims))
    error = store_R8P(self, self%group, data_name, x, dims)
  elseif (self%topology == TOPOLOGY_HTG .and. self%is_temporal .and. self%group_name /= 'FieldData') then
    error = store_R8P(self, self%group, data_name, x, dims) ! appended
  elseif (self%topology == TOPOLOGY_AMR .and. self%group_name /= 'FieldData') then ! a box
    if (is_new .and. (self%level_counts(1, self%level + 1_I4P) > 1_I8P .or. &
                      (.not.self%is_recorded(data_name) .and. self%n_pieces > 1_I4P))) then
      error = refuse('the array '//data_name//' is not in the first box: every box of every level writes the same arrays')
      return
    endif
    error = store_R8P(self, self%group, data_name, x, dims)
  elseif (self%is_partitioned() .and. self%group_name /= 'FieldData') then
    if (is_new .and. self%local_pieces > 1_I4P) then
      error = refuse('the array '//data_name//' is not in the first piece: every piece writes the same arrays')
      return
    endif
    error = store_R8P(self, self%group, data_name, x, dims)
  elseif (self%is_parallel .and. self%group_name /= 'FieldData') then ! a grid: the block of this rank
    call self%region_block(self%group_name == 'CellData', n_components, dims, mem_dims, mem_start, &
                           file_start, count)
    if (self%is_temporal) then ! the time axis last: one slice more per step, each rank its block
      dims = [dims, 1_I8P] ; mem_dims = [mem_dims, 1_I8P] ; mem_start = [mem_start, 0_I8P]
      file_start = [file_start, 0_I8P] ; count = [count, 1_I8P]
      error = h5_dataset_slab_R8P(self%group, data_name, x, dims, mem_dims, mem_start, file_start, count, is_new, &
                          append=.true., chunk_bytes=self%chunk_bytes)
    else
      error = h5_dataset_slab_R8P(self%group, data_name, x, dims, mem_dims, mem_start, file_start, count, .true.)
    endif
  elseif (self%is_temporal .and. self%group_name /= 'FieldData') then ! a grid: the time axis last
    error = store_R8P(self, self%group, data_name, x, [dims, 1_I8P])
  elseif (self%is_temporal .and. self%is_parallel) then ! field data: written once (by rank 0)
    if (self%rank /= 0_I4P) dims(size(dims)) = 0_I8P
    error = store_R8P(self, self%group, data_name, x, dims)
  elseif (self%is_temporal) then
    error = store_R8P(self, self%group, data_name, x, dims)
  else
    error = h5_dataset_write_R8P(self%group, data_name, x, dims, deflate=self%deflate_level(), &
                                chunk_bytes=self%chunk_bytes)
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

  if (len(h5_name_invalid(data_name)) > 0) then ! a VTKHDF name: no / nor .
    error = refuse('the array name "'//data_name//'" '//h5_name_invalid(data_name))
  else
    error = self%dataset_dims(data_name, n, n_components, dims)
  endif
  error = self%agree(error) ! with comm, a refusal on every rank before the collective writes
  if (error /= 0_I4P) return
  if (self%is_parallel) error = self%agree_name(data_name) ! the same array on every rank
  if (error /= 0_I4P) return
  is_new = .not.h5_link_exists(self%group, data_name)
  if (self%is_temporal .and. is_new .and. self%n_steps > 1_I8P .and. self%topology /= TOPOLOGY_AMR) then
    error = refuse('the array '//data_name//' is not in the first step: every step writes the same arrays')
    return
  endif
  if (self%group_name == 'RowData') then ! a column of a Table: the rows of the others
    if (self%table_rows >= 0_I8P .and. dims(size(dims)) /= self%table_rows) then
      error = refuse('the column '//data_name//' has '//trim(str(n=dims(size(dims)), no_sign=.true.))// &
                     ' rows, the other columns '//trim(str(n=self%table_rows, no_sign=.true.)))
      return
    endif
    self%table_rows = dims(size(dims))
    error = store_R4P(self, self%group, data_name, x, dims)
  elseif (self%topology == TOPOLOGY_HTG .and. self%is_temporal .and. self%group_name /= 'FieldData') then
    error = store_R4P(self, self%group, data_name, x, dims) ! appended
  elseif (self%topology == TOPOLOGY_AMR .and. self%group_name /= 'FieldData') then ! a box
    if (is_new .and. (self%level_counts(1, self%level + 1_I4P) > 1_I8P .or. &
                      (.not.self%is_recorded(data_name) .and. self%n_pieces > 1_I4P))) then
      error = refuse('the array '//data_name//' is not in the first box: every box of every level writes the same arrays')
      return
    endif
    error = store_R4P(self, self%group, data_name, x, dims)
  elseif (self%is_partitioned() .and. self%group_name /= 'FieldData') then
    if (is_new .and. self%local_pieces > 1_I4P) then
      error = refuse('the array '//data_name//' is not in the first piece: every piece writes the same arrays')
      return
    endif
    error = store_R4P(self, self%group, data_name, x, dims)
  elseif (self%is_parallel .and. self%group_name /= 'FieldData') then ! a grid: the block of this rank
    call self%region_block(self%group_name == 'CellData', n_components, dims, mem_dims, mem_start, &
                           file_start, count)
    if (self%is_temporal) then ! the time axis last: one slice more per step, each rank its block
      dims = [dims, 1_I8P] ; mem_dims = [mem_dims, 1_I8P] ; mem_start = [mem_start, 0_I8P]
      file_start = [file_start, 0_I8P] ; count = [count, 1_I8P]
      error = h5_dataset_slab_R4P(self%group, data_name, x, dims, mem_dims, mem_start, file_start, count, is_new, &
                          append=.true., chunk_bytes=self%chunk_bytes)
    else
      error = h5_dataset_slab_R4P(self%group, data_name, x, dims, mem_dims, mem_start, file_start, count, .true.)
    endif
  elseif (self%is_temporal .and. self%group_name /= 'FieldData') then ! a grid: the time axis last
    error = store_R4P(self, self%group, data_name, x, [dims, 1_I8P])
  elseif (self%is_temporal .and. self%is_parallel) then ! field data: written once (by rank 0)
    if (self%rank /= 0_I4P) dims(size(dims)) = 0_I8P
    error = store_R4P(self, self%group, data_name, x, dims)
  elseif (self%is_temporal) then
    error = store_R4P(self, self%group, data_name, x, dims)
  else
    error = h5_dataset_write_R4P(self%group, data_name, x, dims, deflate=self%deflate_level(), &
                                chunk_bytes=self%chunk_bytes)
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

  if (len(h5_name_invalid(data_name)) > 0) then ! a VTKHDF name: no / nor .
    error = refuse('the array name "'//data_name//'" '//h5_name_invalid(data_name))
  else
    error = self%dataset_dims(data_name, n, n_components, dims)
  endif
  error = self%agree(error) ! with comm, a refusal on every rank before the collective writes
  if (error /= 0_I4P) return
  if (self%is_parallel) error = self%agree_name(data_name) ! the same array on every rank
  if (error /= 0_I4P) return
  is_new = .not.h5_link_exists(self%group, data_name)
  if (self%is_temporal .and. is_new .and. self%n_steps > 1_I8P .and. self%topology /= TOPOLOGY_AMR) then
    error = refuse('the array '//data_name//' is not in the first step: every step writes the same arrays')
    return
  endif
  if (self%group_name == 'RowData') then ! a column of a Table: the rows of the others
    if (self%table_rows >= 0_I8P .and. dims(size(dims)) /= self%table_rows) then
      error = refuse('the column '//data_name//' has '//trim(str(n=dims(size(dims)), no_sign=.true.))// &
                     ' rows, the other columns '//trim(str(n=self%table_rows, no_sign=.true.)))
      return
    endif
    self%table_rows = dims(size(dims))
    error = store_I8P(self, self%group, data_name, x, dims, unsigned=unsigned)
  elseif (self%topology == TOPOLOGY_HTG .and. self%is_temporal .and. self%group_name /= 'FieldData') then
    error = store_I8P(self, self%group, data_name, x, dims, unsigned=unsigned) ! appended
  elseif (self%topology == TOPOLOGY_AMR .and. self%group_name /= 'FieldData') then ! a box
    if (is_new .and. (self%level_counts(1, self%level + 1_I4P) > 1_I8P .or. &
                      (.not.self%is_recorded(data_name) .and. self%n_pieces > 1_I4P))) then
      error = refuse('the array '//data_name//' is not in the first box: every box of every level writes the same arrays')
      return
    endif
    error = store_I8P(self, self%group, data_name, x, dims, unsigned=unsigned)
  elseif (self%is_partitioned() .and. self%group_name /= 'FieldData') then
    if (is_new .and. self%local_pieces > 1_I4P) then
      error = refuse('the array '//data_name//' is not in the first piece: every piece writes the same arrays')
      return
    endif
    error = store_I8P(self, self%group, data_name, x, dims, unsigned=unsigned)
  elseif (self%is_parallel .and. self%group_name /= 'FieldData') then ! a grid: the block of this rank
    call self%region_block(self%group_name == 'CellData', n_components, dims, mem_dims, mem_start, &
                           file_start, count)
    if (self%is_temporal) then ! the time axis last: one slice more per step, each rank its block
      dims = [dims, 1_I8P] ; mem_dims = [mem_dims, 1_I8P] ; mem_start = [mem_start, 0_I8P]
      file_start = [file_start, 0_I8P] ; count = [count, 1_I8P]
      error = h5_dataset_slab_I8P(self%group, data_name, x, dims, mem_dims, mem_start, file_start, count, is_new, &
                          unsigned=unsigned, append=.true., chunk_bytes=self%chunk_bytes)
    else
      error = h5_dataset_slab_I8P(self%group, data_name, x, dims, mem_dims, mem_start, file_start, count, .true., unsigned=unsigned)
    endif
  elseif (self%is_temporal .and. self%group_name /= 'FieldData') then ! a grid: the time axis last
    error = store_I8P(self, self%group, data_name, x, [dims, 1_I8P], unsigned=unsigned)
  elseif (self%is_temporal .and. self%is_parallel) then ! field data: written once (by rank 0)
    if (self%rank /= 0_I4P) dims(size(dims)) = 0_I8P
    error = store_I8P(self, self%group, data_name, x, dims, unsigned=unsigned)
  elseif (self%is_temporal) then
    error = store_I8P(self, self%group, data_name, x, dims, unsigned=unsigned)
  else
    error = h5_dataset_write_I8P(self%group, data_name, x, dims, deflate=self%deflate_level(), unsigned=unsigned, &
                                chunk_bytes=self%chunk_bytes)
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

  if (len(h5_name_invalid(data_name)) > 0) then ! a VTKHDF name: no / nor .
    error = refuse('the array name "'//data_name//'" '//h5_name_invalid(data_name))
  else
    error = self%dataset_dims(data_name, n, n_components, dims)
  endif
  error = self%agree(error) ! with comm, a refusal on every rank before the collective writes
  if (error /= 0_I4P) return
  if (self%is_parallel) error = self%agree_name(data_name) ! the same array on every rank
  if (error /= 0_I4P) return
  is_new = .not.h5_link_exists(self%group, data_name)
  if (self%is_temporal .and. is_new .and. self%n_steps > 1_I8P .and. self%topology /= TOPOLOGY_AMR) then
    error = refuse('the array '//data_name//' is not in the first step: every step writes the same arrays')
    return
  endif
  if (self%group_name == 'RowData') then ! a column of a Table: the rows of the others
    if (self%table_rows >= 0_I8P .and. dims(size(dims)) /= self%table_rows) then
      error = refuse('the column '//data_name//' has '//trim(str(n=dims(size(dims)), no_sign=.true.))// &
                     ' rows, the other columns '//trim(str(n=self%table_rows, no_sign=.true.)))
      return
    endif
    self%table_rows = dims(size(dims))
    error = store_I4P(self, self%group, data_name, x, dims, unsigned=unsigned)
  elseif (self%topology == TOPOLOGY_HTG .and. self%is_temporal .and. self%group_name /= 'FieldData') then
    error = store_I4P(self, self%group, data_name, x, dims, unsigned=unsigned) ! appended
  elseif (self%topology == TOPOLOGY_AMR .and. self%group_name /= 'FieldData') then ! a box
    if (is_new .and. (self%level_counts(1, self%level + 1_I4P) > 1_I8P .or. &
                      (.not.self%is_recorded(data_name) .and. self%n_pieces > 1_I4P))) then
      error = refuse('the array '//data_name//' is not in the first box: every box of every level writes the same arrays')
      return
    endif
    error = store_I4P(self, self%group, data_name, x, dims, unsigned=unsigned)
  elseif (self%is_partitioned() .and. self%group_name /= 'FieldData') then
    if (is_new .and. self%local_pieces > 1_I4P) then
      error = refuse('the array '//data_name//' is not in the first piece: every piece writes the same arrays')
      return
    endif
    error = store_I4P(self, self%group, data_name, x, dims, unsigned=unsigned)
  elseif (self%is_parallel .and. self%group_name /= 'FieldData') then ! a grid: the block of this rank
    call self%region_block(self%group_name == 'CellData', n_components, dims, mem_dims, mem_start, &
                           file_start, count)
    if (self%is_temporal) then ! the time axis last: one slice more per step, each rank its block
      dims = [dims, 1_I8P] ; mem_dims = [mem_dims, 1_I8P] ; mem_start = [mem_start, 0_I8P]
      file_start = [file_start, 0_I8P] ; count = [count, 1_I8P]
      error = h5_dataset_slab_I4P(self%group, data_name, x, dims, mem_dims, mem_start, file_start, count, is_new, &
                          unsigned=unsigned, append=.true., chunk_bytes=self%chunk_bytes)
    else
      error = h5_dataset_slab_I4P(self%group, data_name, x, dims, mem_dims, mem_start, file_start, count, .true., unsigned=unsigned)
    endif
  elseif (self%is_temporal .and. self%group_name /= 'FieldData') then ! a grid: the time axis last
    error = store_I4P(self, self%group, data_name, x, [dims, 1_I8P], unsigned=unsigned)
  elseif (self%is_temporal .and. self%is_parallel) then ! field data: written once (by rank 0)
    if (self%rank /= 0_I4P) dims(size(dims)) = 0_I8P
    error = store_I4P(self, self%group, data_name, x, dims, unsigned=unsigned)
  elseif (self%is_temporal) then
    error = store_I4P(self, self%group, data_name, x, dims, unsigned=unsigned)
  else
    error = h5_dataset_write_I4P(self%group, data_name, x, dims, deflate=self%deflate_level(), unsigned=unsigned, &
                                chunk_bytes=self%chunk_bytes)
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

  if (len(h5_name_invalid(data_name)) > 0) then ! a VTKHDF name: no / nor .
    error = refuse('the array name "'//data_name//'" '//h5_name_invalid(data_name))
  else
    error = self%dataset_dims(data_name, n, n_components, dims)
  endif
  error = self%agree(error) ! with comm, a refusal on every rank before the collective writes
  if (error /= 0_I4P) return
  if (self%is_parallel) error = self%agree_name(data_name) ! the same array on every rank
  if (error /= 0_I4P) return
  is_new = .not.h5_link_exists(self%group, data_name)
  if (self%is_temporal .and. is_new .and. self%n_steps > 1_I8P .and. self%topology /= TOPOLOGY_AMR) then
    error = refuse('the array '//data_name//' is not in the first step: every step writes the same arrays')
    return
  endif
  if (self%group_name == 'RowData') then ! a column of a Table: the rows of the others
    if (self%table_rows >= 0_I8P .and. dims(size(dims)) /= self%table_rows) then
      error = refuse('the column '//data_name//' has '//trim(str(n=dims(size(dims)), no_sign=.true.))// &
                     ' rows, the other columns '//trim(str(n=self%table_rows, no_sign=.true.)))
      return
    endif
    self%table_rows = dims(size(dims))
    error = store_I2P(self, self%group, data_name, x, dims, unsigned=unsigned)
  elseif (self%topology == TOPOLOGY_HTG .and. self%is_temporal .and. self%group_name /= 'FieldData') then
    error = store_I2P(self, self%group, data_name, x, dims, unsigned=unsigned) ! appended
  elseif (self%topology == TOPOLOGY_AMR .and. self%group_name /= 'FieldData') then ! a box
    if (is_new .and. (self%level_counts(1, self%level + 1_I4P) > 1_I8P .or. &
                      (.not.self%is_recorded(data_name) .and. self%n_pieces > 1_I4P))) then
      error = refuse('the array '//data_name//' is not in the first box: every box of every level writes the same arrays')
      return
    endif
    error = store_I2P(self, self%group, data_name, x, dims, unsigned=unsigned)
  elseif (self%is_partitioned() .and. self%group_name /= 'FieldData') then
    if (is_new .and. self%local_pieces > 1_I4P) then
      error = refuse('the array '//data_name//' is not in the first piece: every piece writes the same arrays')
      return
    endif
    error = store_I2P(self, self%group, data_name, x, dims, unsigned=unsigned)
  elseif (self%is_parallel .and. self%group_name /= 'FieldData') then ! a grid: the block of this rank
    call self%region_block(self%group_name == 'CellData', n_components, dims, mem_dims, mem_start, &
                           file_start, count)
    if (self%is_temporal) then ! the time axis last: one slice more per step, each rank its block
      dims = [dims, 1_I8P] ; mem_dims = [mem_dims, 1_I8P] ; mem_start = [mem_start, 0_I8P]
      file_start = [file_start, 0_I8P] ; count = [count, 1_I8P]
      error = h5_dataset_slab_I2P(self%group, data_name, x, dims, mem_dims, mem_start, file_start, count, is_new, &
                          unsigned=unsigned, append=.true., chunk_bytes=self%chunk_bytes)
    else
      error = h5_dataset_slab_I2P(self%group, data_name, x, dims, mem_dims, mem_start, file_start, count, .true., unsigned=unsigned)
    endif
  elseif (self%is_temporal .and. self%group_name /= 'FieldData') then ! a grid: the time axis last
    error = store_I2P(self, self%group, data_name, x, [dims, 1_I8P], unsigned=unsigned)
  elseif (self%is_temporal .and. self%is_parallel) then ! field data: written once (by rank 0)
    if (self%rank /= 0_I4P) dims(size(dims)) = 0_I8P
    error = store_I2P(self, self%group, data_name, x, dims, unsigned=unsigned)
  elseif (self%is_temporal) then
    error = store_I2P(self, self%group, data_name, x, dims, unsigned=unsigned)
  else
    error = h5_dataset_write_I2P(self%group, data_name, x, dims, deflate=self%deflate_level(), unsigned=unsigned, &
                                chunk_bytes=self%chunk_bytes)
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

  if (len(h5_name_invalid(data_name)) > 0) then ! a VTKHDF name: no / nor .
    error = refuse('the array name "'//data_name//'" '//h5_name_invalid(data_name))
  else
    error = self%dataset_dims(data_name, n, n_components, dims)
  endif
  error = self%agree(error) ! with comm, a refusal on every rank before the collective writes
  if (error /= 0_I4P) return
  if (self%is_parallel) error = self%agree_name(data_name) ! the same array on every rank
  if (error /= 0_I4P) return
  is_new = .not.h5_link_exists(self%group, data_name)
  if (self%is_temporal .and. is_new .and. self%n_steps > 1_I8P .and. self%topology /= TOPOLOGY_AMR) then
    error = refuse('the array '//data_name//' is not in the first step: every step writes the same arrays')
    return
  endif
  if (self%group_name == 'RowData') then ! a column of a Table: the rows of the others
    if (self%table_rows >= 0_I8P .and. dims(size(dims)) /= self%table_rows) then
      error = refuse('the column '//data_name//' has '//trim(str(n=dims(size(dims)), no_sign=.true.))// &
                     ' rows, the other columns '//trim(str(n=self%table_rows, no_sign=.true.)))
      return
    endif
    self%table_rows = dims(size(dims))
    error = store_I1P(self, self%group, data_name, x, dims, unsigned=unsigned)
  elseif (self%topology == TOPOLOGY_HTG .and. self%is_temporal .and. self%group_name /= 'FieldData') then
    error = store_I1P(self, self%group, data_name, x, dims, unsigned=unsigned) ! appended
  elseif (self%topology == TOPOLOGY_AMR .and. self%group_name /= 'FieldData') then ! a box
    if (is_new .and. (self%level_counts(1, self%level + 1_I4P) > 1_I8P .or. &
                      (.not.self%is_recorded(data_name) .and. self%n_pieces > 1_I4P))) then
      error = refuse('the array '//data_name//' is not in the first box: every box of every level writes the same arrays')
      return
    endif
    error = store_I1P(self, self%group, data_name, x, dims, unsigned=unsigned)
  elseif (self%is_partitioned() .and. self%group_name /= 'FieldData') then
    if (is_new .and. self%local_pieces > 1_I4P) then
      error = refuse('the array '//data_name//' is not in the first piece: every piece writes the same arrays')
      return
    endif
    error = store_I1P(self, self%group, data_name, x, dims, unsigned=unsigned)
  elseif (self%is_parallel .and. self%group_name /= 'FieldData') then ! a grid: the block of this rank
    call self%region_block(self%group_name == 'CellData', n_components, dims, mem_dims, mem_start, &
                           file_start, count)
    if (self%is_temporal) then ! the time axis last: one slice more per step, each rank its block
      dims = [dims, 1_I8P] ; mem_dims = [mem_dims, 1_I8P] ; mem_start = [mem_start, 0_I8P]
      file_start = [file_start, 0_I8P] ; count = [count, 1_I8P]
      error = h5_dataset_slab_I1P(self%group, data_name, x, dims, mem_dims, mem_start, file_start, count, is_new, &
                          unsigned=unsigned, append=.true., chunk_bytes=self%chunk_bytes)
    else
      error = h5_dataset_slab_I1P(self%group, data_name, x, dims, mem_dims, mem_start, file_start, count, .true., unsigned=unsigned)
    endif
  elseif (self%is_temporal .and. self%group_name /= 'FieldData') then ! a grid: the time axis last
    error = store_I1P(self, self%group, data_name, x, [dims, 1_I8P], unsigned=unsigned)
  elseif (self%is_temporal .and. self%is_parallel) then ! field data: written once (by rank 0)
    if (self%rank /= 0_I4P) dims(size(dims)) = 0_I8P
    error = store_I1P(self, self%group, data_name, x, dims, unsigned=unsigned)
  elseif (self%is_temporal) then
    error = store_I1P(self, self%group, data_name, x, dims, unsigned=unsigned)
  else
    error = h5_dataset_write_I1P(self%group, data_name, x, dims, deflate=self%deflate_level(), unsigned=unsigned, &
                                chunk_bytes=self%chunk_bytes)
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
      error = h5_dataset_write_R8P(loc_id, name, x, dims, appendable=.true., deflate=self%deflate_level(), &
                                     comm=self%comm, chunk_bytes=self%chunk_bytes)
    endif
  elseif (h5_link_exists(loc_id, name)) then
    error = h5_dataset_append_R8P(loc_id, name, x, dims)
  else
    error = h5_dataset_write_R8P(loc_id, name, x, dims, appendable=.true., deflate=self%deflate_level(), &
                                   chunk_bytes=self%chunk_bytes)
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
      error = h5_dataset_write_R4P(loc_id, name, x, dims, appendable=.true., deflate=self%deflate_level(), &
                                     comm=self%comm, chunk_bytes=self%chunk_bytes)
    endif
  elseif (h5_link_exists(loc_id, name)) then
    error = h5_dataset_append_R4P(loc_id, name, x, dims)
  else
    error = h5_dataset_write_R4P(loc_id, name, x, dims, appendable=.true., deflate=self%deflate_level(), &
                                   chunk_bytes=self%chunk_bytes)
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
      error = h5_dataset_write_I8P(loc_id, name, x, dims, appendable=.true., deflate=self%deflate_level(), unsigned=unsigned, &
                                     comm=self%comm, chunk_bytes=self%chunk_bytes)
    endif
  elseif (h5_link_exists(loc_id, name)) then
    error = h5_dataset_append_I8P(loc_id, name, x, dims, unsigned=unsigned)
  else
    error = h5_dataset_write_I8P(loc_id, name, x, dims, appendable=.true., deflate=self%deflate_level(), unsigned=unsigned, &
                                   chunk_bytes=self%chunk_bytes)
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
      error = h5_dataset_write_I4P(loc_id, name, x, dims, appendable=.true., deflate=self%deflate_level(), unsigned=unsigned, &
                                     comm=self%comm, chunk_bytes=self%chunk_bytes)
    endif
  elseif (h5_link_exists(loc_id, name)) then
    error = h5_dataset_append_I4P(loc_id, name, x, dims, unsigned=unsigned)
  else
    error = h5_dataset_write_I4P(loc_id, name, x, dims, appendable=.true., deflate=self%deflate_level(), unsigned=unsigned, &
                                   chunk_bytes=self%chunk_bytes)
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
      error = h5_dataset_write_I2P(loc_id, name, x, dims, appendable=.true., deflate=self%deflate_level(), unsigned=unsigned, &
                                     comm=self%comm, chunk_bytes=self%chunk_bytes)
    endif
  elseif (h5_link_exists(loc_id, name)) then
    error = h5_dataset_append_I2P(loc_id, name, x, dims, unsigned=unsigned)
  else
    error = h5_dataset_write_I2P(loc_id, name, x, dims, appendable=.true., deflate=self%deflate_level(), unsigned=unsigned, &
                                   chunk_bytes=self%chunk_bytes)
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
      error = h5_dataset_write_I1P(loc_id, name, x, dims, appendable=.true., deflate=self%deflate_level(), unsigned=unsigned, &
                                     comm=self%comm, chunk_bytes=self%chunk_bytes)
    endif
  elseif (h5_link_exists(loc_id, name)) then
    error = h5_dataset_append_I1P(loc_id, name, x, dims, unsigned=unsigned)
  else
    error = h5_dataset_write_I1P(loc_id, name, x, dims, appendable=.true., deflate=self%deflate_level(), unsigned=unsigned, &
                                   chunk_bytes=self%chunk_bytes)
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
  if (error == 0_I4P .and. self%is_parallel) then ! the ids of all the ranks
    self%totals(4) = self%totals(4) + h5_mpi_sum(self%comm, n_ids)
  elseif (error == 0_I4P) then
    self%totals(4) = self%totals(4) + n_ids
  endif
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
  if (error == 0_I4P .and. self%is_parallel) then ! the ids of all the ranks
    self%totals(4) = self%totals(4) + h5_mpi_sum(self%comm, n_ids)
  elseif (error == 0_I4P) then
    self%totals(4) = self%totals(4) + n_ids
  endif
  endfunction put_cells_I8P

  function put_coordinates_R8P(self, x, y, z) result(error)
  !< Write the coordinates (R8P) of a RectilinearGrid: XCoordinates, YCoordinates, ZCoordinates.
  class(vtkhdf_writer), intent(inout) :: self    !< Writer.
  real(R8P), intent(in)               :: x(:)    !< X coordinates.
  real(R8P), intent(in)               :: y(:)    !< Y coordinates.
  real(R8P), intent(in)               :: z(:)    !< Z coordinates.
  integer(I4P)                        :: error   !< Error status.
  integer(I8P)                        :: grid(3) !< Points along x, y, z.

  error = self%check_grid_geo(TOPOLOGY_RECT) ! or the root grid of a HyperTreeGrid
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
    error = h5_dataset_write_R8P(self%root, 'XCoordinates', x, [grid(1)], deflate=self%deflate_level(), &
                                 chunk_bytes=self%chunk_bytes)
    if (error == 0_I4P) error = h5_dataset_write_R8P(self%root, 'YCoordinates', y, [grid(2)], &
                                         deflate=self%deflate_level(), chunk_bytes=self%chunk_bytes)
    if (error == 0_I4P) error = h5_dataset_write_R8P(self%root, 'ZCoordinates', z, [grid(3)], &
                                         deflate=self%deflate_level(), chunk_bytes=self%chunk_bytes)
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
  integer(I8P)                     :: shift     !< Entries of the steps before (time series).
  integer                          :: r         !< Counter.

  grid = self%grid_points()
  shift = 0_I8P
  if (self%is_temporal) then ! the coordinates of the step after those of the steps before
    shift = self%geo_slices * grid(a)
    error = h5_dataset_slab_R8P(self%root, name, values, [grid(a)], [size(values, kind=I8P)], [0_I8P], &
                                [0_I8P], [0_I8P], .not.h5_link_exists(self%root, name), append=.true., &
                                chunk_bytes=self%chunk_bytes)
  else
    error = h5_dataset_slab_R8P(self%root, name, values, [grid(a)], [size(values, kind=I8P)], [0_I8P], &
                                [0_I8P], [0_I8P], .true.)
  endif
  call self%axis_runs(a, runs)
  ! as many writes on every rank (the largest number of runs; empty ones beyond its own): the dataset is opened
  ! the same number of times by every rank (H5Dopen is collective with collective metadata operations)
  do r=1, int(h5_mpi_max(self%comm, size(runs, dim=2, kind=I8P)))
    if (r <= size(runs, dim=2)) then
      if (error == 0_I4P) error = h5_dataset_slab_R8P(self%root, name, values, [grid(a)], [size(values, kind=I8P)], &
                                                    runs(1:1, r), runs(2:2, r) + shift, runs(3:3, r), .false.)
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
  if (self%is_parallel .and. self%is_temporal) then ! a slice more: Points [t, k, j, i, 3] in C order
    error = h5_dataset_slab_R8P(self%root, 'Points', xyz, [3_I8P, self%grid_points(), 1_I8P], &
                                [mem_dims, 1_I8P], [0_I8P, 0_I8P, 0_I8P, 0_I8P, 0_I8P], [file_start, 0_I8P], &
                                [count, 1_I8P], .not.h5_link_exists(self%root, 'Points'), append=.true., &
                                chunk_bytes=self%chunk_bytes)
  elseif (self%is_parallel) then ! every rank creates Points, writes its block
    error = h5_dataset_slab_R8P(self%root, 'Points', xyz, [3_I8P, self%grid_points()], mem_dims, &
                                [0_I8P, 0_I8P, 0_I8P, 0_I8P], file_start, count, .true.)
  elseif (self%is_temporal) then ! the time axis last: Points [t, k, j, i, 3] in C order
    error = store_R8P(self, self%root, 'Points', xyz, [3_I8P, grid, 1_I8P])
  else
    error = h5_dataset_write_R8P(self%root, 'Points', xyz, [3_I8P, grid], deflate=self%deflate_level(), &
                                 chunk_bytes=self%chunk_bytes)
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

  error = self%check_grid_geo(TOPOLOGY_RECT) ! or the root grid of a HyperTreeGrid
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
    error = h5_dataset_write_R4P(self%root, 'XCoordinates', x, [grid(1)], deflate=self%deflate_level(), &
                                 chunk_bytes=self%chunk_bytes)
    if (error == 0_I4P) error = h5_dataset_write_R4P(self%root, 'YCoordinates', y, [grid(2)], &
                                         deflate=self%deflate_level(), chunk_bytes=self%chunk_bytes)
    if (error == 0_I4P) error = h5_dataset_write_R4P(self%root, 'ZCoordinates', z, [grid(3)], &
                                         deflate=self%deflate_level(), chunk_bytes=self%chunk_bytes)
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
  integer(I8P)                     :: shift     !< Entries of the steps before (time series).
  integer                          :: r         !< Counter.

  grid = self%grid_points()
  shift = 0_I8P
  if (self%is_temporal) then ! the coordinates of the step after those of the steps before
    shift = self%geo_slices * grid(a)
    error = h5_dataset_slab_R4P(self%root, name, values, [grid(a)], [size(values, kind=I8P)], [0_I8P], &
                                [0_I8P], [0_I8P], .not.h5_link_exists(self%root, name), append=.true., &
                                chunk_bytes=self%chunk_bytes)
  else
    error = h5_dataset_slab_R4P(self%root, name, values, [grid(a)], [size(values, kind=I8P)], [0_I8P], &
                                [0_I8P], [0_I8P], .true.)
  endif
  call self%axis_runs(a, runs)
  ! as many writes on every rank (the largest number of runs; empty ones beyond its own): the dataset is opened
  ! the same number of times by every rank (H5Dopen is collective with collective metadata operations)
  do r=1, int(h5_mpi_max(self%comm, size(runs, dim=2, kind=I8P)))
    if (r <= size(runs, dim=2)) then
      if (error == 0_I4P) error = h5_dataset_slab_R4P(self%root, name, values, [grid(a)], [size(values, kind=I8P)], &
                                                    runs(1:1, r), runs(2:2, r) + shift, runs(3:3, r), .false.)
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
  if (self%is_parallel .and. self%is_temporal) then ! a slice more: Points [t, k, j, i, 3] in C order
    error = h5_dataset_slab_R4P(self%root, 'Points', xyz, [3_I8P, self%grid_points(), 1_I8P], &
                                [mem_dims, 1_I8P], [0_I8P, 0_I8P, 0_I8P, 0_I8P, 0_I8P], [file_start, 0_I8P], &
                                [count, 1_I8P], .not.h5_link_exists(self%root, 'Points'), append=.true., &
                                chunk_bytes=self%chunk_bytes)
  elseif (self%is_parallel) then ! every rank creates Points, writes its block
    error = h5_dataset_slab_R4P(self%root, 'Points', xyz, [3_I8P, self%grid_points()], mem_dims, &
                                [0_I8P, 0_I8P, 0_I8P, 0_I8P], file_start, count, .true.)
  elseif (self%is_temporal) then ! the time axis last: Points [t, k, j, i, 3] in C order
    error = store_R4P(self, self%root, 'Points', xyz, [3_I8P, grid, 1_I8P])
  else
    error = h5_dataset_write_R4P(self%root, 'Points', xyz, [3_I8P, grid], deflate=self%deflate_level(), &
                                 chunk_bytes=self%chunk_bytes)
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
    error = self%add_face_totals(n_face_ids, n_faces)
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
    error = self%add_face_totals(n_face_ids, n_faces)
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
    if (self%is_parallel) then ! the cells and ids of all the ranks
      self%block_totals(b) = self%block_totals(b) + h5_mpi_sum(self%comm, nc)
      self%block_totals(4+b) = self%block_totals(4+b) + h5_mpi_sum(self%comm, n_ids)
    else
      self%block_totals(b) = self%block_totals(b) + nc
      self%block_totals(4+b) = self%block_totals(4+b) + n_ids
    endif
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
    if (self%is_parallel) then ! the cells and ids of all the ranks
      self%block_totals(b) = self%block_totals(b) + h5_mpi_sum(self%comm, nc)
      self%block_totals(4+b) = self%block_totals(4+b) + h5_mpi_sum(self%comm, n_ids)
    else
      self%block_totals(b) = self%block_totals(b) + nc
      self%block_totals(4+b) = self%block_totals(4+b) + n_ids
    endif
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
  if (self%topology /= topology .and. .not.(topology == TOPOLOGY_RECT .and. self%topology == TOPOLOGY_HTG)) then
    if (topology == TOPOLOGY_RECT) then
      error = refuse('write_geo(x, y, z) is for a RectilinearGrid or a HyperTreeGrid (its root grid)')
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
  if (self%group_name == 'FieldData' .or. self%group_name == 'RowData') then
    if (mod(n, int(n_components, I8P)) /= 0_I8P) then
      error = refuse('the array '//data_name//' has '//trim(str(n=n, no_sign=.true.))// &
                     ' values, not a multiple of its '//trim(str(n=n_components, no_sign=.true.))//' components')
      return
    endif
    grid = [n / n_components, 1_I8P, 1_I8P]
    is_axis = [.true., .false., .false.]
  elseif (self%topology == TOPOLOGY_HTG) then ! the cells of the trees written, breadth first in each tree
    grid = [self%tree_cells, 1_I8P, 1_I8P]
    is_axis = [.true., .false., .false.]
    if (n /= grid(1) * n_components) then
      error = refuse('the array '//data_name//' has '//trim(str(n=n, no_sign=.true.))//' values, the cells of the '// &
                     'trees need '//trim(str(n=grid(1) * n_components, no_sign=.true.))//' ('// &
                     trim(str(n=n_components, no_sign=.true.))//' components)')
      return
    endif
  elseif (self%topology == TOPOLOGY_AMR) then ! a box of an OverlappingAMR: its points or cells, x fastest
    grid = [self%np_piece, 1_I8P, 1_I8P]
    if (self%group_name == 'CellData') grid(1) = self%nc_piece
    is_axis = [.true., .false., .false.]
    if (n /= grid(1) * n_components) then
      error = refuse('the array '//data_name//' has '//trim(str(n=n, no_sign=.true.))//' values, the box needs '// &
                     trim(str(n=grid(1) * n_components, no_sign=.true.))//' ('// &
                     trim(str(n=n_components, no_sign=.true.))//' components)')
      return
    endif
  elseif (self%is_partitioned()) then
    grid = [self%np_piece, 1_I8P, 1_I8P]
    if (self%group_name == 'CellData') grid(1) = self%nc_piece
    if (self%is_reusing() .and. self%is_parallel) then ! with comm, the points or cells of the rank's part of the mesh
      grid(1) = self%np_piece
      if (self%group_name == 'CellData') grid(1) = self%nc_piece
    elseif (self%is_reusing()) then ! a step without pieces: the points or cells of all the parts of the mesh reused
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
  ! from ranks, the field data of a file that is not a time series are written by every rank alone: not deflated (a
  ! deflated dataset of a parallel file needs collective writes)
  if (self%is_parallel .and. .not.self%is_temporal .and. self%group_name == 'FieldData') level = 0_I4P
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
  !< Open the data group `name` of /VTKHDF (of the open level of an OverlappingAMR), creating it (creation order tracked:
  !< the arrays keep the order written).
  class(vtkhdf_writer), intent(inout) :: self  !< Writer.
  character(*),         intent(in)    :: name  !< Group name.
  integer(I4P)                        :: error !< Error status.
  integer(I4P)                        :: r     !< Counter.
  integer(hid_kind)                   :: base  !< Parent: /VTKHDF, or the open level of an OverlappingAMR.

  base = self%root
  if (self%topology == TOPOLOGY_AMR .and. name /= 'FieldData') base = self%level_group
  if (h5_link_exists(base, name)) then
    error = h5_group_open(base, name, self%group)
  else
    error = h5_group_create(base, name, self%group, track_order=.true.)
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

  function add_face_totals(self, n_face_ids, n_faces) result(error)
  !< Add the faces of the polyhedra of a piece to the running totals: point ids of the faces, faces, faces of the
  !< polyhedra; with comm, those of all the ranks (collective: every rank calls it, its own counts or zeros).
  class(vtkhdf_writer), intent(inout) :: self       !< Writer.
  integer(I8P),         intent(in)    :: n_face_ids !< Point ids of the faces of the piece.
  integer(I8P),         intent(in)    :: n_faces    !< Faces of the piece.
  integer(I4P)                        :: error      !< Error status.

  if (self%is_parallel) then
    self%totals(5:7) = self%totals(5:7) + [h5_mpi_sum(self%comm, n_face_ids), h5_mpi_sum(self%comm, n_faces), &
                                           h5_mpi_sum(self%comm, n_faces)]
  else
    self%totals(5:7) = self%totals(5:7) + [n_face_ids, n_faces, n_faces]
  endif
  error = 0_I4P
  endfunction add_face_totals

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
  ! with comm, the sums of the ranks: collective, after the stores as in put_faces (the same calls in the same order)
  if (error == 0_I4P) error = self%add_face_totals(0_I8P, 0_I8P)
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

  if (.not.(self%is_partitioned() .or. self%is_temporal .or. self%group_name == 'RowData' .or. &
            self%topology == TOPOLOGY_AMR)) return
  if (self%topology == TOPOLOGY_AMR .and. self%is_recorded(data_name)) return ! the name of another level
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
  elseif (self%group_name == 'RowData') then
    names = self%row_arrays//data_name//NAMES_SEPARATOR
    self%row_arrays = names
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

  function put_strings(self, data_name, x) result(error)
  !< Write field data strings as vtkHDFWriter writes a vtkStringArray: a dataset of variable-length strings (trailing
  !< blanks dropped), appended to step after step in a time series (`FieldDataOffsets`/`FieldDataSizes` as numeric field
  !< data). Refused with a communicator (parallel HDF5 writes no variable-length type).
  class(vtkhdf_writer), intent(inout) :: self      !< Writer.
  character(*),         intent(in)    :: data_name !< Data name.
  character(*),         intent(in)    :: x(:)      !< Strings.
  integer(I4P)                        :: error     !< Error status.
  logical                             :: is_new    !< The dataset is created (not appended to).

  error = 1_I4P
  if (self%group < 0_hid_kind .or. self%group_name /= 'FieldData') return
  if (len(h5_name_invalid(data_name)) > 0) then ! a VTKHDF name: no / nor .
    error = refuse('the array name "'//data_name//'" '//h5_name_invalid(data_name))
    return
  endif
  if (self%is_parallel) then
    error = refuse('string field data are not written with comm: parallel HDF5 writes no variable-length strings')
    return
  endif
  is_new = .not.h5_link_exists(self%group, data_name)
  if (self%is_temporal .and. is_new .and. self%n_steps > 1_I8P) then
    error = refuse('the array '//data_name//' is not in the first step: every step writes the same arrays')
    return
  endif
  if (self%is_temporal .and. .not.is_new) then
    error = h5_dataset_append_strings(self%group, data_name, x)
  else
    error = h5_dataset_write_strings(self%group, data_name, x, appendable=self%is_temporal, chunk_bytes=self%chunk_bytes)
  endif
  if (error /= 0_I4P) then
    error = refuse('cannot write the dataset '//data_name)
    return
  endif
  if (is_new) call self%record_array(data_name)
  endfunction put_strings

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

  is_reusing_ = self%is_temporal .and. self%n_steps > 1_I8P .and. self%step_pieces == 0_I8P .and. &
                self%topology /= TOPOLOGY_AMR .and. self%topology /= TOPOLOGY_TABLE .and. self%topology /= TOPOLOGY_HTG
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

  error = 0_I4P
  if (self%n_pieces > 0_I4P .and. .not.self%is_temporal) error = refuse('write_piece: one region per rank of comm')
  error = self%agree(error) ! a second region on one rank: refused on every rank, before the gather
  if (error /= 0_I4P) return
  error = 1_I4P
  if (h5_mpi_allgather(self%comm, int(extent, I8P), self%regions) /= 0_I4P) then
    error = refuse('write_piece: the regions of the ranks cannot be gathered')
    return
  endif
  if (self%is_reopened .and. (self%topology == TOPOLOGY_RECT .or. self%topology == TOPOLOGY_STRG)) then
    ! a RectilinearGrid or StructuredGrid reopened: the start of its extent is not in the file (its size is): the start
    ! of each axis is the lowest lower bound of the regions (the same on every rank), as the serial check allows
    do r=1, 3
      lo(r) = huge(1_I8P)
      do q=1, size(self%regions, dim=2)
        if (any(self%regions(2::2, q) < self%regions(1::2, q))) cycle ! an empty region
        lo(r) = min(lo(r), self%regions(2 * r - 1, q))
      enddo
      if (lo(r) < huge(1_I8P)) self%whole_extent(2 * r - 1:2 * r) = self%whole_extent(2 * r - 1:2 * r) + &
                                                                    int(lo(r), I4P) - self%whole_extent(2 * r - 1)
    enddo
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
  self%has_region = .true.
  self%rank = h5_mpi_rank(self%comm)
  error = 0_I4P
  endfunction start_region

  function agree_name(self, data_name) result(error)
  !< With comm, refuse on every rank the array `data_name` when the ranks write different arrays at this call (each rank
  !< compares a hash of its name with those of the others: collective). A rank leaving a call out is not detected.
  class(vtkhdf_writer), intent(in) :: self      !< Writer.
  character(*),         intent(in) :: data_name !< Name of the array of this rank.
  integer(I4P)                     :: error     !< Error status.
  integer(I8P)                     :: hash      !< Hash of the name.
  integer                          :: c         !< Counter.

  hash = int(len(data_name), I8P)
  do c=1, len(data_name)
    hash = mod(hash * 131_I8P + int(ichar(data_name(c:c)), I8P), 2147483647_I8P) ! 2**31 - 1: no overflow
  enddo
  error = 0_I4P
  if (h5_mpi_max(self%comm, hash) /= -h5_mpi_max(self%comm, -hash)) &
    error = refuse('the ranks of comm write different arrays (this rank '//data_name//'): every rank writes the '// &
                   'same arrays in the same order')
  endfunction agree_name

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

  function end_table(self) result(error)
  !< End the columns of a Table (the static table, or a step of a time series): every column written once, with the
  !< rows of the others; `NumberOfRows` (one entry per step), and the `Steps` entries of the step (time, `RowDataOffsets`
  !< of each column): the file is valid after it.
  class(vtkhdf_writer), intent(inout) :: self      !< Writer.
  integer(I4P)                        :: error     !< Error status.
  integer(I8P), allocatable           :: tuples(:) !< Rows of each column.

  error = array_tuples(self%root, 'RowData', self%row_arrays, tuples)
  if (error /= 0_I4P) return
  if (self%table_rows < 0_I8P) self%table_rows = 0_I8P
  if (any(tuples /= self%row_total + self%table_rows)) then
    error = refuse('every column of a Table is written once in every step, with the same rows')
    return
  endif
  if (self%is_temporal) then
    error = self%write_steps()
  else
    error = store_I8P(self, self%root, 'NumberOfRows', [self%table_rows], [1_I8P])
    if (error /= 0_I4P) error = refuse('cannot write the number of rows')
  endif
  endfunction end_table

  function open_level(self, level, spacing) result(error)
  !< Open the level `level` of an OverlappingAMR, the group `Level<level>` (created with its `Spacing` for a new level).
  class(vtkhdf_writer), intent(inout) :: self       !< Writer.
  integer(I4P),         intent(in)    :: level      !< Level, from 0.
  real(R8P),            intent(in)    :: spacing(3) !< Spacing of the level.
  integer(I4P)                        :: error      !< Error status.
  character(len=:), allocatable       :: name       !< Group name.
  real(R8P),    allocatable           :: spacings(:,:) !< Spacings, one more level.
  integer(I8P), allocatable           :: counts(:,:)   !< Counts, one more level.
  integer(I4P)                        :: e          !< Error of a closing.

  error = 0_I4P
  if (self%level_group >= 0_hid_kind) error = h5_group_close(self%level_group)
  self%level_group = -1_hid_kind
  self%level = -1_I4P
  name = 'Level'//trim(str(n=level, no_sign=.true.))
  if (level == self%n_levels) then ! a new level
    if (error == 0_I4P) error = h5_group_create(self%root, name, self%level_group, track_order=.true.)
    if (error == 0_I4P) error = h5_attribute_write(self%level_group, 'Spacing', spacing)
    if (error == 0_I4P) then
      allocate(spacings(3, level + 1), counts(6, level + 1))
      counts = 0_I8P
      if (level > 0_I4P) then
        spacings(:, 1:level) = self%level_spacing
        counts(:, 1:level) = self%level_counts
      endif
      spacings(:, level + 1) = spacing
      call move_alloc(spacings, self%level_spacing)
      call move_alloc(counts, self%level_counts)
      self%n_levels = level + 1_I4P
    endif
  elseif (error == 0_I4P) then
    error = h5_group_open(self%root, name, self%level_group)
  endif
  if (error /= 0_I4P) then
    if (self%level_group >= 0_hid_kind) e = h5_group_close(self%level_group)
    self%level_group = -1_hid_kind
    error = refuse('cannot open the group '//name)
    return
  endif
  self%level = level
  endfunction open_level

  function start_box(self, extent) result(error)
  !< Start a box of the open level of an OverlappingAMR: its point extent `nx1, nx2, ...` (at least one cell along every
  !< axis: VTK reads every box in 3D), inside the bounds of the boxes of level 0 (of the step) for a finer level; its
  !< row of `AMRBox`, the cells `nx1, nx2-1, ny1, ny2-1, nz1, nz2-1`.
  class(vtkhdf_writer), intent(inout) :: self      !< Writer.
  integer(I4P),         intent(in)    :: extent(6) !< Point extent of the box.
  integer(I4P)                        :: error     !< Error status.
  real(R8P)                           :: lo(3)     !< Lower corner of the box.
  real(R8P)                           :: hi(3)     !< Upper corner of the box.
  real(R8P)                           :: h(3)      !< Spacing of the level.
  real(R8P)                           :: tol       !< Tolerance of the comparison of the corners.
  integer(I4P)                        :: l         !< Level, from 1.

  error = 1_I4P
  if (self%level < 0_I4P) then
    error = refuse('write_piece: a box of an OverlappingAMR is written in a level (write_level before)')
    return
  elseif (any(extent(2::2) <= extent(1::2))) then
    error = refuse('write_piece: a box of an OverlappingAMR has at least one cell along every axis (nx2 > nx1, '// &
                   'ny2 > ny1, nz2 > nz1: VTK reads every box in 3D)')
    return
  endif
  l = self%level + 1_I4P
  h = self%level_spacing(:, l)
  lo = self%origin + real(extent(1::2), R8P) * h
  hi = self%origin + real(extent(2::2), R8P) * h
  if (self%level == 0_I4P) then
    if (.not.self%has_domain) then
      self%domain(1:3) = lo ; self%domain(4:6) = hi
    else
      self%domain(1:3) = min(self%domain(1:3), lo) ; self%domain(4:6) = max(self%domain(4:6), hi)
    endif
    self%has_domain = .true.
  else
    if (.not.self%has_domain) then
      error = refuse('write_piece: the boxes of level 0 come first (they bound the boxes of the finer levels)')
      return
    endif
    tol = 1.e-9_R8P * maxval(abs([self%domain, h]))
    if (any(lo < self%domain(1:3) - tol) .or. any(hi > self%domain(4:6) + tol)) then
      error = refuse('write_piece: the box is outside the boxes of level 0 (their bounds)')
      return
    endif
  endif
  error = store_I4P(self, self%level_group, 'AMRBox', [extent(1), extent(2) - 1_I4P, extent(3), extent(4) - 1_I4P, &
                                                      extent(5), extent(6) - 1_I4P], [6_I8P, 1_I8P])
  if (error /= 0_I4P) then
    error = refuse('cannot write the AMRBox of the box')
    return
  endif
  self%np_piece = product(int(extent(2::2) - extent(1::2), I8P) + 1_I8P)
  self%nc_piece = product(int(extent(2::2) - extent(1::2), I8P))
  self%level_counts(1:3, l) = self%level_counts(1:3, l) + [1_I8P, self%np_piece, self%nc_piece]
  self%n_pieces = self%n_pieces + 1_I4P
  self%step_pieces = self%step_pieces + 1_I8P
  self%is_piece_open = .true.
  endfunction start_box

  function end_box(self) result(error)
  !< End a box of an OverlappingAMR: every point and cell array written once in it (every box of every level writes
  !< the same arrays).
  class(vtkhdf_writer), intent(inout) :: self  !< Writer.
  integer(I4P)                        :: error !< Error status.

  error = level_arrays('PointData', self%point_arrays, self%level_counts(2, self%level + 1_I4P))
  if (error == 0_I4P) error = level_arrays('CellData', self%cell_arrays, self%level_counts(3, self%level + 1_I4P))
  contains
    function level_arrays(group, names, tuples) result(error)
    !< Check that every array of the list holds the tuples of the boxes of the level.
    character(*), intent(in)  :: group  !< PointData or CellData.
    character(*), intent(in)  :: names  !< Names, each followed by NAMES_SEPARATOR.
    integer(I8P), intent(in)  :: tuples !< Tuples of the boxes of the level.
    integer(I4P)              :: error  !< Error status.
    integer(I8P), allocatable :: dims(:) !< Shape of a dataset.
    integer(I8P)              :: first, last !< Bounds of a name.

    error = 0_I4P
    first = 1_I8P
    do while (first <= len(names, kind=I8P) .and. error == 0_I4P)
      last = first - 1_I8P + index(names(first:), NAMES_SEPARATOR, kind=I8P)
      error = 1_I4P
      if (h5_link_exists(self%level_group, group//'/'//names(first:last-1))) &
        error = h5_dataset_dims(self%level_group, group//'/'//names(first:last-1), dims)
      if (error == 0_I4P) then
        if (dims(size(dims)) /= tuples) error = 1_I4P
      endif
      if (error /= 0_I4P) error = refuse('the array '//names(first:last-1)//' of '//group//' is not written once in '// &
                                         'every box of every level')
      first = last + 1_I8P
    enddo
    endfunction level_arrays
  endfunction end_box

  function end_trees(self) result(error)
  !< End the piece of a HyperTreeGrid: its trees written (bits packed, MSB first: `Descriptors`, `Mask`), their counts;
  !< in a time series appended to those of the steps before (each step from a new byte), the mask always written.
  class(vtkhdf_writer), intent(inout) :: self  !< Writer.
  integer(I4P)                        :: error !< Error status.

  if (self%is_temporal) then
    if (self%n_tree_ids == 0_I8P) then
      error = refuse('the piece of a HyperTreeGrid needs its trees (write_tree)')
      return
    endif
    error = store_I1P(self, self%root, 'Descriptors', pack_bits(self%tree_bits(1:self%n_tree_bits)), &
                      [(self%n_tree_bits + 7_I8P) / 8_I8P], unsigned=.true.)
    if (error == 0_I4P) error = store_I8P(self, self%root, 'DescriptorsSize', [self%n_tree_bits], [1_I8P])
    if (error == 0_I4P) error = store_I8P(self, self%root, 'TreeIds', self%tree_ids(1:self%n_tree_ids), [self%n_tree_ids])
    if (error == 0_I4P) error = store_I8P(self, self%root, 'DepthPerTree', self%tree_depths(1:self%n_tree_ids), &
                                          [self%n_tree_ids])
    if (error == 0_I4P) error = store_I8P(self, self%root, 'NumberOfCellsPerTreeDepth', &
                                          self%tree_counts(1:self%n_tree_counts), [self%n_tree_counts])
    if (error == 0_I4P) error = store_I8P(self, self%root, 'NumberOfTrees', [self%n_tree_ids], [1_I8P])
    if (error == 0_I4P) error = store_I8P(self, self%root, 'NumberOfDepths', [self%n_tree_counts], [1_I8P])
    if (error == 0_I4P) error = store_I8P(self, self%root, 'NumberOfCells', [self%tree_cells], [1_I8P])
    if (error == 0_I4P) error = store_I1P(self, self%root, 'Mask', pack_bits(self%tree_mask(1:self%tree_cells)), &
                                          [(self%tree_cells + 7_I8P) / 8_I8P], unsigned=.true.)
    if (error /= 0_I4P) error = refuse('cannot write the trees of the HyperTreeGrid')
    return
  endif

  if (self%n_tree_ids == 0_I8P) then
    error = refuse('the piece of a HyperTreeGrid needs its trees (write_tree)')
    return
  endif
  error = h5_dataset_write_I1P(self%root, 'Descriptors', pack_bits(self%tree_bits(1:self%n_tree_bits)), &
                               [(self%n_tree_bits + 7_I8P) / 8_I8P], unsigned=.true.)
  if (error == 0_I4P) error = h5_dataset_write_I8P(self%root, 'DescriptorsSize', [self%n_tree_bits], [1_I8P])
  if (error == 0_I4P) error = h5_dataset_write_I8P(self%root, 'TreeIds', self%tree_ids(1:self%n_tree_ids), [self%n_tree_ids])
  if (error == 0_I4P) error = h5_dataset_write_I8P(self%root, 'DepthPerTree', self%tree_depths(1:self%n_tree_ids), &
                                                   [self%n_tree_ids])
  if (error == 0_I4P) error = h5_dataset_write_I8P(self%root, 'NumberOfCellsPerTreeDepth', &
                                                   self%tree_counts(1:self%n_tree_counts), [self%n_tree_counts])
  if (error == 0_I4P) error = h5_dataset_write_I8P(self%root, 'NumberOfTrees', [self%n_tree_ids], [1_I8P])
  if (error == 0_I4P) error = h5_dataset_write_I8P(self%root, 'NumberOfDepths', [self%n_tree_counts], [1_I8P])
  if (error == 0_I4P) error = h5_dataset_write_I8P(self%root, 'NumberOfCells', [self%tree_cells], [1_I8P])
  if (error == 0_I4P .and. self%has_mask) &
    error = h5_dataset_write_I1P(self%root, 'Mask', pack_bits(self%tree_mask(1:self%tree_cells)), &
                                 [(self%tree_cells + 7_I8P) / 8_I8P], unsigned=.true.)
  if (error /= 0_I4P) error = refuse('cannot write the trees of the HyperTreeGrid')
  endfunction end_trees

  function is_table_complete(self) result(complete)
  !< Return true if the Table has its `NumberOfRows` entry of every step (one for a static Table): the close of its
  !< columns was not refused.
  class(vtkhdf_writer), intent(in) :: self     !< Writer.
  logical                          :: complete !< Check result.
  integer(I8P), allocatable        :: dims(:)  !< Shape of NumberOfRows.

  complete = .false.
  if (.not.h5_link_exists(self%root, 'NumberOfRows')) return
  if (h5_dataset_dims(self%root, 'NumberOfRows', dims) /= 0_I4P) return
  complete = dims(size(dims)) == max(self%n_steps, 1_I8P)
  endfunction is_table_complete

  pure function is_recorded(self, data_name) result(is_recorded_)
  !< Return true if `data_name` is recorded in the list of the open data group (point or cell data).
  class(vtkhdf_writer), intent(in) :: self         !< Writer.
  character(*),         intent(in) :: data_name    !< Data name.
  logical                          :: is_recorded_ !< Check result.

  is_recorded_ = .false.
  if (self%group_name == 'PointData') then
    is_recorded_ = index(NAMES_SEPARATOR//self%point_arrays, NAMES_SEPARATOR//data_name//NAMES_SEPARATOR) > 0
  elseif (self%group_name == 'CellData') then
    is_recorded_ = index(NAMES_SEPARATOR//self%cell_arrays, NAMES_SEPARATOR//data_name//NAMES_SEPARATOR) > 0
  endif
  endfunction is_recorded

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
  !< one partition of the UnstructuredGrid or PolyData, appended after the pieces of the ranks before (one piece per rank,
  !< in each step of a time series; polyhedra on some ranks only: the others write zero entries; compressor zlib with an
  !< HDF5 of parallel filtered writes, the writes collective), or its region of a grid (in each step of a time series too:
  !< one slice more of every dataset, each rank its block). Refused: compressor zlib for a grid.
  class(vtkhdf_writer), intent(inout) :: self !< Writer.
  integer(I4P),         intent(in)    :: comm !< MPI communicator (Fortran handle).

  self%is_parallel = .true.
  self%comm = comm
  self%rank = h5_mpi_rank(comm)
  endsubroutine set_comm

  function reopen(self, filename) result(error)
  !< Reopen a VTKHDF time series to append steps (`vtk_file%initialize(format='vtkhdf', filename=..., action='append')`),
  !< `reopen_file`; when it is refused, the file is closed as it is and the writer reset: a `finalize` after a refused
  !< reopen writes nothing (it returns an error).
  class(vtkhdf_writer), intent(inout) :: self     !< Writer.
  character(*),         intent(in)    :: filename !< File name.
  integer(I4P)                        :: error    !< Error status.
  integer(I4P)                        :: e        !< Error of a closing.

  error = self%reopen_file(filename)
  if (error == 0_I4P) return
  if (self%group >= 0_hid_kind) e = h5_group_close(self%group)
  if (self%root >= 0_hid_kind) e = h5_group_close(self%root)
  if (self%file_id >= 0_hid_kind) e = h5_file_close(self%file_id)
  self%file_id = -1_hid_kind ; self%root = -1_hid_kind ; self%group = -1_hid_kind ; self%level_group = -1_hid_kind
  call self%reset
  self%is_parallel = .false.
  endfunction reopen

  function reopen_file(self, filename) result(error)
  !< Read the state of a VTKHDF time series reopened to append steps:
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
  integer(I8P), allocatable           :: point_rows(:), cell_rows(:) !< Points and cells of each step (slices: 1).

  self%file_id = -1_hid_kind ; self%root = -1_hid_kind ; self%group = -1_hid_kind
  call self%reset
  if (.not.is_hdf5_enabled) then
    error = refuse('the library is built without HDF5 (VTKFORTRAN_USE_HDF5)')
    return
  endif
  if (self%is_parallel) then ! every rank of comm (a parallel restart)
    error = h5_file_open(filename, self%file_id, readwrite=.true., comm=self%comm)
  else
    error = h5_file_open(filename, self%file_id, readwrite=.true.)
  endif
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
    error = refuse('action append: '//filename//' is not an ImageData, RectilinearGrid, StructuredGrid, '// &
                   'UnstructuredGrid or PolyData: it cannot be continued')
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
    self%local_pieces = int(n_parts, I4P)
    self%step_pieces = counts(k)
    if (self%is_parallel) then ! the part of this rank in the last mesh, for a step reusing it (its parts in rank order)
      self%np_piece = -1_I8P ; self%nc_piece = -1_I8P ! another number of ranks than the parts: no reuse
      if (counts(k) == h5_mpi_sum(self%comm, 1_I8P)) then
        error = h5_dataset_read(self%root, 'NumberOfPoints', more)
        if (error == 0_I4P) self%np_piece = more(ivalues(k) + self%rank + 1)
        if (self%topology == TOPOLOGY_UNSTRUCTURED) then
          if (error == 0_I4P) error = h5_dataset_read(self%root, 'NumberOfCells', more)
          if (error == 0_I4P) self%nc_piece = more(ivalues(k) + self%rank + 1)
        else
          self%nc_piece = 0_I8P
          do b=1_I8P, size(BLOCKS, kind=I8P)
            if (error == 0_I4P) error = h5_dataset_read(self%root, trim(BLOCKS(b))//'/NumberOfCells', more)
            if (error == 0_I4P) self%nc_piece = self%nc_piece + more(ivalues(k) + self%rank + 1)
          enddo
        endif
      endif
    endif
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
    ! the arrays: the tuples each complete step reads (its offset and the points or cells of its parts)
    if (error == 0_I4P) error = step_tuples(self%root, self%topology, ivalues(1:k), counts(1:k), point_rows, cell_rows)
    if (error == 0_I4P) error = trimmed_arrays(self%root, 'PointData', k, point_rows, self%mesh_start(2), &
                                               self%point_arrays, self%point_start, self%point_prev)
    if (error == 0_I4P) error = trimmed_arrays(self%root, 'CellData', k, cell_rows, self%total_cells - rows(2), &
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
    point_rows = [(1_I8P, b=1_I8P, k)] ! one time slice per step
    if (error == 0_I4P) error = trimmed_arrays(self%root, 'PointData', k, point_rows, k - 1_I8P, self%point_arrays, &
                                               self%point_start, self%point_prev)
    if (error == 0_I4P) error = trimmed_arrays(self%root, 'CellData', k, point_rows, k - 1_I8P, self%cell_arrays, &
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
    !< Trim every array of `group` to the tuples (slices) the complete steps read: the largest end, offset of a step plus its
    !< `rows`, over the steps 1 to `k` (a step that reused an array, or wrote none of it, points before the end of the
    !< ones before); without its offsets dataset, the `rows` of the step k from `default`. List their names (none if the
    !< group was never written) and start their bookkeeping: tuples at the start of the next step, offset of step k.
    integer(hid_kind),             intent(in)  :: root     !< Group /VTKHDF.
    character(*),                  intent(in)  :: group    !< PointData or CellData.
    integer(I8P),                  intent(in)  :: k        !< Last complete step (NSteps).
    integer(I8P),                  intent(in)  :: rows(:)  !< Tuples (slices) of each step, 1 to k.
    integer(I8P),                  intent(in)  :: default  !< Offset of an array without its offsets dataset.
    character(len=:), allocatable, intent(out) :: list     !< Names, each followed by NAMES_SEPARATOR.
    integer(I8P), allocatable,     intent(out) :: start(:) !< Tuples of each array kept.
    integer(I8P), allocatable,     intent(out) :: prev(:)  !< Offset of each array at the last step.
    integer(I4P)                               :: error    !< Error status.
    integer(I8P), allocatable                  :: offsets(:) !< Offsets of an array.
    integer(I8P)                               :: first, last !< Bounds of a name.
    integer(I8P)                               :: offset   !< Offset of an array at the last step.
    integer(I8P)                               :: length   !< Tuples kept.

    list = ''
    allocate(start(0), prev(0))
    error = 0_I4P
    if (h5_link_exists(root, group)) error = h5_group_names(root, group, list)
    first = 1_I8P
    do while (first <= len(list, kind=I8P) .and. error == 0_I4P)
      last = first - 1_I8P + index(list(first:), NAMES_SEPARATOR, kind=I8P)
      offset = default
      length = default + rows(k)
      if (h5_link_exists(root, 'Steps/'//group//'Offsets/'//list(first:last-1))) then
        error = h5_dataset_read(root, 'Steps/'//group//'Offsets/'//list(first:last-1), offsets)
        ! the entries of steps 1 to k: an interrupted run may have left one more, of a step not complete
        if (error == 0_I4P) then
          if (size(offsets, kind=I8P) >= k .and. size(rows, kind=I8P) >= k) then
            offset = offsets(k)
            length = maxval(offsets(1:k) + rows(1:k))
          else
            error = 1_I4P
          endif
        endif
      endif
      if (error == 0_I4P) error = trim_to(root, group//'/'//list(first:last-1), length)
      start = [start, length]
      prev = [prev, offset]
      first = last + 1_I8P
    enddo
    endfunction trimmed_arrays

    function step_tuples(root, topology, part_offsets, parts, point_rows, cell_rows) result(error)
    !< The points and cells of each step of an UnstructuredGrid or PolyData: those of its parts (`NumberOfPoints`,
    !< `NumberOfCells` or the cells of the four PolyData blocks).
    integer(hid_kind),         intent(in)  :: root            !< Group /VTKHDF.
    integer(I4P),              intent(in)  :: topology        !< Topology.
    integer(I8P),              intent(in)  :: part_offsets(:) !< First part of each step, from 0.
    integer(I8P),              intent(in)  :: parts(:)        !< Parts of each step.
    integer(I8P), allocatable, intent(out) :: point_rows(:)   !< Points of each step.
    integer(I8P), allocatable, intent(out) :: cell_rows(:)    !< Cells of each step.
    integer(I4P)                           :: error           !< Error status.
    integer(I8P), allocatable              :: n(:)            !< Counts of the parts.
    integer(I8P)                           :: j, b            !< Counters.

    allocate(point_rows(size(parts)), cell_rows(size(parts)))
    cell_rows = 0_I8P
    error = h5_dataset_read(root, 'NumberOfPoints', n)
    if (error /= 0_I4P) return
    do j=1, size(parts, kind=I8P)
      point_rows(j) = sum(n(part_offsets(j)+1:part_offsets(j)+parts(j)))
    enddo
    do b=1_I8P, merge(1_I8P, 4_I8P, topology == TOPOLOGY_UNSTRUCTURED)
      if (topology == TOPOLOGY_UNSTRUCTURED) then
        error = h5_dataset_read(root, 'NumberOfCells', n)
      else
        error = h5_dataset_read(root, trim(BLOCKS(b))//'/NumberOfCells', n)
      endif
      if (error /= 0_I4P) return
      do j=1, size(parts, kind=I8P)
        cell_rows(j) = cell_rows(j) + sum(n(part_offsets(j)+1:part_offsets(j)+parts(j)))
      enddo
    enddo
    endfunction step_tuples

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
  endfunction reopen_file

  elemental subroutine reset(self)
  !< Reset the state of the writer, but the HDF5 identifiers.
  class(vtkhdf_writer), intent(inout) :: self !< Writer.
  integer(I4P)                        :: r    !< Counter.

  self%group_name = ''
  self%whole_extent = 0_I4P
  self%n_pieces = 0_I4P
  self%local_pieces = 0_I4P
  self%step_local = 0_I4P
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
  self%has_region = .false.
  if (allocated(self%regions)) deallocate(self%regions)
  self%point_start = [integer(I8P) ::] ; self%point_prev = [integer(I8P) ::]
  self%cell_start = [integer(I8P) ::] ; self%cell_prev = [integer(I8P) ::]
  self%field_arrays = ''
  self%is_reopened = .false.
  self%point_arrays = ''
  self%cell_arrays = ''
  self%row_arrays = ''
  self%table_rows = -1_I8P
  self%row_total = 0_I8P
  self%level = -1_I4P
  self%n_levels = 0_I4P
  if (allocated(self%level_spacing)) deallocate(self%level_spacing)
  if (allocated(self%level_counts)) deallocate(self%level_counts)
  self%has_domain = .false.
  self%domain = 0._R8P
  if (allocated(self%tree_bits)) deallocate(self%tree_bits)
  if (allocated(self%tree_mask)) deallocate(self%tree_mask)
  self%has_mask = .false.
  if (allocated(self%tree_ids)) deallocate(self%tree_ids)
  if (allocated(self%tree_depths)) deallocate(self%tree_depths)
  if (allocated(self%tree_counts)) deallocate(self%tree_counts)
  self%tree_cells = 0_I8P
  self%n_tree_ids = 0_I8P
  self%n_tree_counts = 0_I8P
  self%n_tree_bits = 0_I8P
  self%is_tree_data = .false.
  self%tree_totals = 0_I8P
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
  ! the checks of this rank's arguments and state, agreed by the ranks of comm before their collective calls
  error = 0_I4P
  if (np < 0_I8P .or. nc < 0_I8P) then
    error = refuse('write_piece: negative counts')
  elseif (present(block_cells)) then
    if (any(block_cells < 0_I8P)) error = refuse('write_piece: negative counts')
  endif
  if (error == 0_I4P .and. self%is_parallel .and. self%step_local > 0_I4P) &
    error = refuse('write_piece: one piece per rank of comm (in each step of a time series)')
  error = self%agree(error)
  if (error /= 0_I4P) return
  if (present(block_cells)) self%block_cells = block_cells
  if (self%is_reusing()) then
    if (self%is_written_in_step()) then
      error = refuse('write_piece: arrays are already written in this step without its pieces')
      return
    endif
  endif
  self%local_pieces = self%local_pieces + 1_I4P
  self%step_local = self%step_local + 1_I4P
  if (self%is_parallel) then ! one partition per rank: the parts of the file are those of all the ranks
    self%n_pieces = self%n_pieces + int(h5_mpi_sum(self%comm, 1_I8P), I4P)
    self%step_pieces = self%step_pieces + h5_mpi_sum(self%comm, 1_I8P)
  else
    self%n_pieces = self%n_pieces + 1_I4P
    self%step_pieces = self%step_pieces + 1_I8P
  endif
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
  integer(I8P)                        :: n      !< Pieces before the open step (with comm).
  integer(I8P)                        :: rows   !< 1 on rank 0, 0 on the others (with comm).
  integer(I8P), allocatable           :: zeros(:) !< Zero entries.

  error = 0_I4P
  if (self%has_polyhedra) return
  error = h5_dataset_read(self%root, 'NumberOfCells', nc)
  if (error /= 0_I4P) then
    error = refuse('cannot read the counts of cells of the pieces')
    return
  endif
  if (self%is_parallel) then ! the parts before those of the open step (a part per rank), written once (rank 0)
    ! (the zeros in blocks of at most ZERO_BLOCK values, on rank 0 alone: the parts before may be many; the number of
    ! blocks is the same on every rank, each block a collective append)
    n = size(nc, kind=I8P) - h5_mpi_sum(self%comm, 1_I8P)
    rows = merge(1_I8P, 0_I8P, self%rank == 0_I4P)
    allocate(zeros(rows * min(ZERO_BLOCK, max(n, sum(nc(1:n)) + n))))
    zeros = 0_I8P
    error = store_I8P(self, self%root, 'FaceConnectivity', [integer(I8P) ::], [0_I8P])
    if (error == 0_I4P) error = store_zeros(self, zeros, rows, 'FaceOffsets', n)
    if (error == 0_I4P) error = store_I8P(self, self%root, 'PolyhedronToFaces', [integer(I8P) ::], [0_I8P])
    if (error == 0_I4P) error = store_zeros(self, zeros, rows, 'PolyhedronOffsets', sum(nc(1:n)) + n)
    if (error == 0_I4P) error = store_zeros(self, zeros, rows, 'NumberOfFaces', n)
    if (error == 0_I4P) error = store_zeros(self, zeros, rows, 'NumberOfFaceConnectivityIds', n)
    if (error == 0_I4P) error = store_zeros(self, zeros, rows, 'NumberOfPolyhedronToFaceIds', n)
    if (error /= 0_I4P) then
      error = refuse('cannot write the (empty) polyhedra of the pieces before')
      return
    endif
  else
    do p=1_I8P, size(nc, kind=I8P) - 1_I8P ! the open piece is the last one
      error = self%no_faces(nc(p))
      if (error /= 0_I4P) return
    enddo
  endif
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
  integer(I8P)                        :: rows      !< Rows written by this process (with comm, rank 0 alone).

  n = size(values, kind=I8P)
  rows = 1_I8P
  if (self%is_parallel .and. self%rank /= 0_I4P) rows = 0_I8P ! with comm, written once (by rank 0), extended by all
  error = 0_I4P
  if (h5_link_exists(self%root, 'Steps/'//name)) then
    error = h5_dataset_resize(self%root, 'Steps/'//name, self%n_steps - 1_I8P)
  elseif (self%n_steps > 1_I8P) then
    allocate(zeros(n * (self%n_steps - 1_I8P)))
    zeros = 0_I8P
    if (n == 1_I8P) then
      error = store_I8P(self, self%root, 'Steps/'//name, zeros, [rows * (self%n_steps - 1_I8P)])
    else
      error = store_I8P(self, self%root, 'Steps/'//name, zeros, [n, rows * (self%n_steps - 1_I8P)])
    endif
  endif
  if (error == 0_I4P) then
    if (n == 1_I8P) then
      error = store_I8P(self, self%root, 'Steps/'//name, values, [rows])
    else
      error = store_I8P(self, self%root, 'Steps/'//name, values, [n, rows])
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
  integer(I4P)                        :: lv        !< A level of an OverlappingAMR, from 1.

  k = self%n_steps
  if (h5_link_exists(self%root, 'Steps/Values')) then
    error = h5_dataset_resize(self%root, 'Steps/Values', k - 1_I8P)
    if (error /= 0_I4P) return
  endif
  error = store_R8P(self, self%root, 'Steps/Values', [self%step_time], &
                    [merge(0_I8P, 1_I8P, self%is_parallel .and. self%rank /= 0_I4P)]) ! with comm, once (rank 0)
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
  elseif (self%topology == TOPOLOGY_HTG) then ! where the coordinates, trees, bits and cells of the step start
    grid = self%grid_points()
    if (error == 0_I4P) error = self%step_entry('PartOffsets', [k - 1_I8P])
    if (error == 0_I4P) error = self%step_entry('NumberOfParts', [1_I8P])
    if (error == 0_I4P) error = self%step_entry('XCoordinatesOffsets', [(k - 1_I8P) * grid(1)])
    if (error == 0_I4P) error = self%step_entry('YCoordinatesOffsets', [(k - 1_I8P) * grid(2)])
    if (error == 0_I4P) error = self%step_entry('ZCoordinatesOffsets', [(k - 1_I8P) * grid(3)])
    if (error == 0_I4P) error = self%step_entry('TreeIdsOffsets', [self%tree_totals(1)])
    if (error == 0_I4P) error = self%step_entry('DepthPerTreeOffsets', [self%tree_totals(1)])
    if (error == 0_I4P) error = self%step_entry('NumberOfCellsPerTreeDepthOffsets', [self%tree_totals(2)])
    if (error == 0_I4P) error = self%step_entry('DescriptorsOffsets', [self%tree_totals(3)])
    if (error == 0_I4P) error = self%step_entry('MaskOffsets', [self%tree_totals(4)])
  elseif (self%topology == TOPOLOGY_RECT) then ! the coordinates of the last step that wrote them
    grid = self%grid_points()
    if (error == 0_I4P) error = self%step_entry('XCoordinatesOffsets', [(self%geo_slices - 1_I8P) * grid(1)])
    if (error == 0_I4P) error = self%step_entry('YCoordinatesOffsets', [(self%geo_slices - 1_I8P) * grid(2)])
    if (error == 0_I4P) error = self%step_entry('ZCoordinatesOffsets', [(self%geo_slices - 1_I8P) * grid(3)])
  elseif (self%topology == TOPOLOGY_STRG) then ! in time slices (required by VTK)
    if (error == 0_I4P) error = self%step_entry('PointOffsets', [self%geo_slices - 1_I8P])
  elseif (self%topology == TOPOLOGY_AMR) then ! each level: its boxes in the step, where they and their arrays start
    do lv=1_I4P, self%n_levels
      name = 'Level'//trim(str(n=lv - 1_I4P, no_sign=.true.))
      if (error == 0_I4P .and. .not.h5_link_exists(self%root, 'Steps/'//name)) then
        error = h5_group_create(self%root, 'Steps/'//name, steps, track_order=.true.)
        if (error == 0_I4P) error = h5_group_close(steps)
        if (error == 0_I4P) error = h5_group_create(self%root, 'Steps/'//name//'/PointDataOffsets', steps, track_order=.true.)
        if (error == 0_I4P) error = h5_group_close(steps)
        if (error == 0_I4P) error = h5_group_create(self%root, 'Steps/'//name//'/CellDataOffsets', steps, track_order=.true.)
        if (error == 0_I4P) error = h5_group_close(steps)
      endif
      if (error == 0_I4P) error = self%step_entry(name//'/AMRBoxOffsets', [self%level_counts(4, lv)])
      if (error == 0_I4P) error = self%step_entry(name//'/NumberOfAMRBoxes', [self%level_counts(1, lv) - self%level_counts(4, lv)])
      first = 1_I8P
      do while (first <= len(self%point_arrays, kind=I8P) .and. error == 0_I4P)
        last = first - 1_I8P + index(self%point_arrays(first:), NAMES_SEPARATOR, kind=I8P)
        error = self%step_entry(name//'/PointDataOffsets/'//self%point_arrays(first:last-1), [self%level_counts(5, lv)])
        first = last + 1_I8P
      enddo
      first = 1_I8P
      do while (first <= len(self%cell_arrays, kind=I8P) .and. error == 0_I4P)
        last = first - 1_I8P + index(self%cell_arrays(first:), NAMES_SEPARATOR, kind=I8P)
        error = self%step_entry(name//'/CellDataOffsets/'//self%cell_arrays(first:last-1), [self%level_counts(6, lv)])
        first = last + 1_I8P
      enddo
    enddo
  elseif (self%topology == TOPOLOGY_TABLE) then ! the rows of the step (NumberOfRows, one per step), where they start
    if (error == 0_I4P .and. h5_link_exists(self%root, 'NumberOfRows')) &
      error = h5_dataset_resize(self%root, 'NumberOfRows', k - 1_I8P)
    if (error == 0_I4P) error = store_I8P(self, self%root, 'NumberOfRows', [max(self%table_rows, 0_I8P)], [1_I8P])
    first = 1_I8P
    do while (first <= len(self%row_arrays, kind=I8P) .and. error == 0_I4P)
      last = first - 1_I8P + index(self%row_arrays(first:), NAMES_SEPARATOR, kind=I8P)
      error = self%step_entry('RowDataOffsets/'//self%row_arrays(first:last-1), [self%row_total])
      first = last + 1_I8P
    enddo
  endif
  ! point and cell data: where each array was written in the step, or its offset of the step before (reused); tuples,
  ! time slices for a grid
  if (error == 0_I4P .and. self%topology /= TOPOLOGY_AMR) &
    error = array_entries('PointData', 'PointDataOffsets', self%point_arrays, self%point_start, self%point_prev)
  if (error == 0_I4P .and. self%topology /= TOPOLOGY_AMR) &
    error = array_entries('CellData', 'CellDataOffsets', self%cell_arrays, self%cell_start, self%cell_prev)
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
  function store_zeros(self, zeros, rows, name, count) result(error)
  !< Append `count` zeros to the dataset `name`, from rank 0 (`rows` 1, 0 on the others), in blocks of at most ZERO_BLOCK
  !< values (`zeros`, of that size on rank 0): collective, as many blocks on every rank; the dataset is created by the
  !< first block even when `count` is 0.
  class(vtkhdf_writer), intent(in) :: self     !< Writer.
  integer(I8P),         intent(in) :: zeros(:) !< Zeros (a block, on rank 0).
  integer(I8P),         intent(in) :: rows     !< 1 on rank 0, 0 on the others.
  character(*),         intent(in) :: name     !< Dataset.
  integer(I8P),         intent(in) :: count    !< Zeros appended.
  integer(I4P)                     :: error    !< Error status.
  integer(I8P)                     :: done     !< Zeros appended so far.
  integer(I8P)                     :: m        !< Zeros of a block.

  done = 0_I8P
  do
    m = min(ZERO_BLOCK, count - done)
    error = store_I8P(self, self%root, name, zeros, [rows * m])
    done = done + m
    if (error /= 0_I4P .or. done >= count) exit
  enddo
  endfunction store_zeros

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

  pure function pack_bits(bits) result(bytes)
  !< Pack bits in bytes, the first bit the most significant one of the first byte (the last byte padded with zeros), as
  !< VTK packs the `Descriptors` and `Mask` of a HyperTreeGrid.
  logical, intent(in)       :: bits(:)  !< Bits.
  integer(I1P), allocatable :: bytes(:) !< Bytes (unsigned values in two's complement).
  integer(I4P)              :: value    !< Value of a byte, 0 to 255.
  integer(I8P)              :: b, i     !< Counters.

  allocate(bytes((size(bits, kind=I8P) + 7_I8P) / 8_I8P))
  do b=1_I8P, size(bytes, kind=I8P)
    value = 0_I4P
    do i=1_I8P, 8_I8P
      value = 2_I4P * value
      if (8_I8P * (b - 1_I8P) + i <= size(bits, kind=I8P)) then
        if (bits(8_I8P * (b - 1_I8P) + i)) value = value + 1_I4P
      endif
    enddo
    bytes(b) = int(value - merge(256_I4P, 0_I4P, value > 127_I4P), I1P)
  enddo
  endfunction pack_bits

  pure subroutine append_I8P(values, n, more)
  !< Append `more` to the first `n` entries of `values` (its capacity doubled when full), `n` updated.
  integer(I8P), allocatable, intent(inout) :: values(:) !< Values, the first n used.
  integer(I8P),              intent(inout) :: n         !< Entries used.
  integer(I8P),              intent(in)    :: more(:)   !< Values appended.
  integer(I8P), allocatable                :: bigger(:) !< Values, a larger capacity.

  if (.not.allocated(values)) allocate(values(max(16_I8P, size(more, kind=I8P))))
  if (n + size(more, kind=I8P) > size(values, kind=I8P)) then
    allocate(bigger(max(2_I8P * size(values, kind=I8P), n + size(more, kind=I8P))))
    bigger(1:n) = values(1:n)
    call move_alloc(bigger, values)
  endif
  values(n + 1_I8P:n + size(more, kind=I8P)) = more
  n = n + size(more, kind=I8P)
  endsubroutine append_I8P

  pure subroutine append_bits(values, n, more, n_more)
  !< Append `n_more` bits (`more`, or false if not present) to the first `n` of `values` (capacity doubled), `n` updated.
  logical, allocatable, intent(inout)        :: values(:) !< Bits, the first n used.
  integer(I8P),         intent(inout)        :: n         !< Bits used.
  logical,              intent(in), optional :: more(:)   !< Bits appended.
  integer(I8P),         intent(in)           :: n_more    !< Bits appended.
  logical, allocatable                       :: bigger(:) !< Bits, a larger capacity.

  if (.not.allocated(values)) allocate(values(max(64_I8P, n_more)))
  if (n + n_more > size(values, kind=I8P)) then
    allocate(bigger(max(2_I8P * size(values, kind=I8P), n + n_more)))
    bigger(1:n) = values(1:n)
    call move_alloc(bigger, values)
  endif
  if (present(more)) then
    values(n + 1_I8P:n + n_more) = more
  else
    values(n + 1_I8P:n + n_more) = .false.
  endif
  n = n + n_more
  endsubroutine append_bits

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
