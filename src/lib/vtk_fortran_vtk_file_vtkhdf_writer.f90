!< VTK file VTKHDF writer: an HDF5 file in the VTKHDF layout (ImageData, UnstructuredGrid).
module vtk_fortran_vtk_file_vtkhdf_writer
!< VTK file VTKHDF writer: an HDF5 file in the VTKHDF layout (ImageData, UnstructuredGrid).
!<
!< The writer of `vtk_file%initialize(format='VTKHDF', ...)`: the same calls as the XML formats write the group `/VTKHDF`
!< of an HDF5 file (VTKHDF specification 2.x, read by VTK >= 9.1 and ParaView): the attributes `Version`, `Type`,
!< `WholeExtent`, `Origin`, `Spacing`, `Direction`, the groups `PointData`, `CellData` and `FieldData`, one dataset per
!< data array. A Fortran array `x(n_components, i, j, k)` is stored as it is in memory: it is the VTKHDF dataset of shape
!< `[k, j, i, n_components]` (C order), so nothing is transposed. With `compressor='zlib'` the datasets are deflated.
!<
!< Supported: ImageData, one piece (its extent is the whole extent); UnstructuredGrid, each piece a partition (its counts
!< in `NumberOfPoints`, `NumberOfCells`, `NumberOfConnectivityIds`, its points, cells and arrays appended to datasets
!< shared by the pieces; the offsets written from 0, VTKFortran's end offsets converted), ids and counts I4P or I8P;
!< data arrays of every kind and layout, unsigned arrays, active arrays (`Attribute`, VTKHDF 2.6), numeric field data.
!< Refused (non-zero error, message on stderr): other topologies and polyhedra (not yet), a volatile file, string field
!< data (the VTK reader crashes on them), zlib without the HDF5 deflate filter, an array missing from a piece or new
!< after the first one, and every call when the library is built without HDF5 (`VTKFORTRAN_USE_HDF5`).
!<
!< `Version` is written by `finalize`: 2.0, or 2.6 when an active array is marked.
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
character(*), parameter :: NAMES_SEPARATOR = achar(0)    !< Separator of the names of a list (a name has no NUL).

type :: active_array
  !< Name of the active array of a role.
  character(len=:), allocatable :: name !< Array name.
endtype active_array

type, extends(vtk_writer_abstract) :: vtkhdf_writer
  !< VTKHDF writer (ImageData).
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
  integer(I4P)                  :: topology=0_I4P          !< Topology: TOPOLOGY_IMAGE or TOPOLOGY_UNSTRUCTURED.
  integer(I8P)                  :: np_piece=0_I8P          !< Points of the open piece (UnstructuredGrid).
  integer(I8P)                  :: nc_piece=0_I8P          !< Cells of the open piece (UnstructuredGrid).
  integer(I8P)                  :: total_points=0_I8P      !< Points of the pieces started (UnstructuredGrid).
  integer(I8P)                  :: total_cells=0_I8P       !< Cells of the pieces started (UnstructuredGrid).
  logical                       :: is_geo_written=.false.  !< The points of the open piece are written.
  logical                       :: is_connectivity_written=.false. !< The cells of the open piece are written.
  character(len=:), allocatable :: point_arrays            !< Names of the point data arrays, NUL terminated.
  character(len=:), allocatable :: cell_arrays             !< Names of the cell data arrays, NUL terminated.
  contains
    procedure, pass(self) :: initialize
    procedure, pass(self) :: finalize
    procedure, pass(self) :: write_piece_start_tag
    procedure, pass(self) :: write_piece_end_tag
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
    procedure, pass(self) :: write_geo_strg_data1_rank2_R8P
    procedure, pass(self) :: write_geo_strg_data1_rank2_R4P
    procedure, pass(self) :: write_geo_strg_data1_rank4_R8P
    procedure, pass(self) :: write_geo_strg_data1_rank4_R4P
    procedure, pass(self) :: write_geo_strg_data3_rank1_R8P
    procedure, pass(self) :: write_geo_strg_data3_rank1_R4P
    procedure, pass(self) :: write_geo_strg_data3_rank3_R8P
    procedure, pass(self) :: write_geo_strg_data3_rank3_R4P
    procedure, pass(self) :: write_geo_rect_data3_rank1_R8P
    procedure, pass(self) :: write_geo_rect_data3_rank1_R4P
    procedure, pass(self) :: write_piece_start_tag_poly
    procedure, pass(self) :: write_piece_start_tag_poly_I8P
    procedure, pass(self) :: write_polydata_cells
    ! private helpers
    procedure, pass(self), private :: check_cells   !< Check a write_connectivity call of an UnstructuredGrid piece.
    procedure, pass(self), private :: check_geo     !< Check a write_geo call of an UnstructuredGrid piece.
    procedure, pass(self), private :: close_group   !< Close the open data group.
    procedure, pass(self), private :: dataset_dims  !< Shape of the dataset of a data array.
    procedure, pass(self), private :: deflate_level !< Deflate level of the datasets.
    procedure, pass(self), private :: end_partition !< End a piece of an UnstructuredGrid: check it.
    procedure, pass(self), private :: mark_active   !< Mark a dataset as active array.
    procedure, pass(self), private :: open_group    !< Open (or create) a data group.
    procedure, pass(self), private :: record_array  !< Record the name of a point or cell data array.
    procedure, pass(self), private :: reset         !< Reset the state of the writer.
    procedure, pass(self), private :: start_partition !< Start a piece of an UnstructuredGrid.
endtype vtkhdf_writer

contains
  function initialize(self, format, filename, mesh_topology, nx1, nx2, ny1, ny2, nz1, nz2, &
      is_volatile, mesh_kind) result(error)
  !< Initialize the writer: create the file and write the header of the topology.
  !<
  !< `format` is VTKHDF (not checked here), `mesh_topology` ImageData, with its whole extent `nx1`...`nz2` (origin,
  !< spacing and direction are set by `vtk_file%initialize`), or UnstructuredGrid. `is_volatile` is refused, `mesh_kind`
  !< unused (the points keep the kind they are given).
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
  case default
    error = refuse('topology '//trim(adjustl(mesh_topology))//': only ImageData and UnstructuredGrid are supported yet')
    return
  endselect
  if (self%topology == TOPOLOGY_IMAGE) then
    if (.not.(present(nx1).and.present(nx2).and.present(ny1).and.present(ny2).and.present(nz1).and.present(nz2))) then
      error = refuse('ImageData needs its whole extent: nx1, nx2, ny1, ny2, nz1, nz2')
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
  error = h5_file_create(filename, self%file_id)
  if (error /= 0_I4P) error = refuse('cannot create '//filename)
  if (error == 0_I4P) error = h5_group_create(self%file_id, 'VTKHDF', self%root)
  if (self%topology == TOPOLOGY_IMAGE) then
    if (error == 0_I4P) error = h5_attribute_write(self%root, 'Type', 'ImageData')
    if (error == 0_I4P) error = h5_attribute_write(self%root, 'WholeExtent', int(self%whole_extent, I8P))
    if (error == 0_I4P) error = h5_attribute_write(self%root, 'Origin', self%origin)
    if (error == 0_I4P) error = h5_attribute_write(self%root, 'Spacing', self%spacing)
    if (error == 0_I4P) error = h5_attribute_write(self%root, 'Direction', self%direction)
  else
    if (error == 0_I4P) error = h5_attribute_write(self%root, 'Type', 'UnstructuredGrid')
  endif
  endfunction initialize

  function finalize(self) result(error)
  !< Finalize the writer: write the version (the lowest one with the features used) and close the file.
  !<
  !< A group or a piece left open is closed.
  class(vtkhdf_writer),       intent(inout) :: self  !< Writer.
  integer(I4P)                              :: error !< Error status.

  error = 1_I4P
  if (self%file_id < 0_hid_kind) return
  error = 0_I4P
  if (self%group >= 0_hid_kind) then
    if (h5_group_close(self%group) /= 0_I4P) error = 1_I4P
  endif
  if (h5_attribute_write(self%root, 'Version', [2_I8P, self%version_minor]) /= 0_I4P) error = 1_I4P
  if (h5_group_close(self%root) /= 0_I4P) error = 1_I4P
  if (h5_file_close(self%file_id) /= 0_I4P) error = 1_I4P
  self%file_id = -1_hid_kind ; self%root = -1_hid_kind ; self%group = -1_hid_kind
  call self%reset
  endfunction finalize

  function write_piece_start_tag(self, nx1, nx2, ny1, ny2, nz1, nz2) result(error)
  !< Start the piece of an ImageData: VTKHDF stores one piece, whose extent is the whole extent.
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
  if (self%topology /= TOPOLOGY_IMAGE) then
    error = refuse('write_piece(nx1, ..., nz2) is for an ImageData: an UnstructuredGrid piece is write_piece(np, nc)')
  elseif (self%n_pieces > 0_I4P) then
    error = refuse('an ImageData has one piece in VTKHDF')
  elseif (any([nx1, nx2, ny1, ny2, nz1, nz2] /= self%whole_extent)) then
    error = refuse('the extent of the piece must be the whole extent (one piece in VTKHDF)')
  else
    self%n_pieces = 1_I4P
    self%is_piece_open = .true.
    error = 0_I4P
  endif
  endfunction write_piece_start_tag

  function write_piece_end_tag(self) result(error)
  !< End the piece. A piece of an UnstructuredGrid needs its points and cells, and every array of the first piece.
  class(vtkhdf_writer),       intent(inout) :: self
  integer(I4P)                              :: error

  error = 1_I4P
  if (.not.self%is_piece_open .or. self%group >= 0_hid_kind) return
  self%is_piece_open = .false.
  error = 0_I4P
  if (self%topology == TOPOLOGY_UNSTRUCTURED) error = self%end_partition()
  endfunction write_piece_end_tag

  function write_dataarray_location_tag(self, location, action, scalars, vectors, normals, tensors, tcoords) result(error)
  !< Open or close the group of the point (`node`) or cell (`cell`) data, inside the piece.
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
    if (.not.self%is_piece_open .or. self%group >= 0_hid_kind) return
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
  !< Write the points of the piece of an UnstructuredGrid (R8P), `xyz(1:3,1:np)`.
  class(vtkhdf_writer),       intent(inout) :: self
  integer(I4P),               intent(in)    :: np
  integer(I4P),               intent(in)    :: nc
  real(R8P),                  intent(in)    :: xyz(1:,1:)
  integer(I4P)                              :: error

  error = put_points_R8P(self, int(np, I8P), int(nc, I8P), xyz, size(xyz, dim=1, kind=I8P), size(xyz, dim=2, kind=I8P))
  endfunction write_geo_unst_data1_rank2_R8P

  function write_geo_unst_data3_rank1_R8P(self, np, nc, x, y, z) result(error)
  !< Write the points of the piece of an UnstructuredGrid (R8P), by coordinate.
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
  !< Write the points of the piece of an UnstructuredGrid (R8P), `xyz(1:3,1:np)`.
  class(vtkhdf_writer),       intent(inout) :: self
  integer(I8P),               intent(in)    :: np
  integer(I8P),               intent(in)    :: nc
  real(R8P),                  intent(in)    :: xyz(1:,1:)
  integer(I4P)                              :: error

  error = put_points_R8P(self, np, nc, xyz, size(xyz, dim=1, kind=I8P), size(xyz, dim=2, kind=I8P))
  endfunction write_geo_unst_data1_rank2_R8P_I8P

  function write_geo_unst_data3_rank1_R8P_I8P(self, np, nc, x, y, z) result(error)
  !< Write the points of the piece of an UnstructuredGrid (R8P), by coordinate.
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
  !< Write the points of the piece of an UnstructuredGrid (R4P), `xyz(1:3,1:np)`.
  class(vtkhdf_writer),       intent(inout) :: self
  integer(I4P),               intent(in)    :: np
  integer(I4P),               intent(in)    :: nc
  real(R4P),                  intent(in)    :: xyz(1:,1:)
  integer(I4P)                              :: error

  error = put_points_R4P(self, int(np, I8P), int(nc, I8P), xyz, size(xyz, dim=1, kind=I8P), size(xyz, dim=2, kind=I8P))
  endfunction write_geo_unst_data1_rank2_R4P

  function write_geo_unst_data3_rank1_R4P(self, np, nc, x, y, z) result(error)
  !< Write the points of the piece of an UnstructuredGrid (R4P), by coordinate.
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
  !< Write the points of the piece of an UnstructuredGrid (R4P), `xyz(1:3,1:np)`.
  class(vtkhdf_writer),       intent(inout) :: self
  integer(I8P),               intent(in)    :: np
  integer(I8P),               intent(in)    :: nc
  real(R4P),                  intent(in)    :: xyz(1:,1:)
  integer(I4P)                              :: error

  error = put_points_R4P(self, np, nc, xyz, size(xyz, dim=1, kind=I8P), size(xyz, dim=2, kind=I8P))
  endfunction write_geo_unst_data1_rank2_R4P_I8P

  function write_geo_unst_data3_rank1_R4P_I8P(self, np, nc, x, y, z) result(error)
  !< Write the points of the piece of an UnstructuredGrid (R4P), by coordinate.
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
  !< written from 0 as VTKHDF wants them), types. Polyhedra (`face`, `faceoffset`) are not supported yet.
  class(vtkhdf_writer),       intent(inout) :: self
  integer(I4P),               intent(in)    :: nc
  integer(I4P),               intent(in)    :: connectivity(1:)
  integer(I4P),               intent(in)    :: offset(1:)
  integer(I4P),   optional,   intent(in)    :: face(1:)
  integer(I4P),   optional,   intent(in)    :: faceoffset(1:)
  integer(I1P),               intent(in)    :: cell_type(1:)
  integer(I4P)                              :: error

  if (present(face) .or. present(faceoffset)) then
    error = refuse('polyhedra (face, faceoffset) are not supported yet')
    return
  endif
  error = put_cells_I4P(self, int(nc, I8P), connectivity, offset, cell_type)
  endfunction write_connectivity_I4P

  function write_connectivity_I8P(self, nc, connectivity, offset, cell_type, face, faceoffset) result(error)
  !< Write the cells of the piece of an UnstructuredGrid (I8P ids): connectivity, offsets (VTKFortran end offsets,
  !< written from 0 as VTKHDF wants them), types. Polyhedra (`face`, `faceoffset`) are not supported yet.
  class(vtkhdf_writer),       intent(inout) :: self
  integer(I8P),               intent(in)    :: nc
  integer(I8P),               intent(in)    :: connectivity(1:)
  integer(I8P),               intent(in)    :: offset(1:)
  integer(I8P),   optional,   intent(in)    :: face(1:)
  integer(I8P),   optional,   intent(in)    :: faceoffset(1:)
  integer(I1P),               intent(in)    :: cell_type(1:)
  integer(I4P)                              :: error

  if (present(face) .or. present(faceoffset)) then
    error = refuse('polyhedra (face, faceoffset) are not supported yet')
    return
  endif
  error = put_cells_I8P(self, int(nc, I8P), connectivity, offset, cell_type)
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

  function write_geo_strg_data1_rank2_R8P(self, xyz) result(error)
  !< Not supported yet by the VTKHDF writer (topologies other than ImageData and UnstructuredGrid): an error.
  class(vtkhdf_writer),       intent(inout) :: self
  real(R8P),                  intent(in)    :: xyz(1:,1:)
  integer(I4P)                              :: error

  error = refuse('write_geo of this topology: only ImageData and UnstructuredGrid are supported yet')
  endfunction write_geo_strg_data1_rank2_R8P

  function write_geo_strg_data1_rank2_R4P(self, xyz) result(error)
  !< Not supported yet by the VTKHDF writer (topologies other than ImageData and UnstructuredGrid): an error.
  class(vtkhdf_writer),       intent(inout) :: self
  real(R4P),                  intent(in)    :: xyz(1:,1:)
  integer(I4P)                              :: error

  error = refuse('write_geo of this topology: only ImageData and UnstructuredGrid are supported yet')
  endfunction write_geo_strg_data1_rank2_R4P

  function write_geo_strg_data1_rank4_R8P(self, xyz) result(error)
  !< Not supported yet by the VTKHDF writer (topologies other than ImageData and UnstructuredGrid): an error.
  class(vtkhdf_writer),       intent(inout) :: self
  real(R8P),                  intent(in)    :: xyz(1:,1:,1:,1:)
  integer(I4P)                              :: error

  error = refuse('write_geo of this topology: only ImageData and UnstructuredGrid are supported yet')
  endfunction write_geo_strg_data1_rank4_R8P

  function write_geo_strg_data1_rank4_R4P(self, xyz) result(error)
  !< Not supported yet by the VTKHDF writer (topologies other than ImageData and UnstructuredGrid): an error.
  class(vtkhdf_writer),       intent(inout) :: self
  real(R4P),                  intent(in)    :: xyz(1:,1:,1:,1:)
  integer(I4P)                              :: error

  error = refuse('write_geo of this topology: only ImageData and UnstructuredGrid are supported yet')
  endfunction write_geo_strg_data1_rank4_R4P

  function write_geo_strg_data3_rank1_R8P(self, n, x, y, z) result(error)
  !< Not supported yet by the VTKHDF writer (topologies other than ImageData and UnstructuredGrid): an error.
  class(vtkhdf_writer),       intent(inout) :: self
  integer(I4P),               intent(in)    :: n
  real(R8P),                  intent(in)    :: x(1:)
  real(R8P),                  intent(in)    :: y(1:)
  real(R8P),                  intent(in)    :: z(1:)
  integer(I4P)                              :: error

  error = refuse('write_geo of this topology: only ImageData and UnstructuredGrid are supported yet')
  endfunction write_geo_strg_data3_rank1_R8P

  function write_geo_strg_data3_rank1_R4P(self, n, x, y, z) result(error)
  !< Not supported yet by the VTKHDF writer (topologies other than ImageData and UnstructuredGrid): an error.
  class(vtkhdf_writer),       intent(inout) :: self
  integer(I4P),               intent(in)    :: n
  real(R4P),                  intent(in)    :: x(1:)
  real(R4P),                  intent(in)    :: y(1:)
  real(R4P),                  intent(in)    :: z(1:)
  integer(I4P)                              :: error

  error = refuse('write_geo of this topology: only ImageData and UnstructuredGrid are supported yet')
  endfunction write_geo_strg_data3_rank1_R4P

  function write_geo_strg_data3_rank3_R8P(self, n, x, y, z) result(error)
  !< Not supported yet by the VTKHDF writer (topologies other than ImageData and UnstructuredGrid): an error.
  class(vtkhdf_writer),       intent(inout) :: self
  integer(I4P),               intent(in)    :: n
  real(R8P),                  intent(in)    :: x(1:,1:,1:)
  real(R8P),                  intent(in)    :: y(1:,1:,1:)
  real(R8P),                  intent(in)    :: z(1:,1:,1:)
  integer(I4P)                              :: error

  error = refuse('write_geo of this topology: only ImageData and UnstructuredGrid are supported yet')
  endfunction write_geo_strg_data3_rank3_R8P

  function write_geo_strg_data3_rank3_R4P(self, n, x, y, z) result(error)
  !< Not supported yet by the VTKHDF writer (topologies other than ImageData and UnstructuredGrid): an error.
  class(vtkhdf_writer),       intent(inout) :: self
  integer(I4P),               intent(in)    :: n
  real(R4P),                  intent(in)    :: x(1:,1:,1:)
  real(R4P),                  intent(in)    :: y(1:,1:,1:)
  real(R4P),                  intent(in)    :: z(1:,1:,1:)
  integer(I4P)                              :: error

  error = refuse('write_geo of this topology: only ImageData and UnstructuredGrid are supported yet')
  endfunction write_geo_strg_data3_rank3_R4P

  function write_geo_rect_data3_rank1_R8P(self, x, y, z) result(error)
  !< Not supported yet by the VTKHDF writer (topologies other than ImageData and UnstructuredGrid): an error.
  class(vtkhdf_writer),       intent(inout) :: self
  real(R8P),                  intent(in)    :: x(1:)
  real(R8P),                  intent(in)    :: y(1:)
  real(R8P),                  intent(in)    :: z(1:)
  integer(I4P)                              :: error

  error = refuse('write_geo of this topology: only ImageData and UnstructuredGrid are supported yet')
  endfunction write_geo_rect_data3_rank1_R8P

  function write_geo_rect_data3_rank1_R4P(self, x, y, z) result(error)
  !< Not supported yet by the VTKHDF writer (topologies other than ImageData and UnstructuredGrid): an error.
  class(vtkhdf_writer),       intent(inout) :: self
  real(R4P),                  intent(in)    :: x(1:)
  real(R4P),                  intent(in)    :: y(1:)
  real(R4P),                  intent(in)    :: z(1:)
  integer(I4P)                              :: error

  error = refuse('write_geo of this topology: only ImageData and UnstructuredGrid are supported yet')
  endfunction write_geo_rect_data3_rank1_R4P

  function write_piece_start_tag_poly(self, np, nverts, nlines, nstrips, npolys) result(error)
  !< Not supported yet by the VTKHDF writer (topologies other than ImageData and UnstructuredGrid): an error.
  class(vtkhdf_writer),       intent(inout) :: self
  integer(I4P),               intent(in)    :: np
  integer(I4P),               intent(in)    :: nverts
  integer(I4P),               intent(in)    :: nlines
  integer(I4P),               intent(in)    :: nstrips
  integer(I4P),               intent(in)    :: npolys
  integer(I4P)                              :: error

  error = refuse('write_piece of this topology: only ImageData and UnstructuredGrid are supported yet')
  endfunction write_piece_start_tag_poly

  function write_piece_start_tag_poly_I8P(self, np, nverts, nlines, nstrips, npolys) result(error)
  !< Not supported yet by the VTKHDF writer (topologies other than ImageData and UnstructuredGrid): an error.
  class(vtkhdf_writer),       intent(inout) :: self
  integer(I8P),               intent(in)    :: np
  integer(I8P),               intent(in)    :: nverts
  integer(I8P),               intent(in)    :: nlines
  integer(I8P),               intent(in)    :: nstrips
  integer(I8P),               intent(in)    :: npolys
  integer(I4P)                              :: error

  error = refuse('write_piece of this topology: only ImageData and UnstructuredGrid are supported yet')
  endfunction write_piece_start_tag_poly_I8P

  function write_polydata_cells(self, verts_connectivity, verts_offset, lines_connectivity, lines_offset, &
      strips_connectivity, strips_offset, polys_connectivity, polys_offset) result(error)
  !< Not supported yet by the VTKHDF writer (topologies other than ImageData and UnstructuredGrid): an error.
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

  error = refuse('write_polydata_cells of this topology: only ImageData and UnstructuredGrid are supported yet')
  endfunction write_polydata_cells

  ! private helpers
  function put_R8P(self, data_name, x, n, n_components) result(error)
  !< Write `n` values (R8P) as the dataset `data_name` of the open group: in an UnstructuredGrid, created by
  !< the first piece and appended to by the others.
  class(vtkhdf_writer), intent(inout) :: self         !< Writer.
  character(*), intent(in)            :: data_name    !< Data name.
  real(R8P), intent(in)               :: x(*)         !< Values.
  integer(I8P), intent(in)            :: n            !< Number of values.
  integer(I4P), intent(in)            :: n_components !< Number of components.
  integer(I4P)                        :: error        !< Error status.
  integer(I8P), allocatable           :: dims(:)      !< Shape of the dataset, Fortran order.
  logical                             :: is_new       !< The dataset is created (not appended to).

  error = self%dataset_dims(data_name, n, n_components, dims)
  if (error /= 0_I4P) return
  is_new = .not.h5_link_exists(self%group, data_name)
  if (self%topology == TOPOLOGY_UNSTRUCTURED .and. self%group_name /= 'FieldData') then
    if (is_new .and. self%n_pieces > 1_I4P) then
      error = refuse('the array '//data_name//' is not in the first piece: every piece writes the same arrays')
      return
    endif
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
  !< Write `n` values (R4P) as the dataset `data_name` of the open group: in an UnstructuredGrid, created by
  !< the first piece and appended to by the others.
  class(vtkhdf_writer), intent(inout) :: self         !< Writer.
  character(*), intent(in)            :: data_name    !< Data name.
  real(R4P), intent(in)               :: x(*)         !< Values.
  integer(I8P), intent(in)            :: n            !< Number of values.
  integer(I4P), intent(in)            :: n_components !< Number of components.
  integer(I4P)                        :: error        !< Error status.
  integer(I8P), allocatable           :: dims(:)      !< Shape of the dataset, Fortran order.
  logical                             :: is_new       !< The dataset is created (not appended to).

  error = self%dataset_dims(data_name, n, n_components, dims)
  if (error /= 0_I4P) return
  is_new = .not.h5_link_exists(self%group, data_name)
  if (self%topology == TOPOLOGY_UNSTRUCTURED .and. self%group_name /= 'FieldData') then
    if (is_new .and. self%n_pieces > 1_I4P) then
      error = refuse('the array '//data_name//' is not in the first piece: every piece writes the same arrays')
      return
    endif
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
  !< Write `n` values (I8P) as the dataset `data_name` of the open group: in an UnstructuredGrid, created by
  !< the first piece and appended to by the others.
  class(vtkhdf_writer), intent(inout) :: self         !< Writer.
  character(*), intent(in)            :: data_name    !< Data name.
  integer(I8P), intent(in)            :: x(*)         !< Values.
  integer(I8P), intent(in)            :: n            !< Number of values.
  integer(I4P), intent(in)            :: n_components !< Number of components.
  logical, intent(in), optional       :: unsigned     !< Unsigned values.
  integer(I4P)                        :: error        !< Error status.
  integer(I8P), allocatable           :: dims(:)      !< Shape of the dataset, Fortran order.
  logical                             :: is_new       !< The dataset is created (not appended to).

  error = self%dataset_dims(data_name, n, n_components, dims)
  if (error /= 0_I4P) return
  is_new = .not.h5_link_exists(self%group, data_name)
  if (self%topology == TOPOLOGY_UNSTRUCTURED .and. self%group_name /= 'FieldData') then
    if (is_new .and. self%n_pieces > 1_I4P) then
      error = refuse('the array '//data_name//' is not in the first piece: every piece writes the same arrays')
      return
    endif
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
  !< Write `n` values (I4P) as the dataset `data_name` of the open group: in an UnstructuredGrid, created by
  !< the first piece and appended to by the others.
  class(vtkhdf_writer), intent(inout) :: self         !< Writer.
  character(*), intent(in)            :: data_name    !< Data name.
  integer(I4P), intent(in)            :: x(*)         !< Values.
  integer(I8P), intent(in)            :: n            !< Number of values.
  integer(I4P), intent(in)            :: n_components !< Number of components.
  logical, intent(in), optional       :: unsigned     !< Unsigned values.
  integer(I4P)                        :: error        !< Error status.
  integer(I8P), allocatable           :: dims(:)      !< Shape of the dataset, Fortran order.
  logical                             :: is_new       !< The dataset is created (not appended to).

  error = self%dataset_dims(data_name, n, n_components, dims)
  if (error /= 0_I4P) return
  is_new = .not.h5_link_exists(self%group, data_name)
  if (self%topology == TOPOLOGY_UNSTRUCTURED .and. self%group_name /= 'FieldData') then
    if (is_new .and. self%n_pieces > 1_I4P) then
      error = refuse('the array '//data_name//' is not in the first piece: every piece writes the same arrays')
      return
    endif
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
  !< Write `n` values (I2P) as the dataset `data_name` of the open group: in an UnstructuredGrid, created by
  !< the first piece and appended to by the others.
  class(vtkhdf_writer), intent(inout) :: self         !< Writer.
  character(*), intent(in)            :: data_name    !< Data name.
  integer(I2P), intent(in)            :: x(*)         !< Values.
  integer(I8P), intent(in)            :: n            !< Number of values.
  integer(I4P), intent(in)            :: n_components !< Number of components.
  logical, intent(in), optional       :: unsigned     !< Unsigned values.
  integer(I4P)                        :: error        !< Error status.
  integer(I8P), allocatable           :: dims(:)      !< Shape of the dataset, Fortran order.
  logical                             :: is_new       !< The dataset is created (not appended to).

  error = self%dataset_dims(data_name, n, n_components, dims)
  if (error /= 0_I4P) return
  is_new = .not.h5_link_exists(self%group, data_name)
  if (self%topology == TOPOLOGY_UNSTRUCTURED .and. self%group_name /= 'FieldData') then
    if (is_new .and. self%n_pieces > 1_I4P) then
      error = refuse('the array '//data_name//' is not in the first piece: every piece writes the same arrays')
      return
    endif
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
  !< Write `n` values (I1P) as the dataset `data_name` of the open group: in an UnstructuredGrid, created by
  !< the first piece and appended to by the others.
  class(vtkhdf_writer), intent(inout) :: self         !< Writer.
  character(*), intent(in)            :: data_name    !< Data name.
  integer(I1P), intent(in)            :: x(*)         !< Values.
  integer(I8P), intent(in)            :: n            !< Number of values.
  integer(I4P), intent(in)            :: n_components !< Number of components.
  logical, intent(in), optional       :: unsigned     !< Unsigned values.
  integer(I4P)                        :: error        !< Error status.
  integer(I8P), allocatable           :: dims(:)      !< Shape of the dataset, Fortran order.
  logical                             :: is_new       !< The dataset is created (not appended to).

  error = self%dataset_dims(data_name, n, n_components, dims)
  if (error /= 0_I4P) return
  is_new = .not.h5_link_exists(self%group, data_name)
  if (self%topology == TOPOLOGY_UNSTRUCTURED .and. self%group_name /= 'FieldData') then
    if (is_new .and. self%n_pieces > 1_I4P) then
      error = refuse('the array '//data_name//' is not in the first piece: every piece writes the same arrays')
      return
    endif
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
  !< Create the appendable dataset `name` (R8P) with `x`, or append `x` to it if it exists.
  class(vtkhdf_writer), intent(in) :: self    !< Writer.
  integer(hid_kind), intent(in)    :: loc_id  !< Parent group.
  character(*), intent(in)         :: name    !< Dataset name.
  real(R8P), intent(in)            :: x(*)    !< Values.
  integer(I8P), intent(in)         :: dims(:) !< Shape of the values, Fortran order (the last dimension grows).
  integer(I4P)                     :: error   !< Error status.

  if (h5_link_exists(loc_id, name)) then
    error = h5_dataset_append_R8P(loc_id, name, x, dims)
  else
    error = h5_dataset_write_R8P(loc_id, name, x, dims, appendable=.true., deflate=self%deflate_level())
  endif
  endfunction store_R8P

  function store_R4P(self, loc_id, name, x, dims) result(error)
  !< Create the appendable dataset `name` (R4P) with `x`, or append `x` to it if it exists.
  class(vtkhdf_writer), intent(in) :: self    !< Writer.
  integer(hid_kind), intent(in)    :: loc_id  !< Parent group.
  character(*), intent(in)         :: name    !< Dataset name.
  real(R4P), intent(in)            :: x(*)    !< Values.
  integer(I8P), intent(in)         :: dims(:) !< Shape of the values, Fortran order (the last dimension grows).
  integer(I4P)                     :: error   !< Error status.

  if (h5_link_exists(loc_id, name)) then
    error = h5_dataset_append_R4P(loc_id, name, x, dims)
  else
    error = h5_dataset_write_R4P(loc_id, name, x, dims, appendable=.true., deflate=self%deflate_level())
  endif
  endfunction store_R4P

  function store_I8P(self, loc_id, name, x, dims, unsigned) result(error)
  !< Create the appendable dataset `name` (I8P) with `x`, or append `x` to it if it exists.
  class(vtkhdf_writer), intent(in) :: self     !< Writer.
  integer(hid_kind), intent(in)    :: loc_id   !< Parent group.
  character(*), intent(in)         :: name     !< Dataset name.
  integer(I8P), intent(in)         :: x(*)     !< Values.
  integer(I8P), intent(in)         :: dims(:)  !< Shape of the values, Fortran order (the last dimension grows).
  logical, intent(in), optional    :: unsigned !< Unsigned values.
  integer(I4P)                     :: error    !< Error status.

  if (h5_link_exists(loc_id, name)) then
    error = h5_dataset_append_I8P(loc_id, name, x, dims, unsigned=unsigned)
  else
    error = h5_dataset_write_I8P(loc_id, name, x, dims, appendable=.true., deflate=self%deflate_level(), unsigned=unsigned)
  endif
  endfunction store_I8P

  function store_I4P(self, loc_id, name, x, dims, unsigned) result(error)
  !< Create the appendable dataset `name` (I4P) with `x`, or append `x` to it if it exists.
  class(vtkhdf_writer), intent(in) :: self     !< Writer.
  integer(hid_kind), intent(in)    :: loc_id   !< Parent group.
  character(*), intent(in)         :: name     !< Dataset name.
  integer(I4P), intent(in)         :: x(*)     !< Values.
  integer(I8P), intent(in)         :: dims(:)  !< Shape of the values, Fortran order (the last dimension grows).
  logical, intent(in), optional    :: unsigned !< Unsigned values.
  integer(I4P)                     :: error    !< Error status.

  if (h5_link_exists(loc_id, name)) then
    error = h5_dataset_append_I4P(loc_id, name, x, dims, unsigned=unsigned)
  else
    error = h5_dataset_write_I4P(loc_id, name, x, dims, appendable=.true., deflate=self%deflate_level(), unsigned=unsigned)
  endif
  endfunction store_I4P

  function store_I2P(self, loc_id, name, x, dims, unsigned) result(error)
  !< Create the appendable dataset `name` (I2P) with `x`, or append `x` to it if it exists.
  class(vtkhdf_writer), intent(in) :: self     !< Writer.
  integer(hid_kind), intent(in)    :: loc_id   !< Parent group.
  character(*), intent(in)         :: name     !< Dataset name.
  integer(I2P), intent(in)         :: x(*)     !< Values.
  integer(I8P), intent(in)         :: dims(:)  !< Shape of the values, Fortran order (the last dimension grows).
  logical, intent(in), optional    :: unsigned !< Unsigned values.
  integer(I4P)                     :: error    !< Error status.

  if (h5_link_exists(loc_id, name)) then
    error = h5_dataset_append_I2P(loc_id, name, x, dims, unsigned=unsigned)
  else
    error = h5_dataset_write_I2P(loc_id, name, x, dims, appendable=.true., deflate=self%deflate_level(), unsigned=unsigned)
  endif
  endfunction store_I2P

  function store_I1P(self, loc_id, name, x, dims, unsigned) result(error)
  !< Create the appendable dataset `name` (I1P) with `x`, or append `x` to it if it exists.
  class(vtkhdf_writer), intent(in) :: self     !< Writer.
  integer(hid_kind), intent(in)    :: loc_id   !< Parent group.
  character(*), intent(in)         :: name     !< Dataset name.
  integer(I1P), intent(in)         :: x(*)     !< Values.
  integer(I8P), intent(in)         :: dims(:)  !< Shape of the values, Fortran order (the last dimension grows).
  logical, intent(in), optional    :: unsigned !< Unsigned values.
  integer(I4P)                     :: error    !< Error status.

  if (h5_link_exists(loc_id, name)) then
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
  if (error /= 0_I4P) return
  n_ids = 0_I8P
  if (nc > 0_I8P) n_ids = int(offset(nc), I8P)
  if (n_ids < 0_I8P .or. n_ids > size(connectivity, kind=I8P)) then
    error = refuse('write_connectivity: the last offset is not within the connectivity')
    return
  endif
  allocate(offsets(nc+1_I8P))
  offsets(1) = 0_I4P
  if (nc > 0_I8P) offsets(2:) = offset(1:nc)
  error = store_I4P(self, self%root, 'Connectivity', connectivity, [n_ids])
  if (error == 0_I4P) error = store_I4P(self, self%root, 'Offsets', offsets, [nc+1_I8P])
  if (error == 0_I4P) error = store_I1P(self, self%root, 'Types', cell_type, [nc], unsigned=.true.)
  if (error == 0_I4P) error = store_I8P(self, self%root, 'NumberOfConnectivityIds', [n_ids], [1_I8P])
  if (error /= 0_I4P) error = refuse('cannot write the cells')
  if (error == 0_I4P) self%is_connectivity_written = .true.
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
  if (error /= 0_I4P) return
  n_ids = 0_I8P
  if (nc > 0_I8P) n_ids = int(offset(nc), I8P)
  if (n_ids < 0_I8P .or. n_ids > size(connectivity, kind=I8P)) then
    error = refuse('write_connectivity: the last offset is not within the connectivity')
    return
  endif
  allocate(offsets(nc+1_I8P))
  offsets(1) = 0_I8P
  if (nc > 0_I8P) offsets(2:) = offset(1:nc)
  error = store_I8P(self, self%root, 'Connectivity', connectivity, [n_ids])
  if (error == 0_I4P) error = store_I8P(self, self%root, 'Offsets', offsets, [nc+1_I8P])
  if (error == 0_I4P) error = store_I1P(self, self%root, 'Types', cell_type, [nc], unsigned=.true.)
  if (error == 0_I4P) error = store_I8P(self, self%root, 'NumberOfConnectivityIds', [n_ids], [1_I8P])
  if (error /= 0_I4P) error = refuse('cannot write the cells')
  if (error == 0_I4P) self%is_connectivity_written = .true.
  endfunction put_cells_I8P

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
  !< Check a write_geo call: an UnstructuredGrid piece open (no data group), its points once, sizes.
  class(vtkhdf_writer), intent(in) :: self  !< Writer.
  integer(I8P),         intent(in) :: np    !< Number of points.
  integer(I8P),         intent(in) :: nc    !< Number of cells.
  integer(I8P),         intent(in) :: n1    !< First dimension of the coordinates.
  integer(I8P),         intent(in) :: n2    !< Second dimension of the coordinates.
  integer(I4P)                     :: error !< Error status.

  error = 1_I4P
  if (self%topology /= TOPOLOGY_UNSTRUCTURED) then
    error = refuse('write_geo(np, nc, ...) is for an UnstructuredGrid (an ImageData has no geometry)')
  elseif (.not.self%is_piece_open .or. self%group >= 0_hid_kind) then
    error = refuse('write_geo: open the piece first, outside the point and cell data')
  elseif (self%is_geo_written) then
    error = refuse('write_geo: the points of this piece are already written')
  elseif (np /= self%np_piece .or. nc /= self%nc_piece) then
    error = refuse('write_geo: np and nc are not those of the piece')
  elseif (n1 /= 3_I8P .or. n2 /= np) then
    error = refuse('write_geo: the coordinates are not 3 by np')
  else
    error = 0_I4P
  endif
  endfunction check_geo

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
  !< for cell data. The axes of one point (a 2D or 1D grid) are dropped, as VTK writes and requires (VTK 9.7 reads neither a
  !< size-1 axis nor a flat array), and so is the first dimension with one component. Field data are
  !< `[n_components, n_tuples]`. In an UnstructuredGrid, the tuples are the points or cells of the piece, `[n_components, n]`.
  class(vtkhdf_writer),      intent(in)  :: self         !< Writer.
  character(*),              intent(in)  :: data_name    !< Data name (for the messages).
  integer(I8P),              intent(in)  :: n            !< Number of values.
  integer(I4P),              intent(in)  :: n_components !< Number of components.
  integer(I8P), allocatable, intent(out) :: dims(:)      !< Shape of the dataset.
  integer(I4P)                           :: error        !< Error status.
  integer(I8P)                           :: grid(3)      !< Points or cells along each axis.
  logical                                :: is_axis(3)   !< The axis has more than one point.

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
  elseif (self%topology == TOPOLOGY_UNSTRUCTURED) then
    grid = [self%np_piece, 1_I8P, 1_I8P]
    if (self%group_name == 'CellData') grid(1) = self%nc_piece
    is_axis = [.true., .false., .false.]
    if (n /= grid(1) * n_components) then
      error = refuse('the array '//data_name//' has '//trim(str(n=n, no_sign=.true.))//' values, the piece needs '// &
                     trim(str(n=grid(1) * n_components, no_sign=.true.))//' ('//                                   &
                     trim(str(n=n_components, no_sign=.true.))//' components)')
      return
    endif
  else
    grid = int(self%whole_extent(2::2) - self%whole_extent(1::2), I8P) + 1_I8P
    is_axis = grid > 1_I8P
    if (.not.any(is_axis)) is_axis(1) = .true. ! one point: one value
    if (self%group_name == 'CellData') grid = max(grid - 1_I8P, 1_I8P)
    if (n /= product(grid) * n_components) then
      error = refuse('the array '//data_name//' has '//trim(str(n=n, no_sign=.true.))//' values, the grid needs '// &
                     trim(str(n=product(grid) * n_components, no_sign=.true.))//' ('//                              &
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
  !< End a piece of an UnstructuredGrid: its points and cells written, every array of the first piece written once.
  class(vtkhdf_writer), intent(inout) :: self  !< Writer.
  integer(I4P)                        :: error !< Error status.

  error = 0_I4P
  if (.not.(self%is_geo_written .and. self%is_connectivity_written)) then
    error = refuse('a piece of an UnstructuredGrid needs write_geo and write_connectivity')
    return
  endif
  ! the root passed, not read from the host: nvfortran internal procedures can read stale host values
  error = check_arrays(self%root, 'PointData', self%point_arrays, self%total_points)
  if (error == 0_I4P) error = check_arrays(self%root, 'CellData', self%cell_arrays, self%total_cells)
  contains
    function check_arrays(root, group, names, total) result(error)
    !< Check that the tuples of every array of the list are the tuples of the pieces written.
    integer(hid_kind), intent(in) :: root !< Group /VTKHDF.
    character(*), intent(in)  :: group  !< Group of the arrays.
    character(*), intent(in)  :: names  !< Names, each followed by NAMES_SEPARATOR.
    integer(I8P), intent(in)  :: total  !< Tuples of the pieces written.
    integer(I4P)              :: error  !< Error status.
    integer(I8P), allocatable :: dims(:) !< Shape of a dataset, Fortran order.
    integer(I8P)              :: first   !< First character of a name.
    integer(I8P)              :: last    !< Separator after a name.

    error = 0_I4P
    first = 1_I8P
    do while (first <= len(names, kind=I8P))
      last = first - 1_I8P + index(names(first:), NAMES_SEPARATOR, kind=I8P)
      error = h5_dataset_dims(root, group//'/'//names(first:last-1), dims)
      if (error == 0_I4P) then
        if (dims(size(dims)) /= total) error = 1_I4P
      endif
      if (error /= 0_I4P) then
        error = refuse('the array '//names(first:last-1)//' of '//group//' is not written once in every piece')
        return
      endif
      first = last + 1_I8P
    enddo
    endfunction check_arrays
  endfunction end_partition

  pure function deflate_level(self) result(level)
  !< Deflate level of the datasets: 0 (none) unless compressed.
  class(vtkhdf_writer), intent(in) :: self  !< Writer.
  integer(I4P)                     :: level !< Deflate level.

  level = 0_I4P
  if (self%is_compressed) level = DEFLATE_LEVEL_ZLIB
  endfunction deflate_level

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

  subroutine record_array(self, data_name)
  !< Record the name of an array of the open point or cell data group (UnstructuredGrid, checked at the end of the pieces).
  class(vtkhdf_writer), intent(inout) :: self      !< Writer.
  character(*),         intent(in)    :: data_name !< Data name.
  character(len=:), allocatable       :: names     !< Names, updated (not an assignment of a component to itself).

  if (self%topology /= TOPOLOGY_UNSTRUCTURED) return
  if (self%group_name == 'PointData') then
    names = self%point_arrays//data_name//NAMES_SEPARATOR
    self%point_arrays = names
  elseif (self%group_name == 'CellData') then
    names = self%cell_arrays//data_name//NAMES_SEPARATOR
    self%cell_arrays = names
  endif
  endsubroutine record_array

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
  self%total_points = 0_I8P
  self%total_cells = 0_I8P
  self%is_geo_written = .false.
  self%is_connectivity_written = .false.
  self%point_arrays = ''
  self%cell_arrays = ''
  do r=1, size(ROLES)
    if (allocated(self%active(r)%name)) deallocate(self%active(r)%name)
  enddo
  endsubroutine reset

  function start_partition(self, np, nc) result(error)
  !< Start a piece of an UnstructuredGrid: a partition, its counts appended to NumberOfPoints and NumberOfCells.
  class(vtkhdf_writer), intent(inout) :: self  !< Writer.
  integer(I8P),         intent(in)    :: np    !< Number of points.
  integer(I8P),         intent(in)    :: nc    !< Number of cells.
  integer(I4P)                        :: error !< Error status.

  error = 1_I4P
  if (self%topology /= TOPOLOGY_UNSTRUCTURED) then
    error = refuse('write_piece(np, nc) is for an UnstructuredGrid: an ImageData piece is write_piece(nx1, ..., nz2)')
    return
  endif
  if (self%file_id < 0_hid_kind .or. self%is_piece_open .or. self%group >= 0_hid_kind) return
  if (np < 0_I8P .or. nc < 0_I8P) then
    error = refuse('write_piece: negative counts')
    return
  endif
  self%n_pieces = self%n_pieces + 1_I4P
  self%np_piece = np
  self%nc_piece = nc
  self%total_points = self%total_points + np
  self%total_cells = self%total_cells + nc
  self%is_geo_written = .false.
  self%is_connectivity_written = .false.
  error = store_I8P(self, self%root, 'NumberOfPoints', [np], [1_I8P])
  if (error == 0_I4P) error = store_I8P(self, self%root, 'NumberOfCells', [nc], [1_I8P])
  if (error /= 0_I4P) then
    error = refuse('cannot write the counts of the piece')
    return
  endif
  self%is_piece_open = .true.
  endfunction start_partition

  ! non type-bound helpers
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
