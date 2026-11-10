!< VTK file VTKHDF writer: an HDF5 file in the VTKHDF layout (ImageData).
module vtk_fortran_vtk_file_vtkhdf_writer
!< VTK file VTKHDF writer: an HDF5 file in the VTKHDF layout (ImageData).
!<
!< The writer of `vtk_file%initialize(format='VTKHDF', ...)`: the same calls as the XML formats write the group `/VTKHDF`
!< of an HDF5 file (VTKHDF specification 2.x, read by VTK >= 9.1 and ParaView): the attributes `Version`, `Type`,
!< `WholeExtent`, `Origin`, `Spacing`, `Direction`, the groups `PointData`, `CellData` and `FieldData`, one dataset per
!< data array. A Fortran array `x(n_components, i, j, k)` is stored as it is in memory: it is the VTKHDF dataset of shape
!< `[k, j, i, n_components]` (C order), so nothing is transposed. With `compressor='zlib'` the datasets are deflated.
!<
!< Supported: ImageData, one piece (its extent is the whole extent), data arrays of every kind and layout, unsigned
!< arrays, active arrays (`Attribute`, VTKHDF 2.6), numeric field data. Refused (non-zero error, message on stderr):
!< other topologies (not yet), a volatile file, string field data (the VTK reader crashes on them), zlib without the
!< HDF5 deflate filter, and every call when the library is built without HDF5 (`VTKFORTRAN_USE_HDF5`).
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
    procedure, pass(self) :: write_connectivity_I4P
    procedure, pass(self) :: write_connectivity_I8P
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
    procedure, pass(self) :: write_geo_unst_data1_rank2_R8P
    procedure, pass(self) :: write_geo_unst_data1_rank2_R4P
    procedure, pass(self) :: write_geo_unst_data3_rank1_R8P
    procedure, pass(self) :: write_geo_unst_data3_rank1_R4P
    procedure, pass(self) :: write_geo_unst_data1_rank2_R8P_I8P
    procedure, pass(self) :: write_geo_unst_data1_rank2_R4P_I8P
    procedure, pass(self) :: write_geo_unst_data3_rank1_R8P_I8P
    procedure, pass(self) :: write_geo_unst_data3_rank1_R4P_I8P
    procedure, pass(self) :: write_piece_start_tag_unst
    procedure, pass(self) :: write_piece_start_tag_poly
    procedure, pass(self) :: write_piece_start_tag_unst_I8P
    procedure, pass(self) :: write_piece_start_tag_poly_I8P
    procedure, pass(self) :: write_polydata_cells
    ! private helpers
    procedure, pass(self), private :: close_group   !< Close the open data group.
    procedure, pass(self), private :: dataset_dims  !< Shape of the dataset of a data array.
    procedure, pass(self), private :: deflate_level !< Deflate level of the datasets.
    procedure, pass(self), private :: mark_active   !< Mark a dataset as active array.
    procedure, pass(self), private :: open_group    !< Open (or create) a data group.
    procedure, pass(self), private :: reset         !< Reset the state of the writer.
endtype vtkhdf_writer

contains
  function initialize(self, format, filename, mesh_topology, nx1, nx2, ny1, ny2, nz1, nz2, &
      is_volatile, mesh_kind) result(error)
  !< Initialize the writer: create the file and write the ImageData header.
  !<
  !< `format` is VTKHDF (not checked here), `mesh_topology` must be ImageData, with its whole extent `nx1`...`nz2`
  !< (origin, spacing and direction are set by `vtk_file%initialize`). `is_volatile` is refused, `mesh_kind` unused.
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
  if (trim(adjustl(mesh_topology)) /= 'ImageData') then
    error = refuse('topology '//trim(adjustl(mesh_topology))//': only ImageData is supported yet')
    return
  endif
  if (.not.(present(nx1).and.present(nx2).and.present(ny1).and.present(ny2).and.present(nz1).and.present(nz2))) then
    error = refuse('ImageData needs its whole extent: nx1, nx2, ny1, ny2, nz1, nz2')
    return
  endif
  self%whole_extent = [nx1, nx2, ny1, ny2, nz1, nz2]
  if (any(self%whole_extent(2::2) < self%whole_extent(1::2))) then
    error = refuse('the whole extent is empty: each final node must not precede the initial one')
    return
  endif
  if (self%is_compressed .and. .not.h5_deflate_available()) then
    error = refuse('compressor zlib: the HDF5 library has no deflate filter')
    return
  endif
  error = h5_file_create(filename, self%file_id)
  if (error /= 0_I4P) error = refuse('cannot create '//filename)
  if (error == 0_I4P) error = h5_group_create(self%file_id, 'VTKHDF', self%root)
  if (error == 0_I4P) error = h5_attribute_write(self%root, 'Type', 'ImageData')
  if (error == 0_I4P) error = h5_attribute_write(self%root, 'WholeExtent', int(self%whole_extent, I8P))
  if (error == 0_I4P) error = h5_attribute_write(self%root, 'Origin', self%origin)
  if (error == 0_I4P) error = h5_attribute_write(self%root, 'Spacing', self%spacing)
  if (error == 0_I4P) error = h5_attribute_write(self%root, 'Direction', self%direction)
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
  if (self%n_pieces > 0_I4P) then
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
  !< End the piece.
  class(vtkhdf_writer),       intent(inout) :: self
  integer(I4P)                              :: error

  error = 1_I4P
  if (.not.self%is_piece_open .or. self%group >= 0_hid_kind) return
  self%is_piece_open = .false.
  error = 0_I4P
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

  function write_connectivity_I4P(self, nc, connectivity, offset, cell_type, face, faceoffset) result(error)
  !< Not supported yet by the VTKHDF writer (other topologies than ImageData): an error.
  class(vtkhdf_writer),       intent(inout) :: self
  integer(I4P),               intent(in)    :: nc
  integer(I4P),               intent(in)    :: connectivity(1:)
  integer(I4P),               intent(in)    :: offset(1:)
  integer(I4P),   optional,   intent(in)    :: face(1:)
  integer(I4P),   optional,   intent(in)    :: faceoffset(1:)
  integer(I1P),               intent(in)    :: cell_type(1:)
  integer(I4P)                              :: error

  error = refuse('write_connectivity of this topology: only ImageData is supported yet')
  endfunction write_connectivity_I4P

  function write_connectivity_I8P(self, nc, connectivity, offset, cell_type, face, faceoffset) result(error)
  !< Not supported yet by the VTKHDF writer (other topologies than ImageData): an error.
  class(vtkhdf_writer),       intent(inout) :: self
  integer(I8P),               intent(in)    :: nc
  integer(I8P),               intent(in)    :: connectivity(1:)
  integer(I8P),               intent(in)    :: offset(1:)
  integer(I8P),   optional,   intent(in)    :: face(1:)
  integer(I8P),   optional,   intent(in)    :: faceoffset(1:)
  integer(I1P),               intent(in)    :: cell_type(1:)
  integer(I4P)                              :: error

  error = refuse('write_connectivity of this topology: only ImageData is supported yet')
  endfunction write_connectivity_I8P

  function write_geo_strg_data1_rank2_R8P(self, xyz) result(error)
  !< Not supported yet by the VTKHDF writer (other topologies than ImageData): an error.
  class(vtkhdf_writer),       intent(inout) :: self
  real(R8P),                  intent(in)    :: xyz(1:,1:)
  integer(I4P)                              :: error

  error = refuse('write_geo of this topology: only ImageData is supported yet')
  endfunction write_geo_strg_data1_rank2_R8P

  function write_geo_strg_data1_rank2_R4P(self, xyz) result(error)
  !< Not supported yet by the VTKHDF writer (other topologies than ImageData): an error.
  class(vtkhdf_writer),       intent(inout) :: self
  real(R4P),                  intent(in)    :: xyz(1:,1:)
  integer(I4P)                              :: error

  error = refuse('write_geo of this topology: only ImageData is supported yet')
  endfunction write_geo_strg_data1_rank2_R4P

  function write_geo_strg_data1_rank4_R8P(self, xyz) result(error)
  !< Not supported yet by the VTKHDF writer (other topologies than ImageData): an error.
  class(vtkhdf_writer),       intent(inout) :: self
  real(R8P),                  intent(in)    :: xyz(1:,1:,1:,1:)
  integer(I4P)                              :: error

  error = refuse('write_geo of this topology: only ImageData is supported yet')
  endfunction write_geo_strg_data1_rank4_R8P

  function write_geo_strg_data1_rank4_R4P(self, xyz) result(error)
  !< Not supported yet by the VTKHDF writer (other topologies than ImageData): an error.
  class(vtkhdf_writer),       intent(inout) :: self
  real(R4P),                  intent(in)    :: xyz(1:,1:,1:,1:)
  integer(I4P)                              :: error

  error = refuse('write_geo of this topology: only ImageData is supported yet')
  endfunction write_geo_strg_data1_rank4_R4P

  function write_geo_strg_data3_rank1_R8P(self, n, x, y, z) result(error)
  !< Not supported yet by the VTKHDF writer (other topologies than ImageData): an error.
  class(vtkhdf_writer),       intent(inout) :: self
  integer(I4P),               intent(in)    :: n
  real(R8P),                  intent(in)    :: x(1:)
  real(R8P),                  intent(in)    :: y(1:)
  real(R8P),                  intent(in)    :: z(1:)
  integer(I4P)                              :: error

  error = refuse('write_geo of this topology: only ImageData is supported yet')
  endfunction write_geo_strg_data3_rank1_R8P

  function write_geo_strg_data3_rank1_R4P(self, n, x, y, z) result(error)
  !< Not supported yet by the VTKHDF writer (other topologies than ImageData): an error.
  class(vtkhdf_writer),       intent(inout) :: self
  integer(I4P),               intent(in)    :: n
  real(R4P),                  intent(in)    :: x(1:)
  real(R4P),                  intent(in)    :: y(1:)
  real(R4P),                  intent(in)    :: z(1:)
  integer(I4P)                              :: error

  error = refuse('write_geo of this topology: only ImageData is supported yet')
  endfunction write_geo_strg_data3_rank1_R4P

  function write_geo_strg_data3_rank3_R8P(self, n, x, y, z) result(error)
  !< Not supported yet by the VTKHDF writer (other topologies than ImageData): an error.
  class(vtkhdf_writer),       intent(inout) :: self
  integer(I4P),               intent(in)    :: n
  real(R8P),                  intent(in)    :: x(1:,1:,1:)
  real(R8P),                  intent(in)    :: y(1:,1:,1:)
  real(R8P),                  intent(in)    :: z(1:,1:,1:)
  integer(I4P)                              :: error

  error = refuse('write_geo of this topology: only ImageData is supported yet')
  endfunction write_geo_strg_data3_rank3_R8P

  function write_geo_strg_data3_rank3_R4P(self, n, x, y, z) result(error)
  !< Not supported yet by the VTKHDF writer (other topologies than ImageData): an error.
  class(vtkhdf_writer),       intent(inout) :: self
  integer(I4P),               intent(in)    :: n
  real(R4P),                  intent(in)    :: x(1:,1:,1:)
  real(R4P),                  intent(in)    :: y(1:,1:,1:)
  real(R4P),                  intent(in)    :: z(1:,1:,1:)
  integer(I4P)                              :: error

  error = refuse('write_geo of this topology: only ImageData is supported yet')
  endfunction write_geo_strg_data3_rank3_R4P

  function write_geo_rect_data3_rank1_R8P(self, x, y, z) result(error)
  !< Not supported yet by the VTKHDF writer (other topologies than ImageData): an error.
  class(vtkhdf_writer),       intent(inout) :: self
  real(R8P),                  intent(in)    :: x(1:)
  real(R8P),                  intent(in)    :: y(1:)
  real(R8P),                  intent(in)    :: z(1:)
  integer(I4P)                              :: error

  error = refuse('write_geo of this topology: only ImageData is supported yet')
  endfunction write_geo_rect_data3_rank1_R8P

  function write_geo_rect_data3_rank1_R4P(self, x, y, z) result(error)
  !< Not supported yet by the VTKHDF writer (other topologies than ImageData): an error.
  class(vtkhdf_writer),       intent(inout) :: self
  real(R4P),                  intent(in)    :: x(1:)
  real(R4P),                  intent(in)    :: y(1:)
  real(R4P),                  intent(in)    :: z(1:)
  integer(I4P)                              :: error

  error = refuse('write_geo of this topology: only ImageData is supported yet')
  endfunction write_geo_rect_data3_rank1_R4P

  function write_geo_unst_data1_rank2_R8P(self, np, nc, xyz) result(error)
  !< Not supported yet by the VTKHDF writer (other topologies than ImageData): an error.
  class(vtkhdf_writer),       intent(inout) :: self
  integer(I4P),               intent(in)    :: np
  integer(I4P),               intent(in)    :: nc
  real(R8P),                  intent(in)    :: xyz(1:,1:)
  integer(I4P)                              :: error

  error = refuse('write_geo of this topology: only ImageData is supported yet')
  endfunction write_geo_unst_data1_rank2_R8P

  function write_geo_unst_data1_rank2_R4P(self, np, nc, xyz) result(error)
  !< Not supported yet by the VTKHDF writer (other topologies than ImageData): an error.
  class(vtkhdf_writer),       intent(inout) :: self
  integer(I4P),               intent(in)    :: np
  integer(I4P),               intent(in)    :: nc
  real(R4P),                  intent(in)    :: xyz(1:,1:)
  integer(I4P)                              :: error

  error = refuse('write_geo of this topology: only ImageData is supported yet')
  endfunction write_geo_unst_data1_rank2_R4P

  function write_geo_unst_data3_rank1_R8P(self, np, nc, x, y, z) result(error)
  !< Not supported yet by the VTKHDF writer (other topologies than ImageData): an error.
  class(vtkhdf_writer),       intent(inout) :: self
  integer(I4P),               intent(in)    :: np
  integer(I4P),               intent(in)    :: nc
  real(R8P),                  intent(in)    :: x(1:)
  real(R8P),                  intent(in)    :: y(1:)
  real(R8P),                  intent(in)    :: z(1:)
  integer(I4P)                              :: error

  error = refuse('write_geo of this topology: only ImageData is supported yet')
  endfunction write_geo_unst_data3_rank1_R8P

  function write_geo_unst_data3_rank1_R4P(self, np, nc, x, y, z) result(error)
  !< Not supported yet by the VTKHDF writer (other topologies than ImageData): an error.
  class(vtkhdf_writer),       intent(inout) :: self
  integer(I4P),               intent(in)    :: np
  integer(I4P),               intent(in)    :: nc
  real(R4P),                  intent(in)    :: x(1:)
  real(R4P),                  intent(in)    :: y(1:)
  real(R4P),                  intent(in)    :: z(1:)
  integer(I4P)                              :: error

  error = refuse('write_geo of this topology: only ImageData is supported yet')
  endfunction write_geo_unst_data3_rank1_R4P

  function write_geo_unst_data1_rank2_R8P_I8P(self, np, nc, xyz) result(error)
  !< Not supported yet by the VTKHDF writer (other topologies than ImageData): an error.
  class(vtkhdf_writer),       intent(inout) :: self
  integer(I8P),               intent(in)    :: np
  integer(I8P),               intent(in)    :: nc
  real(R8P),                  intent(in)    :: xyz(1:,1:)
  integer(I4P)                              :: error

  error = refuse('write_geo of this topology: only ImageData is supported yet')
  endfunction write_geo_unst_data1_rank2_R8P_I8P

  function write_geo_unst_data1_rank2_R4P_I8P(self, np, nc, xyz) result(error)
  !< Not supported yet by the VTKHDF writer (other topologies than ImageData): an error.
  class(vtkhdf_writer),       intent(inout) :: self
  integer(I8P),               intent(in)    :: np
  integer(I8P),               intent(in)    :: nc
  real(R4P),                  intent(in)    :: xyz(1:,1:)
  integer(I4P)                              :: error

  error = refuse('write_geo of this topology: only ImageData is supported yet')
  endfunction write_geo_unst_data1_rank2_R4P_I8P

  function write_geo_unst_data3_rank1_R8P_I8P(self, np, nc, x, y, z) result(error)
  !< Not supported yet by the VTKHDF writer (other topologies than ImageData): an error.
  class(vtkhdf_writer),       intent(inout) :: self
  integer(I8P),               intent(in)    :: np
  integer(I8P),               intent(in)    :: nc
  real(R8P),                  intent(in)    :: x(1:)
  real(R8P),                  intent(in)    :: y(1:)
  real(R8P),                  intent(in)    :: z(1:)
  integer(I4P)                              :: error

  error = refuse('write_geo of this topology: only ImageData is supported yet')
  endfunction write_geo_unst_data3_rank1_R8P_I8P

  function write_geo_unst_data3_rank1_R4P_I8P(self, np, nc, x, y, z) result(error)
  !< Not supported yet by the VTKHDF writer (other topologies than ImageData): an error.
  class(vtkhdf_writer),       intent(inout) :: self
  integer(I8P),               intent(in)    :: np
  integer(I8P),               intent(in)    :: nc
  real(R4P),                  intent(in)    :: x(1:)
  real(R4P),                  intent(in)    :: y(1:)
  real(R4P),                  intent(in)    :: z(1:)
  integer(I4P)                              :: error

  error = refuse('write_geo of this topology: only ImageData is supported yet')
  endfunction write_geo_unst_data3_rank1_R4P_I8P

  function write_piece_start_tag_unst(self, np, nc) result(error)
  !< Not supported yet by the VTKHDF writer (other topologies than ImageData): an error.
  class(vtkhdf_writer),       intent(inout) :: self
  integer(I4P),               intent(in)    :: np
  integer(I4P),               intent(in)    :: nc
  integer(I4P)                              :: error

  error = refuse('write_piece of this topology: only ImageData is supported yet')
  endfunction write_piece_start_tag_unst

  function write_piece_start_tag_poly(self, np, nverts, nlines, nstrips, npolys) result(error)
  !< Not supported yet by the VTKHDF writer (other topologies than ImageData): an error.
  class(vtkhdf_writer),       intent(inout) :: self
  integer(I4P),               intent(in)    :: np
  integer(I4P),               intent(in)    :: nverts
  integer(I4P),               intent(in)    :: nlines
  integer(I4P),               intent(in)    :: nstrips
  integer(I4P),               intent(in)    :: npolys
  integer(I4P)                              :: error

  error = refuse('write_piece of this topology: only ImageData is supported yet')
  endfunction write_piece_start_tag_poly

  function write_piece_start_tag_unst_I8P(self, np, nc) result(error)
  !< Not supported yet by the VTKHDF writer (other topologies than ImageData): an error.
  class(vtkhdf_writer),       intent(inout) :: self
  integer(I8P),               intent(in)    :: np
  integer(I8P),               intent(in)    :: nc
  integer(I4P)                              :: error

  error = refuse('write_piece of this topology: only ImageData is supported yet')
  endfunction write_piece_start_tag_unst_I8P

  function write_piece_start_tag_poly_I8P(self, np, nverts, nlines, nstrips, npolys) result(error)
  !< Not supported yet by the VTKHDF writer (other topologies than ImageData): an error.
  class(vtkhdf_writer),       intent(inout) :: self
  integer(I8P),               intent(in)    :: np
  integer(I8P),               intent(in)    :: nverts
  integer(I8P),               intent(in)    :: nlines
  integer(I8P),               intent(in)    :: nstrips
  integer(I8P),               intent(in)    :: npolys
  integer(I4P)                              :: error

  error = refuse('write_piece of this topology: only ImageData is supported yet')
  endfunction write_piece_start_tag_poly_I8P

  function write_polydata_cells(self, verts_connectivity, verts_offset, lines_connectivity, lines_offset, &
      strips_connectivity, strips_offset, polys_connectivity, polys_offset) result(error)
  !< Not supported yet by the VTKHDF writer (other topologies than ImageData): an error.
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

  error = refuse('write_polydata_cells of this topology: only ImageData is supported yet')
  endfunction write_polydata_cells

  ! private helpers
  function put_R8P(self, data_name, x, n, n_components) result(error)
  !< Write `n` values (R8P) as the dataset `data_name` of the open group.
  class(vtkhdf_writer), intent(inout) :: self         !< Writer.
  character(*), intent(in)            :: data_name    !< Data name.
  real(R8P), intent(in)               :: x(*)         !< Values.
  integer(I8P), intent(in)            :: n            !< Number of values.
  integer(I4P), intent(in)            :: n_components !< Number of components.
  integer(I4P)                        :: error        !< Error status.
  integer(I8P), allocatable           :: dims(:)      !< Shape of the dataset, Fortran order.

  error = self%dataset_dims(data_name, n, n_components, dims)
  if (error /= 0_I4P) return
  error = h5_dataset_write_R8P(self%group, data_name, x, dims, deflate=self%deflate_level())
  if (error /= 0_I4P) error = refuse('cannot write the dataset '//data_name)
  if (error == 0_I4P) error = self%mark_active(data_name)
  endfunction put_R8P

  function put_R4P(self, data_name, x, n, n_components) result(error)
  !< Write `n` values (R4P) as the dataset `data_name` of the open group.
  class(vtkhdf_writer), intent(inout) :: self         !< Writer.
  character(*), intent(in)            :: data_name    !< Data name.
  real(R4P), intent(in)               :: x(*)         !< Values.
  integer(I8P), intent(in)            :: n            !< Number of values.
  integer(I4P), intent(in)            :: n_components !< Number of components.
  integer(I4P)                        :: error        !< Error status.
  integer(I8P), allocatable           :: dims(:)      !< Shape of the dataset, Fortran order.

  error = self%dataset_dims(data_name, n, n_components, dims)
  if (error /= 0_I4P) return
  error = h5_dataset_write_R4P(self%group, data_name, x, dims, deflate=self%deflate_level())
  if (error /= 0_I4P) error = refuse('cannot write the dataset '//data_name)
  if (error == 0_I4P) error = self%mark_active(data_name)
  endfunction put_R4P

  function put_I8P(self, data_name, x, n, n_components, unsigned) result(error)
  !< Write `n` values (I8P) as the dataset `data_name` of the open group.
  class(vtkhdf_writer), intent(inout) :: self         !< Writer.
  character(*), intent(in)            :: data_name    !< Data name.
  integer(I8P), intent(in)            :: x(*)         !< Values.
  integer(I8P), intent(in)            :: n            !< Number of values.
  integer(I4P), intent(in)            :: n_components !< Number of components.
  logical, intent(in), optional       :: unsigned     !< Unsigned values.
  integer(I4P)                        :: error        !< Error status.
  integer(I8P), allocatable           :: dims(:)      !< Shape of the dataset, Fortran order.

  error = self%dataset_dims(data_name, n, n_components, dims)
  if (error /= 0_I4P) return
  error = h5_dataset_write_I8P(self%group, data_name, x, dims, deflate=self%deflate_level(), unsigned=unsigned)
  if (error /= 0_I4P) error = refuse('cannot write the dataset '//data_name)
  if (error == 0_I4P) error = self%mark_active(data_name)
  endfunction put_I8P

  function put_I4P(self, data_name, x, n, n_components, unsigned) result(error)
  !< Write `n` values (I4P) as the dataset `data_name` of the open group.
  class(vtkhdf_writer), intent(inout) :: self         !< Writer.
  character(*), intent(in)            :: data_name    !< Data name.
  integer(I4P), intent(in)            :: x(*)         !< Values.
  integer(I8P), intent(in)            :: n            !< Number of values.
  integer(I4P), intent(in)            :: n_components !< Number of components.
  logical, intent(in), optional       :: unsigned     !< Unsigned values.
  integer(I4P)                        :: error        !< Error status.
  integer(I8P), allocatable           :: dims(:)      !< Shape of the dataset, Fortran order.

  error = self%dataset_dims(data_name, n, n_components, dims)
  if (error /= 0_I4P) return
  error = h5_dataset_write_I4P(self%group, data_name, x, dims, deflate=self%deflate_level(), unsigned=unsigned)
  if (error /= 0_I4P) error = refuse('cannot write the dataset '//data_name)
  if (error == 0_I4P) error = self%mark_active(data_name)
  endfunction put_I4P

  function put_I2P(self, data_name, x, n, n_components, unsigned) result(error)
  !< Write `n` values (I2P) as the dataset `data_name` of the open group.
  class(vtkhdf_writer), intent(inout) :: self         !< Writer.
  character(*), intent(in)            :: data_name    !< Data name.
  integer(I2P), intent(in)            :: x(*)         !< Values.
  integer(I8P), intent(in)            :: n            !< Number of values.
  integer(I4P), intent(in)            :: n_components !< Number of components.
  logical, intent(in), optional       :: unsigned     !< Unsigned values.
  integer(I4P)                        :: error        !< Error status.
  integer(I8P), allocatable           :: dims(:)      !< Shape of the dataset, Fortran order.

  error = self%dataset_dims(data_name, n, n_components, dims)
  if (error /= 0_I4P) return
  error = h5_dataset_write_I2P(self%group, data_name, x, dims, deflate=self%deflate_level(), unsigned=unsigned)
  if (error /= 0_I4P) error = refuse('cannot write the dataset '//data_name)
  if (error == 0_I4P) error = self%mark_active(data_name)
  endfunction put_I2P

  function put_I1P(self, data_name, x, n, n_components, unsigned) result(error)
  !< Write `n` values (I1P) as the dataset `data_name` of the open group.
  class(vtkhdf_writer), intent(inout) :: self         !< Writer.
  character(*), intent(in)            :: data_name    !< Data name.
  integer(I1P), intent(in)            :: x(*)         !< Values.
  integer(I8P), intent(in)            :: n            !< Number of values.
  integer(I4P), intent(in)            :: n_components !< Number of components.
  logical, intent(in), optional       :: unsigned     !< Unsigned values.
  integer(I4P)                        :: error        !< Error status.
  integer(I8P), allocatable           :: dims(:)      !< Shape of the dataset, Fortran order.

  error = self%dataset_dims(data_name, n, n_components, dims)
  if (error /= 0_I4P) return
  error = h5_dataset_write_I1P(self%group, data_name, x, dims, deflate=self%deflate_level(), unsigned=unsigned)
  if (error /= 0_I4P) error = refuse('cannot write the dataset '//data_name)
  if (error == 0_I4P) error = self%mark_active(data_name)
  endfunction put_I1P

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
  !< `[n_components, n_tuples]`.
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

  elemental subroutine reset(self)
  !< Reset the state of the writer, but the HDF5 identifiers.
  class(vtkhdf_writer), intent(inout) :: self !< Writer.
  integer(I4P)                        :: r    !< Counter.

  self%group_name = ''
  self%whole_extent = 0_I4P
  self%n_pieces = 0_I4P
  self%is_piece_open = .false.
  self%version_minor = 0_I8P
  do r=1, size(ROLES)
    if (allocated(self%active(r)%name)) deallocate(self%active(r)%name)
  enddo
  endsubroutine reset

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
