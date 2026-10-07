!< Parallel (partioned) VTK file class.
module vtk_fortran_pvtk_file
!< Parallel (partioned) VTK file class.
use befor64
use penf
use vtk_fortran_vtk_file_xml_writer_abstract
use vtk_fortran_vtk_file_xml_writer_ascii_local
use vtk_fortran_vtk_file_xml_reader

implicit none
private
public :: pvtk_file

type :: pvtk_file
  !< VTK parallel (partioned) file class.
  private
  class(xml_writer_abstract), allocatable, public :: xml_writer !< XML writer.
  type(xml_reader),                        public :: xml_reader !< XML reader (`initialize(..., action='read')`).
  logical                                         :: is_reading=.false. !< The file is open for reading.
  contains
    procedure, pass(self) :: initialize !< Initialize file.
    procedure, pass(self) :: finalize   !< Finalize file.
endtype pvtk_file
contains
  function initialize(self, filename, mesh_topology, mesh_kind, nx1, nx2, ny1, ny2, nz1, nz2, ghost_level, &
                      origin, spacing, direction, action) result(error)
  !< Initialize file: open it for writing (default) or for reading.
  !<
  !< @note This function must be the first to be called.
  !<
  !<### Reading
  !<
  !< With `action='read'` only `filename` is used: the header is read through the `xml_reader` component (`get_info`,
  !< `get_sources`, `read_piece` for the extents of the pieces, `get_dataarray_names`, `get_dataarray_info` for the declared
  !< arrays, and `check_pieces` to check the pieces against the header); `finalize` frees it.
  !<
  !<```fortran
  !< type(pvtk_file)               :: pvtk
  !< character(len=:), allocatable :: sources(:), message
  !< error = pvtk%initialize(filename='mesh.pvtu', action='read')
  !< error = pvtk%xml_reader%get_sources(sources)
  !< error = pvtk%xml_reader%check_pieces(message=message)
  !< error = pvtk%finalize()
  !<```
  !<
  !<### Writing
  !<
  !< `mesh_topology` is required.
  !<
  !<### Supported topologies are:
  !<
  !<- PRectilinearGrid;
  !<- PStructuredGrid;
  !<- PUnstructuredGrid;
  !<- PImageData: `origin` and `spacing` are required, `direction` is optional, `mesh_kind` is not used (the pieces have no
  !<  points coordinates).
  !<- PPolyData: `mesh_kind` (type of the points coordinates of the pieces) is required.
  !<
  !<### Example of usage
  !<
  !<```fortran
  !< type(pvtk_file) :: pvtk
  !< integer(I4P)    :: nx1, nx2, ny1, ny2, nz1, nz2
  !< ...
  !< error = pvtk%initialize('XML_RECT_BINARY.pvtr','PRectilinearGrid',nx1=nx1,nx2=nx2,ny1=ny1,ny2=ny2,nz1=nz1,nz2=nz2)
  !< ...
  !<```
  !< @note `ghost_level` is the number of ghost levels (layers of cells shared with the neighbour pieces) of the pieces,
  !< default 0.
  !< @note The file extension is necessary in the file name. The XML standard has different extensions for each
  !< different topologies (e.g. *pvtr* for rectilinear topology). See the VTK-standard file for more information.
  class(pvtk_file), intent(inout)         :: self          !< VTK file.
  character(*),     intent(in)            :: filename      !< File name.
  character(*),     intent(in),  optional :: mesh_topology !< Mesh topology.
  character(*),     intent(in),  optional :: mesh_kind     !< Kind of points coordinates: Float64, Float32 (not for PImageData).
  integer(I4P),     intent(in),  optional :: nx1           !< Initial node of x axis.
  integer(I4P),     intent(in),  optional :: nx2           !< Final node of x axis.
  integer(I4P),     intent(in),  optional :: ny1           !< Initial node of y axis.
  integer(I4P),     intent(in),  optional :: ny2           !< Final node of y axis.
  integer(I4P),     intent(in),  optional :: nz1           !< Initial node of z axis.
  integer(I4P),     intent(in),  optional :: nz2           !< Final node of z axis.
  integer(I4P),     intent(in),  optional :: ghost_level   !< Number of ghost levels of the pieces (default 0).
  real(R8P),        intent(in),  optional :: origin(3)      !< Origin of ImageData: coordinates of the point of indexes (0,0,0).
  real(R8P),        intent(in),  optional :: spacing(3)     !< Spacing of ImageData along each axis.
  real(R8P),        intent(in),  optional :: direction(9)   !< Axes directions of ImageData, row-major 3x3 (default identity).
  character(*),     intent(in),  optional :: action         !< Action: **write** (default) or **read**, case insensitive.
  integer(I4P)                            :: error         !< Error status.
  character(len=:), allocatable           :: action_       !< Action, upper case.

  if (.not.is_initialized) call penf_init
  if (.not.is_b64_initialized) call b64_init
  if (allocated(self%xml_writer)) deallocate(self%xml_writer)
  call self%xml_reader%finalize
  self%is_reading = .false.
  action_ = 'WRITE'
  if (present(action)) action_ = upper_case(trim(adjustl(action)))
  select case(action_)
  case('READ')
    error = self%xml_reader%initialize(filename=filename)
    self%is_reading = error == 0_I4P
    return
  case('WRITE')
    error = 1_I4P
    if (.not.present(mesh_topology)) return
  case default
    error = 1_I4P
    return
  endselect
  allocate(xml_writer_ascii_local :: self%xml_writer)
  if (present(ghost_level)) self%xml_writer%ghost_level = ghost_level
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
  error = self%xml_writer%initialize(format='ascii', filename=filename, mesh_topology=mesh_topology, &
                                     nx1=nx1, nx2=nx2, ny1=ny1, ny2=ny2, nz1=nz1, nz2=nz2, mesh_kind=mesh_kind)
  endfunction initialize

  function finalize(self) result(error)
  !< Finalize file: close the writer, or free the reader.
  class(pvtk_file), intent(inout) :: self  !< VTK file.
  integer(I4P)                    :: error !< Error status.

  error = 1
  if (self%is_reading) then
    call self%xml_reader%finalize
    self%is_reading = .false.
    error = 0
  elseif (allocated(self%xml_writer)) then
    error = self%xml_writer%finalize()
  endif
  endfunction finalize

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
endmodule vtk_fortran_pvtk_file
