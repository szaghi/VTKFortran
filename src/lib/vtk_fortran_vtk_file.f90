!< VTK file class.
module vtk_fortran_vtk_file
!< VTK file class.
use befor64
use penf
use stringifor
use vtk_fortran_vtk_writer_abstract
use vtk_fortran_vtk_file_xml_writer_appended
use vtk_fortran_vtk_file_xml_writer_ascii_local
use vtk_fortran_vtk_file_xml_writer_binary_local
use vtk_fortran_vtk_file_vtkhdf_writer
use vtk_fortran_vtk_file_xml_reader
use vtk_fortran_zlib, only : is_zlib_enabled
use vtk_fortran_hdf5, only : h5_mpi_max
use vtk_fortran_parameters, only : stderr

implicit none
private
public :: vtk_file

type :: vtk_file
  !< VTK file class.
  private
  class(vtk_writer_abstract), allocatable, public :: xml_writer !< Writer (XML; its name is kept for compatibility).
  type(xml_reader),                        public :: xml_reader !< XML reader (`initialize(..., action='read')`).
  logical                                         :: is_reading=.false. !< The file is open for reading.
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
                       origin, spacing, direction, header_type, compressor, action, comm, branch_factor, &
                       chunk_bytes) result(error)
   !< Initialize file: open it for writing (default) or for reading.
   !<
   !< @note This function must be the first to be called.
   !<
   !<### Reading
   !<
   !< With `action='read'` only `filename` is used: the file is indexed and its data are then read through the `xml_reader`
   !< component (`get_info`, `read_piece`, `read_geo`, `read_connectivity`, `read_polydata_cells`, `get_dataarray_names`,
   !< `get_dataarray_info`, `read_dataarray`); `finalize` frees it. Its format, topology, header type and compressor come from
   !< the file. The error codes of reading are listed in the `vtk_fortran_vtk_file_xml_reader` module.
   !<
   !<```fortran
   !< type(vtk_file)         :: vtk
   !< real(R8P), allocatable :: x(:), y(:), z(:), pressure(:)
   !< error = vtk%initialize(filename='mesh.vtu', action='read')
   !< error = vtk%xml_reader%read_geo(x=x, y=y, z=z)
   !< error = vtk%xml_reader%read_dataarray(location='node', data_name='pressure', x=pressure)
   !< error = vtk%finalize()
   !<```
   !<
   !<### Continuing a time series
   !<
   !< With `format='vtkhdf'` and `action='append'` a VTKHDF time series written before (`xml_writer%write_step`) is reopened
   !< to append steps, e.g. after a restart: `filename` is the only other argument used, the rest comes from the file. What
   !< an interrupted run left after its last complete piece is dropped. The next `write_step` needs a later time.
   !<
   !<```fortran
   !< error = vtk%initialize(format='vtkhdf', filename='run.vtkhdf', action='append')
   !< error = vtk%xml_writer%write_step(time=t)
   !< ...
   !<```
   !<
   !<### Writing
   !<
   !< `format` and `mesh_topology` are required. `mesh_topology='Table'` (columns of values, no mesh) is a `.vtt` file in
   !< the XML formats (one Piece: `NumberOfCols`, `NumberOfRows` and the columns in `RowData`, written between
   !< `write_dataarray(location='row', action='open')` and `...'close')`) or a VTKHDF Table. `'OverlappingAMR'` (the
   !< boxes of the levels of a block-structured AMR, `origin` its origin) and `'HyperTreeGrid'` (the trees of a tree-based
   !< AMR on a root grid, `nx1...nz2` its points, `branch_factor` 2 or 3) are written in the VTKHDF format only: the XML
   !< formats refuse them.
   !<
   !<### Writing one file from MPI ranks
   !<
   !< With `format='vtkhdf'` and `comm` (a Fortran MPI communicator: `MPI_Comm%MPI_VAL` of mpi_f08, or the integer handle of
   !< the `mpi` module) the ranks of `comm` write one file together (the library built with `VTKFORTRAN_USE_MPI` and a
   !< parallel HDF5): an UnstructuredGrid or a PolyData, each rank one piece, a partition of the file; or an ImageData,
   !< RectilinearGrid or StructuredGrid of the whole extent given here, each rank one region of it (`write_piece` with
   !< its sub-extent, its arrays and geometry of that region; the regions checked: inside the whole extent, not
   !< overlapping, covering it; an empty one, an upper bound before its lower one, writes nothing). Every rank makes the
   !< same calls, with the same arrays in the same order: the HDF5 calls are collective (ranks writing different arrays
   !< in a call are refused on every rank). A time series too: every rank calls `write_step` with the same time, then
   !< writes its piece (its region of a grid) of the step; `action='append'` with `comm` continues one (a parallel
   !< restart). Polyhedra (on any ranks) and `compressor='zlib'` (an HDF5 1.10.2 or later with the deflate filter:
   !< parallel filtered writes, collective) too. Refused with `comm`: `compressor='zlib'` for a grid, the other formats,
   !< `action='read'`.
   !<
   !<```fortran
   !< error = vtk%initialize(format='vtkhdf', filename='mesh.vtkhdf', mesh_topology='UnstructuredGrid', &
   !<                        comm=MPI_COMM_WORLD%MPI_VAL)
   !< error = vtk%xml_writer%write_piece(np=np, nc=nc) ! the piece of this rank
   !< ...
   !<```
   !<
   !<### Supported output formats are (the passed specifier value is case insensitive):
   !<
   !<- ASCII: data are saved in ASCII format;
   !<- BINARY: data are saved in base64 encoded format;
   !<- RAW: data are saved in raw-binary format in the appended tag of the XML file;
   !<- RAW-ZLIB: shorthand of RAW with `compressor='zlib'`;
   !<- BINARY-APPENDED: data are saved in base64 encoded format in the appended tag of the XML file;
   !<- VTKHDF: an HDF5 file in the VTKHDF layout (needs the library built with `VTKFORTRAN_USE_HDF5`): ImageData,
   !<  RectilinearGrid, StructuredGrid (one piece), UnstructuredGrid and PolyData (each piece a partition), Table,
   !<  OverlappingAMR, HyperTreeGrid; see the `vtk_fortran_vtk_file_vtkhdf_writer` module.
   !<
   !<### Compression of binary data
   !<
   !< The optional `compressor` compresses the data of the binary formats (BINARY, RAW, BINARY-APPENDED) as VTK does
   !< (`vtkZLibDataCompressor`, blocks of 32 KiB): **none** (default) or **zlib**, case insensitive; it is ignored by the ASCII
   !< format. zlib needs the library built with `VTKFORTRAN_USE_ZLIB`: otherwise, as for an unknown compressor, `initialize`
   !< returns a non-zero error. RAW-ZLIB with `compressor='none'` is an error too. With VTKHDF, zlib deflates the HDF5
   !< datasets: it needs the deflate filter of the HDF5 library, not `VTKFORTRAN_USE_ZLIB`.
   !<
   !<### Chunks of the VTKHDF datasets
   !<
   !< The VTKHDF datasets that grow (the partitions of an UnstructuredGrid or a PolyData, the steps of a time series) or
   !< are deflated (`compressor='zlib'`) are stored by chunks. A chunk holds whole rows (the last dimension of the values,
   !< e.g. the points, or the time slices of a grid): as many as fit in `chunk_bytes` (default 1 MiB), at least one, at most
   !< the rows written when the dataset is created (those of all the ranks with `comm`; one row for a dataset created
   !< empty). Larger chunks mean fewer HDF5 metadata operations (large parallel runs, a chunk aligned to the stripe size
   !< of a parallel file system), smaller ones less unused space in short series. `chunk_bytes` is refused when not
   !< positive, and with the XML formats or `action='read'`; with `action='append'` it applies to the datasets created
   !< from then on, those of the file keep their chunks. With `comm` every rank gives the same `chunk_bytes` (or none):
   !< different ones are refused on every rank.
   !<
   !<### Bytes count header of binary data
   !<
   !< Each binary DataArray (formats BINARY, RAW, RAW-ZLIB, BINARY-APPENDED) is prefixed by its bytes count (by the header of
   !< its compressed blocks, when compressed). The optional
   !< `header_type` selects its width: **UInt32** (default) limits each DataArray to 2 GiB (larger ones stop the execution with
   !< an explicit error), **UInt64** lifts the limit; it is case insensitive and ignored by the ASCII and VTKHDF formats.
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
   !<- Table: columns of values, no mesh (`.vtt` in the XML formats); see *Writing* above.
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
   character(*),    intent(in), optional :: format        !< File format: ASCII, BINARY, RAW, RAW-ZLIB, BINARY-APPENDED, VTKHDF.
   character(*),    intent(in)           :: filename      !< File name.
   character(*),    intent(in), optional :: mesh_topology !< Mesh topology.
   logical,         intent(in), optional :: is_volatile   !< Write into memory (get_xml_volatile): ascii and binary only.
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
   character(*),    intent(in), optional :: action         !< Action: **write** (default), **read** or **append** (VTKHDF).
   integer(I4P),    intent(in), optional :: comm           !< MPI communicator (Fortran handle): one VTKHDF file from its ranks.
   integer(I4P),    intent(in), optional :: branch_factor  !< Branch factor of a HyperTreeGrid (VTKHDF): 2 (default) or 3.
   integer(I4P),    intent(in), optional :: chunk_bytes    !< Bytes of a chunk of the VTKHDF datasets at most (default 1 MiB).
   integer(I4P)                          :: error         !< Error status.
   type(string)                          :: fformat       !< File format.
   logical                               :: is_compressed !< Compress the binary data.
   integer(I8P)                          :: chunk         !< chunk_bytes of this rank, 0 if absent (comm).

   if (.not.is_initialized) call penf_init
   if (.not.is_b64_initialized) call b64_init
   if (allocated(self%xml_writer)) deallocate(self%xml_writer)
   call self%xml_reader%close
   self%is_reading = .false.
   if (present(comm)) then ! the ranks give the same chunk_bytes (0 if absent): checked by all, before any refusal
      chunk = 0_I8P ; if (present(chunk_bytes)) chunk = int(chunk_bytes, I8P)
      if (h5_mpi_max(comm, chunk) /= -h5_mpi_max(comm, -chunk)) then
         write(stderr, '(A)') 'error: VTKFortran vtk_file%initialize: the ranks of comm give different chunk_bytes'
         error = 1_I4P
         return
      endif
   endif
   if (present(chunk_bytes)) then ! VTKHDF only, positive
      error = 1_I4P
      if (chunk_bytes < 1_I4P) return
      if (.not.present(format)) return
      if (upper_case(trim(adjustl(format))) /= 'VTKHDF') return
      if (present(action)) then
         if (upper_case(trim(adjustl(action))) == 'READ') return
      endif
   endif
   if (present(action)) then
      if (present(comm) .and. upper_case(trim(adjustl(action))) == 'READ') then
         error = 1_I4P ! a file is read by one rank
         return
      endif
      select case(upper_case(trim(adjustl(action))))
      case('READ')
         error = self%xml_reader%initialize(filename=filename)
         self%is_reading = error == 0_I4P
         return
      case('APPEND')
         ! a VTKHDF time series continued (restart): its writer reads its state from the file
         error = 1_I4P
         if (.not.present(format)) return
         if (upper_case(trim(adjustl(format))) /= 'VTKHDF') return
         allocate(vtkhdf_writer :: self%xml_writer)
         if (present(chunk_bytes)) self%xml_writer%chunk_bytes = int(chunk_bytes, I8P) ! the datasets created from now on
         select type(writer => self%xml_writer)
         type is(vtkhdf_writer)
            if (present(comm)) call writer%set_comm(comm) ! continued by the ranks of comm
            error = writer%reopen(filename=filename)
         endselect
         return
      case('WRITE')
      case default
         error = 1_I4P
         return
      endselect
   endif
   error = 1_I4P
   if (.not.(present(format).and.present(mesh_topology))) return
   fformat = trim(adjustl(format))
   fformat = fformat%upper()
   ! an OverlappingAMR, a HyperTreeGrid: VTKHDF only (a Table is a .vtt in the XML formats)
   if (any(upper_case(trim(adjustl(mesh_topology))) == ['OVERLAPPINGAMR', 'HYPERTREEGRID ']) .and. &
       fformat /= 'VTKHDF') return
   error = 0_I4P
   select case(fformat%chars())
   case('ASCII')
      allocate(xml_writer_ascii_local :: self%xml_writer)
   case('BINARY-APPENDED', 'RAW', 'RAW-ZLIB')
      ! the appended data are written to the file directly: a volatile (in memory) file is not supported
      if (present(is_volatile)) then
         if (is_volatile) error = 1
      endif
      if (error == 0) allocate(xml_writer_appended :: self%xml_writer)
   case('BINARY')
      allocate(xml_writer_binary_local :: self%xml_writer)
   case('VTKHDF')
      ! an HDF5 file: the writer refuses a volatile file, and what it does not support yet
      allocate(vtkhdf_writer :: self%xml_writer)
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
   ! VTKHDF compresses with the deflate filter of HDF5, checked by its writer
   if (is_compressed .and. .not.is_zlib_enabled .and. fformat /= 'VTKHDF') then
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
   if (present(branch_factor)) self%xml_writer%branch_factor = branch_factor
   if (present(chunk_bytes)) self%xml_writer%chunk_bytes = int(chunk_bytes, I8P)
   if (present(direction)) then
      self%xml_writer%direction = direction
      self%xml_writer%is_direction_set = .true.
   endif
   if (present(comm)) then
      ! the ranks of comm write one file: VTKHDF only, the writer refuses what it cannot write in parallel
      select type(writer => self%xml_writer)
      type is(vtkhdf_writer)
         call writer%set_comm(comm)
      class default
         error = 1_I4P
         return
      endselect
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
   !< Finalize file: close the writer, or free the reader.
   class(vtk_file), intent(inout)  :: self  !< VTK file.
   integer(I4P)                    :: error !< Error status.

   error = 1
   if (self%is_reading) then
      call self%xml_reader%close
      self%is_reading = .false.
      error = 0
   elseif (allocated(self%xml_writer)) then
      error = self%xml_writer%finalize()
   endif
   endfunction finalize

   elemental subroutine free(self, error)
   !< Free allocated memory.
   class(vtk_file), intent(inout)         :: self  !< VTK file.
   integer(I4P),    intent(out), optional :: error !< Error status.

   if (allocated(self%xml_writer)) call self%xml_writer%free(error=error)
   call self%xml_reader%finalize
   self%is_reading = .false.
   endsubroutine free
endmodule vtk_fortran_vtk_file
