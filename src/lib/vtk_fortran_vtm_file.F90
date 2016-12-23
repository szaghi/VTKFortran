!< VTM file class.
module vtk_fortran_vtm_file
!-----------------------------------------------------------------------------------------------------------------------------------
!< VTM file class.
!-----------------------------------------------------------------------------------------------------------------------------------
use befor64
use penf
use vtk_fortran_vtk_file_xml_writer_abstract
use vtk_fortran_vtk_file_xml_writer_ascii_local
!-----------------------------------------------------------------------------------------------------------------------------------

!-----------------------------------------------------------------------------------------------------------------------------------
implicit none
private
public :: vtm_file
!-----------------------------------------------------------------------------------------------------------------------------------

!-----------------------------------------------------------------------------------------------------------------------------------
type :: vtm_file
  !< VTM file class.
  class(xml_writer_abstract), allocatable, public :: xml_writer !< XML writer.
  contains
    ! public methods
    procedure, pass(self) :: initialize           !< Initialize file.
    procedure, pass(self) :: finalize             !< Finalize file.
    generic               :: write_block => &
                             write_block_array, &
                             write_block_string   !< Write one block dataset.
    ! private methods
    procedure, pass(self), private :: write_block_array  !< Write one block dataset (array input).
    procedure, pass(self), private :: write_block_string !< Write one block dataset (string input).
endtype vtm_file
!-----------------------------------------------------------------------------------------------------------------------------------
contains
  ! public methods
  function initialize(self, filename) result(error)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Initialize file (writer).
  !---------------------------------------------------------------------------------------------------------------------------------
  class(vtm_file), intent(inout) :: self     !< VTM file.
  character(*),    intent(in)    :: filename !< File name of output VTM file.
  integer(I4P)                   :: error    !< Error status.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  if (.not.is_initialized) call penf_init
  if (.not.is_b64_initialized) call b64_init
  if (allocated(self%xml_writer)) deallocate(self%xml_writer)
  allocate(xml_writer_ascii_local :: self%xml_writer)
  error = self%xml_writer%initialize(format='ascii', filename=filename, mesh_topology='vtkMultiBlockDataSet')
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction initialize

  function finalize(self) result(error)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Finalize file (writer).
  !---------------------------------------------------------------------------------------------------------------------------------
  class(vtm_file), intent(inout) :: self  !< VTM file.
  integer(I4P)                   :: error !< Error status.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  error = 1
  if (allocated(self%xml_writer)) error = self%xml_writer%finalize()
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction finalize

  ! private methods
  function write_block_array(self, filenames, names, name) result(error)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Write one block dataset (array input).
  !<
  !<#### Example of usage: 3 files blocks
  !<```fortran
  !< error = vtm%write_block(filenames=['file_1.vts', 'file_2.vts', 'file_3.vtu'], name='my_block')
  !<```
  !<
  !<#### Example of usage: 3 files blocks with custom name
  !<```fortran
  !< error = vtm%write_block(filenames=['file_1.vts', 'file_2.vts', 'file_3.vtu'], &
  !<                         names=['block-bar', 'block-foo', 'block-baz'], name='my_block')
  !<```
  !---------------------------------------------------------------------------------------------------------------------------------
  class(vtm_file), intent(inout)        :: self          !< VTM file.
  character(*),    intent(in)           :: filenames(1:) !< File names of VTK files grouped into current block.
  character(*),    intent(in), optional :: names(1:)     !< Auxiliary names attributed to each files.
  character(*),    intent(in), optional :: name          !< Block name
  integer(I4P)                          :: error         !< Error status.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  error = self%xml_writer%write_parallel_open_block(name=name)
  error = self%xml_writer%write_parallel_block_files(filenames=filenames, names=names)
  error = self%xml_writer%write_parallel_close_block()
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction write_block_array

  function write_block_string(self, filenames, names, name) result(error)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Write one block dataset (string input).
  !<
  !<#### Example of usage: 3 files blocks
  !<```fortran
  !< error = vtm%write_block(filenames='file_1.vts file_2.vts file_3.vtu', name='my_block')
  !<```
  !<
  !<#### Example of usage: 3 files blocks with custom name
  !<```fortran
  !< error = vtm%write_block(filenames='file_1.vts file_2.vts file_3.vtu', names='block-bar block-foo block-baz', name='my_block')
  !<```
  !---------------------------------------------------------------------------------------------------------------------------------
  class(vtm_file), intent(inout)        :: self      !< VTM file.
  character(*),    intent(in)           :: filenames !< File names of VTK files grouped into current block.
  character(*),    intent(in), optional :: names     !< Auxiliary names attributed to each files.
  character(*),    intent(in), optional :: name      !< Block name
  integer(I4P)                          :: error     !< Error status.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  error = self%xml_writer%write_parallel_open_block(name=name)
  error = self%xml_writer%write_parallel_block_files(filenames=filenames, names=names)
  error = self%xml_writer%write_parallel_close_block()
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction write_block_string
endmodule vtk_fortran_vtm_file
