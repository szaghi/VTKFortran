!< VTM file class.
module vtk_fortran_vtm_file
!< VTM file class.
use befor64
use penf
use stringifor
use vtk_fortran_vtk_file_xml_writer_abstract
use vtk_fortran_vtk_file_xml_writer_ascii_local

implicit none
private
public :: vtm_file

type :: vtm_file
   !< VTM file class.
   class(xml_writer_abstract), allocatable, public :: xml_writer      !< XML writer.
   integer(I4P), allocatable                       :: scratch_unit(:) !< Scratch units for very large list of named blocks.
   contains
      ! public methods
      procedure, pass(self) :: initialize          !< Initialize file.
      procedure, pass(self) :: finalize            !< Finalize file.
      generic               :: write_block =>      &
                               write_block_array,  &
                               write_block_string, &
                               write_block_scratch !< Write one block dataset.
      ! private methods
      procedure, pass(self), private :: write_block_array  !< Write one block dataset (array input).
      procedure, pass(self), private :: write_block_string !< Write one block dataset (string input).
      ! scratch files methods``
      procedure, pass(self), private :: parse_scratch_files !< Parse scratch files.
      procedure, pass(self), private :: write_block_scratch !< Write one block dataset on scratch files.
endtype vtm_file
contains
  ! public methods
  function initialize(self, filename, scratch_units_number) result(error)
  !< Initialize file (writer).
  class(vtm_file), intent(inout)        :: self                  !< VTM file.
  character(*),    intent(in)           :: filename              !< File name of output VTM file.
  integer(I4P),    intent(in), optional :: scratch_units_number  !< Number of scratch units for very large list of named blocks.
  integer(I4P)                          :: scratch_units_number_ !< Number of scratch units for very large list of named blocks.
  integer(I4P)                          :: error                 !< Error status.

  if (.not.is_initialized) call penf_init
  if (.not.is_b64_initialized) call b64_init
  scratch_units_number_ = 0_I4P ; if (present(scratch_units_number)) scratch_units_number_ = scratch_units_number
  error = self%finalize()
  if (allocated(self%xml_writer)) deallocate(self%xml_writer)
  allocate(xml_writer_ascii_local :: self%xml_writer)
  error = self%xml_writer%initialize(format='ascii', filename=filename, mesh_topology='vtkMultiBlockDataSet')
  if (scratch_units_number_>0_I4P) allocate(self%scratch_unit(scratch_units_number_))
  endfunction initialize

  function finalize(self) result(error)
  !< Finalize file (writer).
  class(vtm_file), intent(inout) :: self  !< VTM file.
  integer(I4P)                   :: error !< Error status.

  error = 1
  if (allocated(self%scratch_unit)) then
     error = self%parse_scratch_files()
     deallocate(self%scratch_unit)
  endif
  if (allocated(self%xml_writer)) error = self%xml_writer%finalize()
  endfunction finalize

  ! private methods
  function write_block_array(self, filenames, names, name) result(error)
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
  class(vtm_file), intent(inout)        :: self          !< VTM file.
  character(*),    intent(in)           :: filenames(1:) !< File names of VTK files grouped into current block.
  character(*),    intent(in), optional :: names(1:)     !< Auxiliary names attributed to each files.
  character(*),    intent(in), optional :: name          !< Block name
  integer(I4P)                          :: error         !< Error status.

  error = self%xml_writer%write_parallel_open_block(name=name)
  error = self%xml_writer%write_parallel_block_files(filenames=filenames, names=names)
  error = self%xml_writer%write_parallel_close_block()
  endfunction write_block_array

   function write_block_string(self, action, filenames, names, name) result(error)
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
   class(vtm_file), intent(inout)        :: self      !< VTM file.
   character(*),    intent(in), optional :: action    !< Action: [open, close, write].
   character(*),    intent(in), optional :: filenames !< File names of VTK files grouped into current block.
   character(*),    intent(in), optional :: names     !< Auxiliary names attributed to each files.
   character(*),    intent(in), optional :: name      !< Block name
   integer(I4P)                          :: error     !< Error status.
   type(string)                          :: action_   !< Action string.

   if (present(action)) then
      action_ = trim(adjustl(action)) ; action_ = action_%upper()
      select case(action_%chars())
      case('OPEN')
         error = self%xml_writer%write_parallel_open_block(name=name)
      case('CLOSE')
         error = self%xml_writer%write_parallel_close_block()
      case('WRITE')
         if (present(filenames)) error = self%xml_writer%write_parallel_block_files(filenames=filenames, names=names)
      endselect
   else
      error = self%xml_writer%write_parallel_open_block(name=name)
      error = self%xml_writer%write_parallel_block_files(filenames=filenames, names=names)
      error = self%xml_writer%write_parallel_close_block()
   endif
   endfunction write_block_string

   ! scratch files methods
   function parse_scratch_files(self) result(error)
   !< Parse scratch files.
   class(vtm_file), intent(inout) :: self     !< VTM file.
   integer(I4P)                   :: error    !< Error status.
   character(9999)                :: filename !< File name of VTK file grouped into current block.
   character(9999)                :: name     !< Block name
   integer(I4P)                   :: s, f     !< Counter.

   if (allocated(self%scratch_unit)) then
      do s=1, size(self%scratch_unit, dim=1)
         ! rewind scratch file
         rewind(self%scratch_unit(s))
         ! write group name
         f = 0_I4P
         read(self%scratch_unit(s), iostat=error, fmt=*) name
         error = self%write_block(action='open', name=trim(adjustl(name)))
         ! write group filenames
         parse_file_loop : do
            read(self%scratch_unit(s), iostat=error, fmt=*) filename
            if (is_iostat_end(error)) exit parse_file_loop
            error = self%xml_writer%write_parallel_block_files(file_index=f,                     &
                                                               filename=trim(adjustl(filename)), &
                                                               name=trim(adjustl(filename)))
            f = f + 1_I4P
         enddo parse_file_loop
         ! close group
         error = self%write_block(action='close')
         ! close scratch file
         close(self%scratch_unit(s))
      enddo
   endif
   endfunction parse_scratch_files

   function write_block_scratch(self, scratch, action, filename, name) result(error)
   !< Write one block dataset on scratch files.
   class(vtm_file), intent(inout)        :: self     !< VTM file.
   integer(I4P),    intent(in)           :: scratch  !< Scratch unit.
   character(*),    intent(in)           :: action   !< Action: [open, write].
   character(*),    intent(in), optional :: filename !< File name of VTK file grouped into current block.
   character(*),    intent(in), optional :: name     !< Block name
   integer(I4P)                          :: error    !< Error status.
   type(string)                          :: action_  !< Action string.
   type(string)                          :: name_    !< Block name, local variable

   action_ = trim(adjustl(action)) ; action_ = action_%upper()
   select case(action_%chars())
   case('OPEN')
     open(newunit=self%scratch_unit(scratch), &
          form='FORMATTED',                   &
          action='READWRITE',                 &
          status='SCRATCH',                   &
          iostat=error)
      name_ = '' ; if (present(name)) name_ = trim(adjustl(name))
      write(self%scratch_unit(scratch), iostat=error, fmt='(A)') name_%chars()
   case('WRITE')
      if (present(filename)) write(self%scratch_unit(scratch), iostat=error, fmt='(A)') trim(filename)
   endselect
   endfunction write_block_scratch
endmodule vtk_fortran_vtm_file
