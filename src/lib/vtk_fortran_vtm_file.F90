!< VTM file class.
module vtk_fortran_vtm_file
!-----------------------------------------------------------------------------------------------------------------------------------
!< VTM file class.
!-----------------------------------------------------------------------------------------------------------------------------------
use befor64
use penf
use stringifor
!-----------------------------------------------------------------------------------------------------------------------------------

!-----------------------------------------------------------------------------------------------------------------------------------
implicit none
private
public :: vtm_file
!-----------------------------------------------------------------------------------------------------------------------------------

!-----------------------------------------------------------------------------------------------------------------------------------
type :: vtm_file
  !< VTM file class.
  integer(I4P) :: unit=0_I4P              !< Logical unit.
  integer(I4P) :: blk(1:2)=[0_I4P, 0_I4P] !< Block indexes.
  integer(I4P) :: indent=0_I4P            !< Indent pointer.
  integer(I4P) :: error=0_I4P             !< IO Error status.
  contains
    ! public methods
    procedure, pass(self) :: initialize                       !< Initialize file.
    procedure, pass(self) :: finalize                         !< Finalize file.
    procedure, pass(self) :: open_block                       !< Open a block container.
    procedure, pass(self) :: close_block                      !< Close a block container.
    generic               :: write_files_list_of_block =>     &
                             write_files_list_of_block_array, &
                             write_files_list_of_block_string !< Write list of files that belong to the current block.
    ! private methods
    procedure, pass(self), private :: write_files_list_of_block_array  !< Write list of files (array input).
    procedure, pass(self), private :: write_files_list_of_block_string !< Write list of files (string input).
endtype vtm_file
!-----------------------------------------------------------------------------------------------------------------------------------
contains
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
  open(newunit=self%unit, file=trim(adjustl(filename)), action='WRITE', status='REPLACE', iostat=self%error)
  write(unit=self%unit, fmt='(A)', iostat=self%error)'<?xml version="1.0"?>'
  if (endian==endianL) then
    write(unit=self%unit, fmt='(A)', iostat=self%error) &
      '<VTKFile type="vtkMultiBlockDataSet" version="1.0" byte_order="LittleEndian">'
  else
    write(unit=self%unit, fmt='(A)', iostat=self%error) &
      '<VTKFile type="vtkMultiBlockDataSet" version="1.0" byte_order="BigEndian">'
  endif
  self%indent = 2
  write(unit=self%unit, fmt='(A)', iostat=self%error)repeat(' ', self%indent)//'<vtkMultiBlockDataSet>'
  self%indent = self%indent + 2
  self%blk = -1
  error = self%error
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
  self%indent = self%indent - 2
  write(unit=self%unit, fmt='(A)', iostat=self%error)repeat(' ', self%indent)//'</vtkMultiBlockDataSet>'
  write(unit=self%unit, fmt='(A)', iostat=self%error)'</VTKFile>'
  close(unit=self%unit, iostat=self%error)
  error = self%error
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction finalize

  function open_block(self, name) result(error)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Open a block container.
  !---------------------------------------------------------------------------------------------------------------------------------
  class(vtm_file), intent(inout)        :: self  !< VTM file.
  character(*),    intent(in), optional :: name  !< Block name.
  integer(I4P)                          :: error !< Error status.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  self%blk = self%blk + 1
  if (present(name)) then
    write(unit=self%unit, fmt='(A)', iostat=self%error) &
      repeat(' ', self%indent)//'<Block index="'//trim(str((self%blk(1)+self%blk(2)),.true.))//'" name="'//trim(adjustl(name))//'">'
  else
    write(unit=self%unit, fmt='(A)', iostat=self%error) &
      repeat(' ', self%indent)//'<Block index="'//trim(str((self%blk(1)+self%blk(2)), .true.))//'">'
  endif
  self%indent = self%indent + 2
  error = self%error
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction open_block

  function close_block(self, name) result(error)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Close a block container.
  !---------------------------------------------------------------------------------------------------------------------------------
  class(vtm_file), intent(inout)        :: self  !< VTM file.
  character(*),    intent(in), optional :: name  !< Block name.
  integer(I4P)                          :: error !< Error status.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  self%indent = self%indent - 2
  write(unit=self%unit, fmt='(A)', iostat=self%error)repeat(' ', self%indent)//'</Block>'
  self%blk(2) = -1
  error = self%error
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction close_block

  ! private methods
  function write_files_list_of_block_array(self, filenames, names) result(error)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Write list of files that belong to the current block (list passed as rank 1 array).
  !<
  !<#### Example of usage: 3 files blocks
  !<```fortran
  !< error = vtm%write_files_list_of_block(filenames=['file_1.vts','file_2.vts','file_3.vtu'])
  !<```
  !<
  !<#### Example of usage: 3 files blocks with custom name
  !<```fortran
  !< error = vtm%write_files_list_of_block(filenames=['file_1.vts','file_2.vts','file_3.vtu'],&
  !<                                       names=['block-bar','block-foo','block-baz'])
  !<```
  !---------------------------------------------------------------------------------------------------------------------------------
  class(vtm_file), intent(inout)        :: self         !< VTM file.
  character(*),    intent(in)           :: filenames(:) !< List of VTK-XML wrapped file names.
  character(*),    intent(in), optional :: names(:)     !< List names attributed to wrapped files.
  integer(I4P)                          :: error        !< Error status.
  integer(I4P)                          :: f            !< File counter.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  if (present(names)) then
    if (size(names, dim=1)==size(filenames, dim=1)) then
      do f=1, size(filenames, dim=1)
        write(unit=self%unit, fmt='(A)', iostat=self%error)                                                               &
          repeat(' ', self%indent)//'<DataSet index="'//trim(str(f-1, .true.))//'" file="'//trim(adjustl(filenames(f)))// &
          '" name="'//trim(adjustl(names(f)))//'"/>'
      enddo
    endif
  else
    do f=1,size(filenames, dim=1)
      write(unit=self%unit, fmt='(A)', iostat=self%error) &
        repeat(' ', self%indent)//'<DataSet index="'//trim(str(f-1, .true.))//'" file="'//trim(adjustl(filenames(f)))//'"/>'
    enddo
  endif
  error = self%error
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction write_files_list_of_block_array

  function write_files_list_of_block_string(self, filenames, names, delimiter) result(error)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Write list of files that belong to the current block (list passed as single string).
  !<
  !<#### Example of usage: 3 files blocks
  !<```fortran
  !< error = vtm%write_files_list_of_block(filenames='file_1.vts file_2.vts file_3.vtu')
  !<```
  !<
  !<#### Example of usage: 3 files blocks with custom name
  !<```fortran
  !< error = vtm%write_files_list_of_block(filenames='file_1.vts file_2.vts file_3.vtu',&
  !<                                       names='block-bar block-foo block-baz')
  !<```
  !---------------------------------------------------------------------------------------------------------------------------------
  class(vtm_file), intent(inout)        :: self          !< VTM file.
  character(*),    intent(in)           :: filenames     !< List of VTK-XML wrapped file names.
  character(*),    intent(in), optional :: names         !< List names attributed to wrapped files.
  character(*),    intent(in), optional :: delimiter     !< Delimiter character.
  integer(I4P)                          :: error         !< Error status.
  type(string), allocatable             :: filenames_(:) !< List of VTK-XML wrapped file names.
  type(string), allocatable             :: names_(:)     !< List names attributed to wrapped files.
  type(string)                          :: delimiter_    !< Delimiter character.
  type(string)                          :: buffer        !< A string buffer.
  integer(I4P)                          :: f             !< File counter.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  delimiter_ = ' ' ; if (present(delimiter)) delimiter_ = delimiter
  buffer = filenames
  call buffer%split(tokens=filenames_, sep=delimiter_%chars())
  if (present(names)) then
    buffer = names
    call buffer%split(tokens=names_, sep=delimiter_%chars())
    if (size(names_, dim=1)==size(filenames_, dim=1)) then
      do f=1, size(filenames_, dim=1)
        write(unit=self%unit, fmt='(A)', iostat=self%error)                                                                       &
          repeat(' ', self%indent)//'<DataSet index="'//trim(str(f-1, .true.))//'" file="'//trim(adjustl(filenames_(f)%chars()))//&
          '" name="'//trim(adjustl(names_(f)%chars()))//'"/>'
      enddo
    endif
  else
    do f=1,size(filenames_, dim=1)
      write(unit=self%unit, fmt='(A)', iostat=self%error) &
        repeat(' ', self%indent)//'<DataSet index="'//trim(str(f-1,.true.))//'" file="'//trim(adjustl(filenames_(f)%chars()))//'"/>'
    enddo
  endif
  error = self%error
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction write_files_list_of_block_string
endmodule vtk_fortran_vtm_file
