!< VTM file class.
module vtk_fortran_vtm_file
!< VTM file class.
use befor64
use penf
use stringifor
use vtk_fortran_vtk_file_xml_writer_abstract
use vtk_fortran_vtk_file_xml_writer_ascii_local
use vtk_fortran_xml_scanner

implicit none
private
public :: vtm_file

type :: vtm_file
   !< VTM file class.
   class(xml_writer_abstract), allocatable, public :: xml_writer      !< XML writer.
   integer(I4P), allocatable                       :: scratch_unit(:) !< Scratch units for very large list of named blocks.
   type(xml_scanner),          private             :: xml             !< Index of the file read (`action='read'`).
   logical,                    private             :: is_reading=.false. !< The file is open for reading.
   contains
      ! public methods
      procedure, pass(self) :: initialize          !< Initialize file.
      procedure, pass(self) :: finalize            !< Finalize file.
      procedure, pass(self) :: get_entries         !< Return the blocks and datasets of a file read.
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
  function initialize(self, filename, scratch_units_number, action) result(error)
  !< Initialize file: open it for writing (default) or for reading.
  !<
  !< With `action='read'` (case insensitive) the file is indexed: its blocks and datasets are returned by `get_entries`, and
  !< each dataset file can then be read with `vtk_file` (or `pvtk_file`); `finalize` frees the index. The error is 1 if the
  !< file cannot be read, 2 if it is not a multi-block file (`vtkMultiBlockDataSet`).
  class(vtm_file), intent(inout)        :: self                  !< VTM file.
  character(*),    intent(in)           :: filename              !< File name of output VTM file.
  integer(I4P),    intent(in), optional :: scratch_units_number  !< Number of scratch units for very large list of named blocks.
  character(*),    intent(in), optional :: action                !< Action: **write** (default) or **read**.
  integer(I4P)                          :: scratch_units_number_ !< Number of scratch units for very large list of named blocks.
  integer(I4P)                          :: error                 !< Error status.
  character(len=:), allocatable         :: value                 !< Attribute value.
  integer(I4P)                          :: root                  !< Index of the VTKFile element.
  type(string)                          :: action_               !< Action, upper case.

  if (.not.is_initialized) call penf_init
  if (.not.is_b64_initialized) call b64_init
  if (present(action)) then
     action_ = trim(adjustl(action)) ; action_ = action_%upper()
     select case(action_%chars())
     case('READ')
        error = self%finalize()
        call self%xml%scan(filename=filename, error=error)
        if (error /= 0) return
        error = 2
        root = self%xml%find_child(parent=0, name='VTKFile')
        if (root == 0) return
        call self%xml%element(root)%get_attribute(name='type', value=value)
        if (value /= 'vtkMultiBlockDataSet') return
        if (self%xml%find_child(parent=root, name='vtkMultiBlockDataSet') == 0) return
        self%is_reading = .true.
        error = 0
        return
     case('WRITE')
     case default
        error = 1
        return
     endselect
  endif
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
  if (self%is_reading) then
     call self%xml%free
     self%is_reading = .false.
     error = 0
     return
  endif
  if (allocated(self%scratch_unit)) then
     error = self%parse_scratch_files()
     deallocate(self%scratch_unit)
  endif
  if (allocated(self%xml_writer)) then
     error = self%xml_writer%finalize()
     ! a finalized writer is dropped: finalizing it again (e.g. by a new initialize) would write the closing tags twice
     deallocate(self%xml_writer)
  endif
  endfunction finalize

  function get_entries(self, level, kind, index, name, file) result(error)
  !< Return the blocks and datasets of a file read (`initialize(..., action='read')`), in the order of the file.
  !<
  !< The hierarchy is flattened depth first: each entry has its nesting level (1 for the children of the root), its kind
  !< (**block**, **dataset**, or **piece** for the pieces of a `vtkMultiPieceDataSet` written by VTK), its index among the
  !< children of its parent (-1 if the file has none), its name and, for datasets, its file (empty for blocks and pieces,
  !< or for datasets without file). The children of an entry are the following entries of the next level. The names and files
  !< are blank padded to the longest one; relative files are relative to the directory of the `.vtm` file.
  !<
  !<```fortran
  !< type(vtm_file)                :: vtm
  !< integer(I4P),     allocatable :: level(:)
  !< character(len=:), allocatable :: kind(:), file(:)
  !< error = vtm%initialize(filename='assembly.vtm', action='read')
  !< error = vtm%get_entries(level=level, kind=kind, file=file)
  !< error = vtm%finalize()
  !<```
  class(vtm_file),               intent(in)            :: self     !< VTM file.
  integer(I4P),     allocatable, intent(out), optional :: level(:) !< Nesting level of the entries, from 1.
  character(len=:), allocatable, intent(out), optional :: kind(:)  !< Kind of the entries: block, dataset or piece.
  integer(I4P),     allocatable, intent(out), optional :: index(:) !< Index of the entries among their siblings.
  character(len=:), allocatable, intent(out), optional :: name(:)  !< Name of the entries.
  character(len=:), allocatable, intent(out), optional :: file(:)  !< File of the datasets.
  integer(I4P)                                         :: error    !< Error status: 0, or 4 if no file is open for reading.
  integer(I4P),     allocatable                        :: ids(:)   !< Indexes of the entry elements.
  character(len=:), allocatable                        :: value    !< Attribute value.
  integer(I4P)                                         :: base     !< Level of the multi-block element.
  integer(I4P)                                         :: n        !< Number of entries.
  integer(I4P)                                         :: e        !< Counter.
  integer(I4P)                                         :: ln       !< Length of the longest name.
  integer(I4P)                                         :: lf       !< Length of the longest file.
  integer(I4P)                                         :: iostat   !< IO status.

  error = 4
  if (.not.self%is_reading) return
  base = self%xml%element(self%xml%find_child(parent=self%xml%find_child(parent=0, name='VTKFile'), &
                                              name='vtkMultiBlockDataSet'))%level
  allocate(ids(1:self%xml%elements_number))
  n = 0 ; ln = 0 ; lf = 0
  do e=1, self%xml%elements_number
     if (self%xml%element(e)%level <= base) cycle
     select case(self%xml%element(e)%name)
     case('Block', 'DataSet', 'Piece')
        n = n + 1
        ids(n) = e
        call self%xml%element(e)%get_attribute(name='name', value=value)
        ln = max(ln, len(value))
        call self%xml%element(e)%get_attribute(name='file', value=value)
        lf = max(lf, len(value))
     endselect
  enddo
  if (present(level)) then
     allocate(level(1:n))
     do e=1, n
        level(e) = self%xml%element(ids(e))%level - base
     enddo
  endif
  if (present(kind)) then
     allocate(character(len=7) :: kind(1:n))
     do e=1, n
        select case(self%xml%element(ids(e))%name)
        case('Block')
           kind(e) = 'block'
        case('DataSet')
           kind(e) = 'dataset'
        case default
           kind(e) = 'piece'
        endselect
     enddo
  endif
  if (present(index)) then
     allocate(index(1:n))
     do e=1, n
        index(e) = -1
        call self%xml%element(ids(e))%get_attribute(name='index', value=value)
        if (len(value) > 0) read(value, *, iostat=iostat) index(e)
     enddo
  endif
  if (present(name)) then
     allocate(character(len=ln) :: name(1:n))
     do e=1, n
        call self%xml%element(ids(e))%get_attribute(name='name', value=value)
        name(e) = value
     enddo
  endif
  if (present(file)) then
     allocate(character(len=lf) :: file(1:n))
     do e=1, n
        call self%xml%element(ids(e))%get_attribute(name='file', value=value)
        file(e) = value
     enddo
  endif
  error = 0
  endfunction get_entries

  ! private methods
  function write_block_array(self, filenames, names, name, action) result(error)
  !< Write one block dataset (array input).
  !<
  !< By default the files are wrapped in a new block; with `action='write'` they are written as datasets of the current
  !< (open) block, without a new block.
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
  !<
  !<#### Example of usage: nested blocks
  !<```fortran
  !< error = vtm%write_block(action='open', name='assembly')
  !< error = vtm%write_block(filenames=['part_1.vtu', 'part_2.vtu'], action='write') ! datasets 0, 1 of assembly
  !< error = vtm%write_block(filenames=['bolt_1.vtu', 'bolt_2.vtu'], name='bolts')  ! block 2 of assembly
  !< error = vtm%write_block(action='close')
  !<```
  class(vtm_file), intent(inout)        :: self          !< VTM file.
  character(*),    intent(in)           :: filenames(1:) !< File names of VTK files grouped into current block.
  character(*),    intent(in), optional :: names(1:)     !< Auxiliary names attributed to each files.
  character(*),    intent(in), optional :: name          !< Block name
  character(*),    intent(in), optional :: action        !< Action: 'write' (datasets of the current block) or default.
  integer(I4P)                          :: error         !< Error status.
  logical                               :: is_new_block  !< Wrap the files in a new block.

  is_new_block = .true.
  if (present(action)) is_new_block = trim(adjustl(action)) /= 'write' .and. trim(adjustl(action)) /= 'WRITE'
  if (is_new_block) error = self%xml_writer%write_parallel_open_block(name=name)
  error = self%xml_writer%write_parallel_block_files(filenames=filenames, names=names)
  if (is_new_block) error = self%xml_writer%write_parallel_close_block()
  endfunction write_block_array

   function write_block_string(self, action, filenames, names, name) result(error)
   !< Write one block dataset (string input).
   !<
   !< With `action` the block is written in steps, so blocks can be nested: `'open'` opens a (child) block, `'write'` writes the
   !< files as datasets of the current block, `'close'` closes it. The children of a block (blocks and datasets) are indexed
   !< from 0 in the order they are written.
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
