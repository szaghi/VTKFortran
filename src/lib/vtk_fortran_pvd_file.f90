!< PVD file class: VTK collection of datasets (time series).
module vtk_fortran_pvd_file
!< PVD file class: VTK collection of datasets (time series).
!<
!< A `.pvd` file lists datasets (VTK XML files written with `vtk_file`, `pvtk_file` or `vtm_file`) with their time step, and
!< optionally part, group and name: readers such as ParaView load the listed files as a time series.
!<
!< The file is kept valid after each `write_dataset`: every new dataset is written over the closing tags, which are written
!< again after it. Therefore a run that stops before `finalize` (crash, killed job) still leaves a readable collection of all the
!< datasets written so far, and a restarted run can continue an existing collection with `action='append'`.
use penf

implicit none
private
public :: pvd_file

type :: pvd_file
   !< PVD file class.
   private
   integer(I4P) :: unit=0_I4P      !< Logical unit, 0 when the file is not open.
   integer(I8P) :: pos_close=0_I8P !< Stream position of the closing tags, i.e. where the next dataset is written.
   contains
      ! public methods
      procedure, pass(self) :: initialize    !< Initialize (create or append to) the file.
      procedure, pass(self) :: write_dataset !< Write one dataset entry.
      procedure, pass(self) :: finalize      !< Finalize (close) the file.
      ! private methods
      procedure, pass(self), private :: write_closing_tags !< Write the closing tags at the current end of datasets.
endtype pvd_file

character(*), parameter :: closing_tags='  </Collection>'//new_line('a')//'</VTKFile>'//new_line('a') !< Closing tags.

contains
   ! public methods
   function initialize(self, filename, action) result(error)
   !< Initialize the file: create a new collection, or reopen an existing one to append datasets.
   !<
   !<### Example of usage
   !<
   !<```fortran
   !< type(pvd_file) :: pvd
   !< ...
   !< error = pvd%initialize(filename='simulation.pvd')                  ! new collection (an existing file is replaced)
   !< error = pvd%initialize(filename='simulation.pvd', action='append') ! continue an existing collection (e.g. restart)
   !< ...
   !<```
   !< @note With `action='append'` the file must exist and be a VTK collection (`<Collection>`): the datasets already listed
   !< are kept and the new ones are written after them.
   class(pvd_file), intent(inout)        :: self      !< PVD file.
   character(*),    intent(in)           :: filename  !< File name, with the `.pvd` extension.
   character(*),    intent(in), optional :: action    !< Action: **new** (default) or **append**.
   integer(I4P)                          :: error     !< Error status.
   character(len=:), allocatable         :: action_   !< Action, upper case.
   character(len=:), allocatable         :: buffer    !< Content of an existing file.
   character(len=:), allocatable         :: header    !< Header of a new file.
   integer(I8P)                          :: file_size !< Size of an existing file.
   integer(I8P)                          :: c         !< Character position.

   if (self%unit /= 0_I4P) error = self%finalize()
   action_ = 'NEW' ; if (present(action)) action_ = upper(trim(adjustl(action)))
   select case(action_)
   case('NEW')
      open(newunit=self%unit, file=trim(adjustl(filename)), access='stream', form='unformatted', status='replace', &
           action='readwrite', iostat=error)
      if (error /= 0) then
         self%unit = 0_I4P
         return
      endif
      header = '<?xml version="1.0"?>'//new_line('a')
      if (endian==endianL) then
         header = header//'<VTKFile type="Collection" version="1.0" byte_order="LittleEndian">'//new_line('a')
      else
         header = header//'<VTKFile type="Collection" version="1.0" byte_order="BigEndian">'//new_line('a')
      endif
      header = header//'  <Collection>'//new_line('a')
      write(unit=self%unit, iostat=error) header
      if (error /= 0) return
      self%pos_close = len(header, kind=I8P) + 1_I8P
   case('APPEND')
      inquire(file=trim(adjustl(filename)), size=file_size)
      if (file_size <= 0_I8P) then
         error = 1_I4P
         return
      endif
      open(newunit=self%unit, file=trim(adjustl(filename)), access='stream', form='unformatted', status='old', &
           action='readwrite', iostat=error)
      if (error /= 0) then
         self%unit = 0_I4P
         return
      endif
      allocate(character(len=file_size) :: buffer)
      read(unit=self%unit, pos=1, iostat=error) buffer
      if (error /= 0) return
      ! the datasets end where the line of the (last) closing Collection tag starts
      c = index(buffer, '</Collection>', back=.true., kind=I8P)
      if (c == 0_I8P .or. index(buffer, '<Collection>') == 0) then
         close(unit=self%unit)
         self%unit = 0_I4P
         error = 1_I4P
         return
      endif
      c = index(buffer(:c-1), new_line('a'), back=.true., kind=I8P)
      self%pos_close = c + 1_I8P
   case default
      error = 1_I4P
      return
   endselect
   call self%write_closing_tags(error=error)
   endfunction initialize

   function write_dataset(self, filename, timestep, part, group, name) result(error)
   !< Write one dataset entry: the file name of the dataset and its time step (and optionally part, group and name).
   !<
   !< The collection is valid (closing tags included) as soon as this function returns.
   !<
   !<### Example of usage
   !<
   !<```fortran
   !< error = pvd%write_dataset(filename='simulation_0010.vtu', timestep=0.1_R8P)
   !< error = pvd%write_dataset(filename='simulation_0010_rank1.vtu', timestep=0.1_R8P, part=1)
   !<```
   !< @note `filename` is written as given: a relative path is relative to the directory of the `.pvd` file.
   class(pvd_file), intent(inout)        :: self     !< PVD file.
   character(*),    intent(in)           :: filename !< File name of the dataset.
   real(R8P),       intent(in)           :: timestep !< Time step (time value) of the dataset.
   integer(I4P),    intent(in), optional :: part     !< Part (piece) of the dataset at this time step, default 0.
   character(*),    intent(in), optional :: group    !< Group of the dataset.
   character(*),    intent(in), optional :: name     !< Name of the dataset.
   integer(I4P)                          :: error    !< Error status.
   character(len=:), allocatable         :: entry    !< Dataset entry.
   character(len=:), allocatable         :: time     !< Time step, string.
   integer(I4P)                          :: part_    !< Part, local variable.

   if (self%unit == 0_I4P) then
      error = 1_I4P
      return
   endif
   part_ = 0_I4P ; if (present(part)) part_ = part
   ! the `+` of positive values is dropped by hand: PENF `no_sign` would drop the `-` of negative ones too
   time = trim(str(n=timestep, compact=.true.)) ; if (time(1:1) == '+') time = time(2:)
   entry = '    <DataSet timestep="'//time//'"'
   if (present(group)) entry = entry//' group="'//trim(adjustl(group))//'"'
   entry = entry//' part="'//trim(str(n=part_, no_sign=.true.))//'"'
   if (present(name)) entry = entry//' name="'//trim(adjustl(name))//'"'
   entry = entry//' file="'//trim(adjustl(filename))//'"/>'//new_line('a')
   write(unit=self%unit, pos=self%pos_close, iostat=error) entry
   if (error /= 0) return
   self%pos_close = self%pos_close + len(entry, kind=I8P)
   call self%write_closing_tags(error=error)
   endfunction write_dataset

   function finalize(self) result(error)
   !< Finalize the file: close it. The file is already a valid collection, the datasets written so far included.
   class(pvd_file), intent(inout) :: self  !< PVD file.
   integer(I4P)                   :: error !< Error status.

   error = 0_I4P
   if (self%unit /= 0_I4P) close(unit=self%unit, iostat=error)
   self%unit = 0_I4P
   self%pos_close = 0_I8P
   endfunction finalize

   ! private methods
   subroutine write_closing_tags(self, error)
   !< Write the closing tags at the current end of datasets, drop anything after them and flush the file.
   class(pvd_file), intent(inout) :: self  !< PVD file.
   integer(I4P),    intent(out)   :: error !< Error status.

   write(unit=self%unit, pos=self%pos_close, iostat=error) closing_tags
   if (error /= 0) return
   endfile(unit=self%unit, iostat=error) ! truncate: an appended file may have had more content after its closing tags
   if (error /= 0) return
   flush(unit=self%unit, iostat=error)
   endsubroutine write_closing_tags

   pure function upper(string) result(upper_string)
   !< Return a string in upper case (ASCII letters only).
   character(*), intent(in) :: string       !< Input string.
   character(len(string))   :: upper_string !< Upper case string.
   integer                  :: i            !< Counter.

   upper_string = string
   do i=1, len(string)
      if (string(i:i) >= 'a' .and. string(i:i) <= 'z') upper_string(i:i) = achar(iachar(string(i:i)) - 32)
   enddo
   endfunction upper
endmodule vtk_fortran_pvd_file
