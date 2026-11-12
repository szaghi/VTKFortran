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
!<
!< A file name ending in `.series` is a ParaView file series instead (JSON: `{"file-series-version" : "1.0", "files" :
!< [{"name" : ..., "time" : ...}, ...]}`), written, continued and read by the same calls: it lists files and times only
!< (`part`, `group` and `name` are refused), and is valid after each `write_dataset` too.
use penf
use foxy, only : xml_index, XML_MALFORMED, xml_escape

implicit none
private
public :: pvd_file

type :: pvd_file
   !< PVD file class.
   private
   integer(I4P) :: unit=0_I4P      !< Logical unit, 0 when the file is not open.
   integer(I8P) :: pos_close=0_I8P !< Stream position of the closing tags, i.e. where the next dataset is written.
   integer(I8P) :: file_end=0_I8P  !< Size of the file: its last byte.
   type(xml_index)   :: xml        !< Index of the file read (`action='read'`, FoXy).
   logical      :: is_reading=.false. !< The file is open for reading.
   logical      :: is_series=.false.  !< A ParaView `.series` file (JSON) instead of a `.pvd`.
   integer(I4P) :: n_entries=0_I4P    !< Entries of a `.series` file written (a comma before the next one).
   ! a `.series` file read: the time and the file of each entry (start and length in series_chars)
   real(R8P),    allocatable :: series_times(:)  !< Times of the entries.
   integer(I4P), allocatable :: series_text(:,:) !< Start and length of the file name of each entry.
   character(len=:), allocatable :: series_chars !< The file names.
   contains
      ! public methods
      procedure, pass(self) :: get_datasets  !< Return the datasets of a file read.
      procedure, pass(self) :: initialize    !< Initialize (create or append to) the file.
      procedure, pass(self) :: write_dataset !< Write one dataset entry.
      procedure, pass(self) :: finalize      !< Finalize (close) the file.
      ! private methods
      procedure, pass(self), private :: write_closing_tags !< Write the closing tags at the current end of datasets.
      procedure, pass(self), private :: read_series        !< Read the entries of a `.series` file.
endtype pvd_file

character(*), parameter :: closing_tags='  </Collection>'//new_line('a')//'</VTKFile>'//new_line('a') !< Closing tags.
character(*), parameter :: series_closing=new_line('a')//'  ]'//new_line('a')//'}'//new_line('a') !< Closing of .series.

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
   !< error = pvd%initialize(filename='simulation.pvd', action='read')   ! read the collection, see get_datasets
   !< ...
   !<```
   !< @note With `action='append'` the file must exist and be a VTK collection (`<Collection>`): the datasets already listed
   !< are kept and the new ones are written after them.
   !< @note With `action='read'` the file is indexed, its datasets are returned by `get_datasets` and `finalize` frees the
   !< index: the error is 1 if the file cannot be read, 2 if it is not a VTK collection.
   !< @note A file name ending in `.series` (case insensitive) is a ParaView file series (JSON), with the same actions.
   class(pvd_file), intent(inout)        :: self      !< PVD file.
   character(*),    intent(in)           :: filename  !< File name, with the `.pvd` (or `.series`) extension.
   character(*),    intent(in), optional :: action    !< Action: **new** (default), **append** or **read**.
   integer(I4P)                          :: error     !< Error status.
   character(len=:), allocatable         :: action_   !< Action, upper case.
   character(len=:), allocatable         :: buffer    !< Content of an existing file.
   character(len=:), allocatable         :: header    !< Header of a new file.
   integer(I8P)                          :: file_size !< Size of an existing file.
   integer(I8P)                          :: c         !< Character position.
   integer(I4P)                          :: root      !< Index of the VTKFile element.
   integer(I4P)                          :: iostat    !< IO status of the scan.
   character(len=:), allocatable         :: value     !< Attribute value.

   if (self%unit /= 0_I4P) error = self%finalize()
   call self%xml%free
   self%is_reading = .false.
   self%n_entries = 0_I4P
   value = trim(adjustl(filename))
   self%is_series = .false.
   if (len(value) > 7) self%is_series = upper(value(len(value)-6:)) == '.SERIES'
   action_ = 'NEW' ; if (present(action)) action_ = upper(trim(adjustl(action)))
   if (self%is_series) then
      select case(action_)
      case('READ')
         error = self%read_series(filename=trim(adjustl(filename)))
         self%is_reading = error == 0_I4P
         return
      case('NEW')
         header = '{'//new_line('a')//'  "file-series-version" : "1.0",'//new_line('a')//'  "files" : ['
      case('APPEND')
         continue
      case default
         error = 1_I4P
         return
      endselect
   endif
   select case(action_)
   case('READ')
      call self%xml%scan(filename=trim(adjustl(filename)), iostat=iostat)
      if (iostat /= 0) then ! 2 if malformed XML, 1 if it cannot be read
         error = merge(2_I4P, 1_I4P, iostat == XML_MALFORMED)
         return
      endif
      error = 2_I4P
      root = self%xml%find(name='VTKFile', parent=0)
      if (root == 0) return
      value = self%xml%attribute(id=root, name='type')
      if (value /= 'Collection') return
      if (self%xml%find(name='Collection', parent=root) == 0) return
      self%is_reading = .true.
      error = 0_I4P
      return
   case('NEW')
      open(newunit=self%unit, file=trim(adjustl(filename)), access='stream', form='unformatted', status='replace', &
           action='readwrite', iostat=error)
      if (error /= 0) then
         self%unit = 0_I4P
         return
      endif
      if (.not.self%is_series) then
         header = '<?xml version="1.0"?>'//new_line('a')
         if (endian==endianL) then
            header = header//'<VTKFile type="Collection" version="1.0" byte_order="LittleEndian">'//new_line('a')
         else
            header = header//'<VTKFile type="Collection" version="1.0" byte_order="BigEndian">'//new_line('a')
         endif
         header = header//'  <Collection>'//new_line('a')
      endif
      write(unit=self%unit, iostat=error) header
      if (error /= 0) return
      self%pos_close = len(header, kind=I8P) + 1_I8P
      self%file_end = len(header, kind=I8P)
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
      if (self%is_series) then ! the entries end at the last non-blank before the last ]: a } (entries) or the [
         c = index(buffer, ']', back=.true., kind=I8P)
         if (c > 0_I8P .and. index(buffer, '"file-series-version"') > 0) c = len_trim(buffer(:c-1), kind=I8P)
         if (c > 0_I8P) then
            do while (c > 0_I8P)
               if (index(' '//achar(9)//achar(10)//achar(13), buffer(c:c)) == 0) exit
               c = c - 1_I8P
            enddo
         endif
         if (c == 0_I8P .or. index(buffer, '"file-series-version"') == 0) then
            close(unit=self%unit)
            self%unit = 0_I4P
            error = 1_I4P
            return
         endif
         if (buffer(c:c) /= '}' .and. buffer(c:c) /= '[') then
            close(unit=self%unit)
            self%unit = 0_I4P
            error = 1_I4P
            return
         endif
         if (buffer(c:c) == '}') self%n_entries = 1_I4P
         self%pos_close = c + 1_I8P
         self%file_end = file_size
         call self%write_closing_tags(error=error)
         return
      endif
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
      self%file_end = file_size
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
   !< @note `filename` is written as given: a relative path is relative to the directory of the `.pvd` file. `filename`,
   !< `group` and `name` are escaped as XML attribute values (`&`, `<`, `>`, `"`, `'`). In a `.series` file the entry is
   !< `{ "name" : filename, "time" : timestep }` (a JSON string: `"` and `\` escaped); `part`, `group` and `name` are
   !< refused (error 1).
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

   allocate(character(26) :: time)

   if (self%unit == 0_I4P) then
      error = 1_I4P
      return
   endif
   if (self%is_series) then ! files and times only
      error = 1_I4P
      if (present(part) .or. present(group) .or. present(name)) return
      write(time, '(ES25.16E3)') timestep ! 17 digits: the value read back exactly
      entry = merge(',', ' ', self%n_entries > 0_I4P)
      entry = trim(entry)//new_line('a')//'    { "name" : "'//json_escape(trim(adjustl(filename)))//'", "time" : '// &
              trim(adjustl(time))//' }'
      write(unit=self%unit, pos=self%pos_close, iostat=error) entry
      if (error /= 0) return
      self%pos_close = self%pos_close + len(entry, kind=I8P)
      self%n_entries = self%n_entries + 1_I4P
      call self%write_closing_tags(error=error)
      return
   endif
   part_ = 0_I4P ; if (present(part)) part_ = part
   ! the `+` of positive values is dropped by hand: PENF `no_sign` would drop the `-` of negative ones too
   time = trim(str(n=timestep, compact=.true.)) ; if (time(1:1) == '+') time = time(2:)
   entry = '    <DataSet timestep="'//time//'"'
   if (present(group)) entry = entry//' group="'//xml_escape(trim(adjustl(group)), attribute=.true.)//'"'
   entry = entry//' part="'//trim(str(n=part_, no_sign=.true.))//'"'
   if (present(name)) entry = entry//' name="'//xml_escape(trim(adjustl(name)), attribute=.true.)//'"'
   entry = entry//' file="'//xml_escape(trim(adjustl(filename)), attribute=.true.)//'"/>'//new_line('a')
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
   self%file_end = 0_I8P
   call self%xml%free
   self%is_reading = .false.
   self%is_series = .false.
   self%n_entries = 0_I4P
   if (allocated(self%series_times)) deallocate(self%series_times)
   if (allocated(self%series_text)) deallocate(self%series_text)
   if (allocated(self%series_chars)) deallocate(self%series_chars)
   endfunction finalize

   function get_datasets(self, timestep, part, group, name, file) result(error)
   !< Return the datasets of a file read (`initialize(..., action='read')`), in the order of the file.
   !<
   !< Missing attributes are returned as 0 (`timestep`, `part`) or empty (`group`, `name`, `file`). The strings are blank
   !< padded to the longest one; relative files are relative to the directory of the `.pvd` file. A `.series` file gives
   !< the `name` of each entry as `file` and its `time` as `timestep` (its position in the list, from 0, without `time`,
   !< as ParaView takes it); `part` 0, `group` and `name` empty.
   !<
   !<```fortran
   !< type(pvd_file)                :: pvd
   !< real(R8P),        allocatable :: timestep(:)
   !< character(len=:), allocatable :: file(:)
   !< error = pvd%initialize(filename='simulation.pvd', action='read')
   !< error = pvd%get_datasets(timestep=timestep, file=file)
   !< error = pvd%finalize()
   !<```
   class(pvd_file),               intent(in)            :: self        !< PVD file.
   real(R8P),        allocatable, intent(out), optional :: timestep(:) !< Time step of the datasets.
   integer(I4P),     allocatable, intent(out), optional :: part(:)     !< Part of the datasets.
   character(len=:), allocatable, intent(out), optional :: group(:)    !< Group of the datasets.
   character(len=:), allocatable, intent(out), optional :: name(:)     !< Name of the datasets.
   character(len=:), allocatable, intent(out), optional :: file(:)     !< File of the datasets.
   integer(I4P)                                         :: error       !< Error status: 0, or 4 if no file is open for reading.
   integer(I4P)                                         :: collection  !< Index of the Collection element.
   integer(I4P)                                         :: n           !< Number of datasets.
   integer(I4P)                                         :: d           !< Counter.
   integer(I4P)                                         :: iostat      !< IO status.
   character(len=:), allocatable                        :: value       !< Attribute value.
   integer(I4P),     allocatable                        :: datasets(:) !< Indexes of the DataSet elements.

   error = 4_I4P
   if (.not.self%is_reading) return
   if (self%is_series) then
      n = size(self%series_times)
      if (present(timestep)) timestep = self%series_times
      if (present(part)) then
         allocate(part(1:n))
         part = 0_I4P
      endif
      if (present(group)) allocate(character(len=0) :: group(1:n))
      if (present(name)) allocate(character(len=0) :: name(1:n))
      if (present(file)) then
         allocate(character(len=maxval([0_I4P, self%series_text(2,:)])) :: file(1:n))
         do d=1, n
            file(d) = self%series_chars(self%series_text(1,d):self%series_text(1,d)+self%series_text(2,d)-1)
         enddo
      endif
      error = 0_I4P
      return
   endif
   collection = self%xml%find(name='Collection', parent=self%xml%find(name='VTKFile', parent=0))
   datasets = self%xml%find_all(name='DataSet', parent=collection)
   n = size(datasets, dim=1)
   if (present(timestep)) then
      allocate(timestep(1:n))
      do d=1, n
         timestep(d) = 0._R8P
         call self%xml%get_attribute(id=datasets(d), name='timestep', value=timestep(d))
      enddo
   endif
   if (present(part)) then
      allocate(part(1:n))
      do d=1, n
         part(d) = 0
         call self%xml%get_attribute(id=datasets(d), name='part', value=part(d))
      enddo
   endif
   if (present(group)) call dataset_strings(attribute='group', strings=group)
   if (present(name)) call dataset_strings(attribute='name', strings=name)
   if (present(file)) call dataset_strings(attribute='file', strings=file)
   error = 0_I4P
   contains
      subroutine dataset_strings(attribute, strings)
      !< Return an attribute of the datasets, blank padded to the longest one.
      character(*),                  intent(in)  :: attribute  !< Attribute name.
      character(len=:), allocatable, intent(out) :: strings(:) !< Attribute values.
      integer(I4P)                               :: l          !< Length of the longest value.

      l = 0
      do d=1, n
         value = self%xml%attribute(id=datasets(d), name=attribute)
         l = max(l, len(value))
      enddo
      allocate(character(len=l) :: strings(1:n))
      do d=1, n
         value = self%xml%attribute(id=datasets(d), name=attribute)
         strings(d) = value
      enddo
      endsubroutine dataset_strings
   endfunction get_datasets

   ! private methods
   subroutine write_closing_tags(self, error)
   !< Write the closing tags at the current end of datasets, blank anything after them and flush the file.
   !<
   !< Older content after the closing tags (an appended file edited by hand) is overwritten with blanks on the line of the
   !< closing tag, not truncated: the file is a valid collection at any time, and ENDFILE does not truncate a stream file
   !< with every compiler (nvfortran 26.9).
   class(pvd_file), intent(inout) :: self  !< PVD file.
   integer(I4P),    intent(out)   :: error !< Error status.
   integer(I8P)                   :: last  !< Last byte of the closing tags.
   integer(I8P)                   :: b     !< Counter.
   character(len=:), allocatable  :: tags  !< The closing tags of the file.

   tags = closing_tags
   if (self%is_series) tags = series_closing
   last = self%pos_close + len(tags, kind=I8P) - 1_I8P
   if (last >= self%file_end) then
      write(unit=self%unit, pos=self%pos_close, iostat=error) tags
      if (error /= 0) return
      self%file_end = last
   else
      write(unit=self%unit, pos=self%pos_close, iostat=error) tags(:len(tags)-1)
      if (error /= 0) return
      do b=last, self%file_end - 1_I8P
         write(unit=self%unit, iostat=error) ' '
         if (error /= 0) return
      enddo
      write(unit=self%unit, iostat=error) new_line('a')
      if (error /= 0) return
   endif
   flush(unit=self%unit, iostat=error)
   endsubroutine write_closing_tags

   function read_series(self, filename) result(error)
   !< Read the entries of a `.series` file (JSON): the `name` and `time` of each object of the array `files`. Error 1 if the
   !< file cannot be read, 2 if it is not a file series (no `file-series-version`, no `files` array, an entry without
   !< `name`, a `time` that is not a number).
   class(pvd_file), intent(inout) :: self     !< PVD file.
   character(*),    intent(in)    :: filename !< File name.
   integer(I4P)                   :: error    !< Error status.
   character(len=:), allocatable  :: buffer   !< Content of the file.
   character(len=:), allocatable  :: key      !< A key of an entry.
   character(len=:), allocatable  :: text     !< A string value.
   integer(I8P)                   :: file_size !< Size of the file.
   integer(I4P)                   :: unit     !< Logical unit.
   integer(I4P)                   :: c        !< Character position.
   integer(I4P)                   :: n        !< Entries read.
   integer(I4P)                   :: iostat   !< IO status.
   real(R8P)                      :: time     !< Time of an entry.
   logical                        :: has_name !< The entry has its name.
   logical                        :: has_time !< The entry has its time.

   error = 1_I4P
   inquire(file=filename, size=file_size)
   if (file_size <= 0_I8P) return
   open(newunit=unit, file=filename, access='stream', form='unformatted', status='old', action='read', iostat=iostat)
   if (iostat /= 0) return
   allocate(character(len=file_size) :: buffer)
   read(unit=unit, pos=1, iostat=iostat) buffer
   close(unit)
   if (iostat /= 0) return
   error = 2_I4P
   if (index(buffer, '"file-series-version"') == 0) return
   c = index(buffer, '"files"')
   if (c == 0) return
   c = c + 7
   call skip_blanks
   if (.not.next_is(':')) return
   call skip_blanks
   if (.not.next_is('[')) return
   allocate(self%series_times(0), self%series_text(2,0))
   self%series_chars = ''
   n = 0
   do
      call skip_blanks
      if (c > len(buffer)) return
      if (buffer(c:c) == ']') exit
      if (buffer(c:c) == ',') then
         c = c + 1
         cycle
      endif
      if (.not.next_is('{')) return
      has_name = .false. ; has_time = .false.
      do ! the members of the entry
         call skip_blanks
         if (c > len(buffer)) return
         if (buffer(c:c) == '}') then
            c = c + 1
            exit
         endif
         if (buffer(c:c) == ',') then
            c = c + 1
            cycle
         endif
         if (.not.json_string(key)) return
         call skip_blanks
         if (.not.next_is(':')) return
         call skip_blanks
         if (key == 'name') then
            if (.not.json_string(text)) return
            has_name = .true.
         elseif (key == 'time') then
            if (.not.json_number(time)) return
            has_time = .true.
         else ! another member: a string, a number or a literal, skipped
            if (c > len(buffer)) return
            if (buffer(c:c) == '"') then
               if (.not.json_string(key)) return
            else
               do while (c <= len(buffer))
                  if (index(',}', buffer(c:c)) > 0) exit
                  c = c + 1
               enddo
            endif
         endif
      enddo
      if (.not.has_name) return
      if (.not.has_time) time = real(n, R8P)
      n = n + 1
      self%series_times = [self%series_times, time]
      self%series_text = reshape([self%series_text, len(self%series_chars) + 1, len(text)], [2, n])
      self%series_chars = self%series_chars//text
   enddo
   error = 0_I4P
   contains
      subroutine skip_blanks
      !< Move c past blanks, tabs and line ends.
      do while (c <= len(buffer))
         if (index(' '//achar(9)//achar(10)//achar(13), buffer(c:c)) == 0) exit
         c = c + 1
      enddo
      endsubroutine skip_blanks

      logical function next_is(char)
      !< The character at c is char: move past it.
      character(1), intent(in) :: char !< Expected character.

      next_is = .false.
      if (c > len(buffer)) return
      next_is = buffer(c:c) == char
      if (next_is) c = c + 1
      endfunction next_is

      logical function json_string(string)
      !< A JSON string at c (escapes \" \\ \/ \n \t \r \b \f decoded): its value, c past the closing quote.
      character(len=:), allocatable, intent(out) :: string !< The string.

      string = ''
      json_string = .false.
      if (.not.next_is('"')) return
      do while (c <= len(buffer))
         if (buffer(c:c) == '"') then
            c = c + 1
            json_string = .true.
            return
         elseif (buffer(c:c) == achar(92) .and. c < len(buffer)) then
            c = c + 1
            select case(buffer(c:c))
            case('n')
               string = string//achar(10)
            case('t')
               string = string//achar(9)
            case('r')
               string = string//achar(13)
            case('b')
               string = string//achar(8)
            case('f')
               string = string//achar(12)
            case default
               string = string//buffer(c:c)
            endselect
         else
            string = string//buffer(c:c)
         endif
         c = c + 1
      enddo
      endfunction json_string

      logical function json_number(value)
      !< A JSON number at c: its value, c past it.
      real(R8P), intent(out) :: value !< The number.
      integer(I4P)           :: e     !< End of the number.

      json_number = .false.
      value = 0._R8P
      e = c
      do while (e <= len(buffer))
         if (index('+-0123456789.eE', buffer(e:e)) == 0) exit
         e = e + 1
      enddo
      if (e == c) return
      read(buffer(c:e-1), *, iostat=iostat) value
      if (iostat /= 0) return
      c = e
      json_number = .true.
      endfunction json_number
   endfunction read_series

   pure function json_escape(string) result(escaped)
   !< Return a string as the content of a JSON string: `"` and `\` escaped, control characters as `\u00XX`.
   character(*), intent(in)      :: string  !< Input string.
   character(len=:), allocatable :: escaped !< Escaped string.
   character(16), parameter      :: DIGITS='0123456789abcdef' !< Hexadecimal digits.
   integer                       :: i       !< Counter.
   integer                       :: k       !< Character code.

   escaped = ''
   do i=1, len(string)
      k = iachar(string(i:i))
      if (string(i:i) == '"' .or. k == 92) then
         escaped = escaped//achar(92)//string(i:i)
      elseif (k < 32) then
         escaped = escaped//achar(92)//'u00'//DIGITS(k/16+1:k/16+1)//DIGITS(mod(k,16)+1:mod(k,16)+1)
      else
         escaped = escaped//string(i:i)
      endif
   enddo
   endfunction json_escape

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
