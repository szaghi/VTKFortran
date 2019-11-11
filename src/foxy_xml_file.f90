!< FoXy XML file class.
module foxy_xml_file
!< FoXy XML file class.
use foxy_xml_tag, only : xml_tag
use penf

implicit none
private

type, public:: xml_file
  !< XML file class.
  !<
  !< @todo The "delete" facility is incomplete: nested tags are not taken into account. Better support will with the
  !< "dom" facility.
  private
  integer(I4P)               :: Nt = 0 !< Number of XML tags.
  type(xml_tag), allocatable :: tag(:) !< XML tags array.
  contains
    ! public methods
    procedure :: free       !< Free dynamic memory.
    procedure :: parse      !< Parse xml data from string or file.
    procedure :: content    !< Return tag content of tag named *name*.
    procedure :: stringify  !< Convert the whole file data into a string.
    procedure :: add_tag    !< Add tag to XML file.
    procedure :: delete_tag !< Add tag from XML file.
    final     :: finalize   !< Free dynamic memory when finalizing.
    ! private methods
    procedure, private :: parse_from_string !< Parse xml data from string.
endtype xml_file
contains
  ! public methods
  elemental subroutine free(self)
  !< Free dynamic memory.
  class(xml_file), intent(inout) :: self !< XML file.

  if (allocated(self%tag)) then
    call self%tag%free
    deallocate(self%tag)
    self%Nt = 0
  endif
  endsubroutine free

  subroutine finalize(file)
  !< Free dynamic memory when finalizing.
  type(xml_file), intent(inout) :: file !< XML file.

  call file%free
  endsubroutine finalize

  subroutine parse(self, string, filename)
  !< Parse xml data from string or file.
  !<
  !< @note Self data are free before trying to parse new xml data: all previously parsed data are lost.
  class(xml_file),        intent(inout) :: self     !< XML file.
  character(*), optional, intent(in)    :: string   !< String containing xml data.
  character(*), optional, intent(in)    :: filename !< File name containing xml data.
  character(len=:), allocatable         :: source   !< String containing xml data.

  call self%free
  if (present(string)) then
    call self%parse_from_string(source_string=string)
  elseif (present(filename)) then
    source = load_file_as_stream(filename=filename, fast_read=.true.)
    call self%parse_from_string(source_string=source)
  endif
  endsubroutine parse

  pure function content(self, name)
  !< Return tag content of tag named *name*.
  !<
  !< @note If there is no value, the *tag_content* string is returned empty, but allocated.
  class(xml_file), intent(in)   :: self    !< XML file.
  character(*),    intent(in)   :: name    !< Tag name.
  character(len=:), allocatable :: content !< Tag content.
  integer(I4P)                  :: t       !< Counter.

  if (allocated(content)) deallocate(content)
  if (self%Nt>0) then
    do t=1, self%Nt
      call self%tag(t)%get_content(name=name, content=content)
      if (allocated(content)) exit
    enddo
  endif
  if (.not.allocated(content)) content = ''
  endfunction content

  pure function stringify(self) result(string)
  !< Convert the whole file data into a string.
  class(xml_file), intent(in)   :: self       !< XML file.
  character(len=:), allocatable :: string     !< Output string containing the whole xml file.
  character(len=:), allocatable :: tag_string !< Output string containing the current tag.
  integer(I4P)                  :: t          !< Counter.

  string = ''
  if (self%Nt>0) then
    do t=1, self%Nt - 1
      tag_string = self%tag(t)%stringify()
      string = string//tag_string//new_line('a')
    enddo
    tag_string = self%tag(self%Nt)%stringify()
    string = string//tag_string
  endif
  endfunction stringify

  elemental subroutine add_tag(self, tag)
  !< Add tag to XML file.
  class(xml_file), intent(inout) :: self       !< XML file.
  type(xml_tag),   intent(in)    :: tag        !< XML tag.
  type(xml_tag), allocatable     :: tag_new(:) !< New (extended) tags array.

  if (self%Nt>0_I4P) then
    allocate(tag_new(1:self%Nt + 1))
    tag_new(1:self%Nt) = self%tag(1:self%Nt)
    tag_new(self%Nt + 1) = tag
  else
    allocate(tag_new(1:1))
    tag_new(1) = tag
  endif
  call move_alloc(from=tag_new, to=self%tag)
  self%Nt = self%Nt + 1
  endsubroutine add_tag

  elemental subroutine delete_tag(self, name)
  !< Delete tag from XML file.
  class(xml_file), intent(inout) :: self       !< XML file.
  character(*),    intent(in)    :: name       !< XML tag name.
  type(xml_tag), allocatable     :: tag_new(:) !< New (extended) tags array.
  integer(I4P)                   :: t          !< Counter.

  if (self%Nt>0_I4P) then
    do t=1, self%Nt
      if (name==self%tag(t)%name()) then
        allocate(tag_new(1:self%Nt - 1))
        if (t==1) then
          tag_new(t:) = self%tag(t+1:)
        elseif (t==self%Nt) then
          tag_new(:t-1) = self%tag(:t-1)
        else
          tag_new(:t-1) = self%tag(:t-1)
          tag_new(t:) = self%tag(t+1:)
        endif
        call move_alloc(from=tag_new, to=self%tag)
        self%Nt = self%Nt - 1
        exit
      endif
    enddo
  endif
  endsubroutine delete_tag

  ! private methods
  subroutine parse_from_string(self, source_string)
  !< Parse xml data from string.
  class(xml_file), intent(inout) :: self          !< XML file.
  character(*),    intent(in)    :: source_string !< String containing xml data.
  type(xml_tag)                  :: tag           !< Dummy xml tag.
  integer(I4P)                   :: tstart        !< Counter for tracking string parsing.
  integer(I4P)                   :: tend          !< Counter for tracking string parsing.

  tstart = 1
  tend = 0
  do while(tstart<len(source_string))
    call tag%free
    call tag%parse(source=source_string(tstart:), tend=tend)
    if (tend==0) exit
    if (tag%is_parsed()) call self%add_tag(tag)
    tstart = tstart + tend
  enddo
  endsubroutine parse_from_string

  ! non TBP
  function load_file_as_stream(filename, delimiter_start, delimiter_end, fast_read, iostat, iomsg) result(stream)
  !< Load file contents and store as single characters stream.
  character(*),           intent(in)  :: filename        !< File name.
  character(*), optional, intent(in)  :: delimiter_start !< Delimiter from which start the stream.
  character(*), optional, intent(in)  :: delimiter_end   !< Delimiter to which end the stream.
  logical,      optional, intent(in)  :: fast_read       !< Flag for activating efficient reading with one single read.
  integer(I4P), optional, intent(out) :: iostat          !< IO error.
  character(*), optional, intent(out) :: iomsg           !< IO error message.
  character(len=:), allocatable       :: stream          !< Output string containing the file data as a single stream.
  logical                             :: is_file         !< Flag for inquiring the presence of the file.
  integer(I4P)                        :: unit            !< Unit file.
  integer(I4P)                        :: iostatd         !< IO error.
  character(500)                      :: iomsgd          !< IO error message.
  character(1)                        :: c1              !< Single character.
  character(len=:), allocatable       :: string          !< Dummy string.
  logical                             :: cstart          !< Flag for stream capturing trigging.
  logical                             :: cend            !< Flag for stream capturing trigging.
  logical                             :: fast            !< Flag for activating efficient reading with one single read.
  integer(I4P)                        :: filesize        !< Size of the file for fast reading.

  fast = .false. ; if (present(fast_read)) fast = fast_read
  ! inquire file existance
  inquire(file=adjustl(trim(filename)), exist=is_file, iostat=iostatd, iomsg=iomsgd)
  if (.not.is_file) then
    if (present(iostat)) iostat = iostatd
    if (present(iomsg )) iomsg  = iomsgd
    return
  endif
  ! open file
  open(newunit=unit, file=adjustl(trim(filename)), access='STREAM', form='UNFORMATTED', iostat=iostatd, iomsg=iomsgd)
  if (iostatd/=0) then
    if (present(iostat)) iostat = iostatd
    if (present(iomsg )) iomsg  = iomsgd
    return
  endif
  ! loadg data
  stream = ''
  if (present(delimiter_start).and.present(delimiter_end)) then
    ! load only data inside delimiter_start and delimiter_end
    string = ''
    Main_Read_Loop: do
      read(unit=unit, iostat=iostatd, iomsg=iomsgd, end=10)c1
      if (c1==delimiter_start(1:1)) then
        cstart = .true.
        string = c1
        Start_Read_Loop: do while(len(string)<len(delimiter_start))
          read(unit=unit, iostat=iostatd, iomsg=iomsgd, end=10)c1
          string = string//c1
          if (.not.(index(string=delimiter_start, substring=string)>0)) then
            cstart = .false.
            exit Start_Read_Loop
          endif
        enddo Start_Read_Loop
        if (cstart) then
          cend = .false.
          stream = string
          do while(.not.cend)
            read(unit=unit, iostat=iostatd, iomsg=iomsgd, end=10)c1
            if (c1==delimiter_end(1:1)) then ! maybe the end
              string = c1
              End_Read_Loop: do while(len(string)<len(delimiter_end))
                read(unit=unit, iostat=iostatd, iomsg=iomsgd, end=10)c1
                string = string//c1
                if (.not.(index(string=delimiter_end, substring=string)>0)) then
                  stream = stream//string
                  exit End_Read_Loop
                elseif (len(string)==len(delimiter_end)) then
                  cend = .true.
                  stream = stream//string
                  exit Main_Read_Loop
                endif
              enddo End_Read_Loop
            else
              stream = stream//c1
            endif
          enddo
        endif
      endif
    enddo Main_Read_Loop
  else
    ! load all data
    if (fast) then
      ! load fast
      inquire(file=adjustl(trim(filename)), size=filesize, iostat=iostatd, iomsg=iomsgd)
      if (iostatd==0) then
        if (allocated(stream)) deallocate(stream)
        allocate(character(len=filesize):: stream)
        read(unit=unit, iostat=iostatd, iomsg=iomsgd, end=10)stream
      endif
    else
      ! load slow, one character loop
      Read_Loop: do
        read(unit=unit,iostat=iostatd,iomsg=iomsgd,end=10)c1
        stream = stream//c1
      enddo Read_Loop
    endif
  endif
  10 close(unit)
  if (present(iostat)) iostat = iostatd
  if (present(iomsg))  iomsg  = iomsgd
  endfunction load_file_as_stream
endmodule foxy_xml_file
