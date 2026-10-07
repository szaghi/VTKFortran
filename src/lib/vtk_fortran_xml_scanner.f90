!< XML scanner: index the elements of an XML file without loading it.
module vtk_fortran_xml_scanner
!< XML scanner: index the elements of an XML file without loading it.
!<
!< The scanner reads the file once, in chunks, and records for each element its name, attributes, position in the hierarchy
!< and the positions (in bytes, from 1) of its content in the file; the content itself is never stored, so the memory used
!< does not depend on the data held in the file. The content is read afterwards, on demand, from the recorded positions.
!< The scan can stop at the start tag of a given element (e.g. `AppendedData` of VTK files, whose content is raw binary).
!<
!< The module depends only on PENF: it is meant to be moved into FoXy.
!<
!< Supported XML: elements, attributes (single or double quoted, with the predefined entities), comments, processing
!< instructions and declarations (skipped). CDATA sections are not supported.
use penf

implicit none
private
public :: xml_attribute
public :: xml_element
public :: xml_scanner

integer(I8P), parameter :: chunk_size=65536_I8P !< Size of the chunks read from the file.

type :: xml_attribute
  !< Attribute of an XML element: name and (entities decoded) value.
  character(len=:), allocatable :: name  !< Attribute name.
  character(len=:), allocatable :: value !< Attribute value.
endtype xml_attribute

type :: xml_element
  !< Element of an XML file: name, attributes, hierarchy and positions of its content in the file.
  !<
  !< The content of an element spans `content_start:content_end`; its text, the part before the first child element (where
  !< VTK puts the data of a DataArray), spans `content_start:text_end`. Both are empty for a self closing element.
  character(len=:),    allocatable :: name                    !< Element name.
  type(xml_attribute), allocatable :: attribute(:)            !< Attributes.
  integer(I4P)                     :: attributes_number=0     !< Number of attributes.
  integer(I4P)                     :: parent=0                !< Index of the parent element (0 for the root).
  integer(I4P)                     :: level=0                 !< Depth in the hierarchy (1 for the root).
  integer(I4P),        allocatable :: child(:)                !< Indexes of the children elements.
  integer(I4P)                     :: children_number=0       !< Number of children elements.
  integer(I8P)                     :: content_start=0_I8P     !< First byte of the content (after the start tag).
  integer(I8P)                     :: text_end=-1_I8P         !< Last byte of the text (before the first child or end tag).
  integer(I8P)                     :: content_end=-1_I8P      !< Last byte of the content (before the end tag).
  logical                          :: is_self_closing=.false. !< The element is self closing, `<name .../>`.
  contains
    procedure, pass(self) :: get_attribute !< Return the value of an attribute.
    procedure, pass(self) :: has_attribute !< Return .true. if an attribute is present.
endtype xml_element

type :: xml_scanner
  !< Index of the elements of an XML file.
  type(xml_element), allocatable :: element(:)         !< Elements, in the order of their start tags.
  integer(I4P)                   :: elements_number=0  !< Number of elements.
  logical                        :: is_stopped=.false. !< The scan stopped at the start tag of the `stop_at` element.
  contains
    procedure, pass(self) :: find_child !< Return the index of the n-th child element with a given name.
    procedure, pass(self) :: free       !< Free the index.
    procedure, pass(self) :: scan       !< Scan an XML file.
    procedure, pass(self), private :: add_element !< Add an element.
endtype xml_scanner

type :: stream_buffer
  !< Buffered reader of a file opened for stream access.
  integer(I4P)                  :: unit=0          !< File unit.
  integer(I8P)                  :: file_size=0_I8P !< File size in bytes.
  integer(I8P)                  :: first=1_I8P     !< Position in the file of the first character of the buffer.
  integer(I8P)                  :: length=0_I8P    !< Number of valid characters in the buffer.
  character(len=:), allocatable :: chars           !< Buffer.
  contains
    procedure, pass(self) :: char_at !< Return the character at a position of the file.
    procedure, pass(self) :: find    !< Return the position of the next occurrence of a character.
    procedure, pass(self) :: load    !< Load the chunk starting at a position of the file.
endtype stream_buffer

contains
  ! xml_element methods
  subroutine get_attribute(self, name, value, is_found)
  !< Return the value of an attribute (empty if the attribute is not present).
  class(xml_element),            intent(in)            :: self     !< XML element.
  character(*),                  intent(in)            :: name     !< Attribute name.
  character(len=:), allocatable, intent(out)           :: value    !< Attribute value.
  logical,                       intent(out), optional :: is_found !< The attribute is present.
  integer(I4P)                                         :: a        !< Counter.

  value = ''
  if (present(is_found)) is_found = .false.
  do a=1, self%attributes_number
    if (self%attribute(a)%name == name) then
      value = self%attribute(a)%value
      if (present(is_found)) is_found = .true.
      return
    endif
  enddo
  endsubroutine get_attribute

  pure function has_attribute(self, name) result(is_present)
  !< Return .true. if an attribute is present.
  class(xml_element), intent(in) :: self       !< XML element.
  character(*),       intent(in) :: name       !< Attribute name.
  logical                        :: is_present !< Inquire result.
  integer(I4P)                   :: a          !< Counter.

  is_present = .false.
  do a=1, self%attributes_number
    if (self%attribute(a)%name == name) then
      is_present = .true.
      return
    endif
  enddo
  endfunction has_attribute

  ! xml_scanner methods
  pure function find_child(self, parent, name, n) result(id)
  !< Return the index of the n-th (default first) child element with a given name, 0 if there is none.
  !<
  !< With `parent=0` the root elements are searched.
  class(xml_scanner), intent(in)           :: self   !< XML scanner.
  integer(I4P),       intent(in)           :: parent !< Index of the parent element, 0 for the root elements.
  character(*),       intent(in)           :: name   !< Element name.
  integer(I4P),       intent(in), optional :: n      !< Occurrence of the element among the children with that name.
  integer(I4P)                             :: id     !< Index of the element, 0 if not found.
  integer(I4P)                             :: n_     !< Occurrence, local variable.
  integer(I4P)                             :: found  !< Occurrences found.
  integer(I4P)                             :: c      !< Counter.

  id = 0
  n_ = 1 ; if (present(n)) n_ = n
  found = 0
  if (parent == 0) then
    do c=1, self%elements_number
      if (self%element(c)%parent /= 0) cycle
      if (self%element(c)%name /= name) cycle
      found = found + 1
      if (found == n_) then
        id = c
        return
      endif
    enddo
  elseif (parent > 0 .and. parent <= self%elements_number) then
    do c=1, self%element(parent)%children_number
      if (self%element(self%element(parent)%child(c))%name /= name) cycle
      found = found + 1
      if (found == n_) then
        id = self%element(parent)%child(c)
        return
      endif
    enddo
  endif
  endfunction find_child

  elemental subroutine free(self)
  !< Free the index.
  class(xml_scanner), intent(inout) :: self !< XML scanner.

  if (allocated(self%element)) deallocate(self%element)
  self%elements_number = 0
  self%is_stopped = .false.
  endsubroutine free

  subroutine scan(self, filename, error, stop_at)
  !< Scan an XML file, indexing its elements.
  !<
  !< The error is 0 on success, 1 if the file cannot be read, 2 if the XML is malformed (unterminated tag, comment or
  !< attribute value, end tag not matching the start tag, unclosed element).
  class(xml_scanner), intent(inout)        :: self     !< XML scanner.
  character(*),       intent(in)           :: filename !< File name.
  integer(I4P),       intent(out)          :: error    !< Error status.
  character(*),       intent(in), optional :: stop_at  !< Stop the scan after the start tag of the element with this name.
  type(stream_buffer)                      :: buffer   !< File buffer.
  integer(I4P),       allocatable          :: stack(:) !< Indexes of the open elements.
  integer(I4P),       allocatable          :: grown(:) !< Grown stack.
  integer(I4P)                             :: depth    !< Number of open elements.
  integer(I8P)                             :: pos      !< Current position.
  integer(I8P)                             :: lt       !< Position of the next '<'.
  integer(I8P)                             :: gt       !< Position of the '>' closing the tag.
  character(len=:), allocatable            :: tag      !< Text of the tag, between '<' and '>'.
  integer(I4P)                             :: iostat   !< IO status.
  integer(I4P)                             :: id       !< Index of the new element.

  call self%free
  error = 1
  open(newunit=buffer%unit, file=filename, access='stream', form='unformatted', action='read', status='old', &
       iostat=iostat)
  if (iostat /= 0) return
  inquire(unit=buffer%unit, size=buffer%file_size)
  allocate(character(len=chunk_size) :: buffer%chars)
  allocate(stack(1:16))
  depth = 0
  pos = 1_I8P
  error = 2
  scan_loop: do
    lt = buffer%find(c='<', pos=pos)
    if (lt == 0_I8P) exit scan_loop
    ! a tag ends the text of the open element
    if (depth > 0) then
      if (self%element(stack(depth))%text_end < 0_I8P) self%element(stack(depth))%text_end = lt - 1_I8P
    endif
    if (buffer%char_at(lt+1_I8P) == '!' .and. buffer%char_at(lt+2_I8P) == '-' .and. buffer%char_at(lt+3_I8P) == '-') then
      ! comment: skip to '-->'
      gt = lt + 4_I8P
      do
        gt = buffer%find(c='>', pos=gt)
        if (gt == 0_I8P) then
          close(buffer%unit)
          return
        endif
        if (gt-2_I8P > lt+3_I8P .and. buffer%char_at(gt-1_I8P) == '-' .and. buffer%char_at(gt-2_I8P) == '-') exit
        gt = gt + 1_I8P
      enddo
      pos = gt + 1_I8P
      cycle scan_loop
    endif
    call read_tag(buffer=buffer, lt=lt, tag=tag, gt=gt)
    if (gt == 0_I8P .or. len(tag) == 0) exit scan_loop
    pos = gt + 1_I8P
    select case(tag(1:1))
    case('?', '!')
      ! processing instruction or declaration: skipped
    case('/')
      ! end tag: it must close the open element
      if (depth == 0) exit scan_loop
      if (self%element(stack(depth))%name /= trim(adjustl(tag(2:)))) exit scan_loop
      self%element(stack(depth))%content_end = lt - 1_I8P
      depth = depth - 1
    case default
      ! start tag
      call self%add_element(tag=tag, id=id, iostat=iostat)
      if (iostat /= 0) exit scan_loop
      if (depth > 0) then
        self%element(id)%parent = stack(depth)
        call add_child(element=self%element(stack(depth)), child=id)
      endif
      self%element(id)%level = depth + 1
      if (.not.self%element(id)%is_self_closing) then
        self%element(id)%content_start = gt + 1_I8P
        if (depth == size(stack)) then
          allocate(grown(1:2*depth))
          grown(1:depth) = stack(1:depth)
          call move_alloc(from=grown, to=stack)
        endif
        depth = depth + 1
        stack(depth) = id
      endif
      if (present(stop_at)) then
        if (self%element(id)%name == stop_at) then
          self%is_stopped = .true.
          exit scan_loop
        endif
      endif
    endselect
  enddo scan_loop
  if (self%is_stopped .or. (depth == 0 .and. self%elements_number > 0 .and. lt == 0_I8P)) error = 0
  close(buffer%unit)
  endsubroutine scan

  subroutine add_element(self, tag, id, iostat)
  !< Add an element, parsing its start tag (the text between '<' and '>').
  class(xml_scanner), intent(inout) :: self     !< XML scanner.
  character(*),       intent(in)    :: tag      !< Start tag text.
  integer(I4P),       intent(out)   :: id       !< Index of the new element.
  integer(I4P),       intent(out)   :: iostat   !< Status: non-zero if the tag is malformed.
  type(xml_element),  allocatable   :: grown(:) !< Grown elements list.
  integer(I4P)                      :: last     !< Last character of the tag, without the self closing '/'.
  integer(I4P)                      :: c        !< Character position.
  integer(I4P)                      :: s        !< Start of a token.
  character(len=1)                  :: quote    !< Quote of an attribute value.
  character(len=:), allocatable     :: aname    !< Attribute name.

  iostat = 1
  if (.not.allocated(self%element)) allocate(self%element(1:64))
  if (self%elements_number == size(self%element)) then
    allocate(grown(1:2*self%elements_number))
    grown(1:self%elements_number) = self%element(1:self%elements_number)
    call move_alloc(from=grown, to=self%element)
  endif
  self%elements_number = self%elements_number + 1
  id = self%elements_number
  last = len_trim(tag)
  if (last == 0) return
  self%element(id)%is_self_closing = tag(last:last) == '/'
  if (self%element(id)%is_self_closing) last = last - 1
  ! name
  c = 1
  do while (c <= last)
    if (is_space(tag(c:c))) exit
    c = c + 1
  enddo
  if (c == 1) return
  self%element(id)%name = tag(1:c-1)
  ! attributes: name="value" or name='value'
  do
    c = skip_spaces(tag(1:last), c)
    if (c > last) exit
    s = c
    do while (c <= last)
      if (tag(c:c) == '=' .or. is_space(tag(c:c))) exit
      c = c + 1
    enddo
    if (c == s) return
    aname = tag(s:c-1)
    c = skip_spaces(tag(1:last), c)
    if (c > last) return
    if (tag(c:c) /= '=') return
    c = skip_spaces(tag(1:last), c+1)
    if (c > last) return
    quote = tag(c:c)
    if (quote /= '"' .and. quote /= "'") return
    s = c + 1
    c = index(tag(s:last), quote)
    if (c == 0) return
    c = s + c - 1
    call add_attribute(element=self%element(id), name=aname, value=decode_entities(tag(s:c-1)))
    c = c + 1
  enddo
  iostat = 0
  endsubroutine add_element

  ! stream_buffer methods
  function char_at(self, pos) result(c)
  !< Return the character at a position of the file (a blank beyond its end).
  class(stream_buffer), intent(inout) :: self !< File buffer.
  integer(I8P),         intent(in)    :: pos  !< Position in the file.
  character(len=1)                    :: c    !< Character.

  c = ' '
  if (pos < 1_I8P .or. pos > self%file_size) return
  if (pos < self%first .or. pos >= self%first + self%length) call self%load(pos=pos)
  c = self%chars(pos-self%first+1_I8P:pos-self%first+1_I8P)
  endfunction char_at

  function find(self, c, pos) result(found)
  !< Return the position of the next occurrence of a character from a position of the file, 0 if there is none.
  class(stream_buffer), intent(inout) :: self  !< File buffer.
  character(len=1),     intent(in)    :: c     !< Character searched.
  integer(I8P),         intent(in)    :: pos   !< Position in the file where the search starts.
  integer(I8P)                        :: found !< Position of the character, 0 if not found.
  integer(I8P)                        :: p     !< Current position.
  integer(I8P)                        :: i     !< Position in the buffer.

  found = 0_I8P
  p = pos
  do while (p >= 1_I8P .and. p <= self%file_size)
    if (p < self%first .or. p >= self%first + self%length) call self%load(pos=p)
    i = index(self%chars(p-self%first+1_I8P:self%length), c, kind=I8P)
    if (i > 0_I8P) then
      found = p + i - 1_I8P
      return
    endif
    p = self%first + self%length
  enddo
  endfunction find

  subroutine load(self, pos)
  !< Load the chunk starting at a position of the file.
  class(stream_buffer), intent(inout) :: self !< File buffer.
  integer(I8P),         intent(in)    :: pos  !< Position in the file.

  self%first = pos
  self%length = min(chunk_size, self%file_size - pos + 1_I8P)
  read(self%unit, pos=pos) self%chars(1:self%length)
  endsubroutine load

  ! private non type-bound procedures
  subroutine read_tag(buffer, lt, tag, gt)
  !< Read the text of a tag starting at '<', up to the '>' closing it (outside quoted attribute values).
  type(stream_buffer),           intent(inout) :: buffer !< File buffer.
  integer(I8P),                  intent(in)    :: lt     !< Position of '<'.
  character(len=:), allocatable, intent(out)   :: tag    !< Text of the tag, between '<' and '>'.
  integer(I8P),                  intent(out)   :: gt     !< Position of '>', 0 if the tag is not terminated.
  character(len=:), allocatable                :: grown  !< Grown tag.
  character(len=1)                             :: c      !< Current character.
  character(len=1)                             :: quote  !< Open quote, blank if none.
  integer(I8P)                                 :: p      !< Current position.
  integer                                      :: n      !< Length of the tag.

  allocate(character(len=256) :: tag)
  n = 0
  quote = ' '
  gt = 0_I8P
  p = lt + 1_I8P
  do while (p <= buffer%file_size)
    c = buffer%char_at(p)
    if (quote == ' ') then
      if (c == '>') then
        gt = p
        exit
      endif
      if (c == '"' .or. c == "'") quote = c
    elseif (c == quote) then
      quote = ' '
    endif
    if (n == len(tag)) then
      allocate(character(len=2*n) :: grown)
      grown(1:n) = tag
      call move_alloc(from=grown, to=tag)
    endif
    n = n + 1
    tag(n:n) = c
    p = p + 1_I8P
  enddo
  tag = tag(1:n)
  endsubroutine read_tag

  subroutine add_attribute(element, name, value)
  !< Add an attribute to an element.
  type(xml_element), intent(inout) :: element  !< XML element.
  character(*),      intent(in)    :: name     !< Attribute name.
  character(*),      intent(in)    :: value    !< Attribute value.
  type(xml_attribute), allocatable :: grown(:) !< Grown attributes list.

  ! allocated here, on the element dummy: allocating it through the scanner (self%element(id)%attribute) is miscompiled by
  ! gfortran 16 with -fcheck=bounds
  if (.not.allocated(element%attribute)) allocate(element%attribute(1:4))
  if (element%attributes_number == size(element%attribute)) then
    allocate(grown(1:max(4, 2*element%attributes_number)))
    grown(1:element%attributes_number) = element%attribute(1:element%attributes_number)
    call move_alloc(from=grown, to=element%attribute)
  endif
  element%attributes_number = element%attributes_number + 1
  element%attribute(element%attributes_number)%name = name
  element%attribute(element%attributes_number)%value = value
  endsubroutine add_attribute

  subroutine add_child(element, child)
  !< Add a child index to an element.
  type(xml_element), intent(inout) :: element  !< XML element.
  integer(I4P),      intent(in)    :: child    !< Index of the child.
  integer(I4P),      allocatable   :: grown(:) !< Grown children list.

  if (.not.allocated(element%child)) allocate(element%child(1:4))
  if (element%children_number == size(element%child)) then
    allocate(grown(1:2*element%children_number))
    grown(1:element%children_number) = element%child(1:element%children_number)
    call move_alloc(from=grown, to=element%child)
  endif
  element%children_number = element%children_number + 1
  element%child(element%children_number) = child
  endsubroutine add_child

  pure function decode_entities(string) result(decoded)
  !< Decode the predefined XML entities of a string (&lt; &gt; &amp; &quot; &apos;).
  character(*), intent(in)      :: string  !< String.
  character(len=:), allocatable :: decoded !< Decoded string.
  integer                       :: c       !< Character position.
  integer                       :: e       !< End of an entity.

  if (index(string, '&') == 0) then
    decoded = string
    return
  endif
  decoded = ''
  c = 1
  do while (c <= len(string))
    if (string(c:c) == '&') then
      e = index(string(c:), ';')
      if (e > 0) then
        select case(string(c:c+e-1))
        case('&lt;')
          decoded = decoded//'<'
        case('&gt;')
          decoded = decoded//'>'
        case('&amp;')
          decoded = decoded//'&'
        case('&quot;')
          decoded = decoded//'"'
        case('&apos;')
          decoded = decoded//"'"
        case default
          decoded = decoded//string(c:c+e-1)
        endselect
        c = c + e
        cycle
      endif
    endif
    decoded = decoded//string(c:c)
    c = c + 1
  enddo
  endfunction decode_entities

  elemental function is_space(c) result(is_blank)
  !< Return .true. if a character is XML white space (blank, tab, line feed, carriage return).
  character(len=1), intent(in) :: c        !< Character.
  logical                      :: is_blank !< Inquire result.

  is_blank = c == ' ' .or. c == achar(9) .or. c == achar(10) .or. c == achar(13)
  endfunction is_space

  pure function skip_spaces(string, c) result(next)
  !< Return the position of the first character not white space from a position of a string (len+1 if none).
  character(*), intent(in) :: string !< String.
  integer(I4P), intent(in) :: c      !< Start position.
  integer(I4P)             :: next   !< Position of the first character not white space.

  next = c
  do while (next <= len(string))
    if (.not.is_space(string(next:next))) exit
    next = next + 1
  enddo
  endfunction skip_spaces
endmodule vtk_fortran_xml_scanner
