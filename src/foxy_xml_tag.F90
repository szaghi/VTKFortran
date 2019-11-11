!< FoXy XML tag class.
module foxy_xml_tag
!< FoXy XML tag class.
use penf
use stringifor

implicit none
private
public :: xml_tag

type :: xml_tag
  !< XML tag class.
  !<
  !< A valid XML tag must have the following syntax for a tag without a content (with only attributes):
  !<```xml
  !<   <Tag_Name att#1_Name="att#1_val" att#2_Name="att#2_val"... att#Nt_Name="att#Nt_val"/>
  !<```
  !< while a tag with a content must have the following syntax:
  !<```xml
  !<   <Tag_Name att#1_Name="att#1_val" att#2_Name="att#2_val"... att#Nt_Name="att#Nt_val">Tag_Content</Tag_Name>
  !<```
  !<
  !< It is worth noting that the syntax is case sensitive and that the attributes are optional. Each attribute name must be followed
  !< by '="' without any additional white spaces and its value must be termined by '"'. Each attribute is separated by one or more
  !< white spaces.
  private
  type(string)              :: tag_name                !< Tag name.
  type(string)              :: tag_content             !< Tag content.
  type(string), allocatable :: attribute(:,:)          !< Attributes names/values pairs, [1:2, 1:].
  integer(I4P)              :: attributes_number=0     !< Number of defined attributes.
  integer(I4P)              :: indent=0                !< Number of indent-white-spaces.
  logical                   :: is_self_closing=.false. !< Self closing tag flag.
  contains
    ! public methods
    generic               :: add_attributes =>        &
                             add_single_attribute,    &
                             add_multiple_attributes, &
                             add_stream_attributes       !< Add attributes name/value pairs.
    procedure, pass(self) :: attributes                  !< Return attributes name/value pairs as string.
    procedure, pass(self) :: get_content                 !< Return tag content.
    generic               :: delete_attributes =>     &
                             delete_single_attribute, &
                             delete_multiple_attributes  !< Delete attributes name/value pairs.
    procedure, pass(self) :: delete_content              !< Delete tag conent.
    procedure, pass(self) :: end_tag                     !< Return `</tag_name>` end tag.
    procedure, pass(self) :: free                        !< Free dynamic memory.
    procedure, pass(self) :: is_attribute_present        !< Return .true. it the queried attribute name is defined.
    procedure, pass(self) :: is_parsed                   !< Check is tag is correctly parsed, i.e. its *tag_name* is allocated.
    procedure, pass(self) :: name                        !< Return tag name.
    procedure, pass(self) :: parse                       !< Parse the tag contained into a source string.
    procedure, pass(self) :: self_closing_tag            !< Return `<tag_name.../>` self closing tag.
    procedure, pass(self) :: set                         !< Set tag data.
    procedure, pass(self) :: start_tag                   !< Return `<tag_name...>` start tag.
    procedure, pass(self) :: stringify                   !< Convert the whole tag into a string.
    procedure, pass(self) :: write => write_tag          !< Write tag to unit file.
    generic               :: assignment(=) => assign_tag !< Assignment operator overloading.
    ! private methods
    procedure, pass(self), private :: add_single_attribute       !< Add one attribute name/value pair.
    procedure, pass(self), private :: add_multiple_attributes    !< Add list of attributes name/value pairs.
    procedure, pass(self), private :: add_stream_attributes      !< Add list of attributes name/value pairs passed as stream.
    procedure, pass(self), private :: alloc_attributes           !< Allocate (prepare for filling) dynamic memory of attributes.
    procedure, pass(self), private :: delete_single_attribute    !< Delete one attribute name/value pair.
    procedure, pass(self), private :: delete_multiple_attributes !< Delete list of attributes name/value pairs.
    procedure, pass(self), private :: get                        !< Get the tag value and attributes from source.
    procedure, pass(self), private :: get_value                  !< Get the tag value from source after tag_name has been set.
    procedure, pass(self), private :: get_attributes             !< Get the attributes values from source.
    procedure, pass(self), private :: parse_tag_name             !< Parse the tag name contained into a string.
    procedure, pass(self), private :: parse_attributes_names     !< Parse the tag attributes names contained into a string.
    procedure, pass(self), private :: search                     !< Search tag named *tag_name* into a string.
    ! operators
    procedure, pass(lhs), private :: assign_tag !< Assignment between two tags.
#if (__GNUC__ < 9)
    final                         :: finalize   !< Free dynamic memory when finalizing.
#endif
endtype xml_tag

interface xml_tag
  !< Overload *xml_tag* with creator procedures.
  module procedure create_tag_flat, create_tag_nested
endinterface
contains
  ! creator procedures overloading *xml_tag* name
  pure function create_tag_flat(name, attribute, attributes, attributes_stream, sanitize_attributes_value, content, indent, &
                                is_content_indented, is_self_closing) result(tag)
  !< Return an instance of xml tag.
  !<
  !< Attributes are passed by array.
  character(*), intent(in)           :: name                      !< Tag name.
  character(*), intent(in), optional :: attribute(1:)             !< Attribute name/value pair [1:2].
  character(*), intent(in), optional :: attributes(1:,1:)         !< Attributes list of name/value pairs [1:2,1:].
  character(*), intent(in), optional :: attributes_stream         !< Attributes list of name/value pairs as single stream.
  logical,      intent(in), optional :: sanitize_attributes_value !< Sanitize attributes value.
  character(*), intent(in), optional :: content                   !< Tag value.
  integer(I4P), intent(in), optional :: indent                    !< Number of indent-white-spaces.
  logical,      intent(in), optional :: is_content_indented       !< Activate content indentation.
  logical,      intent(in), optional :: is_self_closing           !< The tag is self closing.
  type(xml_tag)                      :: tag                       !< XML tag.

  call tag%set(name=name, attribute=attribute, attributes=attributes, attributes_stream=attributes_stream, &
               sanitize_attributes_value=sanitize_attributes_value, content=content,                       &
               indent=indent, is_content_indented=is_content_indented, is_self_closing=is_self_closing)
  endfunction create_tag_flat

  pure function create_tag_nested(name, content, attribute, attributes, attributes_stream, sanitize_attributes_value, indent, &
                                  is_content_indented) result(tag)
  !< Return an instance of xml tag with value being a nested tag.
  !<
  !< Attributes are passed by array.
  character(*),  intent(in)           :: name                      !< Tag name.
  type(xml_tag), intent(in)           :: content                   !< Tag value as nested tag..
  character(*),  intent(in), optional :: attribute(1:)             !< Attribute name/value pair [1:2].
  character(*),  intent(in), optional :: attributes(1:,1:)         !< Attributes list of name/value pairs [1:2,1:].
  character(*),  intent(in), optional :: attributes_stream         !< Attributes list of name/value pairs as single stream.
  logical,       intent(in), optional :: sanitize_attributes_value !< Sanitize attributes value.
  integer(I4P),  intent(in), optional :: indent                    !< Number of indent-white-spaces.
  logical,       intent(in), optional :: is_content_indented       !< Activate value indentation.
  type(xml_tag)                       :: tag                       !< XML tag.

  call tag%set(name=name, attribute=attribute, attributes=attributes, content=content%stringify(),                      &
               sanitize_attributes_value=sanitize_attributes_value, attributes_stream=attributes_stream, indent=indent, &
               is_content_indented=is_content_indented)
  endfunction create_tag_nested

  ! public methods
  pure function attributes(self) result(att_)
  !< Return attributes name/value pairs as string.
  class(xml_tag), intent(in)    :: self !< XML tag.
  character(len=:), allocatable :: att_ !< The attributes string.
  integer(I4P)                  :: a    !< Counter.

  if (self%attributes_number>0) then
    att_ = ''
    do a=1, self%attributes_number
      att_ = att_//' '//self%attribute(1, a)//'="'//self%attribute(2, a)//'"'
    enddo
    att_ = trim(adjustl(att_))
  endif
  endfunction attributes

  pure subroutine get_content(self, name, content)
  !< Return tag content of self (or its nested tags) if named *name*.
  !<
  !< @note If there is no value, the *content* string is returned deallocated.
  class(xml_tag),                intent(in)  :: self    !< XML tag.
  character(*),                  intent(in)  :: name    !< Searched tag name.
  character(len=:), allocatable, intent(out) :: content !< Tag content.
  type(xml_tag)                              :: tag     !< Dummy XML tag.

  if (allocated(content)) deallocate(content)
  if (self%tag_name%is_allocated()) then
    if (self%tag_name==name) then
      if (self%tag_content%is_allocated()) content = self%tag_content%chars()
    else
      if (self%tag_content%is_allocated()) then
        call tag%search(tag_name=name, source=self%tag_content%chars())
        if (tag%tag_content%is_allocated()) content = tag%tag_content%chars()
      endif
    endif
  endif
  endsubroutine get_content

  pure function end_tag(self, is_indented) result(tag_)
  !< Return `</tag_name>` end tag.
  class(xml_tag), intent(in)           :: self        !< XML tag.
  logical,        intent(in), optional :: is_indented !< Activate content indentation.
  character(len=:), allocatable        :: tag_        !< The end tag string.

  tag_ = '</'//self%tag_name//'>'
  if (present(is_indented)) then
    if (is_indented) tag_ = repeat(' ', self%indent)//tag_
  endif
  endfunction end_tag

  elemental subroutine free(self)
  !< Free dynamic memory.
  class(xml_tag), intent(inout) :: self !< XML tag.

  call self%tag_name%free
  call self%tag_content%free
  if (allocated(self%attribute)) then
    call self%attribute%free
    deallocate(self%attribute)
  endif
  self%attributes_number = 0
  self%indent = 0
  self%is_self_closing = .false.
  endsubroutine free

  elemental function is_parsed(self)
  !< Check is tag is correctly parsed, i.e. its *tag_name* is allocated.
  class(xml_tag), intent(in) :: self      !< XML tag.
  logical                    :: is_parsed !< Result of check.

  is_parsed = self%tag_name%is_allocated()
  endfunction is_parsed

  pure function name(self)
  !< Return tag name.
  class(xml_tag), intent(in)    :: self !< XML tag.
  character(len=:), allocatable :: name !< XML tag name.

  name = self%tag_name%chars()
  endfunction name

  elemental subroutine parse(self, source, tstart, tend)
  !< Parse the tag contained into a source string.
  !<
  !< It is assumed that the first tag contained into the source string is parsed, the others eventually present are omitted.
  !< Valid syntax are:
  !< + `<tag_name att1="att1 val" att2="att2 val"...>...</tag_name>`
  !< + `<tag_name att1="att1 val" att2="att2 val".../>`
  !< @note Inside the attributes value the symbols `<` and `>` are not allowed.
  class(xml_tag),         intent(inout) :: self      !< XML tag.
  character(*),           intent(in)    :: source    !< String containing the input.
  integer(I4P), optional, intent(out)   :: tstart    !< Starting index of tag inside the string.
  integer(I4P), optional, intent(out)   :: tend      !< Ending index of tag inside the string.
  integer(I4P)                          :: tstartd   !< Starting index of tag inside the string.
  integer(I4P)                          :: tendd     !< Ending index of tag inside the string.

  tstartd = 0
  tendd   = 0
  call self%parse_tag_name(source=source, tstart=tstartd, tend=tendd)
  if (self%tag_name%is_allocated()) then
    if (index(string=source(tstartd:tendd), substring='=')>0) call self%parse_attributes_names(source=source(tstartd:tendd))
    if (index(string=source, substring='</'//self%tag_name//'>')>0) &
      tendd = index(string=source, substring='</'//self%tag_name//'>') + len('</'//self%tag_name//'>') - 1
    call self%get(source=source(tstartd:tendd))
  endif
  if (present(tstart)) tstart = tstartd
  if (present(tend  )) tend   = tendd
  endsubroutine parse

  pure subroutine set(self, name, attribute, attributes, attributes_stream, sanitize_attributes_value, content, &
                      indent, is_content_indented, is_self_closing)
  !< Set tag data.
  class(xml_tag), intent(inout)        :: self                      !< XML tag.
  character(*),   intent(in), optional :: name                      !< Tag name.
  character(*),   intent(in), optional :: attribute(1:)             !< Attribute name/value pair [1:2].
  character(*),   intent(in), optional :: attributes(1:,1:)         !< Attributes list of name/value pairs [1:2,1:].
  character(*),   intent(in), optional :: attributes_stream         !< Attributes list of name/value pairs as single stream.
  logical,        intent(in), optional :: sanitize_attributes_value !< Sanitize attributes value.
  character(*),   intent(in), optional :: content                   !< Tag value.
  integer(I4P),   intent(in), optional :: indent                    !< Number of indent-white-spaces.
  logical,        intent(in), optional :: is_content_indented       !< Activate value indentation.
  logical,        intent(in), optional :: is_self_closing           !< The tag is self closing.
  logical                              :: is_content_indented_      !< Activate value indentation.

  if (present(name)) self%tag_name = name
  if (present(attribute)) call self%add_single_attribute(attribute=attribute, sanitize_value=sanitize_attributes_value)
  if (present(attributes)) call self%add_multiple_attributes(attributes=attributes, sanitize_values=sanitize_attributes_value)
  if (present(attributes_stream)) call self%add_stream_attributes(attributes_stream=attributes_stream, &
                                                                  sanitize_values=sanitize_attributes_value)
  if (present(indent)) self%indent = indent
  if (present(content)) then
    is_content_indented_ = .false. ; if (present(is_content_indented)) is_content_indented_ = is_content_indented
    if (is_content_indented_) then
      self%tag_content = new_line('a')//repeat(' ', self%indent+2)//content//new_line('a')
    else
      self%tag_content = content
    endif
  endif
  if (present(is_self_closing)) self%is_self_closing = is_self_closing
  endsubroutine set

  pure function self_closing_tag(self, is_indented) result(tag_)
  !< Return `<tag_name.../>` self closing tag.
  class(xml_tag), intent(in)           :: self        !< XML tag.
  logical,        intent(in), optional :: is_indented !< Flag to check if tag is indented.
  character(len=:), allocatable        :: tag_        !< The self closing tag string.

  tag_ = '<'//self%tag_name
  if (self%attributes_number>0) tag_ = tag_//' '//self%attributes()
  tag_ = tag_//'/>'
  if (present(is_indented)) then
    if (is_indented) tag_ = repeat(' ', self%indent)//tag_
  endif
  endfunction self_closing_tag

  pure function start_tag(self, is_indented) result(tag_)
  !< Return `<tag_name...>` start tag.
  class(xml_tag), intent(in)           :: self        !< XML tag.
  logical,        intent(in), optional :: is_indented !< Flag to check if tag is indented.
  character(len=:), allocatable        :: tag_        !< The start tag string.

  tag_ = '<'//self%tag_name
  if (self%attributes_number>0) tag_ = tag_//' '//self%attributes()
  tag_ = tag_//'>'
  if (present(is_indented)) then
    if (is_indented) tag_ = repeat(' ', self%indent)//tag_
  endif
  endfunction start_tag

  pure function stringify(self, is_indented, is_content_indented, only_start, only_content, only_end) result(stringed)
  !< Convert the whole tag into a string.
  class(xml_tag), intent(in)           :: self                 !< XML tag.
  logical,        intent(in), optional :: is_indented          !< Activate content indentation.
  logical,        intent(in), optional :: is_content_indented  !< Activate content indentation.
  logical,        intent(in), optional :: only_start           !< Write only start tag.
  logical,        intent(in), optional :: only_content         !< Write only content.
  logical,        intent(in), optional :: only_end             !< Write only end tag.
  character(len=:), allocatable        :: stringed             !< Output string containing the whole tag.
  logical                              :: is_content_indented_ !< Activate content indentation.
  logical                              :: only_start_          !< Write only start tag.
  logical                              :: only_content_        !< Write only content.
  logical                              :: only_end_            !< Write only end tag.

  is_content_indented_ = .false. ; if (present(is_content_indented)) is_content_indented_ = is_content_indented
  only_start_ = .false. ; if (present(only_start)) only_start_ = only_start
  only_content_ = .false. ; if (present(only_content)) only_content_ = only_content
  only_end_ = .false. ; if (present(only_end)) only_end_ = only_end
  if (only_start_) then
    stringed = self%start_tag(is_indented=is_indented)
  elseif (only_content_) then
    if (self%tag_content%is_allocated()) then
      if (is_content_indented_) then
        stringed = repeat(' ', self%indent+2)//self%tag_content
      else
        stringed = self%tag_content%chars()
      endif
    endif
  elseif (only_end_) then
    stringed = self%end_tag(is_indented=is_indented)
  else
    stringed = ''
    if (self%tag_name%is_allocated()) then
      if (self%is_self_closing) then
        stringed = self%self_closing_tag(is_indented=is_indented)
      else
        stringed = self%start_tag(is_indented=is_indented)
        if (self%tag_content%is_allocated()) then
          if (is_content_indented_) then
            stringed = stringed//new_line('a')//repeat(' ', self%indent+2)//&
                       self%tag_content//new_line('a')//repeat(' ', self%indent)
          else
            stringed = stringed//self%tag_content
          endif
        endif
        stringed = stringed//self%end_tag()
      endif
    endif
  endif
  endfunction stringify

  subroutine write_tag(self, unit, is_indented, is_content_indented, form, end_record, only_start, only_content, only_end, &
                       iostat, iomsg)
  !< Write tag to unit file.
  class(xml_tag), intent(in)            :: self                !< XML tag.
  integer(I4P),   intent(in)            :: unit                !< File unit.
  logical,        intent(in),  optional :: is_indented         !< Activate content indentation.
  logical,        intent(in),  optional :: is_content_indented !< Activate content indentation.
  character(*),   intent(in),  optional :: form                !< Format.
  character(*),   intent(in),  optional :: end_record          !< Ending record.
  logical,        intent(in),  optional :: only_start          !< Write only start tag.
  logical,        intent(in),  optional :: only_content        !< Write only content.
  logical,        intent(in),  optional :: only_end            !< Write only end tag.
  integer(I4P),   intent(out), optional :: iostat              !< IO status.
  character(*),   intent(out), optional :: iomsg               !< IO message.
  type(string)                          :: form_               !< Format.
  type(string)                          :: end_record_         !< Ending record.
  integer(I4P)                          :: iostat_             !< IO status.
  character(500)                        :: iomsg_              !< IO message.

  form_ = 'UNFORMATTED'
  if (present(form)) then
    form_ = form
    form_ = form_%upper()
  endif
  end_record_ = '' ; if (present(end_record)) end_record_ = end_record
  select case(form_%chars())
  case('UNFORMATTED')
    write(unit=unit, iostat=iostat_, iomsg=iomsg_)self%stringify(is_indented=is_indented,                 &
                                                                 is_content_indented=is_content_indented, &
                                                                 only_start=only_start,                   &
                                                                 only_content=only_content,               &
                                                                 only_end=only_end)//end_record_
  case('FORMATTED')
    write(unit=unit, fmt='(A)', iostat=iostat_, iomsg=iomsg_)self%stringify(is_indented=is_indented,                 &
                                                                            is_content_indented=is_content_indented, &
                                                                            only_start=only_start,                   &
                                                                            only_content=only_content,               &
                                                                            only_end=only_end)//end_record_
  endselect
  if (present(iostat)) iostat = iostat_
  if (present(iomsg)) iomsg = iomsg_
  endsubroutine write_tag

  ! private methods
  pure function is_attribute_present(self, name) result(is_present)
  !< Return .true. it the queried attribute name is defined, .false. otherwise.
  class(xml_tag), intent(in) :: self       !< XML tag.
  character(*),   intent(in) :: name       !< Attribute name.
  logical                    :: is_present !< Inquire result.
  integer(I4P)               :: a          !< Counter.

  is_present = .false.
  if (self%attributes_number>0) then
    do a=1, self%attributes_number
      if (self%attribute(1, a)==name) then
        is_present = .true.
        exit
      endif
    enddo
  endif
  endfunction is_attribute_present

  pure subroutine add_single_attribute(self, attribute, sanitize_value)
  !< Add one attribute name/value pair.
  !<
  !< @note Leading and trailing white spaces are trimmed out by attribute's name.
  class(xml_tag), intent(inout)        :: self               !< XML tag.
  character(*),   intent(in)           :: attribute(1:)      !< Attribute name/value pair [1:2].
  logical,        intent(in), optional :: sanitize_value     !< Sanitize attribute value.
  type(string), allocatable            :: new_attribute(:,:) !< Temporary storage for attributes.
  logical                              :: sanitize_value_    !< Sanitize attribute value.
  logical                              :: is_updated         !< Flag to check if the attribute has been updeted.
  integer(I4P)                         :: a                  !< Counter.

  sanitize_value_ = .false. ; if (present(sanitize_value)) sanitize_value_ = sanitize_value
  if (self%attributes_number>0) then
    is_updated = .false.
    update_if_already_present: do a=1, self%attributes_number
      if (self%attribute(1, a)==attribute(1)) then
        if (sanitize_value_) then
          self%attribute(2, a) = trim(adjustl(attribute(2)))
        else
          self%attribute(2, a) = attribute(2)
        endif
        is_updated = .true.
        exit update_if_already_present
      endif
    enddo update_if_already_present
    if (.not.is_updated) then
      allocate(new_attribute(1:2, 1:self%attributes_number+1))
      new_attribute(1:2, 1:self%attributes_number) = self%attribute
      new_attribute(1, self%attributes_number+1) = trim(adjustl(attribute(1)))
      if (sanitize_value_) then
        new_attribute(2, self%attributes_number+1) = trim(adjustl(attribute(2)))
      else
        new_attribute(2, self%attributes_number+1) = attribute(2)
      endif
      call move_alloc(from=new_attribute, to=self%attribute)
      self%attributes_number = self%attributes_number + 1
    endif
  else
    call self%alloc_attributes(Na=1)
    self%attribute(1, 1) = trim(adjustl(attribute(1)))
    if (sanitize_value_) then
      self%attribute(2, 1) = trim(adjustl(attribute(2)))
    else
      self%attribute(2, 1) = attribute(2)
    endif
  endif
  endsubroutine add_single_attribute

  pure subroutine add_multiple_attributes(self, attributes, sanitize_values)
  !< Add list of attributes name/value pairs.
  class(xml_tag), intent(inout)        :: self              !< XML tag.
  character(*),   intent(in)           :: attributes(1:,1:) !< Attribute name/value pair list [1:2,1:].
  logical,        intent(in), optional :: sanitize_values   !< Sanitize attribute values.
  integer(I4P)                         :: a                 !< Counter.

  do a=1, size(attributes, dim=2)
    ! not efficient: many reallocation, but safe
    call self%add_single_attribute(attribute=attributes(1:,a), sanitize_value=sanitize_values)
  enddo
  endsubroutine add_multiple_attributes

  pure subroutine add_stream_attributes(self, attributes_stream, sanitize_values)
  !< Add list of attributes name/value pairs passed as stream.
  !<
  !< @note The character `=` cannot compare into the attributes names of values.
  class(xml_tag), intent(inout)        :: self              !< XML tag.
  character(*),   intent(in)           :: attributes_stream !< Attribute name/value pair list passed as stream.
  logical,        intent(in), optional :: sanitize_values   !< Sanitize attribute values.
  type(string)                         :: attributes_string !< Attribute name/value pair list as string.
  type(string)                         :: tokens(1:3)       !< Attributes tokenized by `=`.
  type(string)                         :: attribute(1:2)    !< Attribute name/value pair.
  logical                              :: continue_to_parse !< Sentinel to stop attributes stream parsing.
  integer(I4P)                         :: max_chars         !< Counter.

  attributes_string = attributes_stream
  continue_to_parse = .true.
  do while(continue_to_parse)
    tokens = attributes_string%partition(sep='=')
    attribute(1) = trim(adjustl(tokens(1)))
    if (attribute(1)/='') then
      tokens(3) = tokens(3)%slice(istart=tokens(3)%index('"')+1, iend=tokens(3)%len())
      attribute(2) = tokens(3)%slice(istart=1, iend=tokens(3)%index('"')-1)
      tokens(3) = tokens(3)%slice(istart=tokens(3)%index('"')+1, iend=tokens(3)%len())
      max_chars = max(attribute(1)%len(), attribute(2)%len())
      attribute(1) = attribute(1)%fill(width=max_chars, right=.true., filling_char=' ')
      attribute(2) = attribute(2)%fill(width=max_chars, right=.true., filling_char=' ')
      call self%add_single_attribute(attribute=[attribute(1)//'', attribute(2)//''], sanitize_value=sanitize_values)
      if (tokens(3)%index('=')>0) then
        attributes_string = tokens(3)
      else
        continue_to_parse = .false.
      endif
    else
      continue_to_parse = .false.
    endif
  enddo
  endsubroutine add_stream_attributes

  elemental subroutine alloc_attributes(self, Na)
  !< Allocate (prepare for filling) dynamic memory of attributes.
  class(xml_tag),    intent(inout) :: self     !< XML tag.
  integer(I4P),      intent(in)    :: Na       !< Number of attributes.

  if (allocated(self%attribute)) then
    call self%attribute%free
    deallocate(self%attribute)
  endif
  allocate(self%attribute(1:2, 1:Na))
  self%attributes_number = Na
  endsubroutine alloc_attributes

  pure subroutine delete_content(self)
  !< Delete tag content.
  class(xml_tag), intent(inout) :: self !< XML tag.

  call self%tag_content%free
  endsubroutine delete_content

  pure subroutine delete_single_attribute(self, name)
  !< Delete one attribute name/value pair.
  class(xml_tag), intent(inout) :: self               !< XML tag.
  character(*),   intent(in)    :: name               !< Attribute name.
  type(string), allocatable     :: new_attribute(:,:) !< Temporary storage for attributes.
  integer(I4P)                  :: a                  !< Counter.

  if (self%attributes_number>0) then
    search_tag: do a=1, self%attributes_number
      if (self%attribute(1, a)==name) then
        if (self%attributes_number>1) then
          allocate(new_attribute(1:2, 1:self%attributes_number-1))
          if (a==1) then
            new_attribute(:, a:) = self%attribute(:, a+1:)
          elseif (a==self%attributes_number) then
            new_attribute(:, :a-1) = self%attribute(:, :a-1)
          else
            new_attribute(:, :a-1) = self%attribute(:, :a-1)
            new_attribute(:, a:) = self%attribute(:, a+1:)
          endif
          call move_alloc(from=new_attribute, to=self%attribute)
        else
          call self%attribute%free
          deallocate(self%attribute)
        endif
        self%attributes_number = self%attributes_number - 1
        exit search_tag
      endif
    enddo search_tag
  endif
  endsubroutine delete_single_attribute

  pure subroutine delete_multiple_attributes(self, name)
  !< Delete list of attributes name/value pairs.
  class(xml_tag), intent(inout) :: self     !< XML tag.
  character(*),   intent(in)    :: name(1:) !< Attributes names.
  integer(I4P)                  :: a        !< Counter.

  do a=1, size(name, dim=1)
    call self%delete_single_attribute(name=name(a))
  enddo
  endsubroutine delete_multiple_attributes

  elemental subroutine get(self, source)
  !< Get the tag content and attributes from source after tag_name and attributes names have been set.
  class(xml_tag), intent(inout) :: self   !< XML tag.
  character(*),   intent(in)    :: source !< String containing data.

  call self%get_value(source=source)
  call self%get_attributes(source=source)
  ! call self%get_nested()
  endsubroutine get

  elemental subroutine get_attributes(self, source)
  !< Get the attributes values from source after tag_name and attributes names have been set.
  class(xml_tag), intent(inout) :: self   !< XML tag.
  character(*),   intent(in)    :: source !< String containing data.
  integer                       :: a      !< Counter.
  integer                       :: c1     !< Counter.
  integer                       :: c2     !< Counter.

  if (index(string=source, substring='<'//self%tag_name)>0) then
    if (self%attributes_number>0) then ! parsing attributes
      do a=1, self%attributes_number
        c1 = index(string=source, substring=self%attribute(1, a)//'="') + self%attribute(1, a)%len() + 2
        if (c1>self%attribute(1, a)%len() + 2) then
          c2 = index(string=source(c1:), substring='"')
          if (c2>0) then
            self%attribute(2, a) = source(c1:c1+c2-2)
          else
            call self%attribute(2, a)%free
          endif
        else
          call self%attribute(2, a)%free
        endif
      enddo
    endif
  endif
  endsubroutine get_attributes

  elemental subroutine get_value(self, source)
  !< Get the tag value from source after tag_name has been set.
  class(xml_tag), intent(inout) :: self   !< XML tag.
  character(*),   intent(in)    :: source !< String containing data.
  integer                       :: c1     !< Counter.
  integer                       :: c2     !< Counter.

  call self%tag_content%free
  self%is_self_closing = .false.
  if (index(string=source, substring='<'//self%tag_name)>0) then
    c2 = index(string=source, substring='</'//self%tag_name//'>')
    if (c2>0) then ! parsing tag value
      c1 = index(string=source, substring='>')
      if (c1+1<c2-1) self%tag_content = source(c1+1:c2-1)
    else
      self%is_self_closing = .true.
    endif
  endif
  endsubroutine get_value

  elemental subroutine parse_attributes_names(self, source)
  !< Parse the tag attributes names contained into a string.
  !<
  !< Valid syntax is:
  !< + `att1="att1 val" att2="att2 val"...`
  !< @note Inside the attributes value the symbols `<` and `>` are not allowed.
  class(xml_tag), intent(inout) :: self   !< XML tag.
  character(*),   intent(in)    :: source !< String containing the input.
  character(len=:), allocatable :: att    !< Dummy string for parsing file.
  integer(I4P)                  :: c      !< Counter.
  integer(I4P)                  :: s      !< Counter.
  integer(I4P)                  :: a      !< Counter.
  integer(I4P)                  :: Na     !< Counter.

  Na = 0
  c = 1
  Att_Count: do while(c<=len(source))
    if (source(c:c)=='=') Na = Na + 1
    c = c + 1
  enddo Att_Count
  if (Na>0) then
    call self%alloc_attributes(Na=Na)
    c = index(string=source, substring=' ')
    att = source(c:)
    c = 1
    a = 1
    Att_Search: do while(c<=len(att))
      if (att(c:c)=='=') then
        s = max(0, index(string=att, substring=' '))
        self%attribute(1, a) = trim(adjustl(att(s+1:c-1)))
        att = att(c+1:)
        c = 1
        a = a + 1
      endif
      c = c + 1
    enddo Att_Search
  endif
  endsubroutine parse_attributes_names

  elemental subroutine parse_tag_name(self, source, tstart, tend)
  !< Parse the tag name contained into a string.
  !<
  !< It is assumed that the first tag contained into the source is parsed, the others eventually present are omitted.
  !< Valid syntax are:
  !< + `<tag_name att1="att1 val" att2="att2 val"...>...</tag_name>`
  !< + `<tag_name att1="att1 val" att2="att2 val".../>`
  !< @note Inside the attributes value the symbols `<` and `>` are not allowed.
  class(xml_tag),         intent(inout) :: self    !< XML tag.
  character(*),           intent(in)    :: source  !< String containing the input.
  integer(I4P), optional, intent(out)   :: tstart  !< Starting index of tag inside the source.
  integer(I4P), optional, intent(out)   :: tend    !< Ending index of tag inside the source.
  integer(I4P)                          :: tstartd !< Starting index of tag inside the source.
  integer(I4P)                          :: tendd   !< Ending index of tag inside the source.
  character(len=1)                      :: c1      !< Dummy string for parsing file.
  character(len=:), allocatable         :: c2      !< Dummy string for parsing file.
  integer(I4P)                          :: c       !< Counter.
  integer(I4P)                          :: s       !< Counter.

  tstartd = 0
  tendd   = 0
  c = 1
  Tag_Search: do while(c<=len(source))
    c1 = source(c:c)
    if (c1=='<') then
      tstartd = c
      c2 = c1
      Tag_Name: do while(c<len(source))
        c = c + 1 ; c1 = source(c:c)
        c2 = c2//c1
        if (c1=='>') then
          tendd = c
          exit Tag_Name
        endif
      enddo Tag_Name
      s = index(string=c2, substring=' ')
      if (s>0) then ! there are attributes
        self%tag_name = c2(2:s-1)
      else
        if (index(string=c2, substring='/>')>0) then ! self closing tag
          self%tag_name = c2(2:len(c2)-2)
        else
          self%tag_name = c2(2:len(c2)-1)
        endif
      endif
      exit Tag_Search
    endif
    c = c + 1
  enddo Tag_Search
  if (present(tstart)) tstart = tstartd
  if (present(tend  )) tend   = tendd
  endsubroutine parse_tag_name

  elemental subroutine search(self, tag_name, source, tstart, tend)
  !< Search tag named *tag_name* into a string and, in case it is found, store into self.
  !<
  !< @note If *tag_name* is not found, self is returned empty.
  class(xml_tag),         intent(inout) :: self     !< XML tag.
  character(*),           intent(in)    :: tag_name !< Searched tag name.
  character(*),           intent(in)    :: source   !< String containing the input.
  integer(I4P), optional, intent(out)   :: tstart   !< Starting index of tag inside the source.
  integer(I4P), optional, intent(out)   :: tend     !< Ending index of tag inside the source.
  type(xml_tag)                         :: tag      !< Dummy XML tag.
  integer(I4P)                          :: tstart_  !< Starting index of tag inside the source, local variable.
  integer(I4P)                          :: tend_    !< Ending index of tag inside the source, local variable.
  logical                               :: found    !< Flag for inquiring search result.
  integer(I4P)                          :: tstart_c !< Starting index of tag inside the current slice of source.
  integer(I4P)                          :: tend_c   !< Starting index of tag inside the current slice of source.
  integer(I4P)                          :: i

  call self%free
  self%tag_name = tag_name
  tstart_ = 1
  tend_   = 0
  found = .false.
  tstart_c = 0
  tend_c = 0
  Tag_Search: do
    call tag%parse(source=source(tend_ + 1:), tstart=tstart_c, tend=tend_c)
    tstart_ = tstart_ + tend_
    tend_ = tend_ + tend_c
    if (tstart_c==0.and.tend_c==0) then
      exit Tag_Search ! no tag found
    else
      if (tag%tag_name%is_allocated()) then
        if (tag%tag_name==self%tag_name) then
          found = .true.
        endif
      endif
    endif
    if (found) exit Tag_Search
  enddo Tag_Search
  if (found) then
    self = tag
  else
    call self%free
  endif
  if (present(tstart)) tstart = tstart_
  if (present(tend  )) tend   = tend_
  endsubroutine search

  ! assignment (=)
  elemental subroutine assign_tag(lhs, rhs)
  !< Assignment between two tags.
  class(xml_tag), intent(inout) :: lhs !< Left hand side.
  type(xml_tag),  intent(in)    :: rhs !< Right hand side.
  integer(I4P)                  :: a   !< Counter.

  call lhs%free
  if (rhs%tag_name%is_allocated()) lhs%tag_name = rhs%tag_name
  if (rhs%tag_content%is_allocated()) lhs%tag_content = rhs%tag_content
  if (rhs%attributes_number>0) then
    allocate(lhs%attribute(1:2, 1:rhs%attributes_number))
    do a=1, rhs%attributes_number
      lhs%attribute(1:2, a) = rhs%attribute(1:2, a)
    enddo
  endif
  lhs%attributes_number = rhs%attributes_number
  lhs%indent = rhs%indent
  lhs%is_self_closing = rhs%is_self_closing
  endsubroutine assign_tag

  ! finalize
  elemental subroutine finalize(tag)
  !< Free dynamic memory when finalizing.
  type(xml_tag), intent(inout) :: tag !< XML tag.

  call tag%free
  endsubroutine finalize
endmodule foxy_xml_tag
