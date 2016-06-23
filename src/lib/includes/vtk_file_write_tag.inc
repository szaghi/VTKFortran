  ! Implementation of write-tag-related methods of VTK file class.
  subroutine write_self_closing_tag(self, tag_name, tag_attributes)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Write `<tag_name.../>` self closing tag.
  !---------------------------------------------------------------------------------------------------------------------------------
  class(vtk_file), intent(inout)        :: self           !< VTK file.
  character(*),    intent(in)           :: tag_name       !< Tag name.
  character(*),    intent(in), optional :: tag_attributes !< Tag attributes.
  type(string)                          :: tag            !< The tag.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  tag = self%self_closing_tag(tag_name=tag_name, tag_attributes=tag_attributes)
  write(unit=self%xml, iostat=self%error)tag%chars()
  return
  !---------------------------------------------------------------------------------------------------------------------------------
  endsubroutine write_self_closing_tag

  subroutine write_tag(self, tag_name, tag_attributes, tag_content)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Write `<tag_name...>...</tag_name>` tag.
  !---------------------------------------------------------------------------------------------------------------------------------
  class(vtk_file), intent(inout)        :: self           !< VTK file.
  character(*),    intent(in)           :: tag_name       !< Tag name.
  character(*),    intent(in), optional :: tag_attributes !< Tag attributes.
  character(*),    intent(in), optional :: tag_content    !< Tag content.
  type(string)                          :: tag            !< The tag.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  tag = self%tag(tag_name=tag_name, tag_attributes=tag_attributes, tag_content=tag_content)
  write(unit=self%xml, iostat=self%error)tag%chars()
  return
  !---------------------------------------------------------------------------------------------------------------------------------
  endsubroutine write_tag

  subroutine write_start_tag(self, tag_name, tag_attributes)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Write `<tag_name...>` start tag.
  !---------------------------------------------------------------------------------------------------------------------------------
  class(vtk_file), intent(inout)        :: self           !< VTK file.
  character(*),    intent(in)           :: tag_name       !< Tag name.
  character(*),    intent(in), optional :: tag_attributes !< Tag attributes.
  type(string)                          :: tag            !< The tag.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  tag = self%start_tag(tag_name=tag_name, tag_attributes=tag_attributes)
  write(unit=self%xml, iostat=self%error)tag%chars()
  self%indent = self%indent + 2
  return
  !---------------------------------------------------------------------------------------------------------------------------------
  endsubroutine write_start_tag

  subroutine write_end_tag(self, tag_name)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Write `</tag_name>` end tag.
  !---------------------------------------------------------------------------------------------------------------------------------
  class(vtk_file), intent(inout) :: self     !< VTK file.
  character(*),    intent(in)    :: tag_name !< Tag name.
  type(string)                   :: tag      !< The tag.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  self%indent = self%indent - 2
  tag = self%end_tag(tag_name=tag_name)
  write(unit=self%xml, iostat=self%error)tag%chars()
  return
  !---------------------------------------------------------------------------------------------------------------------------------
  endsubroutine write_end_tag

  subroutine write_header_tag(self)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Write header tag.
  !---------------------------------------------------------------------------------------------------------------------------------
  class(vtk_file), intent(inout) :: self   !< VTK file.
  type(string)                   :: buffer !< Buffer string.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  buffer = '<?xml version="1.0"?>'//end_rec
  if (endian==endianL) then
    buffer = buffer//'<VTKFile type="'//self%topology//'" version="0.1" byte_order="LittleEndian">'
  else
    buffer = buffer//'<VTKFile type="'//self%topology//'" version="0.1" byte_order="BigEndian">'
  endif
  write(unit=self%xml, iostat=self%error)buffer//end_rec
  self%indent = 2
  return
  !---------------------------------------------------------------------------------------------------------------------------------
  endsubroutine write_header_tag

  subroutine write_topology_tag(self, nx1, nx2, ny1, ny2, nz1, nz2)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Write XML topology tag.
  !---------------------------------------------------------------------------------------------------------------------------------
  class(vtk_file), intent(inout)         :: self   !< VTK file.
  integer(I4P),    intent(in),  optional :: nx1    !< Initial node of x axis.
  integer(I4P),    intent(in),  optional :: nx2    !< Final node of x axis.
  integer(I4P),    intent(in),  optional :: ny1    !< Initial node of y axis.
  integer(I4P),    intent(in),  optional :: ny2    !< Final node of y axis.
  integer(I4P),    intent(in),  optional :: nz1    !< Initial node of z axis.
  integer(I4P),    intent(in),  optional :: nz2    !< Final node of z axis.
  type(string)                           :: buffer !< Buffer string.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  select case(self%topology%chars())
  case('RectilinearGrid', 'StructuredGrid')
    buffer = repeat(' ', self%indent)//'<'//self%topology//' WholeExtent="'//&
             trim(str(n=nx1))//' '//trim(str(n=nx2))//' '//                  &
             trim(str(n=ny1))//' '//trim(str(n=ny2))//' '//                  &
             trim(str(n=nz1))//' '//trim(str(n=nz2))//'">'
  case('UnstructuredGrid')
    buffer = repeat(' ', self%indent)//'<'//self%topology//'>'
  endselect
  write(unit=self%xml, iostat=self%error)buffer//end_rec
  self%indent = self%indent + 2
  return
  !---------------------------------------------------------------------------------------------------------------------------------
  endsubroutine write_topology_tag
