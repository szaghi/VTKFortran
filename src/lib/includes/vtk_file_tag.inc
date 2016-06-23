  ! Implementation of tag-related methods of VTK file class.
  elemental function self_closing_tag(self, tag_name, tag_attributes) result(tag_)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Return `<tag_name.../>` self closing tag.
  !---------------------------------------------------------------------------------------------------------------------------------
  class(vtk_file), intent(in)           :: self           !< VTK file.
  character(*),    intent(in)           :: tag_name       !< Tag name.
  character(*),    intent(in), optional :: tag_attributes !< Tag attributes.
  type(string)                          :: tag_           !< The tag.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  if (present(tag_attributes)) then
    tag_ = repeat(' ', self%indent)//'<'//trim(adjustl(tag_name))//' '//trim(adjustl(tag_attributes))//'/>'//end_rec
  else
    tag_ = repeat(' ', self%indent)//'<'//trim(adjustl(tag_name))//'/>'//end_rec
  endif
  return
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction self_closing_tag

  elemental function tag(self, tag_name, tag_attributes, tag_content) result(tag_)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Return `<tag_name...>...</tag_name>` tag.
  !---------------------------------------------------------------------------------------------------------------------------------
  class(vtk_file), intent(in)           :: self           !< VTK file.
  character(*),    intent(in)           :: tag_name       !< Tag name.
  character(*),    intent(in), optional :: tag_attributes !< Tag attributes.
  character(*),    intent(in), optional :: tag_content    !< Tag content.
  type(string)                          :: tag_           !< The tag.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  tag_ = self%start_tag(tag_name=tag_name, tag_attributes=tag_attributes)
  if (present(tag_content)) tag_ = tag_//repeat(' ', self%indent+2)//tag_content//end_rec
  tag_ = tag_//self%end_tag(tag_name=tag_name)
  return
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction tag

  elemental function start_tag(self, tag_name, tag_attributes) result(tag_)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Return `<tag_name...>` start tag.
  !---------------------------------------------------------------------------------------------------------------------------------
  class(vtk_file), intent(in)           :: self           !< VTK file.
  character(*),    intent(in)           :: tag_name       !< Tag name.
  character(*),    intent(in), optional :: tag_attributes !< Tag attributes.
  type(string)                          :: tag_           !< The tag.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  tag_ = ''
  if (present(tag_attributes)) then
    tag_ = tag_//repeat(' ', self%indent)//'<'//trim(adjustl(tag_name))//' '//trim(adjustl(tag_attributes))//'>'//end_rec
  else
    tag_ = tag_//repeat(' ', self%indent)//'<'//trim(adjustl(tag_name))//'>'//end_rec
  endif
  return
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction start_tag

  elemental function end_tag(self, tag_name) result(tag_)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Return `</tag_name>` end tag.
  !---------------------------------------------------------------------------------------------------------------------------------
  class(vtk_file), intent(in) :: self     !< VTK file.
  character(*),    intent(in) :: tag_name !< Tag name.
  type(string)                :: tag_     !< The tag.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  tag_ = ''
  tag_ = tag_//repeat(' ', self%indent)//'</'//trim(adjustl(tag_name))//'>'//end_rec
  return
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction end_tag

