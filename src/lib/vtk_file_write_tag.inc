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

  subroutine write_dataarray_tag(self, data_type, number_of_components, data_name, data_content)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Write `<DataArray...>...</DataArray>` tag.
  !---------------------------------------------------------------------------------------------------------------------------------
  class(vtk_file), intent(inout)        :: self                 !< VTK file.
  character(*),    intent(in)           :: data_type            !< Type of dataarray.
  integer(I4P),    intent(in)           :: number_of_components !< Number of dataarray components.
  character(*),    intent(in)           :: data_name            !< Data name.
  character(*),    intent(in), optional :: data_content         !< Data content.
  type(string)                          :: tag_attributes       !< Tag attributes.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  tag_attributes = 'type="'//trim(adjustl(data_type))//                 &
    '" NumberOfComponents="'//trim(str(number_of_components, .true.))// &
    '" Name="'//trim(adjustl(data_name))//                              &
    '" format="'//self%format_ch//'"'
  call self%write_tag(tag_name='DataArray', tag_attributes=tag_attributes%chars(), tag_content=data_content)
  return
  !---------------------------------------------------------------------------------------------------------------------------------
  endsubroutine write_dataarray_tag

  subroutine write_dataarray_tag_appended(self, data_type, number_of_components, data_name)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Write `<DataArray.../>` tag.
  !---------------------------------------------------------------------------------------------------------------------------------
  class(vtk_file), intent(inout) :: self                 !< VTK file.
  character(*),    intent(in)    :: data_type            !< Type of dataarray.
  integer(I4P),    intent(in)    :: number_of_components !< Number of dataarray components.
  character(*),    intent(in)    :: data_name            !< Data name.
  type(string)                   :: tag_attributes       !< Tag attributes.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  tag_attributes = 'type="'//trim(adjustl(data_type))//                 &
    '" NumberOfComponents="'//trim(str(number_of_components, .true.))// &
    '" Name="'//trim(adjustl(data_name))//                              &
    '" format="'//self%format_ch//'"'//                                 &
    '" offset="'//trim(str(self%ioffset, .true.))//'"'
  call self%write_self_closing_tag(tag_name='DataArray', tag_attributes=tag_attributes%chars())
  return
  !---------------------------------------------------------------------------------------------------------------------------------
  endsubroutine write_dataarray_tag_appended

  function write_dataarray_location_tag(self, location, action) result(error)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Write `<[/]PointData>` or `<[/]CellData>` open/close tag.
  !<
  !< @note **must** be called before saving the data related to geometric mesh, this function initializes the
  !< saving of data variables indicating the *location* (node or cell centered) of variables that will be saved.
  !<
  !< @note A single file can contain both cell and node centered variables. In this case the VTK_DAT_XML function must be
  !< called two times, before saving cell-centered variables and before saving node-centered variables.
  !<
  !<### Examples of usage
  !<
  !<#### Opening node piece
  !<```fortran
  !< error = vtk%write_data('node','OPeN')
  !<```
  !<
  !<#### Closing node piece
  !<```fortran
  !< error = vtk%write_data('node','Close')
  !<```
  !<
  !<#### Opening cell piece
  !<```fortran
  !< error = vtk%write_data('cell','OPEN')
  !<```
  !<
  !<#### Closing cell piece
  !<```fortran
  !< error = vtk%write_data('cell','close')
  !<```
  !---------------------------------------------------------------------------------------------------------------------------------
  class(vtk_file), intent(inout) :: self      !< VTK file.
  character(*),    intent(in)    :: location  !< Location of variables: **cell** or **node** centered.
  character(*),    intent(in)    :: action    !< Action: **open** or **close** tag.
  integer(I4P)                   :: error     !< Error status.
  type(string)                   :: location_ !< Location string.
  type(string)                   :: action_   !< Action string.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  location_ = trim(adjustl(location)) ; location_ = location_%upper()
  action_ = trim(adjustl(action)) ; action_ = action_%upper()
  select case(location_%chars())
  case('CELL')
    select case(action_%chars())
    case('OPEN')
      call self%write_start_tag(tag_name='CellData')
    case('CLOSE')
      call self%write_end_tag(tag_name='CellData')
    endselect
  case('NODE')
    select case(action_%chars())
    case('OPEN')
      call self%write_start_tag(tag_name='PointData')
    case('CLOSE')
      call self%write_end_tag(tag_name='PointData')
    endselect
  endselect
  error = self%error
  return
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction write_dataarray_location_tag

  subroutine write_dataarray_appended(self)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Write `<AppendedData...>...</AppendedData>` tag.
  !---------------------------------------------------------------------------------------------------------------------------------
  class(vtk_file), intent(inout) :: self              !< VTK file.
  type(string)                   :: tag_attributes    !< Tag attributes.
  integer(I4P)                   :: n_byte            !< Bytes count.
  character(len=2)               :: dataarray_type    !< Dataarray type = R8,R4,I8,I4,I2,I1.
  integer(I4P)                   :: dataarray_dim     !< Dataarray dimension.
  real(R8P),    allocatable      :: dataarray_R8P(:)  !< Dataarray buffer of R8P.
  real(R4P),    allocatable      :: dataarray_R4P(:)  !< Dataarray buffer of R4P.
  integer(I8P), allocatable      :: dataarray_I8P(:)  !< Dataarray buffer of I8P.
  integer(I4P), allocatable      :: dataarray_I4P(:)  !< Dataarray buffer of I4P.
  integer(I2P), allocatable      :: dataarray_I2P(:)  !< Dataarray buffer of I2P.
  integer(I1P), allocatable      :: dataarray_I1P(:)  !< Dataarray buffer of I1P.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  select case(self%format)
  case(raw)
    tag_attributes = 'encoding="raw"'
  case(bin_app)
    tag_attributes = 'encoding="base64"'
  endselect
  call self%write_start_tag(tag_name='AppendedData', tag_attributes=tag_attributes%chars())
  write(unit=self%xml, iostat=self%error)'_'
  endfile(unit=self%scratch, iostat=self%error)
  rewind(unit=self%scratch, iostat=self%error)
  do
    call read_dataarray_from_scratch
    if (self%error==0) call write_dataarray_on_xml
  enddo
  close(unit=self%scratch, iostat=self%error)
  write(unit=self%xml, iostat=self%error)end_rec
  call self%write_end_tag(tag_name='AppendedData')
  return
  !---------------------------------------------------------------------------------------------------------------------------------
  contains
    subroutine read_dataarray_from_scratch
    !-------------------------------------------------------------------------------------------------------------------------------
    !< Read the current dataaray from scratch file.
    !-------------------------------------------------------------------------------------------------------------------------------

    !-------------------------------------------------------------------------------------------------------------------------------
    read(unit=self%scratch, iostat=self%error, end=10)n_byte, dataarray_type, dataarray_dim
    select case(dataarray_type)
    case('R8')
      if (allocated(dataarray_R8P)) deallocate(dataarray_R8P) ; allocate(dataarray_R8P(1:dataarray_dim))
      read(unit=self%scratch, iostat=self%error)dataarray_R8P
    case('R4')
      if (allocated(dataarray_R4P)) deallocate(dataarray_R4P) ; allocate(dataarray_R4P(1:dataarray_dim))
      read(unit=self%scratch, iostat=self%error)dataarray_R4P
    case('I8')
      if (allocated(dataarray_I8P)) deallocate(dataarray_I8P) ; allocate(dataarray_I8P(1:dataarray_dim))
      read(unit=self%scratch, iostat=self%error)dataarray_I8P
    case('I4')
      if (allocated(dataarray_I4P)) deallocate(dataarray_I4P) ; allocate(dataarray_I4P(1:dataarray_dim))
      read(unit=self%scratch, iostat=self%error)dataarray_I4P
    case('I2')
      if (allocated(dataarray_I2P)) deallocate(dataarray_I2P) ; allocate(dataarray_I2P(1:dataarray_dim))
      read(unit=self%scratch, iostat=self%error)dataarray_I2P
    case('I1')
      if (allocated(dataarray_I1P)) deallocate(dataarray_I1P) ; allocate(dataarray_I1P(1:dataarray_dim))
      read(unit=self%scratch, iostat=self%error)dataarray_I1P
    case default
      self%error = 1
      write (stderr,'(A)')' error: bad dataarray_type = '//dataarray_type
      write (stderr,'(A)')' bytes = '//trim(str(n=n_byte))
      write (stderr,'(A)')' dataarray dimension = '//trim(str(n=dataarray_dim))
    endselect
    10 return
    !-------------------------------------------------------------------------------------------------------------------------------
    endsubroutine read_dataarray_from_scratch

    subroutine write_dataarray_on_xml
    !-------------------------------------------------------------------------------------------------------------------------------
    !< Write the current dataaray on xml file.
    !-------------------------------------------------------------------------------------------------------------------------------
    character(len=:), allocatable  :: code !< Dataarray encoded with Base64 codec.
    !-------------------------------------------------------------------------------------------------------------------------------

    !-------------------------------------------------------------------------------------------------------------------------------
    if (self%format==raw) then
      select case(dataarray_type)
      case('R8')
        write(unit=self%xml, iostat=self%error)n_byte, dataarray_R8P
        deallocate(dataarray_R8P)
      case('R4')
        write(unit=self%xml, iostat=self%error)n_byte, dataarray_R4P
        deallocate(dataarray_R4P)
      case('I8')
        write(unit=self%xml, iostat=self%error)n_byte, dataarray_I8P
        deallocate(dataarray_I8P)
      case('I4')
        write(unit=self%xml, iostat=self%error)n_byte, dataarray_I4P
        deallocate(dataarray_I4P)
      case('I2')
        write(unit=self%xml, iostat=self%error)n_byte, dataarray_I2P
        deallocate(dataarray_I2P)
      case('I1')
        write(unit=self%xml, iostat=self%error)n_byte, dataarray_I1P
        deallocate(dataarray_I1P)
      endselect
    else
      select case(dataarray_type)
      case('R8')
        code = self%encode_base64_dataarray(x=dataarray_R8P)
        write(unit=self%xml, iostat=self%error)code
      case('R4')
        code = self%encode_base64_dataarray(x=dataarray_R4P)
        write(unit=self%xml, iostat=self%error)code
      case('I8')
        code = self%encode_base64_dataarray(x=dataarray_I8P)
        write(unit=self%xml, iostat=self%error)code
      case('I4')
        code = self%encode_base64_dataarray(x=dataarray_I4P)
        write(unit=self%xml, iostat=self%error)code
      case('I2')
        code = self%encode_base64_dataarray(x=dataarray_I2P)
        write(unit=self%xml, iostat=self%error)code
      case('I1')
        code = self%encode_base64_dataarray(x=dataarray_I1P)
        write(unit=self%xml, iostat=self%error)code
      endselect
    endif
    !-------------------------------------------------------------------------------------------------------------------------------
    endsubroutine write_dataarray_on_xml
  endsubroutine write_dataarray_appended
