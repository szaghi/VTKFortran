  ! Implementation of write piece of VTK file class.
  function write_piece_start_tag(self, nx1, nx2, ny1, ny2, nz1, nz2) result(error)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Write `<Piece ...>` start tag.
  !---------------------------------------------------------------------------------------------------------------------------------
  class(vtk_file), intent(inout) :: self           !< VTK file.
  integer(I4P),    intent(in)    :: nx1            !< Initial node of x axis.
  integer(I4P),    intent(in)    :: nx2            !< Final node of x axis.
  integer(I4P),    intent(in)    :: ny1            !< Initial node of y axis.
  integer(I4P),    intent(in)    :: ny2            !< Final node of y axis.
  integer(I4P),    intent(in)    :: nz1            !< Initial node of z axis.
  integer(I4P),    intent(in)    :: nz2            !< Final node of z axis.
  integer(I4P)                   :: error          !< Error status.
  type(string)                   :: tag_attributes !< Tag attributes.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  tag_attributes = 'Extent="'//trim(str(n=nx1))//' '//trim(str(n=nx2))//' '// &
                               trim(str(n=ny1))//' '//trim(str(n=ny2))//' '// &
                               trim(str(n=nz1))//' '//trim(str(n=nz2))//'"'
  call self%write_start_tag(tag_name='Piece', tag_attributes=tag_attributes%chars())
  error = self%error
  return
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction write_piece_start_tag

  function write_piece_start_tag_unst(self, np, nc) result(error)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Write `<Piece ...>` start tag for unstructured topology.
  !---------------------------------------------------------------------------------------------------------------------------------
  class(vtk_file), intent(inout) :: self           !< VTK file.
  integer(I4P),    intent(in)    :: np             !< Number of points.
  integer(I4P),    intent(in)    :: nc             !< Number of cells.
  integer(I4P)                   :: error          !< Error status.
  type(string)                   :: tag_attributes !< Tag attributes.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  tag_attributes = 'NumberOfPoints="'//trim(str(n=np))//'" NumberOfCells="'//trim(str(n=nc))//'"'
  call self%write_start_tag(tag_name='Piece', tag_attributes=tag_attributes%chars())
  error = self%error
  return
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction write_piece_start_tag_unst

  function write_piece_end_tag(self) result(error)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Write `</Piece>` end tag.
  !---------------------------------------------------------------------------------------------------------------------------------
  class(vtk_file), intent(inout) :: self  !< VTK file.
  integer(I4P)                   :: error !< Error status.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  call self%write_end_tag(tag_name='Piece')
  error = self%error
  return
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction write_piece_end_tag
