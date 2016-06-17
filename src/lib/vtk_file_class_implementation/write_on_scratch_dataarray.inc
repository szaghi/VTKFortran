  ! Implementation of write on scratch dataarray of VTK file class.
  function write_on_scratch_dataarray1_rank1(self, x) result(n_byte)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Write a dataarray with 1 components of rank 1.
  !---------------------------------------------------------------------------------------------------------------------------------
  class(vtk_file), intent(inout) :: self   !< VTK file.
  class(*),        intent(in)    :: x(1:)  !< Data variable.
  integer(I4P)                   :: n_byte !< Number of bytes
  integer(I4P)                   :: nn     !< Number of elements.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  nn = size(x, dim=1)
  select type(x)
  type is(real(R8P))
    n_byte = nn*BYR8P
    write(unit=self%scratch, iostat=self%error)n_byte, 'R8', nn
    write(unit=self%scratch, iostat=self%error)x
  type is(real(R4P))
    n_byte = nn*BYR4P
    write(unit=self%scratch, iostat=self%error)n_byte, 'R4', nn
    write(unit=self%scratch, iostat=self%error)x
  type is(integer(I8P))
    n_byte = nn*BYI8P
    write(unit=self%scratch, iostat=self%error)n_byte, 'I8', nn
    write(unit=self%scratch, iostat=self%error)x
  type is(integer(I4P))
    n_byte = nn*BYI4P
    write(unit=self%scratch, iostat=self%error)n_byte, 'I4', nn
    write(unit=self%scratch, iostat=self%error)x
  type is(integer(I2P))
    n_byte = nn*BYI2P
    write(unit=self%scratch, iostat=self%error)n_byte, 'I2', nn
    write(unit=self%scratch, iostat=self%error)x
  type is(integer(I1P))
    n_byte = nn*BYI1P
    write(unit=self%scratch, iostat=self%error)n_byte, 'I1', nn
    write(unit=self%scratch, iostat=self%error)x
  endselect
  return
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction write_on_scratch_dataarray1_rank1

  function write_on_scratch_dataarray1_rank2(self, x) result(n_byte)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Write a dataarray with 1 components of rank 2.
  !---------------------------------------------------------------------------------------------------------------------------------
  class(vtk_file), intent(inout) :: self     !< VTK file.
  class(*),        intent(in)    :: x(1:,1:) !< Data variable.
  integer(I4P)                   :: n_byte   !< Number of bytes
  integer(I4P)                   :: nn       !< Number of elements.
  !------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  nn = size(x, dim=1)*size(x, dim=2)
  select type(x)
  type is(real(R8P))
    n_byte = nn*BYR8P
    write(unit=self%scratch, iostat=self%error)n_byte, 'R8', nn
    write(unit=self%scratch, iostat=self%error)x
  type is(real(R4P))
    n_byte = nn*BYR4P
    write(unit=self%scratch, iostat=self%error)n_byte, 'R4', nn
    write(unit=self%scratch, iostat=self%error)x
  type is(integer(I8P))
    n_byte = nn*BYI8P
    write(unit=self%scratch, iostat=self%error)n_byte, 'I8', nn
    write(unit=self%scratch, iostat=self%error)x
  type is(integer(I4P))
    n_byte = nn*BYI4P
    write(unit=self%scratch, iostat=self%error)n_byte, 'I4', nn
    write(unit=self%scratch, iostat=self%error)x
  type is(integer(I2P))
    n_byte = nn*BYI2P
    write(unit=self%scratch, iostat=self%error)n_byte, 'I2', nn
    write(unit=self%scratch, iostat=self%error)x
  type is(integer(I1P))
    n_byte = nn*BYI1P
    write(unit=self%scratch, iostat=self%error)n_byte, 'I1', nn
    write(unit=self%scratch, iostat=self%error)x
  endselect
  return
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction write_on_scratch_dataarray1_rank2

  function write_on_scratch_dataarray1_rank3(self, x) result(n_byte)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Write a dataarray with 1 components of rank 3.
  !---------------------------------------------------------------------------------------------------------------------------------
  class(vtk_file), intent(inout) :: self        !< VTK file.
  class(*),        intent(in)    :: x(1:,1:,1:) !< Data variable.
  integer(I4P)                   :: n_byte      !< Number of bytes
  integer(I4P)                   :: nn          !< Number of elements.
  !------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  nn = size(x, dim=1)*size(x, dim=2)*size(x, dim=3)
  select type(x)
  type is(real(R8P))
    n_byte = nn*BYR8P
    write(unit=self%scratch, iostat=self%error)n_byte, 'R8', nn
    write(unit=self%scratch, iostat=self%error)x
  type is(real(R4P))
    n_byte = nn*BYR4P
    write(unit=self%scratch, iostat=self%error)n_byte, 'R4', nn
    write(unit=self%scratch, iostat=self%error)x
  type is(integer(I8P))
    n_byte = nn*BYI8P
    write(unit=self%scratch, iostat=self%error)n_byte, 'I8', nn
    write(unit=self%scratch, iostat=self%error)x
  type is(integer(I4P))
    n_byte = nn*BYI4P
    write(unit=self%scratch, iostat=self%error)n_byte, 'I4', nn
    write(unit=self%scratch, iostat=self%error)x
  type is(integer(I2P))
    n_byte = nn*BYI2P
    write(unit=self%scratch, iostat=self%error)n_byte, 'I2', nn
    write(unit=self%scratch, iostat=self%error)x
  type is(integer(I1P))
    n_byte = nn*BYI1P
    write(unit=self%scratch, iostat=self%error)n_byte, 'I1', nn
    write(unit=self%scratch, iostat=self%error)x
  endselect
  return
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction write_on_scratch_dataarray1_rank3

  function write_on_scratch_dataarray1_rank4(self, x) result(n_byte)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Write a dataarray with 1 components of rank 4.
  !---------------------------------------------------------------------------------------------------------------------------------
  class(vtk_file), intent(inout) :: self           !< VTK file.
  class(*),        intent(in)    :: x(1:,1:,1:,1:) !< Data variable.
  integer(I4P)                   :: n_byte         !< Number of bytes
  integer(I4P)                   :: nn             !< Number of elements.
  !------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  nn = size(x, dim=1)*size(x, dim=2)*size(x, dim=3)*size(x, dim=4)
  select type(x)
  type is(real(R8P))
    n_byte = nn*BYR8P
    write(unit=self%scratch, iostat=self%error)n_byte, 'R8', nn
    write(unit=self%scratch, iostat=self%error)x
  type is(real(R4P))
    n_byte = nn*BYR4P
    write(unit=self%scratch, iostat=self%error)n_byte, 'R4', nn
    write(unit=self%scratch, iostat=self%error)x
  type is(integer(I8P))
    n_byte = nn*BYI8P
    write(unit=self%scratch, iostat=self%error)n_byte, 'I8', nn
    write(unit=self%scratch, iostat=self%error)x
  type is(integer(I4P))
    n_byte = nn*BYI4P
    write(unit=self%scratch, iostat=self%error)n_byte, 'I4', nn
    write(unit=self%scratch, iostat=self%error)x
  type is(integer(I2P))
    n_byte = nn*BYI2P
    write(unit=self%scratch, iostat=self%error)n_byte, 'I2', nn
    write(unit=self%scratch, iostat=self%error)x
  type is(integer(I1P))
    n_byte = nn*BYI1P
    write(unit=self%scratch, iostat=self%error)n_byte, 'I1', nn
    write(unit=self%scratch, iostat=self%error)x
  endselect
  return
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction write_on_scratch_dataarray1_rank4

  function write_on_scratch_dataarray3_rank1(self, x, y, z) result(n_byte)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Write a dataarray with 3 components of rank 1.
  !---------------------------------------------------------------------------------------------------------------------------------
  class(vtk_file), intent(inout) :: self   !< VTK file.
  class(*),        intent(in)    :: x(1:)  !< X component.
  class(*),        intent(in)    :: y(1:)  !< Y component.
  class(*),        intent(in)    :: z(1:)  !< Z component.
  integer(I4P)                   :: n_byte !< Number of bytes
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  n_byte = self%write_on_scratch_dataarray(x=x) + self%write_on_scratch_dataarray(x=y) + self%write_on_scratch_dataarray(x=z)
  return
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction write_on_scratch_dataarray3_rank1

  function write_on_scratch_dataarray3_rank2(self, x, y, z) result(n_byte)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Write a dataarray with 3 components of rank 2.
  !---------------------------------------------------------------------------------------------------------------------------------
  class(vtk_file), intent(inout) :: self     !< VTK file.
  class(*),        intent(in)    :: x(1:,1:) !< X component.
  class(*),        intent(in)    :: y(1:,1:) !< Y component.
  class(*),        intent(in)    :: z(1:,1:) !< Z component.
  integer(I4P)                   :: n_byte   !< Number of bytes
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  n_byte = self%write_on_scratch_dataarray(x=x) + self%write_on_scratch_dataarray(x=y) + self%write_on_scratch_dataarray(x=z)
  return
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction write_on_scratch_dataarray3_rank2

  function write_on_scratch_dataarray3_rank3(self, x, y, z) result(n_byte)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Write a dataarray with 3 components of rank 3.
  !---------------------------------------------------------------------------------------------------------------------------------
  class(vtk_file), intent(inout) :: self        !< VTK file.
  class(*),        intent(in)    :: x(1:,1:,1:) !< X component.
  class(*),        intent(in)    :: y(1:,1:,1:) !< Y component.
  class(*),        intent(in)    :: z(1:,1:,1:) !< Z component.
  integer(I4P)                   :: n_byte      !< Number of bytes
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  n_byte = self%write_on_scratch_dataarray(x=x) + self%write_on_scratch_dataarray(x=y) + self%write_on_scratch_dataarray(x=z)
  return
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction write_on_scratch_dataarray3_rank3
