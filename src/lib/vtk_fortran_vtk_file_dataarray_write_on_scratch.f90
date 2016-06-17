!< Implementation of dataarray write on scratch of VTK file class.
submodule (vtk_fortran_vtk_file) dataarray_write_on_scratch
!-----------------------------------------------------------------------------------------------------------------------------------
!< Implementation of dataarray write on scratch of VTK file class.
!-----------------------------------------------------------------------------------------------------------------------------------

!-----------------------------------------------------------------------------------------------------------------------------------
implicit none
!-----------------------------------------------------------------------------------------------------------------------------------
contains
  module function write_on_scratch_comp1_rank1(self, x) result(n_byte)
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
  endselect
  return
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction write_on_scratch_comp1_rank1

  module function write_on_scratch_comp1_rank2(self, x) result(n_byte)
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
  endselect
  return
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction write_on_scratch_comp1_rank2

  module function write_on_scratch_comp1_rank3(self, x) result(n_byte)
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
  endselect
  return
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction write_on_scratch_comp1_rank3

  module function write_on_scratch_comp1_rank4(self, x) result(n_byte)
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
  endselect
  return
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction write_on_scratch_comp1_rank4

  module function write_on_scratch_comp3_rank1(self, x, y, z) result(n_byte)
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
  n_byte = self%write_on_scratch(x=x) + self%write_on_scratch(x=y) + self%write_on_scratch(x=z)
  return
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction write_on_scratch_comp3_rank1

  module function write_on_scratch_comp3_rank2(self, x, y, z) result(n_byte)
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
  n_byte = self%write_on_scratch(x=x) + self%write_on_scratch(x=y) + self%write_on_scratch(x=z)
  return
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction write_on_scratch_comp3_rank2

  module function write_on_scratch_comp3_rank3(self, x, y, z) result(n_byte)
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
  n_byte = self%write_on_scratch(x=x) + self%write_on_scratch(x=y) + self%write_on_scratch(x=z)
  return
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction write_on_scratch_comp3_rank3
endsubmodule dataarray_write_on_scratch
