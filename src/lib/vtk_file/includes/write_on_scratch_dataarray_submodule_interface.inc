!< Dataarray write on scratch submodule interface for VTK file class.
  module function write_on_scratch_dataarray1_rank1(self, x) result(n_byte)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Write a dataarray with 1 components of rank 1.
  !---------------------------------------------------------------------------------------------------------------------------------
  class(vtk_file), intent(inout) :: self   !< VTK file.
  class(*),        intent(in)    :: x(1:)  !< Data variable.
  integer(I4P)                   :: n_byte !< Number of bytes
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction write_on_scratch_dataarray1_rank1

  module function write_on_scratch_dataarray1_rank2(self, x) result(n_byte)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Write a dataarray with 1 components of rank 2.
  !---------------------------------------------------------------------------------------------------------------------------------
  class(vtk_file), intent(inout) :: self     !< VTK file.
  class(*),        intent(in)    :: x(1:,1:) !< Data variable.
  integer(I4P)                   :: n_byte   !< Number of bytes
  !------------------------------------------------------------------------------------------------------------------------------
  endfunction write_on_scratch_dataarray1_rank2

  module function write_on_scratch_dataarray1_rank3(self, x) result(n_byte)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Write a dataarray with 1 components of rank 3.
  !---------------------------------------------------------------------------------------------------------------------------------
  class(vtk_file), intent(inout) :: self        !< VTK file.
  class(*),        intent(in)    :: x(1:,1:,1:) !< Data variable.
  integer(I4P)                   :: n_byte      !< Number of bytes
  !------------------------------------------------------------------------------------------------------------------------------
  endfunction write_on_scratch_dataarray1_rank3

  module function write_on_scratch_dataarray1_rank4(self, x) result(n_byte)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Write a dataarray with 1 components of rank 4.
  !---------------------------------------------------------------------------------------------------------------------------------
  class(vtk_file), intent(inout) :: self           !< VTK file.
  class(*),        intent(in)    :: x(1:,1:,1:,1:) !< Data variable.
  integer(I4P)                   :: n_byte         !< Number of bytes
  !------------------------------------------------------------------------------------------------------------------------------
  endfunction write_on_scratch_dataarray1_rank4

  module function write_on_scratch_dataarray3_rank1(self, x, y, z) result(n_byte)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Write a dataarray with 3 components of rank 1.
  !---------------------------------------------------------------------------------------------------------------------------------
  class(vtk_file), intent(inout) :: self   !< VTK file.
  class(*),        intent(in)    :: x(1:)  !< X component.
  class(*),        intent(in)    :: y(1:)  !< Y component.
  class(*),        intent(in)    :: z(1:)  !< Z component.
  integer(I4P)                   :: n_byte !< Number of bytes
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction write_on_scratch_dataarray3_rank1

  module function write_on_scratch_dataarray3_rank2(self, x, y, z) result(n_byte)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Write a dataarray with 3 components of rank 2.
  !---------------------------------------------------------------------------------------------------------------------------------
  class(vtk_file), intent(inout) :: self     !< VTK file.
  class(*),        intent(in)    :: x(1:,1:) !< X component.
  class(*),        intent(in)    :: y(1:,1:) !< Y component.
  class(*),        intent(in)    :: z(1:,1:) !< Z component.
  integer(I4P)                   :: n_byte   !< Number of bytes
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction write_on_scratch_dataarray3_rank2

  module function write_on_scratch_dataarray3_rank3(self, x, y, z) result(n_byte)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Write a dataarray with 3 components of rank 3.
  !---------------------------------------------------------------------------------------------------------------------------------
  class(vtk_file), intent(inout) :: self        !< VTK file.
  class(*),        intent(in)    :: x(1:,1:,1:) !< X component.
  class(*),        intent(in)    :: y(1:,1:,1:) !< Y component.
  class(*),        intent(in)    :: z(1:,1:,1:) !< Z component.
  integer(I4P)                   :: n_byte      !< Number of bytes
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction write_on_scratch_dataarray3_rank3
