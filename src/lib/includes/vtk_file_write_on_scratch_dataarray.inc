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

  function write_on_scratch_dataarray3_rank1_R8P(self, x, y, z) result(n_byte)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Write a dataarray with 3 components of rank 1 (R8P).
  !---------------------------------------------------------------------------------------------------------------------------------
  class(vtk_file), intent(inout) :: self   !< VTK file.
  real(R8P),       intent(in)    :: x(1:)  !< X component.
  real(R8P),       intent(in)    :: y(1:)  !< Y component.
  real(R8P),       intent(in)    :: z(1:)  !< Z component.
  integer(I4P)                   :: n_byte !< Number of bytes
  integer(I4P)                   :: n      !< Counter.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  n_byte = self%write_on_scratch_dataarray(x=[(x(n), y(n), z(n), n=1,size(x, dim=1))])
  return
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction write_on_scratch_dataarray3_rank1_R8P

  function write_on_scratch_dataarray3_rank1_R4P(self, x, y, z) result(n_byte)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Write a dataarray with 3 components of rank 1 (R4P).
  !---------------------------------------------------------------------------------------------------------------------------------
  class(vtk_file), intent(inout) :: self   !< VTK file.
  real(R4P),       intent(in)    :: x(1:)  !< X component.
  real(R4P),       intent(in)    :: y(1:)  !< Y component.
  real(R4P),       intent(in)    :: z(1:)  !< Z component.
  integer(I4P)                   :: n_byte !< Number of bytes
  integer(I4P)                   :: n      !< Counter.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  n_byte = self%write_on_scratch_dataarray(x=[(x(n), y(n), z(n), n=1,size(x, dim=1))])
  return
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction write_on_scratch_dataarray3_rank1_R4P  

  function write_on_scratch_dataarray3_rank1_I8P(self, x, y, z) result(n_byte)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Write a dataarray with 3 components of rank 1 (I8P).
  !---------------------------------------------------------------------------------------------------------------------------------
  class(vtk_file), intent(inout) :: self   !< VTK file.
  integer(I8P),    intent(in)    :: x(1:)  !< X component.
  integer(I8P),    intent(in)    :: y(1:)  !< Y component.
  integer(I8P),    intent(in)    :: z(1:)  !< Z component.
  integer(I4P)                   :: n_byte !< Number of bytes
  integer(I4P)                   :: n      !< Counter.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  n_byte = self%write_on_scratch_dataarray(x=[(x(n), y(n), z(n), n=1,size(x, dim=1))])
  return
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction write_on_scratch_dataarray3_rank1_I8P  
 
  function write_on_scratch_dataarray3_rank1_I4P(self, x, y, z) result(n_byte)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Write a dataarray with 3 components of rank 1 (I4P).
  !---------------------------------------------------------------------------------------------------------------------------------
  class(vtk_file), intent(inout) :: self   !< VTK file.
  integer(I4P),    intent(in)    :: x(1:)  !< X component.
  integer(I4P),    intent(in)    :: y(1:)  !< Y component.
  integer(I4P),    intent(in)    :: z(1:)  !< Z component.
  integer(I4P)                   :: n_byte !< Number of bytes
  integer(I4P)                   :: n      !< Counter.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  n_byte = self%write_on_scratch_dataarray(x=[(x(n), y(n), z(n), n=1,size(x, dim=1))])
  return
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction write_on_scratch_dataarray3_rank1_I4P  
 
  function write_on_scratch_dataarray3_rank1_I2P(self, x, y, z) result(n_byte)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Write a dataarray with 3 components of rank 1 (I2P).
  !---------------------------------------------------------------------------------------------------------------------------------
  class(vtk_file), intent(inout) :: self   !< VTK file.
  integer(I2P),    intent(in)    :: x(1:)  !< X component.
  integer(I2P),    intent(in)    :: y(1:)  !< Y component.
  integer(I2P),    intent(in)    :: z(1:)  !< Z component.
  integer(I4P)                   :: n_byte !< Number of bytes
  integer(I4P)                   :: n      !< Counter.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  n_byte = self%write_on_scratch_dataarray(x=[(x(n), y(n), z(n), n=1,size(x, dim=1))])
  return
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction write_on_scratch_dataarray3_rank1_I2P  
 
  function write_on_scratch_dataarray3_rank1_I1P(self, x, y, z) result(n_byte)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Write a dataarray with 3 components of rank 1 (I1P).
  !---------------------------------------------------------------------------------------------------------------------------------
  class(vtk_file), intent(inout) :: self   !< VTK file.
  integer(I1P),    intent(in)    :: x(1:)  !< X component.
  integer(I1P),    intent(in)    :: y(1:)  !< Y component.
  integer(I1P),    intent(in)    :: z(1:)  !< Z component.
  integer(I4P)                   :: n_byte !< Number of bytes
  integer(I4P)                   :: n      !< Counter.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  n_byte = self%write_on_scratch_dataarray(x=[(x(n), y(n), z(n), n=1,size(x, dim=1))])
  return
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction write_on_scratch_dataarray3_rank1_I1P  

  function write_on_scratch_dataarray3_rank2_R8P(self, x, y, z) result(n_byte)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Write a dataarray with 3 components of rank 2 (R8P).
  !---------------------------------------------------------------------------------------------------------------------------------
  class(vtk_file), intent(inout) :: self     !< VTK file.
  real(R8P),       intent(in)    :: x(1:,1:) !< X component.
  real(R8P),       intent(in)    :: y(1:,1:) !< Y component.
  real(R8P),       intent(in)    :: z(1:,1:) !< Z component.
  integer(I4P)                   :: n_byte   !< Number of bytes
  integer(I4P)                   :: n1       !< Counter.
  integer(I4P)                   :: n2       !< Counter.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  n_byte = self%write_on_scratch_dataarray(x=[((x(n1,n2), y(n1,n2), z(n1,n2), n1=1,size(x, dim=1)),n2=1,size(x, dim=2))])
  return
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction write_on_scratch_dataarray3_rank2_R8P

  function write_on_scratch_dataarray3_rank2_R4P(self, x, y, z) result(n_byte)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Write a dataarray with 3 components of rank 2 (R4P).
  !---------------------------------------------------------------------------------------------------------------------------------
  class(vtk_file), intent(inout) :: self     !< VTK file.
  real(R4P),       intent(in)    :: x(1:,1:) !< X component.
  real(R4P),       intent(in)    :: y(1:,1:) !< Y component.
  real(R4P),       intent(in)    :: z(1:,1:) !< Z component.
  integer(I4P)                   :: n_byte   !< Number of bytes
  integer(I4P)                   :: n1       !< Counter.
  integer(I4P)                   :: n2       !< Counter.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  n_byte = self%write_on_scratch_dataarray(x=[((x(n1,n2), y(n1,n2), z(n1,n2), n1=1,size(x, dim=1)),n2=1,size(x, dim=2))])
  return
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction write_on_scratch_dataarray3_rank2_R4P  
                                                  
  function write_on_scratch_dataarray3_rank2_I8P(self, x, y, z) result(n_byte)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Write a dataarray with 3 components of rank 2 (I8P).
  !---------------------------------------------------------------------------------------------------------------------------------
  class(vtk_file), intent(inout) :: self     !< VTK file.
  integer(I8P),    intent(in)    :: x(1:,1:) !< X component.
  integer(I8P),    intent(in)    :: y(1:,1:) !< Y component.
  integer(I8P),    intent(in)    :: z(1:,1:) !< Z component.
  integer(I4P)                   :: n_byte   !< Number of bytes
  integer(I4P)                   :: n1       !< Counter.
  integer(I4P)                   :: n2       !< Counter.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  n_byte = self%write_on_scratch_dataarray(x=[((x(n1,n2), y(n1,n2), z(n1,n2), n1=1,size(x, dim=1)),n2=1,size(x, dim=2))])
  return
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction write_on_scratch_dataarray3_rank2_I8P
                                                     
  function write_on_scratch_dataarray3_rank2_I4P(self, x, y, z) result(n_byte)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Write a dataarray with 3 components of rank 2 (I4P).
  !---------------------------------------------------------------------------------------------------------------------------------
  class(vtk_file), intent(inout) :: self     !< VTK file.
  integer(I4P),    intent(in)    :: x(1:,1:) !< X component.
  integer(I4P),    intent(in)    :: y(1:,1:) !< Y component.
  integer(I4P),    intent(in)    :: z(1:,1:) !< Z component.
  integer(I4P)                   :: n_byte   !< Number of bytes
  integer(I4P)                   :: n1       !< Counter.
  integer(I4P)                   :: n2       !< Counter.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  n_byte = self%write_on_scratch_dataarray(x=[((x(n1,n2), y(n1,n2), z(n1,n2), n1=1,size(x, dim=1)),n2=1,size(x, dim=2))])
  return
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction write_on_scratch_dataarray3_rank2_I4P
                                                  
  function write_on_scratch_dataarray3_rank2_I2P(self, x, y, z) result(n_byte)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Write a dataarray with 3 components of rank 2 (I2P).
  !---------------------------------------------------------------------------------------------------------------------------------
  class(vtk_file), intent(inout) :: self     !< VTK file.
  integer(I2P),    intent(in)    :: x(1:,1:) !< X component.
  integer(I2P),    intent(in)    :: y(1:,1:) !< Y component.
  integer(I2P),    intent(in)    :: z(1:,1:) !< Z component.
  integer(I4P)                   :: n_byte   !< Number of bytes
  integer(I4P)                   :: n1       !< Counter.
  integer(I4P)                   :: n2       !< Counter.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  n_byte = self%write_on_scratch_dataarray(x=[((x(n1,n2), y(n1,n2), z(n1,n2), n1=1,size(x, dim=1)),n2=1,size(x, dim=2))])
  return
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction write_on_scratch_dataarray3_rank2_I2P
                                                  
  function write_on_scratch_dataarray3_rank2_I1P(self, x, y, z) result(n_byte)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Write a dataarray with 3 components of rank 2 (I1P).
  !---------------------------------------------------------------------------------------------------------------------------------
  class(vtk_file), intent(inout) :: self     !< VTK file.
  integer(I1P),    intent(in)    :: x(1:,1:) !< X component.
  integer(I1P),    intent(in)    :: y(1:,1:) !< Y component.
  integer(I1P),    intent(in)    :: z(1:,1:) !< Z component.
  integer(I4P)                   :: n_byte   !< Number of bytes
  integer(I4P)                   :: n1       !< Counter.
  integer(I4P)                   :: n2       !< Counter.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  n_byte = self%write_on_scratch_dataarray(x=[((x(n1,n2), y(n1,n2), z(n1,n2), n1=1,size(x, dim=1)),n2=1,size(x, dim=2))])
  return
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction write_on_scratch_dataarray3_rank2_I1P

  function write_on_scratch_dataarray3_rank3_R8P(self, x, y, z) result(n_byte)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Write a dataarray with 3 components of rank 3 (R8P).
  !---------------------------------------------------------------------------------------------------------------------------------
  class(vtk_file), intent(inout) :: self        !< VTK file.
  real(R8P),       intent(in)    :: x(1:,1:,1:) !< X component.
  real(R8P),       intent(in)    :: y(1:,1:,1:) !< Y component.
  real(R8P),       intent(in)    :: z(1:,1:,1:) !< Z component.
  integer(I4P)                   :: n_byte      !< Number of bytes
  integer(I4P)                   :: n1          !< Counter.
  integer(I4P)                   :: n2          !< Counter.
  integer(I4P)                   :: n3          !< Counter.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  n_byte = self%write_on_scratch_dataarray(x=[(((x(n1,n2,n3), y(n1,n2,n3), z(n1,n2,n3), &
                                           n1=1,size(x, dim=1)),n2=1,size(x, dim=2)),n3=1,size(x, dim=3))])
  return
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction write_on_scratch_dataarray3_rank3_R8P

  function write_on_scratch_dataarray3_rank3_R4P(self, x, y, z) result(n_byte)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Write a dataarray with 3 components of rank 3 (R4P).
  !---------------------------------------------------------------------------------------------------------------------------------
  class(vtk_file), intent(inout) :: self        !< VTK file.
  real(R4P),       intent(in)    :: x(1:,1:,1:) !< X component.
  real(R4P),       intent(in)    :: y(1:,1:,1:) !< Y component.
  real(R4P),       intent(in)    :: z(1:,1:,1:) !< Z component.
  integer(I4P)                   :: n_byte      !< Number of bytes
  integer(I4P)                   :: n1          !< Counter.
  integer(I4P)                   :: n2          !< Counter.
  integer(I4P)                   :: n3          !< Counter.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  n_byte = self%write_on_scratch_dataarray(x=[(((x(n1,n2,n3), y(n1,n2,n3), z(n1,n2,n3), &
                                           n1=1,size(x, dim=1)),n2=1,size(x, dim=2)),n3=1,size(x, dim=3))])
  return
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction write_on_scratch_dataarray3_rank3_R4P  
 
  function write_on_scratch_dataarray3_rank3_I8P(self, x, y, z) result(n_byte)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Write a dataarray with 3 components of rank 3 (I8P).
  !---------------------------------------------------------------------------------------------------------------------------------
  class(vtk_file), intent(inout) :: self        !< VTK file.
  integer(I8P),    intent(in)    :: x(1:,1:,1:) !< X component.
  integer(I8P),    intent(in)    :: y(1:,1:,1:) !< Y component.
  integer(I8P),    intent(in)    :: z(1:,1:,1:) !< Z component.
  integer(I4P)                   :: n_byte      !< Number of bytes
  integer(I4P)                   :: n1          !< Counter.
  integer(I4P)                   :: n2          !< Counter.
  integer(I4P)                   :: n3          !< Counter.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  n_byte = self%write_on_scratch_dataarray(x=[(((x(n1,n2,n3), y(n1,n2,n3), z(n1,n2,n3), &
                                           n1=1,size(x, dim=1)),n2=1,size(x, dim=2)),n3=1,size(x, dim=3))])
  return
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction write_on_scratch_dataarray3_rank3_I8P
 
  function write_on_scratch_dataarray3_rank3_I4P(self, x, y, z) result(n_byte)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Write a dataarray with 3 components of rank 3 (I4P).
  !---------------------------------------------------------------------------------------------------------------------------------
  class(vtk_file), intent(inout) :: self        !< VTK file.
  integer(I4P),    intent(in)    :: x(1:,1:,1:) !< X component.
  integer(I4P),    intent(in)    :: y(1:,1:,1:) !< Y component.
  integer(I4P),    intent(in)    :: z(1:,1:,1:) !< Z component.
  integer(I4P)                   :: n_byte      !< Number of bytes
  integer(I4P)                   :: n1          !< Counter.
  integer(I4P)                   :: n2          !< Counter.
  integer(I4P)                   :: n3          !< Counter.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  n_byte = self%write_on_scratch_dataarray(x=[(((x(n1,n2,n3), y(n1,n2,n3), z(n1,n2,n3), &
                                           n1=1,size(x, dim=1)),n2=1,size(x, dim=2)),n3=1,size(x, dim=3))])
  return
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction write_on_scratch_dataarray3_rank3_I4P
    
  function write_on_scratch_dataarray3_rank3_I2P(self, x, y, z) result(n_byte)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Write a dataarray with 3 components of rank 3 (I2P).
  !---------------------------------------------------------------------------------------------------------------------------------
  class(vtk_file), intent(inout) :: self        !< VTK file.
  integer(I2P),    intent(in)    :: x(1:,1:,1:) !< X component.
  integer(I2P),    intent(in)    :: y(1:,1:,1:) !< Y component.
  integer(I2P),    intent(in)    :: z(1:,1:,1:) !< Z component.
  integer(I4P)                   :: n_byte      !< Number of bytes
  integer(I4P)                   :: n1          !< Counter.
  integer(I4P)                   :: n2          !< Counter.
  integer(I4P)                   :: n3          !< Counter.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  n_byte = self%write_on_scratch_dataarray(x=[(((x(n1,n2,n3), y(n1,n2,n3), z(n1,n2,n3), &
                                           n1=1,size(x, dim=1)),n2=1,size(x, dim=2)),n3=1,size(x, dim=3))])
  return
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction write_on_scratch_dataarray3_rank3_I2P
 
  function write_on_scratch_dataarray3_rank3_I1P(self, x, y, z) result(n_byte)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Write a dataarray with 3 components of rank 3 (I1P).
  !---------------------------------------------------------------------------------------------------------------------------------
  class(vtk_file), intent(inout) :: self        !< VTK file.
  integer(I1P),    intent(in)    :: x(1:,1:,1:) !< X component.
  integer(I1P),    intent(in)    :: y(1:,1:,1:) !< Y component.
  integer(I1P),    intent(in)    :: z(1:,1:,1:) !< Z component.
  integer(I4P)                   :: n_byte      !< Number of bytes
  integer(I4P)                   :: n1          !< Counter.
  integer(I4P)                   :: n2          !< Counter.
  integer(I4P)                   :: n3          !< Counter.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  n_byte = self%write_on_scratch_dataarray(x=[(((x(n1,n2,n3), y(n1,n2,n3), z(n1,n2,n3), &
                                           n1=1,size(x, dim=1)),n2=1,size(x, dim=2)),n3=1,size(x, dim=3))])
  return
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction write_on_scratch_dataarray3_rank3_I1P
