!< Dataarray encode ascii submodule interface for VTK file class.
  module function encode_ascii_dataarray1_rank1_R8P(self, x) result(code)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Encode (Base64) a dataarray with 1 components of rank 1 (R8P).
  !---------------------------------------------------------------------------------------------------------------------------------
  class(vtk_file), intent(in)   :: self  !< VTK file.
  real(R8P),       intent(in)   :: x(1:) !< Data variable.
  character(len=:), allocatable :: code  !< Encoded base64 dataarray.
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction encode_ascii_dataarray1_rank1_R8P

  module function encode_ascii_dataarray1_rank1_R4P(self, x) result(code)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Encode (Base64) a dataarray with 1 components of rank 1 (R4P).
  !---------------------------------------------------------------------------------------------------------------------------------
  class(vtk_file), intent(in)   :: self  !< VTK file.
  real(R4P),       intent(in)   :: x(1:) !< Data variable.
  character(len=:), allocatable :: code  !< Encoded base64 dataarray.
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction encode_ascii_dataarray1_rank1_R4P

  module function encode_ascii_dataarray1_rank1_I8P(self, x) result(code)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Encode (Base64) a dataarray with 1 components of rank 1 (I8P).
  !---------------------------------------------------------------------------------------------------------------------------------
  class(vtk_file), intent(in)   :: self  !< VTK file.
  integer(I8P),    intent(in)   :: x(1:) !< Data variable.
  character(len=:), allocatable :: code  !< Encoded base64 dataarray.
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction encode_ascii_dataarray1_rank1_I8P

  module function encode_ascii_dataarray1_rank1_I4P(self, x) result(code)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Encode (Base64) a dataarray with 1 components of rank 1 (I4P).
  !---------------------------------------------------------------------------------------------------------------------------------
  class(vtk_file), intent(in)   :: self  !< VTK file.
  integer(I4P),    intent(in)   :: x(1:) !< Data variable.
  character(len=:), allocatable :: code  !< Encoded base64 dataarray.
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction encode_ascii_dataarray1_rank1_I4P

  module function encode_ascii_dataarray1_rank1_I2P(self, x) result(code)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Encode (Base64) a dataarray with 1 components of rank 1 (I2P).
  !---------------------------------------------------------------------------------------------------------------------------------
  class(vtk_file), intent(in)   :: self  !< VTK file.
  integer(I2P),    intent(in)   :: x(1:) !< Data variable.
  character(len=:), allocatable :: code  !< Encoded base64 dataarray.
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction encode_ascii_dataarray1_rank1_I2P

  module function encode_ascii_dataarray1_rank1_I1P(self, x) result(code)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Encode (Base64) a dataarray with 1 components of rank 1 (I1P).
  !---------------------------------------------------------------------------------------------------------------------------------
  class(vtk_file), intent(in)   :: self  !< VTK file.
  integer(I1P),    intent(in)   :: x(1:) !< Data variable.
  character(len=:), allocatable :: code  !< Encoded base64 dataarray.
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction encode_ascii_dataarray1_rank1_I1P

  module function encode_ascii_dataarray1_rank2_R8P(self, x) result(code)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Encode (Base64) a dataarray with 1 components of rank 2 (R8P).
  !---------------------------------------------------------------------------------------------------------------------------------
  class(vtk_file), intent(in)   :: self     !< VTK file.
  real(R8P),       intent(in)   :: x(1:,1:) !< Data variable
  character(len=:), allocatable :: code     !< Encoded base64 dataarray.
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction encode_ascii_dataarray1_rank2_R8P

  module function encode_ascii_dataarray1_rank2_R4P(self, x) result(code)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Encode (Base64) a dataarray with 1 components of rank 2 (R4P).
  !---------------------------------------------------------------------------------------------------------------------------------
  class(vtk_file), intent(in)   :: self     !< VTK file.
  real(R4P),       intent(in)   :: x(1:,1:) !< Data variable
  character(len=:), allocatable :: code     !< Encoded base64 dataarray.
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction encode_ascii_dataarray1_rank2_R4P

  module function encode_ascii_dataarray1_rank2_I8P(self, x) result(code)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Encode (Base64) a dataarray with 1 components of rank 2 (I8P).
  !---------------------------------------------------------------------------------------------------------------------------------
  class(vtk_file), intent(in)   :: self     !< VTK file.
  integer(I8P),    intent(in)   :: x(1:,1:) !< Data variable
  character(len=:), allocatable :: code     !< Encoded base64 dataarray.
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction encode_ascii_dataarray1_rank2_I8P

  module function encode_ascii_dataarray1_rank2_I4P(self, x) result(code)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Encode (Base64) a dataarray with 1 components of rank 2 (I4P).
  !---------------------------------------------------------------------------------------------------------------------------------
  class(vtk_file), intent(in)   :: self     !< VTK file.
  integer(I4P),    intent(in)   :: x(1:,1:) !< Data variable
  character(len=:), allocatable :: code     !< Encoded base64 dataarray.
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction encode_ascii_dataarray1_rank2_I4P

  module function encode_ascii_dataarray1_rank2_I2P(self, x) result(code)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Encode (Base64) a dataarray with 1 components of rank 2 (I2P).
  !---------------------------------------------------------------------------------------------------------------------------------
  class(vtk_file), intent(in)   :: self     !< VTK file.
  integer(I2P),    intent(in)   :: x(1:,1:) !< Data variable
  character(len=:), allocatable :: code     !< Encoded base64 dataarray.
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction encode_ascii_dataarray1_rank2_I2P

  module function encode_ascii_dataarray1_rank2_I1P(self, x) result(code)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Encode (Base64) a dataarray with 1 components of rank 2 (I1P).
  !---------------------------------------------------------------------------------------------------------------------------------
  class(vtk_file), intent(in)   :: self     !< VTK file.
  integer(I1P),    intent(in)   :: x(1:,1:) !< Data variable
  character(len=:), allocatable :: code     !< Encoded base64 dataarray.
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction encode_ascii_dataarray1_rank2_I1P

  module function encode_ascii_dataarray1_rank3_R8P(self, x) result(code)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Encode (Base64) a dataarray with 1 components of rank 3 (R8P).
  !---------------------------------------------------------------------------------------------------------------------------------
  class(vtk_file), intent(in)   :: self        !< VTK file.
  real(R8P),       intent(in)   :: x(1:,1:,1:) !< Data variable
  character(len=:), allocatable :: code        !< Encoded base64 dataarray.
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction encode_ascii_dataarray1_rank3_R8P

  module function encode_ascii_dataarray1_rank3_R4P(self, x) result(code)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Encode (Base64) a dataarray with 1 components of rank 3 (R4P).
  !---------------------------------------------------------------------------------------------------------------------------------
  class(vtk_file), intent(in)   :: self        !< VTK file.
  real(R4P),       intent(in)   :: x(1:,1:,1:) !< Data variable
  character(len=:), allocatable :: code        !< Encoded base64 dataarray.
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction encode_ascii_dataarray1_rank3_R4P

  module function encode_ascii_dataarray1_rank3_I8P(self, x) result(code)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Encode (Base64) a dataarray with 1 components of rank 3 (I8P).
  !---------------------------------------------------------------------------------------------------------------------------------
  class(vtk_file), intent(in)   :: self        !< VTK file.
  integer(I8P),    intent(in)   :: x(1:,1:,1:) !< Data variable
  character(len=:), allocatable :: code        !< Encoded base64 dataarray.
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction encode_ascii_dataarray1_rank3_I8P

  module function encode_ascii_dataarray1_rank3_I4P(self, x) result(code)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Encode (Base64) a dataarray with 1 components of rank 3 (I4P).
  !---------------------------------------------------------------------------------------------------------------------------------
  class(vtk_file), intent(in)   :: self        !< VTK file.
  integer(I4P),    intent(in)   :: x(1:,1:,1:) !< Data variable
  character(len=:), allocatable :: code        !< Encoded base64 dataarray.
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction encode_ascii_dataarray1_rank3_I4P

  module function encode_ascii_dataarray1_rank3_I2P(self, x) result(code)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Encode (Base64) a dataarray with 1 components of rank 3 (I2P).
  !---------------------------------------------------------------------------------------------------------------------------------
  class(vtk_file), intent(in)   :: self        !< VTK file.
  integer(I2P),    intent(in)   :: x(1:,1:,1:) !< Data variable
  character(len=:), allocatable :: code        !< Encoded base64 dataarray.
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction encode_ascii_dataarray1_rank3_I2P

  module function encode_ascii_dataarray1_rank3_I1P(self, x) result(code)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Encode (Base64) a dataarray with 1 components of rank 3 (I1P).
  !---------------------------------------------------------------------------------------------------------------------------------
  class(vtk_file), intent(in)   :: self        !< VTK file.
  integer(I1P),    intent(in)   :: x(1:,1:,1:) !< Data variable
  character(len=:), allocatable :: code        !< Encoded base64 dataarray.
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction encode_ascii_dataarray1_rank3_I1P

  module function encode_ascii_dataarray1_rank4_R8P(self, x) result(code)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Encode (Base64) a dataarray with 1 components of rank 4 (R8P).
  !---------------------------------------------------------------------------------------------------------------------------------
  class(vtk_file), intent(in)   :: self           !< VTK file.
  real(R8P),       intent(in)   :: x(1:,1:,1:,1:) !< Data variable.
  character(len=:), allocatable :: code           !< Encoded base64 dataarray.
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction encode_ascii_dataarray1_rank4_R8P

  module function encode_ascii_dataarray1_rank4_R4P(self, x) result(code)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Encode (Base64) a dataarray with 1 components of rank 4 (R4P).
  !---------------------------------------------------------------------------------------------------------------------------------
  class(vtk_file), intent(in)   :: self           !< VTK file.
  real(R4P),       intent(in)   :: x(1:,1:,1:,1:) !< Data variable.
  character(len=:), allocatable :: code           !< Encoded base64 dataarray.
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction encode_ascii_dataarray1_rank4_R4P

  module function encode_ascii_dataarray1_rank4_I8P(self, x) result(code)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Encode (Base64) a dataarray with 1 components of rank 4 (I8P).
  !---------------------------------------------------------------------------------------------------------------------------------
  class(vtk_file), intent(in)   :: self           !< VTK file.
  integer(I8P),    intent(in)   :: x(1:,1:,1:,1:) !< Data variable.
  character(len=:), allocatable :: code           !< Encoded base64 dataarray.
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction encode_ascii_dataarray1_rank4_I8P

  module function encode_ascii_dataarray1_rank4_I4P(self, x) result(code)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Encode (Base64) a dataarray with 1 components of rank 4 (I4P).
  !---------------------------------------------------------------------------------------------------------------------------------
  class(vtk_file), intent(in)   :: self           !< VTK file.
  integer(I4P),    intent(in)   :: x(1:,1:,1:,1:) !< Data variable.
  character(len=:), allocatable :: code           !< Encoded base64 dataarray.
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction encode_ascii_dataarray1_rank4_I4P

  module function encode_ascii_dataarray1_rank4_I2P(self, x) result(code)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Encode (Base64) a dataarray with 1 components of rank 4 (I2P).
  !---------------------------------------------------------------------------------------------------------------------------------
  class(vtk_file), intent(in)   :: self           !< VTK file.
  integer(I2P),    intent(in)   :: x(1:,1:,1:,1:) !< Data variable.
  character(len=:), allocatable :: code           !< Encoded base64 dataarray.
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction encode_ascii_dataarray1_rank4_I2P

  module function encode_ascii_dataarray1_rank4_I1P(self, x) result(code)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Encode (Base64) a dataarray with 1 components of rank 4 (I1P).
  !---------------------------------------------------------------------------------------------------------------------------------
  class(vtk_file), intent(in)   :: self           !< VTK file.
  integer(I1P),    intent(in)   :: x(1:,1:,1:,1:) !< Data variable.
  character(len=:), allocatable :: code           !< Encoded base64 dataarray.
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction encode_ascii_dataarray1_rank4_I1P

  module function encode_ascii_dataarray3_rank1_R8P(self, x, y, z) result(code)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Encode (Base64) a dataarray with 3 components of rank 1 (R8P).
  !---------------------------------------------------------------------------------------------------------------------------------
  class(vtk_file), intent(in)   :: self  !< VTK file.
  real(R8P),       intent(in)   :: x(1:) !< X component.
  real(R8P),       intent(in)   :: y(1:) !< Y component.
  real(R8P),       intent(in)   :: z(1:) !< Z component.
  character(len=:), allocatable :: code  !< Encoded base64 dataarray.
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction encode_ascii_dataarray3_rank1_R8P

  module function encode_ascii_dataarray3_rank1_R4P(self, x, y, z) result(code)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Encode (Base64) a dataarray with 3 components of rank 1 (R4P).
  !---------------------------------------------------------------------------------------------------------------------------------
  class(vtk_file), intent(in)   :: self  !< VTK file.
  real(R4P),       intent(in)   :: x(1:) !< X component.
  real(R4P),       intent(in)   :: y(1:) !< Y component.
  real(R4P),       intent(in)   :: z(1:) !< Z component.
  character(len=:), allocatable :: code  !< Encoded base64 dataarray.
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction encode_ascii_dataarray3_rank1_R4P

  module function encode_ascii_dataarray3_rank1_I8P(self, x, y, z) result(code)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Encode (Base64) a dataarray with 3 components of rank 1 (I8P).
  !---------------------------------------------------------------------------------------------------------------------------------
  class(vtk_file), intent(in)   :: self  !< VTK file.
  integer(I8P),    intent(in)   :: x(1:) !< X component.
  integer(I8P),    intent(in)   :: y(1:) !< Y component.
  integer(I8P),    intent(in)   :: z(1:) !< Z component.
  character(len=:), allocatable :: code  !< Encoded base64 dataarray.
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction encode_ascii_dataarray3_rank1_I8P

  module function encode_ascii_dataarray3_rank1_I4P(self, x, y, z) result(code)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Encode (Base64) a dataarray with 3 components of rank 1 (I4P).
  !---------------------------------------------------------------------------------------------------------------------------------
  class(vtk_file), intent(in)   :: self  !< VTK file.
  integer(I4P),    intent(in)   :: x(1:) !< X component.
  integer(I4P),    intent(in)   :: y(1:) !< Y component.
  integer(I4P),    intent(in)   :: z(1:) !< Z component.
  character(len=:), allocatable :: code  !< Encoded base64 dataarray.
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction encode_ascii_dataarray3_rank1_I4P

  module function encode_ascii_dataarray3_rank1_I2P(self, x, y, z) result(code)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Encode (Base64) a dataarray with 3 components of rank 1 (I2P).
  !---------------------------------------------------------------------------------------------------------------------------------
  class(vtk_file), intent(in)   :: self  !< VTK file.
  integer(I2P),    intent(in)   :: x(1:) !< X component.
  integer(I2P),    intent(in)   :: y(1:) !< Y component.
  integer(I2P),    intent(in)   :: z(1:) !< Z component.
  character(len=:), allocatable :: code  !< Encoded base64 dataarray.
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction encode_ascii_dataarray3_rank1_I2P

  module function encode_ascii_dataarray3_rank1_I1P(self, x, y, z) result(code)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Encode (Base64) a dataarray with 3 components of rank 1 (I1P).
  !---------------------------------------------------------------------------------------------------------------------------------
  class(vtk_file), intent(in)   :: self  !< VTK file.
  integer(I1P),    intent(in)   :: x(1:) !< X component.
  integer(I1P),    intent(in)   :: y(1:) !< Y component.
  integer(I1P),    intent(in)   :: z(1:) !< Z component.
  character(len=:), allocatable :: code  !< Encoded base64 dataarray.
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction encode_ascii_dataarray3_rank1_I1P

  module function encode_ascii_dataarray3_rank3_R8P(self, x, y, z) result(code)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Encode (Base64) a dataarray with 3 components of rank 3 (R8P).
  !---------------------------------------------------------------------------------------------------------------------------------
  class(vtk_file), intent(in)   :: self        !< VTK file.
  real(R8P),       intent(in)   :: x(1:,1:,1:) !< X component.
  real(R8P),       intent(in)   :: y(1:,1:,1:) !< Y component.
  real(R8P),       intent(in)   :: z(1:,1:,1:) !< Z component.
  character(len=:), allocatable :: code        !< Encoded base64 dataarray.
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction encode_ascii_dataarray3_rank3_R8P

  module function encode_ascii_dataarray3_rank3_R4P(self, x, y, z) result(code)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Encode (Base64) a dataarray with 3 components of rank 3 (R4P).
  !---------------------------------------------------------------------------------------------------------------------------------
  class(vtk_file), intent(in)   :: self        !< VTK file.
  real(R4P),       intent(in)   :: x(1:,1:,1:) !< X component.
  real(R4P),       intent(in)   :: y(1:,1:,1:) !< Y component.
  real(R4P),       intent(in)   :: z(1:,1:,1:) !< Z component.
  character(len=:), allocatable :: code        !< Encoded base64 dataarray.
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction encode_ascii_dataarray3_rank3_R4P

  module function encode_ascii_dataarray3_rank3_I8P(self, x, y, z) result(code)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Encode (Base64) a dataarray with 3 components of rank 3 (I8P).
  !---------------------------------------------------------------------------------------------------------------------------------
  class(vtk_file), intent(in)   :: self        !< VTK file.
  integer(I8P),    intent(in)   :: x(1:,1:,1:) !< X component.
  integer(I8P),    intent(in)   :: y(1:,1:,1:) !< Y component.
  integer(I8P),    intent(in)   :: z(1:,1:,1:) !< Z component.
  character(len=:), allocatable :: code        !< Encoded base64 dataarray.
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction encode_ascii_dataarray3_rank3_I8P

  module function encode_ascii_dataarray3_rank3_I4P(self, x, y, z) result(code)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Encode (Base64) a dataarray with 3 components of rank 3 (I4P).
  !---------------------------------------------------------------------------------------------------------------------------------
  class(vtk_file), intent(in)   :: self        !< VTK file.
  integer(I4P),    intent(in)   :: x(1:,1:,1:) !< X component.
  integer(I4P),    intent(in)   :: y(1:,1:,1:) !< Y component.
  integer(I4P),    intent(in)   :: z(1:,1:,1:) !< Z component.
  character(len=:), allocatable :: code        !< Encoded base64 dataarray.
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction encode_ascii_dataarray3_rank3_I4P

  module function encode_ascii_dataarray3_rank3_I2P(self, x, y, z) result(code)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Encode (Base64) a dataarray with 3 components of rank 3 (I2P).
  !---------------------------------------------------------------------------------------------------------------------------------
  class(vtk_file), intent(in)   :: self        !< VTK file.
  integer(I2P),    intent(in)   :: x(1:,1:,1:) !< X component.
  integer(I2P),    intent(in)   :: y(1:,1:,1:) !< Y component.
  integer(I2P),    intent(in)   :: z(1:,1:,1:) !< Z component.
  character(len=:), allocatable :: code        !< Encoded base64 dataarray.
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction encode_ascii_dataarray3_rank3_I2P

  module function encode_ascii_dataarray3_rank3_I1P(self, x, y, z) result(code)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Encode (Base64) a dataarray with 3 components of rank 3 (I1P).
  !---------------------------------------------------------------------------------------------------------------------------------
  class(vtk_file), intent(in)   :: self        !< VTK file.
  integer(I1P),    intent(in)   :: x(1:,1:,1:) !< X component.
  integer(I1P),    intent(in)   :: y(1:,1:,1:) !< Y component.
  integer(I1P),    intent(in)   :: z(1:,1:,1:) !< Z component.
  character(len=:), allocatable :: code        !< Encoded base64 dataarray.
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction encode_ascii_dataarray3_rank3_I1P
