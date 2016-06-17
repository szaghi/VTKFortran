!< Implementation of dataarray Base64 encode of VTK file class.
submodule (vtk_file_class) encode_base64_dataarray
!-----------------------------------------------------------------------------------------------------------------------------------
!< Implementation of dataarray Base64 encode of VTK file class.
!-----------------------------------------------------------------------------------------------------------------------------------

!-----------------------------------------------------------------------------------------------------------------------------------
implicit none
!-----------------------------------------------------------------------------------------------------------------------------------
contains
  module function encode_base64_dataarray1_rank1_R8P(x) result(code)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Encode (Base64) a dataarray with 1 components of rank 1 (R8P).
  !---------------------------------------------------------------------------------------------------------------------------------
  real(R8P), intent(in)         :: x(1:) !< Data variable.
  character(len=:), allocatable :: code  !< Encoded base64 dataarray.
  integer(I1P),     allocatable :: xp(:) !< Packed data.
  integer(I4P)                  :: nn    !< Number of elements.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  nn = size(x, dim=1)
  call pack_data(a1=[int(nn*BYR8P, I4P)], a2=reshape(x, [nn]), packed=xp)
  call b64_encode(n=xp, code=code)
  return
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction encode_base64_dataarray1_rank1_R8P

  module function encode_base64_dataarray1_rank1_R4P(x) result(code)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Encode (Base64) a dataarray with 1 components of rank 1 (R4P).
  !---------------------------------------------------------------------------------------------------------------------------------
  real(R4P), intent(in)         :: x(1:) !< Data variable.
  character(len=:), allocatable :: code  !< Encoded base64 dataarray.
  integer(I1P),     allocatable :: xp(:) !< Packed data.
  integer(I4P)                  :: nn    !< Number of elements.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  nn = size(x, dim=1)
  call pack_data(a1=[int(nn*BYR4P, I4P)], a2=reshape(x, [nn]), packed=xp)
  call b64_encode(n=xp, code=code)
  return
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction encode_base64_dataarray1_rank1_R4P

  module function encode_base64_dataarray1_rank1_I8P(x) result(code)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Encode (Base64) a dataarray with 1 components of rank 1 (I8P).
  !---------------------------------------------------------------------------------------------------------------------------------
  integer(I8P), intent(in)      :: x(1:) !< Data variable.
  character(len=:), allocatable :: code  !< Encoded base64 dataarray.
  integer(I1P),     allocatable :: xp(:) !< Packed data.
  integer(I4P)                  :: nn    !< Number of elements.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  nn = size(x, dim=1)
  call pack_data(a1=[int(nn*BYI8P, I4P)], a2=reshape(x, [nn]), packed=xp)
  call b64_encode(n=xp, code=code)
  return
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction encode_base64_dataarray1_rank1_I8P

  module function encode_base64_dataarray1_rank1_I4P(x) result(code)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Encode (Base64) a dataarray with 1 components of rank 1 (I4P).
  !---------------------------------------------------------------------------------------------------------------------------------
  integer(I4P), intent(in)      :: x(1:) !< Data variable.
  character(len=:), allocatable :: code  !< Encoded base64 dataarray.
  integer(I1P),     allocatable :: xp(:) !< Packed data.
  integer(I4P)                  :: nn    !< Number of elements.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  nn = size(x, dim=1)
  xp = transfer([int(nn*BYI4P, I4P), reshape(x, [nn])], xp)
  call b64_encode(n=xp, code=code)
  return
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction encode_base64_dataarray1_rank1_I4P

  module function encode_base64_dataarray1_rank1_I2P(x) result(code)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Encode (Base64) a dataarray with 1 components of rank 1 (I2P).
  !---------------------------------------------------------------------------------------------------------------------------------
  integer(I2P), intent(in)      :: x(1:) !< Data variable.
  character(len=:), allocatable :: code  !< Encoded base64 dataarray.
  integer(I1P),     allocatable :: xp(:) !< Packed data.
  integer(I4P)                  :: nn    !< Number of elements.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  nn = size(x, dim=1)
  call pack_data(a1=[int(nn*BYI2P, I4P)], a2=reshape(x, [nn]), packed=xp)
  call b64_encode(n=xp, code=code)
  return
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction encode_base64_dataarray1_rank1_I2P

  module function encode_base64_dataarray1_rank1_I1P(x) result(code)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Encode (Base64) a dataarray with 1 components of rank 1 (I1P).
  !---------------------------------------------------------------------------------------------------------------------------------
  integer(I1P), intent(in)      :: x(1:) !< Data variable.
  character(len=:), allocatable :: code  !< Encoded base64 dataarray.
  integer(I1P),     allocatable :: xp(:) !< Packed data.
  integer(I4P)                  :: nn    !< Number of elements.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  nn = size(x, dim=1)
  call pack_data(a1=[int(nn*BYI1P, I4P)], a2=reshape(x, [nn]), packed=xp)
  call b64_encode(n=xp, code=code)
  return
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction encode_base64_dataarray1_rank1_I1P

  module function encode_base64_dataarray1_rank2_R8P(x) result(code)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Encode (Base64) a dataarray with 1 components of rank 2 (R8P).
  !---------------------------------------------------------------------------------------------------------------------------------
  real(R8P), intent(in)         :: x(1:,1:) !< Data variable
  character(len=:), allocatable :: code     !< Encoded base64 dataarray.
  integer(I1P),     allocatable :: xp(:)    !< Packed data.
  integer(I4P)                  :: nn       !< Number of elements.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  nn = size(x, dim=1)*size(x, dim=2)
  call pack_data(a1=[int(nn*BYR8P, I4P)], a2=reshape(x, [nn]), packed=xp)
  call b64_encode(n=xp, code=code)
  return
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction encode_base64_dataarray1_rank2_R8P

  module function encode_base64_dataarray1_rank2_R4P(x) result(code)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Encode (Base64) a dataarray with 1 components of rank 2 (R4P).
  !---------------------------------------------------------------------------------------------------------------------------------
  real(R4P), intent(in)         :: x(1:,1:) !< Data variable
  character(len=:), allocatable :: code     !< Encoded base64 dataarray.
  integer(I1P),     allocatable :: xp(:)    !< Packed data.
  integer(I4P)                  :: nn       !< Number of elements.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  nn = size(x, dim=1)*size(x, dim=2)
  call pack_data(a1=[int(nn*BYR4P, I4P)], a2=reshape(x, [nn]), packed=xp)
  call b64_encode(n=xp, code=code)
  return
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction encode_base64_dataarray1_rank2_R4P

  module function encode_base64_dataarray1_rank2_I8P(x) result(code)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Encode (Base64) a dataarray with 1 components of rank 2 (I8P).
  !---------------------------------------------------------------------------------------------------------------------------------
  integer(I8P), intent(in)      :: x(1:,1:) !< Data variable
  character(len=:), allocatable :: code     !< Encoded base64 dataarray.
  integer(I1P),     allocatable :: xp(:)    !< Packed data.
  integer(I4P)                  :: nn       !< Number of elements.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  nn = size(x, dim=1)*size(x, dim=2)
  call pack_data(a1=[int(nn*BYI8P, I4P)], a2=reshape(x, [nn]), packed=xp)
  call b64_encode(n=xp, code=code)
  return
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction encode_base64_dataarray1_rank2_I8P

  module function encode_base64_dataarray1_rank2_I4P(x) result(code)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Encode (Base64) a dataarray with 1 components of rank 2 (I4P).
  !---------------------------------------------------------------------------------------------------------------------------------
  integer(I4P), intent(in)      :: x(1:,1:) !< Data variable
  character(len=:), allocatable :: code     !< Encoded base64 dataarray.
  integer(I1P),     allocatable :: xp(:)    !< Packed data.
  integer(I4P)                  :: nn       !< Number of elements.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  nn = size(x, dim=1)*size(x, dim=2)
  xp = transfer([int(nn*BYI4P, I4P), reshape(x, [nn])], xp)
  call b64_encode(n=xp, code=code)
  return
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction encode_base64_dataarray1_rank2_I4P

  module function encode_base64_dataarray1_rank2_I2P(x) result(code)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Encode (Base64) a dataarray with 1 components of rank 2 (I2P).
  !---------------------------------------------------------------------------------------------------------------------------------
  integer(I2P), intent(in)      :: x(1:,1:) !< Data variable
  character(len=:), allocatable :: code     !< Encoded base64 dataarray.
  integer(I1P),     allocatable :: xp(:)    !< Packed data.
  integer(I4P)                  :: nn       !< Number of elements.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  nn = size(x, dim=1)*size(x, dim=2)
  call pack_data(a1=[int(nn*BYI2P, I4P)], a2=reshape(x, [nn]), packed=xp)
  call b64_encode(n=xp, code=code)
  return
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction encode_base64_dataarray1_rank2_I2P

  module function encode_base64_dataarray1_rank2_I1P(x) result(code)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Encode (Base64) a dataarray with 1 components of rank 2 (I1P).
  !---------------------------------------------------------------------------------------------------------------------------------
  integer(I1P), intent(in)      :: x(1:,1:) !< Data variable
  character(len=:), allocatable :: code     !< Encoded base64 dataarray.
  integer(I1P),     allocatable :: xp(:)    !< Packed data.
  integer(I4P)                  :: nn       !< Number of elements.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  nn = size(x, dim=1)*size(x, dim=2)
  call pack_data(a1=[int(nn*BYI1P, I4P)], a2=reshape(x, [nn]), packed=xp)
  call b64_encode(n=xp, code=code)
  return
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction encode_base64_dataarray1_rank2_I1P

  module function encode_base64_dataarray1_rank3_R8P(x) result(code)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Encode (Base64) a dataarray with 1 components of rank 3 (R8P).
  !---------------------------------------------------------------------------------------------------------------------------------
  real(R8P), intent(in)         :: x(1:,1:,1:) !< Data variable
  character(len=:), allocatable :: code        !< Encoded base64 dataarray.
  integer(I1P),     allocatable :: xp(:)       !< Packed data.
  integer(I4P)                  :: nn          !< Number of elements.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  nn = size(x, dim=1)*size(x, dim=2)*size(x, dim=3)
  call pack_data(a1=[int(nn*BYR8P, I4P)], a2=reshape(x, [nn]), packed=xp)
  call b64_encode(n=xp, code=code)
  return
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction encode_base64_dataarray1_rank3_R8P

  module function encode_base64_dataarray1_rank3_R4P(x) result(code)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Encode (Base64) a dataarray with 1 components of rank 3 (R4P).
  !---------------------------------------------------------------------------------------------------------------------------------
  real(R4P), intent(in)         :: x(1:,1:,1:) !< Data variable
  character(len=:), allocatable :: code        !< Encoded base64 dataarray.
  integer(I1P),     allocatable :: xp(:)       !< Packed data.
  integer(I4P)                  :: nn          !< Number of elements.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  nn = size(x, dim=1)*size(x, dim=2)*size(x, dim=3)
  call pack_data(a1=[int(nn*BYR4P, I4P)], a2=reshape(x, [nn]), packed=xp)
  call b64_encode(n=xp, code=code)
  return
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction encode_base64_dataarray1_rank3_R4P

  module function encode_base64_dataarray1_rank3_I8P(x) result(code)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Encode (Base64) a dataarray with 1 components of rank 3 (I8P).
  !---------------------------------------------------------------------------------------------------------------------------------
  integer(I8P), intent(in)      :: x(1:,1:,1:) !< Data variable
  character(len=:), allocatable :: code        !< Encoded base64 dataarray.
  integer(I1P),     allocatable :: xp(:)       !< Packed data.
  integer(I4P)                  :: nn          !< Number of elements.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  nn = size(x, dim=1)*size(x, dim=2)*size(x, dim=3)
  call pack_data(a1=[int(nn*BYI8P, I4P)], a2=reshape(x, [nn]), packed=xp)
  call b64_encode(n=xp, code=code)
  return
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction encode_base64_dataarray1_rank3_I8P

  module function encode_base64_dataarray1_rank3_I4P(x) result(code)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Encode (Base64) a dataarray with 1 components of rank 3 (I4P).
  !---------------------------------------------------------------------------------------------------------------------------------
  integer(I4P), intent(in)      :: x(1:,1:,1:) !< Data variable
  character(len=:), allocatable :: code        !< Encoded base64 dataarray.
  integer(I1P),     allocatable :: xp(:)       !< Packed data.
  integer(I4P)                  :: nn          !< Number of elements.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  nn = size(x, dim=1)*size(x, dim=2)*size(x, dim=3)
  xp = transfer([int(nn*BYI4P, I4P), reshape(x, [nn])], xp)
  call b64_encode(n=xp, code=code)
  return
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction encode_base64_dataarray1_rank3_I4P

  module function encode_base64_dataarray1_rank3_I2P(x) result(code)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Encode (Base64) a dataarray with 1 components of rank 3 (I2P).
  !---------------------------------------------------------------------------------------------------------------------------------
  integer(I2P), intent(in)      :: x(1:,1:,1:) !< Data variable
  character(len=:), allocatable :: code        !< Encoded base64 dataarray.
  integer(I1P),     allocatable :: xp(:)       !< Packed data.
  integer(I4P)                  :: nn          !< Number of elements.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  nn = size(x, dim=1)*size(x, dim=2)*size(x, dim=3)
  call pack_data(a1=[int(nn*BYI2P, I4P)], a2=reshape(x, [nn]), packed=xp)
  call b64_encode(n=xp, code=code)
  return
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction encode_base64_dataarray1_rank3_I2P

  module function encode_base64_dataarray1_rank3_I1P(x) result(code)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Encode (Base64) a dataarray with 1 components of rank 3 (I1P).
  !---------------------------------------------------------------------------------------------------------------------------------
  integer(I1P), intent(in)      :: x(1:,1:,1:) !< Data variable
  character(len=:), allocatable :: code        !< Encoded base64 dataarray.
  integer(I1P),     allocatable :: xp(:)       !< Packed data.
  integer(I4P)                  :: nn          !< Number of elements.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  nn = size(x, dim=1)*size(x, dim=2)*size(x, dim=3)
  call pack_data(a1=[int(nn*BYI1P, I4P)], a2=reshape(x, [nn]), packed=xp)
  call b64_encode(n=xp, code=code)
  return
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction encode_base64_dataarray1_rank3_I1P

  module function encode_base64_dataarray1_rank4_R8P(x) result(code)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Encode (Base64) a dataarray with 1 components of rank 4 (R8P).
  !---------------------------------------------------------------------------------------------------------------------------------
  real(R8P), intent(in)         :: x(1:,1:,1:,1:) !< Data variable.
  character(len=:), allocatable :: code           !< Encoded base64 dataarray.
  integer(I1P),     allocatable :: xp(:)          !< Packed data.
  integer(I4P)                  :: nn             !< Number of elements.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  nn = size(x, dim=1)*size(x, dim=2)*size(x, dim=3)*size(x, dim=4)
  call pack_data(a1=[int(nn*BYR8P, I4P)], a2=reshape(x, [nn]), packed=xp)
  call b64_encode(n=xp, code=code)
  return
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction encode_base64_dataarray1_rank4_R8P

  module function encode_base64_dataarray1_rank4_R4P(x) result(code)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Encode (Base64) a dataarray with 1 components of rank 4 (R4P).
  !---------------------------------------------------------------------------------------------------------------------------------
  real(R4P), intent(in)         :: x(1:,1:,1:,1:) !< Data variable.
  character(len=:), allocatable :: code           !< Encoded base64 dataarray.
  integer(I1P),     allocatable :: xp(:)          !< Packed data.
  integer(I4P)                  :: nn             !< Number of elements.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  nn = size(x, dim=1)*size(x, dim=2)*size(x, dim=3)*size(x, dim=4)
  call pack_data(a1=[int(nn*BYR4P, I4P)], a2=reshape(x, [nn]), packed=xp)
  call b64_encode(n=xp, code=code)
  return
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction encode_base64_dataarray1_rank4_R4P

  module function encode_base64_dataarray1_rank4_I8P(x) result(code)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Encode (Base64) a dataarray with 1 components of rank 4 (I8P).
  !---------------------------------------------------------------------------------------------------------------------------------
  integer(I8P), intent(in)      :: x(1:,1:,1:,1:) !< Data variable.
  character(len=:), allocatable :: code           !< Encoded base64 dataarray.
  integer(I1P),     allocatable :: xp(:)          !< Packed data.
  integer(I4P)                  :: nn             !< Number of elements.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  nn = size(x, dim=1)*size(x, dim=2)*size(x, dim=3)*size(x, dim=4)
  call pack_data(a1=[int(nn*BYI8P, I4P)], a2=reshape(x, [nn]), packed=xp)
  call b64_encode(n=xp, code=code)
  return
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction encode_base64_dataarray1_rank4_I8P

  module function encode_base64_dataarray1_rank4_I4P(x) result(code)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Encode (Base64) a dataarray with 1 components of rank 4 (I4P).
  !---------------------------------------------------------------------------------------------------------------------------------
  integer(I4P), intent(in)      :: x(1:,1:,1:,1:) !< Data variable.
  character(len=:), allocatable :: code           !< Encoded base64 dataarray.
  integer(I1P),     allocatable :: xp(:)          !< Packed data.
  integer(I4P)                  :: nn             !< Number of elements.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  nn = size(x, dim=1)*size(x, dim=2)*size(x, dim=3)*size(x, dim=4)
  xp = transfer([int(nn*BYI4P, I4P), reshape(x, [nn])], xp)
  call b64_encode(n=xp, code=code)
  return
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction encode_base64_dataarray1_rank4_I4P

  module function encode_base64_dataarray1_rank4_I2P(x) result(code)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Encode (Base64) a dataarray with 1 components of rank 4 (I2P).
  !---------------------------------------------------------------------------------------------------------------------------------
  integer(I2P), intent(in)      :: x(1:,1:,1:,1:) !< Data variable.
  character(len=:), allocatable :: code           !< Encoded base64 dataarray.
  integer(I1P),     allocatable :: xp(:)          !< Packed data.
  integer(I4P)                  :: nn             !< Number of elements.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  nn = size(x, dim=1)*size(x, dim=2)*size(x, dim=3)*size(x, dim=4)
  call pack_data(a1=[int(nn*BYI2P, I4P)], a2=reshape(x, [nn]), packed=xp)
  call b64_encode(n=xp, code=code)
  return
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction encode_base64_dataarray1_rank4_I2P

  module function encode_base64_dataarray1_rank4_I1P(x) result(code)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Encode (Base64) a dataarray with 1 components of rank 4 (I1P).
  !---------------------------------------------------------------------------------------------------------------------------------
  integer(I1P), intent(in)      :: x(1:,1:,1:,1:) !< Data variable.
  character(len=:), allocatable :: code           !< Encoded base64 dataarray.
  integer(I1P),     allocatable :: xp(:)          !< Packed data.
  integer(I4P)                  :: nn             !< Number of elements.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  nn = size(x, dim=1)*size(x, dim=2)*size(x, dim=3)*size(x, dim=4)
  call pack_data(a1=[int(nn*BYI1P, I4P)], a2=reshape(x, [nn]), packed=xp)
  call b64_encode(n=xp, code=code)
  return
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction encode_base64_dataarray1_rank4_I1P

  module function encode_base64_dataarray3_rank1_R8P(x, y, z) result(code)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Encode (Base64) a dataarray with 3 components of rank 1 (R8P).
  !---------------------------------------------------------------------------------------------------------------------------------
  real(R8P),    intent(in)      :: x(1:)  !< X component.
  real(R8P),    intent(in)      :: y(1:)  !< Y component.
  real(R8P),    intent(in)      :: z(1:)  !< Z component.
  character(len=:), allocatable :: code   !< Encoded base64 dataarray.
  integer(I1P),     allocatable :: xyz(:) !< Packed data.
  integer(I4P)                  :: nn     !< Number of elements.
  integer(I4P)                  :: n      !< Counter.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  nn = size(x, dim=1)
  call pack_data(a1=[int(3*nn*BYR8P, I4P)], a2=[(x(n), y(n), z(n), n=1, nn)], packed=xyz)
  call b64_encode(n=xyz, code=code)
  return
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction encode_base64_dataarray3_rank1_R8P

  module function encode_base64_dataarray3_rank1_R4P(x, y, z) result(code)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Encode (Base64) a dataarray with 3 components of rank 1 (R4P).
  !---------------------------------------------------------------------------------------------------------------------------------
  real(R4P),    intent(in)      :: x(1:)  !< X component.
  real(R4P),    intent(in)      :: y(1:)  !< Y component.
  real(R4P),    intent(in)      :: z(1:)  !< Z component.
  character(len=:), allocatable :: code   !< Encoded base64 dataarray.
  integer(I1P),     allocatable :: xyz(:) !< Packed data.
  integer(I4P)                  :: nn     !< Number of elements.
  integer(I4P)                  :: n      !< Counter.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  nn = size(x, dim=1)
  call pack_data(a1=[int(3*nn*BYR4P, I4P)], a2=[(x(n), y(n), z(n), n=1, nn)], packed=xyz)
  call b64_encode(n=xyz, code=code)
  return
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction encode_base64_dataarray3_rank1_R4P

  module function encode_base64_dataarray3_rank1_I8P(x, y, z) result(code)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Encode (Base64) a dataarray with 3 components of rank 1 (I8P).
  !---------------------------------------------------------------------------------------------------------------------------------
  integer(I8P), intent(in)      :: x(1:)  !< X component.
  integer(I8P), intent(in)      :: y(1:)  !< Y component.
  integer(I8P), intent(in)      :: z(1:)  !< Z component.
  character(len=:), allocatable :: code   !< Encoded base64 dataarray.
  integer(I1P),     allocatable :: xyz(:) !< Packed data.
  integer(I4P)                  :: nn     !< Number of elements.
  integer(I4P)                  :: n      !< Counter.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  nn = size(x, dim=1)
  call pack_data(a1=[int(3*nn*BYI8P, I4P)], a2=[(x(n), y(n), z(n), n=1, nn)], packed=xyz)
  call b64_encode(n=xyz, code=code)
  return
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction encode_base64_dataarray3_rank1_I8P

  module function encode_base64_dataarray3_rank1_I4P(x, y, z) result(code)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Encode (Base64) a dataarray with 3 components of rank 1 (I4P).
  !---------------------------------------------------------------------------------------------------------------------------------
  integer(I4P), intent(in)      :: x(1:)  !< X component.
  integer(I4P), intent(in)      :: y(1:)  !< Y component.
  integer(I4P), intent(in)      :: z(1:)  !< Z component.
  character(len=:), allocatable :: code   !< Encoded base64 dataarray.
  integer(I1P),     allocatable :: xyz(:) !< Packed data.
  integer(I4P)                  :: nn     !< Number of elements.
  integer(I4P)                  :: n      !< Counter.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  nn = size(x, dim=1)
  xyz = transfer([int(3*nn*BYI4P, I4P), [(x(n), y(n), z(n), n=1, nn)]], xyz)
  call b64_encode(n=xyz, code=code)
  return
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction encode_base64_dataarray3_rank1_I4P

  module function encode_base64_dataarray3_rank1_I2P(x, y, z) result(code)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Encode (Base64) a dataarray with 3 components of rank 1 (I2P).
  !---------------------------------------------------------------------------------------------------------------------------------
  integer(I2P), intent(in)      :: x(1:)  !< X component.
  integer(I2P), intent(in)      :: y(1:)  !< Y component.
  integer(I2P), intent(in)      :: z(1:)  !< Z component.
  character(len=:), allocatable :: code   !< Encoded base64 dataarray.
  integer(I1P),     allocatable :: xyz(:) !< Packed data.
  integer(I4P)                  :: nn     !< Number of elements.
  integer(I4P)                  :: n      !< Counter.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  nn = size(x, dim=1)
  call pack_data(a1=[int(3*nn*BYI2P, I4P)], a2=[(x(n), y(n), z(n), n=1, nn)], packed=xyz)
  call b64_encode(n=xyz, code=code)
  return
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction encode_base64_dataarray3_rank1_I2P

  module function encode_base64_dataarray3_rank1_I1P(x, y, z) result(code)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Encode (Base64) a dataarray with 3 components of rank 1 (I1P).
  !---------------------------------------------------------------------------------------------------------------------------------
  integer(I1P), intent(in)      :: x(1:)  !< X component.
  integer(I1P), intent(in)      :: y(1:)  !< Y component.
  integer(I1P), intent(in)      :: z(1:)  !< Z component.
  character(len=:), allocatable :: code   !< Encoded base64 dataarray.
  integer(I1P),     allocatable :: xyz(:) !< Packed data.
  integer(I4P)                  :: nn     !< Number of elements.
  integer(I4P)                  :: n      !< Counter.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  nn = size(x, dim=1)
  call pack_data(a1=[int(3*nn*BYI1P, I4P)], a2=[(x(n), y(n), z(n), n=1, nn)], packed=xyz)
  call b64_encode(n=xyz, code=code)
  return
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction encode_base64_dataarray3_rank1_I1P

  module function encode_base64_dataarray3_rank3_R8P(x, y, z) result(code)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Encode (Base64) a dataarray with 3 components of rank 3 (R8P).
  !---------------------------------------------------------------------------------------------------------------------------------
  real(R8P),    intent(in)      :: x(1:,1:,1:) !< X component.
  real(R8P),    intent(in)      :: y(1:,1:,1:) !< Y component.
  real(R8P),    intent(in)      :: z(1:,1:,1:) !< Z component.
  character(len=:), allocatable :: code        !< Encoded base64 dataarray.
  integer(I1P),     allocatable :: xyz(:)      !< Packed data.
  integer(I4P)                  :: nn1         !< Number of elements along dim 1.
  integer(I4P)                  :: nn2         !< Number of elements along dim 2.
  integer(I4P)                  :: nn3         !< Number of elements along dim 3.
  integer(I4P)                  :: nn          !< Number of elements.
  integer(I4P)                  :: n1          !< Counter.
  integer(I4P)                  :: n2          !< Counter.
  integer(I4P)                  :: n3          !< Counter.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  nn1 = size(x, dim=1)
  nn2 = size(x, dim=2)
  nn3 = size(x, dim=3)
  nn = nn1*nn2*nn3
  call pack_data(a1=[int(3*nn*BYR8P, I4P)], a2=[(((x(n1, n2, n3), y(n1, n2, n3), z(n1, n2, n3), n1=1, nn1),  &
                                                                                                n2=1, nn2),  &
                                                                                                n3=1, nn3)], &
                 packed=xyz)
  call b64_encode(n=xyz,code=code)
  return
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction encode_base64_dataarray3_rank3_R8P

  module function encode_base64_dataarray3_rank3_R4P(x, y, z) result(code)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Encode (Base64) a dataarray with 3 components of rank 3 (R4P).
  !---------------------------------------------------------------------------------------------------------------------------------
  real(R4P),    intent(in)      :: x(1:,1:,1:) !< X component.
  real(R4P),    intent(in)      :: y(1:,1:,1:) !< Y component.
  real(R4P),    intent(in)      :: z(1:,1:,1:) !< Z component.
  character(len=:), allocatable :: code        !< Encoded base64 dataarray.
  integer(I1P),     allocatable :: xyz(:)      !< Packed data.
  integer(I4P)                  :: nn1         !< Number of elements along dim 1.
  integer(I4P)                  :: nn2         !< Number of elements along dim 2.
  integer(I4P)                  :: nn3         !< Number of elements along dim 3.
  integer(I4P)                  :: nn          !< Number of elements.
  integer(I4P)                  :: n1          !< Counter.
  integer(I4P)                  :: n2          !< Counter.
  integer(I4P)                  :: n3          !< Counter.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  nn1 = size(x, dim=1)
  nn2 = size(x, dim=2)
  nn3 = size(x, dim=3)
  nn = nn1*nn2*nn3
  call pack_data(a1=[int(3*nn*BYR4P, I4P)], a2=[(((x(n1, n2, n3), y(n1, n2, n3), z(n1, n2, n3), n1=1, nn1),  &
                                                                                                n2=1, nn2),  &
                                                                                                n3=1, nn3)], &
                 packed=xyz)
  call b64_encode(n=xyz,code=code)
  return
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction encode_base64_dataarray3_rank3_R4P

  module function encode_base64_dataarray3_rank3_I8P(x, y, z) result(code)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Encode (Base64) a dataarray with 3 components of rank 3 (I8P).
  !---------------------------------------------------------------------------------------------------------------------------------
  integer(I8P), intent(in)      :: x(1:,1:,1:) !< X component.
  integer(I8P), intent(in)      :: y(1:,1:,1:) !< Y component.
  integer(I8P), intent(in)      :: z(1:,1:,1:) !< Z component.
  character(len=:), allocatable :: code        !< Encoded base64 dataarray.
  integer(I1P),     allocatable :: xyz(:)      !< Packed data.
  integer(I4P)                  :: nn1         !< Number of elements along dim 1.
  integer(I4P)                  :: nn2         !< Number of elements along dim 2.
  integer(I4P)                  :: nn3         !< Number of elements along dim 3.
  integer(I4P)                  :: nn          !< Number of elements.
  integer(I4P)                  :: n1          !< Counter.
  integer(I4P)                  :: n2          !< Counter.
  integer(I4P)                  :: n3          !< Counter.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  nn1 = size(x, dim=1)
  nn2 = size(x, dim=2)
  nn3 = size(x, dim=3)
  nn = nn1*nn2*nn3
  call pack_data(a1=[int(3*nn*BYI8P, I4P)], a2=[(((x(n1, n2, n3), y(n1, n2, n3), z(n1, n2, n3), n1=1, nn1),  &
                                                                                                n2=1, nn2),  &
                                                                                                n3=1, nn3)], &
                 packed=xyz)
  call b64_encode(n=xyz,code=code)
  return
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction encode_base64_dataarray3_rank3_I8P

  module function encode_base64_dataarray3_rank3_I4P(x, y, z) result(code)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Encode (Base64) a dataarray with 3 components of rank 3 (I4P).
  !---------------------------------------------------------------------------------------------------------------------------------
  integer(I4P), intent(in)      :: x(1:,1:,1:) !< X component.
  integer(I4P), intent(in)      :: y(1:,1:,1:) !< Y component.
  integer(I4P), intent(in)      :: z(1:,1:,1:) !< Z component.
  character(len=:), allocatable :: code        !< Encoded base64 dataarray.
  integer(I1P),     allocatable :: xyz(:)      !< Packed data.
  integer(I4P)                  :: nn1         !< Number of elements along dim 1.
  integer(I4P)                  :: nn2         !< Number of elements along dim 2.
  integer(I4P)                  :: nn3         !< Number of elements along dim 3.
  integer(I4P)                  :: nn          !< Number of elements.
  integer(I4P)                  :: n1          !< Counter.
  integer(I4P)                  :: n2          !< Counter.
  integer(I4P)                  :: n3          !< Counter.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  nn1 = size(x, dim=1)
  nn2 = size(x, dim=2)
  nn3 = size(x, dim=3)
  nn = nn1*nn2*nn3
  xyz = transfer([int(3*nn*BYI4P, I4P), [(((x(n1, n2, n3), y(n1, n2, n3), z(n1, n2, n3), n1=1, nn1),  &
                                                                                         n2=1, nn2),  &
                                                                                         n3=1, nn3)]], xyz)
  call b64_encode(n=xyz,code=code)
  return
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction encode_base64_dataarray3_rank3_I4P

  module function encode_base64_dataarray3_rank3_I2P(x, y, z) result(code)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Encode (Base64) a dataarray with 3 components of rank 3 (I2P).
  !---------------------------------------------------------------------------------------------------------------------------------
  integer(I2P), intent(in)      :: x(1:,1:,1:) !< X component.
  integer(I2P), intent(in)      :: y(1:,1:,1:) !< Y component.
  integer(I2P), intent(in)      :: z(1:,1:,1:) !< Z component.
  character(len=:), allocatable :: code        !< Encoded base64 dataarray.
  integer(I1P),     allocatable :: xyz(:)      !< Packed data.
  integer(I4P)                  :: nn1         !< Number of elements along dim 1.
  integer(I4P)                  :: nn2         !< Number of elements along dim 2.
  integer(I4P)                  :: nn3         !< Number of elements along dim 3.
  integer(I4P)                  :: nn          !< Number of elements.
  integer(I4P)                  :: n1          !< Counter.
  integer(I4P)                  :: n2          !< Counter.
  integer(I4P)                  :: n3          !< Counter.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  nn1 = size(x, dim=1)
  nn2 = size(x, dim=2)
  nn3 = size(x, dim=3)
  nn = nn1*nn2*nn3
  call pack_data(a1=[int(3*nn*BYI2P, I4P)], a2=[(((x(n1, n2, n3), y(n1, n2, n3), z(n1, n2, n3), n1=1, nn1),  &
                                                                                                n2=1, nn2),  &
                                                                                                n3=1, nn3)], &
                 packed=xyz)
  call b64_encode(n=xyz,code=code)
  return
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction encode_base64_dataarray3_rank3_I2P

  module function encode_base64_dataarray3_rank3_I1P(x, y, z) result(code)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Encode (Base64) a dataarray with 3 components of rank 3 (I1P).
  !---------------------------------------------------------------------------------------------------------------------------------
  integer(I1P), intent(in)      :: x(1:,1:,1:) !< X component.
  integer(I1P), intent(in)      :: y(1:,1:,1:) !< Y component.
  integer(I1P), intent(in)      :: z(1:,1:,1:) !< Z component.
  character(len=:), allocatable :: code        !< Encoded base64 dataarray.
  integer(I1P),     allocatable :: xyz(:)      !< Packed data.
  integer(I4P)                  :: nn1         !< Number of elements along dim 1.
  integer(I4P)                  :: nn2         !< Number of elements along dim 2.
  integer(I4P)                  :: nn3         !< Number of elements along dim 3.
  integer(I4P)                  :: nn          !< Number of elements.
  integer(I4P)                  :: n1          !< Counter.
  integer(I4P)                  :: n2          !< Counter.
  integer(I4P)                  :: n3          !< Counter.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  nn1 = size(x, dim=1)
  nn2 = size(x, dim=2)
  nn3 = size(x, dim=3)
  nn = nn1*nn2*nn3
  call pack_data(a1=[int(3*nn*BYI1P, I4P)], a2=[(((x(n1, n2, n3), y(n1, n2, n3), z(n1, n2, n3), n1=1, nn1),  &
                                                                                                n2=1, nn2),  &
                                                                                                n3=1, nn3)], &
                 packed=xyz)
  call b64_encode(n=xyz,code=code)
  return
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction encode_base64_dataarray3_rank3_I1P
endsubmodule encode_base64_dataarray
