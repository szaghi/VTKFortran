!< BeFoR64, Base64 encoding/decoding library for FoRtran poor men
module Lib_Base64
!-----------------------------------------------------------------------------------------------------------------------------------
!< BeFoR64, Base64 encoding/decoding library for FoRtran poor men
!<{!README-BeFoR64.md!}
!<
!<### ChangeLog
!<
!<{!ChangeLog-BeFoR64.md!}
!-----------------------------------------------------------------------------------------------------------------------------------
USE IR_Precision  ! Integers and reals precision definition.
USE Lib_Pack_Data ! Library for packing heterogeneous data into single (homogeneous) packed one.
!-----------------------------------------------------------------------------------------------------------------------------------

!-----------------------------------------------------------------------------------------------------------------------------------
implicit none
private
public:: b64_encode
public:: b64_decode
public:: pack_data
public:: b64_initialized,b64_init
public:: autotest
!-----------------------------------------------------------------------------------------------------------------------------------

!-----------------------------------------------------------------------------------------------------------------------------------
logical::       b64_initialized = .false. !< Flag for chcecking the initialization of the library.
character(64):: base64="ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/" !< Base64 alphabet.
!-----------------------------------------------------------------------------------------------------------------------------------

!-----------------------------------------------------------------------------------------------------------------------------------
interface b64_encode
  !< Procedure for encoding numbers (integer and real) to base64.
  !<
  !< This is an interface for encoding integer and real numbers of any kinds into a base64 string. This interface can encode both
  !< scalar and array.
  !<
  !< @warning The encoded string is returned as varying length character string, `character(len=:), allocatable:: string`, thus the
  !< compiler must support such a Fortran (2003) feature.
  !<
  !< @note Before start to encode anything the library must be initialized. The procedure `b64_init` must be called at first. The
  !< global variable `b64_initialized` can be used to check the status of the initialization.
  !<
  !<### Usage
  !< For a practical example see the `autotest` procedure.
  !<
  !<#### Scalar encoding
  !<```fortran
  !<character(len=:), allocatable:: code64 ! base64 encoded string
  !<...
  !<call b64_encode(n=12._R8P,code=code64)
  !<```
  !<
  !<#### Array encoding
  !<```fortran
  !<character(len=:), allocatable:: code64 ! base64 encoded string
  !<...
  !<call b64_encode(n=[12_I4P,1_I4P],code=code64)
  !<```
  !<
  !< @note If you want to encode heterogenous data (e.g. integer and real numbers), you must use the auxiliary `pack_data`
  !< procedure.
  module procedure &
#ifdef r16p
                   b64_encode_R16,b64_encode_R16_a, &
#endif
                   b64_encode_R8, b64_encode_R8_a,  &
                   b64_encode_R4, b64_encode_R4_a,  &
                   b64_encode_I8, b64_encode_I8_a,  &
                   b64_encode_I4, b64_encode_I4_a,  &
                   b64_encode_I2, b64_encode_I2_a,  &
                   b64_encode_I1, b64_encode_I1_a
endinterface
interface b64_decode
  !< Procedure for decoding numbers (integer and real) from base64.
  !<
  !< This is an interface for decoding integer and real numbers of any kinds from a base64 string. This interface can decode both
  !< scalar and array.
  !<
  !< @note Before start to decode anything the library must be initialized. The procedure `b64_init` must be called at first. The
  !< global variable `b64_initialized` can be used to check the status of the initialization.
  !<
  !<### Usage
  !< For a practical example see the `autotest` procedure.
  !<
  !<#### Scalar decoding
  !<```fortran
  !<real(R8P):: decoded ! scalar to be decoded
  !<...
  !<call b64_decode(code='AAAAAAAA8D8=',n=decoded)
  !<```
  !<
  !<#### Array decoding
  !<```fortran
  !<integer(I8P):: decoded(1:4) ! array to be decoded
  !<...
  !<call b64_decode(code='FwAAAAAAAABEAQAAAAAAABBwhAEAAAAAAgAAAAAAAAA=',n=decoded)
  !<```
  !<
  !< @note If you want to decode heterogenous data (e.g. integer and real numbers), you must use the auxiliary `pack_data`
  !< procedure.
  module procedure &
#ifdef r16p
                   b64_decode_R16,b64_decode_R16_a, &
#endif
                   b64_decode_R8, b64_decode_R8_a,  &
                   b64_decode_R4, b64_decode_R4_a,  &
                   b64_decode_I8, b64_decode_I8_a,  &
                   b64_decode_I4, b64_decode_I4_a,  &
                   b64_decode_I2, b64_decode_I2_a,  &
                   b64_decode_I1, b64_decode_I1_a
endinterface
!-----------------------------------------------------------------------------------------------------------------------------------
contains
  subroutine b64_init()
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Procedure for initializing the BeFoR64 library.
  !<
  !< @note This procedure **must** be called before encoding/decoding anything!
  !---------------------------------------------------------------------------------------------------------------------------------
  implicit none
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  if (.not.ir_initialized) call IR_Init
  b64_initialized = .true.
  return
  !---------------------------------------------------------------------------------------------------------------------------------
  endsubroutine b64_init

  pure subroutine encode_bits(bits,padd,code)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Procedure for encoding a bits stream (must be multiple of 24 bits) into base64 charcaters code (of length multiple of 4).
  !<
  !< The bits stream are encoded in chunks of 24 bits as the following example (in little endian order)
  !<```
  !< +--first octet--+-second octet--+--third octet--+
  !< |7 6 5 4 3 2 1 0|7 6 5 4 3 2 1 0|7 6 5 4 3 2 1 0|
  !< +-----------+---+-------+-------+---+-----------+
  !< |5 4 3 2 1 0|5 4 3 2 1 0|5 4 3 2 1 0|5 4 3 2 1 0|
  !< +--1.index--+--2.index--+--3.index--+--4.index--+
  !<```
  !< @note The 4 indexes are stored into 4 elements 8 bits array, thus 2 bits of each array element are not used.
  !<
  !< @note The number of paddings must be computed outside this procedure, into the calling scope.
  !<
  !< @warning This procedure is the backend of encoding, thus it must be never called outside the module.
  !---------------------------------------------------------------------------------------------------------------------------------
  implicit none
  integer(I1P), intent(IN)::  bits(1:)  !< Bits to be encoded.
  integer(I4P), intent(IN)::  padd      !< Number of padding characters ('=').
  character(*), intent(OUT):: code      !< Characters code.
  integer(I1P)::              sixb(1:4) !< 6 bits slices (stored into 8 bits integer) of 24 bits input.
  integer(I8P)::              c         !< Counter.
  integer(I8P)::              e         !< Counter.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  c = 1_I8P
  do e=1_I8P,size(bits,dim=1,kind=I8P),3_I8P ! loop over array elements: 3 bytes (24 bits) scanning
    sixb = 0_I1P
    call mvbits(bits(e  ),2,6,sixb(1),0)
    call mvbits(bits(e  ),0,2,sixb(2),4) ; call mvbits(bits(e+1),4,4,sixb(2),0)
    call mvbits(bits(e+1),0,4,sixb(3),2) ; call mvbits(bits(e+2),6,2,sixb(3),0)
    call mvbits(bits(e+2),0,6,sixb(4),0)
    sixb = sixb + 1_I1P
    code(c  :c  ) = base64(sixb(1):sixb(1))
    code(c+1:c+1) = base64(sixb(2):sixb(2))
    code(c+2:c+2) = base64(sixb(3):sixb(3))
    code(c+3:c+3) = base64(sixb(4):sixb(4))
    c = c + 4_I8P
  enddo
  if (padd>0) code(len(code)-padd+1:)=repeat('=',padd)
  return
  !---------------------------------------------------------------------------------------------------------------------------------
  endsubroutine encode_bits

  pure subroutine decode_bits(code,bits)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Procedure for decoding a base64 string into a sequence of bits stream.
  !<
  !< The base64 string must be parsed with a strike of 4 characters and converted into a 3 bytes stream. Considering the base64 code
  !< `QUJD` the decoding process must do
  !<```
  !< +-b64 char--+-b64 char--+-b64 char--+-b64 char--+
  !< |      Q    |      U    |      J    |      D    |
  !< +-b64 index-+-b64 index-+-b64 index-+-b64 index-+
  !< !      16   |      20   |      9    |      3    |
  !< +-6 bits----+-6 bits----+-6 bits----+-6 bits----+
  !< |0 1 0 0 0 0|0 1 0 1 0 0|0 0 1 0 0 1|0 0 0 0 1 1|
  !< +-----------+---+-------+-------+---+-----------+
  !< |0 1 0 0 0 0 0 1|0 1 0 0 0 0 1 0|0 1 0 0 0 0 1 1|
  !< +-----8 bits----+-----8 bits----+-----8 bits----+
  !<```
  !< @note The bits pattern is returned as a 1-byte element array, the dimension of witch must be computed outside this procedure.
  !<
  !< @warning This procedure is the backend of decoding, thus it must be never called outside the module.
  !---------------------------------------------------------------------------------------------------------------------------------
  implicit none
  character(*), intent(IN)::  code      !< Characters code.
  integer(I1P), intent(OUT):: bits(1:)  !< Bits decoded.
  integer(I1P)::              sixb(1:4) !< 6 bits slices (stored into 8 bits integer) of 24 bits input.
  integer(I8P)::              c         !< Counter.
  integer(I8P)::              e         !< Counter.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  e = 1_I8P
  do c=1_I8P,len(code),4_I8P ! loop over code characters: 3 bytes (24 bits) scanning
    sixb = 0_I1P
    sixb(1) = index(base64,code(c  :c  )) - 1
    sixb(2) = index(base64,code(c+1:c+1)) - 1
    sixb(3) = index(base64,code(c+2:c+2)) - 1
    sixb(4) = index(base64,code(c+3:c+3)) - 1
      call mvbits(sixb(1),0,6,bits(e  ),2) ; call mvbits(sixb(2),4,2,bits(e  ),0)
    if (size(bits,dim=1)>=2) then
      call mvbits(sixb(2),0,4,bits(e+1),4) ; call mvbits(sixb(3),2,4,bits(e+1),0)
    endif
    if (size(bits,dim=1)>=3) then
      call mvbits(sixb(3),0,2,bits(e+2),6) ; call mvbits(sixb(4),0,6,bits(e+2),0)
    endif
    e = e + 3_I8P
  enddo
  ! padding handling
  ! if (code(len(code)-1:len(code))=='==') then
  !   bits(size(bits)-1:) = 0_I1P
  ! elseif (code(len(code):len(code))=='=') then
  !   bits(size(bits)) = 0_I1P
  ! endif
  return
  !---------------------------------------------------------------------------------------------------------------------------------
  endsubroutine decode_bits

  elemental subroutine b64_encode_R16(n,code)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Procedure for encoding scalar number to base64 (R16P).
  !---------------------------------------------------------------------------------------------------------------------------------
  implicit none
  real(R16P),                    intent(IN)::  n       !< Number to be encoded.
  character(len=:), allocatable, intent(OUT):: code    !< Encoded scalar.
  integer(I1P),     allocatable::              nI1P(:) !< One byte integer array containing n.
  integer(I4P)::                               padd    !< Number of padding characters ('=').
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  allocate(nI1P(1:((BYR16P+2)/3)*3)) ; nI1P = 0_I1P
  code = repeat(' ',((BYR16P+2)/3)*4)
  nI1P = transfer(n,nI1P)
  padd = mod((BYR16P),3_I1P) ; if (padd>0_I4P) padd = 3_I4P - padd
  call encode_bits(bits=nI1P,padd=padd,code=code)
  return
  !---------------------------------------------------------------------------------------------------------------------------------
  endsubroutine b64_encode_R16

  elemental subroutine b64_encode_R8(n,code)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Procedure for encoding scalar number to base64 (R8P).
  !---------------------------------------------------------------------------------------------------------------------------------
  implicit none
  real(R8P),                     intent(IN)::  n       !< Number to be encoded.
  character(len=:), allocatable, intent(OUT):: code    !< Encoded scalar.
  integer(I1P),     allocatable::              nI1P(:) !< One byte integer array containing n.
  integer(I4P)::                               padd    !< Number of padding characters ('=').
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  allocate(nI1P(1:((BYR8P+2)/3)*3)) ; nI1P = 0_I1P
  code = repeat(' ',((BYR8P+2)/3)*4)
  nI1P = transfer(n,nI1P)
  padd = mod((BYR8P),3_I1P) ; if (padd>0_I4P) padd = 3_I4P - padd
  call encode_bits(bits=nI1P,padd=padd,code=code)
  return
  !---------------------------------------------------------------------------------------------------------------------------------
  endsubroutine b64_encode_R8

  elemental subroutine b64_encode_R4(n,code)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Procedure for encoding scalar number to base64 (R4P).
  !---------------------------------------------------------------------------------------------------------------------------------
  implicit none
  real(R4P),                     intent(IN)::  n       !< Number to be encoded.
  character(len=:), allocatable, intent(OUT):: code    !< Encoded scalar.
  integer(I1P),     allocatable::              nI1P(:) !< One byte integer array containing n.
  integer(I4P)::                               padd    !< Number of padding characters ('=').
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  allocate(nI1P(1:((BYR4P+2)/3)*3)) ; nI1P = 0_I1P
  code = repeat(' ',((BYR4P+2)/3)*4)
  nI1P = transfer(n,nI1P)
  padd = mod((BYR4P),3_I1P) ; if (padd>0_I4P) padd = 3_I4P - padd
  call encode_bits(bits=nI1P,padd=padd,code=code)
  return
  !---------------------------------------------------------------------------------------------------------------------------------
  endsubroutine b64_encode_R4

  elemental subroutine b64_encode_I8(n,code)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Procedure for encoding scalar number to base64 (I8P).
  !---------------------------------------------------------------------------------------------------------------------------------
  implicit none
  integer(I8P),                  intent(IN)::  n       !< Number to be encoded.
  character(len=:), allocatable, intent(OUT):: code    !< Encoded scalar.
  integer(I1P),     allocatable::              nI1P(:) !< One byte integer array containing n.
  integer(I4P)::                               padd    !< Number of padding characters ('=').
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  allocate(nI1P(1:((BYI8P+2)/3)*3)) ; nI1P = 0_I1P
  code = repeat(' ',((BYI8P+2)/3)*4)
  nI1P = transfer(n,nI1P)
  padd = mod((BYI8P),3_I8P) ; if (padd>0_I4P) padd = 3_I4P - padd
  call encode_bits(bits=nI1P,padd=padd,code=code)
  return
  !---------------------------------------------------------------------------------------------------------------------------------
  endsubroutine b64_encode_I8

  elemental subroutine b64_encode_I4(n,code)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Procedure for encoding scalar number to base64 (I4P).
  !---------------------------------------------------------------------------------------------------------------------------------
  implicit none
  integer(I4P),                  intent(IN)::  n       !< Number to be encoded.
  character(len=:), allocatable, intent(OUT):: code    !< Encoded scalar.
  integer(I1P),     allocatable::              nI1P(:) !< One byte integer array containing n.
  integer(I4P)::                               padd    !< Number of padding characters ('=').
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  allocate(nI1P(1:((BYI4P+2)/3)*3)) ; nI1P = 0_I1P
  code = repeat(' ',((BYI4P+2)/3)*4)
  nI1P = transfer(n,nI1P)
  padd = mod((BYI4P),3_I4P) ; if (padd>0_I4P) padd = 3_I4P - padd
  call encode_bits(bits=nI1P,padd=padd,code=code)
  return
  !---------------------------------------------------------------------------------------------------------------------------------
  endsubroutine b64_encode_I4

  elemental subroutine b64_encode_I2(n,code)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Procedure for encoding scalar number to base64 (I2P).
  !---------------------------------------------------------------------------------------------------------------------------------
  implicit none
  integer(I2P),                  intent(IN)::  n       !< Number to be encoded.
  character(len=:), allocatable, intent(OUT):: code    !< Encoded scalar.
  integer(I1P),     allocatable::              nI1P(:) !< One byte integer array containing n.
  integer(I4P)::                               padd    !< Number of padding characters ('=').
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  allocate(nI1P(1:((BYI2P+2)/3)*3)) ; nI1P = 0_I1P
  code = repeat(' ',((BYI2P+2)/3)*4)
  nI1P = transfer(n,nI1P)
  padd = mod((BYI2P),3_I2P) ; if (padd>0_I4P) padd = 3_I4P - padd
  call encode_bits(bits=nI1P,padd=padd,code=code)
  return
  !---------------------------------------------------------------------------------------------------------------------------------
  endsubroutine b64_encode_I2

  elemental subroutine b64_encode_I1(n,code)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Procedure for encoding scalar number to base64 (I1P).
  !---------------------------------------------------------------------------------------------------------------------------------
  implicit none
  integer(I1P),                  intent(IN)::  n       !< Number to be encoded.
  character(len=:), allocatable, intent(OUT):: code    !< Encoded scalar.
  integer(I1P),     allocatable::              nI1P(:) !< One byte integer array containing n.
  integer(I4P)::                               padd    !< Number of padding characters ('=').
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  allocate(nI1P(1:((BYI1P+2)/3)*3)) ; nI1P = 0_I1P
  code = repeat(' ',((BYI1P+2)/3)*4)
  nI1P = transfer(n,nI1P)
  padd = mod((BYI1P),3_I1P) ; if (padd>0_I4P) padd = 3_I4P - padd
  call encode_bits(bits=nI1P,padd=padd,code=code)
  return
  !---------------------------------------------------------------------------------------------------------------------------------
  endsubroutine b64_encode_I1

  pure subroutine b64_encode_R16_a(n,code)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Procedure for encoding array numbers to base64 (R16P).
  !---------------------------------------------------------------------------------------------------------------------------------
  implicit none
  real(R16P),                    intent(IN)::  n(1:)   !< Array of numbers to be encoded.
  character(len=:), allocatable, intent(OUT):: code    !< Encoded array.
  integer(I1P),     allocatable::              nI1P(:) !< One byte integer array containing n.
  integer(I4P)::                               padd    !< Number of padding characters ('=').
  integer(I8P)::                               ns      !< Size of n.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  ns = size(n,dim=1)
  allocate(nI1P(1:((ns*BYR16P+2)/3)*3)) ; nI1P = 0_I1P
  code = repeat(' ',((ns*BYR16P+2)/3)*4)
  nI1P = transfer(n,nI1P)
  padd = mod((ns*BYR16P),3_I8P) ; if (padd>0_I4P) padd = 3_I4P - padd
  call encode_bits(bits=nI1P,padd=padd,code=code)
  return
  !---------------------------------------------------------------------------------------------------------------------------------
  endsubroutine b64_encode_R16_a

  pure subroutine b64_encode_R8_a(n,code)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Procedure for encoding array numbers to base64 (R8P).
  !---------------------------------------------------------------------------------------------------------------------------------
  implicit none
  real(R8P),                     intent(IN)::  n(1:)   !< Array of numbers to be encoded.
  character(len=:), allocatable, intent(OUT):: code    !< Encoded array.
  integer(I1P),     allocatable::              nI1P(:) !< One byte integer array containing n.
  integer(I4P)::                               padd    !< Number of padding characters ('=').
  integer(I8P)::                               ns      !< Size of n.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  ns = size(n,dim=1)
  allocate(nI1P(1:((ns*BYR8P+2)/3)*3)) ; nI1P = 0_I1P
  code = repeat(' ',((ns*BYR8P+2)/3)*4)
  nI1P = transfer(n,nI1P)
  padd = mod((ns*BYR8P),3_I8P) ; if (padd>0_I4P) padd = 3_I4P - padd
  call encode_bits(bits=nI1P,padd=padd,code=code)
  return
  !---------------------------------------------------------------------------------------------------------------------------------
  endsubroutine b64_encode_R8_a

  pure subroutine b64_encode_R4_a(n,code)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Procedure for encoding array numbers to base64 (R4P).
  !---------------------------------------------------------------------------------------------------------------------------------
  implicit none
  real(R4P),                     intent(IN)::  n(1:)   !< Array of numbers to be encoded.
  character(len=:), allocatable, intent(OUT):: code    !< Encoded array.
  integer(I1P),     allocatable::              nI1P(:) !< One byte integer array containing n.
  integer(I4P)::                               padd    !< Number of padding characters ('=').
  integer(I8P)::                               ns      !< Size of n.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  ns = size(n,dim=1)
  allocate(nI1P(1:((ns*BYR4P+2)/3)*3)) ; nI1P = 0_I1P
  code = repeat(' ',((ns*BYR4P+2)/3)*4)
  nI1P = transfer(n,nI1P)
  padd = mod((ns*BYR4P),3_I8P) ; if (padd>0_I4P) padd = 3_I4P - padd
  call encode_bits(bits=nI1P,padd=padd,code=code)
  return
  !---------------------------------------------------------------------------------------------------------------------------------
  endsubroutine b64_encode_R4_a

  pure subroutine b64_encode_I8_a(n,code)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Procedure for encoding array numbers to base64 (I8P).
  !---------------------------------------------------------------------------------------------------------------------------------
  implicit none
  integer(I8P),                  intent(IN)::  n(1:)   !< Array of numbers to be encoded.
  character(len=:), allocatable, intent(OUT):: code    !< Encoded array.
  integer(I1P),     allocatable::              nI1P(:) !< One byte integer array containing n.
  integer(I4P)::                               padd    !< Number of padding characters ('=').
  integer(I8P)::                               ns      !< Size of n.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  ns = size(n,dim=1)
  allocate(nI1P(1:((ns*BYI8P+2)/3)*3)) ; nI1P = 0_I1P
  code = repeat(' ',((ns*BYI8P+2)/3)*4)
  nI1P = transfer(n,nI1P)
  padd = mod((ns*BYI8P),3_I8P) ; if (padd>0_I4P) padd = 3_I4P - padd
  call encode_bits(bits=nI1P,padd=padd,code=code)
  return
  !---------------------------------------------------------------------------------------------------------------------------------
  endsubroutine b64_encode_I8_a

  pure subroutine b64_encode_I4_a(n,code)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Procedure for encoding array numbers to base64 (I4P).
  !---------------------------------------------------------------------------------------------------------------------------------
  implicit none
  integer(I4P),                  intent(IN)::  n(1:)   !< Array of numbers to be encoded.
  character(len=:), allocatable, intent(OUT):: code    !< Encoded array.
  integer(I1P),     allocatable::              nI1P(:) !< One byte integer array containing n.
  integer(I4P)::                               padd    !< Number of padding characters ('=').
  integer(I8P)::                               ns      !< Size of n.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  ns = size(n,dim=1)
  allocate(nI1P(1:((ns*BYI4P+2)/3)*3)) ; nI1P = 0_I1P
  code = repeat(' ',((ns*BYI4P+2)/3)*4)
  nI1P = transfer(n,nI1P)
  padd = mod((ns*BYI4P),3_I8P) ; if (padd>0_I4P) padd = 3_I4P - padd
  call encode_bits(bits=nI1P,padd=padd,code=code)
  return
  !---------------------------------------------------------------------------------------------------------------------------------
  endsubroutine b64_encode_I4_a

  pure subroutine b64_encode_I2_a(n,code)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Procedure for encoding array numbers to base64 (I2P).
  !---------------------------------------------------------------------------------------------------------------------------------
  implicit none
  integer(I2P),                  intent(IN)::  n(1:)   !< Array of numbers to be encoded.
  character(len=:), allocatable, intent(OUT):: code    !< Encoded array.
  integer(I1P),     allocatable::              nI1P(:) !< One byte integer array containing n.
  integer(I4P)::                               padd    !< Number of padding characters ('=').
  integer(I8P)::                               ns      !< Size of n.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  ns = size(n,dim=1)
  allocate(nI1P(1:((ns*BYI2P+2)/3)*3)) ; nI1P = 0_I1P
  code = repeat(' ',((ns*BYI2P+2)/3)*4)
  nI1P = transfer(n,nI1P)
  padd = mod((ns*BYI2P),3_I8P) ; if (padd>0_I4P) padd = 3_I4P - padd
  call encode_bits(bits=nI1P,padd=padd,code=code)
  return
  !---------------------------------------------------------------------------------------------------------------------------------
  endsubroutine b64_encode_I2_a

  pure subroutine b64_encode_I1_a(n,code)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Procedure for encoding array numbers to base64 (I1P).
  !---------------------------------------------------------------------------------------------------------------------------------
  implicit none
  integer(I1P),                  intent(IN)::  n(1:)   !< Array of numbers to be encoded.
  character(len=:), allocatable, intent(OUT):: code    !< Encoded array.
  integer(I1P),     allocatable::              nI1P(:) !< One byte integer array containing n.
  integer(I4P)::                               padd    !< Number of padding characters ('=').
  integer(I8P)::                               ns      !< Size of n.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  ns = size(n,dim=1)
  allocate(nI1P(1:((ns*BYI1P+2)/3)*3)) ; nI1P = 0_I1P
  code = repeat(' ',((ns*BYI1P+2)/3)*4)
  nI1P = transfer(n,nI1P)
  padd = mod((ns*BYI1P),3_I8P) ; if (padd>0_I4P) padd = 3_I4P - padd
  call encode_bits(bits=nI1P,padd=padd,code=code)
  return
  !---------------------------------------------------------------------------------------------------------------------------------
  endsubroutine b64_encode_I1_a

  elemental subroutine b64_decode_R16(n,code)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Procedure for decoding a base64 code into a scalar number (R16P).
  !---------------------------------------------------------------------------------------------------------------------------------
  implicit none
  character(*), intent(IN)::  code    !< Encoded scalar.
  real(R16P),   intent(OUT):: n       !< Number to be decoded.
  integer(I1P), allocatable:: nI1P(:) !< One byte integer array containing n.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  allocate(nI1P(1:BYR16P)) ; nI1P = 0_I1P
  call decode_bits(code=code,bits=nI1P)
  n = transfer(nI1P,n)
  return
  !---------------------------------------------------------------------------------------------------------------------------------
  endsubroutine b64_decode_R16

  elemental subroutine b64_decode_R8(n,code)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Procedure for decoding a base64 code into a scalar number (R8P).
  !---------------------------------------------------------------------------------------------------------------------------------
  implicit none
  character(*), intent(IN)::  code    !< Encoded scalar.
  real(R8P),    intent(OUT):: n       !< Number to be decoded.
  integer(I1P), allocatable:: nI1P(:) !< One byte integer array containing n.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  allocate(nI1P(1:BYR8P)) ; nI1P = 0_I1P
  call decode_bits(code=code,bits=nI1P)
  n = transfer(nI1P,n)
  return
  !---------------------------------------------------------------------------------------------------------------------------------
  endsubroutine b64_decode_R8

  elemental subroutine b64_decode_R4(n,code)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Procedure for decoding a base64 code into a scalar number (R4P).
  !---------------------------------------------------------------------------------------------------------------------------------
  implicit none
  character(*), intent(IN)::  code    !< Encoded scalar.
  real(R4P),    intent(OUT):: n       !< Number to be decoded.
  integer(I1P), allocatable:: nI1P(:) !< One byte integer array containing n.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  allocate(nI1P(1:BYR4P)) ; nI1P = 0_I1P
  call decode_bits(code=code,bits=nI1P)
  n = transfer(nI1P,n)
  return
  !---------------------------------------------------------------------------------------------------------------------------------
  endsubroutine b64_decode_R4

  elemental subroutine b64_decode_I8(n,code)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Procedure for decoding a base64 code into a scalar number (I8P).
  !---------------------------------------------------------------------------------------------------------------------------------
  implicit none
  character(*), intent(IN)::  code    !< Encoded scalar.
  integer(I8P), intent(OUT):: n       !< Number to be decoded.
  integer(I1P), allocatable:: nI1P(:) !< One byte integer array containing n.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  allocate(nI1P(1:BYI8P)) ; nI1P = 0_I1P
  call decode_bits(code=code,bits=nI1P)
  n = transfer(nI1P,n)
  return
  !---------------------------------------------------------------------------------------------------------------------------------
  endsubroutine b64_decode_I8

  elemental subroutine b64_decode_I4(n,code)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Procedure for decoding a base64 code into a scalar number (I4P).
  !---------------------------------------------------------------------------------------------------------------------------------
  implicit none
  character(*), intent(IN)::  code    !< Encoded scalar.
  integer(I4P), intent(OUT):: n       !< Number to be decoded.
  integer(I1P), allocatable:: nI1P(:) !< One byte integer array containing n.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  allocate(nI1P(1:BYI4P)) ; nI1P = 0_I1P
  call decode_bits(code=code,bits=nI1P)
  n = transfer(nI1P,n)
  return
  !---------------------------------------------------------------------------------------------------------------------------------
  endsubroutine b64_decode_I4

  elemental subroutine b64_decode_I2(n,code)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Procedure for decoding a base64 code into a scalar number (I2P).
  !---------------------------------------------------------------------------------------------------------------------------------
  implicit none
  character(*), intent(IN)::  code    !< Encoded scalar.
  integer(I2P), intent(OUT):: n       !< Number to be decoded.
  integer(I1P), allocatable:: nI1P(:) !< One byte integer array containing n.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  allocate(nI1P(1:BYI2P)) ; nI1P = 0_I1P
  call decode_bits(code=code,bits=nI1P)
  n = transfer(nI1P,n)
  return
  !---------------------------------------------------------------------------------------------------------------------------------
  endsubroutine b64_decode_I2

  elemental subroutine b64_decode_I1(n,code)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Procedure for decoding a base64 code into a scalar number (I1P).
  !---------------------------------------------------------------------------------------------------------------------------------
  implicit none
  character(*), intent(IN)::  code    !< Encoded scalar.
  integer(I1P), intent(OUT):: n       !< Number to be decoded.
  integer(I1P), allocatable:: nI1P(:) !< One byte integer array containing n.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  allocate(nI1P(1:BYI1P)) ; nI1P = 0_I1P
  call decode_bits(code=code,bits=nI1P)
  n = transfer(nI1P,n)
  return
  !---------------------------------------------------------------------------------------------------------------------------------
  endsubroutine b64_decode_I1

  pure subroutine b64_decode_R16_a(n,code)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Procedure for decoding a base64 code into an array numbers (R16P).
  !---------------------------------------------------------------------------------------------------------------------------------
  implicit none
  character(len=:), allocatable, intent(IN)::  code    !< Encoded array.
  real(R16P),                    intent(OUT):: n(1:)   !< Array of numbers to be decoded.
  integer(I1P),     allocatable::              nI1P(:) !< One byte integer array containing n.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  allocate(nI1P(1:size(n,dim=1)*BYR16P)) ; nI1P = 0_I1P
  call decode_bits(code=code,bits=nI1P)
  n = transfer(nI1P,n)
  return
  !---------------------------------------------------------------------------------------------------------------------------------
  endsubroutine b64_decode_R16_a

  pure subroutine b64_decode_R8_a(n,code)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Procedure for decoding a base64 code into an array numbers (R8P).
  !---------------------------------------------------------------------------------------------------------------------------------
  implicit none
  character(len=:), allocatable, intent(IN)::  code    !< Encoded array.
  real(R8P),                     intent(OUT):: n(1:)   !< Array of numbers to be decoded.
  integer(I1P),     allocatable::              nI1P(:) !< One byte integer array containing n.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  allocate(nI1P(1:size(n,dim=1)*BYR8P)) ; nI1P = 0_I1P
  call decode_bits(code=code,bits=nI1P)
  n = transfer(nI1P,n)
  return
  !---------------------------------------------------------------------------------------------------------------------------------
  endsubroutine b64_decode_R8_a

  pure subroutine b64_decode_R4_a(n,code)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Procedure for decoding a base64 code into an array numbers (R4P).
  !---------------------------------------------------------------------------------------------------------------------------------
  implicit none
  character(len=:), allocatable, intent(IN)::  code    !< Encoded array.
  real(R4P),                     intent(OUT):: n(1:)   !< Array of numbers to be decoded.
  integer(I1P),     allocatable::              nI1P(:) !< One byte integer array containing n.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  allocate(nI1P(1:size(n,dim=1)*BYR4P)) ; nI1P = 0_I1P
  call decode_bits(code=code,bits=nI1P)
  n = transfer(nI1P,n)
  return
  !---------------------------------------------------------------------------------------------------------------------------------
  endsubroutine b64_decode_R4_a

  pure subroutine b64_decode_I8_a(n,code)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Procedure for decoding a base64 code into an array numbers (I8P).
  !---------------------------------------------------------------------------------------------------------------------------------
  implicit none
  character(len=:), allocatable, intent(IN)::  code    !< Encoded array.
  integer(I8P),                  intent(OUT):: n(1:)   !< Array of numbers to be decoded.
  integer(I1P),     allocatable::              nI1P(:) !< One byte integer array containing n.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  allocate(nI1P(1:size(n,dim=1)*BYI8P)) ; nI1P = 0_I1P
  call decode_bits(code=code,bits=nI1P)
  n = transfer(nI1P,n)
  return
  !---------------------------------------------------------------------------------------------------------------------------------
  endsubroutine b64_decode_I8_a

  pure subroutine b64_decode_I4_a(n,code)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Procedure for decoding a base64 code into an array numbers (I4P).
  !---------------------------------------------------------------------------------------------------------------------------------
  implicit none
  character(len=:), allocatable, intent(IN)::  code    !< Encoded array.
  integer(I4P),                  intent(OUT):: n(1:)   !< Array of numbers to be decoded.
  integer(I1P),     allocatable::              nI1P(:) !< One byte integer array containing n.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  allocate(nI1P(1:size(n,dim=1)*BYI4P)) ; nI1P = 0_I1P
  call decode_bits(code=code,bits=nI1P)
  n = transfer(nI1P,n)
  return
  !---------------------------------------------------------------------------------------------------------------------------------
  endsubroutine b64_decode_I4_a

  pure subroutine b64_decode_I2_a(n,code)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Procedure for decoding a base64 code into an array numbers (I2P).
  !---------------------------------------------------------------------------------------------------------------------------------
  implicit none
  character(len=:), allocatable, intent(IN)::  code    !< Encoded array.
  integer(I2P),                  intent(OUT):: n(1:)   !< Array of numbers to be decoded.
  integer(I1P),     allocatable::              nI1P(:) !< One byte integer array containing n.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  allocate(nI1P(1:size(n,dim=1)*BYI2P)) ; nI1P = 0_I1P
  call decode_bits(code=code,bits=nI1P)
  n = transfer(nI1P,n)
  return
  !---------------------------------------------------------------------------------------------------------------------------------
  endsubroutine b64_decode_I2_a

  pure subroutine b64_decode_I1_a(n,code)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Procedure for decoding a base64 code into an array numbers (I1P).
  !---------------------------------------------------------------------------------------------------------------------------------
  implicit none
  character(len=:), allocatable, intent(IN)::  code    !< Encoded array.
  integer(I1P),                  intent(OUT):: n(1:)   !< Array of numbers to be decoded.
  integer(I1P),     allocatable::              nI1P(:) !< One byte integer array containing n.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  allocate(nI1P(1:size(n,dim=1)*BYI1P)) ; nI1P = 0_I1P
  call decode_bits(code=code,bits=nI1P)
  n = transfer(nI1P,n)
  return
  !---------------------------------------------------------------------------------------------------------------------------------
  endsubroutine b64_decode_I1_a

  subroutine autotest()
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Procedure for autotesting the library functionalities.
  !<
  !< @note Into the *src* directory there is a small python script (*validation.py*) that can be used to validate the library
  !< correctness by a comparison with other widely used tools such as the python builtin module *struct*.
  !---------------------------------------------------------------------------------------------------------------------------------
  implicit none
  real(R16P)::                    scalar_R16 = 134.231_R16P             !< Real input to be encoded.
  real(R8P)::                     scalar_R8  = 1._R8P                   !< Real input to be encoded.
  real(R4P)::                     scalar_R4  = 0._R4P                   !< Real input to be encoded.
  integer(I8P)::                  scalar_I8  = 23_I8P                   !< Integer input to be encoded.
  integer(I4P)::                  scalar_I4  = 2023_I4P                 !< Integer input to be encoded.
  integer(I2P)::                  scalar_I2  = -203_I2P                 !< Integer input to be encoded.
  integer(I1P)::                  scalar_I1  = 120_I1P                  !< Integer input to be encoded.
  real(R16P)::                    array_R16(1:2)= [121._R16P,2.32_R16P] !< Real input to be encoded.
  real(R8P)::                     array_R8(1:2) = [1._R8P,2._R8P      ] !< Real input to be encoded.
  real(R4P)::                     array_R4(1:2) = [0._R4P,-32.12_R4P  ] !< Real input to be encoded.
  integer(I8P)::                  array_I8(1:4) = [23,324,25456656,2  ] !< Integer input to be encoded.
  integer(I4P)::                  array_I4(1:2) = [2023,-24           ] !< Integer input to be encoded.
  integer(I2P)::                  array_I2(1:2) = [-203,-10           ] !< Integer input to be encoded.
  integer(I1P)::                  array_I1(1:2) = [+120,-1            ] !< Integer input to be encoded.
  character(len=:), allocatable:: code64                                !< Base64 encoded array.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  call b64_Init

  print "(A)", 'Encoders'

  print "(A)", 'Scalars'

  call b64_encode(n=scalar_R16,code=code64)
  print "(A,1X,L1)", '+ Code of '//trim(str(n=scalar_R16))//': "'//trim(code64)//'", Is it correct?',trim(code64)=='CKwcWmTHYEA='

  call b64_encode(n=scalar_R8,code=code64)
  print "(A,1X,L1)", '+ Code of '//trim(str(n=scalar_R8))//': "'//trim(code64)//'", Is it correct?',trim(code64)=='AAAAAAAA8D8='

  call b64_encode(n=scalar_R4,code=code64)
  print "(A,1X,L1)", '+ Code of '//trim(str(n=scalar_R4))//': "'//trim(code64)//'", Is it correct?',trim(code64)=='AAAAAA=='

  call b64_encode(n=scalar_I8,code=code64)
  print "(A,1X,L1)", '+ Code of '//trim(str(n=scalar_I8))//': "'//trim(code64)//'", Is it correct?',trim(code64)=='FwAAAAAAAAA='

  call b64_encode(n=scalar_I4,code=code64)
  print "(A,1X,L1)", '+ Code of '//trim(str(n=scalar_I4))//': "'//trim(code64)//'", Is it correct?',trim(code64)=='5wcAAA=='

  call b64_encode(n=scalar_I2,code=code64)
  print "(A,1X,L1)", '+ Code of '//trim(str(n=scalar_I2))//': "'//trim(code64)//'", Is it correct?',trim(code64)=='Nf8='

  call b64_encode(n=scalar_I1,code=code64)
  print "(A,1X,L1)", '+ Code of '//trim(str(n=scalar_I1))//': "'//trim(code64)//'", Is it correct?',trim(code64)=='eA=='

  print "(A)", 'Arrays'

  call b64_encode(n=array_R16,code=code64)
  print "(A,1X,L1)", '+ Code of ['//trim(str(n=array_R16(1)))//','//trim(str(n=array_R16(2)))//']: "'//trim(code64)//&
    '", Is it correct?',trim(code64)=='AAAAAABAXkCPwvUoXI8CQA=='

  call b64_encode(n=array_R8,code=code64)
  print "(A,1X,L1)", '+ Code of ['//trim(str(n=array_R8(1)))//','//trim(str(n=array_R8(2)))//']: "'//trim(code64)//&
    '", Is it correct?',trim(code64)=='AAAAAAAA8D8AAAAAAAAAQA=='

  call b64_encode(n=array_R4,code=code64)
  print "(A,1X,L1)", '+ Code of ['//trim(str(n=array_R4(1)))//','//trim(str(n=array_R4(2)))//']: "'//trim(code64)//&
    '", Is it correct?',trim(code64)=='AAAAAOF6AMI='

  call b64_encode(n=array_I8,code=code64)
  print "(A,1X,L1)", '+ Code of ['//trim(str(n=array_I8(1)))//','//trim(str(n=array_I8(2)))//','//                 &
                                    trim(str(n=array_I8(3)))//','//trim(str(n=array_I8(4)))//']: "'//trim(code64)//&
                                    '", Is it correct?',trim(code64)=='FwAAAAAAAABEAQAAAAAAABBwhAEAAAAAAgAAAAAAAAA='

  call b64_encode(n=array_I4,code=code64)
  print "(A,1X,L1)", '+ Code of ['//trim(str(n=array_I4(1)))//','//trim(str(n=array_I4(2)))//']: "'//trim(code64)//&
    '", Is it correct?',trim(code64)=='5wcAAOj///8='

  call b64_encode(n=array_I2,code=code64)
  print "(A,1X,L1)", '+ Code of ['//trim(str(n=array_I2(1)))//','//trim(str(n=array_I2(2)))//']: "'//trim(code64)//&
    '", Is it correct?',trim(code64)=='Nf/2/w=='

  call b64_encode(n=array_I1,code=code64)
  print "(A,1X,L1)", '+ Code of ['//trim(str(n=array_I1(1)))//','//trim(str(n=array_I1(2)))//']: "'//trim(code64)//&
    '", Is it correct?',trim(code64)=='eP8='

  print "(A)", 'Decoders'

  print "(A)", 'Scalars'

  code64 = 'AAAAAAAA8D8=' ; call b64_decode(code=code64,n=scalar_R8)
  print "(A)", '+ Decode of "'//code64//'": '//trim(str(n=scalar_R8))//', Should be 1.0'

  code64 = 'AAAAAA==' ; call b64_decode(code=code64,n=scalar_R4)
  print "(A)", '+ Decode of "'//code64//'": '//trim(str(n=scalar_R4))//', Should be 0.0'

  code64 = 'FwAAAAAAAAA=' ; call b64_decode(code=code64,n=scalar_I8)
  print "(A)", '+ Decode of "'//code64//'": '//trim(str(n=scalar_I8))//', Should be 23'

  code64 = '5wcAAA==' ; call b64_decode(code=code64,n=scalar_I4)
  print "(A)", '+ Decode of "'//code64//'": '//trim(str(n=scalar_I4))//', Should be 2023'

  code64 = 'Nf8=' ; call b64_decode(code=code64,n=scalar_I2)
  print "(A)", '+ Decode of "'//code64//'": '//trim(str(n=scalar_I2))//', Should be -203'

  code64 = 'eA==' ; call b64_decode(code=code64,n=scalar_I1)
  print "(A)", '+ Decode of "'//code64//'": '//trim(str(n=scalar_I1))//', Should be 120'

  print "(A)", 'Arrays'

  code64 = 'AAAAAAAA8D8AAAAAAAAAQA==' ; call b64_decode(code=code64,n=array_R8)
  print "(A)", '+ Decode of "'//code64//'": ['//trim(str(n=array_R8(1)))//','//trim(str(n=array_R8(2)))//'], Should be [1.0,2.0]'

  code64 = 'AAAAAOF6AMI=' ; call b64_decode(code=code64,n=array_R4)
  print "(A)", '+ Decode of "'//code64//'": ['//trim(str(n=array_R4(1)))//','//trim(str(n=array_R4(2)))//'], Should be [0.0,-32.12]'

  code64 = 'FwAAAAAAAABEAQAAAAAAABBwhAEAAAAAAgAAAAAAAAA=' ; call b64_decode(code=code64,n=array_I8)
  print "(A)", '+ Decode of "'//code64//'": ['//trim(str(n=array_I8(1)))//','//&
                                                trim(str(n=array_I8(2)))//','//&
                                                trim(str(n=array_I8(3)))//','//&
                                                trim(str(n=array_I8(4)))//'], Should be [23,324,25456656,2]'

  code64 = '5wcAAOj///8=' ; call b64_decode(code=code64,n=array_I4)
  print "(A)", '+ Decode of "'//code64//'": ['//trim(str(n=array_I4(1)))//','//trim(str(n=array_I4(2)))//'], Should be [2023,-24]'

  code64 = 'Nf/2/w==' ; call b64_decode(code=code64,n=array_I2)
  print "(A)", '+ Decode of "'//code64//'": ['//trim(str(n=array_I2(1)))//','//trim(str(n=array_I2(2)))//'], Should be [-203,-10]'

  code64 = 'eP8=' ; call b64_decode(code=code64,n=array_I1)
  print "(A)", '+ Decode of "'//code64//'": ['//trim(str(n=array_I1(1)))//','//trim(str(n=array_I1(2)))//'], Should be [120,-1]'
  return
  !---------------------------------------------------------------------------------------------------------------------------------
  endsubroutine autotest
endmodule Lib_Base64
