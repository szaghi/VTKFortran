!< Portability Environment for Fortran poor people.

module penf
!< Portability Environment for Fortran poor people.
use penf_global_parameters_variables
#ifdef __INTEL_COMPILER
use penf_b_size
#else
use penf_b_size, only : bit_size, byte_size
#endif
use penf_stringify, only : str_ascii, str_ucs4, str, strz, cton, bstr, bcton

implicit none
private
save
! global parameters and variables
public :: endianL, endianB, endian, is_initialized
public :: ASCII, UCS4, CK
public :: R16P, FR16P, DR16P, MinR16P, MaxR16P, BIR16P, BYR16P, smallR16P, ZeroR16P
public :: R8P,  FR8P,  DR8P,  MinR8P,  MaxR8P,  BIR8P,  BYR8P,  smallR8P,  ZeroR8P
public :: R4P,  FR4P,  DR4P,  MinR4P,  MaxR4P,  BIR4P,  BYR4P,  smallR4P,  ZeroR4P
public :: R_P,  FR_P,  DR_P,  MinR_P,  MaxR_P,  BIR_P,  BYR_P,  smallR_P,  ZeroR_P
public :: I8P,  FI8P,  DI8P,  MinI8P,  MaxI8P,  BII8P,  BYI8P
public :: I4P,  FI4P,  DI4P,  MinI4P,  MaxI4P,  BII4P,  BYI4P
public :: I2P,  FI2P,  DI2P,  MinI2P,  MaxI2P,  BII2P,  BYI2P
public :: I1P,  FI1P,  DI1P,  MinI1P,  MaxI1P,  BII1P,  BYI1P
public :: I_P,  FI_P,  DI_P,  MinI_P,  MaxI_P,  BII_P,  BYI_P
public :: CHARACTER_KINDS_LIST, REAL_KINDS_LIST, REAL_FORMATS_LIST
public :: INTEGER_KINDS_LIST, INTEGER_FORMATS_LIST
! bit/byte size functions
public :: bit_size, byte_size
! stringify facility
public :: str_ascii, str_ucs4
public :: str, strz, cton
public :: bstr, bcton
! miscellanea facility
public :: check_endian
public :: digit
public :: penf_Init
public :: penf_print

integer, protected :: endian         = endianL !< Bit ordering: Little endian (endianL), or Big endian (endianB).
logical, protected :: is_initialized = .false. !< Check the initialization of some variables that must be initialized.

#ifdef __GFORTRAN__
! work-around for strange gfortran bug...
interface bit_size
  !< Overloading of the intrinsic *bit_size* function for computing the number of bits of (also) real and character variables.
endinterface
#endif

interface digit
  !< Compute the number of digits in decimal base of the input integer.
  module procedure digit_I8, digit_I4, digit_I2, digit_I1
endinterface

contains
   ! public procedures
   subroutine check_endian()
   !< Check the type of bit ordering (big or little endian) of the running architecture.
   !<
   !> @note The result is stored into the *endian* global variable.
   !<
   !<```fortran
   !< use penf
   !< call check_endian
   !< print *, endian
   !<```
   !=> 1 <<<
   if (is_little_endian()) then
      endian = endianL
   else
      endian = endianB
   endif
   contains
      pure function is_little_endian() result(is_little)
      !< Check if the type of the bit ordering of the running architecture is little endian.
      logical      :: is_little !< Logical output: true is the running architecture uses little endian ordering, false otherwise.
      integer(I1P) :: int1(1:4) !< One byte integer array for casting 4 bytes integer.

      int1 = transfer(1_I4P, int1)
      is_little = (int1(1)==1_I1P)
      endfunction is_little_endian
   endsubroutine check_endian

   subroutine penf_init()
   !< Initialize PENF's variables that are not initialized into the definition specification.
   !<
   !<```fortran
   !< use penf
   !< call penf_init
   !< print FI1P, BYR4P
   !<```
   !=> 4 <<<

   call check_endian
   BIR16P = bit_size(MaxR16P) ; BYR16P = BIR16P / 8_I2P
   BIR8P  = bit_size(MaxR8P)  ; BYR8P  = BIR8P  / 8_I1P
   BIR4P  = bit_size(MaxR4P)  ; BYR4P  = BIR4P  / 8_I1P
   BIR_P  = bit_size(MaxR_P)  ; BYR_P  = BIR_P  / 8_I1P
   is_initialized = .true.
   endsubroutine penf_init

   subroutine penf_print(unit, pref, iostat, iomsg)
   !< Print to the specified unit the PENF's environment data.
   !<
   !<```fortran
   !< use penf
   !< integer :: u
   !< open(newunit=u, status='scratch')
   !< call penf_print(u)
   !< close(u)
   !< print "(A)", 'done'
   !<```
   !=> done <<<
   integer(I4P), intent(in)            :: unit    !< Logic unit.
   character(*), intent(in),  optional :: pref    !< Prefixing string.
   integer(I4P), intent(out), optional :: iostat  !< IO error.
   character(*), intent(out), optional :: iomsg   !< IO error message.
   character(len=:), allocatable       :: prefd   !< Prefixing string.
   integer(I4P)                        :: iostatd !< IO error.
   character(500)                      :: iomsgd  !< Temporary variable for IO error message.

   if (.not.is_initialized) call penf_init
   prefd = '' ; if (present(pref)) prefd = pref
   if (endian==endianL) then
     write(unit=unit,fmt='(A)',iostat=iostatd,iomsg=iomsgd)prefd//'This architecture has LITTLE Endian bit ordering'
   else
     write(unit=unit,fmt='(A)',iostat=iostatd,iomsg=iomsgd)prefd//'This architecture has BIG Endian bit ordering'
   endif
   write(unit=unit,fmt='(A)',iostat=iostatd,iomsg=iomsgd)  prefd//'Character kind:'
   write(unit=unit,fmt='(A)',iostat=iostatd,iomsg=iomsgd)  prefd//'  ASCII: '//str(n=ASCII)
   write(unit=unit,fmt='(A)',iostat=iostatd,iomsg=iomsgd)  prefd//'  UCS4:  '//str(n=UCS4)
   write(unit=unit,fmt='(A)',iostat=iostatd,iomsg=iomsgd)  prefd//'  CK:    '//str(n=CK)
   write(unit=unit,fmt='(A)',iostat=iostatd,iomsg=iomsgd)  prefd//'Reals kind, format and characters number:'
   write(unit=unit,fmt='(A)',iostat=iostatd,iomsg=iomsgd)  prefd//'  R16P: '//str(n=R16P)//','//FR16P//','//str(n=DR16P)
   write(unit=unit,fmt='(A)',iostat=iostatd,iomsg=iomsgd)  prefd//'  R8P:  '//str(n=R8P )//','//FR8P //','//str(n=DR8P )
   write(unit=unit,fmt='(A)',iostat=iostatd,iomsg=iomsgd)  prefd//'  R4P:  '//str(n=R4P )//','//FR4P //','//str(n=DR4P )
   write(unit=unit,fmt='(A)',iostat=iostatd,iomsg=iomsgd)  prefd//'  R_P:  '//str(n=R_P )//','//FR_P //','//str(n=DR_P )
   write(unit=unit,fmt='(A)',iostat=iostatd,iomsg=iomsgd)  prefd//'Integers kind, format and characters number:'
   write(unit=unit,fmt='(A)',iostat=iostatd,iomsg=iomsgd)  prefd//'  I8P:  '//str(n=I8P)//','//FI8P //','//str(n=DI8P)
   write(unit=unit,fmt='(A)',iostat=iostatd,iomsg=iomsgd)  prefd//'  I4P:  '//str(n=I4P)//','//FI4P //','//str(n=DI4P)
   write(unit=unit,fmt='(A)',iostat=iostatd,iomsg=iomsgd)  prefd//'  I2P:  '//str(n=I2P)//','//FI2P //','//str(n=DI2P)
   write(unit=unit,fmt='(A)',iostat=iostatd,iomsg=iomsgd)  prefd//'  I1P:  '//str(n=I1P)//','//FI1P //','//str(n=DI1P)
   write(unit=unit,fmt='(A)',iostat=iostatd,iomsg=iomsgd)  prefd//'Reals minimum and maximum values:'
   write(unit=unit,fmt='(A)',iostat=iostatd,iomsg=iomsgd)  prefd//'  R16P: '//str(n=MinR16P)//','//str(n=MaxR16P)
   write(unit=unit,fmt='(A)',iostat=iostatd,iomsg=iomsgd)  prefd//'  R8P:  '//str(n=MinR8P )//','//str(n=MaxR8P )
   write(unit=unit,fmt='(A)',iostat=iostatd,iomsg=iomsgd)  prefd//'  R4P:  '//str(n=MinR4P )//','//str(n=MaxR4P )
   write(unit=unit,fmt='(A)',iostat=iostatd,iomsg=iomsgd)  prefd//'  R_P:  '//str(n=MinR_P )//','//str(n=MaxR_P )
   write(unit=unit,fmt='(A)',iostat=iostatd,iomsg=iomsgd)  prefd//'Integergs minimum and maximum values:'
   write(unit=unit,fmt='(A)',iostat=iostatd,iomsg=iomsgd)  prefd//'  I8P:  '//str(n=MinI8P )//','//str(n=MaxI8P)
   write(unit=unit,fmt='(A)',iostat=iostatd,iomsg=iomsgd)  prefd//'  I4P:  '//str(n=MinI4P )//','//str(n=MaxI4P)
   write(unit=unit,fmt='(A)',iostat=iostatd,iomsg=iomsgd)  prefd//'  I2P:  '//str(n=MinI2P )//','//str(n=MaxI2P)
   write(unit=unit,fmt='(A)',iostat=iostatd,iomsg=iomsgd)  prefd//'  I1P:  '//str(n=MinI1P )//','//str(n=MaxI1P)
   write(unit=unit,fmt='(A)',iostat=iostatd,iomsg=iomsgd)  prefd//'Reals bits/bytes sizes:'
   write(unit=unit,fmt='(A)',iostat=iostatd,iomsg=iomsgd)  prefd//'  R16P: '//str(n=BIR16P)//'/'//str(n=BYR16P)
   write(unit=unit,fmt='(A)',iostat=iostatd,iomsg=iomsgd)  prefd//'  R8P:  '//str(n=BIR8P )//'/'//str(n=BYR8P )
   write(unit=unit,fmt='(A)',iostat=iostatd,iomsg=iomsgd)  prefd//'  R4P:  '//str(n=BIR4P )//'/'//str(n=BYR4P )
   write(unit=unit,fmt='(A)',iostat=iostatd,iomsg=iomsgd)  prefd//'  R_P:  '//str(n=BIR_P )//'/'//str(n=BYR_P )
   write(unit=unit,fmt='(A)',iostat=iostatd,iomsg=iomsgd)  prefd//'Integers bits/bytes sizes:'
   write(unit=unit,fmt='(A)',iostat=iostatd,iomsg=iomsgd)  prefd//'  I8P:  '//str(n=BII8P)//'/'//str(n=BYI8P)
   write(unit=unit,fmt='(A)',iostat=iostatd,iomsg=iomsgd)  prefd//'  I4P:  '//str(n=BII4P)//'/'//str(n=BYI4P)
   write(unit=unit,fmt='(A)',iostat=iostatd,iomsg=iomsgd)  prefd//'  I2P:  '//str(n=BII2P)//'/'//str(n=BYI2P)
   write(unit=unit,fmt='(A)',iostat=iostatd,iomsg=iomsgd)  prefd//'  I1P:  '//str(n=BII1P)//'/'//str(n=BYI1P)
   write(unit=unit,fmt='(A)',iostat=iostatd,iomsg=iomsgd)  prefd//'Smallest reals'
   write(unit=unit,fmt='(A)',iostat=iostatd,iomsg=iomsgd)  prefd//'  smallR16P: '//str(smallR16P, .true.)
   write(unit=unit,fmt='(A)',iostat=iostatd,iomsg=iomsgd)  prefd//'  smallR8P:  '//str(smallR8P,  .true.)
   write(unit=unit,fmt='(A)',iostat=iostatd,iomsg=iomsgd)  prefd//'  smallR4P:  '//str(smallR4P,  .true.)
   write(unit=unit,fmt='(A)',iostat=iostatd,iomsg=iomsgd)  prefd//'  smallR_P:  '//str(smallR_P,  .true.)
   write(unit=unit,fmt='(A)',iostat=iostatd,iomsg=iomsgd)  prefd//'Machine zero'
   write(unit=unit,fmt='(A)',iostat=iostatd,iomsg=iomsgd)  prefd//'  ZeroR16P: '//str(ZeroR16P, .true.)
   write(unit=unit,fmt='(A)',iostat=iostatd,iomsg=iomsgd)  prefd//'  ZeroR8P:  '//str(ZeroR8P,  .true.)
   write(unit=unit,fmt='(A)',iostat=iostatd,iomsg=iomsgd)  prefd//'  ZeroR4P:  '//str(ZeroR4P,  .true.)
   write(unit=unit,fmt='(A)',iostat=iostatd,iomsg=iomsgd)  prefd//'  ZeroR_P:  '//str(ZeroR_P,  .true.)
   if (present(iostat)) iostat = iostatd
   if (present(iomsg))  iomsg  = iomsgd
   endsubroutine penf_print

   ! private procedures
   elemental function digit_I8(n) result(digit)
   !< Compute the number of digits in decimal base of the input integer.
   !<
   !<```fortran
   !< use penf
   !< print FI4P, digit(100_I8P)
   !<```
   !=> 3 <<<
   integer(I8P), intent(in) :: n     !< Input integer.
   character(DI8P)          :: str   !< Returned string containing input number plus padding zeros.
   integer(I4P)             :: digit !< Number of digits.

   write(str, FI8P) abs(n)        ! Casting of n to string.
   digit = len_trim(adjustl(str)) ! Calculating the digits number of n.
   endfunction digit_I8

   elemental function digit_I4(n) result(digit)
   !< Compute the number of digits in decimal base of the input integer.
   !<
   !<```fortran
   !< use penf
   !< print FI4P, digit(100_I4P)
   !<```
   !=> 3 <<<
   integer(I4P), intent(in) :: n     !< Input integer.
   character(DI4P)          :: str   !< Returned string containing input number plus padding zeros.
   integer(I4P)             :: digit !< Number of digits.

   write(str, FI4P) abs(n)        ! Casting of n to string.
   digit = len_trim(adjustl(str)) ! Calculating the digits number of n.
   endfunction digit_I4

   elemental function digit_I2(n) result(digit)
   !< Compute the number of digits in decimal base of the input integer.
   !<
   !<```fortran
   !< use penf
   !< print FI4P, digit(100_I2P)
   !<```
   !=> 3 <<<
   integer(I2P), intent(in) :: n     !< Input integer.
   character(DI2P)          :: str   !< Returned string containing input number plus padding zeros.
   integer(I4P)             :: digit !< Number of digits.

   write(str, FI2P) abs(n)        ! Casting of n to string.
   digit = len_trim(adjustl(str)) ! Calculating the digits number of n.
   endfunction digit_I2

   elemental function digit_I1(n) result(digit)
   !< Compute the number of digits in decimal base of the input integer.
   !<
   !<```fortran
   !< use penf
   !< print FI4P, digit(100_I1P)
   !<```
   !=> 3 <<<
   integer(I1P), intent(in) :: n     !< Input integer.
   character(DI1P)          :: str   !< Returned string containing input number plus padding zeros.
   integer(I4P)             :: digit !< Number of digits.

   write(str, FI1P) abs(n)        ! Casting of n to string.
   digit = len_trim(adjustl(str)) ! Calculating the digits number of n.
   endfunction digit_I1
endmodule penf
