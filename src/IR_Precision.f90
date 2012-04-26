!> @brief     The module IR_Precision makes available some portable kind-parameters and some useful procedures to deal with them.
!> @details   It also provides variables that contain the minimum and maximum representable values, smallest real values and
!>            smallest representable differences by the running calculator.
!>
!>            Finally the module provides procedures to convert a string to number and vice versa, a function to check the endianism
!>            of the running calculator and a procedure to print all the aboves values.
!> @note The \em quadruple precision R16P could be activated defining \b r16p pre-processor flag (e.g. -Dr16p). Furthermore if
!> compiling with Portland Group Compiler define the pre-processor flag \b pgf95 to avoid error in computing \em Zero variables:
!> pgf compiler doesn't accept \b nearest built-in function in variables initialization.
!> @author    Stefano Zaghi
!> @version   1.0
!> @date      2012-04-24
!> @copyright GNU Public License version 3.
!> @todo \b g95_test: Test g95 compiler
!> @param[out] endianL,endianB,endian
!> @param[out] R16P, FR16P, DR16P, MinR16P, MaxR16P, smallR16P, ZeroR16
!> @param[out] R8P,  FR8P,  DR8P,  MinR8P,  MaxR8P,  smallR8P,  ZeroR8
!> @param[out] R4P,  FR4P,  DR4P,  MinR4P,  MaxR4P,  smallR4P,  ZeroR4
!> @param[out] R_P,  FR_P,  DR_P,  MinR_P,  MaxR_P,  smallR_P,  Zero
!> @param[out] I8P,  FI8P,  DI8P,  MinI8P,  MaxI8P
!> @param[out] I4P,  FI4P,  DI4P,  MinI4P,  MaxI4P
!> @param[out] I2P,  FI2P,  DI2P,  MinI2P,  MaxI2P
!> @param[out] I1P,  FI1P,  DI1P,  MinI1P,  MaxI1P
!> @param[out] I_P,  FI_P,  DI_P,  MinI_P,  MaxI_P
!> @param[out] check_endian
!> @param[out] str, strz, cton
!> @param[out] IR_Print
module IR_Precision
!-----------------------------------------------------------------------------------------------------------------------------------
USE, intrinsic:: ISO_FORTRAN_ENV, only: stdout => OUTPUT_UNIT, stderr => ERROR_UNIT ! Standard output/error logical units.
!-----------------------------------------------------------------------------------------------------------------------------------

!-----------------------------------------------------------------------------------------------------------------------------------
implicit none
private
public:: endianL,endianB,endian
#ifdef r16p
public:: R16P, FR16P, DR16P, MinR16P, MaxR16P, smallR16P, ZeroR16
#endif
public:: R8P,  FR8P,  DR8P,  MinR8P,  MaxR8P,  smallR8P,  ZeroR8
public:: R4P,  FR4P,  DR4P,  MinR4P,  MaxR4P,  smallR4P,  ZeroR4
public:: R_P,  FR_P,  DR_P,  MinR_P,  MaxR_P,  smallR_P,  Zero
public:: I8P,  FI8P,  DI8P,  MinI8P,  MaxI8P
public:: I4P,  FI4P,  DI4P,  MinI4P,  MaxI4P
public:: I2P,  FI2P,  DI2P,  MinI2P,  MaxI2P
public:: I1P,  FI1P,  DI1P,  MinI1P,  MaxI1P
public:: I_P,  FI_P,  DI_P,  MinI_P,  MaxI_P
public:: check_endian
public:: str, strz, cton
public:: IR_Print
!-----------------------------------------------------------------------------------------------------------------------------------

!-----------------------------------------------------------------------------------------------------------------------------------
! Bit ordering of the running architecture:
integer, parameter:: endianL = 1       !< Little endian parameter.
integer, parameter:: endianB = 0       !< Big endian parameter.
integer::            endian  = endianL !< Bit ordering: Little endian (endianL), or Big endian (endianB).

! The following are the portable kind parameters available.
! Real precision definitions:
#ifdef r16p
integer, parameter:: R16P = selected_real_kind(33,4931) !< 33  digits, range \f$[10^{-4931}, 10^{+4931} - 1]\f$.
#endif
integer, parameter:: R8P  = selected_real_kind(15,307)  !< 15  digits, range \f$[10^{-307} , 10^{+307}  - 1]\f$.
integer, parameter:: R4P  = selected_real_kind(6,37)    !< 6   digits, range \f$[10^{-37}  , 10^{+37}   - 1]\f$.
integer, parameter:: R_P  = R8P                         !< Default real precision.
! Integer precision definitions:
integer, parameter:: I8P  = selected_int_kind(18) !< Range \f$[-2^{63},+2^{63} - 1]\f$, 19 digits plus sign.
integer, parameter:: I4P  = selected_int_kind(9)  !< Range \f$[-2^{31},+2^{31} - 1]\f$, 10 digits plus sign.
integer, parameter:: I2P  = selected_int_kind(4)  !< Range \f$[-2^{15},+2^{15} - 1]\f$, 5  digits plus sign.
integer, parameter:: I1P  = selected_int_kind(2)  !< Range \f$[-2^{7} ,+2^{7}  - 1]\f$, 3  digits plus sign.
integer, parameter:: I_P  = I4P                   !< Default integer precision.

! Besides the kind parameters there are also the format parameters useful for writing in a well-ascii-format numeric variables.
! Real output formats:
#ifdef r16p
character(10), parameter:: FR16P = '(E42.33E4)' !< Output format for kind=R16P variable.
#endif
character(10), parameter:: FR8P  = '(E23.15E3)' !< Output format for kind=R8P variable.
character(9),  parameter:: FR4P  = '(E13.6E2)'  !< Output format for kind=R4P variable.
character(10), parameter:: FR_P  = FR8P         !< Output format for kind=R_P variable.
! Real number of digits of output formats:
#ifdef r16p
integer, parameter:: DR16P = 42   !< Number of digits of output format FR16P.
#endif
integer, parameter:: DR8P  = 23   !< Number of digits of output format FR8P.
integer, parameter:: DR4P  = 13   !< Number of digits of output format FR4P.
integer, parameter:: DR_P  = DR8P !< Number of digits of output format FR_P.
! Integer output formats:
character(5), parameter:: FI8P   = '(I20)'    !< Output format                     for kind=I8P variable.
character(8), parameter:: FI8PZP = '(I20.19)' !< Output format with zero prefixing for kind=I8P variable.
character(5), parameter:: FI4P   = '(I11)'    !< Output format                     for kind=I4P variable.
character(8), parameter:: FI4PZP = '(I11.10)' !< Output format with zero prefixing for kind=I4P variable.
character(4), parameter:: FI2P   = '(I6)'     !< Output format                     for kind=I2P variable.
character(6), parameter:: FI2PZP = '(I6.5)'   !< Output format with zero prefixing for kind=I2P variable.
character(4), parameter:: FI1P   = '(I4)'     !< Output format                     for kind=I1P variable.
character(6), parameter:: FI1PZP = '(I4.3)'   !< Output format with zero prefixing for kind=I1P variable.
character(5), parameter:: FI_P   = FI4P       !< Output format                     for kind=I_P variable.
character(8), parameter:: FI_PZP = FI4PZP     !< Output format with zero prefixing for kind=I_P variable.
! Integer number of digits of output formats:
integer, parameter:: DI8P = 20   !< Number of digits of output format I8P.
integer, parameter:: DI4P = 11   !< Number of digits of output format I4P.
integer, parameter:: DI2P = 6    !< Number of digits of output format I2P.
integer, parameter:: DI1P = 4    !< Number of digits of output format I1P.
integer, parameter:: DI_P = DI4P !< Number of digits of output format I_P.

! Useful parameters for handling numbers ranges.
! Real min and max values:
#ifdef r16p
real(R16P), parameter:: MinR16P = -huge(1._R16P), MaxR16P = huge(1._R16P) !< Min and max values of kind=R16P variable.
#endif
real(R8P),  parameter:: MinR8P  = -huge(1._R8P ), MaxR8P  = huge(1._R8P ) !< Min and max values of kind=R8P variable.
real(R4P),  parameter:: MinR4P  = -huge(1._R4P ), MaxR4P  = huge(1._R4P ) !< Min and max values of kind=R4P variable.
real(R_P),  parameter:: MinR_P  = MinR8P,         MaxR_P  = MaxR8P        !< Min and max values of kind=R_P variable.
! Real smallest values:
#ifdef r16p
real(R16P), parameter:: smallR16P = tiny(1._R16P) !< Smallest representable value of kind=R16P variable.
#endif
real(R8P),  parameter:: smallR8P  = tiny(1._R8P ) !< Smallest representable value of kind=R8P variable.
real(R4P),  parameter:: smallR4P  = tiny(1._R4P ) !< Smallest representable value of kind=R4P variable.
real(R_P),  parameter:: smallR_P  = smallR8P      !< Smallest representable value of kind=R_P variable.
! Integer min and max values:
integer(I8P), parameter:: MinI8P = -huge(1_I8P)-1_I8P, MaxI8P = huge(1_I8P) !< Min and max values of kind=I8P variable.
integer(I4P), parameter:: MinI4P = -huge(1_I4P)-1_I4P, MaxI4P = huge(1_I4P) !< Min and max values of kind=I4P variable.
integer(I2P), parameter:: MinI2P = -huge(1_I2P)-1_I2P, MaxI2P = huge(1_I2P) !< Min and max values of kind=I2P variable.
integer(I2P), parameter:: MinI1P = -huge(1_I1P)-1_I1P, MaxI1P = huge(1_I1P) !< Min and max values of kind=I1P variable.
integer(I_P), parameter:: MinI_P = MinI4P,             MaxI_P = MaxI4P      !< Min and max values of kind=I_P variable.

! Smallest real representable difference by the running calculator.
#ifdef r16p
real(R16P), parameter:: ZeroR16 = nearest(1._R16P,1._R16P) - nearest(1._R16P,-1._R16P) !< Smallest representable difference of
                                                                                       !< kind=R16P variable.
#endif
#ifdef pgf95
real(R8P),  parameter:: ZeroR8  = 0._R8P
real(R4P),  parameter:: ZeroR4  = 0._R4P
#else
real(R8P),  parameter:: ZeroR8  = nearest(1._R8P,1._R8P) - nearest(1._R8P,-1._R8P) !< Smallest representable difference of
                                                                                   !< kind=R8P variable.
real(R4P),  parameter:: ZeroR4  = nearest(1._R4P,1._R4P) - nearest(1._R4P,-1._R4P) !< Smallest representable difference of
                                                                                   !< kind=R4P variable.
#endif
real(R_P),  parameter:: Zero    = ZeroR8                                           !< Smallest representable difference of
                                                                                   !< kind=R_P variable.
!-----------------------------------------------------------------------------------------------------------------------------------

!-----------------------------------------------------------------------------------------------------------------------------------
!> @brief Function for converting number (real and integer) to string (number to string type casting);
!> logical, intent(\b IN), optional:: <b>\em no_sign</b> flag for do not write sign;
!> number,  intent(\b IN)::           <b>\em n</b> input number;
!> string,  intent(\b OUT)::          <b>\em str</b> output string.
interface str
  module procedure           &
#ifdef r16p
                   str_R16P, &
#endif
                   str_R8P,  &
                   str_R4P,  &
                   str_I8P,  &
                   str_I4P,  &
                   str_I2P,  &
                   str_I1P
endinterface
!> @brief Function for converting number (integer) to string, prefixing with the right number of zeros (number to string type
!>        casting with zero padding);
!> number,  intent(\b IN), optional:: <b>\em no_zpad</b> number of padding zeros;
!> number,  intent(\b IN)::           <b>\em n  </b> input number;
!> string,  intent(\b OUT)::          <b>\em str</b> output string.
interface strz
  module procedure strz_I8P,  &
                   strz_I4P,  &
                   strz_I2P,  &
                   strz_I1P
endinterface
!> @brief Function for converting string to number (real or initeger, string to number type casting);
!> string,  intent(\b IN)::  <b>\em str</b> input string;
!> number,  intent(\b OUT):: <b>\em n  </b> output number.
interface cton
  module procedure            &
#ifdef r16p
                   ctor_R16P, &
#endif
                   ctor_R8P,  &
                   ctor_R4P,  &
                   ctoi_I8P,  &
                   ctoi_I4P,  &
                   ctoi_I2P,  &
                   ctoi_I1P
endinterface
!-----------------------------------------------------------------------------------------------------------------------------------
contains
  !>Subroutine for checking the type of bit ordering (big or little endian) of the running architecture; the result is
  !>stored into the "endian" global variable.
  !>@return endian
  subroutine check_endian()
  !---------------------------------------------------------------------------------------------------------------------------------
  implicit none
  integer(I1P):: int1(1:4) !< One byte integer array for casting 4 bytes integer.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  int1 = transfer(1_I4P,int1)
  if (int1(1)==1_I1P) then
    endian = endianL
  else
    endian = endianB
  endif
  return
  !---------------------------------------------------------------------------------------------------------------------------------
  endsubroutine check_endian

#ifdef r16p
  elemental function str_R16P(no_sign,n) result(str)
  !---------------------------------------------------------------------------------------------------------------------------------
  !!The function str_R16P converts real to string. This function achieves casting of real to string.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  implicit none
  logical,    intent(IN), optional:: no_sign ! Flag for leaving out the sign.
  real(R16P), intent(IN)::           n       ! Real to be converted.
  character(DR16P)::                 str     ! Returned string containing input number.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  write(str,FR16P) n                ! Casting of n to string.
  if (n>0._R16P) str(1:1)='+'       ! Prefixing plus if n>0.
  if (present(no_sign)) str=str(2:) ! Leaving out the sign.
  return
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction str_R16P
#endif

  elemental function str_R8P(no_sign,n) result(str)
  !---------------------------------------------------------------------------------------------------------------------------------
  !!The function str_R8P converts real to string. This function achieves casting of real to string.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  implicit none
  logical,   intent(IN), optional:: no_sign ! Flag for leaving out the sign.
  real(R8P), intent(IN)::           n       ! Real to be converted.
  character(DR8P)::                 str     ! Returned string containing input number.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  write(str,FR8P) n                 ! Casting of n to string.
  if (n>0._R8P) str(1:1)='+'        ! Prefixing plus if n>0.
  if (present(no_sign)) str=str(2:) ! Leaving out the sign.
  return
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction str_R8P

  elemental function str_R4P(no_sign,n) result(str)
  !---------------------------------------------------------------------------------------------------------------------------------
  !!The function str_R4P converts real to string. This function achieves casting of real to string.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  implicit none
  logical,   intent(IN), optional:: no_sign ! Flag for leaving out the sign.
  real(R4P), intent(IN)::           n       ! Real to be converted.
  character(DR4P)::                 str     ! Returned string containing input number.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  write(str,FR4P) n                 ! Casting of n to string.
  if (n>0._R4P) str(1:1)='+'        ! Prefixing plus if n>0.
  if (present(no_sign)) str=str(2:) ! Leaving out the sign.
  return
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction str_R4P

  elemental function str_I8P(no_sign,n) result(str)
  !---------------------------------------------------------------------------------------------------------------------------------
  !!The function str_I8P converts integer to string. This function achieves casting of integer to string.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  implicit none
  logical,      intent(IN), optional:: no_sign ! Flag for leaving out the sign.
  integer(I8P), intent(IN)::           n       ! Integer to be converted.
  character(DI8P)::                    str     ! Returned string containing input number plus padding zeros.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  write(str,FI8P) n                 ! Casting of n to string.
  str = adjustl(trim(str))          ! Removing white spaces.
  if (n>=0_I8P) str='+'//trim(str)  ! Prefixing plus if n>0.
  if (present(no_sign)) str=str(2:) ! Leaving out the sign.
  return
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction str_I8P

  elemental function str_I4P(no_sign,n) result(str)
  !---------------------------------------------------------------------------------------------------------------------------------
  !!The function str_I4P converts integer to string. This function achieves casting of integer to string.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  implicit none
  logical,      intent(IN), optional:: no_sign ! Flag for leaving out the sign.
  integer(I4P), intent(IN)::           n       ! Integer to be converted.
  character(DI4P)::                    str     ! Returned string containing input number plus padding zeros.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  write(str,FI4P) n                 ! Casting of n to string.
  str = adjustl(trim(str))          ! Removing white spaces.
  if (n>=0_I4P) str='+'//trim(str)  ! Prefixing plus if n>0.
  if (present(no_sign)) str=str(2:) ! Leaving out the sign.
  return
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction str_I4P

  elemental function str_I2P(no_sign,n) result(str)
  !---------------------------------------------------------------------------------------------------------------------------------
  !!The function str_I2P converts integer to string. This function achieves casting of integer to string.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  implicit none
  logical,      intent(IN), optional:: no_sign ! Flag for leaving out the sign.
  integer(I2P), intent(IN)::           n       ! Integer to be converted.
  character(DI2P)::                    str     ! Returned string containing input number plus padding zeros.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  write(str,FI2P) n                 ! Casting of n to string.
  str = adjustl(trim(str))          ! Removing white spaces.
  if (n>=0_I2P) str='+'//trim(str)  ! Prefixing plus if n>0.
  if (present(no_sign)) str=str(2:) ! Leaving out the sign.
  return
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction str_I2P

  elemental function str_I1P(no_sign,n) result(str)
  !---------------------------------------------------------------------------------------------------------------------------------
  !!The function str_I1P converts integer to string. This function achieves casting of integer to string.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  implicit none
  logical,      intent(IN), optional:: no_sign ! Flag for leaving out the sign.
  integer(I1P), intent(IN)::           n       ! Integer to be converted.
  character(DI1P)::                    str     ! Returned string containing input number plus padding zeros.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  write(str,FI1P) n                 ! Casting of n to string.
  str = adjustl(trim(str))          ! Removing white spaces.
  if (n>=0_I1P) str='+'//trim(str)  ! Prefixing plus if n>0.
  if (present(no_sign)) str=str(2:) ! Leaving out the sign.
  return
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction str_I1P

  elemental function strz_I8P(nz_pad,n) result(str)
  !---------------------------------------------------------------------------------------------------------------------------------
  !!The function strz_I8P converts integer to string, prefixing with the right number of zeros. This function achieves casting of
  !!integer to string.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  implicit none
  integer(I4P), intent(IN), optional:: nz_pad ! Number of zeros padding.
  integer(I8P), intent(IN)::           n      ! Integer to be converted.
  character(DI8P)::                    str    ! Returned string containing input number plus padding zeros.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  write(str,FI8PZP) n                              ! Casting of n to string.
  str=str(2:)                                      ! Leaving out the sign.
  if (present(nz_pad)) str=str(DI8P-nz_pad:DI8P-1) ! Leaving out the extra zeros padding
  return
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction strz_I8P

  elemental function strz_I4P(nz_pad,n) result(str)
  !---------------------------------------------------------------------------------------------------------------------------------
  !!The function strz_I4P converts integer to string, prefixing with the right number of zeros. This function achieves casting of
  !!integer to string.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  implicit none
  integer(I4P), intent(IN), optional:: nz_pad ! Number of zeros padding.
  integer(I4P), intent(IN)::           n      ! Integer to be converted.
  character(DI4P)::                    str    ! Returned string containing input number plus padding zeros.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  write(str,FI4PZP) n                              ! Casting of n to string.
  str=str(2:)                                      ! Leaving out the sign.
  if (present(nz_pad)) str=str(DI4P-nz_pad:DI4P-1) ! Leaving out the extra zeros padding
  return
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction strz_I4P

  elemental function strz_I2P(nz_pad,n) result(str)
  !---------------------------------------------------------------------------------------------------------------------------------
  !!The function strz_I2P converts integer to string, prefixing with the right number of zeros. This function achieves casting of
  !!integer to string.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  implicit none
  integer(I4P), intent(IN), optional:: nz_pad ! Number of zeros padding.
  integer(I2P), intent(IN)::           n      ! Integer to be converted.
  character(DI2P)::                    str    ! Returned string containing input number plus padding zeros.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  write(str,FI2PZP) n                              ! Casting of n to string.
  str=str(2:)                                      ! Leaving out the sign.
  if (present(nz_pad)) str=str(DI2P-nz_pad:DI2P-1) ! Leaving out the extra zeros padding
  return
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction strz_I2P

  elemental function strz_I1P(nz_pad,n) result(str)
  !---------------------------------------------------------------------------------------------------------------------------------
  !!The function strz_I1P converts integer to string, prefixing with the right number of zeros. This function achieves casting of
  !!integer to string.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  implicit none
  integer(I4P), intent(IN), optional:: nz_pad ! Number of zeros padding.
  integer(I1P), intent(IN)::           n      ! Integer to be converted.
  character(DI1P)::                    str    ! Returned string containing input number plus padding zeros.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  write(str,FI1PZP) n                              ! Casting of n to string.
  str=str(2:)                                      ! Leaving out the sign.
  if (present(nz_pad)) str=str(DI1P-nz_pad:DI1P-1) ! Leaving out the extra zeros padding
  return
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction strz_I1P

#ifdef r16p
  function ctor_R16P(str,knd) result(n)
  !---------------------------------------------------------------------------------------------------------------------------------
  !!The function ctor_R16P converts string to real. This function achieves casting of string to real.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  implicit none
  character(*), intent(IN):: str   ! String containing input number.
  real(R16P),   intent(IN):: knd   ! Number kind.
  real(R16P)::               n     ! Number returned.
  integer(I4P)::             err   ! Error traping flag: 0 no errors, >0 error occours.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  read(str,*,iostat=err) n ! Casting of str to n.
  if (err/=0) then
    write(stderr,'(A)')             'Conversion of string "'//str//'" to real failed'
    write(stderr,'(A,'//FR16P//')') 'Kind parameter ',knd
    write(stderr,'(A)')             'Function used "ctor_R16P"'
  endif
  return
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction ctor_R16P
#endif

  function ctor_R8P(str,knd) result(n)
  !---------------------------------------------------------------------------------------------------------------------------------
  !!The function ctor_R8P converts string to real. This function achieves casting of string to real.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  implicit none
  character(*), intent(IN):: str   ! String containing input number.
  real(R8P),    intent(IN):: knd   ! Number kind.
  real(R8P)::                n     ! Number returned.
  integer(I4P)::             err   ! Error traping flag: 0 no errors, >0 error occours.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  read(str,*,iostat=err) n ! Casting of str to n.
  if (err/=0) then
    write(stderr,'(A)')            'Conversion of string "'//str//'" to real failed'
    write(stderr,'(A,'//FR8P//')') 'Kind parameter ',knd
    write(stderr,'(A)')            'Function used "ctor_R8P"'
  endif
  return
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction ctor_R8P

  function ctor_R4P(str,knd) result(n)
  !---------------------------------------------------------------------------------------------------------------------------------
  !!The function ctor_R4P converts string to real. This function achieves casting of string to real.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  implicit none
  character(*), intent(IN):: str   ! String containing input number.
  real(R4P),    intent(IN):: knd   ! Number kind.
  real(R4P)::                n     ! Number returned.
  integer(I4P)::             err   ! Error traping flag: 0 no errors, >0 error occours.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  read(str,*,iostat=err) n ! Casting of str to n.
  if (err/=0) then
    write(stderr,'(A)')            'Conversion of string "'//str//'" to real failed'
    write(stderr,'(A,'//FR4P//')') 'Kind parameter ',knd
    write(stderr,'(A)')            'Function used "ctor_R4P"'
  endif
  return
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction ctor_R4P

  function ctoi_I8P(str,knd) result(n)
  !---------------------------------------------------------------------------------------------------------------------------------
  !!The function ctoi_I8P converts string to integer. This function achieves casting of string to integer.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  implicit none
  character(*), intent(IN):: str   ! String containing input number.
  integer(I8P), intent(IN):: knd   ! Number kind.
  integer(I8P)::             n     ! Number returned.
  integer(I4P)::             err   ! Error traping flag: 0 no errors, >0 error occours.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  read(str,*,iostat=err) n ! Casting of str to n.
  if (err/=0) then
    write(stderr,'(A)')            'Conversion of string "'//str//'" to integer failed'
    write(stderr,'(A,'//FI8P//')') 'Kind parameter ',knd
    write(stderr,'(A)')            'Function used "ctoi_I8P"'
  endif
  return
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction ctoi_I8P

  function ctoi_I4P(str,knd) result(n)
  !---------------------------------------------------------------------------------------------------------------------------------
  !!The function ctoi_I4P converts string to integer. This function achieves casting of string to integer.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  implicit none
  character(*), intent(IN):: str   ! String containing input number.
  integer(I4P), intent(IN):: knd   ! Number kind.
  integer(I4P)::             n     ! Number returned.
  integer(I4P)::             err   ! Error traping flag: 0 no errors, >0 error occours.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  read(str,*,iostat=err) n ! Casting of str to n.
  if (err/=0) then
    write(stderr,'(A)')            'Conversion of string "'//str//'" to integer failed'
    write(stderr,'(A,'//FI4P//')') 'Kind parameter ',knd
    write(stderr,'(A)')            'Function used "ctoi_I4P"'
  endif
  return
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction ctoi_I4P

  function ctoi_I2P(str,knd) result(n)
  !---------------------------------------------------------------------------------------------------------------------------------
  !!The function ctoi_I2P converts string to integer. This function achieves casting of string to integer.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  implicit none
  character(*), intent(IN):: str   ! String containing input number.
  integer(I2P), intent(IN):: knd   ! Number kind.
  integer(I2P)::             n     ! Number returned.
  integer(I4P)::             err   ! Error traping flag: 0 no errors, >0 error occours.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  read(str,*,iostat=err) n ! Casting of str to n.
  if (err/=0) then
    write(stderr,'(A)')            'Conversion of string "'//str//'" to integer failed'
    write(stderr,'(A,'//FI2P//')') 'Kind parameter ',knd
    write(stderr,'(A)')            'Function used "ctoi_I2P"'
  endif
  return
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction ctoi_I2P

  function ctoi_I1P(str,knd) result(n)
  !---------------------------------------------------------------------------------------------------------------------------------
  !!The function ctoi_I1P converts string to integer. This function achieves casting of string to integer.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  implicit none
  character(*), intent(IN):: str   ! String containing input number.
  integer(I1P), intent(IN):: knd   ! Number kind.
  integer(I1P)::             n     ! Number returned.
  integer(I4P)::             err   ! Error traping flag: 0 no errors, >0 error occours.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  read(str,*,iostat=err) n ! Casting of str to n.
  if (err/=0) then
    write(stderr,'(A)')            'Conversion of string "'//str//'" to integer failed'
    write(stderr,'(A,'//FI1P//')') 'Kind parameter ',knd
    write(stderr,'(A)')            'Function used "ctoi_I1P"'
  endif
  return
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction ctoi_I1P

  !>Subroutine for printing to the standard output the kind definition of reals and integers and the utility variables.
  subroutine IR_Print()
  !---------------------------------------------------------------------------------------------------------------------------------
  implicit none
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  ! checking the bit ordering architecture
  call check_endian()
  if (endian==endianL) then
    write(stdout,'(A)')                                  ' This architecture has Little Endian bit ordering'
  else
    write(stdout,'(A)')                                  ' This architecture has Big Endian bit ordering'
  endif
  write(stdout,'(A)')                                    ' Reals kind precision definition'
#ifdef r16p
  write(stdout,'(A,I2,A,I2)')                            ' R16P Kind "',R16P,'" | FR16P format "'//FR16P//'" | DR16P chars ',DR16P
#endif
  write(stdout,'(A,I2,A,I2)')                            ' R8P  Kind "',R8P, '" | FR8P  format "'//FR8P// '" | DR8P  chars ',DR8P
  write(stdout,'(A,I2,A,I2)')                            ' R4P  Kind "',R4P, '" | FR4P  format "'//FR4P//'"  | DR4P  chars ',DR4P
  write(stdout,'(A)')                                    ' Integers kind precision definition'
  write(stdout,'(A,I2,A,I2)')                            ' I8P Kind "',I8P,'" | FI8P format "'//FI8P// '" | DI8P chars ',DI8P
  write(stdout,'(A,I2,A,I2)')                            ' I4P Kind "',I4P,'" | FI4P format "'//FI4P// '" | DI4P chars ',DI4P
  write(stdout,'(A,I2,A,I2)')                            ' I2P Kind "',I2P,'" | FI2P format "'//FI2P//'"  | DI2P chars ',DI2P
  write(stdout,'(A,I2,A,I2)')                            ' I1P Kind "',I1P,'" | FI1P format "'//FI1P//'"  | DI1P chars ',DI1P
  write(stdout,'(A)')                                    ' Reals minimum and maximum values'
#ifdef r16p
  write(stdout,'(A,'//FR16P//',A,'       //FR16P//',A)') ' MinR16P "',MinR16P,   '" | MaxR16P "',MaxR16P,'"'
#endif
  write(stdout,'(A,'//FR8P// ',A,19X,A,' //FR8P// ',A)') ' MinR8P  "', MinR8P,'"',' | MaxR8P  "',MaxR8P,'"'
  write(stdout,'(A,'//FR4P// ',A,29X,A,' //FR4P// ',A)') ' MinR4P  "', MinR4P,'"',' | MaxR4P  "',MaxR4P,'"'
  write(stdout,'(A)')                                    ' Integers minimum and maximum values'
  write(stdout,'(A,'//FI8P// ',A,'       //FI8P// ',A)') ' MinI8P  "', MinI8P,   '" | MaxI8P  "',MaxI8P,'"'
  write(stdout,'(A,'//FI4P// ',A,9X,A,'  //FI4P// ',A)') ' MinI4P  "', MinI4P,'"',' | MaxI4P  "',MaxI4P,'"'
  write(stdout,'(A,'//FI2P// ',A,14X,A,' //FI2P// ',A)') ' MinI2P  "', MinI2P,'"',' | MaxI2P  "',MaxI2P,'"'
  write(stdout,'(A,'//FI1P// ',A,16X,A,' //FI1P// ',A)') ' MinI1P  "', MinI1P,'"',' | MaxI1P  "',MaxI1P,'"'
  write(stdout,'(A)')                                    ' Machine precions'
#ifdef r16p
  write(stdout,'(A,'//FR16P//')')                        ' ZeroR16 "',ZeroR16
#endif
  write(stdout,'(A,'//FR8P// ')')                        ' ZeroR8  "',ZeroR8
  write(stdout,'(A,'//FR4P// ')')                        ' ZeroR4  "',ZeroR4
  !---------------------------------------------------------------------------------------------------------------------------------
  endsubroutine IR_Print
endmodule IR_Precision
