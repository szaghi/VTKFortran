module IR_Precision
!-----------------------------------------------------------------------------------------------------------------------------------
!!The module IR_Precision makes available some portable kind-parameters and some usefull procedures to deal with them. It also
!!provides variables that contains the smallest real evaluable by the running calculator, Zero, Zero16, Zero8 and Zero4.
!!Finally the module provides a function to convert a string to number, 'cton', that accepts both reals and integers, and 2 other
!!functions that convert a number to string: 'str' that convert real and integer to string and 'strz' that convert only integer to
!!string prefixing the string with zeros.
!-----------------------------------------------------------------------------------------------------------------------------------

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
!!Bit ordering of the running architecture:
integer, parameter:: endianL = 1
integer, parameter:: endianB = 0
integer::            endian  = endianL
!!The following are the portable kind parameters available.
!!
!!Real precision definitions:
!!
#ifdef r16p
integer, parameter:: R16P = selected_real_kind(33,4931) ! 33  digits, range $[10^{-4931}, 10^{+4931} - 1]$
#endif
integer, parameter:: R8P  = selected_real_kind(15,307)  ! 15  digits, range $[10^{-307} , 10^{+307}  - 1]$
integer, parameter:: R4P  = selected_real_kind(6,37)    ! 6   digits, range $[10^{-37}  , 10^{+37}   - 1]$
integer, parameter:: R_P  = R8P                         ! default real precision
!!
!!Integer precision definitions:
!!
integer, parameter:: I8P  = selected_int_kind(18) ! range $[-2^{63},+2^{63} - 1]$, 19 number of digits plus sign
integer, parameter:: I4P  = selected_int_kind(9)  ! range $[-2^{31},+2^{31} - 1]$, 10 number of digits plus sign
integer, parameter:: I2P  = selected_int_kind(4)  ! range $[-2^{15},+2^{15} - 1]$, 5  number of digits plus sign
integer, parameter:: I1P  = selected_int_kind(2)  ! range $[-2^{7} ,+2^{7}  - 1]$, 3  number of digits plus sign
integer, parameter:: I_P  = I4P                   ! default integer precision
!!
!!Besides the kind parameters there are also the format parameters useful for writing in a well-ascii-format numeric variables.
!!
!!Real output formats:
!!
#ifdef r16p
character(10), parameter:: FR16P = '(E42.33E4)' ! R16P  output format
#endif
character(10), parameter:: FR8P  = '(E23.15E3)' ! R8P   output format
character(9),  parameter:: FR4P  = '(E13.6E2)'  ! R4P   output format
character(10), parameter:: FR_P  = FR8P         ! R\_P  output format
!!
!!Real number of digits of output formats:
#ifdef r16p
integer, parameter:: DR16P = 42   ! R16P output format digits
#endif
integer, parameter:: DR8P  = 23   ! R8P  output format digits
integer, parameter:: DR4P  = 13   ! R4P  output format digits
integer, parameter:: DR_P  = DR8P ! R\_P  output format digits
!!
!!Real min and max values:
#ifdef r16p
real(R16P), parameter:: MinR16P = -huge(1._R16P), MaxR16P = huge(1._R16P) ! R16P min and max values
#endif
real(R8P),  parameter:: MinR8P  = -huge(1._R8P ), MaxR8P  = huge(1._R8P ) ! R8P  min and max values
real(R4P),  parameter:: MinR4P  = -huge(1._R4P ), MaxR4P  = huge(1._R4P ) ! R4P  min and max values
real(R_P),  parameter:: MinR_P  = MinR8P,         MaxR_P  = MaxR8P        ! R\_P  min and max values
!!smallest values:
#ifdef r16p
real(R16P), parameter:: smallR16P = tiny(1._R16P) ! R16P smallest value
#endif
real(R8P),  parameter:: smallR8P  = tiny(1._R8P ) ! R8P  smallest value
real(R4P),  parameter:: smallR4P  = tiny(1._R4P ) ! R4P  smallest value
real(R_P),  parameter:: smallR_P  = smallR8P      ! R_P  smallest value
!!
!!Integer output formats:
!!
character(5), parameter:: FI8P   = '(I20)'    ! I8P output format
character(8), parameter:: FI8PZP = '(I20.19)' ! I8P output format with zero prefixing
character(5), parameter:: FI4P   = '(I11)'    ! I4P output format
character(8), parameter:: FI4PZP = '(I11.10)' ! I4P output format with zero prefixing
character(4), parameter:: FI2P   = '(I6)'     ! I2P output format
character(6), parameter:: FI2PZP = '(I6.5)'   ! I2P output format with zero prefixing
character(4), parameter:: FI1P   = '(I4)'     ! I1P output format
character(6), parameter:: FI1PZP = '(I4.3)'   ! I1P output format with zero prefixing
character(5), parameter:: FI_P   = FI4P       ! I\_P output format
character(8), parameter:: FI_PZP = FI4PZP     ! I\_P output format with zero prefixing
!!
!!Integer number of digits of output formats:
integer, parameter:: DI8P = 20   ! I8P output format digits
integer, parameter:: DI4P = 11   ! I4P output format digits
integer, parameter:: DI2P = 6    ! I2P output format digits
integer, parameter:: DI1P = 4    ! I1P output format digits
integer, parameter:: DI_P = DI4P ! I\_P output format digits
!!
!!Integer min and max values:
integer(I8P), parameter:: MinI8P = -huge(1_I8P)-1_I8P, MaxI8P = huge(1_I8P) ! I8P min and max values
integer(I4P), parameter:: MinI4P = -huge(1_I4P)-1_I4P, MaxI4P = huge(1_I4P) ! I4P min and max values
integer(I2P), parameter:: MinI2P = -huge(1_I2P)-1_I2P, MaxI2P = huge(1_I2P) ! I2P min and max values
integer(I2P), parameter:: MinI1P = -huge(1_I1P)-1_I1P, MaxI1P = huge(1_I1P) ! I1P min and max values
integer(I_P), parameter:: MinI_P = MinI4P,             MaxI_P = MaxI4P      ! I\_P min and max values
!!
!!IR\_Precision module provides some global variables in order to store the smallest real evaluable by the running calculator.
!!
#ifdef r16p
real(R16P), parameter:: ZeroR16 = nearest(1._R16P,1._R16P) - nearest(1._R16P,-1._R16P)
#endif
#ifdef pgf95
real(R8P),  parameter:: ZeroR8  = 0._R8P
real(R4P),  parameter:: ZeroR4  = 0._R4P
#else
real(R8P),  parameter:: ZeroR8  = nearest(1._R8P,1._R8P) - nearest(1._R8P,-1._R8P)
real(R4P),  parameter:: ZeroR4  = nearest(1._R4P,1._R4P) - nearest(1._R4P,-1._R4P)
#endif
real(R_P),  parameter:: Zero    = ZeroR8
!-----------------------------------------------------------------------------------------------------------------------------------

!-----------------------------------------------------------------------------------------------------------------------------------
!! str overloading
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
!! strz overloading
interface strz
  module procedure strz_I8P,  &
                   strz_I4P,  &
                   strz_I2P,  &
                   strz_I1P
endinterface
!! ctoi overloading
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
  subroutine check_endian()
  !---------------------------------------------------------------------------------------------------------------------------------
  !!The subroutine check_endian checks the type bit ordering (big or little endian) of the running architecture. The result is
  !! stored into the endian global variable.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  implicit none
  integer(I1P):: int1(1:4) ! One byte integer array for casting 4 bytes integer.
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
  !!The function str\_R16P converts real to string. This function achieves casting of real to string.
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
  !!The function str\_R8P converts real to string. This function achieves casting of real to string.
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
  !!The function str\_R4P converts real to string. This function achieves casting of real to string.
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
  !!The function str\_I8P converts integer to string. This function achieves casting of integer to string.
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
  !!The function str\_I4P converts integer to string. This function achieves casting of integer to string.
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
  !!The function str\_I2P converts integer to string. This function achieves casting of integer to string.
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
  !!The function str\_I1P converts integer to string. This function achieves casting of integer to string.
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
  !!The function strz\_I8P converts integer to string, prefixing with the right number of zeros. This function achieves casting of
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
  !!The function strz\_I4P converts integer to string, prefixing with the right number of zeros. This function achieves casting of
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
  !!The function strz\_I2P converts integer to string, prefixing with the right number of zeros. This function achieves casting of
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
  !!The function strz\_I1P converts integer to string, prefixing with the right number of zeros. This function achieves casting of
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
  !!The function ctor\_R16P converts string to real. This function achieves casting of string to real.
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
  !!The function ctor\_R8P converts string to real. This function achieves casting of string to real.
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
  !!The function ctor\_R4P converts string to real. This function achieves casting of string to real.
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
  !!The function ctoi\_I8P converts string to integer. This function achieves casting of string to integer.
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
  !!The function ctoi\_I4P converts string to integer. This function achieves casting of string to integer.
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
  !!The function ctoi\_I2P converts string to integer. This function achieves casting of string to integer.
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
  !!The function ctoi\_I1P converts string to integer. This function achieves casting of string to integer.
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

  subroutine IR_Print()
  !---------------------------------------------------------------------------------------------------------------------------------
  !!The subroutine IR\_Print print to the standard output the kind definition of reals and integers and the utility variables.
  !---------------------------------------------------------------------------------------------------------------------------------

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
