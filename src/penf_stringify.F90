!< PENF string-to-number (and viceversa) facility.

module penf_stringify
!< PENF string-to-number (and viceversa) facility.
use, intrinsic :: iso_fortran_env, only : stderr=>error_unit
use penf_b_size
use penf_global_parameters_variables

implicit none
private
save
public :: str_ascii, str_ucs4
public :: str, strz, cton
public :: bstr, bcton

interface str_ascii
   !< Convert string of any kind to ASCII string.
   module procedure str_ascii_default
#if defined _ASCII_SUPPORTED && defined _ASCII_NEQ_DEFAULT
   module procedure str_ascii_ascii
#endif
#ifdef _UCS4_SUPPORTED
   module procedure str_ascii_ucs4
#endif
endinterface

interface str_ucs4
   !< Convert string of any kind to UCS4 string.
   module procedure str_ucs4_default
#if defined _ASCII_SUPPORTED && defined _ASCII_NEQ_DEFAULT
   module procedure str_ucs4_ascii
#endif
#ifdef _UCS4_SUPPORTED
   module procedure str_ucs4_ucs4
#endif
endinterface

interface str
  !< Convert number (real and integer) to string (number to string type casting).
  module procedure                       &
                   strf_R16P,str_R16P,   &
                   strf_R8P ,str_R8P,    &
                   strf_R4P ,str_R4P,    &
                   strf_I8P ,str_I8P,    &
                   strf_I4P ,str_I4P,    &
                   strf_I2P ,str_I2P,    &
                   strf_I1P ,str_I1P,    &
                             str_bol,    &
                             str_a_R16P, &
                             str_a_R8P,  &
                             str_a_R4P,  &
                             str_a_I8P,  &
                             str_a_I4P,  &
                             str_a_I2P,  &
                             str_a_I1P
endinterface

interface strz
  !< Convert integer, to string, prefixing with the right number of zeros (integer to string type casting with zero padding).
  module procedure strz_I8P, strz_I4P, strz_I2P, strz_I1P
endinterface

interface cton
  !< Convert string to number (real and integer, string to number type casting).
  module procedure            &
                   ctor_R16P, &
                   ctor_R8P,  &
                   ctor_R4P,  &
                   ctoi_I8P,  &
                   ctoi_I4P,  &
                   ctoi_I2P,  &
                   ctoi_I1P
endinterface

interface bstr
  !< Convert number (real and integer) to bit-string (number to bit-string type casting).
  module procedure            &
                   bstr_R16P, &
                   bstr_R8P,  &
                   bstr_R4P,  &
                   bstr_I8P,  &
                   bstr_I4P,  &
                   bstr_I2P,  &
                   bstr_I1P
endinterface

interface bcton
  !< Convert bit-string to number (real and integer, bit-string to number type casting).
  module procedure             &
                   bctor_R16P, &
                   bctor_R8P,  &
                   bctor_R4P,  &
                   bctoi_I8P,  &
                   bctoi_I4P,  &
                   bctoi_I2P,  &
                   bctoi_I1P
endinterface

contains
   pure function str_ascii_default(input) result(output)
   !< Convert string of default kind to ASCII string.
   !<
   !<```fortran
   !< use penf
   !< character(len=:, kind=ASCII), allocatable :: string
   !< string = str_ascii('I was DEFAULT kind, but now I am ASCII')
   !< print "(A)", string
   !<```
   !=> I was DEFAULT kind, but now I am ASCII <<<
   character(len=*), intent(in)              :: input  !< Input string of default kind.
   character(len=:, kind=ASCII), allocatable :: output !< Output string of ASCII kind.

   output = input
   endfunction str_ascii_default

   pure function str_ascii_ascii(input) result(output)
   !< Convert string of ASCII kind to ASCII string, just for convenience in sanitize strings.
   !<
   !<```fortran
   !< use penf
   !< character(len=:, kind=ASCII), allocatable :: string
   !< string = str_ascii('I was ASCII kind and I am still ASCII')
   !< print "(A)", string
   !<```
   !=> I was ASCII kind and I am still ASCII <<<
   character(len=*, kind=ASCII), intent(in)  :: input  !< Input string of ASCII kind.
   character(len=:, kind=ASCII), allocatable :: output !< Output string of ASCII kind.

   output = input
   endfunction str_ascii_ascii

   pure function str_ascii_ucs4(input) result(output)
   !< Convert string of UCS4 kind to ASCII string.
   !<
   !<```fortran
   !< use penf
   !< character(len=:, kind=ASCII), allocatable :: string
   !< string = str_ascii(UCS4_'I was UCS4 kind, but now I am ASCII')
   !< print "(A)", string
   !<```
   !=> I was UCS4 kind, but now I am ASCII <<<
   character(len=*, kind=UCS4), intent(in)   :: input  !< Input string of UCS4 kind.
   character(len=:, kind=ASCII), allocatable :: output !< Output string of ASCII kind.

   output = input
   endfunction str_ascii_ucs4

   pure function str_ucs4_default(input) result(output)
   !< Convert string of default kind to UCS4 string.
   !<
   !<```fortran
   !< use penf
   !< character(len=:, kind=UCS4), allocatable :: string
   !< string = str_ascii('I was DEFAULT kind, but now I am UCS4')
   !< print "(A)", string
   !<```
   !=> I was DEFAULT kind, but now I am UCS4 <<<
   character(len=*), intent(in)             :: input  !< Input string of default kind.
   character(len=:, kind=UCS4), allocatable :: output !< Output string of UCS4 kind.

   output = input
   endfunction str_ucs4_default

   pure function str_ucs4_ascii(input) result(output)
   !< Convert string of ASCII kind to UCS4 string.
   !<
   !<```fortran
   !< use penf
   !< character(len=:, kind=UCS4), allocatable :: string
   !< string = str_ascii(ASCII_'I was ASCII kind, but now I am UCS4')
   !< print "(A)", string
   !<```
   !=> I was ASCII kind, but now I am UCS4 <<<
   character(len=*, kind=ASCII), intent(in) :: input  !< Input string of ASCII kind.
   character(len=:, kind=UCS4), allocatable :: output !< Output string of UCS4 kind.

   output = input
   endfunction str_ucs4_ascii

   pure function str_ucs4_ucs4(input) result(output)
   !< Convert string of UCS4 kind to UCS4 string, just for convenience in sanitize strings.
   !<
   !<```fortran
   !< use penf
   !< character(len=:, kind=UCS4), allocatable :: string
   !< string = str_ascii(UCS4_'I was UCS4 kind and I am still UCS4')
   !< print "(A)", string
   !<```
   !=> I was UCS4 kind and I am still UCS4 <<<
   character(len=*, kind=UCS4), intent(in)  :: input  !< Input string of UCS4 kind.
   character(len=:, kind=UCS4), allocatable :: output !< Output string of UCS4 kind.

   output = input
   endfunction str_ucs4_ucs4

   elemental function strf_R16P(fm, n) result(str)
   !< Convert real to string.
   !<
   !<```fortran
   !< use penf
   !< print "(A)", str(fm=FR16P, n=1._R16P)
   !<```
   !=> 0.100000000000000000000000000000000E+0001 <<<
   character(*), intent(in) :: fm  !< Format different from the standard for the kind.
   real(R16P),   intent(in) :: n   !< Real to be converted.
   character(DR16P)         :: str !< Returned string containing input number.

   write(str, trim(fm)) n
   endfunction strf_R16P

   elemental function strf_R8P(fm, n) result(str)
   !< Convert real to string.
   !<
   !<```fortran
   !< use penf
   !< print "(A)", str(fm=FR8P, n=1._R8P)
   !<```
   !=> 0.100000000000000E+001 <<<
   character(*), intent(in) :: fm  !< Format different from the standard for the kind.
   real(R8P),    intent(in) :: n   !< Real to be converted.
   character(DR8P)          :: str !< Returned string containing input number.

   write(str, trim(fm)) n
   endfunction strf_R8P

   elemental function strf_R4P(fm, n) result(str)
   !< Convert real to string.
   !<
   !<```fortran
   !< use penf
   !< print "(A)", str(fm=FR4P, n=1._R4P)
   !<```
   !=> 0.100000E+01 <<<
   character(*), intent(in) :: fm  !< Format different from the standard for the kind.
   real(R4P),    intent(in) :: n   !< Real to be converted.
   character(DR4P)          :: str !< Returned string containing input number.

   write(str, trim(fm)) n
   endfunction strf_R4P

   elemental function strf_I8P(fm, n) result(str)
   !< Convert integer to string.
   !<
   !<```fortran
   !< use penf
   !< print "(A)", str(fm=FI8P, n=1_I8P)
   !<```
   !=> 1 <<<
   character(*), intent(in) :: fm  !< Format different from the standard for the kind.
   integer(I8P), intent(in) :: n   !< Integer to be converted.
   character(DI8P)          :: str !< Returned string containing input number.

   write(str, trim(fm)) n
   endfunction strf_I8P

   elemental function strf_I4P(fm, n) result(str)
   !< Convert integer to string.
   !<
   !<```fortran
   !< use penf
   !< print "(A)", str(fm=FI4P, n=1_I4P)
   !<```
   !=> 1 <<<
   character(*), intent(in) :: fm  !< Format different from the standard for the kind.
   integer(I4P), intent(in) :: n   !< Integer to be converted.
   character(DI4P)          :: str !< Returned string containing input number.

   write(str, trim(fm)) n
   endfunction strf_I4P

   elemental function strf_I2P(fm, n) result(str)
   !< Convert integer to string.
   !<
   !<```fortran
   !< use penf
   !< print "(A)", str(fm=FI2P, n=1_I2P)
   !<```
   !=> 1 <<<
   character(*), intent(in) :: fm  !< Format different from the standard for the kind.
   integer(I2P), intent(in) :: n   !< Integer to be converted.
   character(DI2P)          :: str !< Returned string containing input number.

   write(str, trim(fm)) n
   endfunction strf_I2P

   elemental function strf_I1P(fm, n) result(str)
   !< Convert integer to string.
   !<
   !<```fortran
   !< use penf
   !< print "(A)", str(fm=FI1P, n=1_I1P)
   !<```
   !=> 1 <<<
   character(*), intent(in) :: fm  !< Format different from the standard for the kind.
   integer(I1P), intent(in) :: n   !< Integer to be converted.
   character(DI1P)          :: str !< Returned string containing input number.

   write(str, trim(fm)) n
   endfunction strf_I1P

   elemental function str_R16P(n, no_sign, compact) result(str)
   !< Convert real to string.
   !<
   !<```fortran
   !< use penf
   !< print "(A)", str(n=-1._R16P)
   !<```
   !=> -0.100000000000000000000000000000000E+0001 <<<
   !<
   !<```fortran
   !< use penf
   !< print "(A)", str(n=-1._R16P, no_sign=.true.)
   !<```
   !=> 0.100000000000000000000000000000000E+0001 <<<
   !<
   !<```fortran
   !< use penf
   !< print "(A)", str(n=-1._R16P, compact=.true.)
   !<```
   !=> -0.1E+1 <<<
   real(R16P), intent(in)           :: n       !< Real to be converted.
   logical,    intent(in), optional :: no_sign !< Flag for leaving out the sign.
   logical,    intent(in), optional :: compact !< Flag for *compacting* string encoding.
   character(DR16P)                 :: str     !< Returned string containing input number.

   write(str, FR16P) n               ! Casting of n to string.
   if (n>0._R16P) str(1:1)='+'       ! Prefixing plus if n>0.
   if (present(no_sign)) str=str(2:) ! Leaving out the sign.
   if (present(compact)) then
     if (compact) call compact_real_string(string=str)
   endif
   endfunction str_R16P

   elemental function str_R8P(n, no_sign, compact) result(str)
   !< Convert real to string.
   !<
   !<```fortran
   !< use penf
   !< print "(A)", str(n=-1._R8P)
   !<```
   !=> -0.100000000000000E+001 <<<
   !<
   !<```fortran
   !< use penf
   !< print "(A)", str(n=-1._R8P, no_sign=.true.)
   !<```
   !=> 0.100000000000000E+001 <<<
   !<
   !<```fortran
   !< use penf
   !< print "(A)", str(n=-1._R8P, compact=.true.)
   !<```
   !=> -0.1E+1 <<<
   real(R8P), intent(in)           :: n       !< Real to be converted.
   logical,   intent(in), optional :: no_sign !< Flag for leaving out the sign.
   logical,   intent(in), optional :: compact !< Flag for *compacting* string encoding.
   character(DR8P)                 :: str     !< Returned string containing input number.

   write(str, FR8P) n                ! Casting of n to string.
   if (n>0._R8P) str(1:1)='+'        ! Prefixing plus if n>0.
   if (present(no_sign)) str=str(2:) ! Leaving out the sign.
   if (present(compact)) then
     if (compact) call compact_real_string(string=str)
   endif
   endfunction str_R8P

   elemental function str_R4P(n, no_sign, compact) result(str)
   !< Convert real to string.
   !<
   !<```fortran
   !< use penf
   !< print "(A)", str(n=-1._R4P)
   !<```
   !=> -0.100000E+01 <<<
   !<
   !<```fortran
   !< use penf
   !< print "(A)", str(n=-1._R4P, no_sign=.true.)
   !<```
   !=> 0.100000E+01 <<<
   !<
   !<```fortran
   !< use penf
   !< print "(A)", str(n=-1._R4P, compact=.true.)
   !<```
   !=> -0.1E+1 <<<
   real(R4P), intent(in)           :: n       !< Real to be converted.
   logical,   intent(in), optional :: no_sign !< Flag for leaving out the sign.
   logical,   intent(in), optional :: compact !< Flag for *compacting* string encoding.
   character(DR4P)                 :: str     !< Returned string containing input number.

   write(str, FR4P) n                ! Casting of n to string.
   if (n>0._R4P) str(1:1)='+'        ! Prefixing plus if n>0.
   if (present(no_sign)) str=str(2:) ! Leaving out the sign.
   if (present(compact)) then
     if (compact) call compact_real_string(string=str)
   endif
   endfunction str_R4P

   elemental function str_I8P(n, no_sign) result(str)
   !< Convert integer to string.
   !<
   !<```fortran
   !< use penf
   !< print "(A)", str(n=-1_I8P)
   !<```
   !=> -1 <<<
   !<
   !<```fortran
   !< use penf
   !< print "(A)", str(n=-1_I8P, no_sign=.true.)
   !<```
   !=> 1 <<<
   integer(I8P), intent(in)           :: n       !< Integer to be converted.
   logical,      intent(in), optional :: no_sign !< Flag for leaving out the sign.
   character(DI8P)                    :: str     !< Returned string containing input number plus padding zeros.

   write(str, FI8P) n                ! Casting of n to string.
   str = adjustl(trim(str))          ! Removing white spaces.
   if (n>=0_I8P) str='+'//trim(str)  ! Prefixing plus if n>0.
   if (present(no_sign)) str=str(2:) ! Leaving out the sign.
   endfunction str_I8P

   elemental function str_I4P(n, no_sign) result(str)
   !< Converting integer to string.
   !<
   !<```fortran
   !< use penf
   !< print "(A)", str(n=-1_I4P)
   !<```
   !=> -1 <<<
   !<
   !<```fortran
   !< use penf
   !< print "(A)", str(n=-1_I4P, no_sign=.true.)
   !<```
   !=> 1 <<<
   integer(I4P), intent(in)           :: n       !< Integer to be converted.
   logical,      intent(in), optional :: no_sign !< Flag for leaving out the sign.
   character(DI4P)                    :: str     !< Returned string containing input number plus padding zeros.

   write(str, FI4P) n                ! Casting of n to string.
   str = adjustl(trim(str))          ! Removing white spaces.
   if (n>=0_I4P) str='+'//trim(str)  ! Prefixing plus if n>0.
   if (present(no_sign)) str=str(2:) ! Leaving out the sign.
   endfunction str_I4P

   elemental function str_I2P(n, no_sign) result(str)
   !< Convert integer to string.
   !<
   !<```fortran
   !< use penf
   !< print "(A)", str(n=-1_I2P)
   !<```
   !=> -1 <<<
   !<
   !<```fortran
   !< use penf
   !< print "(A)", str(n=-1_I2P, no_sign=.true.)
   !<```
   !=> 1 <<<
   integer(I2P), intent(in)           :: n       !< Integer to be converted.
   logical,      intent(in), optional :: no_sign !< Flag for leaving out the sign.
   character(DI2P)                    :: str     !< Returned string containing input number plus padding zeros.

   write(str, FI2P) n                ! Casting of n to string.
   str = adjustl(trim(str))          ! Removing white spaces.
   if (n>=0_I2P) str='+'//trim(str)  ! Prefixing plus if n>0.
   if (present(no_sign)) str=str(2:) ! Leaving out the sign.
   endfunction str_I2P

   elemental function str_I1P(n, no_sign) result(str)
   !< Convert integer to string.
   !<
   !<```fortran
   !< use penf
   !< print "(A)", str(n=-1_I1P)
   !<```
   !=> -1 <<<
   !<
   !<```fortran
   !< use penf
   !< print "(A)", str(n=-1_I1P, no_sign=.true.)
   !<```
   !=> 1 <<<
   integer(I1P), intent(in)           :: n       !< Integer to be converted.
   logical,      intent(in), optional :: no_sign !< Flag for leaving out the sign.
   character(DI1P)                    :: str     !< Returned string containing input number plus padding zeros.

   write(str, FI1P) n                ! Casting of n to string.
   str = adjustl(trim(str))          ! Removing white spaces.
   if (n>=0_I1P) str='+'//trim(str)  ! Prefixing plus if n>0.
   if (present(no_sign)) str=str(2:) ! Leaving out the sign.
   endfunction str_I1P

   elemental function str_bol(n) result(str)
   !< Convert logical to string.
   !<
   !<```fortran
   !< use penf
   !< print "(A)", str(n=.true.)
   !<```
   !=> T <<<
   logical, intent(in):: n   !< Logical to be converted.
   character(1)::        str !< Returned string containing input number plus padding zeros.

   write(str, '(L1)') n
   endfunction str_bol

   pure function str_a_R16P(n, no_sign, separator, delimiters, compact) result(str)
   !< Converting real array to string.
   !<
   !<```fortran
   !< use penf
   !< print "(A)", str(n=[1._R16P, -2._R16P])
   !<```
   !=> +0.100000000000000000000000000000000E+0001,-0.200000000000000000000000000000000E+0001 <<<
   !<
   !<```fortran
   !< use penf
   !< print "(A)", str(n=[1._R16P, 2._R16P], no_sign=.true.)
   !<```
   !=> 0.100000000000000000000000000000000E+0001,0.200000000000000000000000000000000E+0001 <<<
   !<
   !<```fortran
   !< use penf
   !< print "(A)", str(n=[1._R16P, -2._R16P], separator='|')
   !<```
   !=> +0.100000000000000000000000000000000E+0001|-0.200000000000000000000000000000000E+0001 <<<
   !<
   !<```fortran
   !< use penf
   !< print "(A)", str(n=[1._R16P, -2._R16P], delimiters=['(', ')'])
   !<```
   !=> (+0.100000000000000000000000000000000E+0001,-0.200000000000000000000000000000000E+0001) <<<
   !<
   !<```fortran
   !< use penf
   !< print "(A)", str(n=[1._R16P, -2._R16P], compact=.true.)
   !<```
   !=> +0.1E+1,-0.2E+1 <<<
   real(R16P),   intent(in)           :: n(:)            !< Real array to be converted.
   logical,      intent(in), optional :: no_sign         !< Flag for leaving out the sign.
   character(1), intent(in), optional :: separator       !< Eventual separator of array values.
   character(*), intent(in), optional :: delimiters(1:2) !< Eventual delimiters of array values.
   logical,      intent(in), optional :: compact         !< Flag for *compacting* string encoding.
   character(len=:), allocatable      :: str             !< Returned string containing input number.
   character(DR16P)                   :: strn            !< String containing of element of input array number.
   character(len=1)                   :: sep             !< Array values separator
   integer                            :: i               !< Counter.

   str = ''
   sep = ','
   if(present(separator)) sep = separator
   do i=1,size(n)
     strn = str_R16P(no_sign=no_sign, compact=compact, n=n(i))
     str = str//sep//trim(strn)
   enddo
   str = trim(str(2:))
   if (present(delimiters)) str = delimiters(1)//str//delimiters(2)
   endfunction str_a_R16P

   pure function str_a_R8P(n, no_sign, separator, delimiters, compact) result(str)
   !< Convert real array to string.
   !<
   !<```fortran
   !< use penf
   !< print "(A)", str(n=[1._R8P, -2._R8P])
   !<```
   !=> +0.100000000000000E+001,-0.200000000000000E+001 <<<
   !<
   !<```fortran
   !< use penf
   !< print "(A)", str(n=[1._R8P, 2._R8P], no_sign=.true.)
   !<```
   !=> 0.100000000000000E+001,0.200000000000000E+001 <<<
   !<
   !<```fortran
   !< use penf
   !< print "(A)", str(n=[1._R8P, -2._R8P], separator='|')
   !<```
   !=> +0.100000000000000E+001|-0.200000000000000E+001 <<<
   !<
   !<```fortran
   !< use penf
   !< print "(A)", str(n=[1._R8P, -2._R8P], delimiters=['(', ')'])
   !<```
   !=> (+0.100000000000000E+001,-0.200000000000000E+001) <<<
   !<
   !<```fortran
   !< use penf
   !< print "(A)", str(n=[1._R8P, -2._R8P], compact=.true.)
   !<```
   !=> +0.1E+1,-0.2E+1 <<<
   real(R8P),    intent(in)           :: n(:)            !< Real array to be converted.
   logical,      intent(in), optional :: no_sign         !< Flag for leaving out the sign.
   character(1), intent(in), optional :: separator       !< Eventual separator of array values.
   character(*), intent(in), optional :: delimiters(1:2) !< Eventual delimiters of array values.
   logical,      intent(in), optional :: compact         !< Flag for *compacting* string encoding.
   character(len=:), allocatable      :: str             !< Returned string containing input number.
   character(DR8P)                    :: strn            !< String containing of element of input array number.
   character(len=1)                   :: sep             !< Array values separator
   integer                            :: i               !< Counter.

   str = ''
   sep = ','
   if(present(separator)) sep = separator
   do i=1,size(n)
     strn = str_R8P(no_sign=no_sign, compact=compact, n=n(i))
     str = str//sep//trim(strn)
   enddo
   str = trim(str(2:))
   if (present(delimiters)) str = delimiters(1)//str//delimiters(2)
   endfunction str_a_R8P

   pure function str_a_R4P(n, no_sign, separator, delimiters, compact) result(str)
   !< Convert real array to string.
   !<
   !<```fortran
   !< use penf
   !< print "(A)", str(n=[1._R4P, -2._R4P])
   !<```
   !=> +0.100000E+01,-0.200000E+01 <<<
   !<
   !<```fortran
   !< use penf
   !< print "(A)", str(n=[1._R4P, 2._R4P], no_sign=.true.)
   !<```
   !=> 0.100000E+01,0.200000E+01 <<<
   !<
   !<```fortran
   !< use penf
   !< print "(A)", str(n=[1._R4P, -2._R4P], separator='|')
   !<```
   !=> +0.100000E+01|-0.200000E+01 <<<
   !<
   !<```fortran
   !< use penf
   !< print "(A)", str(n=[1._R4P, -2._R4P], delimiters=['(', ')'])
   !<```
   !=> (+0.100000E+01,-0.200000E+01) <<<
   !<
   !<```fortran
   !< use penf
   !< print "(A)", str(n=[1._R4P, -2._R4P], compact=.true.)
   !<```
   !=> +0.1E+1,-0.2E+1 <<<
   real(R4P),    intent(in)           :: n(:)            !< Real array to be converted.
   logical,      intent(in), optional :: no_sign         !< Flag for leaving out the sign.
   character(1), intent(in), optional :: separator       !< Eventual separator of array values.
   character(*), intent(in), optional :: delimiters(1:2) !< Eventual delimiters of array values.
   logical,      intent(in), optional :: compact         !< Flag for *compacting* string encoding.
   character(len=:), allocatable      :: str             !< Returned string containing input number.
   character(DR4P)                    :: strn            !< String containing of element of input array number.
   character(len=1)                   :: sep             !< Array values separator
   integer                            :: i               !< Counter.

   str = ''
   sep = ','
   if(present(separator)) sep = separator
   do i=1,size(n)
     strn = str_R4P(no_sign=no_sign, compact=compact, n=n(i))
     str = str//sep//trim(strn)
   enddo
   str = trim(str(2:))
   if (present(delimiters)) str = delimiters(1)//str//delimiters(2)
   endfunction str_a_R4P

   pure function str_a_I8P(n, no_sign, separator, delimiters) result(str)
   !< Convert integer array to string.
   !<
   !<```fortran
   !< use penf
   !< print "(A)", str(n=[1_I8P, -2_I8P])
   !<```
   !=> +1,-2 <<<
   !<
   !<```fortran
   !< use penf
   !< print "(A)", str(n=[1_I8P, 2_I8P], no_sign=.true.)
   !<```
   !=> 1,2 <<<
   !<
   !<```fortran
   !< use penf
   !< print "(A)", str(n=[1_I8P, -2_I8P], separator='|')
   !<```
   !=> +1|-2 <<<
   !<
   !<```fortran
   !< use penf
   !< print "(A)", str(n=[1_I8P, -2_I8P], delimiters=['(', ')'])
   !<```
   !=> (+1,-2) <<<
   integer(I8P), intent(in)           :: n(:)            !< Integer array to be converted.
   logical,      intent(in), optional :: no_sign         !< Flag for leaving out the sign.
   character(1), intent(in), optional :: separator       !< Eventual separator of array values.
   character(*), intent(in), optional :: delimiters(1:2) !< Eventual delimiters of array values.
   character(len=:), allocatable      :: str             !< Returned string containing input number.
   character(DI8P)                    :: strn            !< String containing of element of input array number.
   character(len=1)                   :: sep             !< Array values separator
   integer                            :: i               !< Counter.

   str = ''
   sep = ','
   if(present(separator)) sep = separator
   if (present(no_sign)) then
     do i=1,size(n)
       strn = str_I8P(no_sign=no_sign, n=n(i))
       str = str//sep//trim(strn)
     enddo
   else
     do i=1,size(n)
       strn = str_I8P(n=n(i))
       str = str//sep//trim(strn)
     enddo
   endif
   str = trim(str(2:))
   if (present(delimiters)) str = delimiters(1)//str//delimiters(2)
   endfunction str_a_I8P

   pure function str_a_I4P(n, no_sign, separator, delimiters) result(str)
   !< Convert integer array to string.
   !<
   !<```fortran
   !< use penf
   !< print "(A)", str(n=[1_I4P, -2_I4P])
   !<```
   !=> +1,-2 <<<
   !<
   !<```fortran
   !< use penf
   !< print "(A)", str(n=[1_I4P, 2_I4P], no_sign=.true.)
   !<```
   !=> 1,2 <<<
   !<
   !<```fortran
   !< use penf
   !< print "(A)", str(n=[1_I4P, -2_I4P], separator='|')
   !<```
   !=> +1|-2 <<<
   !<
   !<```fortran
   !< use penf
   !< print "(A)", str(n=[1_I4P, -2_I4P], delimiters=['(', ')'])
   !<```
   !=> (+1,-2) <<<
   integer(I4P), intent(in)           :: n(:)            !< Integer array to be converted.
   logical,      intent(in), optional :: no_sign         !< Flag for leaving out the sign.
   character(1), intent(in), optional :: separator       !< Eventual separator of array values.
   character(*), intent(in), optional :: delimiters(1:2) !< Eventual delimiters of array values.
   character(len=:), allocatable      :: str             !< Returned string containing input number.
   character(DI4P)                    :: strn            !< String containing of element of input array number.
   character(len=1)                   :: sep             !< Array values separator
   integer                            :: i               !< Counter.

   str = ''
   sep = ','
   if(present(separator)) sep = separator
   if (present(no_sign)) then
     do i=1,size(n)
       strn = str_I4P(no_sign=no_sign, n=n(i))
       str = str//sep//trim(strn)
     enddo
   else
     do i=1,size(n)
       strn = str_I4P(n=n(i))
       str = str//sep//trim(strn)
     enddo
   endif
   str = trim(str(2:))
   if (present(delimiters)) str = delimiters(1)//str//delimiters(2)
   endfunction str_a_I4P

   pure function str_a_I2P(n, no_sign, separator, delimiters) result(str)
   !< Convert integer array to string.
   !<
   !<```fortran
   !< use penf
   !< print "(A)", str(n=[1_I2P, -2_I2P])
   !<```
   !=> +1,-2 <<<
   !<
   !<```fortran
   !< use penf
   !< print "(A)", str(n=[1_I2P, 2_I2P], no_sign=.true.)
   !<```
   !=> 1,2 <<<
   !<
   !<```fortran
   !< use penf
   !< print "(A)", str(n=[1_I2P, -2_I2P], separator='|')
   !<```
   !=> +1|-2 <<<
   !<
   !<```fortran
   !< use penf
   !< print "(A)", str(n=[1_I2P, -2_I2P], delimiters=['(', ')'])
   !<```
   !=> (+1,-2) <<<
   integer(I2P), intent(in)           :: n(:)            !< Integer array to be converted.
   logical,      intent(in), optional :: no_sign         !< Flag for leaving out the sign.
   character(1), intent(in), optional :: separator       !< Eventual separator of array values.
   character(*), intent(in), optional :: delimiters(1:2) !< Eventual delimiters of array values.
   character(len=:), allocatable      :: str             !< Returned string containing input number.
   character(DI2P)                    :: strn            !< String containing of element of input array number.
   character(len=1)                   :: sep             !< Array values separator
   integer                            :: i               !< Counter.

   str = ''
   sep = ','
   if(present(separator)) sep = separator
   if (present(no_sign)) then
     do i=1,size(n)
       strn = str_I2P(no_sign=no_sign, n=n(i))
       str = str//sep//trim(strn)
     enddo
   else
     do i=1,size(n)
       strn = str_I2P(n=n(i))
       str = str//sep//trim(strn)
     enddo
   endif
   str = trim(str(2:))
   if (present(delimiters)) str = delimiters(1)//str//delimiters(2)
   endfunction str_a_I2P

   pure function str_a_I1P(n, no_sign, separator, delimiters) result(str)
   !< Convert integer array to string.
   !<
   !<```fortran
   !< use penf
   !< print "(A)", str(n=[1_I1P, -2_I1P])
   !<```
   !=> +1,-2 <<<
   !<
   !<```fortran
   !< use penf
   !< print "(A)", str(n=[1_I1P, 2_I1P], no_sign=.true.)
   !<```
   !=> 1,2 <<<
   !<
   !<```fortran
   !< use penf
   !< print "(A)", str(n=[1_I1P, -2_I1P], separator='|')
   !<```
   !=> +1|-2 <<<
   !<
   !<```fortran
   !< use penf
   !< print "(A)", str(n=[1_I1P, -2_I1P], delimiters=['(', ')'])
   !<```
   !=> (+1,-2) <<<
   integer(I1P), intent(in)           :: n(:)            !< Integer array to be converted.
   logical,      intent(in), optional :: no_sign         !< Flag for leaving out the sign.
   character(1), intent(in), optional :: separator       !< Eventual separator of array values.
   character(*), intent(in), optional :: delimiters(1:2) !< Eventual delimiters of array values.
   character(len=:), allocatable      :: str             !< Returned string containing input number.
   character(DI1P)                    :: strn            !< String containing of element of input array number.
   character(len=1)                   :: sep             !< Array values separator
   integer                            :: i               !< Counter.

   str = ''
   sep = ','
   if(present(separator)) sep = separator
   if (present(no_sign)) then
     do i=1,size(n)
       strn = str_I1P(no_sign=no_sign, n=n(i))
       str = str//sep//trim(strn)
     enddo
   else
     do i=1,size(n)
       strn = str_I1P(n=n(i))
       str = str//sep//trim(strn)
     enddo
   endif
   str = trim(str(2:))
   if (present(delimiters)) str = delimiters(1)//str//delimiters(2)
   endfunction str_a_I1P

   pure subroutine compact_real_string(string)
   !< author: Izaak Beekman
   !< date: 02/24/2015
   !<
   !< Compact a string representing a real number, so that the same value is displayed with fewer characters.
   !<
   !< @note No need to add doctest: this is tested by a lot of doctests of other TBPs.
   character(len=*),intent(inout) :: string      !< string representation of a real number.
   character(len=len(string))     :: significand !< Significand characters.
   character(len=len(string))     :: expnt       !< Exponent characters.
   character(len=2)               :: separator   !< Separator characters.
   integer(I4P)                   :: exp_start   !< Start position of exponent.
   integer(I4P)                   :: decimal_pos !< Decimal positions.
   integer(I4P)                   :: sig_trim    !< Signature trim.
   integer(I4P)                   :: exp_trim    !< Exponent trim.
   integer(I4P)                   :: i           !< counter

   string = adjustl(string)
   exp_start = scan(string, 'eEdD')
   if (exp_start == 0) exp_start = scan(string, '-+', back=.true.)
   decimal_pos = scan(string, '.')
   if (exp_start /= 0) separator = string(exp_start:exp_start)
   if ( exp_start < decimal_pos ) then ! possibly signed, exponent-less float
     significand = string
     sig_trim = len(trim(significand))
     do i = len(trim(significand)), decimal_pos+2, -1 ! look from right to left at 0s, but save one after the decimal place
       if (significand(i:i) == '0') then
         sig_trim = i-1
       else
         exit
       endif
     enddo
     string = trim(significand(1:sig_trim))
   elseif (exp_start > decimal_pos) then ! float has exponent
     significand = string(1:exp_start-1)
     sig_trim = len(trim(significand))
     do i = len(trim(significand)),decimal_pos+2,-1 ! look from right to left at 0s
       if (significand(i:i) == '0') then
         sig_trim = i-1
       else
         exit
       endif
     enddo
     expnt = adjustl(string(exp_start+1:))
     if (expnt(1:1) == '+' .or. expnt(1:1) == '-') then
       separator = trim(adjustl(separator))//expnt(1:1)
       exp_start = exp_start + 1
       expnt     = adjustl(string(exp_start+1:))
     endif
     exp_trim = 1
     do i = 1,(len(trim(expnt))-1) ! look at exponent leading zeros saving last
       if (expnt(i:i) == '0') then
         exp_trim = i+1
       else
         exit
       endif
     enddo
     string = trim(adjustl(significand(1:sig_trim)))// &
              trim(adjustl(separator))// &
              trim(adjustl(expnt(exp_trim:)))
   !else ! mal-formed real, BUT this code should be unreachable
   endif
   endsubroutine compact_real_string

   elemental function strz_I8P(n, nz_pad) result(str)
   !< Converting integer to string, prefixing with the right number of zeros.
   !<
   !<```fortran
   !< use penf
   !< print "(A)", strz(n=1_I8P)
   !<```
   !=> 0000000000000000001 <<<
   !<
   !<```fortran
   !< use penf
   !< print "(A)", strz(n=1_I8P, nz_pad=5)
   !<```
   !=> 00001 <<<
   integer(I8P), intent(in)           :: n      !< Integer to be converted.
   integer(I4P), intent(in), optional :: nz_pad !< Number of zeros padding.
   character(DI8P)                    :: str    !< Returned string containing input number plus padding zeros.

   write(str,FI8PZP) n                              ! Casting of n to string.
   str=str(2:)                                      ! Leaving out the sign.
   if (present(nz_pad)) str=str(DI8P-nz_pad:DI8P-1) ! Leaving out the extra zeros padding
   endfunction strz_I8P

   elemental function strz_I4P(n, nz_pad) result(str)
   !< Convert integer to string, prefixing with the right number of zeros.
   !<
   !<```fortran
   !< use penf
   !< print "(A)", strz(n=1_I4P)
   !<```
   !=> 0000000001 <<<
   !<
   !<```fortran
   !< use penf
   !< print "(A)", strz(n=1_I4P, nz_pad=5)
   !<```
   !=> 00001 <<<
   integer(I4P), intent(in)           :: n      !< Integer to be converted.
   integer(I4P), intent(in), optional :: nz_pad !< Number of zeros padding.
   character(DI4P)                    :: str    !< Returned string containing input number plus padding zeros.

   write(str,FI4PZP) n                              ! Casting of n to string.
   str=str(2:)                                      ! Leaving out the sign.
   if (present(nz_pad)) str=str(DI4P-nz_pad:DI4P-1) ! Leaving out the extra zeros padding
   endfunction strz_I4P

   elemental function strz_I2P(n, nz_pad) result(str)
   !< Convert integer to string, prefixing with the right number of zeros.
   !<
   !<```fortran
   !< use penf
   !< print "(A)", strz(n=1_I2P)
   !<```
   !=> 00001 <<<
   !<
   !<```fortran
   !< use penf
   !< print "(A)", strz(n=1_I2P, nz_pad=3)
   !<```
   !=> 001 <<<
   integer(I2P), intent(in)           :: n      !< Integer to be converted.
   integer(I4P), intent(in), optional :: nz_pad !< Number of zeros padding.
   character(DI2P)                    :: str    !< Returned string containing input number plus padding zeros.

   write(str,FI2PZP) n                              ! Casting of n to string.
   str=str(2:)                                      ! Leaving out the sign.
   if (present(nz_pad)) str=str(DI2P-nz_pad:DI2P-1) ! Leaving out the extra zeros padding
   endfunction strz_I2P

   elemental function strz_I1P(n, nz_pad) result(str)
   !< Convert integer to string, prefixing with the right number of zeros.
   !<
   !<```fortran
   !< use penf
   !< print "(A)", strz(n=1_I1P)
   !<```
   !=> 001 <<<
   !<
   !<```fortran
   !< use penf
   !< print "(A)", strz(n=1_I1P, nz_pad=3)
   !<```
   !=> 001 <<<
   integer(I1P), intent(in)           :: n      !< Integer to be converted.
   integer(I4P), intent(in), optional :: nz_pad !< Number of zeros padding.
   character(DI1P)                    :: str    !< Returned string containing input number plus padding zeros.

   write(str,FI1PZP) n                              ! Casting of n to string.
   str=str(2:)                                      ! Leaving out the sign.
   if (present(nz_pad)) str=str(DI1P-nz_pad:DI1P-1) ! Leaving out the extra zeros padding
   endfunction strz_I1P

   function ctor_R16P(str, knd, pref, error) result(n)
   !< Convert string to real.
   !<
   !<```fortran
   !< use penf
   !< print FR16P, cton(str='-1.0', knd=1._R16P)
   !<```
   !=> -0.100000000000000000000000000000000E+0001 <<<
   character(*),           intent(in)  :: str   !< String containing input number.
   real(R16P),             intent(in)  :: knd   !< Number kind.
   character(*), optional, intent(in)  :: pref  !< Prefixing string.
   integer(I4P), optional, intent(out) :: error !< Error trapping flag: 0 no errors, >0 error occurs.
   real(R16P)                          :: n     !< Number returned.
   integer(I4P)                        :: err   !< Error trapping flag: 0 no errors, >0 error occurs.
   character(len=:), allocatable       :: prefd !< Prefixing string.

   read(str, *, iostat=err) n ! Casting of str to n.
   if (err/=0) then
     prefd = '' ; if (present(pref)) prefd = pref
     write(stderr, '(A,I1,A)') prefd//' Error: conversion of string "'//str//'" to real failed! real(', kind(knd), ')'
   endif
   if (present(error)) error = err
   endfunction ctor_R16P

   function ctor_R8P(str, knd, pref, error) result(n)
   !< Convert string to real.
   !<
   !<```fortran
   !< use penf
   !< print FR8P, cton(str='-1.0', knd=1._R8P)
   !<```
   !=> -0.100000000000000E+001 <<<
   character(*),           intent(in)  :: str   !< String containing input number.
   real(R8P),              intent(in)  :: knd   !< Number kind.
   character(*), optional, intent(in)  :: pref  !< Prefixing string.
   integer(I4P), optional, intent(out) :: error !< Error trapping flag: 0 no errors, >0 error occurs.
   real(R8P)                           :: n     !< Number returned.
   integer(I4P)                        :: err   !< Error trapping flag: 0 no errors, >0 error occurs.
   character(len=:), allocatable       :: prefd !< Prefixing string.

   read(str, *, iostat=err) n ! Casting of str to n.
   if (err/=0) then
     prefd = '' ; if (present(pref)) prefd = pref
     write(stderr, '(A,I1,A)') prefd//' Error: conversion of string "'//str//'" to real failed! real(', kind(knd), ')'
   endif
   if (present(error)) error = err
   endfunction ctor_R8P

   function ctor_R4P(str, knd, pref, error) result(n)
   !< Convert string to real.
   !<
   !<```fortran
   !< use penf
   !< print FR4P, cton(str='-1.0', knd=1._R4P)
   !<```
   !=> -0.100000E+01 <<<
   character(*),           intent(in)  :: str   !< String containing input number.
   real(R4P),              intent(in)  :: knd   !< Number kind.
   character(*), optional, intent(in)  :: pref  !< Prefixing string.
   integer(I4P), optional, intent(out) :: error !< Error trapping flag: 0 no errors, >0 error occurs.
   real(R4P)                           :: n     !< Number returned.
   integer(I4P)                        :: err   !< Error trapping flag: 0 no errors, >0 error occurs.
   character(len=:), allocatable       :: prefd !< Prefixing string.

   read(str, *, iostat=err) n ! Casting of str to n.
   if (err/=0) then
     prefd = '' ; if (present(pref)) prefd = pref
     write(stderr, '(A,I1,A)') prefd//' Error: conversion of string "'//str//'" to real failed! real(', kind(knd), ')'
   endif
   if (present(error)) error = err
   endfunction ctor_R4P

   function ctoi_I8P(str, knd, pref, error) result(n)
   !< Convert string to integer.
   !<
   !<```fortran
   !< use penf
   !< print FI8P, cton(str='-1', knd=1_I8P)
   !<```
   !=> -1 <<<
   character(*),           intent(in)  :: str   !< String containing input number.
   integer(I8P),           intent(in)  :: knd   !< Number kind.
   character(*), optional, intent(in)  :: pref  !< Prefixing string.
   integer(I4P), optional, intent(out) :: error !< Error trapping flag: 0 no errors, >0 error occurs.
   integer(I8P)                        :: n     !< Number returned.
   integer(I4P)                        :: err   !< Error trapping flag: 0 no errors, >0 error occurs.
   character(len=:), allocatable       :: prefd !< Prefixing string.

   read(str, *, iostat=err) n ! Casting of str to n.
   if (err/=0) then
     prefd = '' ; if (present(pref)) prefd = pref
     write(stderr, '(A,I1,A)') prefd//' Error: conversion of string "'//str//'" to integer failed! integer(', kind(knd), ')'
   endif
   if (present(error)) error = err
   endfunction ctoi_I8P

   function ctoi_I4P(str, knd, pref, error) result(n)
   !< Convert string to integer.
   !<
   !<```fortran
   !< use penf
   !< print FI4P, cton(str='-1', knd=1_I4P)
   !<```
   !=> -1 <<<
   character(*),           intent(in)  :: str   !< String containing input number.
   integer(I4P),           intent(in)  :: knd   !< Number kind.
   character(*), optional, intent(in)  :: pref  !< Prefixing string.
   integer(I4P), optional, intent(out) :: error !< Error trapping flag: 0 no errors, >0 error occurs.
   integer(I4P)                        :: n     !< Number returned.
   integer(I4P)                        :: err   !< Error trapping flag: 0 no errors, >0 error occurs.
   character(len=:), allocatable       :: prefd !< Prefixing string.

   read(str, *, iostat=err) n ! Casting of str to n.
   if (err/=0) then
     prefd = '' ; if (present(pref)) prefd = pref
     write(stderr, '(A,I1,A)') prefd//' Error: conversion of string "'//str//'" to integer failed! integer(', kind(knd), ')'
   endif
   if (present(error)) error = err
   endfunction ctoi_I4P

   function ctoi_I2P(str, knd, pref, error) result(n)
   !< Convert string to integer.
   !<
   !<```fortran
   !< use penf
   !< print FI2P, cton(str='-1', knd=1_I2P)
   !<```
   !=> -1 <<<
   character(*),           intent(in)  :: str   !< String containing input number.
   integer(I2P),           intent(in)  :: knd   !< Number kind.
   character(*), optional, intent(in)  :: pref  !< Prefixing string.
   integer(I4P), optional, intent(out) :: error !< Error trapping flag: 0 no errors, >0 error occurs.
   integer(I2P)                        :: n     !< Number returned.
   integer(I4P)                        :: err   !< Error trapping flag: 0 no errors, >0 error occurs.
   character(len=:), allocatable       :: prefd !< Prefixing string.

   read(str, *, iostat=err) n ! Casting of str to n.
   if (err/=0) then
     prefd = '' ; if (present(pref)) prefd = pref
     write(stderr, '(A,I1,A)') prefd//' Error: conversion of string "'//str//'" to integer failed! integer(', kind(knd), ')'
   endif
   if (present(error)) error = err
   endfunction ctoi_I2P

   function ctoi_I1P(str, knd, pref, error) result(n)
   !< Convert string to integer.
   !<
   !<```fortran
   !< use penf
   !< print FI1P, cton(str='-1', knd=1_I1P)
   !<```
   !=> -1 <<<
   character(*),           intent(in)  :: str   !< String containing input number.
   integer(I1P),           intent(in)  :: knd   !< Number kind.
   character(*), optional, intent(in)  :: pref  !< Prefixing string.
   integer(I4P), optional, intent(out) :: error !< Error trapping flag: 0 no errors, >0 error occurs.
   integer(I1P)                        :: n     !< Number returned.
   integer(I4P)                        :: err   !< Error trapping flag: 0 no errors, >0 error occurs.
   character(len=:), allocatable       :: prefd !< Prefixing string.

   read(str, *, iostat=err) n ! Casting of str to n.
   if (err/=0) then
     prefd = '' ; if (present(pref)) prefd = pref
     write(stderr, '(A,I1,A)') prefd//' Error: conversion of string "'//str//'" to integer failed! integer(', kind(knd), ')'
   endif
   if (present(error)) error = err
   endfunction ctoi_I1P

   elemental function bstr_R16P(n) result(bstr)
   !< Convert real to string of bits.
   !<
   !< @note It is assumed that R16P is represented by means of 128 bits, but this is not ensured in all architectures.
   !<
   !<```fortran
   !< use penf
   !< character(128) :: b
   !< b = bstr(n=1._R16P)
   !< print "(A)", b(17:)
   !<```
   !=> 0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000001111111100111111 <<<
   real(R16P), intent(in) :: n          !< Real to be converted.
   character(128)         :: bstr       !< Returned bit-string containing input number.
   integer(I1P)           :: buffer(16) !< Transfer buffer.

   buffer = transfer(n, buffer)
   write(bstr, '(16B8.8)') buffer
   endfunction bstr_R16P

   elemental function bstr_R8P(n) result(bstr)
   !< Convert real to string of bits.
   !<
   !< @note It is assumed that R8P is represented by means of 64 bits, but this is not ensured in all architectures.
   !<
   !<```fortran
   !< use penf
   !< print "(A)", bstr(n=1._R8P)
   !<```
   !=> 0000000000000000000000000000000000000000000000001111000000111111 <<<
   real(R8P), intent(in) :: n         !< Real to be converted.
   character(64)         :: bstr      !< Returned bit-string containing input number.
   integer(I1P)          :: buffer(8) !< Transfer buffer.

   buffer = transfer(n, buffer)
   write(bstr, '(8B8.8)') buffer
   endfunction bstr_R8P

   elemental function bstr_R4P(n) result(bstr)
   !< Convert real to string of bits.
   !<
   !< @note It is assumed that R4P is represented by means of 32 bits, but this is not ensured in all architectures.
   !<
   !<```fortran
   !< use penf
   !< print "(A)", bstr(n=1._R4P)
   !<```
   !=> 00000000000000001000000000111111 <<<
   real(R4P), intent(in) :: n         !< Real to be converted.
   character(32)         :: bstr      !< Returned bit-string containing input number.
   integer(I1P)          :: buffer(4) !< Transfer buffer.

   buffer = transfer(n, buffer)
   write(bstr, '(4B8.8)') buffer
   endfunction bstr_R4P

   elemental function bstr_I8P(n) result(bstr)
   !< Convert integer to string of bits.
   !<
   !< @note It is assumed that I8P is represented by means of 64 bits, but this is not ensured in all architectures.
   !<
   !<```fortran
   !< use penf
   !< print "(A)", bstr(n=1_I8P)
   !<```
   !=> 0000000000000000000000000000000000000000000000000000000000000001 <<<
   integer(I8P), intent(in) :: n    !< Real to be converted.
   character(64)            :: bstr !< Returned bit-string containing input number.

   write(bstr, '(B64.64)') n
   endfunction bstr_I8P

   elemental function bstr_I4P(n) result(bstr)
   !< Convert integer to string of bits.
   !<
   !< @note It is assumed that I4P is represented by means of 32 bits, but this is not ensured in all architectures.
   !<
   !<```fortran
   !< use penf
   !< print "(A)", bstr(n=1_I4P)
   !<```
   !=> 00000000000000000000000000000001 <<<
   integer(I4P), intent(in) :: n    !< Real to be converted.
   character(32)            :: bstr !< Returned bit-string containing input number.

   write(bstr, '(B32.32)') n
   endfunction bstr_I4P

   elemental function bstr_I2P(n) result(bstr)
   !< Convert integer to string of bits.
   !<
   !< @note It is assumed that I2P is represented by means of 16 bits, but this is not ensured in all architectures.
   !<
   !<```fortran
   !< use penf
   !< print "(A)", bstr(n=1_I2P)
   !<```
   !=> 0000000000000001 <<<
   integer(I2P), intent(in) :: n    !< Real to be converted.
   character(16)            :: bstr !< Returned bit-string containing input number.

   write(bstr, '(B16.16)') n
   endfunction bstr_I2P

   elemental function bstr_I1P(n) result(bstr)
   !< Convert integer to string of bits.
   !<
   !< @note It is assumed that I1P is represented by means of 8 bits, but this is not ensured in all architectures.
   !<
   !<```fortran
   !< use penf
   !< print "(A)", bstr(n=1_I1P)
   !<```
   !=> 00000001 <<<
   integer(I1P), intent(in) :: n    !< Real to be converted.
   character(8)             :: bstr !< Returned bit-string containing input number.

   write(bstr, '(B8.8)') n
   endfunction bstr_I1P

   elemental function bctor_R16P(bstr, knd) result(n)
   !< Convert bit-string to real.
   !<
   !<```fortran
   !< use penf
   !< print FR16P, bcton('00000000000000000000000000000000000000000000000000000000000000000000000000000'//&
   !<                    '000000000000000000000000000000000001111111100111111', knd=1._R16P)
   !<```
   !=> 0.100000000000000000000000000000000E+0001 <<<
   character(*), intent(in) :: bstr       !< String containing input number.
   real(R16P),   intent(in) :: knd        !< Number kind.
   real(R16P)               :: n          !< Number returned.
   integer(I1P)             :: buffer(16) !< Transfer buffer.

   read(bstr, '(16B8.8)') buffer
   n = transfer(buffer, n)
   endfunction bctor_R16P

   elemental function bctor_R8P(bstr, knd) result(n)
   !< Convert bit-string to real.
   !<
   !<```fortran
   !< use penf
   !< print FR8P, bcton('0000000000000000000000000000000000000000000000001111000000111111', knd=1._R8P)
   !<```
   !=> 0.100000000000000E+001 <<<
   character(*), intent(in) :: bstr      !< String containing input number.
   real(R8P),    intent(in) :: knd       !< Number kind.
   real(R8P)                :: n         !< Number returned.
   integer(I1P)             :: buffer(8) !< Transfer buffer.

   read(bstr, '(8B8.8)') buffer
   n = transfer(buffer, n)
   endfunction bctor_R8P

   elemental function bctor_R4P(bstr, knd) result(n)
   !< Convert bit-string to real.
   !<
   !<```fortran
   !< use penf
   !< print FR4P, bcton('00000000000000001000000000111111', knd=1._R4P)
   !<```
   !=> 0.100000E+01 <<<
   character(*), intent(in) :: bstr      !< String containing input number.
   real(R4P),    intent(in) :: knd       !< Number kind.
   real(R4P)                :: n         !< Number returned.
   integer(I1P)             :: buffer(4) !< Transfer buffer.

   read(bstr, '(4B8.8)') buffer
   n = transfer(buffer, n)
   endfunction bctor_R4P

   elemental function bctoi_I8P(bstr, knd) result(n)
   !< Convert bit-string to integer.
   !<
   !<```fortran
   !< use penf
   !< print FI8P, bcton('0000000000000000000000000000000000000000000000000000000000000001', knd=1_I8P)
   !<```
   !=> 1 <<<
   character(*), intent(in) :: bstr !< String containing input number.
   integer(I8P), intent(in) :: knd  !< Number kind.
   integer(I8P)             :: n    !< Number returned.

   read(bstr,'(B'//trim(str(bit_size(knd), .true.))//'.'//trim(str(bit_size(knd), .true.))//')') n
   endfunction bctoi_I8P

   elemental function bctoi_I4P(bstr, knd) result(n)
   !< Convert bit-string to integer.
   !<
   !<```fortran
   !< use penf
   !< print FI4P, bcton('00000000000000000000000000000001', knd=1_I4P)
   !<```
   !=> 1 <<<
   character(*), intent(in) :: bstr !< String containing input number.
   integer(I4P), intent(in) :: knd  !< Number kind.
   integer(I4P)             :: n    !< Number returned.

   read(bstr,'(B'//trim(str(bit_size(knd), .true.))//'.'//trim(str(bit_size(knd), .true.))//')') n
   endfunction bctoi_I4P

   elemental function bctoi_I2P(bstr, knd) result(n)
   !< Convert bit-string to integer.
   !<
   !<```fortran
   !< use penf
   !< print FI2P, bcton('0000000000000001', knd=1_I2P)
   !<```
   !=> 1 <<<
   character(*), intent(in) :: bstr !< String containing input number.
   integer(I2P), intent(in) :: knd  !< Number kind.
   integer(I2P)             :: n    !< Number returned.

   read(bstr,'(B'//trim(str(bit_size(knd), .true.))//'.'//trim(str(bit_size(knd), .true.))//')') n
   endfunction bctoi_I2P

   elemental function bctoi_I1P(bstr, knd) result(n)
   !< Convert bit-string to integer.
   !<
   !<```fortran
   !< use penf
   !< print FI1P, bcton('00000001', knd=1_I1P)
   !<```
   !=> 1 <<<
   character(*), intent(in) :: bstr !< String containing input number.
   integer(I1P), intent(in) :: knd  !< Number kind.
   integer(I1P)             :: n    !< Number returned.

   read(bstr,'(B'//trim(str(bit_size(knd), .true.))//'.'//trim(str(bit_size(knd), .true.))//')') n
   endfunction bctoi_I1P
endmodule penf_stringify
