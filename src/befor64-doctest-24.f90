program volatile_doctest
use befor64
 use befor64
 use penf
 real(R4P) :: scalar_R4
 call b64_decode(code='AAAAAA==',n=scalar_R4)
 print "(L1)", scalar_R4==0._R4P
endprogram volatile_doctest