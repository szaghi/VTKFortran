program volatile_doctest
use befor64
 use befor64
 use penf
 real(R8P) :: scalar_R8
 call b64_decode(code='AAAAAAAA8D8=',n=scalar_R8)
 print "(L1)", scalar_R8==1._R8P
endprogram volatile_doctest