program volatile_doctest
use befor64
 use befor64
 use penf
 real(R16P) :: scalar_R16
 call b64_decode(code='CKwcWmTHYEA=',n=scalar_R16)
 print "(L1)", scalar_R16==134.231_R16P
endprogram volatile_doctest