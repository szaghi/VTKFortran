program volatile_doctest
use befor64
 use befor64
 use penf
 integer(I1P) :: scalar_I1
 call b64_decode(code='eA==',n=scalar_I1)
 print "(L1)", scalar_I1==120_I1P
endprogram volatile_doctest