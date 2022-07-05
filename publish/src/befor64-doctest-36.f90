program volatile_doctest
use befor64
 use befor64
 use penf
 integer(I1P) :: array_I1(1:2)
 call b64_decode(code='eP8=',n=array_I1)
 print "(L1)", str(n=array_I1)==str(n=[120_I1P,-1_I1P])
endprogram volatile_doctest