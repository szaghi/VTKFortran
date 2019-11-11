program volatile_doctest
use befor64
 use befor64
 use penf
 integer(I4P) :: array_I4(1:2)
 call b64_decode(code='5wcAAOj///8=',n=array_I4)
 print "(L1)", str(n=array_I4)==str(n=[2023_I4P,-24_I4P])
endprogram volatile_doctest