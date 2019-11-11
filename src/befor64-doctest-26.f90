program volatile_doctest
use befor64
 use befor64
 use penf
 integer(I4P) :: scalar_I4
 call b64_decode(code='5wcAAA==',n=scalar_I4)
 print "(L1)", scalar_I4==2023_I4P
endprogram volatile_doctest