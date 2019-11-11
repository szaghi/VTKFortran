program volatile_doctest
use befor64
 use befor64
 use penf
 integer(I8P) :: scalar_I8
 call b64_decode(code='FwAAAAAAAAA=',n=scalar_I8)
 print "(L1)", scalar_I8==23_I8P
endprogram volatile_doctest