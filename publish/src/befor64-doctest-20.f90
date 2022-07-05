program volatile_doctest
use befor64
 use befor64
 use penf
 character(len=:), allocatable :: code64
 call b64_encode(n=[120_I1P,-1_I1P], code=code64)
 print "(A)", code64
endprogram volatile_doctest