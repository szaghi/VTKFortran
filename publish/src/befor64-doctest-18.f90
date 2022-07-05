program volatile_doctest
use befor64
 use befor64
 use penf
 character(len=:), allocatable :: code64
 call b64_encode(n=[2023_I4P,-24_I4P], code=code64)
 print "(A)", code64
endprogram volatile_doctest