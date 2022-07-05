program volatile_doctest
use befor64
 use befor64
 use penf
 character(:), allocatable :: code64
 code64 = repeat(' ',5)
 call b64_decode(code='aGVsbG8=',s=code64)
 print "(L1)", code64=='hello'
endprogram volatile_doctest