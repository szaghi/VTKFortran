program volatile_doctest
use befor64
 use befor64
 use penf
 real(R8P) :: array_R8(1:2)
 call b64_decode(code='AAAAAAAA8D8AAAAAAAAAQA==',n=array_R8)
 print "(L1)", str(n=array_R8)==str(n=[1._R8P,2._R8P])
endprogram volatile_doctest