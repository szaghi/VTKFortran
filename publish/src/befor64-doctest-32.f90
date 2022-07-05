program volatile_doctest
use befor64
 use befor64
 use penf
 real(R4P) :: array_R4(1:2)
 call b64_decode(code='AAAAAOF6AMI=',n=array_R4)
 print "(L1)", str(n=array_R4)==str(n=[0._R4P,-32.12_R4P])
endprogram volatile_doctest