program volatile_doctest
use befor64
 use befor64
 use penf
 integer(I2P) :: array_I2(1:2)
 call b64_decode(code='Nf/2/w==',n=array_I2)
 print "(L1)", str(n=array_I2)==str(n=[-203_I2P,-10_I2P])
endprogram volatile_doctest