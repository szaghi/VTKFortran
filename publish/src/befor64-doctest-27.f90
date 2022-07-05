program volatile_doctest
use befor64
 use befor64
 use penf
 integer(I2P) :: scalar_I2
 call b64_decode(code='Nf8=',n=scalar_I2)
 print "(L1)", scalar_I2==-203_I2P
endprogram volatile_doctest