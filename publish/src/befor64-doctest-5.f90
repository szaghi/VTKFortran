program volatile_doctest
use befor64
 use befor64
 use penf
 integer(I8P) :: array_I8(1:4)
 call b64_decode_up(code='FwAAAAAAAABEAQAAAAAAABBwhAEAAAAAAgAAAAAAAAA=', up=array_I8)
 print "(L1)", str(n=array_I8)==str(n=[23_I8P,324_I8P,25456656_I8P,2_I8P])
endprogram volatile_doctest