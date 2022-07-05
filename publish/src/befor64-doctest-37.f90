program volatile_doctest
use befor64
 use befor64
 use penf
 character(5) :: array_s(1:2)
 call b64_decode(code='aGVsbG93b3JsZA==',s=array_s)
 print "(L1)", array_s(1)//array_s(2)=='helloworld'
endprogram volatile_doctest