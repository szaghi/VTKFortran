program volatile_doctest
use stringifor_string_t
 type(string) :: astring
 astring = 'y'
 print "(L1)", astring%repeat('x', 5)//''=='xxxxx'
endprogram volatile_doctest