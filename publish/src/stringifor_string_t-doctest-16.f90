program volatile_doctest
use stringifor_string_t
 type(string) :: astring
 astring = 'x'
 print "(L1)", astring%repeat(5)//''=='xxxxx'
endprogram volatile_doctest