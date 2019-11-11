program volatile_doctest
use stringifor_string_t
 type(string) :: astring
 astring = '   Hello World!'
 print "(L1)", astring%adjustl()//''=='Hello World!   '
endprogram volatile_doctest