program volatile_doctest
use stringifor_string_t
 type(string) :: astring
 astring = '   Hello World!'
 print "(L1)", adjustl(astring)=='Hello World!   '
endprogram volatile_doctest