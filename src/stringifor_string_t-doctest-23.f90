program volatile_doctest
use stringifor_string_t
 type(string) :: astring
 astring = 'caMeL caSe var'
 print '(L1)', astring%camelcase()//''=='CamelCaseVar'
endprogram volatile_doctest