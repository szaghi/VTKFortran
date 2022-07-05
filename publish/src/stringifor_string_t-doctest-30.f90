program volatile_doctest
use stringifor_string_t
 type(string) :: astring
 astring = 'How are you?'
 print '(L1)', astring%encode(codec='base64')//''=='SG93IGFyZSB5b3U/'
endprogram volatile_doctest