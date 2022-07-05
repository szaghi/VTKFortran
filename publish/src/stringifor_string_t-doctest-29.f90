program volatile_doctest
use stringifor_string_t
 type(string) :: astring
 astring = 'SG93IGFyZSB5b3U/'
 print '(L1)', astring%decode(codec='base64')//''=='How are you?'
endprogram volatile_doctest