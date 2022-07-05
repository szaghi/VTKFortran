program volatile_doctest
use stringifor_string_t
 type(string) :: astring
 astring = 'say all Hello WorLD!'
 print '(L1)', astring%capitalize()//''=='Say all hello world!'
endprogram volatile_doctest