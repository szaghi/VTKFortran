program volatile_doctest
use stringifor_string_t
 type(string) :: astring
 astring = 'say all Hello WorLD!'
 print '(L1)', astring%chars()=='say all Hello WorLD!'
endprogram volatile_doctest