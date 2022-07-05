program volatile_doctest
use stringifor_string_t
 type(string) :: astring
 astring = 'this is string example....wow!!!'
 call astring%free
 print '(L1)', astring%is_allocated().eqv..false.
endprogram volatile_doctest