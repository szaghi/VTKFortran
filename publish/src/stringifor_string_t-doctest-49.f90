program volatile_doctest
use stringifor_string_t
 type(string) :: astring
 astring = 'the Quick Brown fox Jumps over the Lazy Dog.'
 print "(A)", astring%slice(11,25)
endprogram volatile_doctest