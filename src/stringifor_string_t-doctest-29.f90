program volatile_doctest
use stringifor_string_t
 type(string) :: astring
 astring = '/bar/foo.tar.bz2'
 print '(L1)', astring%extension()//''=='.bz2'
endprogram volatile_doctest