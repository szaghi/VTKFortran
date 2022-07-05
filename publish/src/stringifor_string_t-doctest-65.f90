program volatile_doctest
use stringifor_string_t
 type(string) :: astring
 logical :: test_passed(1)
 astring = '+++ab-++cre-++cre-ab+++++'
 test_passed(1) = astring%unique(substring='+')//''=='+ab-+cre-+cre-ab+'
 print '(L1)', all(test_passed)
endprogram volatile_doctest