program volatile_doctest
use stringifor_string_t
 type(string) :: astring
 logical      :: test_passed(1)
 astring = 'hello'
 test_passed(1) = astring%chars()=='hello'
 print '(L1)', all(test_passed)
endprogram volatile_doctest