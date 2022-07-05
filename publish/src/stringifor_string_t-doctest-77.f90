program volatile_doctest
use stringifor_string_t
 type(string) :: astring
 type(string) :: anotherstring
 logical :: test_passed(1)
 astring = 'hello'
 anotherstring = astring
 test_passed(1) = astring%chars()==anotherstring%chars()
 print '(L1)', all(test_passed)
endprogram volatile_doctest