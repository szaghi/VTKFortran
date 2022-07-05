program volatile_doctest
use stringifor_string_t
 type(string) :: astring
 type(string) :: anotherstring
 type(string) :: yetanotherstring
 logical :: test_passed(1)
 astring = 'Hello '
 anotherstring = 'Bye bye'
 yetanotherstring = astring.cat.anotherstring
 test_passed(1) = yetanotherstring%chars()=='Hello Bye bye'
 print '(L1)', all(test_passed)
endprogram volatile_doctest