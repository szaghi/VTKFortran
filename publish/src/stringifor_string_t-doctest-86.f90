program volatile_doctest
use stringifor_string_t
 type(string) :: astring
 type(string) :: anotherstring
 logical :: test_passed(1)
 astring = 'Hello '
 anotherstring = 'Bye bye'
 test_passed(1) = astring//anotherstring=='Hello Bye bye'
 print '(L1)', all(test_passed)
endprogram volatile_doctest