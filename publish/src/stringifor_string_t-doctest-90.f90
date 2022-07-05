program volatile_doctest
use stringifor_string_t
 type(string) :: astring
 type(string) :: yetanotherstring
 character(len=:), allocatable :: acharacter
 logical :: test_passed(1)
 astring = 'Hello '
 acharacter = 'World!'
 yetanotherstring = astring.cat.acharacter
 test_passed(1) = yetanotherstring%chars()=='Hello World!'
 print '(L1)', all(test_passed)
endprogram volatile_doctest