program volatile_doctest
use stringifor_string_t
 type(string)                  :: astring
 character(len=:), allocatable :: acharacter
 logical                       :: test_passed(1)
 astring = 'Hello '
 acharacter = 'World!'
 test_passed(1) = astring//acharacter=='Hello World!'
 print '(L1)', all(test_passed)
endprogram volatile_doctest