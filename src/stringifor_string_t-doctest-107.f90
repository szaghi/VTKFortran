program volatile_doctest
use stringifor_string_t
 type(string) :: astring
 type(string) :: anotherstring
 logical :: test_passed(2)
 astring = 'one'
 anotherstring = 'ONE'
 test_passed(1) = ((astring>anotherstring).eqv..true.)
 astring = 'ONE'
 anotherstring = 'one'
 test_passed(2) = ((astring>anotherstring).eqv..false.)
 print '(L1)', all(test_passed)
endprogram volatile_doctest