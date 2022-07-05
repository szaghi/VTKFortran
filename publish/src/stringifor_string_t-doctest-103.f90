program volatile_doctest
use stringifor_string_t
 type(string) :: astring
 character(len=:), allocatable :: acharacter
 logical :: test_passed(3)
 astring = 'one'
 acharacter = 'ONE'
 test_passed(1) = ((acharacter<=astring).eqv..true.)
 astring = 'ONE'
 acharacter = 'one'
 test_passed(2) = ((acharacter<=astring).eqv..false.)
 astring = 'ONE'
 acharacter = 'ONE'
 test_passed(3) = ((acharacter<=astring).eqv..true.)
 print '(L1)', all(test_passed)
endprogram volatile_doctest