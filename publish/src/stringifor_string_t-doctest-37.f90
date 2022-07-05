program volatile_doctest
use stringifor_string_t
 type(string) :: astring
 character(len=:), allocatable :: acharacter
 logical :: test_passed(5)
 astring = 'this is string example wow!!!'
 acharacter = '... '
 test_passed(1) = astring%insert(substring=acharacter, pos=1)//''=='... this is string example wow!!!'
 test_passed(2) = astring%insert(substring=acharacter, pos=23)//''=='this is string example...  wow!!!'
 test_passed(3) = astring%insert(substring=acharacter, pos=29)//''=='this is string example wow!!!... '
 test_passed(4) = astring%insert(substring=acharacter, pos=-1)//''=='... this is string example wow!!!'
 test_passed(5) = astring%insert(substring=acharacter, pos=100)//''=='this is string example wow!!!... '
 print '(L1)', all(test_passed)
endprogram volatile_doctest