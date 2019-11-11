program volatile_doctest
use stringifor_string_t
 type(string) :: astring
 character(5) :: characters(3)
 logical      :: test_passed(6)
 characters(1) = 'one'
 characters(2) = 'two'
 characters(3) = 'three'
 test_passed(1) = (astring%join(array=characters)//''==characters(1)//characters(2)//characters(3))
 test_passed(2) = (astring%join(array=characters, sep='-')//''==characters(1)//'-'//characters(2)//'-'//characters(3))
 characters(1) = ''
 characters(2) = 'two'
 characters(3) = 'three'
 test_passed(3) = (astring%join(array=characters, sep='-')//''==characters(2)//'-'//characters(3))
 characters(1) = 'one'
 characters(2) = 'two'
 characters(3) = ''
 test_passed(4) = (astring%join(array=characters, sep='-')//''==characters(1)//'-'//characters(2))
 characters(1) = 'one'
 characters(2) = ''
 characters(3) = 'three'
 test_passed(5) = (astring%join(array=characters, sep='-')//''==characters(1)//'-'//characters(3))
 characters(1) = 'one'
 characters(2) = 'two'
 characters(3) = 'three'
 astring = '_'
 test_passed(6) = (astring%join(array=characters)//''==characters(1)//'_'//characters(2)//'_'//characters(3))
 print '(L1)', all(test_passed)
endprogram volatile_doctest