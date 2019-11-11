program volatile_doctest
use stringifor_string_t
 type(string) :: astring
 type(string) :: strings(3)
 logical      :: test_passed(5)
 strings(1) = 'one'
 strings(2) = 'two'
 strings(3) = 'three'
 test_passed(1) = (astring%join(array=strings)//''==strings(1)//strings(2)//strings(3))
 test_passed(2) = (astring%join(array=strings, sep='-')//''==strings(1)//'-'//strings(2)//'-'//strings(3))
 call strings(1)%free
 strings(2) = 'two'
 strings(3) = 'three'
 test_passed(3) = (astring%join(array=strings, sep='-')//''==strings(2)//'-'//strings(3))
 strings(1) = 'one'
 strings(2) = 'two'
 call strings(3)%free
 test_passed(4) = (astring%join(array=strings, sep='-')//''==strings(1)//'-'//strings(2))
 strings(1) = 'one'
 call strings(2)%free
 strings(3) = 'three'
 test_passed(5) = (astring%join(array=strings, sep='-')//''==strings(1)//'-'//strings(3))
 print '(L1)', all(test_passed)
endprogram volatile_doctest