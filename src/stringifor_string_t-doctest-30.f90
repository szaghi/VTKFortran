program volatile_doctest
use stringifor_string_t
 type(string) :: astring
 logical      :: test_passed(4)
 astring = 'this is string example....wow!!!'
 test_passed(1) = astring%fill(width=40)//''=='00000000this is string example....wow!!!'
 test_passed(2) = astring%fill(width=50)//''=='000000000000000000this is string example....wow!!!'
 test_passed(3) = astring%fill(width=50, right=.true.)//''=='this is string example....wow!!!000000000000000000'
 test_passed(4) = astring%fill(width=40, filling_char='*')//''=='********this is string example....wow!!!'
 print '(L1)', all(test_passed)
endprogram volatile_doctest