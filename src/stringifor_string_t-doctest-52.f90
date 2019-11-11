program volatile_doctest
use stringifor_string_t
 type(string) :: astring
 logical      :: test_passed(1)
 astring = '  Hello World!   '
 test_passed(1) = astring%swapcase()//''=='  hELLO wORLD!   '
 print '(L1)', all(test_passed)
endprogram volatile_doctest