program volatile_doctest
use stringifor_string_t
 type(string) :: astring
 logical :: test_passed(6)
 astring = '   -1212112 '
 test_passed(1) = astring%is_integer().eqv..true.
 astring = '   -1212112'
 test_passed(2) = astring%is_integer(allow_spaces=.false.).eqv..false.
 astring = '-1212112   '
 test_passed(3) = astring%is_integer(allow_spaces=.false.).eqv..false.
 astring = '+2e20'
 test_passed(4) = astring%is_integer().eqv..true.
 astring = ' -2E13 '
 test_passed(5) = astring%is_integer().eqv..true.
 astring = ' -2 E13 '
 test_passed(6) = astring%is_integer().eqv..false.
 print '(L1)', all(test_passed)
endprogram volatile_doctest