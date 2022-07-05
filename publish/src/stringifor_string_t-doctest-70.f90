program volatile_doctest
use stringifor_string_t
 type(string) :: astring
 logical :: test_passed(2)
 astring = '   -1212112.3 '
 test_passed(1) = astring%is_digit().eqv..false.
 astring = '12121123'
 test_passed(2) = astring%is_digit().eqv..true.
 print '(L1)', all(test_passed)
endprogram volatile_doctest