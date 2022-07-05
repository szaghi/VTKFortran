program volatile_doctest
use stringifor_string_t
 type(string) :: astring
 logical :: test_passed(2)
 test_passed(1) = astring%is_allocated().eqv..false.
 astring = 'hello'
 test_passed(2) = astring%is_allocated().eqv..true.
 print '(L1)', all(test_passed)
endprogram volatile_doctest