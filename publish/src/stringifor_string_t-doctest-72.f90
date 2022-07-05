program volatile_doctest
use stringifor_string_t
 type(string) :: astring
 logical :: test_passed(3)
 astring = ' Hello World'
 test_passed(1) = astring%is_lower().eqv..false.
 astring = ' HELLO WORLD'
 test_passed(2) = astring%is_lower().eqv..false.
 astring = ' hello world'
 test_passed(3) = astring%is_lower().eqv..true.
 print '(L1)', all(test_passed)
endprogram volatile_doctest