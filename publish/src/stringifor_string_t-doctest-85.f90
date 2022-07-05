program volatile_doctest
use stringifor_string_t
 use penf
 type(string) :: astring
 logical :: test_passed(1)
 astring = 3.021e6_R8P
 test_passed(1) = astring%to_number(kind=1._R8P)==3.021e6_R8P
 print '(L1)', all(test_passed)
endprogram volatile_doctest