program volatile_doctest
use stringifor_string_t
 use penf
 type(string) :: astring
 integer(I4P) :: integer_
 logical :: test_passed(1)
 astring = '127'
 integer_ = astring%to_number(kind=1_I4P)
 test_passed(1) = integer_==127_I4P
 print '(L1)', all(test_passed)
endprogram volatile_doctest