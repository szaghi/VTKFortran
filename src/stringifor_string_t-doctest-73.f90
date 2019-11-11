program volatile_doctest
use stringifor_string_t
 type(string) :: astring
 logical      :: test_passed(4)
 astring = 'Hello WorLD!'
 test_passed(1) = astring%start_with(prefix='Hello').eqv..true.
 test_passed(2) = astring%start_with(prefix='hell').eqv..false.
 test_passed(3) = astring%start_with(prefix='llo Wor', start=3).eqv..true.
 test_passed(4) = astring%start_with(prefix='lo W', start=4, end=7).eqv..true.
 print '(L1)', all(test_passed)
endprogram volatile_doctest