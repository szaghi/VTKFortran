program volatile_doctest
use stringifor_string_t
 type(string) :: astring
 logical      :: test_passed(5)
 astring = 'Hello WorLD!'
 test_passed(1) = astring%end_with(suffix='LD!').eqv..true.
 test_passed(2) = astring%end_with(suffix='lD!').eqv..false.
 test_passed(3) = astring%end_with(suffix='orLD!', start=5).eqv..true.
 test_passed(4) = astring%end_with(suffix='orLD!', start=8, end=12).eqv..true.
 test_passed(5) = astring%end_with(suffix='!').eqv..true.
 print '(L1)', all(test_passed)
endprogram volatile_doctest