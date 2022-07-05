program volatile_doctest
use stringifor_string_t
 type(string) :: string1
 logical :: test_passed(2)
 string1 = 'ell'
 test_passed(1) = verify(s='Hello World Hello!', set=string1)==verify(string='Hello World Hello!', set='llo')
 test_passed(2) = verify(s='Hello World Hello!', set=string1, back=.true.)==verify(string='Hello World Hello!', set='llo', &
 back=.true.)
 print '(L1)', all(test_passed)
endprogram volatile_doctest