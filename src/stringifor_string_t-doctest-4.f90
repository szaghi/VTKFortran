program volatile_doctest
use stringifor_string_t
 type(string) :: string1
 logical      :: test_passed(2)
 string1 = 'llo'
 test_passed(1) = index(s='Hello World Hello!', substring=string1)==index(string='Hello World Hello!', substring='llo')
 test_passed(2) = index(s='Hello World Hello!', substring=string1, back=.true.)==index(string='Hello World Hello!', &
                                                                                       substring='llo', back=.true.)
 print '(L1)', all(test_passed)
endprogram volatile_doctest