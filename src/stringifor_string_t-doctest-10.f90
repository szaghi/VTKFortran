program volatile_doctest
use stringifor_string_t
 type(string) :: string1
 type(string) :: string2
 logical      :: test_passed(2)
 string1 = 'Hello World Hello!'
 string2 = 'llo'
 test_passed(1) = string1%index(substring=string2)==index(string='Hello World Hello!', substring='llo')
 test_passed(2) = string1%index(substring=string2, back=.true.)==index(string='Hello World Hello!', substring='llo', &
                                                                       back=.true.)
 print '(L1)', all(test_passed)
endprogram volatile_doctest