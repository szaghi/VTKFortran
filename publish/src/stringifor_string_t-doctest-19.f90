program volatile_doctest
use stringifor_string_t
 type(string) :: string1
 logical :: test_passed(2)
 string1 = 'Hello World Hello!'
 test_passed(1) = string1%scan(set='llo')==scan(string='Hello World Hello!', set='llo')
 test_passed(2) = string1%scan(set='llo', back=.true.)==scan(string='Hello World Hello!', set='llo', back=.true.)
 print '(L1)', all(test_passed)
endprogram volatile_doctest