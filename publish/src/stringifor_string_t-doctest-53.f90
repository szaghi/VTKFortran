program volatile_doctest
use stringifor_string_t
 type(string) :: astring
 logical :: test_passed(1)
 astring = 'the Quick Brown fox Jumps over the Lazy Dog.'
 test_passed(1) = astring%startcase()//''=='The Quick Brown Fox Jumps Over The Lazy Dog.'
 print '(L1)', all(test_passed)
endprogram volatile_doctest