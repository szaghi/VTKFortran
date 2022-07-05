program volatile_doctest
use stringifor_string_t
 type(string) :: astring
 logical :: test_passed(2)
 astring = 'abcdefghilmnopqrstuvz'
 test_passed(1) = (astring%reverse()//''=='zvutsrqponmlihgfedcba')
 astring = '0123456789'
 test_passed(2) = (astring%reverse()//''=='9876543210')
 print '(L1)', all(test_passed)
endprogram volatile_doctest