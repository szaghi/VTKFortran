program volatile_doctest
use stringifor_string_t
 type(string) :: astring
 logical      :: test_passed(2)
 astring = '^\s \d+\s*'
 test_passed(1) = astring%escape(to_escape='\')//''=='^\\s \\d+\\s*'
 test_passed(2) = astring%escape(to_escape='\', esc='|')//''=='^|\s |\d+|\s*'
 print '(L1)', all(test_passed)
endprogram volatile_doctest