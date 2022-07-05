program volatile_doctest
use stringifor_string_t
 type(string) :: astring
 type(string) :: strings(3)
 logical :: test_passed(3)
 astring = 'Hello WorLD!'
 strings = astring%partition(sep='lo Wo')
 test_passed(1) = (strings(1)//''=='Hel'.and.strings(2)//''=='lo Wo'.and.strings(3)//''=='rLD!')
 strings = astring%partition(sep='Hello')
 test_passed(2) = (strings(1)//''==''.and.strings(2)//''=='Hello'.and.strings(3)//''==' WorLD!')
 astring = 'Hello WorLD!'
 strings = astring%partition()
 test_passed(3) = (strings(1)//''=='Hello'.and.strings(2)//''==' '.and.strings(3)//''=='WorLD!')
 print '(L1)', all(test_passed)
endprogram volatile_doctest