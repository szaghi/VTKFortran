program volatile_doctest
use stringifor_string_t
 type(string) :: astring
 type(string), allocatable :: strings(:)
 logical :: test_passed(1)
 astring = '-1-2-3-4-5-6-7-8-'
 call astring%split_chunked(tokens=strings, sep='-', chunks=3)
 test_passed(1) = (strings(1)//''=='1'.and.strings(2)//''=='2'.and.strings(3)//''=='3'.and.strings(4)//''=='4'.and. &
 strings(5)//''=='5'.and.strings(6)//''=='6'.and.strings(7)//''=='7'.and.strings(8)//''=='8')
 print '(L1)', all(test_passed)
endprogram volatile_doctest