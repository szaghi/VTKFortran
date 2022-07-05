program volatile_doctest
use stringifor_string_t
 type(string) :: astring
 type(string) :: anotherstring
 character(len=:), allocatable :: acharacter
 integer :: istart
 integer :: iend
 logical :: test_passed(5)
 astring = '<test> <first> hello </first> <first> not the first </first> </test>'
 anotherstring = astring%search(tag_start='<first>', tag_end='</first>')
 test_passed(1) = anotherstring//''=='<first> hello </first>'
 astring = '<test> <a> <a> <a> the nested a </a> </a> </a> </test>'
 anotherstring = astring%search(tag_start='<a>', tag_end='</a>')
 test_passed(2) = anotherstring//''=='<a> <a> <a> the nested a </a> </a> </a>'
 call astring%free
 anotherstring = '<test> <a> <a> <a> the nested a </a> </a> </a> </test>'
 astring = astring%search(in_string=anotherstring, tag_start='<a>', tag_end='</a>')
 test_passed(3) = astring//''=='<a> <a> <a> the nested a </a> </a> </a>'
 call astring%free
 acharacter = '<test> <a> <a> <a> the nested a </a> </a> </a> </test>'
 astring = astring%search(in_character=acharacter, tag_start='<a>', tag_end='</a>')
 test_passed(4) = astring//''=='<a> <a> <a> the nested a </a> </a> </a>'
 acharacter = '<test> <first> hello </first> <sec> <sec>not the first</sec> </sec> </test>'
 astring = astring%search(in_character=acharacter, tag_start='<sec>', tag_end='</sec>', istart=istart, iend=iend)
 test_passed(5) = astring//''==acharacter(31:67)
 print '(L1)', all(test_passed)
endprogram volatile_doctest