program volatile_doctest
use stringifor_string_t
 type(string) :: astring
 character(len=:), allocatable :: tmpname
 logical :: test_passed(5)
 tmpname = astring%tempname()
 inquire(file=tmpname, exist=test_passed(1))
 test_passed(1) = .not.test_passed(1)
 tmpname = astring%tempname(is_file=.false.)
 inquire(file=tmpname, exist=test_passed(2))
 test_passed(2) = .not.test_passed(2)
 tmpname = astring%tempname(path='./')
 inquire(file=tmpname, exist=test_passed(3))
 test_passed(3) = .not.test_passed(3)
 astring = 'me-'
 tmpname = astring%tempname()
 inquire(file=tmpname, exist=test_passed(4))
 test_passed(4) = .not.test_passed(4)
 tmpname = astring%tempname(prefix='you-')
 inquire(file=tmpname, exist=test_passed(5))
 test_passed(5) = .not.test_passed(5)
 print '(L1)', all(test_passed)
endprogram volatile_doctest