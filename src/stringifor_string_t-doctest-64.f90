program volatile_doctest
use stringifor_string_t
 type(string)              :: astring
 type(string)              :: anotherstring
 type(string), allocatable :: strings(:)
 type(string)              :: line(3)
 integer                   :: iostat
 character(len=99)         :: iomsg
 integer                   :: scratch
 integer                   :: l
 logical                   :: test_passed(8)
 line(1) = ' Hello World!   '
 line(2) = 'How are you?  '
 line(3) = '   All say: "Fine thanks"'
 anotherstring = anotherstring%join(array=line, sep=new_line('a'))
 call anotherstring%write_file(file='write_file_test.tmp', iostat=iostat, iomsg=iomsg)
 call astring%read_file(file='write_file_test.tmp', iostat=iostat, iomsg=iomsg)
 call astring%split(tokens=strings, sep=new_line('a'))
 test_passed(1) = (size(strings, dim=1)==size(line, dim=1))
 do l=1, size(strings, dim=1)
   test_passed(l+1) = (strings(l)==line(l))
 enddo
 call anotherstring%write_file(file='write_file_test.tmp', form='unformatted', iostat=iostat, iomsg=iomsg)
 call astring%read_file(file='write_file_test.tmp', form='unformatted', iostat=iostat, iomsg=iomsg)
 call astring%split(tokens=strings, sep=new_line('a'))
 test_passed(5) = (size(strings, dim=1)==size(line, dim=1))
 do l=1, size(strings, dim=1)
   test_passed(l+5) = (strings(l)==line(l))
 enddo
 open(newunit=scratch, file='write_file_test.tmp')
 close(unit=scratch, status='delete')
 print '(L1)', all(test_passed)
endprogram volatile_doctest