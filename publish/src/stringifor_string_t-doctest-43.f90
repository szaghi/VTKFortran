program volatile_doctest
use stringifor_string_t
 type(string) :: astring
 type(string), allocatable :: strings(:)
 type(string) :: line(3)
 integer :: iostat
 character(len=99) :: iomsg
 integer :: scratch
 integer :: l
 logical :: test_passed(9)
 line(1) = ' Hello World!   '
 line(2) = 'How are you?  '
 line(3) = '   All say: "Fine thanks"'
 open(newunit=scratch, file='read_file_test.tmp')
 write(scratch, "(A)") line(1)%chars()
 write(scratch, "(A)") line(2)%chars()
 write(scratch, "(A)") line(3)%chars()
 close(scratch)
 call astring%read_file(file='read_file_test.tmp', iostat=iostat, iomsg=iomsg)
 call astring%split(tokens=strings, sep=new_line('a'))
 test_passed(1) = (size(strings, dim=1)==size(line, dim=1))
 do l=1, size(strings, dim=1)
 test_passed(l+1) = (strings(l)==line(l))
 enddo
 open(newunit=scratch, file='read_file_test.tmp', form='UNFORMATTED', access='STREAM')
 write(scratch) line(1)%chars()//new_line('a')
 write(scratch) line(2)%chars()//new_line('a')
 write(scratch) line(3)%chars()//new_line('a')
 close(scratch)
 call astring%read_file(file='read_file_test.tmp', form='unformatted', iostat=iostat, iomsg=iomsg)
 call astring%split(tokens=strings, sep=new_line('a'))
 test_passed(5) = (size(strings, dim=1)==size(line, dim=1))
 do l=1, size(strings, dim=1)
 test_passed(l+5) = (strings(l)==line(l))
 enddo
 open(newunit=scratch, file='read_file_test.tmp', form='UNFORMATTED', access='STREAM')
 close(scratch, status='DELETE')
 call astring%read_file(file='read_file_test.tmp', iostat=iostat)
 test_passed(9) = (iostat/=0)
 print '(L1)', all(test_passed)
endprogram volatile_doctest