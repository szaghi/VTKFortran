program volatile_doctest
use stringifor_string_t
 type(string)      :: astring
 type(string)      :: line(3)
 integer           :: iostat
 character(len=99) :: iomsg
 integer           :: scratch
 integer           :: l
 logical           :: test_passed(6)
 line(1) = ' Hello World!   '
 line(2) = 'How are you?  '
 line(3) = '   All say: "Fine thanks"'
 open(newunit=scratch, status='SCRATCH')
 write(scratch, "(A)") line(1)%chars()
 write(scratch, "(A)") line(2)%chars()
 write(scratch, "(A)") line(3)%chars()
 rewind(scratch)
 l = 0
 iostat = 0
 do
   l = l + 1
   call astring%read_line(unit=scratch, iostat=iostat, iomsg=iomsg)
   if (iostat/=0.and..not.is_iostat_eor(iostat)) then
     exit
   else
     test_passed(l) = (astring==line(l))
   endif
 enddo
 close(scratch)
 open(newunit=scratch, status='SCRATCH', form='UNFORMATTED', access='STREAM')
 write(scratch) line(1)%chars()//new_line('a')
 write(scratch) line(2)%chars()//new_line('a')
 write(scratch) line(3)%chars()//new_line('a')
 rewind(scratch)
 l = 0
 iostat = 0
 do
   l = l + 1
   call astring%read_line(unit=scratch, iostat=iostat, iomsg=iomsg, form='UnfORMatteD')
   if (iostat/=0.and..not.is_iostat_eor(iostat)) then
     exit
   else
     test_passed(l+3) = (astring==line(l))
   endif
 enddo
 close(scratch)
 print '(L1)', all(test_passed)
endprogram volatile_doctest