program volatile_doctest
use stringifor_string_t
 type(string) :: astring
 character(len=:), allocatable :: alist_chr(:)
 integer, parameter :: Nf=5
 character(14) :: files(1:Nf)
 integer :: file_unit
 integer :: f
 integer :: ff
 logical :: test_passed
 do f=1, Nf
 files(f) = astring%tempname(prefix='foo-')
 open(newunit=file_unit, file=files(f))
 write(file_unit, *)f
 close(unit=file_unit)
 enddo
 call astring%glob(pattern='foo-*', list=alist_chr)
 do f=1, Nf
 open(newunit=file_unit, file=files(f))
 close(unit=file_unit, status='delete')
 enddo
 test_passed = .false.
 outer_chr: do f=1, size(alist_chr, dim=1)
 do ff=1, Nf
 test_passed = alist_chr(f) == files(ff)
 if (test_passed) cycle outer_chr
 enddo
 enddo outer_chr
 print '(L1)', test_passed
endprogram volatile_doctest