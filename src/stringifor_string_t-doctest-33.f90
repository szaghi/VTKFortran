program volatile_doctest
use stringifor_string_t
 type(string)                  :: astring
 type(string),     allocatable :: alist_str(:)
 integer, parameter            :: Nf=5
 character(14)                 :: files(1:Nf)
 integer                       :: file_unit
 integer                       :: f
 integer                       :: ff
 logical                       :: test_passed

 do f=1, Nf
    files(f) = astring%tempname(prefix='foo-')
    open(newunit=file_unit, file=files(f))
    write(file_unit, *)f
    close(unit=file_unit)
 enddo
 call astring%glob(pattern='foo-*', list=alist_str)
 do f=1, Nf
    open(newunit=file_unit, file=files(f))
    close(unit=file_unit, status='delete')
 enddo
 test_passed = .false.
 outer_str: do f=1, size(alist_str, dim=1)
    do ff=1, Nf
       test_passed = alist_str(f) == files(ff)
       if (test_passed) cycle outer_str
    enddo
 enddo outer_str
 print '(L1)', test_passed
endprogram volatile_doctest