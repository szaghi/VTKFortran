program volatile_doctest
use stringifor_string_t
 type(string) :: astring
 logical :: test_passed(5)
 astring = 'bar/foo.tar.bz2'
 test_passed(1) = astring%basename()//''=='foo.tar.bz2'
 test_passed(2) = astring%basename(extension='.tar.bz2')//''=='foo'
 test_passed(3) = astring%basename(strip_last_extension=.true.)//''=='foo.tar'
 astring = '\bar\foo.tar.bz2'
 test_passed(4) = astring%basename(sep='\')//''=='foo.tar.bz2'
 astring = 'bar'
 test_passed(5) = astring%basename(strip_last_extension=.true.)//''=='bar'
 print '(L1)', all(test_passed)
endprogram volatile_doctest