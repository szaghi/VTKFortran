program volatile_doctest
use stringifor_string_t
 type(string) :: string1
 logical      :: test_passed(4)
 string1 = '/bar/foo.tar.bz2'
 test_passed(1) = string1%basedir()//''=='/bar'
 string1 = './bar/foo.tar.bz2'
 test_passed(2) = string1%basedir()//''=='./bar'
 string1 = 'bar/foo.tar.bz2'
 test_passed(3) = string1%basedir()//''=='bar'
 string1 = '\bar\foo.tar.bz2'
 test_passed(4) = string1%basedir(sep='\')//''=='\bar'
 print '(L1)', all(test_passed)
endprogram volatile_doctest