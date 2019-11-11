program volatile_doctest
use penf_stringify
 use penf
 character(128) :: b
 b = bstr(n=1._R16P)
 print "(A)", b(17:)
endprogram volatile_doctest