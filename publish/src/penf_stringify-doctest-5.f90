program volatile_doctest
use penf_stringify
 use penf
 character(len=:, kind=UCS4), allocatable :: string
 string = str_ascii(ASCII_'I was ASCII kind, but now I am UCS4')
 print "(A)", string
endprogram volatile_doctest