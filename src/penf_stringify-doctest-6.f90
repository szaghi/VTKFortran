program volatile_doctest
use penf_stringify
 use penf
 character(len=:, kind=UCS4), allocatable :: string
 string = str_ascii(UCS4_'I was UCS4 kind and I am still UCS4')
 print "(A)", string
endprogram volatile_doctest