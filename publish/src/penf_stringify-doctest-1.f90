program volatile_doctest
use penf_stringify
 use penf
 character(len=:, kind=ASCII), allocatable :: string
 string = str_ascii('I was DEFAULT kind, but now I am ASCII')
 print "(A)", string
endprogram volatile_doctest