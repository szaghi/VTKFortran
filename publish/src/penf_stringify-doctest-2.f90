program volatile_doctest
use penf_stringify
 use penf
 character(len=:, kind=ASCII), allocatable :: string
 string = str_ascii('I was ASCII kind and I am still ASCII')
 print "(A)", string
endprogram volatile_doctest