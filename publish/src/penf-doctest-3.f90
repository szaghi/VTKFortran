program volatile_doctest
use penf
 use penf
 integer :: u
 open(newunit=u, status='scratch')
 call penf_print(u)
 close(u)
 print "(A)", 'done'
endprogram volatile_doctest