program volatile_doctest
use befor64
 use befor64
 use penf
 real(R16P) :: array_R16(1:2)
 call b64_decode(code='AAAAAABAXkCPwvUoXI8CQA==',n=array_R16)
 print "(L1)", str(n=array_R16)==str(n=[121._R16P,2.32_R16P])
endprogram volatile_doctest