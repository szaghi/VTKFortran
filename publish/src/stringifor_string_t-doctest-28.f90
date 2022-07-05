program volatile_doctest
use stringifor_string_t
 type(string) :: astring
 astring = 'say all Hello WorLD!'
 print '(L1)', astring%colorize(color_fg='red')=='[31msay all Hello WorLD![0m'
endprogram volatile_doctest