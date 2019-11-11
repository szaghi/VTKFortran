!< FoXy test.
program parse_file_simple
!< FoXy test.
use foxy, only: xml_file

implicit none
character(len=:), allocatable :: source         !< String containing the source XML data.
character(len=:), allocatable :: parsed         !< String containing the parsed XML data.
type(xml_file)                :: xfile          !< XML file handler.
integer                       :: xunit          !< XML file unit.
logical                       :: test_passed(1) !< List of passed tests.

test_passed = .false.

print "(A)", 'Input XML data:'
source = '<first x="1" y="c" z="2">lorem ipsum...</first>'//new_line('a')//&
         '<second a1="2"/>'//new_line('a')//&
         '<third>bye</third>'//new_line('a')//&
         '<fourth a="3">bye bye Mrs. Robinson</fourth>'//new_line('a')//&
         '<fift>'//new_line('a')//&
         '  <nested level="1">I am supported! Nested tag at level 1</nested>'//new_line('a')//&
         '  <nested2 level="1">'//new_line('a')//&
         '    <nested3 level="2">Nested tag at level 2</nested3>'//new_line('a')//&
         '  </nested2>'//new_line('a')//&
         '</fift>'
print "(A)", source
open(newunit=xunit, file='parse_file_simple.xml', access='STREAM', form='UNFORMATTED')
write(unit=xunit)source
close(unit=xunit)

print "(A)", 'Parsing file'
call xfile%parse(filename='parse_file_simple.xml')
print "(A)", 'Parsed data'
parsed = xfile%stringify()
print "(A)", parsed
test_passed(1) = trim(source)==trim(parsed)
print "(A,L1)", 'Is parsed data correct? ', test_passed(1)

open(newunit=xunit, file='parse_file_simple.xml')
close(unit=xunit, status='DELETE')

print "(A,L1)", new_line('a')//'Are all tests passed? ', all(test_passed)
endprogram parse_file_simple
