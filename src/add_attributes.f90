!< FoXy test.
program add_attributes
!-----------------------------------------------------------------------------------------------------------------------------------
!< FoXy test.
!-----------------------------------------------------------------------------------------------------------------------------------
use foxy, only: xml_file, xml_tag
!-----------------------------------------------------------------------------------------------------------------------------------

!-----------------------------------------------------------------------------------------------------------------------------------
implicit none
character(len=:), allocatable :: source         !< String containing the source XML data.
character(len=:), allocatable :: parsed         !< String containing the parsed XML data.
type(xml_tag)                 :: a_tag          !< XML tag handler.
logical                       :: test_passed(3) !< List of passed tests.
!-----------------------------------------------------------------------------------------------------------------------------------

!-----------------------------------------------------------------------------------------------------------------------------------
test_passed = .false.

print "(A)", 'source'
source = '<first x="1" y="c" z="2">lorem ipsum...</first>'
print "(A)", source
print "(A)", 'created'
a_tag = xml_tag(name='first', content='lorem ipsum...', attributes=reshape([['x', '1'], ['y', 'c'], ['z', '2']], [2,3]))

parsed = a_tag%stringify()
test_passed(1) = trim(adjustl(source))==trim(adjustl(parsed))
print "(A,L1)", parsed//' Is correct? ', test_passed(1)

call a_tag%add_attributes(attributes_stream='y="3" a="one" b = "two" cc="tree"')
print "(A)", 'source'
source = '<first x="1" y="3" z="2" a="one" b="two" cc="tree">lorem ipsum...</first>'
print "(A)", source
print "(A)", 'created'
parsed = a_tag%stringify()
test_passed(2) = trim(adjustl(source))==trim(adjustl(parsed))
print "(A,L1)", parsed//' Is correct? ', test_passed(2)

call a_tag%add_attributes(attributes_stream='')
print "(A)", 'source'
source = '<first x="1" y="3" z="2" a="one" b="two" cc="tree">lorem ipsum...</first>'
print "(A)", source
print "(A)", 'created'
parsed = a_tag%stringify()
test_passed(3) = trim(adjustl(source))==trim(adjustl(parsed))
print "(A,L1)", parsed//' Is correct? ', test_passed(3)

print "(A,L1)", new_line('a')//'Are all tests passed? ', all(test_passed)
stop
!-----------------------------------------------------------------------------------------------------------------------------------
endprogram add_attributes
