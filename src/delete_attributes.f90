!< FoXy test.
program delete_attributes
!-----------------------------------------------------------------------------------------------------------------------------------
!< FoXy test.
!-----------------------------------------------------------------------------------------------------------------------------------
use foxy, only: xml_tag
!-----------------------------------------------------------------------------------------------------------------------------------

!-----------------------------------------------------------------------------------------------------------------------------------
implicit none
character(len=:), allocatable :: source         !< String containing the source XML data.
character(len=:), allocatable :: parsed         !< String containing the parsed XML data.
type(xml_tag)                 :: a_tag          !< XML tag handler.
logical                       :: test_passed(4) !< List of passed tests.
!-----------------------------------------------------------------------------------------------------------------------------------

!-----------------------------------------------------------------------------------------------------------------------------------
test_passed = .false.

print "(A)", 'source'
source = '<first x="1" y="c" z="2">lorem ipsum...</first>'
print "(A)", source

print "(A)", 'delete "y" attribute'
a_tag = xml_tag(name='first', content='lorem ipsum...', attributes=reshape([['x', '1'], ['y', 'c'], ['z', '2']], [2,3]))
call a_tag%delete_attributes(name="y")
parsed = a_tag%stringify()
source = '<first x="1" z="2">lorem ipsum...</first>'
test_passed(1) = trim(adjustl(source))==trim(adjustl(parsed))
print "(A,L1)", parsed//' Is correct? ', test_passed(1)

print "(A)", 'delete "x" attribute'
a_tag = xml_tag(name='first', content='lorem ipsum...', attributes=reshape([['x', '1'], ['y', 'c'], ['z', '2']], [2,3]))
call a_tag%delete_attributes(name="x")
parsed = a_tag%stringify()
source = '<first y="c" z="2">lorem ipsum...</first>'
test_passed(2) = trim(adjustl(source))==trim(adjustl(parsed))
print "(A,L1)", parsed//' Is correct? ', test_passed(2)

print "(A)", 'delete "z" attribute'
a_tag = xml_tag(name='first', content='lorem ipsum...', attributes=reshape([['x', '1'], ['y', 'c'], ['z', '2']], [2,3]))
call a_tag%delete_attributes(name="z")
parsed = a_tag%stringify()
source = '<first x="1" y="c">lorem ipsum...</first>'
test_passed(3) = trim(adjustl(source))==trim(adjustl(parsed))
print "(A,L1)", parsed//' Is correct? ', test_passed(3)

print "(A)", 'delete "x" "z" attributes'
a_tag = xml_tag(name='first', content='lorem ipsum...', attributes=reshape([['x', '1'], ['y', 'c'], ['z', '2']], [2,3]))
call a_tag%delete_attributes(name=["z", "x"])
parsed = a_tag%stringify()
source = '<first y="c">lorem ipsum...</first>'
test_passed(4) = trim(adjustl(source))==trim(adjustl(parsed))
print "(A,L1)", parsed//' Is correct? ', test_passed(4)

print "(A,L1)", new_line('a')//'Are all tests passed? ', all(test_passed)
stop
!-----------------------------------------------------------------------------------------------------------------------------------
endprogram delete_attributes
