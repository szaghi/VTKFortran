!< FoXy test.
program create_tag
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

! create using xml_tag overloaded procedures
print "(A)", 'source'
source = '<first x="1" y="c" z="2">lorem ipsum...</first>'
print "(A)", source
print "(A)", 'created'
a_tag = xml_tag(name='first', content='lorem ipsum...', attributes=reshape([['x', '1'], ['y', 'c'], ['z', '2']], [2,3]))
parsed = a_tag%stringify()
test_passed(1) = trim(source)==trim(parsed)
print "(A,L1)", parsed//'Is correct? ', test_passed(1)

print "(A)", 'source'
source = '<second x="1" y="c" z="2"/>'
print "(A)", source
print "(A)", 'created'
a_tag = xml_tag(name='second', attributes=reshape([['x', '1'], ['y', 'c'], ['z', '2']], [2,3]), is_self_closing=.true.)
parsed = a_tag%stringify()
test_passed(2) = trim(source)==trim(parsed)
print "(A,L1)", parsed//' Is correct? ', test_passed(2)

! create parsing a source
print "(A)", 'source'
source = '<third x="1" y="c" z="2"/>'
print "(A)", source
print "(A)", 'created'
call a_tag%set(name='third')
call a_tag%parse(source=source)
parsed = a_tag%stringify()
test_passed(3) = trim(source)==trim(parsed)
print "(A,L1)", parsed//' Is correct? ', test_passed(3)

print "(A)", 'source'
source = '<fourth x="1" y="c" z="2"></fourth>'
print "(A)", source
print "(A)", 'created'
call a_tag%set(name='fourth')
call a_tag%parse(source=source)
parsed = a_tag%stringify()
test_passed(4) = trim(source)==trim(parsed)
print "(A,L1)", parsed//' Is correct? ', test_passed(4)

print "(A,L1)", new_line('a')//'Are all tests passed? ', all(test_passed)
stop
!-----------------------------------------------------------------------------------------------------------------------------------
endprogram create_tag
