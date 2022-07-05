!< FoXy test.
program indent_tag
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
logical                       :: test_passed(2) !< List of passed tests.
!-----------------------------------------------------------------------------------------------------------------------------------

!-----------------------------------------------------------------------------------------------------------------------------------
test_passed = .false.

print "(A)", 'source'
source = '  <first>lorem ipsum...</first>'
print "(A)", source
print "(A)", 'created'
a_tag = xml_tag(name='first', content='lorem ipsum...', indent=2)
parsed = a_tag%stringify(is_indented=.true.)
test_passed(1) = trim(adjustl(source))==trim(adjustl(parsed))
print "(A,L1)", parsed//' Is correct? ', test_passed(1)

print "(A)", 'source'
source = '  <first>'//new_line('a')//          &
         '    lorem ipsum...'//new_line('a')// &
         '  </first>'
print "(A)", source
print "(A)", 'created'
a_tag = xml_tag(name='first', content='lorem ipsum...', indent=2)
parsed = a_tag%stringify(is_indented=.true., is_content_indented=.true.)
test_passed(2) = trim(adjustl(source))==trim(adjustl(parsed))
print "(A,L1)", parsed//' Is correct? ', test_passed(2)

print "(A,L1)", new_line('a')//'Are all tests passed? ', all(test_passed)
stop
!-----------------------------------------------------------------------------------------------------------------------------------
endprogram indent_tag
