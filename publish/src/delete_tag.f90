!< FoXy test.
program delete_tag
!-----------------------------------------------------------------------------------------------------------------------------------
!< FoXy test.
!-----------------------------------------------------------------------------------------------------------------------------------
use foxy, only: xml_file, xml_tag
!-----------------------------------------------------------------------------------------------------------------------------------

!-----------------------------------------------------------------------------------------------------------------------------------
implicit none
character(len=:), allocatable :: source         !< String containing the source XML data.
character(len=:), allocatable :: parsed         !< String containing the parsed XML data.
type(xml_file)                :: a_file         !< XML tag handler.
type(xml_tag)                 :: a_tag          !< XML tag handler.
type(xml_tag)                 :: another_tag    !< XML tag handler.
logical                       :: test_passed(2) !< List of passed tests.
!-----------------------------------------------------------------------------------------------------------------------------------

!-----------------------------------------------------------------------------------------------------------------------------------
test_passed = .false.

print "(A)", 'source'
source = '<first x="1" y="c" z="2">lorem ipsum...</first>'//new_line('a')//&
         '<third>bye</third>'//new_line('a')//&
         '<fourth a="3">bye bye Mrs. Robinson</fourth>'
print "(A)", source
print "(A)", 'parsed'
call a_file%parse(string=source)
print "(A)", 'remove "third" tag'
source = '<first x="1" y="c" z="2">lorem ipsum...</first>'//new_line('a')//&
         '<fourth a="3">bye bye Mrs. Robinson</fourth>'
call a_file%delete_tag(name="third")
parsed = a_file%stringify()
test_passed(1) = trim(adjustl(source))==trim(adjustl(parsed))
print "(A,L1)", parsed//' Is correct? ', test_passed(1)

print "(A)", 'remove "first" tag'
source = '<fourth a="3">bye bye Mrs. Robinson</fourth>'
call a_file%delete_tag(name="first")
parsed = a_file%stringify()
test_passed(2) = trim(adjustl(source))==trim(adjustl(parsed))
print "(A,L1)", parsed//' Is correct? ', test_passed(2)

print "(A,L1)", new_line('a')//'Are all tests passed? ', all(test_passed)
stop
!-----------------------------------------------------------------------------------------------------------------------------------
endprogram delete_tag
