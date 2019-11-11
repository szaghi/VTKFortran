!< FoXy test.
program add_tag
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
logical                       :: test_passed(1) !< List of passed tests.
!-----------------------------------------------------------------------------------------------------------------------------------

!-----------------------------------------------------------------------------------------------------------------------------------
test_passed = .false.

print "(A)", 'source'
source = '<first x="1" y="c" z="2">lorem ipsum...</first>'//new_line('a')//&
         '<second a1="2"/>'//new_line('a')//&
         '<third>bye</third>'//new_line('a')//&
         '<fourth a="3">bye bye Mrs. Robinson</fourth>'//new_line('a')//&
         '<fift>'//new_line('a')//&
         '  <nested l="1">I am supported! Nested tag at level 1</nested>'//new_line('a')//&
         '</fift>'
print "(A)", source
print "(A)", 'created'
a_tag = xml_tag(name='first', content='lorem ipsum...', attributes=reshape([['x', '1'], ['y', 'c'], ['z', '2']], [2,3]))
call a_file%add_tag(tag=a_tag)
a_tag = xml_tag(name='second', attribute=['a1', '2 '], is_self_closing=.true., sanitize_attributes_value=.true.)
call a_file%add_tag(tag=a_tag)
a_tag = xml_tag(name='third', content='bye')
call a_file%add_tag(tag=a_tag)
a_tag = xml_tag(name='fourth', content='bye bye Mrs. Robinson', attribute=['a', '3'])
call a_file%add_tag(tag=a_tag)
another_tag = xml_tag(name='nested', content='I am supported! Nested tag at level 1', attribute=['l', '1'])
a_tag = xml_tag(name='fift', content=another_tag, is_content_indented=.true.)
call a_file%add_tag(tag=a_tag)

parsed = a_file%stringify()
test_passed(1) = trim(adjustl(source))==trim(adjustl(parsed))
print "(A,L1)", parsed//' Is correct? ', test_passed(1)

print "(A,L1)", new_line('a')//'Are all tests passed? ', all(test_passed)
stop
!-----------------------------------------------------------------------------------------------------------------------------------
endprogram add_tag
