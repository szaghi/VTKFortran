!< StringiFor *csv naive parser* test.

program stringifor_test_csv_naive_parser
!< StringiFor *csv naive parser* test.
!<
!< This is an example of the usefulness of StringiFor.
use stringifor

implicit none
type(string)              :: csv            !< The CSV file as a single stream.
type(string), allocatable :: rows(:)        !< The CSV table rows.
type(string), allocatable :: columns(:)     !< The CSV table columns.
type(string), allocatable :: cells(:,:)     !< The CSV table cells.
type(string)              :: lines(4)       !< The CSV file lines.
type(string)              :: most_expensive !< The most expensive car.
real(R8P)                 :: highest_cost   !< The highest cost.
integer                   :: rows_number    !< The CSV file rows number.
integer                   :: columns_number !< The CSV file columns number.
integer                   :: r              !< Counter.
logical                   :: test_passed(1) !< List of passed tests.

test_passed = .false.

! a cars database
lines(1) = 'Year,Make,Model,Description,Price'
lines(2) = '1997,Ford,E350,ac abs moon,3000.00'
lines(3) = '1999,Chevy,Venture "Extended Edition", ,4900.00'
lines(4) = '1999,Chevy,Venture "Extended Edition Very Large", ,5000.00'

! preparing a CSV file test
call write_file(file='file_test_temp.csv', lines=lines)

! parsing the just created CSV file
call csv%read_file(file='file_test_temp.csv')    ! read the CSV file as a single stream
call csv%split(tokens=rows, sep=new_line('a'))   ! get the CSV file rows
rows_number = size(rows, dim=1)                  ! get the CSV file rows number
columns_number = rows(1)%count(',') + 1          ! get the CSV file columns number
allocate(cells(1:columns_number, 1:rows_number)) ! allocate the CSV file cells
do r=1, rows_number                              ! parse all cells
  call rows(r)%split(tokens=columns, sep=',')    ! get current columns
  cells(1:columns_number, r) = columns           ! save current columns into cells
enddo

! eliminating the file
open(newunit=r, file='file_test_temp.csv') ; close(unit=r, status='DELETE')

! now you can do whatever with your parsed data
! print the table in markdown syntax
print "(A)", 'A markdown-formatted table'
print "(A)", ''
print "(A)", '|'//csv%join(array=cells(:, 1), sep='|')//'|'
columns = '----' ! re-use columns for printing separators
print "(A)", '|'//csv%join(array=columns, sep='|')//'|'
do r=2, rows_number
  print "(A)", '|'//csv%join(array=cells(:, r), sep='|')//'|'
enddo
print "(A)", ''
! find the most expensive car
print "(A)", 'Searching for the most expensive car'
most_expensive = 'unknown'
highest_cost = -1._R8P
do r=2, rows_number
  if (cells(5, r)%to_number(kind=1._R8P)>=highest_cost) then
    highest_cost = cells(5, r)%to_number(kind=1._R8P)
    most_expensive = csv%join(array=[cells(2, r), cells(3, r)], sep=' ')
  endif
enddo
test_passed(1) = most_expensive//'' == 'Chevy Venture "Extended Edition Very Large"'
print "(A,L1)", 'The most expensive car is : '//most_expensive//', is correct? ', test_passed(1)

print "(A,L1)", new_line('a')//'Are all tests passed? ', all(test_passed)
endprogram stringifor_test_csv_naive_parser
