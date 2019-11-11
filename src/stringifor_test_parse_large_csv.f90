!< StringiFor `csv_naive_parser` test.
program stringifor_test_parse_large_csv
!< StringiFor `csv_naive_parser` test.
use stringifor

implicit none
type(string)                  :: csv            !< The CSV file as a single stream.
type(string), allocatable     :: rows(:)        !< The CSV table rows.
type(string), allocatable     :: columns(:)     !< The CSV table columns.
type(string), allocatable     :: cells(:,:)     !< The CSV table cells.
integer                       :: rows_number    !< The CSV file rows number.
integer                       :: columns_number !< The CSV file columns number.
integer                       :: r              !< Counter.
logical                       :: test_passed(1) !< List of passed tests.

test_passed = .false.

call csv%read_file(file='src/tests/stringifor/stringifor_test_parse_large_csv.csv', is_fast=.true.)
call csv%split_chunked(tokens=rows, sep=new_line('a'), chunks=10) ! get the CSV file rows
rows_number = size(rows, dim=1)                                   ! get the CSV file rows number
columns_number = rows(1)%count(',') + 1                           ! get the CSV file columns number
allocate(cells(1:columns_number, 1:rows_number))                  ! allocate the CSV file cells
do r=1, rows_number                                               ! parse all cells
  call rows(r)%split(tokens=columns, sep=',')                     ! get current columns
  cells(1:columns_number, r) = columns                            ! save current columns into cells
enddo

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

test_passed = .true.
print "(A,L1)", new_line('a')//'Are all tests passed? ', all(test_passed)
endprogram stringifor_test_parse_large_csv
