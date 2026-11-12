!< VTK_Fortran test: write PVD file (collection of datasets, time series).
program vtk_fortran_write_pvd
!< VTK_Fortran test: write PVD file (collection of datasets, time series).
!<
!< Three time steps of a one-cell mesh: the first two are collected in a new `.pvd` file, the third one is appended to it as a
!< restarted run would do. The test checks the content of the collection after each phase, the append to a file with content
!< after its closing tags, and the error cases. The same steps as a ParaView file series (`.series`, JSON): written,
!< continued, read back (files and times), a series written by hand without times (their positions), the refusals.
use penf
use vtk_fortran, only : pvd_file, vtk_file

implicit none
type(pvd_file)          :: pvd                                  !< A PVD file.
character(*), parameter :: pvd_name='vtkfortran_write_pvd.pvd'  !< PVD file name.
real(R8P),    parameter :: times(3)=[-0.5_R8P, 0.1_R8P, 0.25_R8P] !< Time steps (a negative one included).
character(len=32)       :: step_name(3)                         !< Dataset (step) file names.
integer(I4P)            :: error                                !< Status error.
integer(I4P)            :: s                                    !< Counter.
integer(I4P)            :: u                                    !< File unit.
logical                 :: test_passed(9)                       !< List of passed tests.

do s=1, 3
  step_name(s) = 'vtkfortran_write_pvd_'//trim(strz(s-1, 4))//'.vtu'
  call write_step(filename=trim(step_name(s)), value=times(s))
enddo

! new collection with the first two steps
error = pvd%initialize(filename=pvd_name)
do s=1, 2
  error = pvd%write_dataset(filename=trim(step_name(s)), timestep=times(s))
enddo
error = pvd%finalize()
test_passed(1) = count_datasets(pvd_name) == 2 .and. is_closed(pvd_name)

! restart: append the third step
error = pvd%initialize(filename=pvd_name, action='append')
error = pvd%write_dataset(filename=trim(step_name(3)), timestep=times(3))
error = pvd%finalize()
test_passed(2) = count_datasets(pvd_name) == 3 .and. is_closed(pvd_name) .and. &
                 has_line(pvd_name, '<DataSet timestep="0.25" part="0" file="'//trim(step_name(3))//'"/>')

! optional attributes
error = pvd%initialize(filename='vtkfortran_write_pvd_attributes.pvd')
error = pvd%write_dataset(filename=trim(step_name(1)), timestep=times(1), part=1, group='fluid', name='mesh')
error = pvd%finalize()
test_passed(3) = has_line('vtkfortran_write_pvd_attributes.pvd', &
                          '<DataSet timestep="-0.5" group="fluid" part="1" name="mesh" file="'//trim(step_name(1))//'"/>')

! append to a file with content after its closing tags (e.g. edited by hand): the content is dropped
open(newunit=u, file='vtkfortran_write_pvd_edited.pvd', action='write', status='replace')
write(u, '(A)') '<?xml version="1.0"?>'
write(u, '(A)') '<VTKFile type="Collection" version="0.1">'
write(u, '(A)') '<Collection>'
write(u, '(A)') '<DataSet timestep="0" part="0" file="'//trim(step_name(1))//'"/>'
write(u, '(A)') '</Collection>'
write(u, '(A)') '</VTKFile>'
write(u, '(A)') '<!-- a comment after the closing tags, longer than the closing tags written by the library -->'
close(u)
error = pvd%initialize(filename='vtkfortran_write_pvd_edited.pvd', action='append')
error = pvd%write_dataset(filename=trim(step_name(2)), timestep=times(2))
error = pvd%finalize()
test_passed(4) = count_datasets('vtkfortran_write_pvd_edited.pvd') == 2 .and. is_closed('vtkfortran_write_pvd_edited.pvd')

! errors: append to a missing file, write before initialize
test_passed(5) = pvd%initialize(filename='vtkfortran_write_pvd_missing.pvd', action='append') /= 0
test_passed(6) = pvd%write_dataset(filename='any.vtu', timestep=0._R8P) /= 0

! a ParaView file series (.series, JSON): new, continued, read back
error = pvd%initialize(filename='vtkfortran_write_pvd.vtu.series')
do s=1, 2
  error = error + pvd%write_dataset(filename=trim(step_name(s)), timestep=times(s))
enddo
error = error + pvd%finalize()
error = error + pvd%initialize(filename='vtkfortran_write_pvd.vtu.series', action='append')
error = error + pvd%write_dataset(filename=trim(step_name(3)), timestep=times(3))
error = error + pvd%write_dataset(filename='a "quoted" name.vtu', timestep=1._R8P)
error = error + pvd%finalize()
test_passed(7) = error == 0 .and. has_line('vtkfortran_write_pvd.vtu.series', '"file-series-version" : "1.0",') .and. &
                 has_line('vtkfortran_write_pvd.vtu.series', '{ "name" : "a '//achar(92)//'"quoted'//achar(92)// &
                          '" name.vtu", "time" : 1.0000000000000000E+000 }') ! (no backslash literal: nvfortran escapes it)
test_passed(7) = test_passed(7) .and. series_read('vtkfortran_write_pvd.vtu.series', [times, 1._R8P], &
                                                  [character(32) :: step_name, 'a "quoted" name.vtu'])
! a name with a tab (written \u0009, read back), the file read twice by the same object
error = pvd%initialize(filename='vtkfortran_write_pvd_tab.series')
error = error + pvd%write_dataset(filename='a'//achar(9)//'b.vtu', timestep=2._R8P)
error = error + pvd%finalize()
test_passed(7) = test_passed(7) .and. error == 0 .and. series_twice('vtkfortran_write_pvd_tab.series', 'a'//achar(9)//'b.vtu')
error = pvd%initialize(filename='vtkfortran_write_pvd_paraview.vtu.series') ! the three steps, read by ParaView outside
do s=1, 3
  error = error + pvd%write_dataset(filename=trim(step_name(s)), timestep=times(s))
enddo
error = error + pvd%finalize()
test_passed(7) = test_passed(7) .and. error == 0
! a series written by hand (ParaView's layout, a member more, no times): the times are the positions
open(newunit=u, file='vtkfortran_write_pvd_hand.series', action='write', status='replace')
write(u, '(A)') '{', '  "file-series-version" : "1.0",', '  "files" : [', &
                '    { "name" : "a.vtu", "size" : 10 },', '    { "name" : "b.vtu", "size" : 20 }', '  ]', '}'
close(u)
test_passed(8) = series_read('vtkfortran_write_pvd_hand.series', [0._R8P, 1._R8P], [character(5) :: 'a.vtu', 'b.vtu'])
! refusals: part, group, name; a file that is not a series
error = pvd%initialize(filename='vtkfortran_write_pvd_refused.series')
test_passed(9) = pvd%write_dataset(filename='a.vtu', timestep=0._R8P, part=1) /= 0
test_passed(9) = test_passed(9) .and. pvd%write_dataset(filename='a.vtu', timestep=0._R8P, group='g') /= 0
test_passed(9) = test_passed(9) .and. pvd%write_dataset(filename='a.vtu', timestep=0._R8P, name='n') /= 0
error = pvd%finalize()
test_passed(9) = test_passed(9) .and. pvd%initialize(filename=pvd_name//'.series', action='read') /= 0
test_passed(9) = test_passed(9) .and. pvd%initialize(filename='vtkfortran_write_pvd_missing.series', action='append') /= 0
error = pvd%finalize()

print "(A,L1)", new_line('a')//'Are all tests passed? ', all(test_passed)
if (.not.all(test_passed)) error stop 'some tests failed'
stop
contains
  subroutine write_step(filename, value)
  !< Write one time step: a tetrahedron with a point-centered value.
  character(*), intent(in) :: filename   !< Output file name.
  real(R8P),    intent(in) :: value      !< Value at points.
  type(vtk_file)           :: a_vtk_file !< A VTK file.

  error = a_vtk_file%initialize(format='ascii', filename=filename, mesh_topology='UnstructuredGrid')
  error = a_vtk_file%xml_writer%write_piece(np=4, nc=1)
  error = a_vtk_file%xml_writer%write_geo(np=4, nc=1, x=[0._R8P,1._R8P,0._R8P,0._R8P], y=[0._R8P,0._R8P,1._R8P,0._R8P], &
                                          z=[0._R8P,0._R8P,0._R8P,1._R8P])
  error = a_vtk_file%xml_writer%write_connectivity(nc=1, connectivity=[0_I4P,1_I4P,2_I4P,3_I4P], offset=[4_I4P], &
                                                   cell_type=[10_I1P])
  error = a_vtk_file%xml_writer%write_dataarray(location='node', action='open')
  error = a_vtk_file%xml_writer%write_dataarray(data_name='value', x=[value, value, value, value])
  error = a_vtk_file%xml_writer%write_dataarray(location='node', action='close')
  error = a_vtk_file%xml_writer%write_piece()
  error = a_vtk_file%finalize()
  endsubroutine write_step

  function count_datasets(filename) result(n)
  !< Count the DataSet entries of a collection.
  character(*), intent(in) :: filename !< File name.
  integer(I4P)             :: n        !< Number of datasets.
  character(len=1024)      :: line     !< Line buffer.
  integer(I4P)             :: v        !< File unit.
  integer(I4P)             :: iostat   !< IO status.

  n = 0
  open(newunit=v, file=filename, action='read')
  do
    read(v, '(A)', iostat=iostat) line
    if (iostat /= 0) exit
    if (index(line, '<DataSet ') > 0) n = n + 1
  enddo
  close(v)
  endfunction count_datasets

  function is_closed(filename) result(is_valid)
  !< Check that the collection ends with its closing tags, written once, and nothing after them.
  character(*), intent(in) :: filename !< File name.
  logical                  :: is_valid !< Check result.
  character(len=1024)      :: line     !< Line buffer.
  character(len=1024)      :: last(2)  !< Last two lines.
  integer(I4P)             :: n_close  !< Number of closing Collection tags.
  integer(I4P)             :: v        !< File unit.
  integer(I4P)             :: iostat   !< IO status.

  last = ''
  n_close = 0
  open(newunit=v, file=filename, action='read')
  do
    read(v, '(A)', iostat=iostat) line
    if (iostat /= 0) exit
    if (index(line, '</Collection>') > 0) n_close = n_close + 1
    last(1) = last(2) ; last(2) = line
  enddo
  close(v)
  is_valid = n_close == 1 .and. trim(adjustl(last(1))) == '</Collection>' .and. trim(adjustl(last(2))) == '</VTKFile>'
  endfunction is_closed

  function series_twice(filename, expected) result(is_passed)
  !< Read a `.series` of one entry twice with the same object: its file both times.
  character(*), intent(in)      :: filename !< File name.
  character(*), intent(in)      :: expected !< File of the entry.
  logical                       :: is_passed !< Check result.
  type(pvd_file)                :: reader   !< The file read.
  character(len=:), allocatable :: file(:)  !< Files read.
  integer(I4P)                  :: k, e     !< Counter, error.

  is_passed = .true.
  do k=1, 2
    e = reader%initialize(filename=filename, action='read')
    if (e == 0) e = reader%get_datasets(file=file)
    is_passed = is_passed .and. e == 0
    if (is_passed) is_passed = size(file) == 1
    if (is_passed) is_passed = file(1) == expected
  enddo
  e = reader%finalize()
  if (.not.is_passed) print '(A)', 'failed: reading twice '//filename
  endfunction series_twice

  function series_read(filename, times_expected, files_expected) result(is_passed)
  !< Read a `.series` file back: the times and files of its entries.
  character(*), intent(in)      :: filename          !< File name.
  real(R8P),    intent(in)      :: times_expected(:) !< Times expected.
  character(*), intent(in)      :: files_expected(:) !< Files expected.
  logical                       :: is_passed         !< Check result.
  type(pvd_file)                :: reader            !< The file read.
  real(R8P),        allocatable :: timestep(:)       !< Times read.
  integer(I4P),     allocatable :: part(:)           !< Parts read.
  character(len=:), allocatable :: file(:)           !< Files read.
  integer(I4P)                  :: e                 !< Error status.

  is_passed = .false.
  e = reader%initialize(filename=filename, action='read')
  if (e == 0) e = reader%get_datasets(timestep=timestep, part=part, file=file)
  if (e == 0) then
    is_passed = size(timestep) == size(times_expected) .and. size(file) == size(files_expected)
    if (is_passed) is_passed = all(timestep == times_expected) .and. all(file == files_expected) .and. all(part == 0)
  endif
  e = reader%finalize()
  if (.not.is_passed) print '(A)', 'failed: reading '//filename
  endfunction series_read

  function has_line(filename, expected) result(is_found)
  !< Check that the file contains a line made of the expected text (leading indentation apart).
  character(*), intent(in) :: filename !< File name.
  character(*), intent(in) :: expected !< Expected line.
  logical                  :: is_found !< Check result.
  character(len=1024)      :: line     !< Line buffer.
  integer(I4P)             :: v        !< File unit.
  integer(I4P)             :: iostat   !< IO status.

  is_found = .false.
  open(newunit=v, file=filename, action='read')
  do
    read(v, '(A)', iostat=iostat) line
    if (iostat /= 0) exit
    if (trim(adjustl(line)) == expected) is_found = .true.
  enddo
  close(v)
  endfunction has_line
endprogram vtk_fortran_write_pvd
