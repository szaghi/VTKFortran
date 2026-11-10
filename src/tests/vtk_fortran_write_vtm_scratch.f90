!< VTK_Fortran test: write a VTM (multi-block) file through scratch units, the files of each block added in any order.
program vtk_fortran_write_vtm_scratch
!< VTK_Fortran test: write a VTM (multi-block) file through scratch units, the files of each block added in any order.
!<
!< Each scratch unit collects the files of one block while they are written (e.g. at every time step, interleaved with
!< the other blocks); `finalize` writes the blocks. The test checks that block names with blanks and file names with
!< directories are written verbatim (they were cut at the first blank or slash by list-directed reads).
use penf
use vtk_fortran, only : vtm_file

implicit none
type(vtm_file)                :: a_vtm_file    !< A VTM file.
integer(I4P)                  :: e(7)          !< Errors of each call.
integer(I4P)                  :: s             !< Counter.
integer(I4P),     allocatable :: level(:)      !< Nesting level of the entries.
character(len=:), allocatable :: kind(:)       !< Kind of the entries.
character(len=:), allocatable :: name(:)       !< Name of the entries.
character(len=:), allocatable :: file(:)       !< File of the entries.
logical                       :: test_passed(2) !< List of passed tests.

e(1) = a_vtm_file%initialize(filename='vtkfortran_write_vtm_scratch.vtm', scratch_units_number=2)
e(2) = a_vtm_file%write_block(scratch=1, action='open', name='fluid domain')
e(3) = a_vtm_file%write_block(scratch=2, action='open', name='solid')
do s=1, 2
  e(4) = a_vtm_file%write_block(scratch=1, action='write', filename='out/fluid_'//trim(str(s, .true.))//'.vtu')
  e(5) = a_vtm_file%write_block(scratch=2, action='write', filename='solid_'//trim(str(s, .true.))//'.vtu')
enddo
e(6) = a_vtm_file%finalize()
test_passed(1) = all(e(1:6) == 0)

e(7) = a_vtm_file%initialize(filename='vtkfortran_write_vtm_scratch.vtm', action='read')
if (e(7) == 0) e(7) = a_vtm_file%get_entries(level=level, kind=kind, name=name, file=file)
test_passed(2) = .false.
if (e(7) == 0 .and. size(level) == 6) then
  test_passed(2) = all(level == [1, 2, 2, 1, 2, 2])                                                          .and. &
                   all(kind == [character(7) :: 'block', 'dataset', 'dataset', 'block', 'dataset', 'dataset']) .and. &
                   trim(name(1)) == 'fluid domain' .and. trim(name(4)) == 'solid'                             .and. &
                   trim(file(2)) == 'out/fluid_1.vtu' .and. trim(file(3)) == 'out/fluid_2.vtu'                 .and. &
                   trim(file(5)) == 'solid_1.vtu' .and. trim(file(6)) == 'solid_2.vtu'
endif
e(7) = a_vtm_file%finalize()

print "(A,L1)", new_line('a')//'Are all tests passed? ', all(test_passed)
if (.not.all(test_passed)) error stop 'some tests failed'
stop
endprogram vtk_fortran_write_vtm_scratch
