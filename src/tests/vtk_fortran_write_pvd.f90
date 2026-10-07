!< VTK_Fortran test: write PVD file (collection of datasets, time series).
program vtk_fortran_write_pvd
!< VTK_Fortran test: write PVD file (collection of datasets, time series).
!<
!< Three time steps of a one-cell mesh: the first two are collected in a new `.pvd` file, the third one is appended to it as a
!< restarted run would do. The test checks the content of the collection after each phase, the append to a file with content
!< after its closing tags, and the error cases.
use penf
use vtk_fortran, only : pvd_file, vtk_file

implicit none
type(pvd_file)          :: pvd                                  !< A PVD file.
character(*), parameter :: pvd_name='vtkfortran_write_pvd.pvd'  !< PVD file name.
real(R8P),    parameter :: times(3)=[0._R8P, 0.1_R8P, 0.25_R8P] !< Time steps.
character(len=32)       :: step_name(3)                         !< Dataset (step) file names.
integer(I4P)            :: error                                !< Status error.
integer(I4P)            :: s                                    !< Counter.
integer(I4P)            :: u                                    !< File unit.
logical                 :: test_passed(6)                       !< List of passed tests.

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
                          '<DataSet timestep="0.0" group="fluid" part="1" name="mesh" file="'//trim(step_name(1))//'"/>')

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

print "(A,L1)", new_line('a')//'Are all tests passed? ', all(test_passed)
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
