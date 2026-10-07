!< VTK_Fortran test: write a VTM (multi-block) file with nested blocks (issue #25).
program vtk_fortran_write_vtm_nested
!< VTK_Fortran test: write a VTM (multi-block) file with nested blocks (issue #25).
!<
!< The hierarchy of issue #25, blocks and datasets mixed at every level:
!<```
!< Block 0
!<   DataSet 0
!<   DataSet 1
!<   Block 2
!<     DataSet 0
!<     DataSet 1
!<   DataSet 3
!<   Block 4
!<     Block 0
!<       DataSet 0
!<     Block 1
!<       DataSet 0, 1, 2
!<```
!< The children of each block are indexed from 0 in the order they are written. The datasets all reference one small
!< unstructured grid file. The test checks the indexes written; the structure is checked by VTK readers, outside this test.
use penf
use vtk_fortran, only : vtk_file, vtm_file

implicit none
character(*), parameter :: piece='vtkfortran_write_vtm_nested_piece.vtu'       !< Dataset file.
integer(I4P), parameter :: expected(14)=[0, 0, 1, 2, 0, 1, 3, 4, 0, 0, 1, 0, 1, 2] !< Indexes, in order.
type(vtm_file)          :: a_vtm_file                                           !< A VTM file.
integer(I4P)            :: e(13)                                                !< Errors of each call.
logical                 :: test_passed(2)                                       !< List of passed tests.

call write_piece
e(1)  = a_vtm_file%initialize(filename='vtkfortran_write_vtm_nested.vtm')
e(2)  = a_vtm_file%write_block(action='open', name='assembly')
e(3)  = a_vtm_file%write_block(filenames=[piece, piece], names=['part-a', 'part-b'], action='write')
e(4)  = a_vtm_file%write_block(filenames=[piece, piece], names=['bolt-a', 'bolt-b'], name='bolts')
e(5)  = a_vtm_file%write_block(filenames=[piece], names=['part-c'], action='write')
e(6)  = a_vtm_file%write_block(action='open', name='sub-assembly')
e(7)  = a_vtm_file%write_block(filenames=[piece], names=['nut-a'], name='nuts')
e(8)  = a_vtm_file%write_block(action='open', name='washers')
e(9)  = a_vtm_file%write_block(action='write', filenames=piece//' '//piece//' '//piece, names='washer-a washer-b washer-c')
e(10) = a_vtm_file%write_block(action='close')
e(11) = a_vtm_file%write_block(action='close')
e(12) = a_vtm_file%write_block(action='close')
e(13) = a_vtm_file%finalize()
test_passed(1) = all(e == 0)
test_passed(2) = all(read_indexes('vtkfortran_write_vtm_nested.vtm', size(expected)) == expected)

print "(A,L1)", new_line('a')//'Are all tests passed? ', all(test_passed)
if (.not.all(test_passed)) error stop 'some tests failed'
stop
contains
  subroutine write_piece
  !< Write the dataset file: one tetrahedron.
  type(vtk_file) :: a_vtk_file !< A VTK file.
  integer(I4P)   :: error      !< Status error.

  error = a_vtk_file%initialize(format='ascii', filename=piece, mesh_topology='UnstructuredGrid')
  error = a_vtk_file%xml_writer%write_piece(np=4, nc=1)
  error = a_vtk_file%xml_writer%write_geo(np=4, nc=1, x=[0._R8P, 1._R8P, 0._R8P, 0._R8P], &
                                                      y=[0._R8P, 0._R8P, 1._R8P, 0._R8P], &
                                                      z=[0._R8P, 0._R8P, 0._R8P, 1._R8P])
  error = a_vtk_file%xml_writer%write_connectivity(nc=1, connectivity=[0_I4P, 1_I4P, 2_I4P, 3_I4P], offset=[4_I4P], &
                                                   cell_type=[10_I1P])
  error = a_vtk_file%xml_writer%write_piece()
  error = a_vtk_file%finalize()
  endsubroutine write_piece

  function read_indexes(filename, n) result(indexes)
  !< Read, in order, the `index` attributes of the first `n` Block and DataSet elements of a VTM file.
  character(*), intent(in) :: filename   !< File name.
  integer(I4P), intent(in) :: n          !< Number of indexes.
  integer(I4P)             :: indexes(n) !< Indexes read (-1 if missing).
  character(len=1024)      :: line       !< Line buffer.
  integer(I4P)             :: u          !< File unit.
  integer(I4P)             :: iostat     !< IO status.
  integer(I4P)             :: i          !< Counter.
  integer(I4P)             :: p          !< Position in the line.

  indexes = -1
  i = 0
  open(newunit=u, file=filename, action='read')
  do
    read(u, '(A)', iostat=iostat) line
    if (iostat /= 0 .or. i == n) exit
    if (index(line, '<Block ') == 0 .and. index(line, '<DataSet ') == 0) cycle
    p = index(line, 'index="')
    if (p == 0) cycle
    i = i + 1
    read(line(p+7:index(line(p+7:), '"')+p+5), *, iostat=iostat) indexes(i)
  enddo
  close(u)
  endfunction read_indexes
endprogram vtk_fortran_write_vtm_nested
