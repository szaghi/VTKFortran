!< VTK_Fortran test: write files with several pieces.
program vtk_fortran_write_multipiece
!< VTK_Fortran test: write files with several pieces.
!<
!< A file can hold several `<Piece>` elements: each one is opened and closed with `write_piece` and holds its own geometry,
!< connectivity and data, as a single-piece file does. Readers merge the pieces into one dataset. The test writes, in every
!< format, an unstructured grid of two tetrahedra (one per piece, with point ids local to the piece) and a rectilinear grid
!< split in two pieces along x (extents inside the whole extent); it checks the errors and the number of pieces. The values
!< are checked by VTK readers, outside this test.
use penf
use vtk_fortran, only : vtk_file

implicit none
character(*), parameter :: formats(4)=['ascii          ', 'binary         ', 'raw            ', 'binary-appended'] !< Formats.
type(vtk_file)          :: a_vtk_file     !< A VTK file.
integer(I4P)            :: error          !< Status error: the largest error of all calls.
integer(I4P)            :: f              !< Counter.
logical                 :: test_passed(3) !< List of passed tests.

error = 0
do f=1, size(formats)
  error = max(error, write_vtu(format=trim(formats(f)), filename='vtkfortran_write_multipiece-'//trim(formats(f))//'.vtu'))
  error = max(error, write_vtr(format=trim(formats(f)), filename='vtkfortran_write_multipiece-'//trim(formats(f))//'.vtr'))
enddo
test_passed(1) = error == 0
test_passed(2) = count_pieces('vtkfortran_write_multipiece-ascii.vtu') == 2
test_passed(3) = count_pieces('vtkfortran_write_multipiece-binary.vtr') == 2

print "(A,L1)", new_line('a')//'Are all tests passed? ', all(test_passed)
if (.not.all(test_passed)) error stop 'some tests failed'
stop
contains
  function write_vtu(format, filename) result(error)
  !< Write an unstructured grid of two pieces, one tetrahedron each.
  character(*), intent(in) :: format   !< File format.
  character(*), intent(in) :: filename !< Output file name.
  integer(I4P)             :: error    !< Status error: the largest error of all calls.
  integer(I4P)             :: p        !< Counter.

  error = abs(a_vtk_file%initialize(format=format, filename=filename, mesh_topology='UnstructuredGrid'))
  do p=1, 2
    ! each piece has its own points: point ids in the connectivity are local to the piece
    error = max(error, abs(a_vtk_file%xml_writer%write_piece(np=4, nc=1)))
    error = max(error, abs(a_vtk_file%xml_writer%write_geo(np=4, nc=1,                                       &
                                                           x=[0._R8P, 1._R8P, 0._R8P, 0._R8P] + 2._R8P*(p-1), &
                                                           y=[0._R8P, 0._R8P, 1._R8P, 0._R8P],                &
                                                           z=[0._R8P, 0._R8P, 0._R8P, 1._R8P])))
    error = max(error, abs(a_vtk_file%xml_writer%write_connectivity(nc=1, connectivity=[0_I4P, 1_I4P, 2_I4P, 3_I4P], &
                                                                    offset=[4_I4P], cell_type=[10_I1P])))
    error = max(error, abs(a_vtk_file%xml_writer%write_dataarray(location='node', action='open')))
    error = max(error, abs(a_vtk_file%xml_writer%write_dataarray(data_name='piece', x=[p, p, p, p])))
    error = max(error, abs(a_vtk_file%xml_writer%write_dataarray(location='node', action='close')))
    error = max(error, abs(a_vtk_file%xml_writer%write_dataarray(location='cell', action='open')))
    error = max(error, abs(a_vtk_file%xml_writer%write_dataarray(data_name='cell_piece', x=[p])))
    error = max(error, abs(a_vtk_file%xml_writer%write_dataarray(location='cell', action='close')))
    error = max(error, abs(a_vtk_file%xml_writer%write_piece()))
  enddo
  error = max(error, abs(a_vtk_file%finalize()))
  endfunction write_vtu

  function write_vtr(format, filename) result(error)
  !< Write a rectilinear grid of 4x1x1 cells (whole extent 0 4 0 1 0 1) in two pieces, x extents [0,2] and [2,4].
  character(*), intent(in) :: format   !< File format.
  character(*), intent(in) :: filename !< Output file name.
  integer(I4P)             :: error    !< Status error: the largest error of all calls.
  integer(I4P)             :: p        !< Counter.
  integer(I4P)             :: i        !< Counter.

  error = abs(a_vtk_file%initialize(format=format, filename=filename, mesh_topology='RectilinearGrid', &
                                    nx1=0, nx2=4, ny1=0, ny2=1, nz1=0, nz2=1))
  do p=1, 2
    error = max(error, abs(a_vtk_file%xml_writer%write_piece(nx1=2*(p-1), nx2=2*p, ny1=0, ny2=1, nz1=0, nz2=1)))
    error = max(error, abs(a_vtk_file%xml_writer%write_geo(x=[(real(i, R8P), i=2*(p-1), 2*p)], &
                                                           y=[0._R8P, 1._R8P], z=[0._R8P, 1._R8P])))
    error = max(error, abs(a_vtk_file%xml_writer%write_dataarray(location='cell', action='open')))
    error = max(error, abs(a_vtk_file%xml_writer%write_dataarray(data_name='cell_piece', x=[p, p])))
    error = max(error, abs(a_vtk_file%xml_writer%write_dataarray(location='cell', action='close')))
    error = max(error, abs(a_vtk_file%xml_writer%write_piece()))
  enddo
  error = max(error, abs(a_vtk_file%finalize()))
  endfunction write_vtr

  function count_pieces(filename) result(pieces)
  !< Count the `<Piece` elements of a (text) file.
  character(*), intent(in) :: filename !< File name.
  integer(I4P)             :: pieces   !< Number of pieces.
  character(len=1024)      :: line     !< Line buffer.
  integer(I4P)             :: u        !< File unit.
  integer(I4P)             :: iostat   !< IO status.

  pieces = 0
  open(newunit=u, file=filename, action='read')
  do
    read(u, '(A)', iostat=iostat) line
    if (iostat /= 0) exit
    if (index(line, '<Piece ') > 0) pieces = pieces + 1
  enddo
  close(u)
  endfunction count_pieces
endprogram vtk_fortran_write_multipiece
