!< VTK_Fortran test: write PVTR file (parallel, partitioned rectilinear grid).
program vtk_fortran_write_pvtr
!< VTK_Fortran test: write PVTR file (parallel, partitioned rectilinear grid).
!<
!< A rectilinear grid of 4x2x2 cells split in two pieces along x, as two processes of a parallel code would write it: each
!< piece is a complete `.vtr` file with the coordinates of its own extent, the `.pvtr` file declares the data layout
!< (PCoordinates, PPointData, PCellData) and lists the pieces with their extents. The pieces share the plane i=2. The values
!< are checked by VTK's parallel reader, outside this test.
use penf
use vtk_fortran, only : vtk_file, pvtk_file

implicit none
integer(I4P), parameter :: nx1=0_I4P, nx2=4_I4P, ny1=0_I4P, ny2=2_I4P, nz1=0_I4P, nz2=2_I4P !< Whole extent.
integer(I4P), parameter :: nx1_p(2)=[nx1, 2_I4P]                                            !< Lower x extent of pieces.
integer(I4P), parameter :: nx2_p(2)=[2_I4P, nx2]                                            !< Upper x extent of pieces.
character(*), parameter :: pieces(2)=['vtkfortran_write_pvtr_01.vtr', &
                                      'vtkfortran_write_pvtr_02.vtr']                       !< Pieces file name.
integer(I4P)            :: errors(3)                                                        !< Errors of pieces and header.
integer(I4P)            :: p                                                                !< Counter.
logical                 :: test_passed(3)                                                   !< List of passed tests.

do p=1, size(pieces)
  errors(p) = write_piece(part=p, filename=pieces(p))
enddo
errors(3) = write_pvtr(filename='vtkfortran_write_pvtr.pvtr')
test_passed(1) = all(errors == 0)
test_passed(2) = count_text(filename='vtkfortran_write_pvtr.pvtr', text='<PCoordinates>') == 1 .and. &
                 count_text(filename='vtkfortran_write_pvtr.pvtr', text='GhostLevel="0"') == 1
test_passed(3) = count_text(filename='vtkfortran_write_pvtr.pvtr', text='<Piece ') == 2

print "(A,L1)", new_line('a')//'Are all tests passed? ', all(test_passed)
if (.not.all(test_passed)) error stop 'some tests failed'
stop
contains
  function write_piece(part, filename) result(error)
  !< Write one piece: the cells of x extent [nx1_p(part), nx2_p(part)], with point and cell data.
  integer(I4P), intent(in)  :: part       !< Piece number.
  character(*), intent(in)  :: filename   !< Output file name.
  integer(I4P)              :: error      !< Status error: the largest error of all calls.
  type(vtk_file)            :: a_vtk_file !< A VTK file.
  real(R8P), allocatable    :: x(:)       !< X coordinates.
  real(R8P)                 :: y(ny1:ny2) !< Y coordinates.
  real(R8P)                 :: z(nz1:nz2) !< Z coordinates.
  real(R8P), allocatable    :: xp(:)      !< X coordinate of each point (point data).
  integer(I4P), allocatable :: cell_id(:) !< Global x index of each cell (cell data).
  integer(I4P)              :: e(11)      !< Errors of each call.
  integer(I4P)              :: i, j, k, n !< Counters.

  allocate(x(nx1_p(part):nx2_p(part)))
  x = [(0.5_R8P*i, i=nx1_p(part), nx2_p(part))]
  y = [(1._R8P*j, j=ny1, ny2)]
  z = [(2._R8P*k, k=nz1, nz2)]
  allocate(xp(size(x)*size(y)*size(z)), cell_id((size(x)-1)*(size(y)-1)*(size(z)-1)))
  n = 0
  do k=nz1, nz2 ; do j=ny1, ny2 ; do i=nx1_p(part), nx2_p(part)
    n = n + 1 ; xp(n) = x(i)
  enddo ; enddo ; enddo
  n = 0
  do k=nz1, nz2-1 ; do j=ny1, ny2-1 ; do i=nx1_p(part), nx2_p(part)-1
    n = n + 1 ; cell_id(n) = i
  enddo ; enddo ; enddo
  e(1) = a_vtk_file%initialize(format='raw', filename=filename, mesh_topology='RectilinearGrid', &
                               nx1=nx1_p(part), nx2=nx2_p(part), ny1=ny1, ny2=ny2, nz1=nz1, nz2=nz2)
  e(2) = a_vtk_file%xml_writer%write_piece(nx1=nx1_p(part), nx2=nx2_p(part), ny1=ny1, ny2=ny2, nz1=nz1, nz2=nz2)
  e(3) = a_vtk_file%xml_writer%write_geo(x=x, y=y, z=z)
  e(4) = a_vtk_file%xml_writer%write_dataarray(location='node', action='open')
  e(5) = a_vtk_file%xml_writer%write_dataarray(data_name='x_coordinate', x=xp)
  e(6) = a_vtk_file%xml_writer%write_dataarray(location='node', action='close')
  e(7) = a_vtk_file%xml_writer%write_dataarray(location='cell', action='open')
  e(8) = a_vtk_file%xml_writer%write_dataarray(data_name='cell_id', x=cell_id)
  e(9) = a_vtk_file%xml_writer%write_dataarray(location='cell', action='close')
  e(10) = a_vtk_file%xml_writer%write_piece()
  e(11) = a_vtk_file%finalize()
  error = maxval(abs(e))
  endfunction write_piece

  function write_pvtr(filename) result(error)
  !< Write the parallel header: data layout and pieces with their extents.
  character(*), intent(in) :: filename    !< Output file name.
  integer(I4P)             :: error       !< Status error: the largest error of all calls.
  type(pvtk_file)          :: a_pvtk_file !< A parallel (partitioned) VTK file.
  integer(I4P)             :: e(10)       !< Errors of each call.

  e(1) = a_pvtk_file%initialize(filename=filename, mesh_topology='PRectilinearGrid', mesh_kind='Float64', &
                                nx1=nx1, nx2=nx2, ny1=ny1, ny2=ny2, nz1=nz1, nz2=nz2)
  e(2) = a_pvtk_file%xml_writer%write_dataarray(location='node', action='open')
  e(3) = a_pvtk_file%xml_writer%write_parallel_dataarray(data_name='x_coordinate', data_type='Float64', number_of_components=1)
  e(4) = a_pvtk_file%xml_writer%write_dataarray(location='node', action='close')
  e(5) = a_pvtk_file%xml_writer%write_dataarray(location='cell', action='open')
  e(6) = a_pvtk_file%xml_writer%write_parallel_dataarray(data_name='cell_id', data_type='Int32', number_of_components=1)
  e(7) = a_pvtk_file%xml_writer%write_dataarray(location='cell', action='close')
  e(8) = a_pvtk_file%xml_writer%write_parallel_geo(source=pieces(1), &
                                                   nx1=nx1_p(1), nx2=nx2_p(1), ny1=ny1, ny2=ny2, nz1=nz1, nz2=nz2)
  e(9) = a_pvtk_file%xml_writer%write_parallel_geo(source=pieces(2), &
                                                   nx1=nx1_p(2), nx2=nx2_p(2), ny1=ny1, ny2=ny2, nz1=nz1, nz2=nz2)
  e(10) = a_pvtk_file%finalize()
  error = maxval(abs(e))
  endfunction write_pvtr

  function count_text(filename, text) result(found)
  !< Count the lines of the file containing the text.
  character(*), intent(in) :: filename !< File name.
  character(*), intent(in) :: text     !< Expected text.
  integer(I4P)             :: found    !< Number of lines containing the text.
  character(len=1024)      :: line     !< Line buffer.
  integer(I4P)             :: u        !< File unit.
  integer(I4P)             :: iostat   !< IO status.

  found = 0
  open(newunit=u, file=filename, action='read')
  do
    read(u, '(A)', iostat=iostat) line
    if (iostat /= 0) exit
    if (index(line, text) > 0) found = found + 1
  enddo
  close(u)
  endfunction count_text
endprogram vtk_fortran_write_pvtr
