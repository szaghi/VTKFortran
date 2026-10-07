program pvts
!< Write a structured grid split in two pieces, as two processes would, and the .pvts header that lists them.
use penf, only : I4P, R8P
use vtk_fortran, only : pvtk_file, vtk_file
implicit none
type(pvtk_file)               :: header
character(len=:), allocatable :: message
integer(I4P)                  :: error

! each process: a complete .vts with the extent of its piece (adjacent pieces share the plane i=3)
call write_piece('part_1.vts', 1, 3)
call write_piece('part_2.vts', 3, 5)
! one process: the header, with the layout of the data and the extent of each piece
error = header%initialize(filename='grid.pvts', mesh_topology='PStructuredGrid', mesh_kind='Float64', &
                          nx1=1, nx2=5, ny1=1, ny2=2, nz1=1, nz2=2)
error = header%xml_writer%write_dataarray(location='node', action='open')
error = header%xml_writer%write_parallel_dataarray(data_name='x', data_type='Float64', number_of_components=1)
error = header%xml_writer%write_dataarray(location='node', action='close')
error = header%xml_writer%write_parallel_geo(source='part_1.vts', nx1=1, nx2=3, ny1=1, ny2=2, nz1=1, nz2=2)
error = header%xml_writer%write_parallel_geo(source='part_2.vts', nx1=3, nx2=5, ny1=1, ny2=2, nz1=1, nz2=2)
error = header%finalize()
! check the pieces against the header
error = header%initialize(filename='grid.pvts', action='read')
error = header%xml_reader%check_pieces(message=message)
print '(A,I0)', 'grid.pvts: check_pieces error ', error
error = header%finalize()
contains
  subroutine write_piece(filename, i1, i2)
  !< Write the piece of the points i1...i2 along x.
  character(*), intent(in) :: filename
  integer(I4P), intent(in) :: i1, i2
  real(R8P)                :: x(i1:i2,2,2), y(i1:i2,2,2), z(i1:i2,2,2)
  type(vtk_file)           :: a_vtk_file
  integer(I4P)             :: i, error

  do i=i1, i2
    x(i,:,:) = i ; y(i,1,:) = 0 ; y(i,2,:) = 1 ; z(i,:,1) = 0 ; z(i,:,2) = 1
  enddo
  error = a_vtk_file%initialize(format='raw', filename=filename, mesh_topology='StructuredGrid', &
                                nx1=1, nx2=5, ny1=1, ny2=2, nz1=1, nz2=2)
  error = a_vtk_file%xml_writer%write_piece(nx1=i1, nx2=i2, ny1=1, ny2=2, nz1=1, nz2=2)
  error = a_vtk_file%xml_writer%write_geo(n=size(x), x=x, y=y, z=z)
  error = a_vtk_file%xml_writer%write_dataarray(location='node', action='open')
  error = a_vtk_file%xml_writer%write_dataarray(data_name='x', x=x, one_component=.true.)
  error = a_vtk_file%xml_writer%write_dataarray(location='node', action='close')
  error = a_vtk_file%xml_writer%write_piece()
  error = a_vtk_file%finalize()
  endsubroutine write_piece
endprogram pvts
