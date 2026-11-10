program ref_formats
!< The examples of the reference page "Formats and large data": zlib compression, UInt64 headers, 64-bit counts and ids.
use penf, only : I1P, I4P, I8P, R8P
use vtk_fortran, only : vtk_file
implicit none
character(len=:), allocatable :: compressor, header_type
type(vtk_file)                :: a_vtk_file
integer(I4P)                  :: f, error
character(16), parameter      :: files(3) = [character(16) :: 'ref_zlib.vtu', 'ref_uint64.vtu', 'ref_i8p.vtu']

error = a_vtk_file%initialize(format='binary', filename='ref_zlib.vtu', mesh_topology='UnstructuredGrid', compressor='zlib')
call tetrahedron_i4p(a_vtk_file)
error = a_vtk_file%initialize(format='raw', filename='ref_uint64.vtu', mesh_topology='UnstructuredGrid', header_type='UInt64')
call tetrahedron_i4p(a_vtk_file)
error = a_vtk_file%initialize(format='raw', filename='ref_i8p.vtu', mesh_topology='UnstructuredGrid')
call tetrahedron_i8p(a_vtk_file)
do f=1, size(files)
  error = a_vtk_file%initialize(filename=trim(files(f)), action='read')
  if (error == 0) error = a_vtk_file%xml_reader%get_info(header_type=header_type, compressor=compressor)
  if (error /= 0) error stop 'read error'
  print '(A)', trim(files(f))//': header_type '//header_type//', compressor '//compressor
  error = a_vtk_file%finalize()
enddo
contains
  subroutine tetrahedron_i4p(a_vtk_file)
  !< Write one tetrahedron with I4P counts and ids, then finalize.
  type(vtk_file), intent(inout) :: a_vtk_file
  integer(I4P)                  :: error

  error = a_vtk_file%xml_writer%write_piece(np=4, nc=1)
  error = a_vtk_file%xml_writer%write_geo(np=4, nc=1, x=[0._R8P, 1._R8P, 0._R8P, 0._R8P], &
                                          y=[0._R8P, 0._R8P, 1._R8P, 0._R8P], z=[0._R8P, 0._R8P, 0._R8P, 1._R8P])
  error = a_vtk_file%xml_writer%write_connectivity(nc=1, connectivity=[0,1,2,3], offset=[4], cell_type=[10_I1P])
  error = a_vtk_file%xml_writer%write_piece()
  error = a_vtk_file%finalize()
  endsubroutine tetrahedron_i4p

  subroutine tetrahedron_i8p(a_vtk_file)
  !< Write one tetrahedron with I8P counts and ids, then finalize.
  type(vtk_file), intent(inout) :: a_vtk_file
  integer(I4P)                  :: error
  real(R8P)                     :: x(4), y(4), z(4)
  integer(I1P)                  :: cell_type(1)
  integer(I8P) :: np, nc, connect(4), offset(1)   ! allocatable in a real code

  x = [0, 1, 0, 0] ; y = [0, 0, 1, 0] ; z = [0, 0, 0, 1]
  np = 4 ; nc = 1 ; connect = [0, 1, 2, 3] ; offset = [4] ; cell_type = [10_I1P]
  error = a_vtk_file%xml_writer%write_piece(np=np, nc=nc)
  error = a_vtk_file%xml_writer%write_geo(np=np, nc=nc, x=x, y=y, z=z)
  error = a_vtk_file%xml_writer%write_connectivity(nc=nc, connectivity=connect, offset=offset, cell_type=cell_type)
  error = a_vtk_file%xml_writer%write_piece()
  error = a_vtk_file%finalize()
  endsubroutine tetrahedron_i8p
endprogram ref_formats
