!hdf5
!run vtkhdf_pieces vtkhdf_pieces
!render vtkhdf_pieces blocks.vtkhdf array=block categorical=1 edges=1 size=560x340 zoom=1.1
program vtkhdf_pieces
!< Write an unstructured mesh in pieces into one VTKHDF file: each piece is a partition of the file. Then print the
!< partitions as they are stored, read back with the HDF5 bindings of the library (VTKFortran does not read VTKHDF yet).
use penf, only : I1P, I4P, I8P, R8P
use vtk_fortran, only : vtk_file
use vtk_fortran_hdf5
implicit none
integer(I4P), parameter   :: np=18, nc=4 ! each block: 3x3x2 points, 2x2x1 hexahedra
type(vtk_file)            :: a_vtk_file
real(R8P)                 :: x(np), y(np), z(np)
integer(I4P)              :: connect(8*nc), offset(nc), b, c, error
integer(I1P)              :: cell_type(nc)
integer(hid_kind)         :: file_id, root
integer(I8P), allocatable :: values(:)

!region write
error = a_vtk_file%initialize(format='vtkhdf', filename='blocks.vtkhdf', mesh_topology='UnstructuredGrid')
do b=1, 3
  call block_mesh(b, x, y, z, connect, offset, cell_type) ! the hexahedra of block b, point ids from 0 in the block
  error = a_vtk_file%xml_writer%write_piece(np=np, nc=nc)
  error = a_vtk_file%xml_writer%write_geo(np=np, nc=nc, x=x, y=y, z=z)
  error = a_vtk_file%xml_writer%write_connectivity(nc=nc, connectivity=connect, offset=offset, cell_type=cell_type)
  error = a_vtk_file%xml_writer%write_dataarray(location='cell', action='open')
  error = a_vtk_file%xml_writer%write_dataarray(data_name='block', x=[(b, c=1, nc)])
  error = a_vtk_file%xml_writer%write_dataarray(location='cell', action='close')
  error = a_vtk_file%xml_writer%write_piece()
enddo
error = a_vtk_file%finalize()
!endregion write
if (error /= 0) error stop 'write error'

error = h5_file_open('blocks.vtkhdf', file_id)
if (error == 0) error = h5_group_open(file_id, 'VTKHDF', root)
if (error /= 0) error stop 'read error'
print '(A)', 'blocks.vtkhdf, one partition per piece:'
error = h5_dataset_read(root, 'NumberOfPoints', values)
print '(A,*(1X,I0))', '  NumberOfPoints         ', values
error = h5_dataset_read(root, 'NumberOfCells', values)
print '(A,*(1X,I0))', '  NumberOfCells          ', values
error = h5_dataset_read(root, 'NumberOfConnectivityIds', values)
print '(A,*(1X,I0))', '  NumberOfConnectivityIds', values
error = h5_dataset_read(root, 'Offsets', values)
print '(A,*(1X,I0))', '  Offsets                ', values
error = h5_group_close(root)
error = h5_file_close(file_id)
contains
  subroutine block_mesh(b, x, y, z, connect, offset, cell_type)
  !< The points and hexahedra of block b: 2x2x1 unit cells, the block moved by 2.5 along x from the previous one.
  integer(I4P), intent(in)  :: b
  real(R8P),    intent(out) :: x(:), y(:), z(:)
  integer(I4P), intent(out) :: connect(:), offset(:)
  integer(I1P), intent(out) :: cell_type(:)
  integer(I4P)              :: i, j, k, p, c, n0

  p = 0
  do k=0, 1 ; do j=0, 2 ; do i=0, 2
    p = p + 1 ; x(p) = 2.5_R8P * (b - 1) + i ; y(p) = j ; z(p) = k
  enddo ; enddo ; enddo
  c = 0
  do j=0, 1 ; do i=0, 1
    n0 = i + 3 * j ! the point of the cell at (i, j, 0), from 0
    connect(8*c+1:8*c+8) = [n0, n0 + 1, n0 + 4, n0 + 3, n0 + 9, n0 + 10, n0 + 13, n0 + 12]
    c = c + 1
  enddo ; enddo
  offset = [(8 * c, c=1, size(offset))]
  cell_type = 12_I1P
  endsubroutine block_mesh
endprogram vtkhdf_pieces
