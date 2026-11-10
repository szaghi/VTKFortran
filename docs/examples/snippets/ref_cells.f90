program ref_cells
!< The linear cells of VTK, each on its own points, numbered in the order VTK defines for its type: the gallery of the
!< reference page "Topologies". The point array "local" holds the index of each point in its cell, the cell array "type"
!< the VTK code of the cell.
use penf, only : I1P, I4P, R8P
use vtk_fortran, only : vtk_file
implicit none
integer(I4P), parameter :: nc=14
real(R8P),    allocatable :: x(:), y(:), z(:)
integer(I4P), allocatable :: connect(:), local(:)
integer(I4P)            :: offset(nc), np, c, i, error
integer(I1P)            :: cell_type(nc)
type(vtk_file)          :: a_vtk_file

allocate(x(0), y(0), z(0), connect(0), local(0))
np = 0
! row 1: 0D, 1D and 2D cells
call add(1,  0, 0, [0.5_R8P], [0.5_R8P], [0._R8P])                                                ! vertex
call add(2,  1, 0, [0.1_R8P, 0.5_R8P, 0.9_R8P], [0.2_R8P, 0.8_R8P, 0.3_R8P], [0._R8P, 0._R8P, 0._R8P]) ! poly-vertex
call add(3,  2, 0, [0._R8P, 1._R8P], [0._R8P, 1._R8P], [0._R8P, 0._R8P])                             ! line
call add(4,  3, 0, [0._R8P, 0.35_R8P, 0.65_R8P, 1._R8P], [0._R8P, 1._R8P, 0._R8P, 1._R8P], &
         [0._R8P, 0._R8P, 0._R8P, 0._R8P])                                                       ! polyline
call add(5,  4, 0, [0._R8P, 1._R8P, 0.5_R8P], [0._R8P, 0._R8P, 1._R8P], [0._R8P, 0._R8P, 0._R8P])     ! triangle
! row 2: 2D cells and the tetrahedron
call add(6,  0, 1, [0._R8P, 0.4_R8P, 0.8_R8P, 1.2_R8P, 1.6_R8P], [0._R8P, 1._R8P, 0._R8P, 1._R8P, 0._R8P], &
         [0._R8P, 0._R8P, 0._R8P, 0._R8P, 0._R8P])                                                ! triangle strip
call add(7,  1, 1, [0.5_R8P, 1._R8P, 0.8_R8P, 0.2_R8P, 0._R8P], [0._R8P, 0.4_R8P, 1._R8P, 1._R8P, 0.4_R8P], &
         [0._R8P, 0._R8P, 0._R8P, 0._R8P, 0._R8P])                                                ! polygon
call add(8,  2, 1, [0._R8P, 1._R8P, 0._R8P, 1._R8P], [0._R8P, 0._R8P, 1._R8P, 1._R8P], [0._R8P, 0._R8P, 0._R8P, 0._R8P]) ! pixel
call add(9,  3, 1, [0._R8P, 1._R8P, 1._R8P, 0._R8P], [0._R8P, 0._R8P, 1._R8P, 1._R8P], [0._R8P, 0._R8P, 0._R8P, 0._R8P]) ! quad
call add(10, 4, 1, [0._R8P, 1._R8P, 0.5_R8P, 0.5_R8P], [0._R8P, 0._R8P, 0.9_R8P, 0.3_R8P], &
         [0._R8P, 0._R8P, 0._R8P, 0.9_R8P])                                                      ! tetra
! row 3: 3D cells
call add(11, 0, 2, [0._R8P, 1._R8P, 0._R8P, 1._R8P, 0._R8P, 1._R8P, 0._R8P, 1._R8P], &
         [0._R8P, 0._R8P, 1._R8P, 1._R8P, 0._R8P, 0._R8P, 1._R8P, 1._R8P], &
         [0._R8P, 0._R8P, 0._R8P, 0._R8P, 1._R8P, 1._R8P, 1._R8P, 1._R8P])                          ! voxel
call add(12, 1, 2, [0._R8P, 1._R8P, 1._R8P, 0._R8P, 0._R8P, 1._R8P, 1._R8P, 0._R8P], &
         [0._R8P, 0._R8P, 1._R8P, 1._R8P, 0._R8P, 0._R8P, 1._R8P, 1._R8P], &
         [0._R8P, 0._R8P, 0._R8P, 0._R8P, 1._R8P, 1._R8P, 1._R8P, 1._R8P])                          ! hexahedron
call add(13, 2, 2, [0._R8P, 1._R8P, 0.5_R8P, 0._R8P, 1._R8P, 0.5_R8P], [0._R8P, 0._R8P, 0.9_R8P, 0._R8P, 0._R8P, 0.9_R8P], &
         [0._R8P, 0._R8P, 0._R8P, 1._R8P, 1._R8P, 1._R8P])                                         ! wedge
call add(14, 3, 2, [0._R8P, 1._R8P, 1._R8P, 0._R8P, 0.5_R8P], [0._R8P, 0._R8P, 1._R8P, 1._R8P, 0.5_R8P], &
         [0._R8P, 0._R8P, 0._R8P, 0._R8P, 1._R8P])                                                 ! pyramid
error = a_vtk_file%initialize(format='ascii', filename='ref_cells.vtu', mesh_topology='UnstructuredGrid')
error = a_vtk_file%xml_writer%write_piece(np=np, nc=nc)
error = a_vtk_file%xml_writer%write_geo(np=np, nc=nc, x=x, y=y, z=z)
error = a_vtk_file%xml_writer%write_connectivity(nc=nc, connectivity=connect, offset=offset, cell_type=cell_type)
error = a_vtk_file%xml_writer%write_dataarray(location='node', action='open')
error = a_vtk_file%xml_writer%write_dataarray(data_name='local', x=local)
error = a_vtk_file%xml_writer%write_dataarray(location='node', action='close')
error = a_vtk_file%xml_writer%write_dataarray(location='cell', action='open')
error = a_vtk_file%xml_writer%write_dataarray(data_name='type', x=[(int(cell_type(c), I4P), c=1, nc)])
error = a_vtk_file%xml_writer%write_dataarray(location='cell', action='close')
error = a_vtk_file%xml_writer%write_piece()
error = a_vtk_file%finalize()
print '(A,I0,A,I0,A,I0)', 'ref_cells.vtu: ', nc, ' cells, ', np, ' points, error ', error
contains
  subroutine add(code, column, row, px, py, pz)
  !< Add a cell of the VTK type code, its points given in the order of the type, at a column and row of the gallery: the
  !< cells of 0 to 2 dimensions, given in the plane z=0, stand upright (in the plane y=0), facing the camera.
  integer,      intent(in) :: code, column, row
  real(R8P),    intent(in) :: px(:), py(:), pz(:)
  integer(I4P), save       :: ncell = 0
  integer(I4P)             :: n

  n = size(px)
  ncell = ncell + 1
  x = [x, px + 2.2_R8P*column]
  if (code < 10 .or. code == 8 .or. code == 9) then
    y = [y, pz] ; z = [z, py - 2.6_R8P*row]
  else
    y = [y, py] ; z = [z, pz - 2.6_R8P*row]
  endif
  connect = [connect, (np + i, i=0, n - 1)] ; local = [local, (i, i=0, n - 1)]
  np = np + n
  offset(ncell) = np
  cell_type(ncell) = int(code, I1P)
  endsubroutine add
endprogram ref_cells
