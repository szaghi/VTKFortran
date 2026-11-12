!hdf5
!run vtkhdf_htg vtkhdf_htg
!render vtkhdf_htg quadtree.vtkhdf array=depth edges=1 camera=xy size=560x300 zoom=1.1
program vtkhdf_htg
!< A tree-based AMR in one VTKHDF file (a HyperTreeGrid): a root grid of 4x2 cells, each the root of a quadtree (branch
!< factor 2 in 2D: 4 children per refined cell), refined down to depth 4 around a circle. Each tree is given breadth
!< first: the refinement of its cells, depth by depth (the deepest depth left out, its cells all leaves), the children of a
!< cell x fastest. The depth of each cell is its cell data. The program prints the trees written.
use penf, only : I4P, R8P
use vtk_fortran, only : vtk_file
implicit none
integer(I4P), parameter :: nx=4, ny=2, max_depth=4    ! root cells, depths of a tree (0 to max_depth-1)
type(vtk_file)          :: a_vtk_file
integer(I4P)            :: i, j, error
integer(I4P), allocatable :: depths(:)                 ! the cell data: depth of each cell, tree after tree

!region write
error = a_vtk_file%initialize(format='vtkhdf', filename='quadtree.vtkhdf', mesh_topology='HyperTreeGrid', &
                              nx1=0, nx2=nx, ny1=0, ny2=ny, nz1=0, nz2=0, branch_factor=2)
error = a_vtk_file%xml_writer%write_piece(nx1=0, nx2=nx, ny1=0, ny2=ny, nz1=0, nz2=0)
error = a_vtk_file%xml_writer%write_geo(x=[(real(i, R8P), i=0, nx)], y=[(real(j, R8P), j=0, ny)], z=[0._R8P])
allocate(depths(0))
do j=0, ny - 1 ; do i=0, nx - 1 ! the tree of root cell (i, j): its id i + nx j
  error = a_vtk_file%xml_writer%write_tree(tree_id=i + nx * j, descriptor=tree(real(i, R8P), real(j, R8P)))
enddo ; enddo
error = a_vtk_file%xml_writer%write_dataarray(location='cell', action='open')
error = a_vtk_file%xml_writer%write_dataarray(data_name='depth', x=depths)
error = a_vtk_file%xml_writer%write_dataarray(location='cell', action='close')
error = a_vtk_file%xml_writer%write_piece()
error = a_vtk_file%finalize()
!endregion write
if (error /= 0) error stop 'write error'
print '(A,I0,A,I0,A)', 'quadtree.vtkhdf: ', nx * ny, ' trees, ', size(depths), ' cells'
print '(A,4(1X,I0))', '  cells of each depth:', (count(depths == i), i=0, max_depth - 1)
contains
  function tree(x0, y0) result(descriptor)
  !< The refinement of the tree of the root cell at (x0, y0), breadth first: a cell is refined when the circle of centre
  !< (2, 1) and radius 0.8 crosses it, down to depth max_depth-1; the depths of its cells appended to `depths`.
  real(R8P), intent(in) :: x0, y0         !< Lower corner of the root cell.
  logical, allocatable  :: descriptor(:)  !< Refinement of the cells, every depth but the deepest.
  real(R8P), allocatable :: corner(:,:)   !< Lower corners of the cells of a depth.
  real(R8P), allocatable :: next(:,:)     !< Lower corners of the cells of the next depth.
  real(R8P)              :: h             !< Size of the cells of a depth.
  logical, allocatable   :: refined(:)    !< Refinement of the cells of a depth.
  integer(I4P)           :: d, c          !< Depth, cell counters.

  allocate(descriptor(0))
  corner = reshape([x0, y0], [2, 1])
  h = 1._R8P
  do d=0, max_depth - 1
    depths = [depths, spread(d, 1, size(corner, 2))]
    if (d == max_depth - 1) exit ! the deepest depth: leaves, not in the descriptor
    refined = [(crosses(corner(:,c), h), c=1, size(corner, 2))]
    if (.not.any(refined)) exit  ! no refined cell: this depth is the deepest one
    descriptor = [descriptor, refined]
    ! the 4 children of every cell (x fastest), then those of the refined ones only
    next = reshape([([corner(:,c), corner(:,c) + [h/2, 0._R8P], corner(:,c) + [0._R8P, h/2], corner(:,c) + h/2], &
                     c=1, size(corner, 2))], [2, 4 * size(corner, 2)])
    corner = next(:, pack([(c, c=1, 4 * size(corner, 2))], [(spread(refined(c), 1, 4), c=1, size(corner, 2))]))
    h = h / 2
  enddo
  endfunction tree

  pure logical function crosses(corner, h)
  !< True if the circle of centre (2, 1) and radius 0.8 crosses the square of lower corner `corner` and side `h`.
  real(R8P), intent(in) :: corner(2), h
  real(R8P)             :: near(2), far(2)

  near = max(corner, min([2._R8P, 1._R8P], corner + h)) - [2._R8P, 1._R8P] ! nearest point of the square to the centre
  far = max(abs(corner - [2._R8P, 1._R8P]), abs(corner + h - [2._R8P, 1._R8P])) ! farthest corner offsets
  crosses = norm2(near) <= 0.8_R8P .and. norm2(far) >= 0.8_R8P
  endfunction crosses
endprogram vtkhdf_htg
