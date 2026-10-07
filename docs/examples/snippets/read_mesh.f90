program read_mesh
!< Read the points and the cells of an unstructured grid (here written by VTK, with Int64 ids and zlib compression).
use penf, only : I1P, I4P, R8P
use vtk_fortran, only : vtk_file
implicit none
type(vtk_file)            :: a_vtk_file
real(R8P),    allocatable :: x(:), y(:), z(:)
integer(I4P), allocatable :: connectivity(:), offset(:)
integer(I1P), allocatable :: cell_type(:)
integer(I4P)              :: i, error

error = a_vtk_file%initialize(filename='vtk_tetra.vtu', action='read')
error = a_vtk_file%xml_reader%read_geo(x=x, y=y, z=z)
! the ids are Int64 in the file: they read into I4P, since they fit
error = a_vtk_file%xml_reader%read_connectivity(connectivity=connectivity, offset=offset, cell_type=cell_type)
error = a_vtk_file%finalize()
do i=1, size(x)
  print '(A,I0,A,3F5.1)', 'point ', i - 1, ':', x(i), y(i), z(i)
enddo
print '(A,I0)', 'cell type ', cell_type(1)
print '(A,*(1X,I0))', 'its points', connectivity
endprogram read_mesh
