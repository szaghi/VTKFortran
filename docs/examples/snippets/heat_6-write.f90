function write_piece(p, filename) result(error)
!< Write the piece p: the cells i1...i2 along x it owns and one layer of ghost cells on each side.
integer(I4P), intent(in)  :: p
character(*), intent(in)  :: filename
integer(I4P)              :: error
type(vtk_file)            :: a_vtk_file
integer(I4P)              :: i1, i2, g1, g2, ni, np, nc, c, i, j, k, p0
real(R8P),    allocatable :: px(:), py(:), pz(:), tp(:)
integer(I4P), allocatable :: connect(:), offset(:), owner(:)
integer(I1P), allocatable :: cell_type(:), ghost(:)

i1 = (p - 1)*(n - 1)/pieces + 1 ; i2 = p*(n - 1)/pieces   ! owned cells along x
g1 = max(1, i1 - 1) ; g2 = min(n - 1, i2 + 1)             ! with the ghost cells
ni = g2 - g1 + 2 ; np = ni*n*n ; nc = (g2 - g1 + 1)*(n - 1)**2
allocate(px(np), py(np), pz(np), tp(np), connect(8*nc), offset(nc), owner(nc), cell_type(nc), ghost(nc))
do k=1, n ; do j=1, n ; do i=g1, g2 + 1
  p0 = id(i,j,k,g1,ni) + 1
  px(p0) = x(i) ; py(p0) = x(j) ; pz(p0) = x(k) ; tp(p0) = t(i,j,k)
enddo ; enddo ; enddo
c = 0
do k=1, n - 1 ; do j=1, n - 1 ; do i=g1, g2
  c = c + 1
  connect(8*c-7:8*c) = id([i, i+1, i+1, i, i, i+1, i+1, i], [j, j, j+1, j+1, j, j, j+1, j+1], &
                          [k, k, k, k, k+1, k+1, k+1, k+1], g1, ni) ! the VTK order of a hexahedron
  offset(c) = 8*c
  ghost(c) = merge(0_I1P, 1_I1P, i >= i1 .and. i <= i2) ! 1: a duplicate (ghost) cell, owned by a neighbour
  owner(c) = merge(p, merge(p - 1, p + 1, i < i1), ghost(c) == 0)
enddo ; enddo ; enddo
cell_type = 12_I1P
error = a_vtk_file%initialize(format='raw', filename=filename, mesh_topology='UnstructuredGrid', compressor='zlib')
error = a_vtk_file%xml_writer%write_piece(np=np, nc=nc)
error = a_vtk_file%xml_writer%write_geo(np=np, nc=nc, x=px, y=py, z=pz)
error = a_vtk_file%xml_writer%write_connectivity(nc=nc, connectivity=connect, offset=offset, cell_type=cell_type)
error = a_vtk_file%xml_writer%write_dataarray(location='node', action='open')
error = a_vtk_file%xml_writer%write_dataarray(data_name='temperature', x=tp)
error = a_vtk_file%xml_writer%write_dataarray(location='node', action='close')
error = a_vtk_file%xml_writer%write_dataarray(location='cell', action='open')
error = a_vtk_file%xml_writer%write_dataarray(data_name='piece', x=owner)
error = a_vtk_file%xml_writer%write_dataarray_unsigned(data_name='vtkGhostType', x=ghost)
error = a_vtk_file%xml_writer%write_dataarray(location='cell', action='close')
error = a_vtk_file%xml_writer%write_piece()
error = a_vtk_file%finalize()
endfunction write_piece
