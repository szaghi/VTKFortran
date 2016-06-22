  !< Implementation of geo-unstructured write of VTK file class.
  function write_geo_unst_data1_rank2_R8P(self, np, nc, xyz) result(error)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Write mesh with **UnstructuredGrid** topology (data 1, rank 2, R8P).
  !---------------------------------------------------------------------------------------------------------------------------------
  class(vtk_file), intent(inout) :: self       !< VTK file.
  integer(I4P),    intent(in)    :: np         !< Number of points.
  integer(I4P),    intent(in)    :: nc         !< Number of cells.
  real(R8P),       intent(in)    :: xyz(1:,1:) !< X, y, z coordinates [1:3,:].
  integer(I4P)                   :: error      !< Error status.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  if (np/=size(xyz, dim=2)) then
    self%error = 1
    return
  endif
  if (nc>np) then
    self%error = 2
    return
  endif
  call self%write_start_tag(tag_name='Points')
  error = self%write_dataarray(data_name='Points', x=xyz)
  call self%write_end_tag(tag_name='Points')
  error = self%error
  return
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction write_geo_unst_data1_rank2_R8P  

  function write_geo_unst_data1_rank2_R4P(self, np, nc, xyz) result(error)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Write mesh with **UnstructuredGrid** topology (data 1, rank 2, R4P).
  !---------------------------------------------------------------------------------------------------------------------------------
  class(vtk_file), intent(inout) :: self       !< VTK file.
  integer(I4P),    intent(in)    :: np         !< Number of points.
  integer(I4P),    intent(in)    :: nc         !< Number of cells.
  real(R4P),       intent(in)    :: xyz(1:,1:) !< X, y, z coordinates [1:3,:].
  integer(I4P)                   :: error      !< Error status.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  if (np/=size(xyz, dim=2)) then
    self%error = 1
    return
  endif
  if (nc>np) then
    self%error = 2
    return
  endif
  call self%write_start_tag(tag_name='Points')
  error = self%write_dataarray(data_name='Points', x=xyz)
  call self%write_end_tag(tag_name='Points')
  error = self%error
  return
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction write_geo_unst_data1_rank2_R4P    

  function write_geo_unst_data3_rank1_R8P(self, np, nc, x, y, z) result(error)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Write mesh with **UnstructuredGrid** topology (data 3, rank 1, R8P).
  !---------------------------------------------------------------------------------------------------------------------------------
  class(vtk_file), intent(inout) :: self  !< VTK file.
  integer(I4P),    intent(in)    :: np    !< Number of points.
  integer(I4P),    intent(in)    :: nc    !< Number of cells.
  real(R8P),       intent(in)    :: x(1:) !< X coordinates.
  real(R8P),       intent(in)    :: y(1:) !< Y coordinates.
  real(R8P),       intent(in)    :: z(1:) !< Z coordinates.
  integer(I4P)                   :: error !< Error status.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  if ((np/=size(x, dim=1)).or.(np/=size(y, dim=1)).or.(np/=size(z, dim=1))) then
    self%error = 1
    return
  endif
  if (nc>np) then
    self%error = 2
    return
  endif
  call self%write_start_tag(tag_name='Points')
  error = self%write_dataarray(data_name='Points', x=x, y=y, z=z)
  call self%write_end_tag(tag_name='Points')
  error = self%error
  return
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction write_geo_unst_data3_rank1_R8P

  function write_geo_unst_data3_rank1_R4P(self, np, nc, x, y, z) result(error)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Write mesh with **UnstructuredGrid** topology (data 3, rank 1, R4P).
  !---------------------------------------------------------------------------------------------------------------------------------
  class(vtk_file), intent(inout) :: self  !< VTK file.
  integer(I4P),    intent(in)    :: np    !< Number of points.
  integer(I4P),    intent(in)    :: nc    !< Number of cells.
  real(R4P),       intent(in)    :: x(1:) !< X coordinates.
  real(R4P),       intent(in)    :: y(1:) !< Y coordinates.
  real(R4P),       intent(in)    :: z(1:) !< Z coordinates.
  integer(I4P)                   :: error !< Error status.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  if ((np/=size(x, dim=1)).or.(np/=size(y, dim=1)).or.(np/=size(z, dim=1))) then
    self%error = 1
    return
  endif
  if (nc>np) then
    self%error = 2
    return
  endif
  call self%write_start_tag(tag_name='Points')
  error = self%write_dataarray(data_name='Points', x=x, y=y, z=z)
  call self%write_end_tag(tag_name='Points')
  error = self%error
  return
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction write_geo_unst_data3_rank1_R4P

  function write_connectivity(self, nc, connectivity, offset, cell_type) result(error)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Write mesh connectivity.
  !<
  !< **Must** be used when unstructured grid is used, it saves the connectivity of the unstructured gird.
  !< @note The vector **connect** must follow the VTK-XML standard. It is passed as *assumed-shape array*
  !< because its dimensions is related to the mesh dimensions in a complex way. Its dimensions can be calculated by the following
  !< equation: \(dc = \sum\limits_{i = 1}^{NC} {nvertex_i }\).
  !< Note that this equation is different from the legacy one. The XML connectivity convention is quite different from the
  !< legacy standard.
  !< As an example suppose we have a mesh composed by 2 cells, one hexahedron (8 vertices) and one pyramid with
  !< square basis (5 vertices) and suppose that the basis of pyramid is constitute by a face of the hexahedron and so the two cells
  !< share 4 vertices. The above equation gives \(dc=8+5=13\). The connectivity vector for this mesh can be:
  !<
  !<##### first cell
  !<+ connect(1)  = 0 identification flag of \(1^\circ\) vertex of first cell
  !<+ connect(2)  = 1 identification flag of \(2^\circ\) vertex of first cell
  !<+ connect(3)  = 2 identification flag of \(3^\circ\) vertex of first cell
  !<+ connect(4)  = 3 identification flag of \(4^\circ\) vertex of first cell
  !<+ connect(5)  = 4 identification flag of \(5^\circ\) vertex of first cell
  !<+ connect(6)  = 5 identification flag of \(6^\circ\) vertex of first cell
  !<+ connect(7)  = 6 identification flag of \(7^\circ\) vertex of first cell
  !<+ connect(8)  = 7 identification flag of \(8^\circ\) vertex of first cell
  !<
  !<##### second cell
  !<+ connect(9 ) = 0 identification flag of \(1^\circ\) vertex of second cell
  !<+ connect(10) = 1 identification flag of \(2^\circ\) vertex of second cell
  !<+ connect(11) = 2 identification flag of \(3^\circ\) vertex of second cell
  !<+ connect(12) = 3 identification flag of \(4^\circ\) vertex of second cell
  !<+ connect(13) = 8 identification flag of \(5^\circ\) vertex of second cell
  !<
  !< Therefore this connectivity vector convention is more simple than the legacy convention, now we must create also the
  !< *offset* vector that contains the data now missing in the *connect* vector. The offset
  !< vector for this mesh can be:
  !<
  !<##### first cell
  !<+ offset(1) = 8  => summ of nodes of \(1^\circ\) cell
  !<
  !<##### second cell
  !<+ offset(2) = 13 => summ of nodes of \(1^\circ\) and \(2^\circ\) cells
  !<
  !< The value of every cell-offset can be calculated by the following equation: \(offset_c=\sum\limits_{i=1}^{c}{nvertex_i}\)
  !< where \(offset_c\) is the value of \(c^{th}\) cell and \(nvertex_i\) is the number of vertices of \(i^{th}\) cell.
  !< The function VTK_CON_XML does not calculate the connectivity and offset vectors: it writes the connectivity and offset
  !< vectors conforming the VTK-XML standard, but does not calculate them.
  !< The vector variable *cell\_type* must conform the VTK-XML standard (see the file VTK-Standard at the
  !< Kitware homepage) that is the same of the legacy standard. It contains the
  !< *type* of each cells. For the above example this vector is:
  !<
  !<##### first cell
  !<+ cell\_type(1) = 12 hexahedron type of first cell
  !<
  !<##### second cell
  !<+ cell\_type(2) = 14 pyramid type of second cell
  !---------------------------------------------------------------------------------------------------------------------------------
  class(vtk_file), intent(inout) :: self             !< VTK file.
  integer(I4P),    intent(in)    :: nc               !< Number of cells.
  integer(I4P),    intent(in)    :: connectivity(1:) !< Mesh connectivity.
  integer(I4P),    intent(in)    :: offset(1:)       !< Cell offset.
  integer(I1P),    intent(in)    :: cell_type(1:)    !< VTK cell type.
  integer(I4P)                   :: error            !< Error status.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  call self%write_start_tag(tag_name='Cells')
  error = self%write_dataarray(data_name='connectivity', x=connectivity)
  error = self%write_dataarray(data_name='offsets', x=offset)
  error = self%write_dataarray(data_name='types', x=cell_type)
  call self%write_end_tag(tag_name='Cells')
  return
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction write_connectivity  
