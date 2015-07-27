!< CON_XML interface definition for Lib_VTK_IO.
module Lib_VTK_IO_CON_XML
!-----------------------------------------------------------------------------------------------------------------------------------
!< CON_XML interface definition for Lib_VTK_IO.
!-----------------------------------------------------------------------------------------------------------------------------------
USE IR_Precision        ! Integers and reals precision definition.
USE Lib_Base64          ! Base64 encoding/decoding procedures.
USE Lib_VTK_IO_Back_End ! Lib_VTK_IO back end module.
!-----------------------------------------------------------------------------------------------------------------------------------

!-----------------------------------------------------------------------------------------------------------------------------------
implicit none
private
save
public:: VTK_CON_XML
!-----------------------------------------------------------------------------------------------------------------------------------
contains
  function VTK_CON_XML(NC,connect,offset,cell_type,idx,cf) result(E_IO)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Function for saving mesh connectivity.
  !<
  !< Function that **must** be used when unstructured grid is used, it saves the connectivity of the unstructured gird.
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
  implicit none
  integer(I4P), intent(IN)::           NC            !< Number of cells.
  integer(I4P), intent(IN)::           connect(1:)   !< Mesh connectivity.
  integer(I4P), intent(IN)::           offset(1:NC)  !< Cell offset.
  integer(I1P), intent(IN)::           cell_type(1:) !< VTK cell type.
  integer(I1P), intent(IN), optional:: idx           !< Id offset to convert Fortran (first id 1) to C (first id 0) standards.
  integer(I4P), intent(IN), optional:: cf            !< Current file index (for concurrent files IO).
  integer(I4P)::                       E_IO          !< Input/Output inquiring flag: $0$ if IO is done, $> 0$ if IO is not done.
  character(len=maxlen)::              s_buffer      !< Buffer string.
  integer(I1P), allocatable::          cocp(:)       !< Packed data.
  character(len=:), allocatable::      coc64         !< Data encoded in base64.
  integer(I1P)::                       incr          !< Actual id offset increment.
  integer(I4P)::                       rf            !< Real file index.
  integer(I4P)::                       n1            !< Counter.
  integer(I8P)::                       Ncocp         !< Dimension of cocp, packed data.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  E_IO = -1_I4P
  rf = f
  if (present(cf)) then
    rf = cf ; f = cf
  endif
  incr = 0_I1P
  if (present(idx)) then
    incr = idx
  endif
  select case(vtk(rf)%f)
  case(ascii)
    write(unit=vtk(rf)%u,fmt='(A)',iostat=E_IO)repeat(' ',vtk(rf)%indent)//'<Cells>' ; vtk(rf)%indent = vtk(rf)%indent + 2
    write(unit=vtk(rf)%u,fmt='(A)',iostat=E_IO)repeat(' ',vtk(rf)%indent)//&
                                               '<DataArray type="Int32" Name="connectivity" format="ascii">'
    write(unit=vtk(rf)%u,fmt=FI4P, iostat=E_IO)(connect(n1)+incr,n1=1,offset(NC))
    write(unit=vtk(rf)%u,fmt='(A)',iostat=E_IO)repeat(' ',vtk(rf)%indent)//'</DataArray>'
    write(unit=vtk(rf)%u,fmt='(A)',iostat=E_IO)repeat(' ',vtk(rf)%indent)//'<DataArray type="Int32" Name="offsets" format="ascii">'
    write(unit=vtk(rf)%u,fmt=FI4P, iostat=E_IO)(offset(n1),n1=1,NC)
    write(unit=vtk(rf)%u,fmt='(A)',iostat=E_IO)repeat(' ',vtk(rf)%indent)//'</DataArray>'
    write(unit=vtk(rf)%u,fmt='(A)',iostat=E_IO)repeat(' ',vtk(rf)%indent)//'<DataArray type="Int8" Name="types" format="ascii">'
    if (lbound(cell_type,dim=1)==ubound(cell_type,dim=1)) then
      write(unit=vtk(rf)%u,fmt=FI1P, iostat=E_IO)(cell_type(1),n1=1,NC)
    else
      write(unit=vtk(rf)%u,fmt=FI1P, iostat=E_IO)(cell_type(n1),n1=1,NC)
    endif
    write(unit=vtk(rf)%u,fmt='(A)',iostat=E_IO)repeat(' ',vtk(rf)%indent)//'</DataArray>' ; vtk(rf)%indent = vtk(rf)%indent - 2
    write(unit=vtk(rf)%u,fmt='(A)',iostat=E_IO)repeat(' ',vtk(rf)%indent)//'</Cells>'
  case(raw,bin_app)
    write(unit=vtk(rf)%u,iostat=E_IO)repeat(' ',vtk(rf)%indent)//'<Cells>'//end_rec ; vtk(rf)%indent = vtk(rf)%indent + 2
    s_buffer = repeat(' ',vtk(rf)%indent)//'<DataArray type="Int32" Name="connectivity" format="appended" offset="'// &
               trim(str(.true.,vtk(rf)%ioffset))//'"/>'
    write(unit=vtk(rf)%u,iostat=E_IO)trim(s_buffer)//end_rec
    call vtk(rf)%byte_update(N_Byte = offset(NC)*BYI4P)
    write(unit=vtk(rf)%ua,iostat=E_IO)vtk(rf)%N_Byte,'I4',offset(NC)
    write(unit=vtk(rf)%ua,iostat=E_IO)(connect(n1)+incr,n1=1,offset(NC))
    s_buffer = repeat(' ',vtk(rf)%indent)//'<DataArray type="Int32" Name="offsets" format="appended" offset="'// &
               trim(str(.true.,vtk(rf)%ioffset))//'"/>'
    write(unit=vtk(rf)%u,iostat=E_IO)trim(s_buffer)//end_rec
    call vtk(rf)%byte_update(N_Byte = NC*BYI4P)
    write(unit=vtk(rf)%ua,iostat=E_IO)vtk(rf)%N_Byte,'I4',NC
    write(unit=vtk(rf)%ua,iostat=E_IO)(offset(n1),n1=1,NC)
    s_buffer = repeat(' ',vtk(rf)%indent)//'<DataArray type="Int8" Name="types" format="appended" offset="'// &
               trim(str(.true.,vtk(rf)%ioffset))//'"/>'
    write(unit=vtk(rf)%u,iostat=E_IO)trim(s_buffer)//end_rec
    call vtk(rf)%byte_update(N_Byte = NC*BYI1P)
    write(unit=vtk(rf)%ua,iostat=E_IO)vtk(rf)%N_Byte,'I1',NC
    if (lbound(cell_type,dim=1)==ubound(cell_type,dim=1)) then
      write(unit=vtk(rf)%ua,iostat=E_IO)(cell_type(1),n1=1,NC)
    else
      write(unit=vtk(rf)%ua,iostat=E_IO)(cell_type(n1),n1=1,NC)
    endif
    vtk(rf)%indent = vtk(rf)%indent - 2 ; write(unit=vtk(rf)%u,iostat=E_IO)repeat(' ',vtk(rf)%indent)//'</Cells>'//end_rec
  case(binary)
    write(unit=vtk(rf)%u,iostat=E_IO)repeat(' ',vtk(rf)%indent)//'<Cells>'//end_rec ; vtk(rf)%indent = vtk(rf)%indent + 2
    write(unit=vtk(rf)%u,iostat=E_IO)repeat(' ',vtk(rf)%indent)//&
                                     '<DataArray type="Int32" Name="connectivity" format="binary">'//end_rec
    Ncocp=size(transfer([int(offset(NC)*BYI4P,I4P),connect],cocp),kind=I8P)
    if (allocated(cocp)) deallocate(cocp) ; allocate(cocp(1:Ncocp))
    cocp = transfer([int(offset(NC)*BYI4P,I4P),connect],cocp)
    call b64_encode(n=cocp,code=coc64)
    deallocate(cocp)
    write(unit=vtk(rf)%u,iostat=E_IO)repeat(' ',vtk(rf)%indent+2)//coc64//end_rec
    write(unit=vtk(rf)%u,iostat=E_IO)repeat(' ',vtk(rf)%indent)//'</DataArray>'//end_rec
    write(unit=vtk(rf)%u,iostat=E_IO)repeat(' ',vtk(rf)%indent)//'<DataArray type="Int32" Name="offsets" format="binary">'//end_rec
    Ncocp=size(transfer([int(NC*BYI4P,I4P),offset],cocp),kind=I8P) ; if (allocated(cocp)) deallocate(cocp) ; allocate(cocp(1:Ncocp))
    cocp = transfer([int(NC*BYI4P,I4P),offset],cocp)
    call b64_encode(n=cocp,code=coc64)
    deallocate(cocp)
    write(unit=vtk(rf)%u,iostat=E_IO)repeat(' ',vtk(rf)%indent+2)//coc64//end_rec
    write(unit=vtk(rf)%u,iostat=E_IO)repeat(' ',vtk(rf)%indent)//'</DataArray>'//end_rec
    write(unit=vtk(rf)%u,iostat=E_IO)repeat(' ',vtk(rf)%indent)//'<DataArray type="Int8" Name="types" format="binary">'//end_rec
    if (lbound(cell_type,dim=1)==ubound(cell_type,dim=1)) then
      call pack_data(a1=[int(NC*BYI1P,I4P)],a2=[(cell_type(1),n1=1,NC)],packed=cocp)
    else
      call pack_data(a1=[int(NC*BYI1P,I4P)],a2=cell_type,packed=cocp)
    endif
    call b64_encode(n=cocp,code=coc64) ; deallocate(cocp)
    write(unit=vtk(rf)%u,iostat=E_IO)repeat(' ',vtk(rf)%indent+2)//coc64//end_rec ; deallocate(coc64)
    write(unit=vtk(rf)%u,iostat=E_IO)repeat(' ',vtk(rf)%indent)//'</DataArray>'//end_rec ; vtk(rf)%indent = vtk(rf)%indent - 2
    write(unit=vtk(rf)%u,iostat=E_IO)repeat(' ',vtk(rf)%indent)//'</Cells>'//end_rec
  endselect
  return
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction VTK_CON_XML
endmodule Lib_VTK_IO_CON_XML
