!< GEO_XML interface definition for Lib_VTK_IO.
module Lib_VTK_IO_GEO_XML
!-----------------------------------------------------------------------------------------------------------------------------------
!< Pure Fortran (2003+) library to write and read data conforming the VTK standard
!-----------------------------------------------------------------------------------------------------------------------------------
USE IR_Precision        ! Integers and reals precision definition.
USE Lib_Base64          ! Base64 encoding/decoding procedures.
USE Lib_VTK_IO_Back_End ! Lib_VTK_IO back end module.
!-----------------------------------------------------------------------------------------------------------------------------------

!-----------------------------------------------------------------------------------------------------------------------------------
implicit none
private
save
public:: VTK_GEO_XML
!-----------------------------------------------------------------------------------------------------------------------------------

!-----------------------------------------------------------------------------------------------------------------------------------
interface VTK_GEO_XML
  !< Procedure for saving mesh with different topologies in VTK-XML standard.
  !<
  !< VTK_GEO_XML is an interface to 15 different functions; there are 2 functions for each of 3 topologies supported and a function
  !< for closing XML pieces: one function for mesh coordinates with R8P precision and one for mesh coordinates with R4P precision.
  !< 1D/3D-rank arrays and packed API for any kinds
  !<
  !<- For StructuredGrid there are 4 functions for each real kinds:
  !<    - inputs are 1D-rank arrays: X[1:NN],Y[1:NN],Z[1:NN];
  !<    - inputs are 3D-rank arrays: X[nx1:nx2,ny1:ny2,nz1:nz2],Y[nx1:nx2,ny1:ny2,nz1:nz2],Z[nx1:nx2,ny1:ny2,nz1:nz2];
  !<    - input is 1D-rank array (packed API): XYZ[1:3,1:NN];
  !<    - input is 3D-rank array (packed API): XYZ[1:3,nx1:nx2,ny1:ny2,nz1:nz2].
  !<- For UnStructuredGrid there are 2 functions for each real kinds:
  !<    - inputs are 1D arrays: X[1:NN],Y[1:NN],Z[1:NN];
  !<    - input is 1D array (packed API): XYZ[1:3,1:NN].
  !<
  !< VTK_GEO_XML must be called after VTK_INI_XML. It saves the mesh geometry. The inputs that must be passed
  !< change depending on the topologies chosen. Not all VTK topologies have been implemented (*polydata* topologies are absent).
  !<
  !< @note The XML standard is more powerful than legacy. XML file can contain more than 1 mesh with its
  !< associated variables. Thus there is the necessity to close each *pieces* that compose the data-set saved in the
  !< XML file. The VTK_GEO_XML called in the *close piece* format is used just to close the
  !< current piece before saving another piece or closing the file.
  !<
  !<### Examples of usage
  !<
  !<#### Structured grid calling
  !<```fortran
  !< integer(I4P):: nx1,nx2,ny1,ny2,nz1,nz2,NN
  !< real(R8P)::    X(1:NN),Y(1:NN),Z(1:NN)
  !< ...
  !< E_IO=VTK_GEO_XML(nx1,nx2,ny1,ny2,nz1,nz2,Nn,X,Y,Z)
  !<```
  !<
  !<#### Rectilinear grid calling
  !<```fortran
  !< integer(I4P):: nx1,nx2,ny1,ny2,nz1,nz2
  !< real(R8P)::    X(nx1:nx2),Y(ny1:ny2),Z(nz1:nz2)
  !< ...
  !< E_IO=VTK_GEO_XML(nx1,nx2,ny1,ny2,nz1,nz2,X,Y,Z)
  !<```
  !<
  !<#### Unstructured grid calling
  !<```fortran
  !< integer(I4P):: Nn,Nc
  !< real(R8P)::    X(1:Nn),Y(1:Nn),Z(1:Nn)
  !< ...
  !< E_IO=VTK_GEO_XML(Nn,Nc,X,Y,Z)
  !<```
  !<
  !<#### Closing piece calling
  !<```fortran
  !< E_IO=VTK_GEO_XML()
  !<```
  module procedure VTK_GEO_XML_STRG_1DA_R8, VTK_GEO_XML_STRG_3DA_R8,  & ! real(R8P) StructuredGrid, 1D/3D Arrays
                   VTK_GEO_XML_STRG_1DAP_R8,VTK_GEO_XML_STRG_3DAP_R8, & ! real(R8P) StructuredGrid, 1D/3D Arrays packed API
                   VTK_GEO_XML_STRG_1DA_R4, VTK_GEO_XML_STRG_3DA_R4,  & ! real(R4P) StructuredGrid, 1D/3D Arrays
                   VTK_GEO_XML_STRG_1DAP_R4,VTK_GEO_XML_STRG_3DAP_R4, & ! real(R4P) StructuredGrid, 1D/3D Arrays packed API
                   VTK_GEO_XML_RECT_R8,                               & ! real(R8P) RectilinearGrid
                   VTK_GEO_XML_RECT_R4,                               & ! real(R4P) RectilinearGrid
                   VTK_GEO_XML_UNST_R8,VTK_GEO_XML_UNST_PACK_R4,      & ! real(R8P) UnstructuredGrid, standard and packed API
                   VTK_GEO_XML_UNST_R4,VTK_GEO_XML_UNST_PACK_R8,      & ! real(R4P) UnstructuredGrid, standard and packed API
                   VTK_GEO_XML_CLOSEP                                   ! closing tag "Piece" function
endinterface
!-----------------------------------------------------------------------------------------------------------------------------------
contains
  function VTK_GEO_XML_STRG_1DA_R8(nx1,nx2,ny1,ny2,nz1,nz2,NN,X,Y,Z,cf) result(E_IO)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Function for saving mesh with \b StructuredGrid topology (R8P, 1D Arrays).
  !---------------------------------------------------------------------------------------------------------------------------------
  implicit none
  integer(I4P), intent(IN)::           nx1      !< Initial node of x axis.
  integer(I4P), intent(IN)::           nx2      !< Final node of x axis.
  integer(I4P), intent(IN)::           ny1      !< Initial node of y axis.
  integer(I4P), intent(IN)::           ny2      !< Final node of y axis.
  integer(I4P), intent(IN)::           nz1      !< Initial node of z axis.
  integer(I4P), intent(IN)::           nz2      !< Final node of z axis.
  integer(I4P), intent(IN)::           NN       !< Number of all nodes.
  real(R8P),    intent(IN)::           X(1:)    !< X coordinates [1:NN].
  real(R8P),    intent(IN)::           Y(1:)    !< Y coordinates [1:NN].
  real(R8P),    intent(IN)::           Z(1:)    !< Z coordinates [1:NN].
  integer(I4P), intent(IN), optional:: cf       !< Current file index (for concurrent files IO).
  integer(I4P)::                       E_IO     !< Input/Output inquiring flag: $0$ if IO is done, $> 0$ if IO is not done.
  integer(I1P), allocatable::          XYZp(:)  !< Packed coordinates data.
  character(len=:), allocatable::      XYZ64    !< X, Y, Z coordinates encoded in base64.
  character(len=maxlen)::              s_buffer !< Buffer string.
  integer(I4P)::                       rf       !< Real file index.
  integer(I4P)::                       n1       !< Counter.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  E_IO = -1_I4P
  rf = f
  if (present(cf)) then
    rf = cf ; f = cf
  endif
  select case(vtk(rf)%f)
  case(ascii)
    s_buffer = repeat(' ',vtk(rf)%indent)//'<Piece Extent="'//trim(str(n=nx1))//' '//trim(str(n=nx2))//' '// &
                                                              trim(str(n=ny1))//' '//trim(str(n=ny2))//' '// &
                                                              trim(str(n=nz1))//' '//trim(str(n=nz2))//'">'
    write(unit=vtk(rf)%u,fmt='(A)',iostat=E_IO)trim(s_buffer) ; vtk(rf)%indent = vtk(rf)%indent + 2
    write(unit=vtk(rf)%u,fmt='(A)',iostat=E_IO)repeat(' ',vtk(rf)%indent)//'<Points>' ; vtk(rf)%indent = vtk(rf)%indent + 2
    s_buffer = repeat(' ',vtk(rf)%indent)//'<DataArray type="Float64" NumberOfComponents="3" Name="Points" format="ascii">'
    write(unit=vtk(rf)%u,fmt='(A)',iostat=E_IO)trim(s_buffer)
    do n1=1,NN
      write(unit=vtk(rf)%u,fmt='(A)',iostat=E_IO)repeat(' ',vtk(rf)%indent)//str(n=X(n1))//' '//str(n=Y(n1))//' '//str(n=Z(n1))
    enddo
    write(unit=vtk(rf)%u,fmt='(A)',iostat=E_IO)repeat(' ',vtk(rf)%indent)//'</DataArray>' ; vtk(rf)%indent = vtk(rf)%indent - 2
    write(unit=vtk(rf)%u,fmt='(A)',iostat=E_IO)repeat(' ',vtk(rf)%indent)//'</Points>'
  case(raw,bin_app)
    s_buffer = repeat(' ',vtk(rf)%indent)//'<Piece Extent="'//trim(str(n=nx1))//' '//trim(str(n=nx2))//' '// &
                                                              trim(str(n=ny1))//' '//trim(str(n=ny2))//' '// &
                                                              trim(str(n=nz1))//' '//trim(str(n=nz2))//'">'
    write(unit=vtk(rf)%u,iostat=E_IO)trim(s_buffer)//end_rec ; vtk(rf)%indent = vtk(rf)%indent + 2
    write(unit=vtk(rf)%u,iostat=E_IO)repeat(' ',vtk(rf)%indent)//'<Points>'//end_rec ; vtk(rf)%indent = vtk(rf)%indent + 2
    s_buffer = repeat(' ',vtk(rf)%indent)//                                                                  &
               '<DataArray type="Float64" NumberOfComponents="3" Name="Points" format="appended" offset="'// &
               trim(str(.true.,vtk(rf)%ioffset))//'"/>'
    write(unit=vtk(rf)%u,iostat=E_IO)trim(s_buffer)//end_rec
    call vtk(rf)%byte_update(N_Byte = 3*NN*BYR8P)
    write(unit=vtk(rf)%ua,iostat=E_IO)vtk(rf)%N_Byte,'R8',3*NN
    write(unit=vtk(rf)%ua,iostat=E_IO)(X(n1),Y(n1),Z(n1),n1=1,NN)
    vtk(rf)%indent = vtk(rf)%indent - 2 ; write(unit=vtk(rf)%u,iostat=E_IO)repeat(' ',vtk(rf)%indent)//'</Points>'//end_rec
  case(binary)
    s_buffer = repeat(' ',vtk(rf)%indent)//'<Piece Extent="'//trim(str(n=nx1))//' '//trim(str(n=nx2))//' '// &
                                                              trim(str(n=ny1))//' '//trim(str(n=ny2))//' '// &
                                                              trim(str(n=nz1))//' '//trim(str(n=nz2))//'">'
    write(unit=vtk(rf)%u,iostat=E_IO)trim(s_buffer)//end_rec ; vtk(rf)%indent = vtk(rf)%indent + 2
    write(unit=vtk(rf)%u,iostat=E_IO)repeat(' ',vtk(rf)%indent)//'<Points>'//end_rec ; vtk(rf)%indent = vtk(rf)%indent + 2
    s_buffer = repeat(' ',vtk(rf)%indent)//'<DataArray type="Float64" NumberOfComponents="3" Name="Points" format="binary">'
    write(unit=vtk(rf)%u,iostat=E_IO)trim(s_buffer)//end_rec
    call pack_data(a1=[int(3*NN*BYR8P,I4P)],a2=[(X(n1),Y(n1),Z(n1),n1=1,NN)],packed=XYZp)
    call b64_encode(n=XYZp,code=XYZ64) ; deallocate(XYZp)
    write(unit=vtk(rf)%u,iostat=E_IO)repeat(' ',vtk(rf)%indent+2)//XYZ64//end_rec ; deallocate(XYZ64)
    write(unit=vtk(rf)%u,iostat=E_IO)repeat(' ',vtk(rf)%indent)//'</DataArray>'//end_rec
    vtk(rf)%indent = vtk(rf)%indent - 2 ; write(unit=vtk(rf)%u,iostat=E_IO)repeat(' ',vtk(rf)%indent)//'</Points>'//end_rec
  endselect
  return
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction VTK_GEO_XML_STRG_1DA_R8

  function VTK_GEO_XML_STRG_3DA_R8(nx1,nx2,ny1,ny2,nz1,nz2,NN,X,Y,Z,cf) result(E_IO)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Function for saving mesh with \b StructuredGrid topology (R8P, 3D Arrays).
  !---------------------------------------------------------------------------------------------------------------------------------
  implicit none
  integer(I4P), intent(IN)::           nx1               !< Initial node of x axis.
  integer(I4P), intent(IN)::           nx2               !< Final node of x axis.
  integer(I4P), intent(IN)::           ny1               !< Initial node of y axis.
  integer(I4P), intent(IN)::           ny2               !< Final node of y axis.
  integer(I4P), intent(IN)::           nz1               !< Initial node of z axis.
  integer(I4P), intent(IN)::           nz2               !< Final node of z axis.
  integer(I4P), intent(IN)::           NN                !< Number of all nodes.
  real(R8P),    intent(IN)::           X(nx1:,ny1:,nz1:) !< X coordinates [nx1:nx2,ny1:ny2,nz1:nz2].
  real(R8P),    intent(IN)::           Y(nx1:,ny1:,nz1:) !< Y coordinates [nx1:nx2,ny1:ny2,nz1:nz2].
  real(R8P),    intent(IN)::           Z(nx1:,ny1:,nz1:) !< Z coordinates [nx1:nx2,ny1:ny2,nz1:nz2].
  integer(I4P), intent(IN), optional:: cf                !< Current file index (for concurrent files IO).
  integer(I4P)::                       E_IO              !< Input/Output inquiring flag: $0$ if IO is done, $> 0$ if IO is not done.
  integer(I1P), allocatable::          XYZp(:)           !< Packed coordinates data.
  character(len=:), allocatable::      XYZ64             !< X, Y, Z coordinates encoded in base64.
  character(len=maxlen)::              s_buffer          !< Buffer string.
  integer(I4P)::                       rf                !< Real file index.
  integer(I4P)::                       nx,ny,nz          !< Counters.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  E_IO = -1_I4P
  rf = f
  if (present(cf)) then
    rf = cf ; f = cf
  endif
  select case(vtk(rf)%f)
  case(ascii)
    s_buffer = repeat(' ',vtk(rf)%indent)//'<Piece Extent="'//trim(str(n=nx1))//' '//trim(str(n=nx2))//' '// &
                                                              trim(str(n=ny1))//' '//trim(str(n=ny2))//' '// &
                                                              trim(str(n=nz1))//' '//trim(str(n=nz2))//'">'
    write(unit=vtk(rf)%u,fmt='(A)',iostat=E_IO)trim(s_buffer) ; vtk(rf)%indent = vtk(rf)%indent + 2
    write(unit=vtk(rf)%u,fmt='(A)',iostat=E_IO)repeat(' ',vtk(rf)%indent)//'<Points>' ; vtk(rf)%indent = vtk(rf)%indent + 2
    s_buffer = repeat(' ',vtk(rf)%indent)//'<DataArray type="Float64" NumberOfComponents="3" Name="Points" format="ascii">'
    write(unit=vtk(rf)%u,fmt='(A)',iostat=E_IO)trim(s_buffer)
    do nz=nz1,nz2
      do ny=ny1,ny2
        do nx=nx1,nx2
          write(unit=vtk(rf)%u,fmt='(A)',iostat=E_IO)repeat(' ',vtk(rf)%indent)//&
                                                     str(n=X(nx,ny,nz))//' '//str(n=Y(nx,ny,nz))//' '//str(n=Z(nx,ny,nz))
        enddo
      enddo
    enddo
    write(unit=vtk(rf)%u,fmt='(A)',iostat=E_IO)repeat(' ',vtk(rf)%indent)//'</DataArray>' ; vtk(rf)%indent = vtk(rf)%indent - 2
    write(unit=vtk(rf)%u,fmt='(A)',iostat=E_IO)repeat(' ',vtk(rf)%indent)//'</Points>'
  case(raw,bin_app)
    s_buffer = repeat(' ',vtk(rf)%indent)//'<Piece Extent="'//trim(str(n=nx1))//' '//trim(str(n=nx2))//' '// &
                                                              trim(str(n=ny1))//' '//trim(str(n=ny2))//' '// &
                                                              trim(str(n=nz1))//' '//trim(str(n=nz2))//'">'
    write(unit=vtk(rf)%u,iostat=E_IO)trim(s_buffer)//end_rec ; vtk(rf)%indent = vtk(rf)%indent + 2
    write(unit=vtk(rf)%u,iostat=E_IO)repeat(' ',vtk(rf)%indent)//'<Points>'//end_rec ; vtk(rf)%indent = vtk(rf)%indent + 2
    s_buffer = repeat(' ',vtk(rf)%indent)//                                                                  &
               '<DataArray type="Float64" NumberOfComponents="3" Name="Points" format="appended" offset="'// &
               trim(str(.true.,vtk(rf)%ioffset))//'"/>'
    write(unit=vtk(rf)%u,iostat=E_IO)trim(s_buffer)//end_rec
    call vtk(rf)%byte_update(N_Byte = 3*NN*BYR8P)
    write(unit=vtk(rf)%ua,iostat=E_IO)vtk(rf)%N_Byte,'R8',3*NN
    write(unit=vtk(rf)%ua,iostat=E_IO)(((X(nx,ny,nz),Y(nx,ny,nz),Z(nx,ny,nz),nx=nx1,nx2),ny=ny1,ny2),nz=nz1,nz2)
    vtk(rf)%indent = vtk(rf)%indent - 2 ; write(unit=vtk(rf)%u,iostat=E_IO)repeat(' ',vtk(rf)%indent)//'</Points>'//end_rec
  case(binary)
    s_buffer = repeat(' ',vtk(rf)%indent)//'<Piece Extent="'//trim(str(n=nx1))//' '//trim(str(n=nx2))//' '// &
                                                              trim(str(n=ny1))//' '//trim(str(n=ny2))//' '// &
                                                              trim(str(n=nz1))//' '//trim(str(n=nz2))//'">'
    write(unit=vtk(rf)%u,iostat=E_IO)trim(s_buffer)//end_rec ; vtk(rf)%indent = vtk(rf)%indent + 2
    write(unit=vtk(rf)%u,iostat=E_IO)repeat(' ',vtk(rf)%indent)//'<Points>'//end_rec ; vtk(rf)%indent = vtk(rf)%indent + 2
    s_buffer = repeat(' ',vtk(rf)%indent)//'<DataArray type="Float64" NumberOfComponents="3" Name="Points" format="binary">'
    write(unit=vtk(rf)%u,iostat=E_IO)trim(s_buffer)//end_rec
    call pack_data(a1=[int(3*NN*BYR8P,I4P)],a2=[(((X(nx,ny,nz),Y(nx,ny,nz),Z(nx,ny,nz),nx=nx1,nx2),ny=ny1,ny2),nz=nz1,nz2)],&
                   packed=XYZp)
    call b64_encode(n=XYZp,code=XYZ64) ; deallocate(XYZp)
    write(unit=vtk(rf)%u,iostat=E_IO)repeat(' ',vtk(rf)%indent+2)//XYZ64//end_rec ; deallocate(XYZ64)
    write(unit=vtk(rf)%u,iostat=E_IO)repeat(' ',vtk(rf)%indent)//'</DataArray>'//end_rec
    vtk(rf)%indent = vtk(rf)%indent - 2 ; write(unit=vtk(rf)%u,iostat=E_IO)repeat(' ',vtk(rf)%indent)//'</Points>'//end_rec
  endselect
  return
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction VTK_GEO_XML_STRG_3DA_R8

  function VTK_GEO_XML_STRG_1DAP_R8(nx1,nx2,ny1,ny2,nz1,nz2,NN,XYZ,cf) result(E_IO)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Function for saving mesh with \b StructuredGrid topology (R8P, 1D Arrays, packed API).
  !---------------------------------------------------------------------------------------------------------------------------------
  implicit none
  integer(I4P), intent(IN)::           nx1        !< Initial node of x axis.
  integer(I4P), intent(IN)::           nx2        !< Final node of x axis.
  integer(I4P), intent(IN)::           ny1        !< Initial node of y axis.
  integer(I4P), intent(IN)::           ny2        !< Final node of y axis.
  integer(I4P), intent(IN)::           nz1        !< Initial node of z axis.
  integer(I4P), intent(IN)::           nz2        !< Final node of z axis.
  integer(I4P), intent(IN)::           NN         !< Number of all nodes.
  real(R8P),    intent(IN)::           XYZ(1:,1:) !< X, Y, Z coordinates (packed API) [1:3,1:NN].
  integer(I4P), intent(IN), optional:: cf         !< Current file index (for concurrent files IO).
  integer(I4P)::                       E_IO       !< Input/Output inquiring flag: $0$ if IO is done, $> 0$ if IO is not done.
  integer(I1P), allocatable::          XYZp(:)    !< Packed coordinates data.
  character(len=:), allocatable::      XYZ64      !< X, Y, Z coordinates encoded in base64.
  character(len=maxlen)::              s_buffer   !< Buffer string.
  integer(I4P)::                       rf         !< Real file index.
  integer(I4P)::                       n1         !< Counter.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  E_IO = -1_I4P
  rf = f
  if (present(cf)) then
    rf = cf ; f = cf
  endif
  select case(vtk(rf)%f)
  case(ascii)
    s_buffer = repeat(' ',vtk(rf)%indent)//'<Piece Extent="'//trim(str(n=nx1))//' '//trim(str(n=nx2))//' '// &
                                                              trim(str(n=ny1))//' '//trim(str(n=ny2))//' '// &
                                                              trim(str(n=nz1))//' '//trim(str(n=nz2))//'">'
    write(unit=vtk(rf)%u,fmt='(A)',iostat=E_IO)trim(s_buffer) ; vtk(rf)%indent = vtk(rf)%indent + 2
    write(unit=vtk(rf)%u,fmt='(A)',iostat=E_IO)repeat(' ',vtk(rf)%indent)//'<Points>' ; vtk(rf)%indent = vtk(rf)%indent + 2
    s_buffer = repeat(' ',vtk(rf)%indent)//'<DataArray type="Float64" NumberOfComponents="3" Name="Points" format="ascii">'
    write(unit=vtk(rf)%u,fmt='(A)',iostat=E_IO)trim(s_buffer)
    do n1=1,NN
      write(unit=vtk(rf)%u,fmt='(A)',iostat=E_IO)repeat(' ',vtk(rf)%indent)//&
                                                 str(n=XYZ(1,n1))//' '//str(n=XYZ(2,n1))//' '//str(n=XYZ(3,n1))
    enddo
    write(unit=vtk(rf)%u,fmt='(A)',iostat=E_IO)repeat(' ',vtk(rf)%indent)//'</DataArray>' ; vtk(rf)%indent = vtk(rf)%indent - 2
    write(unit=vtk(rf)%u,fmt='(A)',iostat=E_IO)repeat(' ',vtk(rf)%indent)//'</Points>'
  case(raw,bin_app)
    s_buffer = repeat(' ',vtk(rf)%indent)//'<Piece Extent="'//trim(str(n=nx1))//' '//trim(str(n=nx2))//' '// &
                                                              trim(str(n=ny1))//' '//trim(str(n=ny2))//' '// &
                                                              trim(str(n=nz1))//' '//trim(str(n=nz2))//'">'
    write(unit=vtk(rf)%u,iostat=E_IO)trim(s_buffer)//end_rec ; vtk(rf)%indent = vtk(rf)%indent + 2
    write(unit=vtk(rf)%u,iostat=E_IO)repeat(' ',vtk(rf)%indent)//'<Points>'//end_rec ; vtk(rf)%indent = vtk(rf)%indent + 2
    s_buffer = repeat(' ',vtk(rf)%indent)//                                                                  &
               '<DataArray type="Float64" NumberOfComponents="3" Name="Points" format="appended" offset="'// &
               trim(str(.true.,vtk(rf)%ioffset))//'"/>'
    write(unit=vtk(rf)%u,iostat=E_IO)trim(s_buffer)//end_rec
    call vtk(rf)%byte_update(N_Byte = 3*NN*BYR8P)
    write(unit=vtk(rf)%ua,iostat=E_IO)vtk(rf)%N_Byte,'R8',3*NN
    write(unit=vtk(rf)%ua,iostat=E_IO)XYZ
    vtk(rf)%indent = vtk(rf)%indent - 2 ; write(unit=vtk(rf)%u,iostat=E_IO)repeat(' ',vtk(rf)%indent)//'</Points>'//end_rec
  case(binary)
    s_buffer = repeat(' ',vtk(rf)%indent)//'<Piece Extent="'//trim(str(n=nx1))//' '//trim(str(n=nx2))//' '// &
                                                              trim(str(n=ny1))//' '//trim(str(n=ny2))//' '// &
                                                              trim(str(n=nz1))//' '//trim(str(n=nz2))//'">'
    write(unit=vtk(rf)%u,iostat=E_IO)trim(s_buffer)//end_rec ; vtk(rf)%indent = vtk(rf)%indent + 2
    write(unit=vtk(rf)%u,iostat=E_IO)repeat(' ',vtk(rf)%indent)//'<Points>'//end_rec ; vtk(rf)%indent = vtk(rf)%indent + 2
    s_buffer = repeat(' ',vtk(rf)%indent)//'<DataArray type="Float64" NumberOfComponents="3" Name="Points" format="binary">'
    write(unit=vtk(rf)%u,iostat=E_IO)trim(s_buffer)//end_rec
    call pack_data(a1=[int(3*NN*BYR8P,I4P)],a2=reshape(XYZ,[3*NN]),packed=XYZp)
    call b64_encode(n=XYZp,code=XYZ64) ; deallocate(XYZp)
    write(unit=vtk(rf)%u,iostat=E_IO)repeat(' ',vtk(rf)%indent+2)//XYZ64//end_rec ; deallocate(XYZ64)
    write(unit=vtk(rf)%u,iostat=E_IO)repeat(' ',vtk(rf)%indent)//'</DataArray>'//end_rec
    vtk(rf)%indent = vtk(rf)%indent - 2 ; write(unit=vtk(rf)%u,iostat=E_IO)repeat(' ',vtk(rf)%indent)//'</Points>'//end_rec
  endselect
  return
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction VTK_GEO_XML_STRG_1DAP_R8

  function VTK_GEO_XML_STRG_3DAP_R8(nx1,nx2,ny1,ny2,nz1,nz2,NN,XYZ,cf) result(E_IO)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Function for saving mesh with \b StructuredGrid topology (R8P, 3D Arrays, packed API).
  !---------------------------------------------------------------------------------------------------------------------------------
  implicit none
  integer(I4P), intent(IN)::           nx1                    !< Initial node of x axis.
  integer(I4P), intent(IN)::           nx2                    !< Final node of x axis.
  integer(I4P), intent(IN)::           ny1                    !< Initial node of y axis.
  integer(I4P), intent(IN)::           ny2                    !< Final node of y axis.
  integer(I4P), intent(IN)::           nz1                    !< Initial node of z axis.
  integer(I4P), intent(IN)::           nz2                    !< Final node of z axis.
  integer(I4P), intent(IN)::           NN                     !< Number of all nodes.
  real(R8P),    intent(IN)::           XYZ(1:,nx1:,ny1:,nz1:) !< X, Y, Z coordinates (packed API).
  integer(I4P), intent(IN), optional:: cf                     !< Current file index (for concurrent files IO).
  integer(I4P)::                       E_IO              !< Input/Output inquiring flag: $0$ if IO is done, $> 0$ if IO is not done.
  integer(I1P), allocatable::          XYZp(:)                !< Packed coordinates data.
  character(len=:), allocatable::      XYZ64                  !< X, Y, Z coordinates encoded in base64.
  character(len=maxlen)::              s_buffer               !< Buffer string.
  integer(I4P)::                       rf                     !< Real file index.
  integer(I4P)::                       nx,ny,nz               !< Counters.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  E_IO = -1_I4P
  rf = f
  if (present(cf)) then
    rf = cf ; f = cf
  endif
  select case(vtk(rf)%f)
  case(ascii)
    s_buffer = repeat(' ',vtk(rf)%indent)//'<Piece Extent="'//trim(str(n=nx1))//' '//trim(str(n=nx2))//' '// &
                                                              trim(str(n=ny1))//' '//trim(str(n=ny2))//' '// &
                                                              trim(str(n=nz1))//' '//trim(str(n=nz2))//'">'
    write(unit=vtk(rf)%u,fmt='(A)',iostat=E_IO)trim(s_buffer) ; vtk(rf)%indent = vtk(rf)%indent + 2
    write(unit=vtk(rf)%u,fmt='(A)',iostat=E_IO)repeat(' ',vtk(rf)%indent)//'<Points>' ; vtk(rf)%indent = vtk(rf)%indent + 2
    s_buffer = repeat(' ',vtk(rf)%indent)//'<DataArray type="Float64" NumberOfComponents="3" Name="Points" format="ascii">'
    write(unit=vtk(rf)%u,fmt='(A)',iostat=E_IO)trim(s_buffer)
    do nz=nz1,nz2
      do ny=ny1,ny2
        do nx=nx1,nx2
          write(unit=vtk(rf)%u,fmt='(A)',iostat=E_IO)repeat(' ',vtk(rf)%indent)//&
                                                    str(n=XYZ(1,nx,ny,nz))//' '//str(n=XYZ(2,nx,ny,nz))//' '//str(n=XYZ(3,nx,ny,nz))
        enddo
      enddo
    enddo
    write(unit=vtk(rf)%u,fmt='(A)',iostat=E_IO)repeat(' ',vtk(rf)%indent)//'</DataArray>' ; vtk(rf)%indent = vtk(rf)%indent - 2
    write(unit=vtk(rf)%u,fmt='(A)',iostat=E_IO)repeat(' ',vtk(rf)%indent)//'</Points>'
  case(raw,bin_app)
    s_buffer = repeat(' ',vtk(rf)%indent)//'<Piece Extent="'//trim(str(n=nx1))//' '//trim(str(n=nx2))//' '// &
                                                              trim(str(n=ny1))//' '//trim(str(n=ny2))//' '// &
                                                              trim(str(n=nz1))//' '//trim(str(n=nz2))//'">'
    write(unit=vtk(rf)%u,iostat=E_IO)trim(s_buffer)//end_rec ; vtk(rf)%indent = vtk(rf)%indent + 2
    write(unit=vtk(rf)%u,iostat=E_IO)repeat(' ',vtk(rf)%indent)//'<Points>'//end_rec ; vtk(rf)%indent = vtk(rf)%indent + 2
    s_buffer = repeat(' ',vtk(rf)%indent)//                                                                  &
               '<DataArray type="Float64" NumberOfComponents="3" Name="Points" format="appended" offset="'// &
               trim(str(.true.,vtk(rf)%ioffset))//'"/>'
    write(unit=vtk(rf)%u,iostat=E_IO)trim(s_buffer)//end_rec
    call vtk(rf)%byte_update(N_Byte = 3*NN*BYR8P)
    write(unit=vtk(rf)%ua,iostat=E_IO)vtk(rf)%N_Byte,'R8',3*NN
    write(unit=vtk(rf)%ua,iostat=E_IO)XYZ
    vtk(rf)%indent = vtk(rf)%indent - 2 ; write(unit=vtk(rf)%u,iostat=E_IO)repeat(' ',vtk(rf)%indent)//'</Points>'//end_rec
  case(binary)
    s_buffer = repeat(' ',vtk(rf)%indent)//'<Piece Extent="'//trim(str(n=nx1))//' '//trim(str(n=nx2))//' '// &
                                                              trim(str(n=ny1))//' '//trim(str(n=ny2))//' '// &
                                                              trim(str(n=nz1))//' '//trim(str(n=nz2))//'">'
    write(unit=vtk(rf)%u,iostat=E_IO)trim(s_buffer)//end_rec ; vtk(rf)%indent = vtk(rf)%indent + 2
    write(unit=vtk(rf)%u,iostat=E_IO)repeat(' ',vtk(rf)%indent)//'<Points>'//end_rec ; vtk(rf)%indent = vtk(rf)%indent + 2
    s_buffer = repeat(' ',vtk(rf)%indent)//'<DataArray type="Float64" NumberOfComponents="3" Name="Points" format="binary">'
    write(unit=vtk(rf)%u,iostat=E_IO)trim(s_buffer)//end_rec
    call pack_data(a1=[int(3*NN*BYR8P,I4P)],a2=reshape(XYZ,[3*NN]),packed=XYZp)
    call b64_encode(n=XYZp,code=XYZ64) ; deallocate(XYZp)
    write(unit=vtk(rf)%u,iostat=E_IO)repeat(' ',vtk(rf)%indent+2)//XYZ64//end_rec ; deallocate(XYZ64)
    write(unit=vtk(rf)%u,iostat=E_IO)repeat(' ',vtk(rf)%indent)//'</DataArray>'//end_rec
    vtk(rf)%indent = vtk(rf)%indent - 2 ; write(unit=vtk(rf)%u,iostat=E_IO)repeat(' ',vtk(rf)%indent)//'</Points>'//end_rec
  endselect
  return
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction VTK_GEO_XML_STRG_3DAP_R8

  function VTK_GEO_XML_STRG_1DA_R4(nx1,nx2,ny1,ny2,nz1,nz2,NN,X,Y,Z,cf) result(E_IO)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Function for saving mesh with \b StructuredGrid topology (R4P, 1D Arrays).
  !---------------------------------------------------------------------------------------------------------------------------------
  implicit none
  integer(I4P), intent(IN)::           nx1      !< Initial node of x axis.
  integer(I4P), intent(IN)::           nx2      !< Final node of x axis.
  integer(I4P), intent(IN)::           ny1      !< Initial node of y axis.
  integer(I4P), intent(IN)::           ny2      !< Final node of y axis.
  integer(I4P), intent(IN)::           nz1      !< Initial node of z axis.
  integer(I4P), intent(IN)::           nz2      !< Final node of z axis.
  integer(I4P), intent(IN)::           NN       !< Number of all nodes.
  real(R4P),    intent(IN)::           X(1:)    !< X coordinates [1:NN].
  real(R4P),    intent(IN)::           Y(1:)    !< Y coordinates [1:NN].
  real(R4P),    intent(IN)::           Z(1:)    !< Z coordinates [1:NN].
  integer(I4P), intent(IN), optional:: cf       !< Current file index (for concurrent files IO).
  integer(I4P)::                       E_IO     !< Input/Output inquiring flag: $0$ if IO is done, $> 0$ if IO is not done.
  character(len=maxlen)::              s_buffer !< Buffer string.
  integer(I1P), allocatable::          XYZp(:)  !< Packed data.
  character(len=:), allocatable::      XYZ64    !< X, Y, Z coordinates encoded in base64.
  integer(I4P)::                       rf       !< Real file index.
  integer(I4P)::                       n1       !< Counter.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  E_IO = -1_I4P
  rf = f
  if (present(cf)) then
    rf = cf ; f = cf
  endif
  select case(vtk(rf)%f)
  case(ascii)
    s_buffer = repeat(' ',vtk(rf)%indent)//'<Piece Extent="'//trim(str(n=nx1))//' '//trim(str(n=nx2))//' '// &
                                                              trim(str(n=ny1))//' '//trim(str(n=ny2))//' '// &
                                                              trim(str(n=nz1))//' '//trim(str(n=nz2))//'">'
    write(unit=vtk(rf)%u,fmt='(A)',iostat=E_IO)trim(s_buffer) ; vtk(rf)%indent = vtk(rf)%indent + 2
    write(unit=vtk(rf)%u,fmt='(A)',iostat=E_IO)repeat(' ',vtk(rf)%indent)//'<Points>' ; vtk(rf)%indent = vtk(rf)%indent + 2
    s_buffer = repeat(' ',vtk(rf)%indent)//'<DataArray type="Float32" NumberOfComponents="3" Name="Points" format="ascii">'
    write(unit=vtk(rf)%u,fmt='(A)',iostat=E_IO)trim(s_buffer)
    do n1=1,NN
      write(unit=vtk(rf)%u,fmt='(A)',iostat=E_IO)repeat(' ',vtk(rf)%indent)//str(n=X(n1))//' '//str(n=Y(n1))//' '//str(n=Z(n1))
    enddo
    write(unit=vtk(rf)%u,fmt='(A)',iostat=E_IO)repeat(' ',vtk(rf)%indent)//'</DataArray>' ; vtk(rf)%indent = vtk(rf)%indent - 2
    write(unit=vtk(rf)%u,fmt='(A)',iostat=E_IO)repeat(' ',vtk(rf)%indent)//'</Points>'
  case(raw,bin_app)
    s_buffer = repeat(' ',vtk(rf)%indent)//'<Piece Extent="'//trim(str(n=nx1))//' '//trim(str(n=nx2))//' '// &
                                                              trim(str(n=ny1))//' '//trim(str(n=ny2))//' '// &
                                                              trim(str(n=nz1))//' '//trim(str(n=nz2))//'">'
    write(unit=vtk(rf)%u,iostat=E_IO)trim(s_buffer)//end_rec ; vtk(rf)%indent = vtk(rf)%indent + 2
    write(unit=vtk(rf)%u,iostat=E_IO)repeat(' ',vtk(rf)%indent)//'<Points>'//end_rec ; vtk(rf)%indent = vtk(rf)%indent + 2
    s_buffer = repeat(' ',vtk(rf)%indent)//                                                                  &
               '<DataArray type="Float32" NumberOfComponents="3" Name="Points" format="appended" offset="'// &
               trim(str(.true.,vtk(rf)%ioffset))//'"/>'
    write(unit=vtk(rf)%u,iostat=E_IO)trim(s_buffer)//end_rec
    call vtk(rf)%byte_update(N_Byte = 3*NN*BYR4P)
    write(unit=vtk(rf)%ua,iostat=E_IO)vtk(rf)%N_Byte,'R4',3*NN
    write(unit=vtk(rf)%ua,iostat=E_IO)(X(n1),Y(n1),Z(n1),n1=1,NN)
    vtk(rf)%indent = vtk(rf)%indent - 2 ; write(unit=vtk(rf)%u,iostat=E_IO)repeat(' ',vtk(rf)%indent)//'</Points>'//end_rec
  case(binary)
    s_buffer = repeat(' ',vtk(rf)%indent)//'<Piece Extent="'//trim(str(n=nx1))//' '//trim(str(n=nx2))//' '// &
                                                              trim(str(n=ny1))//' '//trim(str(n=ny2))//' '// &
                                                              trim(str(n=nz1))//' '//trim(str(n=nz2))//'">'
    write(unit=vtk(rf)%u,iostat=E_IO)trim(s_buffer)//end_rec ; vtk(rf)%indent = vtk(rf)%indent + 2
    write(unit=vtk(rf)%u,iostat=E_IO)repeat(' ',vtk(rf)%indent)//'<Points>'//end_rec ; vtk(rf)%indent = vtk(rf)%indent + 2
    s_buffer = repeat(' ',vtk(rf)%indent)//'<DataArray type="Float32" NumberOfComponents="3" Name="Points" format="binary">'
    write(unit=vtk(rf)%u,iostat=E_IO)trim(s_buffer)//end_rec
    call pack_data(a1=[int(3*NN*BYR4P,I4P)],a2=[(X(n1),Y(n1),Z(n1),n1=1,NN)],packed=XYZp)
    call b64_encode(n=XYZp,code=XYZ64) ; deallocate(XYZp)
    write(unit=vtk(rf)%u,iostat=E_IO)repeat(' ',vtk(rf)%indent+2)//XYZ64//end_rec ; deallocate(XYZ64)
    write(unit=vtk(rf)%u,iostat=E_IO)repeat(' ',vtk(rf)%indent)//'</DataArray>'//end_rec
    vtk(rf)%indent = vtk(rf)%indent - 2 ; write(unit=vtk(rf)%u,iostat=E_IO)repeat(' ',vtk(rf)%indent)//'</Points>'//end_rec
  endselect
  return
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction VTK_GEO_XML_STRG_1DA_R4

  function VTK_GEO_XML_STRG_3DA_R4(nx1,nx2,ny1,ny2,nz1,nz2,NN,X,Y,Z,cf) result(E_IO)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Function for saving mesh with \b StructuredGrid topology (R4P, 3D Arrays).
  !---------------------------------------------------------------------------------------------------------------------------------
  implicit none
  integer(I4P), intent(IN)::           nx1               !< Initial node of x axis.
  integer(I4P), intent(IN)::           nx2               !< Final node of x axis.
  integer(I4P), intent(IN)::           ny1               !< Initial node of y axis.
  integer(I4P), intent(IN)::           ny2               !< Final node of y axis.
  integer(I4P), intent(IN)::           nz1               !< Initial node of z axis.
  integer(I4P), intent(IN)::           nz2               !< Final node of z axis.
  integer(I4P), intent(IN)::           NN                !< Number of all nodes.
  real(R4P),    intent(IN)::           X(nx1:,ny1:,nz1:) !< X coordinates [nx1:nx2,ny1:ny2,nz1:nz2].
  real(R4P),    intent(IN)::           Y(nx1:,ny1:,nz1:) !< Y coordinates [nx1:nx2,ny1:ny2,nz1:nz2].
  real(R4P),    intent(IN)::           Z(nx1:,ny1:,nz1:) !< Z coordinates [nx1:nx2,ny1:ny2,nz1:nz2].
  integer(I4P), intent(IN), optional:: cf                !< Current file index (for concurrent files IO).
  integer(I4P)::                       E_IO              !< Input/Output inquiring flag: $0$ if IO is done, $> 0$ if IO is not done.
  character(len=maxlen)::              s_buffer          !< Buffer string.
  integer(I1P), allocatable::          XYZp(:)           !< Packed data.
  character(len=:), allocatable::      XYZ64             !< X, Y, Z coordinates encoded in base64.
  integer(I4P)::                       rf                !< Real file index.
  integer(I4P)::                       nx,ny,nz          !< Counters.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  E_IO = -1_I4P
  rf = f
  if (present(cf)) then
    rf = cf ; f = cf
  endif
  select case(vtk(rf)%f)
  case(ascii)
    s_buffer = repeat(' ',vtk(rf)%indent)//'<Piece Extent="'//trim(str(n=nx1))//' '//trim(str(n=nx2))//' '// &
                                                              trim(str(n=ny1))//' '//trim(str(n=ny2))//' '// &
                                                              trim(str(n=nz1))//' '//trim(str(n=nz2))//'">'
    write(unit=vtk(rf)%u,fmt='(A)',iostat=E_IO)trim(s_buffer) ; vtk(rf)%indent = vtk(rf)%indent + 2
    write(unit=vtk(rf)%u,fmt='(A)',iostat=E_IO)repeat(' ',vtk(rf)%indent)//'<Points>' ; vtk(rf)%indent = vtk(rf)%indent + 2
    s_buffer = repeat(' ',vtk(rf)%indent)//'<DataArray type="Float32" NumberOfComponents="3" Name="Points" format="ascii">'
    write(unit=vtk(rf)%u,fmt='(A)',iostat=E_IO)trim(s_buffer)
    do nz=nz1,nz2
      do ny=ny1,ny2
        do nx=nx1,nx2
          write(unit=vtk(rf)%u,fmt='(A)',iostat=E_IO)repeat(' ',vtk(rf)%indent)//&
                                                     str(n=X(nx,ny,nz))//' '//str(n=Y(nx,ny,nz))//' '//str(n=Z(nx,ny,nz))
        enddo
      enddo
    enddo
    write(unit=vtk(rf)%u,fmt='(A)',iostat=E_IO)repeat(' ',vtk(rf)%indent)//'</DataArray>' ; vtk(rf)%indent = vtk(rf)%indent - 2
    write(unit=vtk(rf)%u,fmt='(A)',iostat=E_IO)repeat(' ',vtk(rf)%indent)//'</Points>'
  case(raw,bin_app)
    s_buffer = repeat(' ',vtk(rf)%indent)//'<Piece Extent="'//trim(str(n=nx1))//' '//trim(str(n=nx2))//' '// &
                                                              trim(str(n=ny1))//' '//trim(str(n=ny2))//' '// &
                                                              trim(str(n=nz1))//' '//trim(str(n=nz2))//'">'
    write(unit=vtk(rf)%u,iostat=E_IO)trim(s_buffer)//end_rec ; vtk(rf)%indent = vtk(rf)%indent + 2
    write(unit=vtk(rf)%u,iostat=E_IO)repeat(' ',vtk(rf)%indent)//'<Points>'//end_rec ; vtk(rf)%indent = vtk(rf)%indent + 2
    s_buffer = repeat(' ',vtk(rf)%indent)//                                                                  &
               '<DataArray type="Float32" NumberOfComponents="3" Name="Points" format="appended" offset="'// &
               trim(str(.true.,vtk(rf)%ioffset))//'"/>'
    write(unit=vtk(rf)%u,iostat=E_IO)trim(s_buffer)//end_rec
    call vtk(rf)%byte_update(N_Byte = 3*NN*BYR4P)
    write(unit=vtk(rf)%ua,iostat=E_IO)vtk(rf)%N_Byte,'R4',3*NN
    write(unit=vtk(rf)%ua,iostat=E_IO)(((X(nx,ny,nz),Y(nx,ny,nz),Z(nx,ny,nz),nx=nx1,nx2),ny=ny1,ny2),nz=nz1,nz2)
    vtk(rf)%indent = vtk(rf)%indent - 2 ; write(unit=vtk(rf)%u,iostat=E_IO)repeat(' ',vtk(rf)%indent)//'</Points>'//end_rec
  case(binary)
    s_buffer = repeat(' ',vtk(rf)%indent)//'<Piece Extent="'//trim(str(n=nx1))//' '//trim(str(n=nx2))//' '// &
                                                              trim(str(n=ny1))//' '//trim(str(n=ny2))//' '// &
                                                              trim(str(n=nz1))//' '//trim(str(n=nz2))//'">'
    write(unit=vtk(rf)%u,iostat=E_IO)trim(s_buffer)//end_rec ; vtk(rf)%indent = vtk(rf)%indent + 2
    write(unit=vtk(rf)%u,iostat=E_IO)repeat(' ',vtk(rf)%indent)//'<Points>'//end_rec ; vtk(rf)%indent = vtk(rf)%indent + 2
    s_buffer = repeat(' ',vtk(rf)%indent)//'<DataArray type="Float32" NumberOfComponents="3" Name="Points" format="binary">'
    write(unit=vtk(rf)%u,iostat=E_IO)trim(s_buffer)//end_rec
    call pack_data(a1=[int(3*NN*BYR4P,I4P)],a2=[(((X(nx,ny,nz),Y(nx,ny,nz),Z(nx,ny,nz),nx=nx1,nx2),ny=ny1,ny2),nz=nz1,nz2)], &
                   packed=XYZp)
    call b64_encode(n=XYZp,code=XYZ64) ; deallocate(XYZp)
    write(unit=vtk(rf)%u,iostat=E_IO)repeat(' ',vtk(rf)%indent+2)//XYZ64//end_rec ; deallocate(XYZ64)
    write(unit=vtk(rf)%u,iostat=E_IO)repeat(' ',vtk(rf)%indent)//'</DataArray>'//end_rec
    vtk(rf)%indent = vtk(rf)%indent - 2 ; write(unit=vtk(rf)%u,iostat=E_IO)repeat(' ',vtk(rf)%indent)//'</Points>'//end_rec
  endselect
  return
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction VTK_GEO_XML_STRG_3DA_R4

  function VTK_GEO_XML_STRG_1DAP_R4(nx1,nx2,ny1,ny2,nz1,nz2,NN,XYZ,cf) result(E_IO)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Function for saving mesh with \b StructuredGrid topology (R4P, 1D Arrays, packed API).
  !---------------------------------------------------------------------------------------------------------------------------------
  implicit none
  integer(I4P), intent(IN)::           nx1        !< Initial node of x axis.
  integer(I4P), intent(IN)::           nx2        !< Final node of x axis.
  integer(I4P), intent(IN)::           ny1        !< Initial node of y axis.
  integer(I4P), intent(IN)::           ny2        !< Final node of y axis.
  integer(I4P), intent(IN)::           nz1        !< Initial node of z axis.
  integer(I4P), intent(IN)::           nz2        !< Final node of z axis.
  integer(I4P), intent(IN)::           NN         !< Number of all nodes.
  real(R4P),    intent(IN)::           XYZ(1:,1:) !< X, Y, Z coordinates (packed API) [1:3,1:NN].
  integer(I4P), intent(IN), optional:: cf         !< Current file index (for concurrent files IO).
  integer(I4P)::                       E_IO       !< Input/Output inquiring flag: $0$ if IO is done, $> 0$ if IO is not done.
  character(len=maxlen)::              s_buffer   !< Buffer string.
  integer(I1P), allocatable::          XYZp(:)    !< Packed data.
  character(len=:), allocatable::      XYZ64      !< X, Y, Z coordinates encoded in base64.
  integer(I4P)::                       rf         !< Real file index.
  integer(I4P)::                       n1         !< Counter.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  E_IO = -1_I4P
  rf = f
  if (present(cf)) then
    rf = cf ; f = cf
  endif
  select case(vtk(rf)%f)
  case(ascii)
    s_buffer = repeat(' ',vtk(rf)%indent)//'<Piece Extent="'//trim(str(n=nx1))//' '//trim(str(n=nx2))//' '// &
                                                              trim(str(n=ny1))//' '//trim(str(n=ny2))//' '// &
                                                              trim(str(n=nz1))//' '//trim(str(n=nz2))//'">'
    write(unit=vtk(rf)%u,fmt='(A)',iostat=E_IO)trim(s_buffer) ; vtk(rf)%indent = vtk(rf)%indent + 2
    write(unit=vtk(rf)%u,fmt='(A)',iostat=E_IO)repeat(' ',vtk(rf)%indent)//'<Points>' ; vtk(rf)%indent = vtk(rf)%indent + 2
    s_buffer = repeat(' ',vtk(rf)%indent)//'<DataArray type="Float32" NumberOfComponents="3" Name="Points" format="ascii">'
    write(unit=vtk(rf)%u,fmt='(A)',iostat=E_IO)trim(s_buffer)
    do n1=1,NN
      write(unit=vtk(rf)%u,fmt='(A)',iostat=E_IO)repeat(' ',vtk(rf)%indent)//&
                                                 str(n=XYZ(1,n1))//' '//str(n=XYZ(2,n1))//' '//str(n=XYZ(3,n1))
    enddo
    write(unit=vtk(rf)%u,fmt='(A)',iostat=E_IO)repeat(' ',vtk(rf)%indent)//'</DataArray>' ; vtk(rf)%indent = vtk(rf)%indent - 2
    write(unit=vtk(rf)%u,fmt='(A)',iostat=E_IO)repeat(' ',vtk(rf)%indent)//'</Points>'
  case(raw,bin_app)
    s_buffer = repeat(' ',vtk(rf)%indent)//'<Piece Extent="'//trim(str(n=nx1))//' '//trim(str(n=nx2))//' '// &
                                                              trim(str(n=ny1))//' '//trim(str(n=ny2))//' '// &
                                                              trim(str(n=nz1))//' '//trim(str(n=nz2))//'">'
    write(unit=vtk(rf)%u,iostat=E_IO)trim(s_buffer)//end_rec ; vtk(rf)%indent = vtk(rf)%indent + 2
    write(unit=vtk(rf)%u,iostat=E_IO)repeat(' ',vtk(rf)%indent)//'<Points>'//end_rec ; vtk(rf)%indent = vtk(rf)%indent + 2
    s_buffer = repeat(' ',vtk(rf)%indent)//                                                                  &
               '<DataArray type="Float32" NumberOfComponents="3" Name="Points" format="appended" offset="'// &
               trim(str(.true.,vtk(rf)%ioffset))//'"/>'
    write(unit=vtk(rf)%u,iostat=E_IO)trim(s_buffer)//end_rec
    call vtk(rf)%byte_update(N_Byte = 3*NN*BYR4P)
    write(unit=vtk(rf)%ua,iostat=E_IO)vtk(rf)%N_Byte,'R4',3*NN
    write(unit=vtk(rf)%ua,iostat=E_IO)XYZ
    vtk(rf)%indent = vtk(rf)%indent - 2 ; write(unit=vtk(rf)%u,iostat=E_IO)repeat(' ',vtk(rf)%indent)//'</Points>'//end_rec
  case(binary)
    s_buffer = repeat(' ',vtk(rf)%indent)//'<Piece Extent="'//trim(str(n=nx1))//' '//trim(str(n=nx2))//' '// &
                                                              trim(str(n=ny1))//' '//trim(str(n=ny2))//' '// &
                                                              trim(str(n=nz1))//' '//trim(str(n=nz2))//'">'
    write(unit=vtk(rf)%u,iostat=E_IO)trim(s_buffer)//end_rec ; vtk(rf)%indent = vtk(rf)%indent + 2
    write(unit=vtk(rf)%u,iostat=E_IO)repeat(' ',vtk(rf)%indent)//'<Points>'//end_rec ; vtk(rf)%indent = vtk(rf)%indent + 2
    s_buffer = repeat(' ',vtk(rf)%indent)//'<DataArray type="Float32" NumberOfComponents="3" Name="Points" format="binary">'
    write(unit=vtk(rf)%u,iostat=E_IO)trim(s_buffer)//end_rec
    call pack_data(a1=[int(3*NN*BYR4P,I4P)],a2=reshape(XYZ,[3*NN]),packed=XYZp)
    call b64_encode(n=XYZp,code=XYZ64) ; deallocate(XYZp)
    write(unit=vtk(rf)%u,iostat=E_IO)repeat(' ',vtk(rf)%indent+2)//XYZ64//end_rec ; deallocate(XYZ64)
    write(unit=vtk(rf)%u,iostat=E_IO)repeat(' ',vtk(rf)%indent)//'</DataArray>'//end_rec
    vtk(rf)%indent = vtk(rf)%indent - 2 ; write(unit=vtk(rf)%u,iostat=E_IO)repeat(' ',vtk(rf)%indent)//'</Points>'//end_rec
  endselect
  return
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction VTK_GEO_XML_STRG_1DAP_R4

  function VTK_GEO_XML_STRG_3DAP_R4(nx1,nx2,ny1,ny2,nz1,nz2,NN,XYZ,cf) result(E_IO)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Function for saving mesh with \b StructuredGrid topology (R4P, 3D Arrays, packed API).
  !---------------------------------------------------------------------------------------------------------------------------------
  implicit none
  integer(I4P), intent(IN)::           nx1                    !< Initial node of x axis.
  integer(I4P), intent(IN)::           nx2                    !< Final node of x axis.
  integer(I4P), intent(IN)::           ny1                    !< Initial node of y axis.
  integer(I4P), intent(IN)::           ny2                    !< Final node of y axis.
  integer(I4P), intent(IN)::           nz1                    !< Initial node of z axis.
  integer(I4P), intent(IN)::           nz2                    !< Final node of z axis.
  integer(I4P), intent(IN)::           NN                     !< Number of all nodes.
  real(R4P),    intent(IN)::           XYZ(1:,nx1:,ny1:,nz1:) !< X, Y, Z coordinates (packed API) [1:3,nx1:nx2,ny1:ny2,nz1:nz2].
  integer(I4P), intent(IN), optional:: cf                     !< Current file index (for concurrent files IO).
  integer(I4P)::                       E_IO             !< Input/Output inquiring flag: $0$ if IO is done, $> 0$ if IO is not done.
  character(len=maxlen)::              s_buffer               !< Buffer string.
  integer(I1P), allocatable::          XYZp(:)                !< Packed data.
  character(len=:), allocatable::      XYZ64                  !< X, Y, Z coordinates encoded in base64.
  integer(I4P)::                       rf                     !< Real file index.
  integer(I4P)::                       nx,ny,nz               !< Counters.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  E_IO = -1_I4P
  rf = f
  if (present(cf)) then
    rf = cf ; f = cf
  endif
  select case(vtk(rf)%f)
  case(ascii)
    s_buffer = repeat(' ',vtk(rf)%indent)//'<Piece Extent="'//trim(str(n=nx1))//' '//trim(str(n=nx2))//' '// &
                                                              trim(str(n=ny1))//' '//trim(str(n=ny2))//' '// &
                                                              trim(str(n=nz1))//' '//trim(str(n=nz2))//'">'
    write(unit=vtk(rf)%u,fmt='(A)',iostat=E_IO)trim(s_buffer) ; vtk(rf)%indent = vtk(rf)%indent + 2
    write(unit=vtk(rf)%u,fmt='(A)',iostat=E_IO)repeat(' ',vtk(rf)%indent)//'<Points>' ; vtk(rf)%indent = vtk(rf)%indent + 2
    s_buffer = repeat(' ',vtk(rf)%indent)//'<DataArray type="Float32" NumberOfComponents="3" Name="Points" format="ascii">'
    write(unit=vtk(rf)%u,fmt='(A)',iostat=E_IO)trim(s_buffer)
    do nz=nz1,nz2
      do ny=ny1,ny2
        do nx=nx1,nx2
          write(unit=vtk(rf)%u,fmt='(A)',iostat=E_IO)repeat(' ',vtk(rf)%indent)//&
                                                    str(n=XYZ(1,nx,ny,nz))//' '//str(n=XYZ(2,nx,ny,nz))//' '//str(n=XYZ(3,nx,ny,nz))
        enddo
      enddo
    enddo
    write(unit=vtk(rf)%u,fmt='(A)',iostat=E_IO)repeat(' ',vtk(rf)%indent)//'</DataArray>' ; vtk(rf)%indent = vtk(rf)%indent - 2
    write(unit=vtk(rf)%u,fmt='(A)',iostat=E_IO)repeat(' ',vtk(rf)%indent)//'</Points>'
  case(raw,bin_app)
    s_buffer = repeat(' ',vtk(rf)%indent)//'<Piece Extent="'//trim(str(n=nx1))//' '//trim(str(n=nx2))//' '// &
                                                              trim(str(n=ny1))//' '//trim(str(n=ny2))//' '// &
                                                              trim(str(n=nz1))//' '//trim(str(n=nz2))//'">'
    write(unit=vtk(rf)%u,iostat=E_IO)trim(s_buffer)//end_rec ; vtk(rf)%indent = vtk(rf)%indent + 2
    write(unit=vtk(rf)%u,iostat=E_IO)repeat(' ',vtk(rf)%indent)//'<Points>'//end_rec ; vtk(rf)%indent = vtk(rf)%indent + 2
    s_buffer = repeat(' ',vtk(rf)%indent)//                                                                  &
               '<DataArray type="Float32" NumberOfComponents="3" Name="Points" format="appended" offset="'// &
               trim(str(.true.,vtk(rf)%ioffset))//'"/>'
    write(unit=vtk(rf)%u,iostat=E_IO)trim(s_buffer)//end_rec
    call vtk(rf)%byte_update(N_Byte = 3*NN*BYR4P)
    write(unit=vtk(rf)%ua,iostat=E_IO)vtk(rf)%N_Byte,'R4',3*NN
    write(unit=vtk(rf)%ua,iostat=E_IO)XYZ
    vtk(rf)%indent = vtk(rf)%indent - 2 ; write(unit=vtk(rf)%u,iostat=E_IO)repeat(' ',vtk(rf)%indent)//'</Points>'//end_rec
  case(binary)
    s_buffer = repeat(' ',vtk(rf)%indent)//'<Piece Extent="'//trim(str(n=nx1))//' '//trim(str(n=nx2))//' '// &
                                                              trim(str(n=ny1))//' '//trim(str(n=ny2))//' '// &
                                                              trim(str(n=nz1))//' '//trim(str(n=nz2))//'">'
    write(unit=vtk(rf)%u,iostat=E_IO)trim(s_buffer)//end_rec ; vtk(rf)%indent = vtk(rf)%indent + 2
    write(unit=vtk(rf)%u,iostat=E_IO)repeat(' ',vtk(rf)%indent)//'<Points>'//end_rec ; vtk(rf)%indent = vtk(rf)%indent + 2
    s_buffer = repeat(' ',vtk(rf)%indent)//'<DataArray type="Float32" NumberOfComponents="3" Name="Points" format="binary">'
    write(unit=vtk(rf)%u,iostat=E_IO)trim(s_buffer)//end_rec
    call pack_data(a1=[int(3*NN*BYR4P,I4P)],a2=reshape(XYZ,[3*NN]),packed=XYZp)
    call b64_encode(n=XYZp,code=XYZ64) ; deallocate(XYZp)
    write(unit=vtk(rf)%u,iostat=E_IO)repeat(' ',vtk(rf)%indent+2)//XYZ64//end_rec ; deallocate(XYZ64)
    write(unit=vtk(rf)%u,iostat=E_IO)repeat(' ',vtk(rf)%indent)//'</DataArray>'//end_rec
    vtk(rf)%indent = vtk(rf)%indent - 2 ; write(unit=vtk(rf)%u,iostat=E_IO)repeat(' ',vtk(rf)%indent)//'</Points>'//end_rec
  endselect
  return
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction VTK_GEO_XML_STRG_3DAP_R4

  function VTK_GEO_XML_RECT_R8(nx1,nx2,ny1,ny2,nz1,nz2,X,Y,Z,cf) result(E_IO)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Function for saving mesh with \b RectilinearGrid topology (R8P).
  !---------------------------------------------------------------------------------------------------------------------------------
  implicit none
  integer(I4P), intent(IN)::           nx1        !< Initial node of x axis.
  integer(I4P), intent(IN)::           nx2        !< Final node of x axis.
  integer(I4P), intent(IN)::           ny1        !< Initial node of y axis.
  integer(I4P), intent(IN)::           ny2        !< Final node of y axis.
  integer(I4P), intent(IN)::           nz1        !< Initial node of z axis.
  integer(I4P), intent(IN)::           nz2        !< Final node of z axis.
  real(R8P),    intent(IN)::           X(nx1:nx2) !< X coordinates.
  real(R8P),    intent(IN)::           Y(ny1:ny2) !< Y coordinates.
  real(R8P),    intent(IN)::           Z(nz1:nz2) !< Z coordinates.
  integer(I4P), intent(IN), optional:: cf         !< Current file index (for concurrent files IO).
  integer(I4P)::                       E_IO       !< Input/Output inquiring flag: $0$ if IO is done, $> 0$ if IO is not done.
  character(len=maxlen)::              s_buffer   !< Buffer string.
  integer(I1P), allocatable::          XYZp(:)    !< Packed data.
  character(len=:), allocatable::      XYZ64      !< X, Y, Z coordinates encoded in base64.
  integer(I4P)::                       rf         !< Real file index.
  integer(I4P)::                       n1         !< Counter.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  E_IO = -1_I4P
  rf = f
  if (present(cf)) then
    rf = cf ; f = cf
  endif
  select case(vtk(rf)%f)
  case(ascii)
    s_buffer = repeat(' ',vtk(rf)%indent)//'<Piece Extent="'//trim(str(n=nx1))//' '//trim(str(n=nx2))//' '// &
                                                              trim(str(n=ny1))//' '//trim(str(n=ny2))//' '// &
                                                              trim(str(n=nz1))//' '//trim(str(n=nz2))//'">'
    write(unit=vtk(rf)%u,fmt='(A)',iostat=E_IO)trim(s_buffer) ; vtk(rf)%indent = vtk(rf)%indent + 2
    write(unit=vtk(rf)%u,fmt='(A)',iostat=E_IO)repeat(' ',vtk(rf)%indent)//'<Coordinates>' ; vtk(rf)%indent = vtk(rf)%indent + 2
    write(unit=vtk(rf)%u,fmt='(A)',iostat=E_IO)repeat(' ',vtk(rf)%indent)//'<DataArray type="Float64" Name="X" format="ascii">'
    write(unit=vtk(rf)%u,fmt=FR8P, iostat=E_IO)(X(n1),n1=nx1,nx2)
    write(unit=vtk(rf)%u,fmt='(A)',iostat=E_IO)repeat(' ',vtk(rf)%indent)//'</DataArray>'
    write(unit=vtk(rf)%u,fmt='(A)',iostat=E_IO)repeat(' ',vtk(rf)%indent)//'<DataArray type="Float64" Name="Y" format="ascii">'
    write(unit=vtk(rf)%u,fmt=FR8P, iostat=E_IO)(Y(n1),n1=ny1,ny2)
    write(unit=vtk(rf)%u,fmt='(A)',iostat=E_IO)repeat(' ',vtk(rf)%indent)//'</DataArray>'
    write(unit=vtk(rf)%u,fmt='(A)',iostat=E_IO)repeat(' ',vtk(rf)%indent)//'<DataArray type="Float64" Name="Z" format="ascii">'
    write(unit=vtk(rf)%u,fmt=FR8P, iostat=E_IO)(Z(n1),n1=nz1,nz2)
    write(unit=vtk(rf)%u,fmt='(A)',iostat=E_IO)repeat(' ',vtk(rf)%indent)//'</DataArray>' ; vtk(rf)%indent = vtk(rf)%indent - 2
    write(unit=vtk(rf)%u,fmt='(A)',iostat=E_IO)repeat(' ',vtk(rf)%indent)//'</Coordinates>'
  case(raw,bin_app)
    s_buffer = repeat(' ',vtk(rf)%indent)//'<Piece Extent="'//trim(str(n=nx1))//' '//trim(str(n=nx2))//' '// &
                                                              trim(str(n=ny1))//' '//trim(str(n=ny2))//' '// &
                                                              trim(str(n=nz1))//' '//trim(str(n=nz2))//'">'
    write(unit=vtk(rf)%u,iostat=E_IO)trim(s_buffer)//end_rec ; vtk(rf)%indent = vtk(rf)%indent + 2
    write(unit=vtk(rf)%u,iostat=E_IO)repeat(' ',vtk(rf)%indent)//'<Coordinates>'//end_rec ; vtk(rf)%indent = vtk(rf)%indent + 2
    s_buffer = repeat(' ',vtk(rf)%indent)//'<DataArray type="Float64" Name="X" format="appended" offset="'//&
               trim(str(.true.,vtk(rf)%ioffset))//'"/>'
    write(unit=vtk(rf)%u,iostat=E_IO)trim(s_buffer)//end_rec
    call vtk(rf)%byte_update(N_Byte = (nx2-nx1+1)*BYR8P)
    write(unit=vtk(rf)%ua,iostat=E_IO)vtk(rf)%N_Byte,'R8',(nx2-nx1+1)
    write(unit=vtk(rf)%ua,iostat=E_IO)(X(n1),n1=nx1,nx2)
    s_buffer = repeat(' ',vtk(rf)%indent)//'<DataArray type="Float64" Name="Y" format="appended" offset="'//&
               trim(str(.true.,vtk(rf)%ioffset))//'"/>'
    write(unit=vtk(rf)%u,iostat=E_IO)trim(s_buffer)//end_rec
    call vtk(rf)%byte_update(N_Byte = (ny2-ny1+1)*BYR8P)
    write(unit=vtk(rf)%ua,iostat=E_IO)vtk(rf)%N_Byte,'R8',(ny2-ny1+1)
    write(unit=vtk(rf)%ua,iostat=E_IO)(Y(n1),n1=ny1,ny2)
    s_buffer = repeat(' ',vtk(rf)%indent)//'<DataArray type="Float64" Name="Z" format="appended" offset="'//&
               trim(str(.true.,vtk(rf)%ioffset))//'"/>'
    write(unit=vtk(rf)%u,iostat=E_IO)trim(s_buffer)//end_rec
    call vtk(rf)%byte_update(N_Byte = (nz2-nz1+1)*BYR8P)
    write(unit=vtk(rf)%ua,iostat=E_IO)vtk(rf)%N_Byte,'R8',(nz2-nz1+1)
    write(unit=vtk(rf)%ua,iostat=E_IO)(Z(n1),n1=nz1,nz2)
    vtk(rf)%indent = vtk(rf)%indent - 2 ; write(unit=vtk(rf)%u,iostat=E_IO)repeat(' ',vtk(rf)%indent)//'</Coordinates>'//end_rec
  case(binary)
    s_buffer = repeat(' ',vtk(rf)%indent)//'<Piece Extent="'//trim(str(n=nx1))//' '//trim(str(n=nx2))//' '// &
                                                              trim(str(n=ny1))//' '//trim(str(n=ny2))//' '// &
                                                              trim(str(n=nz1))//' '//trim(str(n=nz2))//'">'
    write(unit=vtk(rf)%u,iostat=E_IO)trim(s_buffer)//end_rec ; vtk(rf)%indent = vtk(rf)%indent + 2
    s_buffer = repeat(' ',vtk(rf)%indent)//'<Coordinates>'
    write(unit=vtk(rf)%u,iostat=E_IO)trim(s_buffer)//end_rec ; vtk(rf)%indent = vtk(rf)%indent + 2
    s_buffer = repeat(' ',vtk(rf)%indent)//'<DataArray type="Float64" Name="X" format="binary">'
    write(unit=vtk(rf)%u,iostat=E_IO)trim(s_buffer)//end_rec
    call pack_data(a1=[int((nx2-nx1+1)*BYR8P,I4P)],a2=X,packed=XYZp)
    call b64_encode(n=XYZp,code=XYZ64)
    write(unit=vtk(rf)%u,iostat=E_IO)repeat(' ',vtk(rf)%indent+2)//XYZ64//end_rec
    write(unit=vtk(rf)%u,iostat=E_IO)repeat(' ',vtk(rf)%indent)//'</DataArray>'//end_rec
    s_buffer = repeat(' ',vtk(rf)%indent)//'<DataArray type="Float64" Name="Y" format="binary">'
    write(unit=vtk(rf)%u,iostat=E_IO)trim(s_buffer)//end_rec
    call pack_data(a1=[int((ny2-ny1+1)*BYR8P,I4P)],a2=Y,packed=XYZp)
    call b64_encode(n=XYZp,code=XYZ64)
    write(unit=vtk(rf)%u,iostat=E_IO)repeat(' ',vtk(rf)%indent+2)//XYZ64//end_rec
    write(unit=vtk(rf)%u,iostat=E_IO)repeat(' ',vtk(rf)%indent)//'</DataArray>'//end_rec
    s_buffer = repeat(' ',vtk(rf)%indent)//'<DataArray type="Float64" Name="Z" format="binary">'
    write(unit=vtk(rf)%u,iostat=E_IO)trim(s_buffer)//end_rec
    call pack_data(a1=[int((nz2-nz1+1)*BYR8P,I4P)],a2=Z,packed=XYZp)
    call b64_encode(n=XYZp,code=XYZ64) ; deallocate(XYZp)
    write(unit=vtk(rf)%u,iostat=E_IO)repeat(' ',vtk(rf)%indent+2)//XYZ64//end_rec ; deallocate(XYZ64)
    write(unit=vtk(rf)%u,iostat=E_IO)repeat(' ',vtk(rf)%indent)//'</DataArray>'//end_rec
    vtk(rf)%indent = vtk(rf)%indent - 2
    write(unit=vtk(rf)%u,iostat=E_IO)repeat(' ',vtk(rf)%indent)//'</Coordinates>'//end_rec
  endselect
  return
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction VTK_GEO_XML_RECT_R8

  function VTK_GEO_XML_RECT_R4(nx1,nx2,ny1,ny2,nz1,nz2,X,Y,Z,cf) result(E_IO)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Function for saving mesh with \b RectilinearGrid topology (R4P).
  !---------------------------------------------------------------------------------------------------------------------------------
  implicit none
  integer(I4P), intent(IN)::           nx1        !< Initial node of x axis.
  integer(I4P), intent(IN)::           nx2        !< Final node of x axis.
  integer(I4P), intent(IN)::           ny1        !< Initial node of y axis.
  integer(I4P), intent(IN)::           ny2        !< Final node of y axis.
  integer(I4P), intent(IN)::           nz1        !< Initial node of z axis.
  integer(I4P), intent(IN)::           nz2        !< Final node of z axis.
  real(R4P),    intent(IN)::           X(nx1:nx2) !< X coordinates.
  real(R4P),    intent(IN)::           Y(ny1:ny2) !< Y coordinates.
  real(R4P),    intent(IN)::           Z(nz1:nz2) !< Z coordinates.
  integer(I4P), intent(IN), optional:: cf         !< Current file index (for concurrent files IO).
  integer(I4P)::                       E_IO       !< Input/Output inquiring flag: $0$ if IO is done, $> 0$ if IO is not done.
  character(len=maxlen)::              s_buffer   !< Buffer string.
  integer(I1P), allocatable::          XYZp(:)    !< Packed data.
  character(len=:), allocatable::      XYZ64      !< X, Y, Z coordinates encoded in base64.
  integer(I4P)::                       rf         !< Real file index.
  integer(I4P)::                       n1         !< Counter.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  E_IO = -1_I4P
  rf = f
  if (present(cf)) then
    rf = cf ; f = cf
  endif
  select case(vtk(rf)%f)
  case(ascii)
    s_buffer = repeat(' ',vtk(rf)%indent)//'<Piece Extent="'//trim(str(n=nx1))//' '//trim(str(n=nx2))//' '// &
                                                      trim(str(n=ny1))//' '//trim(str(n=ny2))//' '// &
                                                      trim(str(n=nz1))//' '//trim(str(n=nz2))//'">'
    write(unit=vtk(rf)%u,fmt='(A)',iostat=E_IO)trim(s_buffer) ; vtk(rf)%indent = vtk(rf)%indent + 2
    write(unit=vtk(rf)%u,fmt='(A)',iostat=E_IO)repeat(' ',vtk(rf)%indent)//'<Coordinates>' ; vtk(rf)%indent = vtk(rf)%indent + 2
    write(unit=vtk(rf)%u,fmt='(A)',iostat=E_IO)repeat(' ',vtk(rf)%indent)//'<DataArray type="Float32" Name="X" format="ascii">'
    write(unit=vtk(rf)%u,fmt=FR4P, iostat=E_IO)(X(n1),n1=nx1,nx2)
    write(unit=vtk(rf)%u,fmt='(A)',iostat=E_IO)repeat(' ',vtk(rf)%indent)//'</DataArray>'
    write(unit=vtk(rf)%u,fmt='(A)',iostat=E_IO)repeat(' ',vtk(rf)%indent)//'<DataArray type="Float32" Name="Y" format="ascii">'
    write(unit=vtk(rf)%u,fmt=FR4P, iostat=E_IO)(Y(n1),n1=ny1,ny2)
    write(unit=vtk(rf)%u,fmt='(A)',iostat=E_IO)repeat(' ',vtk(rf)%indent)//'</DataArray>'
    write(unit=vtk(rf)%u,fmt='(A)',iostat=E_IO)repeat(' ',vtk(rf)%indent)//'<DataArray type="Float32" Name="Z" format="ascii">'
    write(unit=vtk(rf)%u,fmt=FR4P, iostat=E_IO)(Z(n1),n1=nz1,nz2)
    write(unit=vtk(rf)%u,fmt='(A)',iostat=E_IO)repeat(' ',vtk(rf)%indent)//'</DataArray>' ; vtk(rf)%indent = vtk(rf)%indent - 2
    write(unit=vtk(rf)%u,fmt='(A)',iostat=E_IO)repeat(' ',vtk(rf)%indent)//'</Coordinates>'
  case(raw,bin_app)
    s_buffer = repeat(' ',vtk(rf)%indent)//'<Piece Extent="'//trim(str(n=nx1))//' '//trim(str(n=nx2))//' '// &
                                                      trim(str(n=ny1))//' '//trim(str(n=ny2))//' '// &
                                                      trim(str(n=nz1))//' '//trim(str(n=nz2))//'">'
    write(unit=vtk(rf)%u,iostat=E_IO)trim(s_buffer)//end_rec ; vtk(rf)%indent = vtk(rf)%indent + 2
    write(unit=vtk(rf)%u,iostat=E_IO)repeat(' ',vtk(rf)%indent)//'<Coordinates>'//end_rec ; vtk(rf)%indent = vtk(rf)%indent + 2
    s_buffer = repeat(' ',vtk(rf)%indent)//'<DataArray type="Float32" Name="X" format="appended" offset="'//&
               trim(str(.true.,vtk(rf)%ioffset))//'"/>'
    write(unit=vtk(rf)%u,iostat=E_IO)trim(s_buffer)//end_rec
    call vtk(rf)%byte_update(N_Byte = (nx2-nx1+1)*BYR4P)
    write(unit=vtk(rf)%ua,iostat=E_IO)vtk(rf)%N_Byte,'R4',(nx2-nx1+1)
    write(unit=vtk(rf)%ua,iostat=E_IO)(X(n1),n1=nx1,nx2)
    s_buffer = repeat(' ',vtk(rf)%indent)//'<DataArray type="Float32" Name="Y" format="appended" offset="'//&
               trim(str(.true.,vtk(rf)%ioffset))//'"/>'
    write(unit=vtk(rf)%u,iostat=E_IO)trim(s_buffer)//end_rec
    call vtk(rf)%byte_update(N_Byte = (ny2-ny1+1)*BYR4P)
    write(unit=vtk(rf)%ua,iostat=E_IO)vtk(rf)%N_Byte,'R4',(ny2-ny1+1)
    write(unit=vtk(rf)%ua,iostat=E_IO)(Y(n1),n1=ny1,ny2)
    s_buffer = repeat(' ',vtk(rf)%indent)//'<DataArray type="Float32" Name="Z" format="appended" offset="'//&
               trim(str(.true.,vtk(rf)%ioffset))//'"/>'
    write(unit=vtk(rf)%u,iostat=E_IO)trim(s_buffer)//end_rec
    call vtk(rf)%byte_update(N_Byte = (nz2-nz1+1)*BYR4P)
    write(unit=vtk(rf)%ua,iostat=E_IO)vtk(rf)%N_Byte,'R4',(nz2-nz1+1)
    write(unit=vtk(rf)%ua,iostat=E_IO)(Z(n1),n1=nz1,nz2)
    vtk(rf)%indent = vtk(rf)%indent - 2  ; write(unit=vtk(rf)%u,iostat=E_IO)repeat(' ',vtk(rf)%indent)//'</Coordinates>'//end_rec
  case(binary)
    s_buffer = repeat(' ',vtk(rf)%indent)//'<Piece Extent="'//trim(str(n=nx1))//' '//trim(str(n=nx2))//' '// &
                                                              trim(str(n=ny1))//' '//trim(str(n=ny2))//' '// &
                                                              trim(str(n=nz1))//' '//trim(str(n=nz2))//'">'
    write(unit=vtk(rf)%u,iostat=E_IO)trim(s_buffer)//end_rec ; vtk(rf)%indent = vtk(rf)%indent + 2
    s_buffer = repeat(' ',vtk(rf)%indent)//'<Coordinates>'
    write(unit=vtk(rf)%u,iostat=E_IO)trim(s_buffer)//end_rec ; vtk(rf)%indent = vtk(rf)%indent + 2
    s_buffer = repeat(' ',vtk(rf)%indent)//'<DataArray type="Float32" Name="X" format="binary">'
    write(unit=vtk(rf)%u,iostat=E_IO)trim(s_buffer)//end_rec
    call pack_data(a1=[int((nx2-nx1+1)*BYR4P,I4P)],a2=X,packed=XYZp)
    call b64_encode(n=XYZp,code=XYZ64)
    write(unit=vtk(rf)%u,iostat=E_IO)repeat(' ',vtk(rf)%indent+2)//XYZ64//end_rec
    write(unit=vtk(rf)%u,iostat=E_IO)repeat(' ',vtk(rf)%indent)//'</DataArray>'//end_rec
    s_buffer = repeat(' ',vtk(rf)%indent)//'<DataArray type="Float32" Name="Y" format="binary">'
    write(unit=vtk(rf)%u,iostat=E_IO)trim(s_buffer)//end_rec
    call pack_data(a1=[int((ny2-ny1+1)*BYR4P,I4P)],a2=Y,packed=XYZp)
    call b64_encode(n=XYZp,code=XYZ64)
    write(unit=vtk(rf)%u,iostat=E_IO)repeat(' ',vtk(rf)%indent+2)//XYZ64//end_rec
    write(unit=vtk(rf)%u,iostat=E_IO)repeat(' ',vtk(rf)%indent)//'</DataArray>'//end_rec
    s_buffer = repeat(' ',vtk(rf)%indent)//'<DataArray type="Float32" Name="Z" format="binary">'
    write(unit=vtk(rf)%u,iostat=E_IO)trim(s_buffer)//end_rec
    call pack_data(a1=[int((nz2-nz1+1)*BYR4P,I4P)],a2=Z,packed=XYZp)
    call b64_encode(n=XYZp,code=XYZ64) ; deallocate(XYZp)
    write(unit=vtk(rf)%u,iostat=E_IO)repeat(' ',vtk(rf)%indent+2)//XYZ64//end_rec ; deallocate(XYZ64)
    write(unit=vtk(rf)%u,iostat=E_IO)repeat(' ',vtk(rf)%indent)//'</DataArray>'//end_rec
    vtk(rf)%indent = vtk(rf)%indent - 2
    write(unit=vtk(rf)%u,iostat=E_IO)repeat(' ',vtk(rf)%indent)//'</Coordinates>'//end_rec
  endselect
  return
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction VTK_GEO_XML_RECT_R4

  function VTK_GEO_XML_UNST_R8(NN,NC,X,Y,Z,cf) result(E_IO)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Function for saving mesh with \b UnstructuredGrid topology (R8P).
  !---------------------------------------------------------------------------------------------------------------------------------
  implicit none
  integer(I4P), intent(IN)::           NN       !< Number of nodes.
  integer(I4P), intent(IN)::           NC       !< Number of cells.
  real(R8P),    intent(IN)::           X(1:NN)  !< X coordinates.
  real(R8P),    intent(IN)::           Y(1:NN)  !< Y coordinates.
  real(R8P),    intent(IN)::           Z(1:NN)  !< Z coordinates.
  integer(I4P), intent(IN), optional:: cf       !< Current file index (for concurrent files IO).
  integer(I4P)::                       E_IO     !< Input/Output inquiring flag: $0$ if IO is done, $> 0$ if IO is not done.
  character(len=maxlen)::              s_buffer !< Buffer string.
  real(R8P), allocatable::             XYZa(:)  !< X, Y, Z coordinates.
  integer(I1P), allocatable::          XYZp(:)  !< Packed data.
  character(len=:), allocatable::      XYZ64    !< X, Y, Z coordinates encoded in base64.
  integer(I4P)::                       rf       !< Real file index.
  integer(I4P)::                       n1       !< Counter.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  E_IO = -1_I4P
  rf = f
  if (present(cf)) then
    rf = cf ; f = cf
  endif
  select case(vtk(rf)%f)
  case(ascii)
    s_buffer = repeat(' ',vtk(rf)%indent)//'<Piece NumberOfPoints="'//trim(str(n=NN))//'" NumberOfCells="'//trim(str(n=NC))//'">'
    write(unit=vtk(rf)%u,fmt='(A)',iostat=E_IO)trim(s_buffer) ; vtk(rf)%indent = vtk(rf)%indent + 2
    write(unit=vtk(rf)%u,fmt='(A)',iostat=E_IO)repeat(' ',vtk(rf)%indent)//'<Points>' ; vtk(rf)%indent = vtk(rf)%indent + 2
    s_buffer = repeat(' ',vtk(rf)%indent)//'<DataArray type="Float64" NumberOfComponents="3" Name="Points" format="ascii">'
    write(unit=vtk(rf)%u,fmt='(A)',iostat=E_IO)trim(s_buffer)
    do n1=1,NN
      write(unit=vtk(rf)%u,fmt='(A)',iostat=E_IO)repeat(' ',vtk(rf)%indent)//str(n=X(n1))//' '//str(n=Y(n1))//' '//str(n=Z(n1))
    enddo
    write(unit=vtk(rf)%u,fmt='(A)',iostat=E_IO)repeat(' ',vtk(rf)%indent)//'</DataArray>' ; vtk(rf)%indent = vtk(rf)%indent - 2
    write(unit=vtk(rf)%u,fmt='(A)',iostat=E_IO)repeat(' ',vtk(rf)%indent)//'</Points>'
  case(raw,bin_app)
    s_buffer = repeat(' ',vtk(rf)%indent)//'<Piece NumberOfPoints="'//trim(str(n=NN))//'" NumberOfCells="'//trim(str(n=NC))//'">'
    write(unit=vtk(rf)%u,iostat=E_IO)trim(s_buffer)//end_rec ; vtk(rf)%indent = vtk(rf)%indent + 2
    write(unit=vtk(rf)%u,iostat=E_IO)repeat(' ',vtk(rf)%indent)//'<Points>'//end_rec ; vtk(rf)%indent = vtk(rf)%indent + 2
    s_buffer = repeat(' ',vtk(rf)%indent)//                                                                  &
               '<DataArray type="Float64" NumberOfComponents="3" Name="Points" format="appended" offset="'// &
               trim(str(.true.,vtk(rf)%ioffset))//'"/>'
    write(unit=vtk(rf)%u,iostat=E_IO)trim(s_buffer)//end_rec
    call vtk(rf)%byte_update(N_Byte = 3*NN*BYR8P)
    write(unit=vtk(rf)%ua,iostat=E_IO)vtk(rf)%N_Byte,'R8',3*NN
    write(unit=vtk(rf)%ua,iostat=E_IO)(X(n1),Y(n1),Z(n1),n1=1,NN)
    vtk(rf)%indent = vtk(rf)%indent - 2 ; write(unit=vtk(rf)%u,iostat=E_IO)repeat(' ',vtk(rf)%indent)//'</Points>'//end_rec
  case(binary)
    s_buffer = repeat(' ',vtk(rf)%indent)//'<Piece NumberOfPoints="'//trim(str(n=NN))//'" NumberOfCells="'//trim(str(n=NC))//'">'
    write(unit=vtk(rf)%u,iostat=E_IO)trim(s_buffer)//end_rec ; vtk(rf)%indent = vtk(rf)%indent + 2
    write(unit=vtk(rf)%u,iostat=E_IO)repeat(' ',vtk(rf)%indent)//'<Points>'//end_rec ; vtk(rf)%indent = vtk(rf)%indent + 2
    s_buffer = repeat(' ',vtk(rf)%indent)//'<DataArray type="Float64" NumberOfComponents="3" Name="Points" format="binary">'
    write(unit=vtk(rf)%u,iostat=E_IO)trim(s_buffer)//end_rec
    allocate(XYZa(1:3*NN))
    do n1 = 1,NN
      XYZa(1+(n1-1)*3:1+(n1-1)*3+2)=[X(n1),Y(n1),Z(n1)]
    enddo
    call pack_data(a1=[int(3*NN*BYR8P,I4P)],a2=XYZa,packed=XYZp) ; deallocate(XYZa)
    call b64_encode(n=XYZp,code=XYZ64) ; deallocate(XYZp)
    write(unit=vtk(rf)%u,iostat=E_IO)repeat(' ',vtk(rf)%indent+2)//XYZ64//end_rec ; deallocate(XYZ64)
    write(unit=vtk(rf)%u,iostat=E_IO)repeat(' ',vtk(rf)%indent)//'</DataArray>'//end_rec
    vtk(rf)%indent = vtk(rf)%indent - 2 ; write(unit=vtk(rf)%u,iostat=E_IO)repeat(' ',vtk(rf)%indent)//'</Points>'//end_rec
  endselect
  return
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction VTK_GEO_XML_UNST_R8

  function VTK_GEO_XML_UNST_PACK_R8(NN,NC,XYZ,cf) result(E_IO)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Function for saving mesh with \b UnstructuredGrid topology (R8P, packed API).
  !---------------------------------------------------------------------------------------------------------------------------------
  implicit none
  integer(I4P), intent(IN)::           NN            !< Number of nodes.
  integer(I4P), intent(IN)::           NC            !< Number of cells.
  real(R8P),    intent(IN)::           XYZ(1:3,1:NN) !< X, Y, Z coordinates (packed API).
  integer(I4P), intent(IN), optional:: cf            !< Current file index (for concurrent files IO).
  integer(I4P)::                       E_IO          !< Input/Output inquiring flag: $0$ if IO is done, $> 0$ if IO is not done.
  character(len=maxlen)::              s_buffer      !< Buffer string.
  real(R8P), allocatable::             XYZa(:)       !< X, Y, Z coordinates.
  integer(I1P), allocatable::          XYZp(:)       !< Packed data.
  character(len=:), allocatable::      XYZ64         !< X, Y, Z coordinates encoded in base64.
  integer(I4P)::                       rf            !< Real file index.
  integer(I4P)::                       n1            !< Counter.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  E_IO = -1_I4P
  rf = f
  if (present(cf)) then
    rf = cf ; f = cf
  endif
  select case(vtk(rf)%f)
  case(ascii)
    s_buffer = repeat(' ',vtk(rf)%indent)//'<Piece NumberOfPoints="'//trim(str(n=NN))//'" NumberOfCells="'//trim(str(n=NC))//'">'
    write(unit=vtk(rf)%u,fmt='(A)',iostat=E_IO)trim(s_buffer) ; vtk(rf)%indent = vtk(rf)%indent + 2
    write(unit=vtk(rf)%u,fmt='(A)',iostat=E_IO)repeat(' ',vtk(rf)%indent)//'<Points>' ; vtk(rf)%indent = vtk(rf)%indent + 2
    s_buffer = repeat(' ',vtk(rf)%indent)//'<DataArray type="Float64" NumberOfComponents="3" Name="Points" format="ascii">'
    write(unit=vtk(rf)%u,fmt='(A)',iostat=E_IO)trim(s_buffer)
    do n1=1,NN
      write(unit=vtk(rf)%u,fmt='(A)',iostat=E_IO)repeat(' ',vtk(rf)%indent)//&
                                                 str(n=XYZ(1,n1))//' '//str(n=XYZ(2,n1))//' '//str(n=XYZ(3,n1))
    enddo
    write(unit=vtk(rf)%u,fmt='(A)',iostat=E_IO)repeat(' ',vtk(rf)%indent)//'</DataArray>' ; vtk(rf)%indent = vtk(rf)%indent - 2
    write(unit=vtk(rf)%u,fmt='(A)',iostat=E_IO)repeat(' ',vtk(rf)%indent)//'</Points>'
  case(raw,bin_app)
    s_buffer = repeat(' ',vtk(rf)%indent)//'<Piece NumberOfPoints="'//trim(str(n=NN))//'" NumberOfCells="'//trim(str(n=NC))//'">'
    write(unit=vtk(rf)%u,iostat=E_IO)trim(s_buffer)//end_rec ; vtk(rf)%indent = vtk(rf)%indent + 2
    write(unit=vtk(rf)%u,iostat=E_IO)repeat(' ',vtk(rf)%indent)//'<Points>'//end_rec ; vtk(rf)%indent = vtk(rf)%indent + 2
    s_buffer = repeat(' ',vtk(rf)%indent)//                                                                  &
               '<DataArray type="Float64" NumberOfComponents="3" Name="Points" format="appended" offset="'// &
               trim(str(.true.,vtk(rf)%ioffset))//'"/>'
    write(unit=vtk(rf)%u,iostat=E_IO)trim(s_buffer)//end_rec
    call vtk(rf)%byte_update(N_Byte = 3*NN*BYR8P)
    write(unit=vtk(rf)%ua,iostat=E_IO)vtk(rf)%N_Byte,'R8',3*NN
    write(unit=vtk(rf)%ua,iostat=E_IO)XYZ
    vtk(rf)%indent = vtk(rf)%indent - 2 ; write(unit=vtk(rf)%u,iostat=E_IO)repeat(' ',vtk(rf)%indent)//'</Points>'//end_rec
  case(binary)
    s_buffer = repeat(' ',vtk(rf)%indent)//'<Piece NumberOfPoints="'//trim(str(n=NN))//'" NumberOfCells="'//trim(str(n=NC))//'">'
    write(unit=vtk(rf)%u,iostat=E_IO)trim(s_buffer)//end_rec ; vtk(rf)%indent = vtk(rf)%indent + 2
    write(unit=vtk(rf)%u,iostat=E_IO)repeat(' ',vtk(rf)%indent)//'<Points>'//end_rec ; vtk(rf)%indent = vtk(rf)%indent + 2
    s_buffer = repeat(' ',vtk(rf)%indent)//'<DataArray type="Float64" NumberOfComponents="3" Name="Points" format="binary">'
    write(unit=vtk(rf)%u,iostat=E_IO)trim(s_buffer)//end_rec
    allocate(XYZa(1:3*NN))
    do n1 = 1,NN
      XYZa(1+(n1-1)*3:1+(n1-1)*3+2)=XYZ(1:3,n1)
    enddo
    call pack_data(a1=[int(3*NN*BYR8P,I4P)],a2=XYZa,packed=XYZp) ; deallocate(XYZa)
    call b64_encode(n=XYZp,code=XYZ64) ; deallocate(XYZp)
    write(unit=vtk(rf)%u,iostat=E_IO)repeat(' ',vtk(rf)%indent+2)//XYZ64//end_rec ; deallocate(XYZ64)
    write(unit=vtk(rf)%u,iostat=E_IO)repeat(' ',vtk(rf)%indent)//'</DataArray>'//end_rec
    vtk(rf)%indent = vtk(rf)%indent - 2 ; write(unit=vtk(rf)%u,iostat=E_IO)repeat(' ',vtk(rf)%indent)//'</Points>'//end_rec
  endselect
  return
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction VTK_GEO_XML_UNST_PACK_R8

  function VTK_GEO_XML_UNST_R4(NN,NC,X,Y,Z,cf) result(E_IO)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Function for saving mesh with \b UnstructuredGrid topology (R4P).
  !---------------------------------------------------------------------------------------------------------------------------------
  implicit none
  integer(I4P), intent(IN)::           NN       !< Number of nodes.
  integer(I4P), intent(IN)::           NC       !< Number of cells.
  real(R4P),    intent(IN)::           X(1:NN)  !< X coordinates.
  real(R4P),    intent(IN)::           Y(1:NN)  !< Y coordinates.
  real(R4P),    intent(IN)::           Z(1:NN)  !< Z coordinates.
  integer(I4P), intent(IN), optional:: cf       !< Current file index (for concurrent files IO).
  integer(I4P)::                       E_IO     !< Input/Output inquiring flag: $0$ if IO is done, $> 0$ if IO is not done.
  character(len=maxlen)::              s_buffer !< Buffer string.
  real(R4P), allocatable::             XYZa(:)  !< X, Y, Z coordinates.
  integer(I1P), allocatable::          XYZp(:)  !< Packed data.
  character(len=:), allocatable::      XYZ64    !< X, Y, Z coordinates encoded in base64.
  integer(I4P)::                       rf       !< Real file index.
  integer(I4P)::                       n1       !< Counter.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  E_IO = -1_I4P
  rf = f
  if (present(cf)) then
    rf = cf ; f = cf
  endif
  select case(vtk(rf)%f)
  case(ascii)
    s_buffer = repeat(' ',vtk(rf)%indent)//'<Piece NumberOfPoints="'//trim(str(n=NN))//'" NumberOfCells="'//trim(str(n=NC))//'">'
    write(unit=vtk(rf)%u,fmt='(A)',iostat=E_IO)trim(s_buffer) ; vtk(rf)%indent = vtk(rf)%indent + 2
    write(unit=vtk(rf)%u,fmt='(A)',iostat=E_IO)repeat(' ',vtk(rf)%indent)//'<Points>' ; vtk(rf)%indent = vtk(rf)%indent + 2
    s_buffer = repeat(' ',vtk(rf)%indent)//'<DataArray type="Float32" NumberOfComponents="3" Name="Points" format="ascii">'
    write(unit=vtk(rf)%u,fmt='(A)',iostat=E_IO)trim(s_buffer)
    do n1=1,NN
      write(unit=vtk(rf)%u,fmt='(A)',iostat=E_IO)repeat(' ',vtk(rf)%indent)//str(n=X(n1))//' '//str(n=Y(n1))//' '//str(n=Z(n1))
    enddo
    write(unit=vtk(rf)%u,fmt='(A)',iostat=E_IO)repeat(' ',vtk(rf)%indent)//'</DataArray>' ; vtk(rf)%indent = vtk(rf)%indent - 2
    write(unit=vtk(rf)%u,fmt='(A)',iostat=E_IO)repeat(' ',vtk(rf)%indent)//'</Points>'
  case(raw,bin_app)
    s_buffer = repeat(' ',vtk(rf)%indent)//'<Piece NumberOfPoints="'//trim(str(n=NN))//'" NumberOfCells="'//trim(str(n=NC))//'">'
    write(unit=vtk(rf)%u,iostat=E_IO)trim(s_buffer)//end_rec ; vtk(rf)%indent = vtk(rf)%indent + 2
    write(unit=vtk(rf)%u,iostat=E_IO)repeat(' ',vtk(rf)%indent)//'<Points>'//end_rec ; vtk(rf)%indent = vtk(rf)%indent + 2
    s_buffer = repeat(' ',vtk(rf)%indent)//                                                                  &
               '<DataArray type="Float32" NumberOfComponents="3" Name="Points" format="appended" offset="'// &
               trim(str(.true.,vtk(rf)%ioffset))//'"/>'
    write(unit=vtk(rf)%u,iostat=E_IO)trim(s_buffer)//end_rec
    call vtk(rf)%byte_update(N_Byte = 3*NN*BYR4P)
    write(unit=vtk(rf)%ua,iostat=E_IO)vtk(rf)%N_Byte,'R4',3*NN
    write(unit=vtk(rf)%ua,iostat=E_IO)(X(n1),Y(n1),Z(n1),n1=1,NN)
    vtk(rf)%indent = vtk(rf)%indent - 2 ; write(unit=vtk(rf)%u,iostat=E_IO)repeat(' ',vtk(rf)%indent)//'</Points>'//end_rec
  case(binary)
    s_buffer = repeat(' ',vtk(rf)%indent)//'<Piece NumberOfPoints="'//trim(str(n=NN))//'" NumberOfCells="'//trim(str(n=NC))//'">'
    write(unit=vtk(rf)%u,iostat=E_IO)trim(s_buffer)//end_rec ; vtk(rf)%indent = vtk(rf)%indent + 2
    write(unit=vtk(rf)%u,iostat=E_IO)repeat(' ',vtk(rf)%indent)//'<Points>'//end_rec ; vtk(rf)%indent = vtk(rf)%indent + 2
    s_buffer = repeat(' ',vtk(rf)%indent)//'<DataArray type="Float32" NumberOfComponents="3" Name="Points" format="binary">'
    write(unit=vtk(rf)%u,iostat=E_IO)trim(s_buffer)//end_rec
    allocate(XYZa(1:3*NN))
    do n1 = 1,NN
      XYZa(1+(n1-1)*3:1+(n1-1)*3+2)=[X(n1),Y(n1),Z(n1)]
    enddo
    call pack_data(a1=[int(3*NN*BYR4P,I4P)],a2=XYZa,packed=XYZp) ; deallocate(XYZa)
    call b64_encode(n=XYZp,code=XYZ64) ; deallocate(XYZp)
    write(unit=vtk(rf)%u,iostat=E_IO)repeat(' ',vtk(rf)%indent+2)//XYZ64//end_rec ; deallocate(XYZ64)
    write(unit=vtk(rf)%u,iostat=E_IO)repeat(' ',vtk(rf)%indent)//'</DataArray>'//end_rec
    vtk(rf)%indent = vtk(rf)%indent - 2 ; write(unit=vtk(rf)%u,iostat=E_IO)repeat(' ',vtk(rf)%indent)//'</Points>'//end_rec
  endselect
  return
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction VTK_GEO_XML_UNST_R4

  function VTK_GEO_XML_UNST_PACK_R4(NN,NC,XYZ,cf) result(E_IO)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Function for saving mesh with \b UnstructuredGrid topology (R4P, packed API).
  !---------------------------------------------------------------------------------------------------------------------------------
  implicit none
  integer(I4P), intent(IN)::           NN            !< Number of nodes.
  integer(I4P), intent(IN)::           NC            !< Number of cells.
  real(R4P),    intent(IN)::           XYZ(1:3,1:NN) !< X, Y, Z coordinates (packed API).
  integer(I4P), intent(IN), optional:: cf            !< Current file index (for concurrent files IO).
  integer(I4P)::                       E_IO          !< Input/Output inquiring flag: $0$ if IO is done, $> 0$ if IO is not done.
  character(len=maxlen)::              s_buffer      !< Buffer string.
  real(R4P), allocatable::             XYZa(:)       !< X, Y, Z coordinates.
  integer(I1P), allocatable::          XYZp(:)       !< Packed data.
  character(len=:), allocatable::      XYZ64         !< X, Y, Z coordinates encoded in base64.
  integer(I4P)::                       rf            !< Real file index.
  integer(I4P)::                       n1            !< Counter.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  E_IO = -1_I4P
  rf = f
  if (present(cf)) then
    rf = cf ; f = cf
  endif
  select case(vtk(rf)%f)
  case(ascii)
    s_buffer = repeat(' ',vtk(rf)%indent)//'<Piece NumberOfPoints="'//trim(str(n=NN))//'" NumberOfCells="'//trim(str(n=NC))//'">'
    write(unit=vtk(rf)%u,fmt='(A)',iostat=E_IO)trim(s_buffer) ; vtk(rf)%indent = vtk(rf)%indent + 2
    write(unit=vtk(rf)%u,fmt='(A)',iostat=E_IO)repeat(' ',vtk(rf)%indent)//'<Points>' ; vtk(rf)%indent = vtk(rf)%indent + 2
    s_buffer = repeat(' ',vtk(rf)%indent)//'<DataArray type="Float32" NumberOfComponents="3" Name="Points" format="ascii">'
    write(unit=vtk(rf)%u,fmt='(A)',iostat=E_IO)trim(s_buffer)
    do n1=1,NN
      write(unit=vtk(rf)%u,fmt='(A)',iostat=E_IO)repeat(' ',vtk(rf)%indent)//&
                                                 str(n=XYZ(1,n1))//' '//str(n=XYZ(2,n1))//' '//str(n=XYZ(3,n1))
    enddo
    write(unit=vtk(rf)%u,fmt='(A)',iostat=E_IO)repeat(' ',vtk(rf)%indent)//'</DataArray>' ; vtk(rf)%indent = vtk(rf)%indent - 2
    write(unit=vtk(rf)%u,fmt='(A)',iostat=E_IO)repeat(' ',vtk(rf)%indent)//'</Points>'
  case(raw,bin_app)
    s_buffer = repeat(' ',vtk(rf)%indent)//'<Piece NumberOfPoints="'//trim(str(n=NN))//'" NumberOfCells="'//trim(str(n=NC))//'">'
    write(unit=vtk(rf)%u,iostat=E_IO)trim(s_buffer)//end_rec ; vtk(rf)%indent = vtk(rf)%indent + 2
    write(unit=vtk(rf)%u,iostat=E_IO)repeat(' ',vtk(rf)%indent)//'<Points>'//end_rec ; vtk(rf)%indent = vtk(rf)%indent + 2
    s_buffer = repeat(' ',vtk(rf)%indent)//                                                                  &
               '<DataArray type="Float32" NumberOfComponents="3" Name="Points" format="appended" offset="'// &
               trim(str(.true.,vtk(rf)%ioffset))//'"/>'
    write(unit=vtk(rf)%u,iostat=E_IO)trim(s_buffer)//end_rec
    call vtk(rf)%byte_update(N_Byte = 3*NN*BYR4P)
    write(unit=vtk(rf)%ua,iostat=E_IO)vtk(rf)%N_Byte,'R4',3*NN
    write(unit=vtk(rf)%ua,iostat=E_IO)XYZ
    vtk(rf)%indent = vtk(rf)%indent - 2 ; write(unit=vtk(rf)%u,iostat=E_IO)repeat(' ',vtk(rf)%indent)//'</Points>'//end_rec
  case(binary)
    s_buffer = repeat(' ',vtk(rf)%indent)//'<Piece NumberOfPoints="'//trim(str(n=NN))//'" NumberOfCells="'//trim(str(n=NC))//'">'
    write(unit=vtk(rf)%u,iostat=E_IO)trim(s_buffer)//end_rec ; vtk(rf)%indent = vtk(rf)%indent + 2
    write(unit=vtk(rf)%u,iostat=E_IO)repeat(' ',vtk(rf)%indent)//'<Points>'//end_rec ; vtk(rf)%indent = vtk(rf)%indent + 2
    s_buffer = repeat(' ',vtk(rf)%indent)//'<DataArray type="Float32" NumberOfComponents="3" Name="Points" format="binary">'
    write(unit=vtk(rf)%u,iostat=E_IO)trim(s_buffer)//end_rec
    allocate(XYZa(1:3*NN))
    do n1 = 1,NN
      XYZa(1+(n1-1)*3:1+(n1-1)*3+2)=XYZ(1:3,n1)
    enddo
    call pack_data(a1=[int(3*NN*BYR4P,I4P)],a2=XYZa,packed=XYZp) ; deallocate(XYZa)
    call b64_encode(n=XYZp,code=XYZ64) ; deallocate(XYZp)
    write(unit=vtk(rf)%u,iostat=E_IO)repeat(' ',vtk(rf)%indent+2)//XYZ64//end_rec ; deallocate(XYZ64)
    write(unit=vtk(rf)%u,iostat=E_IO)repeat(' ',vtk(rf)%indent)//'</DataArray>'//end_rec
    vtk(rf)%indent = vtk(rf)%indent - 2 ; write(unit=vtk(rf)%u,iostat=E_IO)repeat(' ',vtk(rf)%indent)//'</Points>'//end_rec
  endselect
  return
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction VTK_GEO_XML_UNST_PACK_R4

  function VTK_GEO_XML_CLOSEP(cf) result(E_IO)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Function for closing mesh block data.
  !---------------------------------------------------------------------------------------------------------------------------------
  implicit none
  integer(I4P), intent(IN), optional:: cf   !< Current file index (for concurrent files IO).
  integer(I4P)::                       E_IO !< Input/Output inquiring flag: $0$ if IO is done, $> 0$ if IO is not done.
  integer(I4P)::                       rf   !< Real file index.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  E_IO = -1_I4P
  rf = f
  if (present(cf)) then
    rf = cf ; f = cf
  endif
  vtk(rf)%indent = vtk(rf)%indent - 2
  select case(vtk(rf)%f)
  case(ascii)
    write(unit=vtk(rf)%u,fmt='(A)',iostat=E_IO)repeat(' ',vtk(rf)%indent)//'</Piece>'
  case(raw,binary,bin_app)
    write(unit=vtk(rf)%u,iostat=E_IO)repeat(' ',vtk(rf)%indent)//'</Piece>'//end_rec
  endselect
  return
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction VTK_GEO_XML_CLOSEP
endmodule Lib_VTK_IO_GEO_XML
