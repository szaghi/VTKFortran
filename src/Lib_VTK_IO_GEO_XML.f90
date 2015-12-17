!< GEO_XML interface definition for Lib_VTK_IO.
module Lib_VTK_IO_GEO_XML
!-----------------------------------------------------------------------------------------------------------------------------------
!< GEO_XML interface definition for Lib_VTK_IO.
!-----------------------------------------------------------------------------------------------------------------------------------
USE IR_Precision        ! Integers and reals precision definition.
USE Lib_Base64          ! Base64 encoding/decoding procedures.
USE Lib_VTK_IO_Back_End ! Lib_VTK_IO back end module.
!-----------------------------------------------------------------------------------------------------------------------------------

!-----------------------------------------------------------------------------------------------------------------------------------
implicit none
private
save
public:: VTK_GEO_XML_WRITE
public:: VTK_GEO_XML_READ
!-----------------------------------------------------------------------------------------------------------------------------------

!-----------------------------------------------------------------------------------------------------------------------------------
interface VTK_GEO_XML_WRITE
  !< Export (write) mesh with different topologies in VTK-XML standard.
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
  module procedure VTK_GEO_XML_STRG_1DA_R8_WRITE , VTK_GEO_XML_STRG_3DA_R8_WRITE , &
                   VTK_GEO_XML_STRG_1DAP_R8_WRITE, VTK_GEO_XML_STRG_3DAP_R8_WRITE, &
                   VTK_GEO_XML_STRG_1DA_R4_WRITE , VTK_GEO_XML_STRG_3DA_R4_WRITE , &
                   VTK_GEO_XML_STRG_1DAP_R4_WRITE, VTK_GEO_XML_STRG_3DAP_R4_WRITE, &
                   VTK_GEO_XML_RECT_R8_WRITE     ,                                 &
                   VTK_GEO_XML_RECT_R4_WRITE     ,                                 &
                   VTK_GEO_XML_UNST_R8_WRITE     , VTK_GEO_XML_UNST_PACK_R4_WRITE, &
                   VTK_GEO_XML_UNST_R4_WRITE     , VTK_GEO_XML_UNST_PACK_R8_WRITE, &
                   VTK_GEO_XML_CLOSEP_WRITE
endinterface
interface VTK_GEO_XML_READ
  !< Import (read) mesh with different topologies in VTK-XML standard.
  !<
  !< VTK_GEO_XML_READ is an interface to 14 different functions; there are 2 functions for each of 3 topologies supported and a
  !< function for closing XML pieces: one function for mesh coordinates with R8P (Ok!) precision and one for mesh coordinates with
  !< R4P (Not tested!) precision. 1D/3D-rank arrays and packed API for ascii and raw data, binary is not implemented yet!
  !<
  !<- For StructuredGrid there are 4 functions for each real kinds:
  !<    - inputs are 1D-rank arrays: X[1:NN],Y[1:NN],Z[1:NN]; (Not tested!)
  !<    - inputs are 3D-rank arrays: X[nx1:nx2,ny1:ny2,nz1:nz2],Y[nx1:nx2,ny1:ny2,nz1:nz2],Z[nx1:nx2,ny1:ny2,nz1:nz2]; (Not tested!)
  !<    - input is 1D-rank array (packed API): XYZ[1:3,1:NN]; (Not tested!)
  !<    - input is 3D-rank array (packed API): XYZ[1:3,nx1:nx2,ny1:ny2,nz1:nz2]. (Not tested!)
  !<- For UnStructuredGrid there are 2 functions for each real kinds:
  !<    - inputs are 1D arrays: X[1:NN],Y[1:NN],Z[1:NN]; (Ok!)
  !<    - input is 1D array (packed API): XYZ[1:3,1:NN]. (Not tested!)
  !<
  !< VTK_GEO_XML_READ must be called after VTK_INI_XML_READ. It reads the mesh geometry. The inputs that must be passed
  !< change depending on the topologies chosen. Not all VTK topologies have been implemented (*polydata* topologies are absent).
  !<
  !< @note The XML standard is more powerful than legacy. XML file can contain more than 1 mesh with its
  !< associated variables. Thus there is the necessity to close each *pieces* that compose the data-set saved in the
  !< XML file. The VTK_GEO_XML_READ uses the *close piece* format is used just to close the
  !< current piece before saving another piece or closing the file.
  !<
  !<### Examples of usage
  module procedure VTK_GEO_XML_STRG_1DA_R8_READ , VTK_GEO_XML_STRG_3DA_R8_READ , &
                   VTK_GEO_XML_STRG_1DAP_R8_READ, VTK_GEO_XML_STRG_3DAP_R8_READ, &
                   VTK_GEO_XML_STRG_1DA_R4_READ , VTK_GEO_XML_STRG_3DA_R4_READ , &
                   VTK_GEO_XML_STRG_1DAP_R4_READ, VTK_GEO_XML_STRG_3DAP_R4_READ, &
                   VTK_GEO_XML_RECT_R8_READ     ,                                &
                   VTK_GEO_XML_RECT_R4_READ     ,                                &
                   VTK_GEO_XML_UNST_R8_READ     , VTK_GEO_XML_UNST_PACK_R4_READ, &
                   VTK_GEO_XML_UNST_R4_READ     , VTK_GEO_XML_UNST_PACK_R8_READ
endinterface
!-----------------------------------------------------------------------------------------------------------------------------------
contains
  ! exporters
  function VTK_GEO_XML_STRG_1DA_R8_WRITE(nx1, nx2, ny1, ny2, nz1, nz2, NN, X, Y, Z, cf) result(E_IO)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Function for saving mesh with \b StructuredGrid topology (R8P, 1D Arrays).
  !---------------------------------------------------------------------------------------------------------------------------------
  integer(I4P), intent(IN)           :: nx1      !< Initial node of x axis.
  integer(I4P), intent(IN)           :: nx2      !< Final node of x axis.
  integer(I4P), intent(IN)           :: ny1      !< Initial node of y axis.
  integer(I4P), intent(IN)           :: ny2      !< Final node of y axis.
  integer(I4P), intent(IN)           :: nz1      !< Initial node of z axis.
  integer(I4P), intent(IN)           :: nz2      !< Final node of z axis.
  integer(I4P), intent(IN)           :: NN       !< Number of all nodes.
  real(R8P),    intent(IN)           :: X(1:)    !< X coordinates [1:NN].
  real(R8P),    intent(IN)           :: Y(1:)    !< Y coordinates [1:NN].
  real(R8P),    intent(IN)           :: Z(1:)    !< Z coordinates [1:NN].
  integer(I4P), intent(IN), optional :: cf       !< Current file index (for concurrent files IO).
  integer(I4P)                       :: E_IO     !< Input/Output inquiring flag: $0$ if IO is done, $> 0$ if IO is not done.
  integer(I1P),     allocatable      :: XYZp(:)  !< Packed coordinates data.
  character(len=:), allocatable      :: XYZ64    !< X, Y, Z coordinates encoded in base64.
  character(len=maxlen)              :: s_buffer !< Buffer string.
  integer(I4P)                       :: rf       !< Real file index.
  integer(I4P)                       :: n1       !< Counter.
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
  endfunction VTK_GEO_XML_STRG_1DA_R8_WRITE

  function VTK_GEO_XML_STRG_3DA_R8_WRITE(nx1, nx2, ny1, ny2, nz1, nz2, NN, X, Y, Z, cf) result(E_IO)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Function for saving mesh with \b StructuredGrid topology (R8P, 3D Arrays).
  !---------------------------------------------------------------------------------------------------------------------------------
  integer(I4P), intent(IN)           :: nx1               !< Initial node of x axis.
  integer(I4P), intent(IN)           :: nx2               !< Final node of x axis.
  integer(I4P), intent(IN)           :: ny1               !< Initial node of y axis.
  integer(I4P), intent(IN)           :: ny2               !< Final node of y axis.
  integer(I4P), intent(IN)           :: nz1               !< Initial node of z axis.
  integer(I4P), intent(IN)           :: nz2               !< Final node of z axis.
  integer(I4P), intent(IN)           :: NN                !< Number of all nodes.
  real(R8P),    intent(IN)           :: X(nx1:,ny1:,nz1:) !< X coordinates [nx1:nx2,ny1:ny2,nz1:nz2].
  real(R8P),    intent(IN)           :: Y(nx1:,ny1:,nz1:) !< Y coordinates [nx1:nx2,ny1:ny2,nz1:nz2].
  real(R8P),    intent(IN)           :: Z(nx1:,ny1:,nz1:) !< Z coordinates [nx1:nx2,ny1:ny2,nz1:nz2].
  integer(I4P), intent(IN), optional :: cf                !< Current file index (for concurrent files IO).
  integer(I4P)                       :: E_IO              !< Error trapping flag.
  integer(I1P),     allocatable      :: XYZp(:)           !< Packed coordinates data.
  character(len=:), allocatable      :: XYZ64             !< X, Y, Z coordinates encoded in base64.
  character(len=maxlen)              :: s_buffer          !< Buffer string.
  integer(I4P)                       :: rf                !< Real file index.
  integer(I4P)                       :: nx,ny,nz          !< Counters.
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
  endfunction VTK_GEO_XML_STRG_3DA_R8_WRITE

  function VTK_GEO_XML_STRG_1DAP_R8_WRITE(nx1, nx2, ny1, ny2, nz1, nz2, NN, XYZ, cf) result(E_IO)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Function for saving mesh with \b StructuredGrid topology (R8P, 1D Arrays, packed API).
  !---------------------------------------------------------------------------------------------------------------------------------
  integer(I4P), intent(IN)           :: nx1        !< Initial node of x axis.
  integer(I4P), intent(IN)           :: nx2        !< Final node of x axis.
  integer(I4P), intent(IN)           :: ny1        !< Initial node of y axis.
  integer(I4P), intent(IN)           :: ny2        !< Final node of y axis.
  integer(I4P), intent(IN)           :: nz1        !< Initial node of z axis.
  integer(I4P), intent(IN)           :: nz2        !< Final node of z axis.
  integer(I4P), intent(IN)           :: NN         !< Number of all nodes.
  real(R8P),    intent(IN)           :: XYZ(1:,1:) !< X, Y, Z coordinates (packed API) [1:3,1:NN].
  integer(I4P), intent(IN), optional :: cf         !< Current file index (for concurrent files IO).
  integer(I4P)                       :: E_IO       !< Input/Output inquiring flag: $0$ if IO is done, $> 0$ if IO is not done.
  integer(I1P),     allocatable      :: XYZp(:)    !< Packed coordinates data.
  character(len=:), allocatable      :: XYZ64      !< X, Y, Z coordinates encoded in base64.
  character(len=maxlen)              :: s_buffer   !< Buffer string.
  integer(I4P)                       :: rf         !< Real file index.
  integer(I4P)                       :: n1         !< Counter.
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
  endfunction VTK_GEO_XML_STRG_1DAP_R8_WRITE

  function VTK_GEO_XML_STRG_3DAP_R8_WRITE(nx1, nx2, ny1, ny2, nz1, nz2, NN, XYZ, cf) result(E_IO)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Function for saving mesh with \b StructuredGrid topology (R8P, 3D Arrays, packed API).
  !---------------------------------------------------------------------------------------------------------------------------------
  integer(I4P), intent(IN)           :: nx1                    !< Initial node of x axis.
  integer(I4P), intent(IN)           :: nx2                    !< Final node of x axis.
  integer(I4P), intent(IN)           :: ny1                    !< Initial node of y axis.
  integer(I4P), intent(IN)           :: ny2                    !< Final node of y axis.
  integer(I4P), intent(IN)           :: nz1                    !< Initial node of z axis.
  integer(I4P), intent(IN)           :: nz2                    !< Final node of z axis.
  integer(I4P), intent(IN)           :: NN                     !< Number of all nodes.
  real(R8P),    intent(IN)           :: XYZ(1:,nx1:,ny1:,nz1:) !< X, Y, Z coordinates (packed API).
  integer(I4P), intent(IN), optional :: cf                     !< Current file index (for concurrent files IO).
  integer(I4P)                       :: E_IO                   !< Error trapping flag.
  integer(I1P),     allocatable      :: XYZp(:)                !< Packed coordinates data.
  character(len=:), allocatable      :: XYZ64                  !< X, Y, Z coordinates encoded in base64.
  character(len=maxlen)              :: s_buffer               !< Buffer string.
  integer(I4P)                       :: rf                     !< Real file index.
  integer(I4P)                       :: nx,ny,nz               !< Counters.
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
  endfunction VTK_GEO_XML_STRG_3DAP_R8_WRITE

  function VTK_GEO_XML_STRG_1DA_R4_WRITE(nx1, nx2, ny1, ny2, nz1, nz2, NN, X, Y, Z, cf) result(E_IO)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Function for saving mesh with \b StructuredGrid topology (R4P, 1D Arrays).
  !---------------------------------------------------------------------------------------------------------------------------------
  integer(I4P), intent(IN)           :: nx1      !< Initial node of x axis.
  integer(I4P), intent(IN)           :: nx2      !< Final node of x axis.
  integer(I4P), intent(IN)           :: ny1      !< Initial node of y axis.
  integer(I4P), intent(IN)           :: ny2      !< Final node of y axis.
  integer(I4P), intent(IN)           :: nz1      !< Initial node of z axis.
  integer(I4P), intent(IN)           :: nz2      !< Final node of z axis.
  integer(I4P), intent(IN)           :: NN       !< Number of all nodes.
  real(R4P),    intent(IN)           :: X(1:)    !< X coordinates [1:NN].
  real(R4P),    intent(IN)           :: Y(1:)    !< Y coordinates [1:NN].
  real(R4P),    intent(IN)           :: Z(1:)    !< Z coordinates [1:NN].
  integer(I4P), intent(IN), optional :: cf       !< Current file index (for concurrent files IO).
  integer(I4P)                       :: E_IO     !< Input/Output inquiring flag: $0$ if IO is done, $> 0$ if IO is not done.
  character(len=maxlen)              :: s_buffer !< Buffer string.
  integer(I1P),     allocatable      :: XYZp(:)  !< Packed data.
  character(len=:), allocatable      :: XYZ64    !< X, Y, Z coordinates encoded in base64.
  integer(I4P)                       :: rf       !< Real file index.
  integer(I4P)                       :: n1       !< Counter.
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
  endfunction VTK_GEO_XML_STRG_1DA_R4_WRITE

  function VTK_GEO_XML_STRG_3DA_R4_WRITE(nx1, nx2, ny1, ny2, nz1, nz2, NN, X, Y, Z, cf) result(E_IO)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Function for saving mesh with \b StructuredGrid topology (R4P, 3D Arrays).
  !---------------------------------------------------------------------------------------------------------------------------------
  integer(I4P), intent(IN)           :: nx1               !< Initial node of x axis.
  integer(I4P), intent(IN)           :: nx2               !< Final node of x axis.
  integer(I4P), intent(IN)           :: ny1               !< Initial node of y axis.
  integer(I4P), intent(IN)           :: ny2               !< Final node of y axis.
  integer(I4P), intent(IN)           :: nz1               !< Initial node of z axis.
  integer(I4P), intent(IN)           :: nz2               !< Final node of z axis.
  integer(I4P), intent(IN)           :: NN                !< Number of all nodes.
  real(R4P),    intent(IN)           :: X(nx1:,ny1:,nz1:) !< X coordinates [nx1:nx2,ny1:ny2,nz1:nz2].
  real(R4P),    intent(IN)           :: Y(nx1:,ny1:,nz1:) !< Y coordinates [nx1:nx2,ny1:ny2,nz1:nz2].
  real(R4P),    intent(IN)           :: Z(nx1:,ny1:,nz1:) !< Z coordinates [nx1:nx2,ny1:ny2,nz1:nz2].
  integer(I4P), intent(IN), optional :: cf                !< Current file index (for concurrent files IO).
  integer(I4P)                       :: E_IO              !< Error trapping flag.
  character(len=maxlen)              :: s_buffer          !< Buffer string.
  integer(I1P),     allocatable      :: XYZp(:)           !< Packed data.
  character(len=:), allocatable      :: XYZ64             !< X, Y, Z coordinates encoded in base64.
  integer(I4P)                       :: rf                !< Real file index.
  integer(I4P)                       :: nx,ny,nz          !< Counters.
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
  endfunction VTK_GEO_XML_STRG_3DA_R4_WRITE

  function VTK_GEO_XML_STRG_1DAP_R4_WRITE(nx1, nx2, ny1, ny2, nz1, nz2, NN, XYZ, cf) result(E_IO)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Function for saving mesh with \b StructuredGrid topology (R4P, 1D Arrays, packed API).
  !---------------------------------------------------------------------------------------------------------------------------------
  integer(I4P), intent(IN)           :: nx1        !< Initial node of x axis.
  integer(I4P), intent(IN)           :: nx2        !< Final node of x axis.
  integer(I4P), intent(IN)           :: ny1        !< Initial node of y axis.
  integer(I4P), intent(IN)           :: ny2        !< Final node of y axis.
  integer(I4P), intent(IN)           :: nz1        !< Initial node of z axis.
  integer(I4P), intent(IN)           :: nz2        !< Final node of z axis.
  integer(I4P), intent(IN)           :: NN         !< Number of all nodes.
  real(R4P),    intent(IN)           :: XYZ(1:,1:) !< X, Y, Z coordinates (packed API) [1:3,1:NN].
  integer(I4P), intent(IN), optional :: cf         !< Current file index (for concurrent files IO).
  integer(I4P)                       :: E_IO       !< Input/Output inquiring flag: $0$ if IO is done, $> 0$ if IO is not done.
  character(len=maxlen)              :: s_buffer   !< Buffer string.
  integer(I1P),     allocatable      :: XYZp(:)    !< Packed data.
  character(len=:), allocatable      :: XYZ64      !< X, Y, Z coordinates encoded in base64.
  integer(I4P)                       :: rf         !< Real file index.
  integer(I4P)                       :: n1         !< Counter.
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
  endfunction VTK_GEO_XML_STRG_1DAP_R4_WRITE

  function VTK_GEO_XML_STRG_3DAP_R4_WRITE(nx1, nx2, ny1, ny2, nz1, nz2, NN, XYZ, cf) result(E_IO)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Function for saving mesh with \b StructuredGrid topology (R4P, 3D Arrays, packed API).
  !---------------------------------------------------------------------------------------------------------------------------------
  integer(I4P), intent(IN)           :: nx1                    !< Initial node of x axis.
  integer(I4P), intent(IN)           :: nx2                    !< Final node of x axis.
  integer(I4P), intent(IN)           :: ny1                    !< Initial node of y axis.
  integer(I4P), intent(IN)           :: ny2                    !< Final node of y axis.
  integer(I4P), intent(IN)           :: nz1                    !< Initial node of z axis.
  integer(I4P), intent(IN)           :: nz2                    !< Final node of z axis.
  integer(I4P), intent(IN)           :: NN                     !< Number of all nodes.
  real(R4P),    intent(IN)           :: XYZ(1:,nx1:,ny1:,nz1:) !< X, Y, Z coordinates (packed API) [1:3,nx1:nx2,ny1:ny2,nz1:nz2].
  integer(I4P), intent(IN), optional :: cf                     !< Current file index (for concurrent files IO).
  integer(I4P)                       :: E_IO                   !< Error trapping flag.
  character(len=maxlen)              :: s_buffer               !< Buffer string.
  integer(I1P),     allocatable      :: XYZp(:)                !< Packed data.
  character(len=:), allocatable      :: XYZ64                  !< X, Y, Z coordinates encoded in base64.
  integer(I4P)                       :: rf                     !< Real file index.
  integer(I4P)                       :: nx,ny,nz               !< Counters.
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
  endfunction VTK_GEO_XML_STRG_3DAP_R4_WRITE

  function VTK_GEO_XML_RECT_R8_WRITE(nx1, nx2, ny1, ny2, nz1, nz2, X, Y, Z, cf) result(E_IO)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Function for saving mesh with \b RectilinearGrid topology (R8P).
  !---------------------------------------------------------------------------------------------------------------------------------
  integer(I4P), intent(IN)           :: nx1        !< Initial node of x axis.
  integer(I4P), intent(IN)           :: nx2        !< Final node of x axis.
  integer(I4P), intent(IN)           :: ny1        !< Initial node of y axis.
  integer(I4P), intent(IN)           :: ny2        !< Final node of y axis.
  integer(I4P), intent(IN)           :: nz1        !< Initial node of z axis.
  integer(I4P), intent(IN)           :: nz2        !< Final node of z axis.
  real(R8P),    intent(IN)           :: X(nx1:nx2) !< X coordinates.
  real(R8P),    intent(IN)           :: Y(ny1:ny2) !< Y coordinates.
  real(R8P),    intent(IN)           :: Z(nz1:nz2) !< Z coordinates.
  integer(I4P), intent(IN), optional :: cf         !< Current file index (for concurrent files IO).
  integer(I4P)                       :: E_IO       !< Input/Output inquiring flag: $0$ if IO is done, $> 0$ if IO is not done.
  character(len=maxlen)              :: s_buffer   !< Buffer string.
  integer(I1P),     allocatable      :: XYZp(:)    !< Packed data.
  character(len=:), allocatable      :: XYZ64      !< X, Y, Z coordinates encoded in base64.
  integer(I4P)                       :: rf         !< Real file index.
  integer(I4P)                       :: n1         !< Counter.
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
  endfunction VTK_GEO_XML_RECT_R8_WRITE

  function VTK_GEO_XML_RECT_R4_WRITE(nx1, nx2, ny1, ny2, nz1, nz2, X, Y, Z, cf) result(E_IO)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Function for saving mesh with \b RectilinearGrid topology (R4P).
  !---------------------------------------------------------------------------------------------------------------------------------
  integer(I4P), intent(IN)           :: nx1        !< Initial node of x axis.
  integer(I4P), intent(IN)           :: nx2        !< Final node of x axis.
  integer(I4P), intent(IN)           :: ny1        !< Initial node of y axis.
  integer(I4P), intent(IN)           :: ny2        !< Final node of y axis.
  integer(I4P), intent(IN)           :: nz1        !< Initial node of z axis.
  integer(I4P), intent(IN)           :: nz2        !< Final node of z axis.
  real(R4P),    intent(IN)           :: X(nx1:nx2) !< X coordinates.
  real(R4P),    intent(IN)           :: Y(ny1:ny2) !< Y coordinates.
  real(R4P),    intent(IN)           :: Z(nz1:nz2) !< Z coordinates.
  integer(I4P), intent(IN), optional :: cf         !< Current file index (for concurrent files IO).
  integer(I4P)                       :: E_IO       !< Input/Output inquiring flag: $0$ if IO is done, $> 0$ if IO is not done.
  character(len=maxlen)              :: s_buffer   !< Buffer string.
  integer(I1P),     allocatable      :: XYZp(:)    !< Packed data.
  character(len=:), allocatable      :: XYZ64      !< X, Y, Z coordinates encoded in base64.
  integer(I4P)                       :: rf         !< Real file index.
  integer(I4P)                       :: n1         !< Counter.
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
  endfunction VTK_GEO_XML_RECT_R4_WRITE

  function VTK_GEO_XML_UNST_R8_WRITE(NN, NC, X, Y, Z, cf) result(E_IO)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Function for saving mesh with \b UnstructuredGrid topology (R8P).
  !---------------------------------------------------------------------------------------------------------------------------------
  integer(I4P), intent(IN)           :: NN       !< Number of nodes.
  integer(I4P), intent(IN)           :: NC       !< Number of cells.
  real(R8P),    intent(IN)           :: X(1:NN)  !< X coordinates.
  real(R8P),    intent(IN)           :: Y(1:NN)  !< Y coordinates.
  real(R8P),    intent(IN)           :: Z(1:NN)  !< Z coordinates.
  integer(I4P), intent(IN), optional :: cf       !< Current file index (for concurrent files IO).
  integer(I4P)                       :: E_IO     !< Input/Output inquiring flag: $0$ if IO is done, $> 0$ if IO is not done.
  character(len=maxlen)              :: s_buffer !< Buffer string.
  real(R8P),        allocatable      :: XYZa(:)  !< X, Y, Z coordinates.
  integer(I1P),     allocatable      :: XYZp(:)  !< Packed data.
  character(len=:), allocatable      :: XYZ64    !< X, Y, Z coordinates encoded in base64.
  integer(I4P)                       :: rf       !< Real file index.
  integer(I4P)                       :: n1       !< Counter.
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
  endfunction VTK_GEO_XML_UNST_R8_WRITE

  function VTK_GEO_XML_UNST_PACK_R8_WRITE(NN, NC, XYZ, cf) result(E_IO)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Function for saving mesh with \b UnstructuredGrid topology (R8P, packed API).
  !---------------------------------------------------------------------------------------------------------------------------------
  integer(I4P), intent(IN)           :: NN            !< Number of nodes.
  integer(I4P), intent(IN)           :: NC            !< Number of cells.
  real(R8P),    intent(IN)           :: XYZ(1:3,1:NN) !< X, Y, Z coordinates (packed API).
  integer(I4P), intent(IN), optional :: cf            !< Current file index (for concurrent files IO).
  integer(I4P)                       :: E_IO          !< Input/Output inquiring flag: $0$ if IO is done, $> 0$ if IO is not done.
  character(len=maxlen)              :: s_buffer      !< Buffer string.
  real(R8P),        allocatable      :: XYZa(:)       !< X, Y, Z coordinates.
  integer(I1P),     allocatable      :: XYZp(:)       !< Packed data.
  character(len=:), allocatable      :: XYZ64         !< X, Y, Z coordinates encoded in base64.
  integer(I4P)                       :: rf            !< Real file index.
  integer(I4P)                       :: n1            !< Counter.
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
  endfunction VTK_GEO_XML_UNST_PACK_R8_WRITE

  function VTK_GEO_XML_UNST_R4_WRITE(NN, NC, X, Y, Z, cf) result(E_IO)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Function for saving mesh with \b UnstructuredGrid topology (R4P).
  !---------------------------------------------------------------------------------------------------------------------------------
  integer(I4P), intent(IN)           :: NN       !< Number of nodes.
  integer(I4P), intent(IN)           :: NC       !< Number of cells.
  real(R4P),    intent(IN)           :: X(1:NN)  !< X coordinates.
  real(R4P),    intent(IN)           :: Y(1:NN)  !< Y coordinates.
  real(R4P),    intent(IN)           :: Z(1:NN)  !< Z coordinates.
  integer(I4P), intent(IN), optional :: cf       !< Current file index (for concurrent files IO).
  integer(I4P)                       :: E_IO     !< Input/Output inquiring flag: $0$ if IO is done, $> 0$ if IO is not done.
  character(len=maxlen)              :: s_buffer !< Buffer string.
  real(R4P),        allocatable      :: XYZa(:)  !< X, Y, Z coordinates.
  integer(I1P),     allocatable      :: XYZp(:)  !< Packed data.
  character(len=:), allocatable      :: XYZ64    !< X, Y, Z coordinates encoded in base64.
  integer(I4P)                       :: rf       !< Real file index.
  integer(I4P)                       :: n1       !< Counter.
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
  endfunction VTK_GEO_XML_UNST_R4_WRITE

  function VTK_GEO_XML_UNST_PACK_R4_WRITE(NN, NC, XYZ, cf) result(E_IO)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Function for saving mesh with \b UnstructuredGrid topology (R4P, packed API).
  !---------------------------------------------------------------------------------------------------------------------------------
  integer(I4P), intent(IN)           :: NN            !< Number of nodes.
  integer(I4P), intent(IN)           :: NC            !< Number of cells.
  real(R4P),    intent(IN)           :: XYZ(1:3,1:NN) !< X, Y, Z coordinates (packed API).
  integer(I4P), intent(IN), optional :: cf            !< Current file index (for concurrent files IO).
  integer(I4P)                       :: E_IO          !< Input/Output inquiring flag: $0$ if IO is done, $> 0$ if IO is not done.
  character(len=maxlen)              :: s_buffer      !< Buffer string.
  real(R4P),        allocatable      :: XYZa(:)       !< X, Y, Z coordinates.
  integer(I1P),     allocatable      :: XYZp(:)       !< Packed data.
  character(len=:), allocatable      :: XYZ64         !< X, Y, Z coordinates encoded in base64.
  integer(I4P)                       :: rf            !< Real file index.
  integer(I4P)                       :: n1            !< Counter.
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
  endfunction VTK_GEO_XML_UNST_PACK_R4_WRITE

  function VTK_GEO_XML_CLOSEP_WRITE(cf) result(E_IO)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Function for closing mesh block data.
  !---------------------------------------------------------------------------------------------------------------------------------
  integer(I4P), intent(IN), optional :: cf   !< Current file index (for concurrent files IO).
  integer(I4P)                       :: E_IO !< Input/Output inquiring flag: $0$ if IO is done, $> 0$ if IO is not done.
  integer(I4P)                       :: rf   !< Real file index.
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
  endfunction VTK_GEO_XML_CLOSEP_WRITE

  ! importers
  function VTK_GEO_XML_STRG_1DA_R8_READ(nx1, nx2, ny1, ny2, nz1, nz2, NN, X, Y, Z, npiece, cf) result(E_IO)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Function for reading mesh with \b UnstructuredGrid topology (R8P).
  !---------------------------------------------------------------------------------------------------------------------------------
  integer(I4P),           intent(OUT) :: nx1      !< Initial node of x axis.
  integer(I4P),           intent(OUT) :: nx2      !< Final node of x axis.
  integer(I4P),           intent(OUT) :: ny1      !< Initial node of y axis.
  integer(I4P),           intent(OUT) :: ny2      !< Final node of y axis.
  integer(I4P),           intent(OUT) :: nz1      !< Initial node of z axis.
  integer(I4P),           intent(OUT) :: nz2      !< Final node of z axis.
  integer(I4P),           intent(OUT) :: NN       !< Number of nodes
  real(R8P), allocatable, intent(OUT) :: X(:)     !< x coordinates
  real(R8P), allocatable, intent(OUT) :: Y(:)     !< y coordinates
  real(R8P), allocatable, intent(OUT) :: Z(:)     !< z coordinates
  integer(I4P), optional, intent(IN)  :: npiece   !< Number of the piece to read (by default: 1)
  integer(I4P), optional, intent(IN)  :: cf       !< Current file index (for concurrent files IO).
  integer(I4P)                        :: E_IO     !< Input/Output inquiring flag: $0$ if IO is done, $> 0$ if IO is not done
  integer(I4P)                        :: rf       !< Real file index.
  character(len=:), allocatable       :: s_buffer !< Buffer string.
  character(len=:), allocatable       :: aux      !< Auxiliary string.
  character(len=:), allocatable       :: fmt
  character(len=:), allocatable       :: type
  character(len=:), allocatable       :: data
  integer(I4P)                        :: np, i, offs, N_Byte, pos, s
  integer(I1P),     allocatable       :: dI1P(:)
  real(R8P),        allocatable       :: XYZp(:)
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  rf = f
  if (present(cf)) then
    rf = cf ; f = cf
  endif
  np = 1_I4P; if (present(npiece)) np = npiece
  E_IO = -1_I4P
  select case(vtk(rf)%f)

    case(ascii)

      rewind(unit=vtk(rf)%u, iostat=E_IO)
      E_IO = move(inside='StructuredGrid', to_find='Piece', repeat=np, cf=rf, buffer=s_buffer) ! find the 'np' piece
      inquire(unit=vtk(rf)%u, pos=pos, iostat=E_IO) !annotate the current position in the file
      call get_char(buffer=s_buffer, attrib='Extent', val=aux, E_IO=E_IO)
      if(E_IO == 0) then
        read(aux,*, iostat=E_IO) nx1,nx2,ny1,ny2,nz1,nz2
        if(E_IO == 0) then
          NN = (nx2-nx1+1)*(ny2-ny1+1)*(nz2-nz1+1)
          E_IO = search(from=pos, inside='Points', to_find='DataArray', with_attribute='Name', of_value='Points', &
                        buffer=s_buffer,content=data)
          call get_char(buffer=s_buffer, attrib='format', val=fmt,  E_IO=E_IO)
          call get_char(buffer=s_buffer, attrib='type', val=type,  E_IO=E_IO)
          if (trim(adjustlt(Upper_Case(fmt)))/='ASCII' .or. &
              trim(adjustlt(Upper_Case(type)))/='FLOAT64') then
            E_IO = -1_I4P
          else
            allocate(X(NN), Y(NN), Z(NN), stat=E_IO)
            read(data, fmt=*, iostat=E_IO) (X(i), Y(i), Z(i), i=1,NN) !get ascii array
          endif
          if(allocated(data)) deallocate(data)
        endif
      endif

    case(binary)

      rewind(unit=vtk(rf)%u, iostat=E_IO)
      E_IO = move(inside='StructuredGrid', to_find='Piece', repeat=np, cf=rf, buffer=s_buffer) ! find the 'np' piece
      inquire(unit=vtk(rf)%u, pos=pos, iostat=E_IO) !annotate the current position in the file
      call get_char(buffer=s_buffer, attrib='Extent', val=aux, E_IO=E_IO)
      if(E_IO == 0) then
        read(aux,*, iostat=E_IO) nx1,nx2,ny1,ny2,nz1,nz2
        if(E_IO == 0) then
          NN = (nx2-nx1+1)*(ny2-ny1+1)*(nz2-nz1+1)
          E_IO = search(from=pos, inside='Points', to_find='DataArray', with_attribute='Name', of_value='Points', &
                        buffer=s_buffer,content=data)
          call get_char(buffer=s_buffer, attrib='format', val=fmt,  E_IO=E_IO)
          call get_char(buffer=s_buffer, attrib='type', val=type,  E_IO=E_IO)
          if (trim(adjustlt(Upper_Case(fmt)))/='BINARY' .or. &
              trim(adjustlt(Upper_Case(type)))/='FLOAT64') then
            E_IO = -1_I4P
          else
            ! Decode base64 packed data
            data=trim(adjustlt(data))
            allocate(dI1P(3*NN*int(BYR8P,I4P)+int(BYI4P,I4P)))
            call b64_decode(code=data,n=dI1P); if(allocated(data)) deallocate(data)
            ! Unpack data [1xI4P,3*NNxR8P]
            N_byte =  transfer(dI1P(1:int(BYI4P,I4P)),N_byte)
            s = size(transfer(dI1P(int(BYI4P,I4P)+1:),XYZp)); allocate(XYZp(1:s))
            XYZp = transfer(dI1P(int(BYI4P,I4P)+1:),XYZp); if(allocated(dI1P)) deallocate(dI1P)
            allocate(X(NN), Y(NN), Z(NN), stat=E_IO)
            do i=1,NN; X(i)=XYZp(i*3-2); Y(i)=XYZp(i*3-1); Z(i)=XYZp(i*3); enddo
            if(allocated(XYZp)) deallocate(XYZp)
          endif
        endif
      endif

    case(raw)

      rewind(unit=vtk(rf)%u, iostat=E_IO)
      E_IO = move(inside='StructuredGrid', to_find='Piece', repeat=np, cf=rf, buffer=s_buffer) ! find the 'np' piece
      inquire(unit=vtk(rf)%u, pos=pos, iostat=E_IO) !annotate the current position in the file
      call get_char(buffer=s_buffer, attrib='Extent', val=aux, E_IO=E_IO)
      if(E_IO == 0) then
        read(aux,*, iostat=E_IO) nx1,nx2,ny1,ny2,nz1,nz2
        if(E_IO == 0) then
          NN = (nx2-nx1+1)*(ny2-ny1+1)*(nz2-nz1+1)
          E_IO = search(from=pos, inside='Points', to_find='DataArray', with_attribute='Name', of_value='Points', &
                        buffer=s_buffer)
          call get_int(buffer=s_buffer,  attrib='offset', val=offs, E_IO=E_IO)
          call get_char(buffer=s_buffer, attrib='format', val=fmt,  E_IO=E_IO)
          call get_char(buffer=s_buffer, attrib='type', val=type,  E_IO=E_IO)
          if (trim(adjustlt(Upper_Case(fmt)))/='APPENDED' .or. &
              trim(adjustlt(Upper_Case(type)))/='FLOAT64') then
            E_IO = -1_I4P
          else
            allocate(X(NN), Y(NN), Z(NN), stat=E_IO)
            read(unit=vtk(rf)%u, iostat=E_IO, pos = vtk(rf)%ioffset+offs) N_Byte, (X(i), Y(i), Z(i), i=1,NN) !get appended array
          endif
        endif
      endif
    end select
  !---------------------------------------------------------------------------------------------------------------------------------
  end function VTK_GEO_XML_STRG_1DA_R8_READ

  function VTK_GEO_XML_STRG_3DA_R8_READ(nx1,nx2,ny1,ny2,nz1,nz2,NN,X,Y,Z,npiece,cf) result(E_IO)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Function for reading mesh with \b UnstructuredGrid topology (R8P).
  !---------------------------------------------------------------------------------------------------------------------------------
  integer(I4P),           intent(OUT) :: nx1      !< Initial node of x axis.
  integer(I4P),           intent(OUT) :: nx2      !< Final node of x axis.
  integer(I4P),           intent(OUT) :: ny1      !< Initial node of y axis.
  integer(I4P),           intent(OUT) :: ny2      !< Final node of y axis.
  integer(I4P),           intent(OUT) :: nz1      !< Initial node of z axis.
  integer(I4P),           intent(OUT) :: nz2      !< Final node of z axis.
  integer(I4P),           intent(OUT) :: NN       !< Number of nodes
  real(R8P), allocatable, intent(OUT) :: X(:,:,:)     !< x coordinates
  real(R8P), allocatable, intent(OUT) :: Y(:,:,:)     !< y coordinates
  real(R8P), allocatable, intent(OUT) :: Z(:,:,:)     !< z coordinates
  integer(I4P), optional, intent(IN)  :: npiece   !< Number of the piece to read (by default: 1)
  integer(I4P), optional, intent(IN)  :: cf       !< Current file index (for concurrent files IO).
  integer(I4P)                        :: E_IO     !< Input/Output inquiring flag: $0$ if IO is done, $> 0$ if IO is not done
  integer(I4P)                        :: rf       !< Real file index.
  character(len=:),allocatable        :: s_buffer !< Buffer string.
  character(len=:),allocatable        :: aux      !< Auxiliary string.
  character(len=:), allocatable       :: fmt
  character(len=:), allocatable       :: type
  character(len=:), allocatable       :: data
  integer(I4P)                        :: np, i, j, k, offs, N_Byte, pos, s
  integer(I1P), allocatable           :: dI1P(:)
  real(R8P), allocatable              :: XYZp(:)
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  rf = f
  if (present(cf)) then
    rf = cf ; f = cf
  endif
  np = 1_I4P; if (present(npiece)) np = npiece
  E_IO = -1_I4P
  select case(vtk(rf)%f)

    case(ascii)

      rewind(unit=vtk(rf)%u, iostat=E_IO)
      E_IO = move(inside='StructuredGrid', to_find='Piece', repeat=np, cf=rf, buffer=s_buffer) ! find the 'np' piece
      inquire(unit=vtk(rf)%u, pos=pos, iostat=E_IO) !annotate the current position in the file
      call get_char(buffer=s_buffer, attrib='Extent', val=aux, E_IO=E_IO)
      if(E_IO == 0) then
        read(aux,*, iostat=E_IO) nx1,nx2,ny1,ny2,nz1,nz2
        if(E_IO == 0) then
          NN = (nx2-nx1+1)*(ny2-ny1+1)*(nz2-nz1+1)
          E_IO = search(from=pos, inside='Points', to_find='DataArray', with_attribute='Name', of_value='Points', &
                        buffer=s_buffer,content=data)
          call get_char(buffer=s_buffer, attrib='format', val=fmt,  E_IO=E_IO)
          call get_char(buffer=s_buffer, attrib='type', val=type,  E_IO=E_IO)
          if (trim(adjustlt(Upper_Case(fmt)))/='ASCII' .or. &
              trim(adjustlt(Upper_Case(type)))/='FLOAT64') then
            E_IO = -1_I4P
          else
            allocate(X(nx1:nx2,ny1:ny2,nz1:nz2), Y(nx1:nx2,ny1:ny2,nz1:nz2), Z(nx1:nx2,ny1:ny2,nz1:nz2), stat=E_IO)
            read(data, fmt=*, iostat=E_IO) (((X(i,j,k),Y(i,j,k),Z(i,j,k),i=nx1,nx2),j=ny1,ny2),k=nz1,nz2) !get ascii array
          endif
          if(allocated(data)) deallocate(data)
        endif
      endif

    case(binary)

      rewind(unit=vtk(rf)%u, iostat=E_IO)
      E_IO = move(inside='StructuredGrid', to_find='Piece', repeat=np, cf=rf, buffer=s_buffer) ! find the 'np' piece
      inquire(unit=vtk(rf)%u, pos=pos, iostat=E_IO) !annotate the current position in the file
      call get_char(buffer=s_buffer, attrib='Extent', val=aux, E_IO=E_IO)
      if(E_IO == 0) then
        read(aux,*, iostat=E_IO) nx1,nx2,ny1,ny2,nz1,nz2
        if(E_IO == 0) then
          NN = (nx2-nx1+1)*(ny2-ny1+1)*(nz2-nz1+1)
          E_IO = search(from=pos, inside='Points', to_find='DataArray', with_attribute='Name', of_value='Points', &
                        buffer=s_buffer,content=data)
          call get_char(buffer=s_buffer, attrib='format', val=fmt,  E_IO=E_IO)
          call get_char(buffer=s_buffer, attrib='type', val=type,  E_IO=E_IO)
          if (trim(adjustlt(Upper_Case(fmt)))/='BINARY' .or. &
              trim(adjustlt(Upper_Case(type)))/='FLOAT64') then
            E_IO = -1_I4P
          else
            ! Decode base64 packed data
            data=trim(adjustlt(data))
            allocate(dI1P(3*NN*int(BYR8P,I4P)+int(BYI4P,I4P)))
            call b64_decode(code=data,n=dI1P); if(allocated(data)) deallocate(data)
            ! Unpack data [1xI4P,3*NNxR8P]
            N_byte =  transfer(dI1P(1:int(BYI4P,I4P)),N_byte)
            s = size(transfer(dI1P(int(BYI4P,I4P)+1:),XYZp)); allocate(XYZp(1:s))
            XYZp = transfer(dI1P(int(BYI4P,I4P)+1:),XYZp); if(allocated(dI1P)) deallocate(dI1P)
            allocate(X(nx1:nx2,ny1:ny2,nz1:nz2), Y(nx1:nx2,ny1:ny2,nz1:nz2), Z(nx1:nx2,ny1:ny2,nz1:nz2), stat=E_IO)
            s=1;do k=nz1,nz2-nz1;do j=ny1,ny2;do i=nx1,nx2;
              X(i,j,k)=XYZp(s); s=s+1; Y(i,j,k)=XYZp(s); s=s+1; Z(i,j,k)=XYZp(s); s=s+1
            enddo;enddo;enddo;
            if(allocated(XYZp)) deallocate(XYZp)
          endif
        endif
      endif

    case(raw)

      rewind(unit=vtk(rf)%u, iostat=E_IO)
      E_IO = move(inside='StructuredGrid', to_find='Piece', repeat=np, cf=rf, buffer=s_buffer) ! find the 'np' piece
      inquire(unit=vtk(rf)%u, pos=pos, iostat=E_IO) !annotate the current position in the file
      call get_char(buffer=s_buffer, attrib='Extent', val=aux, E_IO=E_IO)
      if(E_IO == 0) then
        read(aux,*, iostat=E_IO) nx1,nx2,ny1,ny2,nz1,nz2
        if(E_IO == 0) then
          NN = (nx2-nx1+1)*(ny2-ny1+1)*(nz2-nz1+1)
          E_IO = search(from=pos, inside='Points', to_find='DataArray', with_attribute='Name', of_value='Points', &
                        buffer=s_buffer)
          call get_int(buffer=s_buffer,  attrib='offset', val=offs, E_IO=E_IO)
          call get_char(buffer=s_buffer, attrib='format', val=fmt,  E_IO=E_IO)
          call get_char(buffer=s_buffer, attrib='type', val=type,  E_IO=E_IO)
          if (trim(adjustlt(Upper_Case(fmt)))/='APPENDED' .or. &
              trim(adjustlt(Upper_Case(type)))/='FLOAT64') then
            E_IO = -1_I4P
          else
            allocate(X(nx1:nx2,ny1:ny2,nz1:nz2), Y(nx1:nx2,ny1:ny2,nz1:nz2), Z(nx1:nx2,ny1:ny2,nz1:nz2), stat=E_IO)
            read(unit=vtk(rf)%u, iostat=E_IO, pos = vtk(rf)%ioffset+offs) N_Byte, &
                (((X(i,j,k),Y(i,j,k),Z(i,j,k),i=nx1,nx2),j=ny1,ny2),k=nz1,nz2) !get appended array
          endif
        endif
      endif
    end select
  !---------------------------------------------------------------------------------------------------------------------------------
  end function VTK_GEO_XML_STRG_3DA_R8_READ

  function VTK_GEO_XML_STRG_1DAP_R8_READ(nx1,nx2,ny1,ny2,nz1,nz2,NN,XYZ,npiece,cf) result(E_IO)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Function for reading mesh with \b UnstructuredGrid topology (R8P).
  !---------------------------------------------------------------------------------------------------------------------------------
  integer(I4P),           intent(OUT) :: nx1      !< Initial node of x axis.
  integer(I4P),           intent(OUT) :: nx2      !< Final node of x axis.
  integer(I4P),           intent(OUT) :: ny1      !< Initial node of y axis.
  integer(I4P),           intent(OUT) :: ny2      !< Final node of y axis.
  integer(I4P),           intent(OUT) :: nz1      !< Initial node of z axis.
  integer(I4P),           intent(OUT) :: nz2      !< Final node of z axis.
  integer(I4P),           intent(OUT) :: NN       !< Number of nodes
  real(R8P), allocatable, intent(OUT) :: XYZ(:,:)     !< x coordinates
  integer(I4P), optional, intent(IN)  :: npiece   !< Number of the piece to read (by default: 1)
  integer(I4P), optional, intent(IN)  :: cf       !< Current file index (for concurrent files IO).
  integer(I4P)                        :: E_IO     !< Input/Output inquiring flag: $0$ if IO is done, $> 0$ if IO is not done
  integer(I4P)                        :: rf       !< Real file index.
  character(len=:),allocatable        :: s_buffer !< Buffer string.
  character(len=:),allocatable        :: aux      !< Auxiliary string.
  character(len=:), allocatable       :: fmt
  character(len=:), allocatable       :: type
  character(len=:), allocatable       :: data
  integer(I4P)                        :: np, i, j, offs, N_Byte, pos, s
  integer(I1P), allocatable           :: dI1P(:)
  real(R8P), allocatable              :: XYZp(:)
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  rf = f
  if (present(cf)) then
    rf = cf ; f = cf
  endif
  np = 1_I4P; if (present(npiece)) np = npiece
  E_IO = -1_I4P
  select case(vtk(rf)%f)

    case(ascii)

      rewind(unit=vtk(rf)%u, iostat=E_IO)
      E_IO = move(inside='StructuredGrid', to_find='Piece', repeat=np, cf=rf, buffer=s_buffer) ! find the 'np' piece
      inquire(unit=vtk(rf)%u, pos=pos, iostat=E_IO) !annotate the current position in the file
      call get_char(buffer=s_buffer, attrib='Extent', val=aux, E_IO=E_IO)
      if(E_IO == 0) then
        read(aux,*, iostat=E_IO) nx1,nx2,ny1,ny2,nz1,nz2
        if(E_IO == 0) then
          NN = (nx2-nx1+1)*(ny2-ny1+1)*(nz2-nz1+1)
          E_IO = search(from=pos, inside='Points', to_find='DataArray', with_attribute='Name', of_value='Points', &
                        buffer=s_buffer,content=data)
          call get_char(buffer=s_buffer, attrib='format', val=fmt,  E_IO=E_IO)
          call get_char(buffer=s_buffer, attrib='type', val=type,  E_IO=E_IO)
          if (trim(adjustlt(Upper_Case(fmt)))/='ASCII' .or. &
              trim(adjustlt(Upper_Case(type)))/='FLOAT64') then
            E_IO = -1_I4P
          else
            allocate(XYZ(3,NN), stat=E_IO)
            read(data, fmt=*, iostat=E_IO) ((XYZ(i,j),i=1,3),j=1,NN) !get ascii array
          endif
          if(allocated(data)) deallocate(data)
        endif
      endif

    case(binary)

      rewind(unit=vtk(rf)%u, iostat=E_IO)
      E_IO = move(inside='StructuredGrid', to_find='Piece', repeat=np, cf=rf, buffer=s_buffer) ! find the 'np' piece
      inquire(unit=vtk(rf)%u, pos=pos, iostat=E_IO) !annotate the current position in the file
      call get_char(buffer=s_buffer, attrib='Extent', val=aux, E_IO=E_IO)
      if(E_IO == 0) then
        read(aux,*, iostat=E_IO) nx1,nx2,ny1,ny2,nz1,nz2
        if(E_IO == 0) then
          NN = (nx2-nx1+1)*(ny2-ny1+1)*(nz2-nz1+1)
          E_IO = search(from=pos, inside='Points', to_find='DataArray', with_attribute='Name', of_value='Points', &
                        buffer=s_buffer,content=data)
          call get_char(buffer=s_buffer, attrib='format', val=fmt,  E_IO=E_IO)
          call get_char(buffer=s_buffer, attrib='type', val=type,  E_IO=E_IO)
          if (trim(adjustlt(Upper_Case(fmt)))/='BINARY' .or. &
              trim(adjustlt(Upper_Case(type)))/='FLOAT64') then
            E_IO = -1_I4P
          else
            ! Decode base64 packed data
            data=trim(adjustlt(data))
            allocate(dI1P(3*NN*int(BYR8P,I4P)+int(BYI4P,I4P)))
            call b64_decode(code=data,n=dI1P); if(allocated(data)) deallocate(data)
            ! Unpack data [1xI4P,3*NNxR8P]
            N_byte =  transfer(dI1P(1:int(BYI4P,I4P)),N_byte)
            s = size(transfer(dI1P(int(BYI4P,I4P)+1:),XYZp)); allocate(XYZp(1:s))
            XYZp = transfer(dI1P(int(BYI4P,I4P)+1:),XYZp); if(allocated(dI1P)) deallocate(dI1P)
            allocate(XYZ(3,NN), stat=E_IO)
            XYZ = reshape(XYZp,(/3,NN/))
            if(allocated(XYZp)) deallocate(XYZp)
          endif
        endif
      endif

    case(raw)

      rewind(unit=vtk(rf)%u, iostat=E_IO)
      E_IO = move(inside='StructuredGrid', to_find='Piece', repeat=np, cf=rf, buffer=s_buffer) ! find the 'np' piece
      inquire(unit=vtk(rf)%u, pos=pos, iostat=E_IO) !annotate the current position in the file
      call get_char(buffer=s_buffer, attrib='Extent', val=aux, E_IO=E_IO)
      if(E_IO == 0) then
        read(aux,*, iostat=E_IO) nx1,nx2,ny1,ny2,nz1,nz2
        if(E_IO == 0) then
          NN = (nx2-nx1+1)*(ny2-ny1+1)*(nz2-nz1+1)
          E_IO = search(from=pos, inside='Points', to_find='DataArray', with_attribute='Name', of_value='Points', &
                        buffer=s_buffer)
          call get_int(buffer=s_buffer,  attrib='offset', val=offs, E_IO=E_IO)
          call get_char(buffer=s_buffer, attrib='format', val=fmt,  E_IO=E_IO)
          call get_char(buffer=s_buffer, attrib='type', val=type,  E_IO=E_IO)
          if (trim(adjustlt(Upper_Case(fmt)))/='APPENDED' .or. &
              trim(adjustlt(Upper_Case(type)))/='FLOAT64') then
            E_IO = -1_I4P
          else
            allocate(XYZ(3,NN), stat=E_IO)
            read(unit=vtk(rf)%u, iostat=E_IO, pos = vtk(rf)%ioffset+offs) N_Byte, ((XYZ(i,j),i=1,3),j=1,NN) !get appended array
          endif
        endif
      endif
    end select
  !---------------------------------------------------------------------------------------------------------------------------------
  end function VTK_GEO_XML_STRG_1DAP_R8_READ

  function VTK_GEO_XML_STRG_3DAP_R8_READ(nx1,nx2,ny1,ny2,nz1,nz2,NN,XYZ,npiece,cf) result(E_IO)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Function for reading mesh with \b UnstructuredGrid topology (R8P).
  !---------------------------------------------------------------------------------------------------------------------------------
  integer(I4P),           intent(OUT) :: nx1      !< Initial node of x axis.
  integer(I4P),           intent(OUT) :: nx2      !< Final node of x axis.
  integer(I4P),           intent(OUT) :: ny1      !< Initial node of y axis.
  integer(I4P),           intent(OUT) :: ny2      !< Final node of y axis.
  integer(I4P),           intent(OUT) :: nz1      !< Initial node of z axis.
  integer(I4P),           intent(OUT) :: nz2      !< Final node of z axis.
  integer(I4P),           intent(OUT) :: NN       !< Number of nodes
  real(R8P), allocatable, intent(OUT) :: XYZ(:,:,:,:)     !< x coordinates
  integer(I4P), optional, intent(IN)  :: npiece   !< Number of the piece to read (by default: 1)
  integer(I4P), optional, intent(IN)  :: cf       !< Current file index (for concurrent files IO).
  integer(I4P)                        :: E_IO     !< Input/Output inquiring flag: $0$ if IO is done, $> 0$ if IO is not done
  integer(I4P)                        :: rf       !< Real file index.
  character(len=:),allocatable        :: s_buffer !< Buffer string.
  character(len=:),allocatable        :: aux      !< Auxiliary string.
  character(len=:), allocatable       :: fmt
  character(len=:), allocatable       :: type
  character(len=:), allocatable       :: data
  integer(I4P)                        :: np, i, j, k, l, offs, N_Byte, pos, s
  integer(I1P), allocatable           :: dI1P(:)
  real(R8P), allocatable              :: XYZp(:)
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  rf = f
  if (present(cf)) then
    rf = cf ; f = cf
  endif
  np = 1_I4P; if (present(npiece)) np = npiece
  E_IO = -1_I4P
  select case(vtk(rf)%f)

    case(ascii)

      rewind(unit=vtk(rf)%u, iostat=E_IO)
      E_IO = move(inside='StructuredGrid', to_find='Piece', repeat=np, cf=rf, buffer=s_buffer) ! find the 'np' piece
      inquire(unit=vtk(rf)%u, pos=pos, iostat=E_IO) !annotate the current position in the file
      call get_char(buffer=s_buffer, attrib='Extent', val=aux, E_IO=E_IO)
      if(E_IO == 0) then
        read(aux,*, iostat=E_IO) nx1,nx2,ny1,ny2,nz1,nz2
        if(E_IO == 0) then
          NN = (nx2-nx1+1)*(ny2-ny1+1)*(nz2-nz1+1)
          E_IO = search(from=pos, inside='Points', to_find='DataArray', with_attribute='Name', of_value='Points', &
                        buffer=s_buffer,content=data)
          call get_char(buffer=s_buffer, attrib='format', val=fmt,  E_IO=E_IO)
          call get_char(buffer=s_buffer, attrib='type', val=type,  E_IO=E_IO)
          if (trim(adjustlt(Upper_Case(fmt)))/='ASCII' .or. &
              trim(adjustlt(Upper_Case(type)))/='FLOAT64') then
            E_IO = -1_I4P
          else
            allocate(XYZ(3,nx1:nx2,ny1:ny2,nz1:nz2), stat=E_IO)
            read(data, fmt=*, iostat=E_IO) ((((XYZ(i,j,k,l),i=1,3),j=nx1,nx2),k=ny1,ny2),l=nz1,nz2) !get ascii array
          endif
          if(allocated(data)) deallocate(data)
        endif
      endif

    case(binary)

      rewind(unit=vtk(rf)%u, iostat=E_IO)
      E_IO = move(inside='StructuredGrid', to_find='Piece', repeat=np, cf=rf, buffer=s_buffer) ! find the 'np' piece
      inquire(unit=vtk(rf)%u, pos=pos, iostat=E_IO) !annotate the current position in the file
      call get_char(buffer=s_buffer, attrib='Extent', val=aux, E_IO=E_IO)
      if(E_IO == 0) then
        read(aux,*, iostat=E_IO) nx1,nx2,ny1,ny2,nz1,nz2
        if(E_IO == 0) then
          NN = (nx2-nx1+1)*(ny2-ny1+1)*(nz2-nz1+1)
          E_IO = search(from=pos, inside='Points', to_find='DataArray', with_attribute='Name', of_value='Points', &
                        buffer=s_buffer,content=data)
          call get_char(buffer=s_buffer, attrib='format', val=fmt,  E_IO=E_IO)
          call get_char(buffer=s_buffer, attrib='type', val=type,  E_IO=E_IO)
          if (trim(adjustlt(Upper_Case(fmt)))/='BINARY' .or. &
              trim(adjustlt(Upper_Case(type)))/='FLOAT64') then
            E_IO = -1_I4P
          else
            ! Decode base64 packed data
            data=trim(adjustlt(data))
            allocate(dI1P(3*NN*int(BYR8P,I4P)+int(BYI4P,I4P)))
            call b64_decode(code=data,n=dI1P); if(allocated(data)) deallocate(data)
            ! Unpack data [1xI4P,3*NNxR8P]
            N_byte =  transfer(dI1P(1:int(BYI4P,I4P)),N_byte)
            s = size(transfer(dI1P(int(BYI4P,I4P)+1:),XYZp)); allocate(XYZp(1:s))
            XYZp = transfer(dI1P(int(BYI4P,I4P)+1:),XYZp); if(allocated(dI1P)) deallocate(dI1P)
            allocate(XYZ(3,nx1:nx2,ny1:ny2,nz1:nz2), stat=E_IO)
            XYZ(1:3,nx1:nx2,ny1:ny2,nz1:nz2) = reshape(XYZp, (/3,nx2-nx1+1,ny2-ny1+1,nz2-nz1+1/))
            if(allocated(XYZp)) deallocate(XYZp)
          endif
        endif
      endif

    case(raw)

      rewind(unit=vtk(rf)%u, iostat=E_IO)
      E_IO = move(inside='StructuredGrid', to_find='Piece', repeat=np, cf=rf, buffer=s_buffer) ! find the 'np' piece
      inquire(unit=vtk(rf)%u, pos=pos, iostat=E_IO) !annotate the current position in the file
      call get_char(buffer=s_buffer, attrib='Extent', val=aux, E_IO=E_IO)
      if(E_IO == 0) then
        read(aux,*, iostat=E_IO) nx1,nx2,ny1,ny2,nz1,nz2
        if(E_IO == 0) then
          NN = (nx2-nx1+1)*(ny2-ny1+1)*(nz2-nz1+1)
          E_IO = search(from=pos, inside='Points', to_find='DataArray', with_attribute='Name', of_value='Points', buffer=s_buffer)
          call get_int(buffer=s_buffer,  attrib='offset', val=offs, E_IO=E_IO)
          call get_char(buffer=s_buffer, attrib='format', val=fmt,  E_IO=E_IO)
          call get_char(buffer=s_buffer, attrib='type', val=type,  E_IO=E_IO)
          if (trim(adjustlt(Upper_Case(fmt)))/='APPENDED' .or. &
              trim(adjustlt(Upper_Case(type)))/='FLOAT64') then
            E_IO = -1_I4P
          else
            allocate(XYZ(3,nx1:nx2,ny1:ny2,nz1:nz2), stat=E_IO)
            read(unit=vtk(rf)%u, iostat=E_IO, pos = vtk(rf)%ioffset+offs) N_Byte, &
                ((((XYZ(i,j,k,l),i=1,3),j=nx1,nx2),k=ny1,ny2),l=nz1,nz2) !get appended array
          endif
        endif
      endif
    end select
  !---------------------------------------------------------------------------------------------------------------------------------
  end function VTK_GEO_XML_STRG_3DAP_R8_READ

  function VTK_GEO_XML_STRG_1DA_R4_READ(nx1,nx2,ny1,ny2,nz1,nz2,NN,X,Y,Z,npiece,cf) result(E_IO)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Function for reading mesh with \b UnstructuredGrid topology (R8P).
  !---------------------------------------------------------------------------------------------------------------------------------
  integer(I4P),           intent(OUT) :: nx1      !< Initial node of x axis.
  integer(I4P),           intent(OUT) :: nx2      !< Final node of x axis.
  integer(I4P),           intent(OUT) :: ny1      !< Initial node of y axis.
  integer(I4P),           intent(OUT) :: ny2      !< Final node of y axis.
  integer(I4P),           intent(OUT) :: nz1      !< Initial node of z axis.
  integer(I4P),           intent(OUT) :: nz2      !< Final node of z axis.
  integer(I4P),           intent(OUT) :: NN       !< Number of nodes
  real(R4P), allocatable, intent(OUT) :: X(:)     !< x coordinates
  real(R4P), allocatable, intent(OUT) :: Y(:)     !< y coordinates
  real(R4P), allocatable, intent(OUT) :: Z(:)     !< z coordinates
  integer(I4P), optional, intent(IN)  :: npiece   !< Number of the piece to read (by default: 1)
  integer(I4P), optional, intent(IN)  :: cf       !< Current file index (for concurrent files IO).
  integer(I4P)                        :: E_IO     !< Input/Output inquiring flag: $0$ if IO is done, $> 0$ if IO is not done
  integer(I4P)                        :: rf       !< Real file index.
  character(len=:),allocatable        :: s_buffer !< Buffer string.
  character(len=:),allocatable        :: aux      !< Auxiliary string.
  character(len=:), allocatable       :: fmt
  character(len=:), allocatable       :: type
  character(len=:), allocatable       :: data
  integer(I4P)                        :: np, i, offs, N_Byte, pos, s
  integer(I1P), allocatable           :: dI1P(:)
  real(R4P), allocatable              :: XYZp(:)
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  rf = f
  if (present(cf)) then
    rf = cf ; f = cf
  endif
  np = 1_I4P; if (present(npiece)) np = npiece
  E_IO = -1_I4P
  select case(vtk(rf)%f)

    case(ascii)

      rewind(unit=vtk(rf)%u, iostat=E_IO)
      E_IO = move(inside='StructuredGrid', to_find='Piece', repeat=np, cf=rf, buffer=s_buffer) ! find the 'np' piece
      inquire(unit=vtk(rf)%u, pos=pos, iostat=E_IO) !annotate the current position in the file
      call get_char(buffer=s_buffer, attrib='Extent', val=aux, E_IO=E_IO)
      if(E_IO == 0) then
        read(aux,*, iostat=E_IO) nx1,nx2,ny1,ny2,nz1,nz2
        if(E_IO == 0) then
          NN = (nx2-nx1+1)*(ny2-ny1+1)*(nz2-nz1+1)
          E_IO = search(from=pos, inside='Points', to_find='DataArray', with_attribute='Name', of_value='Points', &
                        buffer=s_buffer,content=data)
          call get_char(buffer=s_buffer, attrib='format', val=fmt,  E_IO=E_IO)
          call get_char(buffer=s_buffer, attrib='type', val=type,  E_IO=E_IO)
          if (trim(adjustlt(Upper_Case(fmt)))/='ASCII' .or. &
              trim(adjustlt(Upper_Case(type)))/='FLOAT32') then
            E_IO = -1_I4P
          else
            allocate(X(NN), Y(NN), Z(NN), stat=E_IO)
            read(data, fmt=*, iostat=E_IO) (X(i), Y(i), Z(i), i=1,NN) !get ascii array
          endif
          if(allocated(data)) deallocate(data)
        endif
      endif

    case(binary)

      rewind(unit=vtk(rf)%u, iostat=E_IO)
      E_IO = move(inside='StructuredGrid', to_find='Piece', repeat=np, cf=rf, buffer=s_buffer) ! find the 'np' piece
      inquire(unit=vtk(rf)%u, pos=pos, iostat=E_IO) !annotate the current position in the file
      call get_char(buffer=s_buffer, attrib='Extent', val=aux, E_IO=E_IO)
      if(E_IO == 0) then
        read(aux,*, iostat=E_IO) nx1,nx2,ny1,ny2,nz1,nz2
        if(E_IO == 0) then
          NN = (nx2-nx1+1)*(ny2-ny1+1)*(nz2-nz1+1)
          E_IO = search(from=pos, inside='Points', to_find='DataArray', with_attribute='Name', of_value='Points', &
                        buffer=s_buffer,content=data)
          call get_char(buffer=s_buffer, attrib='format', val=fmt,  E_IO=E_IO)
          call get_char(buffer=s_buffer, attrib='type', val=type,  E_IO=E_IO)
          if (trim(adjustlt(Upper_Case(fmt)))/='BINARY' .or. &
              trim(adjustlt(Upper_Case(type)))/='FLOAT32') then
            E_IO = -1_I4P
          else
            ! Decode base64 packed data
            data=trim(adjustlt(data))
            allocate(dI1P(3*NN*int(BYR4P,I4P)+int(BYI4P,I4P)))
            call b64_decode(code=data,n=dI1P); if(allocated(data)) deallocate(data)
            ! Unpack data [1xI4P,3*NNxR4P]
            N_byte =  transfer(dI1P(1:int(BYI4P,I4P)),N_byte)
            s = size(transfer(dI1P(int(BYI4P,I4P)+1:),XYZp)); allocate(XYZp(1:s))
            XYZp = transfer(dI1P(int(BYI4P,I4P)+1:),XYZp); if(allocated(dI1P)) deallocate(dI1P)
            allocate(X(NN), Y(NN), Z(NN), stat=E_IO)
            do i=1,NN; X(i)=XYZp(i*3-2); Y(i)=XYZp(i*3-1); Z(i)=XYZp(i*3); enddo
            if(allocated(XYZp)) deallocate(XYZp)
          endif
        endif
      endif

    case(raw)

      rewind(unit=vtk(rf)%u, iostat=E_IO)
      E_IO = move(inside='StructuredGrid', to_find='Piece', repeat=np, cf=rf, buffer=s_buffer) ! find the 'np' piece
      inquire(unit=vtk(rf)%u, pos=pos, iostat=E_IO) !annotate the current position in the file
      call get_char(buffer=s_buffer, attrib='Extent', val=aux, E_IO=E_IO)
      if(E_IO == 0) then
        read(aux,*, iostat=E_IO) nx1,nx2,ny1,ny2,nz1,nz2
        if(E_IO == 0) then
          NN = (nx2-nx1+1)*(ny2-ny1+1)*(nz2-nz1+1)
          E_IO = search(from=pos, inside='Points', to_find='DataArray', with_attribute='Name', of_value='Points', &
                        buffer=s_buffer)
          call get_int(buffer=s_buffer,  attrib='offset', val=offs, E_IO=E_IO)
          call get_char(buffer=s_buffer, attrib='format', val=fmt,  E_IO=E_IO)
          call get_char(buffer=s_buffer, attrib='type', val=type,  E_IO=E_IO)
          if (trim(adjustlt(Upper_Case(fmt)))/='APPENDED' .or. &
              trim(adjustlt(Upper_Case(type)))/='FLOAT32') then
            E_IO = -1_I4P
          else
            allocate(X(NN), Y(NN), Z(NN), stat=E_IO)
            read(unit=vtk(rf)%u, iostat=E_IO, pos = vtk(rf)%ioffset+offs) N_Byte, (X(i), Y(i), Z(i), i=1,NN) !get appended array
          endif
        endif
      endif
    end select
  !---------------------------------------------------------------------------------------------------------------------------------
  end function VTK_GEO_XML_STRG_1DA_R4_READ

  function VTK_GEO_XML_STRG_3DA_R4_READ(nx1,nx2,ny1,ny2,nz1,nz2,NN,X,Y,Z,npiece,cf) result(E_IO)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Function for reading mesh with \b UnstructuredGrid topology (R8P).
  !---------------------------------------------------------------------------------------------------------------------------------
  integer(I4P),           intent(OUT) :: nx1      !< Initial node of x axis.
  integer(I4P),           intent(OUT) :: nx2      !< Final node of x axis.
  integer(I4P),           intent(OUT) :: ny1      !< Initial node of y axis.
  integer(I4P),           intent(OUT) :: ny2      !< Final node of y axis.
  integer(I4P),           intent(OUT) :: nz1      !< Initial node of z axis.
  integer(I4P),           intent(OUT) :: nz2      !< Final node of z axis.
  integer(I4P),           intent(OUT) :: NN       !< Number of nodes
  real(R4P), allocatable, intent(OUT) :: X(:,:,:)     !< x coordinates
  real(R4P), allocatable, intent(OUT) :: Y(:,:,:)     !< y coordinates
  real(R4P), allocatable, intent(OUT) :: Z(:,:,:)     !< z coordinates
  integer(I4P), optional, intent(IN)  :: npiece   !< Number of the piece to read (by default: 1)
  integer(I4P), optional, intent(IN)  :: cf       !< Current file index (for concurrent files IO).
  integer(I4P)                        :: E_IO     !< Input/Output inquiring flag: $0$ if IO is done, $> 0$ if IO is not done
  integer(I4P)                        :: rf       !< Real file index.
  character(len=:),allocatable        :: s_buffer !< Buffer string.
  character(len=:),allocatable        :: aux      !< Auxiliary string.
  character(len=:), allocatable       :: fmt
  character(len=:), allocatable       :: type
  character(len=:), allocatable       :: data
  integer(I4P)                        :: np, i, j, k, offs, N_Byte, pos, s
  integer(I1P), allocatable           :: dI1P(:)
  real(R4P), allocatable              :: XYZp(:)
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  rf = f
  if (present(cf)) then
    rf = cf ; f = cf
  endif
  np = 1_I4P; if (present(npiece)) np = npiece
  E_IO = -1_I4P
  select case(vtk(rf)%f)

    case(ascii)

      rewind(unit=vtk(rf)%u, iostat=E_IO)
      E_IO = move(inside='StructuredGrid', to_find='Piece', repeat=np, cf=rf, buffer=s_buffer) ! find the 'np' piece
      inquire(unit=vtk(rf)%u, pos=pos, iostat=E_IO) !annotate the current position in the file
      call get_char(buffer=s_buffer, attrib='Extent', val=aux, E_IO=E_IO)
      if(E_IO == 0) then
        read(aux,*, iostat=E_IO) nx1,nx2,ny1,ny2,nz1,nz2
        if(E_IO == 0) then
          NN = (nx2-nx1+1)*(ny2-ny1+1)*(nz2-nz1+1)
          E_IO = search(from=pos, inside='Points', to_find='DataArray', with_attribute='Name', of_value='Points', &
                        buffer=s_buffer,content=data)
          call get_char(buffer=s_buffer, attrib='format', val=fmt,  E_IO=E_IO)
          call get_char(buffer=s_buffer, attrib='type', val=type,  E_IO=E_IO)
          if (trim(adjustlt(Upper_Case(fmt)))/='ASCII' .or. &
              trim(adjustlt(Upper_Case(type)))/='FLOAT32') then
            E_IO = -1_I4P
          else
            allocate(X(nx1:nx2,ny1:ny2,nz1:nz2), Y(nx1:nx2,ny1:ny2,nz1:nz2), Z(nx1:nx2,ny1:ny2,nz1:nz2), stat=E_IO)
            read(data, fmt=*, iostat=E_IO) (((X(i,j,k),Y(i,j,k),Z(i,j,k),i=nx1,nx2),j=ny1,ny2),k=nz1,nz2) !get ascii array
          endif
          if(allocated(data)) deallocate(data)
        endif
      endif

    case(binary)

      rewind(unit=vtk(rf)%u, iostat=E_IO)
      E_IO = move(inside='StructuredGrid', to_find='Piece', repeat=np, cf=rf, buffer=s_buffer) ! find the 'np' piece
      inquire(unit=vtk(rf)%u, pos=pos, iostat=E_IO) !annotate the current position in the file
      call get_char(buffer=s_buffer, attrib='Extent', val=aux, E_IO=E_IO)
      if(E_IO == 0) then
        read(aux,*, iostat=E_IO) nx1,nx2,ny1,ny2,nz1,nz2
        if(E_IO == 0) then
          NN = (nx2-nx1+1)*(ny2-ny1+1)*(nz2-nz1+1)
          E_IO = search(from=pos, inside='Points', to_find='DataArray', with_attribute='Name', of_value='Points', &
                        buffer=s_buffer,content=data)
          call get_char(buffer=s_buffer, attrib='format', val=fmt,  E_IO=E_IO)
          call get_char(buffer=s_buffer, attrib='type', val=type,  E_IO=E_IO)
          if (trim(adjustlt(Upper_Case(fmt)))/='BINARY' .or. &
              trim(adjustlt(Upper_Case(type)))/='FLOAT32') then
            E_IO = -1_I4P
          else
            ! Decode base64 packed data
            data=trim(adjustlt(data))
            allocate(dI1P(3*NN*int(BYR4P,I4P)+int(BYI4P,I4P)))
            call b64_decode(code=data,n=dI1P); if(allocated(data)) deallocate(data)
            ! Unpack data [1xI4P,3*NNxR4P]
            N_byte =  transfer(dI1P(1:int(BYI4P,I4P)),N_byte)
            s = size(transfer(dI1P(int(BYI4P,I4P)+1:),XYZp)); allocate(XYZp(1:s))
            XYZp = transfer(dI1P(int(BYI4P,I4P)+1:),XYZp); if(allocated(dI1P)) deallocate(dI1P)
            allocate(X(nx1:nx2,ny1:ny2,nz1:nz2), Y(nx1:nx2,ny1:ny2,nz1:nz2), Z(nx1:nx2,ny1:ny2,nz1:nz2), stat=E_IO)
            s=1;do k=nz1,nz2-nz1;do j=ny1,ny2;do i=nx1,nx2;
              X(i,j,k)=XYZp(s); s=s+1; Y(i,j,k)=XYZp(s); s=s+1; Z(i,j,k)=XYZp(s); s=s+1
            enddo;enddo;enddo;
            if(allocated(XYZp)) deallocate(XYZp)
          endif
        endif
      endif

    case(raw)

      rewind(unit=vtk(rf)%u, iostat=E_IO)
      E_IO = move(inside='StructuredGrid', to_find='Piece', repeat=np, cf=rf, buffer=s_buffer) ! find the 'np' piece
      inquire(unit=vtk(rf)%u, pos=pos, iostat=E_IO) !annotate the current position in the file
      call get_char(buffer=s_buffer, attrib='Extent', val=aux, E_IO=E_IO)
      if(E_IO == 0) then
        read(aux,*, iostat=E_IO) nx1,nx2,ny1,ny2,nz1,nz2
        if(E_IO == 0) then
          NN = (nx2-nx1+1)*(ny2-ny1+1)*(nz2-nz1+1)
          E_IO = search(from=pos, inside='Points', to_find='DataArray', with_attribute='Name', of_value='Points', &
                        buffer=s_buffer)
          call get_int(buffer=s_buffer,  attrib='offset', val=offs, E_IO=E_IO)
          call get_char(buffer=s_buffer, attrib='format', val=fmt,  E_IO=E_IO)
          call get_char(buffer=s_buffer, attrib='type', val=type,  E_IO=E_IO)
          if (trim(adjustlt(Upper_Case(fmt)))/='APPENDED' .or. &
              trim(adjustlt(Upper_Case(type)))/='FLOAT32') then
            E_IO = -1_I4P
          else
            allocate(X(nx1:nx2,ny1:ny2,nz1:nz2), Y(nx1:nx2,ny1:ny2,nz1:nz2), Z(nx1:nx2,ny1:ny2,nz1:nz2), stat=E_IO)
            read(unit=vtk(rf)%u, iostat=E_IO, pos = vtk(rf)%ioffset+offs) N_Byte, &
                (((X(i,j,k),Y(i,j,k),Z(i,j,k),i=nx1,nx2),j=ny1,ny2),k=nz1,nz2) !get appended array
          endif
        endif
      endif
    end select
  !---------------------------------------------------------------------------------------------------------------------------------
  end function

  function VTK_GEO_XML_STRG_1DAP_R4_READ(nx1,nx2,ny1,ny2,nz1,nz2,NN,XYZ,npiece,cf) result(E_IO)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Function for reading mesh with \b UnstructuredGrid topology (R8P).
  !---------------------------------------------------------------------------------------------------------------------------------
  integer(I4P),           intent(OUT) :: nx1      !< Initial node of x axis.
  integer(I4P),           intent(OUT) :: nx2      !< Final node of x axis.
  integer(I4P),           intent(OUT) :: ny1      !< Initial node of y axis.
  integer(I4P),           intent(OUT) :: ny2      !< Final node of y axis.
  integer(I4P),           intent(OUT) :: nz1      !< Initial node of z axis.
  integer(I4P),           intent(OUT) :: nz2      !< Final node of z axis.
  integer(I4P),           intent(OUT) :: NN       !< Number of nodes
  real(R4P), allocatable, intent(OUT) :: XYZ(:,:) !< x coordinates
  integer(I4P), optional, intent(IN)  :: npiece   !< Number of the piece to read (by default: 1)
  integer(I4P), optional, intent(IN)  :: cf       !< Current file index (for concurrent files IO).
  integer(I4P)                        :: E_IO     !< Input/Output inquiring flag: $0$ if IO is done, $> 0$ if IO is not done
  integer(I4P)                        :: rf       !< Real file index.
  character(len=:),allocatable        :: s_buffer !< Buffer string.
  character(len=:),allocatable        :: aux      !< Auxiliary string.
  character(len=:), allocatable       :: fmt
  character(len=:), allocatable       :: type
  character(len=:), allocatable       :: data
  integer(I4P)                        :: np, i, j, offs, N_Byte, pos, s
  integer(I1P), allocatable           :: dI1P(:)
  real(R4P), allocatable              :: XYZp(:)
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  rf = f
  if (present(cf)) then
    rf = cf ; f = cf
  endif
  np = 1_I4P; if (present(npiece)) np = npiece
  E_IO = -1_I4P
  select case(vtk(rf)%f)

    case(ascii)

      rewind(unit=vtk(rf)%u, iostat=E_IO)
      E_IO = move(inside='StructuredGrid', to_find='Piece', repeat=np, cf=rf, buffer=s_buffer) ! find the 'np' piece
      inquire(unit=vtk(rf)%u, pos=pos, iostat=E_IO) !annotate the current position in the file
      call get_char(buffer=s_buffer, attrib='Extent', val=aux, E_IO=E_IO)
      if(E_IO == 0) then
        read(aux,*, iostat=E_IO) nx1,nx2,ny1,ny2,nz1,nz2
        if(E_IO == 0) then
          NN = (nx2-nx1+1)*(ny2-ny1+1)*(nz2-nz1+1)
          E_IO = search(from=pos, inside='Points', to_find='DataArray', with_attribute='Name', of_value='Points', &
                        buffer=s_buffer,content=data)
          call get_char(buffer=s_buffer, attrib='format', val=fmt,  E_IO=E_IO)
          call get_char(buffer=s_buffer, attrib='type', val=type,  E_IO=E_IO)
          if (trim(adjustlt(Upper_Case(fmt)))/='ASCII' .or. &
              trim(adjustlt(Upper_Case(type)))/='FLOAT32') then
            E_IO = -1_I4P
          else
            allocate(XYZ(3,NN), stat=E_IO)
            read(data, fmt=*, iostat=E_IO) ((XYZ(i,j),i=1,3),j=1,NN) !get ascii array
          endif
          if(allocated(data)) deallocate(data)
        endif
      endif

    case(binary)

      rewind(unit=vtk(rf)%u, iostat=E_IO)
      E_IO = move(inside='StructuredGrid', to_find='Piece', repeat=np, cf=rf, buffer=s_buffer) ! find the 'np' piece
      inquire(unit=vtk(rf)%u, pos=pos, iostat=E_IO) !annotate the current position in the file
      call get_char(buffer=s_buffer, attrib='Extent', val=aux, E_IO=E_IO)
      if(E_IO == 0) then
        read(aux,*, iostat=E_IO) nx1,nx2,ny1,ny2,nz1,nz2
        if(E_IO == 0) then
          NN = (nx2-nx1+1)*(ny2-ny1+1)*(nz2-nz1+1)
          E_IO = search(from=pos, inside='Points', to_find='DataArray', with_attribute='Name', of_value='Points', &
                        buffer=s_buffer,content=data)
          call get_char(buffer=s_buffer, attrib='format', val=fmt,  E_IO=E_IO)
          call get_char(buffer=s_buffer, attrib='type', val=type,  E_IO=E_IO)
          if (trim(adjustlt(Upper_Case(fmt)))/='BINARY' .or. &
              trim(adjustlt(Upper_Case(type)))/='FLOAT32') then
            E_IO = -1_I4P
          else
            ! Decode base64 packed data
            data=trim(adjustlt(data))
            allocate(dI1P(3*NN*int(BYR4P,I4P)+int(BYI4P,I4P)))
            call b64_decode(code=data,n=dI1P); if(allocated(data)) deallocate(data)
            ! Unpack data [1xI4P,3*NNxR4P]
            N_byte =  transfer(dI1P(1:int(BYI4P,I4P)),N_byte)
            s = size(transfer(dI1P(int(BYI4P,I4P)+1:),XYZp)); allocate(XYZp(1:s))
            XYZp = transfer(dI1P(int(BYI4P,I4P)+1:),XYZp); if(allocated(dI1P)) deallocate(dI1P)
            allocate(XYZ(3,NN), stat=E_IO)
            XYZ = reshape(XYZp,(/3,NN/))
            if(allocated(XYZp)) deallocate(XYZp)
          endif
        endif
      endif

    case(raw)

      rewind(unit=vtk(rf)%u, iostat=E_IO)
      E_IO = move(inside='StructuredGrid', to_find='Piece', repeat=np, cf=rf, buffer=s_buffer) ! find the 'np' piece
      inquire(unit=vtk(rf)%u, pos=pos, iostat=E_IO) !annotate the current position in the file
      call get_char(buffer=s_buffer, attrib='Extent', val=aux, E_IO=E_IO)
      if(E_IO == 0) then
        read(aux,*, iostat=E_IO) nx1,nx2,ny1,ny2,nz1,nz2
        if(E_IO == 0) then
          NN = (nx2-nx1+1)*(ny2-ny1+1)*(nz2-nz1+1)
          E_IO = search(from=pos, inside='Points', to_find='DataArray', with_attribute='Name', of_value='Points', &
                        buffer=s_buffer)
          call get_int(buffer=s_buffer,  attrib='offset', val=offs, E_IO=E_IO)
          call get_char(buffer=s_buffer, attrib='format', val=fmt,  E_IO=E_IO)
          call get_char(buffer=s_buffer, attrib='type', val=type,  E_IO=E_IO)
          if (trim(adjustlt(Upper_Case(fmt)))/='APPENDED' .or. &
              trim(adjustlt(Upper_Case(type)))/='FLOAT32') then
            E_IO = -1_I4P
          else
            allocate(XYZ(3,NN), stat=E_IO)
            read(unit=vtk(rf)%u, iostat=E_IO, pos = vtk(rf)%ioffset+offs) N_Byte, ((XYZ(i,j),i=1,3),j=1,NN) !get appended array
          endif
        endif
      endif
    end select
  !---------------------------------------------------------------------------------------------------------------------------------
  end function VTK_GEO_XML_STRG_1DAP_R4_READ

  function VTK_GEO_XML_STRG_3DAP_R4_READ(nx1,nx2,ny1,ny2,nz1,nz2,NN,XYZ,npiece,cf) result(E_IO)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Function for reading mesh with \b UnstructuredGrid topology (R8P).
  !---------------------------------------------------------------------------------------------------------------------------------
  integer(I4P),           intent(OUT) :: nx1      !< Initial node of x axis.
  integer(I4P),           intent(OUT) :: nx2      !< Final node of x axis.
  integer(I4P),           intent(OUT) :: ny1      !< Initial node of y axis.
  integer(I4P),           intent(OUT) :: ny2      !< Final node of y axis.
  integer(I4P),           intent(OUT) :: nz1      !< Initial node of z axis.
  integer(I4P),           intent(OUT) :: nz2      !< Final node of z axis.
  integer(I4P),           intent(OUT) :: NN       !< Number of nodes
  real(R4P), allocatable, intent(OUT) :: XYZ(:,:,:,:)     !< x coordinates
  integer(I4P), optional, intent(IN)  :: npiece   !< Number of the piece to read (by default: 1)
  integer(I4P), optional, intent(IN)  :: cf       !< Current file index (for concurrent files IO).
  integer(I4P)                        :: E_IO     !< Input/Output inquiring flag: $0$ if IO is done, $> 0$ if IO is not done
  integer(I4P)                        :: rf       !< Real file index.
  character(len=:),allocatable        :: s_buffer !< Buffer string.
  character(len=:),allocatable        :: aux      !< Auxiliary string.
  character(len=:), allocatable       :: fmt
  character(len=:), allocatable       :: type
  character(len=:), allocatable       :: data
  integer(I4P)                        :: np, i, j, k, l, offs, N_Byte, pos, s
  integer(I1P), allocatable           :: dI1P(:)
  real(R4P), allocatable              :: XYZp(:)
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  rf = f
  if (present(cf)) then
    rf = cf ; f = cf
  endif
  np = 1_I4P; if (present(npiece)) np = npiece
  E_IO = -1_I4P
  select case(vtk(rf)%f)

    case(ascii)

      rewind(unit=vtk(rf)%u, iostat=E_IO)
      E_IO = move(inside='StructuredGrid', to_find='Piece', repeat=np, cf=rf, buffer=s_buffer) ! find the 'np' piece
      inquire(unit=vtk(rf)%u, pos=pos, iostat=E_IO) !annotate the current position in the file
      call get_char(buffer=s_buffer, attrib='Extent', val=aux, E_IO=E_IO)
      if(E_IO == 0) then
        read(aux,*, iostat=E_IO) nx1,nx2,ny1,ny2,nz1,nz2
        if(E_IO == 0) then
          NN = (nx2-nx1+1)*(ny2-ny1+1)*(nz2-nz1+1)
          E_IO = search(from=pos, inside='Points', to_find='DataArray', with_attribute='Name', of_value='Points', &
                        buffer=s_buffer,content=data)
          call get_char(buffer=s_buffer, attrib='format', val=fmt,  E_IO=E_IO)
          call get_char(buffer=s_buffer, attrib='type', val=type,  E_IO=E_IO)
          if (trim(adjustlt(Upper_Case(fmt)))/='ASCII' .or. &
              trim(adjustlt(Upper_Case(type)))/='FLOAT32') then
            E_IO = -1_I4P
          else
            allocate(XYZ(3,nx1:nx2,ny1:ny2,nz1:nz2), stat=E_IO)
            read(data, fmt=*, iostat=E_IO) ((((XYZ(i,j,k,l),i=1,3),j=nx1,nx2),k=ny1,ny2),l=nz1,nz2) !get ascii array
          endif
          if(allocated(data)) deallocate(data)
        endif
      endif

    case(binary)

      rewind(unit=vtk(rf)%u, iostat=E_IO)
      E_IO = move(inside='StructuredGrid', to_find='Piece', repeat=np, cf=rf, buffer=s_buffer) ! find the 'np' piece
      inquire(unit=vtk(rf)%u, pos=pos, iostat=E_IO) !annotate the current position in the file
      call get_char(buffer=s_buffer, attrib='Extent', val=aux, E_IO=E_IO)
      if(E_IO == 0) then
        read(aux,*, iostat=E_IO) nx1,nx2,ny1,ny2,nz1,nz2
        if(E_IO == 0) then
          NN = (nx2-nx1+1)*(ny2-ny1+1)*(nz2-nz1+1)
          E_IO = search(from=pos, inside='Points', to_find='DataArray', with_attribute='Name', of_value='Points', &
                        buffer=s_buffer,content=data)
          call get_char(buffer=s_buffer, attrib='format', val=fmt,  E_IO=E_IO)
          call get_char(buffer=s_buffer, attrib='type', val=type,  E_IO=E_IO)
          if (trim(adjustlt(Upper_Case(fmt)))/='BINARY' .or. &
              trim(adjustlt(Upper_Case(type)))/='FLOAT32') then
            E_IO = -1_I4P
          else
            ! Decode base64 packed data
            data=trim(adjustlt(data))
            allocate(dI1P(3*NN*int(BYR4P,I4P)+int(BYI4P,I4P)))
            call b64_decode(code=data,n=dI1P); if(allocated(data)) deallocate(data)
            ! Unpack data [1xI4P,3*NNxR4P]
            N_byte =  transfer(dI1P(1:int(BYI4P,I4P)),N_byte)
            s = size(transfer(dI1P(int(BYI4P,I4P)+1:),XYZp)); allocate(XYZp(1:s))
            XYZp = transfer(dI1P(int(BYI4P,I4P)+1:),XYZp); if(allocated(dI1P)) deallocate(dI1P)
            allocate(XYZ(3,nx1:nx2,ny1:ny2,nz1:nz2), stat=E_IO)
            XYZ(1:3,nx1:nx2,ny1:ny2,nz1:nz2) = reshape(XYZp, (/3,nx2-nx1+1,ny2-ny1+1,nz2-nz1+1/))
            if(allocated(XYZp)) deallocate(XYZp)
          endif
        endif
      endif

    case(raw)

      rewind(unit=vtk(rf)%u, iostat=E_IO)
      E_IO = move(inside='StructuredGrid', to_find='Piece', repeat=np, cf=rf, buffer=s_buffer) ! find the 'np' piece
      inquire(unit=vtk(rf)%u, pos=pos, iostat=E_IO) !annotate the current position in the file
      call get_char(buffer=s_buffer, attrib='Extent', val=aux, E_IO=E_IO)
      if(E_IO == 0) then
        read(aux,*, iostat=E_IO) nx1,nx2,ny1,ny2,nz1,nz2
        if(E_IO == 0) then
          NN = (nx2-nx1+1)*(ny2-ny1+1)*(nz2-nz1+1)
          E_IO = search(from=pos, inside='Points', to_find='DataArray', with_attribute='Name', of_value='Points', buffer=s_buffer)
          call get_int(buffer=s_buffer,  attrib='offset', val=offs, E_IO=E_IO)
          call get_char(buffer=s_buffer, attrib='format', val=fmt,  E_IO=E_IO)
          call get_char(buffer=s_buffer, attrib='type', val=type,  E_IO=E_IO)
          if (trim(adjustlt(Upper_Case(fmt)))/='APPENDED' .or. &
              trim(adjustlt(Upper_Case(type)))/='FLOAT32') then
            E_IO = -1_I4P
          else
            allocate(XYZ(3,nx1:nx2,ny1:ny2,nz1:nz2), stat=E_IO)
            read(unit=vtk(rf)%u, iostat=E_IO, pos = vtk(rf)%ioffset+offs) N_Byte, &
                ((((XYZ(i,j,k,l),i=1,3),j=nx1,nx2),k=ny1,ny2),l=nz1,nz2) !get appended array
          endif
        endif
      endif
    end select
  !---------------------------------------------------------------------------------------------------------------------------------
  end function VTK_GEO_XML_STRG_3DAP_R4_READ

  function VTK_GEO_XML_RECT_R8_READ(nx1,nx2,ny1,ny2,nz1,nz2,X,Y,Z,npiece,cf) result(E_IO)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Function for reading mesh with \b UnstructuredGrid topology (R8P).
  !---------------------------------------------------------------------------------------------------------------------------------
  integer(I4P),           intent(OUT) :: nx1      !< Initial node of x axis.
  integer(I4P),           intent(OUT) :: nx2      !< Final node of x axis.
  integer(I4P),           intent(OUT) :: ny1      !< Initial node of y axis.
  integer(I4P),           intent(OUT) :: ny2      !< Final node of y axis.
  integer(I4P),           intent(OUT) :: nz1      !< Initial node of z axis.
  integer(I4P),           intent(OUT) :: nz2      !< Final node of z axis.
  real(R8P), allocatable, intent(OUT) :: X(:)     !< x coordinates
  real(R8P), allocatable, intent(OUT) :: Y(:)     !< y coordinates
  real(R8P), allocatable, intent(OUT) :: Z(:)     !< z coordinates
  integer(I4P), optional, intent(IN)  :: npiece   !< Number of the piece to read (by default: 1)
  integer(I4P), optional, intent(IN)  :: cf       !< Current file index (for concurrent files IO).
  integer(I4P)                        :: E_IO     !< Input/Output inquiring flag: $0$ if IO is done, $> 0$ if IO is not done
  integer(I4P)                        :: rf       !< Real file index.
  character(len=:),allocatable        :: s_buffer !< Buffer string.
  character(len=:),allocatable        :: aux      !< Auxiliary string.
  character(len=:), allocatable       :: fmt, fmtX, fmtY, fmtZ
  character(len=:), allocatable       :: type, typeX, typeY, typeZ
  character(len=:), allocatable       :: data
  integer(I4P)                        :: offsX, offsY, offsZ
  integer(I4P)                        :: np, i, N_Byte, pos, s
  integer(I1P), allocatable           :: dI1P(:)
  real(R8P), allocatable              :: XYZp(:)
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  rf = f
  if (present(cf)) then
    rf = cf ; f = cf
  endif
  np = 1_I4P; if (present(npiece)) np = npiece
  E_IO = -1_I4P
  select case(vtk(rf)%f)

    case(ascii)

      rewind(unit=vtk(rf)%u, iostat=E_IO)
      E_IO = move(inside='RectilinearGrid', to_find='Piece', repeat=np, cf=rf, buffer=s_buffer) ! find the 'np' piece
      inquire(unit=vtk(rf)%u, pos=pos, iostat=E_IO) !annotate the current position in the file
      call get_char(buffer=s_buffer, attrib='Extent', val=aux, E_IO=E_IO)
      if(E_IO == 0) then
        read(aux,*, iostat=E_IO) nx1,nx2,ny1,ny2,nz1,nz2
        if(E_IO == 0) then
          E_IO = search(from=pos, inside='Coordinates', to_find='DataArray', with_attribute='Name', of_value='X', &
                        buffer=s_buffer,content=data)
          call get_char(buffer=s_buffer, attrib='format', val=fmt,  E_IO=E_IO)
          call get_char(buffer=s_buffer, attrib='type',   val=type, E_IO=E_IO)
          if (trim(adjustlt(Upper_Case(fmt)))/='ASCII' .or. &
              trim(adjustlt(Upper_Case(type)))/='FLOAT64') then
            E_IO = -1_I4P
          else
            allocate(X(nx1:nx2), stat=E_IO)
            read(data, fmt=*, iostat=E_IO) (X(i), i=nx1,nx2) !get X ascii array
          endif
          E_IO = search(from=pos, inside='Coordinates', to_find='DataArray', with_attribute='Name', of_value='Y', &
                        buffer=s_buffer,content=data)
          call get_char(buffer=s_buffer, attrib='format', val=fmt,  E_IO=E_IO)
          call get_char(buffer=s_buffer, attrib='type',   val=type, E_IO=E_IO)
          if (trim(adjustlt(Upper_Case(fmt)))/='ASCII' .or. &
              trim(adjustlt(Upper_Case(type)))/='FLOAT64') then
            E_IO = -1_I4P
          else
            allocate(Y(ny1:ny2), stat=E_IO)
            read(data, fmt=*, iostat=E_IO) (Y(i), i=ny1,ny2) !get Y ascii array
          endif
          E_IO = search(from=pos, inside='Coordinates', to_find='DataArray', with_attribute='Name', of_value='Z', &
                        buffer=s_buffer,content=data)
          call get_char(buffer=s_buffer, attrib='format', val=fmt,  E_IO=E_IO)
          call get_char(buffer=s_buffer, attrib='type',   val=type, E_IO=E_IO)
          if (trim(adjustlt(Upper_Case(fmt)))/='ASCII' .or. &
              trim(adjustlt(Upper_Case(type)))/='FLOAT64') then
            E_IO = -1_I4P
          else
            allocate(Z(nz1:nz2), stat=E_IO)
            read(data, fmt=*, iostat=E_IO) (Z(i), i=nz1,nz2) !get Z ascii array
          endif
          if(allocated(data)) deallocate(data)
        endif
      endif

    case(binary)

      rewind(unit=vtk(rf)%u, iostat=E_IO)
      E_IO = move(inside='RectilinearGrid', to_find='Piece', repeat=np, cf=rf, buffer=s_buffer) ! find the 'np' piece
      inquire(unit=vtk(rf)%u, pos=pos, iostat=E_IO) !annotate the current position in the file
      call get_char(buffer=s_buffer, attrib='Extent', val=aux, E_IO=E_IO)
      if(E_IO == 0) then
        read(aux,*, iostat=E_IO) nx1,nx2,ny1,ny2,nz1,nz2
        if(E_IO == 0) then
          E_IO = search(from=pos, inside='Coordinates', to_find='DataArray', with_attribute='Name', of_value='X', &
                        buffer=s_buffer,content=data)
          call get_char(buffer=s_buffer, attrib='format', val=fmt,  E_IO=E_IO)
          call get_char(buffer=s_buffer, attrib='type',   val=type, E_IO=E_IO)
          if (trim(adjustlt(Upper_Case(fmt)))/='BINARY' .or. &
              trim(adjustlt(Upper_Case(type)))/='FLOAT64') then
            E_IO = -1_I4P
          else
            ! Decode base64 packed data
            data=trim(adjustlt(data))
            allocate(dI1P(3*((nx2-nx1+1)*(ny2-ny1+1)*(nz2-nz1+1))*int(BYR8P,I4P)+int(BYI4P,I4P)))
            call b64_decode(code=data,n=dI1P); if(allocated(data)) deallocate(data)
            ! Unpack data [1xI4P,3*NNxR8P]
            N_byte =  transfer(dI1P(1:int(BYI4P,I4P)),N_byte)
            s = size(transfer(dI1P(int(BYI4P,I4P)+1:),XYZp)); allocate(XYZp(1:s))
            XYZp = transfer(dI1P(int(BYI4P,I4P)+1:),XYZp); if(allocated(dI1P)) deallocate(dI1P)
            allocate(X(nx1:nx2), stat=E_IO)
            X(nx1:nx2) = XYZp(:)
            if(allocated(XYZp)) deallocate(XYZp)
          endif
          E_IO = search(from=pos, inside='Coordinates', to_find='DataArray', with_attribute='Name', of_value='Y', &
                        buffer=s_buffer,content=data)
          call get_char(buffer=s_buffer, attrib='format', val=fmt,  E_IO=E_IO)
          call get_char(buffer=s_buffer, attrib='type',   val=type, E_IO=E_IO)
          if (trim(adjustlt(Upper_Case(fmt)))/='BINARY' .or. &
              trim(adjustlt(Upper_Case(type)))/='FLOAT64') then
            E_IO = -1_I4P
          else
            ! Decode base64 packed data
            data=trim(adjustlt(data))
            allocate(dI1P(3*((nx2-nx1+1)*(ny2-ny1+1)*(nz2-nz1+1))*int(BYR8P,I4P)+int(BYI4P,I4P)))
            call b64_decode(code=data,n=dI1P); if(allocated(data)) deallocate(data)
            ! Unpack data [1xI4P,3*NNxR8P]
            N_byte =  transfer(dI1P(1:int(BYI4P,I4P)),N_byte)
            s = size(transfer(dI1P(int(BYI4P,I4P)+1:),XYZp)); allocate(XYZp(1:s))
            XYZp = transfer(dI1P(int(BYI4P,I4P)+1:),XYZp); if(allocated(dI1P)) deallocate(dI1P)
            allocate(Y(ny1:ny2), stat=E_IO)
            Y(ny1:ny2) = XYZp(:)
            if(allocated(XYZp)) deallocate(XYZp)
          endif
          E_IO = search(from=pos, inside='Coordinates', to_find='DataArray', with_attribute='Name', of_value='Z', &
                        buffer=s_buffer,content=data)
          call get_char(buffer=s_buffer, attrib='format', val=fmt,  E_IO=E_IO)
          call get_char(buffer=s_buffer, attrib='type',   val=type, E_IO=E_IO)
          if (trim(adjustlt(Upper_Case(fmt)))/='BINARY' .or. &
              trim(adjustlt(Upper_Case(type)))/='FLOAT64') then
            E_IO = -1_I4P
          else
            ! Decode base64 packed data
            data=trim(adjustlt(data))
            allocate(dI1P(3*((nx2-nx1+1)*(ny2-ny1+1)*(nz2-nz1+1))*int(BYR8P,I4P)+int(BYI4P,I4P)))
            call b64_decode(code=data,n=dI1P); if(allocated(data)) deallocate(data)
            ! Unpack data [1xI4P,3*NNxR8P]
            N_byte =  transfer(dI1P(1:int(BYI4P,I4P)),N_byte)
            s = size(transfer(dI1P(int(BYI4P,I4P)+1:),XYZp)); allocate(XYZp(1:s))
            XYZp = transfer(dI1P(int(BYI4P,I4P)+1:),XYZp); if(allocated(dI1P)) deallocate(dI1P)
            allocate(Z(nz1:nz2), stat=E_IO)
            Z(nz1:nz2) = XYZp(:)
            if(allocated(XYZp)) deallocate(XYZp)
          endif
          if(allocated(data)) deallocate(data)
        endif
      endif

    case(raw)

      rewind(unit=vtk(rf)%u, iostat=E_IO)
      E_IO = move(inside='RectilinearGrid', to_find='Piece', repeat=np, cf=rf, buffer=s_buffer) ! find the 'np' piece
      inquire(unit=vtk(rf)%u, pos=pos, iostat=E_IO) !annotate the current position in the file
      call get_char(buffer=s_buffer, attrib='Extent', val=aux, E_IO=E_IO)
      if(E_IO == 0) then
        read(aux,*, iostat=E_IO) nx1,nx2,ny1,ny2,nz1,nz2
        E_IO = search(from=pos, inside='Coordinates', to_find='DataArray', with_attribute='Name', of_value='X', buffer=s_buffer)
        call get_int(buffer=s_buffer,  attrib='offset', val=offsX, E_IO=E_IO)
        call get_char(buffer=s_buffer, attrib='format', val=fmtX,  E_IO=E_IO)
        call get_char(buffer=s_buffer, attrib='type',   val=typeX, E_IO=E_IO)
        E_IO = search(from=pos, inside='Coordinates', to_find='DataArray', with_attribute='Name', of_value='Y', buffer=s_buffer)
        call get_int(buffer=s_buffer,  attrib='offset', val=offsY, E_IO=E_IO)
        call get_char(buffer=s_buffer, attrib='format', val=fmtY,  E_IO=E_IO)
        call get_char(buffer=s_buffer, attrib='type',   val=typeY, E_IO=E_IO)
        E_IO = search(from=pos, inside='Coordinates', to_find='DataArray', with_attribute='Name', of_value='Z', buffer=s_buffer)
        call get_int(buffer=s_buffer,  attrib='offset', val=offsZ, E_IO=E_IO)
        call get_char(buffer=s_buffer, attrib='format', val=fmtZ,  E_IO=E_IO)
        call get_char(buffer=s_buffer, attrib='type',   val=typeZ, E_IO=E_IO)
        if(E_IO == 0) then
          if (trim(adjustlt(Upper_Case(fmtX)))/='APPENDED' .or. &
              trim(adjustlt(Upper_Case(fmtY)))/='APPENDED' .or. &
              trim(adjustlt(Upper_Case(fmtZ)))/='APPENDED' .or. &
              trim(adjustlt(Upper_Case(typeX)))/='FLOAT64'  .or. &
              trim(adjustlt(Upper_Case(typeY)))/='FLOAT64'  .or. &
              trim(adjustlt(Upper_Case(typeZ)))/='FLOAT64') then
            E_IO = -1_I4P
          else
            allocate(X(nx1:nx2), Y(ny1:ny2), Z(nz1:nz2), stat=E_IO)
            read(unit=vtk(rf)%u, iostat=E_IO, pos = vtk(rf)%ioffset+offsX) N_Byte, (X(i), i=nx1,nx2) !get appended array
            read(unit=vtk(rf)%u, iostat=E_IO, pos = vtk(rf)%ioffset+offsY) N_Byte, (Y(i), i=ny1,ny2) !get appended array
            read(unit=vtk(rf)%u, iostat=E_IO, pos = vtk(rf)%ioffset+offsZ) N_Byte, (Z(i), i=nz1,nz2) !get appended array
          endif
        endif
      endif
    end select
  !---------------------------------------------------------------------------------------------------------------------------------
  end function VTK_GEO_XML_RECT_R8_READ

  function VTK_GEO_XML_RECT_R4_READ(nx1,nx2,ny1,ny2,nz1,nz2,X,Y,Z,npiece,cf) result(E_IO)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Function for reading mesh with \b UnstructuredGrid topology (R8P).
  !---------------------------------------------------------------------------------------------------------------------------------
  integer(I4P),           intent(OUT) :: nx1      !< Initial node of x axis.
  integer(I4P),           intent(OUT) :: nx2      !< Final node of x axis.
  integer(I4P),           intent(OUT) :: ny1      !< Initial node of y axis.
  integer(I4P),           intent(OUT) :: ny2      !< Final node of y axis.
  integer(I4P),           intent(OUT) :: nz1      !< Initial node of z axis.
  integer(I4P),           intent(OUT) :: nz2      !< Final node of z axis.
  real(R4P), allocatable, intent(OUT) :: X(:)     !< x coordinates
  real(R4P), allocatable, intent(OUT) :: Y(:)     !< y coordinates
  real(R4P), allocatable, intent(OUT) :: Z(:)     !< z coordinates
  integer(I4P), optional, intent(IN)  :: npiece   !< Number of the piece to read (by default: 1)
  integer(I4P), optional, intent(IN)  :: cf       !< Current file index (for concurrent files IO).
  integer(I4P)                        :: E_IO     !< Input/Output inquiring flag: $0$ if IO is done, $> 0$ if IO is not done
  integer(I4P)                        :: rf       !< Real file index.
  character(len=:),allocatable        :: s_buffer !< Buffer string.
  character(len=:),allocatable        :: aux      !< Auxiliary string.
  character(len=:), allocatable       :: fmt, fmtX, fmtY, fmtZ
  character(len=:), allocatable       :: type, typeX, typeY, typeZ
  character(len=:), allocatable       :: data
  integer(I4P)                        :: offsX, offsY, offsZ
  integer(I4P)                        :: np, i, N_Byte, pos, s
  integer(I1P), allocatable           :: dI1P(:)
  real(R4P), allocatable              :: XYZp(:)
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  rf = f
  if (present(cf)) then
    rf = cf ; f = cf
  endif
  np = 1_I4P; if (present(npiece)) np = npiece
  E_IO = -1_I4P
  select case(vtk(rf)%f)

    case(ascii)

      rewind(unit=vtk(rf)%u, iostat=E_IO)
      E_IO = move(inside='RectilinearGrid', to_find='Piece', repeat=np, cf=rf, buffer=s_buffer) ! find the 'np' piece
      inquire(unit=vtk(rf)%u, pos=pos, iostat=E_IO) !annotate the current position in the file
      call get_char(buffer=s_buffer, attrib='Extent', val=aux, E_IO=E_IO)
      if(E_IO == 0) then
        read(aux,*, iostat=E_IO) nx1,nx2,ny1,ny2,nz1,nz2
        if(E_IO == 0) then
          E_IO = search(from=pos, inside='Coordinates', to_find='DataArray', with_attribute='Name', of_value='X', &
                        buffer=s_buffer,content=data)
          call get_char(buffer=s_buffer, attrib='format', val=fmt,  E_IO=E_IO)
          call get_char(buffer=s_buffer, attrib='type',   val=type, E_IO=E_IO)
          if (trim(adjustlt(Upper_Case(fmt)))/='ASCII' .or. &
              trim(adjustlt(Upper_Case(type)))/='FLOAT32') then
            E_IO = -1_I4P
          else
            allocate(X(nx1:nx2), stat=E_IO)
            read(data, fmt=*, iostat=E_IO) (X(i), i=nx1,nx2) !get X ascii array
          endif
          E_IO = search(from=pos, inside='Coordinates', to_find='DataArray', with_attribute='Name', of_value='Y', &
                        buffer=s_buffer,content=data)
          call get_char(buffer=s_buffer, attrib='format', val=fmt,  E_IO=E_IO)
          call get_char(buffer=s_buffer, attrib='type',   val=type, E_IO=E_IO)
          if (trim(adjustlt(Upper_Case(fmt)))/='ASCII' .or. &
              trim(adjustlt(Upper_Case(type)))/='FLOAT32') then
            E_IO = -1_I4P
          else
            allocate(Y(ny1:ny2), stat=E_IO)
            read(data, fmt=*, iostat=E_IO) (Y(i), i=ny1,ny2) !get Y ascii array
          endif
          E_IO = search(from=pos, inside='Coordinates', to_find='DataArray', with_attribute='Name', of_value='Z', &
                        buffer=s_buffer,content=data)
          call get_char(buffer=s_buffer, attrib='format', val=fmt,  E_IO=E_IO)
          call get_char(buffer=s_buffer, attrib='type',   val=type, E_IO=E_IO)
          if (trim(adjustlt(Upper_Case(fmt)))/='ASCII' .or. &
              trim(adjustlt(Upper_Case(type)))/='FLOAT32') then
            E_IO = -1_I4P
          else
            allocate(Z(nz1:nz2), stat=E_IO)
            read(data, fmt=*, iostat=E_IO) (Z(i), i=nz1,nz2) !get Z ascii array
          endif
          if(allocated(data)) deallocate(data)
        endif
      endif

    case(binary)

      rewind(unit=vtk(rf)%u, iostat=E_IO)
      E_IO = move(inside='RectilinearGrid', to_find='Piece', repeat=np, cf=rf, buffer=s_buffer) ! find the 'np' piece
      inquire(unit=vtk(rf)%u, pos=pos, iostat=E_IO) !annotate the current position in the file
      call get_char(buffer=s_buffer, attrib='Extent', val=aux, E_IO=E_IO)
      if(E_IO == 0) then
        read(aux,*, iostat=E_IO) nx1,nx2,ny1,ny2,nz1,nz2
        if(E_IO == 0) then
          E_IO = search(from=pos, inside='Coordinates', to_find='DataArray', with_attribute='Name', of_value='X', &
                        buffer=s_buffer,content=data)
          call get_char(buffer=s_buffer, attrib='format', val=fmt,  E_IO=E_IO)
          call get_char(buffer=s_buffer, attrib='type',   val=type, E_IO=E_IO)
          if (trim(adjustlt(Upper_Case(fmt)))/='BINARY' .or. &
              trim(adjustlt(Upper_Case(type)))/='FLOAT32') then
            E_IO = -1_I4P
          else
            ! Decode base64 packed data
            data=trim(adjustlt(data))
            allocate(dI1P(3*((nx2-nx1+1)*(ny2-ny1+1)*(nz2-nz1+1))*int(BYR4P,I4P)+int(BYI4P,I4P)))
            call b64_decode(code=data,n=dI1P); if(allocated(data)) deallocate(data)
            ! Unpack data [1xI4P,3*NNxR4P]
            N_byte =  transfer(dI1P(1:int(BYI4P,I4P)),N_byte)
            s = size(transfer(dI1P(int(BYI4P,I4P)+1:),XYZp)); allocate(XYZp(1:s))
            XYZp = transfer(dI1P(int(BYI4P,I4P)+1:),XYZp); if(allocated(dI1P)) deallocate(dI1P)
            allocate(X(nx1:nx2), stat=E_IO)
            X(nx1:nx2) = XYZp(:)
            if(allocated(XYZp)) deallocate(XYZp)
          endif
          E_IO = search(from=pos, inside='Coordinates', to_find='DataArray', with_attribute='Name', of_value='Y', &
                        buffer=s_buffer,content=data)
          call get_char(buffer=s_buffer, attrib='format', val=fmt,  E_IO=E_IO)
          call get_char(buffer=s_buffer, attrib='type',   val=type, E_IO=E_IO)
          if (trim(adjustlt(Upper_Case(fmt)))/='BINARY' .or. &
              trim(adjustlt(Upper_Case(type)))/='FLOAT32') then
            E_IO = -1_I4P
          else
            ! Decode base64 packed data
            data=trim(adjustlt(data))
            allocate(dI1P(3*((nx2-nx1+1)*(ny2-ny1+1)*(nz2-nz1+1))*int(BYR4P,I4P)+int(BYI4P,I4P)))
            call b64_decode(code=data,n=dI1P); if(allocated(data)) deallocate(data)
            ! Unpack data [1xI4P,3*NNxR4P]
            N_byte =  transfer(dI1P(1:int(BYI4P,I4P)),N_byte)
            s = size(transfer(dI1P(int(BYI4P,I4P)+1:),XYZp)); allocate(XYZp(1:s))
            XYZp = transfer(dI1P(int(BYI4P,I4P)+1:),XYZp); if(allocated(dI1P)) deallocate(dI1P)
            allocate(Y(ny1:ny2), stat=E_IO)
            Y(ny1:ny2) = XYZp(:)
            if(allocated(XYZp)) deallocate(XYZp)
          endif
          E_IO = search(from=pos, inside='Coordinates', to_find='DataArray', with_attribute='Name', of_value='Z', &
                        buffer=s_buffer,content=data)
          call get_char(buffer=s_buffer, attrib='format', val=fmt,  E_IO=E_IO)
          call get_char(buffer=s_buffer, attrib='type',   val=type, E_IO=E_IO)
          if (trim(adjustlt(Upper_Case(fmt)))/='BINARY' .or. &
              trim(adjustlt(Upper_Case(type)))/='FLOAT32') then
            E_IO = -1_I4P
          else
            ! Decode base64 packed data
            data=trim(adjustlt(data))
            allocate(dI1P(3*((nx2-nx1+1)*(ny2-ny1+1)*(nz2-nz1+1))*int(BYR4P,I4P)+int(BYI4P,I4P)))
            call b64_decode(code=data,n=dI1P); if(allocated(data)) deallocate(data)
            ! Unpack data [1xI4P,3*NNxR4P]
            N_byte =  transfer(dI1P(1:int(BYI4P,I4P)),N_byte)
            s = size(transfer(dI1P(int(BYI4P,I4P)+1:),XYZp)); allocate(XYZp(1:s))
            XYZp = transfer(dI1P(int(BYI4P,I4P)+1:),XYZp); if(allocated(dI1P)) deallocate(dI1P)
            allocate(Z(nz1:nz2), stat=E_IO)
            Z(nz1:nz2) = XYZp(:)
            if(allocated(XYZp)) deallocate(XYZp)
          endif
          if(allocated(data)) deallocate(data)
        endif
      endif

    case(raw)

      rewind(unit=vtk(rf)%u, iostat=E_IO)
      E_IO = move(inside='RectilinearGrid', to_find='Piece', repeat=np, cf=rf, buffer=s_buffer) ! find the 'np' piece
      inquire(unit=vtk(rf)%u, pos=pos, iostat=E_IO) !annotate the current position in the file
      call get_char(buffer=s_buffer, attrib='Extent', val=aux, E_IO=E_IO)
      if(E_IO == 0) then
        read(aux,*, iostat=E_IO) nx1,nx2,ny1,ny2,nz1,nz2
        E_IO = search(from=pos, inside='Coordinates', to_find='DataArray', with_attribute='Name', of_value='X', buffer=s_buffer)
        call get_int(buffer=s_buffer,  attrib='offset', val=offsX, E_IO=E_IO)
        call get_char(buffer=s_buffer, attrib='format', val=fmtX,  E_IO=E_IO)
        call get_char(buffer=s_buffer, attrib='type',   val=typeX, E_IO=E_IO)
        E_IO = search(from=pos, inside='Coordinates', to_find='DataArray', with_attribute='Name', of_value='Y', buffer=s_buffer)
        call get_int(buffer=s_buffer,  attrib='offset', val=offsY, E_IO=E_IO)
        call get_char(buffer=s_buffer, attrib='format', val=fmtY,  E_IO=E_IO)
        call get_char(buffer=s_buffer, attrib='type',   val=typeY, E_IO=E_IO)
        E_IO = search(from=pos, inside='Coordinates', to_find='DataArray', with_attribute='Name', of_value='Z', buffer=s_buffer)
        call get_int(buffer=s_buffer,  attrib='offset', val=offsZ, E_IO=E_IO)
        call get_char(buffer=s_buffer, attrib='format', val=fmtZ,  E_IO=E_IO)
        call get_char(buffer=s_buffer, attrib='type',   val=typeZ, E_IO=E_IO)
        if(E_IO == 0) then
          if (trim(adjustlt(Upper_Case(fmtX)))/='APPENDED' .or. &
              trim(adjustlt(Upper_Case(fmtY)))/='APPENDED' .or. &
              trim(adjustlt(Upper_Case(fmtZ)))/='APPENDED' .or. &
              trim(adjustlt(Upper_Case(typeX)))/='FLOAT32'  .or. &
              trim(adjustlt(Upper_Case(typeY)))/='FLOAT32'  .or. &
              trim(adjustlt(Upper_Case(typeZ)))/='FLOAT32') then
            E_IO = -1_I4P
          else
            allocate(X(nx1:nx2), Y(ny1:ny2), Z(nz1:nz2), stat=E_IO)
            read(unit=vtk(rf)%u, iostat=E_IO, pos = vtk(rf)%ioffset+offsX) N_Byte, (X(i), i=nx1,nx2) !get appended array
            read(unit=vtk(rf)%u, iostat=E_IO, pos = vtk(rf)%ioffset+offsY) N_Byte, (Y(i), i=ny1,ny2) !get appended array
            read(unit=vtk(rf)%u, iostat=E_IO, pos = vtk(rf)%ioffset+offsZ) N_Byte, (Z(i), i=nz1,nz2) !get appended array
          endif
        endif
      endif
    end select
  !---------------------------------------------------------------------------------------------------------------------------------
  end function VTK_GEO_XML_RECT_R4_READ

  function VTK_GEO_XML_UNST_R8_READ(NN,NC,X,Y,Z,npiece,cf) result(E_IO)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Function for reading mesh with \b UnstructuredGrid topology (R8P).
  !---------------------------------------------------------------------------------------------------------------------------------
  integer(I4P),           intent(OUT) :: NN       !< number of nodes
  integer(I4P),           intent(OUT) :: NC       !< number of cells
  real(R8P), allocatable, intent(OUT) :: X(:)     !< x coordinates
  real(R8P), allocatable, intent(OUT) :: Y(:)     !< y coordinates
  real(R8P), allocatable, intent(OUT) :: Z(:)     !< z coordinates
  integer(I4P), optional, intent(IN)  :: npiece   !< Number of the piece to read (by default: 1)
  integer(I4P), optional, intent(IN)  :: cf       !< Current file index (for concurrent files IO).
  integer(I4P)                        :: E_IO     !< Input/Output inquiring flag: $0$ if IO is done, $> 0$ if IO is not done
  integer(I4P)                        :: rf       !< Real file index.
  character(len=:),allocatable        :: s_buffer !< Buffer string.
  character(len=:), allocatable       :: fmt
  character(len=:), allocatable       :: type
  character(len=:), allocatable       :: data
  integer(I4P)                        :: np, i, offs, N_Byte, pos, s
  integer(I1P), allocatable           :: dI1P(:)
  real(R8P), allocatable              :: XYZp(:)
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  rf = f
  if (present(cf)) then
    rf = cf ; f = cf
  endif
  np = 1_I4P; if (present(npiece)) np = npiece
  E_IO = -1_I4P
  select case(vtk(rf)%f)

    case(ascii)

      rewind(unit=vtk(rf)%u, iostat=E_IO)
      E_IO = move(inside='UnstructuredGrid', to_find='Piece', repeat=np, cf=rf, buffer=s_buffer) ! find the 'np' piece
      inquire(unit=vtk(rf)%u, pos=pos, iostat=E_IO) !annotate the current position in the file
      call get_int(buffer=s_buffer, attrib='NumberOfPoints', val=NN, E_IO=E_IO)
      if(E_IO == 0) then
        call get_int(buffer=s_buffer, attrib='NumberOfCells', val=NC, E_IO=E_IO)
        E_IO = search(from=pos, inside='Points', to_find='DataArray', with_attribute='Name', of_value='Points', &
                      buffer=s_buffer,content=data)
        call get_char(buffer=s_buffer, attrib='format', val=fmt,  E_IO=E_IO)
        call get_char(buffer=s_buffer, attrib='type', val=type,  E_IO=E_IO)
        if (trim(adjustlt(Upper_Case(fmt)))/='ASCII' .or. &
            trim(adjustlt(Upper_Case(type)))/='FLOAT64') then
          E_IO = -1_I4P
        else
          allocate(X(NN), Y(NN), Z(NN), stat=E_IO)
          read(data, fmt=*, iostat=E_IO) (X(i), Y(i), Z(i), i=1,NN) !get ascii array
        endif
        if(allocated(data)) deallocate(data)
      endif

    case(binary)

      rewind(unit=vtk(rf)%u, iostat=E_IO)
      E_IO = move(inside='UnstructuredGrid', to_find='Piece', repeat=np, cf=rf, buffer=s_buffer) ! find the 'np' piece
      call get_int(buffer=s_buffer, attrib='NumberOfPoints', val=NN, E_IO=E_IO)
      if(E_IO == 0) then
        call get_int(buffer=s_buffer, attrib='NumberOfCells', val=NC, E_IO=E_IO)
        E_IO = search(inside='Points', to_find='DataArray', with_attribute='Name', of_value='Points', &
                      buffer=s_buffer,content=data)
        call get_char(buffer=s_buffer,  attrib='type', val=type, E_IO=E_IO)
        call get_char(buffer=s_buffer, attrib='format', val=fmt,  E_IO=E_IO)
        if (trim(adjustlt(Upper_Case(fmt)))/='BINARY' .or. &
            trim(adjustlt(Upper_Case(type)))/='FLOAT64') then
          E_IO = -1_I4P !stop 'Format not implemented'
        else
          ! Decode base64 packed data
          data=trim(adjustlt(data))
          allocate(dI1P(3*NN*int(BYR8P,I4P)+int(BYI4P,I4P)))
          call b64_decode(code=data,n=dI1P); if(allocated(data)) deallocate(data)
          ! Unpack data [1xI4P,3*NNxR8P]
          N_byte =  transfer(dI1P(1:int(BYI4P,I4P)),N_byte)
          s = size(transfer(dI1P(int(BYI4P,I4P)+1:),XYZp)); allocate(XYZp(1:s))
          XYZp = transfer(dI1P(int(BYI4P,I4P)+1:),XYZp); if(allocated(dI1P)) deallocate(dI1P)
          allocate(X(NN), Y(NN), Z(NN), stat=E_IO)
          do i=1,NN; X(i)=XYZp(i*3-2); Y(i)=XYZp(i*3-1); Z(i)=XYZp(i*3); enddo
          if(allocated(XYZp)) deallocate(XYZp)
        endif
      endif

    case(raw)

      rewind(unit=vtk(rf)%u, iostat=E_IO)
      E_IO = move(inside='UnstructuredGrid', to_find='Piece', repeat=np, cf=rf, buffer=s_buffer) ! find the 'np' piece
      inquire(unit=vtk(rf)%u, pos=pos, iostat=E_IO) !annotate the current position in the file
      call get_int(buffer=s_buffer, attrib='NumberOfPoints', val=NN, E_IO=E_IO)
      if(E_IO == 0) then
        call get_int(buffer=s_buffer, attrib='NumberOfCells', val=NC, E_IO=E_IO)
        E_IO = search(from=pos, inside='Points', to_find='DataArray', with_attribute='Name', of_value='Points', &
                      buffer=s_buffer)
        call get_int(buffer=s_buffer,  attrib='offset', val=offs, E_IO=E_IO)
        call get_char(buffer=s_buffer, attrib='format', val=fmt,  E_IO=E_IO)
        call get_char(buffer=s_buffer, attrib='type', val=type,  E_IO=E_IO)
        if (trim(adjustlt(Upper_Case(fmt)))/='APPENDED' .or. &
            trim(adjustlt(Upper_Case(type)))/='FLOAT64') then
          E_IO = -1_I4P
        else
          allocate(X(NN), Y(NN), Z(NN), stat=E_IO)
          read(unit=vtk(rf)%u, iostat=E_IO, pos = vtk(rf)%ioffset+offs) N_Byte, (X(i), Y(i), Z(i), i=1,NN) !get appended array
        endif
      endif
    end select
  !---------------------------------------------------------------------------------------------------------------------------------
  end function VTK_GEO_XML_UNST_R8_READ

  function VTK_GEO_XML_UNST_PACK_R8_READ(NN,NC,XYZ,npiece,cf) result(E_IO)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Function for reading mesh with \b UnstructuredGrid topology (R8P).
  !---------------------------------------------------------------------------------------------------------------------------------
  integer(I4P),           intent(OUT) :: NN       !< number of nodes
  integer(I4P),           intent(OUT) :: NC       !< number of cells
  real(R8P), allocatable, intent(OUT) :: XYZ(:,:)   !< Coordinates
  integer(I4P), optional, intent(IN)  :: npiece   !< Number of the piece to read (by default: 1)
  integer(I4P), optional, intent(IN)  :: cf       !< Current file index (for concurrent files IO).
  integer(I4P)                        :: E_IO     !< Input/Output inquiring flag: $0$ if IO is done, $> 0$ if IO is not done
  integer(I4P)                        :: rf       !< Real file index.
  character(len=:),allocatable        :: s_buffer !< Buffer string.
  character(len=:), allocatable       :: fmt
  character(len=:), allocatable       :: type
  character(len=:), allocatable       :: data
  integer(I4P)                        :: np, i, j, offs, N_Byte, pos, s
  integer(I1P), allocatable           :: dI1P(:)
  real(R8P), allocatable              :: XYZp(:)
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  rf = f
  if (present(cf)) then
    rf = cf ; f = cf
  endif
  np = 1_I4P; if (present(npiece)) np = npiece
  E_IO = -1_I4P
  select case(vtk(rf)%f)

    case(ascii)

      rewind(unit=vtk(rf)%u, iostat=E_IO)
      E_IO = move(inside='UnstructuredGrid', to_find='Piece', repeat=np, cf=rf, buffer=s_buffer) ! find the 'np' piece
      inquire(unit=vtk(rf)%u, pos=pos, iostat=E_IO) !annotate the current position in the file
      call get_int(buffer=s_buffer, attrib='NumberOfPoints', val=NN, E_IO=E_IO)
      if(E_IO == 0) then
        call get_int(buffer=s_buffer, attrib='NumberOfCells', val=NC, E_IO=E_IO)
        E_IO = search(from=pos, inside='Points', to_find='DataArray', with_attribute='Name', of_value='Points', &
                      buffer=s_buffer,content=data)
        call get_char(buffer=s_buffer, attrib='format', val=fmt,  E_IO=E_IO)
        call get_char(buffer=s_buffer, attrib='type', val=type,  E_IO=E_IO)
        if (trim(adjustlt(Upper_Case(fmt)))/='ASCII' .or. &
            trim(adjustlt(Upper_Case(type)))/='FLOAT64') then
          E_IO = -1_I4P
        else
          allocate(XYZ(3,NN), stat=E_IO)
          read(data, fmt=*, iostat=E_IO) ((XYZ(i,j),i=1,3),j=1,NN) !get ascii array
        endif
        if(allocated(data)) deallocate(data)
      endif

    case(binary)

      rewind(unit=vtk(rf)%u, iostat=E_IO)
      E_IO = move(inside='UnstructuredGrid', to_find='Piece', repeat=np, cf=rf, buffer=s_buffer) ! find the 'np' piece
      call get_int(buffer=s_buffer, attrib='NumberOfPoints', val=NN, E_IO=E_IO)
      if(E_IO == 0) then
        call get_int(buffer=s_buffer, attrib='NumberOfCells', val=NC, E_IO=E_IO)
        E_IO = search(inside='Points', to_find='DataArray', with_attribute='Name', of_value='Points', &
                      buffer=s_buffer,content=data)
        call get_char(buffer=s_buffer,  attrib='type', val=type, E_IO=E_IO)
        call get_char(buffer=s_buffer, attrib='format', val=fmt,  E_IO=E_IO)
        if (trim(adjustlt(Upper_Case(fmt)))/='BINARY' .or. &
            trim(adjustlt(Upper_Case(type)))/='FLOAT64') then
          E_IO = -1_I4P !stop 'Format not implemented'
        else
          ! Decode base64 packed data
          data=trim(adjustlt(data))
          allocate(dI1P(3*NN*int(BYR8P,I4P)+int(BYI4P,I4P)))
          call b64_decode(code=data,n=dI1P); if(allocated(data)) deallocate(data)
          ! Unpack data [1xI4P,3*NNxR8P]
          N_byte =  transfer(dI1P(1:int(BYI4P,I4P)),N_byte)
          s = size(transfer(dI1P(int(BYI4P,I4P)+1:),XYZp)); allocate(XYZp(1:s))
          XYZp = transfer(dI1P(int(BYI4P,I4P)+1:),XYZp); if(allocated(dI1P)) deallocate(dI1P)
          allocate(XYZ(3,NN), stat=E_IO)
          XYZ = reshape(XYZp,(/3,NN/))
          if(allocated(XYZp)) deallocate(XYZp)
        endif
      endif

    case(raw)

      rewind(unit=vtk(rf)%u, iostat=E_IO)
      E_IO = move(inside='UnstructuredGrid', to_find='Piece', repeat=np, cf=rf, buffer=s_buffer) ! find the 'np' piece
      inquire(unit=vtk(rf)%u, pos=pos, iostat=E_IO) !annotate the current position in the file
      call get_int(buffer=s_buffer, attrib='NumberOfPoints', val=NN, E_IO=E_IO)
      if(E_IO == 0) then
        call get_int(buffer=s_buffer, attrib='NumberOfCells', val=NC, E_IO=E_IO)
        E_IO = search(from=pos, inside='Points', to_find='DataArray', with_attribute='Name', of_value='Points', &
                      buffer=s_buffer)
        call get_int(buffer=s_buffer,  attrib='offset', val=offs, E_IO=E_IO)
        call get_char(buffer=s_buffer, attrib='format', val=fmt,  E_IO=E_IO)
        call get_char(buffer=s_buffer, attrib='type', val=type,  E_IO=E_IO)
        if (trim(adjustlt(Upper_Case(fmt)))/='APPENDED' .or. &
            trim(adjustlt(Upper_Case(type)))/='FLOAT64') then
          E_IO = -1_I4P
        else
          allocate(XYZ(3,NN), stat=E_IO)
          read(unit=vtk(rf)%u, iostat=E_IO, pos = vtk(rf)%ioffset+offs) N_Byte, ((XYZ(i,j),i=1,3),j=1,NN) !get appended array
        endif
      endif
    end select
  !---------------------------------------------------------------------------------------------------------------------------------
  end function VTK_GEO_XML_UNST_PACK_R8_READ

  function VTK_GEO_XML_UNST_R4_READ(NN,NC,X,Y,Z,npiece,cf) result(E_IO)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Function for reading mesh with \b UnstructuredGrid topology (R8P).
  !---------------------------------------------------------------------------------------------------------------------------------
  integer(I4P),           intent(OUT) :: NN       !< number of nodes
  integer(I4P),           intent(OUT) :: NC       !< number of cells
  real(R4P), allocatable, intent(OUT) :: X(:)     !< x coordinates
  real(R4P), allocatable, intent(OUT) :: Y(:)     !< y coordinates
  real(R4P), allocatable, intent(OUT) :: Z(:)     !< z coordinates
  integer(I4P), optional, intent(IN)  :: npiece   !< Number of the piece to read (by default: 1)
  integer(I4P), optional, intent(IN)  :: cf       !< Current file index (for concurrent files IO).
  integer(I4P)                        :: E_IO     !< Input/Output inquiring flag: $0$ if IO is done, $> 0$ if IO is not done
  integer(I4P)                        :: rf       !< Real file index.
  character(len=:),allocatable        :: s_buffer !< Buffer string.
  character(len=:), allocatable       :: fmt
  character(len=:), allocatable       :: type
  character(len=:), allocatable       :: data
  integer(I4P)                        :: np, i, offs, N_Byte, pos, s
  integer(I1P), allocatable           :: dI1P(:)
  real(R4P), allocatable              :: XYZp(:)
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  rf = f
  if (present(cf)) then
    rf = cf ; f = cf
  endif
  np = 1_I4P; if (present(npiece)) np = npiece
  E_IO = -1_I4P
  select case(vtk(rf)%f)

    case(ascii)

      rewind(unit=vtk(rf)%u, iostat=E_IO)
      E_IO = move(inside='UnstructuredGrid', to_find='Piece', repeat=np, cf=rf, buffer=s_buffer) ! find the 'np' piece
      inquire(unit=vtk(rf)%u, pos=pos, iostat=E_IO) !annotate the current position in the file
      call get_int(buffer=s_buffer, attrib='NumberOfPoints', val=NN, E_IO=E_IO)
      if(E_IO == 0) then
        call get_int(buffer=s_buffer, attrib='NumberOfCells', val=NC, E_IO=E_IO)
        E_IO = search(from=pos, inside='Points', to_find='DataArray', with_attribute='Name', of_value='Points', &
                      buffer=s_buffer,content=data)
        call get_char(buffer=s_buffer, attrib='format', val=fmt,  E_IO=E_IO)
        call get_char(buffer=s_buffer, attrib='type', val=type,  E_IO=E_IO)
        if (trim(adjustlt(Upper_Case(fmt)))/='ASCII' .or. &
            trim(adjustlt(Upper_Case(type)))/='FLOAT32') then
          E_IO = -1_I4P
        else
          allocate(X(NN), Y(NN), Z(NN), stat=E_IO)
          read(data, fmt=*, iostat=E_IO) (X(i), Y(i), Z(i), i=1,NN) !get ascii array
        endif
        if(allocated(data)) deallocate(data)
      endif

    case(binary)

      rewind(unit=vtk(rf)%u, iostat=E_IO)
      E_IO = move(inside='UnstructuredGrid', to_find='Piece', repeat=np, cf=rf, buffer=s_buffer) ! find the 'np' piece
      call get_int(buffer=s_buffer, attrib='NumberOfPoints', val=NN, E_IO=E_IO)
      if(E_IO == 0) then
        call get_int(buffer=s_buffer, attrib='NumberOfCells', val=NC, E_IO=E_IO)
        E_IO = search(inside='Points', to_find='DataArray', with_attribute='Name', of_value='Points', &
                      buffer=s_buffer,content=data)
        call get_char(buffer=s_buffer,  attrib='type', val=type, E_IO=E_IO)
        call get_char(buffer=s_buffer, attrib='format', val=fmt,  E_IO=E_IO)
        if (trim(adjustlt(Upper_Case(fmt)))/='BINARY' .or. &
            trim(adjustlt(Upper_Case(type)))/='FLOAT32') then
          E_IO = -1_I4P !stop 'Format not implemented'
        else
          ! Decode base64 packed data
          data=trim(adjustlt(data))
          allocate(dI1P(3*NN*int(BYR4P,I4P)+int(BYI4P,I4P)))
          call b64_decode(code=data,n=dI1P); if(allocated(data)) deallocate(data)
          ! Unpack data [1xI4P,3*NNxR4P]
          N_byte =  transfer(dI1P(1:int(BYI4P,I4P)),N_byte)
          s = size(transfer(dI1P(int(BYI4P,I4P)+1:),XYZp)); allocate(XYZp(1:s))
          XYZp = transfer(dI1P(int(BYI4P,I4P)+1:),XYZp); if(allocated(dI1P)) deallocate(dI1P)
          allocate(X(NN), Y(NN), Z(NN), stat=E_IO)
          do i=1,NN; X(i)=XYZp(i*3-2); Y(i)=XYZp(i*3-1); Z(i)=XYZp(i*3); enddo
          if(allocated(XYZp)) deallocate(XYZp)
        endif
      endif

    case(raw)

      rewind(unit=vtk(rf)%u, iostat=E_IO)
      E_IO = move(inside='UnstructuredGrid', to_find='Piece', repeat=np, cf=rf, buffer=s_buffer) ! find the 'np' piece
      inquire(unit=vtk(rf)%u, pos=pos, iostat=E_IO) !annotate the current position in the file
      call get_int(buffer=s_buffer, attrib='NumberOfPoints', val=NN, E_IO=E_IO)
      if(E_IO == 0) then
        call get_int(buffer=s_buffer, attrib='NumberOfCells', val=NC, E_IO=E_IO)
        E_IO = search(from=pos, inside='Points', to_find='DataArray', with_attribute='Name', of_value='Points', &
                      buffer=s_buffer)
        call get_int(buffer=s_buffer,  attrib='offset', val=offs, E_IO=E_IO)
        call get_char(buffer=s_buffer, attrib='format', val=fmt,  E_IO=E_IO)
        call get_char(buffer=s_buffer, attrib='type', val=type,  E_IO=E_IO)
        if (trim(adjustlt(Upper_Case(fmt)))/='APPENDED' .or. &
            trim(adjustlt(Upper_Case(type)))/='FLOAT32') then
          E_IO = -1_I4P
        else
          allocate(X(NN), Y(NN), Z(NN), stat=E_IO)
          read(unit=vtk(rf)%u, iostat=E_IO, pos = vtk(rf)%ioffset+offs) N_Byte, (X(i), Y(i), Z(i), i=1,NN) !get appended array
        endif
      endif
    end select
  !---------------------------------------------------------------------------------------------------------------------------------
  end function VTK_GEO_XML_UNST_R4_READ

  function VTK_GEO_XML_UNST_PACK_R4_READ(NN,NC,XYZ,npiece,cf) result(E_IO)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Function for reading mesh with \b UnstructuredGrid topology (R8P).
  !---------------------------------------------------------------------------------------------------------------------------------
  integer(I4P),           intent(OUT) :: NN       !< number of nodes
  integer(I4P),           intent(OUT) :: NC       !< number of cells
  real(R4P), allocatable, intent(OUT) :: XYZ(:,:)   !< Coordinates
  integer(I4P), optional, intent(IN)  :: npiece   !< Number of the piece to read (by default: 1)
  integer(I4P), optional, intent(IN)  :: cf       !< Current file index (for concurrent files IO).
  integer(I4P)                        :: E_IO     !< Input/Output inquiring flag: $0$ if IO is done, $> 0$ if IO is not done
  integer(I4P)                        :: rf       !< Real file index.
  character(len=:),allocatable        :: s_buffer !< Buffer string.
  character(len=:), allocatable       :: fmt
  character(len=:), allocatable       :: type
  character(len=:), allocatable       :: data
  integer(I4P)                        :: np, i, j, offs, N_Byte, pos, s
  integer(I1P), allocatable           :: dI1P(:)
  real(R4P), allocatable              :: XYZp(:)
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  rf = f
  if (present(cf)) then
    rf = cf ; f = cf
  endif
  np = 1_I4P; if (present(npiece)) np = npiece
  E_IO = -1_I4P
  select case(vtk(rf)%f)

    case(ascii)

      rewind(unit=vtk(rf)%u, iostat=E_IO)
      E_IO = move(inside='UnstructuredGrid', to_find='Piece', repeat=np, cf=rf, buffer=s_buffer) ! find the 'np' piece
      inquire(unit=vtk(rf)%u, pos=pos, iostat=E_IO) !annotate the current position in the file
      call get_int(buffer=s_buffer, attrib='NumberOfPoints', val=NN, E_IO=E_IO)
      if(E_IO == 0) then
        call get_int(buffer=s_buffer, attrib='NumberOfCells', val=NC, E_IO=E_IO)
        E_IO = search(from=pos, inside='Points', to_find='DataArray', with_attribute='Name', of_value='Points', &
                      buffer=s_buffer,content=data)
        call get_char(buffer=s_buffer, attrib='format', val=fmt,  E_IO=E_IO)
        call get_char(buffer=s_buffer, attrib='type', val=type,  E_IO=E_IO)
        if (trim(adjustlt(Upper_Case(fmt)))/='ASCII' .or. &
            trim(adjustlt(Upper_Case(type)))/='FLOAT32') then
          E_IO = -1_I4P
        else
          allocate(XYZ(3,NN), stat=E_IO)
          read(data, fmt=*, iostat=E_IO) ((XYZ(i,j),i=1,3),j=1,NN) !get ascii array
        endif
        if(allocated(data)) deallocate(data)
      endif

    case(binary)

      rewind(unit=vtk(rf)%u, iostat=E_IO)
      E_IO = move(inside='UnstructuredGrid', to_find='Piece', repeat=np, cf=rf, buffer=s_buffer) ! find the 'np' piece
      call get_int(buffer=s_buffer, attrib='NumberOfPoints', val=NN, E_IO=E_IO)
      if(E_IO == 0) then
        call get_int(buffer=s_buffer, attrib='NumberOfCells', val=NC, E_IO=E_IO)
        E_IO = search(inside='Points', to_find='DataArray', with_attribute='Name', of_value='Points', &
                      buffer=s_buffer,content=data)
        call get_char(buffer=s_buffer,  attrib='type', val=type, E_IO=E_IO)
        call get_char(buffer=s_buffer, attrib='format', val=fmt,  E_IO=E_IO)
        if (trim(adjustlt(Upper_Case(fmt)))/='BINARY' .or. &
            trim(adjustlt(Upper_Case(type)))/='FLOAT32') then
          E_IO = -1_I4P !stop 'Format not implemented'
        else
          ! Decode base64 packed data
          data=trim(adjustlt(data))
          allocate(dI1P(3*NN*int(BYR4P,I4P)+int(BYI4P,I4P)))
          call b64_decode(code=data,n=dI1P); if(allocated(data)) deallocate(data)
          ! Unpack data [1xI4P,3*NNxR4P]
          N_byte =  transfer(dI1P(1:int(BYI4P,I4P)),N_byte)
          s = size(transfer(dI1P(int(BYI4P,I4P)+1:),XYZp)); allocate(XYZp(1:s))
          XYZp = transfer(dI1P(int(BYI4P,I4P)+1:),XYZp); if(allocated(dI1P)) deallocate(dI1P)
          allocate(XYZ(3,NN), stat=E_IO)
          XYZ = reshape(XYZp,(/3,NN/))
          if(allocated(XYZp)) deallocate(XYZp)
        endif
      endif

    case(raw)

      rewind(unit=vtk(rf)%u, iostat=E_IO)
      E_IO = move(inside='UnstructuredGrid', to_find='Piece', repeat=np, cf=rf, buffer=s_buffer) ! find the 'np' piece
      inquire(unit=vtk(rf)%u, pos=pos, iostat=E_IO) !annotate the current position in the file
      call get_int(buffer=s_buffer, attrib='NumberOfPoints', val=NN, E_IO=E_IO)
      if(E_IO == 0) then
        call get_int(buffer=s_buffer, attrib='NumberOfCells', val=NC, E_IO=E_IO)
        E_IO = search(from=pos, inside='Points', to_find='DataArray', with_attribute='Name', of_value='Points', &
                      buffer=s_buffer)
        call get_int(buffer=s_buffer,  attrib='offset', val=offs, E_IO=E_IO)
        call get_char(buffer=s_buffer, attrib='format', val=fmt,  E_IO=E_IO)
        call get_char(buffer=s_buffer, attrib='type', val=type,  E_IO=E_IO)
        if (trim(adjustlt(Upper_Case(fmt)))/='APPENDED' .or. &
            trim(adjustlt(Upper_Case(type)))/='FLOAT32') then
          E_IO = -1_I4P
        else
          allocate(XYZ(3,NN), stat=E_IO)
          read(unit=vtk(rf)%u, iostat=E_IO, pos = vtk(rf)%ioffset+offs) N_Byte, ((XYZ(i,j),i=1,3),j=1,NN) !get appended array
        endif
      endif
    end select
  !---------------------------------------------------------------------------------------------------------------------------------
  end function VTK_GEO_XML_UNST_PACK_R4_READ
endmodule Lib_VTK_IO_GEO_XML
