!< GEO interface definition for Lib_VTK_IO.
module Lib_VTK_IO_GEO
!-----------------------------------------------------------------------------------------------------------------------------------
!< GEO interface definition for Lib_VTK_IO.
!-----------------------------------------------------------------------------------------------------------------------------------
USE IR_Precision        ! Integers and reals precision definition.
USE Lib_VTK_IO_Back_End ! Lib_VTK_IO back end module.
!-----------------------------------------------------------------------------------------------------------------------------------

!-----------------------------------------------------------------------------------------------------------------------------------
implicit none
private
save
public:: VTK_GEO
!-----------------------------------------------------------------------------------------------------------------------------------

!-----------------------------------------------------------------------------------------------------------------------------------
interface VTK_GEO
  !< Procedure for saving mesh with different topologies in VTK-legacy standard.
  !<
  !< VTK_GEO is an interface to 16 different functions, there are 2 functions for each of 4 different topologies actually supported:
  !< one function for mesh coordinates with R8P precision and one for mesh coordinates with R4P precision.
  !<
  !< @note This function must be called after VTK_INI. It saves the mesh geometry. The inputs that must be passed change depending
  !< on the topologies chosen. Not all VTK topologies have been implemented (*polydata* topologies are absent).
  !<
  !<### Examples of usage
  !<
  !<#### Structured points calling
  !<```fortran
  !< integer(I4P):: Nx,Ny,Nz
  !< real(I8P)::    X0,Y0,Z0,Dx,Dy,Dz
  !< ...
  !< E_IO=VTK_GEO(Nx,Ny,Nz,X0,Y0,Z0,Dx,Dy,Dz)
  !< ...
  !<```
  !<
  !<#### Structured grid calling
  !<```fortran
  !< integer(I4P):: Nx,Ny,Nz,Nnodes
  !< real(R8P)::    X(1:Nnodes),Y(1:Nnodes),Z(1:Nnodes)
  !< ...
  !< E_IO=VTK_GEO(Nx,Ny,Nz,Nnodes,X,Y,Z)
  !< ...
  !<```
  !<
  !<#### Rectilinear grid calling
  !<```fortran
  !< integer(I4P):: Nx,Ny,Nz
  !< real(R8P)::    X(1:Nx),Y(1:Ny),Z(1:Nz)
  !< ...
  !< E_IO=VTK_GEO(Nx,Ny,Nz,X,Y,Z)
  !< ...
  !<```
  !<
  !<#### Unstructured grid calling
  !<```fortran
  !< integer(I4P):: NN
  !< real(R4P)::    X(1:NN),Y(1:NN),Z(1:NN)
  !< ...
  !< E_IO=VTK_GEO(NN,X,Y,Z)
  !< ...
  !<```
  module procedure VTK_GEO_UNST_R8,VTK_GEO_UNST_P_R8,         & ! real(R8P) UNSTRUCTURED_GRID, standard and packed API
                   VTK_GEO_UNST_R4,VTK_GEO_UNST_P_R4,         & ! real(R4P) UNSTRUCTURED_GRID, standard and packed API
                   VTK_GEO_STRP_R8,                           & ! real(R8P) STRUCTURED_POINTS
                   VTK_GEO_STRP_R4,                           & ! real(R4P) STRUCTURED_POINTS
                   VTK_GEO_STRG_1DA_R8, VTK_GEO_STRG_3DA_R8,  & ! real(R8P) STRUCTURED_GRID 1D/3D arrays
                   VTK_GEO_STRG_1DAP_R8,VTK_GEO_STRG_3DAP_R8, & ! real(R8P) STRUCTURED_GRID 1D/3D arrays, packed API
                   VTK_GEO_STRG_1DA_R4, VTK_GEO_STRG_3DA_R4,  & ! real(R4P) STRUCTURED_GRID 1D/3D arrays
                   VTK_GEO_STRG_1DAP_R4,VTK_GEO_STRG_3DAP_R4, & ! real(R4P) STRUCTURED_GRID 1D/3D arrays, packed API
                   VTK_GEO_RECT_R8,                           & ! real(R8P) RECTILINEAR_GRID
                   VTK_GEO_RECT_R4                              ! real(R4P) RECTILINEAR_GRID
endinterface
!-----------------------------------------------------------------------------------------------------------------------------------
contains
  function VTK_GEO_STRP_R8(Nx,Ny,Nz,X0,Y0,Z0,Dx,Dy,Dz,cf) result(E_IO)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Function for saving mesh with STRUCTURED_POINTS topology (R8P).
  !---------------------------------------------------------------------------------------------------------------------------------
  implicit none
  integer(I4P), intent(IN)::           Nx   !< Number of nodes in x direction.
  integer(I4P), intent(IN)::           Ny   !< Number of nodes in y direction.
  integer(I4P), intent(IN)::           Nz   !< Number of nodes in z direction.
  real(R8P),    intent(IN)::           X0   !< X coordinate of origin.
  real(R8P),    intent(IN)::           Y0   !< Y coordinate of origin.
  real(R8P),    intent(IN)::           Z0   !< Z coordinate of origin.
  real(R8P),    intent(IN)::           Dx   !< Space step in x direction.
  real(R8P),    intent(IN)::           Dy   !< Space step in y direction.
  real(R8P),    intent(IN)::           Dz   !< Space step in z direction.
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
  select case(vtk(rf)%f)
  case(ascii)
    write(vtk(rf)%u,'(A)',iostat=E_IO)'DIMENSIONS '//trim(str(.true.,Nx))//' '//trim(str(.true.,Ny))//' '//trim(str(.true.,Nz))
    write(vtk(rf)%u,'(A)',iostat=E_IO)'ORIGIN '//trim(str(n=X0))//' '//trim(str(n=Y0))//' '//trim(str(n=Z0))
    write(vtk(rf)%u,'(A)',iostat=E_IO)'SPACING '//trim(str(n=Dx))//' '//trim(str(n=Dy))//' '//trim(str(n=Dz))
  case(raw)
    write(vtk(rf)%u,iostat=E_IO)'DIMENSIONS '//trim(str(.true.,Nx))//' '//trim(str(.true.,Ny))//' '//trim(str(.true.,Nz))//end_rec
    write(vtk(rf)%u,iostat=E_IO)'ORIGIN '//trim(str(n=X0))//' '//trim(str(n=Y0))//' '//trim(str(n=Z0))//end_rec
    write(vtk(rf)%u,iostat=E_IO)'SPACING '//trim(str(n=Dx))//' '//trim(str(n=Dy))//' '//trim(str(n=Dz))//end_rec
  endselect
  return
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction VTK_GEO_STRP_R8

  function VTK_GEO_STRP_R4(Nx,Ny,Nz,X0,Y0,Z0,Dx,Dy,Dz,cf) result(E_IO)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Function for saving mesh with STRUCTURED_POINTS topology (R4P).
  !---------------------------------------------------------------------------------------------------------------------------------
  implicit none
  integer(I4P), intent(IN)::           Nx   !< Number of nodes in x direction.
  integer(I4P), intent(IN)::           Ny   !< Number of nodes in y direction.
  integer(I4P), intent(IN)::           Nz   !< Number of nodes in z direction.
  real(R4P),    intent(IN)::           X0   !< X coordinate of origin.
  real(R4P),    intent(IN)::           Y0   !< Y coordinate of origin.
  real(R4P),    intent(IN)::           Z0   !< Z coordinate of origin.
  real(R4P),    intent(IN)::           Dx   !< Space step in x direction.
  real(R4P),    intent(IN)::           Dy   !< Space step in y direction.
  real(R4P),    intent(IN)::           Dz   !< Space step in z direction.
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
  select case(vtk(rf)%f)
  case(ascii)
    write(vtk(rf)%u,'(A)',iostat=E_IO)'DIMENSIONS '//trim(str(.true.,Nx))//' '//trim(str(.true.,Ny))//' '//trim(str(.true.,Nz))
    write(vtk(rf)%u,'(A)',iostat=E_IO)'ORIGIN '//trim(str(n=X0))//' '//trim(str(n=Y0))//' '//trim(str(n=Z0))
    write(vtk(rf)%u,'(A)',iostat=E_IO)'SPACING '//trim(str(n=Dx))//' '//trim(str(n=Dy))//' '//trim(str(n=Dz))
  case(raw)
    write(vtk(rf)%u,iostat=E_IO)'DIMENSIONS '//trim(str(.true.,Nx))//' '//trim(str(.true.,Ny))//' '//trim(str(.true.,Nz))//end_rec
    write(vtk(rf)%u,iostat=E_IO)'ORIGIN '//trim(str(n=X0))//' '//trim(str(n=Y0))//' '//trim(str(n=Z0))//end_rec
    write(vtk(rf)%u,iostat=E_IO)'SPACING '//trim(str(n=Dx))//' '//trim(str(n=Dy))//' '//trim(str(n=Dz))//end_rec
  endselect
  return
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction VTK_GEO_STRP_R4

  function VTK_GEO_STRG_1DA_R8(Nx,Ny,Nz,NN,X,Y,Z,cf) result(E_IO)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Function for saving mesh with STRUCTURED_GRID topology (R8P, 1D arrays).
  !---------------------------------------------------------------------------------------------------------------------------------
  implicit none
  integer(I4P), intent(IN)::           Nx       !< Number of nodes in x direction.
  integer(I4P), intent(IN)::           Ny       !< Number of nodes in y direction.
  integer(I4P), intent(IN)::           Nz       !< Number of nodes in z direction.
  integer(I4P), intent(IN)::           NN       !< Number of all nodes.
  real(R8P),    intent(IN)::           X(1:)    !< X coordinates [1:NN].
  real(R8P),    intent(IN)::           Y(1:)    !< Y coordinates [1:NN].
  real(R8P),    intent(IN)::           Z(1:)    !< Z coordinates [1:NN].
  integer(I4P), intent(IN), optional:: cf       !< Current file index (for concurrent files IO).
  integer(I4P)::                       E_IO     !< Input/Output inquiring flag: $0$ if IO is done, $> 0$ if IO is not done.
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
    write(vtk(rf)%u,'(A)',iostat=E_IO)'DIMENSIONS '//trim(str(.true.,Nx))//' '//trim(str(.true.,Ny))//' '//trim(str(.true.,Nz))
    write(vtk(rf)%u,'(A)',iostat=E_IO)'POINTS '//trim(str(.true.,NN))//' double'
    do n1=1,NN
      write(vtk(rf)%u,'(A)',iostat=E_IO)str(n=X(n1))//' '//str(n=Y(n1))//' '//str(n=Z(n1))
    enddo
  case(raw)
    write(vtk(rf)%u,iostat=E_IO)'DIMENSIONS '//trim(str(.true.,Nx))//' '//trim(str(.true.,Ny))//' '//trim(str(.true.,Nz))//end_rec
    write(vtk(rf)%u,iostat=E_IO)'POINTS '//trim(str(.true.,NN))//' double'//end_rec
    write(vtk(rf)%u,iostat=E_IO)(X(n1),Y(n1),Z(n1),n1=1,NN)
    write(vtk(rf)%u,iostat=E_IO)end_rec
  endselect
  return
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction VTK_GEO_STRG_1DA_R8

  function VTK_GEO_STRG_1DAP_R8(Nx,Ny,Nz,NN,XYZ,cf) result(E_IO)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Function for saving mesh with STRUCTURED_GRID topology (R8P, 1D arrays, packed API).
  !---------------------------------------------------------------------------------------------------------------------------------
  implicit none
  integer(I4P), intent(IN)::           Nx         !< Number of nodes in x direction.
  integer(I4P), intent(IN)::           Ny         !< Number of nodes in y direction.
  integer(I4P), intent(IN)::           Nz         !< Number of nodes in z direction.
  integer(I4P), intent(IN)::           NN         !< Number of all nodes.
  real(R8P),    intent(IN)::           XYZ(1:,1:) !< X, Y and Z coordinates [1:3,1:NN].
  integer(I4P), intent(IN), optional:: cf         !< Current file index (for concurrent files IO).
  integer(I4P)::                       E_IO       !< Input/Output inquiring flag: $0$ if IO is done, $> 0$ if IO is not done.
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
    write(vtk(rf)%u,'(A)',iostat=E_IO)'DIMENSIONS '//trim(str(.true.,Nx))//' '//trim(str(.true.,Ny))//' '//trim(str(.true.,Nz))
    write(vtk(rf)%u,'(A)',iostat=E_IO)'POINTS '//trim(str(.true.,NN))//' double'
    do n1=1,NN
      write(vtk(rf)%u,'(A)',iostat=E_IO)str(n=XYZ(1,n1))//' '//str(n=XYZ(2,n1))//' '//str(n=XYZ(3,n1))
    enddo
  case(raw)
    write(vtk(rf)%u,iostat=E_IO)'DIMENSIONS '//trim(str(.true.,Nx))//' '//trim(str(.true.,Ny))//' '//trim(str(.true.,Nz))//end_rec
    write(vtk(rf)%u,iostat=E_IO)'POINTS '//trim(str(.true.,NN))//' double'//end_rec
    write(vtk(rf)%u,iostat=E_IO)XYZ
    write(vtk(rf)%u,iostat=E_IO)end_rec
  endselect
  return
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction VTK_GEO_STRG_1DAP_R8

  function VTK_GEO_STRG_3DA_R8(Nx,Ny,Nz,NN,X,Y,Z,cf) result(E_IO)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Function for saving mesh with STRUCTURED_GRID topology (R8P, 3D arrays).
  !---------------------------------------------------------------------------------------------------------------------------------
  implicit none
  integer(I4P), intent(IN)::           Nx          !< Number of nodes in x direction.
  integer(I4P), intent(IN)::           Ny          !< Number of nodes in y direction.
  integer(I4P), intent(IN)::           Nz          !< Number of nodes in z direction.
  integer(I4P), intent(IN)::           NN          !< Number of all nodes.
  real(R8P),    intent(IN)::           X(1:,1:,1:) !< X coordinates [1:Nx,1:Ny,1:Nz].
  real(R8P),    intent(IN)::           Y(1:,1:,1:) !< Y coordinates [1:Nx,1:Ny,1:Nz].
  real(R8P),    intent(IN)::           Z(1:,1:,1:) !< Z coordinates [1:Nx,1:Ny,1:Nz].
  integer(I4P), intent(IN), optional:: cf          !< Current file index (for concurrent files IO).
  integer(I4P)::                       E_IO        !< Input/Output inquiring flag: $0$ if IO is done, $> 0$ if IO is not done.
  integer(I4P)::                       rf          !< Real file index.
  integer(I4P)::                       n1,n2,n3    !< Counters.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  E_IO = -1_I4P
  rf = f
  if (present(cf)) then
    rf = cf ; f = cf
  endif
  select case(vtk(rf)%f)
  case(ascii)
    write(vtk(rf)%u,'(A)',iostat=E_IO)'DIMENSIONS '//trim(str(.true.,Nx))//' '//trim(str(.true.,Ny))//' '//trim(str(.true.,Nz))
    write(vtk(rf)%u,'(A)',iostat=E_IO)'POINTS '//trim(str(.true.,NN))//' double'
    do n3=1,Nz
      do n2=1,Ny
        do n1=1,Nx
          write(vtk(rf)%u,'(A)',iostat=E_IO)str(n=X(n1,n2,n3))//' '//str(n=Y(n1,n2,n3))//' '//str(n=Z(n1,n2,n3))
        enddo
      enddo
    enddo
  case(raw)
    write(vtk(rf)%u,iostat=E_IO)'DIMENSIONS '//trim(str(.true.,Nx))//' '//trim(str(.true.,Ny))//' '//trim(str(.true.,Nz))//end_rec
    write(vtk(rf)%u,iostat=E_IO)'POINTS '//trim(str(.true.,NN))//' double'//end_rec
    write(vtk(rf)%u,iostat=E_IO)(((X(n1,n2,n3),Y(n1,n2,n3),Z(n1,n2,n3),n1=1,Nx),n2=1,Ny),n3=1,Nz)
    write(vtk(rf)%u,iostat=E_IO)end_rec
  endselect
  return
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction VTK_GEO_STRG_3DA_R8

  function VTK_GEO_STRG_3DAP_R8(Nx,Ny,Nz,NN,XYZ,cf) result(E_IO)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Function for saving mesh with STRUCTURED_GRID topology (R8P, 3D arrays, packed API).
  !---------------------------------------------------------------------------------------------------------------------------------
  implicit none
  integer(I4P), intent(IN)::           Nx               !< Number of nodes in x direction.
  integer(I4P), intent(IN)::           Ny               !< Number of nodes in y direction.
  integer(I4P), intent(IN)::           Nz               !< Number of nodes in z direction.
  integer(I4P), intent(IN)::           NN               !< Number of all nodes.
  real(R8P),    intent(IN)::           XYZ(1:,1:,1:,1:) !< X, Y and Z coordinates [1:3,1:Nx,1:Ny,1:Nz].
  integer(I4P), intent(IN), optional:: cf               !< Current file index (for concurrent files IO).
  integer(I4P)::                       E_IO             !< Input/Output inquiring flag: $0$ if IO is done, $> 0$ if IO is not done.
  integer(I4P)::                       rf               !< Real file index.
  integer(I4P)::                       n1,n2,n3         !< Counters.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  E_IO = -1_I4P
  rf = f
  if (present(cf)) then
    rf = cf ; f = cf
  endif
  select case(vtk(rf)%f)
  case(ascii)
    write(vtk(rf)%u,'(A)',iostat=E_IO)'DIMENSIONS '//trim(str(.true.,Nx))//' '//trim(str(.true.,Ny))//' '//trim(str(.true.,Nz))
    write(vtk(rf)%u,'(A)',iostat=E_IO)'POINTS '//trim(str(.true.,NN))//' double'
    do n3=1,Nz
      do n2=1,Ny
        do n1=1,Nx
         write(vtk(rf)%u,'(A)',iostat=E_IO)str(n=XYZ(1,n1,n2,n3))//' '//str(n=XYZ(2,n1,n2,n3))//' '//str(n=XYZ(3,n1,n2,n3))
        enddo
      enddo
    enddo
  case(raw)
    write(vtk(rf)%u,iostat=E_IO)'DIMENSIONS '//trim(str(.true.,Nx))//' '//trim(str(.true.,Ny))//' '//trim(str(.true.,Nz))//end_rec
    write(vtk(rf)%u,iostat=E_IO)'POINTS '//trim(str(.true.,NN))//' double'//end_rec
    write(vtk(rf)%u,iostat=E_IO)XYZ
    write(vtk(rf)%u,iostat=E_IO)end_rec
  endselect
  return
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction VTK_GEO_STRG_3DAP_R8

  function VTK_GEO_STRG_1DA_R4(Nx,Ny,Nz,NN,X,Y,Z,cf) result(E_IO)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Function for saving mesh with STRUCTURED_GRID topology (R4P, 1D arrays).
  !---------------------------------------------------------------------------------------------------------------------------------
  implicit none
  integer(I4P), intent(IN)::           Nx       !< Number of nodes in x direction.
  integer(I4P), intent(IN)::           Ny       !< Number of nodes in y direction.
  integer(I4P), intent(IN)::           Nz       !< Number of nodes in z direction.
  integer(I4P), intent(IN)::           NN       !< Number of all nodes.
  real(R4P),    intent(IN)::           X(1:)    !< X coordinates [1:NN].
  real(R4P),    intent(IN)::           Y(1:)    !< Y coordinates [1:NN].
  real(R4P),    intent(IN)::           Z(1:)    !< Z coordinates [1:NN].
  integer(I4P), intent(IN), optional:: cf       !< Current file index (for concurrent files IO).
  integer(I4P)::                       E_IO     !< Input/Output inquiring flag: $0$ if IO is done, $> 0$ if IO is not done.
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
    write(vtk(rf)%u,'(A)',iostat=E_IO)'DIMENSIONS '//trim(str(.true.,Nx))//' '//trim(str(.true.,Ny))//' '//trim(str(.true.,Nz))
    write(vtk(rf)%u,'(A)',iostat=E_IO)'POINTS '//trim(str(.true.,NN))//' float'
    do n1=1,NN
      write(vtk(rf)%u,'(A)',iostat=E_IO)str(n=X(n1))//' '//str(n=Y(n1))//' '//str(n=Z(n1))
    enddo
  case(raw)
    write(vtk(rf)%u,iostat=E_IO)'DIMENSIONS '//trim(str(.true.,Nx))//' '//trim(str(.true.,Ny))//' '//trim(str(.true.,Nz))//end_rec
    write(vtk(rf)%u,iostat=E_IO)'POINTS '//trim(str(.true.,NN))//' float'//end_rec
    write(vtk(rf)%u,iostat=E_IO)(X(n1),Y(n1),Z(n1),n1=1,NN)
    write(vtk(rf)%u,iostat=E_IO)end_rec
  endselect
  return
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction VTK_GEO_STRG_1DA_R4

  function VTK_GEO_STRG_1DAP_R4(Nx,Ny,Nz,NN,XYZ,cf) result(E_IO)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Function for saving mesh with STRUCTURED_GRID topology (R4P, 1D arrays, packed API).
  !---------------------------------------------------------------------------------------------------------------------------------
  implicit none
  integer(I4P), intent(IN)::           Nx         !< Number of nodes in x direction.
  integer(I4P), intent(IN)::           Ny         !< Number of nodes in y direction.
  integer(I4P), intent(IN)::           Nz         !< Number of nodes in z direction.
  integer(I4P), intent(IN)::           NN         !< Number of all nodes.
  real(R4P),    intent(IN)::           XYZ(1:,1:) !< X, Y and Z coordinates [1:3,1:NN].
  integer(I4P), intent(IN), optional:: cf         !< Current file index (for concurrent files IO).
  integer(I4P)::                       E_IO       !< Input/Output inquiring flag: $0$ if IO is done, $> 0$ if IO is not done.
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
    write(vtk(rf)%u,'(A)',iostat=E_IO)'DIMENSIONS '//trim(str(.true.,Nx))//' '//trim(str(.true.,Ny))//' '//trim(str(.true.,Nz))
    write(vtk(rf)%u,'(A)',iostat=E_IO)'POINTS '//trim(str(.true.,NN))//' float'
    do n1=1,NN
      write(vtk(rf)%u,'(A)',iostat=E_IO)str(n=XYZ(1,n1))//' '//str(n=XYZ(2,n1))//' '//str(n=XYZ(3,n1))
    enddo
  case(raw)
    write(vtk(rf)%u,iostat=E_IO)'DIMENSIONS '//trim(str(.true.,Nx))//' '//trim(str(.true.,Ny))//' '//trim(str(.true.,Nz))//end_rec
    write(vtk(rf)%u,iostat=E_IO)'POINTS '//trim(str(.true.,NN))//' float'//end_rec
    write(vtk(rf)%u,iostat=E_IO)XYZ
    write(vtk(rf)%u,iostat=E_IO)end_rec
  endselect
  return
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction VTK_GEO_STRG_1DAP_R4

  function VTK_GEO_STRG_3DA_R4(Nx,Ny,Nz,NN,X,Y,Z,cf) result(E_IO)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Function for saving mesh with STRUCTURED_GRID topology (R4P, 3D arrays).
  !---------------------------------------------------------------------------------------------------------------------------------
  implicit none
  integer(I4P), intent(IN)::           Nx          !< Number of nodes in x direction.
  integer(I4P), intent(IN)::           Ny          !< Number of nodes in y direction.
  integer(I4P), intent(IN)::           Nz          !< Number of nodes in z direction.
  integer(I4P), intent(IN)::           NN          !< Number of all nodes.
  real(R4P),    intent(IN)::           X(1:,1:,1:) !< X coordinates [1:Nx,1:Ny,1:Nz].
  real(R4P),    intent(IN)::           Y(1:,1:,1:) !< Y coordinates [1:Nx,1:Ny,1:Nz].
  real(R4P),    intent(IN)::           Z(1:,1:,1:) !< Z coordinates [1:Nx,1:Ny,1:Nz].
  integer(I4P), intent(IN), optional:: cf          !< Current file index (for concurrent files IO).
  integer(I4P)::                       E_IO        !< Input/Output inquiring flag: $0$ if IO is done, $> 0$ if IO is not done.
  integer(I4P)::                       rf          !< Real file index.
  integer(I4P)::                       n1,n2,n3    !< Counters.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  E_IO = -1_I4P
  rf = f
  if (present(cf)) then
    rf = cf ; f = cf
  endif
  select case(vtk(rf)%f)
  case(ascii)
    write(vtk(rf)%u,'(A)',iostat=E_IO)'DIMENSIONS '//trim(str(.true.,Nx))//' '//trim(str(.true.,Ny))//' '//trim(str(.true.,Nz))
    write(vtk(rf)%u,'(A)',iostat=E_IO)'POINTS '//trim(str(.true.,NN))//' float'
    do n3=1,Nz
      do n2=1,Ny
        do n1=1,Nx
          write(vtk(rf)%u,'(A)',iostat=E_IO)str(n=X(n1,n2,n3))//' '//str(n=Y(n1,n2,n3))//' '//str(n=Z(n1,n2,n3))
        enddo
      enddo
    enddo
  case(raw)
    write(vtk(rf)%u,iostat=E_IO)'DIMENSIONS '//trim(str(.true.,Nx))//' '//trim(str(.true.,Ny))//' '//trim(str(.true.,Nz))//end_rec
    write(vtk(rf)%u,iostat=E_IO)'POINTS '//trim(str(.true.,NN))//' float'//end_rec
    write(vtk(rf)%u,iostat=E_IO)(((X(n1,n2,n3),Y(n1,n2,n3),Z(n1,n2,n3),n1=1,Nx),n2=1,Ny),n3=1,Nz)
    write(vtk(rf)%u,iostat=E_IO)end_rec
  endselect
  return
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction VTK_GEO_STRG_3DA_R4

  function VTK_GEO_STRG_3DAP_R4(Nx,Ny,Nz,NN,XYZ,cf) result(E_IO)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Function for saving mesh with STRUCTURED_GRID topology (R4P, 3D arrays, packed API).
  !---------------------------------------------------------------------------------------------------------------------------------
  implicit none
  integer(I4P), intent(IN)::           Nx               !< Number of nodes in x direction.
  integer(I4P), intent(IN)::           Ny               !< Number of nodes in y direction.
  integer(I4P), intent(IN)::           Nz               !< Number of nodes in z direction.
  integer(I4P), intent(IN)::           NN               !< Number of all nodes.
  real(R4P),    intent(IN)::           XYZ(1:,1:,1:,1:) !< X, Y and Z coordinates [1:3,1:Nx,1:Ny,1:Nz].
  integer(I4P), intent(IN), optional:: cf               !< Current file index (for concurrent files IO).
  integer(I4P)::                       E_IO             !< Input/Output inquiring flag: $0$ if IO is done, $> 0$ if IO is not done.
  integer(I4P)::                       rf               !< Real file index.
  integer(I4P)::                       n1,n2,n3         !< Counters.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  E_IO = -1_I4P
  rf = f
  if (present(cf)) then
    rf = cf ; f = cf
  endif
  select case(vtk(rf)%f)
  case(ascii)
    write(vtk(rf)%u,'(A)',iostat=E_IO)'DIMENSIONS '//trim(str(.true.,Nx))//' '//trim(str(.true.,Ny))//' '//trim(str(.true.,Nz))
    write(vtk(rf)%u,'(A)',iostat=E_IO)'POINTS '//trim(str(.true.,NN))//' float'
    do n3=1,Nz
      do n2=1,Ny
        do n1=1,Nx
         write(vtk(rf)%u,'(A)',iostat=E_IO)str(n=XYZ(1,n1,n2,n3))//' '//str(n=XYZ(2,n1,n2,n3))//' '//str(n=XYZ(3,n1,n2,n3))
        enddo
      enddo
    enddo
  case(raw)
    write(vtk(rf)%u,iostat=E_IO)'DIMENSIONS '//trim(str(.true.,Nx))//' '//trim(str(.true.,Ny))//' '//trim(str(.true.,Nz))//end_rec
    write(vtk(rf)%u,iostat=E_IO)'POINTS '//trim(str(.true.,NN))//' float'//end_rec
    write(vtk(rf)%u,iostat=E_IO)XYZ
    write(vtk(rf)%u,iostat=E_IO)end_rec
  endselect
  return
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction VTK_GEO_STRG_3DAP_R4

  function VTK_GEO_RECT_R8(Nx,Ny,Nz,X,Y,Z,cf) result(E_IO)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Function for saving mesh with RECTILINEAR_GRID topology (R8P).
  !---------------------------------------------------------------------------------------------------------------------------------
  implicit none
  integer(I4P), intent(IN)::           Nx       !< Number of nodes in x direction.
  integer(I4P), intent(IN)::           Ny       !< Number of nodes in y direction.
  integer(I4P), intent(IN)::           Nz       !< Number of nodes in z direction.
  real(R8P),    intent(IN)::           X(1:Nx)  !< X coordinates.
  real(R8P),    intent(IN)::           Y(1:Ny)  !< Y coordinates.
  real(R8P),    intent(IN)::           Z(1:Nz)  !< Z coordinates.
  integer(I4P), intent(IN), optional:: cf       !< Current file index (for concurrent files IO).
  integer(I4P)::                       E_IO     !< Input/Output inquiring flag: $0$ if IO is done, $> 0$ if IO is not done.
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
    write(vtk(rf)%u,'(A)',iostat=E_IO)'DIMENSIONS '//trim(str(.true.,Nx))//' '//trim(str(.true.,Ny))//' '//trim(str(.true.,Nz))
    write(vtk(rf)%u,'(A)',iostat=E_IO)'X_COORDINATES '//trim(str(.true.,Nx))//' double'
    do n1=1,Nx
      write(vtk(rf)%u,'(A)',iostat=E_IO)str(n=X(n1))
    enddo
    write(vtk(rf)%u,'(A)',iostat=E_IO)'Y_COORDINATES '//trim(str(.true.,Ny))//' double'
    do n1=1,Ny
      write(vtk(rf)%u,'(A)',iostat=E_IO)str(n=Y(n1))
    enddo
    write(vtk(rf)%u,'(A)',iostat=E_IO)'Z_COORDINATES '//trim(str(.true.,Nz))//' double'
    do n1=1,Nz
      write(vtk(rf)%u,'(A)',iostat=E_IO)str(n=Z(n1))
    enddo
  case(raw)
    write(vtk(rf)%u,iostat=E_IO)'DIMENSIONS '//trim(str(.true.,Nx))//' '//trim(str(.true.,Ny))//' '//trim(str(.true.,Nz))//end_rec
    write(vtk(rf)%u,iostat=E_IO)'X_COORDINATES '//trim(str(.true.,Nx))//' double'//end_rec
    write(vtk(rf)%u,iostat=E_IO)X
    write(vtk(rf)%u,iostat=E_IO)end_rec
    write(vtk(rf)%u,iostat=E_IO)'Y_COORDINATES '//trim(str(.true.,Ny))//' double'//end_rec
    write(vtk(rf)%u,iostat=E_IO)Y
    write(vtk(rf)%u,iostat=E_IO)end_rec
    write(vtk(rf)%u,iostat=E_IO)'Z_COORDINATES '//trim(str(.true.,Nz))//' double'//end_rec
    write(vtk(rf)%u,iostat=E_IO)Z
    write(vtk(rf)%u,iostat=E_IO)end_rec
  endselect
  return
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction VTK_GEO_RECT_R8

  function VTK_GEO_RECT_R4(Nx,Ny,Nz,X,Y,Z,cf) result(E_IO)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Function for saving mesh with RECTILINEAR_GRID topology (R4P).
  !---------------------------------------------------------------------------------------------------------------------------------
  implicit none
  integer(I4P), intent(IN)::           Nx       !< Number of nodes in x direction.
  integer(I4P), intent(IN)::           Ny       !< Number of nodes in y direction.
  integer(I4P), intent(IN)::           Nz       !< Number of nodes in z direction.
  real(R4P),    intent(IN)::           X(1:Nx)  !< X coordinates.
  real(R4P),    intent(IN)::           Y(1:Ny)  !< Y coordinates.
  real(R4P),    intent(IN)::           Z(1:Nz)  !< Z coordinates.
  integer(I4P), intent(IN), optional:: cf       !< Current file index (for concurrent files IO).
  integer(I4P)::                       E_IO     !< Input/Output inquiring flag: $0$ if IO is done, $> 0$ if IO is not done.
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
    write(vtk(rf)%u,'(A)',iostat=E_IO)'DIMENSIONS '//trim(str(.true.,Nx))//' '//trim(str(.true.,Ny))//' '//trim(str(.true.,Nz))
    write(vtk(rf)%u,'(A)',iostat=E_IO)'X_COORDINATES '//trim(str(.true.,Nx))//' float'
    do n1=1,Nx
      write(vtk(rf)%u,'(A)',iostat=E_IO)str(n=X(n1))
    enddo
    write(vtk(rf)%u,'(A)',iostat=E_IO)'Y_COORDINATES '//trim(str(.true.,Ny))//' float'
    do n1=1,Ny
      write(vtk(rf)%u,'(A)',iostat=E_IO)str(n=Y(n1))
    enddo
    write(vtk(rf)%u,'(A)',iostat=E_IO)'Z_COORDINATES '//trim(str(.true.,Nz))//' float'
    do n1=1,Nz
      write(vtk(rf)%u,'(A)',iostat=E_IO)str(n=Z(n1))
    enddo
  case(raw)
    write(vtk(rf)%u,iostat=E_IO)'DIMENSIONS '//trim(str(.true.,Nx))//' '//trim(str(.true.,Ny))//' '//trim(str(.true.,Nz))//end_rec
    write(vtk(rf)%u,iostat=E_IO)'X_COORDINATES '//trim(str(.true.,Nx))//' float'//end_rec
    write(vtk(rf)%u,iostat=E_IO)X
    write(vtk(rf)%u,iostat=E_IO)end_rec
    write(vtk(rf)%u,iostat=E_IO)'Y_COORDINATES '//trim(str(.true.,Ny))//' float'//end_rec
    write(vtk(rf)%u,iostat=E_IO)Y
    write(vtk(rf)%u,iostat=E_IO)end_rec
    write(vtk(rf)%u,iostat=E_IO)'Z_COORDINATES '//trim(str(.true.,Nz))//' float'//end_rec
    write(vtk(rf)%u,iostat=E_IO)Z
    write(vtk(rf)%u,iostat=E_IO)end_rec
  endselect
  return
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction VTK_GEO_RECT_R4

  function VTK_GEO_UNST_R8(NN,X,Y,Z,cf) result(E_IO)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Function for saving mesh with UNSTRUCTURED_GRID topology (R8P).
  !---------------------------------------------------------------------------------------------------------------------------------
  implicit none
  integer(I4P), intent(IN)::           NN       !< Number of nodes.
  real(R8P),    intent(IN)::           X(1:)    !< X coordinates of all nodes [1:NN].
  real(R8P),    intent(IN)::           Y(1:)    !< Y coordinates of all nodes [1:NN].
  real(R8P),    intent(IN)::           Z(1:)    !< Z coordinates of all nodes [1:NN].
  integer(I4P), intent(IN), optional:: cf       !< Current file index (for concurrent files IO).
  integer(I4P)::                       E_IO     !< Input/Output inquiring flag: $0$ if IO is done, $> 0$ if IO is not done.
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
    write(unit=vtk(rf)%u,fmt='(A)',iostat=E_IO)'POINTS '//str(.true.,NN)//' double'
    do n1=1,NN
      write(unit=vtk(rf)%u,fmt='(A)',iostat=E_IO)str(n=X(n1))//' '//str(n=Y(n1))//' '//str(n=Z(n1))
    enddo
  case(raw)
    write(unit=vtk(rf)%u,iostat=E_IO)'POINTS '//str(.true.,NN)//' double'//end_rec
    write(unit=vtk(rf)%u,iostat=E_IO)(X(n1),Y(n1),Z(n1),n1=1,NN)
    write(unit=vtk(rf)%u,iostat=E_IO)end_rec
  endselect
  return
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction VTK_GEO_UNST_R8

  function VTK_GEO_UNST_P_R8(NN,XYZ,cf) result(E_IO)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Function for saving mesh with UNSTRUCTURED_GRID topology (R8P, packed API).
  !---------------------------------------------------------------------------------------------------------------------------------
  implicit none
  integer(I4P), intent(IN)::           NN         !< Number of nodes.
  real(R8P),    intent(IN)::           XYZ(1:,1:) !< X, Y and Z coordinates of all nodes [1:3,1:NN].
  integer(I4P), intent(IN), optional:: cf         !< Current file index (for concurrent files IO).
  integer(I4P)::                       E_IO       !< Input/Output inquiring flag: $0$ if IO is done, $> 0$ if IO is not done.
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
    write(unit=vtk(rf)%u,fmt='(A)',iostat=E_IO)'POINTS '//str(.true.,NN)//' double'
    do n1=1,NN
      write(unit=vtk(rf)%u,fmt='(A)',iostat=E_IO)str(n=XYZ(1,n1))//' '//str(n=XYZ(2,n1))//' '//str(n=XYZ(3,n1))
    enddo
  case(raw)
    write(unit=vtk(rf)%u,iostat=E_IO)'POINTS '//str(.true.,NN)//' double'//end_rec
    write(unit=vtk(rf)%u,iostat=E_IO)XYZ
    write(unit=vtk(rf)%u,iostat=E_IO)end_rec
  endselect
  return
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction VTK_GEO_UNST_P_R8

  function VTK_GEO_UNST_R4(NN,X,Y,Z,cf) result(E_IO)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Function for saving mesh with UNSTRUCTURED_GRID topology (R4P).
  !---------------------------------------------------------------------------------------------------------------------------------
  implicit none
  integer(I4P), intent(IN)::           NN       !< number of nodes.
  real(R4P),    intent(IN)::           X(1:)    !< X coordinates of all nodes [1:NN].
  real(R4P),    intent(IN)::           Y(1:)    !< Y coordinates of all nodes [1:NN].
  real(R4P),    intent(IN)::           Z(1:)    !< Z coordinates of all nodes [1:NN].
  integer(I4P), intent(IN), optional:: cf       !< Current file index (for concurrent files IO).
  integer(I4P)::                       E_IO     !< Input/Output inquiring flag: $0$ if IO is done, $> 0$ if IO is not done.
  integer(I4P)::                       rf       !< Real file index.
  integer(I4P)::                       n1       !< counter.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  E_IO = -1_I4P
  rf = f
  if (present(cf)) then
    rf = cf ; f = cf
  endif
  select case(vtk(rf)%f)
  case(ascii)
    write(unit=vtk(rf)%u,fmt='(A)',iostat=E_IO)'POINTS '//str(.true.,NN)//' float'
    do n1=1,NN
      write(unit=vtk(rf)%u,fmt='(A)',iostat=E_IO)str(n=X(n1))//' '//str(n=Y(n1))//' '//str(n=Z(n1))
    enddo
  case(raw)
    write(unit=vtk(rf)%u,iostat=E_IO)'POINTS '//str(.true.,NN)//' float'//end_rec
    write(unit=vtk(rf)%u,iostat=E_IO)(X(n1),Y(n1),Z(n1),n1=1,NN)
    write(unit=vtk(rf)%u,iostat=E_IO)end_rec
  endselect
  return
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction VTK_GEO_UNST_R4

  function VTK_GEO_UNST_P_R4(NN,XYZ,cf) result(E_IO)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Function for saving mesh with UNSTRUCTURED_GRID topology (R4P, packed API).
  !---------------------------------------------------------------------------------------------------------------------------------
  implicit none
  integer(I4P), intent(IN)::           NN         !< number of nodes.
  real(R4P),    intent(IN)::           XYZ(1:,1:) !< X, Y and Z coordinates of all nodes [1:3,1:NN].
  integer(I4P), intent(IN), optional:: cf         !< Current file index (for concurrent files IO).
  integer(I4P)::                       E_IO       !< Input/Output inquiring flag: $0$ if IO is done, $> 0$ if IO is not done.
  integer(I4P)::                       rf         !< Real file index.
  integer(I4P)::                       n1         !< counter.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  E_IO = -1_I4P
  rf = f
  if (present(cf)) then
    rf = cf ; f = cf
  endif
  select case(vtk(rf)%f)
  case(ascii)
    write(unit=vtk(rf)%u,fmt='(A)',iostat=E_IO)'POINTS '//str(.true.,NN)//' float'
    do n1=1,NN
      write(unit=vtk(rf)%u,fmt='(A)',iostat=E_IO)str(n=XYZ(1,n1))//' '//str(n=XYZ(2,n1))//' '//str(n=XYZ(3,n1))
    enddo
  case(raw)
    write(unit=vtk(rf)%u,iostat=E_IO)'POINTS '//str(.true.,NN)//' float'//end_rec
    write(unit=vtk(rf)%u,iostat=E_IO)XYZ
    write(unit=vtk(rf)%u,iostat=E_IO)end_rec
  endselect
  return
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction VTK_GEO_UNST_P_R4
endmodule Lib_VTK_IO_GEO
