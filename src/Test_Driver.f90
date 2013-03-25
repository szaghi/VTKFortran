Program Test_Driver
!-----------------------------------------------------------------------------------------------------------------------------------
USE IR_Precision
USE Lib_VTK_IO
!-----------------------------------------------------------------------------------------------------------------------------------

!-----------------------------------------------------------------------------------------------------------------------------------
implicit none
!! RectilinearGrid variables
!integer(I4P), parameter::          nx1  = 0_I4P
!integer(I4P), parameter::          nx2  = 1_I4P
!integer(I4P), parameter::          ny1  = 0_I4P
!integer(I4P), parameter::          ny2  = 2_I4P
!integer(I4P), parameter::          nz1  = 0_I4P
!integer(I4P), parameter::          nz2  = 3_I4P
!integer(I4P), parameter::          nxyz = (nx2-nx1+1)*(ny2-ny1+1)*(nz2-nz1+1)
!real(R8P),    dimension(nx1:nx2):: x_xml_rect = (/0._R8P,1._R8P/)
!real(R8P),    dimension(ny1:ny2):: y_xml_rect = (/0._R8P,1._R8P,2._R8P/)
!real(R8P),    dimension(nz1:nz2):: z_xml_rect = (/0._R8P,1._R8P,2._R8P,3._R8P/)
!integer(I4P), dimension(1:nxyz)::  var_xml_rect_point = (/0_I4P,1_I4P,7_I4P,5_I4P,2_I4P,6_I4P,9_I4P,0_I4P/)
! StructuredGrid variables
real(R8P),    parameter:: pi_R8=2._R8P*asin(1._R8P)
integer(I4P), parameter:: nx1_sgl=0_I4P
integer(I4P), parameter:: nx2_sgl=1_I4P
integer(I4P), parameter:: ny1_sgl=0_I4P
integer(I4P), parameter:: ny2_sgl=5_I4P
integer(I4P), parameter:: nz1_sgl=0_I4P
integer(I4P), parameter:: nz2_sgl=15_I4P
integer(I4P), parameter:: nn_sgl=(nx2_sgl-nx1_sgl+1)*(ny2_sgl-ny1_sgl+1)*(nz2_sgl-nz1_sgl+1)
real(R8P)::               x_sgl(nx1_sgl:nx2_sgl,ny1_sgl:ny2_sgl,nz1_sgl:nz2_sgl)
real(R8P)::               y_sgl(nx1_sgl:nx2_sgl,ny1_sgl:ny2_sgl,nz1_sgl:nz2_sgl)
real(R8P)::               z_sgl(nx1_sgl:nx2_sgl,ny1_sgl:ny2_sgl,nz1_sgl:nz2_sgl)
real(R8P)::               r_sgl(nx1_sgl:nx2_sgl,ny1_sgl:ny2_sgl,nz1_sgl:nz2_sgl)
real(R8P)::               a_sgl(nx1_sgl:nx2_sgl,ny1_sgl:ny2_sgl,nz1_sgl:nz2_sgl)
integer(I4P)::            var_sgl(nx1_sgl:nx2_sgl,ny1_sgl:ny2_sgl,nz1_sgl:nz2_sgl)
! UnstructuredGrid variables
integer(I4P), parameter::       Nn = 27_I4P
integer(I4P), parameter::       Ne = 11_I4P
real(R4P),    dimension(1:Nn):: x_uns = (/0,1,2,0,1,2,0,1,2,0,1,2,0,1,2,0,1,2,0,1,2,0,1,2,0,1,2/)
real(R4P),    dimension(1:Nn):: y_uns = (/0,0,0,1,1,1,0,0,0,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1/)
real(R4P),    dimension(1:Nn):: z_uns = (/0,0,0,0,0,0,1,1,1,1,1,1,2,2,2,3,3,3,4,4,4,5,5,5,6,6,6/)
integer(I1P), dimension(1:Ne):: cell_type = (/12,12,10,10,7,6,9,5,5,3,1/)
integer(I4P), dimension(1:Ne):: offset = (/8,16,20,24,30,36,40,43,46,48,49/)
integer(I4P), dimension(1:49):: connect
real(R8P),    dimension(1:Nn):: var_uns_grid
integer(I4P), dimension(1:Nn):: var_uns_grid_X
integer(I4P), dimension(1:Nn):: var_uns_grid_Y
integer(I4P), dimension(1:Nn):: var_uns_grid_Z
! Auxiliary variables
integer(I4P):: E_IO  ! IO error flag.
integer(I4P):: i,j,k ! Counters.
!-----------------------------------------------------------------------------------------------------------------------------------

!-----------------------------------------------------------------------------------------------------------------------------------
! RectilinearGrid
!E_IO = VTK_INI_XML(output_format = 'Binary',              &
!                   filename      = 'XML_RECT_BINARY.vtr', &
!                   mesh_topology = 'RectilinearGrid',     &
!                   nx1=nx1,nx2=nx2,ny1=ny1,ny2=ny2,nz1=nz1,nz2=nz2)
!E_IO = VTK_GEO_XML(nx1=nx1,nx2=nx2,ny1=ny1,ny2=ny2,nz1=nz1,nz2=nz2,X=x_xml_rect,Y=y_xml_rect,Z=z_xml_rect)
!E_IO = VTK_DAT_XML(var_location = 'node', var_block_action = 'OPEN')
!E_IO = VTK_VAR_XML(NC_NN=(nx2-nx1+1)*(ny2-ny1+1)*(nz2-nz1+1),varname='volume_scalars',var=var_xml_rect_point)
!E_IO = VTK_DAT_XML(var_location = 'node', var_block_action = 'Close')
!E_IO = VTK_GEO_XML()
!E_IO = VTK_END_XML()
! StructuredGrid
do i=nx1_sgl,nx2_sgl
 x_sgl(i,:,:)=i*100._R8P
enddo
do j=ny1_sgl,ny2_sgl
 r_sgl(:,j,:)=real(j,R8P)
enddo
do k=nz1_sgl,nz2_sgl
 a_sgl(:,:,k)=k*20._R8P/180*pi_R8
enddo
do k=nz1_sgl,nz2_sgl
 do j=ny1_sgl,ny2_sgl
   y_sgl(:,j,k)=r_sgl(:,j,k)*cos(a_sgl(:,j,k))
   z_sgl(:,j,k)=r_sgl(:,j,k)*sin(a_sgl(:,j,k))
 enddo
enddo
do k=nz1_sgl,nz2_sgl
 do j=ny1_sgl,ny2_sgl
   do i=nx1_sgl,nx2_sgl
     var_sgl(i,j,k)=j
   enddo
 enddo
enddo
E_IO = VTK_INI_XML(output_format = 'binary',              &
                   filename      = 'XML_STRG_BINARY.vts',  &
                   mesh_topology = 'StructuredGrid',      &
                   nx1 = nx1_sgl,nx2 = nx2_sgl, ny1 = ny1_sgl,ny2 = ny2_sgl, nz1 = nz1_sgl,nz2 = nz2_sgl)
E_IO = VTK_GEO_XML(nx1=nx1_sgl,nx2=nx2_sgl, ny1=ny1_sgl,ny2=ny2_sgl, nz1=nz1_sgl,nz2=nz2_sgl, NN=nn_sgl, &
                   X=reshape(x_sgl(:,:,:),(/nn_sgl/)), Y=reshape(y_sgl(:,:,:),(/nn_sgl/)), Z=reshape(z_sgl(:,:,:),(/nn_sgl/)))
E_IO = VTK_DAT_XML(var_location = 'node', var_block_action = 'OPEN')
E_IO = VTK_VAR_XML(NC_NN = nn_sgl, varname = 'node_value', var = reshape(var_sgl(:,:,:),(/nn_sgl/)))
E_IO = VTK_DAT_XML(var_location = 'node', var_block_action = 'Close')
E_IO = VTK_GEO_XML()
E_IO = VTK_END_XML()
! UnstructuredGrid
connect = (/0 ,1 ,4 ,3 ,6 ,7 ,10,9 , &
           1 ,2 ,5 ,4 ,7 ,8 ,11,10, &
           6 ,10,9 ,12,             &
           5 ,11,10,14,             &
           15,16,17,14,13,12,       &
           18,15,19,16,20,17,       &
           22,23,20,19,             &
           21,22,18,                &
           22,19,18,                &
           26,25,                   &
           24/)
var_uns_grid =(/0.0,1.0,2.0,3.0,4.0,5.0,6.0,7.0,8.0,9.0,10.0,11.0,12.0,13.0,14.0,15.0,16.0,17.0, &
               18.0,19.0,20.0,21.0,22.0,23.0,24.0,25.0,26.0/)
var_uns_grid_X=(/1,1,0,1,1,0,1,1,0,1,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0/)
var_uns_grid_Y=(/0,1,2,0,1,2,0,1,2,0,1,2,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0/)
var_uns_grid_Z=(/0,0,0,0,0,0,0,0,0,0,0,0,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1/)
E_IO = VTK_INI_XML(output_format = 'BINARY', filename = 'XML_UNST_BINARY.vtu', mesh_topology = 'UnstructuredGrid')
E_IO = VTK_GEO_XML(NN = Nn, NC = Ne, X = x_uns, Y = y_uns, Z = z_uns)
E_IO = VTK_CON_XML(NC = Ne, connect = connect, offset = offset, cell_type = cell_type )
E_IO = VTK_DAT_XML(var_location = 'node', var_block_action = 'opeN')
E_IO = VTK_VAR_XML(NC_NN = Nn, varname = 'scalars', var = var_uns_grid)
E_IO = VTK_VAR_XML(NC_NN = Nn, varname = 'vector', varX=var_uns_grid_X,varY=var_uns_grid_Y,varZ=var_uns_grid_Z)
E_IO = VTK_DAT_XML(var_location = 'node', var_block_action = 'CLOSE')
E_IO = VTK_GEO_XML()
E_IO = VTK_END_XML()
stop
!-----------------------------------------------------------------------------------------------------------------------------------
endprogram Test_Driver
