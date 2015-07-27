!< Testing program for Lib_VTK_IO, a pure Fortran (2003+) library to write and read data conforming the VTK standard
module Lib_Testers
!< Module library of procedures for testing Lib_VTK_IO and for providing practical examples.
!-----------------------------------------------------------------------------------------------------------------------------------
USE IR_Precision
USE Lib_VTK_IO
USE, intrinsic:: ISO_FORTRAN_ENV, only: stdout=>OUTPUT_UNIT, stderr=>ERROR_UNIT
!-----------------------------------------------------------------------------------------------------------------------------------

!-----------------------------------------------------------------------------------------------------------------------------------
implicit none
private
public:: test_stress
public:: test_rect
public:: test_unst
public:: test_strg
public:: test_punst
public:: test_pstrg
public:: test_vtm
#ifdef OPENMP
public:: test_openmp
#endif
#ifdef MPI2
public:: test_mpi
#endif
!-----------------------------------------------------------------------------------------------------------------------------------
contains
  subroutine test_stress()
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Procedure for testing all functions.
  !<
  !< R4P and R8P mesh data, 1D and 3D arrays inputs, standard (X,Y,Z,... separated arrays) and
  !< packed API (X,Y,Z,... packed into a single array). All available formats are used. The StructuredGrid topology is used.
  !< @note This subroutine is designed not as an example rather than a comprehensive stress-tester for functions of any kind/rank.
  !---------------------------------------------------------------------------------------------------------------------------------
  ! dataset dimensions
  integer(I4P), parameter:: nx1=0_I4P,nx2=9_I4P,ny1=0_I4P,ny2=5_I4P,nz1=0_I4P,nz2=5_I4P
  integer(I4P), parameter:: nn=(nx2-nx1+1)*(ny2-ny1+1)*(nz2-nz1+1)
  ! grid coordinates
  real(R8P),    dimension(    nx1:nx2,ny1:ny2,nz1:nz2):: x,y,z ! coordinates components
  real(R8P),    dimension(1:3,nx1:nx2,ny1:ny2,nz1:nz2):: xyz   ! packed coordinates components
  ! variables associated at grid nodes
  real(R8P),    dimension(    nx1:nx2,ny1:ny2,nz1:nz2):: v_R            ! scalar (real)
  integer(I8P), dimension(    nx1:nx2,ny1:ny2,nz1:nz2):: v_I            ! scalar (integer)
  real(R8P),    dimension(    nx1:nx2,ny1:ny2,nz1:nz2):: vX_R,vY_R,Vz_R ! 3 dimensional vector components (real)
  integer(I8P), dimension(    nx1:nx2,ny1:ny2,nz1:nz2):: vX_I,vY_I,Vz_I ! 3 dimensional vector components (integer)
  real(R8P),    dimension(1:4,nx1:nx2,ny1:ny2,nz1:nz2):: vP_R           ! packed 4 dimensional vector (real)
  integer(I8P), dimension(1:4,nx1:nx2,ny1:ny2,nz1:nz2):: vP_I           ! packed 4 dimensional vector (integer)
  ! auxiliary variables
  integer(I4P):: E_IO
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  write(stdout,'(A)')' Testing StructuredGrid functions. Output files are XML_STRG#.vts'
  ! arrays initialization
  call initialize
  ! testing 64 bits grid coordinates data, 1D rank arrays non packed, ASCII format
  E_IO = save_strg(x64=x,y64=y,z64=z,threeD=.false.,out_f='ASCII')
  ! testing 64 bits grid coordinates data, 1D rank arrays packed, ASCII format
  E_IO = save_strg(xyz64=xyz,threeD=.false.,out_f='ASCII')
  ! testing 64 bits grid coordinates data, 3D rank arrays non packed, ASCII format
  E_IO = save_strg(x64=x,y64=y,z64=z,threeD=.true.,out_f='ASCII')
  ! testing 64 bits grid coordinates data, 3D rank arrays packed, ASCII format
  E_IO = save_strg(xyz64=xyz,threeD=.true.,out_f='ASCII')
  ! testing 64 bits grid coordinates data, 1D rank arrays non packed, BINARY format
  E_IO = save_strg(x64=x,y64=y,z64=z,threeD=.false.,out_f='BINARY')
  ! testing 64 bits grid coordinates data, 1D rank arrays packed, BINARY format
  E_IO = save_strg(xyz64=xyz,threeD=.false.,out_f='BINARY')
  ! testing 64 bits grid coordinates data, 3D rank arrays non packed, ABINARYformat
  E_IO = save_strg(x64=x,y64=y,z64=z,threeD=.true.,out_f='BINARY')
  ! testing 64 bits grid coordinates data, 3D rank arrays packed, BINARY format
  E_IO = save_strg(xyz64=xyz,threeD=.true.,out_f='BINARY')
  ! testing 64 bits grid coordinates data, 1D rank arrays non packed, RAW format
  E_IO = save_strg(x64=x,y64=y,z64=z,threeD=.false.,out_f='RAW')
  ! testing 64 bits grid coordinates data, 1D rank arrays packed, RAW format
  E_IO = save_strg(xyz64=xyz,threeD=.false.,out_f='RAW')
  ! testing 64 bits grid coordinates data, 3D rank arrays non packed, RAW format
  E_IO = save_strg(x64=x,y64=y,z64=z,threeD=.true.,out_f='RAW')
  ! testing 64 bits grid coordinates data, 3D rank arrays packed, RAW format
  E_IO = save_strg(xyz64=xyz,threeD=.true.,out_f='RAW')
  ! testing 32 bits grid coordinates data, 1D rank arrays non packed, ASCII format
  E_IO = save_strg(x32=real(x, R4P),y32=real(y, R4P),z32=real(z, R4P),threeD=.false.,out_f='ASCII')
  ! testing 32 bits grid coordinates data, 1D rank arrays packed, ASCII format
  E_IO = save_strg(xyz32=real(xyz, R4P),threeD=.false.,out_f='ASCII')
  ! testing 32 bits grid coordinates data, 3D rank arrays non packed, ASCII format
  E_IO = save_strg(x32=real(x, R4P),y32=real(y, R4P),z32=real(z, R4P),threeD=.true.,out_f='ASCII')
  ! testing 32 bits grid coordinates data, 3D rank arrays packed, ASCII format
  E_IO = save_strg(xyz32=real(xyz, R4P),threeD=.true.,out_f='ASCII')
  ! testing 32 bits grid coordinates data, 1D rank arrays non packed, BINARY format
  E_IO = save_strg(x32=real(x, R4P),y32=real(y, R4P),z32=real(z, R4P),threeD=.false.,out_f='BINARY')
  ! testing 32 bits grid coordinates data, 1D rank arrays packed, BINARY format
  E_IO = save_strg(xyz32=real(xyz, R4P),threeD=.false.,out_f='BINARY')
  ! testing 32 bits grid coordinates data, 3D rank arrays non packed, ABINARYformat
  E_IO = save_strg(x32=real(x, R4P),y32=real(y, R4P),z32=real(z, R4P),threeD=.true.,out_f='BINARY')
  ! testing 32 bits grid coordinates data, 3D rank arrays packed, BINARY format
  E_IO = save_strg(xyz32=real(xyz, R4P),threeD=.true.,out_f='BINARY')
  ! testing 32 bits grid coordinates data, 1D rank arrays non packed, RAW format
  E_IO = save_strg(x32=real(x, R4P),y32=real(y, R4P),z32=real(z, R4P),threeD=.false.,out_f='RAW')
  ! testing 32 bits grid coordinates data, 1D rank arrays packed, RAW format
  E_IO = save_strg(xyz32=real(xyz, R4P),threeD=.false.,out_f='RAW')
  ! testing 32 bits grid coordinates data, 3D rank arrays non packed, RAW format
  E_IO = save_strg(x32=real(x, R4P),y32=real(y, R4P),z32=real(z, R4P),threeD=.true.,out_f='RAW')
  ! testing 32 bits grid coordinates data, 3D rank arrays packed, RAW format
  E_IO = save_strg(xyz32=real(xyz, R4P),threeD=.true.,out_f='RAW')
  return
  !---------------------------------------------------------------------------------------------------------------------------------
  contains
    subroutine initialize()
    !-------------------------------------------------------------------------------------------------------------------------------
    !< Procedure for initializing data.
    !-------------------------------------------------------------------------------------------------------------------------------
    implicit none
    integer(I4P):: i,j,k,p
    !-------------------------------------------------------------------------------------------------------------------------------

    !-------------------------------------------------------------------------------------------------------------------------------
    do k=nz1,nz2
      do j=ny1,ny2
        do i=nx1,nx2
          x(     i,j,k) = i*1._R8P ; xyz(1,i,j,k) = x(i,j,k)
          y(     i,j,k) = j*1._R8P ; xyz(2,i,j,k) = y(i,j,k)
          z(     i,j,k) = k*1._R8P ; xyz(3,i,j,k) = z(i,j,k)
          v_R(   i,j,k) = real(i*j*k, R8P)
          v_I(   i,j,k) = int( i*j*k, I8P)
          vX_R(  i,j,k) = x(i,j,k) ; vX_I(i,j,k) = int(x(i,j,k), I8P)
          vY_R(  i,j,k) = y(i,j,k) ; vY_I(i,j,k) = int(y(i,j,k), I8P)
          vZ_R(  i,j,k) = z(i,j,k) ; vZ_I(i,j,k) = int(z(i,j,k), I8P)
          vP_R(:,i,j,k) = [(x(i,j,k)*p,p=1,4)]
          vP_I(:,i,j,k) = int(vP_R(:,i,j,k), I8P)
        enddo
      enddo
    enddo
    return
    !-------------------------------------------------------------------------------------------------------------------------------
    endsubroutine initialize

    function save_node_variables(threeD) result(E_IO)
    !-------------------------------------------------------------------------------------------------------------------------------
    !< Procedure for saving StructuredGrid files.
    !-------------------------------------------------------------------------------------------------------------------------------
    implicit none
    logical, intent(IN):: threeD !< Flag for checking the rank-dimensions of outputs.
    integer(I4P)::        E_IO   !< Error trapping flag.
    !-------------------------------------------------------------------------------------------------------------------------------

    !-------------------------------------------------------------------------------------------------------------------------------
    E_IO = VTK_DAT_XML(var_location = 'node', var_block_action = 'open')
    if (threeD) then ! 3D rank arrays
      E_IO = VTK_VAR_XML(NC_NN=nn,varname='scal_R8',var=     v_R      ) ! scalar, R8P
      E_IO = VTK_VAR_XML(NC_NN=nn,varname='scal_R4',var=real(v_R, R4P)) ! scalar, R4P
      E_IO = VTK_VAR_XML(NC_NN=nn,varname='scal_I8',var=     v_I      ) ! scalar, I8P
      E_IO = VTK_VAR_XML(NC_NN=nn,varname='scal_I4',var= int(v_I, I4P)) ! scalar, I4P
      E_IO = VTK_VAR_XML(NC_NN=nn,varname='scal_I2',var= int(v_I, I2P)) ! scalar, I2P
      E_IO = VTK_VAR_XML(NC_NN=nn,varname='scal_I1',var= int(v_I, I1P)) ! scalar, I1P

      E_IO = VTK_VAR_XML(NC_NN=nn,varname='vect_R8',varX=     vX_R,     varY=     vY_R,     varZ=     vZ_R     ) ! vector, R8P
      E_IO = VTK_VAR_XML(NC_NN=nn,varname='vect_R4',varX=real(vX_R,R4P),varY=real(vY_R,R4P),varZ=real(vZ_R,R4P)) ! vector, R4P
      E_IO = VTK_VAR_XML(NC_NN=nn,varname='vect_I8',varX=     vX_I,     varY=     vY_I,     varZ=     vZ_I     ) ! vector, I8P
      E_IO = VTK_VAR_XML(NC_NN=nn,varname='vect_I4',varX= int(vX_I,I4P),varY= int(vY_I,I4P),varZ= int(vZ_I,I4P)) ! vector, I4P
      E_IO = VTK_VAR_XML(NC_NN=nn,varname='vect_I2',varX= int(vX_I,I2P),varY= int(vY_I,I2P),varZ= int(vZ_I,I2P)) ! vector, I2P
      E_IO = VTK_VAR_XML(NC_NN=nn,varname='vect_I1',varX= int(vX_I,I1P),varY= int(vY_I,I1P),varZ= int(vZ_I,I1P)) ! vector, I1P

      E_IO = VTK_VAR_XML(NC_NN=nn,N_COL=4_I4P,varname='vectP_R8',var=     vP_R     ) ! packed 4D vector, R8P
      E_IO = VTK_VAR_XML(NC_NN=nn,N_COL=4_I4P,varname='vectP_R4',var=real(vP_R,R4P)) ! packed 4D vector, R4P
      E_IO = VTK_VAR_XML(NC_NN=nn,N_COL=4_I4P,varname='vectP_I8',var=     vP_I     ) ! packed 4D vector, I8P
      E_IO = VTK_VAR_XML(NC_NN=nn,N_COL=4_I4P,varname='vectP_I4',var= int(vP_I,I4P)) ! packed 4D vector, I4P
      E_IO = VTK_VAR_XML(NC_NN=nn,N_COL=4_I4P,varname='vectP_I2',var= int(vP_I,I2P)) ! packed 4D vector, I2P
      E_IO = VTK_VAR_XML(NC_NN=nn,N_COL=4_I4P,varname='vectP_I1',var= int(vP_I,I1P)) ! packed 4D vector, I1P
    else ! 1D rank arrays
      E_IO = VTK_VAR_XML(NC_NN=nn,varname='scal_R8',var=     reshape(v_R,[nn])     ) ! scalar, R8P
      E_IO = VTK_VAR_XML(NC_NN=nn,varname='scal_R4',var=real(reshape(v_R,[nn]),R4P)) ! scalar, R4P
      E_IO = VTK_VAR_XML(NC_NN=nn,varname='scal_I8',var=     reshape(v_I,[nn])     ) ! scalar, I8P
      E_IO = VTK_VAR_XML(NC_NN=nn,varname='scal_I4',var= int(reshape(v_I,[nn]),I4P)) ! scalar, I4P
      E_IO = VTK_VAR_XML(NC_NN=nn,varname='scal_I2',var= int(reshape(v_I,[nn]),I2P)) ! scalar, I2P
      E_IO = VTK_VAR_XML(NC_NN=nn,varname='scal_I1',var= int(reshape(v_I,[nn]),I1P)) ! scalar, I1P

      E_IO = VTK_VAR_XML(NC_NN=nn,varname='vect_R8',varX=     reshape(vX_R,[nn]),     &
                                                    varY=     reshape(vY_R,[nn]),     &
                                                    varZ=     reshape(vZ_R,[nn])     ) ! vector, R8P
      E_IO = VTK_VAR_XML(NC_NN=nn,varname='vect_R4',varX=real(reshape(vX_R,[nn]),R4P),&
                                                    varY=real(reshape(vY_R,[nn]),R4P),&
                                                    varZ=real(reshape(vZ_R,[nn]),R4P)) ! vector, R4P
      E_IO = VTK_VAR_XML(NC_NN=nn,varname='vect_I8',varX=     reshape(vX_I,[nn]),     &
                                                    varY=     reshape(vY_I,[nn]),     &
                                                    varZ=     reshape(vZ_I,[nn])     ) ! vector, I8P
      E_IO = VTK_VAR_XML(NC_NN=nn,varname='vect_I4',varX= int(reshape(vX_I,[nn]),I4P),&
                                                    varY= int(reshape(vY_I,[nn]),I4P),&
                                                    varZ= int(reshape(vZ_I,[nn]),I4P)) ! vector, I4P
      E_IO = VTK_VAR_XML(NC_NN=nn,varname='vect_I2',varX= int(reshape(vX_I,[nn]),I2P),&
                                                    varY= int(reshape(vY_I,[nn]),I2P),&
                                                    varZ= int(reshape(vZ_I,[nn]),I2P)) ! vector, I2P
      E_IO = VTK_VAR_XML(NC_NN=nn,varname='vect_I1',varX= int(reshape(vX_I,[nn]),I1P),&
                                                    varY= int(reshape(vY_I,[nn]),I1P),&
                                                    varZ= int(reshape(vZ_I,[nn]),I1P)) ! vector, I1P

      E_IO = VTK_VAR_XML(NC_NN=nn,N_COL=4_I4P,varname='vectP_R8',var=     reshape(vP_R,[4,nn])     ) ! packed 4D vector, R8P
      E_IO = VTK_VAR_XML(NC_NN=nn,N_COL=4_I4P,varname='vectP_R4',var=real(reshape(vP_R,[4,nn]),R4P)) ! packed 4D vector, R4P
      E_IO = VTK_VAR_XML(NC_NN=nn,N_COL=4_I4P,varname='vectP_I8',var=     reshape(vP_I,[4,nn])     ) ! packed 4D vector, I8P
      E_IO = VTK_VAR_XML(NC_NN=nn,N_COL=4_I4P,varname='vectP_I4',var= int(reshape(vP_I,[4,nn]),I4P)) ! packed 4D vector, I4P
      E_IO = VTK_VAR_XML(NC_NN=nn,N_COL=4_I4P,varname='vectP_I2',var= int(reshape(vP_I,[4,nn]),I2P)) ! packed 4D vector, I2P
      E_IO = VTK_VAR_XML(NC_NN=nn,N_COL=4_I4P,varname='vectP_I1',var= int(reshape(vP_I,[4,nn]),I1P)) ! packed 4D vector, I1P
    endif
    E_IO = VTK_DAT_XML(var_location = 'node', var_block_action = 'close')
    return
    !-------------------------------------------------------------------------------------------------------------------------------
    endfunction save_node_variables

    function save_strg(x64,y64,z64,xyz64,x32,y32,z32,xyz32,threeD,out_f) result(E_IO)
    !-------------------------------------------------------------------------------------------------------------------------------
    !< Procedure for saving node-located variables.
    !-------------------------------------------------------------------------------------------------------------------------------
    real(R8P), optional, intent(IN) :: x64(:,:,:)     !< X Coordinates components (64 bits).
    real(R8P), optional, intent(IN) :: y64(:,:,:)     !< Y Coordinates components (64 bits).
    real(R8P), optional, intent(IN) :: z64(:,:,:)     !< Z Coordinates components (64 bits).
    real(R8P), optional, intent(IN) :: xyz64(:,:,:,:) !< Packed coordinates components (64 bits).
    real(R4P), optional, intent(IN) :: x32(:,:,:)     !< X Coordinates components (32 bits).
    real(R4P), optional, intent(IN) :: y32(:,:,:)     !< Y Coordinates components (32 bits).
    real(R4P), optional, intent(IN) :: z32(:,:,:)     !< Z Coordinates components (32 bits).
    real(R4P), optional, intent(IN) :: xyz32(:,:,:,:) !< Packed coordinates components (32 bits).
    logical,             intent(IN) :: threeD         !< Flag for checking the rank-dimensions of outputs.
    character(*),        intent(IN) :: out_f          !< Output format.
    integer(I4P)::                     E_IO           !< Error trapping flag.
    !-------------------------------------------------------------------------------------------------------------------------------

    !-------------------------------------------------------------------------------------------------------------------------------
    if (threeD) then
      if (present(x64)) then ! non packed, 3D rank array, 64 bits data
        E_IO = VTK_INI_XML_WRITE(fformat=trim(out_f), filename='XML_STRG-3DA-'//trim(out_f)//'-64.vts',&
                           mesh_topology='StructuredGrid', nx1=nx1, nx2=nx2, ny1=ny1, ny2=ny2, nz1=nz1, nz2=nz2)
        E_IO = VTK_GEO_XML_WRITE(nx1=nx1,nx2=nx2,ny1=ny1,ny2=ny2,nz1=nz1,nz2=nz2,NN=nn,X=x64,Y=y64,Z=z64)
      elseif (present(x32)) then ! non packed, 3D rank array, 32 bits data
        E_IO = VTK_INI_XML_WRITE(fformat=trim(out_f),filename='XML_STRG-3DA-'//trim(out_f)//'-32.vts',&
                           mesh_topology='StructuredGrid',nx1=nx1,nx2=nx2,ny1=ny1,ny2=ny2,nz1=nz1,nz2=nz2)
        E_IO = VTK_GEO_XML_WRITE(nx1=nx1,nx2=nx2,ny1=ny1,ny2=ny2,nz1=nz1,nz2=nz2,NN=nn,X=x32,Y=y32,Z=z32)
      elseif (present(xyz64)) then ! packed, 3D rank array, 64 bits data
        E_IO = VTK_INI_XML_WRITE(fformat=trim(out_f),filename='XML_STRG-3DAP-'//trim(out_f)//'-64.vts',&
                           mesh_topology='StructuredGrid',nx1=nx1,nx2=nx2,ny1=ny1,ny2=ny2,nz1=nz1,nz2=nz2)
        E_IO = VTK_GEO_XML_WRITE(nx1=nx1,nx2=nx2,ny1=ny1,ny2=ny2,nz1=nz1,nz2=nz2,NN=nn,XYZ=xyz64)
      elseif (present(xyz32)) then ! packed, 3D rank array, 32 bits data
        E_IO = VTK_INI_XML_WRITE(fformat=trim(out_f),filename='XML_STRG-3DAP-'//trim(out_f)//'-32.vts',&
                           mesh_topology='StructuredGrid',nx1=nx1,nx2=nx2,ny1=ny1,ny2=ny2,nz1=nz1,nz2=nz2)
        E_IO = VTK_GEO_XML_WRITE(nx1=nx1,nx2=nx2,ny1=ny1,ny2=ny2,nz1=nz1,nz2=nz2,NN=nn,XYZ=xyz32)
      endif
    else
      if (present(x64)) then ! non packed, 1D rank array, 64 bits data
        E_IO = VTK_INI_XML_WRITE(fformat=trim(out_f),filename='XML_STRG-1DA-'//trim(out_f)//'-64.vts',&
                           mesh_topology='StructuredGrid',nx1=nx1,nx2=nx2,ny1=ny1,ny2=ny2,nz1=nz1,nz2=nz2)
        E_IO = VTK_GEO_XML_WRITE(nx1=nx1,nx2=nx2,ny1=ny1,ny2=ny2,nz1=nz1,nz2=nz2,NN=nn,&
                           X=reshape(x64,[nn]),Y=reshape(y64,[nn]),Z=reshape(z64,[nn]))
      elseif (present(x32)) then ! non packed, 1D rank array, 32 bits data
        E_IO = VTK_INI_XML_WRITE(fformat=trim(out_f),filename='XML_STRG-1DA-'//trim(out_f)//'-32.vts',&
                           mesh_topology='StructuredGrid',nx1=nx1,nx2=nx2,ny1=ny1,ny2=ny2,nz1=nz1,nz2=nz2)
        E_IO = VTK_GEO_XML_WRITE(nx1=nx1,nx2=nx2,ny1=ny1,ny2=ny2,nz1=nz1,nz2=nz2,NN=nn,&
                           X=reshape(x32,[nn]),Y=reshape(y32,[nn]),Z=reshape(z32,[nn]))
      elseif (present(xyz64)) then ! packed, 1D rank array, 64 bits data
        E_IO = VTK_INI_XML_WRITE(fformat=trim(out_f),filename='XML_STRG-1DAP-'//trim(out_f)//'-64.vts',&
                           mesh_topology='StructuredGrid',nx1=nx1,nx2=nx2,ny1=ny1,ny2=ny2,nz1=nz1,nz2=nz2)
        E_IO = VTK_GEO_XML_WRITE(nx1=nx1,nx2=nx2,ny1=ny1,ny2=ny2,nz1=nz1,nz2=nz2,NN=nn,XYZ=reshape(xyz64,[3,nn]))
      elseif (present(xyz32)) then ! packed, 1D rank array, 32 bits data
        E_IO = VTK_INI_XML_WRITE(fformat=trim(out_f),filename='XML_STRG-1DAP-'//trim(out_f)//'-32.vts',&
                           mesh_topology='StructuredGrid',nx1=nx1,nx2=nx2,ny1=ny1,ny2=ny2,nz1=nz1,nz2=nz2)
        E_IO = VTK_GEO_XML_WRITE(nx1=nx1,nx2=nx2,ny1=ny1,ny2=ny2,nz1=nz1,nz2=nz2,NN=nn,XYZ=reshape(xyz32,[3,nn]))
      endif
    endif
    E_IO = save_node_variables(threeD=threeD)
    E_IO = VTK_GEO_XML_WRITE()
    E_IO = VTK_END_XML()
    return
    !-------------------------------------------------------------------------------------------------------------------------------
    endfunction save_strg
  endsubroutine test_stress

  subroutine test_unst()
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Procedure for testing UnstructuredGrid functions.
  !---------------------------------------------------------------------------------------------------------------------------------
  implicit none
  integer(I4P), parameter::       Nn = 27_I4P
  integer(I4P), parameter::       Ne = 11_I4P
  real(R4P),    dimension(1:Nn):: x = [0,1,2,0,1,2,0,1,2,0,1,2,0,1,2,0,1,2,0,1,2,0,1,2,0,1,2]
  real(R4P),    dimension(1:Nn):: y = [0,0,0,1,1,1,0,0,0,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1]
  real(R4P),    dimension(1:Nn):: z = [0,0,0,0,0,0,1,1,1,1,1,1,2,2,2,3,3,3,4,4,4,5,5,5,6,6,6]
  integer(I1P), dimension(1:Ne):: cell_type = [12_I1P,12_I1P,10_I1P,10_I1P,7_I1P,6_I1P,9_I1P,5_I1P,5_I1P,3_I1P,1_I1P]
  integer(I4P), dimension(1:Ne):: offset = [8_I4P,16_I4P,20_I4P,24_I4P,30_I4P,36_I4P,40_I4P,43_I4P,46_I4P,48_I4P,49_I4P]
  integer(I4P), dimension(1:49):: connect
  real(R8P),    dimension(1:Nn):: v
  integer(I4P), dimension(1:Nn):: v_X
  integer(I4P), dimension(1:Nn):: v_Y
  integer(I4P), dimension(1:Nn):: v_Z
  integer(I4P)::                  E_IO
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  write(stdout,'(A)')' Testing UnstructuredGrid functions. Output file is XML_UNST#.vtu'
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
  v=(/0.0,1.0,2.0,3.0,4.0,5.0,6.0,7.0,8.0,9.0,10.0,11.0,12.0,13.0,14.0,15.0,16.0,17.0, &
      18.0,19.0,20.0,21.0,22.0,23.0,24.0,25.0,26.0/)
  v_X=(/1,1,0,1,1,0,1,1,0,1,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0/)
  v_Y=(/0,1,2,0,1,2,0,1,2,0,1,2,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0/)
  v_Z=(/0,0,0,0,0,0,0,0,0,0,0,0,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1/)
  ! ascii
  E_IO = VTK_INI_XML_WRITE(fformat = 'ascii', filename = 'XML_UNST-ascii.vtu', mesh_topology = 'UnstructuredGrid')
  E_IO = VTK_GEO_XML_WRITE(NN = Nn, NC = Ne, X = x, Y = y, Z = z)
  E_IO = VTK_CON_XML(NC = Ne, connect = connect, offset = offset, cell_type = cell_type )
  E_IO = VTK_DAT_XML(var_location = 'node', var_block_action = 'opeN')
  E_IO = VTK_VAR_XML(NC_NN = Nn, varname = 'scalars', var = v)
  E_IO = VTK_VAR_XML(NC_NN = Nn, varname = 'vector', varX=v_X,varY=v_Y,varZ=v_Z)
  E_IO = VTK_DAT_XML(var_location = 'node', var_block_action = 'CLOSE')
  E_IO = VTK_GEO_XML_WRITE()
  E_IO = VTK_END_XML()
  ! raw
  E_IO = VTK_INI_XML_WRITE(fformat = 'raw', filename = 'XML_UNST-raw.vtu', mesh_topology = 'UnstructuredGrid')
  E_IO = VTK_GEO_XML_WRITE(NN = Nn, NC = Ne, X = x, Y = y, Z = z)
  E_IO = VTK_CON_XML(NC = Ne, connect = connect, offset = offset, cell_type = cell_type )
  E_IO = VTK_DAT_XML(var_location = 'node', var_block_action = 'opeN')
  E_IO = VTK_VAR_XML(NC_NN = Nn, varname = 'scalars', var = v)
  E_IO = VTK_VAR_XML(NC_NN = Nn, varname = 'vector', varX=v_X,varY=v_Y,varZ=v_Z)
  E_IO = VTK_DAT_XML(var_location = 'node', var_block_action = 'CLOSE')
  E_IO = VTK_GEO_XML_WRITE()
  E_IO = VTK_END_XML()
  ! binary
  E_IO = VTK_INI_XML_WRITE(fformat = 'binary', filename = 'XML_UNST-binary.vtu', mesh_topology = 'UnstructuredGrid')
  E_IO = VTK_GEO_XML_WRITE(NN = Nn, NC = Ne, X = x, Y = y, Z = z)
  E_IO = VTK_CON_XML(NC = Ne, connect = connect, offset = offset, cell_type = cell_type )
  E_IO = VTK_DAT_XML(var_location = 'node', var_block_action = 'opeN')
  E_IO = VTK_VAR_XML(NC_NN = Nn, varname = 'scalars', var = v)
  E_IO = VTK_VAR_XML(NC_NN = Nn, varname = 'vector', varX=v_X,varY=v_Y,varZ=v_Z)
  E_IO = VTK_DAT_XML(var_location = 'node', var_block_action = 'CLOSE')
  E_IO = VTK_GEO_XML_WRITE()
  E_IO = VTK_END_XML()
  return
  !---------------------------------------------------------------------------------------------------------------------------------
  endsubroutine test_unst

  subroutine test_strg()
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Procedure for testing StructuredGrid functions.
  !---------------------------------------------------------------------------------------------------------------------------------
  implicit none
  integer(I4P), parameter::                       nx1=0_I4P,nx2=9_I4P,ny1=0_I4P,ny2=5_I4P,nz1=0_I4P,nz2=5_I4P
  integer(I4P), parameter::                       nn=(nx2-nx1+1)*(ny2-ny1+1)*(nz2-nz1+1)
  real(R8P), dimension(nx1:nx2,ny1:ny2,nz1:nz2):: x,y,z
  real(R8P), dimension(nx1:nx2,ny1:ny2,nz1:nz2):: v_R
  integer(I4P)::                                  E_IO
  integer(I4P)::                                  i,j,k
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  write(stdout,'(A)')' Testing StructuredGrid functions. Output files is XML_STRG.vts'
  ! initializing data
  do k=nz1,nz2
    do j=ny1,ny2
      do i=nx1,nx2
        x(  i,j,k) = i*1._R8P
        y(  i,j,k) = j*1._R8P
        z(  i,j,k) = k*1._R8P
        v_R(i,j,k) = real(i*j*k,R8P)
      enddo
    enddo
  enddo
  E_IO = VTK_INI_XML_WRITE(fformat='binary',filename='XML_STRG.vts',mesh_topology='StructuredGrid',&
                     nx1=nx1, nx2=nx2, ny1=ny1, ny2=ny2, nz1=nz1, nz2=nz2)
  E_IO = VTK_GEO_XML_WRITE(nx1=nx1, nx2=nx2, ny1=ny1, ny2=ny2, nz1=nz1, nz2=nz2, NN=nn, X=x, Y=y, Z=z)
  E_IO = VTK_DAT_XML(var_location='node', var_block_action='open')
  E_IO = VTK_VAR_XML(NC_NN=nn, varname='scal_R8', var=v_R)
  E_IO = VTK_DAT_XML(var_location='node', var_block_action='close')
  E_IO = VTK_GEO_XML_WRITE()
  E_IO = VTK_END_XML()
  return
  !---------------------------------------------------------------------------------------------------------------------------------
  endsubroutine test_strg

  subroutine test_rect()
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Procedure for testing RectilinearGrid functions.
  !<
  !< @note This subroutine also shows the usage of FieldData functions that are useful for saving global auxiliary data, e.g. time,
  !< time step, ecc.
  !---------------------------------------------------------------------------------------------------------------------------------
  implicit none
  integer(I4P), parameter:: nx1=0_I4P,nx2=16_I4P,ny1=0_I4P,ny2=16_I4P,nz1=0_I4P,nz2=16_I4P
  integer(I4P), parameter:: nn=(nx2-nx1+1)*(ny2-ny1+1)*(nz2-nz1+1)
  real(R8P)::               x(nx1:nx2),y(ny1:ny2),z(nz1:nz2)
  real(R8P)::               x1=-3._R8P,x2=3._R8P
  real(R8P)::               y1=-2._R8P,y2=0.25_R8P
  real(R8P)::               z1=-2._R8P,z2=0.16_R8P
  integer(I4P)::            v(1:nn)
  integer(I4P)::            i,j,k,n,E_IO
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  write(stdout,'(A)')' Testing RectilinearGrid functions. Output file is XML_RECT#.vtr'
  ! arrays initialization
  n = 0
  do k=nz1,nz2
   do j=ny1,ny2
     do i=nx1,nx2
       n = n + 1
       v(n) = i*j*k
     enddo
   enddo
  enddo
  do i=nx1,nx2
    x(i) = x1 + (i - 1) * (x2 - x1)/real(nx2 - nx1, kind=R8P)
  enddo
  do j=ny1,ny2
    y(j) = y1 + (j - 1) * (y2 - y1)/real(ny2 - ny1, kind=R8P)
  enddo
  do k=nz1,nz2
    z(k) = z1 + (k - 1) * (z2 - z1)/real(nz2 - nz1, kind=R8P)
  enddo
  ! ascii
  E_IO = VTK_INI_XML_WRITE(fformat='ascii', filename='XML_RECT-ascii.vtr', &
                     mesh_topology='RectilinearGrid', nx1=nx1, nx2=nx2, ny1=ny1, ny2=ny2, nz1=nz1, nz2=nz2)
  E_IO = VTK_FLD_XML(fld_action='open')
  E_IO = VTK_FLD_XML(fld=0._R8P,fname='TIME')
  E_IO = VTK_FLD_XML(fld=1_I8P,fname='CYCLE')
  E_IO = VTK_FLD_XML(fld_action='close')
  E_IO = VTK_GEO_XML_WRITE(nx1=nx1, nx2=nx2, ny1=ny1, ny2=ny2, nz1=nz1, nz2=nz2, X=x, Y=y, Z=z)
  E_IO = VTK_DAT_XML(var_location = 'cell', var_block_action = 'open')
  E_IO = VTK_VAR_XML(NC_NN = nn, varname = 'cell_value', var = v)
  E_IO = VTK_DAT_XML(var_location = 'cell', var_block_action = 'close')
  E_IO = VTK_GEO_XML_WRITE()
  E_IO = VTK_END_XML()
  ! raw
  E_IO = VTK_INI_XML_WRITE(fformat='raw', filename='XML_RECT-raw.vtr', &
                     mesh_topology='RectilinearGrid', nx1=nx1, nx2=nx2, ny1=ny1, ny2=ny2, nz1=nz1, nz2=nz2)
  E_IO = VTK_FLD_XML(fld_action='open')
  E_IO = VTK_FLD_XML(fld=0._R8P,fname='TIME')
  E_IO = VTK_FLD_XML(fld=1_I8P,fname='CYCLE')
  E_IO = VTK_FLD_XML(fld_action='close')
  E_IO = VTK_GEO_XML_WRITE(nx1=nx1, nx2=nx2, ny1=ny1, ny2=ny2, nz1=nz1, nz2=nz2, X=x, Y=y, Z=z)
  E_IO = VTK_DAT_XML(var_location = 'cell', var_block_action = 'open')
  E_IO = VTK_VAR_XML(NC_NN = nn, varname = 'cell_value', var = v)
  E_IO = VTK_DAT_XML(var_location = 'cell', var_block_action = 'close')
  E_IO = VTK_GEO_XML_WRITE()
  E_IO = VTK_END_XML()
  ! binary
  E_IO = VTK_INI_XML_WRITE(fformat='binary', filename='XML_RECT-binary.vtr', &
                     mesh_topology='RectilinearGrid', nx1=nx1, nx2=nx2, ny1=ny1, ny2=ny2, nz1=nz1, nz2=nz2)
  E_IO = VTK_FLD_XML(fld_action='open')
  E_IO = VTK_FLD_XML(fld=0._R8P,fname='TIME')
  E_IO = VTK_FLD_XML(fld=1_I8P,fname='CYCLE')
  E_IO = VTK_FLD_XML(fld_action='close')
  E_IO = VTK_GEO_XML_WRITE(nx1=nx1, nx2=nx2, ny1=ny1, ny2=ny2, nz1=nz1, nz2=nz2, X=x, Y=y, Z=z)
  E_IO = VTK_DAT_XML(var_location = 'cell', var_block_action = 'open')
  E_IO = VTK_VAR_XML(NC_NN = nn, varname = 'cell_value', var = v)
  E_IO = VTK_DAT_XML(var_location = 'cell', var_block_action = 'close')
  E_IO = VTK_GEO_XML_WRITE()
  E_IO = VTK_END_XML()
  return
  !---------------------------------------------------------------------------------------------------------------------------------
  endsubroutine test_rect

  subroutine test_punst()
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Procedure for testing parallel (partitioned) PStructuredGrid functions.
  !<
  !< @note Note that the two parts are completely independet.
  !---------------------------------------------------------------------------------------------------------------------------------
  implicit none
  integer(I4P), parameter::       Nn = 27_I4P
  integer(I4P), parameter::       Ne = 11_I4P
  real(R4P),    dimension(1:Nn):: x = [0,1,2,0,1,2,0,1,2,0,1,2,0,1,2,0,1,2,0,1,2,0,1,2,0,1,2]
  real(R4P),    dimension(1:Nn):: y = [0,0,0,1,1,1,0,0,0,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1]
  real(R4P),    dimension(1:Nn):: z = [0,0,0,0,0,0,1,1,1,1,1,1,2,2,2,3,3,3,4,4,4,5,5,5,6,6,6]
  integer(I1P), dimension(1:Ne):: cell_type = [12_I1P,12_I1P,10_I1P,10_I1P,7_I1P,6_I1P,9_I1P,5_I1P,5_I1P,3_I1P,1_I1P]
  integer(I4P), dimension(1:Ne):: offset = [8_I4P,16_I4P,20_I4P,24_I4P,30_I4P,36_I4P,40_I4P,43_I4P,46_I4P,48_I4P,49_I4P]
  integer(I4P), dimension(1:49):: connect
  real(R8P),    dimension(1:Nn):: v
  integer(I4P), dimension(1:Nn):: v_X
  integer(I4P), dimension(1:Nn):: v_Y
  integer(I4P), dimension(1:Nn):: v_Z
  integer(I4P)::                  E_IO
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  write(stdout,'(A)')' Testing parallel (partitioned) PUnstructuredGrid functions'
  write(stdout,'(A)')' Output files are XML_UNST.pvtu, XML_UNST_part0.vtu, XML_UNST_part1.vtu'
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
  v=(/0.0,1.0,2.0,3.0,4.0,5.0,6.0,7.0,8.0,9.0,10.0,11.0,12.0,13.0,14.0,15.0,16.0,17.0, &
      18.0,19.0,20.0,21.0,22.0,23.0,24.0,25.0,26.0/)
  v_X=(/1,1,0,1,1,0,1,1,0,1,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0/)
  v_Y=(/0,1,2,0,1,2,0,1,2,0,1,2,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0/)
  v_Z=(/0,0,0,0,0,0,0,0,0,0,0,0,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1/)
  ! first part
  E_IO = VTK_INI_XML_WRITE(fformat = 'raw', filename = 'XML_UNST_part0.vtu', mesh_topology = 'UnstructuredGrid')
  E_IO = VTK_GEO_XML_WRITE(NN = Nn, NC = Ne, X = x, Y = y, Z = z)
  E_IO = VTK_CON_XML(NC = Ne, connect = connect, offset = offset, cell_type = cell_type )
  E_IO = VTK_DAT_XML(var_location = 'node', var_block_action = 'opeN')
  E_IO = VTK_VAR_XML(NC_NN = Nn, varname = 'scalars', var = v)
  E_IO = VTK_VAR_XML(NC_NN = Nn, varname = 'vector', varX=v_X,varY=v_Y,varZ=v_Z)
  E_IO = VTK_DAT_XML(var_location = 'node', var_block_action = 'CLOSE')
  E_IO = VTK_GEO_XML_WRITE()
  E_IO = VTK_END_XML()
  ! second part
  x = x + 10._R4P
  E_IO = VTK_INI_XML_WRITE(fformat = 'raw', filename = 'XML_UNST_part1.vtu', mesh_topology = 'UnstructuredGrid')
  E_IO = VTK_GEO_XML_WRITE(NN = Nn, NC = Ne, X = x, Y = y, Z = z)
  E_IO = VTK_CON_XML(NC = Ne, connect = connect, offset = offset, cell_type = cell_type )
  E_IO = VTK_DAT_XML(var_location = 'node', var_block_action = 'opeN')
  E_IO = VTK_VAR_XML(NC_NN = Nn, varname = 'scalars', var = v)
  E_IO = VTK_VAR_XML(NC_NN = Nn, varname = 'vector', varX=v_X,varY=v_Y,varZ=v_Z)
  E_IO = VTK_DAT_XML(var_location = 'node', var_block_action = 'CLOSE')
  E_IO = VTK_GEO_XML_WRITE()
  E_IO = VTK_END_XML()
  ! pvtu
  E_IO = PVTK_INI_XML(filename = 'XML_UNST.pvtu', mesh_topology = 'PUnstructuredGrid', tp='Float32')
  E_IO = PVTK_GEO_XML(source='XML_UNST_part0.vtu')
  E_IO = PVTK_GEO_XML(source='XML_UNST_part1.vtu')
  E_IO = PVTK_DAT_XML(var_location = 'node', var_block_action = 'OPEN')
  E_IO = PVTK_VAR_XML(varname = 'scalars', tp='Float64')
  E_IO = PVTK_VAR_XML(Nc = 3, varname = 'vector', tp='Int32' )
  E_IO = PVTK_DAT_XML(var_location = 'node', var_block_action = 'Close')
  E_IO = PVTK_END_XML()
  return
  !---------------------------------------------------------------------------------------------------------------------------------
  endsubroutine test_punst

  subroutine test_pstrg()
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Procedure for testing parallel (partitioned) PStructuredGrid functions.
  !<
  !< The mesh is a simple prism partitioned into two pieces along x direction at ordinate i=nx2_p(1).
  !<```
  !< y ^
  !<   |               ny2 +-----------------+--------------+
  !<   |                   |                 |              |
  !<   |                   |                 |              |
  !<   |                   |                 |              |
  !<   |                   |                 |              |
  !<   o-------->      ny1 +-----------------+--------------+
  !<            x         nx1               i=nx2_p(1)     nx2
  !<```
  !---------------------------------------------------------------------------------------------------------------------------------
  implicit none
  integer(I4P), parameter::                          nx1=0_I4P,nx2=9_I4P,ny1=0_I4P,ny2=5_I4P,nz1=0_I4P,nz2=5_I4P
  integer(I4P), parameter::                          nx1_p(1:2)=[nx1,4_I4P]
  integer(I4P), parameter::                          nx2_p(1:2)=[4_I4P,nx2]
  integer(I4P), parameter::                          nn       = (nx2     -nx1     +1)*(ny2-ny1+1)*(nz2-nz1+1)
  integer(I4P), parameter::                          nn_p(1:2)=[(nx2_p(1)-nx1     +1)*(ny2-ny1+1)*(nz2-nz1+1),&
                                                                (nx2_p(2)-nx2_p(1)+1)*(ny2-ny1+1)*(nz2-nz1+1)]
  real(R8P),    dimension(nx1:nx2,ny1:ny2,nz1:nz2):: x,y,z
  integer(I4P), dimension(nx1:nx2,ny1:ny2,nz1:nz2):: v
  integer(I4P)::                                     i,j,k,p,mf(1:2),E_IO
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  write(stdout,'(A)')' Testing parallel (partitioned) PStructuredGrid functions'
  write(stdout,'(A)')' Output files are XML_STRG.pvts, XML_STRG_part0.vts, XML_STRG_part1.vts'
  ! arrays initialization
  do k=nz1,nz2
   do j=ny1,ny2
     do i=nx1,nx2
       x(i,j,k) = i*1._R8P
       y(i,j,k) = j*1._R8P
       z(i,j,k) = k*1._R8P
       v(i,j,k) = i*j*k
     enddo
   enddo
  enddo
  do p=1,2 ! loop over pieces
    E_IO = VTK_INI_XML_WRITE(cf=mf(p),fformat='raw', filename='XML_STRG_part'//trim(str(.true.,p-1))//'.vts', &
                       mesh_topology='StructuredGrid', nx1=nx1_p(p), nx2=nx2_p(p), ny1=ny1, ny2=ny2, nz1=nz1, nz2=nz2)
    E_IO = VTK_GEO_XML_WRITE(cf=mf(p),nx1=nx1_p(p), nx2=nx2_p(p), ny1=ny1, ny2=ny2, nz1=nz1, nz2=nz2, NN=nn_p(p), &
                       X=reshape(x(nx1_p(p):nx2_p(p),:,:),(/nn_p(p)/)),                                     &
                       Y=reshape(y(nx1_p(p):nx2_p(p),:,:),(/nn_p(p)/)),                                     &
                       Z=reshape(z(nx1_p(p):nx2_p(p),:,:),(/nn_p(p)/)))
    E_IO = VTK_DAT_XML(cf=mf(p),var_location = 'node', var_block_action = 'open')
    E_IO = VTK_VAR_XML(cf=mf(p),NC_NN = nn_p(p), varname = 'node_value', var = reshape(v(nx1_p(p):nx2_p(p),:,:),(/nn_p(p)/)))
    E_IO = VTK_DAT_XML(cf=mf(p),var_location = 'node', var_block_action = 'close')
    E_IO = VTK_GEO_XML_WRITE(cf=mf(p))
    E_IO = VTK_END_XML()
  enddo
  ! pvts
  E_IO = PVTK_INI_XML(filename = 'XML_STRG.pvts', mesh_topology = 'PStructuredGrid', &
                      nx1=nx1,      nx2=nx2,      ny1=ny1, ny2=ny2, nz1=nz1, nz2=nz2, tp='Float64')
  E_IO = PVTK_GEO_XML(nx1=nx1,      nx2=nx2_p(1), ny1=ny1, ny2=ny2, nz1=nz1, nz2=nz2, source='XML_STRG_part0.vts')
  E_IO = PVTK_GEO_XML(nx1=nx2_p(1), nx2=nx2_p(2), ny1=ny1, ny2=ny2, nz1=nz1, nz2=nz2, source='XML_STRG_part1.vts')
  E_IO = PVTK_DAT_XML(var_location = 'node', var_block_action = 'open')
  E_IO = PVTK_VAR_XML(varname = 'node_value', tp='Int32')
  E_IO = PVTK_DAT_XML(var_location = 'node', var_block_action = 'close')
  E_IO = PVTK_END_XML()
  return
  !---------------------------------------------------------------------------------------------------------------------------------
  endsubroutine test_pstrg

  subroutine test_vtm()
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Procedure for testing multi-blocks VTM functions.
  !<
  !< There are 4 subset of data organized into 2 blocks. All the subsets are simple StructuredGrid prisms shifted along x direction.
  !---------------------------------------------------------------------------------------------------------------------------------
  implicit none
  integer(I4P), parameter::                          nx1=0_I4P,nx2=9_I4P,ny1=0_I4P,ny2=5_I4P,nz1=0_I4P,nz2=5_I4P
  integer(I4P), parameter::                          nn=(nx2-nx1+1)*(ny2-ny1+1)*(nz2-nz1+1)
  real(R8P),    dimension(nx1:nx2,ny1:ny2,nz1:nz2):: x,y,z
  integer(I4P), dimension(nx1:nx2,ny1:ny2,nz1:nz2):: v
  integer(I4P)::                                     i,j,k,b,mf(1:4),E_IO
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  write(stdout,'(A)')' Testing multi-blocks VTM functions'
  write(stdout,'(A)')' Output files are XML_M-STRG.vtm, XML_M-STRG_part.0.vts, XML_M-STRG_part.1.vts'
  ! arrays initialization
  do k=nz1,nz2
   do j=ny1,ny2
     do i=nx1,nx2
       x(i,j,k) = i*1._R8P
       y(i,j,k) = j*1._R8P
       z(i,j,k) = k*1._R8P
       v(i,j,k) = 1_I4P
     enddo
   enddo
  enddo
  ! vts
  do b=1,4 ! loop over blocks
    E_IO = VTK_INI_XML_WRITE(cf=mf(b), fformat='binary', filename='XML_M-STRG_part.'//trim(str(.true.,b-1))//'.vts', &
                       mesh_topology='StructuredGrid', nx1=nx1, nx2=nx2, ny1=ny1, ny2=ny2, nz1=nz1, nz2=nz2)
    if (b>1) then
      x = x + nx2*1._R8P
      v = b
    endif
    E_IO = VTK_GEO_XML_WRITE(cf=mf(b),nx1=nx1, nx2=nx2, ny1=ny1, ny2=ny2, nz1=nz1, nz2=nz2, NN=nn, &
                       X=reshape(x(nx1:nx2,:,:),(/nn/)),                                     &
                       Y=reshape(y(nx1:nx2,:,:),(/nn/)),                                     &
                       Z=reshape(z(nx1:nx2,:,:),(/nn/)))
    E_IO = VTK_DAT_XML(cf=mf(b),var_location = 'node', var_block_action = 'open')
    E_IO = VTK_VAR_XML(cf=mf(b),NC_NN = nn, varname = 'node_value', var = reshape(v(nx1:nx2,:,:),(/nn/)))
    E_IO = VTK_DAT_XML(cf=mf(b),var_location = 'node', var_block_action = 'close')
    E_IO = VTK_GEO_XML_WRITE(cf=mf(b))
    E_IO = VTK_END_XML()
  enddo
  ! vtm
  E_IO = VTM_INI_XML('XML_M-STRG.vtm')
  E_IO = VTM_BLK_XML(block_action='open')
  E_IO = VTM_WRF_XML(flist=(/('XML_M-STRG_part.'//trim(str(.true.,b-1))//'.vts',b=1,2)/))
  E_IO = VTM_BLK_XML(block_action='close')
  E_IO = VTM_BLK_XML(block_action='open')
  E_IO = VTM_WRF_XML(flist=(/('XML_M-STRG_part.'//trim(str(.true.,b-1))//'.vts',b=3,4)/))
  E_IO = VTM_BLK_XML(block_action='close')
  E_IO = VTM_END_XML()
  return
  !---------------------------------------------------------------------------------------------------------------------------------
  endsubroutine test_vtm

#ifdef OPENMP
  subroutine test_openmp(Nf_tot)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Procedure for testing the libray in an OpenMP parallel framework.
  !<
  !< It is used for testing thread-safe capability and the
  !< library speedup into OpenMP parallel framework. The output is a parallel (partitioned) PStructuredGrid file.
  !< @note The whole grid is composed of blocks of 32x32x32 structured mesh. The total number of blocks/files, Nf_tot, is passed as
  !< argument. The whole grid is built up composing the blocks along the X axis with a regular shift as following:
  !<```
  !< y ^
  !<   |               ny2 +------------+------------+------------+------///////////-----+
  !<   |                   |            |            |            |                      |
  !<   |                   |            |            |            |                      |
  !<   |                   |            |            |            |                      |
  !<   |                   |            |            |            |                      |
  !<   o-------->      ny1 +------------+------------+------------+------///////////-----+
  !<            x         nx1          nx2    2*(nx2-nx+1)  3*(nx2-nx+1)          Nf_tot*(nx2-nx+1)
  !<```
  !< @note When the total number of blocks/files, Nf_tot, is not an integral of the number of threads used, Nths, the last
  !< thread saves its own files (Nf_tot/Nths) plus the remainder blocks/files (mod(Nf_tot,Nths)). As a consequence the last
  !< thread could has different elapsed time and it could degrade the speedup. Therefore the subroutine prints to stdout the
  !< maximum and minimum elapsed time among the threads as well the average elapsed time in order to facilitate the assessing
  !< of the parallel scalability.
  !<
  !< @note It is important to note that the output files initialization and finalization must be done outside the parallel ambient.
  !<
  !< @note The array containing the files indexes could be shared among threads, but the counter of this array ('p' in this example)
  !< must be private.
  !---------------------------------------------------------------------------------------------------------------------------------
  USE omp_lib ! OpenMP runtime library.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  implicit none
  integer(I4P), intent(IN)::  Nf_tot                                             !< Total number of files saved.
  integer(I4P), parameter::   nx1=1_I4P                                          !< First node in x direction.
  integer(I4P), parameter::   nx2=32_I4P                                         !< Last node in x direction.
  integer(I4P), parameter::   ny1=1_I4P                                          !< First node in y direction.
  integer(I4P), parameter::   ny2=32_I4P                                         !< Last node in y direction.
  integer(I4P), parameter::   nz1=1_I4P                                          !< First node in z direction.
  integer(I4P), parameter::   nz2=32_I4P                                         !< Last node in z direction.
  integer(I4P), parameter::   nn=(nx2-nx1+1_I4P)*(ny2-ny1+1_I4P)*(nz2-nz1+1_I4P) !< Whole grid extents.
  real(R8P)::                 x( nx1:nx2,ny1:ny2,nz1:nz2)                        !< Coordinates in x direction.
  real(R8P)::                 y( nx1:nx2,ny1:ny2,nz1:nz2)                        !< Coordinates in y direction.
  real(R8P)::                 z( nx1:nx2,ny1:ny2,nz1:nz2)                        !< Coordinates in z direction.
  real(R8P)::                 xf(nx1:nx2,ny1:ny2,nz1:nz2)                        !< Coordinates in x shifted by file offset.
  integer(I4P), allocatable:: v(:,:,:,:)                                         !< Variable associated to nodes.
  integer(I4P)::              mf(1:Nf_tot)                                       !< File indexes.
  integer(I4P)::              Nths                                               !< Number of concurrent threads.
  integer(I4P)::              i,j,k,f,nxf,th                                     !< Counters.
  integer(I4P)::              E_IO                                               !< Error trapping flag.
  real(R8P)::                 vtk_start,vtk_stop                                 !< Timing variables.
  real(R8P), allocatable::    t(:)                                               !< Timing variables.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  !$OMP PARALLEL
  Nths = OMP_GET_NUM_THREADS()
  !$OMP END PARALLEL
  allocate(v(0:Nths-1,nx1:nx2,ny1:ny2,nz1:nz2))
  allocate(t(0:Nths-1))
  write(stdout,'(A)')' Testing OpenMP parallel framework'
  write(stdout,'(A)')' The test uses '//trim(str(.true.,Nf_tot))//' vts files of '//trim(str(.true.,nn))//' grid nodes as'//&
                     ' benchmark'
  write(stdout,'(A)')' Number of files saved by each thread is '//trim(str(.true.,Nf_tot/Nths))
  ! arrays initialization
  do k=nz1,nz2
    do j=ny1,ny2
      do i=nx1,nx2
        x(i,j,k) = real(i-nx1,R8P)/real(nx2-nx1,R8P)
        y(i,j,k) = real(j-ny1,R8P)/real(ny2-ny1,R8P)
        z(i,j,k) = real(k-nz1,R8P)/real(nz2-nz1,R8P)
        do f=0,Nths-1
          v(f,i,j,k) = f
        enddo
      enddo
    enddo
  enddo
  ! opening files outside OpenMP framework
  do f=1,Nf_tot
    nxf = (nx2-nx1+1)*f-(f-1)
    E_IO = VTK_INI_XML_WRITE(cf=mf(f),fformat='binary',filename='XML_OPENMP_f'//trim(strz(3,f))//'.vts', &
                       mesh_topology='StructuredGrid',nx1=nxf-(nx2-nx1+1)+1,nx2=nxf,&
                       ny1=ny1,ny2=ny2,nz1=nz1,nz2=nz2)
  enddo
  ! saving files
  t= 0._R8P
  !$OMP PARALLEL                                     &
  !$OMP DEFAULT(NONE)                                &
  !$OMP PRIVATE(f,E_IO,th,nxf,vtk_start,vtk_stop,xf) &
  !$OMP SHARED(mf,x,y,z,v,Nf_tot,t)
  th = OMP_GET_THREAD_NUM()
  !$OMP DO
  do f=1,Nf_tot ! loop over files
    nxf = (nx2-nx1+1)*f-(f-1)
    xf = x + real(f-1_I4P, R8P)
    vtk_start = OMP_GET_WTIME()
    E_IO = VTK_GEO_XML_WRITE(cf=mf(f),nx1=nxf-(nx2-nx1+1)+1,nx2=nxf,ny1=ny1,ny2=ny2,nz1=nz1,nz2=nz2,NN=nn,X=xf,Y=y,Z=z)
    E_IO = VTK_DAT_XML(cf=mf(f),var_location='node',var_block_action='open')
    E_IO = VTK_VAR_XML(cf=mf(f),NC_NN=nn,varname='node_value',var=v(th,:,:,:))
    E_IO = VTK_DAT_XML(cf=mf(f),var_location='node',var_block_action='close')
    E_IO = VTK_GEO_XML_WRITE(cf=mf(f))
    vtk_stop = OMP_GET_WTIME()
    t(th) = t(th) + vtk_stop-vtk_start
  enddo
  !$OMP END PARALLEL
  ! closing files outside OpenMP framework
  do f=1,Nf_tot
    E_IO = VTK_END_XML()
  enddo
  write(stdout,'(A)')' Maximum elapsed time of single thread '//trim(adjustl(str('(F9.4)',maxval(t))))
  write(stdout,'(A)')' Minimum elapsed time of single thread '//trim(adjustl(str('(F9.4)',minval(t))))
  write(stdout,'(A)')' Average elapsed time '//trim(adjustl(str('(F9.4)',sum(t)/Nths)))
  ! saving the composite .pvts file
  E_IO = PVTK_INI_XML(filename = 'XML_OPENMP.pvts', mesh_topology = 'PStructuredGrid',&
                      nx1=nx1, nx2=(nx2-nx1+1)*Nf_tot-(Nf_tot-1), ny1=ny1, ny2=ny2, nz1=nz1, nz2=nz2, tp='Float64')
  do f=1,Nf_tot
    nxf = (nx2-nx1+1)*f-(f-1)
    E_IO = PVTK_GEO_XML(nx1=nxf-(nx2-nx1+1)+1,nx2=nxf,ny1=ny1,ny2=ny2,nz1=nz1,nz2=nz2,&
                        source='XML_OPENMP_f'//trim(strz(3,f))//'.vts')
  enddo
  E_IO = PVTK_DAT_XML(var_location='node',var_block_action='open')
  E_IO = PVTK_VAR_XML(varname='node_value',tp='Int32')
  E_IO = PVTK_DAT_XML(var_location='node',var_block_action='close')
  E_IO = PVTK_END_XML()
  return
  !---------------------------------------------------------------------------------------------------------------------------------
  endsubroutine test_openmp
#endif

#ifdef MPI2
  subroutine test_mpi(Nf_tot)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Procedure for testing the library in an MPI parallel framework.
  !<
  !< It is used for testing the process-safe capability and the
  !< library speedup into MPI parallel framework.  The output is a parallel (partitioned) PStructuredGrid file.
  !< @note The whole grid is composed of blocks of 32x32x32 structured mesh. The total number of blocks/files, Nf_tot, is passed as
  !< argument. The whole grid is built up composing the blocks along the X axis with a regular shift as following:
  !<```
  !< y ^
  !<   |               ny2 +------------+------------+------------+------///////////-----+
  !<   |                   |            |            |            |                      |
  !<   |                   |            |            |            |                      |
  !<   |                   |            |            |            |                      |
  !<   |                   |            |            |            |                      |
  !<   o-------->      ny1 +------------+------------+------------+------///////////-----+
  !<            x         nx1          nx2    2*(nx2-nx+1)  3*(nx2-nx+1)          Nf_tot*(nx2-nx+1)
  !<```
  !< @note When the total number of blocks/files, Nf_tot, is not an integral of the number of processes used, nproc, the last
  !< process saves its own files (Nf_tot/nproc) plus the remainder blocks/files (mod(Nf_tot,nproc)). As a consequence the last
  !< process could has different elapsed time and it could degrade the speedup. Therefore the subroutine prints to stdout the
  !< maximum and minimum elapsed time among the processes as well the average elapsed time in order to facilitate the assessing
  !< of the parallel scalability.
  !---------------------------------------------------------------------------------------------------------------------------------
  USE MPI ! MPI runtime library.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  implicit none
  integer(I4P), intent(IN)::Nf_tot                                             !< Total number of files saved.
  integer(I4P), parameter:: nx1=1_I4P                                          !< First node in x direction.
  integer(I4P), parameter:: nx2=32_I4P                                         !< Last node in x direction.
  integer(I4P), parameter:: ny1=1_I4P                                          !< First node in y direction.
  integer(I4P), parameter:: ny2=32_I4P                                         !< Last node in y direction.
  integer(I4P), parameter:: nz1=1_I4P                                          !< First node in z direction.
  integer(I4P), parameter:: nz2=32_I4P                                         !< Last node in z direction.
  integer(I4P), parameter:: nn=(nx2-nx1+1_I4P)*(ny2-ny1+1_I4P)*(nz2-nz1+1_I4P) !< Whole grid extents.
  real(R8P)::               x(nx1:nx2,ny1:ny2,nz1:nz2)                         !< Coordinates in x direction.
  real(R8P)::               y(nx1:nx2,ny1:ny2,nz1:nz2)                         !< Coordinates in y direction.
  real(R8P)::               z(nx1:nx2,ny1:ny2,nz1:nz2)                         !< Coordinates in z direction.
  integer(I4P)::            v(nx1:nx2,ny1:ny2,nz1:nz2)                         !< Variable associated to nodes.
  real(R8P)::               xf(nx1:nx2,ny1:ny2,nz1:nz2)                        !< Coordinates in x shifted by file offset direction.
  integer(I4P)::            i,j,k,f,foffset,nxf                                !< Counters.
  integer(I4P)::            myrank                                             !< Rank of current process.
  integer(I4P)::            Nf                                                 !< Number of files saved by the current process.
  integer(I4P)::            nproc                                              !< Number of concurrent processes.
  integer(I4P)::            E_IO                                               !< Error trapping flag.
  real(R8P)::               vtk_start,vtk_stop,t,tmax,tmin,tmean               !< Timing variables.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  ! initializing parallel environments
  call MPI_INIT(E_IO)
  call MPI_COMM_RANK(MPI_COMM_WORLD,myrank,E_IO)
  call MPI_COMM_SIZE(MPI_COMM_WORLD,nproc,E_IO)
  ! computing the number of files to be saved
  if (myrank==nproc-1) then
    ! the last process must save also the rest of the files if Nf_tot is not an integral of nproc
    Nf = Nf_tot/nproc + mod(Nf_tot,nproc)
  else
    Nf = Nf_tot/nproc
  endif
  if (myrank==0) then
    write(stdout,'(A)')' Testing MPI parallel framework'
    write(stdout,'(A)')' The test uses '//trim(str(.true.,Nf_tot))//' vts files of '//trim(str(.true.,nn))//' grid nodes as'//&
                       ' benchmark'
    write(stdout,'(A)')' Number of files saved by processes other than the last is '//trim(str(.true.,Nf))
  elseif (myrank==nproc-1) then
    write(stdout,'(A)')' Number of files saved by last process is '//trim(str(.true.,Nf))
  endif
  ! arrays initialization
  do k=nz1,nz2
    do j=ny1,ny2
      do i=nx1,nx2
        x(i,j,k) = real(i-nx1,R8P)/real(nx2-nx1,R8P)
        y(i,j,k) = real(j-ny1,R8P)/real(ny2-ny1,R8P)
        z(i,j,k) = real(k-nz1,R8P)/real(nz2-nz1,R8P)
      enddo
    enddo
  enddo
  v = myrank
  ! saving files
  t= 0._R8P
  do f=1,Nf ! loop over files of current process
    if (myrank<nproc-1) then
      foffset = f+myrank*Nf
    else
      foffset = f+myrank*(Nf-mod(Nf_tot,nproc))
    endif
    nxf = (nx2-nx1+1)*foffset-(foffset-1)
    vtk_start = MPI_Wtime()
    E_IO = VTK_INI_XML_WRITE(fformat='binary', filename='XML_MPI_f'//trim(strz(3,foffset))//'.vts', &
                       mesh_topology='StructuredGrid', nx1=nxf-(nx2-nx1+1)+1, nx2=nxf,        &
                       ny1=ny1, ny2=ny2, nz1=nz1, nz2=nz2)
    ! updating x coordinates of current process/file
    xf = x + real(foffset-1_I4P, R8P)
    E_IO = VTK_GEO_XML_WRITE(nx1=nxf-(nx2-nx1+1)+1,nx2=nxf,ny1=ny1,ny2=ny2,nz1=nz1,nz2=nz2,NN=nn,X=xf,Y=y,Z=z)
    E_IO = VTK_DAT_XML(var_location='node',var_block_action='open')
    E_IO = VTK_VAR_XML(NC_NN=nn,varname='node_value',var=v)
    E_IO = VTK_DAT_XML(var_location='node',var_block_action='close')
    E_IO = VTK_GEO_XML_WRITE()
    E_IO = VTK_END_XML()
    vtk_stop = MPI_Wtime()
    t = t + vtk_stop-vtk_start
  enddo
  call MPI_REDUCE(t,tmax,1,MPI_REAL8,MPI_MAX,0,MPI_COMM_WORLD, E_IO)
  call MPI_REDUCE(t,tmin,1,MPI_REAL8,MPI_MIN,0,MPI_COMM_WORLD, E_IO)
  call MPI_REDUCE(t,tmean,1,MPI_REAL8,MPI_SUM,0,MPI_COMM_WORLD, E_IO)
  if (myrank==0) then
    tmean = tmean/nproc
    ! first process prints the elapsed time to stdout
    write(stdout,'(A)')' Maximum elapsed time of single process '//trim(adjustl(str('(F9.4)',tmax)))
    write(stdout,'(A)')' Minimum elapsed time of single process '//trim(adjustl(str('(F9.4)',tmin)))
    write(stdout,'(A)')' Average elapsed time '//trim(adjustl(str('(F9.4)',tmean)))
    ! first process saves also the composite .pvts file
    E_IO = PVTK_INI_XML(filename = 'XML_MPI.pvts', mesh_topology = 'PStructuredGrid',&
                        nx1=nx1, nx2=(nx2-nx1+1)*Nf_tot-(Nf_tot-1), ny1=ny1, ny2=ny2, nz1=nz1, nz2=nz2, tp='Float64')
    do f=1,Nf_tot
      nxf = (nx2-nx1+1)*f-(f-1)
      E_IO = PVTK_GEO_XML(nx1=nxf-(nx2-nx1+1)+1,nx2=nxf,ny1=ny1,ny2=ny2,nz1=nz1,nz2=nz2,&
                          source='XML_MPI_f'//trim(strz(3,f))//'.vts')
    enddo
    E_IO = PVTK_DAT_XML(var_location='node',var_block_action='open')
    E_IO = PVTK_VAR_XML(varname='node_value',tp='Int32')
    E_IO = PVTK_DAT_XML(var_location='node',var_block_action='close')
    E_IO = PVTK_END_XML()
  endif
  ! finalizing parallel environments
  call MPI_FINALIZE(E_IO)
  return
  !---------------------------------------------------------------------------------------------------------------------------------
  endsubroutine test_mpi
#endif
endmodule Lib_Testers

program Test_Driver
!-----------------------------------------------------------------------------------------------------------------------------------
!< Testing program for Lib_VTK_IO, a pure Fortran (2003+) library to write and read data conforming the VTK standard
!<
!<### Usage
!< For printing help message for usage run it without command line arguments
!<```bash
!< ./Test_Driver
!<```
!<
!<#### Testing UnstructuredGrid functions
!<```bash
!< ./Test_Driver -unst
!<```
!<
!<#### Testing StructuredGrid functions
!<```bash
!< ./Test_Driver -strg
!<```
!<
!<#### Testing RectilinearGrid functions
!<```bash
!< ./Test_Driver -rect
!<```
!<
!<#### Testing parallel (partitioned) PUnstructuredGrid functions
!<```bash
!< ./Test_Driver -punst
!<```
!<
!<#### Testing parallel (partitioned) PStructuredGrid functions
!<```bash
!< ./Test_Driver -pstrg
!<```
!<
!<#### Testing multi-blocks VTM functions
!<```bash
!< ./Test_Driver -vtm
!<```
!<
!<#### Testing thread-safe capability into an OpenMP parallel framework
!<```bash
!< ./Test_Driver -openmp
!<```
!<
!<#### Testing process-safe capability into a MPI parallel framework
!<```bash
!< ./Test_Driver -mpi
!<```
!-----------------------------------------------------------------------------------------------------------------------------------
USE IR_Precision
USE Lib_Testers
USE, intrinsic:: ISO_FORTRAN_ENV, only: stdout=>OUTPUT_UNIT, stderr=>ERROR_UNIT
!-----------------------------------------------------------------------------------------------------------------------------------

!-----------------------------------------------------------------------------------------------------------------------------------
implicit none
integer(I4P):: Nca = 0 !<  Number of command line arguments.
character(7):: cas     !<  Command line argument switch.
character(10):: nF     !<  Number of files for MPI and OpenMP benchmarks.
!-----------------------------------------------------------------------------------------------------------------------------------

!-----------------------------------------------------------------------------------------------------------------------------------
nF = ''
Nca = command_argument_count()
if (Nca==0) then
  call print_usage
endif
call get_command_argument(1,cas)
select case(trim(cas))
case('-unst')
  call test_unst
case('-strg')
  call test_strg
case('-rect')
  call test_rect
case('-punst')
  call test_punst
case('-pstrg')
  call test_pstrg
case('-vtm')
  call test_vtm
case('-openmp')
#ifndef OPENMP
  write(stderr,'(A)')' The code has not been compiled with OpenMP directives... nothing to test'
  stop
#else
  call get_command_argument(2,nF)
  call test_openmp(Nf_tot=cton(str=trim(nF),knd=I4P))
#endif
case('-mpi')
#ifndef MPI2
  write(stderr,'(A)')' The code has not been compiled with MPI library... nothing to test'
  stop
#else
  call get_command_argument(2,nF)
  call test_mpi(Nf_tot=cton(str=trim(nF),knd=I4P))
#endif
case('-all')
  call test_rect
  call test_unst
  call test_strg
  call test_punst
  call test_pstrg
  call test_vtm
case('-stress')
  call test_stress
case default
  write(stderr,'(A)')' Switch '//trim(cas)//' unknown'
  call print_usage
endselect
stop
!-----------------------------------------------------------------------------------------------------------------------------------
contains
  subroutine print_usage()
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Procedure for printing usage help message to stdout.
  !---------------------------------------------------------------------------------------------------------------------------------
  implicit none
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  write(stdout,'(A)')' Test_Driver: a "driver" program for testing Lib_VTK_IO functions'
  write(stdout,'(A)')' Usage:'
  write(stdout,'(A)')'   Test_Driver [-switch]'
  write(stdout,'(A)')'     switch = unst   => testing UnstructuredGrid functions'
  write(stdout,'(A)')'     switch = strg   => testing StructuredGrid functions'
  write(stdout,'(A)')'     switch = rect   => testing RectilinearGrid functions'
  write(stdout,'(A)')'     switch = punst  => testing parallel (partitioned) PUnstructuredGrid functions'
  write(stdout,'(A)')'     switch = pstrg  => testing parallel (partitioned) StructuredGrid functions'
  write(stdout,'(A)')'     switch = vtm    => testing multi-block XML functions'
  write(stdout,'(A)')'     switch = all    => testing all above functions'
  write(stdout,'(A)')'     switch = openmp => testing functions in parallel OpenMP framework'
  write(stdout,'(A)')'     switch = mpi    => testing functions in parallel MPI    framework'
  write(stdout,'(A)')'     switch = stress => testing functions of any kind/rank producing many files'
  write(stdout,'(A)')' Examples:'
  write(stdout,'(A)')'   Test_Driver -pstrg'
  write(stdout,'(A)')'   Test_Driver -vtm'
  write(stdout,'(A)')'   Test_Driver -openmp'
  write(stdout,'(A)')' If switch is not passed (or is unknown) this help message is printed to stdout'
  stop
  return
  !---------------------------------------------------------------------------------------------------------------------------------
  endsubroutine print_usage
endprogram Test_Driver
