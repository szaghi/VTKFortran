!> @addtogroup Program Programs
!> List of excutable programs.

!> @ingroup Program
!> @{
!> @defgroup Test_DriverProgram Test_Driver
!> @}

!> "Driver" program for testing @libvtk functions.
!> @note
!> For printing help message for usage run it without command line arguments
!> @code
!> ./Test_Driver
!> @endcode
!> For testing UnstructuredGrid functions rut it as following:
!> @code
!> ./Test_Driver -unst
!> @endcode
!> For testing StructuredGrid functions rut it as following:
!> @code
!> ./Test_Driver -strg
!> @endcode
!> For testing parallel (partitioned) PUnstructuredGrid functions rut it as following:
!> @code
!> ./Test_Driver -punst
!> @endcode
!> For testing parallel (partitioned) PStructuredGrid functions rut it as following:
!> @code
!> ./Test_Driver -pstrg
!> @endcode
!> @author    Stefano Zaghi
!> @version   1.0
!> @date      2013-03-28
!> @copyright GNU Public License version 3.
!> @todo \b RectilinearGrid: implement RectilinearGrid tests.
!> @ingroup Test_DriverProgram
program Test_Driver
!-----------------------------------------------------------------------------------------------------------------------------------
USE IR_Precision
USE Lib_VTK_IO
USE, intrinsic:: ISO_FORTRAN_ENV, only: stdout=>OUTPUT_UNIT, stderr=>ERROR_UNIT
!-----------------------------------------------------------------------------------------------------------------------------------

!-----------------------------------------------------------------------------------------------------------------------------------
implicit none
integer(I4P):: Nca = 0 !< Number of command line arguments.
character(6):: cas     !< Command line argument switch.
!-----------------------------------------------------------------------------------------------------------------------------------

!-----------------------------------------------------------------------------------------------------------------------------------
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
case('-all')
  call test_unst
  call test_strg
  call test_punst
  call test_pstrg
  call test_vtm
case default
  write(stderr,'(A)')' Switch '//trim(cas)//' unknown'
  call print_usage
endselect
stop
!-----------------------------------------------------------------------------------------------------------------------------------
contains
  !> Subroutine for printing usage help message to stdout.
  subroutine print_usage()
  !---------------------------------------------------------------------------------------------------------------------------------
  implicit none
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  write(stdout,'(A)')' Test_Driver: a "driver" program for testing Lib_VTK_IO functions'
  write(stdout,'(A)')' Usage:'
  write(stdout,'(A)')'   Test_Driver [-switch]'
  write(stdout,'(A)')'     switch = unst  => testing UnstructuredGrid functions'
  write(stdout,'(A)')'     switch = strg  => testing StructuredGrid functions'
  write(stdout,'(A)')'     switch = rect  => testing RectilinearGrid functions'
  write(stdout,'(A)')'     switch = punst => testing parallel (partitioned) PUnstructuredGrid functions'
  write(stdout,'(A)')'     switch = pstrg => testing parallel (partitioned) StructuredGrid functions'
  write(stdout,'(A)')'     switch = vtm   => testing multi-block XML functions'
  write(stdout,'(A)')'     switch = all   => testing all functions'
  write(stdout,'(A)')' Examples:'
  write(stdout,'(A)')'   Test_Driver -pstrg'
  write(stdout,'(A)')'   Test_Driver -vtm'
  write(stdout,'(A)')' If switch is not passed this help message is printed to stdout'
  stop
  return
  !---------------------------------------------------------------------------------------------------------------------------------
  endsubroutine print_usage

  !> Subroutine for testing UnstructuredGrid functions.
  subroutine test_unst()
  !---------------------------------------------------------------------------------------------------------------------------------
  implicit none
  integer(I4P), parameter::       Nn = 27_I4P
  integer(I4P), parameter::       Ne = 11_I4P
  real(R4P),    dimension(1:Nn):: x = (/0,1,2,0,1,2,0,1,2,0,1,2,0,1,2,0,1,2,0,1,2,0,1,2,0,1,2/)
  real(R4P),    dimension(1:Nn):: y = (/0,0,0,1,1,1,0,0,0,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1/)
  real(R4P),    dimension(1:Nn):: z = (/0,0,0,0,0,0,1,1,1,1,1,1,2,2,2,3,3,3,4,4,4,5,5,5,6,6,6/)
  integer(I1P), dimension(1:Ne):: cell_type = (/12,12,10,10,7,6,9,5,5,3,1/)
  integer(I4P), dimension(1:Ne):: offset = (/8,16,20,24,30,36,40,43,46,48,49/)
  integer(I4P), dimension(1:49):: connect
  real(R8P),    dimension(1:Nn):: v
  integer(I4P), dimension(1:Nn):: v_X
  integer(I4P), dimension(1:Nn):: v_Y
  integer(I4P), dimension(1:Nn):: v_Z
  integer(I4P)::                  E_IO
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  write(stdout,'(A)')' Testing UnstructuredGrid functions. Output file is XML_UNST.vtu'
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
  E_IO = VTK_INI_XML(output_format = 'Binary', filename = 'XML_UNST.vtu', mesh_topology = 'UnstructuredGrid')
  E_IO = VTK_GEO_XML(NN = Nn, NC = Ne, X = x, Y = y, Z = z)
  E_IO = VTK_CON_XML(NC = Ne, connect = connect, offset = offset, cell_type = cell_type )
  E_IO = VTK_DAT_XML(var_location = 'node', var_block_action = 'opeN')
  E_IO = VTK_VAR_XML(NC_NN = Nn, varname = 'scalars', var = v)
  E_IO = VTK_VAR_XML(NC_NN = Nn, varname = 'vector', varX=v_X,varY=v_Y,varZ=v_Z)
  E_IO = VTK_DAT_XML(var_location = 'node', var_block_action = 'CLOSE')
  E_IO = VTK_GEO_XML()
  E_IO = VTK_END_XML()
  return
  !---------------------------------------------------------------------------------------------------------------------------------
  endsubroutine test_unst

  !> Subroutine for testing StructuredGrid functions.
  !> @note This subroutine also shows the usage of FieldData functions that are useful for saving global auxiliary data, e.g. time,
  !> time step, ecc.
  subroutine test_strg()
  !---------------------------------------------------------------------------------------------------------------------------------
  implicit none
  integer(I4P), parameter::                          nx1=0_I4P,nx2=9_I4P,ny1=0_I4P,ny2=5_I4P,nz1=0_I4P,nz2=5_I4P
  integer(I4P), parameter::                          nn=(nx2-nx1+1)*(ny2-ny1+1)*(nz2-nz1+1)
  real(R8P),    dimension(nx1:nx2,ny1:ny2,nz1:nz2):: x,y,z
  integer(I4P), dimension(nx1:nx2,ny1:ny2,nz1:nz2):: v
  integer(I4P)::                                     i,j,k,E_IO
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  write(stdout,'(A)')' Testing StructuredGrid functions. Output file is XML_STRG.vts'
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
  E_IO = VTK_INI_XML(output_format='binary', filename='XML_STRG.vts', &
                     mesh_topology='StructuredGrid', nx1=nx1, nx2=nx2, ny1=ny1, ny2=ny2, nz1=nz1, nz2=nz2)
  E_IO = VTK_FLD_XML(fld_action='open')
  E_IO = VTK_FLD_XML(fld=0._R8P,fname='TIME')
  E_IO = VTK_FLD_XML(fld=1_I8P,fname='CYCLE')
  E_IO = VTK_FLD_XML(fld_action='close')
  E_IO = VTK_GEO_XML(nx1=nx1, nx2=nx2, ny1=ny1, ny2=ny2, nz1=nz1, nz2=nz2, NN=nn, &
                     X=reshape(x(nx1:nx2,:,:),(/nn/)),                            &
                     Y=reshape(y(nx1:nx2,:,:),(/nn/)),                            &
                     Z=reshape(z(nx1:nx2,:,:),(/nn/)))
  E_IO = VTK_DAT_XML(var_location = 'node', var_block_action = 'open')
  E_IO = VTK_VAR_XML(NC_NN = nn, varname = 'node_value', var = reshape(v(nx1:nx2,:,:),(/nn/)))
  E_IO = VTK_DAT_XML(var_location = 'node', var_block_action = 'close')
  E_IO = VTK_GEO_XML()
  E_IO = VTK_END_XML()
  return
  !---------------------------------------------------------------------------------------------------------------------------------
  endsubroutine

  !> Subroutine for testing RectilinearGrid functions.
  !> @note This subroutine also shows the usage of FieldData functions that are useful for saving global auxiliary data, e.g. time,
  !> time step, ecc.
  subroutine test_rect()
  !---------------------------------------------------------------------------------------------------------------------------------
  implicit none
  integer(I4P), parameter:: nx1=0_I4P,nx2=30_I4P,ny1=0_I4P,ny2=20_I4P,nz1=0_I4P,nz2=10_I4P
  integer(I4P), parameter:: nn=(nx2-nx1+1)*(ny2-ny1+1)*(nz2-nz1+1)
  real(R8P)::               x(nx1:nx2),y(ny1:ny2),z(nz1:nz2)
  integer(I4P)::            v(1:nn)
  integer(I4P)::            i,j,k,n,E_IO
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  write(stdout,'(A)')' Testing RectilinearGrid functions. Output file is XML_RECT.vtr'
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
    x(i) = i*1._R4P
  enddo
  do j=ny1,ny2
    y(j) = j*1._R4P
  enddo
  do k=nz1,nz2
    z(k) = k*1._R4P
  enddo
  E_IO = VTK_INI_XML(output_format='binary', filename='XML_RECT.vtr', &
                     mesh_topology='RectilinearGrid', nx1=nx1, nx2=nx2, ny1=ny1, ny2=ny2, nz1=nz1, nz2=nz2)
  E_IO = VTK_FLD_XML(fld_action='open')
  E_IO = VTK_FLD_XML(fld=0._R8P,fname='TIME')
  E_IO = VTK_FLD_XML(fld=1_I8P,fname='CYCLE')
  E_IO = VTK_FLD_XML(fld_action='close')
  E_IO = VTK_GEO_XML(nx1=nx1, nx2=nx2, ny1=ny1, ny2=ny2, nz1=nz1, nz2=nz2, X=x, Y=y, Z=z)
  E_IO = VTK_DAT_XML(var_location = 'cell', var_block_action = 'open')
  E_IO = VTK_VAR_XML(NC_NN = nn, varname = 'cell_value', var = v)
  E_IO = VTK_DAT_XML(var_location = 'cell', var_block_action = 'close')
  E_IO = VTK_GEO_XML()
  E_IO = VTK_END_XML()
  return
  !---------------------------------------------------------------------------------------------------------------------------------
  endsubroutine

  !> Subroutine for testing parallel (partitioned) PStructuredGrid functions.
  !> @note Note that the two parts are completely independet.
  subroutine test_punst()
  !---------------------------------------------------------------------------------------------------------------------------------
  implicit none
  integer(I4P), parameter::       Nn = 27_I4P
  integer(I4P), parameter::       Ne = 11_I4P
  real(R4P),    dimension(1:Nn):: x = (/0,1,2,0,1,2,0,1,2,0,1,2,0,1,2,0,1,2,0,1,2,0,1,2,0,1,2/)
  real(R4P),    dimension(1:Nn):: y = (/0,0,0,1,1,1,0,0,0,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1/)
  real(R4P),    dimension(1:Nn):: z = (/0,0,0,0,0,0,1,1,1,1,1,1,2,2,2,3,3,3,4,4,4,5,5,5,6,6,6/)
  integer(I1P), dimension(1:Ne):: cell_type = (/12,12,10,10,7,6,9,5,5,3,1/)
  integer(I4P), dimension(1:Ne):: offset = (/8,16,20,24,30,36,40,43,46,48,49/)
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
  E_IO = VTK_INI_XML(output_format = 'Binary', filename = 'XML_UNST_part0.vtu', mesh_topology = 'UnstructuredGrid')
  E_IO = VTK_GEO_XML(NN = Nn, NC = Ne, X = x, Y = y, Z = z)
  E_IO = VTK_CON_XML(NC = Ne, connect = connect, offset = offset, cell_type = cell_type )
  E_IO = VTK_DAT_XML(var_location = 'node', var_block_action = 'opeN')
  E_IO = VTK_VAR_XML(NC_NN = Nn, varname = 'scalars', var = v)
  E_IO = VTK_VAR_XML(NC_NN = Nn, varname = 'vector', varX=v_X,varY=v_Y,varZ=v_Z)
  E_IO = VTK_DAT_XML(var_location = 'node', var_block_action = 'CLOSE')
  E_IO = VTK_GEO_XML()
  E_IO = VTK_END_XML()
  ! second part
  x = x + 10._R4P
  E_IO = VTK_INI_XML(output_format = 'Binary', filename = 'XML_UNST_part1.vtu', mesh_topology = 'UnstructuredGrid')
  E_IO = VTK_GEO_XML(NN = Nn, NC = Ne, X = x, Y = y, Z = z)
  E_IO = VTK_CON_XML(NC = Ne, connect = connect, offset = offset, cell_type = cell_type )
  E_IO = VTK_DAT_XML(var_location = 'node', var_block_action = 'opeN')
  E_IO = VTK_VAR_XML(NC_NN = Nn, varname = 'scalars', var = v)
  E_IO = VTK_VAR_XML(NC_NN = Nn, varname = 'vector', varX=v_X,varY=v_Y,varZ=v_Z)
  E_IO = VTK_DAT_XML(var_location = 'node', var_block_action = 'CLOSE')
  E_IO = VTK_GEO_XML()
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

  !> Subroutine for testing parallel (partitioned) PStructuredGrid functions.
  !> @note The mesh is a simple prism partitioned into two pieces along x direction at ordinate i=nx2_p(1).
  !> @code
  !> y ^
  !>   |               ny2 +-----------------+--------------+
  !>   |                   |                 |              |
  !>   |                   |                 |              |
  !>   |                   |                 |              |
  !>   |                   |                 |              |
  !>   o-------->      ny1 +-----------------+--------------+
  !>            x         nx1               i=nx2_p(1)     nx2
  !> @endcode
  !> @note This subroutine also tests concurrent multiple files IO.
  !> @bug OpenMP parallel paradigm does not work. The library @libvtk is not yet thread safe.
  subroutine test_pstrg()
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
  ! files initialization
  do p=1,2 ! loop over pieces
    E_IO = VTK_INI_XML(cf=mf(p),output_format='binary', filename='XML_STRG_part'//trim(str(.true.,p-1))//'.vts', &
                       mesh_topology='StructuredGrid', nx1=nx1_p(p), nx2=nx2_p(p), ny1=ny1, ny2=ny2, nz1=nz1, nz2=nz2)
  enddo
  !$OMP PARALLEL DEFAULT(NONE) &
  !$OMP PRIVATE(p,E_IO)        &
  !$OMP SHARED(mf,x,y,z,v)
  !$OMP DO
  do p=1,2 ! loop over pieces
    E_IO = VTK_GEO_XML(cf=mf(p),nx1=nx1_p(p), nx2=nx2_p(p), ny1=ny1, ny2=ny2, nz1=nz1, nz2=nz2, NN=nn_p(p), &
                       X=reshape(x(nx1_p(p):nx2_p(p),:,:),(/nn_p(p)/)),                                     &
                       Y=reshape(y(nx1_p(p):nx2_p(p),:,:),(/nn_p(p)/)),                                     &
                       Z=reshape(z(nx1_p(p):nx2_p(p),:,:),(/nn_p(p)/)))
    E_IO = VTK_DAT_XML(cf=mf(p),var_location = 'node', var_block_action = 'open')
    E_IO = VTK_VAR_XML(cf=mf(p),NC_NN = nn_p(p), varname = 'node_value', var = reshape(v(nx1_p(p):nx2_p(p),:,:),(/nn_p(p)/)))
    E_IO = VTK_DAT_XML(cf=mf(p),var_location = 'node', var_block_action = 'close')
    E_IO = VTK_GEO_XML(cf=mf(p))
  enddo
  !$OMP END PARALLEL
  ! files finalization
  do p=1,2 ! loop over pieces
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

  !> Subroutine for testing multi-blocks VTM functions. There are 4 subset of data organized into 2 blocks. All the subsets are
  !> simple StructuredGrid prisms shifted along x direction.
  subroutine test_vtm()
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
    E_IO = VTK_INI_XML(cf=mf(b),output_format='binary', filename='XML_M-STRG_part.'//trim(str(.true.,b-1))//'.vts', &
                       mesh_topology='StructuredGrid', nx1=nx1, nx2=nx2, ny1=ny1, ny2=ny2, nz1=nz1, nz2=nz2)
    if (b>1) then
      x = x + nx2*1._R8P
      v = b
    endif
    E_IO = VTK_GEO_XML(cf=mf(b),nx1=nx1, nx2=nx2, ny1=ny1, ny2=ny2, nz1=nz1, nz2=nz2, NN=nn, &
                       X=reshape(x(nx1:nx2,:,:),(/nn/)),                                     &
                       Y=reshape(y(nx1:nx2,:,:),(/nn/)),                                     &
                       Z=reshape(z(nx1:nx2,:,:),(/nn/)))
    E_IO = VTK_DAT_XML(cf=mf(b),var_location = 'node', var_block_action = 'open')
    E_IO = VTK_VAR_XML(cf=mf(b),NC_NN = nn, varname = 'node_value', var = reshape(v(nx1:nx2,:,:),(/nn/)))
    E_IO = VTK_DAT_XML(cf=mf(b),var_location = 'node', var_block_action = 'close')
    E_IO = VTK_GEO_XML(cf=mf(b))
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
endprogram Test_Driver
