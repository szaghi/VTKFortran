program fs_test
use Lib_VTK_IO
use penf
implicit none
! dataset dimensions
real,    parameter :: delta=0.1
integer, parameter :: nx1=0, nx2=47, ny1=0, ny2=24, nz1=0, nz2=6
integer            :: nn=(nx2-nx1+1)*(ny2-ny1+1)*(nz2-nz1+1)
integer            :: nx1r, nx2r, ny1r, ny2r, nz1r, nz2r, nnr
! ! grid coordinates
real, dimension(nx1:nx2,ny1:ny2,nz1:nz2) :: x, y, z
real, allocatable, dimension(:,:,:)      :: xr, yr, zr
! variables associated at grid nodes
real, dimension(nx1:nx2,ny1:ny2,nz1:nz2) :: v_R
real, allocatable, dimension(:,:,:)      :: v_Rr
! auxiliary variables
integer :: E_IO
integer :: i, j, k

do k=nz1,nz2
  do j=ny1,ny2
    do i=nx1,nx2
      x(  i, j, k) = i * delta
      y(  i, j, k) = j * delta
      z(  i, j, k) = k * delta
    enddo
  enddo
enddo

call random_number(v_R)

! save
E_IO = VTK_INI_XML_WRITE(fformat='RAW', filename='XML_STRG.vts', mesh_topology='StructuredGrid',&
                         nx1=nx1, nx2=nx2, ny1=ny1, ny2=ny2, nz1=nz1, nz2=nz2)
E_IO = VTK_GEO_XML_WRITE(nx1=nx1, nx2=nx2, ny1=ny1, ny2=ny2, nz1=nz1, nz2=nz2, NN=nn, X=x, Y=y, Z=z)
E_IO = VTK_DAT_XML(var_location='node', var_block_action='open')
E_IO = VTK_VAR_XML_WRITE(NC_NN=nn, varname='real scalar', var=v_R)
E_IO = VTK_DAT_XML(var_location='node', var_block_action='close')
E_IO = VTK_GEO_XML_WRITE()
E_IO = VTK_END_XML_WRITE()

! load
E_IO = VTK_INI_XML_READ(fformat='RAW', filename='XML_STRG.vts', mesh_topology='StructuredGrid')
E_IO = VTK_GEO_XML_READ(nx1r, nx2r, ny1r, ny2r, nz1r, nz2r, NN=nnr, X=xr, Y=yr, Z=zr)
E_IO = VTK_VAR_XML_READ(var_location='node', varname='real scalar', NC_NN=nn, NCOMP=i, var=v_Rr)
E_IO = VTK_END_XML_READ()
print*,'x(1,1,1),y(1,1,1),z(1,1,1): ',x(1,1,1),y(1,1,1),z(1,1,1)
print*,'xr(1,1,1),yr(1,1,1),zr(1,1,1): ',xr(1,1,1),yr(1,1,1),zr(1,1,1)
print*,'v_R(1,1,1): ',v_R(1,1,1)
print*,'v_Rr(1,1,1): ',v_Rr(1,1,1)
stop
endprogram fs_test
