!< INI interface definition for Lib_VTK_IO.
module Lib_VTK_IO_INI
!-----------------------------------------------------------------------------------------------------------------------------------
!< INI interface definition for Lib_VTK_IO.
!-----------------------------------------------------------------------------------------------------------------------------------
USE IR_Precision        ! Integers and reals precision definition.
USE Lib_Base64          ! Base64 encoding/decoding procedures.
USE Lib_VTK_IO_Back_End ! Lib_VTK_IO back end module.
!-----------------------------------------------------------------------------------------------------------------------------------

!-----------------------------------------------------------------------------------------------------------------------------------
implicit none
private
save
public:: VTK_INI
!-----------------------------------------------------------------------------------------------------------------------------------
contains
  function VTK_INI(fformat,filename,title,mesh_topology,cf) result(E_IO)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Function for initializing VTK-legacy file.
  !<
  !< @note This function must be the first to be called.
  !<
  !<### Usage
  !<```fortran
  !< E_IO=VTK_INI('Binary','example.vtk','VTK legacy file','UNSTRUCTURED_GRID')
  !<```
  !---------------------------------------------------------------------------------------------------------------------------------
  implicit none
  character(*), intent(IN)            :: fformat       !< Output format: ASCII or RAW.
  character(*), intent(IN)            :: filename      !< Name of file.
  character(*), intent(IN)            :: title         !< Title.
  character(*), intent(IN)            :: mesh_topology !< Mesh topology.
  integer(I4P), intent(OUT), optional :: cf            !< Current file index (for concurrent files IO).
  integer(I4P)                        :: E_IO          !< Input/Output inquiring flag: $0$ if IO is done, $> 0$ if IO is not done.
  integer(I4P)                        :: rf            !< Real file index.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  E_IO = -1_I4P
  if (.not.ir_initialized) call IR_Init
  if (.not.b64_initialized) call b64_init
  call vtk_update(act='add',cf=rf,Nvtk=Nvtk,vtk=vtk)
  f = rf
  if (present(cf)) cf = rf
  vtk(rf)%topology = trim(mesh_topology)
  select case(trim(Upper_Case(fformat)))
  case('ASCII')
    vtk(rf)%f = ascii
    open(unit=Get_Unit(vtk(rf)%u),file=trim(filename),form='FORMATTED',&
         access='SEQUENTIAL',action='WRITE',status='REPLACE',iostat=E_IO)
    ! writing header of file
    write(unit=vtk(rf)%u,fmt='(A)',iostat=E_IO)'# vtk DataFile Version 3.0'
    write(unit=vtk(rf)%u,fmt='(A)',iostat=E_IO)trim(title)
    write(unit=vtk(rf)%u,fmt='(A)',iostat=E_IO)trim(Upper_Case(fformat))
    write(unit=vtk(rf)%u,fmt='(A)',iostat=E_IO)'DATASET '//trim(vtk(rf)%topology)
  case('RAW')
    vtk(rf)%f = raw
    open(unit=Get_Unit(vtk(rf)%u),file=trim(filename),&
         form='UNFORMATTED',access='STREAM',action='WRITE',status='REPLACE',iostat=E_IO)
    ! writing header of file
    write(unit=vtk(rf)%u,iostat=E_IO)'# vtk DataFile Version 3.0'//end_rec
    write(unit=vtk(rf)%u,iostat=E_IO)trim(title)//end_rec
    write(unit=vtk(rf)%u,iostat=E_IO)trim(Upper_Case(fformat))//end_rec
    write(unit=vtk(rf)%u,iostat=E_IO)'DATASET '//trim(vtk(rf)%topology)//end_rec
  endselect
  return
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction VTK_INI
endmodule Lib_VTK_IO_INI
