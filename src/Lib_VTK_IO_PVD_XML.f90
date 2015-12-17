!< PVD_XML interface definitions for Lib_VTK_IO.
module Lib_VTK_IO_PVD_XML
!-----------------------------------------------------------------------------------------------------------------------------------
!< PVD_XML interface definitions for Lib_VTK_IO.
!-----------------------------------------------------------------------------------------------------------------------------------
USE IR_Precision        ! Integers and reals precision definition.
USE Lib_Base64          ! Base64 encoding/decoding procedures.
USE Lib_VTK_IO_Back_End ! Lib_VTK_IO back end module.
!-----------------------------------------------------------------------------------------------------------------------------------

!-----------------------------------------------------------------------------------------------------------------------------------
implicit none
private
save
public:: PVD_INI_XML
public:: PVD_DAT_XML
public:: PVD_END_XML
!-----------------------------------------------------------------------------------------------------------------------------------

!-----------------------------------------------------------------------------------------------------------------------------------
interface PVD_DAT_XML
  !< Procedure for saving data variable(s) in VTK-XML standard.
  !<
  !< PVD_DAT_XML is an interface to 6 different functions, depending on the datatype of the timestep
  !<
  !<### Examples of usage
  !<
  !<#### Calling PVD_DAT_XML
  !<```fortran
  !< integer(I4P):: timestep
  !< ...
  !< E_IO=PVD_DAT_XML('file.vtu,timestep)
  !< ...
  !<```
  module procedure PVD_DAT_XML_R8,PVD_DAT_XML_R4, & ! real timesteps
                   PVD_DAT_XML_I8,PVD_DAT_XML_I4, & ! integer (I8 and I4) timesteps
                   PVD_DAT_XML_I2,PVD_DAT_XML_I1    ! integer (I2 and I1) timesteps
endinterface
!-----------------------------------------------------------------------------------------------------------------------------------
contains


  function PVD_INI_XML(filename,cf) result(E_IO)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Function for initializing a PVD-XML file.
  !---------------------------------------------------------------------------------------------------------------------------------
  implicit none
  character(*), intent(IN)           :: filename      !< File name.
  integer(I4P), intent(OUT), optional:: cf            !< Current file index (for concurrent files IO).
  integer(I4P)                       :: E_IO          !< Input/Output inquiring flag: $0$ if IO is done, $> 0$ if IO is not done.
  character(len=maxlen)              :: s_buffer      !< Buffer string.
  integer(I4P)                       :: rf            !< Real file index.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  E_IO = -1_I4P
  if (.not.ir_initialized) call IR_Init
  if (.not.b64_initialized) call b64_init
  call vtk_update(act='add',cf=rf,Nvtk=Nvtk,vtk=vtk)
  f = rf
  if (present(cf)) cf = rf

  open(unit=Get_Unit(vtk(rf)%u),file=trim(filename),&
       form='FORMATTED',access='SEQUENTIAL',action='WRITE',status='REPLACE',iostat=E_IO)

  write(unit=vtk(rf)%u,fmt='(A)',iostat=E_IO)'<?xml version="1.0"?>'
  if (endian==endianL) then
    s_buffer = '<VTKFile type="Collection" version="0.1" byte_order="LittleEndian">'
  else
    s_buffer = '<VTKFile type="Collection" version="0.1" byte_order="BigEndian">'
  endif
  write(unit=vtk(rf)%u,fmt='(A)',iostat=E_IO)trim(s_buffer) ; vtk(rf)%indent = 2
  write(unit=vtk(rf)%u,fmt='(A)',iostat=E_IO)repeat(' ',vtk(rf)%indent)//'<Collection>'
  vtk(rf)%indent = vtk(rf)%indent + 2


  end function PVD_INI_XML


  function PVD_DAT_XML_R8(filename,timestep, part, cf) result(E_IO) !group, part, cf) result(E_IO)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Function for saving of PVD data associated to the sequence of VTK files
  !---------------------------------------------------------------------------------------------------------------------------------
  implicit none
  character(*), intent(IN)           :: filename     !< Location of saving variables: CELL or NODE centered.
  real(R8P),    intent(IN)           :: timestep     !< Timestep index
  integer(I4P), intent(IN), optional :: part         !< Part index
  integer(I4P), intent(IN), optional :: cf           !< Current file index (for concurrent files IO).
  integer(I4P)                       :: E_IO         !< Input/Output inquiring flag: $0$ if IO is done, $> 0$ if IO is not done.
  integer(I4P)                       :: rf           !< Real file index.
  integer(I4P)                       :: rp           !< Real part index.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  E_IO = -1_I4P
  rf = f
  if (present(cf)) then
    rf = cf ; f = cf
  endif

  rp = 0
  if (present(part)) rp = part

  write(unit=vtk(rf)%u,fmt='(A)',iostat=E_IO)repeat(' ',vtk(rf)%indent)//&
         '<DataSet timestep="'//trim(str(n=timestep))//'" group="" part="'//trim(str(n=rp))//'" file="'//trim(filename)//'"/>'

  return
  !---------------------------------------------------------------------------------------------------------------------------------
  end function PVD_DAT_XML_R8


  function PVD_DAT_XML_R4(filename,timestep, part, cf) result(E_IO) !group, part, cf) result(E_IO)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Function for saving of PVD data associated to the sequence of VTK files
  !---------------------------------------------------------------------------------------------------------------------------------
  implicit none
  character(*), intent(IN)           :: filename     !< Location of saving variables: CELL or NODE centered.
  real(R4P),    intent(IN)           :: timestep     !< Timestep index
  integer(I4P), intent(IN), optional :: part         !< Part index
  integer(I4P), intent(IN), optional :: cf           !< Current file index (for concurrent files IO).
  integer(I4P)                       :: E_IO         !< Input/Output inquiring flag: $0$ if IO is done, $> 0$ if IO is not done.
  integer(I4P)                       :: rf           !< Real file index.
  integer(I4P)                       :: rp           !< Real part index.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  E_IO = -1_I4P
  rf = f
  if (present(cf)) then
    rf = cf ; f = cf
  endif

  rp = 0
  if (present(part)) rp = part

  write(unit=vtk(rf)%u,fmt='(A)',iostat=E_IO)repeat(' ',vtk(rf)%indent)//&
         '<DataSet timestep="'//trim(str(n=timestep))//'" group="" part="'//trim(str(n=rp))//'" file="'//trim(filename)//'"/>'

  return
  !---------------------------------------------------------------------------------------------------------------------------------
  end function PVD_DAT_XML_R4


  function PVD_DAT_XML_I8(filename,timestep, part, cf) result(E_IO) !group, part, cf) result(E_IO)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Function for saving of PVD data associated to the sequence of VTK files
  !---------------------------------------------------------------------------------------------------------------------------------
  implicit none
  character(*), intent(IN)           :: filename     !< Location of saving variables: CELL or NODE centered.
  integer(I8P), intent(IN)           :: timestep     !< Timestep index
  integer(I4P), intent(IN), optional :: part         !< Part index
  integer(I4P), intent(IN), optional :: cf           !< Current file index (for concurrent files IO).
  integer(I4P)                       :: E_IO         !< Input/Output inquiring flag: $0$ if IO is done, $> 0$ if IO is not done.
  integer(I4P)                       :: rf           !< Real file index.
  integer(I4P)                       :: rp           !< Real part index.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  E_IO = -1_I4P
  rf = f
  if (present(cf)) then
    rf = cf ; f = cf
  endif

  rp = 0
  if (present(part)) rp = part

  write(unit=vtk(rf)%u,fmt='(A)',iostat=E_IO)repeat(' ',vtk(rf)%indent)//&
         '<DataSet timestep="'//trim(str(n=timestep))//'" group="" part="'//trim(str(n=rp))//'" file="'//trim(filename)//'"/>'

  return
  !---------------------------------------------------------------------------------------------------------------------------------
  end function PVD_DAT_XML_I8

  function PVD_DAT_XML_I4(filename,timestep, part, cf) result(E_IO) !group, part, cf) result(E_IO)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Function for saving of PVD data associated to the sequence of VTK files
  !---------------------------------------------------------------------------------------------------------------------------------
  implicit none
  character(*), intent(IN)           :: filename     !< Location of saving variables: CELL or NODE centered.
  integer(I4P), intent(IN)           :: timestep     !< Timestep index
  integer(I4P), intent(IN), optional :: part         !< Part index
  integer(I4P), intent(IN), optional :: cf           !< Current file index (for concurrent files IO).
  integer(I4P)                       :: E_IO         !< Input/Output inquiring flag: $0$ if IO is done, $> 0$ if IO is not done.
  integer(I4P)                       :: rf           !< Real file index.
  integer(I4P)                       :: rp           !< Real part index.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  E_IO = -1_I4P
  rf = f
  if (present(cf)) then
    rf = cf ; f = cf
  endif

  rp = 0
  if (present(part)) rp = part

  write(unit=vtk(rf)%u,fmt='(A)',iostat=E_IO)repeat(' ',vtk(rf)%indent)//&
         '<DataSet timestep="'//trim(str(n=timestep))//'" group="" part="'//trim(str(n=rp))//'" file="'//trim(filename)//'"/>'

  return
  !---------------------------------------------------------------------------------------------------------------------------------
  end function PVD_DAT_XML_I4


  function PVD_DAT_XML_I2(filename,timestep, part, cf) result(E_IO) !group, part, cf) result(E_IO)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Function for saving of PVD data associated to the sequence of VTK files
  !---------------------------------------------------------------------------------------------------------------------------------
  implicit none
  character(*), intent(IN)           :: filename     !< Location of saving variables: CELL or NODE centered.
  integer(I2P), intent(IN)           :: timestep     !< Timestep index
  integer(I4P), intent(IN), optional :: part         !< Part index
  integer(I4P), intent(IN), optional :: cf           !< Current file index (for concurrent files IO).
  integer(I4P)                       :: E_IO         !< Input/Output inquiring flag: $0$ if IO is done, $> 0$ if IO is not done.
  integer(I4P)                       :: rf           !< Real file index.
  integer(I4P)                       :: rp           !< Real part index.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  E_IO = -1_I4P
  rf = f
  if (present(cf)) then
    rf = cf ; f = cf
  endif

  rp = 0
  if (present(part)) rp = part

  write(unit=vtk(rf)%u,fmt='(A)',iostat=E_IO)repeat(' ',vtk(rf)%indent)//&
         '<DataSet timestep="'//trim(str(n=timestep))//'" group="" part="'//trim(str(n=rp))//'" file="'//trim(filename)//'"/>'

  return
  !---------------------------------------------------------------------------------------------------------------------------------
  end function PVD_DAT_XML_I2


  function PVD_DAT_XML_I1(filename,timestep, part, cf) result(E_IO) !group, part, cf) result(E_IO)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Function for saving of PVD data associated to the sequence of VTK files
  !---------------------------------------------------------------------------------------------------------------------------------
  implicit none
  character(*), intent(IN)           :: filename     !< Location of saving variables: CELL or NODE centered.
  integer(I1P), intent(IN)           :: timestep     !< Timestep index
  integer(I4P), intent(IN), optional :: part         !< Part index
  integer(I4P), intent(IN), optional :: cf           !< Current file index (for concurrent files IO).
  integer(I4P)                       :: E_IO         !< Input/Output inquiring flag: $0$ if IO is done, $> 0$ if IO is not done.
  integer(I4P)                       :: rf           !< Real file index.
  integer(I4P)                       :: rp           !< Real part index.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  E_IO = -1_I4P
  rf = f
  if (present(cf)) then
    rf = cf ; f = cf
  endif

  rp = 0
  if (present(part)) rp = part

  write(unit=vtk(rf)%u,fmt='(A)',iostat=E_IO)repeat(' ',vtk(rf)%indent)//&
         '<DataSet timestep="'//trim(str(n=timestep))//'" group="" part="'//trim(str(n=rp))//'" file="'//trim(filename)//'"/>'

  return
  !---------------------------------------------------------------------------------------------------------------------------------
  end function PVD_DAT_XML_I1


  function PVD_END_XML(cf) result(E_IO)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Function for finalizing the PVD-XML file.
  !---------------------------------------------------------------------------------------------------------------------------------
  implicit none
  integer(I4P), intent(INOUT), optional:: cf   !< Current file index (for concurrent files IO).
  integer(I4P)::                          E_IO !< Input/Output inquiring flag: $0$ if IO is done, $> 0$ if IO is not done.
  integer(I4P)::                          rf   !< Real file index.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  E_IO = -1_I4P
  rf = f
  if (present(cf)) then
    rf = cf ; f = cf
  endif

  vtk(rf)%indent = vtk(rf)%indent - 2
  write(unit=vtk(rf)%u,fmt='(A)',iostat=E_IO)repeat(' ',vtk(rf)%indent)//'</Collection>'
  write(unit=vtk(rf)%u,fmt='(A)',iostat=E_IO)'</VTKFile>'

  close(unit=vtk(rf)%u,iostat=E_IO)
  call vtk_update(act='remove',cf=rf,Nvtk=Nvtk,vtk=vtk)
  f = rf
  if (present(cf)) cf = rf
  
  return
  !---------------------------------------------------------------------------------------------------------------------------------
  end function PVD_END_XML

endmodule Lib_VTK_IO_PVD_XML
