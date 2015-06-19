!< FLD_XML interface definition for Lib_VTK_IO.
module Lib_VTK_IO_FLD_XML
!-----------------------------------------------------------------------------------------------------------------------------------
!< FLD_XML interface definition for Lib_VTK_IO.
!-----------------------------------------------------------------------------------------------------------------------------------
USE IR_Precision        ! Integers and reals precision definition.
USE Lib_Base64          ! Base64 encoding/decoding procedures.
USE Lib_VTK_IO_Back_End ! Lib_VTK_IO back end module.
!-----------------------------------------------------------------------------------------------------------------------------------

!-----------------------------------------------------------------------------------------------------------------------------------
implicit none
private
save
public:: VTK_FLD_XML
!-----------------------------------------------------------------------------------------------------------------------------------

!-----------------------------------------------------------------------------------------------------------------------------------
interface VTK_FLD_XML
  !< Procedure for saving field data (global auxiliary data, eg time, step number, dataset name, etc).
  !<
  !< VTK_FLD_XML is an interface to 7 different functions, there are 2 functions for real field data, 4 functions for integer one
  !< and one function for open and close field data tag.
  !< VTK_FLD_XML must be called after VTK_INI_XML and before VTK_GEO_XML. It must always called three times at least:
  !<
  !< 1. for opening the FieldData tag;
  !< 2. for saving at least one FieldData entry;
  !< 3. for closing the FieldData tag.
  !<
  !< Example of usage:
  !<
  !<```fortran
  !<...
  !<real(R8P)::    time
  !<integer(I4P):: step
  !<...
  !<E_IO=VTK_FLD_XML(fld_action='open')
  !<E_IO=VTK_FLD_XML(fld=time,fname='TIME')
  !<E_IO=VTK_FLD_XML(fld=step,fname='CYCLE')
  !<E_IO=VTK_FLD_XML(fld_action='close')
  !<...
  !<```
  module procedure VTK_FLD_XML_OC, & ! open/close field data tag
                   VTK_FLD_XML_R8, & ! real(R8P)    scalar
                   VTK_FLD_XML_R4, & ! real(R4P)    scalar
                   VTK_FLD_XML_I8, & ! integer(I8P) scalar
                   VTK_FLD_XML_I4, & ! integer(I4P) scalar
                   VTK_FLD_XML_I2, & ! integer(I2P) scalar
                   VTK_FLD_XML_I1    ! integer(I1P) scalar
endinterface
!-----------------------------------------------------------------------------------------------------------------------------------
contains
  function VTK_FLD_XML_OC(fld_action,cf) result(E_IO)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Function for open/close field data tag.
  !---------------------------------------------------------------------------------------------------------------------------------
  implicit none
  character(*), intent(IN)::           fld_action !< Field data tag action: OPEN or CLOSE tag.
  integer(I4P), intent(IN), optional:: cf         !< Current file index (for concurrent files IO).
  integer(I4P)::                       E_IO       !< Input/Output inquiring flag: $0$ if IO is done, $> 0$ if IO is not done.
  integer(I4P)::                       rf         !< Real file index.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  E_IO = -1_I4P
  rf = f
  if (present(cf)) then
    rf = cf ; f = cf
  endif
  select case(trim(Upper_Case(fld_action)))
  case('OPEN')
    select case(vtk(rf)%f)
    case(ascii)
      write(unit=vtk(rf)%u,fmt='(A)',iostat=E_IO)repeat(' ',vtk(rf)%indent)//'<FieldData>' ; vtk(rf)%indent = vtk(rf)%indent + 2
    case(raw,binary,bin_app)
      write(unit=vtk(rf)%u,iostat=E_IO)repeat(' ',vtk(rf)%indent)//'<FieldData>'//end_rec ; vtk(rf)%indent = vtk(rf)%indent + 2
    endselect
  case('CLOSE')
    select case(vtk(rf)%f)
    case(ascii)
      vtk(rf)%indent = vtk(rf)%indent - 2 ; write(unit=vtk(rf)%u,fmt='(A)',iostat=E_IO)repeat(' ',vtk(rf)%indent)//'</FieldData>'
    case(raw,binary,bin_app)
      vtk(rf)%indent = vtk(rf)%indent - 2 ; write(unit=vtk(rf)%u,iostat=E_IO)repeat(' ',vtk(rf)%indent)//'</FieldData>'//end_rec
    endselect
  endselect
  return
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction VTK_FLD_XML_OC

  function VTK_FLD_XML_R8(fld,fname,cf) result(E_IO)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Function for saving field data (global auxiliary data, e.g. time, step number, data set name...) (R8P).
  !---------------------------------------------------------------------------------------------------------------------------------
  implicit none
  real(R8P),    intent(IN)::           fld      !< Field data value.
  character(*), intent(IN)::           fname    !< Field data name.
  integer(I4P), intent(IN), optional:: cf       !< Current file index (for concurrent files IO).
  integer(I4P)::                       E_IO     !< Input/Output inquiring flag: $0$ if IO is done, $> 0$ if IO is not done.
  character(len=maxlen)::              s_buffer !< Buffer string.
  integer(I1P), allocatable::          fldp(:)  !< Packed field data.
  character(len=:), allocatable::      fld64    !< Field data encoded in base64.
  integer(I4P)::                       rf       !< Real file index.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  E_IO = -1_I4P
  rf = f
  if (present(cf)) then
    rf = cf ; f = cf
  endif
  select case(vtk(rf)%f)
  case(ascii)
    s_buffer=repeat(' ',vtk(rf)%indent)//'<DataArray type="Float64" NumberOfTuples="1" Name="'//trim(fname)//'" format="ascii">'//&
             trim(str(n=fld))//'</DataArray>'
    write(unit=vtk(rf)%u,fmt='(A)',iostat=E_IO)trim(s_buffer)
  case(raw,bin_app)
    s_buffer=repeat(' ',vtk(rf)%indent)//'<DataArray type="Float64" NumberOfTuples="1" Name="'//trim(fname)// &
             '" format="appended" offset="'//trim(str(.true.,vtk(rf)%ioffset))//'"/>'
    write(unit=vtk(rf)%u,iostat=E_IO)trim(s_buffer)//end_rec
    call vtk(rf)%byte_update(N_Byte = int(BYR8P,I4P))
    write(unit=vtk(rf)%ua,iostat=E_IO)vtk(rf)%N_Byte,'R8',1_I4P
    write(unit=vtk(rf)%ua,iostat=E_IO)fld
  case(binary)
    s_buffer=repeat(' ',vtk(rf)%indent)//'<DataArray type="Float64" NumberOfTuples="1" Name="'//trim(fname)//'" format="binary">'
    write(unit=vtk(rf)%u,iostat=E_IO)trim(s_buffer)//end_rec
    call pack_data(a1=[int(BYR8P,I4P)],a2=[fld],packed=fldp)
    call b64_encode(n=fldp,code=fld64) ; deallocate(fldp)
    write(unit=vtk(rf)%u,iostat=E_IO)repeat(' ',vtk(rf)%indent+2)//fld64//end_rec ; deallocate(fld64)
    write(unit=vtk(rf)%u,iostat=E_IO)repeat(' ',vtk(rf)%indent)//'</DataArray>'//end_rec
  endselect
  return
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction VTK_FLD_XML_R8

  function VTK_FLD_XML_R4(fld,fname,cf) result(E_IO)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Function for saving field data (global auxiliary data, e.g. time, step number, data set name...) (R4P).
  !---------------------------------------------------------------------------------------------------------------------------------
  implicit none
  real(R4P),    intent(IN)::           fld      !< Field data value.
  character(*), intent(IN)::           fname    !< Field data name.
  integer(I4P), intent(IN), optional:: cf       !< Current file index (for concurrent files IO).
  integer(I4P)::                       E_IO     !< Input/Output inquiring flag: $0$ if IO is done, $> 0$ if IO is not done.
  character(len=maxlen)::              s_buffer !< Buffer string.
  integer(I1P), allocatable::          fldp(:)  !< Packed field data.
  character(len=:), allocatable::      fld64    !< Field data encoded in base64.
  integer(I4P)::                       rf       !< Real file index.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  E_IO = -1_I4P
  rf = f
  if (present(cf)) then
    rf = cf ; f = cf
  endif
  select case(vtk(rf)%f)
  case(ascii)
    s_buffer=repeat(' ',vtk(rf)%indent)//'<DataArray type="Float32" NumberOfTuples="1" Name="'//trim(fname)//'" format="ascii">'//&
             trim(str(n=fld))//'</DataArray>'
    write(unit=vtk(rf)%u,fmt='(A)',iostat=E_IO)trim(s_buffer)
  case(raw,bin_app)
    s_buffer=repeat(' ',vtk(rf)%indent)//'<DataArray type="Float32" NumberOfTuples="1" Name="'//trim(fname)// &
             '" format="appended" offset="'//trim(str(.true.,vtk(rf)%ioffset))//'"/>'
    write(unit=vtk(rf)%u,iostat=E_IO)trim(s_buffer)//end_rec
    call vtk(rf)%byte_update(N_Byte = int(BYR4P,I4P))
    write(unit=vtk(rf)%ua,iostat=E_IO)vtk(rf)%N_Byte,'R4',1_I4P
    write(unit=vtk(rf)%ua,iostat=E_IO)fld
  case(binary)
    s_buffer=repeat(' ',vtk(rf)%indent)//'<DataArray type="Float32" NumberOfTuples="1" Name="'//trim(fname)//'" format="binary">'
    write(unit=vtk(rf)%u,iostat=E_IO)trim(s_buffer)//end_rec
    call pack_data(a1=[int(BYR4P,I4P)],a2=[fld],packed=fldp)
    call b64_encode(n=fldp,code=fld64) ; deallocate(fldp)
    write(unit=vtk(rf)%u,iostat=E_IO)repeat(' ',vtk(rf)%indent+2)//fld64//end_rec ; deallocate(fld64)
    write(unit=vtk(rf)%u,iostat=E_IO)repeat(' ',vtk(rf)%indent)//'</DataArray>'//end_rec
  endselect
  return
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction VTK_FLD_XML_R4

  function VTK_FLD_XML_I8(fld,fname,cf) result(E_IO)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Function for saving field data (global auxiliary data, e.g. time, step number, data set name...) (I8P).
  !---------------------------------------------------------------------------------------------------------------------------------
  implicit none
  integer(I8P), intent(IN)::           fld      !< Field data value.
  character(*), intent(IN)::           fname    !< Field data name.
  integer(I4P), intent(IN), optional:: cf       !< Current file index (for concurrent files IO).
  integer(I4P)::                       E_IO     !< Input/Output inquiring flag: $0$ if IO is done, $> 0$ if IO is not done.
  character(len=maxlen)::              s_buffer !< Buffer string.
  integer(I1P), allocatable::          fldp(:)  !< Packed field data.
  character(len=:), allocatable::      fld64    !< Field data encoded in base64.
  integer(I4P)::                       rf       !< Real file index.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  E_IO = -1_I4P
  rf = f
  if (present(cf)) then
    rf = cf ; f = cf
  endif
  select case(vtk(rf)%f)
  case(ascii)
    s_buffer = repeat(' ',vtk(rf)%indent)//'<DataArray type="Int64" NumberOfTuples="1" Name="'//trim(fname)//'" format="ascii">'// &
               trim(str(n=fld))//'</DataArray>'
    write(unit=vtk(rf)%u,fmt='(A)',iostat=E_IO)trim(s_buffer)
  case(raw,bin_app)
    s_buffer = repeat(' ',vtk(rf)%indent)//'<DataArray type="Int64" NumberOfTuples="1" Name="'//trim(fname)// &
               '" format="appended" offset="'//trim(str(.true.,vtk(rf)%ioffset))//'"/>'
    write(unit=vtk(rf)%u,iostat=E_IO)trim(s_buffer)//end_rec
    call vtk(rf)%byte_update(N_Byte = int(BYI8P,I4P))
    write(unit=vtk(rf)%ua,iostat=E_IO)vtk(rf)%N_Byte,'I8',1_I4P
    write(unit=vtk(rf)%ua,iostat=E_IO)fld
  case(binary)
    s_buffer=repeat(' ',vtk(rf)%indent)//'<DataArray type="Int64" NumberOfTuples="1" Name="'//trim(fname)//'" format="binary">'
    write(unit=vtk(rf)%u,iostat=E_IO)trim(s_buffer)//end_rec
    call pack_data(a1=[int(BYI8P,I4P)],a2=[fld],packed=fldp)
    call b64_encode(n=fldp,code=fld64) ; deallocate(fldp)
    write(unit=vtk(rf)%u,iostat=E_IO)repeat(' ',vtk(rf)%indent+2)//fld64//end_rec ; deallocate(fld64)
    write(unit=vtk(rf)%u,iostat=E_IO)repeat(' ',vtk(rf)%indent)//'</DataArray>'//end_rec
  endselect
  return
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction VTK_FLD_XML_I8

  function VTK_FLD_XML_I4(fld,fname,cf) result(E_IO)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Function for saving field data (global auxiliary data, e.g. time, step number, data set name...) (I4P).
  !---------------------------------------------------------------------------------------------------------------------------------
  implicit none
  integer(I4P), intent(IN)::           fld      !< Field data value.
  character(*), intent(IN)::           fname    !< Field data name.
  integer(I4P), intent(IN), optional:: cf       !< Current file index (for concurrent files IO).
  integer(I4P)::                       E_IO     !< Input/Output inquiring flag: $0$ if IO is done, $> 0$ if IO is not done.
  character(len=maxlen)::              s_buffer !< Buffer string.
  integer(I1P), allocatable::          fldp(:)  !< Packed field data.
  character(len=:), allocatable::      fld64    !< Field data encoded in base64.
  integer(I4P)::                       rf       !< Real file index.
  integer(I8P)::                       Nfldp    !< Dimension of fldp, packed data.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  E_IO = -1_I4P
  rf = f
  if (present(cf)) then
    rf = cf ; f = cf
  endif
  select case(vtk(rf)%f)
  case(ascii)
    s_buffer = repeat(' ',vtk(rf)%indent)//'<DataArray type="Int32" NumberOfTuples="1" Name="'//trim(fname)//'" format="ascii">'// &
               trim(str(n=fld))//'</DataArray>'
    write(unit=vtk(rf)%u,fmt='(A)',iostat=E_IO)trim(s_buffer)
  case(raw,bin_app)
    s_buffer = repeat(' ',vtk(rf)%indent)//'<DataArray type="Int32" NumberOfTuples="1" Name="'//trim(fname)// &
               '" format="appended" offset="'//trim(str(.true.,vtk(rf)%ioffset))//'"/>'
    write(unit=vtk(rf)%u,iostat=E_IO)trim(s_buffer)//end_rec
    call vtk(rf)%byte_update(N_Byte = int(BYI4P,I4P))
    write(unit=vtk(rf)%ua,iostat=E_IO)vtk(rf)%N_Byte,'I4',1_I4P
    write(unit=vtk(rf)%ua,iostat=E_IO)fld
  case(binary)
    s_buffer=repeat(' ',vtk(rf)%indent)//'<DataArray type="Int32" NumberOfTuples="1" Name="'//trim(fname)//'" format="binary">'
    write(unit=vtk(rf)%u,iostat=E_IO)trim(s_buffer)//end_rec
    Nfldp=size(transfer([int(BYI4P,I4P),fld],fldp),kind=I8P) ; if (allocated(fldp)) deallocate(fldp) ; allocate(fldp(1:Nfldp))
    fldp = transfer([int(BYI4P,I4P),fld],fldp)
    call b64_encode(n=fldp,code=fld64) ; deallocate(fldp)
    write(unit=vtk(rf)%u,iostat=E_IO)repeat(' ',vtk(rf)%indent+2)//fld64//end_rec ; deallocate(fld64)
    write(unit=vtk(rf)%u,iostat=E_IO)repeat(' ',vtk(rf)%indent)//'</DataArray>'//end_rec
  endselect
  return
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction VTK_FLD_XML_I4

  function VTK_FLD_XML_I2(fld,fname,cf) result(E_IO)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Function for saving field data (global auxiliary data, e.g. time, step number, data set name...) (I2P).
  !---------------------------------------------------------------------------------------------------------------------------------
  implicit none
  integer(I2P), intent(IN)::           fld      !< Field data value.
  character(*), intent(IN)::           fname    !< Field data name.
  integer(I4P), intent(IN), optional:: cf       !< Current file index (for concurrent files IO).
  integer(I4P)::                       E_IO     !< Input/Output inquiring flag: $0$ if IO is done, $> 0$ if IO is not done.
  character(len=maxlen)::              s_buffer !< Buffer string.
  integer(I1P), allocatable::          fldp(:)  !< Packed field data.
  character(len=:), allocatable::      fld64    !< Field data encoded in base64.
  integer(I4P)::                       rf       !< Real file index.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  E_IO = -1_I4P
  rf = f
  if (present(cf)) then
    rf = cf ; f = cf
  endif
  select case(vtk(rf)%f)
  case(ascii)
    s_buffer = repeat(' ',vtk(rf)%indent)//'<DataArray type="Int16" NumberOfTuples="1" Name="'//trim(fname)//'" format="ascii">'// &
               trim(str(n=fld))//'</DataArray>'
    write(unit=vtk(rf)%u,fmt='(A)',iostat=E_IO)trim(s_buffer)
  case(raw,bin_app)
    s_buffer = repeat(' ',vtk(rf)%indent)//'<DataArray type="Int16" NumberOfTuples="1" Name="'//trim(fname)// &
               '" format="appended" offset="'//trim(str(.true.,vtk(rf)%ioffset))//'"/>'
    write(unit=vtk(rf)%u,iostat=E_IO)trim(s_buffer)//end_rec
    call vtk(rf)%byte_update(N_Byte = int(BYI2P,I4P))
    write(unit=vtk(rf)%ua,iostat=E_IO)vtk(rf)%N_Byte,'I2',1_I4P
    write(unit=vtk(rf)%ua,iostat=E_IO)fld
  case(binary)
    s_buffer=repeat(' ',vtk(rf)%indent)//'<DataArray type="Int16" NumberOfTuples="1" Name="'//trim(fname)//'" format="binary">'
    write(unit=vtk(rf)%u,iostat=E_IO)trim(s_buffer)//end_rec
    call pack_data(a1=[int(BYI2P,I4P)],a2=[fld],packed=fldp)
    call b64_encode(n=fldp,code=fld64) ; deallocate(fldp)
    write(unit=vtk(rf)%u,iostat=E_IO)repeat(' ',vtk(rf)%indent+2)//fld64//end_rec ; deallocate(fld64)
    write(unit=vtk(rf)%u,iostat=E_IO)repeat(' ',vtk(rf)%indent)//'</DataArray>'//end_rec
  endselect
  return
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction VTK_FLD_XML_I2

  function VTK_FLD_XML_I1(fld,fname,cf) result(E_IO)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Function for saving field data (global auxiliary data, e.g. time, step number, data set name...) (I1P).
  !---------------------------------------------------------------------------------------------------------------------------------
  implicit none
  integer(I1P), intent(IN)::           fld      !< Field data value.
  character(*), intent(IN)::           fname    !< Field data name.
  integer(I4P), intent(IN), optional:: cf       !< Current file index (for concurrent files IO).
  integer(I4P)::                       E_IO     !< Input/Output inquiring flag: $0$ if IO is done, $> 0$ if IO is not done.
  character(len=maxlen)::              s_buffer !< Buffer string.
  integer(I1P), allocatable::          fldp(:)  !< Packed field data.
  character(len=:), allocatable::      fld64    !< Field data encoded in base64.
  integer(I4P)::                       rf       !< Real file index.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  E_IO = -1_I4P
  rf = f
  if (present(cf)) then
    rf = cf ; f = cf
  endif
  select case(vtk(rf)%f)
  case(ascii)
    s_buffer = repeat(' ',vtk(rf)%indent)//'<DataArray type="Int8" NumberOfTuples="1" Name="'//trim(fname)//'" format="ascii">'// &
               trim(str(n=fld))//'</DataArray>'
    write(unit=vtk(rf)%u,fmt='(A)',iostat=E_IO)trim(s_buffer)
  case(raw,bin_app)
    s_buffer = repeat(' ',vtk(rf)%indent)//'<DataArray type="Int8" NumberOfTuples="1" Name="'//trim(fname)// &
               '" format="appended" offset="'//trim(str(.true.,vtk(rf)%ioffset))//'"/>'
    write(unit=vtk(rf)%u,iostat=E_IO)trim(s_buffer)//end_rec
    call vtk(rf)%byte_update(N_Byte = int(BYI1P,I4P))
    write(unit=vtk(rf)%ua,iostat=E_IO)vtk(rf)%N_Byte,'I1',1_I4P
    write(unit=vtk(rf)%ua,iostat=E_IO)fld
  case(binary)
    s_buffer=repeat(' ',vtk(rf)%indent)//'<DataArray type="Int8" NumberOfTuples="1" Name="'//trim(fname)//'" format="binary">'
    write(unit=vtk(rf)%u,iostat=E_IO)trim(s_buffer)//end_rec
    call pack_data(a1=[int(BYI1P,I4P)],a2=[fld],packed=fldp)
    call b64_encode(n=fldp,code=fld64) ; deallocate(fldp)
    write(unit=vtk(rf)%u,iostat=E_IO)repeat(' ',vtk(rf)%indent+2)//fld64//end_rec ; deallocate(fld64)
    write(unit=vtk(rf)%u,iostat=E_IO)repeat(' ',vtk(rf)%indent)//'</DataArray>'//end_rec
  endselect
  return
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction VTK_FLD_XML_I1
endmodule Lib_VTK_IO_FLD_XML
