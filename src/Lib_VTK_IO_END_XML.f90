!< END_XML interface definition for Lib_VTK_IO.
module Lib_VTK_IO_END_XML
!-----------------------------------------------------------------------------------------------------------------------------------
!< END_XML interface definition for Lib_VTK_IO.
!-----------------------------------------------------------------------------------------------------------------------------------
USE IR_Precision        ! Integers and reals precision definition.
USE Lib_Base64          ! Base64 encoding/decoding procedures.
USE Lib_VTK_IO_Back_End ! Lib_VTK_IO back end module.
!-----------------------------------------------------------------------------------------------------------------------------------

!-----------------------------------------------------------------------------------------------------------------------------------
implicit none
private
save
public:: VTK_END_XML
!-----------------------------------------------------------------------------------------------------------------------------------
contains
  function VTK_END_XML(cf) result(E_IO)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Function for finalizing the VTK-XML file.
  !<
  !<### Usage
  !<```fortran
  !< E_IO = VTK_END_XML()
  !<```
  !---------------------------------------------------------------------------------------------------------------------------------
  implicit none
  integer(I4P), intent(INOUT), optional:: cf       !< Current file index (for concurrent files IO).
  integer(I4P)::                          E_IO     !< Input/Output inquiring flag: $0$ if IO is done, $> 0$ if IO is not done.
  character(2)::                          var_type !< Varable type = R8,R4,I8,I4,I2,I1.
  real(R8P),        allocatable::         v_R8(:)  !< R8 vector for IO in AppendData.
  real(R4P),        allocatable::         v_R4(:)  !< R4 vector for IO in AppendData.
  integer(I8P),     allocatable::         v_I8(:)  !< I8 vector for IO in AppendData.
  integer(I4P),     allocatable::         v_I4(:)  !< I4 vector for IO in AppendData.
  integer(I2P),     allocatable::         v_I2(:)  !< I2 vector for IO in AppendData.
  integer(I1P),     allocatable::         v_I1(:)  !< I1 vector for IO in AppendData.
  integer(I1P),     allocatable::         varp(:)  !< Packed data.
  character(len=:), allocatable::         var64    !< Variable encoded in base64.
  integer(I4P)::                          rf       !< Real file index.
  integer(I8P)::                          Nvarp    !< Dimension of varp, packed data.
#ifdef HUGE
  integer(I8P)::                          N_v      !< Vector dimension.
  integer(I8P)::                          n1       !< Counter.
#else
  integer(I4P)::                          N_v      !< Vector dimension.
  integer(I4P)::                          n1       !< Counter.
#endif
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  E_IO = -1_I4P
  rf = f
  if (present(cf)) then
    rf = cf ; f = cf
  endif
  select case(vtk(rf)%f)
  case(ascii)
    vtk(rf)%indent = vtk(rf)%indent - 2
    write(unit=vtk(rf)%u,fmt='(A)',iostat=E_IO)repeat(' ',vtk(rf)%indent)//'</'//trim(vtk(rf)%topology)//'>'
    write(unit=vtk(rf)%u,fmt='(A)',iostat=E_IO)'</VTKFile>'
  case(raw,bin_app)
    vtk(rf)%indent = vtk(rf)%indent - 2
    write(unit  =vtk(rf)%u, iostat=E_IO)repeat(' ',vtk(rf)%indent)//'</'//trim(vtk(rf)%topology)//'>'//end_rec
    if (vtk(rf)%f==raw) then
      write(unit  =vtk(rf)%u, iostat=E_IO)repeat(' ',vtk(rf)%indent)//'<AppendedData encoding="raw">'//end_rec
    else
      write(unit  =vtk(rf)%u, iostat=E_IO)repeat(' ',vtk(rf)%indent)//'<AppendedData encoding="base64">'//end_rec
    endif
    write(unit  =vtk(rf)%u, iostat=E_IO)'_'
    endfile(unit=vtk(rf)%ua,iostat=E_IO)
    rewind(unit =vtk(rf)%ua,iostat=E_IO)
    do
      read(unit=vtk(rf)%ua,iostat=E_IO,end=100)vtk(rf)%N_Byte,var_type,N_v
      select case(var_type)
      case('R8')
        allocate(v_R8(1:N_v))
        read(unit =vtk(rf)%ua,iostat=E_IO)(v_R8(n1),n1=1,N_v)
        if (vtk(rf)%f==raw) then
          write(unit=vtk(rf)%u,iostat=E_IO)int(vtk(rf)%N_Byte,I4P),(v_R8(n1),n1=1,N_v)
        else
          call pack_data(a1=[int(vtk(rf)%N_Byte,I4P)],a2=v_R8,packed=varp)
          call b64_encode(n=varp,code=var64) ; deallocate(varp)
          write(unit=vtk(rf)%u,iostat=E_IO)var64 ; deallocate(var64)
        endif
        deallocate(v_R8)
      case('R4')
        allocate(v_R4(1:N_v))
        read(unit =vtk(rf)%ua,iostat=E_IO)(v_R4(n1),n1=1,N_v)
        if (vtk(rf)%f==raw) then
          write(unit=vtk(rf)%u,iostat=E_IO)int(vtk(rf)%N_Byte,I4P),(v_R4(n1),n1=1,N_v)
        else
          call pack_data(a1=[int(vtk(rf)%N_Byte,I4P)],a2=v_R4,packed=varp)
          call b64_encode(n=varp,code=var64) ; deallocate(varp)
          write(unit=vtk(rf)%u,iostat=E_IO)var64 ; deallocate(var64)
        endif
        deallocate(v_R4)
      case('I8')
        allocate(v_I8(1:N_v))
        read(unit =vtk(rf)%ua,iostat=E_IO)(v_I8(n1),n1=1,N_v)
        if (vtk(rf)%f==raw) then
          write(unit=vtk(rf)%u,iostat=E_IO)int(vtk(rf)%N_Byte,I4P),(v_I8(n1),n1=1,N_v)
        else
          call pack_data(a1=[int(vtk(rf)%N_Byte,I4P)],a2=v_I8,packed=varp)
          call b64_encode(n=varp,code=var64) ; deallocate(varp)
          write(unit=vtk(rf)%u,iostat=E_IO)var64 ; deallocate(var64)
        endif
        deallocate(v_I8)
      case('I4')
        allocate(v_I4(1:N_v))
        read(unit =vtk(rf)%ua,iostat=E_IO)(v_I4(n1),n1=1,N_v)
        if (vtk(rf)%f==raw) then
          write(unit=vtk(rf)%u,iostat=E_IO)int(vtk(rf)%N_Byte,I4P),(v_I4(n1),n1=1,N_v)
        else
          Nvarp=size(transfer([int(vtk(rf)%N_Byte,I4P),v_I4],varp),kind=I8P)
          if (allocated(varp)) deallocate(varp); allocate(varp(1:Nvarp))
          varp = transfer([int(vtk(rf)%N_Byte,I4P),v_I4],varp)
          call b64_encode(n=varp,code=var64) ; deallocate(varp)
          write(unit=vtk(rf)%u,iostat=E_IO)var64 ; deallocate(var64)
        endif
        deallocate(v_I4)
      case('I2')
        allocate(v_I2(1:N_v))
        read(unit =vtk(rf)%ua,iostat=E_IO)(v_I2(n1),n1=1,N_v)
        if (vtk(rf)%f==raw) then
          write(unit=vtk(rf)%u,iostat=E_IO)int(vtk(rf)%N_Byte,I4P),(v_I2(n1),n1=1,N_v)
        else
          call pack_data(a1=[int(vtk(rf)%N_Byte,I4P)],a2=v_I2,packed=varp)
          call b64_encode(n=varp,code=var64) ; deallocate(varp)
          write(unit=vtk(rf)%u,iostat=E_IO)var64 ; deallocate(var64)
        endif
        deallocate(v_I2)
      case('I1')
        allocate(v_I1(1:N_v))
        read(unit =vtk(rf)%ua,iostat=E_IO)(v_I1(n1),n1=1,N_v)
        if (vtk(rf)%f==raw) then
          write(unit=vtk(rf)%u,iostat=E_IO)int(vtk(rf)%N_Byte,I4P),(v_I1(n1),n1=1,N_v)
        else
          call pack_data(a1=[int(vtk(rf)%N_Byte,I4P)],a2=v_I1,packed=varp)
          call b64_encode(n=varp,code=var64) ; deallocate(varp)
          write(unit=vtk(rf)%u,iostat=E_IO)var64 ; deallocate(var64)
        endif
        deallocate(v_I1)
      case default
        E_IO = 1
        write (stderr,'(A)')' bad var_type = '//var_type
        write (stderr,'(A)')' N_Byte = '//trim(str(n=vtk(rf)%N_Byte))//' N_v = '//trim(str(n=N_v))
        return
      endselect
    enddo
    100 continue
    write(unit=vtk(rf)%u,iostat=E_IO)end_rec
    write(unit=vtk(rf)%u,iostat=E_IO)repeat(' ',vtk(rf)%indent)//'</AppendedData>'//end_rec
    write(unit=vtk(rf)%u,iostat=E_IO)'</VTKFile>'//end_rec
    close(unit=vtk(rf)%ua,iostat=E_IO)
  case(binary)
    vtk(rf)%indent = vtk(rf)%indent - 2
    write(unit=vtk(rf)%u,iostat=E_IO)repeat(' ',vtk(rf)%indent)//'</'//trim(vtk(rf)%topology)//'>'//end_rec
    write(unit=vtk(rf)%u,iostat=E_IO)'</VTKFile>'//end_rec
  endselect
  close(unit=vtk(rf)%u,iostat=E_IO)
  call vtk_update(act='remove',cf=rf,Nvtk=Nvtk,vtk=vtk)
  f = rf
  if (present(cf)) cf = rf
  return
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction VTK_END_XML
endmodule Lib_VTK_IO_END_XML
