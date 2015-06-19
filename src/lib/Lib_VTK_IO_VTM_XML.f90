!< VTM_XML interface definitions for Lib_VTK_IO.
module Lib_VTK_IO_VTM_XML
!-----------------------------------------------------------------------------------------------------------------------------------
!< VTM_XML interface definitions for Lib_VTK_IO.
!-----------------------------------------------------------------------------------------------------------------------------------
USE IR_Precision        ! Integers and reals precision definition.
USE Lib_Base64          ! Base64 encoding/decoding procedures.
USE Lib_VTK_IO_Back_End ! Lib_VTK_IO back end module.
!-----------------------------------------------------------------------------------------------------------------------------------

!-----------------------------------------------------------------------------------------------------------------------------------
implicit none
private
save
public:: VTM_INI_XML
public:: VTM_BLK_XML
public:: VTM_WRF_XML
public:: VTM_END_XML
!-----------------------------------------------------------------------------------------------------------------------------------

!-----------------------------------------------------------------------------------------------------------------------------------
interface VTM_WRF_XML
  !< Procedure for saving the list of VTK-XML wrapped files by a mutliblock VTM file.
  !<
  !< VTK_WRF_XML is an interface to 2 different functions, one for files list passed as an array and one for files list passed
  !< a single string. If a single string is used, the delimiter of each file can be customized, while the default values is '&'.
  !<### Examples of usage
  !<
  !<#### Example with array files list: 3 files block with default delimiter
  !<```fortran
  !< E_IO = VTK_WRF_XML(flist=['file_1.vts','file_2.vts','file_3.vtu'])
  !<```
  !<#### Example with single string files list: 3 files block with default delimiter
  !<```fortran
  !< E_IO = VTK_WRF_XML(flist='file_1.vts&file_2.vts&file_3.vtu')
  !<```
  !<#### Example with single string files list: 2 files block with custom delimiter (!!)
  !<```fortran
  !< E_IO = VTK_WRF_XML(flist='file_1.vts!!file_2.vts',delimiter='!!')
  !<```
  module procedure VTM_WRF_XML_array,VTM_WRF_XML_string
endinterface
!-----------------------------------------------------------------------------------------------------------------------------------
contains
  function VTM_INI_XML(filename) result(E_IO)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Function for initializing a VTM (VTK Multiblocks) XML file that is a wrapper to a set of VTK-XML files.
  !---------------------------------------------------------------------------------------------------------------------------------
  implicit none
  character(*), intent(IN):: filename !< File name of output VTM file.
  integer(I4P)::             E_IO     !< Input/Output inquiring flag: $0$ if IO is done, $> 0$ if IO is not done.
  character(len=maxlen)::    s_buffer !< Buffer string.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  E_IO = -1_I4P
  if (.not.ir_initialized) call IR_Init
  if (.not.b64_initialized) call b64_init
  if (endian==endianL) then
    s_buffer='<VTKFile type="vtkMultiBlockDataSet" version="1.0" byte_order="LittleEndian">'
  else
    s_buffer='<VTKFile type="vtkMultiBlockDataSet" version="1.0" byte_order="BigEndian">'
  endif
  open(unit=Get_Unit(vtm%u),file=trim(filename),form='FORMATTED',access='SEQUENTIAL',action='WRITE',status='REPLACE',iostat=E_IO)
  write(unit=vtm%u,fmt='(A)',iostat=E_IO)'<?xml version="1.0"?>'
  write(unit=vtm%u,fmt='(A)',iostat=E_IO)trim(s_buffer) ; vtm%indent = 2
  write(unit=vtm%u,fmt='(A)',iostat=E_IO)repeat(' ',vtm%indent)//'<vtkMultiBlockDataSet>' ; vtm%indent = vtm%indent + 2
  vtm%blk = -1
  return
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction VTM_INI_XML

  function VTM_BLK_XML(block_action,name) result(E_IO)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Function for opening or closing a block level of a VTM file.
  !---------------------------------------------------------------------------------------------------------------------------------
  implicit none
  character(*),           intent(IN):: block_action !< Block action: OPEN or CLOSE block.
  character(*), optional, intent(IN):: name         !< Block name.
  integer(I4P)::                       E_IO         !< Input/Output inquiring flag: $0$ if IO is done, $> 0$ if IO is not done.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  E_IO = -1_I4P
  select case(trim(Upper_Case(block_action)))
  case('OPEN')
    vtm%blk = vtm%blk + 1
    if (present(name)) then
      write(unit=vtm%u,fmt='(A)',iostat=E_IO)repeat(' ',vtm%indent)//                                     &
                                             '<Block index="'//trim(str(.true.,(vtm%blk(1)+vtm%blk(2))))//&
                                             '" name="'//trim(name)//'">'
    else
      write(unit=vtm%u,fmt='(A)',iostat=E_IO)repeat(' ',vtm%indent)//&
                                             '<Block index="'//trim(str(.true.,(vtm%blk(1)+vtm%blk(2))))//'">'
    endif
    vtm%indent = vtm%indent + 2
  case('CLOSE')
    vtm%indent = vtm%indent - 2 ; write(unit=vtm%u,fmt='(A)',iostat=E_IO)repeat(' ',vtm%indent)//'</Block>'
    vtm%blk(2) = -1
  endselect
  return
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction VTM_BLK_XML

  function VTM_WRF_XML_array(nlist,flist) result(E_IO)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Function for saving the list of VTK-XML wrapped files by the actual block of the mutliblock VTM file.
  !<
  !< @note the list is passed as an array.
  !<
  !<#### Example of usage: 3 files blocks
  !<```fortran
  !< E_IO = VTK_WRF_XML(flist=['file_1.vts','file_2.vts','file_3.vtu'])
  !<```
  !<
  !<#### Example of usage: 3 files blocks with custom name
  !<```fortran
  !< E_IO = VTK_WRF_XML(flist=['file_1.vts','file_2.vts','file_3.vtu'],&
  !<                    nlist=['block-bar','block-foo','block-baz'])
  !<```
  !---------------------------------------------------------------------------------------------------------------------------------
  implicit none
  character(*), optional, intent(IN):: nlist(:) !< List names attributed to wrapped files.
  character(*),           intent(IN):: flist(:) !< List of VTK-XML wrapped files.
  integer(I4P)::                       E_IO     !< Input/Output inquiring flag: 0 if IO is done, > 0 if IO is not done.
  integer(I4P)::                       f        !< File counter.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  E_IO = -1_I4P
  if (present(nlist)) then
    if (size(nlist) == size(flist)) then
      do f=1,size(flist)
        write(unit=vtm%u,fmt='(A)',iostat=E_IO)repeat(' ',vtm%indent)//'<DataSet index="'//trim(str(.true.,f-1))//'" file="'// &
                                               trim(adjustl(flist(f)))//'" name="'//trim(adjustl(nlist(f)))//'"/>'
      enddo
    endif
  else
    do f=1,size(flist)
      write(unit=vtm%u,fmt='(A)',iostat=E_IO)repeat(' ',vtm%indent)//'<DataSet index="'//trim(str(.true.,f-1))//'" file="'// &
                                             trim(adjustl(flist(f)))//'"/>'
    enddo
  endif
  return
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction VTM_WRF_XML_array

  function VTM_WRF_XML_string(delimiter,nlist,flist) result(E_IO)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Function for saving the list of VTK-XML wrapped files by the actual block of the mutliblock VTM file.
  !<
  !< @note the list is passed as a single string. The delimiter of each file can be customized: default value is "&". For supporting
  !< compiler with not varying string support the maximum delimiter length is fixed to 50.
  !<
  !<### Examples of usage
  !<
  !<#### Example: 3 files block with default delimiter
  !<```fortran
  !< E_IO = VTK_WRF_XML(flist='file_1.vts&file_2.vts&file_3.vtu')
  !<```
  !<
  !<#### Example: 3 files block with custom name
  !<```fortran
  !< E_IO = VTK_WRF_XML(flist='file_1.vts&file_2.vts&file_3.vtu',&
  !<                    nlist='foo&bar&baz')
  !<```
  !<
  !<#### Example: 2 files block with custom delimiter (!!)
  !<```fortran
  !< E_IO = VTK_WRF_XML(flist='file_1.vts!!file_2.vts',delimiter='!!')
  !<```
  !---------------------------------------------------------------------------------------------------------------------------------
  implicit none
  character(*), optional, intent(IN):: delimiter !< Delimiter of files into files list string.
  character(*), optional, intent(IN):: nlist     !< List names attributed to wrapped files.
  character(*),           intent(IN):: flist     !< List of VTK-XML wrapped files.
  integer(I4P)::                       E_IO      !< Input/Output inquiring flag: 0 if IO is done, > 0 if IO is not done.
  integer(I4P)::                       f         !< File counter.
  character(50)::                      delimit   !< Delimiter value.
  character(len(flist))::              flistd    !< Dummy files list.
  character(len(flist))::              nlistd    !< Dummy names list.
  character(len(flist))::              dummy(1:2)!< Dummy strings.
  integer(I4P)::                       d_len     !< Delimiter character length.
  integer(I4P)::                       i,n       !< Counters.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  E_IO = -1_I4P
  delimit = '&' ; if (present(delimiter)) delimit = delimiter ; d_len = len_trim(delimit)
  flistd = flist
  if (present(nlist)) nlistd = nlist
  if (len_trim(flistd)<=d_len) return ! no list to save
  ! purging out leading and trailing delimeters
  if (flistd(1:d_len)==trim(delimit)) flistd = flistd(d_len+1:)
  if (flistd(len_trim(flistd)-d_len:)==trim(delimit)) flistd = flistd(1:len_trim(flistd)-d_len-1)
  if (present(nlist)) then
    if (nlistd(1:d_len)==trim(delimit)) nlistd = nlistd(d_len+1:)
    if (nlistd(len_trim(nlistd)-d_len:)==trim(delimit)) nlistd = nlistd(1:len_trim(nlistd)-d_len-1)
  endif
  f = -1
  do while(len_trim(flistd)>0)
    f = f + 1
    i = index(flistd,trim(delimit))
    if (i>0) then
      dummy(1) = trim(adjustl(flistd(1:i-1)))
      flistd = trim(flistd(i+1:))
    elseif (len_trim(flistd)>0) then
      dummy(1) = trim(adjustl(flistd))
      flistd = ''
    else
      exit
    endif
    if (present(nlist)) then
      n = index(nlistd,trim(delimit))
      if (n>0) then
        dummy(2) = trim(adjustl(nlistd(1:n-1)))
        nlistd = trim(nlistd(n+1:))
      else
        dummy(2) = trim(adjustl(nlistd))
        nlistd = ''
      endif
    endif
    if (present(nlist)) then
      write(unit=vtm%u,fmt='(A)',iostat=E_IO)repeat(' ',vtm%indent)//'<DataSet index="'//trim(str(.true.,f))//'" file="'// &
                                             trim(adjustl(dummy(1)))//'" name="'//trim(adjustl(dummy(2)))//'"/>'
    else
      write(unit=vtm%u,fmt='(A)',iostat=E_IO)repeat(' ',vtm%indent)//'<DataSet index="'//trim(str(.true.,f))//'" file="'// &
                                             trim(adjustl(dummy(1)))//'"/>'
    endif
  enddo
  return
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction VTM_WRF_XML_string

  function VTM_END_XML() result(E_IO)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Function for finalizing the VTM-XML file.
  !---------------------------------------------------------------------------------------------------------------------------------
  implicit none
  integer(I4P):: E_IO !< Input/Output inquiring flag: $0$ if IO is done, $> 0$ if IO is not done.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  E_IO = -1_I4P
  vtm%indent = vtm%indent - 2
  write(unit=vtm%u,fmt='(A)',iostat=E_IO)repeat(' ',vtm%indent)//'</vtkMultiBlockDataSet>'
  write(unit=vtm%u,fmt='(A)',iostat=E_IO)'</VTKFile>'
  close(unit=vtm%u)
  return
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction VTM_END_XML
endmodule Lib_VTK_IO_VTM_XML
