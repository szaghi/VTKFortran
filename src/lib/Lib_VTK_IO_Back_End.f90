!< Back-end module for Lib_VTK_IO.
module Lib_VTK_IO_Back_End
!-----------------------------------------------------------------------------------------------------------------------------------
!< Back-end module for Lib_VTK_IO.
!-----------------------------------------------------------------------------------------------------------------------------------
USE IR_Precision                                                                ! Integers and reals precision definition.
USE, intrinsic:: ISO_FORTRAN_ENV, only: stdout=>OUTPUT_UNIT, stderr=>ERROR_UNIT ! Standard output/error logical units.
!-----------------------------------------------------------------------------------------------------------------------------------

!-----------------------------------------------------------------------------------------------------------------------------------
implicit none
private
save
public:: stdout
public:: stderr
public:: maxlen
public:: end_rec
public:: ascii
public:: binary
public:: raw
public:: bin_app
public:: vtk
public:: Nvtk
public:: f
public:: vtm
public:: Get_Unit
public:: Upper_Case
public:: byte_update
public:: vtk_update
public:: adjustlt
public:: get_int
public:: get_char
public:: read_record
public:: move
public:: search
!-----------------------------------------------------------------------------------------------------------------------------------

!-----------------------------------------------------------------------------------------------------------------------------------
integer(I4P), parameter:: maxlen  = 500      !< Max number of characters of static string.
character(1), parameter:: end_rec = char(10) !< End-character for binary-record finalize.
integer(I4P), parameter:: ascii   = 0        !< Ascii-output-format parameter identifier.
integer(I4P), parameter:: binary  = 1        !< Base64-output-format parameter identifier.
integer(I4P), parameter:: raw     = 2        !< Raw-appended-binary-output-format parameter identifier.
integer(I4P), parameter:: bin_app = 3        !< Base64-appended-output-format parameter identifier.

type:: Type_VTK_File
  !< Derived type for handling VTK files.
  !<
  !< @note The OOP encapsulation allows safe use of parallel paradigms.
  integer(I4P)::          f        = ascii !< Current output-format (initialized to ascii format).
  character(len=maxlen):: topology = ''    !< Mesh topology.
  integer(I4P)::          u        = 0_I4P !< Logical unit.
  integer(I4P)::          ua       = 0_I4P !< Logical unit for raw binary XML append file.
#ifdef HUGE
  integer(I8P)::          N_Byte   = 0_I8P !< Number of byte to be written/read.
#else
  integer(I4P)::          N_Byte   = 0_I4P !< Number of byte to be written/read.
#endif
  integer(I8P)::          ioffset  = 0_I8P !< Offset pointer.
  integer(I4P)::          indent   = 0_I4P !< Indent pointer.
  contains
    procedure:: byte_update !< Procedure for updating N_Byte and ioffset pointer.
endtype Type_VTK_File
type(Type_VTK_File), allocatable:: vtk(:)       !< Global data of VTK files [1:Nvtk].
integer(I4P)::                     Nvtk = 0_I4P !< Number of (concurrent) VTK files.
integer(I4P)::                     f    = 0_I4P !< Current VTK file index.

type:: Type_VTM_File
  !< Derived type for handling VTM files.
  integer(I4P):: u        = 0_I4P         !< Logical unit.
  integer(I4P):: blk(1:2) = [0_I4P,0_I4P] !< Block indexes.
  integer(I4P):: indent   = 0_I4P         !< Indent pointer.
endtype Type_VTM_File
type(Type_VTM_File):: vtm !< Global data of VTM files.
!-----------------------------------------------------------------------------------------------------------------------------------
contains
  function Get_Unit(Free_Unit) result(funit)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Procedure for obtaining a free logic unit for safely opening a file.
  !<
  !< @note If no units are available, -1 is returned.
  !<
  !<### On-the-fly usage
  !< The unit value is returned by the function and also by the optional argument *Free_Unit*. This allows the function to
  !< be used directly (on-the-fly) in an open statement like
  !<```fortran
  !< open(unit=Get_Unit(myunit),...) ; read(myunit)...
  !<```
  !---------------------------------------------------------------------------------------------------------------------------------
  implicit none
  integer::                        funit     !< Free logic unit.
  integer, intent(OUT), optional:: Free_Unit !< Free logic unit.
  integer::                        n1        !< Counter.
  integer::                        ios       !< Inquiring flag.
  logical::                        lopen     !< Inquiring flag.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  funit = -1
  if (present(Free_Unit)) Free_Unit = funit
  n1=1
  do
    if ((n1/=stdout).AND.(n1/=stderr)) then
      inquire (unit=n1,opened=lopen,iostat=ios)
      if (ios==0) then
        if (.NOT.lopen) then
          funit = n1 ; if (present(Free_Unit)) Free_Unit = funit
          return
        endif
      endif
    endif
    n1=n1+1
  enddo
  return
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction Get_Unit

  elemental function Upper_Case(string)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Function for converting lower case characters of a string to upper case ones.
  !<
  !< The library uses this function in order to achieve case-insensitivty: all character variables used within the libary functions
  !< are pre-processed by Uppper_Case function before these variables are used. So the users can call the library functions without
  !< pay attention of the case of the keywords passed to the functions: calling the function VTK_INI with the string
  !< `E_IO = VTK_INI('Ascii',...)` is equivalent to `E_IO = VTK_INI('ASCII',...)`.
  !---------------------------------------------------------------------------------------------------------------------------------
  implicit none
  character(len=*), intent(IN):: string     !< String to be converted.
  character(len=len(string))::   Upper_Case !< Converted string.
  integer::                      n1         !< Characters counter.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  Upper_Case = string
  do n1=1,len(string)
    select case(ichar(string(n1:n1)))
    case(97:122)
      Upper_Case(n1:n1)=char(ichar(string(n1:n1))-32) ! Upper case conversion
    endselect
  enddo
  return
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction Upper_Case

  elemental subroutine byte_update(vtk,N_Byte)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Subroutine for updating N_Byte and ioffset pointer.
  !---------------------------------------------------------------------------------------------------------------------------------
  implicit none
  class(Type_VTK_File), intent(INOUT):: vtk    !< Global data of VTK file.
#ifdef HUGE
  integer(I8P),         intent(IN)::    N_Byte !< Number of bytes saved.
#else
  integer(I4P),         intent(IN)::    N_Byte !< Number of bytes saved.
#endif
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  vtk%N_Byte = N_Byte
  if (vtk%f==raw) then
#ifdef HUGE
    vtk%ioffset = vtk%ioffset + BYI8P + N_Byte
#else
    vtk%ioffset = vtk%ioffset + BYI4P + N_Byte
#endif
  else
#ifdef HUGE
    vtk%ioffset = vtk%ioffset + ((N_Byte + BYI8P + 2_I8P)/3_I8P)*4_I8P
#else
    vtk%ioffset = vtk%ioffset + ((N_Byte + BYI4P + 2_I4P)/3_I4P)*4_I4P
#endif
  endif
  return
  !---------------------------------------------------------------------------------------------------------------------------------
  endsubroutine byte_update

  pure subroutine vtk_update(act,cf,Nvtk,vtk)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Subroutine for updating (adding and removing elements into) vtk array.
  !---------------------------------------------------------------------------------------------------------------------------------
  implicit none
  character(*),                     intent(IN)::    act        !< Action: 'ADD' one more element, 'REMOVE' current element file.
  integer(I4P),                     intent(INOUT):: cf         !< Current file index (for concurrent files IO).
  integer(I4P),                     intent(INOUT):: Nvtk       !< Number of (concurrent) VTK files.
  type(Type_VTK_File), allocatable, intent(INOUT):: vtk(:)     !< VTK files data.
  type(Type_VTK_File), allocatable::                vtk_tmp(:) !< Temporary array of VTK files data.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  select case(Upper_Case(trim(act)))
  case('ADD')
    if (Nvtk>0_I4P) then
      allocate(vtk_tmp(1:Nvtk))
      vtk_tmp = vtk
      deallocate(vtk)
      Nvtk = Nvtk + 1
      allocate(vtk(1:Nvtk))
      vtk(1:Nvtk-1) = vtk_tmp
      deallocate(vtk_tmp)
      cf = Nvtk
    else
      Nvtk = 1_I4P
      allocate(vtk(1:Nvtk))
      cf = Nvtk
    endif
  case default
    if (Nvtk>1_I4P) then
      allocate(vtk_tmp(1:Nvtk-1))
      if (cf==Nvtk) then
        vtk_tmp = vtk(1:Nvtk-1)
      else
        vtk_tmp(1 :cf-1) = vtk(1   :cf-1)
        vtk_tmp(cf:    ) = vtk(cf+1:    )
      endif
      deallocate(vtk)
      Nvtk = Nvtk - 1
      allocate(vtk(1:Nvtk))
      vtk = vtk_tmp
      deallocate(vtk_tmp)
      cf = 1_I4P
    else
      Nvtk = 0_I4P
      if (allocated(vtk)) deallocate(vtk)
      cf = Nvtk
    endif
  endselect
  return
  !---------------------------------------------------------------------------------------------------------------------------------
  endsubroutine vtk_update

  elemental function adjustlt(string) result(res)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Extend adjustl to remove tab characters (char(9)).
  !---------------------------------------------------------------------------------------------------------------------------------
  character(len=*), intent(IN):: string !< Input string.
  character(len=len(string))::   res    !< Output string with leading tab characters or blanks removed.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  res = string
  if (len_trim(res)>0) then
    do while((res(1:1) == char(9) .or. res(1:1) == ' ') .and. len_trim(res)>0)
      res = res(2:)
    enddo
  endif
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction adjustlt

  subroutine get_int(case, E_IO, buffer, attrib, val)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Get integer value of attribute 'attrib' defined into buffer.
  !---------------------------------------------------------------------------------------------------------------------------------
  character(len=*), optional, intent(IN)::  case   !< Attribute string case.
  integer(I4P),     optional, intent(OUT):: E_IO   !< Error trapping flag.
  character(len=*),           intent(IN)::  buffer !< String where to search the attrib.
  character(len=*),           intent(IN)::  attrib !< XML attribute id.
  integer(I4P),               intent(OUT):: val    !< Returned integer value.
  integer::                                 pos(2) !< Position counter.
  integer::                                 E_IOD  !< Dummy error trapping flag.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  if (present(E_IO)) E_IO = -1_I4P
  if (present(case)) then
    if (trim(adjustlt(Upper_Case(case)))=='LOWER') then
      pos(1) = index(buffer, trim(adjustlt(attrib))//'="') + len_trim(adjustlt(attrib)) + 2
    else
      pos(1) = index(buffer, trim(adjustlt(Upper_Case(attrib)))//'="') + len_trim(adjustlt(attrib)) + 2
    endif
  else
    pos(1) = index(buffer, trim(adjustlt(Upper_Case(attrib)))//'="') + len_trim(adjustlt(attrib)) + 2
  endif
  if (pos(1) <= len_trim(adjustlt(attrib))+2) return
  pos(2) = index(buffer(pos(1):len_trim(buffer)), '"') + pos(1) - 2
  if (pos(2) < pos(1)) return
  read(buffer(pos(1):pos(2)),fmt=*,iostat=E_IOD) val
  if(present(E_IO)) E_IO = E_IOD
  !---------------------------------------------------------------------------------------------------------------------------------
  endsubroutine get_int

  subroutine get_char(case, E_IO, buffer, attrib, val)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Get character value of attribute 'attrib' defined into buffer.
  !---------------------------------------------------------------------------------------------------------------------------------
  character(len=*), optional,    intent(IN)::  case   !< Attribute string case.
  integer(I4P),     optional,    intent(OUT):: E_IO   !< Error trapping flag.
  character(len=*),              intent(IN)::  buffer !< String where to search the attrib
  character(len=*),              intent(IN)::  attrib !< XML attribute id
  character(len=:), allocatable, intent(OUT):: val    !< Returned string value
  integer::                                    pos(2) !< Position counter.
  integer::                                    E_IOD  !< Dummy error trapping flag.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  if (present(E_IO)) E_IO = -1_I4P
  if (present(case)) then
    if (trim(adjustlt(Upper_Case(case)))=='LOWER') then
      pos(1) = index(buffer, trim(adjustlt(attrib))//'="') + len_trim(adjustlt(attrib)) + 2
    else
      pos(1) = index(buffer, trim(adjustlt(Upper_Case(attrib)))//'="') + len_trim(adjustlt(attrib)) + 2
    endif
  else
    pos(1) = index(buffer, trim(adjustlt(Upper_Case(attrib)))//'="') + len_trim(adjustlt(attrib)) + 2
  endif
  if (pos(1) <= len_trim(adjustlt(attrib))+2) return
  pos(2) = index(buffer(pos(1):len_trim(buffer)), '"') + pos(1) - 2
  if (pos(2) < pos(1)) return
  allocate(character(pos(2)-pos(1)+1) :: val)
  read(buffer(pos(1):pos(2)),fmt='(a)',iostat=E_IOD) val
  if(present(E_IO)) E_IO = E_IOD
  !---------------------------------------------------------------------------------------------------------------------------------
  endsubroutine get_char

  function read_record(from, cf, buffer) result(E_IO)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< read_record: read characters in the unit 'vtk(rf)%u' from position 'from' to read string 'buffer'
  !< The read action stops when finding a EOR character (char(10))
  !---------------------------------------------------------------------------------------------------------------------------------
  integer(I4P),     optional,    intent(IN)::  from   !< Offset.
  integer(I4P),     optional,    intent(IN)::  cf     !< Current file index (for concurrent files IO).
  character(len=:), allocatable, intent(OUT):: buffer !< String containing the next record.
  integer(I4P)::                               rf     !< Real file index.
  integer(i4P)::                               E_IO   !< Error trapping flag.
  character::                                  c      !< Dummy character storage.
  integer::                                    n, p   !< Counters.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  E_IO = -1_I4P
  rf = f
  if (present(cf)) then
    rf = cf ; f = cf
  endif
  n = 1
  buffer = ''
  if (present(from)) then
    p = from
  else
    inquire(unit=vtk(rf)%u, iostat=E_IO, pos=p)
  endif
  read(unit=vtk(rf)%u, iostat=E_IO, pos=p) c
  do while (c /= end_rec)
    buffer = buffer//c
    n = n + 1
    read(unit=vtk(rf)%u, iostat=E_IO) c; if(E_IO /= 0) exit
  enddo
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction read_record

  function move(to_find, repeat, cf, upper, inside, buffer) result(E_IO)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Advance in VTK file inside the mark 'inside', until find the mark 'to_find', 'repeat' times.
  !---------------------------------------------------------------------------------------------------------------------------------
  character(len=*), optional,    intent(IN)::  to_find !< Searched XML element.
  integer,          optional,    intent(IN)::  repeat  !< Number of repetitions.
  integer(I4P),     optional,    intent(IN)::  cf      !< Current file index (for concurrent files IO).
  logical,          optional,    intent(IN)::  upper   !< True if return buffer in upper case.
  character(len=*),              intent(IN)::  inside  !< XML element where to search 'to_find'.
  character(len=:), allocatable, intent(OUT):: buffer  !< String.
  integer(I4P)::                               E_IO    !< Error trapping flag.
  character(len=:), allocatable::              buff    !< Auxiliary buffer.
  integer(I4P)::                               rf      !< Real file index.
  logical::                                    up      !< Readl upper case logical.
  integer(I4P)::                               n       !< Counter.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  rf = f
  if (present(cf)) then
    rf = cf ; f = cf
  endif

  up = .true.
  if (present(upper)) up = upper
  do !search the beginnig of the mark 'inside'
    E_IO = read_record(buffer=buffer, cf=rf); if(E_IO /= 0) exit
    if(.not. up) buff = buffer
    buffer = trim(adjustlt(Upper_Case(buffer)))
    if (index(buffer, '<'//trim(adjustlt(Upper_Case(inside)))) > 0) exit !Mark 'inside' founded once
  enddo

  if (E_IO == 0 .and. present(to_find)) then
    n = 1; if(present(repeat)) n = repeat
    do !search 'repeat' times the mark 'to_find'
      E_IO = read_record(buffer=buffer, cf=rf); if(E_IO /= 0) exit
      if(.not. up) buff = buffer
      buffer = trim(adjustlt(Upper_Case(buffer)))
      if (index(buffer, '</'//trim(adjustlt(Upper_Case(inside)))) > 0) exit
      if (index(buffer, '<'//trim(adjustlt(Upper_Case(to_find)))) > 0) n = n - 1 !Mark 'to_find' founded once
      if (n == 0) exit !Mark 'to_find' founded 'repeat' times
    enddo
    if(.not. up) buffer = buff
    if (n > 0 ) E_IO = -1_I4P
  endif
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction move

  function search(cf, from, content, inside, to_find, with_attribute, of_value, buffer) result(E_IO)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Search in VTK file from position 'pos' inside the mark 'inside', until find the mark 'to_find', eventually, having
  !< attribute 'with_attribute' matching the value 'of_value'.
  !---------------------------------------------------------------------------------------------------------------------------------
  integer(I4P),     optional,              intent(IN)::     cf             !< Current file index (for concurrent files IO).
  integer(I4P),     optional,              intent(IN)::     from           !< Offset. Start point.
  character(len=:), optional, allocatable, intent(OUT)::    content        !< String with the content inside 'to_find' element.
  character(len=*),                        intent(IN)::     inside         !< XML element where to search 'to_find'.
  character(len=*),                        intent(IN)::     to_find        !< Searched XML element.
  character(len=*),                        intent(IN)::     with_attribute !< XML attribute id.
  character(len=*),                        intent(IN)::     of_value       !< Attribute value.
  character(len=:),           allocatable, intent(INOUT)::  buffer         !< String.
  integer(I4P)::                                            rf             !< Real file index
  integer(I4P)::                                            E_IO           !< Error trapping flag.
  character(len=:), allocatable::                           strng          !< String.
  integer(I4P)::                                            pos            !< Positional counter.
  integer(I4P)::                                            p1,p2,p3       !< Positional counters.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  rf = f
  if (present(cf)) then
    rf = cf ; f = cf
  endif
  pos = 1; if (present(from)) pos = from
  if (present(content)) content = ''
  E_IO = read_record(buffer=buffer, from=pos, cf=rf); if(E_IO /= 0) return
  do !search the beginnig of the mark 'inside' from position 'pos'
    buffer = trim(adjustlt(Upper_Case(buffer)))
    if (index(buffer, '<'//trim(adjustlt(Upper_Case(inside)))) > 0) exit !Mark 'inside' founded once
    E_IO = read_record(buffer=buffer); if(E_IO /= 0) exit
  enddo
  if (E_IO == 0) then
    do !search 'repeat' times the mark 'to_find'
      E_IO = read_record(buffer=buffer, cf=rf); if(E_IO /= 0) exit
      buffer = trim(adjustlt(Upper_Case(buffer)))
      if (index(buffer, '</'//trim(adjustlt(Upper_Case(inside)))) > 0) then
        E_IO = -1_I4P; return ! Not found
      endif
      if (index(buffer, '<'//trim(adjustlt(Upper_Case(to_find)))) > 0) then
        if (len_trim(of_value) == 0) exit !there is no attribute value to seach
        call get_char(buffer=buffer, attrib=with_attribute, val=strng, E_IO=E_IO)
        if (E_IO==0 .and. trim(adjustlt(Upper_Case(strng))) == trim(adjustlt(Upper_Case(of_value)))) then  !Attribute match
          if (present(content) .and. index(buffer, '/>') == 0) then                                        !the value
            p1 = index(buffer, '<'//trim(adjustlt(Upper_Case(to_find))))
            p2 = index(buffer, '>')
            p3 = index(buffer, '</'//trim(adjustlt(Upper_Case(to_find))))
            ! Data in the same record
            if (p1/=0 .and. p2/=0 .and. p3/=0 .and. p2<p3) then
              content = buffer(p2+1:p3-1)
            elseif(p1==0 .and. p3/=0) then
              E_IO = -1_I4P
            else
              do
                E_IO = read_record(buffer=strng, cf=rf); if(E_IO /= 0) exit
                if (index(trim(adjustlt(Upper_Case(strng))), '</'//trim(adjustlt(Upper_Case(to_find)))) > 0) exit
                content = content//strng
              enddo
            endif
          endif
          exit
        endif
      endif
    enddo
  endif
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction search
endmodule Lib_VTK_IO_Back_End
