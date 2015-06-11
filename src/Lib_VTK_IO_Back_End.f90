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
public:: vtm
public:: Get_Unit
public:: Upper_Case
public:: byte_update
public:: vtk_update
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
  !< Function for getting a free logic unit.
  !<
  !< The users of does not know which is the logical unit: the library uses this information without boring the users. The logical
  !< unit used is safe-free: if the program calling the library has others logical units used the libary will never use these units,
  !< but it will choice one that is free.
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
      inquire(unit=n1,opened=lopen,iostat=ios)
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
endmodule Lib_VTK_IO_Back_End
