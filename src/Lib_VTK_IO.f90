!(doc)INCLUDE{/home/szaghi/VTK_IO/LIB_VTK_IO/source/DOC_Header.doc}
module Lib_VTK_IO
!-----------------------------------------------------------------------------------------------------------------------------------
!!\LIBVTKIO is a library of functions for Input and Output pure fortran data (both ascii and binary) in VTK format.
!!
!!The VTK standard can be separated into two main catagories: the \MaiuscolettoBS{VTK Legacy Standard} and the
!!\MaiuscolettoBS{VTK XML Standard}. The latter is more powerful and will has a stronger support from VTk comunity than legacy
!!standard; XML file format would to be preferred despite the legacy one.
!!
!!At the present only a few functions of the final library have been implemented. The InPut functions are totaly absent, but the
!!OutPut functions are almost complete (the \virgo{polydata} functions are the only missing).
!!
!!The functions actually present are:
!!
!!\begin{boxred}{Functions for Legacy VTK file format}
!!\begin{enumerate1Red}
!! \item \MaiuscolettoS{VTK\_INI}
!! \item \MaiuscolettoS{VTK\_GEO}
!! \item \MaiuscolettoS{VTK\_CON}
!! \item \MaiuscolettoS{VTK\_DAT}
!! \item \MaiuscolettoS{VTK\_VAR}
!! \item \MaiuscolettoS{VTK\_END}
!!\end{enumerate1Red}
!!\end{boxred}
!!
!!\begin{boxred}{Functions for XML VTK file format}
!!\begin{enumerate1Red}
!! \item \MaiuscolettoS{VTK\_INI\_XML}
!! \item \MaiuscolettoS{VTK\_GEO\_XML}
!! \item \MaiuscolettoS{VTK\_CON\_XML}
!! \item \MaiuscolettoS{VTK\_DAT\_XML}
!! \item \MaiuscolettoS{VTK\_VAR\_XML}
!! \item \MaiuscolettoS{VTK\_END\_XML}
!!\end{enumerate1Red}
!!\end{boxred}
!-----------------------------------------------------------------------------------------------------------------------------------

!-----------------------------------------------------------------------------------------------------------------------------------
USE IR_Precision ! Real and integer portable multi-precision kind definition.
!-----------------------------------------------------------------------------------------------------------------------------------

!-----------------------------------------------------------------------------------------------------------------------------------
implicit none
private
save
! functions for VTK LEGACY
public:: VTK_INI
public:: VTK_GEO
public:: VTK_CON
public:: VTK_DAT
public:: VTK_VAR
public:: VTK_END
! functions for VTK XML
public:: VTK_INI_XML
public:: VTK_GEO_XML
public:: VTK_CON_XML
public:: VTK_DAT_XML
public:: VTK_VAR_XML
public:: VTK_END_XML
! functions for VTM XML
public:: VTM_INI_XML
public:: VTM_BLK_XML
public:: VTM_WRF_XML
public:: VTM_END_XML
!-----------------------------------------------------------------------------------------------------------------------------------

!-----------------------------------------------------------------------------------------------------------------------------------
! overloading of VTK_GEO
interface VTK_GEO
  module procedure VTK_GEO_UNST_R8, & ! real(R8P) UNSTRUCTURED\_GRID
                   VTK_GEO_UNST_R4, & ! real(R4P) UNSTRUCTURED\_GRID
                   VTK_GEO_STRP_R8, & ! real(R8P) STRUCTURED\_POINTS
                   VTK_GEO_STRP_R4, & ! real(R4P) STRUCTURED\_POINTS
                   VTK_GEO_STRG_R8, & ! real(R8P) STRUCTURED\_GRID
                   VTK_GEO_STRG_R4, & ! real(R4P) STRUCTURED\_GRID
                   VTK_GEO_RECT_R8, & ! real(R8P) RECTILINEAR\_GRID
                   VTK_GEO_RECT_R4    ! real(R4P) RECTILINEAR\_GRID
endinterface
! overloading of VTK_VAR
interface VTK_VAR
  module procedure VTK_VAR_SCAL_R8, & ! real(R8P)    scalar
                   VTK_VAR_SCAL_R4, & ! real(R4P)    scalar
                   VTK_VAR_SCAL_I4, & ! integer(I4P) scalar
                   VTK_VAR_VECT_R8, & ! real(R8P)    vectorial
                   VTK_VAR_VECT_R4, & ! real(R4P)    vectorial
                   VTK_VAR_VECT_I4, & ! integer(I4P) vectorial
                   VTK_VAR_TEXT_R8, & ! real(R8P)    vectorial (texture)
                   VTK_VAR_TEXT_R4    ! real(R4P)    vectorial (texture)
endinterface
! overloading of VTK_GEO_XML
interface VTK_GEO_XML
  module procedure VTK_GEO_XML_STRG_R4, & ! real(R4P) StructuredGrid
                   VTK_GEO_XML_STRG_R8, & ! real(R8P) StructuredGrid
                   VTK_GEO_XML_RECT_R8, & ! real(R8P) RectilinearGrid
                   VTK_GEO_XML_RECT_R4, & ! real(R4P) RectilinearGrid
                   VTK_GEO_XML_UNST_R8, & ! real(R8P) UnstructuredGrid
                   VTK_GEO_XML_UNST_R4, & ! real(R4P) UnstructuredGrid
                   VTK_GEO_XML_CLOSEP     ! closing tag "Piece" function
endinterface
! overloading of VTK_VAR_XML
interface VTK_VAR_XML
  module procedure VTK_VAR_XML_SCAL_R8, & ! real(R8P)    scalar
                   VTK_VAR_XML_SCAL_R4, & ! real(R4P)    scalar
                   VTK_VAR_XML_SCAL_I8, & ! integer(I8P) scalar
                   VTK_VAR_XML_SCAL_I4, & ! integer(I4P) scalar
                   VTK_VAR_XML_SCAL_I2, & ! integer(I2P) scalar
                   VTK_VAR_XML_SCAL_I1, & ! integer(I1P) scalar
                   VTK_VAR_XML_VECT_R8, & ! real(R8P)    vectorial
                   VTK_VAR_XML_VECT_R4, & ! real(R4P)    vectorial
                   VTK_VAR_XML_VECT_I8, & ! integer(I4P) vectorial
                   VTK_VAR_XML_VECT_I4, & ! integer(I4P) vectorial
                   VTK_VAR_XML_VECT_I2, & ! integer(I4P) vectorial
                   VTK_VAR_XML_VECT_I1, & ! integer(I4P) vectorial
                   VTK_VAR_XML_LIST_R8, & ! real(R8P)    list
                   VTK_VAR_XML_LIST_R4, & ! real(R4P)    list
                   VTK_VAR_XML_LIST_I8, & ! integer(I4P) list
                   VTK_VAR_XML_LIST_I4, & ! integer(I4P) list
                   VTK_VAR_XML_LIST_I2, & ! integer(I2P) list
                   VTK_VAR_XML_LIST_I1    ! integer(I1P) list
endinterface
!-----------------------------------------------------------------------------------------------------------------------------------

!-----------------------------------------------------------------------------------------------------------------------------------
!!\LIBVTKIO uses a small set of internal variables that are private (not accessible from the outside). The following are
!! private variables:
!!
integer(I4P), parameter:: maxlen       = 500         ! max number of characters of static string
character(1), parameter:: end_rec      = char(10)    ! end-character for binary-record finalize
integer(I4P), parameter:: f_out_ascii  = 0           ! ascii-output-format parameter identifier
integer(I4P), parameter:: f_out_binary = 1           ! binary-output-format parameter identifier
integer(I4P)::            f_out        = f_out_ascii ! current output-format (initialized to ascii format)
character(len=maxlen)::   topology                   ! mesh topology
integer(I4P)::            Unit_VTK                   ! internal logical unit
integer(I4P)::            Unit_VTK_Append            ! internal logical unit for raw binary XML append file
integer(I4P)::            N_Byte                     ! number of byte to be written/read
real(R8P)::               Tipo_R8 = 1._R8P           ! prototype of R8P real
real(R4P)::               Tipo_R4 = 1._R4P           ! prototype of R4P real
integer(I8P)::            Tipo_I8 = 1_I8P            ! prototype of I8P integer
integer(I4P)::            Tipo_I4 = 1_I4P            ! prototype of I4P integer
integer(I2P)::            Tipo_I2 = 1_I2P            ! prototype of I2P integer
integer(I1P)::            Tipo_I1 = 1_I1P            ! prototype of I1P integer
integer(I8P)::            ioffset                    ! offset pointer
integer(I4P)::            indent                     ! indent pointer
! VTM specific variables
integer(I4P)::            Unit_VTM                   ! internal logical unit
integer(I4P)::            blk                        ! block index
integer(I4P)::            vtm_indent                 ! indent pointer
!-----------------------------------------------------------------------------------------------------------------------------------

!!In the following chapters there is the API reference of all functions of \LIBVTKIO.
contains
  !!\chapter{Auxiliary functions}
  !!\minitoc
  !!\vspace*{8mm}
  !!
  !!\LIBVTKIO uses two auxiliary functions that are not connected with the VTK standard. These functions are private and so they
  !!cannot be called outside the library.
  function GetUnit() result(Free_Unit)
  !---------------------------------------------------------------------------------------------------------------------------------
  !!The GetUnit function is used for getting a free logic unit. The users of \LIBVTKIO does not know which is
  !!the logical unit: \LIBVTKIO handels this information without boring the users. The logical unit used is safe-free: if the
  !!program calling \LIBVTKIO has others logical units used \LIBVTKIO will never use these units, but will choice one that is free.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  implicit none
  integer(I4P):: Free_Unit ! free logic unit
  integer(I4P):: n1        ! counter
  integer(I4P):: ios       ! inquiring flag
  logical(4)::   lopen     ! inquiring flag
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  !!The following is the code snippet of GetUnit function: the units 0, 5, 6, 9 and all non-free units are discarded.
  !!
  !(\doc)codesnippet
  Free_Unit = -1_I4P                                      ! initializing free logic unit
  n1=1_I4P                                                ! initializing counter
  do
    if ((n1/=5_I4P).AND.(n1/=6_I4P).AND.(n1/=9_I4P)) then
      inquire (unit=n1,opened=lopen,iostat=ios)           ! verify logic units
      if (ios==0_I4P) then
        if (.NOT.lopen) then
          Free_Unit = n1                                  ! assignment of free logic
          return
        endif
      endif
    endif
    n1=n1+1_I4P                                           ! updating counter
  enddo
  return
  !(doc/)codesnippet
  !!GetUnit function is private and cannot be called outside \LIBVTKIO. If you are interested to use it change its scope to public.
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction GetUnit

  function Upper_Case(string)
  !---------------------------------------------------------------------------------------------------------------------------------
  !!The Upper\_Case function converts the lower case characters of a string to upper case one. \LIBVTKIO uses this function in
  !!order to achieve case-insensitive: all character variables used within \LIBVTKIO functions are pre-processed by
  !!Uppper\_Case function before these variables are used. So the users can call \LIBVTKIO functions whitout pay attention of the
  !!case of the kwywords passed to the functions: calling the function VTK\_INI with the string \code{E_IO = VTK_INI('Ascii',...)}
  !!or with the string  \code{E_IO = VTK_INI('AscII',...)} is equivalent.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  implicit none
  character(len=*), intent(IN):: string     ! string to be converted
  character(len=len(string))::   Upper_Case ! converted string
  integer::                      n1         ! characters counter
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  !!The following is the code snippet of Upper\_Case function.
  !!
  !(\doc)codesnippet
  Upper_Case = string
  do n1=1,len(string)
    select case(ichar(string(n1:n1)))
    case(97:122)
      Upper_Case(n1:n1)=char(ichar(string(n1:n1))-32) ! Upper case conversion
    endselect
  enddo
  return
  !(doc/)codesnippet
  !!Upper\_Case function is private and cannot be called outside \LIBVTKIO. If you are interested to use it change its scope
  !!to public.
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction Upper_Case

  !!\chapter{VTK LEGACY functions}
  !!\minitoc
  !!\vspace*{8mm}
  !!
  function VTK_INI(output_format,filename,title,mesh_topology) result(E_IO)
  !---------------------------------------------------------------------------------------------------------------------------------
  !!The VTK\_INI function is used for initializing file. This function must be the first to be called.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  implicit none
  character(*), intent(IN):: output_format ! output format: ASCII or BINARY
  character(*), intent(IN):: filename      ! name of file
  character(*), intent(IN):: title         ! title
  character(*), intent(IN):: mesh_topology ! mesh topology
  integer(I4P)::             E_IO          ! Input/Output inquiring flag: $0$ if IO is done, $> 0$ if IO is not done
  !!The VTK\_INI variables have the following meaning:
  !!
  !!\begin{description}
  !! \item[{\color{RoyalBlue}output\_format}] indicates the \virgo{format} of output file. It can assume the following values:
  !! \begin{enumerateABlu}
  !!  \item \emph{ascii} (it is case insensitive) $\rightarrow$ creating an ascii output file.
  !!  \item \emph{binary} (it is case insensitive) $\rightarrow$ creating a binary (big\_endian encoding) output file.
  !! \end{enumerateABlu}
  !! \item[{\color{RoyalBlue}filename}] contains the name (with its path) of the output file.
  !! \item[{\color{RoyalBlue}title}] contains the title of the VTK dataset.
  !! \item[{\color{RoyalBlue}topology}] indicates the topology of the mesh and can assume the following values:
  !! \begin{enumerateABlu}
  !!  \item \emph{STRUCTURED\_POINTS}.
  !!  \item \emph{STRUCTURED\_GRID}.
  !!  \item \emph{UNSTRUCTURED\_GRID}.
  !!  \item \emph{RECTILINEAR\_GRID}.
  !! \end{enumerateABlu}
  !! \item[{\color{RoyalBlue}E\_IO}] contains the inquiring integer flag for error handling.
  !!\end{description}
  !!
  !!The following is an example of VTK\_INI calling:
  !!
  !!\begin{boxred}{VTK\_INI Calling}
  !!\begin{verbatim}
  !!...
  !!E_IO = VTK_INI('Binary','example.vtk','VTK legacy file','UNSTRUCTURED_GRID')
  !!...
  !!\end{verbatim}
  !!\end{boxred}
  !!\noindent Note that the \virgo{.vtk} extension is necessary in the file name.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  topology = trim(mesh_topology)
  Unit_VTK=GetUnit()
  select case(trim(Upper_Case(output_format)))
  case('ASCII')
    f_out = f_out_ascii
    open(unit     = Unit_VTK,       &
         file     = trim(filename), &
         form     = 'FORMATTED',    &
         access   = 'SEQUENTIAL',   &
         action   = 'WRITE',        &
         iostat   = E_IO)
    ! writing header of file
    write(unit=Unit_VTK,fmt='(A)',iostat=E_IO)'# vtk DataFile Version 3.0'
    write(unit=Unit_VTK,fmt='(A)',iostat=E_IO)trim(title)
    write(unit=Unit_VTK,fmt='(A)',iostat=E_IO)trim(Upper_Case(output_format))
    write(unit=Unit_VTK,fmt='(A)',iostat=E_IO)'DATASET '//trim(topology)
  case('BINARY')
    f_out = f_out_binary
    open(unit       = Unit_VTK,       &
         file       = trim(filename), &
         form       = 'UNFORMATTED',  &
         access     = 'STREAM',       &
         action     = 'WRITE',        &
         iostat     = E_IO)
    ! writing header of file
    write(unit=Unit_VTK,iostat=E_IO)'# vtk DataFile Version 3.0'//end_rec
    write(unit=Unit_VTK,iostat=E_IO)trim(title)//end_rec
    write(unit=Unit_VTK,iostat=E_IO)trim(Upper_Case(output_format))//end_rec
    write(unit=Unit_VTK,iostat=E_IO)'DATASET '//trim(topology)//end_rec
  endselect
  return
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction VTK_INI

  !(doc)INCLUDE{/home/szaghi/VTK_IO/LIB_VTK_IO/source/DOC_VTK_GEO.doc}
  !(\doc)skippedblock
  function VTK_GEO_STRP_R8(Nx,Ny,Nz,X0,Y0,Z0,Dx,Dy,Dz) result(E_IO)
  !---------------------------------------------------------------------------------------------------------------------------------
  !! Function for saving mesh; topology = STRUCTURED\_POINTS (R8P).
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  implicit none
  integer(I4P), intent(IN):: Nx        ! number of nodes in x direction
  integer(I4P), intent(IN):: Ny        ! number of nodes in y direction
  integer(I4P), intent(IN):: Nz        ! number of nodes in z direction
  real(R8P),    intent(IN):: X0        ! x coordinate of origin
  real(R8P),    intent(IN):: Y0        ! y coordinate of origin
  real(R8P),    intent(IN):: Z0        ! z coordinate of origin
  real(R8P),    intent(IN):: Dx        ! space step in x direction
  real(R8P),    intent(IN):: Dy        ! space step in y direction
  real(R8P),    intent(IN):: Dz        ! space step in z direction
  integer(I4P)::             E_IO      ! Input/Output inquiring flag: $0$ if IO is done, $> 0$ if IO is not done
  character(len=maxlen)::    s_buffer  ! buffer string
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  select case(f_out)
  case(f_out_ascii)
    write(unit=Unit_VTK,fmt='(A,3'//FI4P//')', iostat=E_IO)'DIMENSIONS ',Nx,Ny,Nz
    write(unit=Unit_VTK,fmt='(A,3'//FR8P//')', iostat=E_IO)'ORIGIN ',X0,Y0,Z0
    write(unit=Unit_VTK,fmt='(A,3'//FR8P//')', iostat=E_IO)'SPACING ',Dx,Dy,Dz
  case(f_out_binary)
    write(s_buffer,     fmt='(A,3'//FI4P//')', iostat=E_IO)'DIMENSIONS ',Nx,Ny,Nz
    write(unit=Unit_VTK,                       iostat=E_IO)trim(s_buffer)//end_rec
    write(s_buffer,     fmt='(A,3'//FR8P//')', iostat=E_IO)'ORIGIN ',X0,Y0,Z0
    write(unit=Unit_VTK,                       iostat=E_IO)trim(s_buffer)//end_rec
    write(s_buffer,     fmt='(A,3'//FR8P//')', iostat=E_IO)'SPACING ',Dx,Dy,Dz
    write(unit=Unit_VTK,                       iostat=E_IO)trim(s_buffer)//end_rec
  endselect
  return
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction VTK_GEO_STRP_R8

  function VTK_GEO_STRP_R4(Nx,Ny,Nz,X0,Y0,Z0,Dx,Dy,Dz) result(E_IO)
  !---------------------------------------------------------------------------------------------------------------------------------
  !! Function for saving mesh; topology = STRUCTURED\_POINTS (R4P).
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  implicit none
  integer(I4P), intent(IN):: Nx        ! number of nodes in x direction
  integer(I4P), intent(IN):: Ny        ! number of nodes in y direction
  integer(I4P), intent(IN):: Nz        ! number of nodes in z direction
  real(R4P),    intent(IN):: X0        ! x coordinate of origin
  real(R4P),    intent(IN):: Y0        ! y coordinate of origin
  real(R4P),    intent(IN):: Z0        ! z coordinate of origin
  real(R4P),    intent(IN):: Dx        ! space step in x direction
  real(R4P),    intent(IN):: Dy        ! space step in y direction
  real(R4P),    intent(IN):: Dz        ! space step in z direction
  integer(I4P)::             E_IO      ! Input/Output inquiring flag: $0$ if IO is done, $> 0$ if IO is not done
  character(len=maxlen)::    s_buffer  ! buffer string
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  select case(f_out)
  case(f_out_ascii)
    write(unit=Unit_VTK,fmt='(A,3'//FI4P//')', iostat=E_IO)'DIMENSIONS ',Nx,Ny,Nz
    write(unit=Unit_VTK,fmt='(A,3'//FR4P//')', iostat=E_IO)'ORIGIN ',X0,Y0,Z0
    write(unit=Unit_VTK,fmt='(A,3'//FR4P//')', iostat=E_IO)'SPACING ',Dx,Dy,Dz
  case(f_out_binary)
    write(s_buffer,     fmt='(A,3'//FI4P//')', iostat=E_IO)'DIMENSIONS ',Nx,Ny,Nz
    write(unit=Unit_VTK,                       iostat=E_IO)trim(s_buffer)//end_rec
    write(s_buffer,     fmt='(A,3'//FR4P//')', iostat=E_IO)'ORIGIN ',X0,Y0,Z0
    write(unit=Unit_VTK,                       iostat=E_IO)trim(s_buffer)//end_rec
    write(s_buffer,     fmt='(A,3'//FR4P//')', iostat=E_IO)'SPACING ',Dx,Dy,Dz
    write(unit=Unit_VTK,                       iostat=E_IO)trim(s_buffer)//end_rec
  endselect
  return
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction VTK_GEO_STRP_R4

  function VTK_GEO_STRG_R8(Nx,Ny,Nz,NN,X,Y,Z) result(E_IO)
  !---------------------------------------------------------------------------------------------------------------------------------
  !! Function for saving mesh; topology = STRUCTURED\_GRID (R8P).
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  implicit none
  integer(I4P), intent(IN):: Nx       ! number of nodes in x direction
  integer(I4P), intent(IN):: Ny       ! number of nodes in y direction
  integer(I4P), intent(IN):: Nz       ! number of nodes in z direction
  integer(I4P), intent(IN):: NN       ! number of all nodes
  real(R8P),    intent(IN):: X(1:NN)  ! x coordinates
  real(R8P),    intent(IN):: Y(1:NN)  ! y coordinates
  real(R8P),    intent(IN):: Z(1:NN)  ! z coordinates
  integer(I4P)::             E_IO     ! Input/Output inquiring flag: $0$ if IO is done, $> 0$ if IO is not done
  character(len=maxlen)::    s_buffer ! buffer string
  integer(I4P)::             n1       ! counter
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  select case(f_out)
  case(f_out_ascii)
    write(unit=Unit_VTK,fmt='(A,3'//FI4P//')', iostat=E_IO)'DIMENSIONS ',Nx,Ny,Nz
    write(unit=Unit_VTK,fmt='(A,'//FI4P//',A)',iostat=E_IO)'POINTS ',NN,' double'
    write(unit=Unit_VTK,fmt='(3'//FR8P//')',   iostat=E_IO)(X(n1),Y(n1),Z(n1),n1=1,NN)
  case(f_out_binary)
    write(s_buffer,     fmt='(A,3'//FI4P//')', iostat=E_IO)'DIMENSIONS ',Nx,Ny,Nz
    write(unit=Unit_VTK,                       iostat=E_IO)trim(s_buffer)//end_rec
    write(s_buffer,     fmt='(A,'//FI4P//',A)',iostat=E_IO)'POINTS ',NN,' double'
    write(unit=Unit_VTK,                       iostat=E_IO)trim(s_buffer)//end_rec
    write(unit=Unit_VTK,                       iostat=E_IO)(X(n1),Y(n1),Z(n1),n1=1,NN)
    write(unit=Unit_VTK,                       iostat=E_IO)end_rec
  endselect
  return
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction VTK_GEO_STRG_R8

  function VTK_GEO_STRG_R4(Nx,Ny,Nz,NN,X,Y,Z) result(E_IO)
  !---------------------------------------------------------------------------------------------------------------------------------
  !! Function for saving mesh; topology = STRUCTURED\_GRID (R4P).
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  implicit none
  integer(I4P), intent(IN):: Nx       ! number of nodes in x direction
  integer(I4P), intent(IN):: Ny       ! number of nodes in y direction
  integer(I4P), intent(IN):: Nz       ! number of nodes in z direction
  integer(I4P), intent(IN):: NN       ! number of all nodes
  real(R4P),    intent(IN):: X(1:NN)  ! x coordinates
  real(R4P),    intent(IN):: Y(1:NN)  ! y coordinates
  real(R4P),    intent(IN):: Z(1:NN)  ! z coordinates
  integer(I4P)::             E_IO     ! Input/Output inquiring flag: $0$ if IO is done, $> 0$ if IO is not done
  character(len=maxlen)::    s_buffer ! buffer string
  integer(I4P)::             n1       ! counter
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  select case(f_out)
  case(f_out_ascii)
    write(unit=Unit_VTK,fmt='(A,3'//FI4P//')', iostat=E_IO)'DIMENSIONS ',Nx,Ny,Nz
    write(unit=Unit_VTK,fmt='(A,'//FI4P//',A)',iostat=E_IO)'POINTS ',NN,' float'
    write(unit=Unit_VTK,fmt='(3'//FR4P//')',   iostat=E_IO)(X(n1),Y(n1),Z(n1),n1=1,NN)
  case(f_out_binary)
    write(s_buffer,     fmt='(A,3'//FI4P//')', iostat=E_IO)'DIMENSIONS ',Nx,Ny,Nz
    write(unit=Unit_VTK,                       iostat=E_IO)trim(s_buffer)//end_rec
    write(s_buffer,     fmt='(A,'//FI4P//',A)',iostat=E_IO)'POINTS ',NN,' float'
    write(unit=Unit_VTK,                       iostat=E_IO)trim(s_buffer)//end_rec
    write(unit=Unit_VTK,                       iostat=E_IO)(X(n1),Y(n1),Z(n1),n1=1,NN)
    write(unit=Unit_VTK,                       iostat=E_IO)end_rec
  endselect
  return
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction VTK_GEO_STRG_R4

  function VTK_GEO_RECT_R8(Nx,Ny,Nz,X,Y,Z) result(E_IO)
  !---------------------------------------------------------------------------------------------------------------------------------
  !! Function for saving mesh; topology = RECTILINEAR\_GRID (R8P).
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  implicit none
  integer(I4P), intent(IN):: Nx        ! number of nodes in x direction
  integer(I4P), intent(IN):: Ny        ! number of nodes in y direction
  integer(I4P), intent(IN):: Nz        ! number of nodes in z direction
  real(R8P),    intent(IN):: X(1:Nx)   ! x coordinates
  real(R8P),    intent(IN):: Y(1:Ny)   ! y coordinates
  real(R8P),    intent(IN):: Z(1:Nz)   ! z coordinates
  integer(I4P)::             E_IO      ! Input/Output inquiring flag: $0$ if IO is done, $> 0$ if IO is not done
  character(len=maxlen)::    s_buffer  ! buffer string
  integer(I4P)::             n1        ! counter
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  select case(f_out)
  case(f_out_ascii)
    write(unit=Unit_VTK,fmt='(A,3'//FI4P//')', iostat=E_IO)'DIMENSIONS ',Nx,Ny,Nz
    write(unit=Unit_VTK,fmt='(A,'//FI4P//',A)',iostat=E_IO)'X_COORDINATES ',Nx,' double'
    write(unit=Unit_VTK,fmt=FR8P,              iostat=E_IO)(X(n1),n1=1,Nx)
    write(unit=Unit_VTK,fmt='(A,'//FI4P//',A)',iostat=E_IO)'Y_COORDINATES ',Ny,' double'
    write(unit=Unit_VTK,fmt=FR8P,              iostat=E_IO)(Y(n1),n1=1,Ny)
    write(unit=Unit_VTK,fmt='(A,'//FI4P//',A)',iostat=E_IO)'Z_COORDINATES ',Nz,' double'
    write(unit=Unit_VTK,fmt=FR8P,              iostat=E_IO)(Z(n1),n1=1,Nz)
  case(f_out_binary)
    write(s_buffer,     fmt='(A,3'//FI4P//')', iostat=E_IO)'DIMENSIONS ',Nx,Ny,Nz
    write(unit=Unit_VTK,                       iostat=E_IO)trim(s_buffer)//end_rec
    write(s_buffer,     fmt='(A,'//FI4P//',A)',iostat=E_IO)'X_COORDINATES ',Nx,' double'
    write(unit=Unit_VTK,                       iostat=E_IO)trim(s_buffer)//end_rec
    write(unit=Unit_VTK,                       iostat=E_IO)(X(n1),n1=1,Nx)
    write(unit=Unit_VTK,                       iostat=E_IO)end_rec
    write(s_buffer,     fmt='(A,'//FI4P//',A)',iostat=E_IO)'Y_COORDINATES ',Ny,' double'
    write(unit=Unit_VTK,                       iostat=E_IO)trim(s_buffer)//end_rec
    write(unit=Unit_VTK,                       iostat=E_IO)(Y(n1),n1=1,Ny)
    write(unit=Unit_VTK,                       iostat=E_IO)end_rec
    write(s_buffer,     fmt='(A,'//FI4P//',A)',iostat=E_IO)'Z_COORDINATES ',Nz,' double'
    write(unit=Unit_VTK,                       iostat=E_IO)trim(s_buffer)//end_rec
    write(unit=Unit_VTK,                       iostat=E_IO)(Z(n1),n1=1,Nz)
    write(unit=Unit_VTK,                       iostat=E_IO)end_rec
  endselect
  return
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction VTK_GEO_RECT_R8

  function VTK_GEO_RECT_R4(Nx,Ny,Nz,X,Y,Z) result(E_IO)
  !---------------------------------------------------------------------------------------------------------------------------------
  !! Function for saving mesh; topology = RECTILINEAR\_GRID (R4P).
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  implicit none
  integer(I4P), intent(IN):: Nx        ! number of nodes in x direction
  integer(I4P), intent(IN):: Ny        ! number of nodes in y direction
  integer(I4P), intent(IN):: Nz        ! number of nodes in z direction
  real(R4P),    intent(IN):: X(1:Nx)   ! x coordinates
  real(R4P),    intent(IN):: Y(1:Ny)   ! y coordinates
  real(R4P),    intent(IN):: Z(1:Nz)   ! z coordinates
  integer(I4P)::             E_IO      ! Input/Output inquiring flag: $0$ if IO is done, $> 0$ if IO is not done
  character(len=maxlen)::    s_buffer  ! buffer string
  integer(I4P)::             n1        ! counter
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  select case(f_out)
  case(f_out_ascii)
    write(unit=Unit_VTK,fmt='(A,3'//FI4P//')', iostat=E_IO)'DIMENSIONS ',Nx,Ny,Nz
    write(unit=Unit_VTK,fmt='(A,'//FI4P//',A)',iostat=E_IO)'X_COORDINATES ',Nx,' float'
    write(unit=Unit_VTK,fmt=FR4P,              iostat=E_IO)(X(n1),n1=1,Nx)
    write(unit=Unit_VTK,fmt='(A,'//FI4P//',A)',iostat=E_IO)'Y_COORDINATES ',Ny,' float'
    write(unit=Unit_VTK,fmt=FR4P,              iostat=E_IO)(Y(n1),n1=1,Ny)
    write(unit=Unit_VTK,fmt='(A,'//FI4P//',A)',iostat=E_IO)'Z_COORDINATES ',Nz,' float'
    write(unit=Unit_VTK,fmt=FR4P,              iostat=E_IO)(Z(n1),n1=1,Nz)
  case(f_out_binary)
    write(s_buffer,     fmt='(A,3'//FI4P//')', iostat=E_IO)'DIMENSIONS ',Nx,Ny,Nz
    write(unit=Unit_VTK,                       iostat=E_IO)trim(s_buffer)//end_rec
    write(s_buffer,     fmt='(A,'//FI4P//',A)',iostat=E_IO)'X_COORDINATES ',Nx,' float'
    write(unit=Unit_VTK,                       iostat=E_IO)trim(s_buffer)//end_rec
    write(unit=Unit_VTK,                       iostat=E_IO)(X(n1),n1=1,Nx)
    write(unit=Unit_VTK,                       iostat=E_IO)end_rec
    write(s_buffer,     fmt='(A,'//FI4P//',A)',iostat=E_IO)'Y_COORDINATES ',Ny,' float'
    write(unit=Unit_VTK,                       iostat=E_IO)trim(s_buffer)//end_rec
    write(unit=Unit_VTK,                       iostat=E_IO)(Y(n1),n1=1,Ny)
    write(unit=Unit_VTK,                       iostat=E_IO)end_rec
    write(s_buffer,     fmt='(A,'//FI4P//',A)',iostat=E_IO)'Z_COORDINATES ',Nz,' float'
    write(unit=Unit_VTK,                       iostat=E_IO)trim(s_buffer)//end_rec
    write(unit=Unit_VTK,                       iostat=E_IO)(Z(n1),n1=1,Nz)
    write(unit=Unit_VTK,                       iostat=E_IO)end_rec
  endselect
  return
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction VTK_GEO_RECT_R4

  function VTK_GEO_UNST_R8(NN,X,Y,Z) result(E_IO)
  !---------------------------------------------------------------------------------------------------------------------------------
  !! Function for saving mesh; topology = UNSTRUCTURED\_GRID (R8P).
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  implicit none
  integer(I4P), intent(IN):: NN        ! number of nodes
  real(R8P),    intent(IN):: X(1:NN)   ! x coordinates of all nodes
  real(R8P),    intent(IN):: Y(1:NN)   ! y coordinates of all nodes
  real(R8P),    intent(IN):: Z(1:NN)   ! z coordinates of all nodes
  integer(I4P)::             E_IO      ! Input/Output inquiring flag: $0$ if IO is done, $> 0$ if IO is not done
  character(len=maxlen)::    s_buffer  ! buffer string
  integer(I4P)::             n1        ! counter
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  select case(f_out)
  case(f_out_ascii)
    write(unit=Unit_VTK,fmt='(A,'//FI4P//',A)',iostat=E_IO)'POINTS ',NN,' double'
    write(unit=Unit_VTK,fmt='(3'//FR8P//')',   iostat=E_IO)(X(n1),Y(n1),Z(n1),n1=1,NN)
  case(f_out_binary)
    write(s_buffer,     fmt='(A,'//FI4P//',A)',iostat=E_IO)'POINTS ',NN,' double'
    write(unit=Unit_VTK,                       iostat=E_IO)trim(s_buffer)//end_rec
    write(unit=Unit_VTK,                       iostat=E_IO)(X(n1),Y(n1),Z(n1),n1=1,NN)
    write(unit=Unit_VTK,                       iostat=E_IO)end_rec
  endselect
  return
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction VTK_GEO_UNST_R8

  function VTK_GEO_UNST_R4(NN,X,Y,Z) result(E_IO)
  !---------------------------------------------------------------------------------------------------------------------------------
  !! Function for saving mesh; topology = UNSTRUCTURED\_GRID (R4P).
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  implicit none
  integer(I4P), intent(IN):: NN        ! number of nodes
  real(R4P),    intent(IN):: X(1:NN)   ! x coordinates of all nodes
  real(R4P),    intent(IN):: Y(1:NN)   ! y coordinates of all nodes
  real(R4P),    intent(IN):: Z(1:NN)   ! z coordinates of all nodes
  integer(I4P)::             E_IO      ! Input/Output inquiring flag: $0$ if IO is done, $> 0$ if IO is not done
  character(len=maxlen)::    s_buffer  ! buffer string
  integer(I4P)::             n1        ! counter
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  select case(f_out)
  case(f_out_ascii)
    write(unit=Unit_VTK,fmt='(A,'//FI4P//',A)',iostat=E_IO)'POINTS ',NN,' float'
    write(unit=Unit_VTK,fmt='(3'//FR4P//')',   iostat=E_IO)(X(n1),Y(n1),Z(n1),n1=1,NN)
  case(f_out_binary)
    write(s_buffer,     fmt='(A,'//FI4P//',A)',iostat=E_IO)'POINTS ',NN,' float'
    write(unit=Unit_VTK,                       iostat=E_IO)trim(s_buffer)//end_rec
    write(unit=Unit_VTK,                       iostat=E_IO)(X(n1),Y(n1),Z(n1),n1=1,NN)
    write(unit=Unit_VTK,                       iostat=E_IO)end_rec
  endselect
  return
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction VTK_GEO_UNST_R4
  !(doc/)skippedblock

  function VTK_CON(NC,connect,cell_type) result(E_IO)
  !---------------------------------------------------------------------------------------------------------------------------------
  !!This function \MaiuscolettoBS{must} be used when unstructured grid is used. It saves the connectivity of the unstructured
  !!mesh.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  implicit none
  integer(I4P), intent(IN):: NC              ! number of cells
  integer(I4P), intent(IN):: connect(:)      ! mesh connectivity
  integer(I4P), intent(IN):: cell_type(1:NC) ! VTK cell type
  integer(I4P)::             E_IO            ! Input/Output inquiring flag: $0$ if IO is done, $> 0$ if IO is not done
  character(len=maxlen)::    s_buffer        ! buffer string
  integer(I4P)::             ncon            ! dimension of connectivity vector 
  !!The VTK\_CON variables have the following meaning:
  !!
  !!\begin{description}
  !! \item[{\color{RoyalBlue}NC}] indicates the number of all cells.
  !! \item[{\color{RoyalBlue}connect}] contains the connectivity of the mesh. It is a vector.
  !! \item[{\color{RoyalBlue}cell\_type}] contains the type of every cells. It is a vector of $[1:NC]$.
  !! \item[{\color{RoyalBlue}E\_IO}] contains the inquiring integer flag for error handling.
  !!\end{description}
  !!
  !!The vector \MaiuscolettoBS{connect} must follow the VTK legacy standard. It is passed as \MaiuscolettoBS{assumed-shape} array
  !!because its dimensions is related to the mesh dimensions in a complex way. Its dimensions can be calculated by the following
  !!equation:
  !!
  !!\begin{equation}
  !!dc = NC + \sum\limits_{i = 1}^{NC} {nvertex_i }
  !!\label{eq:connectivity dimensions}
  !!\end{equation}
  !!
  !!\noindent where $dc$ is connectivity vector dimension and $nvertex_i$ is the number of vertices of $i^{th}$ cell. The VTK
  !!legacy standard for the mesh connectivity is quite obscure at least at first sight. It is more simple analizing an example.
  !!Suppose we have a mesh composed by 2 cells, one hexahedron (8 vertices) and one pyramid with square basis (5 vertices); suppose
  !!that the basis of pyramid is constitute by a face of the hexahedron and so the two cells share 4 vertices. The equation
  !!\ref{eq:connectivity dimensions} gives $dc=2+8+5=15$; the connectivity vector for this mesh can be:
  !!
  !!\begin{boxred}{Connectivity vector example for VTK legacy standard}
  !!\begin{verbatim}
  !!! first cell
  !!connect(1)  = 8  => number of vertices of 1° cell
  !!connect(2)  = 0  => identification flag of 1° vertex of 1° cell
  !!connect(3)  = 1  => identification flag of 2° vertex of 1° cell
  !!connect(4)  = 2  => identification flag of 3° vertex of 1° cell
  !!connect(5)  = 3  => identification flag of 4° vertex of 1° cell
  !!connect(6)  = 4  => identification flag of 5° vertex of 1° cell
  !!connect(7)  = 5  => identification flag of 6° vertex of 1° cell
  !!connect(8)  = 6  => identification flag of 7° vertex of 1° cell
  !!connect(9)  = 7  => identification flag of 8° vertex of 1° cell
  !!! second cell
  !!connect(10) = 5  => number of vertices of 2° cell
  !!connect(11) = 0  => identification flag of 1° vertex of 2° cell
  !!connect(12) = 1  => identification flag of 2° vertex of 2° cell
  !!connect(13) = 2  => identification flag of 3° vertex of 2° cell
  !!connect(14) = 3  => identification flag of 4° vertex of 2° cell
  !!connect(15) = 8  => identification flag of 5° vertex of 2° cell
  !!\end{verbatim}
  !!\end{boxred}
  !!
  !!\noindent Note that the first 4 identification flags of pyramid vertices as the same of the first 4 identification flags of
  !!the hexahedron because the two cells share this face. It is also important to note that the identification flags start
  !!form $0$ value: this is impose to the VTK standard. The function VTK\_CON does not calculate the connectivity vector: it
  !!writes the connectivity vector conforming the VTK standard, but does not calculate it. In the future release of \LIBVTKIO will
  !!be included a function to calculate the connectivity vector.
  !!
  !!The vector variable \MaiuscolettoBS{tipo} must conform the VTK standard \footnote{See the file VTK-Standard at the Kitware
  !!homepage.}. It contains the \emph{type} of each cells. For the above example this vector is:
  !!
  !!\begin{boxred}{Cell-Type vector example for VTK legacy standard}
  !!\begin{verbatim}
  !!tipo(1) = 12  => VTK hexahedron type of 1° cell
  !!tipo(2) = 14  => VTK pyramid type of 2° cell
  !!\end{verbatim}
  !!\end{boxred}
  !!
  !!The following is an example of VTK\_CON calling:
  !!
  !!\begin{boxred}{VTK\_CON Calling}
  !!\begin{verbatim}
  !!...
  !!integer(4), parameter:: NC=2
  !!integer(4), parameter:: Nvertex1=8
  !!integer(4), parameter:: Nvertex2=5
  !!integer(4), parameter:: dc=NC+Nvertex1+Nvertex2
  !!integer(4)::            connect(1:dc)
  !!integer(4)::            cell_type(1:NC)
  !!...
  !!E_IO = VTK_CON(NC,connect,cell_type)
  !!...
  !!\end{verbatim}
  !!\end{boxred}
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  ncon = size(connect,1)
  select case(f_out)
  case(f_out_ascii)
    write(unit=Unit_VTK,fmt='(A,2'//FI4P//')',iostat=E_IO)'CELLS ',NC,ncon
    write(unit=Unit_VTK,fmt=FI4P,             iostat=E_IO)connect
    write(unit=Unit_VTK,fmt='(A,'//FI4P//')', iostat=E_IO)'CELL_TYPES ',NC
    write(unit=Unit_VTK,fmt=FI4P,             iostat=E_IO)cell_type
  case(f_out_binary)
    write(s_buffer,     fmt='(A,2'//FI4P//')',iostat=E_IO)'CELLS ',NC,ncon
    write(unit=Unit_VTK,                      iostat=E_IO)trim(s_buffer)//end_rec
    write(unit=Unit_VTK,                      iostat=E_IO)connect
    write(unit=Unit_VTK,                      iostat=E_IO)end_rec
    write(s_buffer,     fmt='(A,'//FI4P//')', iostat=E_IO)'CELL_TYPES ',NC
    write(unit=Unit_VTK,                      iostat=E_IO)trim(s_buffer)//end_rec
    write(unit=Unit_VTK,                      iostat=E_IO)cell_type
    write(unit=Unit_VTK,                      iostat=E_IO)end_rec
  endselect
  return
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction VTK_CON

  function VTK_DAT(NC_NN,var_location) result(E_IO)
  !---------------------------------------------------------------------------------------------------------------------------------
  !!This function \MaiuscolettoBS{must} be called before saving the data related to geometric mesh. This function initializes the
  !!saving of data variables indicating the \emph{type} of variables that will be saved.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  implicit none
  integer(I4P), intent(IN):: NC_NN        ! number of cells or nodes of field
  character(*), intent(IN):: var_location ! location of saving variables: cell for cell-centered, node for node-centered
  integer(I4P)::             E_IO         ! Input/Output inquiring flag: $0$ if IO is done, $> 0$ if IO is not done
  character(len=maxlen)::    s_buffer     ! buffer string
  !!The VTK\_DAT variables have the following meaning:
  !!
  !!\begin{description}
  !! \item[{\color{RoyalBlue}NC\_NN}] indicates the number of all cells or all nodes according to the value of
  !!                                  {\color{RoyalBlue}tipo}.
  !! \item[{\color{RoyalBlue}var\_location}] contains the location-type of variables that will be saved after VTK\_DAT. It is
  !!                                         a scalar and cab assume the following values:
  !! \begin{enumerateABlu}
  !!  \item \emph{cell} (it is case insensitive) $\rightarrow$ variables will be cell-centered.
  !!  \item \emph{node} (it is case insensitive) $\rightarrow$ variables will be node-centered.
  !! \end{enumerateABlu}
  !! \item[{\color{RoyalBlue}E\_IO}] contains the inquiring integer flag for error handling.
  !!\end{description}
  !!
  !!Of course a single file can contain both cell and node centered variables; in this case the VTK\_DAT function must be
  !!called two times, before saving cell-centered variables and before saving node-centered variables.
  !!
  !!The following is an example of VTK\_DAT calling:
  !!
  !!\begin{boxred}{VTK\_DAT Calling}
  !!\begin{verbatim}
  !!...
  !!E_IO = VTK_DAT(50,'node')
  !!...
  !!\end{verbatim}
  !!\end{boxred}
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  select case(f_out)
  case(f_out_ascii)
    select case(trim(Upper_Case(var_location)))
    case('CELL')
      write(unit=Unit_VTK,fmt='(A,'//FI4P//')',iostat=E_IO)'CELL_DATA ',NC_NN
    case('NODE')
      write(unit=Unit_VTK,fmt='(A,'//FI4P//')',iostat=E_IO)'POINT_DATA ',NC_NN
    endselect
  case(f_out_binary)
    select case(trim(Upper_Case(var_location)))
    case('CELL')
      write(s_buffer,fmt='(A,'//FI4P//')',iostat=E_IO)'CELL_DATA ',NC_NN
      write(unit=Unit_VTK,iostat=E_IO)trim(s_buffer)//end_rec
    case('NODE')
      write(s_buffer,fmt='(A,'//FI4P//')',iostat=E_IO)'POINT_DATA ',NC_NN
      write(unit=Unit_VTK,iostat=E_IO)trim(s_buffer)//end_rec
    endselect
  endselect
  return
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction VTK_DAT

  !(doc)INCLUDE{/home/szaghi/VTK_IO/LIB_VTK_IO/source/DOC_VTK_VAR.doc}
  !(\doc)skippedblock
  function VTK_VAR_SCAL_R8(NC_NN,varname,var) result(E_IO)
  !---------------------------------------------------------------------------------------------------------------------------------
  !! Function for saving field of scalar variable (R8P).
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  implicit none
  integer(I4P), intent(IN):: NC_NN        ! number of nodes or cells
  character(*), intent(IN):: varname      ! variable name
  real(R8P),    intent(IN):: var(1:NC_NN) ! variable to be saved
  integer(I4P)::             E_IO         ! Input/Output inquiring flag: $0$ if IO is done, $> 0$ if IO is not done
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  select case(f_out)
  case(f_out_ascii)
    write(unit=Unit_VTK,fmt='(A)',iostat=E_IO)'SCALARS '//trim(varname)//' double 1'
    write(unit=Unit_VTK,fmt='(A)',iostat=E_IO)'LOOKUP_TABLE default'
    write(unit=Unit_VTK,fmt=FR8P, iostat=E_IO)var
  case(f_out_binary)
    write(unit=Unit_VTK,iostat=E_IO)'SCALARS '//trim(varname)//' double 1'//end_rec
    write(unit=Unit_VTK,iostat=E_IO)'LOOKUP_TABLE default'//end_rec
    write(unit=Unit_VTK,iostat=E_IO)var
    write(unit=Unit_VTK,iostat=E_IO)end_rec
  endselect
  return
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction VTK_VAR_SCAL_R8

  function VTK_VAR_SCAL_R4(NC_NN,varname,var) result(E_IO)
  !---------------------------------------------------------------------------------------------------------------------------------
  !! Function for saving field of scalar variable (R4P).
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  implicit none
  integer(I4P), intent(IN):: NC_NN        ! number of nodes or cells
  character(*), intent(IN):: varname      ! variable name
  real(R4P),    intent(IN):: var(1:NC_NN) ! variable to be saved
  integer(I4P)::             E_IO         ! Input/Output inquiring flag: $0$ if IO is done, $> 0$ if IO is not done
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  select case(f_out)
  case(f_out_ascii)
    write(unit=Unit_VTK,fmt='(A)',iostat=E_IO)'SCALARS '//trim(varname)//' float 1'
    write(unit=Unit_VTK,fmt='(A)',iostat=E_IO)'LOOKUP_TABLE default'
    write(unit=Unit_VTK,fmt=FR4P, iostat=E_IO)var
  case(f_out_binary)
    write(unit=Unit_VTK,iostat=E_IO)'SCALARS '//trim(varname)//' float 1'//end_rec
    write(unit=Unit_VTK,iostat=E_IO)'LOOKUP_TABLE default'//end_rec
    write(unit=Unit_VTK,iostat=E_IO)var
    write(unit=Unit_VTK,iostat=E_IO)end_rec
  endselect
  return
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction VTK_VAR_SCAL_R4

  function VTK_VAR_SCAL_I4(NC_NN,varname,var) result(E_IO)
  !---------------------------------------------------------------------------------------------------------------------------------
  !! Function for saving field of scalar variable (I4P).
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  implicit none
  integer(I4P), intent(IN):: NC_NN        ! number of nodes or cells
  character(*), intent(IN):: varname      ! variable name
  integer(I4P), intent(IN):: var(1:NC_NN) ! variable to be saved
  integer(I4P)::             E_IO         ! Input/Output inquiring flag: $0$ if IO is done, $> 0$ if IO is not done
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  select case(f_out)
  case(f_out_ascii)
    write(unit=Unit_VTK,fmt='(A)',iostat=E_IO)'SCALARS '//trim(varname)//' int 1'
    write(unit=Unit_VTK,fmt='(A)',iostat=E_IO)'LOOKUP_TABLE default'
    write(unit=Unit_VTK,fmt=FI4P, iostat=E_IO)var
  case(f_out_binary)
    write(unit=Unit_VTK,iostat=E_IO)'SCALARS '//trim(varname)//' int 1'//end_rec
    write(unit=Unit_VTK,iostat=E_IO)'LOOKUP_TABLE default'//end_rec
    write(unit=Unit_VTK,iostat=E_IO)var
    write(unit=Unit_VTK,iostat=E_IO)end_rec
  endselect
  return
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction VTK_VAR_SCAL_I4

  function VTK_VAR_VECT_R8(vec_type,NC_NN,varname,varX,varY,varZ) result(E_IO)
  !---------------------------------------------------------------------------------------------------------------------------------
  !! Function for saving field of vectorial variable (R8P).
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  implicit none
  character(*), intent(IN):: vec_type      ! vector type: vect = generic vector , norm = normal vector
  integer(I4P), intent(IN):: NC_NN         ! number of nodes or cells
  character(*), intent(IN):: varname       ! variable name
  real(R8P),    intent(IN):: varX(1:NC_NN) ! x component of vector
  real(R8P),    intent(IN):: varY(1:NC_NN) ! y component of vector
  real(R8P),    intent(IN):: varZ(1:NC_NN) ! z component of vector
  integer(I4P)::             E_IO          ! Input/Output inquiring flag: $0$ if IO is done, $> 0$ if IO is not done
  integer(I8P)::             n1            ! counter
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  select case(f_out)
  case(f_out_ascii)
    select case(Upper_Case(trim(vec_type)))
    case('VECT')
      write(unit=Unit_VTK,fmt='(A)',          iostat=E_IO)'VECTORS '//trim(varname)//' double'
    case('NORM')
      write(unit=Unit_VTK,fmt='(A)',          iostat=E_IO)'NORMALS '//trim(varname)//' double'
    endselect
    write(unit=Unit_VTK,fmt='(3'//FR8P//')',iostat=E_IO)(varX(n1),varY(n1),varZ(n1),n1=1,NC_NN)
  case(f_out_binary)
    select case(Upper_Case(trim(vec_type)))
    case('VECT')
      write(unit=Unit_VTK,iostat=E_IO)'VECTORS '//trim(varname)//' double'//end_rec
    case('NORM')
      write(unit=Unit_VTK,iostat=E_IO)'NORMALS '//trim(varname)//' double'//end_rec
    endselect
    write(unit=Unit_VTK,iostat=E_IO)(varX(n1),varY(n1),varZ(n1),n1=1,NC_NN)
    write(unit=Unit_VTK,iostat=E_IO)end_rec
  endselect
  return
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction VTK_VAR_VECT_R8

  function VTK_VAR_VECT_R4(vec_type,NC_NN,varname,varX,varY,varZ) result(E_IO)
  !---------------------------------------------------------------------------------------------------------------------------------
  !! Function for saving field of vectorial variable (R4P).
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  implicit none
  character(*), intent(IN):: vec_type      ! vector type: vect = generic vector , norm = normal vector
  integer(I4P), intent(IN):: NC_NN         ! number of nodes or cells
  character(*), intent(IN):: varname       ! variable name
  real(R4P),    intent(IN):: varX(1:NC_NN) ! x component of vector
  real(R4P),    intent(IN):: varY(1:NC_NN) ! y component of vector
  real(R4P),    intent(IN):: varZ(1:NC_NN) ! z component of vector
  integer(I4P)::             E_IO          ! Input/Output inquiring flag: $0$ if IO is done, $> 0$ if IO is not done
  integer(I8P)::             n1            ! counter
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  select case(f_out)
  case(f_out_ascii)
    select case(Upper_Case(trim(vec_type)))
    case('vect')
      write(unit=Unit_VTK,fmt='(A)',          iostat=E_IO)'VECTORS '//trim(varname)//' float'
    case('norm')
      write(unit=Unit_VTK,fmt='(A)',          iostat=E_IO)'NORMALS '//trim(varname)//' float'
    endselect
    write(unit=Unit_VTK,fmt='(3'//FR4P//')',iostat=E_IO)(varX(n1),varY(n1),varZ(n1),n1=1,NC_NN)
  case(f_out_binary)
    select case(Upper_Case(trim(vec_type)))
    case('vect')
      write(unit=Unit_VTK,iostat=E_IO)'VECTORS '//trim(varname)//' float'//end_rec
    case('norm')
      write(unit=Unit_VTK,iostat=E_IO)'NORMALS '//trim(varname)//' float'//end_rec
    endselect
    write(unit=Unit_VTK,iostat=E_IO)(varX(n1),varY(n1),varZ(n1),n1=1,NC_NN)
    write(unit=Unit_VTK,iostat=E_IO)end_rec
  endselect
  return
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction VTK_VAR_VECT_R4

  function VTK_VAR_VECT_I4(NC_NN,varname,varX,varY,varZ) result(E_IO)
  !---------------------------------------------------------------------------------------------------------------------------------
  !! Function for saving field of vectorial variable (I4P).
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  implicit none
  integer(I4P), intent(IN):: NC_NN         ! number of nodes or cells
  character(*), intent(IN):: varname       ! variable name
  integer(I4P), intent(IN):: varX(1:NC_NN) ! x component of vector
  integer(I4P), intent(IN):: varY(1:NC_NN) ! y component of vector
  integer(I4P), intent(IN):: varZ(1:NC_NN) ! z component of vector
  integer(I4P)::             E_IO          ! Input/Output inquiring flag: $0$ if IO is done, $> 0$ if IO is not done
  integer(I8P)::             n1            ! counter
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  select case(f_out)
  case(f_out_ascii)
    write(unit=Unit_VTK,fmt='(A)',iostat=E_IO)'VECTORS '//trim(varname)//' int'
    write(unit=Unit_VTK,fmt='(3'//FI4P//')',iostat=E_IO)(varX(n1),varY(n1),varZ(n1),n1=1,NC_NN)
  case(f_out_binary)
    write(unit=Unit_VTK,iostat=E_IO)'VECTORS '//trim(varname)//' int'//end_rec
    write(unit=Unit_VTK,iostat=E_IO)(varX(n1),varY(n1),varZ(n1),n1=1,NC_NN)
    write(unit=Unit_VTK,iostat=E_IO)end_rec
  endselect
  return
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction VTK_VAR_VECT_I4

  function VTK_VAR_TEXT_R8(NC_NN,dimm,varname,textCoo) result(E_IO)
  !---------------------------------------------------------------------------------------------------------------------------------
  !! Function for saving texture variable (R8P).
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  implicit none
  integer(I4P), intent(IN):: NC_NN                   ! number of nodes or cells
  integer(I4P), intent(IN):: dimm                    ! texture dimensions
  character(*), intent(IN):: varname                 ! variable name
  real(R8P),    intent(IN):: textCoo(1:NC_NN,1:dimm) ! texture
  integer(I4P)::             E_IO                    ! Input/Output inquiring flag: $0$ if IO is done, $> 0$ if IO is not done
  character(len=maxlen)::    s_buffer                ! buffer string
  integer(I8P)::             n1,n2                   ! counters
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  select case(f_out)
  case(f_out_ascii)
    write(unit=Unit_VTK,fmt='(A,1X,'//FI4P//',1X,A)',iostat=E_IO)'TEXTURE_COORDINATES '//trim(varname),dimm,' double'
    write(s_buffer,fmt='(I1)',iostat=E_IO)dimm
    s_buffer='('//trim(s_buffer)//FR4P//')'
    write(unit=Unit_VTK,fmt=trim(s_buffer),iostat=E_IO)((textCoo(n1,n2),n2=1,dimm),n1=1,NC_NN)
  case(f_out_binary)
    write(s_buffer,fmt='(A,1X,'//FI4P//',1X,A)',iostat=E_IO)'TEXTURE_COORDINATES '//trim(varname),dimm,' double'
    write(unit=Unit_VTK,iostat=E_IO)trim(s_buffer)//end_rec
    write(unit=Unit_VTK,iostat=E_IO)((textCoo(n1,n2),n2=1,dimm),n1=1,NC_NN)
    write(unit=Unit_VTK,iostat=E_IO)end_rec
  endselect
  return
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction VTK_VAR_TEXT_R8

  function VTK_VAR_TEXT_R4(NC_NN,dimm,varname,textCoo) result(E_IO)
  !---------------------------------------------------------------------------------------------------------------------------------
  !! Function for saving texture variable (R4P).
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  implicit none
  integer(I4P), intent(IN):: NC_NN                   ! number of nodes or cells
  integer(I4P), intent(IN):: dimm                    ! texture dimensions
  character(*), intent(IN):: varname                 ! variable name
  real(R4P),    intent(IN):: textCoo(1:NC_NN,1:dimm) ! texture
  integer(I4P)::             E_IO                    ! Input/Output inquiring flag: $0$ if IO is done, $> 0$ if IO is not done
  character(len=maxlen)::    s_buffer                ! buffer string
  integer(I8P)::             n1,n2                   ! counters
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  select case(f_out)
  case(f_out_ascii)
    write(unit=Unit_VTK,fmt='(A,1X,'//FI4P//',1X,A)',iostat=E_IO)'TEXTURE_COORDINATES '//trim(varname),dimm,' float'
    write(s_buffer,fmt='(I1)',iostat=E_IO)dimm
    s_buffer='('//trim(s_buffer)//FR4P//')'
    write(unit=Unit_VTK,fmt=trim(s_buffer),iostat=E_IO)((textCoo(n1,n2),n2=1,dimm),n1=1,NC_NN)
  case(f_out_binary)
    write(s_buffer,fmt='(A,1X,'//FI4P//',1X,A)',iostat=E_IO)'TEXTURE_COORDINATES '//trim(varname),dimm,' float'
    write(unit=Unit_VTK,iostat=E_IO)trim(s_buffer)//end_rec
    write(unit=Unit_VTK,iostat=E_IO)((textCoo(n1,n2),n2=1,dimm),n1=1,NC_NN)
    write(unit=Unit_VTK,iostat=E_IO)end_rec
  endselect
  return
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction VTK_VAR_TEXT_R4
  !(doc/)skippedblock

  function VTK_END() result(E_IO)
  !---------------------------------------------------------------------------------------------------------------------------------
  !!This function is used to finalize the file opened and it has not inputs. The \LIBVTKIO manages the file unit without the
  !!user's action.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  implicit none
  integer(I4P):: E_IO ! Input/Output inquiring flag: $0$ if IO is done, $> 0$ if IO is not done
  !!The VTK\_END variables have the following meaning:
  !!
  !!\begin{description}
  !! \item[{\color{RoyalBlue}E\_IO}] contains the inquiring integer flag for error handling.
  !!\end{description}
  !!
  !!The following is an example of VTK\_END calling:
  !!
  !!\begin{boxred}{VTK\_END Calling}
  !!\begin{verbatim}
  !!...
  !!E_IO = VTK_END()
  !!...
  !!\end{verbatim}
  !!\end{boxred}
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  close(unit=Unit_VTK,iostat=E_IO)
  return
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction VTK_END

  !!\chapter{VTK XML functions}
  !!\minitoc
  !!\vspace*{8mm}
  !!
  !!\lettrine[lines=2,loversize=-.1,lraise=0.2]{{\bf T}}{he} XML standard is more powerful than legacy one. It is more flexible
  !!and free but on the other hand is more (but not so more using a library like \LIBVTKIO...) complex than legacy standard. The
  !!output of XML functions is a well-formated XML file at least for the ascii format (in the binary format \LIBVTKIO use
  !!raw-data format that does not produce a well formated XML file).
  !!
  !!The XML functions follow the same calling-convention of the legacy functions; all the \LIBVTKIO XML functions are
  !!\MaiuscolettoBS{4-byte integer function}: the output of these functions is an integer that is $0$ if the function calling
  !!has been done right while it is $> 0$  if some errors occur. The functions calling is the same as legacy functions:
  !!
  !!\begin{boxred}{Functions Calling}
  !!\begin{verbatim}
  !!...
  !!integer(4):: E_IO
  !!...
  !!E_IO = VTK_INI_XML(....
  !!...
  !!\end{verbatim}
  !!\end{boxred}
  !!
  !!\noindent Note that the XML functions have the same name of legacy functions with the suffix \virgo{\_XML}.
  !!
  function VTK_INI_XML(output_format,filename,mesh_topology,nx1,nx2,ny1,ny2,nz1,nz2) result(E_IO)
  !---------------------------------------------------------------------------------------------------------------------------------
  !!The VTK\_INI\_XML function is used for initializing file. This function must be the first to be called.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  implicit none
  character(*), intent(IN)::           output_format ! output format: ASCII or BINARY
  character(*), intent(IN)::           filename      ! file name
  character(*), intent(IN)::           mesh_topology ! mesh topology
  integer(I4P), intent(IN), optional:: nx1,nx2       ! initial and final nodes of x axis
  integer(I4P), intent(IN), optional:: ny1,ny2       ! initial and final nodes of y axis
  integer(I4P), intent(IN), optional:: nz1,nz2       ! initial and final nodes of z axis
  integer(I4P)::                       E_IO          ! Input/Output inquiring flag: $0$ if IO is done, $> 0$ if IO is not done
  character(len=maxlen)::              s_buffer      ! buffer string
  !!The VTK\_INI\_XML variables have the following meaning:
  !!
  !!\begin{description}
  !!\item[{\color{RoyalBlue}output\_format}] indicates the \virgo{format} of output file. It can assume the following values:
  !! \begin{enumerateABlu}
  !!  \item \emph{ascii} (it is case insensitive) $\rightarrow$ creating an ascii output file.
  !!  \item \emph{binary} (it is case insensitive) $\rightarrow$ creating a binary (big\_endian encoding) output file.
  !! \end{enumerateABlu}
  !! \item[{\color{RoyalBlue}filename}] contains the name (with its path) of the output file.
  !! \item[{\color{RoyalBlue}topology}] indicates the topology of the mesh and can assume the following values:
  !! \begin{enumerateABlu}
  !!  \item \emph{StructuredGrid}.
  !!  \item \emph{RectilinearGrid}.
  !!  \item \emph{UnstructuredGrid}.
  !! \end{enumerateABlu}
  !! \item[{\color{RoyalBlue}nx1,nx2}] contains the extent of X axis; $nx1$ is the initial node and $nx2$ is the final.
  !! \item[{\color{RoyalBlue}ny1,ny2}] contains the extent of Y axis; $ny1$ is the initial node and $ny2$ is the final.
  !! \item[{\color{RoyalBlue}nz1,nz2}] contains the extent of Z axis; $nz1$ is the initial node and $nz2$ is the final.
  !! \item[{\color{RoyalBlue}E\_IO}] contains the inquiring integer flag for error handling.
  !!\end{description}
  !!
  !!This function is quite more complex than the rispective legacy function; it needs more inputs: the XML standard needs more
  !!informations to initialize the file.
  !!
  !!The following is an example of VTK\_INI\_XML calling:
  !!
  !!\begin{boxred}{VTK\_INI\_XML Calling}
  !!\begin{verbatim}
  !!...
  !!...
  !!E_IO = VTK_INI_XML('BINARY','XML_RECT_BINARY.vtr', &
  !!                   'RectilinearGrid',              &
  !!                   nx1=nx1,nx2=nx2,                &
  !!                   ny1=ny1,ny2=ny2,                &
  !!                   nz1=nz1,nz2=nz2)
  !!...
  !!\end{verbatim}
  !!\end{boxred}
  !!
  !!\noindent Note that the file extension is necessary in the file name. The XML standard has different extensions for each
  !!different topologies (i.e. \MaiuscolettoBS{.vtr} for rectilinear topology). See the VTK-standard file for more information.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  ! checking the endian bit ordering of the architecture
  call check_endian()
  topology = trim(mesh_topology)
  Unit_VTK=GetUnit()
  select case(trim(Upper_Case(output_format)))
  case('ASCII')
    f_out = f_out_ascii
    open(unit   = Unit_VTK,       &
         file   = trim(filename), &
         form   = 'FORMATTED',    &
         access = 'SEQUENTIAL',   &
         action = 'WRITE',        &
         iostat = E_IO)
    ! writing header of file
    write(unit=Unit_VTK,fmt='(A)',iostat=E_IO)'<?xml version="1.0"?>'
    if (endian==endianL) then
      write(unit=Unit_VTK,fmt='(A)',iostat=E_IO)'<VTKFile type="'//trim(topology)//'" version="0.1" byte_order="LittleEndian">'
    else
      write(unit=Unit_VTK,fmt='(A)',iostat=E_IO)'<VTKFile type="'//trim(topology)//'" version="0.1" byte_order="BigEndian">'
    endif
    indent = 2
    select case(trim(topology))
    case('RectilinearGrid','StructuredGrid')
      write(unit=Unit_VTK,fmt='(A,6'//FI4P//',A)',iostat=E_IO)repeat(' ',indent)//     &
                                                              '<'//                    &
                                                              trim(topology)//         &
                                                              ' WholeExtent="',        &
                                                              nx1,nx2,ny1,ny2,nz1,nz2, &
                                                              '">'
    case('UnstructuredGrid')
      write(unit=Unit_VTK,fmt='(A)',iostat=E_IO)repeat(' ',indent)//'<'//trim(topology)//'>'
    endselect
    indent = indent + 2
  case('BINARY')
    f_out = f_out_binary
    open(unit       = Unit_VTK,       &
         file       = trim(filename), &
         form       = 'UNFORMATTED',  &
         access     = 'STREAM',       &
         action     = 'WRITE',        &
         iostat     = E_IO)
    ! writing header of file
    write(unit=Unit_VTK,iostat=E_IO)'<?xml version="1.0"?>'//end_rec
    if (endian==endianL) then
      write(unit=Unit_VTK,iostat=E_IO)'<VTKFile type="'//trim(topology)//'" version="0.1" byte_order="LittleEndian">'//end_rec
    else
      write(unit=Unit_VTK,iostat=E_IO)'<VTKFile type="'//trim(topology)//'" version="0.1" byte_order="BigEndian">'//end_rec
    endif
    indent = 2
    select case(trim(topology))
    case('RectilinearGrid','StructuredGrid')
      write(s_buffer,fmt='(A,6'//FI4P//',A)',iostat=E_IO)repeat(' ',indent)//     &
                                                         '<'//                    &
                                                         trim(topology)//         &
                                                         ' WholeExtent="',        &
                                                         nx1,nx2,ny1,ny2,nz1,nz2, &
                                                         '">'
    case('UnstructuredGrid')
      write(s_buffer,fmt='(A)',iostat=E_IO)repeat(' ',indent)//'<'//trim(topology)//'>'
    endselect
    write(unit=Unit_VTK,iostat=E_IO)trim(s_buffer)//end_rec
    indent = indent + 2
    Unit_VTK_Append=GetUnit()
    ! opening the SCRATCH file used for appending raw binary data
    open(unit       = Unit_VTK_Append, &
         form       = 'UNFORMATTED',   &
         access     = 'STREAM',        &
         action     = 'READWRITE',     &
         status     = 'SCRATCH',       &
         iostat     = E_IO)
    ioffset = 0 ! initializing offset puntator
  endselect
  return
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction VTK_INI_XML

  !(doc)INCLUDE{/home/szaghi/VTK_IO/LIB_VTK_IO/source/DOC_VTK_GEO_XML.doc}
  !(\doc)skippedblock
  function VTK_GEO_XML_STRG_R8(nx1,nx2,ny1,ny2,nz1,nz2,NN,X,Y,Z) result(E_IO)
  !---------------------------------------------------------------------------------------------------------------------------------
  !! Function for saving mesh; topology = StructuredGrid (R8P).
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  implicit none
  integer(I4P), intent(IN):: nx1,nx2  ! initial and final nodes of x axis
  integer(I4P), intent(IN):: ny1,ny2  ! initial and final nodes of y axis
  integer(I4P), intent(IN):: nz1,nz2  ! initial and final nodes of z axis
  integer(I4P), intent(IN):: NN       ! number of all nodes
  real(R8P),    intent(IN):: X(1:NN)  ! x coordinates
  real(R8P),    intent(IN):: Y(1:NN)  ! y coordinates
  real(R8P),    intent(IN):: Z(1:NN)  ! z coordinates
  integer(I4P)::             E_IO     ! Input/Output inquiring flag: $0$ if IO is done, $> 0$ if IO is not done
  character(len=maxlen)::    s_buffer ! buffer string
  integer(I4P)::             n1       ! counter
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  select case(f_out)
  case(f_out_ascii)
    write(unit=Unit_VTK,fmt='(A,6'//FI4P//',A)',iostat=E_IO)repeat(' ',indent)//'<Piece Extent="',nx1,nx2,ny1,ny2,nz1,nz2,'">'
    indent = indent + 2
    write(unit=Unit_VTK,fmt='(A)',iostat=E_IO)repeat(' ',indent)//'<Points>'
    indent = indent + 2
    write(unit=Unit_VTK,fmt='(A)',iostat=E_IO)repeat(' ',indent)// &
                                              '<DataArray type="Float64" NumberOfComponents="3" Name="Point" format="ascii">'
    write(unit=Unit_VTK,fmt='(3'//FR8P//')',iostat=E_IO)(X(n1),Y(n1),Z(n1),n1=1,NN)
    write(unit=Unit_VTK,fmt='(A)',iostat=E_IO)repeat(' ',indent)//'</DataArray>'
    indent = indent - 2
    write(unit=Unit_VTK,fmt='(A)',iostat=E_IO)repeat(' ',indent)//'</Points>'
  case(f_out_binary)
    write(s_buffer,fmt='(A,6'//FI4P//',A)',iostat=E_IO)repeat(' ',indent)//'<Piece Extent="',nx1,nx2,ny1,ny2,nz1,nz2,'">'
    indent = indent + 2
    write(unit=Unit_VTK,iostat=E_IO)trim(s_buffer)//end_rec
    write(unit=Unit_VTK,iostat=E_IO)repeat(' ',indent)//'<Points>'//end_rec
    indent = indent + 2
    write(unit=Unit_VTK,iostat=E_IO)repeat(' ',indent)//                                                                        &
                                    '<DataArray type="Float64" NumberOfComponents="3" Name="Point" format="appended" offset="', &
                                    trim(str(.true.,ioffset)),                                                                  &
                                    '">'//                                                                                      &
                                    end_rec
    N_Byte  = 3*NN*int(sizeof(Tipo_R8),I4P)
    ioffset = ioffset + int(sizeof(Tipo_I4),I4P) + N_Byte
    write(unit=Unit_VTK_Append,iostat=E_IO)N_Byte,'R8',3*NN
    write(unit=Unit_VTK_Append,iostat=E_IO)(X(n1),Y(n1),Z(n1),n1=1,NN)
    write(unit=Unit_VTK,iostat=E_IO)repeat(' ',indent)//'</DataArray>'//end_rec
    indent = indent - 2
    write(unit=Unit_VTK,iostat=E_IO)repeat(' ',indent)//'</Points>'//end_rec
  endselect
  return
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction VTK_GEO_XML_STRG_R8

  function VTK_GEO_XML_STRG_R4(nx1,nx2,ny1,ny2,nz1,nz2,NN,X,Y,Z) result(E_IO)
  !---------------------------------------------------------------------------------------------------------------------------------
  !! Function for saving mesh; topology = StructuredGrid (R4P).
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  implicit none
  integer(I4P), intent(IN):: nx1,nx2  ! initial and final nodes of x axis
  integer(I4P), intent(IN):: ny1,ny2  ! initial and final nodes of y axis
  integer(I4P), intent(IN):: nz1,nz2  ! initial and final nodes of z axis
  integer(I4P), intent(IN):: NN       ! number of all nodes
  real(R4P),    intent(IN):: X(1:NN)  ! x coordinates
  real(R4P),    intent(IN):: Y(1:NN)  ! y coordinates
  real(R4P),    intent(IN):: Z(1:NN)  ! z coordinates
  integer(I4P)::             E_IO     ! Input/Output inquiring flag: $0$ if IO is done, $> 0$ if IO is not done
  character(len=maxlen)::    s_buffer ! buffer string
  integer(I4P)::             n1       ! counter
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  select case(f_out)
  case(f_out_ascii)
    write(unit=Unit_VTK,fmt='(A,6'//FI4P//',A)',iostat=E_IO)repeat(' ',indent)//'<Piece Extent="',nx1,nx2,ny1,ny2,nz1,nz2,'">'
    indent = indent + 2
    write(unit=Unit_VTK,fmt='(A)',iostat=E_IO)repeat(' ',indent)//'<Points>'
    indent = indent + 2
    write(unit=Unit_VTK,fmt='(A)',iostat=E_IO)repeat(' ',indent)// &
                                              '<DataArray type="Float32" NumberOfComponents="3" Name="Point" format="ascii">'
    write(unit=Unit_VTK,fmt='(3'//FR4P//')',iostat=E_IO)(X(n1),Y(n1),Z(n1),n1=1,NN)
    write(unit=Unit_VTK,fmt='(A)',iostat=E_IO)repeat(' ',indent)//'</DataArray>'
    indent = indent - 2
    write(unit=Unit_VTK,fmt='(A)',iostat=E_IO)repeat(' ',indent)//'</Points>'
  case(f_out_binary)
    write(s_buffer,fmt='(A,6'//FI4P//',A)',iostat=E_IO)repeat(' ',indent)//'<Piece Extent="',nx1,nx2,ny1,ny2,nz1,nz2,'">'
    indent = indent + 2
    write(unit=Unit_VTK,iostat=E_IO)trim(s_buffer)//end_rec
    write(unit=Unit_VTK,iostat=E_IO)repeat(' ',indent)//'<Points>'//end_rec
    indent = indent + 2
    write(unit=Unit_VTK,iostat=E_IO)repeat(' ',indent)//                                                                        &
                                    '<DataArray type="Float32" NumberOfComponents="3" Name="Point" format="appended" offset="', &
                                    trim(str(.true.,ioffset)),                                                                  &
                                    '">'//                                                                                      &
                                    end_rec
    N_Byte  = 3*NN*int(sizeof(Tipo_R4),I4P)
    ioffset = ioffset + int(sizeof(Tipo_I4),I4P) + N_Byte
    write(unit=Unit_VTK_Append,iostat=E_IO)N_Byte,'R4',3*NN
    write(unit=Unit_VTK_Append,iostat=E_IO)(X(n1),Y(n1),Z(n1),n1=1,NN)
    write(unit=Unit_VTK,iostat=E_IO)repeat(' ',indent)//'</DataArray>'//end_rec
    indent = indent - 2
    write(unit=Unit_VTK,iostat=E_IO)repeat(' ',indent)//'</Points>'//end_rec
  endselect
  return
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction VTK_GEO_XML_STRG_R4

  function VTK_GEO_XML_RECT_R8(nx1,nx2,ny1,ny2,nz1,nz2,X,Y,Z) result(E_IO)
  !---------------------------------------------------------------------------------------------------------------------------------
  !! Function for saving mesh; topology = RectilinearGrid (R8P).
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  implicit none
  integer(I4P), intent(IN):: nx1,nx2    ! initial and final nodes of x axis
  integer(I4P), intent(IN):: ny1,ny2    ! initial and final nodes of y axis
  integer(I4P), intent(IN):: nz1,nz2    ! initial and final nodes of z axis
  real(R8P),    intent(IN):: X(nx1:nx2) ! x coordinates
  real(R8P),    intent(IN):: Y(ny1:ny2) ! y coordinates
  real(R8P),    intent(IN):: Z(nz1:nz2) ! z coordinates
  integer(I4P)::             E_IO       ! Input/Output inquiring flag: $0$ if IO is done, $> 0$ if IO is not done
  character(len=maxlen)::    s_buffer   ! buffer string
  integer(I4P)::             n1         ! counter
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  select case(f_out)
  case(f_out_ascii)
    write(unit=Unit_VTK,fmt='(A,6'//FI4P//',A)',iostat=E_IO)repeat(' ',indent)//'<Piece Extent="',nx1,nx2,ny1,ny2,nz1,nz2,'">'
    indent = indent + 2
    write(unit=Unit_VTK,fmt='(A)',iostat=E_IO)repeat(' ',indent)//'<Coordinates>'
    indent = indent + 2
    write(unit=Unit_VTK,fmt='(A)',iostat=E_IO)repeat(' ',indent)//'<DataArray type="Float64" Name="X" format="ascii">'
    write(unit=Unit_VTK,fmt=FR8P, iostat=E_IO)(X(n1),n1=nx1,nx2)
    write(unit=Unit_VTK,fmt='(A)',iostat=E_IO)repeat(' ',indent)//'</DataArray>'
    write(unit=Unit_VTK,fmt='(A)',iostat=E_IO)repeat(' ',indent)//'<DataArray type="Float64" Name="Y" format="ascii">'
    write(unit=Unit_VTK,fmt=FR8P, iostat=E_IO)(Y(n1),n1=ny1,ny2)
    write(unit=Unit_VTK,fmt='(A)',iostat=E_IO)repeat(' ',indent)//'</DataArray>'
    write(unit=Unit_VTK,fmt='(A)',iostat=E_IO)repeat(' ',indent)//'<DataArray type="Float64" Name="Z" format="ascii">'
    write(unit=Unit_VTK,fmt=FR8P, iostat=E_IO)(Z(n1),n1=nz1,nz2)
    write(unit=Unit_VTK,fmt='(A)',iostat=E_IO)repeat(' ',indent)//'</DataArray>'
    indent = indent - 2
    write(unit=Unit_VTK,fmt='(A)',iostat=E_IO)repeat(' ',indent)//'</Coordinates>'
  case(f_out_binary)
    write(s_buffer,fmt='(A,6'//FI4P//',A)',iostat=E_IO)repeat(' ',indent)//'<Piece Extent="',nx1,nx2,ny1,ny2,nz1,nz2,'">'
    indent = indent + 2
    write(unit=Unit_VTK,iostat=E_IO)trim(s_buffer)//end_rec
    write(unit=Unit_VTK,iostat=E_IO)repeat(' ',indent)//'<Coordinates>'//end_rec
    indent = indent + 2
    write(unit=Unit_VTK,iostat=E_IO)repeat(' ',indent)//                                             &
                                    '<DataArray type="Float64" Name="X" format="appended" offset="', &
                                    trim(str(.true.,ioffset)),                                       &
                                    '">'//                                                           &
                                    end_rec
    N_Byte  = (nx2-nx1+1)*int(sizeof(Tipo_R8),I4P)
    ioffset = ioffset + int(sizeof(Tipo_I4),I4P) + N_Byte
    write(unit=Unit_VTK_Append,iostat=E_IO)N_Byte,'R8',nx2-nx1+1
    write(unit=Unit_VTK_Append,iostat=E_IO)(X(n1),n1=nx1,nx2)
    write(unit=Unit_VTK,iostat=E_IO)repeat(' ',indent)//'</DataArray>'//end_rec
    write(unit=Unit_VTK,iostat=E_IO)repeat(' ',indent)// &
                                    '<DataArray type="Float64" Name="Y" format="appended" offset="', &
                                    trim(str(.true.,ioffset)),                                       &
                                    '">'//                                                           &
                                    end_rec
    N_Byte  = (ny2-ny1+1)*int(sizeof(Tipo_R8),I4P)
    ioffset = ioffset + int(sizeof(Tipo_I4),I4P) + N_Byte
    write(unit=Unit_VTK_Append,iostat=E_IO)N_Byte,'R8',ny2-ny1+1
    write(unit=Unit_VTK_Append,iostat=E_IO)(Y(n1),n1=ny1,ny2)
    write(unit=Unit_VTK,iostat=E_IO)repeat(' ',indent)//'</DataArray>'//end_rec
    write(unit=Unit_VTK,iostat=E_IO)repeat(' ',indent)//                                             &
                                    '<DataArray type="Float64" Name="Z" format="appended" offset="', &
                                    trim(str(.true.,ioffset)),                                       &
                                    '">'//                                                           &
                                    end_rec
    N_Byte  = (nz2-nz1+1)*int(sizeof(Tipo_R8),I4P)
    ioffset = ioffset + int(sizeof(Tipo_I4),I4P) + N_Byte
    write(unit=Unit_VTK_Append,iostat=E_IO)N_Byte,'R8',nz2-nz1+1
    write(unit=Unit_VTK_Append,iostat=E_IO)(Z(n1),n1=nz1,nz2)
    write(unit=Unit_VTK,iostat=E_IO)repeat(' ',indent)//'</DataArray>'//end_rec
    indent = indent - 2
    write(unit=Unit_VTK,iostat=E_IO)repeat(' ',indent)//'</Coordinates>'//end_rec
  endselect
  return
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction VTK_GEO_XML_RECT_R8

  function VTK_GEO_XML_RECT_R4(nx1,nx2,ny1,ny2,nz1,nz2,X,Y,Z) result(E_IO)
  !---------------------------------------------------------------------------------------------------------------------------------
  !! Function for saving mesh; topology = RectilinearGrid (R4P).
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  implicit none
  integer(I4P), intent(IN):: nx1,nx2    ! initial and final nodes of x axis
  integer(I4P), intent(IN):: ny1,ny2    ! initial and final nodes of y axis
  integer(I4P), intent(IN):: nz1,nz2    ! initial and final nodes of z axis
  real(R4P),    intent(IN):: X(nx1:nx2) ! x coordinates
  real(R4P),    intent(IN):: Y(ny1:ny2) ! y coordinates
  real(R4P),    intent(IN):: Z(nz1:nz2) ! z coordinates
  integer(I4P)::             E_IO       ! Input/Output inquiring flag: $0$ if IO is done, $> 0$ if IO is not done
  character(len=maxlen)::    s_buffer   ! buffer string
  integer(I4P)::             n1         ! counter
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  select case(f_out)
  case(f_out_ascii)
    write(unit=Unit_VTK,fmt='(A,6'//FI4P//',A)',iostat=E_IO)repeat(' ',indent)//'<Piece Extent="',nx1,nx2,ny1,ny2,nz1,nz2,'">'
    indent = indent + 2
    write(unit=Unit_VTK,fmt='(A)',iostat=E_IO)repeat(' ',indent)//'<Coordinates>'
    indent = indent + 2
    write(unit=Unit_VTK,fmt='(A)',iostat=E_IO)repeat(' ',indent)//'<DataArray type="Float32" Name="X" format="ascii">'
    write(unit=Unit_VTK,fmt=FR4P, iostat=E_IO)(X(n1),n1=nx1,nx2)
    write(unit=Unit_VTK,fmt='(A)',iostat=E_IO)repeat(' ',indent)//'</DataArray>'
    write(unit=Unit_VTK,fmt='(A)',iostat=E_IO)repeat(' ',indent)//'<DataArray type="Float32" Name="Y" format="ascii">'
    write(unit=Unit_VTK,fmt=FR4P, iostat=E_IO)(Y(n1),n1=ny1,ny2)
    write(unit=Unit_VTK,fmt='(A)',iostat=E_IO)repeat(' ',indent)//'</DataArray>'
    write(unit=Unit_VTK,fmt='(A)',iostat=E_IO)repeat(' ',indent)//'<DataArray type="Float32" Name="Z" format="ascii">'
    write(unit=Unit_VTK,fmt=FR4P, iostat=E_IO)(Z(n1),n1=nz1,nz2)
    write(unit=Unit_VTK,fmt='(A)',iostat=E_IO)repeat(' ',indent)//'</DataArray>'
    indent = indent - 2
    write(unit=Unit_VTK,fmt='(A)',iostat=E_IO)repeat(' ',indent)//'</Coordinates>'
  case(f_out_binary)
    write(s_buffer,fmt='(A,6'//FI4P//',A)',iostat=E_IO)repeat(' ',indent)//'<Piece Extent="',nx1,nx2,ny1,ny2,nz1,nz2,'">'
    indent = indent + 2
    write(unit=Unit_VTK,iostat=E_IO)trim(s_buffer)//end_rec
    write(unit=Unit_VTK,iostat=E_IO)repeat(' ',indent)//'<Coordinates>'//end_rec
    indent = indent + 2
    write(unit=Unit_VTK,iostat=E_IO)repeat(' ',indent)//                                             &
                                    '<DataArray type="Float32" Name="X" format="appended" offset="', &
                                    trim(str(.true.,ioffset)),                                       &
                                    '">'//                                                           &
                                    end_rec
    N_Byte  = (nx2-nx1+1)*int(sizeof(Tipo_R4),I4P)
    ioffset = ioffset + int(sizeof(Tipo_I4),I4P) + N_Byte
    write(unit=Unit_VTK_Append,iostat=E_IO)N_Byte,'R4',nx2-nx1+1
    write(unit=Unit_VTK_Append,iostat=E_IO)(X(n1),n1=nx1,nx2)
    write(unit=Unit_VTK,iostat=E_IO)repeat(' ',indent)//'</DataArray>'//end_rec
    write(unit=Unit_VTK,iostat=E_IO)repeat(' ',indent)//                                             &
                                    '<DataArray type="Float32" Name="Y" format="appended" offset="', &
                                    trim(str(.true.,ioffset)),                                       &
                                    '">'//                                                           &
                                    end_rec
    N_Byte  = (ny2-ny1+1)*int(sizeof(Tipo_R4),I4P)
    ioffset = ioffset + int(sizeof(Tipo_I4),I4P) + N_Byte
    write(unit=Unit_VTK_Append,iostat=E_IO)N_Byte,'R4',ny2-ny1+1
    write(unit=Unit_VTK_Append,iostat=E_IO)(Y(n1),n1=ny1,ny2)
    write(unit=Unit_VTK,iostat=E_IO)repeat(' ',indent)//'</DataArray>'//end_rec
    write(unit=Unit_VTK,iostat=E_IO)repeat(' ',indent)//                                             &
                                    '<DataArray type="Float32" Name="Z" format="appended" offset="', &
                                    trim(str(.true.,ioffset)),                                       &
                                    '">'//                                                           &
                                    end_rec
    N_Byte  = (nz2-nz1+1)*int(sizeof(Tipo_R4),I4P)
    ioffset = ioffset + int(sizeof(Tipo_I4),I4P) + N_Byte
    write(unit=Unit_VTK_Append,iostat=E_IO)N_Byte,'R4',nz2-nz1+1
    write(unit=Unit_VTK_Append,iostat=E_IO)(Z(n1),n1=nz1,nz2)
    write(unit=Unit_VTK,iostat=E_IO)repeat(' ',indent)//'</DataArray>'//end_rec
    indent = indent - 2
    write(unit=Unit_VTK,iostat=E_IO)repeat(' ',indent)//'</Coordinates>'//end_rec
  endselect
  return
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction VTK_GEO_XML_RECT_R4

  function VTK_GEO_XML_UNST_R8(NN,NC,X,Y,Z) result(E_IO)
  !---------------------------------------------------------------------------------------------------------------------------------
  !! Function for saving mesh; topology = UnstructuredGrid (R8P).
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  implicit none
  integer(I4P), intent(IN):: NN       ! number of nodes
  integer(I4P), intent(IN):: NC       ! number of cells
  real(R8P),    intent(IN):: X(1:NN)  ! x coordinates
  real(R8P),    intent(IN):: Y(1:NN)  ! y coordinates
  real(R8P),    intent(IN):: Z(1:NN)  ! z coordinates
  integer(I4P)::             E_IO     ! Input/Output inquiring flag: $0$ if IO is done, $> 0$ if IO is not done
  character(len=maxlen)::    s_buffer ! buffer string
  integer(I4P)::             n1       ! counter
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  select case(f_out)
  case(f_out_ascii)
    write(unit=Unit_VTK,fmt='(A,'//FI4P//',A,'//FI4P//',A)',iostat=E_IO)repeat(' ',indent)//       &
                                                                        '<Piece NumberOfPoints="', &
                                                                        NN,                        &
                                                                        '" NumberOfCells="',       &
                                                                        NC,                        &
                                                                        '">'
    indent = indent + 2
    write(unit=Unit_VTK,fmt='(A)',iostat=E_IO)repeat(' ',indent)//'<Points>'
    indent = indent + 2
    write(unit=Unit_VTK,fmt='(A)',iostat=E_IO)repeat(' ',indent)// &
                                              '<DataArray type="Float64" NumberOfComponents="3" Name="Point" format="ascii">'
    write(unit=Unit_VTK,fmt='(3'//FR8P//')',iostat=E_IO)(X(n1),Y(n1),Z(n1),n1=1,NN)
    write(unit=Unit_VTK,fmt='(A)',iostat=E_IO)repeat(' ',indent)//'</DataArray>'
    indent = indent - 2
    write(unit=Unit_VTK,fmt='(A)',iostat=E_IO)repeat(' ',indent)//'</Points>'
  case(f_out_binary)
    write(s_buffer,fmt='(A,'//FI4P//',A,'//FI4P//',A)',iostat=E_IO)repeat(' ',indent)//       &
                                                                   '<Piece NumberOfPoints="', &
                                                                   NN,                        &
                                                                   '" NumberOfCells="',       &
                                                                   NC,                        &
                                                                   '">'
    indent = indent + 2
    write(unit=Unit_VTK,iostat=E_IO)trim(s_buffer)//end_rec
    write(unit=Unit_VTK,iostat=E_IO)repeat(' ',indent)//'<Points>'//end_rec
    indent = indent + 2
    write(unit=Unit_VTK,iostat=E_IO)repeat(' ',indent)// &
                                    '<DataArray type="Float64" NumberOfComponents="3" Name="Point" format="appended" offset="', &
                                    trim(str(.true.,ioffset)),                                                                  &
                                    '">'//                                                                                      &
                                    end_rec
    N_Byte  = 3*NN*int(sizeof(Tipo_R8),I4P)
    ioffset = ioffset + int(sizeof(Tipo_I4),I4P) + N_Byte
    write(unit=Unit_VTK_Append,iostat=E_IO)N_Byte,'R8',3*NN
    write(unit=Unit_VTK_Append,iostat=E_IO)(X(n1),Y(n1),Z(n1),n1=1,NN)
    write(unit=Unit_VTK,iostat=E_IO)repeat(' ',indent)//'</DataArray>'//end_rec
    indent = indent - 2
    write(unit=Unit_VTK,iostat=E_IO)repeat(' ',indent)//'</Points>'//end_rec
  endselect
  return
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction VTK_GEO_XML_UNST_R8

  function VTK_GEO_XML_UNST_R4(NN,NC,X,Y,Z) result(E_IO)
  !---------------------------------------------------------------------------------------------------------------------------------
  !! Function for saving mesh; topology = UnstructuredGrid (R4P).
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  implicit none
  integer(I4P), intent(IN):: NN       ! number of nodes
  integer(I4P), intent(IN):: NC       ! number of cells
  real(R4P),    intent(IN):: X(1:NN)  ! x coordinates
  real(R4P),    intent(IN):: Y(1:NN)  ! y coordinates
  real(R4P),    intent(IN):: Z(1:NN)  ! z coordinates
  integer(I4P)::             E_IO     ! Input/Output inquiring flag: $0$ if IO is done, $> 0$ if IO is not done
  character(len=maxlen)::    s_buffer ! buffer string
  integer(I4P)::             n1       ! counter
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  select case(f_out)
  case(f_out_ascii)
    write(unit=Unit_VTK,fmt='(A,'//FI4P//',A,'//FI4P//',A)',iostat=E_IO)repeat(' ',indent)//       &
                                                                        '<Piece NumberOfPoints="', &
                                                                        NN,                        &
                                                                        '" NumberOfCells="',       &
                                                                        NC,                        &
                                                                        '">'
    indent = indent + 2
    write(unit=Unit_VTK,fmt='(A)',iostat=E_IO)repeat(' ',indent)//'<Points>'
    indent = indent + 2
    write(unit=Unit_VTK,fmt='(A)',iostat=E_IO)repeat(' ',indent)// &
                                              '<DataArray type="Float32" NumberOfComponents="3" Name="Point" format="ascii">'
    write(unit=Unit_VTK,fmt='(3'//FR4P//')',iostat=E_IO)(X(n1),Y(n1),Z(n1),n1=1,NN)
    write(unit=Unit_VTK,fmt='(A)',iostat=E_IO)repeat(' ',indent)//'</DataArray>'
    indent = indent - 2
    write(unit=Unit_VTK,fmt='(A)',iostat=E_IO)repeat(' ',indent)//'</Points>'
  case(f_out_binary)
    write(s_buffer,fmt='(A,'//FI4P//',A,'//FI4P//',A)',iostat=E_IO)repeat(' ',indent)//       &
                                                                   '<Piece NumberOfPoints="', &
                                                                   NN,                        &
                                                                   '" NumberOfCells="',       &
                                                                   NC,                        &
                                                                   '">'
    indent = indent + 2
    write(unit=Unit_VTK,iostat=E_IO)trim(s_buffer)//end_rec
    write(unit=Unit_VTK,iostat=E_IO)repeat(' ',indent)//'<Points>'//end_rec
    indent = indent + 2
    write(unit=Unit_VTK,iostat=E_IO)repeat(' ',indent)//                                                                        &
                                    '<DataArray type="Float32" NumberOfComponents="3" Name="Point" format="appended" offset="', &
                                    trim(str(.true.,ioffset)),                                                                  &
                                    '">'//                                                                                      &
                                    end_rec
    N_Byte  = 3*NN*int(sizeof(Tipo_R4),I4P)
    ioffset = ioffset + int(sizeof(Tipo_I4),I4P) + N_Byte
    write(unit=Unit_VTK_Append,iostat=E_IO)N_Byte,'R4',3*NN
    write(unit=Unit_VTK_Append,iostat=E_IO)(X(n1),Y(n1),Z(n1),n1=1,NN)
    write(unit=Unit_VTK,iostat=E_IO)repeat(' ',indent)//'</DataArray>'//end_rec
    indent = indent - 2
    write(unit=Unit_VTK,iostat=E_IO)repeat(' ',indent)//'</Points>'//end_rec
  endselect
  return
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction VTK_GEO_XML_UNST_R4

  function VTK_GEO_XML_CLOSEP() result(E_IO)
  !---------------------------------------------------------------------------------------------------------------------------------
  !! Function for closing mesh block data.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  implicit none
  integer(I4P):: E_IO ! Input/Output inquiring flag: $0$ if IO is done, $> 0$ if IO is not done
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  indent = indent - 2
  select case(f_out)
  case(f_out_ascii)
    write(unit=Unit_VTK,fmt='(A)',iostat=E_IO)repeat(' ',indent)//'</Piece>'
  case(f_out_binary)
    write(unit=Unit_VTK,iostat=E_IO)repeat(' ',indent)//'</Piece>'//end_rec
  endselect
  return
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction VTK_GEO_XML_CLOSEP
  !(doc/)skippedblock

  function VTK_CON_XML(NC,connect,offset,cell_type) result(E_IO)
  !---------------------------------------------------------------------------------------------------------------------------------
  !!This function \MaiuscolettoBS{must} be used when unstructured grid is used. It saves the connectivity of the unstructured mesh.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  implicit none
  integer(I4P), intent(IN):: NC              ! number of cells
  integer(I4P), intent(IN):: connect(:)      ! mesh connectivity
  integer(I4P), intent(IN):: offset(1:NC)    ! cell offset
  integer(I1P), intent(IN):: cell_type(1:NC) ! VTK cell type
  integer(I4P)::             E_IO            ! Input/Output inquiring flag: $0$ if IO is done, $> 0$ if IO is not done
  integer(I4P)::             n1              ! counter
  !!The VTK\_CON\_XML variables have the following meaning:
  !!
  !!\begin{description}
  !! \item[{\color{RoyalBlue}NCelle}] indicates the number of all cells.
  !! \item[{\color{RoyalBlue}connect}] contains the connectivity of the mesh. It is a vector.
  !! \item[{\color{RoyalBlue}offset}] contains the offset\footnote{The summ of nodes of all previous cells included the
  !!                                  current cell.} of every cells. It is a vector of $[1:NCelle]$.
  !! \item[{\color{RoyalBlue}tipo}] contains the type of every cells. It is a vector of $[1:NCelle]$.
  !! \item[{\color{RoyalBlue}E\_IO}] contains the inquiring integer flag for error handling.
  !!\end{description}
  !!
  !!The vector \MaiuscolettoBS{connect} must follow the VTK XML standard. It is passed as \MaiuscolettoBS{assumed-shape}
  !!array because its dimensions is related to the mesh dimensions in a complex way. Its dimensions can be calculated by
  !!the following equation:
  !!
  !!\begin{equation}
  !!dc = \sum\limits_{i = 1}^{NCelle} {nvertex_i }
  !!\label{eq:xml connectivity dimensions}
  !!\end{equation}
  !!
  !!\noindent where $dc$ is connectivity vector dimension and $nvertex_i$ is the number of vertices of $i^{th}$ cell.
  !!Note that this equation is different from the legacy one (eq. \ref{eq:connectivity dimensions}). The XML connectivity
  !!convention is quite different from the legacy standard. As an example considering the same mesh of section \ref{sec:VTKCON}:
  !!suppose we have a mesh composed by 2 cells, one hexahedron (8 vertices) and one pyramid with square basis (5 vertices);
  !!suppose that the basis of pyramid is constitute by a face of the hexahedron and so the two cells share 4 vertices. The
  !!equation \ref{eq:xml connectivity dimensions} gives $dc=8+5=13$; the connectivity vector for this mesh can be:
  !!
  !!\begin{boxred}{Connectivity vector example for VTK XML standard}
  !!\begin{verbatim}
  !!! first cell
  !!connect(1)  = 0  => identification flag of 1° vertex of 1° cell
  !!connect(2)  = 1  => identification flag of 2° vertex of 1° cell
  !!connect(3)  = 2  => identification flag of 3° vertex of 1° cell
  !!connect(4)  = 3  => identification flag of 4° vertex of 1° cell
  !!connect(5)  = 4  => identification flag of 5° vertex of 1° cell
  !!connect(6)  = 5  => identification flag of 6° vertex of 1° cell
  !!connect(7)  = 6  => identification flag of 7° vertex of 1° cell
  !!connect(8)  = 7  => identification flag of 8° vertex of 1° cell
  !!! second cell
  !!connect(9)  = 0  => identification flag of 1° vertex of 2° cell
  !!connect(10) = 1  => identification flag of 2° vertex of 2° cell
  !!connect(11) = 2  => identification flag of 3° vertex of 2° cell
  !!connect(12) = 3  => identification flag of 4° vertex of 2° cell
  !!connect(13) = 8  => identification flag of 5° vertex of 2° cell
  !!\end{verbatim}
  !!\end{boxred}
  !!
  !!Therefore this connectivity vector convention is more simple than the legacy convention, now we must create also the
  !!\MaiuscolettoBS{offset} vector that contains the data now missing in the \MaiuscolettoBS{connect} vector. The offset
  !!vector for this mesh can be:
  !!
  !!\begin{boxred}{Offset vector example for VTK XML standard}
  !!\begin{verbatim}
  !!! first cell
  !!offset(1) = 8  => summ of nodes of 1° cell
  !!! second cell
  !!offset(2) = 13 => summ of nodes of 1° and 2° cells
  !!\end{verbatim}
  !!\end{boxred}
  !!
  !!\noindent The value of every cell-offset can be calculated by the following equation:
  !!
  !!\begin{equation}
  !!offset_c = \sum\limits_{i = 1}^{c} {nvertex_i }
  !!\label{eq:xml offset vlue}
  !!\end{equation}
  !!
  !!\noindent where $offset_c$ is the value of $c^{th}$ cell and $nvertex_i$ is the number of vertices of $i^{th}$ cell.
  !!
  !!The function VTK\_CON\_XML does not calculate the connectivity and offset vectors: it writes the connectivity and offset
  !!vectors conforming the VTK XML standard, but does not calculate them. In the future release of \LIBVTKIO will be included
  !!a function to calculate the connectivity and offset vector.
  !!
  !!The vector variable \MaiuscolettoBS{tipo} must conform the VTK XML standard \footnote{See the file VTK-Standard at the
  !!Kitware homepage.} that is the same of the legacy standard presented previous (sec. \ref{sec:VTKCON}). It contains the
  !!\emph{type} of each cells. For the above example this vector is:
  !!
  !!\begin{boxred}{Cell-Type vector example for VTK legacy standard}
  !!\begin{verbatim}
  !!tipo(1) = 12  => VTK hexahedron type of 1° cell
  !!tipo(2) = 14  => VTK pyramid type of 2° cell
  !!\end{verbatim}
  !!\end{boxred}
  !!
  !!The following is an example of VTK\_CON\_XML calling:
  !!
  !!\begin{boxred}{VTK\_CON\_XML Calling}
  !!\begin{verbatim}
  !!...
  !!integer(4), parameter:: NCelle=2
  !!integer(4), parameter:: Nvertex1=8
  !!integer(4), parameter:: Nvertex2=5
  !!integer(4), parameter:: dc=Nvertex1+Nvertex2
  !!integer(4)::            connect(1:dc)
  !!integer(4)::            offset(1:NCelle)
  !!integer(4)::            tipo(1:NCelle)
  !!...
  !!E_IO = VTK_CON_XML(NCelle,connect,offset,tipo)
  !!...
  !!\end{verbatim}
  !!\end{boxred}
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  select case(f_out)
  case(f_out_ascii)
    write(unit=Unit_VTK,fmt='(A)',iostat=E_IO)repeat(' ',indent)//'<Cells>'
    indent = indent + 2
    write(unit=Unit_VTK,fmt='(A)',iostat=E_IO)repeat(' ',indent)//'<DataArray type="Int32" Name="connectivity" format="ascii">'
    write(unit=Unit_VTK,fmt=FI4P, iostat=E_IO)(connect(n1),n1=1,size(connect))
    write(unit=Unit_VTK,fmt='(A)',iostat=E_IO)repeat(' ',indent)//'</DataArray>'
    write(unit=Unit_VTK,fmt='(A)',iostat=E_IO)repeat(' ',indent)//'<DataArray type="Int32" Name="offsets" format="ascii">'
    write(unit=Unit_VTK,fmt=FI4P, iostat=E_IO)(offset(n1),n1=1,NC)
    write(unit=Unit_VTK,fmt='(A)',iostat=E_IO)repeat(' ',indent)//'</DataArray>'
    write(unit=Unit_VTK,fmt='(A)',iostat=E_IO)repeat(' ',indent)//'<DataArray type="Int8" Name="types" format="ascii">'
    write(unit=Unit_VTK,fmt=FI1P, iostat=E_IO)(cell_type(n1),n1=1,NC)
    write(unit=Unit_VTK,fmt='(A)',iostat=E_IO)repeat(' ',indent)//'</DataArray>'
    indent = indent - 2
    write(unit=Unit_VTK,fmt='(A)',iostat=E_IO)repeat(' ',indent)//'</Cells>'
  case(f_out_binary)
    write(unit=Unit_VTK,iostat=E_IO)repeat(' ',indent)//'<Cells>'//end_rec
    indent = indent + 2
    write(unit=Unit_VTK,iostat=E_IO)repeat(' ',indent)//                                                      &
                                    '<DataArray type="Int32" Name="connectivity" format="appended" offset="', &
                                    trim(str(.true.,ioffset)),                                                &
                                    '">'//                                                                    &
                                    end_rec
    N_Byte  = size(connect)*int(sizeof(Tipo_I4),I4P)
    ioffset = ioffset + int(sizeof(Tipo_I4),I4P) + N_Byte
    write(unit=Unit_VTK_Append,iostat=E_IO)N_Byte,'I4',size(connect)
    write(unit=Unit_VTK_Append,iostat=E_IO)(connect(n1),n1=1,size(connect))
    write(unit=Unit_VTK,iostat=E_IO)repeat(' ',indent)//'</DataArray>'//end_rec
    write(unit=Unit_VTK,iostat=E_IO)repeat(' ',indent)//                                                 &
                                    '<DataArray type="Int32" Name="offsets" format="appended" offset="', &
                                    trim(str(.true.,ioffset)),                                           &
                                    '">'//                                                               &
                                    end_rec
    N_Byte  = NC*int(sizeof(Tipo_I4),I4P)
    ioffset = ioffset + int(sizeof(Tipo_I4),I4P) + N_Byte
    write(unit=Unit_VTK_Append,iostat=E_IO)N_Byte,'I4',NC
    write(unit=Unit_VTK_Append,iostat=E_IO)(offset(n1),n1=1,NC)
    write(unit=Unit_VTK,iostat=E_IO)repeat(' ',indent)//'</DataArray>'//end_rec
    write(unit=Unit_VTK,iostat=E_IO)repeat(' ',indent)//                                              &
                                    '<DataArray type="Int8" Name="types" format="appended" offset="', &
                                    trim(str(.true.,ioffset)),                                        &
                                    '">'//                                                            &
                                    end_rec
    N_Byte  = NC*int(sizeof(Tipo_I1),I4P)
    ioffset = ioffset + int(sizeof(Tipo_I4),I4P) + N_Byte
    write(unit=Unit_VTK_Append,iostat=E_IO)N_Byte,'I1',NC
    write(unit=Unit_VTK_Append,iostat=E_IO)(cell_type(n1),n1=1,NC)
    write(unit=Unit_VTK,iostat=E_IO)repeat(' ',indent)//'</DataArray>'//end_rec
    indent = indent - 2
    write(unit=Unit_VTK,iostat=E_IO)repeat(' ',indent)//'</Cells>'//end_rec
  endselect
  return
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction VTK_CON_XML

  function VTK_DAT_XML(var_location,var_block_action) result(E_IO)
  !---------------------------------------------------------------------------------------------------------------------------------
  !!This function \MaiuscolettoBS{must} be called before saving the data related to geometric mesh. This function initializes
  !!the saving of data variables indicating the \emph{type} of variables that will be saved.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  implicit none
  character(*), intent(IN):: var_location     ! location of saving variables: CELL for cell-centered, NODE for node-centered
  character(*), intent(IN):: var_block_action ! variables block action: OPEN or CLOSE block
  integer(I4P)::             E_IO             ! Input/Output inquiring flag: $0$ if IO is done, $> 0$ if IO is not done
  !!The VTK\_DAT\_XML variables have the following meaning:
  !!
  !!\begin{description}
  !!\item[{\color{RoyalBlue}var\_location}] contains the location-type of variables that will be saved after VTK\_DAT.
  !!It is a scalar and cab assume the following values:
  !! \begin{enumerateABlu}
  !!  \item \emph{cell} (it is case insensitive) $\rightarrow$ variables will be cell-centered.
  !!  \item \emph{node} (it is case insensitive) $\rightarrow$ variables will be node-centered.
  !! \end{enumerateABlu}
  !! \item[{\color{RoyalBlue}var\_block\_action}] indicates if the block-data-variables is being opened or closed; it can
  !!                                              assume the following values:
  !! \begin{enumerateABlu}
  !!  \item \emph{open}  (it is case insensitive) $\rightarrow$ block-data is being opened.
  !!  \item \emph{close} (it is case insensitive) $\rightarrow$ block-data is being closed.
  !! \end{enumerateABlu}
  !! \item[{\color{RoyalBlue}E\_IO}] contains the inquiring integer flag for error handling.
  !!\end{description}
  !!
  !!Of course a single file can contain both cell and node centered variables. The \MaiuscolettoBS{VTK\_DAT\_XML} must be
  !!called two times, before saving a block-data-variables in order to open the block, and after the block-data-variables
  !!has been saved in order to close the block. XML file can contains as many blocks as you want.
  !!
  !!The following is an example of VTK\_DAT\_XML calling:
  !!
  !!\begin{boxred}{VTK\_DAT\_XML Calling}
  !!\begin{verbatim}
  !!...
  !!E_IO = VTK_DAT_XML('node','OPEN')
  !!...
  !!SAVE YOUR DATA WITH VTK_VAR_XML
  !!...
  !!E_IO = VTK_DAT_XML('node','CLOSE')
  !!...
  !!\end{verbatim}
  !!\end{boxred}
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  select case(f_out)
  case(f_out_ascii)
    select case(trim(Upper_Case(var_location)))
    case('CELL')
      select case(trim(Upper_Case(var_block_action)))
      case('OPEN')
        write(unit=Unit_VTK,fmt='(A)',iostat=E_IO)repeat(' ',indent)//'<CellData>'
        indent = indent + 2
      case('CLOSE')
        indent = indent - 2
        write(unit=Unit_VTK,fmt='(A)',iostat=E_IO)repeat(' ',indent)//'</CellData>'
      endselect
    case('NODE')
      select case(trim(Upper_Case(var_block_action)))
      case('OPEN')
        write(unit=Unit_VTK,fmt='(A)',iostat=E_IO)repeat(' ',indent)//'<PointData>'
        indent = indent + 2
      case('CLOSE')
        indent = indent - 2
        write(unit=Unit_VTK,fmt='(A)',iostat=E_IO)repeat(' ',indent)//'</PointData>'
      endselect
    endselect
  case(f_out_binary)
    select case(trim(Upper_Case(var_location)))
    case('CELL')
      select case(trim(Upper_Case(var_block_action)))
      case('OPEN')
        write(unit=Unit_VTK,iostat=E_IO)repeat(' ',indent)//'<CellData>'//end_rec
        indent = indent + 2
      case('CLOSE')
        indent = indent - 2
        write(unit=Unit_VTK,iostat=E_IO)repeat(' ',indent)//'</CellData>'//end_rec
      endselect
    case('NODE')
      select case(trim(Upper_Case(var_block_action)))
      case('OPEN')
        write(unit=Unit_VTK,iostat=E_IO)repeat(' ',indent)//'<PointData>'//end_rec
        indent = indent + 2
      case('CLOSE')
        indent = indent - 2
        write(unit=Unit_VTK,iostat=E_IO)repeat(' ',indent)//'</PointData>'//end_rec
      endselect
    endselect
  endselect
  return
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction VTK_DAT_XML

  !(doc)INCLUDE{/home/szaghi/VTK_IO/LIB_VTK_IO/source/DOC_VTK_VAR_XML.doc}
  !(\doc)skippedblock
  function VTK_VAR_XML_SCAL_R8(NC_NN,varname,var) result(E_IO)
  !---------------------------------------------------------------------------------------------------------------------------------
  !! Function for saving scalar variable (R8P).
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  implicit none
  integer(I4P), intent(IN):: NC_NN        ! number of cells or nodes
  character(*), intent(IN):: varname      ! variable name
  real(R8P),    intent(IN):: var(1:NC_NN) ! variable to be saved
  integer(I4P)::             E_IO         ! Input/Output inquiring flag: $0$ if IO is done, $> 0$ if IO is not done
  integer(I4P)::             n1           ! counter
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  select case(f_out)
  case(f_out_ascii)
    write(unit=Unit_VTK,fmt='(A)',iostat=E_IO)repeat(' ',indent)//                 &
                                              '<DataArray type="Float64" Name="'// &
                                              trim(varname)//                      &
                                              '" NumberOfComponents="1" format="ascii">'
    write(unit=Unit_VTK,fmt=FR8P,iostat=E_IO)(var(n1),n1=1,NC_NN)
    write(unit=Unit_VTK,fmt='(A)',iostat=E_IO)repeat(' ',indent)//'</DataArray>'
  case(f_out_binary)
    write(unit=Unit_VTK,iostat=E_IO)repeat(' ',indent)//                                   &
                                    '<DataArray type="Float64" Name="'//                   &
                                    trim(varname)//                                        &
                                    '" NumberOfComponents="1" format="appended" offset="', &
                                    trim(str(.true.,ioffset)),                             &
                                    '">'//                                                 &
                                    end_rec
    N_Byte  = NC_NN*int(sizeof(Tipo_R8),I4P)
    ioffset = ioffset + int(sizeof(Tipo_I4),I4P) + N_Byte
    write(unit=Unit_VTK_Append,iostat=E_IO)N_Byte,'R8',NC_NN
    write(unit=Unit_VTK_Append,iostat=E_IO)(var(n1),n1=1,NC_NN)
    write(unit=Unit_VTK,iostat=E_IO)repeat(' ',indent)//'</DataArray>'//end_rec
  endselect
  return
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction VTK_VAR_XML_SCAL_R8

  function VTK_VAR_XML_SCAL_R4(NC_NN,varname,var) result(E_IO)
  !---------------------------------------------------------------------------------------------------------------------------------
  !! Function for saving scalar variable (R4P).
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  implicit none
  integer(I4P), intent(IN):: NC_NN        ! number of cells or nodes
  character(*), intent(IN):: varname      ! variable name
  real(R4P),    intent(IN):: var(1:NC_NN) ! variable to be saved
  integer(I4P)::             E_IO         ! Input/Output inquiring flag: $0$ if IO is done, $> 0$ if IO is not done
  integer(I4P)::             n1           ! counter
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  select case(f_out)
  case(f_out_ascii)
    write(unit=Unit_VTK,fmt='(A)',iostat=E_IO)repeat(' ',indent)//                 &
                                              '<DataArray type="Float32" Name="'// &
                                              trim(varname)//                      &
                                              '" NumberOfComponents="1" format="ascii">'
    write(unit=Unit_VTK,fmt=FR4P,iostat=E_IO)var
    write(unit=Unit_VTK,fmt='(A)',iostat=E_IO)repeat(' ',indent)//'</DataArray>'
  case(f_out_binary)
    write(unit=Unit_VTK,iostat=E_IO)repeat(' ',indent)//                                   &
                                    '<DataArray type="Float32" Name="'//                   &
                                    trim(varname)//                                        &
                                    '" NumberOfComponents="1" format="appended" offset="', &
                                    trim(str(.true.,ioffset)),                             &
                                    '">'//                                                 &
                                    end_rec
    N_Byte  = NC_NN*int(sizeof(Tipo_R4),I4P)
    ioffset = ioffset + int(sizeof(Tipo_I4),I4P) + N_Byte
    write(unit=Unit_VTK_Append,iostat=E_IO)N_Byte,'R4',NC_NN
    write(unit=Unit_VTK_Append,iostat=E_IO)(var(n1),n1=1,NC_NN)
    write(unit=Unit_VTK,iostat=E_IO)repeat(' ',indent)//'</DataArray>'//end_rec
  endselect
  return
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction VTK_VAR_XML_SCAL_R4

  function VTK_VAR_XML_SCAL_I8(NC_NN,varname,var) result(E_IO)
  !---------------------------------------------------------------------------------------------------------------------------------
  !! Function for saving scalar variable (I8P).
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  implicit none
  integer(I4P), intent(IN):: NC_NN        ! number of cells or nodes
  character(*), intent(IN):: varname      ! variable name
  integer(I8P), intent(IN):: var(1:NC_NN) ! variable to be saved
  integer(I4P)::             E_IO         ! Input/Output inquiring flag: $0$ if IO is done, $> 0$ if IO is not done
  integer(I4P)::             n1           ! counter
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  select case(f_out)
  case(f_out_ascii)
    write(unit=Unit_VTK,fmt='(A)',iostat=E_IO)repeat(' ',indent)//               &
                                              '<DataArray type="Int64" Name="'// &
                                              trim(varname)//                    &
                                              '" NumberOfComponents="1" format="ascii">'
    write(unit=Unit_VTK,fmt=FI8P,iostat=E_IO)var
    write(unit=Unit_VTK,fmt='(A)',iostat=E_IO)'</DataArray>'
  case(f_out_binary)
    write(unit=Unit_VTK,iostat=E_IO)repeat(' ',indent)//                                   &
                                    '<DataArray type="Int64" Name="'//                     &
                                    trim(varname)//                                        &
                                    '" NumberOfComponents="1" format="appended" offset="', &
                                    trim(str(.true.,ioffset)),                             &
                                    '">'//                                                 &
                                    end_rec
    N_Byte  = NC_NN*int(sizeof(Tipo_I8),I4P)
    ioffset = ioffset + int(sizeof(Tipo_I4),I4P) + N_Byte
    write(unit=Unit_VTK_Append,iostat=E_IO)N_Byte,'I8',NC_NN
    write(unit=Unit_VTK_Append,iostat=E_IO)(var(n1),n1=1,NC_NN)
    write(unit=Unit_VTK,iostat=E_IO)repeat(' ',indent)//'</DataArray>'//end_rec
  endselect
  return
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction VTK_VAR_XML_SCAL_I8

  function VTK_VAR_XML_SCAL_I4(NC_NN,varname,var) result(E_IO)
  !---------------------------------------------------------------------------------------------------------------------------------
  !! Function for saving scalar variable (I4P).
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  implicit none
  integer(I4P), intent(IN):: NC_NN        ! number of cells or nodes
  character(*), intent(IN):: varname      ! variable name
  integer(I4P), intent(IN):: var(1:NC_NN) ! variable to be saved
  integer(I4P)::             E_IO         ! Input/Output inquiring flag: $0$ if IO is done, $> 0$ if IO is not done
  integer(I4P)::             n1           ! counter
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  select case(f_out)
  case(f_out_ascii)
    write(unit=Unit_VTK,fmt='(A)',iostat=E_IO)repeat(' ',indent)//               &
                                              '<DataArray type="Int32" Name="'// &
                                              trim(varname)//                    &
                                              '" NumberOfComponents="1" format="ascii">'
    write(unit=Unit_VTK,fmt=FI4P,iostat=E_IO)var
    write(unit=Unit_VTK,fmt='(A)',iostat=E_IO)repeat(' ',indent)//'</DataArray>'
  case(f_out_binary)
    write(unit=Unit_VTK,iostat=E_IO)repeat(' ',indent)//                                   &
                                    '<DataArray type="Int32" Name="'//                     &
                                    trim(varname)//                                        &
                                    '" NumberOfComponents="1" format="appended" offset="', &
                                    trim(str(.true.,ioffset)),                             &
                                    '">'//                                                 &
                                    end_rec
    N_Byte  = NC_NN*int(sizeof(Tipo_I4),I4P)
    ioffset = ioffset + int(sizeof(Tipo_I4),I4P) + N_Byte
    write(unit=Unit_VTK_Append,iostat=E_IO)N_Byte,'I4',NC_NN
    write(unit=Unit_VTK_Append,iostat=E_IO)(var(n1),n1=1,NC_NN)
    write(unit=Unit_VTK,iostat=E_IO)repeat(' ',indent)//'</DataArray>'//end_rec
  endselect
  return
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction VTK_VAR_XML_SCAL_I4

  function VTK_VAR_XML_SCAL_I2(NC_NN,varname,var) result(E_IO)
  !---------------------------------------------------------------------------------------------------------------------------------
  !! Function for saving scalar variable (I2P).
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  implicit none
  integer(I4P), intent(IN):: NC_NN        ! number of cells or nodes
  character(*), intent(IN):: varname      ! variable name
  integer(I2P), intent(IN):: var(1:NC_NN) ! variable to be saved
  integer(I4P)::             E_IO         ! Input/Output inquiring flag: $0$ if IO is done, $> 0$ if IO is not done
  integer(I4P)::             n1           ! counter
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  select case(f_out)
  case(f_out_ascii)
    write(unit=Unit_VTK,fmt='(A)',iostat=E_IO)repeat(' ',indent)//               &
                                              '<DataArray type="Int16" Name="'// &
                                              trim(varname)//                    &
                                              '" NumberOfComponents="1" format="ascii">'
    write(unit=Unit_VTK,fmt=FI2P, iostat=E_IO)var
    write(unit=Unit_VTK,fmt='(A)',iostat=E_IO)repeat(' ',indent)//'</DataArray>'
  case(f_out_binary)
    write(unit=Unit_VTK,iostat=E_IO)repeat(' ',indent)//                                   &
                                    '<DataArray type="Int16" Name="'//                     &
                                    trim(varname)//                                        &
                                    '" NumberOfComponents="1" format="appended" offset="', &
                                    trim(str(.true.,ioffset)),                             &
                                    '">'//                                                 &
                                    end_rec
    N_Byte  = NC_NN*int(sizeof(Tipo_I2),I4P)
    ioffset = ioffset + int(sizeof(Tipo_I4),I4P) + N_Byte
    write(unit=Unit_VTK_Append,iostat=E_IO)N_Byte,'I2',NC_NN
    write(unit=Unit_VTK_Append,iostat=E_IO)(var(n1),n1=1,NC_NN)
    write(unit=Unit_VTK,iostat=E_IO)repeat(' ',indent)//'</DataArray>'//end_rec
  endselect
  return
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction VTK_VAR_XML_SCAL_I2

  function VTK_VAR_XML_SCAL_I1(NC_NN,varname,var) result(E_IO)
  !---------------------------------------------------------------------------------------------------------------------------------
  !! Function for saving scalar variable (I1P).
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  implicit none
  integer(I4P), intent(IN):: NC_NN        ! number of cells or nodes
  character(*), intent(IN):: varname      ! variable name
  integer(I1P), intent(IN):: var(1:NC_NN) ! variable to be saved
  integer(I4P)::             E_IO         ! Input/Output inquiring flag: $0$ if IO is done, $> 0$ if IO is not done
  integer(I4P)::             n1           ! counter
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  select case(f_out)
  case(f_out_ascii)
    write(unit=Unit_VTK,fmt='(A)',iostat=E_IO)repeat(' ',indent)//              &
                                              '<DataArray type="Int8" Name="'// &
                                              trim(varname)//                   &
                                              '" NumberOfComponents="1" format="ascii">'
    write(unit=Unit_VTK,fmt=FI1P, iostat=E_IO)var
    write(unit=Unit_VTK,fmt='(A)',iostat=E_IO)repeat(' ',indent)//'</DataArray>'
  case(f_out_binary)
    write(unit=Unit_VTK,iostat=E_IO)repeat(' ',indent)//                                   &
                                    '<DataArray type="Int8" Name="'//                      &
                                    trim(varname)//                                        &
                                    '" NumberOfComponents="1" format="appended" offset="', &
                                    trim(str(.true.,ioffset)),                             &
                                    '">'//                                                 &
                                    end_rec
    N_Byte  = NC_NN*int(sizeof(Tipo_I1),I4P)
    ioffset = ioffset + int(sizeof(Tipo_I4),I4P) + N_Byte
    write(unit=Unit_VTK_Append,iostat=E_IO)N_Byte,'I1',NC_NN
    write(unit=Unit_VTK_Append,iostat=E_IO)(var(n1),n1=1,NC_NN)
    write(unit=Unit_VTK,iostat=E_IO)repeat(' ',indent)//'</DataArray>'//end_rec
  endselect
  return
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction VTK_VAR_XML_SCAL_I1

  function VTK_VAR_XML_VECT_R8(NC_NN,varname,varX,varY,varZ) result(E_IO)
  !---------------------------------------------------------------------------------------------------------------------------------
  !! Function for saving vectorial variable (R8P).
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  implicit none
  integer(I4P), intent(IN):: NC_NN         ! number of cells or nodes
  character(*), intent(IN):: varname       ! variable name
  real(R8P),    intent(IN):: varX(1:NC_NN) ! x component
  real(R8P),    intent(IN):: varY(1:NC_NN) ! y component
  real(R8P),    intent(IN):: varZ(1:NC_NN) ! z component
  integer(I4P)::             E_IO          ! Input/Output inquiring flag: $0$ if IO is done, $> 0$ if IO is not done
  integer(I4P)::             n1            ! counter
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  select case(f_out)
  case(f_out_ascii)
    write(unit=Unit_VTK,fmt='(A)',iostat=E_IO)repeat(' ',indent)//                 &
                                              '<DataArray type="Float64" Name="'// &
                                              trim(varname)//                      &
                                              '" NumberOfComponents="3" format="ascii">'
    write(unit=Unit_VTK,fmt='(3'//FR8P//')',iostat=E_IO)(varX(n1),varY(n1),varZ(n1),n1=1,NC_NN)
    write(unit=Unit_VTK,fmt='(A)',iostat=E_IO)repeat(' ',indent)//'</DataArray>'
  case(f_out_binary)
    write(unit=Unit_VTK,iostat=E_IO)repeat(' ',indent)//                                   &
                                    '<DataArray type="Float64" Name="'//                   &
                                    trim(varname)//                                        &
                                    '" NumberOfComponents="3" format="appended" offset="', &
                                    trim(str(.true.,ioffset)),                             &
                                    '">'//                                                 &
                                    end_rec
    N_Byte  = 3*NC_NN*int(sizeof(Tipo_R8),I4P)
    ioffset = ioffset + int(sizeof(Tipo_I4),I4P) + N_Byte
    write(unit=Unit_VTK_Append,iostat=E_IO)N_Byte,'R8',3*NC_NN
    write(unit=Unit_VTK_Append,iostat=E_IO)(varX(n1),varY(n1),varZ(n1),n1=1,NC_NN)
    write(unit=Unit_VTK,iostat=E_IO)repeat(' ',indent)//'</DataArray>'//end_rec
  endselect
  return
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction VTK_VAR_XML_VECT_R8

  function VTK_VAR_XML_VECT_R4(NC_NN,varname,varX,varY,varZ) result(E_IO)
  !---------------------------------------------------------------------------------------------------------------------------------
  !! Function for saving vectorial variable (R4P).
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  implicit none
  integer(I4P), intent(IN):: NC_NN         ! number of cells or nodes
  character(*), intent(IN):: varname       ! variable name
  real(R4P),    intent(IN):: varX(1:NC_NN) ! x component
  real(R4P),    intent(IN):: varY(1:NC_NN) ! y component
  real(R4P),    intent(IN):: varZ(1:NC_NN) ! z component
  integer(I4P)::             E_IO          ! Input/Output inquiring flag: $0$ if IO is done, $> 0$ if IO is not done
  integer(I4P)::             n1            ! counter
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  select case(f_out)
  case(f_out_ascii)
    write(unit=Unit_VTK,fmt='(A)',iostat=E_IO)repeat(' ',indent)//                 &
                                              '<DataArray type="Float32" Name="'// &
                                              trim(varname)//                      &
                                              '" NumberOfComponents="3" format="ascii">'
    write(unit=Unit_VTK,fmt='(3'//FR4P//')',iostat=E_IO)(varX(n1),varY(n1),varZ(n1),n1=1,NC_NN)
    write(unit=Unit_VTK,fmt='(A)',iostat=E_IO)repeat(' ',indent)//'</DataArray>'
  case(f_out_binary)
    write(unit=Unit_VTK,iostat=E_IO)repeat(' ',indent)//                                   &
                                    '<DataArray type="Float32" Name="'//                   &
                                    trim(varname)//                                        &
                                    '" NumberOfComponents="3" format="appended" offset="', &
                                    trim(str(.true.,ioffset)),                             &
                                    '">'//                                                 &
                                    end_rec
    N_Byte  = 3*NC_NN*int(sizeof(Tipo_R4),I4P)
    ioffset = ioffset + int(sizeof(Tipo_I4),I4P) + N_Byte
    write(unit=Unit_VTK_Append,iostat=E_IO)N_Byte,'R4',3*NC_NN
    write(unit=Unit_VTK_Append,iostat=E_IO)(varX(n1),varY(n1),varZ(n1),n1=1,NC_NN)
    write(unit=Unit_VTK,iostat=E_IO)repeat(' ',indent)//'</DataArray>'//end_rec
  endselect
  return
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction VTK_VAR_XML_VECT_R4

  function VTK_VAR_XML_VECT_I8(NC_NN,varname,varX,varY,varZ) result(E_IO)
  !---------------------------------------------------------------------------------------------------------------------------------
  !! Function for saving vectorial variable (I8P).
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  implicit none
  integer(I4P), intent(IN):: NC_NN         ! number of cells or nodes
  character(*), intent(IN):: varname       ! variable name
  integer(I8P), intent(IN):: varX(1:NC_NN) ! x component
  integer(I8P), intent(IN):: varY(1:NC_NN) ! y component
  integer(I8P), intent(IN):: varZ(1:NC_NN) ! z component
  integer(I4P)::             E_IO          ! Input/Output inquiring flag: $0$ if IO is done, $> 0$ if IO is not done
  integer(I4P)::             n1            ! counter
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  select case(f_out)
  case(f_out_ascii)
    write(unit=Unit_VTK,fmt='(A)',iostat=E_IO)repeat(' ',indent)//               &
                                              '<DataArray type="Int64" Name="'// &
                                              trim(varname)//                    &
                                              '" NumberOfComponents="3" format="ascii">'
    write(unit=Unit_VTK,fmt='(3'//FI8P//')',iostat=E_IO)(varX(n1),varY(n1),varZ(n1),n1=1,NC_NN)
    write(unit=Unit_VTK,fmt='(A)',iostat=E_IO)repeat(' ',indent)//'</DataArray>'
  case(f_out_binary)
    write(unit=Unit_VTK,iostat=E_IO)repeat(' ',indent)//                                   &
                                    '<DataArray type="Int64" Name="'//                     &
                                    trim(varname)//                                        &
                                    '" NumberOfComponents="3" format="appended" offset="', &
                                    trim(str(.true.,ioffset)),                             &
                                    '">'//                                                 &
                                    end_rec
    N_Byte  = 3*NC_NN*int(sizeof(Tipo_I8),I4P)
    ioffset = ioffset + int(sizeof(Tipo_I4),I4P) + N_Byte
    write(unit=Unit_VTK_Append,iostat=E_IO)N_Byte,'I8',3*NC_NN
    write(unit=Unit_VTK_Append,iostat=E_IO)(varX(n1),varY(n1),varZ(n1),n1=1,NC_NN)
    write(unit=Unit_VTK,iostat=E_IO)repeat(' ',indent)//'</DataArray>'//end_rec
  endselect
  return
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction VTK_VAR_XML_VECT_I8

  function VTK_VAR_XML_VECT_I4(NC_NN,varname,varX,varY,varZ) result(E_IO)
  !---------------------------------------------------------------------------------------------------------------------------------
  !! Function for saving vectorial variable (I4P).
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  implicit none
  integer(I4P), intent(IN):: NC_NN         ! number of cells or nodes
  character(*), intent(IN):: varname       ! variable name
  integer(I4P), intent(IN):: varX(1:NC_NN) ! x component
  integer(I4P), intent(IN):: varY(1:NC_NN) ! y component
  integer(I4P), intent(IN):: varZ(1:NC_NN) ! z component
  integer(I4P)::             E_IO          ! Input/Output inquiring flag: $0$ if IO is done, $> 0$ if IO is not done
  integer(I4P)::             n1            ! counter
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  select case(f_out)
  case(f_out_ascii)
    write(unit=Unit_VTK,fmt='(A)',iostat=E_IO)repeat(' ',indent)//               &
                                              '<DataArray type="Int32" Name="'// &
                                              trim(varname)//                    &
                                              '" NumberOfComponents="3" format="ascii">'
    write(unit=Unit_VTK,fmt='(3'//FI4P//')',iostat=E_IO)(varX(n1),varY(n1),varZ(n1),n1=1,NC_NN)
    write(unit=Unit_VTK,fmt='(A)',iostat=E_IO)repeat(' ',indent)//'</DataArray>'
  case(f_out_binary)
    write(unit=Unit_VTK,iostat=E_IO)repeat(' ',indent)//                                   &
                                    '<DataArray type="Int32" Name="'//                     &
                                    trim(varname)//                                        &
                                    '" NumberOfComponents="3" format="appended" offset="', &
                                    trim(str(.true.,ioffset)),                             &
                                    '">'//                                                 &
                                    end_rec
    N_Byte  = 3*NC_NN*int(sizeof(Tipo_I4),I4P)
    ioffset = ioffset + int(sizeof(Tipo_I4),I4P) + N_Byte
    write(unit=Unit_VTK_Append,iostat=E_IO)N_Byte,'I4',3*NC_NN
    write(unit=Unit_VTK_Append,iostat=E_IO)(varX(n1),varY(n1),varZ(n1),n1=1,NC_NN)
    write(unit=Unit_VTK,iostat=E_IO)repeat(' ',indent)//'</DataArray>'//end_rec
  endselect
  return
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction VTK_VAR_XML_VECT_I4

  function VTK_VAR_XML_VECT_I2(NC_NN,varname,varX,varY,varZ) result(E_IO)
  !---------------------------------------------------------------------------------------------------------------------------------
  !! Function for saving vectorial variable (I2P).
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  implicit none
  integer(I4P), intent(IN):: NC_NN         ! number of cells or nodes
  character(*), intent(IN):: varname       ! variable name
  integer(I2P), intent(IN):: varX(1:NC_NN) ! x component
  integer(I2P), intent(IN):: varY(1:NC_NN) ! y component
  integer(I2P), intent(IN):: varZ(1:NC_NN) ! z component
  integer(I4P)::             E_IO          ! Input/Output inquiring flag: $0$ if IO is done, $> 0$ if IO is not done
  integer(I4P)::             n1            ! counter
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  select case(f_out)
  case(f_out_ascii)
    write(unit=Unit_VTK,fmt='(A)',iostat=E_IO)repeat(' ',indent)//               &
                                              '<DataArray type="Int16" Name="'// &
                                              trim(varname)//                    &
                                              '" NumberOfComponents="3" format="ascii">'
    write(unit=Unit_VTK,fmt='(3'//FI2P//')',iostat=E_IO)(varX(n1),varY(n1),varZ(n1),n1=1,NC_NN)
    write(unit=Unit_VTK,fmt='(A)',iostat=E_IO)repeat(' ',indent)//'</DataArray>'
  case(f_out_binary)
    write(unit=Unit_VTK,iostat=E_IO)repeat(' ',indent)//                                   &
                                    '<DataArray type="Int16" Name="'//                     &
                                    trim(varname)//                                        &
                                    '" NumberOfComponents="3" format="appended" offset="', &
                                    trim(str(.true.,ioffset)),                             &
                                    '">'//                                                 &
                                    end_rec
    N_Byte  = 3*NC_NN*int(sizeof(Tipo_I2),I4P)
    ioffset = ioffset + int(sizeof(Tipo_I4),I4P) + N_Byte
    write(unit=Unit_VTK_Append,iostat=E_IO)N_Byte,'I2',3*NC_NN
    write(unit=Unit_VTK_Append,iostat=E_IO)(varX(n1),varY(n1),varZ(n1),n1=1,NC_NN)
    write(unit=Unit_VTK,iostat=E_IO)repeat(' ',indent)//'</DataArray>'//end_rec
  endselect
  return
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction VTK_VAR_XML_VECT_I2

  function VTK_VAR_XML_VECT_I1(NC_NN,varname,varX,varY,varZ) result(E_IO)
  !---------------------------------------------------------------------------------------------------------------------------------
  !! Function for saving vectorial variable (I1P).
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  implicit none
  integer(I4P), intent(IN):: NC_NN         ! number of cells or nodes
  character(*), intent(IN):: varname       ! variable name
  integer(I1P), intent(IN):: varX(1:NC_NN) ! x component
  integer(I1P), intent(IN):: varY(1:NC_NN) ! y component
  integer(I1P), intent(IN):: varZ(1:NC_NN) ! z component
  integer(I4P)::             E_IO          ! Input/Output inquiring flag: $0$ if IO is done, $> 0$ if IO is not done
  integer(I4P)::             n1            ! counter
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  select case(f_out)
  case(f_out_ascii)
    write(unit=Unit_VTK,fmt='(A)',iostat=E_IO)repeat(' ',indent)//              &
                                              '<DataArray type="Int8" Name="'// &
                                              trim(varname)//                   &
                                              '" NumberOfComponents="3" format="ascii">'
    write(unit=Unit_VTK,fmt='(3'//FI1P//')',iostat=E_IO)(varX(n1),varY(n1),varZ(n1),n1=1,NC_NN)
    write(unit=Unit_VTK,fmt='(A)',iostat=E_IO)repeat(' ',indent)//'</DataArray>'
  case(f_out_binary)
    write(unit=Unit_VTK,iostat=E_IO)repeat(' ',indent)//                                   &
                                    '<DataArray type="Int8" Name="'//                      &
                                    trim(varname)//                                        &
                                    '" NumberOfComponents="3" format="appended" offset="', &
                                    trim(str(.true.,ioffset)),                             &
                                    '">'//                                                 &
                                    end_rec
    N_Byte  = 3*NC_NN*int(sizeof(Tipo_I1),I4P)
    ioffset = ioffset + int(sizeof(Tipo_I4),I4P) + N_Byte
    write(unit=Unit_VTK_Append,iostat=E_IO)N_Byte,'I1',3*NC_NN
    write(unit=Unit_VTK_Append,iostat=E_IO)(varX(n1),varY(n1),varZ(n1),n1=1,NC_NN)
    write(unit=Unit_VTK,iostat=E_IO)repeat(' ',indent)//'</DataArray>'//end_rec
  endselect
  return
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction VTK_VAR_XML_VECT_I1

  function VTK_VAR_XML_LIST_R8(NC_NN,N_COL,varname,var) result(E_IO)
  !---------------------------------------------------------------------------------------------------------------------------------
  !! Function for saving list variable (R8P).
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  implicit none
  integer(I4P), intent(IN):: NC_NN                ! number of cells or nodes
  integer(I4P), intent(IN):: N_COL                ! number of columns
  character(*), intent(IN):: varname              ! variable name
  real(R8P),    intent(IN):: var(1:NC_NN,1:N_COL) ! components
  integer(I4P)::             E_IO                 ! Input/Output inquiring flag: $0$ if IO is done, $> 0$ if IO is not done
  integer(I4P)::             n1,n2                ! counter
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  select case(f_out)
  case(f_out_ascii)
    write(unit=Unit_VTK,fmt='(A)',iostat=E_IO) repeat(' ',indent)//                 &
                                               '<DataArray type="Float64" Name="'// &
                                               trim(varname)//                      &
                                               '" NumberOfComponents="'//           &
                                               trim(str(.true.,N_COL))//            &
                                               '" format="ascii">'
    do n1=1,NC_NN
      write(unit=Unit_VTK,fmt='('//FR8P//')',iostat=E_IO) (var(n1,n2),n2=1,N_COL)
    enddo
    write(unit=Unit_VTK,fmt='(A)',iostat=E_IO) repeat(' ',indent)//'</DataArray>'
  case(f_out_binary)
    write(unit=Unit_VTK,iostat=E_IO) repeat(' ',indent)//                 &
                                     '<DataArray type="Float64" Name="'// &
                                     trim(varname)//                      &
                                     '" NumberOfComponents="'//           &
                                     trim(str(.true.,N_COL))//            &
                                     '" format="appended" offset="',      &
                                     trim(str(.true.,ioffset)),'">'//     &
                                     end_rec
    N_Byte  = N_COL*NC_NN*int(sizeof(Tipo_R8),I4P)
    ioffset = ioffset + int(sizeof(Tipo_I4),I4P) + N_Byte
    write(unit=Unit_VTK_Append,iostat=E_IO)N_Byte,'R8',N_COL*NC_NN
    do n1=1,NC_NN
      write(unit=Unit_VTK_Append,iostat=E_IO) var(n1,:)
    enddo
    write(unit=Unit_VTK,iostat=E_IO) repeat(' ',indent)//'</DataArray>'//end_rec
  endselect
  return
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction VTK_VAR_XML_LIST_R8

  function VTK_VAR_XML_LIST_R4(NC_NN,N_COL,varname,var) result(E_IO)
  !---------------------------------------------------------------------------------------------------------------------------------
  !! Function for saving list variable (R4P).
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  implicit none
  integer(I4P), intent(IN):: NC_NN                ! number of cells or nodes
  integer(I4P), intent(IN):: N_COL                ! number of columns
  character(*), intent(IN):: varname              ! variable name
  real(R4P),    intent(IN):: var(1:NC_NN,1:N_COL) ! components
  integer(I4P)::             E_IO                 ! Input/Output inquiring flag: $0$ if IO is done, $> 0$ if IO is not done
  integer(I4P)::             n1,n2                ! counter
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  select case(f_out)
  case(f_out_ascii)
    write(unit=Unit_VTK,fmt='(A)',iostat=E_IO) repeat(' ',indent)//                 &
                                               '<DataArray type="Float32" Name="'// &
                                               trim(varname)//                      &
                                               '" NumberOfComponents="'//           &
                                               trim(str(.true.,N_COL))//            &
                                               '" format="ascii">'
    do n1=1,NC_NN
      write(unit=Unit_VTK,fmt='('//FR4P//')',iostat=E_IO) (var(n1,n2),n2=1,N_COL)
    enddo
    write(unit=Unit_VTK,fmt='(A)',iostat=E_IO) repeat(' ',indent)//'</DataArray>'
  case(f_out_binary)
    write(unit=Unit_VTK,iostat=E_IO) repeat(' ',indent)//                 &
                                     '<DataArray type="Float32" Name="'// &
                                     trim(varname)//                      &
                                     '" NumberOfComponents="'//           &
                                     trim(str(.true.,N_COL))//            &
                                     '" format="appended" offset="',      &
                                     trim(str(.true.,ioffset)),'">'//     &
                                     end_rec
    N_Byte  = N_COL*NC_NN*int(sizeof(Tipo_R4),I4P)
    ioffset = ioffset + int(sizeof(Tipo_I4),I4P) + N_Byte
    write(unit=Unit_VTK_Append,iostat=E_IO)N_Byte,'R4',N_COL*NC_NN
    do n1=1,NC_NN
      write(unit=Unit_VTK_Append,iostat=E_IO) var(n1,:)
    enddo
    write(unit=Unit_VTK,iostat=E_IO) repeat(' ',indent)//'</DataArray>'//end_rec
  endselect
  return
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction VTK_VAR_XML_LIST_R4

  function VTK_VAR_XML_LIST_I8(NC_NN,N_COL,varname,var) result(E_IO)
  !---------------------------------------------------------------------------------------------------------------------------------
  !! Function for saving list variable (I8P).
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  implicit none
  integer(I4P), intent(IN):: NC_NN                ! number of cells or nodes
  integer(I4P), intent(IN):: N_COL                ! number of columns
  character(*), intent(IN):: varname              ! variable name
  integer(I8P), intent(IN):: var(1:NC_NN,1:N_COL) ! components
  integer(I4P)::             E_IO                 ! Input/Output inquiring flag: $0$ if IO is done, $> 0$ if IO is not done
  integer(I4P)::             n1,n2                ! counter
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  select case(f_out)
  case(f_out_ascii)
    write(unit=Unit_VTK,fmt='(A)',iostat=E_IO) repeat(' ',indent)//               &
                                               '<DataArray type="Int64" Name="'// &
                                               trim(varname)//                    &
                                               '" NumberOfComponents="'//         &
                                               trim(str(.true.,N_COL))//          &
                                               '" format="ascii">'
    do n1=1,NC_NN
      write(unit=Unit_VTK,fmt='('//FI8P//')',iostat=E_IO) (var(n1,n2),n2=1,N_COL)
    enddo
    write(unit=Unit_VTK,fmt='(A)',iostat=E_IO) repeat(' ',indent)//'</DataArray>'
  case(f_out_binary)
    write(unit=Unit_VTK,iostat=E_IO) repeat(' ',indent)//               &
                                     '<DataArray type="Int64" Name="'// &
                                     trim(varname)//                    &
                                     '" NumberOfComponents="'//         &
                                     trim(str(.true.,N_COL))//          &
                                     '" format="appended" offset="',    &
                                     trim(str(.true.,ioffset)),         &
                                     '">'//end_rec
    N_Byte  = N_COL*NC_NN*int(sizeof(Tipo_I8),I4P)
    ioffset = ioffset + int(sizeof(Tipo_I4),I4P) + N_Byte
    write(unit=Unit_VTK_Append,iostat=E_IO)N_Byte,'I8',N_COL*NC_NN
    do n1=1,NC_NN
      write(unit=Unit_VTK_Append,iostat=E_IO) var(n1,:)
    enddo
    write(unit=Unit_VTK,iostat=E_IO) repeat(' ',indent)//'</DataArray>'//end_rec
  endselect
  return
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction VTK_VAR_XML_LIST_I8

  function VTK_VAR_XML_LIST_I4(NC_NN,N_COL,varname,var) result(E_IO)
  !---------------------------------------------------------------------------------------------------------------------------------
  !! Function for saving list variable (I4P).
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  implicit none
  integer(I4P), intent(IN):: NC_NN                ! number of cells or nodes
  integer(I4P), intent(IN):: N_COL                ! number of columns
  character(*), intent(IN):: varname              ! variable name
  integer(I4P), intent(IN):: var(1:NC_NN,1:N_COL) ! components
  integer(I4P)::             E_IO                 ! Input/Output inquiring flag: $0$ if IO is done, $> 0$ if IO is not done
  integer(I4P)::             n1,n2                ! counter
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  select case(f_out)
  case(f_out_ascii)
    write(unit=Unit_VTK,fmt='(A)',iostat=E_IO) repeat(' ',indent)//               &
                                               '<DataArray type="Int32" Name="'// &
                                               trim(varname)//                    &
                                               '" NumberOfComponents="'//         &
                                               trim(str(.true.,N_COL))//          &
                                               '" format="ascii">'
    do n1=1,NC_NN
      write(unit=Unit_VTK,fmt='('//FI4P//')',iostat=E_IO) (var(n1,n2),n2=1,N_COL)
    enddo
    write(unit=Unit_VTK,fmt='(A)',iostat=E_IO) repeat(' ',indent)//'</DataArray>'
  case(f_out_binary)
    write(unit=Unit_VTK,iostat=E_IO) repeat(' ',indent)//               &
                                     '<DataArray type="Int32" Name="'// &
                                     trim(varname)//                    &
                                     '" NumberOfComponents="'//         &
                                     trim(str(.true.,N_COL))//          &
                                     '" format="appended" offset="',    &
                                     trim(str(.true.,ioffset)),         &
                                     '">'//end_rec
    N_Byte  = N_COL*NC_NN*int(sizeof(Tipo_I4),I4P)
    ioffset = ioffset + int(sizeof(Tipo_I4),I4P) + N_Byte
    write(unit=Unit_VTK_Append,iostat=E_IO)N_Byte,'I4',N_COL*NC_NN
    do n1=1,NC_NN
      write(unit=Unit_VTK_Append,iostat=E_IO) var(n1,:)
    enddo
    write(unit=Unit_VTK,iostat=E_IO) repeat(' ',indent)//'</DataArray>'//end_rec
  endselect
  return
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction VTK_VAR_XML_LIST_I4

  function VTK_VAR_XML_LIST_I2(NC_NN,N_COL,varname,var) result(E_IO)
  !---------------------------------------------------------------------------------------------------------------------------------
  !! Function for saving list variable (I2P).
  !---------------------------------------------------------------------------------------------------------------------------------
  implicit none
  !---------------------------------------------------------------------------------------------------------------------------------
  integer(I4P), intent(IN):: NC_NN                ! number of cells or nodes
  integer(I4P), intent(IN):: N_COL                ! number of columns
  character(*), intent(IN):: varname              ! variable name
  integer(I2P), intent(IN):: var(1:NC_NN,1:N_COL) ! components
  integer(I4P)::             E_IO                 ! Input/Output inquiring flag: $0$ if IO is done, $> 0$ if IO is not done
  integer(I4P)::             n1,n2                ! counter
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  select case(f_out)
  case(f_out_ascii)
    write(unit=Unit_VTK,fmt='(A)',iostat=E_IO) repeat(' ',indent)//               &
                                               '<DataArray type="Int16" Name="'// &
                                               trim(varname)//                    &
                                               '" NumberOfComponents="'//         &
                                               trim(str(.true.,N_COL))//          &
                                               '" format="ascii">'
    do n1=1,NC_NN
      write(unit=Unit_VTK,fmt='('//FI2P//')',iostat=E_IO) (var(n1,n2),n2=1,N_COL)
    enddo
    write(unit=Unit_VTK,fmt='(A)',iostat=E_IO) repeat(' ',indent)//'</DataArray>'
  case(f_out_binary)
    write(unit=Unit_VTK,iostat=E_IO) repeat(' ',indent)//               &
                                     '<DataArray type="Int16" Name="'// &
                                     trim(varname)//                    &
                                     '" NumberOfComponents="'//         &
                                     trim(str(.true.,N_COL))//          &
                                     '" format="appended" offset="',    &
                                     trim(str(.true.,ioffset)),         &
                                     '">'//end_rec
    N_Byte  = N_COL*NC_NN*int(sizeof(Tipo_I2),I4P)
    ioffset = ioffset + int(sizeof(Tipo_I4),I4P) + N_Byte
    write(unit=Unit_VTK_Append,iostat=E_IO)N_Byte,'I2',N_COL*NC_NN
    do n1=1,NC_NN
      write(unit=Unit_VTK_Append,iostat=E_IO) var(n1,:)
    enddo
    write(unit=Unit_VTK,iostat=E_IO) repeat(' ',indent)//'</DataArray>'//end_rec
  endselect
  return
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction

  function VTK_VAR_XML_LIST_I1(NC_NN,N_COL,varname,var) result(E_IO)
  !---------------------------------------------------------------------------------------------------------------------------------
  !! Function for saving list variable (I1P).
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  implicit none
  integer(I4P), intent(IN)::  NC_NN                ! number of cells or nodes
  integer(I4P), intent(IN)::  N_COL                ! number of columns
  character(*), intent(IN)::  varname              ! variable name
  integer(I1P),  intent(IN):: var(1:NC_NN,1:N_COL) ! components
  integer(I4P)::              E_IO                 ! Input/Output inquiring flag: $0$ if IO is done, $> 0$ if IO is not done
  integer(I4P)::              n1,n2                ! counter
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  select case(f_out)
  case(f_out_ascii)
    write(unit=Unit_VTK,fmt='(A)',iostat=E_IO) repeat(' ',indent)//              &
                                               '<DataArray type="Int8" Name="'// &
                                               trim(varname)//                   &
                                               '" NumberOfComponents="'//        &
                                               trim(str(.true.,N_COL))//         &
                                               '" format="ascii">'
    do n1=1,NC_NN
      write(unit=Unit_VTK,fmt='('//FI1P//')',iostat=E_IO) (var(n1,n2),n2=1,N_COL)
    enddo
    write(unit=Unit_VTK,fmt='(A)',iostat=E_IO) repeat(' ',indent)//'</DataArray>'
  case(f_out_binary)
    write(unit=Unit_VTK,iostat=E_IO) repeat(' ',indent)//              &
                                     '<DataArray type="Int8" Name="'// &
                                     trim(varname)//                   &
                                     '" NumberOfComponents="'//        &
                                     trim(str(.true.,N_COL))//         &
                                     '" format="appended" offset="',   &
                                     trim(str(.true.,ioffset)),        &
                                     '">'//end_rec
    N_Byte  = N_COL*NC_NN*int(sizeof(Tipo_I1),I4P)
    ioffset = ioffset + int(sizeof(Tipo_I4),I4P) + N_Byte
    write(unit=Unit_VTK_Append,iostat=E_IO)N_Byte,'I1',N_COL*NC_NN
    do n1=1,NC_NN
      write(unit=Unit_VTK_Append,iostat=E_IO) var(n1,:)
    enddo
    write(unit=Unit_VTK,iostat=E_IO) repeat(' ',indent)//'</DataArray>'//end_rec
  endselect
  return
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction VTK_VAR_XML_LIST_I1
!(doc/)skippedblock

  function VTK_END_XML() result(E_IO)
  !---------------------------------------------------------------------------------------------------------------------------------
  !!This function is used to finalize the file opened. The \LIBVTKIO manages the file unit without the user's action.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  implicit none
  integer(I4P)::              E_IO      ! Input/Output inquiring flag: $0$ if IO is done, $> 0$ if IO is not done
  character(2)::              var_type  ! var\_type = R8,R4,I8,I4,I2,I1
  real(R8P),    allocatable:: v_R8(:)   ! R8 vector for IO in AppendData
  real(R4P),    allocatable:: v_R4(:)   ! R4 vector for IO in AppendData
  integer(I8P), allocatable:: v_I8(:)   ! I8 vector for IO in AppendData
  integer(I4P), allocatable:: v_I4(:)   ! I4 vector for IO in AppendData
  integer(I2P), allocatable:: v_I2(:)   ! I2 vector for IO in AppendData
  integer(I1P), allocatable:: v_I1(:)   ! I1 vector for IO in AppendData
  integer(I4P)::              N_v       ! vector dimension
  integer(I4P)::              n1        ! counter
  !!The following is an example of VTK\_END\_XML calling:
  !!
  !!\begin{boxred}{VTK\_END\_XML Calling}
  !!\begin{verbatim}
  !!...
  !!E_IO = VTK_END_XML()
  !!...
  !!\end{verbatim}
  !!\end{boxred}
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  select case(f_out)
  case(f_out_ascii)
    indent = indent - 2
    write(unit=Unit_VTK,fmt='(A)',iostat=E_IO)repeat(' ',indent)//'</'//trim(topology)//'>'
    write(unit=Unit_VTK,fmt='(A)',iostat=E_IO)'</VTKFile>'
  case(f_out_binary)
    indent = indent - 2
    write(unit  =Unit_VTK,       iostat=E_IO)repeat(' ',indent)//'</'//trim(topology)//'>'//end_rec
    write(unit  =Unit_VTK,       iostat=E_IO)repeat(' ',indent)//'<AppendedData encoding="raw">'//end_rec
    write(unit  =Unit_VTK,       iostat=E_IO)'_'
    endfile(unit=Unit_VTK_Append,iostat=E_IO)
    rewind(unit =Unit_VTK_Append,iostat=E_IO)
    do
      read(unit=Unit_VTK_Append,iostat=E_IO,end=100)N_Byte,var_type,N_v
      select case(var_type)
      case('R8')
        allocate(v_R8(1:N_v))
        read(unit =Unit_VTK_Append,iostat=E_IO)(v_R8(n1),n1=1,N_v)
        write(unit=Unit_VTK,       iostat=E_IO)N_Byte,(v_R8(n1),n1=1,N_v)
        deallocate(v_R8)
      case('R4')
        allocate(v_R4(1:N_v))
        read(unit =Unit_VTK_Append,iostat=E_IO)(v_R4(n1),n1=1,N_v)
        write(unit=Unit_VTK,       iostat=E_IO)N_Byte,(v_R4(n1),n1=1,N_v)
        deallocate(v_R4)
      case('I8')
        allocate(v_I8(1:N_v))
        read(unit =Unit_VTK_Append,iostat=E_IO)(v_I8(n1),n1=1,N_v)
        write(unit=Unit_VTK,       iostat=E_IO)N_Byte,(v_I8(n1),n1=1,N_v)
        deallocate(v_I8)
      case('I4')
        allocate(v_I4(1:N_v))
        read(unit =Unit_VTK_Append,iostat=E_IO)(v_I4(n1),n1=1,N_v)
        write(unit=Unit_VTK,       iostat=E_IO)N_Byte,(v_I4(n1),n1=1,N_v)
        deallocate(v_I4)
      case('I2')
        allocate(v_I2(1:N_v))
        read(unit =Unit_VTK_Append,iostat=E_IO)(v_I2(n1),n1=1,N_v)
        write(unit=Unit_VTK,       iostat=E_IO)N_Byte,(v_I2(n1),n1=1,N_v)
        deallocate(v_I2)
      case('I1')
        allocate(v_I1(1:N_v))
        read(unit =Unit_VTK_Append,iostat=E_IO)(v_I1(n1),n1=1,N_v)
        write(unit=Unit_VTK,       iostat=E_IO)N_Byte,(v_I1(n1),n1=1,N_v)
        deallocate(v_I1)
      case default
        E_IO = 1
        write (6,"(' N_Byte =', I10, ' N_v =', I10)") N_Byte, N_v
        write (6,"(' var1 =', I10, ' var2 =', I10)") ichar(var_type(1:1)),ichar(var_type(2:2))
        write (6,'(A)') 'bad var_type'
        return
      endselect
    enddo
    100 continue
    write(unit=Unit_VTK,iostat=E_IO)end_rec
    write(unit=Unit_VTK,iostat=E_IO)repeat(' ',indent)//'</AppendedData>'//end_rec
    write(unit=Unit_VTK,iostat=E_IO)'</VTKFile>'//end_rec
    ! closing AppendData file
    close(unit=Unit_VTK_Append,iostat=E_IO)
  endselect
  close(unit=Unit_VTK,iostat=E_IO)
  return
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction VTK_END_XML

  ! VTM wrapper
  function VTM_INI_XML(filename) result(E_IO)
  !---------------------------------------------------------------------------------------------------------------------------------
  !!The VTK_VTM_XML function is used for initializing a VTM (VTK Multiblocks) XML file that is a wrapper to a set of VTK XML files.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  implicit none
  character(*), intent(IN):: filename ! file name
  integer(I4P)::             E_IO     ! Input/Output inquiring flag: $0$ if IO is done, $> 0$ if IO is not done
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  Unit_VTM=GetUnit()
  open(unit   = Unit_VTM,       &
       file   = trim(filename), &
       form   = 'FORMATTED',    &
       access = 'SEQUENTIAL',   &
       action = 'WRITE',        &
       iostat = E_IO)

  write(unit=Unit_VTM,fmt='(A)',iostat=E_IO)'<?xml version="1.0"?>'
  write(unit=Unit_VTM,fmt='(A)',iostat=E_IO)'<VTKFile type="vtkMultiBlockDataSet" version="1.0"'// &
                                            ' byte_order="BigEndian" compressor="vtkZLibDataCompressor">'
  vtm_indent = 2
  write(unit=Unit_VTM,fmt='(A)',iostat=E_IO)repeat(' ',vtm_indent)//'<vtkMultiBlockDataSet>'
  vtm_indent = vtm_indent + 2
  blk = -1
  return
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction VTM_INI_XML

  function VTM_BLK_XML(block_action) result(E_IO)
  !---------------------------------------------------------------------------------------------------------------------------------
  !!The VTM_BLK_XML function is used for opening or closing a block level of a VTM file.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  implicit none
  character(*), intent(IN):: block_action ! block action: OPEN or CLOSE block
  integer(I4P)::             E_IO         ! Input/Output inquiring flag: $0$ if IO is done, $> 0$ if IO is not done
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  select case(trim(Upper_Case(block_action)))
  case('OPEN')
    blk = blk + 1
    write(unit=Unit_VTM,fmt='(A,I4.4,A)',iostat=E_IO)repeat(' ',vtm_indent)//'<Block index="',blk,'">'
    vtm_indent = vtm_indent + 2
  case('CLOSE')
    vtm_indent = vtm_indent - 2
    write(unit=Unit_VTM,fmt='(A)',iostat=E_IO)repeat(' ',vtm_indent)//'</Block>'
  endselect
  return
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction VTM_BLK_XML

  function VTM_WRF_XML(wrf_dir,vtk_xml_file_list) result(E_IO)
  !---------------------------------------------------------------------------------------------------------------------------------
  !!The VTM_WRF_XML function is used for saving the list of VTK XML wrapped files by the actual block of the mutliblock VTM file.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  implicit none
  character(*), intent(IN), optional:: wrf_dir              ! directory into which wrapped VTK XML are (optional)
  character(*), intent(IN)::           vtk_xml_file_list(:) ! list of VTK XML wrapped files
  integer(I4P)::                       E_IO                 ! Input/Output inquiring flag: 0 if IO is done, > 0 if IO is not done
  integer(I4P)::                       f                    ! file counter
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  if (present(wrf_dir)) then
    do f=1,size(vtk_xml_file_list)
      write(unit=Unit_VTM,fmt='(A,I3.3,A)',iostat=E_IO)repeat(' ',vtm_indent)//                                      &
                                                       '<DataSet index="',f-1,'" file="'//                           &
                                                       adjustl(trim(wrf_dir))//adjustl(trim(vtk_xml_file_list(f)))// &
                                                       '"></DataSet>'
    enddo
  else
    do f=1,size(vtk_xml_file_list)
      write(unit=Unit_VTM,fmt='(A,I3.3,A)',iostat=E_IO)repeat(' ',vtm_indent)//              &
                                                       '<DataSet index="',f-1,'" file="'//   &
                                                       adjustl(trim(vtk_xml_file_list(f)))// &
                                                       '"></DataSet>'
    enddo
  endif
  return
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction VTM_WRF_XML

  function VTM_END_XML() result(E_IO)
  !---------------------------------------------------------------------------------------------------------------------------------
  !!This function is used to finalize the VTM file opened. The \LIBVTKIO manages the file unit without the user's action.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  implicit none
  integer(I4P):: E_IO      ! Input/Output inquiring flag: $0$ if IO is done, $> 0$ if IO is not done
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  vtm_indent = vtm_indent - 2
  write(unit=Unit_VTM,fmt='(A)',iostat=E_IO)repeat(' ',vtm_indent)//'</vtkMultiBlockDataSet>'
  write(unit=Unit_VTM,fmt='(A)',iostat=E_IO)'</VTKFile>'
  close(unit=Unit_VTM)
  return
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction VTM_END_XML
endmodule Lib_VTK_IO
!(doc)INCLUDE{/home/szaghi/VTK_IO/LIB_VTK_IO/source/DOC_Footer.doc}
