!< DAT_XML and VAR_XML interface definitions for Lib_VTK_IO.
module Lib_VTK_IO_DAT_VAR_XML
!-----------------------------------------------------------------------------------------------------------------------------------
!< DAT_XML and VAR_XML interface definitions for Lib_VTK_IO.
!-----------------------------------------------------------------------------------------------------------------------------------
use befor64             ! Base64 encoding/decoding library.
use Lib_VTK_IO_Back_End ! Lib_VTK_IO back end module.
use penf                ! Portability environment.
!-----------------------------------------------------------------------------------------------------------------------------------

!-----------------------------------------------------------------------------------------------------------------------------------
implicit none
private
public :: VTK_DAT_XML
public :: VTK_VAR_XML_WRITE
public :: VTK_VAR_XML_READ
!-----------------------------------------------------------------------------------------------------------------------------------

!-----------------------------------------------------------------------------------------------------------------------------------
interface VTK_VAR_XML_WRITE
  !< Save data variable(s) in VTK-XML standard.
  !<
  !< VTK_VAR_XML is an interface to 36 different functions, there are 6 functions for scalar variables, 6 functions for vectorial
  !< variables and 6 functions for 3D(or higher) vectorial variables: for all of types the precision can be R8P, R4P, I8P, I4P, I2P
  !< and I1P. This function saves the data variables related (cell-centered or node-centered) to geometric mesh.
  !< 1D/3D-rank arrays and packed API for any kinds
  !< The inputs arrays can be passed as 1D-rank or 3D-rank and the vectorial variables can be component-separated (one for each of
  !< the 3 components) or packed into one multidimensional array:
  !<
  !<- scalar input:
  !<    - input is 1D-rank array: var[1:NC_NN];
  !<    - input is 3D-rank array: var[nx1:nx2,ny1:ny2,nz1:nz2];
  !<- vectorial inputs:
  !<    - inputs are 1D-rank arrays: varX[1:NC_NN],varY[1:NC_NN],varZ[1:NC_NN];
  !<    - inputs are 3D-rank arrays: varX[nx1:nx2,ny1:ny2,nz1:nz2],varY[nx1:nx2,ny1:ny2,nz1:nz2],varX[nx1:nx2,ny1:ny2,nz1:nz2];
  !<- 3D(or higher) vectorial inputs:
  !<    - input is 1D-rank (packed API): var[1:N_COL,1:NC_NN];
  !<    - input is 3D-rank (packed API): var[1:N_COL,nx1:nx2,ny1:ny2,nz1:nz2].
  !<
  !< @note Note that the inputs that must be passed change depending on the data variables type.
  !<
  !<### Examples of usage
  !<
  !<#### Scalar data calling
  !<```fortran
  !< integer(I4P):: NN
  !< real(R8P)::    var(1:NN)
  !< ...
  !< E_IO=VTK_VAR_XML(NN,'Sca',var)
  !< ...
  !<```
  !<
  !<#### Vectorial data calling
  !<```fortran
  !< integer(I4P):: NN
  !< real(R8P)::    varX(1:NN),varY(1:NN),varZ(1:NN),
  !< ...
  !< E_IO=VTK_VAR_XML(NN,'Vec',varX,varY,varZ)
  !< ...
  !<```
  module procedure VTK_VAR_XML_SCAL_1DA_R8_WRITE,VTK_VAR_XML_SCAL_3DA_R8_WRITE, & ! real(R8P)    scalar    1D/3D array
                   VTK_VAR_XML_SCAL_1DA_R4_WRITE,VTK_VAR_XML_SCAL_3DA_R4_WRITE, & ! real(R4P)    scalar    1D/3D array
                   VTK_VAR_XML_SCAL_1DA_I8_WRITE,VTK_VAR_XML_SCAL_3DA_I8_WRITE, & ! integer(I8P) scalar    1D/3D array
                   VTK_VAR_XML_SCAL_1DA_I4_WRITE,VTK_VAR_XML_SCAL_3DA_I4_WRITE, & ! integer(I4P) scalar    1D/3D array
                   VTK_VAR_XML_SCAL_1DA_I2_WRITE,VTK_VAR_XML_SCAL_3DA_I2_WRITE, & ! integer(I2P) scalar    1D/3D array
                   VTK_VAR_XML_SCAL_1DA_I1_WRITE,VTK_VAR_XML_SCAL_3DA_I1_WRITE, & ! integer(I1P) scalar    1D/3D array
                   VTK_VAR_XML_VECT_1DA_R8_WRITE,VTK_VAR_XML_VECT_3DA_R8_WRITE, & ! real(R8P)    vectorial 1D/3D arrays
                   VTK_VAR_XML_VECT_1DA_R4_WRITE,VTK_VAR_XML_VECT_3DA_R4_WRITE, & ! real(R4P)    vectorial 1D/3D arrays
                   VTK_VAR_XML_VECT_1DA_I8_WRITE,VTK_VAR_XML_VECT_3DA_I8_WRITE, & ! integer(I8P) vectorial 1D/3D arrays
                   VTK_VAR_XML_VECT_1DA_I4_WRITE,VTK_VAR_XML_VECT_3DA_I4_WRITE, & ! integer(I4P) vectorial 1D/3D arrays
                   VTK_VAR_XML_VECT_1DA_I2_WRITE,VTK_VAR_XML_VECT_3DA_I2_WRITE, & ! integer(I2P) vectorial 1D/3D arrays
                   VTK_VAR_XML_VECT_1DA_I1_WRITE,VTK_VAR_XML_VECT_3DA_I1_WRITE, & ! integer(I1P) vectorial 1D/3D arrays
                   VTK_VAR_XML_LIST_1DA_R8_WRITE,VTK_VAR_XML_LIST_3DA_R8_WRITE, & ! real(R8P)    list      1D/3D array
                   VTK_VAR_XML_LIST_1DA_R4_WRITE,VTK_VAR_XML_LIST_3DA_R4_WRITE, & ! real(R4P)    list      1D/3D array
                   VTK_VAR_XML_LIST_1DA_I8_WRITE,VTK_VAR_XML_LIST_3DA_I8_WRITE, & ! integer(I4P) list      1D/3D array
                   VTK_VAR_XML_LIST_1DA_I4_WRITE,VTK_VAR_XML_LIST_3DA_I4_WRITE, & ! integer(I4P) list      1D/3D array
                   VTK_VAR_XML_LIST_1DA_I2_WRITE,VTK_VAR_XML_LIST_3DA_I2_WRITE, & ! integer(I2P) list      1D/3D array
                   VTK_VAR_XML_LIST_1DA_I1_WRITE,VTK_VAR_XML_LIST_3DA_I1_WRITE    ! integer(I1P) list      1D/3D array
endinterface
interface VTK_VAR_XML_READ
  !< Load data variable(s) in VTK-XML standard.
  !<
  !< VTK_VAR_XML is an interface to 36 different functions, there are 6 functions for scalar variables, 6 functions for vectorial
  !< variables and 6 functions for 3D(or higher) vectorial variables: for all of types the precision can be R8P, R4P, I8P, I4P, I2P
  !< and I1P. This function saves the data variables related (cell-centered or node-centered) to geometric mesh.
  !< 1D/3D-rank arrays and packed API for any kinds
  !< The output arrays can be passed as 1D-rank or 3D-rank and the vectorial variables can be component-separated (one for each of
  !< the 3 components) or packed into one multidimensional array:
  !<
  !<- scalar output:
  !<    - output is 1D-rank array: var[1:NC_NN];
  !<    - output is 3D-rank array: var[nx1:nx2,ny1:ny2,nz1:nz2];
  !<- vectorial output:
  !<    - output are 1D-rank arrays: varX[1:NC_NN],varY[1:NC_NN],varZ[1:NC_NN];
  !<    - output are 3D-rank arrays: varX[nx1:nx2,ny1:ny2,nz1:nz2],varY[nx1:nx2,ny1:ny2,nz1:nz2],varX[nx1:nx2,ny1:ny2,nz1:nz2];
  !<- 3D(or higher) vectorial inputs:
  !<    - output is 1D-rank (packed API): var[1:N_COL,1:NC_NN];
  !<    - output is 3D-rank (packed API): var[1:N_COL,nx1:nx2,ny1:ny2,nz1:nz2].
  !<
  !< @note Note that the output that must be passed change depending on the data variables type.
  module procedure VTK_VAR_XML_SCAL_1DA_R8_READ,VTK_VAR_XML_SCAL_3DA_R8_READ, & ! real(R8P)    scalar    1D/3D array
                   VTK_VAR_XML_SCAL_1DA_R4_READ,VTK_VAR_XML_SCAL_3DA_R4_READ, & ! real(R4P)    scalar    1D/3D array
                   VTK_VAR_XML_SCAL_1DA_I8_READ,VTK_VAR_XML_SCAL_3DA_I8_READ, & ! integer(I8P) scalar    1D/3D array
                   VTK_VAR_XML_SCAL_1DA_I4_READ,VTK_VAR_XML_SCAL_3DA_I4_READ, & ! integer(I4P) scalar    1D/3D array
                   VTK_VAR_XML_SCAL_1DA_I2_READ,VTK_VAR_XML_SCAL_3DA_I2_READ, & ! integer(I2P) scalar    1D/3D array
                   VTK_VAR_XML_SCAL_1DA_I1_READ,VTK_VAR_XML_SCAL_3DA_I1_READ, & ! integer(I1P) scalar    1D/3D array
                   VTK_VAR_XML_VECT_1DA_R8_READ,VTK_VAR_XML_VECT_3DA_R8_READ, & ! real(R8P)    vectorial 1D/3D arrays
                   VTK_VAR_XML_VECT_1DA_R4_READ,VTK_VAR_XML_VECT_3DA_R4_READ, & ! real(R4P)    vectorial 1D/3D arrays
                   VTK_VAR_XML_VECT_1DA_I8_READ,VTK_VAR_XML_VECT_3DA_I8_READ, & ! integer(I8P) vectorial 1D/3D arrays
                   VTK_VAR_XML_VECT_1DA_I4_READ,VTK_VAR_XML_VECT_3DA_I4_READ, & ! integer(I4P) vectorial 1D/3D arrays
                   VTK_VAR_XML_VECT_1DA_I2_READ,VTK_VAR_XML_VECT_3DA_I2_READ, & ! integer(I2P) vectorial 1D/3D arrays
                   VTK_VAR_XML_VECT_1DA_I1_READ,VTK_VAR_XML_VECT_3DA_I1_READ, & ! integer(I1P) vectorial 1D/3D arrays
                   VTK_VAR_XML_LIST_1DA_R8_READ,VTK_VAR_XML_LIST_3DA_R8_READ, & ! real(R8P)    list      1D/3D array
                   VTK_VAR_XML_LIST_1DA_R4_READ,VTK_VAR_XML_LIST_3DA_R4_READ, & ! real(R4P)    list      1D/3D array
                   VTK_VAR_XML_LIST_1DA_I8_READ,VTK_VAR_XML_LIST_3DA_I8_READ, & ! integer(I4P) list      1D/3D array
                   VTK_VAR_XML_LIST_1DA_I4_READ,VTK_VAR_XML_LIST_3DA_I4_READ, & ! integer(I4P) list      1D/3D array
                   VTK_VAR_XML_LIST_1DA_I2_READ,VTK_VAR_XML_LIST_3DA_I2_READ, & ! integer(I2P) list      1D/3D array
                   VTK_VAR_XML_LIST_1DA_I1_READ,VTK_VAR_XML_LIST_3DA_I1_READ    ! integer(I1P) list      1D/3D array
endinterface
!-----------------------------------------------------------------------------------------------------------------------------------
contains
  function VTK_DAT_XML(var_location,var_block_action,cf) result(E_IO)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Function for initializing/finalizing the saving of data associated to the mesh.
  !<
  !< Function that **must** be called before saving the data related to geometric mesh, this function initializes the
  !< saving of data variables indicating the *type* (node or cell centered) of variables that will be saved.
  !< @note A single file can contain both cell and node centered variables. In this case the VTK_DAT_XML function must be
  !< called two times, before saving cell-centered variables and before saving node-centered variables.
  !<
  !<### Examples of usage
  !<
  !<#### Opening node piece
  !<```fortran
  !< E_IO=VTK_DAT_XML('node','OPeN')
  !<```
  !<
  !<#### Closing node piece
  !<```fortran
  !< E_IO=VTK_DAT_XML('node','CLosE')
  !<```
  !<
  !<#### Opening cell piece
  !<```fortran
  !< E_IO=VTK_DAT_XML('cell','OPEN')
  !<```
  !<
  !<#### Closing cell piece
  !<```fortran
  !< E_IO=VTK_DAT_XML('cell','close')
  !<```
  !---------------------------------------------------------------------------------------------------------------------------------
  implicit none
  character(*), intent(IN)::           var_location     !< Location of saving variables: CELL or NODE centered.
  character(*), intent(IN)::           var_block_action !< Variables block action: OPEN or CLOSE block.
  integer(I4P), intent(IN), optional:: cf               !< Current file index (for concurrent files IO).
  integer(I4P)::                       E_IO             !< Input/Output inquiring flag: $0$ if IO is done, $> 0$ if IO is not done.
  integer(I4P)::                       rf               !< Real file index.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  E_IO = -1_I4P
  rf = f
  if (present(cf)) then
    rf = cf ; f = cf
  endif
  select case(vtk(rf)%f)
  case(ascii)
    select case(trim(Upper_Case(var_location)))
    case('CELL')
      select case(trim(Upper_Case(var_block_action)))
      case('OPEN')
        write(unit=vtk(rf)%u,fmt='(A)',iostat=E_IO)repeat(' ',vtk(rf)%indent)//'<CellData>' ; vtk(rf)%indent = vtk(rf)%indent + 2
      case('CLOSE')
        vtk(rf)%indent = vtk(rf)%indent - 2 ; write(unit=vtk(rf)%u,fmt='(A)',iostat=E_IO)repeat(' ',vtk(rf)%indent)//'</CellData>'
      endselect
    case('NODE')
      select case(trim(Upper_Case(var_block_action)))
      case('OPEN')
        write(unit=vtk(rf)%u,fmt='(A)',iostat=E_IO)repeat(' ',vtk(rf)%indent)//'<PointData>' ; vtk(rf)%indent = vtk(rf)%indent + 2
      case('CLOSE')
        vtk(rf)%indent = vtk(rf)%indent - 2 ; write(unit=vtk(rf)%u,fmt='(A)',iostat=E_IO)repeat(' ',vtk(rf)%indent)//'</PointData>'
      endselect
    endselect
  case(raw,binary,bin_app)
    select case(trim(Upper_Case(var_location)))
    case('CELL')
      select case(trim(Upper_Case(var_block_action)))
      case('OPEN')
        write(unit=vtk(rf)%u,iostat=E_IO)repeat(' ',vtk(rf)%indent)//'<CellData>'//end_rec ; vtk(rf)%indent = vtk(rf)%indent + 2
      case('CLOSE')
        vtk(rf)%indent = vtk(rf)%indent - 2 ; write(unit=vtk(rf)%u,iostat=E_IO)repeat(' ',vtk(rf)%indent)//'</CellData>'//end_rec
      endselect
    case('NODE')
      select case(trim(Upper_Case(var_block_action)))
      case('OPEN')
        write(unit=vtk(rf)%u,iostat=E_IO)repeat(' ',vtk(rf)%indent)//'<PointData>'//end_rec ; vtk(rf)%indent = vtk(rf)%indent + 2
      case('CLOSE')
        vtk(rf)%indent = vtk(rf)%indent - 2 ; write(unit=vtk(rf)%u,iostat=E_IO)repeat(' ',vtk(rf)%indent)//'</PointData>'//end_rec
      endselect
    endselect
  endselect
  return
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction VTK_DAT_XML

  ! exporters
  function VTK_VAR_XML_SCAL_1DA_R8_WRITE(NC_NN,varname,var,cf) result(E_IO)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Function for saving field of scalar variable (R8P, 1D array).
  !---------------------------------------------------------------------------------------------------------------------------------
  implicit none
  integer(I4P), intent(IN)::           NC_NN    !< Number of cells or nodes.
  character(*), intent(IN)::           varname  !< Variable name.
  real(R8P),    intent(IN)::           var(1:)  !< Variable to be saved [1:NC_NN].
  integer(I4P), intent(IN), optional:: cf       !< Current file index (for concurrent files IO).
  integer(I4P)::                       E_IO     !< Input/Output inquiring flag: $0$ if IO is done, $> 0$ if IO is not done.
  character(len=maxlen)::              s_buffer !< Buffer string.
  integer(I1P), allocatable::          varp(:)  !< Packed data.
  character(len=:), allocatable::      var64    !< Variable encoded in base64.
  integer(I4P)::                       rf       !< Real file index.
  integer(I4P)::                       n1       !< Counter.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  E_IO = -1_I4P
  rf = f
  if (present(cf)) then
    rf = cf ; f = cf
  endif
  select case(vtk(rf)%f)
  case(ascii)
    s_buffer = repeat(' ',vtk(rf)%indent)//'<DataArray type="Float64" Name="'//trim(varname)//&
               '" NumberOfComponents="1" format="ascii">'
    write(vtk(rf)%u,'(A)',iostat=E_IO)trim(s_buffer)
    write(vtk(rf)%u,'('//trim(str(NC_NN+1,.true.))//'A)',iostat=E_IO)repeat(' ',vtk(rf)%indent),(' '//str(n=var(n1)),n1=1,NC_NN)
    write(vtk(rf)%u,'(A)',iostat=E_IO)repeat(' ',vtk(rf)%indent)//'</DataArray>'
  case(raw,bin_app)
    s_buffer = repeat(' ',vtk(rf)%indent)//'<DataArray type="Float64" Name="'//trim(varname)//&
               '" NumberOfComponents="1" format="appended" offset="'//trim(str(vtk(rf)%ioffset,.true.))//'"/>'
    write(vtk(rf)%u,iostat=E_IO)trim(s_buffer)//end_rec
    call vtk(rf)%byte_update(N_Byte = NC_NN*BYR8P)
    write(vtk(rf)%ua,iostat=E_IO)vtk(rf)%N_Byte,'R8',NC_NN
    write(vtk(rf)%ua,iostat=E_IO)var
  case(binary)
    s_buffer=repeat(' ',vtk(rf)%indent)//'<DataArray type="Float64" Name="'//trim(varname)//&
             '" NumberOfComponents="1" format="binary">'
    write(vtk(rf)%u,iostat=E_IO)trim(s_buffer)//end_rec
    call pack_data(a1=[int(NC_NN*BYR8P,I4P)],a2=var,packed=varp)
    call b64_encode(n=varp,code=var64) ; deallocate(varp)
    write(vtk(rf)%u,iostat=E_IO)repeat(' ',vtk(rf)%indent+2)//var64//end_rec ; deallocate(var64)
    write(vtk(rf)%u,iostat=E_IO)repeat(' ',vtk(rf)%indent)//'</DataArray>'//end_rec
  endselect
  return
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction VTK_VAR_XML_SCAL_1DA_R8_WRITE

  function VTK_VAR_XML_SCAL_3DA_R8_WRITE(NC_NN,varname,var,cf) result(E_IO)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Function for saving field of scalar variable (R8P, 3D array).
  !---------------------------------------------------------------------------------------------------------------------------------
  implicit none
  integer(I4P), intent(IN)::           NC_NN         !< Number of cells or nodes.
  character(*), intent(IN)::           varname       !< Variable name.
  real(R8P),    intent(IN)::           var(1:,1:,1:) !< Variable to be saved [1:Nx,1:Ny,1:Nz].
  integer(I4P), intent(IN), optional:: cf            !< Current file index (for concurrent files IO).
  integer(I4P)::                       E_IO          !< Input/Output inquiring flag: $0$ if IO is done, $> 0$ if IO is not done.
  character(len=maxlen)::              s_buffer      !< Buffer string.
  integer(I1P), allocatable::          varp(:)       !< Packed data.
  character(len=:), allocatable::      var64         !< Variable encoded in base64.
  integer(I4P)::                       rf            !< Real file index.
  integer(I4P)::                       nx,ny,nz      !< Counters.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  E_IO = -1_I4P
  rf = f
  if (present(cf)) then
    rf = cf ; f = cf
  endif
  select case(vtk(rf)%f)
  case(ascii)
    s_buffer = repeat(' ',vtk(rf)%indent)//'<DataArray type="Float64" Name="'//trim(varname)//&
               '" NumberOfComponents="1" format="ascii">'
    write(vtk(rf)%u,'(A)', iostat=E_IO)trim(s_buffer)
    write(vtk(rf)%u,'('//trim(str(NC_NN+1,.true.))//'A)',iostat=E_IO)repeat(' ',vtk(rf)%indent),&
                                      (((' '//str(n=var(nx,ny,nz)),nx=1,size(var,dim=1)),ny=1,size(var,dim=2)),nz=1,size(var,dim=3))
    write(vtk(rf)%u,'(A)', iostat=E_IO)repeat(' ',vtk(rf)%indent)//'</DataArray>'
  case(raw,bin_app)
    s_buffer = repeat(' ',vtk(rf)%indent)//'<DataArray type="Float64" Name="'//trim(varname)//&
               '" NumberOfComponents="1" format="appended" offset="'//trim(str(vtk(rf)%ioffset,.true.))//'"/>'
    write(vtk(rf)%u,iostat=E_IO)trim(s_buffer)//end_rec
    call vtk(rf)%byte_update(N_Byte = NC_NN*BYR8P)
    write(vtk(rf)%ua,iostat=E_IO)vtk(rf)%N_Byte,'R8',NC_NN
    write(vtk(rf)%ua,iostat=E_IO)var
  case(binary)
    s_buffer=repeat(' ',vtk(rf)%indent)//'<DataArray type="Float64" Name="'//trim(varname)//&
             '" NumberOfComponents="1" format="binary">'
    write(vtk(rf)%u,iostat=E_IO)trim(s_buffer)//end_rec
    call pack_data(a1=[int(NC_NN*BYR8P,I4P)],a2=reshape(var,[NC_NN]),packed=varp)
    call b64_encode(n=varp,code=var64) ; deallocate(varp)
    write(vtk(rf)%u,iostat=E_IO)repeat(' ',vtk(rf)%indent+2)//var64//end_rec ; deallocate(var64)
    write(vtk(rf)%u,iostat=E_IO)repeat(' ',vtk(rf)%indent)//'</DataArray>'//end_rec
  endselect
  return
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction VTK_VAR_XML_SCAL_3DA_R8_WRITE

  function VTK_VAR_XML_SCAL_1DA_R4_WRITE(NC_NN,varname,var,cf) result(E_IO)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Function for saving field of scalar variable (R4P, 1D array).
  !---------------------------------------------------------------------------------------------------------------------------------
  implicit none
  integer(I4P), intent(IN)::           NC_NN    !< Number of cells or nodes.
  character(*), intent(IN)::           varname  !< Variable name.
  real(R4P),    intent(IN)::           var(1:)  !< Variable to be saved [1:NC_NN].
  integer(I4P), intent(IN), optional:: cf       !< Current file index (for concurrent files IO).
  integer(I4P)::                       E_IO     !< Input/Output inquiring flag: $0$ if IO is done, $> 0$ if IO is not done.
  character(len=maxlen)::              s_buffer !< Buffer string.
  integer(I1P), allocatable::          varp(:)  !< Packed data.
  character(len=:), allocatable::      var64    !< Variable encoded in base64.
  integer(I4P)::                       rf       !< Real file index.
  integer(I4P)::                       n1       !< Counter.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  E_IO = -1_I4P
  rf = f
  if (present(cf)) then
    rf = cf ; f = cf
  endif
  select case(vtk(rf)%f)
  case(ascii)
    s_buffer = repeat(' ',vtk(rf)%indent)//'<DataArray type="Float32" Name="'//trim(varname)//&
               '" NumberOfComponents="1" format="ascii">'
    write(vtk(rf)%u,'(A)', iostat=E_IO)trim(s_buffer)
    write(vtk(rf)%u,'('//trim(str(NC_NN+1,.true.))//'A)',iostat=E_IO)repeat(' ',vtk(rf)%indent),(' '//str(n=var(n1)),n1=1,NC_NN)
    write(vtk(rf)%u,'(A)', iostat=E_IO)repeat(' ',vtk(rf)%indent)//'</DataArray>'
  case(raw,bin_app)
    s_buffer = repeat(' ',vtk(rf)%indent)//'<DataArray type="Float32" Name="'//trim(varname)//&
               '" NumberOfComponents="1" format="appended" offset="'//trim(str(vtk(rf)%ioffset,.true.))//'"/>'
    write(vtk(rf)%u,iostat=E_IO)trim(s_buffer)//end_rec
    call vtk(rf)%byte_update(N_Byte = NC_NN*BYR4P)
    write(vtk(rf)%ua,iostat=E_IO)vtk(rf)%N_Byte,'R4',NC_NN
    write(vtk(rf)%ua,iostat=E_IO)var
  case(binary)
    s_buffer=repeat(' ',vtk(rf)%indent)//'<DataArray type="Float32" Name="'//trim(varname)//&
             '" NumberOfComponents="1" format="binary">'
    write(vtk(rf)%u,iostat=E_IO)trim(s_buffer)//end_rec
    call pack_data(a1=[int(NC_NN*BYR4P,I4P)],a2=var,packed=varp)
    call b64_encode(n=varp,code=var64) ; deallocate(varp)
    write(vtk(rf)%u,iostat=E_IO)repeat(' ',vtk(rf)%indent+2)//var64//end_rec ; deallocate(var64)
    write(vtk(rf)%u,iostat=E_IO)repeat(' ',vtk(rf)%indent)//'</DataArray>'//end_rec
  endselect
  return
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction VTK_VAR_XML_SCAL_1DA_R4_WRITE

  function VTK_VAR_XML_SCAL_3DA_R4_WRITE(NC_NN,varname,var,cf) result(E_IO)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Function for saving field of scalar variable (R4P, 3D array).
  !---------------------------------------------------------------------------------------------------------------------------------
  implicit none
  integer(I4P), intent(IN)::           NC_NN         !< Number of cells or nodes.
  character(*), intent(IN)::           varname       !< Variable name.
  real(R4P),    intent(IN)::           var(1:,1:,1:) !< Variable to be saved [1:Nx,1:Ny,1:Nz].
  integer(I4P), intent(IN), optional:: cf            !< Current file index (for concurrent files IO).
  integer(I4P)::                       E_IO          !< Input/Output inquiring flag: $0$ if IO is done, $> 0$ if IO is not done.
  character(len=maxlen)::              s_buffer      !< Buffer string.
  integer(I1P), allocatable::          varp(:)       !< Packed data.
  character(len=:), allocatable::      var64         !< Variable encoded in base64.
  integer(I4P)::                       rf            !< Real file index.
  integer(I4P)::                       nx,ny,nz      !< Counters.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  E_IO = -1_I4P
  rf = f
  if (present(cf)) then
    rf = cf ; f = cf
  endif
  select case(vtk(rf)%f)
  case(ascii)
    s_buffer = repeat(' ',vtk(rf)%indent)//'<DataArray type="Float32" Name="'//trim(varname)//&
               '" NumberOfComponents="1" format="ascii">'
    write(vtk(rf)%u,'(A)', iostat=E_IO)trim(s_buffer)
    write(vtk(rf)%u,'('//trim(str(NC_NN+1,.true.))//'A)',iostat=E_IO)repeat(' ',vtk(rf)%indent),&
                                      (((' '//str(n=var(nx,ny,nz)),nx=1,size(var,dim=1)),ny=1,size(var,dim=2)),nz=1,size(var,dim=3))
    write(vtk(rf)%u,'(A)', iostat=E_IO)repeat(' ',vtk(rf)%indent)//'</DataArray>'
  case(raw,bin_app)
    s_buffer = repeat(' ',vtk(rf)%indent)//'<DataArray type="Float32" Name="'//trim(varname)//&
               '" NumberOfComponents="1" format="appended" offset="'//trim(str(vtk(rf)%ioffset,.true.))//'"/>'
    write(vtk(rf)%u,iostat=E_IO)trim(s_buffer)//end_rec
    call vtk(rf)%byte_update(N_Byte = NC_NN*BYR4P)
    write(vtk(rf)%ua,iostat=E_IO)vtk(rf)%N_Byte,'R4',NC_NN
    write(vtk(rf)%ua,iostat=E_IO)var
  case(binary)
    s_buffer=repeat(' ',vtk(rf)%indent)//'<DataArray type="Float32" Name="'//trim(varname)//&
             '" NumberOfComponents="1" format="binary">'
    write(vtk(rf)%u,iostat=E_IO)trim(s_buffer)//end_rec
    call pack_data(a1=[int(NC_NN*BYR4P,I4P)],a2=reshape(var,[NC_NN]),packed=varp)
    call b64_encode(n=varp,code=var64) ; deallocate(varp)
    write(vtk(rf)%u,iostat=E_IO)repeat(' ',vtk(rf)%indent+2)//var64//end_rec ; deallocate(var64)
    write(vtk(rf)%u,iostat=E_IO)repeat(' ',vtk(rf)%indent)//'</DataArray>'//end_rec
  endselect
  return
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction VTK_VAR_XML_SCAL_3DA_R4_WRITE

  function VTK_VAR_XML_SCAL_1DA_I8_WRITE(NC_NN,varname,var,cf) result(E_IO)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Function for saving field of scalar variable (I8P, 1D array).
  !---------------------------------------------------------------------------------------------------------------------------------
  implicit none
  integer(I4P), intent(IN)::           NC_NN    !< Number of cells or nodes.
  character(*), intent(IN)::           varname  !< Variable name.
  integer(I8P), intent(IN)::           var(1:)  !< Variable to be saved [1:NC_NN].
  integer(I4P), intent(IN), optional:: cf       !< Current file index (for concurrent files IO).
  integer(I4P)::                       E_IO     !< Input/Output inquiring flag: $0$ if IO is done, $> 0$ if IO is not done.
  character(len=maxlen)::              s_buffer !< Buffer string.
  integer(I1P), allocatable::          varp(:)  !< Packed data.
  character(len=:), allocatable::      var64    !< Variable encoded in base64.
  integer(I4P)::                       rf       !< Real file index.
  integer(I4P)::                       n1       !< Counter.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  E_IO = -1_I4P
  rf = f
  if (present(cf)) then
    rf = cf ; f = cf
  endif
  select case(vtk(rf)%f)
  case(ascii)
    s_buffer = repeat(' ',vtk(rf)%indent)//'<DataArray type="Int64" Name="'//trim(varname)//&
               '" NumberOfComponents="1" format="ascii">'
    write(vtk(rf)%u,'(A)',iostat=E_IO)trim(s_buffer)
    write(vtk(rf)%u,'('//trim(str(NC_NN+1,.true.))//'A)',iostat=E_IO)repeat(' ',vtk(rf)%indent),(' '//str(n=var(n1)),n1=1,NC_NN)
    write(vtk(rf)%u,'(A)',iostat=E_IO)'</DataArray>'
  case(raw,bin_app)
    s_buffer = repeat(' ',vtk(rf)%indent)//'<DataArray type="Int64" Name="'//trim(varname)//&
               '" NumberOfComponents="1" format="appended" offset="'//trim(str(vtk(rf)%ioffset,.true.))//'"/>'
    write(vtk(rf)%u,iostat=E_IO)trim(s_buffer)//end_rec
    call vtk(rf)%byte_update(N_Byte = int(NC_NN*BYI8P,I4P))
    write(vtk(rf)%ua,iostat=E_IO)vtk(rf)%N_Byte,'I8',NC_NN
    write(vtk(rf)%ua,iostat=E_IO)var
  case(binary)
    s_buffer=repeat(' ',vtk(rf)%indent)//'<DataArray type="Int64" Name="'//trim(varname)//&
             '" NumberOfComponents="1" format="binary">'
    write(vtk(rf)%u,iostat=E_IO)trim(s_buffer)//end_rec
    call pack_data(a1=[int(NC_NN*BYI8P,I4P)],a2=var,packed=varp)
    call b64_encode(n=varp,code=var64) ; deallocate(varp)
    write(vtk(rf)%u,iostat=E_IO)repeat(' ',vtk(rf)%indent+2)//var64//end_rec ; deallocate(var64)
    write(vtk(rf)%u,iostat=E_IO)repeat(' ',vtk(rf)%indent)//'</DataArray>'//end_rec
  endselect
  return
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction VTK_VAR_XML_SCAL_1DA_I8_WRITE

  function VTK_VAR_XML_SCAL_3DA_I8_WRITE(NC_NN,varname,var,cf) result(E_IO)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Function for saving field of scalar variable (I8P, 3D array).
  !---------------------------------------------------------------------------------------------------------------------------------
  implicit none
  integer(I4P), intent(IN)::           NC_NN         !< Number of cells or nodes.
  character(*), intent(IN)::           varname       !< Variable name.
  integer(I8P), intent(IN)::           var(1:,1:,1:) !< Variable to be saved [1:Nx,1:Ny,1:Nz].
  integer(I4P), intent(IN), optional:: cf            !< Current file index (for concurrent files IO).
  integer(I4P)::                       E_IO          !< Input/Output inquiring flag: $0$ if IO is done, $> 0$ if IO is not done.
  character(len=maxlen)::              s_buffer      !< Buffer string.
  integer(I1P), allocatable::          varp(:)       !< Packed data.
  character(len=:), allocatable::      var64         !< Variable encoded in base64.
  integer(I4P)::                       rf            !< Real file index.
  integer(I4P)::                       nx,ny,nz      !< Counters.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  E_IO = -1_I4P
  rf = f
  if (present(cf)) then
    rf = cf ; f = cf
  endif
  select case(vtk(rf)%f)
  case(ascii)
    s_buffer = repeat(' ',vtk(rf)%indent)//'<DataArray type="Int64" Name="'//trim(varname)//&
               '" NumberOfComponents="1" format="ascii">'
    write(vtk(rf)%u,'(A)',iostat=E_IO)trim(s_buffer)
    write(vtk(rf)%u,'('//trim(str(NC_NN+1,.true.))//'A)',iostat=E_IO)repeat(' ',vtk(rf)%indent),&
                                      (((' '//str(n=var(nx,ny,nz)),nx=1,size(var,dim=1)),ny=1,size(var,dim=2)),nz=1,size(var,dim=3))
    write(vtk(rf)%u,'(A)',iostat=E_IO)'</DataArray>'
  case(raw,bin_app)
    s_buffer = repeat(' ',vtk(rf)%indent)//'<DataArray type="Int64" Name="'//trim(varname)//&
               '" NumberOfComponents="1" format="appended" offset="'//trim(str(vtk(rf)%ioffset,.true.))//'"/>'
    write(vtk(rf)%u,iostat=E_IO)trim(s_buffer)//end_rec
    call vtk(rf)%byte_update(N_Byte = int(NC_NN*BYI8P,I4P))
    write(vtk(rf)%ua,iostat=E_IO)vtk(rf)%N_Byte,'I8',NC_NN
    write(vtk(rf)%ua,iostat=E_IO)var
  case(binary)
    s_buffer=repeat(' ',vtk(rf)%indent)//'<DataArray type="Int64" Name="'//trim(varname)//&
             '" NumberOfComponents="1" format="binary">'
    write(vtk(rf)%u,iostat=E_IO)trim(s_buffer)//end_rec
    call pack_data(a1=[int(NC_NN*BYI8P,I4P)],a2=reshape(var,[NC_NN]),packed=varp)
    call b64_encode(n=varp,code=var64) ; deallocate(varp)
    write(vtk(rf)%u,iostat=E_IO)repeat(' ',vtk(rf)%indent+2)//var64//end_rec ; deallocate(var64)
    write(vtk(rf)%u,iostat=E_IO)repeat(' ',vtk(rf)%indent)//'</DataArray>'//end_rec
  endselect
  return
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction VTK_VAR_XML_SCAL_3DA_I8_WRITE

  function VTK_VAR_XML_SCAL_1DA_I4_WRITE(NC_NN,varname,var,cf) result(E_IO)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Function for saving field of scalar variable (I4P, 1D array).
  !---------------------------------------------------------------------------------------------------------------------------------
  implicit none
  integer(I4P), intent(IN)::           NC_NN    !< Number of cells or nodes.
  character(*), intent(IN)::           varname  !< Variable name.
  integer(I4P), intent(IN)::           var(1:)  !< Variable to be saved [1:NC_NN].
  integer(I4P), intent(IN), optional:: cf       !< Current file index (for concurrent files IO).
  integer(I4P)::                       E_IO     !< Input/Output inquiring flag: $0$ if IO is done, $> 0$ if IO is not done.
  character(len=maxlen)::              s_buffer !< Buffer string.
  integer(I1P), allocatable::          varp(:)  !< Packed data.
  character(len=:), allocatable::      var64    !< Variable encoded in base64.
  integer(I4P)::                       rf       !< Real file index.
  integer(I4P)::                       n1       !< Counter.
  integer(I8P)::                       Nvarp    !< Dimension of varp, packed data.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  E_IO = -1_I4P
  rf = f
  if (present(cf)) then
    rf = cf ; f = cf
  endif
  select case(vtk(rf)%f)
  case(ascii)
    s_buffer = repeat(' ',vtk(rf)%indent)//'<DataArray type="Int32" Name="'//trim(varname)//&
               '" NumberOfComponents="1" format="ascii">'
    write(vtk(rf)%u,'(A)',iostat=E_IO)trim(s_buffer)
    write(vtk(rf)%u,'('//trim(str(NC_NN+1,.true.))//'A)',iostat=E_IO)repeat(' ',vtk(rf)%indent),(' '//str(n=var(n1)),n1=1,NC_NN)
    write(vtk(rf)%u,'(A)',iostat=E_IO)repeat(' ',vtk(rf)%indent)//'</DataArray>'
  case(raw,bin_app)
    s_buffer = repeat(' ',vtk(rf)%indent)//'<DataArray type="Int32" Name="'//trim(varname)// &
               '" NumberOfComponents="1" format="appended" offset="'//trim(str(vtk(rf)%ioffset,.true.))//'"/>'
    write(vtk(rf)%u,iostat=E_IO)trim(s_buffer)//end_rec
    call vtk(rf)%byte_update(N_Byte = NC_NN*BYI4P)
    write(vtk(rf)%ua,iostat=E_IO)vtk(rf)%N_Byte,'I4',NC_NN
    write(vtk(rf)%ua,iostat=E_IO)var
  case(binary)
    s_buffer=repeat(' ',vtk(rf)%indent)//'<DataArray type="Int32" Name="'//trim(varname)// &
             '" NumberOfComponents="1" format="binary">'
    write(vtk(rf)%u,iostat=E_IO)trim(s_buffer)//end_rec
    Nvarp=size(transfer([int(NC_NN*BYI4P,I4P),var],varp),kind=I8P) ; if (allocated(varp)) deallocate(varp) ; allocate(varp(1:Nvarp))
    varp = transfer([int(NC_NN*BYI4P,I4P),var],varp)
    call b64_encode(n=varp,code=var64) ; deallocate(varp)
    write(vtk(rf)%u,iostat=E_IO)repeat(' ',vtk(rf)%indent+2)//var64//end_rec ; deallocate(var64)
    write(vtk(rf)%u,iostat=E_IO)repeat(' ',vtk(rf)%indent)//'</DataArray>'//end_rec
  endselect
  return
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction VTK_VAR_XML_SCAL_1DA_I4_WRITE

  function VTK_VAR_XML_SCAL_3DA_I4_WRITE(NC_NN,varname,var,cf) result(E_IO)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Function for saving field of scalar variable (I4P, 3D array).
  !---------------------------------------------------------------------------------------------------------------------------------
  implicit none
  integer(I4P), intent(IN)::           NC_NN         !< Number of cells or nodes.
  character(*), intent(IN)::           varname       !< Variable name.
  integer(I4P), intent(IN)::           var(1:,1:,1:) !< Variable to be saved [1:Nx,1:Ny,1:Nz].
  integer(I4P), intent(IN), optional:: cf            !< Current file index (for concurrent files IO).
  integer(I4P)::                       E_IO          !< Input/Output inquiring flag: $0$ if IO is done, $> 0$ if IO is not done.
  character(len=maxlen)::              s_buffer      !< Buffer string.
  integer(I1P), allocatable::          varp(:)       !< Packed data.
  character(len=:), allocatable::      var64         !< Variable encoded in base64.
  integer(I4P)::                       rf            !< Real file index.
  integer(I4P)::                       nx,ny,nz      !< Counters.
  integer(I8P)::                       Nvarp         !< Dimension of varp, packed data.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  E_IO = -1_I4P
  rf = f
  if (present(cf)) then
    rf = cf ; f = cf
  endif
  select case(vtk(rf)%f)
  case(ascii)
    s_buffer = repeat(' ',vtk(rf)%indent)//'<DataArray type="Int32" Name="'//trim(varname)//&
               '" NumberOfComponents="1" format="ascii">'
    write(vtk(rf)%u,'(A)',iostat=E_IO)trim(s_buffer)
    write(vtk(rf)%u,'('//trim(str(NC_NN+1,.true.))//'A)',iostat=E_IO)repeat(' ',vtk(rf)%indent),&
                                      (((' '//str(n=var(nx,ny,nz)),nx=1,size(var,dim=1)),ny=1,size(var,dim=2)),nz=1,size(var,dim=3))
    write(vtk(rf)%u,'(A)',iostat=E_IO)repeat(' ',vtk(rf)%indent)//'</DataArray>'
  case(raw,bin_app)
    s_buffer = repeat(' ',vtk(rf)%indent)//'<DataArray type="Int32" Name="'//trim(varname)// &
               '" NumberOfComponents="1" format="appended" offset="'//trim(str(vtk(rf)%ioffset,.true.))//'"/>'
    write(vtk(rf)%u,iostat=E_IO)trim(s_buffer)//end_rec
    call vtk(rf)%byte_update(N_Byte = NC_NN*BYI4P)
    write(vtk(rf)%ua,iostat=E_IO)vtk(rf)%N_Byte,'I4',NC_NN
    write(vtk(rf)%ua,iostat=E_IO)var
  case(binary)
    s_buffer=repeat(' ',vtk(rf)%indent)//'<DataArray type="Int32" Name="'//trim(varname)// &
             '" NumberOfComponents="1" format="binary">'
    write(vtk(rf)%u,iostat=E_IO)trim(s_buffer)//end_rec
    Nvarp=size(transfer([int(NC_NN*BYI4P,I4P),reshape(var,[NC_NN])],varp),kind=I8P)
    if (allocated(varp)) deallocate(varp); allocate(varp(1:Nvarp))
    varp = transfer([int(NC_NN*BYI4P,I4P),reshape(var,[NC_NN])],varp)
    call b64_encode(n=varp,code=var64) ; deallocate(varp)
    write(vtk(rf)%u,iostat=E_IO)repeat(' ',vtk(rf)%indent+2)//var64//end_rec ; deallocate(var64)
    write(vtk(rf)%u,iostat=E_IO)repeat(' ',vtk(rf)%indent)//'</DataArray>'//end_rec
  endselect
  return
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction VTK_VAR_XML_SCAL_3DA_I4_WRITE

  function VTK_VAR_XML_SCAL_1DA_I2_WRITE(NC_NN,varname,var,cf) result(E_IO)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Function for saving field of scalar variable (I2P, 1D array).
  !---------------------------------------------------------------------------------------------------------------------------------
  implicit none
  integer(I4P), intent(IN)::           NC_NN    !< Number of cells or nodes.
  character(*), intent(IN)::           varname  !< Variable name.
  integer(I2P), intent(IN)::           var(1:)  !< Variable to be saved [1:NC_NN].
  integer(I4P), intent(IN), optional:: cf       !< Current file index (for concurrent files IO).
  integer(I4P)::                       E_IO     !< Input/Output inquiring flag: $0$ if IO is done, $> 0$ if IO is not done.
  character(len=maxlen)::              s_buffer !< Buffer string.
  integer(I1P), allocatable::          varp(:)  !< Packed data.
  character(len=:), allocatable::      var64    !< Variable encoded in base64.
  integer(I4P)::                       rf       !< Real file index.
  integer(I4P)::                       n1       !< Counter.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  E_IO = -1_I4P
  rf = f
  if (present(cf)) then
    rf = cf ; f = cf
  endif
  select case(vtk(rf)%f)
  case(ascii)
    s_buffer = repeat(' ',vtk(rf)%indent)//'<DataArray type="Int16" Name="'//trim(varname)//&
               '" NumberOfComponents="1" format="ascii">'
    write(vtk(rf)%u,'(A)',iostat=E_IO)trim(s_buffer)
    write(vtk(rf)%u,'('//trim(str(NC_NN+1,.true.))//'A)',iostat=E_IO)repeat(' ',vtk(rf)%indent),(' '//str(n=var(n1)),n1=1,NC_NN)
    write(vtk(rf)%u,'(A)',iostat=E_IO)repeat(' ',vtk(rf)%indent)//'</DataArray>'
  case(raw,bin_app)
    s_buffer = repeat(' ',vtk(rf)%indent)//'<DataArray type="Int16" Name="'//trim(varname)//&
               '" NumberOfComponents="1" format="appended" offset="'//trim(str(vtk(rf)%ioffset,.true.))//'"/>'
    write(vtk(rf)%u,iostat=E_IO)trim(s_buffer)//end_rec
    call vtk(rf)%byte_update(N_Byte = NC_NN*BYI2P)
    write(vtk(rf)%ua,iostat=E_IO)vtk(rf)%N_Byte,'I2',NC_NN
    write(vtk(rf)%ua,iostat=E_IO)var
  case(binary)
    s_buffer=repeat(' ',vtk(rf)%indent)//'<DataArray type="Int16" Name="'//trim(varname)//&
             '" NumberOfComponents="1" format="binary">'
    write(vtk(rf)%u,iostat=E_IO)trim(s_buffer)//end_rec
    call pack_data(a1=[int(NC_NN*BYI2P,I4P)],a2=var,packed=varp)
    call b64_encode(n=varp,code=var64) ; deallocate(varp)
    write(vtk(rf)%u,iostat=E_IO)repeat(' ',vtk(rf)%indent+2)//var64//end_rec ; deallocate(var64)
    write(vtk(rf)%u,iostat=E_IO)repeat(' ',vtk(rf)%indent)//'</DataArray>'//end_rec
  endselect
  return
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction VTK_VAR_XML_SCAL_1DA_I2_WRITE

  function VTK_VAR_XML_SCAL_3DA_I2_WRITE(NC_NN,varname,var,cf) result(E_IO)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Function for saving field of scalar variable (I2P, 3D array).
  !---------------------------------------------------------------------------------------------------------------------------------
  implicit none
  integer(I4P), intent(IN)::           NC_NN         !< Number of cells or nodes.
  character(*), intent(IN)::           varname       !< Variable name.
  integer(I2P), intent(IN)::           var(1:,1:,1:) !< Variable to be saved [1:Nx,1:Ny,1:Nz].
  integer(I4P), intent(IN), optional:: cf            !< Current file index (for concurrent files IO).
  integer(I4P)::                       E_IO          !< Input/Output inquiring flag: $0$ if IO is done, $> 0$ if IO is not done.
  character(len=maxlen)::              s_buffer      !< Buffer string.
  integer(I1P), allocatable::          varp(:)       !< Packed data.
  character(len=:), allocatable::      var64         !< Variable encoded in base64.
  integer(I4P)::                       rf            !< Real file index.
  integer(I4P)::                       nx,ny,nz      !< Counters.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  E_IO = -1_I4P
  rf = f
  if (present(cf)) then
    rf = cf ; f = cf
  endif
  select case(vtk(rf)%f)
  case(ascii)
    s_buffer = repeat(' ',vtk(rf)%indent)//'<DataArray type="Int16" Name="'//trim(varname)//&
               '" NumberOfComponents="1" format="ascii">'
    write(vtk(rf)%u,'(A)',iostat=E_IO)trim(s_buffer)
    write(vtk(rf)%u,'('//trim(str(NC_NN+1,.true.))//'A)',iostat=E_IO)repeat(' ',vtk(rf)%indent),&
                                      (((' '//str(n=var(nx,ny,nz)),nx=1,size(var,dim=1)),ny=1,size(var,dim=2)),nz=1,size(var,dim=3))
    write(vtk(rf)%u,'(A)',iostat=E_IO)repeat(' ',vtk(rf)%indent)//'</DataArray>'
  case(raw,bin_app)
    s_buffer = repeat(' ',vtk(rf)%indent)//'<DataArray type="Int16" Name="'//trim(varname)//&
               '" NumberOfComponents="1" format="appended" offset="'//trim(str(vtk(rf)%ioffset,.true.))//'"/>'
    write(vtk(rf)%u,iostat=E_IO)trim(s_buffer)//end_rec
    call vtk(rf)%byte_update(N_Byte = NC_NN*BYI2P)
    write(vtk(rf)%ua,iostat=E_IO)vtk(rf)%N_Byte,'I2',NC_NN
    write(vtk(rf)%ua,iostat=E_IO)var
  case(binary)
    s_buffer=repeat(' ',vtk(rf)%indent)//'<DataArray type="Int16" Name="'//trim(varname)//&
             '" NumberOfComponents="1" format="binary">'
    write(vtk(rf)%u,iostat=E_IO)trim(s_buffer)//end_rec
    call pack_data(a1=[int(NC_NN*BYI2P,I4P)],a2=reshape(var,[NC_NN]),packed=varp)
    call b64_encode(n=varp,code=var64) ; deallocate(varp)
    write(vtk(rf)%u,iostat=E_IO)repeat(' ',vtk(rf)%indent+2)//var64//end_rec ; deallocate(var64)
    write(vtk(rf)%u,iostat=E_IO)repeat(' ',vtk(rf)%indent)//'</DataArray>'//end_rec
  endselect
  return
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction VTK_VAR_XML_SCAL_3DA_I2_WRITE

  function VTK_VAR_XML_SCAL_1DA_I1_WRITE(NC_NN,varname,var,cf) result(E_IO)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Function for saving field of scalar variable (I1P, 1D array).
  !---------------------------------------------------------------------------------------------------------------------------------
  implicit none
  integer(I4P), intent(IN)::           NC_NN    !< Number of cells or nodes.
  character(*), intent(IN)::           varname  !< Variable name.
  integer(I1P), intent(IN)::           var(1:)  !< Variable to be saved [1:NC_NN].
  integer(I4P), intent(IN), optional:: cf       !< Current file index (for concurrent files IO).
  integer(I4P)::                       E_IO     !< Input/Output inquiring flag: $0$ if IO is done, $> 0$ if IO is not done.
  character(len=maxlen)::              s_buffer !< Buffer string.
  integer(I1P), allocatable::          varp(:)  !< Packed data.
  character(len=:), allocatable::      var64    !< Variable encoded in base64.
  integer(I4P)::                       rf       !< Real file index.
  integer(I4P)::                       n1       !< Counter.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  E_IO = -1_I4P
  rf = f
  if (present(cf)) then
    rf = cf ; f = cf
  endif
  select case(vtk(rf)%f)
  case(ascii)
    s_buffer=repeat(' ',vtk(rf)%indent)//'<DataArray type="Int8" Name="'//trim(varname)//'" NumberOfComponents="1" format="ascii">'
    write(vtk(rf)%u,'(A)',iostat=E_IO)trim(s_buffer)
    write(vtk(rf)%u,'('//trim(str(NC_NN+1,.true.))//'A)',iostat=E_IO)repeat(' ',vtk(rf)%indent),(' '//str(n=var(n1)),n1=1,NC_NN)
    write(vtk(rf)%u,'(A)',iostat=E_IO)repeat(' ',vtk(rf)%indent)//'</DataArray>'
  case(raw,bin_app)
    s_buffer=repeat(' ',vtk(rf)%indent)//'<DataArray type="Int8" Name="'//trim(varname)//&
             '" NumberOfComponents="1" format="appended" offset="'//trim(str(vtk(rf)%ioffset,.true.))//'"/>'
    write(vtk(rf)%u,iostat=E_IO)trim(s_buffer)//end_rec
    call vtk(rf)%byte_update(N_Byte = NC_NN*BYI1P)
    write(vtk(rf)%ua,iostat=E_IO)vtk(rf)%N_Byte,'I1',NC_NN
    write(vtk(rf)%ua,iostat=E_IO)var
  case(binary)
    s_buffer=repeat(' ',vtk(rf)%indent)//'<DataArray type="Int8" Name="'//trim(varname)//&
             '" NumberOfComponents="1" format="binary">'
    write(vtk(rf)%u,iostat=E_IO)trim(s_buffer)//end_rec
    call pack_data(a1=[int(NC_NN*BYI1P,I4P)],a2=var,packed=varp)
    call b64_encode(n=varp,code=var64) ; deallocate(varp)
    write(vtk(rf)%u,iostat=E_IO)repeat(' ',vtk(rf)%indent+2)//var64//end_rec ; deallocate(var64)
    write(vtk(rf)%u,iostat=E_IO)repeat(' ',vtk(rf)%indent)//'</DataArray>'//end_rec
  endselect
  return
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction VTK_VAR_XML_SCAL_1DA_I1_WRITE

  function VTK_VAR_XML_SCAL_3DA_I1_WRITE(NC_NN,varname,var,cf) result(E_IO)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Function for saving field of scalar variable (I1P, 3D array).
  !---------------------------------------------------------------------------------------------------------------------------------
  implicit none
  integer(I4P), intent(IN)::           NC_NN         !< Number of cells or nodes.
  character(*), intent(IN)::           varname       !< Variable name.
  integer(I1P), intent(IN)::           var(1:,1:,1:) !< Variable to be saved [1:Nx,1:ny,1:Nz].
  integer(I4P), intent(IN), optional:: cf            !< Current file index (for concurrent files IO).
  integer(I4P)::                       E_IO          !< Input/Output inquiring flag: $0$ if IO is done, $> 0$ if IO is not done.
  character(len=maxlen)::              s_buffer      !< Buffer string.
  integer(I1P), allocatable::          varp(:)       !< Packed data.
  character(len=:), allocatable::      var64         !< Variable encoded in base64.
  integer(I4P)::                       rf            !< Real file index.
  integer(I4P)::                       nx,ny,nz      !< Counters.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  E_IO = -1_I4P
  rf = f
  if (present(cf)) then
    rf = cf ; f = cf
  endif
  select case(vtk(rf)%f)
  case(ascii)
    s_buffer=repeat(' ',vtk(rf)%indent)//'<DataArray type="Int8" Name="'//trim(varname)//'" NumberOfComponents="1" format="ascii">'
    write(vtk(rf)%u,'(A)',iostat=E_IO)trim(s_buffer)
    write(vtk(rf)%u,'('//trim(str(NC_NN+1,.true.))//'A)',iostat=E_IO)repeat(' ',vtk(rf)%indent),&
                                      (((' '//str(n=var(nx,ny,nz)),nx=1,size(var,dim=1)),ny=1,size(var,dim=2)),nz=1,size(var,dim=3))
    write(vtk(rf)%u,'(A)',iostat=E_IO)repeat(' ',vtk(rf)%indent)//'</DataArray>'
  case(raw,bin_app)
    s_buffer=repeat(' ',vtk(rf)%indent)//'<DataArray type="Int8" Name="'//trim(varname)//&
             '" NumberOfComponents="1" format="appended" offset="'//trim(str(vtk(rf)%ioffset,.true.))//'"/>'
    write(vtk(rf)%u,iostat=E_IO)trim(s_buffer)//end_rec
    call vtk(rf)%byte_update(N_Byte = NC_NN*BYI1P)
    write(vtk(rf)%ua,iostat=E_IO)vtk(rf)%N_Byte,'I1',NC_NN
    write(vtk(rf)%ua,iostat=E_IO)var
  case(binary)
    s_buffer=repeat(' ',vtk(rf)%indent)//'<DataArray type="Int8" Name="'//trim(varname)//&
             '" NumberOfComponents="1" format="binary">'
    write(vtk(rf)%u,iostat=E_IO)trim(s_buffer)//end_rec
    call pack_data(a1=[int(NC_NN*BYI1P,I4P)],a2=reshape(var,[NC_NN]),packed=varp)
    call b64_encode(n=varp,code=var64) ; deallocate(varp)
    write(vtk(rf)%u,iostat=E_IO)repeat(' ',vtk(rf)%indent+2)//var64//end_rec ; deallocate(var64)
    write(vtk(rf)%u,iostat=E_IO)repeat(' ',vtk(rf)%indent)//'</DataArray>'//end_rec
  endselect
  return
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction VTK_VAR_XML_SCAL_3DA_I1_WRITE

  function VTK_VAR_XML_VECT_1DA_R8_WRITE(NC_NN,varname,varX,varY,varZ,cf) result(E_IO)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Function for saving field of vectorial variable (R8P, 1D arrays).
  !---------------------------------------------------------------------------------------------------------------------------------
  implicit none
  integer(I4P), intent(IN)::           NC_NN    !< Number of cells or nodes.
  character(*), intent(IN)::           varname  !< Variable name.
  real(R8P),    intent(IN)::           varX(1:) !< X component [1:NC_NN].
  real(R8P),    intent(IN)::           varY(1:) !< Y component [1:NC_NN].
  real(R8P),    intent(IN)::           varZ(1:) !< Z component [1:NC_NN].
  integer(I4P), intent(IN), optional:: cf       !< Current file index (for concurrent files IO).
  integer(I4P)::                       E_IO     !< Input/Output inquiring flag: $0$ if IO is done, $> 0$ if IO is not done.
  character(len=maxlen)::              s_buffer !< Buffer string.
  real(R8P),    allocatable::          var(:)   !< X, Y, Z component.
  integer(I1P), allocatable::          varp(:)  !< Packed data.
  character(len=:), allocatable::      var64    !< Variable encoded in base64.
  integer(I4P)::                       rf       !< Real file index.
  integer(I4P)::                       n1       !< Counter.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  E_IO = -1_I4P
  rf = f
  if (present(cf)) then
    rf = cf ; f = cf
  endif
  select case(vtk(rf)%f)
  case(ascii)
    s_buffer = repeat(' ',vtk(rf)%indent)//'<DataArray type="Float64" Name="'//trim(varname)//&
               '" NumberOfComponents="3" format="ascii">'
    write(vtk(rf)%u,'(A)',iostat=E_IO)trim(s_buffer)
    do n1=1,NC_NN
      write(vtk(rf)%u,'(A)',iostat=E_IO)repeat(' ',vtk(rf)%indent)//str(n=varX(n1))//' '//str(n=varY(n1))//' '//str(n=varZ(n1))
    enddo
    write(vtk(rf)%u,'(A)',iostat=E_IO)repeat(' ',vtk(rf)%indent)//'</DataArray>'
  case(raw,bin_app)
    s_buffer = repeat(' ',vtk(rf)%indent)//'<DataArray type="Float64" Name="'//trim(varname)//&
               '" NumberOfComponents="3" format="appended" offset="'//trim(str(vtk(rf)%ioffset,.true.))//'"/>'
    write(vtk(rf)%u,iostat=E_IO)trim(s_buffer)//end_rec
    call vtk(rf)%byte_update(N_Byte = 3*NC_NN*BYR8P)
    write(vtk(rf)%ua,iostat=E_IO)vtk(rf)%N_Byte,'R8',3*NC_NN
    write(vtk(rf)%ua,iostat=E_IO)(varX(n1),varY(n1),varZ(n1),n1=1,NC_NN)
  case(binary)
    s_buffer = repeat(' ',vtk(rf)%indent)//'<DataArray type="Float64" Name="'//trim(varname)//&
               '" NumberOfComponents="3" format="binary">'
    write(vtk(rf)%u,iostat=E_IO)trim(s_buffer)//end_rec
    allocate(var(1:3*NC_NN))
    do n1=1,NC_NN
      var(1+(n1-1)*3:1+(n1-1)*3+2)=[varX(n1),varY(n1),varZ(n1)]
    enddo
    call pack_data(a1=[int(3*NC_NN*BYR8P,I4P)],a2=var,packed=varp) ; deallocate(var)
    call b64_encode(n=varp,code=var64) ; deallocate(varp)
    write(vtk(rf)%u,iostat=E_IO)repeat(' ',vtk(rf)%indent+2)//var64//end_rec ; deallocate(var64)
    write(vtk(rf)%u,iostat=E_IO)repeat(' ',vtk(rf)%indent)//'</DataArray>'//end_rec
  endselect
  return
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction VTK_VAR_XML_VECT_1DA_R8_WRITE

  function VTK_VAR_XML_VECT_3DA_R8_WRITE(NC_NN,varname,varX,varY,varZ,cf) result(E_IO)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Function for saving field of vectorial variable (R8P, 3D arrays).
  !---------------------------------------------------------------------------------------------------------------------------------
  implicit none
  integer(I4P), intent(IN)::           NC_NN          !< Number of cells or nodes.
  character(*), intent(IN)::           varname        !< Variable name.
  real(R8P),    intent(IN)::           varX(1:,1:,1:) !< X component [1:Nx,1:Ny,1:Nz].
  real(R8P),    intent(IN)::           varY(1:,1:,1:) !< Y component [1:Nx,1:Ny,1:Nz].
  real(R8P),    intent(IN)::           varZ(1:,1:,1:) !< Z component [1:Nx,1:Ny,1:Nz].
  integer(I4P), intent(IN), optional:: cf             !< Current file index (for concurrent files IO).
  integer(I4P)::                       E_IO           !< Input/Output inquiring flag: $0$ if IO is done, $> 0$ if IO is not done.
  character(len=maxlen)::              s_buffer       !< Buffer string.
  real(R8P),    allocatable::          var(:)         !< X, Y, Z component.
  integer(I1P), allocatable::          varp(:)        !< Packed data.
  character(len=:), allocatable::      var64          !< Variable encoded in base64.
  integer(I4P)::                       rf             !< Real file index.
  integer(I4P)::                       nx,ny,nz,n1    !< Counters.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  E_IO = -1_I4P
  rf = f
  if (present(cf)) then
    rf = cf ; f = cf
  endif
  select case(vtk(rf)%f)
  case(ascii)
    s_buffer = repeat(' ',vtk(rf)%indent)//'<DataArray type="Float64" Name="'//trim(varname)//&
               '" NumberOfComponents="3" format="ascii">'
    write(vtk(rf)%u,'(A)',iostat=E_IO)trim(s_buffer)
    do nz=1,size(varX,dim=3) ; do ny=1,size(varX,dim=2) ; do nx=1,size(varX,dim=1)
      write(vtk(rf)%u,'(A)',iostat=E_IO)repeat(' ',vtk(rf)%indent)//&
                                        str(n=varX(nx,ny,nz))//' '//str(n=varY(nx,ny,nz))//' '//str(n=varZ(nx,ny,nz))
    enddo ; enddo ; enddo
    write(vtk(rf)%u,'(A)',iostat=E_IO)repeat(' ',vtk(rf)%indent)//'</DataArray>'
  case(raw,bin_app)
    s_buffer = repeat(' ',vtk(rf)%indent)//'<DataArray type="Float64" Name="'//trim(varname)//&
               '" NumberOfComponents="3" format="appended" offset="'//trim(str(vtk(rf)%ioffset,.true.))//'"/>'
    write(vtk(rf)%u,iostat=E_IO)trim(s_buffer)//end_rec
    call vtk(rf)%byte_update(N_Byte = 3*NC_NN*BYR8P)
    write(vtk(rf)%ua,iostat=E_IO)vtk(rf)%N_Byte,'R8',3*NC_NN
    write(vtk(rf)%ua,iostat=E_IO)(((varX(nx,ny,nz),varY(nx,ny,nz),varZ(nx,ny,nz),&
                                 nx=1,size(varX,dim=1)),ny=1,size(varX,dim=2)),nz=1,size(varX,dim=3))
  case(binary)
    s_buffer = repeat(' ',vtk(rf)%indent)//'<DataArray type="Float64" Name="'//trim(varname)//&
               '" NumberOfComponents="3" format="binary">'
    write(vtk(rf)%u,iostat=E_IO)trim(s_buffer)//end_rec
    allocate(var(1:3*NC_NN))
    n1 = 0_I4P
    do nz=1,size(varX,dim=3) ; do ny=1,size(varX,dim=2) ; do nx=1,size(varX,dim=1)
      n1 = n1 + 1_I4P ; var(1+(n1-1)*3:1+(n1-1)*3+2)=[varX(nx,ny,nz),varY(nx,ny,nz),varZ(nx,ny,nz)]
    enddo ; enddo ; enddo
    call pack_data(a1=[int(3*NC_NN*BYR8P,I4P)],a2=var,packed=varp) ; deallocate(var)
    call b64_encode(n=varp,code=var64) ; deallocate(varp)
    write(vtk(rf)%u,iostat=E_IO)repeat(' ',vtk(rf)%indent+2)//var64//end_rec ; deallocate(var64)
    write(vtk(rf)%u,iostat=E_IO)repeat(' ',vtk(rf)%indent)//'</DataArray>'//end_rec
  endselect
  return
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction VTK_VAR_XML_VECT_3DA_R8_WRITE

  function VTK_VAR_XML_VECT_1DA_R4_WRITE(NC_NN,varname,varX,varY,varZ,cf) result(E_IO)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Function for saving field of vectorial variable (R4P, 1D arrays).
  !---------------------------------------------------------------------------------------------------------------------------------
  implicit none
  integer(I4P), intent(IN)::           NC_NN    !< Number of cells or nodes.
  character(*), intent(IN)::           varname  !< Variable name.
  real(R4P),    intent(IN)::           varX(1:) !< X component [1:NC_NN].
  real(R4P),    intent(IN)::           varY(1:) !< Y component [1:NC_NN].
  real(R4P),    intent(IN)::           varZ(1:) !< Z component [1:NC_NN].
  integer(I4P), intent(IN), optional:: cf       !< Current file index (for concurrent files IO).
  integer(I4P)::                       E_IO     !< Input/Output inquiring flag: $0$ if IO is done, $> 0$ if IO is not done.
  character(len=maxlen)::              s_buffer !< Buffer string.
  real(R4P),    allocatable::          var(:)   !< X, Y, Z component.
  integer(I1P), allocatable::          varp(:)  !< Packed data.
  character(len=:), allocatable::      var64    !< Variable encoded in base64.
  integer(I4P)::                       rf       !< Real file index.
  integer(I4P)::                       n1       !< Counter.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  E_IO = -1_I4P
  rf = f
  if (present(cf)) then
    rf = cf ; f = cf
  endif
  select case(vtk(rf)%f)
  case(ascii)
    s_buffer = repeat(' ',vtk(rf)%indent)//'<DataArray type="Float32" Name="'//trim(varname)//&
               '" NumberOfComponents="3" format="ascii">'
    write(vtk(rf)%u,'(A)',iostat=E_IO)trim(s_buffer)
    do n1=1,NC_NN
      write(vtk(rf)%u,'(A)',iostat=E_IO)repeat(' ',vtk(rf)%indent)//str(n=varX(n1))//' '//str(n=varY(n1))//' '//str(n=varZ(n1))
    enddo
    write(vtk(rf)%u,'(A)',iostat=E_IO)repeat(' ',vtk(rf)%indent)//'</DataArray>'
  case(raw,bin_app)
    s_buffer = repeat(' ',vtk(rf)%indent)//'<DataArray type="Float32" Name="'//trim(varname)//&
               '" NumberOfComponents="3" format="appended" offset="'//trim(str(vtk(rf)%ioffset,.true.))//'"/>'
    write(vtk(rf)%u,iostat=E_IO)trim(s_buffer)//end_rec
    call vtk(rf)%byte_update(N_Byte = 3*NC_NN*BYR4P)
    write(vtk(rf)%ua,iostat=E_IO)vtk(rf)%N_Byte,'R4',3*NC_NN
    write(vtk(rf)%ua,iostat=E_IO)(varX(n1),varY(n1),varZ(n1),n1=1,NC_NN)
  case(binary)
    s_buffer = repeat(' ',vtk(rf)%indent)//'<DataArray type="Float32" Name="'//trim(varname)//&
               '" NumberOfComponents="3" format="binary">'
    write(vtk(rf)%u,iostat=E_IO)trim(s_buffer)//end_rec
    allocate(var(1:3*NC_NN))
    do n1=1,NC_NN
      var(1+(n1-1)*3:1+(n1-1)*3+2)=[varX(n1),varY(n1),varZ(n1)]
    enddo
    call pack_data(a1=[int(3*NC_NN*BYR4P,I4P)],a2=var,packed=varp) ; deallocate(var)
    call b64_encode(n=varp,code=var64) ; deallocate(varp)
    write(vtk(rf)%u,iostat=E_IO)repeat(' ',vtk(rf)%indent+2)//var64//end_rec ; deallocate(var64)
    write(vtk(rf)%u,iostat=E_IO)repeat(' ',vtk(rf)%indent)//'</DataArray>'//end_rec
  endselect
  return
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction VTK_VAR_XML_VECT_1DA_R4_WRITE

  function VTK_VAR_XML_VECT_3DA_R4_WRITE(NC_NN,varname,varX,varY,varZ,cf) result(E_IO)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Function for saving field of vectorial variable (R4P, 3D arrays).
  !---------------------------------------------------------------------------------------------------------------------------------
  implicit none
  integer(I4P), intent(IN)::           NC_NN          !< Number of cells or nodes.
  character(*), intent(IN)::           varname        !< Variable name.
  real(R4P),    intent(IN)::           varX(1:,1:,1:) !< X component [1:Nx,1:Ny,1:Nz].
  real(R4P),    intent(IN)::           varY(1:,1:,1:) !< Y component [1:Nx,1:Ny,1:Nz].
  real(R4P),    intent(IN)::           varZ(1:,1:,1:) !< Z component [1:Nx,1:Ny,1:Nz].
  integer(I4P), intent(IN), optional:: cf             !< Current file index (for concurrent files IO).
  integer(I4P)::                       E_IO           !< Input/Output inquiring flag: $0$ if IO is done, $> 0$ if IO is not done.
  character(len=maxlen)::              s_buffer       !< Buffer string.
  real(R4P),    allocatable::          var(:)         !< X, Y, Z component.
  integer(I1P), allocatable::          varp(:)        !< Packed data.
  character(len=:), allocatable::      var64          !< Variable encoded in base64.
  integer(I4P)::                       rf             !< Real file index.
  integer(I4P)::                       nx,ny,nz,n1    !< Counters.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  E_IO = -1_I4P
  rf = f
  if (present(cf)) then
    rf = cf ; f = cf
  endif
  select case(vtk(rf)%f)
  case(ascii)
    s_buffer = repeat(' ',vtk(rf)%indent)//'<DataArray type="Float32" Name="'//trim(varname)//&
               '" NumberOfComponents="3" format="ascii">'
    write(vtk(rf)%u,'(A)',iostat=E_IO)trim(s_buffer)
    do nz=1,size(varX,dim=3) ; do ny=1,size(varX,dim=2) ; do nx=1,size(varX,dim=1)
      write(vtk(rf)%u,'(A)',iostat=E_IO)repeat(' ',vtk(rf)%indent)//&
                                        str(n=varX(nx,ny,nz))//' '//str(n=varY(nx,ny,nz))//' '//str(n=varZ(nx,ny,nz))
    enddo ; enddo ; enddo
    write(vtk(rf)%u,'(A)',iostat=E_IO)repeat(' ',vtk(rf)%indent)//'</DataArray>'
  case(raw,bin_app)
    s_buffer = repeat(' ',vtk(rf)%indent)//'<DataArray type="Float32" Name="'//trim(varname)//&
               '" NumberOfComponents="3" format="appended" offset="'//trim(str(vtk(rf)%ioffset,.true.))//'"/>'
    write(vtk(rf)%u,iostat=E_IO)trim(s_buffer)//end_rec
    call vtk(rf)%byte_update(N_Byte = 3*NC_NN*BYR4P)
    write(vtk(rf)%ua,iostat=E_IO)vtk(rf)%N_Byte,'R4',3*NC_NN
    write(vtk(rf)%ua,iostat=E_IO)(((varX(nx,ny,nz),varY(nx,ny,nz),varZ(nx,ny,nz),&
                                 nx=1,size(varX,dim=1)),ny=1,size(varX,dim=2)),nz=1,size(varX,dim=3))
  case(binary)
    s_buffer = repeat(' ',vtk(rf)%indent)//'<DataArray type="Float32" Name="'//trim(varname)//&
               '" NumberOfComponents="3" format="binary">'
    write(vtk(rf)%u,iostat=E_IO)trim(s_buffer)//end_rec
    allocate(var(1:3*NC_NN))
    n1 = 0_I4P
    do nz=1,size(varX,dim=3) ; do ny=1,size(varX,dim=2) ; do nx=1,size(varX,dim=1)
      n1 = n1 + 1_I4P ; var(1+(n1-1)*3:1+(n1-1)*3+2)=[varX(nx,ny,nz),varY(nx,ny,nz),varZ(nx,ny,nz)]
    enddo ; enddo ; enddo
    call pack_data(a1=[int(3*NC_NN*BYR4P,I4P)],a2=var,packed=varp) ; deallocate(var)
    call b64_encode(n=varp,code=var64) ; deallocate(varp)
    write(vtk(rf)%u,iostat=E_IO)repeat(' ',vtk(rf)%indent+2)//var64//end_rec ; deallocate(var64)
    write(vtk(rf)%u,iostat=E_IO)repeat(' ',vtk(rf)%indent)//'</DataArray>'//end_rec
  endselect
  return
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction VTK_VAR_XML_VECT_3DA_R4_WRITE

  function VTK_VAR_XML_VECT_1DA_I8_WRITE(NC_NN,varname,varX,varY,varZ,cf) result(E_IO)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Function for saving field of vectorial variable (I8P, 1D arrays).
  !---------------------------------------------------------------------------------------------------------------------------------
  implicit none
  integer(I4P), intent(IN)::           NC_NN    !< Number of cells or nodes.
  character(*), intent(IN)::           varname  !< Variable name.
  integer(I8P), intent(IN)::           varX(1:) !< X component [1:NC_NN].
  integer(I8P), intent(IN)::           varY(1:) !< Y component [1:NC_NN].
  integer(I8P), intent(IN)::           varZ(1:) !< Z component [1:NC_NN].
  integer(I4P), intent(IN), optional:: cf       !< Current file index (for concurrent files IO).
  integer(I4P)::                       E_IO     !< Input/Output inquiring flag: $0$ if IO is done, $> 0$ if IO is not done.
  character(len=maxlen)::              s_buffer !< Buffer string.
  integer(I8P), allocatable::          var(:)   !< X, Y, Z component.
  integer(I1P), allocatable::          varp(:)  !< Packed data.
  character(len=:), allocatable::      var64    !< Variable encoded in base64.
  integer(I4P)::                       rf       !< Real file index.
  integer(I4P)::                       n1       !< Counter.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  E_IO = -1_I4P
  rf = f
  if (present(cf)) then
    rf = cf ; f = cf
  endif
  select case(vtk(rf)%f)
  case(ascii)
    s_buffer = repeat(' ',vtk(rf)%indent)//'<DataArray type="Int64" Name="'//trim(varname)//&
               '" NumberOfComponents="3" format="ascii">'
    write(vtk(rf)%u,'(A)',iostat=E_IO)trim(s_buffer)
    do n1=1,NC_NN
      write(vtk(rf)%u,'(A)',iostat=E_IO)repeat(' ',vtk(rf)%indent)//str(n=varX(n1))//' '//str(n=varY(n1))//' '//str(n=varZ(n1))
    enddo
    write(vtk(rf)%u,'(A)',iostat=E_IO)repeat(' ',vtk(rf)%indent)//'</DataArray>'
  case(raw,bin_app)
    s_buffer = repeat(' ',vtk(rf)%indent)//'<DataArray type="Int64" Name="'//trim(varname)//&
               '" NumberOfComponents="3" format="appended" offset="'//trim(str(vtk(rf)%ioffset,.true.))//'"/>'
    write(vtk(rf)%u,iostat=E_IO)trim(s_buffer)//end_rec
    call vtk(rf)%byte_update(N_Byte = int(3*NC_NN*BYI8P,I4P))
    write(vtk(rf)%ua,iostat=E_IO)vtk(rf)%N_Byte,'I8',3*NC_NN
    write(vtk(rf)%ua,iostat=E_IO)(varX(n1),varY(n1),varZ(n1),n1=1,NC_NN)
  case(binary)
    s_buffer = repeat(' ',vtk(rf)%indent)//'<DataArray type="Int64" Name="'//trim(varname)//&
               '" NumberOfComponents="3" format="binary">'
    write(vtk(rf)%u,iostat=E_IO)trim(s_buffer)//end_rec
    allocate(var(1:3*NC_NN))
    do n1=1,NC_NN
      var(1+(n1-1)*3:1+(n1-1)*3+2)=[varX(n1),varY(n1),varZ(n1)]
    enddo
    call pack_data(a1=[int(3*NC_NN*BYI8P,I4P)],a2=var,packed=varp) ; deallocate(var)
    call b64_encode(n=varp,code=var64) ; deallocate(varp)
    write(vtk(rf)%u,iostat=E_IO)repeat(' ',vtk(rf)%indent+2)//var64//end_rec ; deallocate(var64)
    write(vtk(rf)%u,iostat=E_IO)repeat(' ',vtk(rf)%indent)//'</DataArray>'//end_rec
  endselect
  return
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction VTK_VAR_XML_VECT_1DA_I8_WRITE

  function VTK_VAR_XML_VECT_3DA_I8_WRITE(NC_NN,varname,varX,varY,varZ,cf) result(E_IO)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Function for saving field of vectorial variable (I8P, 3D arrays).
  !---------------------------------------------------------------------------------------------------------------------------------
  implicit none
  integer(I4P), intent(IN)::           NC_NN          !< Number of cells or nodes.
  character(*), intent(IN)::           varname        !< Variable name.
  integer(I8P), intent(IN)::           varX(1:,1:,1:) !< X component [1:Nx,1:Ny,1:Nz].
  integer(I8P), intent(IN)::           varY(1:,1:,1:) !< Y component [1:Nx,1:Ny,1:Nz].
  integer(I8P), intent(IN)::           varZ(1:,1:,1:) !< Z component [1:Nx,1:Ny,1:Nz].
  integer(I4P), intent(IN), optional:: cf             !< Current file index (for concurrent files IO).
  integer(I4P)::                       E_IO           !< Input/Output inquiring flag: $0$ if IO is done, $> 0$ if IO is not done.
  character(len=maxlen)::              s_buffer       !< Buffer string.
  integer(I8P), allocatable::          var(:)         !< X, Y, Z component.
  integer(I1P), allocatable::          varp(:)        !< Packed data.
  character(len=:), allocatable::      var64          !< Variable encoded in base64.
  integer(I4P)::                       rf             !< Real file index.
  integer(I4P)::                       nx,ny,nz,n1    !< Counters.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  E_IO = -1_I4P
  rf = f
  if (present(cf)) then
    rf = cf ; f = cf
  endif
  select case(vtk(rf)%f)
  case(ascii)
    s_buffer = repeat(' ',vtk(rf)%indent)//'<DataArray type="Int64" Name="'//trim(varname)//&
               '" NumberOfComponents="3" format="ascii">'
    write(vtk(rf)%u,'(A)',iostat=E_IO)trim(s_buffer)
    do nz=1,size(varX,dim=3) ; do ny=1,size(varX,dim=2) ; do nx=1,size(varX,dim=1)
      write(vtk(rf)%u,'(A)',iostat=E_IO)repeat(' ',vtk(rf)%indent)//&
                                        str(n=varX(nx,ny,nz))//' '//str(n=varY(nx,ny,nz))//' '//str(n=varZ(nx,ny,nz))
    enddo ; enddo ; enddo
    write(vtk(rf)%u,'(A)',iostat=E_IO)repeat(' ',vtk(rf)%indent)//'</DataArray>'
  case(raw,bin_app)
    s_buffer = repeat(' ',vtk(rf)%indent)//'<DataArray type="Int64" Name="'//trim(varname)//&
               '" NumberOfComponents="3" format="appended" offset="'//trim(str(vtk(rf)%ioffset,.true.))//'"/>'
    write(vtk(rf)%u,iostat=E_IO)trim(s_buffer)//end_rec
    call vtk(rf)%byte_update(N_Byte = int(3*NC_NN*BYI8P,I4P))
    write(vtk(rf)%ua,iostat=E_IO)vtk(rf)%N_Byte,'I8',3*NC_NN
    write(vtk(rf)%ua,iostat=E_IO)(((varX(nx,ny,nz),varY(nx,ny,nz),varZ(nx,ny,nz),&
                                 nx=1,size(varX,dim=1)),ny=1,size(varX,dim=2)),nz=1,size(varX,dim=3))
  case(binary)
    s_buffer = repeat(' ',vtk(rf)%indent)//'<DataArray type="Int64" Name="'//trim(varname)//&
               '" NumberOfComponents="3" format="binary">'
    write(vtk(rf)%u,iostat=E_IO)trim(s_buffer)//end_rec
    allocate(var(1:3*NC_NN))
    n1 = 0_I4P
    do nz=1,size(varX,dim=3) ; do ny=1,size(varX,dim=2) ; do nx=1,size(varX,dim=1)
      n1 = n1 + 1_I4P ; var(1+(n1-1)*3:1+(n1-1)*3+2)=[varX(nx,ny,nz),varY(nx,ny,nz),varZ(nx,ny,nz)]
    enddo ; enddo ; enddo
    call pack_data(a1=[int(3*NC_NN*BYI8P,I4P)],a2=var,packed=varp) ; deallocate(var)
    call b64_encode(n=varp,code=var64) ; deallocate(varp)
    write(vtk(rf)%u,iostat=E_IO)repeat(' ',vtk(rf)%indent+2)//var64//end_rec ; deallocate(var64)
    write(vtk(rf)%u,iostat=E_IO)repeat(' ',vtk(rf)%indent)//'</DataArray>'//end_rec
  endselect
  return
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction VTK_VAR_XML_VECT_3DA_I8_WRITE

  function VTK_VAR_XML_VECT_1DA_I4_WRITE(NC_NN,varname,varX,varY,varZ,cf) result(E_IO)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Function for saving field of vectorial variable (I4P, 1D arrays).
  !---------------------------------------------------------------------------------------------------------------------------------
  implicit none
  integer(I4P), intent(IN)::           NC_NN    !< Number of cells or nodes.
  character(*), intent(IN)::           varname  !< Variable name.
  integer(I4P), intent(IN)::           varX(1:) !< X component [1:NC_NN].
  integer(I4P), intent(IN)::           varY(1:) !< Y component [1:NC_NN].
  integer(I4P), intent(IN)::           varZ(1:) !< Z component [1:NC_NN].
  integer(I4P), intent(IN), optional:: cf       !< Current file index (for concurrent files IO).
  integer(I4P)::                       E_IO     !< Input/Output inquiring flag: $0$ if IO is done, $> 0$ if IO is not done.
  character(len=maxlen)::              s_buffer !< Buffer string.
  integer(I4P), allocatable::          var(:)   !< X, Y, Z component.
  integer(I1P), allocatable::          varp(:)  !< Packed data.
  character(len=:), allocatable::      var64    !< Variable encoded in base64.
  integer(I4P)::                       rf       !< Real file index.
  integer(I4P)::                       n1       !< Counter.
  integer(I8P)::                       Nvarp    !< Dimension of varp, packed data.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  E_IO = -1_I4P
  rf = f
  if (present(cf)) then
    rf = cf ; f = cf
  endif
  select case(vtk(rf)%f)
  case(ascii)
    s_buffer = repeat(' ',vtk(rf)%indent)//'<DataArray type="Int32" Name="'//trim(varname)//&
               '" NumberOfComponents="3" format="ascii">'
    write(vtk(rf)%u,'(A)',iostat=E_IO)trim(s_buffer)
    do n1=1,NC_NN
      write(vtk(rf)%u,'(A)',iostat=E_IO)repeat(' ',vtk(rf)%indent)//str(n=varX(n1))//' '//str(n=varY(n1))//' '//str(n=varZ(n1))
    enddo
    write(vtk(rf)%u,'(A)',iostat=E_IO)repeat(' ',vtk(rf)%indent)//'</DataArray>'
  case(raw,bin_app)
    s_buffer = repeat(' ',vtk(rf)%indent)//'<DataArray type="Int32" Name="'//trim(varname)//&
               '" NumberOfComponents="3" format="appended" offset="'//trim(str(vtk(rf)%ioffset,.true.))//'"/>'
    write(vtk(rf)%u,iostat=E_IO)trim(s_buffer)//end_rec
    call vtk(rf)%byte_update(N_Byte = 3*NC_NN*BYI4P)
    write(vtk(rf)%ua,iostat=E_IO)vtk(rf)%N_Byte,'I4',3*NC_NN
    write(vtk(rf)%ua,iostat=E_IO)(varX(n1),varY(n1),varZ(n1),n1=1,NC_NN)
  case(binary)
    s_buffer = repeat(' ',vtk(rf)%indent)//'<DataArray type="Int32" Name="'//trim(varname)//&
               '" NumberOfComponents="3" format="binary">'
    write(vtk(rf)%u,iostat=E_IO)trim(s_buffer)//end_rec
    allocate(var(1:3*NC_NN))
    do n1=1,NC_NN
      var(1+(n1-1)*3:1+(n1-1)*3+2)=[varX(n1),varY(n1),varZ(n1)]
    enddo
    Nvarp=size(transfer([int(3*NC_NN*BYI4P,I4P),var],varp),kind=I8P)
    if (allocated(varp)) deallocate(varp); allocate(varp(1:Nvarp))
    varp = transfer([int(3*NC_NN*BYI4P,I4P),var],varp) ; deallocate(var)
    call b64_encode(n=varp,code=var64) ; deallocate(varp)
    write(vtk(rf)%u,iostat=E_IO)repeat(' ',vtk(rf)%indent+2)//var64//end_rec ; deallocate(var64)
    write(vtk(rf)%u,iostat=E_IO)repeat(' ',vtk(rf)%indent)//'</DataArray>'//end_rec
  endselect
  return
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction VTK_VAR_XML_VECT_1DA_I4_WRITE

  function VTK_VAR_XML_VECT_3DA_I4_WRITE(NC_NN,varname,varX,varY,varZ,cf) result(E_IO)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Function for saving field of vectorial variable (I4P, 3D arrays).
  !---------------------------------------------------------------------------------------------------------------------------------
  implicit none
  integer(I4P), intent(IN)::           NC_NN          !< Number of cells or nodes.
  character(*), intent(IN)::           varname        !< Variable name.
  integer(I4P), intent(IN)::           varX(1:,1:,1:) !< X component [1:Nx,1:Ny,1:Nz].
  integer(I4P), intent(IN)::           varY(1:,1:,1:) !< Y component [1:Nx,1:Ny,1:Nz].
  integer(I4P), intent(IN)::           varZ(1:,1:,1:) !< Z component [1:Nx,1:Ny,1:Nz].
  integer(I4P), intent(IN), optional:: cf             !< Current file index (for concurrent files IO).
  integer(I4P)::                       E_IO           !< Input/Output inquiring flag: $0$ if IO is done, $> 0$ if IO is not done.
  character(len=maxlen)::              s_buffer       !< Buffer string.
  integer(I4P), allocatable::          var(:)         !< X, Y, Z component.
  integer(I1P), allocatable::          varp(:)        !< Packed data.
  character(len=:), allocatable::      var64          !< Variable encoded in base64.
  integer(I4P)::                       rf             !< Real file index.
  integer(I4P)::                       nx,ny,nz,n1    !< Counters.
  integer(I8P)::                       Nvarp          !< Dimension of varp, packed data.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  E_IO = -1_I4P
  rf = f
  if (present(cf)) then
    rf = cf ; f = cf
  endif
  select case(vtk(rf)%f)
  case(ascii)
    s_buffer = repeat(' ',vtk(rf)%indent)//'<DataArray type="Int32" Name="'//trim(varname)//&
               '" NumberOfComponents="3" format="ascii">'
    write(vtk(rf)%u,'(A)',iostat=E_IO)trim(s_buffer)
    do nz=1,size(varX,dim=3) ; do ny=1,size(varX,dim=2) ; do nx=1,size(varX,dim=1)
      write(vtk(rf)%u,'(A)',iostat=E_IO)repeat(' ',vtk(rf)%indent)//&
                                        str(n=varX(nx,ny,nz))//' '//str(n=varY(nx,ny,nz))//' '//str(n=varZ(nx,ny,nz))
    enddo ; enddo ; enddo
    write(vtk(rf)%u,'(A)',iostat=E_IO)repeat(' ',vtk(rf)%indent)//'</DataArray>'
  case(raw,bin_app)
    s_buffer = repeat(' ',vtk(rf)%indent)//'<DataArray type="Int32" Name="'//trim(varname)//&
               '" NumberOfComponents="3" format="appended" offset="'//trim(str(vtk(rf)%ioffset,.true.))//'"/>'
    write(vtk(rf)%u,iostat=E_IO)trim(s_buffer)//end_rec
    call vtk(rf)%byte_update(N_Byte = 3*NC_NN*BYI4P)
    write(vtk(rf)%ua,iostat=E_IO)vtk(rf)%N_Byte,'I4',3*NC_NN
    write(vtk(rf)%ua,iostat=E_IO)(((varX(nx,ny,nz),varY(nx,ny,nz),varZ(nx,ny,nz),&
                                 nx=1,size(varX,dim=1)),ny=1,size(varX,dim=2)),nz=1,size(varX,dim=3))
  case(binary)
    s_buffer = repeat(' ',vtk(rf)%indent)//'<DataArray type="Int32" Name="'//trim(varname)//&
               '" NumberOfComponents="3" format="binary">'
    write(vtk(rf)%u,iostat=E_IO)trim(s_buffer)//end_rec
    allocate(var(1:3*NC_NN))
    n1 = 0_I4P
    do nz=1,size(varX,dim=3) ; do ny=1,size(varX,dim=2) ; do nx=1,size(varX,dim=1)
      n1 = n1 + 1_I4P ; var(1+(n1-1)*3:1+(n1-1)*3+2)=[varX(nx,ny,nz),varY(nx,ny,nz),varZ(nx,ny,nz)]
    enddo ; enddo ; enddo
    Nvarp=size(transfer([int(3*NC_NN*BYI4P,I4P),var],varp),kind=I8P)
    if (allocated(varp)) deallocate(varp); allocate(varp(1:Nvarp))
    varp = transfer([int(3*NC_NN*BYI4P,I4P),var],varp) ; deallocate(var)
    call b64_encode(n=varp,code=var64) ; deallocate(varp)
    write(vtk(rf)%u,iostat=E_IO)repeat(' ',vtk(rf)%indent+2)//var64//end_rec ; deallocate(var64)
    write(vtk(rf)%u,iostat=E_IO)repeat(' ',vtk(rf)%indent)//'</DataArray>'//end_rec
  endselect
  return
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction VTK_VAR_XML_VECT_3DA_I4_WRITE

  function VTK_VAR_XML_VECT_1DA_I2_WRITE(NC_NN,varname,varX,varY,varZ,cf) result(E_IO)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Function for saving field of vectorial variable (I2P, 1D arrays).
  !---------------------------------------------------------------------------------------------------------------------------------
  implicit none
  integer(I4P), intent(IN)::           NC_NN    !< Number of cells or nodes.
  character(*), intent(IN)::           varname  !< Variable name.
  integer(I2P), intent(IN)::           varX(1:) !< X component [1:NC_NN].
  integer(I2P), intent(IN)::           varY(1:) !< Y component [1:NC_NN].
  integer(I2P), intent(IN)::           varZ(1:) !< Z component [1:NC_NN].
  integer(I4P), intent(IN), optional:: cf       !< Current file index (for concurrent files IO).
  integer(I4P)::                       E_IO     !< Input/Output inquiring flag: $0$ if IO is done, $> 0$ if IO is not done.
  character(len=maxlen)::              s_buffer !< Buffer string.
  integer(I2P), allocatable::          var(:)   !< X, Y, Z component.
  integer(I1P), allocatable::          varp(:)  !< Packed data.
  character(len=:), allocatable::      var64    !< Variable encoded in base64.
  integer(I4P)::                       rf       !< Real file index.
  integer(I4P)::                       n1       !< Counter.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  E_IO = -1_I4P
  rf = f
  if (present(cf)) then
    rf = cf ; f = cf
  endif
  select case(vtk(rf)%f)
  case(ascii)
    s_buffer = repeat(' ',vtk(rf)%indent)//'<DataArray type="Int16" Name="'//trim(varname)//&
               '" NumberOfComponents="3" format="ascii">'
    write(vtk(rf)%u,'(A)',iostat=E_IO)trim(s_buffer)
    do n1=1,NC_NN
      write(vtk(rf)%u,'(A)',iostat=E_IO)repeat(' ',vtk(rf)%indent)//str(n=varX(n1))//' '//str(n=varY(n1))//' '//str(n=varZ(n1))
    enddo
    write(vtk(rf)%u,'(A)',iostat=E_IO)repeat(' ',vtk(rf)%indent)//'</DataArray>'
  case(raw,bin_app)
    s_buffer = repeat(' ',vtk(rf)%indent)//'<DataArray type="Int16" Name="'//trim(varname)//&
               '" NumberOfComponents="3" format="appended" offset="'//trim(str(vtk(rf)%ioffset,.true.))//'"/>'
    write(vtk(rf)%u,iostat=E_IO)trim(s_buffer)//end_rec
    call vtk(rf)%byte_update(N_Byte = 3*NC_NN*BYI2P)
    write(vtk(rf)%ua,iostat=E_IO)vtk(rf)%N_Byte,'I2',3*NC_NN
    write(vtk(rf)%ua,iostat=E_IO)(varX(n1),varY(n1),varZ(n1),n1=1,NC_NN)
  case(binary)
    s_buffer = repeat(' ',vtk(rf)%indent)//'<DataArray type="Int16" Name="'//trim(varname)//&
               '" NumberOfComponents="3" format="binary">'
    write(vtk(rf)%u,iostat=E_IO)trim(s_buffer)//end_rec
    allocate(var(1:3*NC_NN))
    do n1=1,NC_NN
      var(1+(n1-1)*3:1+(n1-1)*3+2)=[varX(n1),varY(n1),varZ(n1)]
    enddo
    call pack_data(a1=[int(3*NC_NN*BYI2P,I4P)],a2=var,packed=varp) ; deallocate(var)
    call b64_encode(n=varp,code=var64) ; deallocate(varp)
    write(vtk(rf)%u,iostat=E_IO)repeat(' ',vtk(rf)%indent+2)//var64//end_rec ; deallocate(var64)
    write(vtk(rf)%u,iostat=E_IO)repeat(' ',vtk(rf)%indent)//'</DataArray>'//end_rec
  endselect
  return
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction VTK_VAR_XML_VECT_1DA_I2_WRITE

  function VTK_VAR_XML_VECT_3DA_I2_WRITE(NC_NN,varname,varX,varY,varZ,cf) result(E_IO)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Function for saving field of vectorial variable (I2P, 3D arrays).
  !---------------------------------------------------------------------------------------------------------------------------------
  implicit none
  integer(I4P), intent(IN)::           NC_NN          !< Number of cells or nodes.
  character(*), intent(IN)::           varname        !< Variable name.
  integer(I2P), intent(IN)::           varX(1:,1:,1:) !< X component [1:Nx,1:Ny,1:Nz].
  integer(I2P), intent(IN)::           varY(1:,1:,1:) !< Y component [1:Nx,1:Ny,1:Nz].
  integer(I2P), intent(IN)::           varZ(1:,1:,1:) !< Z component [1:Nx,1:Ny,1:Nz].
  integer(I4P), intent(IN), optional:: cf             !< Current file index (for concurrent files IO).
  integer(I4P)::                       E_IO           !< Input/Output inquiring flag: $0$ if IO is done, $> 0$ if IO is not done.
  character(len=maxlen)::              s_buffer       !< Buffer string.
  integer(I2P), allocatable::          var(:)         !< X, Y, Z component.
  integer(I1P), allocatable::          varp(:)        !< Packed data.
  character(len=:), allocatable::      var64          !< Variable encoded in base64.
  integer(I4P)::                       rf             !< Real file index.
  integer(I4P)::                       nx,ny,nz,n1    !< Counters.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  E_IO = -1_I4P
  rf = f
  if (present(cf)) then
    rf = cf ; f = cf
  endif
  select case(vtk(rf)%f)
  case(ascii)
    s_buffer = repeat(' ',vtk(rf)%indent)//'<DataArray type="Int16" Name="'//trim(varname)//&
               '" NumberOfComponents="3" format="ascii">'
    write(vtk(rf)%u,'(A)',iostat=E_IO)trim(s_buffer)
    do nz=1,size(varX,dim=3) ; do ny=1,size(varX,dim=2) ; do nx=1,size(varX,dim=1)
      write(vtk(rf)%u,'(A)',iostat=E_IO)repeat(' ',vtk(rf)%indent)//&
                                        str(n=varX(nx,ny,nz))//' '//str(n=varY(nx,ny,nz))//' '//str(n=varZ(nx,ny,nz))
    enddo ; enddo ; enddo
    write(vtk(rf)%u,'(A)',iostat=E_IO)repeat(' ',vtk(rf)%indent)//'</DataArray>'
  case(raw,bin_app)
    s_buffer = repeat(' ',vtk(rf)%indent)//'<DataArray type="Int16" Name="'//trim(varname)//&
               '" NumberOfComponents="3" format="appended" offset="'//trim(str(vtk(rf)%ioffset,.true.))//'"/>'
    write(vtk(rf)%u,iostat=E_IO)trim(s_buffer)//end_rec
    call vtk(rf)%byte_update(N_Byte = 3*NC_NN*BYI2P)
    write(vtk(rf)%ua,iostat=E_IO)vtk(rf)%N_Byte,'I2',3*NC_NN
    write(vtk(rf)%ua,iostat=E_IO)(((varX(nx,ny,nz),varY(nx,ny,nz),varZ(nx,ny,nz),&
                                 nx=1,size(varX,dim=1)),ny=1,size(varX,dim=2)),nz=1,size(varX,dim=3))
  case(binary)
    s_buffer = repeat(' ',vtk(rf)%indent)//'<DataArray type="Int16" Name="'//trim(varname)//&
               '" NumberOfComponents="3" format="binary">'
    write(vtk(rf)%u,iostat=E_IO)trim(s_buffer)//end_rec
    allocate(var(1:3*NC_NN))
    n1 = 0_I4P
    do nz=1,size(varX,dim=3) ; do ny=1,size(varX,dim=2) ; do nx=1,size(varX,dim=1)
      n1 = n1 + 1_I4P ; var(1+(n1-1)*3:1+(n1-1)*3+2)=[varX(nx,ny,nz),varY(nx,ny,nz),varZ(nx,ny,nz)]
    enddo ; enddo ; enddo
    call pack_data(a1=[int(3*NC_NN*BYI2P,I4P)],a2=var,packed=varp) ; deallocate(var)
    call b64_encode(n=varp,code=var64) ; deallocate(varp)
    write(vtk(rf)%u,iostat=E_IO)repeat(' ',vtk(rf)%indent+2)//var64//end_rec ; deallocate(var64)
    write(vtk(rf)%u,iostat=E_IO)repeat(' ',vtk(rf)%indent)//'</DataArray>'//end_rec
  endselect
  return
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction VTK_VAR_XML_VECT_3DA_I2_WRITE

  function VTK_VAR_XML_VECT_1DA_I1_WRITE(NC_NN,varname,varX,varY,varZ,cf) result(E_IO)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Function for saving field of vectorial variable (I1P, 1D arrays).
  !---------------------------------------------------------------------------------------------------------------------------------
  implicit none
  integer(I4P), intent(IN)::           NC_NN    !< Number of cells or nodes.
  character(*), intent(IN)::           varname  !< Variable name.
  integer(I1P), intent(IN)::           varX(1:) !< X component [1:NC_NN].
  integer(I1P), intent(IN)::           varY(1:) !< Y component [1:NC_NN].
  integer(I1P), intent(IN)::           varZ(1:) !< Z component [1:NC_NN].
  integer(I4P), intent(IN), optional:: cf       !< Current file index (for concurrent files IO).
  integer(I4P)::                       E_IO     !< Input/Output inquiring flag: $0$ if IO is done, $> 0$ if IO is not done.
  character(len=maxlen)::              s_buffer !< Buffer string.
  integer(I1P), allocatable::          var(:)   !< X, Y, Z component.
  integer(I1P), allocatable::          varp(:)  !< Packed data.
  character(len=:), allocatable::      var64    !< Variable encoded in base64.
  integer(I4P)::                       rf       !< Real file index.
  integer(I4P)::                       n1       !< Counter.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  E_IO = -1_I4P
  rf = f
  if (present(cf)) then
    rf = cf ; f = cf
  endif
  select case(vtk(rf)%f)
  case(ascii)
    s_buffer=repeat(' ',vtk(rf)%indent)//'<DataArray type="Int8" Name="'//trim(varname)//'" NumberOfComponents="3" format="ascii">'
    write(vtk(rf)%u,'(A)',iostat=E_IO)trim(s_buffer)
    do n1=1,NC_NN
      write(vtk(rf)%u,'(A)',iostat=E_IO)repeat(' ',vtk(rf)%indent)//str(n=varX(n1))//' '//str(n=varY(n1))//' '//str(n=varZ(n1))
    enddo
    write(vtk(rf)%u,'(A)',iostat=E_IO)repeat(' ',vtk(rf)%indent)//'</DataArray>'
  case(raw,bin_app)
    s_buffer=repeat(' ',vtk(rf)%indent)//'<DataArray type="Int8" Name="'//trim(varname)//&
             '" NumberOfComponents="3" format="appended" offset="'//trim(str(vtk(rf)%ioffset,.true.))//'"/>'
    write(vtk(rf)%u,iostat=E_IO)trim(s_buffer)//end_rec
    vtk(rf)%N_Byte = 3*NC_NN*BYI1P
    call vtk(rf)%byte_update(N_Byte = 3*NC_NN*BYI1P)
    write(vtk(rf)%ua,iostat=E_IO)vtk(rf)%N_Byte,'I1',3*NC_NN
    write(vtk(rf)%ua,iostat=E_IO)(varX(n1),varY(n1),varZ(n1),n1=1,NC_NN)
  case(binary)
    s_buffer = repeat(' ',vtk(rf)%indent)//'<DataArray type="Int8" Name="'//trim(varname)//&
               '" NumberOfComponents="3" format="binary">'
    write(vtk(rf)%u,iostat=E_IO)trim(s_buffer)//end_rec
    allocate(var(1:3*NC_NN))
    do n1=1,NC_NN
      var(1+(n1-1)*3:1+(n1-1)*3+2)=[varX(n1),varY(n1),varZ(n1)]
    enddo
    call pack_data(a1=[int(3*NC_NN*BYI1P,I4P)],a2=var,packed=varp) ; deallocate(var)
    call b64_encode(n=varp,code=var64) ; deallocate(varp)
    write(vtk(rf)%u,iostat=E_IO)repeat(' ',vtk(rf)%indent+2)//var64//end_rec ; deallocate(var64)
    write(vtk(rf)%u,iostat=E_IO)repeat(' ',vtk(rf)%indent)//'</DataArray>'//end_rec
  endselect
  return
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction VTK_VAR_XML_VECT_1DA_I1_WRITE

  function VTK_VAR_XML_VECT_3DA_I1_WRITE(NC_NN,varname,varX,varY,varZ,cf) result(E_IO)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Function for saving field of vectorial variable (I1P, 3D arrays).
  !---------------------------------------------------------------------------------------------------------------------------------
  implicit none
  integer(I4P), intent(IN)::           NC_NN          !< Number of cells or nodes.
  character(*), intent(IN)::           varname        !< Variable name.
  integer(I1P), intent(IN)::           varX(1:,1:,1:) !< X component [1:Nx,1:Ny,1:Nz].
  integer(I1P), intent(IN)::           varY(1:,1:,1:) !< Y component [1:Nx,1:Ny,1:Nz].
  integer(I1P), intent(IN)::           varZ(1:,1:,1:) !< Z component [1:Nx,1:Ny,1:Nz].
  integer(I4P), intent(IN), optional:: cf             !< Current file index (for concurrent files IO).
  integer(I4P)::                       E_IO           !< Input/Output inquiring flag: $0$ if IO is done, $> 0$ if IO is not done.
  character(len=maxlen)::              s_buffer       !< Buffer string.
  integer(I1P), allocatable::          var(:)         !< X, Y, Z component.
  integer(I1P), allocatable::          varp(:)        !< Packed data.
  character(len=:), allocatable::      var64          !< Variable encoded in base64.
  integer(I4P)::                       rf             !< Real file index.
  integer(I4P)::                       nx,ny,nz,n1    !< Counters.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  E_IO = -1_I4P
  rf = f
  if (present(cf)) then
    rf = cf ; f = cf
  endif
  select case(vtk(rf)%f)
  case(ascii)
    s_buffer=repeat(' ',vtk(rf)%indent)//'<DataArray type="Int8" Name="'//trim(varname)//'" NumberOfComponents="3" format="ascii">'
    write(vtk(rf)%u,'(A)',iostat=E_IO)trim(s_buffer)
    do nz=1,size(varX,dim=3) ; do ny=1,size(varX,dim=2) ; do nx=1,size(varX,dim=1)
      write(vtk(rf)%u,'(A)',iostat=E_IO)repeat(' ',vtk(rf)%indent)//&
                                        str(n=varX(nx,ny,nz))//' '//str(n=varY(nx,ny,nz))//' '//str(n=varZ(nx,ny,nz))
    enddo ; enddo ; enddo
    write(vtk(rf)%u,'(A)',iostat=E_IO)repeat(' ',vtk(rf)%indent)//'</DataArray>'
  case(raw,bin_app)
    s_buffer=repeat(' ',vtk(rf)%indent)//'<DataArray type="Int8" Name="'//trim(varname)//&
             '" NumberOfComponents="3" format="appended" offset="'//trim(str(vtk(rf)%ioffset,.true.))//'"/>'
    write(vtk(rf)%u,iostat=E_IO)trim(s_buffer)//end_rec
    vtk(rf)%N_Byte = 3*NC_NN*BYI1P
    call vtk(rf)%byte_update(N_Byte = 3*NC_NN*BYI1P)
    write(vtk(rf)%ua,iostat=E_IO)vtk(rf)%N_Byte,'I1',3*NC_NN
    write(vtk(rf)%ua,iostat=E_IO)(((varX(nx,ny,nz),varY(nx,ny,nz),varZ(nx,ny,nz),&
                                 nx=1,size(varX,dim=1)),ny=1,size(varX,dim=2)),nz=1,size(varX,dim=3))
  case(binary)
    s_buffer = repeat(' ',vtk(rf)%indent)//'<DataArray type="Int8" Name="'//trim(varname)//&
               '" NumberOfComponents="3" format="binary">'
    write(vtk(rf)%u,iostat=E_IO)trim(s_buffer)//end_rec
    allocate(var(1:3*NC_NN))
    n1 = 0_I4P
    do nz=1,size(varX,dim=3) ; do ny=1,size(varX,dim=2) ; do nx=1,size(varX,dim=1)
      n1 = n1 + 1_I4P ; var(1+(n1-1)*3:1+(n1-1)*3+2)=[varX(nx,ny,nz),varY(nx,ny,nz),varZ(nx,ny,nz)]
    enddo ; enddo ; enddo
    call pack_data(a1=[int(3*NC_NN*BYI1P,I4P)],a2=var,packed=varp) ; deallocate(var)
    call b64_encode(n=varp,code=var64) ; deallocate(varp)
    write(vtk(rf)%u,iostat=E_IO)repeat(' ',vtk(rf)%indent+2)//var64//end_rec ; deallocate(var64)
    write(vtk(rf)%u,iostat=E_IO)repeat(' ',vtk(rf)%indent)//'</DataArray>'//end_rec
  endselect
  return
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction VTK_VAR_XML_VECT_3DA_I1_WRITE

  function VTK_VAR_XML_LIST_1DA_R8_WRITE(NC_NN,N_COL,varname,var,cf) result(E_IO)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Function for saving field of list variable (R8P, 1D array).
  !---------------------------------------------------------------------------------------------------------------------------------
  implicit none
  integer(I4P), intent(IN)::           NC_NN      !< Number of cells or nodes.
  integer(I4P), intent(IN)::           N_COL      !< Number of columns.
  character(*), intent(IN)::           varname    !< Variable name.
  real(R8P),    intent(IN)::           var(1:,1:) !< Components [1:N_COL,1:NC_NN].
  integer(I4P), intent(IN), optional:: cf         !< Current file index (for concurrent files IO).
  integer(I4P)::                       E_IO       !< Input/Output inquiring flag: $0$ if IO is done, $> 0$ if IO is not done.
  character(len=maxlen)::              s_buffer   !< Buffer string.
  integer(I1P), allocatable::          varp(:)    !< Packed data.
  character(len=:), allocatable::      var64      !< Variable encoded in base64.
  integer(I4P)::                       rf         !< Real file index.
  integer(I4P)::                       n1,n2      !< Counters.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  E_IO = -1_I4P
  rf = f
  if (present(cf)) then
    rf = cf ; f = cf
  endif
  select case(vtk(rf)%f)
  case(ascii)
    s_buffer = repeat(' ',vtk(rf)%indent)//'<DataArray type="Float64" Name="'//trim(varname)//'" NumberOfComponents="'// &
               trim(str(N_COL,.true.))//'" format="ascii">'
    write(vtk(rf)%u,'(A)',iostat=E_IO)trim(s_buffer)
    do n2=1,NC_NN
      write(vtk(rf)%u,'('//trim(str(N_COL+1,.true.))//'A)',iostat=E_IO)repeat(' ',vtk(rf)%indent),&
                                                                       (' '//str(n=var(n1,n2)),n1=1,N_COL)
    enddo
    write(vtk(rf)%u,'(A)',iostat=E_IO)repeat(' ',vtk(rf)%indent)//'</DataArray>'
  case(raw,bin_app)
    s_buffer = repeat(' ',vtk(rf)%indent)//'<DataArray type="Float64" Name="'//trim(varname)//'" NumberOfComponents="'// &
               trim(str(N_COL,.true.))//'" format="appended" offset="'//trim(str(vtk(rf)%ioffset,.true.))//'"/>'
    write(vtk(rf)%u,iostat=E_IO)trim(s_buffer)//end_rec
    call vtk(rf)%byte_update(N_Byte = N_COL*NC_NN*BYR8P)
    write(vtk(rf)%ua,iostat=E_IO)vtk(rf)%N_Byte,'R8',N_COL*NC_NN
    write(vtk(rf)%ua,iostat=E_IO)var
  case(binary)
    s_buffer = repeat(' ',vtk(rf)%indent)//'<DataArray type="Float64" Name="'//trim(varname)//'" NumberOfComponents="'// &
               trim(str(N_COL,.true.))//'" format="binary">'
    write(vtk(rf)%u,iostat=E_IO)trim(s_buffer)//end_rec
    call pack_data(a1=[int(N_COL*NC_NN*BYR8P,I4P)],a2=reshape(var,[N_COL*NC_NN]),packed=varp)
    call b64_encode(n=varp,code=var64) ; deallocate(varp)
    write(vtk(rf)%u,iostat=E_IO)repeat(' ',vtk(rf)%indent+2)//var64//end_rec ; deallocate(var64)
    write(vtk(rf)%u,iostat=E_IO)repeat(' ',vtk(rf)%indent)//'</DataArray>'//end_rec
  endselect
  return
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction VTK_VAR_XML_LIST_1DA_R8_WRITE

  function VTK_VAR_XML_LIST_3DA_R8_WRITE(NC_NN,N_COL,varname,var,cf) result(E_IO)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Function for saving field of list variable (R8P, 3D array).
  !---------------------------------------------------------------------------------------------------------------------------------
  implicit none
  integer(I4P), intent(IN)::           NC_NN            !< Number of cells or nodes.
  integer(I4P), intent(IN)::           N_COL            !< Number of columns.
  character(*), intent(IN)::           varname          !< Variable name.
  real(R8P),    intent(IN)::           var(1:,1:,1:,1:) !< Components [1:N_COL,1:Nx,1:Ny,1:Nz].
  integer(I4P), intent(IN), optional:: cf               !< Current file index (for concurrent files IO).
  integer(I4P)::                       E_IO             !< Input/Output inquiring flag: $0$ if IO is done, $> 0$ if IO is not done.
  character(len=maxlen)::              s_buffer         !< Buffer string.
  integer(I1P), allocatable::          varp(:)          !< Packed data.
  character(len=:), allocatable::      var64            !< Variable encoded in base64.
  integer(I4P)::                       rf               !< Real file index.
  integer(I4P)::                       nx,ny,nz,n1      !< Counters.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  E_IO = -1_I4P
  rf = f
  if (present(cf)) then
    rf = cf ; f = cf
  endif
  select case(vtk(rf)%f)
  case(ascii)
    s_buffer = repeat(' ',vtk(rf)%indent)//'<DataArray type="Float64" Name="'//trim(varname)//'" NumberOfComponents="'// &
               trim(str(N_COL,.true.))//'" format="ascii">'
    write(vtk(rf)%u,'(A)',iostat=E_IO)trim(s_buffer)
    do nz=1,size(var,dim=4) ; do ny=1,size(var,dim=3) ; do nx=1,size(var,dim=2)
      write(vtk(rf)%u,'('//trim(str(N_COL+1,.true.))//'A)',iostat=E_IO)repeat(' ',vtk(rf)%indent),&
                                                                       (' '//str(n=var(n1,nx,ny,nz)),n1=1,N_COL)
    enddo ; enddo ; enddo
    write(vtk(rf)%u,'(A)',iostat=E_IO)repeat(' ',vtk(rf)%indent)//'</DataArray>'
  case(raw,bin_app)
    s_buffer = repeat(' ',vtk(rf)%indent)//'<DataArray type="Float64" Name="'//trim(varname)//'" NumberOfComponents="'// &
               trim(str(N_COL,.true.))//'" format="appended" offset="'//trim(str(vtk(rf)%ioffset,.true.))//'"/>'
    write(vtk(rf)%u,iostat=E_IO)trim(s_buffer)//end_rec
    call vtk(rf)%byte_update(N_Byte = N_COL*NC_NN*BYR8P)
    write(vtk(rf)%ua,iostat=E_IO)vtk(rf)%N_Byte,'R8',N_COL*NC_NN
    write(vtk(rf)%ua,iostat=E_IO)var
  case(binary)
    s_buffer = repeat(' ',vtk(rf)%indent)//'<DataArray type="Float64" Name="'//trim(varname)//'" NumberOfComponents="'// &
               trim(str(N_COL,.true.))//'" format="binary">'
    write(vtk(rf)%u,iostat=E_IO)trim(s_buffer)//end_rec
    call pack_data(a1=[int(N_COL*NC_NN*BYR8P,I4P)],a2=reshape(var,[N_COL*NC_NN]),packed=varp)
    call b64_encode(n=varp,code=var64) ; deallocate(varp)
    write(vtk(rf)%u,iostat=E_IO)repeat(' ',vtk(rf)%indent+2)//var64//end_rec ; deallocate(var64)
    write(vtk(rf)%u,iostat=E_IO)repeat(' ',vtk(rf)%indent)//'</DataArray>'//end_rec
  endselect
  return
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction VTK_VAR_XML_LIST_3DA_R8_WRITE

  function VTK_VAR_XML_LIST_1DA_R4_WRITE(NC_NN,N_COL,varname,var,cf) result(E_IO)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Function for saving field of list variable (R4P, 1D array).
  !---------------------------------------------------------------------------------------------------------------------------------
  implicit none
  integer(I4P), intent(IN)::           NC_NN      !< Number of cells or nodes.
  integer(I4P), intent(IN)::           N_COL      !< Number of columns.
  character(*), intent(IN)::           varname    !< Variable name.
  real(R4P),    intent(IN)::           var(1:,1:) !< Components [1:N_COL,1:NC_NN].
  integer(I4P), intent(IN), optional:: cf         !< Current file index (for concurrent files IO).
  integer(I4P)::                       E_IO       !< Input/Output inquiring flag: $0$ if IO is done, $> 0$ if IO is not done.
  character(len=maxlen)::              s_buffer   !< Buffer string.
  integer(I1P), allocatable::          varp(:)    !< Packed data.
  character(len=:), allocatable::      var64      !< Variable encoded in base64.
  integer(I4P)::                       rf         !< Real file index.
  integer(I4P)::                       n1,n2      !< Counters.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  E_IO = -1_I4P
  rf = f
  if (present(cf)) then
    rf = cf ; f = cf
  endif
  select case(vtk(rf)%f)
  case(ascii)
    s_buffer = repeat(' ',vtk(rf)%indent)//'<DataArray type="Float32" Name="'//trim(varname)//'" NumberOfComponents="'// &
               trim(str(N_COL,.true.))//'" format="ascii">'
    write(vtk(rf)%u,'(A)',iostat=E_IO)trim(s_buffer)
    do n2=1,NC_NN
      write(vtk(rf)%u,'('//trim(str(N_COL+1,.true.))//'A)',iostat=E_IO)repeat(' ',vtk(rf)%indent),&
                                                                       (' '//str(n=var(n1,n2)),n1=1,N_COL)
    enddo
    write(vtk(rf)%u,'(A)',iostat=E_IO)repeat(' ',vtk(rf)%indent)//'</DataArray>'
  case(raw,bin_app)
    s_buffer = repeat(' ',vtk(rf)%indent)//'<DataArray type="Float32" Name="'//trim(varname)//'" NumberOfComponents="'// &
               trim(str(N_COL,.true.))//'" format="appended" offset="'//trim(str(vtk(rf)%ioffset,.true.))//'"/>'
    write(vtk(rf)%u,iostat=E_IO)trim(s_buffer)//end_rec
    call vtk(rf)%byte_update(N_Byte = N_COL*NC_NN*BYR4P)
    write(vtk(rf)%ua,iostat=E_IO)vtk(rf)%N_Byte,'R4',N_COL*NC_NN
    write(vtk(rf)%ua,iostat=E_IO)var
  case(binary)
    s_buffer = repeat(' ',vtk(rf)%indent)//'<DataArray type="Float32" Name="'//trim(varname)//'" NumberOfComponents="'// &
               trim(str(N_COL,.true.))//'" format="binary">'
    write(vtk(rf)%u,iostat=E_IO)trim(s_buffer)//end_rec
    call pack_data(a1=[int(N_COL*NC_NN*BYR4P,I4P)],a2=reshape(var,[N_COL*NC_NN]),packed=varp)
    call b64_encode(n=varp,code=var64) ; deallocate(varp)
    write(vtk(rf)%u,iostat=E_IO)repeat(' ',vtk(rf)%indent+2)//var64//end_rec ; deallocate(var64)
    write(vtk(rf)%u,iostat=E_IO)repeat(' ',vtk(rf)%indent)//'</DataArray>'//end_rec
  endselect
  return
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction VTK_VAR_XML_LIST_1DA_R4_WRITE

  function VTK_VAR_XML_LIST_3DA_R4_WRITE(NC_NN,N_COL,varname,var,cf) result(E_IO)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Function for saving field of list variable (R4P, 3D array).
  !---------------------------------------------------------------------------------------------------------------------------------
  implicit none
  integer(I4P), intent(IN)::           NC_NN            !< Number of cells or nodes.
  integer(I4P), intent(IN)::           N_COL            !< Number of columns.
  character(*), intent(IN)::           varname          !< Variable name.
  real(R4P),    intent(IN)::           var(1:,1:,1:,1:) !< Components [1:N_COL,1:Nx,1:Ny,1:Nz].
  integer(I4P), intent(IN), optional:: cf               !< Current file index (for concurrent files IO).
  integer(I4P)::                       E_IO             !< Input/Output inquiring flag: $0$ if IO is done, $> 0$ if IO is not done.
  character(len=maxlen)::              s_buffer         !< Buffer string.
  integer(I1P), allocatable::          varp(:)          !< Packed data.
  character(len=:), allocatable::      var64            !< Variable encoded in base64.
  integer(I4P)::                       rf               !< Real file index.
  integer(I4P)::                       nx,ny,nz,n1      !< Counters.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  E_IO = -1_I4P
  rf = f
  if (present(cf)) then
    rf = cf ; f = cf
  endif
  select case(vtk(rf)%f)
  case(ascii)
    s_buffer = repeat(' ',vtk(rf)%indent)//'<DataArray type="Float32" Name="'//trim(varname)//'" NumberOfComponents="'// &
               trim(str(N_COL,.true.))//'" format="ascii">'
    write(vtk(rf)%u,'(A)',iostat=E_IO)trim(s_buffer)
    do nz=1,size(var,dim=4) ; do ny=1,size(var,dim=3) ; do nx=1,size(var,dim=2)
      write(vtk(rf)%u,'('//trim(str(N_COL+1,.true.))//'A)',iostat=E_IO)repeat(' ',vtk(rf)%indent),&
                                                                       (' '//str(n=var(n1,nx,ny,nz)),n1=1,N_COL)
    enddo ; enddo ; enddo
    write(vtk(rf)%u,'(A)',iostat=E_IO)repeat(' ',vtk(rf)%indent)//'</DataArray>'
  case(raw,bin_app)
    s_buffer = repeat(' ',vtk(rf)%indent)//'<DataArray type="Float32" Name="'//trim(varname)//'" NumberOfComponents="'// &
               trim(str(N_COL,.true.))//'" format="appended" offset="'//trim(str(vtk(rf)%ioffset,.true.))//'"/>'
    write(vtk(rf)%u,iostat=E_IO)trim(s_buffer)//end_rec
    call vtk(rf)%byte_update(N_Byte = N_COL*NC_NN*BYR4P)
    write(vtk(rf)%ua,iostat=E_IO)vtk(rf)%N_Byte,'R4',N_COL*NC_NN
    write(vtk(rf)%ua,iostat=E_IO)var
  case(binary)
    s_buffer = repeat(' ',vtk(rf)%indent)//'<DataArray type="Float32" Name="'//trim(varname)//'" NumberOfComponents="'// &
               trim(str(N_COL,.true.))//'" format="binary">'
    write(vtk(rf)%u,iostat=E_IO)trim(s_buffer)//end_rec
    call pack_data(a1=[int(N_COL*NC_NN*BYR4P,I4P)],a2=reshape(var,[N_COL*NC_NN]),packed=varp)
    call b64_encode(n=varp,code=var64) ; deallocate(varp)
    write(vtk(rf)%u,iostat=E_IO)repeat(' ',vtk(rf)%indent+2)//var64//end_rec ; deallocate(var64)
    write(vtk(rf)%u,iostat=E_IO)repeat(' ',vtk(rf)%indent)//'</DataArray>'//end_rec
  endselect
  return
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction VTK_VAR_XML_LIST_3DA_R4_WRITE

  function VTK_VAR_XML_LIST_1DA_I8_WRITE(NC_NN,N_COL,varname,var,cf) result(E_IO)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Function for saving field of list variable (I8P, 1D array).
  !---------------------------------------------------------------------------------------------------------------------------------
  implicit none
  integer(I4P), intent(IN)::           NC_NN      !< Number of cells or nodes.
  integer(I4P), intent(IN)::           N_COL      !< Number of columns.
  character(*), intent(IN)::           varname    !< Variable name.
  integer(I8P), intent(IN)::           var(1:,1:) !< Components [1:N_COL,1:NC_NN].
  integer(I4P), intent(IN), optional:: cf         !< Current file index (for concurrent files IO).
  integer(I4P)::                       E_IO       !< Input/Output inquiring flag: $0$ if IO is done, $> 0$ if IO is not done.
  character(len=maxlen)::              s_buffer   !< Buffer string.
  integer(I1P), allocatable::          varp(:)    !< Packed data.
  character(len=:), allocatable::      var64      !< Variable encoded in base64.
  integer(I4P)::                       rf         !< Real file index.
  integer(I4P)::                       n1,n2      !< Counters.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  E_IO = -1_I4P
  rf = f
  if (present(cf)) then
    rf = cf ; f = cf
  endif
  select case(vtk(rf)%f)
  case(ascii)
    s_buffer = repeat(' ',vtk(rf)%indent)//'<DataArray type="Int64" Name="'//trim(varname)//'" NumberOfComponents="'// &
               trim(str(N_COL,.true.))//'" format="ascii">'
    write(vtk(rf)%u,'(A)',iostat=E_IO)trim(s_buffer)
    do n2=1,NC_NN
      write(vtk(rf)%u,'('//trim(str(N_COL+1,.true.))//'A)',iostat=E_IO)repeat(' ',vtk(rf)%indent),&
                                                                       (' '//str(n=var(n1,n2)),n1=1,N_COL)
    enddo
    write(vtk(rf)%u,'(A)',iostat=E_IO)repeat(' ',vtk(rf)%indent)//'</DataArray>'
  case(raw,bin_app)
    s_buffer = repeat(' ',vtk(rf)%indent)//'<DataArray type="Int64" Name="'//trim(varname)//'" NumberOfComponents="'// &
               trim(str(N_COL,.true.))//'" format="appended" offset="'//trim(str(vtk(rf)%ioffset,.true.))//'"/>'
    write(vtk(rf)%u,iostat=E_IO)trim(s_buffer)//end_rec
    call vtk(rf)%byte_update(N_Byte = int(N_COL*NC_NN*BYI8P,I4P))
    write(vtk(rf)%ua,iostat=E_IO)vtk(rf)%N_Byte,'I8',N_COL*NC_NN
    write(vtk(rf)%ua,iostat=E_IO)var
  case(binary)
    s_buffer = repeat(' ',vtk(rf)%indent)//'<DataArray type="Int64" Name="'//trim(varname)//'" NumberOfComponents="'// &
               trim(str(N_COL,.true.))//'" format="binary">'
    write(vtk(rf)%u,iostat=E_IO)trim(s_buffer)//end_rec
    call pack_data(a1=[int(N_COL*NC_NN*BYI8P,I4P)],a2=reshape(var,[N_COL*NC_NN]),packed=varp)
    call b64_encode(n=varp,code=var64) ; deallocate(varp)
    write(vtk(rf)%u,iostat=E_IO)repeat(' ',vtk(rf)%indent+2)//var64//end_rec ; deallocate(var64)
    write(vtk(rf)%u,iostat=E_IO)repeat(' ',vtk(rf)%indent)//'</DataArray>'//end_rec
  endselect
  return
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction VTK_VAR_XML_LIST_1DA_I8_WRITE

  function VTK_VAR_XML_LIST_3DA_I8_WRITE(NC_NN,N_COL,varname,var,cf) result(E_IO)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Function for saving field of list variable (I8P, 3D array).
  !---------------------------------------------------------------------------------------------------------------------------------
  implicit none
  integer(I4P), intent(IN)::           NC_NN            !< Number of cells or nodes.
  integer(I4P), intent(IN)::           N_COL            !< Number of columns.
  character(*), intent(IN)::           varname          !< Variable name.
  integer(I8P), intent(IN)::           var(1:,1:,1:,1:) !< Components [1:N_COL,1:Nx,1:Ny,1:Nz].
  integer(I4P), intent(IN), optional:: cf               !< Current file index (for concurrent files IO).
  integer(I4P)::                       E_IO             !< Input/Output inquiring flag: $0$ if IO is done, $> 0$ if IO is not done.
  character(len=maxlen)::              s_buffer         !< Buffer string.
  integer(I1P), allocatable::          varp(:)          !< Packed data.
  character(len=:), allocatable::      var64            !< Variable encoded in base64.
  integer(I4P)::                       rf               !< Real file index.
  integer(I4P)::                       nx,ny,nz,n1      !< Counters.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  E_IO = -1_I4P
  rf = f
  if (present(cf)) then
    rf = cf ; f = cf
  endif
  select case(vtk(rf)%f)
  case(ascii)
    s_buffer = repeat(' ',vtk(rf)%indent)//'<DataArray type="Int64" Name="'//trim(varname)//'" NumberOfComponents="'// &
               trim(str(N_COL,.true.))//'" format="ascii">'
    write(vtk(rf)%u,'(A)',iostat=E_IO)trim(s_buffer)
    do nz=1,size(var,dim=4) ; do ny=1,size(var,dim=3) ; do nx=1,size(var,dim=2)
      write(vtk(rf)%u,'('//trim(str(N_COL+1,.true.))//'A)',iostat=E_IO)repeat(' ',vtk(rf)%indent),&
                                                                       (' '//str(n=var(n1,nx,ny,nz)),n1=1,N_COL)
    enddo ; enddo ; enddo
    write(vtk(rf)%u,'(A)',iostat=E_IO)repeat(' ',vtk(rf)%indent)//'</DataArray>'
  case(raw,bin_app)
    s_buffer = repeat(' ',vtk(rf)%indent)//'<DataArray type="Int64" Name="'//trim(varname)//'" NumberOfComponents="'// &
               trim(str(N_COL,.true.))//'" format="appended" offset="'//trim(str(vtk(rf)%ioffset,.true.))//'"/>'
    write(vtk(rf)%u,iostat=E_IO)trim(s_buffer)//end_rec
    call vtk(rf)%byte_update(N_Byte = int(N_COL*NC_NN*BYI8P,I4P))
    write(vtk(rf)%ua,iostat=E_IO)vtk(rf)%N_Byte,'I8',N_COL*NC_NN
    write(vtk(rf)%ua,iostat=E_IO)var
  case(binary)
    s_buffer = repeat(' ',vtk(rf)%indent)//'<DataArray type="Int64" Name="'//trim(varname)//'" NumberOfComponents="'// &
               trim(str(N_COL,.true.))//'" format="binary">'
    write(vtk(rf)%u,iostat=E_IO)trim(s_buffer)//end_rec
    call pack_data(a1=[int(N_COL*NC_NN*BYI8P,I4P)],a2=reshape(var,[N_COL*NC_NN]),packed=varp)
    call b64_encode(n=varp,code=var64) ; deallocate(varp)
    write(vtk(rf)%u,iostat=E_IO)repeat(' ',vtk(rf)%indent+2)//var64//end_rec ; deallocate(var64)
    write(vtk(rf)%u,iostat=E_IO)repeat(' ',vtk(rf)%indent)//'</DataArray>'//end_rec
  endselect
  return
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction VTK_VAR_XML_LIST_3DA_I8_WRITE

  function VTK_VAR_XML_LIST_1DA_I4_WRITE(NC_NN,N_COL,varname,var,cf) result(E_IO)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Function for saving field of list variable (I4P, 1D array).
  !---------------------------------------------------------------------------------------------------------------------------------
  implicit none
  integer(I4P), intent(IN)::           NC_NN      !< Number of cells or nodes.
  integer(I4P), intent(IN)::           N_COL      !< Number of columns.
  character(*), intent(IN)::           varname    !< Variable name.
  integer(I4P), intent(IN)::           var(1:,1:) !< Components [1:N_COL,1:NC_NN].
  integer(I4P), intent(IN), optional:: cf         !< Current file index (for concurrent files IO).
  integer(I4P)::                       E_IO       !< Input/Output inquiring flag: $0$ if IO is done, $> 0$ if IO is not done.
  character(len=maxlen)::              s_buffer   !< Buffer string.
  integer(I1P), allocatable::          varp(:)    !< Packed data.
  character(len=:), allocatable::      var64      !< Variable encoded in base64.
  integer(I4P)::                       rf         !< Real file index.
  integer(I4P)::                       n1,n2      !< Counters.
  integer(I8P)::                       Nvarp      !< Dimension of varp, packed data.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  E_IO = -1_I4P
  rf = f
  if (present(cf)) then
    rf = cf ; f = cf
  endif
  select case(vtk(rf)%f)
  case(ascii)
    s_buffer = repeat(' ',vtk(rf)%indent)//'<DataArray type="Int32" Name="'//trim(varname)//'" NumberOfComponents="'// &
               trim(str(N_COL,.true.))//'" format="ascii">'
    write(vtk(rf)%u,'(A)',iostat=E_IO)trim(s_buffer)
    do n2=1,NC_NN
      write(vtk(rf)%u,'('//trim(str(N_COL+1,.true.))//'A)',iostat=E_IO)repeat(' ',vtk(rf)%indent),&
                                                                       (' '//str(n=var(n1,n2)),n1=1,N_COL)
    enddo
    write(vtk(rf)%u,'(A)',iostat=E_IO)repeat(' ',vtk(rf)%indent)//'</DataArray>'
  case(raw,bin_app)
    s_buffer = repeat(' ',vtk(rf)%indent)//'<DataArray type="Int32" Name="'//trim(varname)//'" NumberOfComponents="'// &
               trim(str(N_COL,.true.))//'" format="appended" offset="'//trim(str(vtk(rf)%ioffset,.true.))//'"/>'
    write(vtk(rf)%u,iostat=E_IO)trim(s_buffer)//end_rec
    call vtk(rf)%byte_update(N_Byte = N_COL*NC_NN*BYI4P)
    write(vtk(rf)%ua,iostat=E_IO)vtk(rf)%N_Byte,'I4',N_COL*NC_NN
    write(vtk(rf)%ua,iostat=E_IO)var
  case(binary)
    s_buffer = repeat(' ',vtk(rf)%indent)//'<DataArray type="Int32" Name="'//trim(varname)//'" NumberOfComponents="'// &
               trim(str(N_COL,.true.))//'" format="binary">'
    write(vtk(rf)%u,iostat=E_IO)trim(s_buffer)//end_rec
    Nvarp=size(transfer([int(N_COL*NC_NN*BYI4P,I4P),reshape(var,[N_COL*NC_NN])],varp),kind=I8P)
    if (allocated(varp)) deallocate(varp); allocate(varp(1:Nvarp))
    varp = transfer([int(N_COL*NC_NN*BYI4P,I4P),reshape(var,[N_COL*NC_NN])],varp)
    call b64_encode(n=varp,code=var64) ; deallocate(varp)
    write(vtk(rf)%u,iostat=E_IO)repeat(' ',vtk(rf)%indent+2)//var64//end_rec ; deallocate(var64)
    write(vtk(rf)%u,iostat=E_IO)repeat(' ',vtk(rf)%indent)//'</DataArray>'//end_rec
  endselect
  return
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction VTK_VAR_XML_LIST_1DA_I4_WRITE

  function VTK_VAR_XML_LIST_3DA_I4_WRITE(NC_NN,N_COL,varname,var,cf) result(E_IO)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Function for saving field of list variable (I4P, 3D array).
  !---------------------------------------------------------------------------------------------------------------------------------
  implicit none
  integer(I4P), intent(IN)::           NC_NN            !< Number of cells or nodes.
  integer(I4P), intent(IN)::           N_COL            !< Number of columns.
  character(*), intent(IN)::           varname          !< Variable name.
  integer(I4P), intent(IN)::           var(1:,1:,1:,1:) !< Components [1:N_COL,1:Nx,1:Ny,1:Nz].
  integer(I4P), intent(IN), optional:: cf               !< Current file index (for concurrent files IO).
  integer(I4P)::                       E_IO             !< Input/Output inquiring flag: $0$ if IO is done, $> 0$ if IO is not done.
  character(len=maxlen)::              s_buffer         !< Buffer string.
  integer(I1P), allocatable::          varp(:)          !< Packed data.
  character(len=:), allocatable::      var64            !< Variable encoded in base64.
  integer(I4P)::                       rf               !< Real file index.
  integer(I4P)::                       nx,ny,nz,n1      !< Counters.
  integer(I8P)::                       Nvarp            !< Dimension of varp, packed data.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  E_IO = -1_I4P
  rf = f
  if (present(cf)) then
    rf = cf ; f = cf
  endif
  select case(vtk(rf)%f)
  case(ascii)
    s_buffer = repeat(' ',vtk(rf)%indent)//'<DataArray type="Int32" Name="'//trim(varname)//'" NumberOfComponents="'// &
               trim(str(N_COL,.true.))//'" format="ascii">'
    write(vtk(rf)%u,'(A)',iostat=E_IO)trim(s_buffer)
    do nz=1,size(var,dim=4) ; do ny=1,size(var,dim=3) ; do nx=1,size(var,dim=2)
      write(vtk(rf)%u,'('//trim(str(N_COL+1,.true.))//'A)',iostat=E_IO)repeat(' ',vtk(rf)%indent),&
                                                                       (' '//str(n=var(n1,nx,ny,nz)),n1=1,N_COL)
    enddo ; enddo ; enddo
    write(vtk(rf)%u,'(A)',iostat=E_IO)repeat(' ',vtk(rf)%indent)//'</DataArray>'
  case(raw,bin_app)
    s_buffer = repeat(' ',vtk(rf)%indent)//'<DataArray type="Int32" Name="'//trim(varname)//'" NumberOfComponents="'// &
               trim(str(N_COL,.true.))//'" format="appended" offset="'//trim(str(vtk(rf)%ioffset,.true.))//'"/>'
    write(vtk(rf)%u,iostat=E_IO)trim(s_buffer)//end_rec
    call vtk(rf)%byte_update(N_Byte = N_COL*NC_NN*BYI4P)
    write(vtk(rf)%ua,iostat=E_IO)vtk(rf)%N_Byte,'I4',N_COL*NC_NN
    write(vtk(rf)%ua,iostat=E_IO)var
  case(binary)
    s_buffer = repeat(' ',vtk(rf)%indent)//'<DataArray type="Int32" Name="'//trim(varname)//'" NumberOfComponents="'// &
               trim(str(N_COL,.true.))//'" format="binary">'
    write(vtk(rf)%u,iostat=E_IO)trim(s_buffer)//end_rec
    Nvarp=size(transfer([int(N_COL*NC_NN*BYI4P,I4P),reshape(var,[N_COL*NC_NN])],varp),kind=I8P)
    if (allocated(varp)) deallocate(varp); allocate(varp(1:Nvarp))
    varp = transfer([int(N_COL*NC_NN*BYI4P,I4P),reshape(var,[N_COL*NC_NN])],varp)
    call b64_encode(n=varp,code=var64) ; deallocate(varp)
    write(vtk(rf)%u,iostat=E_IO)repeat(' ',vtk(rf)%indent+2)//var64//end_rec ; deallocate(var64)
    write(vtk(rf)%u,iostat=E_IO)repeat(' ',vtk(rf)%indent)//'</DataArray>'//end_rec
  endselect
  return
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction VTK_VAR_XML_LIST_3DA_I4_WRITE

  function VTK_VAR_XML_LIST_1DA_I2_WRITE(NC_NN,N_COL,varname,var,cf) result(E_IO)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Function for saving field of list variable (I2P, 1D array).
  !---------------------------------------------------------------------------------------------------------------------------------
  implicit none
  integer(I4P), intent(IN)::           NC_NN      !< Number of cells or nodes.
  integer(I4P), intent(IN)::           N_COL      !< Number of columns.
  character(*), intent(IN)::           varname    !< Variable name.
  integer(I2P), intent(IN)::           var(1:,1:) !< Components [1:N_COL,1:NC_NN].
  integer(I4P), intent(IN), optional:: cf         !< Current file index (for concurrent files IO).
  integer(I4P)::                       E_IO       !< Input/Output inquiring flag: $0$ if IO is done, $> 0$ if IO is not done.
  character(len=maxlen)::              s_buffer   !< Buffer string.
  integer(I1P), allocatable::          varp(:)    !< Packed data.
  character(len=:), allocatable::      var64      !< Variable encoded in base64.
  integer(I4P)::                       rf         !< Real file index.
  integer(I4P)::                       n1,n2      !< Counters.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  E_IO = -1_I4P
  rf = f
  if (present(cf)) then
    rf = cf ; f = cf
  endif
  select case(vtk(rf)%f)
  case(ascii)
    s_buffer = repeat(' ',vtk(rf)%indent)//'<DataArray type="Int16" Name="'//trim(varname)//'" NumberOfComponents="'// &
               trim(str(N_COL,.true.))//'" format="ascii">'
    write(vtk(rf)%u,'(A)',iostat=E_IO)trim(s_buffer)
    do n2=1,NC_NN
      write(vtk(rf)%u,'('//trim(str(N_COL+1,.true.))//'A)',iostat=E_IO)repeat(' ',vtk(rf)%indent),&
                                                                       (' '//str(n=var(n1,n2)),n1=1,N_COL)
    enddo
    write(vtk(rf)%u,'(A)',iostat=E_IO)repeat(' ',vtk(rf)%indent)//'</DataArray>'
  case(raw,bin_app)
    s_buffer = repeat(' ',vtk(rf)%indent)//'<DataArray type="Int16" Name="'//trim(varname)//'" NumberOfComponents="'// &
               trim(str(N_COL,.true.))//'" format="appended" offset="'//trim(str(vtk(rf)%ioffset,.true.))//'"/>'
    write(vtk(rf)%u,iostat=E_IO)trim(s_buffer)//end_rec
    call vtk(rf)%byte_update(N_Byte = N_COL*NC_NN*BYI2P)
    write(vtk(rf)%ua,iostat=E_IO)vtk(rf)%N_Byte,'I2',N_COL*NC_NN
    write(vtk(rf)%ua,iostat=E_IO)var
  case(binary)
    s_buffer = repeat(' ',vtk(rf)%indent)//'<DataArray type="Int16" Name="'//trim(varname)//'" NumberOfComponents="'// &
               trim(str(N_COL,.true.))//'" format="binary">'
    write(vtk(rf)%u,iostat=E_IO)trim(s_buffer)//end_rec
    call pack_data(a1=[int(N_COL*NC_NN*BYI2P,I4P)],a2=reshape(var,[N_COL*NC_NN]),packed=varp)
    call b64_encode(n=varp,code=var64) ; deallocate(varp)
    write(vtk(rf)%u,iostat=E_IO)repeat(' ',vtk(rf)%indent+2)//var64//end_rec ; deallocate(var64)
    write(vtk(rf)%u,iostat=E_IO)repeat(' ',vtk(rf)%indent)//'</DataArray>'//end_rec
  endselect
  return
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction VTK_VAR_XML_LIST_1DA_I2_WRITE

  function VTK_VAR_XML_LIST_3DA_I2_WRITE(NC_NN,N_COL,varname,var,cf) result(E_IO)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Function for saving field of list variable (I2P, 3D array).
  !---------------------------------------------------------------------------------------------------------------------------------
  implicit none
  integer(I4P), intent(IN)::           NC_NN            !< Number of cells or nodes.
  integer(I4P), intent(IN)::           N_COL            !< Number of columns.
  character(*), intent(IN)::           varname          !< Variable name.
  integer(I2P), intent(IN)::           var(1:,1:,1:,1:) !< Components [1:N_COL,1:Nx,1:Ny,1:Nz].
  integer(I4P), intent(IN), optional:: cf               !< Current file index (for concurrent files IO).
  integer(I4P)::                       E_IO             !< Input/Output inquiring flag: $0$ if IO is done, $> 0$ if IO is not done.
  character(len=maxlen)::              s_buffer         !< Buffer string.
  integer(I1P), allocatable::          varp(:)          !< Packed data.
  character(len=:), allocatable::      var64            !< Variable encoded in base64.
  integer(I4P)::                       rf               !< Real file index.
  integer(I4P)::                       nx,ny,nz,n1      !< Counters.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  E_IO = -1_I4P
  rf = f
  if (present(cf)) then
    rf = cf ; f = cf
  endif
  select case(vtk(rf)%f)
  case(ascii)
    s_buffer = repeat(' ',vtk(rf)%indent)//'<DataArray type="Int16" Name="'//trim(varname)//'" NumberOfComponents="'// &
               trim(str(N_COL,.true.))//'" format="ascii">'
    write(vtk(rf)%u,'(A)',iostat=E_IO)trim(s_buffer)
    do nz=1,size(var,dim=4) ; do ny=1,size(var,dim=3) ; do nx=1,size(var,dim=2)
      write(vtk(rf)%u,'('//trim(str(N_COL+1,.true.))//'A)',iostat=E_IO)repeat(' ',vtk(rf)%indent),&
                                                                       (' '//str(n=var(n1,nx,ny,nz)),n1=1,N_COL)
    enddo ; enddo ; enddo
    write(vtk(rf)%u,'(A)',iostat=E_IO)repeat(' ',vtk(rf)%indent)//'</DataArray>'
  case(raw,bin_app)
    s_buffer = repeat(' ',vtk(rf)%indent)//'<DataArray type="Int16" Name="'//trim(varname)//'" NumberOfComponents="'// &
               trim(str(N_COL,.true.))//'" format="appended" offset="'//trim(str(vtk(rf)%ioffset,.true.))//'"/>'
    write(vtk(rf)%u,iostat=E_IO)trim(s_buffer)//end_rec
    call vtk(rf)%byte_update(N_Byte = N_COL*NC_NN*BYI2P)
    write(vtk(rf)%ua,iostat=E_IO)vtk(rf)%N_Byte,'I2',N_COL*NC_NN
    write(vtk(rf)%ua,iostat=E_IO)var
  case(binary)
    s_buffer = repeat(' ',vtk(rf)%indent)//'<DataArray type="Int16" Name="'//trim(varname)//'" NumberOfComponents="'// &
               trim(str(N_COL,.true.))//'" format="binary">'
    write(vtk(rf)%u,iostat=E_IO)trim(s_buffer)//end_rec
    call pack_data(a1=[int(N_COL*NC_NN*BYI2P,I4P)],a2=reshape(var,[N_COL*NC_NN]),packed=varp)
    call b64_encode(n=varp,code=var64) ; deallocate(varp)
    write(vtk(rf)%u,iostat=E_IO)repeat(' ',vtk(rf)%indent+2)//var64//end_rec ; deallocate(var64)
    write(vtk(rf)%u,iostat=E_IO)repeat(' ',vtk(rf)%indent)//'</DataArray>'//end_rec
  endselect
  return
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction VTK_VAR_XML_LIST_3DA_I2_WRITE

  function VTK_VAR_XML_LIST_1DA_I1_WRITE(NC_NN,N_COL,varname,var,cf) result(E_IO)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Function for saving field of list variable (I1P, 1D array).
  !---------------------------------------------------------------------------------------------------------------------------------
  implicit none
  integer(I4P), intent(IN)::           NC_NN      !< Number of cells or nodes.
  integer(I4P), intent(IN)::           N_COL      !< Number of columns.
  character(*), intent(IN)::           varname    !< Variable name.
  integer(I1P), intent(IN)::           var(1:,1:) !< Components [1:N_COL,1:NC_NN].
  integer(I4P), intent(IN), optional:: cf         !< Current file index (for concurrent files IO).
  integer(I4P)::                       E_IO       !< Input/Output inquiring flag: $0$ if IO is done, $> 0$ if IO is not done.
  character(len=maxlen)::              s_buffer   !< Buffer string.
  integer(I1P), allocatable::          varp(:)    !< Packed data.
  character(len=:), allocatable::      var64      !< Variable encoded in base64.
  integer(I4P)::                       rf         !< Real file index.
  integer(I4P)::                       n1,n2      !< Counters.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  E_IO = -1_I4P
  rf = f
  if (present(cf)) then
    rf = cf ; f = cf
  endif
  select case(vtk(rf)%f)
  case(ascii)
    s_buffer = repeat(' ',vtk(rf)%indent)//'<DataArray type="Int8" Name="'//trim(varname)//'" NumberOfComponents="'// &
               trim(str(N_COL,.true.))//'" format="ascii">'
    write(vtk(rf)%u,'(A)',iostat=E_IO)trim(s_buffer)
    do n2=1,NC_NN
      write(vtk(rf)%u,'('//trim(str(N_COL+1,.true.))//'A)',iostat=E_IO)repeat(' ',vtk(rf)%indent),&
                                                                       (' '//str(n=var(n1,n2)),n1=1,N_COL)
    enddo
    write(vtk(rf)%u,'(A)',iostat=E_IO)repeat(' ',vtk(rf)%indent)//'</DataArray>'
  case(raw,bin_app)
    s_buffer = repeat(' ',vtk(rf)%indent)//'<DataArray type="Int8" Name="'//trim(varname)//'" NumberOfComponents="'// &
               trim(str(N_COL,.true.))//'" format="appended" offset="'//trim(str(vtk(rf)%ioffset,.true.))//'"/>'
    write(vtk(rf)%u,iostat=E_IO)trim(s_buffer)//end_rec
    call vtk(rf)%byte_update(N_Byte = N_COL*NC_NN*BYI1P)
    write(vtk(rf)%ua,iostat=E_IO)vtk(rf)%N_Byte,'I1',N_COL*NC_NN
    write(vtk(rf)%ua,iostat=E_IO)var
  case(binary)
    s_buffer = repeat(' ',vtk(rf)%indent)//'<DataArray type="Int8" Name="'//trim(varname)//'" NumberOfComponents="'// &
               trim(str(N_COL,.true.))//'" format="binary">'
    write(vtk(rf)%u,iostat=E_IO)trim(s_buffer)//end_rec
    call pack_data(a1=[int(N_COL*NC_NN*BYI1P,I4P)],a2=reshape(var,[N_COL*NC_NN]),packed=varp)
    call b64_encode(n=varp,code=var64) ; deallocate(varp)
    write(vtk(rf)%u,iostat=E_IO)repeat(' ',vtk(rf)%indent+2)//var64//end_rec ; deallocate(var64)
    write(vtk(rf)%u,iostat=E_IO)repeat(' ',vtk(rf)%indent)//'</DataArray>'//end_rec
  endselect
  return
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction VTK_VAR_XML_LIST_1DA_I1_WRITE

  function VTK_VAR_XML_LIST_3DA_I1_WRITE(NC_NN,N_COL,varname,var,cf) result(E_IO)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Function for saving field of list variable (I1P, 3D array).
  !---------------------------------------------------------------------------------------------------------------------------------
  implicit none
  integer(I4P), intent(IN)::           NC_NN            !< Number of cells or nodes.
  integer(I4P), intent(IN)::           N_COL            !< Number of columns.
  character(*), intent(IN)::           varname          !< Variable name.
  integer(I1P), intent(IN)::           var(1:,1:,1:,1:) !< Components [1:N_COL,1:Nx,1:Ny,1:Nz].
  integer(I4P), intent(IN), optional:: cf               !< Current file index (for concurrent files IO).
  integer(I4P)::                       E_IO             !< Input/Output inquiring flag: $0$ if IO is done, $> 0$ if IO is not done.
  character(len=maxlen)::              s_buffer         !< Buffer string.
  integer(I1P), allocatable::          varp(:)          !< Packed data.
  character(len=:), allocatable::      var64            !< Variable encoded in base64.
  integer(I4P)::                       rf               !< Real file index.
  integer(I4P)::                       nx,ny,nz,n1      !< Counters.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  E_IO = -1_I4P
  rf = f
  if (present(cf)) then
    rf = cf ; f = cf
  endif
  select case(vtk(rf)%f)
  case(ascii)
    s_buffer = repeat(' ',vtk(rf)%indent)//'<DataArray type="Int8" Name="'//trim(varname)//'" NumberOfComponents="'// &
               trim(str(N_COL,.true.))//'" format="ascii">'
    write(vtk(rf)%u,'(A)',iostat=E_IO)trim(s_buffer)
    do nz=1,size(var,dim=4) ; do ny=1,size(var,dim=3) ; do nx=1,size(var,dim=2)
      write(vtk(rf)%u,'('//trim(str(N_COL+1,.true.))//'A)',iostat=E_IO)repeat(' ',vtk(rf)%indent),&
                                                                       (' '//str(n=var(n1,nx,ny,nz)),n1=1,N_COL)
    enddo ; enddo ; enddo
    write(vtk(rf)%u,'(A)',iostat=E_IO)repeat(' ',vtk(rf)%indent)//'</DataArray>'
  case(raw,bin_app)
    s_buffer = repeat(' ',vtk(rf)%indent)//'<DataArray type="Int8" Name="'//trim(varname)//'" NumberOfComponents="'// &
               trim(str(N_COL,.true.))//'" format="appended" offset="'//trim(str(vtk(rf)%ioffset,.true.))//'"/>'
    write(vtk(rf)%u,iostat=E_IO)trim(s_buffer)//end_rec
    call vtk(rf)%byte_update(N_Byte = N_COL*NC_NN*BYI1P)
    write(vtk(rf)%ua,iostat=E_IO)vtk(rf)%N_Byte,'I1',N_COL*NC_NN
    write(vtk(rf)%ua,iostat=E_IO)var
  case(binary)
    s_buffer = repeat(' ',vtk(rf)%indent)//'<DataArray type="Int8" Name="'//trim(varname)//'" NumberOfComponents="'// &
               trim(str(N_COL,.true.))//'" format="binary">'
    write(vtk(rf)%u,iostat=E_IO)trim(s_buffer)//end_rec
    call pack_data(a1=[int(N_COL*NC_NN*BYI1P,I4P)],a2=reshape(var,[N_COL*NC_NN]),packed=varp)
    call b64_encode(n=varp,code=var64) ; deallocate(varp)
    write(vtk(rf)%u,iostat=E_IO)repeat(' ',vtk(rf)%indent+2)//var64//end_rec ; deallocate(var64)
    write(vtk(rf)%u,iostat=E_IO)repeat(' ',vtk(rf)%indent)//'</DataArray>'//end_rec
  endselect
  return
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction VTK_VAR_XML_LIST_3DA_I1_WRITE

  ! importers
  function VTK_VAR_XML_HEADER_READ(var_loc,varname,NC_NN,NCOMP,nx1,nx2,ny1,ny2,nz1,nz2,fmt,type,data,offs,npiece,cf)  result(E_IO)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Function for reading field header information
  !---------------------------------------------------------------------------------------------------------------------------------
  implicit none
  character(*),                  intent(IN)  :: var_loc !< location of variables: CELL for cell-centered, NODE for node-centered
  character(*),                  intent(IN)  :: varname      !< variable name
  integer(I4P),                  intent(OUT) :: NC_NN        !< number of cells or nodes
  integer(I4P),                  intent(OUT) :: NCOMP        !< number of components
  integer(I4P), optional,        intent(OUT) :: nx1          !< Initial node of x axis.
  integer(I4P), optional,        intent(OUT) :: nx2          !< Final node of x axis.
  integer(I4P), optional,        intent(OUT) :: ny1          !< Initial node of y axis.
  integer(I4P), optional,        intent(OUT) :: ny2          !< Final node of y axis.
  integer(I4P), optional,        intent(OUT) :: nz1          !< Initial node of z axis.
  integer(I4P), optional,        intent(OUT) :: nz2          !< Final node of z axis.
  character(len=:), allocatable, intent(OUT) :: fmt          !< VTK data format
  character(len=:), allocatable, intent(OUT) :: type         !< VTK data type
  character(len=:), allocatable, intent(OUT), optional :: data !< VTK data type
  integer(I4P), optional,        intent(OUT) :: offs         !< Raw data offset.
  integer(I4P), optional,        intent(IN)  :: npiece       ! Number of the piece to read (by default: 1)
  integer(I4P), optional,        intent(IN)  :: cf           !< Current file index (for concurrent files IO).
  integer(I4P)                               :: E_IO         ! Input/Output inquiring flag: $0$ if IO is done, $> 0$ if IO is not done
  integer(I4P)                               :: rf           !< Real file index.
  character(len=:), allocatable              :: s_buffer     !< Buffer string.
  character(len=:), allocatable              :: aux
  integer(I4P)                               :: np, pos, of
  integer(I4P)                               :: x1,x2,y1,y2,z1,z2
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  E_IO = -1_I4P
  rf = f
  if (present(cf)) then
    rf = cf ; f = cf
  endif
  np = 1_I4P; if (present(npiece)) np = npiece

  ! Read field headers
  rewind(unit=vtk(rf)%u, iostat=E_IO)
  E_IO = move(inside=trim(vtk(rf)%topology), to_find='Piece', repeat=np, buffer=s_buffer)
  inquire(unit=vtk(rf)%u, pos=pos)

  select case(trim(Upper_case(var_loc)))
    case('NODE')
      select case(trim(vtk(rf)%topology))
        case('RectilinearGrid','StructuredGrid')
          call get_char(buffer=s_buffer, attrib='Extent', val=aux, E_IO=E_IO)
          if(E_IO == 0) then
            read(aux,*, iostat=E_IO) x1,x2,y1,y2,z1,z2
            NC_NN = (x2-x1+1)*(y2-y1+1)*(z2-z1+1)
            if(present(nx1)) nx1=x1; if(present(nx2)) nx2=x2
            if(present(ny1)) ny1=y1; if(present(ny2)) ny2=y2
            if(present(nz1)) nz1=z1; if(present(nz2)) nz2=z2
          endif
        case('UnstructuredGrid')
          call get_int(buffer=s_buffer, attrib='NumberOfPoints', val=NC_NN, E_IO=E_IO)
      end select
      E_IO = search(from=pos, inside='PointData', to_find='DataArray', with_attribute='Name', of_value=varname, &
                    buffer=s_buffer, content=aux)
      if(present(data)) data=aux

    case('CELL')
      select case(trim(vtk(rf)%topology))
        case('RectilinearGrid','StructuredGrid')
          call get_char(buffer=s_buffer, attrib='Extent', val=aux, E_IO=E_IO)
          if(E_IO == 0) then
            read(aux,*, iostat=E_IO) x1,x2,y1,y2,z1,z2
            NC_NN = (x2-x1+1)*(y2-y1+1)*(z2-z1+1)
            if(present(nx1)) nx1=x1; if(present(nx2)) nx2=x2
            if(present(ny1)) ny1=y1; if(present(ny2)) ny2=y2
            if(present(nz1)) nz1=z1; if(present(nz2)) nz2=z2
          endif
        case('UnstructuredGrid')
          call get_int(buffer=s_buffer, attrib='NumberOfCells', val=NC_NN, E_IO=E_IO)
      end select
      E_IO = search(from=pos, inside='CellData', to_find='DataArray', with_attribute='Name', of_value=varname, &
                    buffer=s_buffer, content=aux)
      if(present(data)) data=aux
  end select

  if(E_IO == 0) then
    call get_int(buffer=s_buffer, attrib='NumberOfComponents', val=NCOMP, E_IO=E_IO)
    call get_int(buffer=s_buffer, attrib='offset', val=of, E_IO=E_IO)
    if(present(offs)) offs= of
    call get_char(buffer=s_buffer, attrib='format', val=fmt, E_IO=E_IO)
    call get_char(buffer=s_buffer, attrib='type', val=type, E_IO=E_IO)
  endif
  !---------------------------------------------------------------------------------------------------------------------------------
  end function

  function VTK_VAR_XML_SCAL_1DA_R8_READ(var_location,varname,NC_NN,NCOMP,var,npiece,cf) result(E_IO)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Function for reading field of scalar variable (R8P, 1D array).
  !---------------------------------------------------------------------------------------------------------------------------------
  implicit none
  character(*),           intent(IN)  :: var_location !< location of variables: CELL for cell-centered, NODE for node-centered
  character(*),           intent(IN)  :: varname      !< variable name
  integer(I4P),           intent(OUT) :: NC_NN        !< number of cells or nodes
  integer(I4P),           intent(OUT) :: NCOMP        !< number of components
  real(R8P), allocatable, intent(OUT) :: var(:)       !< variable to be saved
  integer(I4P), optional, intent(IN)  :: npiece       !< Number of the piece to read (by default: 1)
  integer(I4P), optional, intent(IN)  :: cf           !< Current file index (for concurrent files IO).
  integer(I4P)                        :: E_IO         !< Input/Output inquiring flag: $0$ if IO is done, $> 0$ if IO is not done
  integer(I4P)                        :: rf           !< Real file index.
  character(len=:), allocatable       :: fmt
  character(len=:), allocatable       :: type
  character(len=:), allocatable       :: data
  integer(I4P)                        :: np, offs, N_Byte, s
  integer(I1P), allocatable           :: dI1P(:)
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  E_IO = -1_I4P
  rf = f
  if (present(cf)) then
    rf = cf ; f = cf
  endif
  np = 1_I4P; if (present(npiece)) np = npiece

  ! Read field headers
  E_IO = VTK_VAR_XML_HEADER_READ(var_location,varname,NC_NN,NCOMP,fmt=fmt,type=type,data=data,offs=offs,npiece=np,cf=rf)

  if(E_IO == 0) then
    allocate(var(NC_NN*NCOMP), stat=E_IO)

    ! Read field data
    select case(vtk(rf)%f)
      case(ascii)
        if (trim(adjustlt(Upper_Case(fmt)))/='ASCII' .or. &
            trim(adjustlt(Upper_Case(type)))/='FLOAT64') then
          E_IO = -1_I4P
        else
          read(data, fmt=*, iostat=E_IO) var
        endif

      case(binary)
        if (trim(adjustlt(Upper_Case(fmt)))/='BINARY' .or. &
            trim(adjustlt(Upper_Case(type)))/='FLOAT64') then
          E_IO = -1_I4P
        else
          ! Decode packed base64 data
          data=trim(adjustlt(data))
          allocate(dI1P(NC_NN*NCOMP*int(BYR8P,I4P)+int(BYI4P,I4P)))
          call b64_decode(code=data,n=dI1P); if(allocated(data)) deallocate(data)
          ! Unpack data [1xI4P,NN_NC*NCOMPxR8P]
          N_byte =  transfer(dI1P(1:int(BYI4P,I4P)),N_byte); s = size(transfer(dI1P(int(BYI4P,I4P)+1:),var))
          if(s /= NC_NN*NCOMP) E_IO = -1_I4P
          var = transfer(dI1P(int(BYI4P,I4P)+1:),var); if(allocated(dI1P)) deallocate(dI1P)
        endif

      case(raw)
        if(E_IO == 0) then
          if (trim(adjustlt(Upper_Case(fmt)))/='APPENDED' .or. &
              trim(adjustlt(Upper_Case(type)))/='FLOAT64') then
            E_IO = -1_I4P
          else
            read(unit=vtk(rf)%u, iostat=E_IO, pos=vtk(rf)%ioffset+offs) N_Byte, var
          endif
      endif

    endselect
  endif
  !---------------------------------------------------------------------------------------------------------------------------------
  end function

  function VTK_VAR_XML_SCAL_1DA_R4_READ(var_location,varname,NC_NN,NCOMP,var,npiece,cf) result(E_IO)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Function for reading field of scalar variable (R4P, 1D array).
  !---------------------------------------------------------------------------------------------------------------------------------
  implicit none
  character(*),           intent(IN)  :: var_location !< location of variables: CELL for cell-centered, NODE for node-centered
  character(*),           intent(IN)  :: varname      !< variable name
  integer(I4P),           intent(OUT) :: NC_NN        !< number of cells or nodes
  integer(I4P),           intent(OUT) :: NCOMP        !< number of components
  real(R4P), allocatable, intent(OUT) :: var(:)       !< variable to be saved
  integer(I4P), optional, intent(IN)  :: npiece       !< Number of the piece to read (by default: 1)
  integer(I4P), optional, intent(IN)  :: cf           !< Current file index (for concurrent files IO).
  integer(I4P)                        :: E_IO         !< Input/Output inquiring flag: $0$ if IO is done, $> 0$ if IO is not done
  integer(I4P)                        :: rf           !< Real file index.
  character(len=:), allocatable       :: fmt
  character(len=:), allocatable       :: type
  character(len=:), allocatable       :: data
  integer(I4P)                        :: np, offs, N_Byte, s
  integer(I1P), allocatable           :: dI1P(:)
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  E_IO = -1_I4P
  rf = f
  if (present(cf)) then
    rf = cf ; f = cf
  endif
  np = 1_I4P; if (present(npiece)) np = npiece

  ! Read field headers
  E_IO = VTK_VAR_XML_HEADER_READ(var_location,varname,NC_NN,NCOMP,fmt=fmt,type=type,data=data,offs=offs,npiece=np,cf=rf)

  if(E_IO == 0) then
    allocate(var(NC_NN*NCOMP), stat=E_IO)

    ! Read field data
    select case(vtk(rf)%f)
      case(ascii)
        if (trim(adjustlt(Upper_Case(fmt)))/='ASCII' .or. &
            trim(adjustlt(Upper_Case(type)))/='FLOAT32') then
          E_IO = -1_I4P
        else
          read(data, fmt=*, iostat=E_IO) var
        endif

      case(binary)
        if (trim(adjustlt(Upper_Case(fmt)))/='BINARY' .or. &
            trim(adjustlt(Upper_Case(type)))/='FLOAT32') then
          E_IO = -1_I4P
        else
          ! Decode packed base64 data
          data=trim(adjustlt(data))
          allocate(dI1P(NC_NN*NCOMP*int(BYR4P,I4P)+int(BYI4P,I4P)))
          call b64_decode(code=data,n=dI1P); if(allocated(data)) deallocate(data)
          ! Unpack data [1xI4P,NN_NC*NCOMPxR8P]
          N_byte =  transfer(dI1P(1:int(BYI4P,I4P)),N_byte); s = size(transfer(dI1P(int(BYI4P,I4P)+1:),var))
          if(s /= NC_NN*NCOMP) E_IO = -1_I4P
          var = transfer(dI1P(int(BYI4P,I4P)+1:),var); if(allocated(dI1P)) deallocate(dI1P)
        endif

      case(raw)
        if(E_IO == 0) then
          if (trim(adjustlt(Upper_Case(fmt)))/='APPENDED' .or. &
              trim(adjustlt(Upper_Case(type)))/='FLOAT32') then
            E_IO = -1_I4P
          else
            read(unit=vtk(rf)%u, iostat=E_IO, pos=vtk(rf)%ioffset+offs) N_Byte, var
          endif
      endif

    endselect
  endif
  !---------------------------------------------------------------------------------------------------------------------------------
  end function

  function VTK_VAR_XML_SCAL_1DA_I8_READ(var_location,varname,NC_NN,NCOMP,var,npiece,cf) result(E_IO)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Function for reading field of scalar variable (I4P, 1D array).
  !---------------------------------------------------------------------------------------------------------------------------------
  implicit none
  character(*),              intent(IN)  :: var_location !< location of variables: CELL for cell-centered, NODE for node-centered
  character(*),              intent(IN)  :: varname      !< variable name
  integer(I4P),              intent(OUT) :: NC_NN        !< number of cells or nodes
  integer(I4P),              intent(OUT) :: NCOMP        !< number of components
  integer(I8P), allocatable, intent(OUT) :: var(:)       !< variable to be saved
  integer(I4P), optional,    intent(IN)  :: npiece       !< Number of the piece to read (by default: 1)
  integer(I4P), optional,    intent(IN)  :: cf           !< Current file index (for concurrent files IO).
  integer(I4P)                           :: E_IO         !< Input/Output inquiring flag: $0$ if IO is done, $> 0$ if IO is not done
  integer(I4P)                           :: rf           !< Real file index.
  character(len=:), allocatable          :: fmt
  character(len=:), allocatable          :: type
  character(len=:), allocatable          :: data
  integer(I4P)                           :: np, offs, N_Byte, s
  integer(I1P), allocatable              :: dI1P(:)
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  E_IO = -1_I4P
  rf = f
  if (present(cf)) then
    rf = cf ; f = cf
  endif
  np = 1_I4P; if (present(npiece)) np = npiece

  ! Read field headers
  E_IO = VTK_VAR_XML_HEADER_READ(var_location,varname,NC_NN,NCOMP,fmt=fmt,type=type,data=data,offs=offs,npiece=np,cf=rf)

  if(E_IO == 0) then
    allocate(var(NC_NN*NCOMP), stat=E_IO)

    ! Read field data
    select case(vtk(rf)%f)
      case(ascii)
        if (trim(adjustlt(Upper_Case(fmt)))/='ASCII' .or. &
            trim(adjustlt(Upper_Case(type)))/='INT64') then
          E_IO = -1_I4P
        else
          read(data, fmt=*, iostat=E_IO) var
        endif

      case(binary)
        if (trim(adjustlt(Upper_Case(fmt)))/='BINARY' .or. &
            trim(adjustlt(Upper_Case(type)))/='INT64') then
          E_IO = -1_I4P
        else
          ! Decode packed base64 data
          data=trim(adjustlt(data))
          allocate(dI1P(NC_NN*NCOMP*int(BYI8P,I4P)+int(BYI4P,I4P)))
          call b64_decode(code=data,n=dI1P); if(allocated(data)) deallocate(data)
          ! Unpack data [1xI4P,NN_NC*NCOMPxI8P]
          N_byte =  transfer(dI1P(1:int(BYI4P,I4P)),N_byte); s = size(transfer(dI1P(int(BYI4P,I4P)+1:),var))
          if(s /= NC_NN*NCOMP) E_IO = -1_I4P
          var = transfer(dI1P(int(BYI4P,I4P)+1:),var); if(allocated(dI1P)) deallocate(dI1P)
        endif

      case(raw)
        if(E_IO == 0) then
          if (trim(adjustlt(Upper_Case(fmt)))/='APPENDED' .or. &
              trim(adjustlt(Upper_Case(type)))/='INT64') then
            E_IO = -1_I4P
          else
            read(unit=vtk(rf)%u, iostat=E_IO, pos=vtk(rf)%ioffset+offs) N_Byte, var
          endif
      endif

    endselect
  endif
  !---------------------------------------------------------------------------------------------------------------------------------
  end function

  function VTK_VAR_XML_SCAL_1DA_I4_READ(var_location,varname,NC_NN,NCOMP,var,npiece,cf) result(E_IO)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Function for reading field of scalar variable (I4P, 1D array).
  !---------------------------------------------------------------------------------------------------------------------------------
  implicit none
  character(*),              intent(IN)  :: var_location !< location of variables: CELL for cell-centered, NODE for node-centered
  character(*),              intent(IN)  :: varname      !< variable name
  integer(I4P),              intent(OUT) :: NC_NN        !< number of cells or nodes
  integer(I4P),              intent(OUT) :: NCOMP        !< number of components
  integer(I4P), allocatable, intent(OUT) :: var(:)       !< variable to be saved
  integer(I4P), optional,    intent(IN)  :: npiece       !< Number of the piece to read (by default: 1)
  integer(I4P), optional,    intent(IN)  :: cf           !< Current file index (for concurrent files IO).
  integer(I4P)                           :: E_IO         !< Input/Output inquiring flag: $0$ if IO is done, $> 0$ if IO is not done
  integer(I4P)                           :: rf           !< Real file index.
  character(len=:), allocatable          :: fmt
  character(len=:), allocatable          :: type
  character(len=:), allocatable          :: data
  integer(I4P)                           :: np, offs, N_Byte, s
  integer(I1P), allocatable              :: dI1P(:)
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  E_IO = -1_I4P
  rf = f
  if (present(cf)) then
    rf = cf ; f = cf
  endif
  np = 1_I4P; if (present(npiece)) np = npiece

  ! Read field headers
  E_IO = VTK_VAR_XML_HEADER_READ(var_location,varname,NC_NN,NCOMP,fmt=fmt,type=type,data=data,offs=offs,npiece=np,cf=rf)

  if(E_IO == 0) then
    allocate(var(NC_NN*NCOMP), stat=E_IO)

    ! Read field data
    select case(vtk(rf)%f)
      case(ascii)
        if (trim(adjustlt(Upper_Case(fmt)))/='ASCII' .or. &
            trim(adjustlt(Upper_Case(type)))/='INT32') then
          E_IO = -1_I4P
        else
          read(data, fmt=*, iostat=E_IO) var
        endif

      case(binary)
        if (trim(adjustlt(Upper_Case(fmt)))/='BINARY' .or. &
            trim(adjustlt(Upper_Case(type)))/='INT32') then
          E_IO = -1_I4P
        else
          ! Decode packed base64 data
          data=trim(adjustlt(data))
          allocate(dI1P(NC_NN*NCOMP*int(BYI4P,I4P)+int(BYI4P,I4P)))
          call b64_decode(code=data,n=dI1P); if(allocated(data)) deallocate(data)
          ! Unpack data [1xI4P,NN_NC*NCOMPxI4P]
          N_byte =  transfer(dI1P(1:int(BYI4P,I4P)),N_byte); s = size(transfer(dI1P(int(BYI4P,I4P)+1:),var))
          if(s /= NC_NN*NCOMP) E_IO = -1_I4P
          var = transfer(dI1P(int(BYI4P,I4P)+1:),var); if(allocated(dI1P)) deallocate(dI1P)
        endif

      case(raw)
        if(E_IO == 0) then
          if (trim(adjustlt(Upper_Case(fmt)))/='APPENDED' .or. &
              trim(adjustlt(Upper_Case(type)))/='INT32') then
            E_IO = -1_I4P
          else
            read(unit=vtk(rf)%u, iostat=E_IO, pos=vtk(rf)%ioffset+offs) N_Byte, var
          endif
      endif

    endselect
  endif
  !---------------------------------------------------------------------------------------------------------------------------------
  end function

  function VTK_VAR_XML_SCAL_1DA_I2_READ(var_location,varname,NC_NN,NCOMP,var,npiece,cf) result(E_IO)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Function for reading field of scalar variable (I2P, 1D array).
  !---------------------------------------------------------------------------------------------------------------------------------
  implicit none
  character(*),              intent(IN)  :: var_location !< location of variables: CELL for cell-centered, NODE for node-centered
  character(*),              intent(IN)  :: varname      !< variable name
  integer(I4P),              intent(OUT) :: NC_NN        !< number of cells or nodes
  integer(I4P),              intent(OUT) :: NCOMP        !< number of components
  integer(I2P), allocatable, intent(OUT) :: var(:)       !< variable to be saved
  integer(I4P), optional,    intent(IN)  :: npiece       !< Number of the piece to read (by default: 1)
  integer(I4P), optional,    intent(IN)  :: cf           !< Current file index (for concurrent files IO).
  integer(I4P)                           :: E_IO         !< Input/Output inquiring flag: $0$ if IO is done, $> 0$ if IO is not done
  integer(I4P)                           :: rf           !< Real file index.
  character(len=:), allocatable          :: fmt
  character(len=:), allocatable          :: type
  character(len=:), allocatable          :: data
  integer(I4P)                           :: np, offs, N_Byte, s
  integer(I1P), allocatable              :: dI1P(:)
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  E_IO = -1_I4P
  rf = f
  if (present(cf)) then
    rf = cf ; f = cf
  endif
  np = 1_I4P; if (present(npiece)) np = npiece

  ! Read field headers
  E_IO = VTK_VAR_XML_HEADER_READ(var_location,varname,NC_NN,NCOMP,fmt=fmt,type=type,data=data,offs=offs,npiece=np,cf=rf)

  if(E_IO == 0) then
    allocate(var(NC_NN*NCOMP), stat=E_IO)

    ! Read field data
    select case(vtk(rf)%f)
      case(ascii)
        if (trim(adjustlt(Upper_Case(fmt)))/='ASCII' .or. &
            trim(adjustlt(Upper_Case(type)))/='INT16') then
          E_IO = -1_I4P
        else
          read(data, fmt=*, iostat=E_IO) var
        endif

      case(binary)
        if (trim(adjustlt(Upper_Case(fmt)))/='BINARY' .or. &
            trim(adjustlt(Upper_Case(type)))/='INT16') then
          E_IO = -1_I4P
        else
          ! Decode packed base64 data
          data=trim(adjustlt(data))
          allocate(dI1P(NC_NN*NCOMP*int(BYI2P,I4P)+int(BYI4P,I4P)))
          call b64_decode(code=data,n=dI1P); if(allocated(data)) deallocate(data)
          ! Unpack data [1xI4P,NN_NC*NCOMPxI2P]
          N_byte =  transfer(dI1P(1:int(BYI4P,I4P)),N_byte); s = size(transfer(dI1P(int(BYI4P,I4P)+1:),var))
          if(s /= NC_NN*NCOMP) E_IO = -1_I4P
          var = transfer(dI1P(int(BYI4P,I4P)+1:),var); if(allocated(dI1P)) deallocate(dI1P)
        endif

      case(raw)
        if(E_IO == 0) then
          if (trim(adjustlt(Upper_Case(fmt)))/='APPENDED' .or. &
              trim(adjustlt(Upper_Case(type)))/='INT16') then
            E_IO = -1_I4P
          else
            read(unit=vtk(rf)%u, iostat=E_IO, pos=vtk(rf)%ioffset+offs) N_Byte, var
          endif
      endif

    endselect
  endif
  !---------------------------------------------------------------------------------------------------------------------------------
  end function

  function VTK_VAR_XML_SCAL_1DA_I1_READ(var_location,varname,NC_NN,NCOMP,var,npiece,cf) result(E_IO)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Function for reading field of scalar variable (I1P, 1D array).
  !---------------------------------------------------------------------------------------------------------------------------------
  implicit none
  character(*),              intent(IN)  :: var_location !< location of variables: CELL for cell-centered, NODE for node-centered
  character(*),              intent(IN)  :: varname      !< variable name
  integer(I4P),              intent(OUT) :: NC_NN        !< number of cells or nodes
  integer(I4P),              intent(OUT) :: NCOMP        !< number of components
  integer(I1P), allocatable, intent(OUT) :: var(:)       !< variable to be saved
  integer(I4P), optional,    intent(IN)  :: npiece       !< Number of the piece to read (by default: 1)
  integer(I4P), optional,    intent(IN)  :: cf           !< Current file index (for concurrent files IO).
  integer(I4P)                           :: E_IO         !< Input/Output inquiring flag: $0$ if IO is done, $> 0$ if IO is not done
  integer(I4P)                           :: rf           !< Real file index.
  character(len=:), allocatable          :: fmt
  character(len=:), allocatable          :: type
  character(len=:), allocatable          :: data
  integer(I4P)                           :: np, offs, N_Byte, s
  integer(I1P), allocatable              :: dI1P(:)
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  E_IO = -1_I4P
  rf = f
  if (present(cf)) then
    rf = cf ; f = cf
  endif
  np = 1_I4P; if (present(npiece)) np = npiece

  ! Read field headers
    E_IO = VTK_VAR_XML_HEADER_READ(var_location,varname,NC_NN,NCOMP,fmt=fmt,type=type,data=data,offs=offs,npiece=np,cf=rf)

  if(E_IO == 0) then
    allocate(var(NC_NN*NCOMP), stat=E_IO)

    ! Read field data
    select case(vtk(rf)%f)
      case(ascii)
        if (trim(adjustlt(Upper_Case(fmt)))/='ASCII' .or. &
            trim(adjustlt(Upper_Case(type)))/='INT8') then
          E_IO = -1_I4P
        else
          read(data, fmt=*, iostat=E_IO) var
        endif

      case(binary)
        if (trim(adjustlt(Upper_Case(fmt)))/='BINARY' .or. &
            trim(adjustlt(Upper_Case(type)))/='INT8') then
          E_IO = -1_I4P
        else
          ! Decode packed base64 data
          data=trim(adjustlt(data))
          allocate(dI1P(NC_NN*NCOMP*int(BYI1P,I4P)+int(BYI4P,I4P)))
          call b64_decode(code=data,n=dI1P); if(allocated(data)) deallocate(data)
          ! Unpack data [1xI4P,NN_NC*NCOMPxI1P]
          N_byte =  transfer(dI1P(1:int(BYI4P,I4P)),N_byte); s = size(transfer(dI1P(int(BYI4P,I4P)+1:),var))
          if(s /= NC_NN*NCOMP) E_IO = -1_I4P
          var = transfer(dI1P(int(BYI4P,I4P)+1:),var); if(allocated(dI1P)) deallocate(dI1P)
        endif

      case(raw)
        if(E_IO == 0) then
          if (trim(adjustlt(Upper_Case(fmt)))/='APPENDED' .or. &
              trim(adjustlt(Upper_Case(type)))/='INT8') then
            E_IO = -1_I4P
          else
            read(unit=vtk(rf)%u, iostat=E_IO, pos=vtk(rf)%ioffset+offs) N_Byte, var
          endif
      endif

    endselect
  endif
  !---------------------------------------------------------------------------------------------------------------------------------
  end function

  function VTK_VAR_XML_VECT_1DA_R8_READ(var_location,varname,NC_NN,NCOMP,varX,varY,varZ,npiece,cf) result(E_IO)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Function for reading field of vectorial variable (R8P, 1D array).
  !---------------------------------------------------------------------------------------------------------------------------------
  implicit none
  character(*),           intent(IN)  :: var_location !< location of variables: CELL for cell-centered, NODE for node-centered
  character(*),           intent(IN)  :: varname      !< variable name
  integer(I4P),           intent(OUT) :: NC_NN        !< number of cells or nodes
  integer(I4P),           intent(OUT) :: NCOMP        !< number of components
  real(R8P), allocatable, intent(OUT) :: varX(:)      !< variable to be saved [1:NN_NC]
  real(R8P), allocatable, intent(OUT) :: varY(:)      !< variable to be saved [1:NN_NC]
  real(R8P), allocatable, intent(OUT) :: varz(:)      !< variable to be saved [1:NN_NC]
  integer(I4P), optional, intent(IN)  :: npiece       !< Number of the piece to read (by default: 1)
  integer(I4P), optional, intent(IN)  :: cf           !< Current file index (for concurrent files IO).
  integer(I4P)                        :: E_IO         !< Input/Output inquiring flag: $0$ if IO is done, $> 0$ if IO is not done
  integer(I4P)                        :: rf           !< Real file index.
  character(len=:), allocatable       :: fmt
  character(len=:), allocatable       :: type
  character(len=:), allocatable       :: data
  integer(I4P)                        :: np, offs, N_Byte, i, s
  integer(I1P), allocatable           :: dI1P(:)
  real(R8P), allocatable              :: XYZp(:)
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  E_IO = -1_I4P
  rf = f
  if (present(cf)) then
    rf = cf ; f = cf
  endif
  np = 1_I4P; if (present(npiece)) np = npiece

  ! Read field headers
    E_IO = VTK_VAR_XML_HEADER_READ(var_location,varname,NC_NN,NCOMP,fmt=fmt,type=type,data=data,offs=offs,npiece=np,cf=rf)
  if(NCOMP/=3) E_IO=-1_I4P

  if(E_IO == 0) then
    allocate(varX(NC_NN),varY(NC_NN),varZ(NC_NN), stat=E_IO)

    ! Read field data
    select case(vtk(rf)%f)
      case(ascii)
        if (trim(adjustlt(Upper_Case(fmt)))/='ASCII' .or. &
            trim(adjustlt(Upper_Case(type)))/='FLOAT64') then
          E_IO = -1_I4P
        else
          read(data, fmt=*, iostat=E_IO) (varX(i),varY(i),varZ(i),i=1,NC_NN)
        endif

      case(binary)
        if (trim(adjustlt(Upper_Case(fmt)))/='BINARY' .or. &
            trim(adjustlt(Upper_Case(type)))/='FLOAT64') then
          E_IO = -1_I4P
        else
          ! Decode base64 packed data
          data=trim(adjustlt(data))
          allocate(dI1P(3*NC_NN*int(BYR8P,I4P)+int(BYI4P,I4P)))
          call b64_decode(code=data,n=dI1P); if(allocated(data)) deallocate(data)
          ! Unpack data [1xI4P,3*NC_NNxR8P]
          N_byte =  transfer(dI1P(1:int(BYI4P,I4P)),N_byte)
          s = size(transfer(dI1P(int(BYI4P,I4P)+1:),XYZp)); allocate(XYZp(1:s))
          XYZp = transfer(dI1P(int(BYI4P,I4P)+1:),XYZp); if(allocated(dI1P)) deallocate(dI1P)
          do i=1,NC_NN; varX(i)=XYZp(i*3-2); varY(i)=XYZp(i*3-1); varZ(i)=XYZp(i*3); enddo
          if(allocated(XYZp)) deallocate(XYZp)
        endif

      case(raw)
        if(E_IO == 0) then
          if (trim(adjustlt(Upper_Case(fmt)))/='APPENDED' .or. &
              trim(adjustlt(Upper_Case(type)))/='FLOAT64') then
            E_IO = -1_I4P
          else
            read(unit=vtk(rf)%u, iostat=E_IO, pos=vtk(rf)%ioffset+offs) N_Byte, (varX(i),varY(i),varZ(i),i=1,NC_NN)
          endif
      endif

    endselect
  endif
  !---------------------------------------------------------------------------------------------------------------------------------
  end function

  function VTK_VAR_XML_VECT_1DA_R4_READ(var_location,varname,NC_NN,NCOMP,varX,varY,varZ,npiece,cf) result(E_IO)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Function for reading field of vectorial variable (R4P, 1D array).
  !---------------------------------------------------------------------------------------------------------------------------------
  implicit none
  character(*),           intent(IN)  :: var_location !< location of variables: CELL for cell-centered, NODE for node-centered
  character(*),           intent(IN)  :: varname      !< variable name
  integer(I4P),           intent(OUT) :: NC_NN        !< number of cells or nodes
  integer(I4P),           intent(OUT) :: NCOMP        !< number of components
  real(R4P), allocatable, intent(OUT) :: varX(:)      !< variable to be saved [1:NN_NC]
  real(R4P), allocatable, intent(OUT) :: varY(:)      !< variable to be saved [1:NN_NC]
  real(R4P), allocatable, intent(OUT) :: varz(:)      !< variable to be saved [1:NN_NC]
  integer(I4P), optional, intent(IN)  :: npiece       !< Number of the piece to read (by default: 1)
  integer(I4P), optional, intent(IN)  :: cf           !< Current file index (for concurrent files IO).
  integer(I4P)                        :: E_IO         !< Input/Output inquiring flag: $0$ if IO is done, $> 0$ if IO is not done
  integer(I4P)                        :: rf           !< Real file index.
  character(len=:), allocatable       :: fmt
  character(len=:), allocatable       :: type
  character(len=:), allocatable       :: data
  integer(I4P)                        :: np, offs, N_Byte, i, s
  integer(I1P), allocatable           :: dI1P(:)
  real(R4P), allocatable              :: XYZp(:)
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  E_IO = -1_I4P
  rf = f
  if (present(cf)) then
    rf = cf ; f = cf
  endif
  np = 1_I4P; if (present(npiece)) np = npiece

  ! Read field headers
    E_IO = VTK_VAR_XML_HEADER_READ(var_location,varname,NC_NN,NCOMP,fmt=fmt,type=type,data=data,offs=offs,npiece=np,cf=rf)
  if(NCOMP/=3) E_IO=-1_I4P

  if(E_IO == 0) then
    allocate(varX(NC_NN),varY(NC_NN),varZ(NC_NN), stat=E_IO)

    ! Read field data
    select case(vtk(rf)%f)
      case(ascii)
        if (trim(adjustlt(Upper_Case(fmt)))/='ASCII' .or. &
            trim(adjustlt(Upper_Case(type)))/='FLOAT32') then
          E_IO = -1_I4P
        else
          read(data, fmt=*, iostat=E_IO) (varX(i),varY(i),varZ(i),i=1,NC_NN)
        endif

      case(binary)
        if (trim(adjustlt(Upper_Case(fmt)))/='BINARY' .or. &
            trim(adjustlt(Upper_Case(type)))/='FLOAT32') then
          E_IO = -1_I4P
        else
          ! Decode base64 packed data
          data=trim(adjustlt(data))
          allocate(dI1P(3*NC_NN*int(BYR4P,I4P)+int(BYI4P,I4P)))
          call b64_decode(code=data,n=dI1P); if(allocated(data)) deallocate(data)
          ! Unpack data [1xI4P,3*NC_NNxR4P]
          N_byte =  transfer(dI1P(1:int(BYI4P,I4P)),N_byte)
          s = size(transfer(dI1P(int(BYI4P,I4P)+1:),XYZp)); allocate(XYZp(1:s))
          XYZp = transfer(dI1P(int(BYI4P,I4P)+1:),XYZp); if(allocated(dI1P)) deallocate(dI1P)
          do i=1,NC_NN; varX(i)=XYZp(i*3-2); varY(i)=XYZp(i*3-1); varZ(i)=XYZp(i*3); enddo
          if(allocated(XYZp)) deallocate(XYZp)
        endif

      case(raw)
        if(E_IO == 0) then
          if (trim(adjustlt(Upper_Case(fmt)))/='APPENDED' .or. &
              trim(adjustlt(Upper_Case(type)))/='FLOAT32') then
            E_IO = -1_I4P
          else
            read(unit=vtk(rf)%u, iostat=E_IO, pos=vtk(rf)%ioffset+offs) N_Byte, (varX(i),varY(i),varZ(i),i=1,NC_NN)
          endif
      endif

    endselect
  endif
  !---------------------------------------------------------------------------------------------------------------------------------
  end function

  function VTK_VAR_XML_VECT_1DA_I8_READ(var_location,varname,NC_NN,NCOMP,varX,varY,varZ,npiece,cf) result(E_IO)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Function for reading field of vectorial variable (I4P, 1D array).
  !---------------------------------------------------------------------------------------------------------------------------------
  implicit none
  character(*),              intent(IN)  :: var_location !< location of variables: CELL for cell-centered, NODE for node-centered
  character(*),              intent(IN)  :: varname      !< variable name
  integer(I4P),              intent(OUT) :: NC_NN        !< number of cells or nodes
  integer(I4P),              intent(OUT) :: NCOMP        !< number of components
  integer(I8P), allocatable, intent(OUT) :: varX(:)      !< variable to be saved [1:NN_NC]
  integer(I8P), allocatable, intent(OUT) :: varY(:)      !< variable to be saved [1:NN_NC]
  integer(I8P), allocatable, intent(OUT) :: varz(:)      !< variable to be saved [1:NN_NC]
  integer(I4P), optional,    intent(IN)  :: npiece       !< Number of the piece to read (by default: 1)
  integer(I4P), optional,    intent(IN)  :: cf           !< Current file index (for concurrent files IO).
  integer(I4P)                           :: E_IO         !< Input/Output inquiring flag: $0$ if IO is done, $> 0$ if IO is not done
  integer(I4P)                           :: rf           !< Real file index.
  character(len=:), allocatable          :: fmt
  character(len=:), allocatable          :: type
  character(len=:), allocatable          :: data
  integer(I4P)                           :: np, offs, N_Byte, i, s
  integer(I1P), allocatable              :: dI1P(:)
  integer(I8P), allocatable              :: XYZp(:)
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  E_IO = -1_I4P
  rf = f
  if (present(cf)) then
    rf = cf ; f = cf
  endif
  np = 1_I4P; if (present(npiece)) np = npiece

  ! Read field headers
    E_IO = VTK_VAR_XML_HEADER_READ(var_location,varname,NC_NN,NCOMP,fmt=fmt,type=type,data=data,offs=offs,npiece=np,cf=rf)
  if(NCOMP/=3) E_IO=-1_I4P

  if(E_IO == 0) then
    allocate(varX(NC_NN),varY(NC_NN),varZ(NC_NN), stat=E_IO)

    ! Read field data
    select case(vtk(rf)%f)
      case(ascii)
        if (trim(adjustlt(Upper_Case(fmt)))/='ASCII' .or. &
            trim(adjustlt(Upper_Case(type)))/='INT64') then
          E_IO = -1_I4P
        else
          read(data, fmt=*, iostat=E_IO) (varX(i),varY(i),varZ(i),i=1,NC_NN)
        endif

      case(binary)
        if (trim(adjustlt(Upper_Case(fmt)))/='BINARY' .or. &
            trim(adjustlt(Upper_Case(type)))/='INT64') then
          E_IO = -1_I4P
        else
          ! Decode base64 packed data
          data=trim(adjustlt(data))
          allocate(dI1P(3*NC_NN*int(BYI8P,I4P)+int(BYI4P,I4P)))
          call b64_decode(code=data,n=dI1P); if(allocated(data)) deallocate(data)
          ! Unpack data [1xI4P,3*NC_NNxI8P]
          N_byte =  transfer(dI1P(1:int(BYI4P,I4P)),N_byte)
          s = size(transfer(dI1P(int(BYI4P,I4P)+1:),XYZp)); allocate(XYZp(1:s))
          XYZp = transfer(dI1P(int(BYI4P,I4P)+1:),XYZp); if(allocated(dI1P)) deallocate(dI1P)
          do i=1,NC_NN; varX(i)=XYZp(i*3-2); varY(i)=XYZp(i*3-1); varZ(i)=XYZp(i*3); enddo
          if(allocated(XYZp)) deallocate(XYZp)
        endif

      case(raw)
        if(E_IO == 0) then
          if (trim(adjustlt(Upper_Case(fmt)))/='APPENDED' .or. &
              trim(adjustlt(Upper_Case(type)))/='INT64') then
            E_IO = -1_I4P
          else
            read(unit=vtk(rf)%u, iostat=E_IO, pos=vtk(rf)%ioffset+offs) N_Byte, (varX(i),varY(i),varZ(i),i=1,NC_NN)
          endif
      endif

    endselect
  endif
  !---------------------------------------------------------------------------------------------------------------------------------
  end function

  function VTK_VAR_XML_VECT_1DA_I4_READ(var_location,varname,NC_NN,NCOMP,varX,varY,varZ,npiece,cf) result(E_IO)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Function for reading field of vectorial variable (I4P, 1D array).
  !---------------------------------------------------------------------------------------------------------------------------------
  implicit none
  character(*),              intent(IN)  :: var_location !< location of variables: CELL for cell-centered, NODE for node-centered
  character(*),              intent(IN)  :: varname      !< variable name
  integer(I4P),              intent(OUT) :: NC_NN        !< number of cells or nodes
  integer(I4P),              intent(OUT) :: NCOMP        !< number of components
  integer(I4P), allocatable, intent(OUT) :: varX(:)      !< variable to be saved [1:NN_NC]
  integer(I4P), allocatable, intent(OUT) :: varY(:)      !< variable to be saved [1:NN_NC]
  integer(I4P), allocatable, intent(OUT) :: varz(:)      !< variable to be saved [1:NN_NC]
  integer(I4P), optional,    intent(IN)  :: npiece       !< Number of the piece to read (by default: 1)
  integer(I4P), optional,    intent(IN)  :: cf           !< Current file index (for concurrent files IO).
  integer(I4P)                           :: E_IO         !< Input/Output inquiring flag: $0$ if IO is done, $> 0$ if IO is not done
  integer(I4P)                           :: rf           !< Real file index.
  character(len=:), allocatable          :: fmt
  character(len=:), allocatable          :: type
  character(len=:), allocatable          :: data
  integer(I4P)                           :: np, offs, N_Byte, i, s
  integer(I1P), allocatable              :: dI1P(:)
  integer(I4P), allocatable              :: XYZp(:)

  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  E_IO = -1_I4P
  rf = f
  if (present(cf)) then
    rf = cf ; f = cf
  endif
  np = 1_I4P; if (present(npiece)) np = npiece

  ! Read field headers
    E_IO = VTK_VAR_XML_HEADER_READ(var_location,varname,NC_NN,NCOMP,fmt=fmt,type=type,data=data,offs=offs,npiece=np,cf=rf)
  if(NCOMP/=3) E_IO=-1_I4P

  if(E_IO == 0) then
    allocate(varX(NC_NN),varY(NC_NN),varZ(NC_NN), stat=E_IO)

    ! Read field data
    select case(vtk(rf)%f)
      case(ascii)
        if (trim(adjustlt(Upper_Case(fmt)))/='ASCII' .or. &
            trim(adjustlt(Upper_Case(type)))/='INT32') then
          E_IO = -1_I4P
        else
          read(data, fmt=*, iostat=E_IO) (varX(i),varY(i),varZ(i),i=1,NC_NN)
        endif

      case(binary)
        if (trim(adjustlt(Upper_Case(fmt)))/='BINARY' .or. &
            trim(adjustlt(Upper_Case(type)))/='INT32') then
          E_IO = -1_I4P
        else
          ! Decode base64 packed data
          data=trim(adjustlt(data))
          allocate(dI1P(3*NC_NN*int(BYI4P,I4P)+int(BYI4P,I4P)))
          call b64_decode(code=data,n=dI1P); if(allocated(data)) deallocate(data)
          ! Unpack data [1xI4P,3*NC_NNxI4P]
          N_byte =  transfer(dI1P(1:int(BYI4P,I4P)),N_byte)
          s = size(transfer(dI1P(int(BYI4P,I4P)+1:),XYZp)); allocate(XYZp(1:s))
          XYZp = transfer(dI1P(int(BYI4P,I4P)+1:),XYZp); if(allocated(dI1P)) deallocate(dI1P)
          do i=1,NC_NN; varX(i)=XYZp(i*3-2); varY(i)=XYZp(i*3-1); varZ(i)=XYZp(i*3); enddo
          if(allocated(XYZp)) deallocate(XYZp)
        endif

      case(raw)
        if(E_IO == 0) then
          if (trim(adjustlt(Upper_Case(fmt)))/='APPENDED' .or. &
              trim(adjustlt(Upper_Case(type)))/='INT32') then
            E_IO = -1_I4P
          else
            read(unit=vtk(rf)%u, iostat=E_IO, pos=vtk(rf)%ioffset+offs) N_Byte, (varX(i),varY(i),varZ(i),i=1,NC_NN)
          endif
      endif

    endselect
  endif
  !---------------------------------------------------------------------------------------------------------------------------------
  end function

  function VTK_VAR_XML_VECT_1DA_I2_READ(var_location,varname,NC_NN,NCOMP,varX,varY,varZ,npiece,cf) result(E_IO)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Function for reading field of vectorial variable (I2P, 1D array).
  !---------------------------------------------------------------------------------------------------------------------------------
  implicit none
  character(*),              intent(IN)  :: var_location !< location of variables: CELL for cell-centered, NODE for node-centered
  character(*),              intent(IN)  :: varname      !< variable name
  integer(I4P),              intent(OUT) :: NC_NN        !< number of cells or nodes
  integer(I4P),              intent(OUT) :: NCOMP        !< number of components
  integer(I2P), allocatable, intent(OUT) :: varX(:)      !< variable to be saved [1:NN_NC]
  integer(I2P), allocatable, intent(OUT) :: varY(:)      !< variable to be saved [1:NN_NC]
  integer(I2P), allocatable, intent(OUT) :: varZ(:)      !< variable to be saved [1:NN_NC]
  integer(I4P), optional,    intent(IN)  :: npiece       !< Number of the piece to read (by default: 1)
  integer(I4P), optional,    intent(IN)  :: cf           !< Current file index (for concurrent files IO).
  integer(I4P)                           :: E_IO         !< Input/Output inquiring flag: $0$ if IO is done, $> 0$ if IO is not done
  integer(I4P)                           :: rf           !< Real file index.
  character(len=:), allocatable          :: fmt
  character(len=:), allocatable          :: type
  character(len=:), allocatable          :: data
  integer(I4P)                           :: np, offs, N_Byte, i, s
  integer(I1P), allocatable              :: dI1P(:)
  integer(I2P), allocatable              :: XYZp(:)
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  E_IO = -1_I4P
  rf = f
  if (present(cf)) then
    rf = cf ; f = cf
  endif
  np = 1_I4P; if (present(npiece)) np = npiece

  ! Read field headers
    E_IO = VTK_VAR_XML_HEADER_READ(var_location,varname,NC_NN,NCOMP,fmt=fmt,type=type,data=data,offs=offs,npiece=np,cf=rf)
  if(NCOMP/=3) E_IO=-1_I4P

  if(E_IO == 0) then
    allocate(varX(NC_NN),varY(NC_NN),varZ(NC_NN), stat=E_IO)

    ! Read field data
    select case(vtk(rf)%f)
      case(ascii)
        if (trim(adjustlt(Upper_Case(fmt)))/='ASCII' .or. &
            trim(adjustlt(Upper_Case(type)))/='INT16') then
          E_IO = -1_I4P
        else
          read(data, fmt=*, iostat=E_IO) (varX(i),varY(i),varZ(i),i=1,NC_NN)
        endif

      case(binary)
        if (trim(adjustlt(Upper_Case(fmt)))/='BINARY' .or. &
            trim(adjustlt(Upper_Case(type)))/='INT16') then
          E_IO = -1_I4P
        else
          ! Decode base64 packed data
          data=trim(adjustlt(data))
          allocate(dI1P(3*NC_NN*int(BYI2P,I4P)+int(BYI4P,I4P)))
          call b64_decode(code=data,n=dI1P); if(allocated(data)) deallocate(data)
          ! Unpack data [1xI4P,3*NC_NNxI2P]
          N_byte =  transfer(dI1P(1:int(BYI4P,I4P)),N_byte)
          s = size(transfer(dI1P(int(BYI4P,I4P)+1:),XYZp)); allocate(XYZp(1:s))
          XYZp = transfer(dI1P(int(BYI4P,I4P)+1:),XYZp); if(allocated(dI1P)) deallocate(dI1P)
          do i=1,NC_NN; varX(i)=XYZp(i*3-2); varY(i)=XYZp(i*3-1); varZ(i)=XYZp(i*3); enddo
          if(allocated(XYZp)) deallocate(XYZp)
        endif

      case(raw)
        if(E_IO == 0) then
          if (trim(adjustlt(Upper_Case(fmt)))/='APPENDED' .or. &
              trim(adjustlt(Upper_Case(type)))/='INT16') then
            E_IO = -1_I4P
          else
            read(unit=vtk(rf)%u, iostat=E_IO, pos=vtk(rf)%ioffset+offs) N_Byte, (varX(i),varY(i),varZ(i),i=1,NC_NN)
          endif
      endif

    endselect
  endif
  !---------------------------------------------------------------------------------------------------------------------------------
  end function

  function VTK_VAR_XML_VECT_1DA_I1_READ(var_location,varname,NC_NN,NCOMP,varX,varY,varZ,npiece,cf) result(E_IO)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Function for reading field of vectorial variable (I1P, 1D array).
  !---------------------------------------------------------------------------------------------------------------------------------
  implicit none
  character(*),              intent(IN)  :: var_location !< location of variables: CELL for cell-centered, NODE for node-centered
  character(*),              intent(IN)  :: varname      !< variable name
  integer(I4P),              intent(OUT) :: NC_NN        !< number of cells or nodes
  integer(I4P),              intent(OUT) :: NCOMP        !< number of components
  integer(I1P), allocatable, intent(OUT) :: varX(:)      !< variable to be saved [1:NN_NC]
  integer(I1P), allocatable, intent(OUT) :: varY(:)      !< variable to be saved [1:NN_NC]
  integer(I1P), allocatable, intent(OUT) :: varZ(:)      !< variable to be saved [1:NN_NC]
  integer(I4P), optional,    intent(IN)  :: npiece       !< Number of the piece to read (by default: 1)
  integer(I4P), optional,    intent(IN)  :: cf           !< Current file index (for concurrent files IO).
  integer(I4P)                           :: E_IO         !< Input/Output inquiring flag: $0$ if IO is done, $> 0$ if IO is not done
  integer(I4P)                           :: rf           !< Real file index.
  character(len=:), allocatable          :: fmt
  character(len=:), allocatable          :: type
  character(len=:), allocatable          :: data
  integer(I4P)                           :: np, offs, N_Byte, i, s
  integer(I1P), allocatable              :: dI1P(:)
  integer(I1P), allocatable              :: XYZp(:)
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  E_IO = -1_I4P
  rf = f
  if (present(cf)) then
    rf = cf ; f = cf
  endif
  np = 1_I4P; if (present(npiece)) np = npiece

  ! Read field headers
    E_IO = VTK_VAR_XML_HEADER_READ(var_location,varname,NC_NN,NCOMP,fmt=fmt,type=type,data=data,offs=offs,npiece=np,cf=rf)
  if(NCOMP/=3) E_IO=-1_I4P

  if(E_IO == 0) then
    allocate(varX(NC_NN),varY(NC_NN),varZ(NC_NN), stat=E_IO)

    ! Read field data
    select case(vtk(rf)%f)
      case(ascii)
        if (trim(adjustlt(Upper_Case(fmt)))/='ASCII' .or. &
            trim(adjustlt(Upper_Case(type)))/='INT8') then
          E_IO = -1_I4P
        else
          read(data, fmt=*, iostat=E_IO) (varX(i),varY(i),varZ(i),i=1,NC_NN)
        endif

      case(binary)
        if (trim(adjustlt(Upper_Case(fmt)))/='BINARY' .or. &
            trim(adjustlt(Upper_Case(type)))/='INT8') then
          E_IO = -1_I4P
        else
          ! Decode base64 packed data
          data=trim(adjustlt(data))
          allocate(dI1P(3*NC_NN*int(BYI1P,I4P)+int(BYI4P,I4P)))
          call b64_decode(code=data,n=dI1P); if(allocated(data)) deallocate(data)
          ! Unpack data [1xI4P,3*NC_NNxI1P]
          N_byte =  transfer(dI1P(1:int(BYI4P,I4P)),N_byte)
          s = size(transfer(dI1P(int(BYI4P,I4P)+1:),XYZp)); allocate(XYZp(1:s))
          XYZp = transfer(dI1P(int(BYI4P,I4P)+1:),XYZp); if(allocated(dI1P)) deallocate(dI1P)
          do i=1,NC_NN; varX(i)=XYZp(i*3-2); varY(i)=XYZp(i*3-1); varZ(i)=XYZp(i*3); enddo
          if(allocated(XYZp)) deallocate(XYZp)
        endif

      case(raw)
        if(E_IO == 0) then
          if (trim(adjustlt(Upper_Case(fmt)))/='APPENDED' .or. &
              trim(adjustlt(Upper_Case(type)))/='INT8') then
            E_IO = -1_I4P
          else
            read(unit=vtk(rf)%u, iostat=E_IO, pos=vtk(rf)%ioffset+offs) N_Byte, (varX(i),varY(i),varZ(i),i=1,NC_NN)
          endif
      endif

    endselect
  endif
  !---------------------------------------------------------------------------------------------------------------------------------
  end function

  function VTK_VAR_XML_LIST_1DA_R8_READ(var_location,varname,NC_NN,NCOMP,var,npiece,cf) result(E_IO)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Function for reading field of scalar variable (R8P, 1D array).
  !---------------------------------------------------------------------------------------------------------------------------------
  implicit none
  character(*),           intent(IN)  :: var_location !< location of variables: CELL for cell-centered, NODE for node-centered
  character(*),           intent(IN)  :: varname      !< variable name
  integer(I4P),           intent(OUT) :: NC_NN        !< number of cells or nodes
  integer(I4P),           intent(OUT) :: NCOMP        !< number of components
  real(R8P), allocatable, intent(OUT) :: var(:,:)     !< variable to be saved
  integer(I4P), optional, intent(IN)  :: npiece       !< Number of the piece to read (by default: 1)
  integer(I4P), optional, intent(IN)  :: cf           !< Current file index (for concurrent files IO).
  integer(I4P)                        :: E_IO         !< Input/Output inquiring flag: $0$ if IO is done, $> 0$ if IO is not done
  integer(I4P)                        :: rf           !< Real file index.
  character(len=:), allocatable       :: fmt
  character(len=:), allocatable       :: type
  character(len=:), allocatable       :: data
  integer(I4P)                        :: np, offs, N_Byte, s
  integer(I1P), allocatable           :: dI1P(:)
  real(R8P), allocatable              :: XYZp(:)
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  E_IO = -1_I4P
  rf = f
  if (present(cf)) then
    rf = cf ; f = cf
  endif
  np = 1_I4P; if (present(npiece)) np = npiece

  ! Read field headers
    E_IO = VTK_VAR_XML_HEADER_READ(var_location,varname,NC_NN,NCOMP,fmt=fmt,type=type,data=data,offs=offs,npiece=np,cf=rf)

  if(E_IO == 0) then
    allocate(var(NCOMP,NC_NN), stat=E_IO)

    ! Read field data
    select case(vtk(rf)%f)
      case(ascii)
        if (trim(adjustlt(Upper_Case(fmt)))/='ASCII' .or. &
            trim(adjustlt(Upper_Case(type)))/='FLOAT64') then
          E_IO = -1_I4P
        else
          read(data, fmt=*, iostat=E_IO) var
        endif

      case(binary)
        if (trim(adjustlt(Upper_Case(fmt)))/='BINARY' .or. &
            trim(adjustlt(Upper_Case(type)))/='FLOAT64') then
          E_IO = -1_I4P
        else
          data=trim(adjustlt(data))
          allocate(dI1P(NCOMP*NC_NN*int(BYR8P,I4P)+int(BYI4P,I4P)))
          call b64_decode(code=data,n=dI1P); if(allocated(data)) deallocate(data)
          ! Unpack data [1xI4P,NCOMP*NC_NNxR8P]
          N_byte =  transfer(dI1P(1:int(BYI4P,I4P)),N_byte)
          s = size(transfer(dI1P(int(BYI4P,I4P)+1:),XYZp)); allocate(XYZp(1:s))
          XYZp = transfer(dI1P(int(BYI4P,I4P)+1:),XYZp); if(allocated(dI1P)) deallocate(dI1P)
          var = reshape(XYZp,(/NCOMP,NC_NN/))
          if(allocated(XYZp)) deallocate(XYZp)
        endif

      case(raw)
        if(E_IO == 0) then
          if (trim(adjustlt(Upper_Case(fmt)))/='APPENDED' .or. &
              trim(adjustlt(Upper_Case(type)))/='FLOAT64') then
            E_IO = -1_I4P
          else
            read(unit=vtk(rf)%u, iostat=E_IO, pos=vtk(rf)%ioffset+offs) N_Byte, var
          endif
      endif

    endselect
  endif
  !---------------------------------------------------------------------------------------------------------------------------------
  end function

  function VTK_VAR_XML_LIST_1DA_R4_READ(var_location,varname,NC_NN,NCOMP,var,npiece,cf) result(E_IO)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Function for reading field of scalar variable (R4P, 1D array).
  !---------------------------------------------------------------------------------------------------------------------------------
  implicit none
  character(*),           intent(IN)  :: var_location !< location of variables: CELL for cell-centered, NODE for node-centered
  character(*),           intent(IN)  :: varname      !< variable name
  integer(I4P),           intent(OUT) :: NC_NN        !< number of cells or nodes
  integer(I4P),           intent(OUT) :: NCOMP        !< number of components
  real(R4P), allocatable, intent(OUT) :: var(:,:)     !< variable to be saved
  integer(I4P), optional, intent(IN)  :: npiece       !< Number of the piece to read (by default: 1)
  integer(I4P), optional, intent(IN)  :: cf           !< Current file index (for concurrent files IO).
  integer(I4P)                        :: E_IO         !< Input/Output inquiring flag: $0$ if IO is done, $> 0$ if IO is not done
  integer(I4P)                        :: rf           !< Real file index.
  character(len=:), allocatable       :: fmt
  character(len=:), allocatable       :: type
  character(len=:), allocatable       :: data
  integer(I4P)                        :: np, offs, N_Byte, s
  integer(I1P), allocatable           :: dI1P(:)
  real(R4P), allocatable              :: XYZp(:)
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  E_IO = -1_I4P
  rf = f
  if (present(cf)) then
    rf = cf ; f = cf
  endif
  np = 1_I4P; if (present(npiece)) np = npiece

  ! Read field headers
    E_IO = VTK_VAR_XML_HEADER_READ(var_location,varname,NC_NN,NCOMP,fmt=fmt,type=type,data=data,offs=offs,npiece=np,cf=rf)

  if(E_IO == 0) then
    allocate(var(NCOMP,NC_NN), stat=E_IO)

    ! Read field data
    select case(vtk(rf)%f)
      case(ascii)
        if (trim(adjustlt(Upper_Case(fmt)))/='ASCII' .or. &
            trim(adjustlt(Upper_Case(type)))/='FLOAT32') then
          E_IO = -1_I4P
        else
          read(data, fmt=*, iostat=E_IO) var
        endif

      case(binary)
        if (trim(adjustlt(Upper_Case(fmt)))/='BINARY' .or. &
            trim(adjustlt(Upper_Case(type)))/='FLOAT32') then
          E_IO = -1_I4P
        else
          data=trim(adjustlt(data))
          allocate(dI1P(NCOMP*NC_NN*int(BYR4P,I4P)+int(BYI4P,I4P)))
          call b64_decode(code=data,n=dI1P); if(allocated(data)) deallocate(data)
          ! Unpack data [1xI4P,NCOMP*NC_NNxR4P]
          N_byte =  transfer(dI1P(1:int(BYI4P,I4P)),N_byte)
          s = size(transfer(dI1P(int(BYI4P,I4P)+1:),XYZp)); allocate(XYZp(1:s))
          XYZp = transfer(dI1P(int(BYI4P,I4P)+1:),XYZp); if(allocated(dI1P)) deallocate(dI1P)
          var = reshape(XYZp,(/NCOMP,NC_NN/))
          if(allocated(XYZp)) deallocate(XYZp)
        endif

      case(raw)
        if(E_IO == 0) then
          if (trim(adjustlt(Upper_Case(fmt)))/='APPENDED' .or. &
              trim(adjustlt(Upper_Case(type)))/='FLOAT32') then
            E_IO = -1_I4P
          else
            read(unit=vtk(rf)%u, iostat=E_IO, pos=vtk(rf)%ioffset+offs) N_Byte, var
          endif
      endif

    endselect
  endif
  !---------------------------------------------------------------------------------------------------------------------------------
  end function

  function VTK_VAR_XML_LIST_1DA_I8_READ(var_location,varname,NC_NN,NCOMP,var,npiece,cf) result(E_IO)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Function for reading field of scalar variable (I8P, 1D array).
  !---------------------------------------------------------------------------------------------------------------------------------
  implicit none
  character(*),              intent(IN)  :: var_location !< location of variables: CELL for cell-centered, NODE for node-centered
  character(*),              intent(IN)  :: varname      !< variable name
  integer(I4P),              intent(OUT) :: NC_NN        !< number of cells or nodes
  integer(I4P),              intent(OUT) :: NCOMP        !< number of components
  integer(I8P), allocatable, intent(OUT) :: var(:,:)     !< variable to be saved
  integer(I4P), optional,    intent(IN)  :: npiece       !< Number of the piece to read (by default: 1)
  integer(I4P), optional,    intent(IN)  :: cf           !< Current file index (for concurrent files IO).
  integer(I4P)                           :: E_IO         !< Input/Output inquiring flag: $0$ if IO is done, $> 0$ if IO is not done
  integer(I4P)                           :: rf           !< Real file index.
  character(len=:), allocatable          :: fmt
  character(len=:), allocatable          :: type
  character(len=:), allocatable          :: data
  integer(I4P)                           :: np, offs, N_Byte, s
  integer(I1P), allocatable              :: dI1P(:)
  integer(I8P), allocatable              :: XYZp(:)
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  E_IO = -1_I4P
  rf = f
  if (present(cf)) then
    rf = cf ; f = cf
  endif
  np = 1_I4P; if (present(npiece)) np = npiece

  ! Read field headers
    E_IO = VTK_VAR_XML_HEADER_READ(var_location,varname,NC_NN,NCOMP,fmt=fmt,type=type,data=data,offs=offs,npiece=np,cf=rf)

  if(E_IO == 0) then
    allocate(var(NCOMP,NC_NN), stat=E_IO)

    ! Read field data
    select case(vtk(rf)%f)
      case(ascii)
        if (trim(adjustlt(Upper_Case(fmt)))/='ASCII' .or. &
            trim(adjustlt(Upper_Case(type)))/='INT64') then
          E_IO = -1_I4P
        else
          read(data, fmt=*, iostat=E_IO) var
        endif

      case(binary)
        if (trim(adjustlt(Upper_Case(fmt)))/='BINARY' .or. &
            trim(adjustlt(Upper_Case(type)))/='INT64') then
          E_IO = -1_I4P
        else
          data=trim(adjustlt(data))
          allocate(dI1P(NCOMP*NC_NN*int(BYI8P,I4P)+int(BYI4P,I4P)))
          call b64_decode(code=data,n=dI1P); if(allocated(data)) deallocate(data)
          ! Unpack data [1xI4P,NCOMP*NC_NNxI8P]
          N_byte =  transfer(dI1P(1:int(BYI4P,I4P)),N_byte)
          s = size(transfer(dI1P(int(BYI4P,I4P)+1:),XYZp)); allocate(XYZp(1:s))
          XYZp = transfer(dI1P(int(BYI4P,I4P)+1:),XYZp); if(allocated(dI1P)) deallocate(dI1P)
          var = reshape(XYZp,(/NCOMP,NC_NN/))
          if(allocated(XYZp)) deallocate(XYZp)
        endif

      case(raw)
        if(E_IO == 0) then
          if (trim(adjustlt(Upper_Case(fmt)))/='APPENDED' .or. &
              trim(adjustlt(Upper_Case(type)))/='INT64') then
            E_IO = -1_I4P
          else
            read(unit=vtk(rf)%u, iostat=E_IO, pos=vtk(rf)%ioffset+offs) N_Byte, var
          endif
      endif

    endselect
  endif
  !---------------------------------------------------------------------------------------------------------------------------------
  end function

  function VTK_VAR_XML_LIST_1DA_I4_READ(var_location,varname,NC_NN,NCOMP,var,npiece,cf) result(E_IO)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Function for reading field of scalar variable (I4P, 1D array).
  !---------------------------------------------------------------------------------------------------------------------------------
  implicit none
  character(*),              intent(IN)  :: var_location !< location of variables: CELL for cell-centered, NODE for node-centered
  character(*),              intent(IN)  :: varname      !< variable name
  integer(I4P),              intent(OUT) :: NC_NN        !< number of cells or nodes
  integer(I4P),              intent(OUT) :: NCOMP        !< number of components
  integer(I4P), allocatable, intent(OUT) :: var(:,:)     !< variable to be saved
  integer(I4P), optional,    intent(IN)  :: npiece       !< Number of the piece to read (by default: 1)
  integer(I4P), optional,    intent(IN)  :: cf           !< Current file index (for concurrent files IO).
  integer(I4P)                           :: E_IO         !< Input/Output inquiring flag: $0$ if IO is done, $> 0$ if IO is not done
  integer(I4P)                           :: rf           !< Real file index.
  character(len=:), allocatable          :: fmt
  character(len=:), allocatable          :: type
  character(len=:), allocatable          :: data
  integer(I4P)                           :: np, offs, N_Byte, s
  integer(I1P), allocatable              :: dI1P(:)
  integer(I4P), allocatable              :: XYZp(:)
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  E_IO = -1_I4P
  rf = f
  if (present(cf)) then
    rf = cf ; f = cf
  endif
  np = 1_I4P; if (present(npiece)) np = npiece

  ! Read field headers
    E_IO = VTK_VAR_XML_HEADER_READ(var_location,varname,NC_NN,NCOMP,fmt=fmt,type=type,data=data,offs=offs,npiece=np,cf=rf)

  if(E_IO == 0) then
    allocate(var(NCOMP,NC_NN), stat=E_IO)

    ! Read field data
    select case(vtk(rf)%f)
      case(ascii)
        if (trim(adjustlt(Upper_Case(fmt)))/='ASCII' .or. &
            trim(adjustlt(Upper_Case(type)))/='INT32') then
          E_IO = -1_I4P
        else
          read(data, fmt=*, iostat=E_IO) var
        endif

      case(binary)
        if (trim(adjustlt(Upper_Case(fmt)))/='BINARY' .or. &
            trim(adjustlt(Upper_Case(type)))/='INT32') then
          E_IO = -1_I4P
        else
          data=trim(adjustlt(data))
          allocate(dI1P(NCOMP*NC_NN*int(BYI4P,I4P)+int(BYI4P,I4P)))
          call b64_decode(code=data,n=dI1P); if(allocated(data)) deallocate(data)
          ! Unpack data [1xI4P,NCOMP*NC_NNxI4P]
          N_byte =  transfer(dI1P(1:int(BYI4P,I4P)),N_byte)
          s = size(transfer(dI1P(int(BYI4P,I4P)+1:),XYZp)); allocate(XYZp(1:s))
          XYZp = transfer(dI1P(int(BYI4P,I4P)+1:),XYZp); if(allocated(dI1P)) deallocate(dI1P)
          var = reshape(XYZp,(/NCOMP,NC_NN/))
          if(allocated(XYZp)) deallocate(XYZp)
        endif

      case(raw)
        if(E_IO == 0) then
          if (trim(adjustlt(Upper_Case(fmt)))/='APPENDED' .or. &
              trim(adjustlt(Upper_Case(type)))/='INT32') then
            E_IO = -1_I4P
          else
            read(unit=vtk(rf)%u, iostat=E_IO, pos=vtk(rf)%ioffset+offs) N_Byte, var
          endif
      endif

    endselect
  endif
  !---------------------------------------------------------------------------------------------------------------------------------
  end function

  function VTK_VAR_XML_LIST_1DA_I2_READ(var_location,varname,NC_NN,NCOMP,var,npiece,cf) result(E_IO)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Function for reading field of scalar variable (I2P, 1D array).
  !---------------------------------------------------------------------------------------------------------------------------------
  implicit none
  character(*),              intent(IN)  :: var_location !< location of variables: CELL for cell-centered, NODE for node-centered
  character(*),              intent(IN)  :: varname      !< variable name
  integer(I4P),              intent(OUT) :: NC_NN        !< number of cells or nodes
  integer(I4P),              intent(OUT) :: NCOMP        !< number of components
  integer(I2P), allocatable, intent(OUT) :: var(:,:)     !< variable to be saved
  integer(I4P), optional,    intent(IN)  :: npiece       !< Number of the piece to read (by default: 1)
  integer(I4P), optional,    intent(IN)  :: cf           !< Current file index (for concurrent files IO).
  integer(I4P)                           :: E_IO         !< Input/Output inquiring flag: $0$ if IO is done, $> 0$ if IO is not done
  integer(I4P)                           :: rf           !< Real file index.
  character(len=:), allocatable          :: fmt
  character(len=:), allocatable          :: type
  character(len=:), allocatable          :: data
  integer(I4P)                           :: np, offs, N_Byte, s
  integer(I1P), allocatable              :: dI1P(:)
  integer(I2P), allocatable              :: XYZp(:)
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  E_IO = -1_I4P
  rf = f
  if (present(cf)) then
    rf = cf ; f = cf
  endif
  np = 1_I4P; if (present(npiece)) np = npiece

  ! Read field headers
    E_IO = VTK_VAR_XML_HEADER_READ(var_location,varname,NC_NN,NCOMP,fmt=fmt,type=type,data=data,offs=offs,npiece=np,cf=rf)

  if(E_IO == 0) then
    allocate(var(NCOMP,NC_NN), stat=E_IO)

    ! Read field data
    select case(vtk(rf)%f)
      case(ascii)
        if (trim(adjustlt(Upper_Case(fmt)))/='ASCII' .or. &
            trim(adjustlt(Upper_Case(type)))/='INT16') then
          E_IO = -1_I4P
        else
          read(data, fmt=*, iostat=E_IO) var
        endif

      case(binary)
        if (trim(adjustlt(Upper_Case(fmt)))/='BINARY' .or. &
            trim(adjustlt(Upper_Case(type)))/='INT16') then
          E_IO = -1_I4P
        else
          data=trim(adjustlt(data))
          allocate(dI1P(NCOMP*NC_NN*int(BYI2P,I4P)+int(BYI4P,I4P)))
          call b64_decode(code=data,n=dI1P); if(allocated(data)) deallocate(data)
          ! Unpack data [1xI4P,NCOMP*NC_NNxI2P]
          N_byte =  transfer(dI1P(1:int(BYI4P,I4P)),N_byte)
          s = size(transfer(dI1P(int(BYI4P,I4P)+1:),XYZp)); allocate(XYZp(1:s))
          XYZp = transfer(dI1P(int(BYI4P,I4P)+1:),XYZp); if(allocated(dI1P)) deallocate(dI1P)
          var = reshape(XYZp,(/NCOMP,NC_NN/))
          if(allocated(XYZp)) deallocate(XYZp)
        endif

      case(raw)
        if(E_IO == 0) then
          if (trim(adjustlt(Upper_Case(fmt)))/='APPENDED' .or. &
              trim(adjustlt(Upper_Case(type)))/='INT16') then
            E_IO = -1_I4P
          else
            read(unit=vtk(rf)%u, iostat=E_IO, pos=vtk(rf)%ioffset+offs) N_Byte, var
          endif
      endif

    endselect
  endif
  !---------------------------------------------------------------------------------------------------------------------------------
  end function

  function VTK_VAR_XML_LIST_1DA_I1_READ(var_location,varname,NC_NN,NCOMP,var,npiece,cf) result(E_IO)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Function for reading field of scalar variable (I1P, 1D array).
  !---------------------------------------------------------------------------------------------------------------------------------
  implicit none
  character(*),              intent(IN)  :: var_location !< location of variables: CELL for cell-centered, NODE for node-centered
  character(*),              intent(IN)  :: varname      !< variable name
  integer(I4P),              intent(OUT) :: NC_NN        !< number of cells or nodes
  integer(I4P),              intent(OUT) :: NCOMP        !< number of components
  integer(I1P), allocatable, intent(OUT) :: var(:,:)     !< variable to be saved
  integer(I4P), optional,    intent(IN)  :: npiece       !< Number of the piece to read (by default: 1)
  integer(I4P), optional,    intent(IN)  :: cf           !< Current file index (for concurrent files IO).
  integer(I4P)                           :: E_IO         !< Input/Output inquiring flag: $0$ if IO is done, $> 0$ if IO is not done
  integer(I4P)                           :: rf           !< Real file index.
  character(len=:), allocatable          :: fmt
  character(len=:), allocatable          :: type
  character(len=:), allocatable          :: data
  integer(I4P)                           :: np, offs, N_Byte, s
  integer(I1P), allocatable              :: dI1P(:)
  integer(I1P), allocatable              :: XYZp(:)
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  E_IO = -1_I4P
  rf = f
  if (present(cf)) then
    rf = cf ; f = cf
  endif
  np = 1_I4P; if (present(npiece)) np = npiece

  ! Read field headers
    E_IO = VTK_VAR_XML_HEADER_READ(var_location,varname,NC_NN,NCOMP,fmt=fmt,type=type,data=data,offs=offs,npiece=np,cf=rf)

  if(E_IO == 0) then
    allocate(var(NCOMP,NC_NN), stat=E_IO)

    ! Read field data
    select case(vtk(rf)%f)
      case(ascii)
        if (trim(adjustlt(Upper_Case(fmt)))/='ASCII' .or. &
            trim(adjustlt(Upper_Case(type)))/='INT8') then
          E_IO = -1_I4P
        else
          read(data, fmt=*, iostat=E_IO) var
        endif

      case(binary)
        if (trim(adjustlt(Upper_Case(fmt)))/='BINARY' .or. &
            trim(adjustlt(Upper_Case(type)))/='INT8') then
          E_IO = -1_I4P
        else
          data=trim(adjustlt(data))
          allocate(dI1P(NCOMP*NC_NN*int(BYI1P,I4P)+int(BYI4P,I4P)))
          call b64_decode(code=data,n=dI1P); if(allocated(data)) deallocate(data)
          ! Unpack data [1xI4P,NCOMP*NC_NNxI1P]
          N_byte =  transfer(dI1P(1:int(BYI4P,I4P)),N_byte)
          s = size(transfer(dI1P(int(BYI4P,I4P)+1:),XYZp)); allocate(XYZp(1:s))
          XYZp = transfer(dI1P(int(BYI4P,I4P)+1:),XYZp); if(allocated(dI1P)) deallocate(dI1P)
          var = reshape(XYZp,(/NCOMP,NC_NN/))
          if(allocated(XYZp)) deallocate(XYZp)
        endif

      case(raw)
        if(E_IO == 0) then
          if (trim(adjustlt(Upper_Case(fmt)))/='APPENDED' .or. &
              trim(adjustlt(Upper_Case(type)))/='INT8') then
            E_IO = -1_I4P
          else
            read(unit=vtk(rf)%u, iostat=E_IO, pos=vtk(rf)%ioffset+offs) N_Byte, var
          endif
      endif

    endselect
  endif
  !---------------------------------------------------------------------------------------------------------------------------------
  end function

  function VTK_VAR_XML_SCAL_3DA_R8_READ(var_location,varname,NC_NN,NCOMP,var,npiece,cf) result(E_IO)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Function for reading field of scalar variable (R8P, 1D array).
  !---------------------------------------------------------------------------------------------------------------------------------
  implicit none
  character(*),           intent(IN)  :: var_location !< location of variables: CELL for cell-centered, NODE for node-centered
  character(*),           intent(IN)  :: varname      !< variable name
  integer(I4P),           intent(OUT) :: NC_NN        !< number of cells or nodes
  integer(I4P),           intent(OUT) :: NCOMP        !< number of components
  real(R8P), allocatable, intent(OUT) :: var(:,:,:)   !< variable to be saved
  integer(I4P), optional, intent(IN)  :: npiece       !< Number of the piece to read (by default: 1)
  integer(I4P), optional, intent(IN)  :: cf           !< Current file index (for concurrent files IO).
  integer(I4P)                        :: E_IO         !< Input/Output inquiring flag: $0$ if IO is done, $> 0$ if IO is not done
  integer(I4P)                        :: rf           !< Real file index.
  character(len=:), allocatable       :: fmt
  character(len=:), allocatable       :: type
  character(len=:), allocatable       :: data
  integer(I4P)                        :: nx1,nx2,ny1,ny2,nz1,nz2
  integer(I4P)                        :: np, offs, N_Byte, s
  integer(I1P), allocatable           :: dI1P(:)
  real(R8P), allocatable              :: XYZp(:)
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  E_IO = -1_I4P
  rf = f
  if (present(cf)) then
    rf = cf ; f = cf
  endif
  np = 1_I4P; if (present(npiece)) np = npiece

  select case(trim(vtk(rf)%topology))
    case('RectilinearGrid','StructuredGrid')
      ! Read field headers
      E_IO = VTK_VAR_XML_HEADER_READ(var_location,varname,NC_NN,NCOMP,&
                  nx1=nx1,nx2=nx2,ny1=ny1,ny2=ny2,nz1=nz1,nz2=nz2, &
                  fmt=fmt,type=type,data=data,offs=offs,npiece=np,cf=rf)

      if(E_IO == 0) then
        allocate(var(nx1:nx2,ny1:ny2,nz1:nz2), stat=E_IO)

        ! Read field data
        select case(vtk(rf)%f)
          case(ascii)
            if (trim(adjustlt(Upper_Case(fmt)))/='ASCII' .or. &
                trim(adjustlt(Upper_Case(type)))/='FLOAT64') then
              E_IO = -1_I4P
            else
              read(data, fmt=*, iostat=E_IO) var
            endif

          case(binary)
            if (trim(adjustlt(Upper_Case(fmt)))/='BINARY' .or. &
                trim(adjustlt(Upper_Case(type)))/='FLOAT64') then
              E_IO = -1_I4P
            else
              data=trim(adjustlt(data))
              allocate(dI1P((nx2-nx1+1)*(ny2-ny1+1)*(nz2-nz1+1)*int(BYR8P,I4P)+int(BYI4P,I4P)))
              call b64_decode(code=data,n=dI1P); if(allocated(data)) deallocate(data)
              ! Unpack data [1xI4P,(nx2-nx1+1)*(ny2-ny1+1)*(z2-nz1+1)xR8P]
              N_byte =  transfer(dI1P(1:int(BYI4P,I4P)),N_byte)
              s = size(transfer(dI1P(int(BYI4P,I4P)+1:),XYZp)); allocate(XYZp(1:s))
              XYZp = transfer(dI1P(int(BYI4P,I4P)+1:),XYZp); if(allocated(dI1P)) deallocate(dI1P)
              var(:,:,:) = reshape(XYZp, (/nx2-nx1+1,ny2-ny1+1,nz2-nz1+1/))
              if(allocated(XYZp)) deallocate(XYZp)
            endif

          case(raw)
            if(E_IO == 0) then
              if (trim(adjustlt(Upper_Case(fmt)))/='APPENDED' .or. &
                  trim(adjustlt(Upper_Case(type)))/='FLOAT64') then
                E_IO = -1_I4P
              else
                read(unit=vtk(rf)%u, iostat=E_IO, pos=vtk(rf)%ioffset+offs) N_Byte, var
              endif
          endif

        endselect
      endif
  endselect
  !---------------------------------------------------------------------------------------------------------------------------------
  end function

  function VTK_VAR_XML_SCAL_3DA_R4_READ(var_location,varname,NC_NN,NCOMP,var,npiece,cf) result(E_IO)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Function for reading field of scalar variable (R4P, 1D array).
  !---------------------------------------------------------------------------------------------------------------------------------
  implicit none
  character(*),           intent(IN)  :: var_location !< location of variables: CELL for cell-centered, NODE for node-centered
  character(*),           intent(IN)  :: varname      !< variable name
  integer(I4P),           intent(OUT) :: NC_NN        !< number of cells or nodes
  integer(I4P),           intent(OUT) :: NCOMP        !< number of components
  real(R4P), allocatable, intent(OUT) :: var(:,:,:)   !< variable to be saved
  integer(I4P), optional, intent(IN)  :: npiece       !< Number of the piece to read (by default: 1)
  integer(I4P), optional, intent(IN)  :: cf           !< Current file index (for concurrent files IO).
  integer(I4P)                        :: E_IO         !< Input/Output inquiring flag: $0$ if IO is done, $> 0$ if IO is not done
  integer(I4P)                        :: rf           !< Real file index.
  character(len=:), allocatable       :: fmt
  character(len=:), allocatable       :: type
  character(len=:), allocatable       :: data
  integer(I4P)                        :: nx1,nx2,ny1,ny2,nz1,nz2
  integer(I4P)                        :: np, offs, N_Byte, s
  integer(I1P), allocatable           :: dI1P(:)
  real(R4P), allocatable              :: XYZp(:)
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  E_IO = -1_I4P
  rf = f
  if (present(cf)) then
    rf = cf ; f = cf
  endif
  np = 1_I4P; if (present(npiece)) np = npiece

  select case(trim(vtk(rf)%topology))
    case('RectilinearGrid','StructuredGrid')
  ! Read field headers
      E_IO = VTK_VAR_XML_HEADER_READ(var_location,varname,NC_NN,NCOMP,&
                  nx1=nx1,nx2=nx2,ny1=ny1,ny2=ny2,nz1=nz1,nz2=nz2, &
                  fmt=fmt,type=type,data=data,offs=offs,npiece=np,cf=rf)

      if(E_IO == 0) then
        allocate(var(nx1:nx2,ny1:ny2,nz1:nz2), stat=E_IO)

        ! Read field data
        select case(vtk(rf)%f)
          case(ascii)
            if (trim(adjustlt(Upper_Case(fmt)))/='ASCII' .or. &
                trim(adjustlt(Upper_Case(type)))/='FLOAT32') then
              E_IO = -1_I4P
            else
              read(data, fmt=*, iostat=E_IO) var
            endif

          case(binary)
            if (trim(adjustlt(Upper_Case(fmt)))/='BINARY' .or. &
                trim(adjustlt(Upper_Case(type)))/='FLOAT32') then
              E_IO = -1_I4P
            else
              data=trim(adjustlt(data))
              allocate(dI1P((nx2-nx1+1)*(ny2-ny1+1)*(nz2-nz1+1)*int(BYR8P,I4P)+int(BYI4P,I4P)))
              call b64_decode(code=data,n=dI1P); if(allocated(data)) deallocate(data)
              ! Unpack data [1xI4P,(nx2-nx1+1)*(ny2-ny1+1)*(z2-nz1+1)xR4P]
              N_byte =  transfer(dI1P(1:int(BYI4P,I4P)),N_byte)
              s = size(transfer(dI1P(int(BYI4P,I4P)+1:),XYZp)); allocate(XYZp(1:s))
              XYZp = transfer(dI1P(int(BYI4P,I4P)+1:),XYZp); if(allocated(dI1P)) deallocate(dI1P)
              var(:,:,:) = reshape(XYZp, (/nx2-nx1+1,ny2-ny1+1,nz2-nz1+1/))
              if(allocated(XYZp)) deallocate(XYZp)
            endif

          case(raw)
            if(E_IO == 0) then
              if (trim(adjustlt(Upper_Case(fmt)))/='APPENDED' .or. &
                  trim(adjustlt(Upper_Case(type)))/='FLOAT32') then
                E_IO = -1_I4P
              else
                read(unit=vtk(rf)%u, iostat=E_IO, pos=vtk(rf)%ioffset+offs) N_Byte, var
              endif
          endif

        endselect
      endif
  end select
  !---------------------------------------------------------------------------------------------------------------------------------
  end function

  function VTK_VAR_XML_SCAL_3DA_I8_READ(var_location,varname,NC_NN,NCOMP,var,npiece,cf) result(E_IO)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Function for reading field of scalar variable (I8P, 1D array).
  !---------------------------------------------------------------------------------------------------------------------------------
  implicit none
  character(*),              intent(IN)  :: var_location !< location of variables: CELL for cell-centered, NODE for node-centered
  character(*),              intent(IN)  :: varname      !< variable name
  integer(I4P),              intent(OUT) :: NC_NN        !< number of cells or nodes
  integer(I4P),              intent(OUT) :: NCOMP        !< number of components
  integer(I8P), allocatable, intent(OUT) :: var(:,:,:)       !< variable to be saved
  integer(I4P), optional,    intent(IN)  :: npiece       !< Number of the piece to read (by default: 1)
  integer(I4P), optional,    intent(IN)  :: cf           !< Current file index (for concurrent files IO).
  integer(I4P)                           :: E_IO         !< Input/Output inquiring flag: $0$ if IO is done, $> 0$ if IO is not done
  integer(I4P)                           :: rf           !< Real file index.
  character(len=:), allocatable          :: fmt
  character(len=:), allocatable          :: type
  character(len=:), allocatable          :: data
  integer(I4P)                           :: nx1,nx2,ny1,ny2,nz1,nz2
  integer(I4P)                           :: np, offs, N_Byte, s
  integer(I1P), allocatable              :: dI1P(:)
  integer(I8P), allocatable              :: XYZp(:)
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  E_IO = -1_I4P
  rf = f
  if (present(cf)) then
    rf = cf ; f = cf
  endif
  np = 1_I4P; if (present(npiece)) np = npiece

  select case(trim(vtk(rf)%topology))
    case('RectilinearGrid','StructuredGrid')
      ! Read field headers
      E_IO = VTK_VAR_XML_HEADER_READ(var_location,varname,NC_NN,NCOMP,&
                  nx1=nx1,nx2=nx2,ny1=ny1,ny2=ny2,nz1=nz1,nz2=nz2, &
                  fmt=fmt,type=type,data=data,offs=offs,npiece=np,cf=rf)

      if(E_IO == 0) then
        allocate(var(nx1:nx2,ny1:ny2,nz1:nz2), stat=E_IO)

        ! Read field data
        select case(vtk(rf)%f)
          case(ascii)
            if (trim(adjustlt(Upper_Case(fmt)))/='ASCII' .or. &
                trim(adjustlt(Upper_Case(type)))/='INT64') then
              E_IO = -1_I4P
            else
              read(data, fmt=*, iostat=E_IO) var
            endif

          case(binary)
            if (trim(adjustlt(Upper_Case(fmt)))/='BINARY' .or. &
                trim(adjustlt(Upper_Case(type)))/='INT64') then
              E_IO = -1_I4P
            else
              data=trim(adjustlt(data))
              allocate(dI1P((nx2-nx1+1)*(ny2-ny1+1)*(nz2-nz1+1)*int(BYI8P,I4P)+int(BYI4P,I4P)))
              call b64_decode(code=data,n=dI1P); if(allocated(data)) deallocate(data)
              ! Unpack data [1xI4P,(nx2-nx1+1)*(ny2-ny1+1)*(z2-nz1+1)xI8P]
              N_byte =  transfer(dI1P(1:int(BYI4P,I4P)),N_byte)
              s = size(transfer(dI1P(int(BYI4P,I4P)+1:),XYZp)); allocate(XYZp(1:s))
              XYZp = transfer(dI1P(int(BYI4P,I4P)+1:),XYZp); if(allocated(dI1P)) deallocate(dI1P)
              var(:,:,:) = reshape(XYZp, (/nx2-nx1+1,ny2-ny1+1,nz2-nz1+1/))
              if(allocated(XYZp)) deallocate(XYZp)
            endif

          case(raw)
            if(E_IO == 0) then
              if (trim(adjustlt(Upper_Case(fmt)))/='APPENDED' .or. &
                  trim(adjustlt(Upper_Case(type)))/='INT64') then
                E_IO = -1_I4P
              else
                read(unit=vtk(rf)%u, iostat=E_IO, pos=vtk(rf)%ioffset+offs) N_Byte, var
              endif
          endif

        endselect
      endif
  endselect
  !---------------------------------------------------------------------------------------------------------------------------------
  end function

  function VTK_VAR_XML_SCAL_3DA_I4_READ(var_location,varname,NC_NN,NCOMP,var,npiece,cf) result(E_IO)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Function for reading field of scalar variable (I4P, 1D array).
  !---------------------------------------------------------------------------------------------------------------------------------
  implicit none
  character(*),              intent(IN)  :: var_location !< location of variables: CELL for cell-centered, NODE for node-centered
  character(*),              intent(IN)  :: varname      !< variable name
  integer(I4P),              intent(OUT) :: NC_NN        !< number of cells or nodes
  integer(I4P),              intent(OUT) :: NCOMP        !< number of components
  integer(I4P), allocatable, intent(OUT) :: var(:,:,:)   !< variable to be saved
  integer(I4P), optional,    intent(IN)  :: npiece       !< Number of the piece to read (by default: 1)
  integer(I4P), optional,    intent(IN)  :: cf           !< Current file index (for concurrent files IO).
  integer(I4P)                           :: E_IO         !< Input/Output inquiring flag: $0$ if IO is done, $> 0$ if IO is not done
  integer(I4P)                           :: rf           !< Real file index.
  character(len=:), allocatable          :: fmt
  character(len=:), allocatable          :: type
  character(len=:), allocatable          :: data
  integer(I4P)                           :: nx1,nx2,ny1,ny2,nz1,nz2
  integer(I4P)                           :: np, offs, N_Byte, s
  integer(I1P), allocatable              :: dI1P(:)
  integer(I4P), allocatable              :: XYZp(:)
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  E_IO = -1_I4P
  rf = f
  if (present(cf)) then
    rf = cf ; f = cf
  endif
  np = 1_I4P; if (present(npiece)) np = npiece

  select case(trim(vtk(rf)%topology))
    case('RectilinearGrid','StructuredGrid')
      ! Read field headers
      E_IO = VTK_VAR_XML_HEADER_READ(var_location,varname,NC_NN,NCOMP,&
                  nx1=nx1,nx2=nx2,ny1=ny1,ny2=ny2,nz1=nz1,nz2=nz2, &
                  fmt=fmt,type=type,data=data,offs=offs,npiece=np,cf=rf)

      if(E_IO == 0) then
        allocate(var(nx1:nx2,ny1:ny2,nz1:nz2), stat=E_IO)

        ! Read field data
        select case(vtk(rf)%f)
          case(ascii)
            if (trim(adjustlt(Upper_Case(fmt)))/='ASCII' .or. &
                trim(adjustlt(Upper_Case(type)))/='INT32') then
              E_IO = -1_I4P
            else
              read(data, fmt=*, iostat=E_IO) var
            endif

          case(binary)
            if (trim(adjustlt(Upper_Case(fmt)))/='BINARY' .or. &
                trim(adjustlt(Upper_Case(type)))/='INT32') then
              E_IO = -1_I4P
            else
              data=trim(adjustlt(data))
              allocate(dI1P((nx2-nx1+1)*(ny2-ny1+1)*(nz2-nz1+1)*int(BYI4P,I4P)+int(BYI4P,I4P)))
              call b64_decode(code=data,n=dI1P); if(allocated(data)) deallocate(data)
              ! Unpack data [1xI4P,(nx2-nx1+1)*(ny2-ny1+1)*(z2-nz1+1)xI4P]
              N_byte =  transfer(dI1P(1:int(BYI4P,I4P)),N_byte)
              s = size(transfer(dI1P(int(BYI4P,I4P)+1:),XYZp)); allocate(XYZp(1:s))
              XYZp = transfer(dI1P(int(BYI4P,I4P)+1:),XYZp); if(allocated(dI1P)) deallocate(dI1P)
                  var(:,:,:) = reshape(XYZp, (/nx2-nx1+1,ny2-ny1+1,nz2-nz1+1/))
                  if(allocated(XYZp)) deallocate(XYZp)
            endif

          case(raw)
            if(E_IO == 0) then
              if (trim(adjustlt(Upper_Case(fmt)))/='APPENDED' .or. &
                  trim(adjustlt(Upper_Case(type)))/='INT32') then
                E_IO = -1_I4P
              else
                read(unit=vtk(rf)%u, iostat=E_IO, pos=vtk(rf)%ioffset+offs) N_Byte, var
              endif
          endif

        endselect
      endif
  endselect
  !---------------------------------------------------------------------------------------------------------------------------------
  end function

  function VTK_VAR_XML_SCAL_3DA_I2_READ(var_location,varname,NC_NN,NCOMP,var,npiece,cf) result(E_IO)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Function for reading field of scalar variable (I2P, 1D array).
  !---------------------------------------------------------------------------------------------------------------------------------
  implicit none
  character(*),              intent(IN)  :: var_location !< location of variables: CELL for cell-centered, NODE for node-centered
  character(*),              intent(IN)  :: varname      !< variable name
  integer(I4P),              intent(OUT) :: NC_NN        !< number of cells or nodes
  integer(I4P),              intent(OUT) :: NCOMP        !< number of components
  integer(I2P), allocatable, intent(OUT) :: var(:,:,:)       !< variable to be saved
  integer(I4P), optional,    intent(IN)  :: npiece       !< Number of the piece to read (by default: 1)
  integer(I4P), optional,    intent(IN)  :: cf           !< Current file index (for concurrent files IO).
  integer(I4P)                           :: E_IO         !< Input/Output inquiring flag: $0$ if IO is done, $> 0$ if IO is not done
  integer(I4P)                           :: rf           !< Real file index.
  character(len=:), allocatable          :: fmt
  character(len=:), allocatable          :: type
  character(len=:), allocatable          :: data
  integer(I4P)                           :: nx1,nx2,ny1,ny2,nz1,nz2
  integer(I4P)                           :: np, offs, N_Byte, s
  integer(I1P), allocatable              :: dI1P(:)
  integer(I2P), allocatable              :: XYZp(:)
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  E_IO = -1_I4P
  rf = f
  if (present(cf)) then
    rf = cf ; f = cf
  endif
  np = 1_I4P; if (present(npiece)) np = npiece

  select case(trim(vtk(rf)%topology))
    case('RectilinearGrid','StructuredGrid')
      ! Read field headers
      E_IO = VTK_VAR_XML_HEADER_READ(var_location,varname,NC_NN,NCOMP,&
                  nx1=nx1,nx2=nx2,ny1=ny1,ny2=ny2,nz1=nz1,nz2=nz2, &
                  fmt=fmt,type=type,data=data,offs=offs,npiece=np,cf=rf)

      if(E_IO == 0) then
        allocate(var(nx1:nx2,ny1:ny2,nz1:nz2), stat=E_IO)

        ! Read field data
        select case(vtk(rf)%f)
          case(ascii)
            if (trim(adjustlt(Upper_Case(fmt)))/='ASCII' .or. &
                trim(adjustlt(Upper_Case(type)))/='INT16') then
              E_IO = -1_I4P
            else
              read(data, fmt=*, iostat=E_IO) var
            endif

          case(binary)
            if (trim(adjustlt(Upper_Case(fmt)))/='BINARY' .or. &
                trim(adjustlt(Upper_Case(type)))/='INT16') then
              E_IO = -1_I4P
            else
              data=trim(adjustlt(data))
              allocate(dI1P((nx2-nx1+1)*(ny2-ny1+1)*(nz2-nz1+1)*int(BYI2P,I4P)+int(BYI4P,I4P)))
              call b64_decode(code=data,n=dI1P); if(allocated(data)) deallocate(data)
              ! Unpack data [1xI4P,(nx2-nx1+1)*(ny2-ny1+1)*(z2-nz1+1)xI2P]
              N_byte =  transfer(dI1P(1:int(BYI4P,I4P)),N_byte)
              s = size(transfer(dI1P(int(BYI4P,I4P)+1:),XYZp)); allocate(XYZp(1:s))
              XYZp = transfer(dI1P(int(BYI4P,I4P)+1:),XYZp); if(allocated(dI1P)) deallocate(dI1P)
              var(:,:,:) = reshape(XYZp, (/nx2-nx1+1,ny2-ny1+1,nz2-nz1+1/))
              if(allocated(XYZp)) deallocate(XYZp)
            endif

          case(raw)
            if(E_IO == 0) then
              if (trim(adjustlt(Upper_Case(fmt)))/='APPENDED' .or. &
                  trim(adjustlt(Upper_Case(type)))/='INT16') then
                E_IO = -1_I4P
              else
                read(unit=vtk(rf)%u, iostat=E_IO, pos=vtk(rf)%ioffset+offs) N_Byte, var
              endif
          endif

        endselect
      endif
  endselect
  !---------------------------------------------------------------------------------------------------------------------------------
  end function

  function VTK_VAR_XML_SCAL_3DA_I1_READ(var_location,varname,NC_NN,NCOMP,var,npiece,cf) result(E_IO)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Function for reading field of scalar variable (I1P, 1D array).
  !---------------------------------------------------------------------------------------------------------------------------------
  implicit none
  character(*),              intent(IN)  :: var_location !< location of variables: CELL for cell-centered, NODE for node-centered
  character(*),              intent(IN)  :: varname      !< variable name
  integer(I4P),              intent(OUT) :: NC_NN        !< number of cells or nodes
  integer(I4P),              intent(OUT) :: NCOMP        !< number of components
  integer(I1P), allocatable, intent(OUT) :: var(:,:,:)   !< variable to be saved
  integer(I4P), optional,    intent(IN)  :: npiece       !< Number of the piece to read (by default: 1)
  integer(I4P), optional,    intent(IN)  :: cf           !< Current file index (for concurrent files IO).
  integer(I4P)                           :: E_IO         !< Input/Output inquiring flag: $0$ if IO is done, $> 0$ if IO is not done
  integer(I4P)                           :: rf           !< Real file index.
  character(len=:), allocatable          :: fmt
  character(len=:), allocatable          :: type
  character(len=:), allocatable          :: data
  integer(I4P)                           :: nx1,nx2,ny1,ny2,nz1,nz2
  integer(I4P)                           :: np, offs, N_Byte, s
  integer(I1P), allocatable              :: dI1P(:)
  integer(I1P), allocatable              :: XYZp(:)
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  E_IO = -1_I4P
  rf = f
  if (present(cf)) then
    rf = cf ; f = cf
  endif
  np = 1_I4P; if (present(npiece)) np = npiece

  select case(trim(vtk(rf)%topology))
    case('RectilinearGrid','StructuredGrid')
      ! Read field headers
      E_IO = VTK_VAR_XML_HEADER_READ(var_location,varname,NC_NN,NCOMP,&
                  nx1=nx1,nx2=nx2,ny1=ny1,ny2=ny2,nz1=nz1,nz2=nz2, &
                  fmt=fmt,type=type,data=data,offs=offs,npiece=np,cf=rf)

      if(E_IO == 0) then
        allocate(var(nx1:nx2,ny1:ny2,nz1:nz2), stat=E_IO)

        ! Read field data
        select case(vtk(rf)%f)
          case(ascii)
            if (trim(adjustlt(Upper_Case(fmt)))/='ASCII' .or. &
                trim(adjustlt(Upper_Case(type)))/='INT8') then
              E_IO = -1_I4P
            else
              read(data, fmt=*, iostat=E_IO) var
            endif

          case(binary)
            if (trim(adjustlt(Upper_Case(fmt)))/='BINARY' .or. &
                trim(adjustlt(Upper_Case(type)))/='INT8') then
              E_IO = -1_I4P
            else
              data=trim(adjustlt(data))
              allocate(dI1P((nx2-nx1+1)*(ny2-ny1+1)*(nz2-nz1+1)*int(BYI1P,I4P)+int(BYI4P,I4P)))
              call b64_decode(code=data,n=dI1P); if(allocated(data)) deallocate(data)
              ! Unpack data [1xI4P,(nx2-nx1+1)*(ny2-ny1+1)*(z2-nz1+1)xI1P]
              N_byte =  transfer(dI1P(1:int(BYI4P,I4P)),N_byte)
              s = size(transfer(dI1P(int(BYI4P,I4P)+1:),XYZp)); allocate(XYZp(1:s))
              XYZp = transfer(dI1P(int(BYI4P,I4P)+1:),XYZp); if(allocated(dI1P)) deallocate(dI1P)
              var(:,:,:) = reshape(XYZp, (/nx2-nx1+1,ny2-ny1+1,nz2-nz1+1/))
              if(allocated(XYZp)) deallocate(XYZp)
            endif

          case(raw)
            if(E_IO == 0) then
              if (trim(adjustlt(Upper_Case(fmt)))/='APPENDED' .or. &
                  trim(adjustlt(Upper_Case(type)))/='INT8') then
                E_IO = -1_I4P
              else
                read(unit=vtk(rf)%u, iostat=E_IO, pos=vtk(rf)%ioffset+offs) N_Byte, var
              endif
            endif

        endselect
      endif
  end select
  !---------------------------------------------------------------------------------------------------------------------------------
  end function

  function VTK_VAR_XML_VECT_3DA_R8_READ(var_location,varname,NC_NN,NCOMP,varX,varY,varZ,npiece,cf) result(E_IO)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Function for reading field of scalar variable (R8P, 1D array).
  !---------------------------------------------------------------------------------------------------------------------------------
  implicit none
  character(*),           intent(IN)  :: var_location !< location of variables: CELL for cell-centered, NODE for node-centered
  character(*),           intent(IN)  :: varname      !< variable name
  integer(I4P),           intent(OUT) :: NC_NN        !< number of cells or nodes
  integer(I4P),           intent(OUT) :: NCOMP        !< number of components
  real(R8P), allocatable, intent(OUT) :: varX(:,:,:)  !< variable to be saved
  real(R8P), allocatable, intent(OUT) :: varY(:,:,:)  !< variable to be saved
  real(R8P), allocatable, intent(OUT) :: varZ(:,:,:)  !< variable to be saved
  integer(I4P), optional, intent(IN)  :: npiece       !< Number of the piece to read (by default: 1)
  integer(I4P), optional, intent(IN)  :: cf           !< Current file index (for concurrent files IO).
  integer(I4P)                        :: E_IO         !< Input/Output inquiring flag: $0$ if IO is done, $> 0$ if IO is not done
  integer(I4P)                        :: rf           !< Real file index.
  character(len=:), allocatable       :: fmt
  character(len=:), allocatable       :: type
  character(len=:), allocatable       :: data
  integer(I4P)                        :: np, offs, N_Byte
  integer(I4P)                        :: nx1,nx2,ny1,ny2,nz1,nz2
  integer(I4P)                        :: i,j,k,s
  integer(I1P), allocatable           :: dI1P(:)
  real(R8P), allocatable              :: XYZp(:)
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  E_IO = -1_I4P
  rf = f
  if (present(cf)) then
    rf = cf ; f = cf
  endif
  np = 1_I4P; if (present(npiece)) np = npiece

  select case(trim(vtk(rf)%topology))
    case('RectilinearGrid','StructuredGrid')
      ! Read field headers
      E_IO = VTK_VAR_XML_HEADER_READ(var_location,varname,NC_NN,NCOMP,&
                  nx1=nx1,nx2=nx2,ny1=ny1,ny2=ny2,nz1=nz1,nz2=nz2, &
                  fmt=fmt,type=type,data=data,offs=offs,npiece=np,cf=rf)
      if(NCOMP/=3) E_IO=-1_I4P

      if(E_IO == 0) then
        allocate(varX(nx1:nx2,ny1:ny2,nz1:nz2), varY(nx1:nx2,ny1:ny2,nz1:nz2), &
                 varZ(nx1:nx2,ny1:ny2,nz1:nz2), stat=E_IO)

        ! Read field data
        select case(vtk(rf)%f)
          case(ascii)
            if (trim(adjustlt(Upper_Case(fmt)))/='ASCII' .or. &
                trim(adjustlt(Upper_Case(type)))/='FLOAT64') then
              E_IO = -1_I4P
            else
              read(data, fmt=*, iostat=E_IO) &
                  (((varX(i,j,k),varY(i,j,k),varZ(i,j,k), i=nx1,nx2), j=ny1,ny2), k=nz1,nz2)
            endif

          case(binary)
            if (trim(adjustlt(Upper_Case(fmt)))/='BINARY' .or. &
                trim(adjustlt(Upper_Case(type)))/='FLOAT64') then
              E_IO = -1_I4P
            else
              read(data, fmt=*, iostat=E_IO) &
                  (((varX(i,j,k),varY(i,j,k),varZ(i,j,k), i=nx1,nx2), j=ny1,ny2), k=nz1,nz2)
            endif
            ! Decode base64 packed data
            data=trim(adjustlt(data))
            allocate(dI1P(NCOMP*(nx2-nx1+1)*(ny2-ny1+1)*(nz2-nz1+1)*int(BYR8P,I4P)+int(BYI4P,I4P)))
            call b64_decode(code=data,n=dI1P); if(allocated(data)) deallocate(data)
            ! Unpack data [1xI4P,NCOMP*(nx2-nx1+1)*(ny2-ny1+1)*(nz2-nz1+1)xR8P]
            N_byte =  transfer(dI1P(1:int(BYI4P,I4P)),N_byte)
            s = size(transfer(dI1P(int(BYI4P,I4P)+1:),XYZp)); allocate(XYZp(1:s))
            XYZp = transfer(dI1P(int(BYI4P,I4P)+1:),XYZp); if(allocated(dI1P)) deallocate(dI1P)
            s=1;do k=nz1,nz2-nz1;do j=ny1,ny2;do i=nx1,nx2;
              varX(i,j,k)=XYZp(s); s=s+1; varY(i,j,k)=XYZp(s); s=s+1; varZ(i,j,k)=XYZp(s); s=s+1
            enddo;enddo;enddo;
            if(allocated(XYZp)) deallocate(XYZp)

          case(raw)
            if(E_IO == 0) then
              if (trim(adjustlt(Upper_Case(fmt)))/='APPENDED' .or. &
                  trim(adjustlt(Upper_Case(type)))/='FLOAT64') then
                E_IO = -1_I4P
              else
                read(unit=vtk(rf)%u, iostat=E_IO, pos=vtk(rf)%ioffset+offs) N_Byte, &
                  (((varX(i,j,k),varY(i,j,k),varZ(i,j,k), i=nx1,nx2), j=ny1,ny2), k=nz1,nz2)
              endif
          endif

        endselect
      endif
  endselect
  !---------------------------------------------------------------------------------------------------------------------------------
  end function

  function VTK_VAR_XML_VECT_3DA_R4_READ(var_location,varname,NC_NN,NCOMP,varX,varY,varZ,npiece,cf) result(E_IO)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Function for reading field of scalar variable (R4P, 1D array).
  !---------------------------------------------------------------------------------------------------------------------------------
  implicit none
  character(*),           intent(IN)  :: var_location !< location of variables: CELL for cell-centered, NODE for node-centered
  character(*),           intent(IN)  :: varname      !< variable name
  integer(I4P),           intent(OUT) :: NC_NN        !< number of cells or nodes
  integer(I4P),           intent(OUT) :: NCOMP        !< number of components
  real(R4P), allocatable, intent(OUT) :: varX(:,:,:)  !< variable to be saved
  real(R4P), allocatable, intent(OUT) :: varY(:,:,:)  !< variable to be saved
  real(R4P), allocatable, intent(OUT) :: varZ(:,:,:)  !< variable to be saved
  integer(I4P), optional, intent(IN)  :: npiece       !< Number of the piece to read (by default: 1)
  integer(I4P), optional, intent(IN)  :: cf           !< Current file index (for concurrent files IO).
  integer(I4P)                        :: E_IO         !< Input/Output inquiring flag: $0$ if IO is done, $> 0$ if IO is not done
  integer(I4P)                        :: rf           !< Real file index.
  character(len=:), allocatable       :: fmt
  character(len=:), allocatable       :: type
  character(len=:), allocatable       :: data
  integer(I4P)                        :: np, offs, N_Byte
  integer(I4P)                        :: nx1,nx2,ny1,ny2,nz1,nz2
  integer(I4P)                        :: i,j,k,s
  integer(I1P), allocatable           :: dI1P(:)
  real(R4P), allocatable              :: XYZp(:)
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  E_IO = -1_I4P
  rf = f
  if (present(cf)) then
    rf = cf ; f = cf
  endif
  np = 1_I4P; if (present(npiece)) np = npiece

  select case(trim(vtk(rf)%topology))
    case('RectilinearGrid','StructuredGrid')
      ! Read field headers
      E_IO = VTK_VAR_XML_HEADER_READ(var_location,varname,NC_NN,NCOMP,&
                  nx1=nx1,nx2=nx2,ny1=ny1,ny2=ny2,nz1=nz1,nz2=nz2, &
                  fmt=fmt,type=type,data=data,offs=offs,npiece=np,cf=rf)
      if(NCOMP/=3) E_IO=-1_I4P

      if(E_IO == 0) then
        allocate(varX(nx1:nx2,ny1:ny2,nz1:nz2), varY(nx1:nx2,ny1:ny2,nz1:nz2), &
                 varZ(nx1:nx2,ny1:ny2,nz1:nz2), stat=E_IO)

        ! Read field data
        select case(vtk(rf)%f)
          case(ascii)
            if (trim(adjustlt(Upper_Case(fmt)))/='ASCII' .or. &
                trim(adjustlt(Upper_Case(type)))/='FLOAT32') then
              E_IO = -1_I4P
            else
              read(data, fmt=*, iostat=E_IO) &
                  (((varX(i,j,k),varY(i,j,k),varZ(i,j,k), i=nx1,nx2), j=ny1,ny2), k=nz1,nz2)
            endif

          case(binary)
            if (trim(adjustlt(Upper_Case(fmt)))/='BINARY' .or. &
                trim(adjustlt(Upper_Case(type)))/='FLOAT32') then
              E_IO = -1_I4P
            else
              read(data, fmt=*, iostat=E_IO) &
                  (((varX(i,j,k),varY(i,j,k),varZ(i,j,k), i=nx1,nx2), j=ny1,ny2), k=nz1,nz2)
            endif
            ! Decode base64 packed data
            data=trim(adjustlt(data))
            allocate(dI1P(NCOMP*(nx2-nx1+1)*(ny2-ny1+1)*(nz2-nz1+1)*int(BYR4P,I4P)+int(BYI4P,I4P)))
            call b64_decode(code=data,n=dI1P); if(allocated(data)) deallocate(data)
            ! Unpack data [1xI4P,NCOMP*(nx2-nx1+1)*(ny2-ny1+1)*(nz2-nz1+1)xR4P]
            N_byte =  transfer(dI1P(1:int(BYI4P,I4P)),N_byte)
            s = size(transfer(dI1P(int(BYI4P,I4P)+1:),XYZp)); allocate(XYZp(1:s))
            XYZp = transfer(dI1P(int(BYI4P,I4P)+1:),XYZp); if(allocated(dI1P)) deallocate(dI1P)
            s=1;do k=nz1,nz2-nz1;do j=ny1,ny2;do i=nx1,nx2;
              varX(i,j,k)=XYZp(s); s=s+1; varY(i,j,k)=XYZp(s); s=s+1; varZ(i,j,k)=XYZp(s); s=s+1
            enddo;enddo;enddo;
            if(allocated(XYZp)) deallocate(XYZp)

          case(raw)
            if(E_IO == 0) then
              if (trim(adjustlt(Upper_Case(fmt)))/='APPENDED' .or. &
                  trim(adjustlt(Upper_Case(type)))/='FLOAT32') then
                E_IO = -1_I4P
              else
                read(unit=vtk(rf)%u, iostat=E_IO, pos=vtk(rf)%ioffset+offs) N_Byte, &
                  (((varX(i,j,k),varY(i,j,k),varZ(i,j,k), i=nx1,nx2), j=ny1,ny2), k=nz1,nz2)
              endif
          endif

        endselect
      endif
  endselect
  !---------------------------------------------------------------------------------------------------------------------------------
  end function

  function VTK_VAR_XML_VECT_3DA_I8_READ(var_location,varname,NC_NN,NCOMP,varX,varY,varZ,npiece,cf) result(E_IO)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Function for reading field of scalar variable (I8P, 1D array).
  !---------------------------------------------------------------------------------------------------------------------------------
  implicit none
  character(*),              intent(IN)  :: var_location !< location of variables: CELL for cell-centered, NODE for node-centered
  character(*),              intent(IN)  :: varname      !< variable name
  integer(I4P),              intent(OUT) :: NC_NN        !< number of cells or nodes
  integer(I4P),              intent(OUT) :: NCOMP        !< number of components
  integer(I8P), allocatable, intent(OUT) :: varX(:,:,:)  !< variable to be saved
  integer(I8P), allocatable, intent(OUT) :: varY(:,:,:)  !< variable to be saved
  integer(I8P), allocatable, intent(OUT) :: varZ(:,:,:)  !< variable to be saved
  integer(I4P), optional,    intent(IN)  :: npiece       !< Number of the piece to read (by default: 1)
  integer(I4P), optional,    intent(IN)  :: cf           !< Current file index (for concurrent files IO).
  integer(I4P)                           :: E_IO         !< Input/Output inquiring flag: $0$ if IO is done, $> 0$ if IO is not done
  integer(I4P)                           :: rf           !< Real file index.
  character(len=:), allocatable          :: fmt
  character(len=:), allocatable          :: type
  character(len=:), allocatable          :: data
  integer(I4P)                           :: np, offs, N_Byte
  integer(I4P)                           :: nx1,nx2,ny1,ny2,nz1,nz2
  integer(I4P)                           :: i,j,k,s
  integer(I1P), allocatable              :: dI1P(:)
  integer(I8P), allocatable              :: XYZp(:)
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  E_IO = -1_I4P
  rf = f
  if (present(cf)) then
    rf = cf ; f = cf
  endif
  np = 1_I4P; if (present(npiece)) np = npiece
  select case(trim(vtk(rf)%topology))
    case('RectilinearGrid','StructuredGrid')
      ! Read field headers
      E_IO = VTK_VAR_XML_HEADER_READ(var_location,varname,NC_NN,NCOMP,&
                  nx1=nx1,nx2=nx2,ny1=ny1,ny2=ny2,nz1=nz1,nz2=nz2, &
                  fmt=fmt,type=type,data=data,offs=offs,npiece=np,cf=rf)
      if(NCOMP/=3) E_IO=-1_I4P

      if(E_IO == 0) then
        allocate(varX(nx1:nx2,ny1:ny2,nz1:nz2), varY(nx1:nx2,ny1:ny2,nz1:nz2), &
                 varZ(nx1:nx2,ny1:ny2,nz1:nz2), stat=E_IO)

        ! Read field data
        select case(vtk(rf)%f)
          case(ascii)
            if (trim(adjustlt(Upper_Case(fmt)))/='ASCII' .or. &
                trim(adjustlt(Upper_Case(type)))/='INT64') then
              E_IO = -1_I4P
            else
              read(data, fmt=*, iostat=E_IO) &
                  (((varX(i,j,k),varY(i,j,k),varZ(i,j,k), i=nx1,nx2), j=ny1,ny2), k=nz1,nz2)
            endif

          case(binary)
            if (trim(adjustlt(Upper_Case(fmt)))/='BINARY' .or. &
                trim(adjustlt(Upper_Case(type)))/='INT64') then
              E_IO = -1_I4P
            else
              read(data, fmt=*, iostat=E_IO) &
                  (((varX(i,j,k),varY(i,j,k),varZ(i,j,k), i=nx1,nx2), j=ny1,ny2), k=nz1,nz2)
            endif
            ! Decode base64 packed data
            data=trim(adjustlt(data))
            allocate(dI1P(NCOMP*(nx2-nx1+1)*(ny2-ny1+1)*(nz2-nz1+1)*int(BYI8P,I4P)+int(BYI4P,I4P)))
            call b64_decode(code=data,n=dI1P); if(allocated(data)) deallocate(data)
            ! Unpack data [1xI4P,NCOMP*(nx2-nx1+1)*(ny2-ny1+1)*(nz2-nz1+1)xI8P]
            N_byte =  transfer(dI1P(1:int(BYI4P,I4P)),N_byte)
            s = size(transfer(dI1P(int(BYI4P,I4P)+1:),XYZp)); allocate(XYZp(1:s))
            XYZp = transfer(dI1P(int(BYI4P,I4P)+1:),XYZp); if(allocated(dI1P)) deallocate(dI1P)
            s=1;do k=nz1,nz2-nz1;do j=ny1,ny2;do i=nx1,nx2;
              varX(i,j,k)=XYZp(s); s=s+1; varY(i,j,k)=XYZp(s); s=s+1; varZ(i,j,k)=XYZp(s); s=s+1
            enddo;enddo;enddo;
            if(allocated(XYZp)) deallocate(XYZp)

          case(raw)
            if(E_IO == 0) then
              if (trim(adjustlt(Upper_Case(fmt)))/='APPENDED' .or. &
                  trim(adjustlt(Upper_Case(type)))/='INT64') then
                E_IO = -1_I4P
              else
                read(unit=vtk(rf)%u, iostat=E_IO, pos=vtk(rf)%ioffset+offs) N_Byte, &
                  (((varX(i,j,k),varY(i,j,k),varZ(i,j,k), i=nx1,nx2), j=ny1,ny2), k=nz1,nz2)
              endif
          endif

        endselect
      endif
  endselect
  !---------------------------------------------------------------------------------------------------------------------------------
  end function

  function VTK_VAR_XML_VECT_3DA_I4_READ(var_location,varname,NC_NN,NCOMP,varX,varY,varZ,npiece,cf) result(E_IO)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Function for reading field of scalar variable (I4P, 1D array).
  !---------------------------------------------------------------------------------------------------------------------------------
  implicit none
  character(*),              intent(IN)  :: var_location !< location of variables: CELL for cell-centered, NODE for node-centered
  character(*),              intent(IN)  :: varname      !< variable name
  integer(I4P),              intent(OUT) :: NC_NN        !< number of cells or nodes
  integer(I4P),              intent(OUT) :: NCOMP        !< number of components
  integer(I4P), allocatable, intent(OUT) :: varX(:,:,:)  !< variable to be saved
  integer(I4P), allocatable, intent(OUT) :: varY(:,:,:)  !< variable to be saved
  integer(I4P), allocatable, intent(OUT) :: varZ(:,:,:)  !< variable to be saved
  integer(I4P), optional,    intent(IN)  :: npiece       !< Number of the piece to read (by default: 1)
  integer(I4P), optional,    intent(IN)  :: cf           !< Current file index (for concurrent files IO).
  integer(I4P)                           :: E_IO         !< Input/Output inquiring flag: $0$ if IO is done, $> 0$ if IO is not done
  integer(I4P)                           :: rf           !< Real file index.
  character(len=:), allocatable          :: fmt
  character(len=:), allocatable          :: type
  character(len=:), allocatable          :: data
  integer(I4P)                           :: np, offs, N_Byte
  integer(I4P)                           :: nx1,nx2,ny1,ny2,nz1,nz2
  integer(I4P)                           :: i,j,k,s
  integer(I1P), allocatable              :: dI1P(:)
  integer(I4P), allocatable              :: XYZp(:)
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  E_IO = -1_I4P
  rf = f
  if (present(cf)) then
    rf = cf ; f = cf
  endif
  np = 1_I4P; if (present(npiece)) np = npiece

  select case(trim(vtk(rf)%topology))
    case('RectilinearGrid','StructuredGrid')
      ! Read field headers
      E_IO = VTK_VAR_XML_HEADER_READ(var_location,varname,NC_NN,NCOMP,&
                  nx1=nx1,nx2=nx2,ny1=ny1,ny2=ny2,nz1=nz1,nz2=nz2, &
                  fmt=fmt,type=type,data=data,offs=offs,npiece=np,cf=rf)
      if(NCOMP/=3) E_IO=-1_I4P

      if(E_IO == 0) then
        allocate(varX(nx1:nx2,ny1:ny2,nz1:nz2), varY(nx1:nx2,ny1:ny2,nz1:nz2), &
                 varZ(nx1:nx2,ny1:ny2,nz1:nz2), stat=E_IO)

        ! Read field data
        select case(vtk(rf)%f)
          case(ascii)
            if (trim(adjustlt(Upper_Case(fmt)))/='ASCII' .or. &
                trim(adjustlt(Upper_Case(type)))/='INT32') then
              E_IO = -1_I4P
            else
              read(data, fmt=*, iostat=E_IO) &
                  (((varX(i,j,k),varY(i,j,k),varZ(i,j,k), i=nx1,nx2), j=ny1,ny2), k=nz1,nz2)
            endif

          case(binary)
            if (trim(adjustlt(Upper_Case(fmt)))/='BINARY' .or. &
                trim(adjustlt(Upper_Case(type)))/='INT32') then
              E_IO = -1_I4P
            else
              read(data, fmt=*, iostat=E_IO) &
                  (((varX(i,j,k),varY(i,j,k),varZ(i,j,k), i=nx1,nx2), j=ny1,ny2), k=nz1,nz2)
            endif
            ! Decode base64 packed data
            data=trim(adjustlt(data))
            allocate(dI1P(NCOMP*(nx2-nx1+1)*(ny2-ny1+1)*(nz2-nz1+1)*int(BYI4P,I4P)+int(BYI4P,I4P)))
            call b64_decode(code=data,n=dI1P); if(allocated(data)) deallocate(data)
            ! Unpack data [1xI4P,NCOMP*(nx2-nx1+1)*(ny2-ny1+1)*(nz2-nz1+1)xI4P]
            N_byte =  transfer(dI1P(1:int(BYI4P,I4P)),N_byte)
            s = size(transfer(dI1P(int(BYI4P,I4P)+1:),XYZp)); allocate(XYZp(1:s))
            XYZp = transfer(dI1P(int(BYI4P,I4P)+1:),XYZp); if(allocated(dI1P)) deallocate(dI1P)
            s=1;do k=nz1,nz2-nz1;do j=ny1,ny2;do i=nx1,nx2;
              varX(i,j,k)=XYZp(s); s=s+1; varY(i,j,k)=XYZp(s); s=s+1; varZ(i,j,k)=XYZp(s); s=s+1
            enddo;enddo;enddo;
            if(allocated(XYZp)) deallocate(XYZp)

          case(raw)
            if(E_IO == 0) then
              if (trim(adjustlt(Upper_Case(fmt)))/='APPENDED' .or. &
                  trim(adjustlt(Upper_Case(type)))/='INT32') then
                E_IO = -1_I4P
              else
                read(unit=vtk(rf)%u, iostat=E_IO, pos=vtk(rf)%ioffset+offs) N_Byte, &
                  (((varX(i,j,k),varY(i,j,k),varZ(i,j,k), i=nx1,nx2), j=ny1,ny2), k=nz1,nz2)
              endif
          endif

        endselect
      endif
  endselect
  !---------------------------------------------------------------------------------------------------------------------------------
  end function

  function VTK_VAR_XML_VECT_3DA_I2_READ(var_location,varname,NC_NN,NCOMP,varX,varY,varZ,npiece,cf) result(E_IO)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Function for reading field of scalar variable (I2P, 1D array).
  !---------------------------------------------------------------------------------------------------------------------------------
  implicit none
  character(*),              intent(IN)  :: var_location !< location of variables: CELL for cell-centered, NODE for node-centered
  character(*),              intent(IN)  :: varname      !< variable name
  integer(I4P),              intent(OUT) :: NC_NN        !< number of cells or nodes
  integer(I4P),              intent(OUT) :: NCOMP        !< number of components
  integer(I2P), allocatable, intent(OUT) :: varX(:,:,:)  !< variable to be saved
  integer(I2P), allocatable, intent(OUT) :: varY(:,:,:)  !< variable to be saved
  integer(I2P), allocatable, intent(OUT) :: varZ(:,:,:)  !< variable to be saved
  integer(I4P), optional,    intent(IN)  :: npiece       !< Number of the piece to read (by default: 1)
  integer(I4P), optional,    intent(IN)  :: cf           !< Current file index (for concurrent files IO).
  integer(I4P)                           :: E_IO         !< Input/Output inquiring flag: $0$ if IO is done, $> 0$ if IO is not done
  integer(I4P)                           :: rf           !< Real file index.
  character(len=:), allocatable          :: fmt
  character(len=:), allocatable          :: type
  character(len=:), allocatable          :: data
  integer(I4P)                           :: np, offs, N_Byte
  integer(I4P)                           :: nx1,nx2,ny1,ny2,nz1,nz2
  integer(I4P)                           :: i,j,k,s
  integer(I1P), allocatable              :: dI1P(:)
  integer(I2P), allocatable              :: XYZp(:)
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  E_IO = -1_I4P
  rf = f
  if (present(cf)) then
    rf = cf ; f = cf
  endif
  np = 1_I4P; if (present(npiece)) np = npiece

  select case(trim(vtk(rf)%topology))
    case('RectilinearGrid','StructuredGrid')
      ! Read field headers
      E_IO = VTK_VAR_XML_HEADER_READ(var_location,varname,NC_NN,NCOMP,&
                  nx1=nx1,nx2=nx2,ny1=ny1,ny2=ny2,nz1=nz1,nz2=nz2, &
                  fmt=fmt,type=type,data=data,offs=offs,npiece=np,cf=rf)
      if(NCOMP/=3) E_IO=-1_I4P

      if(E_IO == 0) then
        allocate(varX(nx1:nx2,ny1:ny2,nz1:nz2), varY(nx1:nx2,ny1:ny2,nz1:nz2), &
                 varZ(nx1:nx2,ny1:ny2,nz1:nz2), stat=E_IO)

        ! Read field data
        select case(vtk(rf)%f)
          case(ascii)
            if (trim(adjustlt(Upper_Case(fmt)))/='ASCII' .or. &
                trim(adjustlt(Upper_Case(type)))/='INT16') then
              E_IO = -1_I4P
            else
              read(data, fmt=*, iostat=E_IO) &
                  (((varX(i,j,k),varY(i,j,k),varZ(i,j,k), i=nx1,nx2), j=ny1,ny2), k=nz1,nz2)
            endif

          case(binary)
            if (trim(adjustlt(Upper_Case(fmt)))/='BINARY' .or. &
                trim(adjustlt(Upper_Case(type)))/='INT16') then
              E_IO = -1_I4P
            else
              read(data, fmt=*, iostat=E_IO) &
                  (((varX(i,j,k),varY(i,j,k),varZ(i,j,k), i=nx1,nx2), j=ny1,ny2), k=nz1,nz2)
            endif
            ! Decode base64 packed data
            data=trim(adjustlt(data))
            allocate(dI1P(NCOMP*(nx2-nx1+1)*(ny2-ny1+1)*(nz2-nz1+1)*int(BYI2P,I4P)+int(BYI4P,I4P)))
            call b64_decode(code=data,n=dI1P); if(allocated(data)) deallocate(data)
            ! Unpack data [1xI4P,NCOMP*(nx2-nx1+1)*(ny2-ny1+1)*(nz2-nz1+1)xI2P]
            N_byte =  transfer(dI1P(1:int(BYI4P,I4P)),N_byte)
            s = size(transfer(dI1P(int(BYI4P,I4P)+1:),XYZp)); allocate(XYZp(1:s))
            XYZp = transfer(dI1P(int(BYI4P,I4P)+1:),XYZp); if(allocated(dI1P)) deallocate(dI1P)
            s=1;do k=nz1,nz2-nz1;do j=ny1,ny2;do i=nx1,nx2;
              varX(i,j,k)=XYZp(s); s=s+1; varY(i,j,k)=XYZp(s); s=s+1; varZ(i,j,k)=XYZp(s); s=s+1
            enddo;enddo;enddo;
            if(allocated(XYZp)) deallocate(XYZp)

          case(raw)
            if(E_IO == 0) then
              if (trim(adjustlt(Upper_Case(fmt)))/='APPENDED' .or. &
                  trim(adjustlt(Upper_Case(type)))/='INT16') then
                E_IO = -1_I4P
              else
                read(unit=vtk(rf)%u, iostat=E_IO, pos=vtk(rf)%ioffset+offs) N_Byte, &
                  (((varX(i,j,k),varY(i,j,k),varZ(i,j,k), i=nx1,nx2), j=ny1,ny2), k=nz1,nz2)
              endif
          endif

        endselect
      endif
  endselect
  !---------------------------------------------------------------------------------------------------------------------------------
  end function

  function VTK_VAR_XML_VECT_3DA_I1_READ(var_location,varname,NC_NN,NCOMP,varX,varY,varZ,npiece,cf) result(E_IO)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Function for reading field of scalar variable (I1P, 1D array).
  !---------------------------------------------------------------------------------------------------------------------------------
  implicit none
  character(*),              intent(IN)  :: var_location !< location of variables: CELL for cell-centered, NODE for node-centered
  character(*),              intent(IN)  :: varname      !< variable name
  integer(I4P),              intent(OUT) :: NC_NN        !< number of cells or nodes
  integer(I4P),              intent(OUT) :: NCOMP        !< number of components
  integer(I1P), allocatable, intent(OUT) :: varX(:,:,:)  !< variable to be saved
  integer(I1P), allocatable, intent(OUT) :: varY(:,:,:)  !< variable to be saved
  integer(I1P), allocatable, intent(OUT) :: varZ(:,:,:)  !< variable to be saved
  integer(I4P), optional,    intent(IN)  :: npiece       !< Number of the piece to read (by default: 1)
  integer(I4P), optional,    intent(IN)  :: cf           !< Current file index (for concurrent files IO).
  integer(I4P)                           :: E_IO         !< Input/Output inquiring flag: $0$ if IO is done, $> 0$ if IO is not done
  integer(I4P)                           :: rf           !< Real file index.
  character(len=:), allocatable          :: fmt
  character(len=:), allocatable          :: type
  character(len=:), allocatable          :: data
  integer(I4P)                           :: np, offs, N_Byte
  integer(I4P)                           :: nx1,nx2,ny1,ny2,nz1,nz2
  integer(I4P)                           :: i,j,k,s
  integer(I1P), allocatable              :: dI1P(:)
  integer(I1P), allocatable              :: XYZp(:)
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  E_IO = -1_I4P
  rf = f
  if (present(cf)) then
    rf = cf ; f = cf
  endif
  np = 1_I4P; if (present(npiece)) np = npiece

  select case(trim(vtk(rf)%topology))
    case('RectilinearGrid','StructuredGrid')
      ! Read field headers
      E_IO = VTK_VAR_XML_HEADER_READ(var_location,varname,NC_NN,NCOMP,&
                  nx1=nx1,nx2=nx2,ny1=ny1,ny2=ny2,nz1=nz1,nz2=nz2, &
                  fmt=fmt,type=type,data=data,offs=offs,npiece=np,cf=rf)
      if(NCOMP/=3) E_IO=-1_I4P

      if(E_IO == 0) then
        allocate(varX(nx1:nx2,ny1:ny2,nz1:nz2), varY(nx1:nx2,ny1:ny2,nz1:nz2), &
                 varZ(nx1:nx2,ny1:ny2,nz1:nz2), stat=E_IO)

        ! Read field data
        select case(vtk(rf)%f)
          case(ascii)
            if (trim(adjustlt(Upper_Case(fmt)))/='ASCII' .or. &
                trim(adjustlt(Upper_Case(type)))/='INT8') then
              E_IO = -1_I4P
            else
              read(data, fmt=*, iostat=E_IO) &
                  (((varX(i,j,k),varY(i,j,k),varZ(i,j,k), i=nx1,nx2), j=ny1,ny2), k=nz1,nz2)
            endif

          case(binary)
            if (trim(adjustlt(Upper_Case(fmt)))/='BINARY' .or. &
                trim(adjustlt(Upper_Case(type)))/='INT8') then
              E_IO = -1_I4P
            else
              read(data, fmt=*, iostat=E_IO) &
                  (((varX(i,j,k),varY(i,j,k),varZ(i,j,k), i=nx1,nx2), j=ny1,ny2), k=nz1,nz2)
            endif
            ! Decode base64 packed data
            data=trim(adjustlt(data))
            allocate(dI1P(NCOMP*(nx2-nx1+1)*(ny2-ny1+1)*(nz2-nz1+1)*int(BYI1P,I4P)+int(BYI4P,I4P)))
            call b64_decode(code=data,n=dI1P); if(allocated(data)) deallocate(data)
            ! Unpack data [1xI4P,NCOMP*(nx2-nx1+1)*(ny2-ny1+1)*(nz2-nz1+1)xI1P]
            N_byte =  transfer(dI1P(1:int(BYI4P,I4P)),N_byte)
            s = size(transfer(dI1P(int(BYI4P,I4P)+1:),XYZp)); allocate(XYZp(1:s))
            XYZp = transfer(dI1P(int(BYI4P,I4P)+1:),XYZp); if(allocated(dI1P)) deallocate(dI1P)
            s=1;do k=nz1,nz2-nz1;do j=ny1,ny2;do i=nx1,nx2;
              varX(i,j,k)=XYZp(s); s=s+1; varY(i,j,k)=XYZp(s); s=s+1; varZ(i,j,k)=XYZp(s); s=s+1
            enddo;enddo;enddo;
            if(allocated(XYZp)) deallocate(XYZp)

          case(raw)
            if(E_IO == 0) then
              if (trim(adjustlt(Upper_Case(fmt)))/='APPENDED' .or. &
                  trim(adjustlt(Upper_Case(type)))/='INT8') then
                E_IO = -1_I4P
              else
                read(unit=vtk(rf)%u, iostat=E_IO, pos=vtk(rf)%ioffset+offs) N_Byte, &
                  (((varX(i,j,k),varY(i,j,k),varZ(i,j,k), i=nx1,nx2), j=ny1,ny2), k=nz1,nz2)
              endif
          endif

        endselect
      endif
  endselect
  !---------------------------------------------------------------------------------------------------------------------------------
  end function

  function VTK_VAR_XML_LIST_3DA_R8_READ(var_location,varname,NC_NN,NCOMP,var,npiece,cf) result(E_IO)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Function for reading field of scalar variable (R8P, 1D array).
  !---------------------------------------------------------------------------------------------------------------------------------
  implicit none
  character(*),           intent(IN)  :: var_location !< location of variables: CELL for cell-centered, NODE for node-centered
  character(*),           intent(IN)  :: varname      !< variable name
  integer(I4P),           intent(OUT) :: NC_NN        !< number of cells or nodes
  integer(I4P),           intent(OUT) :: NCOMP        !< number of components
  real(R8P), allocatable, intent(OUT) :: var(:,:,:,:) !< variable to be saved
  integer(I4P), optional, intent(IN)  :: npiece       !< Number of the piece to read (by default: 1)
  integer(I4P), optional, intent(IN)  :: cf           !< Current file index (for concurrent files IO).
  integer(I4P)                        :: E_IO         !< Input/Output inquiring flag: $0$ if IO is done, $> 0$ if IO is not done
  integer(I4P)                        :: rf           !< Real file index.
  character(len=:), allocatable       :: fmt
  character(len=:), allocatable       :: type
  character(len=:), allocatable       :: data
  integer(I4P)                        :: np, offs, N_Byte, s
  integer(I4P)                        :: nx1,nx2,ny1,ny2,nz1,nz2
  integer(I1P), allocatable           :: dI1P(:)
  real(R8P), allocatable              :: XYZp(:)
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  E_IO = -1_I4P
  rf = f
  if (present(cf)) then
    rf = cf ; f = cf
  endif
  np = 1_I4P; if (present(npiece)) np = npiece

  select case(trim(vtk(rf)%topology))
    case('RectilinearGrid','StructuredGrid')
      ! Read field headers
      E_IO = VTK_VAR_XML_HEADER_READ(var_location,varname,NC_NN,NCOMP,&
                  nx1=nx1,nx2=nx2,ny1=ny1,ny2=ny2,nz1=nz1,nz2=nz2, &
                  fmt=fmt,type=type,data=data,offs=offs,npiece=np,cf=rf)
      if(NCOMP/=3) E_IO=-1_I4P

      if(E_IO == 0) then
        allocate(var(1:NCOMP,nx1:nx2,ny1:ny2,nz1:nz2), stat=E_IO)

        ! Read field data
        select case(vtk(rf)%f)
          case(ascii)
            if (trim(adjustlt(Upper_Case(fmt)))/='ASCII' .or. &
                trim(adjustlt(Upper_Case(type)))/='FLOAT64') then
              E_IO = -1_I4P
            else
              read(data, fmt=*, iostat=E_IO) var
            endif

          case(binary)
            if (trim(adjustlt(Upper_Case(fmt)))/='BINARY' .or. &
                trim(adjustlt(Upper_Case(type)))/='FLOAT64') then
              E_IO = -1_I4P
            else
              ! Decode base64 packed data
              data=trim(adjustlt(data))
              allocate(dI1P(NCOMP*(nx2-nx1+1)*(ny2-ny1+1)*(nz2-nz1+1)*int(BYR8P,I4P)+int(BYI4P,I4P)))
              call b64_decode(code=data,n=dI1P); if(allocated(data)) deallocate(data)
              ! Unpack data [1xI4P,NCOMP*(nx2-nx1+1)*(ny2-ny1+1)*(nz2-nz1+1)xR8P]
              N_byte =  transfer(dI1P(1:int(BYI4P,I4P)),N_byte)
              s = size(transfer(dI1P(int(BYI4P,I4P)+1:),XYZp)); allocate(XYZp(1:s))
              XYZp = transfer(dI1P(int(BYI4P,I4P)+1:),XYZp); if(allocated(dI1P)) deallocate(dI1P)
              var(1:NCOMP,nx1:nx2,ny1:ny2,nz1:nz2) = reshape(XYZp, (/NCOMP,nx2-nx1+1,ny2-ny1+1,nz2-nz1+1/))
              if(allocated(XYZp)) deallocate(XYZp)
            endif

          case(raw)
            if(E_IO == 0) then
              if (trim(adjustlt(Upper_Case(fmt)))/='APPENDED' .or. &
                  trim(adjustlt(Upper_Case(type)))/='FLOAT64') then
                E_IO = -1_I4P
              else
                read(unit=vtk(rf)%u, iostat=E_IO, pos=vtk(rf)%ioffset+offs) N_Byte, var
              endif
          endif

        endselect
      endif
  endselect
  !---------------------------------------------------------------------------------------------------------------------------------
  end function

  function VTK_VAR_XML_LIST_3DA_R4_READ(var_location,varname,NC_NN,NCOMP,var,npiece,cf) result(E_IO)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Function for reading field of scalar variable (R4P, 1D array).
  !---------------------------------------------------------------------------------------------------------------------------------
  implicit none
  character(*),           intent(IN)  :: var_location !< location of variables: CELL for cell-centered, NODE for node-centered
  character(*),           intent(IN)  :: varname      !< variable name
  integer(I4P),           intent(OUT) :: NC_NN        !< number of cells or nodes
  integer(I4P),           intent(OUT) :: NCOMP        !< number of components
  real(R4P), allocatable, intent(OUT) :: var(:,:,:,:) !< variable to be saved
  integer(I4P), optional, intent(IN)  :: npiece       !< Number of the piece to read (by default: 1)
  integer(I4P), optional, intent(IN)  :: cf           !< Current file index (for concurrent files IO).
  integer(I4P)                        :: E_IO         !< Input/Output inquiring flag: $0$ if IO is done, $> 0$ if IO is not done
  integer(I4P)                        :: rf           !< Real file index.
  character(len=:), allocatable       :: fmt
  character(len=:), allocatable       :: type
  character(len=:), allocatable       :: data
  integer(I4P)                        :: np, offs, N_Byte, s
  integer(I4P)                        :: nx1,nx2,ny1,ny2,nz1,nz2
  integer(I1P), allocatable           :: dI1P(:)
  real(R4P), allocatable              :: XYZp(:)
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  E_IO = -1_I4P
  rf = f
  if (present(cf)) then
    rf = cf ; f = cf
  endif
  np = 1_I4P; if (present(npiece)) np = npiece

  select case(trim(vtk(rf)%topology))
    case('RectilinearGrid','StructuredGrid')
      ! Read field headers
      E_IO = VTK_VAR_XML_HEADER_READ(var_location,varname,NC_NN,NCOMP,&
                  nx1=nx1,nx2=nx2,ny1=ny1,ny2=ny2,nz1=nz1,nz2=nz2, &
                  fmt=fmt,type=type,data=data,offs=offs,npiece=np,cf=rf)
      if(NCOMP/=3) E_IO=-1_I4P

      if(E_IO == 0) then
        allocate(var(1:NCOMP,nx1:nx2,ny1:ny2,nz1:nz2), stat=E_IO)

        ! Read field data
        select case(vtk(rf)%f)
          case(ascii)
            if (trim(adjustlt(Upper_Case(fmt)))/='ASCII' .or. &
                trim(adjustlt(Upper_Case(type)))/='FLOAT32') then
              E_IO = -1_I4P
            else
              read(data, fmt=*, iostat=E_IO) var
            endif

          case(binary)
            if (trim(adjustlt(Upper_Case(fmt)))/='BINARY' .or. &
                trim(adjustlt(Upper_Case(type)))/='FLOAT32') then
              E_IO = -1_I4P
            else
              ! Decode base64 packed data
              data=trim(adjustlt(data))
              allocate(dI1P(NCOMP*(nx2-nx1+1)*(ny2-ny1+1)*(nz2-nz1+1)*int(BYR4P,I4P)+int(BYI4P,I4P)))
              call b64_decode(code=data,n=dI1P); if(allocated(data)) deallocate(data)
              ! Unpack data [1xI4P,NCOMP*(nx2-nx1+1)*(ny2-ny1+1)*(nz2-nz1+1)xR4P]
              N_byte =  transfer(dI1P(1:int(BYI4P,I4P)),N_byte)
              s = size(transfer(dI1P(int(BYI4P,I4P)+1:),XYZp)); allocate(XYZp(1:s))
              XYZp = transfer(dI1P(int(BYI4P,I4P)+1:),XYZp); if(allocated(dI1P)) deallocate(dI1P)
              var(1:NCOMP,nx1:nx2,ny1:ny2,nz1:nz2) = reshape(XYZp, (/NCOMP,nx2-nx1+1,ny2-ny1+1,nz2-nz1+1/))
              if(allocated(XYZp)) deallocate(XYZp)
            endif

          case(raw)
            if(E_IO == 0) then
              if (trim(adjustlt(Upper_Case(fmt)))/='APPENDED' .or. &
                  trim(adjustlt(Upper_Case(type)))/='FLOAT32') then
                E_IO = -1_I4P
              else
                read(unit=vtk(rf)%u, iostat=E_IO, pos=vtk(rf)%ioffset+offs) N_Byte, var
              endif
          endif

        endselect
      endif
  endselect
  !---------------------------------------------------------------------------------------------------------------------------------
  end function

  function VTK_VAR_XML_LIST_3DA_I8_READ(var_location,varname,NC_NN,NCOMP,var,npiece,cf) result(E_IO)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Function for reading field of scalar variable (I8P, 1D array).
  !---------------------------------------------------------------------------------------------------------------------------------
  implicit none
  character(*),              intent(IN)  :: var_location !< location of variables: CELL for cell-centered, NODE for node-centered
  character(*),              intent(IN)  :: varname      !< variable name
  integer(I4P),              intent(OUT) :: NC_NN        !< number of cells or nodes
  integer(I4P),              intent(OUT) :: NCOMP        !< number of components
  integer(I8P), allocatable, intent(OUT) :: var(:,:,:,:) !< variable to be saved
  integer(I4P), optional,    intent(IN)  :: npiece       !< Number of the piece to read (by default: 1)
  integer(I4P), optional,    intent(IN)  :: cf           !< Current file index (for concurrent files IO).
  integer(I4P)                           :: E_IO         !< Input/Output inquiring flag: $0$ if IO is done, $> 0$ if IO is not done
  integer(I4P)                           :: rf           !< Real file index.
  character(len=:), allocatable          :: fmt
  character(len=:), allocatable          :: type
  character(len=:), allocatable          :: data
  integer(I4P)                           :: np, offs, N_Byte, s
  integer(I4P)                           :: nx1,nx2,ny1,ny2,nz1,nz2
  integer(I1P), allocatable              :: dI1P(:)
  integer(I8P), allocatable              :: XYZp(:)
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  E_IO = -1_I4P
  rf = f
  if (present(cf)) then
    rf = cf ; f = cf
  endif
  np = 1_I4P; if (present(npiece)) np = npiece

  select case(trim(vtk(rf)%topology))
    case('RectilinearGrid','StructuredGrid')
      ! Read field headers
      E_IO = VTK_VAR_XML_HEADER_READ(var_location,varname,NC_NN,NCOMP,&
                  nx1=nx1,nx2=nx2,ny1=ny1,ny2=ny2,nz1=nz1,nz2=nz2, &
                  fmt=fmt,type=type,data=data,offs=offs,npiece=np,cf=rf)
      if(NCOMP/=3) E_IO=-1_I4P

      if(E_IO == 0) then
        allocate(var(1:NCOMP,nx1:nx2,ny1:ny2,nz1:nz2), stat=E_IO)

        ! Read field data
        select case(vtk(rf)%f)
          case(ascii)
            if (trim(adjustlt(Upper_Case(fmt)))/='ASCII' .or. &
                trim(adjustlt(Upper_Case(type)))/='INT64') then
              E_IO = -1_I4P
            else
              read(data, fmt=*, iostat=E_IO) var
            endif

          case(binary)
            if (trim(adjustlt(Upper_Case(fmt)))/='BINARY' .or. &
                trim(adjustlt(Upper_Case(type)))/='INT64') then
              E_IO = -1_I4P
            else
              ! Decode base64 packed data
              data=trim(adjustlt(data))
              allocate(dI1P(NCOMP*(nx2-nx1+1)*(ny2-ny1+1)*(nz2-nz1+1)*int(BYI8P,I4P)+int(BYI4P,I4P)))
              call b64_decode(code=data,n=dI1P); if(allocated(data)) deallocate(data)
              ! Unpack data [1xI4P,NCOMP*(nx2-nx1+1)*(ny2-ny1+1)*(nz2-nz1+1)xI8P]
              N_byte =  transfer(dI1P(1:int(BYI4P,I4P)),N_byte)
              s = size(transfer(dI1P(int(BYI4P,I4P)+1:),XYZp)); allocate(XYZp(1:s))
              XYZp = transfer(dI1P(int(BYI4P,I4P)+1:),XYZp); if(allocated(dI1P)) deallocate(dI1P)
              var(1:NCOMP,nx1:nx2,ny1:ny2,nz1:nz2) = reshape(XYZp, (/NCOMP,nx2-nx1+1,ny2-ny1+1,nz2-nz1+1/))
              if(allocated(XYZp)) deallocate(XYZp)
            endif

          case(raw)
            if(E_IO == 0) then
              if (trim(adjustlt(Upper_Case(fmt)))/='APPENDED' .or. &
                  trim(adjustlt(Upper_Case(type)))/='INT64') then
                E_IO = -1_I4P
              else
                read(unit=vtk(rf)%u, iostat=E_IO, pos=vtk(rf)%ioffset+offs) N_Byte, var
              endif
          endif

        endselect
      endif
  endselect
  !---------------------------------------------------------------------------------------------------------------------------------
  end function

  function VTK_VAR_XML_LIST_3DA_I4_READ(var_location,varname,NC_NN,NCOMP,var,npiece,cf) result(E_IO)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Function for reading field of scalar variable (I4P, 1D array).
  !---------------------------------------------------------------------------------------------------------------------------------
  implicit none
  character(*),              intent(IN)  :: var_location !< location of variables: CELL for cell-centered, NODE for node-centered
  character(*),              intent(IN)  :: varname      !< variable name
  integer(I4P),              intent(OUT) :: NC_NN        !< number of cells or nodes
  integer(I4P),              intent(OUT) :: NCOMP        !< number of components
  integer(I4P), allocatable, intent(OUT) :: var(:,:,:,:) !< variable to be saved
  integer(I4P), optional,    intent(IN)  :: npiece       !< Number of the piece to read (by default: 1)
  integer(I4P), optional,    intent(IN)  :: cf           !< Current file index (for concurrent files IO).
  integer(I4P)                           :: E_IO         !< Input/Output inquiring flag: $0$ if IO is done, $> 0$ if IO is not done
  integer(I4P)                           :: rf           !< Real file index.
  character(len=:), allocatable          :: fmt
  character(len=:), allocatable          :: type
  character(len=:), allocatable          :: data
  integer(I4P)                           :: np, offs, N_Byte, s
  integer(I4P)                           :: nx1,nx2,ny1,ny2,nz1,nz2
  integer(I1P), allocatable              :: dI1P(:)
  integer(I4P), allocatable              :: XYZp(:)
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  E_IO = -1_I4P
  rf = f
  if (present(cf)) then
    rf = cf ; f = cf
  endif
  np = 1_I4P; if (present(npiece)) np = npiece

  select case(trim(vtk(rf)%topology))
    case('RectilinearGrid','StructuredGrid')
  ! Read field headers
      E_IO = VTK_VAR_XML_HEADER_READ(var_location,varname,NC_NN,NCOMP,&
                  nx1=nx1,nx2=nx2,ny1=ny1,ny2=ny2,nz1=nz1,nz2=nz2, &
                  fmt=fmt,type=type,data=data,offs=offs,npiece=np,cf=rf)
      if(NCOMP/=3) E_IO=-1_I4P

      if(E_IO == 0) then
        allocate(var(1:NCOMP,nx1:nx2,ny1:ny2,nz1:nz2), stat=E_IO)

        ! Read field data
        select case(vtk(rf)%f)
          case(ascii)
            if (trim(adjustlt(Upper_Case(fmt)))/='ASCII' .or. &
                trim(adjustlt(Upper_Case(type)))/='INT32') then
              E_IO = -1_I4P
            else
              read(data, fmt=*, iostat=E_IO) var
            endif

          case(binary)
            if (trim(adjustlt(Upper_Case(fmt)))/='BINARY' .or. &
                trim(adjustlt(Upper_Case(type)))/='INT32') then
              E_IO = -1_I4P
            else
              ! Decode base64 packed data
              data=trim(adjustlt(data))
              allocate(dI1P(NCOMP*(nx2-nx1+1)*(ny2-ny1+1)*(nz2-nz1+1)*int(BYI4P,I4P)+int(BYI4P,I4P)))
              call b64_decode(code=data,n=dI1P); if(allocated(data)) deallocate(data)
              ! Unpack data [1xI4P,NCOMP*(nx2-nx1+1)*(ny2-ny1+1)*(nz2-nz1+1)xI4P]
              N_byte =  transfer(dI1P(1:int(BYI4P,I4P)),N_byte)
              s = size(transfer(dI1P(int(BYI4P,I4P)+1:),XYZp)); allocate(XYZp(1:s))
              XYZp = transfer(dI1P(int(BYI4P,I4P)+1:),XYZp); if(allocated(dI1P)) deallocate(dI1P)
              var(1:NCOMP,nx1:nx2,ny1:ny2,nz1:nz2) = reshape(XYZp, (/NCOMP,nx2-nx1+1,ny2-ny1+1,nz2-nz1+1/))
              if(allocated(XYZp)) deallocate(XYZp)
            endif

          case(raw)
            if(E_IO == 0) then
              if (trim(adjustlt(Upper_Case(fmt)))/='APPENDED' .or. &
                  trim(adjustlt(Upper_Case(type)))/='INT32') then
                E_IO = -1_I4P
              else
                read(unit=vtk(rf)%u, iostat=E_IO, pos=vtk(rf)%ioffset+offs) N_Byte, var
              endif
          endif

        endselect
      endif
  endselect
  !---------------------------------------------------------------------------------------------------------------------------------
  end function

  function VTK_VAR_XML_LIST_3DA_I2_READ(var_location,varname,NC_NN,NCOMP,var,npiece,cf) result(E_IO)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Function for reading field of scalar variable (I2P, 1D array).
  !---------------------------------------------------------------------------------------------------------------------------------
  implicit none
  character(*),              intent(IN)  :: var_location !< location of variables: CELL for cell-centered, NODE for node-centered
  character(*),              intent(IN)  :: varname      !< variable name
  integer(I4P),              intent(OUT) :: NC_NN        !< number of cells or nodes
  integer(I4P),              intent(OUT) :: NCOMP        !< number of components
  integer(I2P), allocatable, intent(OUT) :: var(:,:,:,:) !< variable to be saved
  integer(I4P), optional,    intent(IN)  :: npiece       !< Number of the piece to read (by default: 1)
  integer(I4P), optional,    intent(IN)  :: cf           !< Current file index (for concurrent files IO).
  integer(I4P)                           :: E_IO         !< Input/Output inquiring flag: $0$ if IO is done, $> 0$ if IO is not done
  integer(I4P)                           :: rf           !< Real file index.
  character(len=:), allocatable          :: fmt
  character(len=:), allocatable          :: type
  character(len=:), allocatable          :: data
  integer(I4P)                           :: np, offs, N_Byte, s
  integer(I4P)                           :: nx1,nx2,ny1,ny2,nz1,nz2
  integer(I1P), allocatable              :: dI1P(:)
  integer(I2P), allocatable              :: XYZp(:)
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  E_IO = -1_I4P
  rf = f
  if (present(cf)) then
    rf = cf ; f = cf
  endif
  np = 1_I4P; if (present(npiece)) np = npiece

  select case(trim(vtk(rf)%topology))
    case('RectilinearGrid','StructuredGrid')
      ! Read field headers
      E_IO = VTK_VAR_XML_HEADER_READ(var_location,varname,NC_NN,NCOMP,&
                  nx1=nx1,nx2=nx2,ny1=ny1,ny2=ny2,nz1=nz1,nz2=nz2, &
                  fmt=fmt,type=type,data=data,offs=offs,npiece=np,cf=rf)
      if(NCOMP/=3) E_IO=-1_I4P

      if(E_IO == 0) then
        allocate(var(1:NCOMP,nx1:nx2,ny1:ny2,nz1:nz2), stat=E_IO)

        ! Read field data
        select case(vtk(rf)%f)
          case(ascii)
            if (trim(adjustlt(Upper_Case(fmt)))/='ASCII' .or. &
                trim(adjustlt(Upper_Case(type)))/='INT16') then
              E_IO = -1_I4P
            else
              read(data, fmt=*, iostat=E_IO) var
            endif

          case(binary)
            if (trim(adjustlt(Upper_Case(fmt)))/='BINARY' .or. &
                trim(adjustlt(Upper_Case(type)))/='INT16') then
              E_IO = -1_I4P
            else
              ! Decode base64 packed data
              data=trim(adjustlt(data))
              allocate(dI1P(NCOMP*(nx2-nx1+1)*(ny2-ny1+1)*(nz2-nz1+1)*int(BYI2P,I4P)+int(BYI4P,I4P)))
              call b64_decode(code=data,n=dI1P); if(allocated(data)) deallocate(data)
              ! Unpack data [1xI4P,NCOMP*(nx2-nx1+1)*(ny2-ny1+1)*(nz2-nz1+1)xI2P]
              N_byte =  transfer(dI1P(1:int(BYI4P,I4P)),N_byte)
              s = size(transfer(dI1P(int(BYI4P,I4P)+1:),XYZp)); allocate(XYZp(1:s))
              XYZp = transfer(dI1P(int(BYI4P,I4P)+1:),XYZp); if(allocated(dI1P)) deallocate(dI1P)
              var(1:NCOMP,nx1:nx2,ny1:ny2,nz1:nz2) = reshape(XYZp, (/NCOMP,nx2-nx1+1,ny2-ny1+1,nz2-nz1+1/))
              if(allocated(XYZp)) deallocate(XYZp)
            endif

          case(raw)
            if(E_IO == 0) then
              if (trim(adjustlt(Upper_Case(fmt)))/='APPENDED' .or. &
                  trim(adjustlt(Upper_Case(type)))/='INT16') then
                E_IO = -1_I4P
              else
                read(unit=vtk(rf)%u, iostat=E_IO, pos=vtk(rf)%ioffset+offs) N_Byte, var
              endif
          endif

        endselect
      endif
  endselect
  !---------------------------------------------------------------------------------------------------------------------------------
  end function

  function VTK_VAR_XML_LIST_3DA_I1_READ(var_location,varname,NC_NN,NCOMP,var,npiece,cf) result(E_IO)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Function for reading field of scalar variable (I1P, 1D array).
  !---------------------------------------------------------------------------------------------------------------------------------
  implicit none
  character(*),              intent(IN)  :: var_location !< location of variables: CELL for cell-centered, NODE for node-centered
  character(*),              intent(IN)  :: varname      !< variable name
  integer(I4P),              intent(OUT) :: NC_NN        !< number of cells or nodes
  integer(I4P),              intent(OUT) :: NCOMP        !< number of components
  integer(I1P), allocatable, intent(OUT) :: var(:,:,:,:) !< variable to be saved
  integer(I4P), optional,    intent(IN)  :: npiece       !< Number of the piece to read (by default: 1)
  integer(I4P), optional,    intent(IN)  :: cf           !< Current file index (for concurrent files IO).
  integer(I4P)                           :: E_IO         !< Input/Output inquiring flag: $0$ if IO is done, $> 0$ if IO is not done
  integer(I4P)                           :: rf           !< Real file index.
  character(len=:), allocatable          :: fmt
  character(len=:), allocatable          :: type
  character(len=:), allocatable          :: data
  integer(I4P)                           :: np, offs, N_Byte, s
  integer(I4P)                           :: nx1,nx2,ny1,ny2,nz1,nz2
  integer(I1P), allocatable              :: dI1P(:)
  integer(I1P), allocatable              :: XYZp(:)
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  E_IO = -1_I4P
  rf = f
  if (present(cf)) then
    rf = cf ; f = cf
  endif
  np = 1_I4P; if (present(npiece)) np = npiece

  select case(trim(vtk(rf)%topology))
    case('RectilinearGrid','StructuredGrid')
      ! Read field headers
      E_IO = VTK_VAR_XML_HEADER_READ(var_location,varname,NC_NN,NCOMP,&
                  nx1=nx1,nx2=nx2,ny1=ny1,ny2=ny2,nz1=nz1,nz2=nz2, &
                  fmt=fmt,type=type,data=data,offs=offs,npiece=np,cf=rf)
      if(NCOMP/=3) E_IO=-1_I4P

      if(E_IO == 0) then
        allocate(var(1:NCOMP,nx1:nx2,ny1:ny2,nz1:nz2), stat=E_IO)

        ! Read field data
        select case(vtk(rf)%f)
          case(ascii)
            if (trim(adjustlt(Upper_Case(fmt)))/='ASCII' .or. &
                trim(adjustlt(Upper_Case(type)))/='INT8') then
              E_IO = -1_I4P
            else
              read(data, fmt=*, iostat=E_IO) var
            endif

          case(binary)
            if (trim(adjustlt(Upper_Case(fmt)))/='BINARY' .or. &
                trim(adjustlt(Upper_Case(type)))/='INT8') then
              E_IO = -1_I4P
            else
              ! Decode base64 packed data
              data=trim(adjustlt(data))
              allocate(dI1P(NCOMP*(nx2-nx1+1)*(ny2-ny1+1)*(nz2-nz1+1)*int(BYI1P,I4P)+int(BYI4P,I4P)))
              call b64_decode(code=data,n=dI1P); if(allocated(data)) deallocate(data)
              ! Unpack data [1xI4P,NCOMP*(nx2-nx1+1)*(ny2-ny1+1)*(nz2-nz1+1)xI1P]
              N_byte =  transfer(dI1P(1:int(BYI4P,I4P)),N_byte)
              s = size(transfer(dI1P(int(BYI4P,I4P)+1:),XYZp)); allocate(XYZp(1:s))
              XYZp = transfer(dI1P(int(BYI4P,I4P)+1:),XYZp); if(allocated(dI1P)) deallocate(dI1P)
              var(1:NCOMP,nx1:nx2,ny1:ny2,nz1:nz2) = reshape(XYZp, (/NCOMP,nx2-nx1+1,ny2-ny1+1,nz2-nz1+1/))
              if(allocated(XYZp)) deallocate(XYZp)
            endif

          case(raw)
            if(E_IO == 0) then
              if (trim(adjustlt(Upper_Case(fmt)))/='APPENDED' .or. &
                  trim(adjustlt(Upper_Case(type)))/='INT8') then
                E_IO = -1_I4P
              else
                read(unit=vtk(rf)%u, iostat=E_IO, pos=vtk(rf)%ioffset+offs) N_Byte, var
              endif
          endif

        endselect
      endif
  endselect
  !---------------------------------------------------------------------------------------------------------------------------------
  end function
endmodule Lib_VTK_IO_DAT_VAR_XML
