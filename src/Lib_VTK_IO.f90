!< Pure Fortran (2003+) library to write and read data conforming the VTK standard
module Lib_VTK_IO
!-----------------------------------------------------------------------------------------------------------------------------------
!< Pure Fortran (2003+) library to write and read data conforming the VTK standard
!<{!README-Lib_VTK_IO.md!}
!<
!<### ChangeLog
!<
!<{!ChangeLog-Lib_VTK_IO.md!}
!-----------------------------------------------------------------------------------------------------------------------------------
USE IR_Precision        ! Integers and reals precision definition.
USE Lib_Base64          ! Base64 encoding/decoding procedures.
USE Lib_VTK_IO_Back_End ! Lib_VTK_IO back end module.
USE Lib_VTK_IO_GEO      ! GEO interface module.
USE Lib_VTK_IO_GEO_XML  ! GEO_XML interface module.
!-----------------------------------------------------------------------------------------------------------------------------------

!-----------------------------------------------------------------------------------------------------------------------------------
implicit none
private
save
! functions for VTK XML
public:: VTK_INI_XML
public:: VTK_FLD_XML
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
! functions for PVTK XML
public:: PVTK_INI_XML
public:: PVTK_GEO_XML
public:: PVTK_DAT_XML
public:: PVTK_VAR_XML
public:: PVTK_END_XML
! functions for VTK LEGACY
public:: VTK_INI
public:: VTK_GEO
public:: VTK_CON
public:: VTK_DAT
public:: VTK_VAR
public:: VTK_END
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
interface VTK_VAR_XML
  !< Procedure for saving data variable(s) in VTK-XML standard.
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
  module procedure VTK_VAR_XML_SCAL_1DA_R8,VTK_VAR_XML_SCAL_3DA_R8, & ! real(R8P)    scalar    1D/3D array
                   VTK_VAR_XML_SCAL_1DA_R4,VTK_VAR_XML_SCAL_3DA_R4, & ! real(R4P)    scalar    1D/3D array
                   VTK_VAR_XML_SCAL_1DA_I8,VTK_VAR_XML_SCAL_3DA_I8, & ! integer(I8P) scalar    1D/3D array
                   VTK_VAR_XML_SCAL_1DA_I4,VTK_VAR_XML_SCAL_3DA_I4, & ! integer(I4P) scalar    1D/3D array
                   VTK_VAR_XML_SCAL_1DA_I2,VTK_VAR_XML_SCAL_3DA_I2, & ! integer(I2P) scalar    1D/3D array
                   VTK_VAR_XML_SCAL_1DA_I1,VTK_VAR_XML_SCAL_3DA_I1, & ! integer(I1P) scalar    1D/3D array
                   VTK_VAR_XML_VECT_1DA_R8,VTK_VAR_XML_VECT_3DA_R8, & ! real(R8P)    vectorial 1D/3D arrays
                   VTK_VAR_XML_VECT_1DA_R4,VTK_VAR_XML_VECT_3DA_R4, & ! real(R4P)    vectorial 1D/3D arrays
                   VTK_VAR_XML_VECT_1DA_I8,VTK_VAR_XML_VECT_3DA_I8, & ! integer(I8P) vectorial 1D/3D arrays
                   VTK_VAR_XML_VECT_1DA_I4,VTK_VAR_XML_VECT_3DA_I4, & ! integer(I4P) vectorial 1D/3D arrays
                   VTK_VAR_XML_VECT_1DA_I2,VTK_VAR_XML_VECT_3DA_I2, & ! integer(I2P) vectorial 1D/3D arrays
                   VTK_VAR_XML_VECT_1DA_I1,VTK_VAR_XML_VECT_3DA_I1, & ! integer(I1P) vectorial 1D/3D arrays
                   VTK_VAR_XML_LIST_1DA_R8,VTK_VAR_XML_LIST_3DA_R8, & ! real(R8P)    list      1D/3D array
                   VTK_VAR_XML_LIST_1DA_R4,VTK_VAR_XML_LIST_3DA_R4, & ! real(R4P)    list      1D/3D array
                   VTK_VAR_XML_LIST_1DA_I8,VTK_VAR_XML_LIST_3DA_I8, & ! integer(I4P) list      1D/3D array
                   VTK_VAR_XML_LIST_1DA_I4,VTK_VAR_XML_LIST_3DA_I4, & ! integer(I4P) list      1D/3D array
                   VTK_VAR_XML_LIST_1DA_I2,VTK_VAR_XML_LIST_3DA_I2, & ! integer(I2P) list      1D/3D array
                   VTK_VAR_XML_LIST_1DA_I1,VTK_VAR_XML_LIST_3DA_I1    ! integer(I1P) list      1D/3D array
endinterface
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
interface VTK_VAR
  !< Procedure for saving data variable(s) in VTK-legacy standard.
  !<
  !< VTK_VAR is an interface to 8 different functions, there are 3 functions for scalar variables, 3 functions for vectorial
  !< variables and 2 functions texture variables: scalar and vectorial data can be R8P, R4P and I4P data while texture variables can
  !< be only R8P or R4P. This function saves the data variables related to geometric mesh.
  !< @note The inputs that must be passed change depending on the data
  !< variables type.
  !<
  !<### Examples of usage
  !<
  !<#### Scalar data calling
  !<```fortran
  !< integer(I4P):: NN
  !< real(R4P)::    var(1:NN)
  !< ...
  !< E_IO=VTK_VAR(NN,'Sca',var)
  !< ...
  !<```
  !<
  !<#### Vectorial data calling
  !<```fortran
  !< integer(I4P):: NN
  !< real(R4P)::    varX(1:NN),varY(1:NN),varZ(1:NN)
  !< ...
  !< E_IO=VTK_VAR('vect',NN,'Vec',varX,varY,varZ)
  !< ...
  !<```
  module procedure VTK_VAR_SCAL_R8, & ! real(R8P)    scalar
                   VTK_VAR_SCAL_R4, & ! real(R4P)    scalar
                   VTK_VAR_SCAL_I4, & ! integer(I4P) scalar
                   VTK_VAR_VECT_R8, & ! real(R8P)    vectorial
                   VTK_VAR_VECT_R4, & ! real(R4P)    vectorial
                   VTK_VAR_VECT_I4, & ! integer(I4P) vectorial
                   VTK_VAR_TEXT_R8, & ! real(R8P)    vectorial (texture)
                   VTK_VAR_TEXT_R4    ! real(R4P)    vectorial (texture)
endinterface
!-----------------------------------------------------------------------------------------------------------------------------------
contains
  function VTK_INI_XML(output_format,filename,mesh_topology,cf,nx1,nx2,ny1,ny2,nz1,nz2) result(E_IO)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Procedure for initializing VTK-XML file.
  !<
  !< The XML standard is more powerful than legacy one. It is flexible but on the other hand is (but not so more using this library
  !< ...) complex than legacy standard. The output of XML functions is a well-formated valid XML file, at least for the
  !< ascii, binary and binary appended formats (in the raw-binary format the library uses raw-binary-appended format that is not a
  !< valid XML file).
  !< Note that the XML functions have the same name of legacy functions with the suffix *XML*.
  !< @note This function must be the first to be called.
  !<
  !< Supported output formats are (the passed specifier value is case insensitive):
  !<- ASCII: data are saved in ASCII format;
  !<- BINARY: data are saved in base64 encoded format;
  !<- RAW: data are saved in raw-binary format in the appended tag of the XML file;
  !<- BINARY-APPENDED: data are saved in base64 encoded format in the appended tag of the XML file.
  !< Supported topologies are:
  !<- RectilinearGrid;
  !<- StructuredGrid;
  !<- UnstructuredGrid.
  !<### Example of usage
  !<```fortran
  !< integer(I4P):: nx1,nx2,ny1,ny2,nz1,nz2
  !< ...
  !< E_IO = VTK_INI_XML('BINARY','XML_RECT_BINARY.vtr','RectilinearGrid',nx1=nx1,nx2=nx2,ny1=ny1,ny2=ny2,nz1=nz1,nz2=nz2)
  !< ...
  !<```
  !< Note that the file extension is necessary in the file name. The XML standard has different extensions for each
  !< different topologies (e.g. *vtr* for rectilinear topology). See the VTK-standard file for more information.
  !---------------------------------------------------------------------------------------------------------------------------------
  implicit none
  character(*), intent(IN)::            output_format !< Output format: ASCII, BINARY, RAW or BINARY-APPENDED.
  character(*), intent(IN)::            filename      !< File name.
  character(*), intent(IN)::            mesh_topology !< Mesh topology.
  integer(I4P), intent(OUT), optional:: cf            !< Current file index (for concurrent files IO).
  integer(I4P), intent(IN),  optional:: nx1           !< Initial node of x axis.
  integer(I4P), intent(IN),  optional:: nx2           !< Final node of x axis.
  integer(I4P), intent(IN),  optional:: ny1           !< Initial node of y axis.
  integer(I4P), intent(IN),  optional:: ny2           !< Final node of y axis.
  integer(I4P), intent(IN),  optional:: nz1           !< Initial node of z axis.
  integer(I4P), intent(IN),  optional:: nz2           !< Final node of z axis.
  integer(I4P)::                        E_IO          !< Input/Output inquiring flag: $0$ if IO is done, $> 0$ if IO is not done.
  character(len=maxlen)::               s_buffer      !< Buffer string.
  integer(I4P)::                        rf            !< Real file index.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  E_IO = -1_I4P
  if (.not.ir_initialized) call IR_Init
  if (.not.b64_initialized) call b64_init
  call vtk_update(act='add',cf=rf,Nvtk=Nvtk,vtk=vtk)
  f = rf
  if (present(cf)) cf = rf
  vtk(rf)%topology = trim(mesh_topology)
  select case(trim(Upper_Case(output_format)))
  case('ASCII')
    vtk(rf)%f = ascii
    open(unit=Get_Unit(vtk(rf)%u),file=trim(filename),form='FORMATTED',&
         access='SEQUENTIAL',action='WRITE',status='REPLACE',iostat=E_IO)
    ! writing header of file
    write(unit=vtk(rf)%u,fmt='(A)',iostat=E_IO)'<?xml version="1.0"?>'
    if (endian==endianL) then
      s_buffer = '<VTKFile type="'//trim(vtk(rf)%topology)//'" version="0.1" byte_order="LittleEndian">'
    else
      s_buffer = '<VTKFile type="'//trim(vtk(rf)%topology)//'" version="0.1" byte_order="BigEndian">'
    endif
    write(unit=vtk(rf)%u,fmt='(A)',iostat=E_IO)trim(s_buffer) ; vtk(rf)%indent = 2
    select case(trim(vtk(rf)%topology))
    case('RectilinearGrid','StructuredGrid')
      s_buffer = repeat(' ',vtk(rf)%indent)//'<'//trim(vtk(rf)%topology)//' WholeExtent="'//&
                 trim(str(n=nx1))//' '//trim(str(n=nx2))//' '//                             &
                 trim(str(n=ny1))//' '//trim(str(n=ny2))//' '//                             &
                 trim(str(n=nz1))//' '//trim(str(n=nz2))//'">'
    case('UnstructuredGrid')
      s_buffer = repeat(' ',vtk(rf)%indent)//'<'//trim(vtk(rf)%topology)//'>'
    endselect
    write(unit=vtk(rf)%u,fmt='(A)',iostat=E_IO)trim(s_buffer) ; vtk(rf)%indent = vtk(rf)%indent + 2
  case('RAW','BINARY-APPENDED')
    vtk(rf)%f = raw
    if (trim(Upper_Case(output_format))=='BINARY-APPENDED') vtk(rf)%f = bin_app
    open(unit=Get_Unit(vtk(rf)%u),file=trim(filename),&
         form='UNFORMATTED',access='STREAM',action='WRITE',status='REPLACE',iostat=E_IO)
    ! writing header of file
    write(unit=vtk(rf)%u,iostat=E_IO)'<?xml version="1.0"?>'//end_rec
    if (endian==endianL) then
      s_buffer = '<VTKFile type="'//trim(vtk(rf)%topology)//'" version="0.1" byte_order="LittleEndian">'
    else
      s_buffer = '<VTKFile type="'//trim(vtk(rf)%topology)//'" version="0.1" byte_order="BigEndian">'
    endif
    write(unit=vtk(rf)%u,iostat=E_IO)trim(s_buffer)//end_rec ; vtk(rf)%indent = 2
    select case(trim(vtk(rf)%topology))
    case('RectilinearGrid','StructuredGrid')
      s_buffer = repeat(' ',vtk(rf)%indent)//'<'//trim(vtk(rf)%topology)//' WholeExtent="'//&
                 trim(str(n=nx1))//' '//trim(str(n=nx2))//' '//                             &
                 trim(str(n=ny1))//' '//trim(str(n=ny2))//' '//                             &
                 trim(str(n=nz1))//' '//trim(str(n=nz2))//'">'
    case('UnstructuredGrid')
      s_buffer = repeat(' ',vtk(rf)%indent)//'<'//trim(vtk(rf)%topology)//'>'
    endselect
    write(unit=vtk(rf)%u,iostat=E_IO)trim(s_buffer)//end_rec ; vtk(rf)%indent = vtk(rf)%indent + 2
    ! opening the SCRATCH file used for appending raw binary data
    open(unit=Get_Unit(vtk(rf)%ua), form='UNFORMATTED', access='STREAM', action='READWRITE', status='SCRATCH', iostat=E_IO)
    vtk(rf)%ioffset = 0 ! initializing offset pointer
  case('BINARY')
    vtk(rf)%f = binary
    open(unit=Get_Unit(vtk(rf)%u),file=trim(filename),&
         form='UNFORMATTED',access='STREAM',action='WRITE',status='REPLACE',iostat=E_IO)
    ! writing header of file
    write(unit=vtk(rf)%u,iostat=E_IO)'<?xml version="1.0"?>'//end_rec
    if (endian==endianL) then
      s_buffer = '<VTKFile type="'//trim(vtk(rf)%topology)//'" version="0.1" byte_order="LittleEndian">'
    else
      s_buffer = '<VTKFile type="'//trim(vtk(rf)%topology)//'" version="0.1" byte_order="BigEndian">'
    endif
    write(unit=vtk(rf)%u,iostat=E_IO)trim(s_buffer)//end_rec ; vtk(rf)%indent = 2
    select case(trim(vtk(rf)%topology))
    case('RectilinearGrid','StructuredGrid')
      s_buffer = repeat(' ',vtk(rf)%indent)//'<'//trim(vtk(rf)%topology)//' WholeExtent="'//&
                 trim(str(n=nx1))//' '//trim(str(n=nx2))//' '//                             &
                 trim(str(n=ny1))//' '//trim(str(n=ny2))//' '//                             &
                 trim(str(n=nz1))//' '//trim(str(n=nz2))//'">'
    case('UnstructuredGrid')
      s_buffer = repeat(' ',vtk(rf)%indent)//'<'//trim(vtk(rf)%topology)//'>'
    endselect
    write(unit=vtk(rf)%u,iostat=E_IO)trim(s_buffer)//end_rec ; vtk(rf)%indent = vtk(rf)%indent + 2
  endselect
  return
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction VTK_INI_XML

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

  function VTK_CON_XML(NC,connect,offset,cell_type,idx,cf) result(E_IO)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Function for saving mesh connectivity.
  !<
  !< Function that **must** be used when unstructured grid is used, it saves the connectivity of the unstructured gird.
  !< @note The vector **connect** must follow the VTK-XML standard. It is passed as *assumed-shape array*
  !< because its dimensions is related to the mesh dimensions in a complex way. Its dimensions can be calculated by the following
  !< equation: \(dc = \sum\limits_{i = 1}^{NC} {nvertex_i }\).
  !< Note that this equation is different from the legacy one. The XML connectivity convention is quite different from the
  !< legacy standard.
  !< As an example suppose we have a mesh composed by 2 cells, one hexahedron (8 vertices) and one pyramid with
  !< square basis (5 vertices) and suppose that the basis of pyramid is constitute by a face of the hexahedron and so the two cells
  !< share 4 vertices. The above equation gives \(dc=8+5=13\). The connectivity vector for this mesh can be:
  !<
  !<##### first cell
  !<+ connect(1)  = 0 identification flag of \(1^\circ\) vertex of first cell
  !<+ connect(2)  = 1 identification flag of \(2^\circ\) vertex of first cell
  !<+ connect(3)  = 2 identification flag of \(3^\circ\) vertex of first cell
  !<+ connect(4)  = 3 identification flag of \(4^\circ\) vertex of first cell
  !<+ connect(5)  = 4 identification flag of \(5^\circ\) vertex of first cell
  !<+ connect(6)  = 5 identification flag of \(6^\circ\) vertex of first cell
  !<+ connect(7)  = 6 identification flag of \(7^\circ\) vertex of first cell
  !<+ connect(8)  = 7 identification flag of \(8^\circ\) vertex of first cell
  !<
  !<##### second cell
  !<+ connect(9 ) = 0 identification flag of \(1^\circ\) vertex of second cell
  !<+ connect(10) = 1 identification flag of \(2^\circ\) vertex of second cell
  !<+ connect(11) = 2 identification flag of \(3^\circ\) vertex of second cell
  !<+ connect(12) = 3 identification flag of \(4^\circ\) vertex of second cell
  !<+ connect(13) = 8 identification flag of \(5^\circ\) vertex of second cell
  !<
  !< Therefore this connectivity vector convention is more simple than the legacy convention, now we must create also the
  !< *offset* vector that contains the data now missing in the *connect* vector. The offset
  !< vector for this mesh can be:
  !<
  !<##### first cell
  !<+ offset(1) = 8  => summ of nodes of \(1^\circ\) cell
  !<
  !<##### second cell
  !<+ offset(2) = 13 => summ of nodes of \(1^\circ\) and \(2^\circ\) cells
  !<
  !< The value of every cell-offset can be calculated by the following equation: \(offset_c=\sum\limits_{i=1}^{c}{nvertex_i}\)
  !< where \(offset_c\) is the value of \(c^{th}\) cell and \(nvertex_i\) is the number of vertices of \(i^{th}\) cell.
  !< The function VTK_CON_XML does not calculate the connectivity and offset vectors: it writes the connectivity and offset
  !< vectors conforming the VTK-XML standard, but does not calculate them.
  !< The vector variable *cell\_type* must conform the VTK-XML standard (see the file VTK-Standard at the
  !< Kitware homepage) that is the same of the legacy standard. It contains the
  !< *type* of each cells. For the above example this vector is:
  !<
  !<##### first cell
  !<+ cell\_type(1) = 12 hexahedron type of first cell
  !<
  !<##### second cell
  !<+ cell\_type(2) = 14 pyramid type of second cell
  !---------------------------------------------------------------------------------------------------------------------------------
  implicit none
  integer(I4P), intent(IN)::           NC            !< Number of cells.
  integer(I4P), intent(IN)::           connect(1:)   !< Mesh connectivity.
  integer(I4P), intent(IN)::           offset(1:NC)  !< Cell offset.
  integer(I1P), intent(IN)::           cell_type(1:) !< VTK cell type.
  integer(I1P), intent(IN), optional:: idx           !< Id offset to convert Fortran (first id 1) to C (first id 0) standards.
  integer(I4P), intent(IN), optional:: cf            !< Current file index (for concurrent files IO).
  integer(I4P)::                       E_IO          !< Input/Output inquiring flag: $0$ if IO is done, $> 0$ if IO is not done.
  character(len=maxlen)::              s_buffer      !< Buffer string.
  integer(I1P), allocatable::          cocp(:)       !< Packed data.
  character(len=:), allocatable::      coc64         !< Data encoded in base64.
  integer(I1P)::                       incr          !< Actual id offset increment.
  integer(I4P)::                       rf            !< Real file index.
  integer(I4P)::                       n1            !< Counter.
  integer(I8P)::                       Ncocp         !< Dimension of cocp, packed data.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  E_IO = -1_I4P
  rf = f
  if (present(cf)) then
    rf = cf ; f = cf
  endif
  incr = 0_I1P
  if (present(idx)) then
    incr = idx
  endif
  select case(vtk(rf)%f)
  case(ascii)
    write(unit=vtk(rf)%u,fmt='(A)',iostat=E_IO)repeat(' ',vtk(rf)%indent)//'<Cells>' ; vtk(rf)%indent = vtk(rf)%indent + 2
    write(unit=vtk(rf)%u,fmt='(A)',iostat=E_IO)repeat(' ',vtk(rf)%indent)//&
                                               '<DataArray type="Int32" Name="connectivity" format="ascii">'
    write(unit=vtk(rf)%u,fmt=FI4P, iostat=E_IO)(connect(n1)+incr,n1=1,offset(NC))
    write(unit=vtk(rf)%u,fmt='(A)',iostat=E_IO)repeat(' ',vtk(rf)%indent)//'</DataArray>'
    write(unit=vtk(rf)%u,fmt='(A)',iostat=E_IO)repeat(' ',vtk(rf)%indent)//'<DataArray type="Int32" Name="offsets" format="ascii">'
    write(unit=vtk(rf)%u,fmt=FI4P, iostat=E_IO)(offset(n1),n1=1,NC)
    write(unit=vtk(rf)%u,fmt='(A)',iostat=E_IO)repeat(' ',vtk(rf)%indent)//'</DataArray>'
    write(unit=vtk(rf)%u,fmt='(A)',iostat=E_IO)repeat(' ',vtk(rf)%indent)//'<DataArray type="Int8" Name="types" format="ascii">'
    if (lbound(cell_type,dim=1)==ubound(cell_type,dim=1)) then
      write(unit=vtk(rf)%u,fmt=FI1P, iostat=E_IO)(cell_type(1),n1=1,NC)
    else
      write(unit=vtk(rf)%u,fmt=FI1P, iostat=E_IO)(cell_type(n1),n1=1,NC)
    endif
    write(unit=vtk(rf)%u,fmt='(A)',iostat=E_IO)repeat(' ',vtk(rf)%indent)//'</DataArray>' ; vtk(rf)%indent = vtk(rf)%indent - 2
    write(unit=vtk(rf)%u,fmt='(A)',iostat=E_IO)repeat(' ',vtk(rf)%indent)//'</Cells>'
  case(raw,bin_app)
    write(unit=vtk(rf)%u,iostat=E_IO)repeat(' ',vtk(rf)%indent)//'<Cells>'//end_rec ; vtk(rf)%indent = vtk(rf)%indent + 2
    s_buffer = repeat(' ',vtk(rf)%indent)//'<DataArray type="Int32" Name="connectivity" format="appended" offset="'// &
               trim(str(.true.,vtk(rf)%ioffset))//'"/>'
    write(unit=vtk(rf)%u,iostat=E_IO)trim(s_buffer)//end_rec
    call vtk(rf)%byte_update(N_Byte = offset(NC)*BYI4P)
    write(unit=vtk(rf)%ua,iostat=E_IO)vtk(rf)%N_Byte,'I4',offset(NC)
    write(unit=vtk(rf)%ua,iostat=E_IO)(connect(n1)+incr,n1=1,offset(NC))
    s_buffer = repeat(' ',vtk(rf)%indent)//'<DataArray type="Int32" Name="offsets" format="appended" offset="'// &
               trim(str(.true.,vtk(rf)%ioffset))//'"/>'
    write(unit=vtk(rf)%u,iostat=E_IO)trim(s_buffer)//end_rec
    call vtk(rf)%byte_update(N_Byte = NC*BYI4P)
    write(unit=vtk(rf)%ua,iostat=E_IO)vtk(rf)%N_Byte,'I4',NC
    write(unit=vtk(rf)%ua,iostat=E_IO)(offset(n1),n1=1,NC)
    s_buffer = repeat(' ',vtk(rf)%indent)//'<DataArray type="Int8" Name="types" format="appended" offset="'// &
               trim(str(.true.,vtk(rf)%ioffset))//'"/>'
    write(unit=vtk(rf)%u,iostat=E_IO)trim(s_buffer)//end_rec
    call vtk(rf)%byte_update(N_Byte = NC*BYI1P)
    write(unit=vtk(rf)%ua,iostat=E_IO)vtk(rf)%N_Byte,'I1',NC
    if (lbound(cell_type,dim=1)==ubound(cell_type,dim=1)) then
      write(unit=vtk(rf)%ua,iostat=E_IO)(cell_type(1),n1=1,NC)
    else
      write(unit=vtk(rf)%ua,iostat=E_IO)(cell_type(n1),n1=1,NC)
    endif
    vtk(rf)%indent = vtk(rf)%indent - 2 ; write(unit=vtk(rf)%u,iostat=E_IO)repeat(' ',vtk(rf)%indent)//'</Cells>'//end_rec
  case(binary)
    write(unit=vtk(rf)%u,iostat=E_IO)repeat(' ',vtk(rf)%indent)//'<Cells>'//end_rec ; vtk(rf)%indent = vtk(rf)%indent + 2
    write(unit=vtk(rf)%u,iostat=E_IO)repeat(' ',vtk(rf)%indent)//&
                                     '<DataArray type="Int32" Name="connectivity" format="binary">'//end_rec
    Ncocp=size(transfer([int(offset(NC)*BYI4P,I4P),connect],cocp),kind=I8P)
    if (allocated(cocp)) deallocate(cocp) ; allocate(cocp(1:Ncocp))
    cocp = transfer([int(offset(NC)*BYI4P,I4P),connect],cocp)
    call b64_encode(n=cocp,code=coc64)
    deallocate(cocp)
    write(unit=vtk(rf)%u,iostat=E_IO)repeat(' ',vtk(rf)%indent+2)//coc64//end_rec
    write(unit=vtk(rf)%u,iostat=E_IO)repeat(' ',vtk(rf)%indent)//'</DataArray>'//end_rec
    write(unit=vtk(rf)%u,iostat=E_IO)repeat(' ',vtk(rf)%indent)//'<DataArray type="Int32" Name="offsets" format="binary">'//end_rec
    Ncocp=size(transfer([int(NC*BYI4P,I4P),offset],cocp),kind=I8P) ; if (allocated(cocp)) deallocate(cocp) ; allocate(cocp(1:Ncocp))
    cocp = transfer([int(NC*BYI4P,I4P),offset],cocp)
    call b64_encode(n=cocp,code=coc64)
    deallocate(cocp)
    write(unit=vtk(rf)%u,iostat=E_IO)repeat(' ',vtk(rf)%indent+2)//coc64//end_rec
    write(unit=vtk(rf)%u,iostat=E_IO)repeat(' ',vtk(rf)%indent)//'</DataArray>'//end_rec
    write(unit=vtk(rf)%u,iostat=E_IO)repeat(' ',vtk(rf)%indent)//'<DataArray type="Int8" Name="types" format="binary">'//end_rec
    if (lbound(cell_type,dim=1)==ubound(cell_type,dim=1)) then
      call pack_data(a1=[int(NC*BYI1P,I4P)],a2=[(cell_type(1),n1=1,NC)],packed=cocp)
    else
      call pack_data(a1=[int(NC*BYI1P,I4P)],a2=cell_type,packed=cocp)
    endif
    call b64_encode(n=cocp,code=coc64) ; deallocate(cocp)
    write(unit=vtk(rf)%u,iostat=E_IO)repeat(' ',vtk(rf)%indent+2)//coc64//end_rec ; deallocate(coc64)
    write(unit=vtk(rf)%u,iostat=E_IO)repeat(' ',vtk(rf)%indent)//'</DataArray>'//end_rec ; vtk(rf)%indent = vtk(rf)%indent - 2
    write(unit=vtk(rf)%u,iostat=E_IO)repeat(' ',vtk(rf)%indent)//'</Cells>'//end_rec
  endselect
  return
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction VTK_CON_XML

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

  function VTK_VAR_XML_SCAL_1DA_R8(NC_NN,varname,var,cf) result(E_IO)
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
    write(vtk(rf)%u,'('//trim(str(.true.,NC_NN+1))//'A)',iostat=E_IO)repeat(' ',vtk(rf)%indent),(' '//str(n=var(n1)),n1=1,NC_NN)
    write(vtk(rf)%u,'(A)',iostat=E_IO)repeat(' ',vtk(rf)%indent)//'</DataArray>'
  case(raw,bin_app)
    s_buffer = repeat(' ',vtk(rf)%indent)//'<DataArray type="Float64" Name="'//trim(varname)//&
               '" NumberOfComponents="1" format="appended" offset="'//trim(str(.true.,vtk(rf)%ioffset))//'"/>'
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
  endfunction VTK_VAR_XML_SCAL_1DA_R8

  function VTK_VAR_XML_SCAL_3DA_R8(NC_NN,varname,var,cf) result(E_IO)
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
    write(vtk(rf)%u,'('//trim(str(.true.,NC_NN+1))//'A)',iostat=E_IO)repeat(' ',vtk(rf)%indent),&
                                      (((' '//str(n=var(nx,ny,nz)),nx=1,size(var,dim=1)),ny=1,size(var,dim=2)),nz=1,size(var,dim=3))
    write(vtk(rf)%u,'(A)', iostat=E_IO)repeat(' ',vtk(rf)%indent)//'</DataArray>'
  case(raw,bin_app)
    s_buffer = repeat(' ',vtk(rf)%indent)//'<DataArray type="Float64" Name="'//trim(varname)//&
               '" NumberOfComponents="1" format="appended" offset="'//trim(str(.true.,vtk(rf)%ioffset))//'"/>'
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
  endfunction VTK_VAR_XML_SCAL_3DA_R8

  function VTK_VAR_XML_SCAL_1DA_R4(NC_NN,varname,var,cf) result(E_IO)
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
    write(vtk(rf)%u,'('//trim(str(.true.,NC_NN+1))//'A)',iostat=E_IO)repeat(' ',vtk(rf)%indent),(' '//str(n=var(n1)),n1=1,NC_NN)
    write(vtk(rf)%u,'(A)', iostat=E_IO)repeat(' ',vtk(rf)%indent)//'</DataArray>'
  case(raw,bin_app)
    s_buffer = repeat(' ',vtk(rf)%indent)//'<DataArray type="Float32" Name="'//trim(varname)//&
               '" NumberOfComponents="1" format="appended" offset="'//trim(str(.true.,vtk(rf)%ioffset))//'"/>'
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
  endfunction VTK_VAR_XML_SCAL_1DA_R4

  function VTK_VAR_XML_SCAL_3DA_R4(NC_NN,varname,var,cf) result(E_IO)
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
    write(vtk(rf)%u,'('//trim(str(.true.,NC_NN+1))//'A)',iostat=E_IO)repeat(' ',vtk(rf)%indent),&
                                      (((' '//str(n=var(nx,ny,nz)),nx=1,size(var,dim=1)),ny=1,size(var,dim=2)),nz=1,size(var,dim=3))
    write(vtk(rf)%u,'(A)', iostat=E_IO)repeat(' ',vtk(rf)%indent)//'</DataArray>'
  case(raw,bin_app)
    s_buffer = repeat(' ',vtk(rf)%indent)//'<DataArray type="Float32" Name="'//trim(varname)//&
               '" NumberOfComponents="1" format="appended" offset="'//trim(str(.true.,vtk(rf)%ioffset))//'"/>'
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
  endfunction VTK_VAR_XML_SCAL_3DA_R4

  function VTK_VAR_XML_SCAL_1DA_I8(NC_NN,varname,var,cf) result(E_IO)
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
    write(vtk(rf)%u,'('//trim(str(.true.,NC_NN+1))//'A)',iostat=E_IO)repeat(' ',vtk(rf)%indent),(' '//str(n=var(n1)),n1=1,NC_NN)
    write(vtk(rf)%u,'(A)',iostat=E_IO)'</DataArray>'
  case(raw,bin_app)
    s_buffer = repeat(' ',vtk(rf)%indent)//'<DataArray type="Int64" Name="'//trim(varname)//&
               '" NumberOfComponents="1" format="appended" offset="'//trim(str(.true.,vtk(rf)%ioffset))//'"/>'
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
  endfunction VTK_VAR_XML_SCAL_1DA_I8

  function VTK_VAR_XML_SCAL_3DA_I8(NC_NN,varname,var,cf) result(E_IO)
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
    write(vtk(rf)%u,'('//trim(str(.true.,NC_NN+1))//'A)',iostat=E_IO)repeat(' ',vtk(rf)%indent),&
                                      (((' '//str(n=var(nx,ny,nz)),nx=1,size(var,dim=1)),ny=1,size(var,dim=2)),nz=1,size(var,dim=3))
    write(vtk(rf)%u,'(A)',iostat=E_IO)'</DataArray>'
  case(raw,bin_app)
    s_buffer = repeat(' ',vtk(rf)%indent)//'<DataArray type="Int64" Name="'//trim(varname)//&
               '" NumberOfComponents="1" format="appended" offset="'//trim(str(.true.,vtk(rf)%ioffset))//'"/>'
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
  endfunction VTK_VAR_XML_SCAL_3DA_I8

  function VTK_VAR_XML_SCAL_1DA_I4(NC_NN,varname,var,cf) result(E_IO)
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
    write(vtk(rf)%u,'('//trim(str(.true.,NC_NN+1))//'A)',iostat=E_IO)repeat(' ',vtk(rf)%indent),(' '//str(n=var(n1)),n1=1,NC_NN)
    write(vtk(rf)%u,'(A)',iostat=E_IO)repeat(' ',vtk(rf)%indent)//'</DataArray>'
  case(raw,bin_app)
    s_buffer = repeat(' ',vtk(rf)%indent)//'<DataArray type="Int32" Name="'//trim(varname)// &
               '" NumberOfComponents="1" format="appended" offset="'//trim(str(.true.,vtk(rf)%ioffset))//'"/>'
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
  endfunction VTK_VAR_XML_SCAL_1DA_I4

  function VTK_VAR_XML_SCAL_3DA_I4(NC_NN,varname,var,cf) result(E_IO)
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
    write(vtk(rf)%u,'('//trim(str(.true.,NC_NN+1))//'A)',iostat=E_IO)repeat(' ',vtk(rf)%indent),&
                                      (((' '//str(n=var(nx,ny,nz)),nx=1,size(var,dim=1)),ny=1,size(var,dim=2)),nz=1,size(var,dim=3))
    write(vtk(rf)%u,'(A)',iostat=E_IO)repeat(' ',vtk(rf)%indent)//'</DataArray>'
  case(raw,bin_app)
    s_buffer = repeat(' ',vtk(rf)%indent)//'<DataArray type="Int32" Name="'//trim(varname)// &
               '" NumberOfComponents="1" format="appended" offset="'//trim(str(.true.,vtk(rf)%ioffset))//'"/>'
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
  endfunction VTK_VAR_XML_SCAL_3DA_I4

  function VTK_VAR_XML_SCAL_1DA_I2(NC_NN,varname,var,cf) result(E_IO)
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
    write(vtk(rf)%u,'('//trim(str(.true.,NC_NN+1))//'A)',iostat=E_IO)repeat(' ',vtk(rf)%indent),(' '//str(n=var(n1)),n1=1,NC_NN)
    write(vtk(rf)%u,'(A)',iostat=E_IO)repeat(' ',vtk(rf)%indent)//'</DataArray>'
  case(raw,bin_app)
    s_buffer = repeat(' ',vtk(rf)%indent)//'<DataArray type="Int16" Name="'//trim(varname)//&
               '" NumberOfComponents="1" format="appended" offset="'//trim(str(.true.,vtk(rf)%ioffset))//'"/>'
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
  endfunction VTK_VAR_XML_SCAL_1DA_I2

  function VTK_VAR_XML_SCAL_3DA_I2(NC_NN,varname,var,cf) result(E_IO)
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
    write(vtk(rf)%u,'('//trim(str(.true.,NC_NN+1))//'A)',iostat=E_IO)repeat(' ',vtk(rf)%indent),&
                                      (((' '//str(n=var(nx,ny,nz)),nx=1,size(var,dim=1)),ny=1,size(var,dim=2)),nz=1,size(var,dim=3))
    write(vtk(rf)%u,'(A)',iostat=E_IO)repeat(' ',vtk(rf)%indent)//'</DataArray>'
  case(raw,bin_app)
    s_buffer = repeat(' ',vtk(rf)%indent)//'<DataArray type="Int16" Name="'//trim(varname)//&
               '" NumberOfComponents="1" format="appended" offset="'//trim(str(.true.,vtk(rf)%ioffset))//'"/>'
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
  endfunction VTK_VAR_XML_SCAL_3DA_I2

  function VTK_VAR_XML_SCAL_1DA_I1(NC_NN,varname,var,cf) result(E_IO)
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
    write(vtk(rf)%u,'('//trim(str(.true.,NC_NN+1))//'A)',iostat=E_IO)repeat(' ',vtk(rf)%indent),(' '//str(n=var(n1)),n1=1,NC_NN)
    write(vtk(rf)%u,'(A)',iostat=E_IO)repeat(' ',vtk(rf)%indent)//'</DataArray>'
  case(raw,bin_app)
    s_buffer=repeat(' ',vtk(rf)%indent)//'<DataArray type="Int8" Name="'//trim(varname)//&
             '" NumberOfComponents="1" format="appended" offset="'//trim(str(.true.,vtk(rf)%ioffset))//'"/>'
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
  endfunction VTK_VAR_XML_SCAL_1DA_I1

  function VTK_VAR_XML_SCAL_3DA_I1(NC_NN,varname,var,cf) result(E_IO)
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
    write(vtk(rf)%u,'('//trim(str(.true.,NC_NN+1))//'A)',iostat=E_IO)repeat(' ',vtk(rf)%indent),&
                                      (((' '//str(n=var(nx,ny,nz)),nx=1,size(var,dim=1)),ny=1,size(var,dim=2)),nz=1,size(var,dim=3))
    write(vtk(rf)%u,'(A)',iostat=E_IO)repeat(' ',vtk(rf)%indent)//'</DataArray>'
  case(raw,bin_app)
    s_buffer=repeat(' ',vtk(rf)%indent)//'<DataArray type="Int8" Name="'//trim(varname)//&
             '" NumberOfComponents="1" format="appended" offset="'//trim(str(.true.,vtk(rf)%ioffset))//'"/>'
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
  endfunction VTK_VAR_XML_SCAL_3DA_I1

  function VTK_VAR_XML_VECT_1DA_R8(NC_NN,varname,varX,varY,varZ,cf) result(E_IO)
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
               '" NumberOfComponents="3" format="appended" offset="'//trim(str(.true.,vtk(rf)%ioffset))//'"/>'
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
  endfunction VTK_VAR_XML_VECT_1DA_R8

  function VTK_VAR_XML_VECT_3DA_R8(NC_NN,varname,varX,varY,varZ,cf) result(E_IO)
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
               '" NumberOfComponents="3" format="appended" offset="'//trim(str(.true.,vtk(rf)%ioffset))//'"/>'
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
  endfunction VTK_VAR_XML_VECT_3DA_R8

  function VTK_VAR_XML_VECT_1DA_R4(NC_NN,varname,varX,varY,varZ,cf) result(E_IO)
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
               '" NumberOfComponents="3" format="appended" offset="'//trim(str(.true.,vtk(rf)%ioffset))//'"/>'
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
  endfunction VTK_VAR_XML_VECT_1DA_R4

  function VTK_VAR_XML_VECT_3DA_R4(NC_NN,varname,varX,varY,varZ,cf) result(E_IO)
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
               '" NumberOfComponents="3" format="appended" offset="'//trim(str(.true.,vtk(rf)%ioffset))//'"/>'
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
  endfunction VTK_VAR_XML_VECT_3DA_R4

  function VTK_VAR_XML_VECT_1DA_I8(NC_NN,varname,varX,varY,varZ,cf) result(E_IO)
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
               '" NumberOfComponents="3" format="appended" offset="'//trim(str(.true.,vtk(rf)%ioffset))//'"/>'
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
  endfunction VTK_VAR_XML_VECT_1DA_I8

  function VTK_VAR_XML_VECT_3DA_I8(NC_NN,varname,varX,varY,varZ,cf) result(E_IO)
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
               '" NumberOfComponents="3" format="appended" offset="'//trim(str(.true.,vtk(rf)%ioffset))//'"/>'
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
  endfunction VTK_VAR_XML_VECT_3DA_I8

  function VTK_VAR_XML_VECT_1DA_I4(NC_NN,varname,varX,varY,varZ,cf) result(E_IO)
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
               '" NumberOfComponents="3" format="appended" offset="'//trim(str(.true.,vtk(rf)%ioffset))//'"/>'
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
  endfunction VTK_VAR_XML_VECT_1DA_I4

  function VTK_VAR_XML_VECT_3DA_I4(NC_NN,varname,varX,varY,varZ,cf) result(E_IO)
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
               '" NumberOfComponents="3" format="appended" offset="'//trim(str(.true.,vtk(rf)%ioffset))//'"/>'
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
  endfunction VTK_VAR_XML_VECT_3DA_I4

  function VTK_VAR_XML_VECT_1DA_I2(NC_NN,varname,varX,varY,varZ,cf) result(E_IO)
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
               '" NumberOfComponents="3" format="appended" offset="'//trim(str(.true.,vtk(rf)%ioffset))//'"/>'
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
  endfunction VTK_VAR_XML_VECT_1DA_I2

  function VTK_VAR_XML_VECT_3DA_I2(NC_NN,varname,varX,varY,varZ,cf) result(E_IO)
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
               '" NumberOfComponents="3" format="appended" offset="'//trim(str(.true.,vtk(rf)%ioffset))//'"/>'
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
  endfunction VTK_VAR_XML_VECT_3DA_I2

  function VTK_VAR_XML_VECT_1DA_I1(NC_NN,varname,varX,varY,varZ,cf) result(E_IO)
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
             '" NumberOfComponents="3" format="appended" offset="'//trim(str(.true.,vtk(rf)%ioffset))//'"/>'
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
  endfunction VTK_VAR_XML_VECT_1DA_I1

  function VTK_VAR_XML_VECT_3DA_I1(NC_NN,varname,varX,varY,varZ,cf) result(E_IO)
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
             '" NumberOfComponents="3" format="appended" offset="'//trim(str(.true.,vtk(rf)%ioffset))//'"/>'
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
  endfunction VTK_VAR_XML_VECT_3DA_I1

  function VTK_VAR_XML_LIST_1DA_R8(NC_NN,N_COL,varname,var,cf) result(E_IO)
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
               trim(str(.true.,N_COL))//'" format="ascii">'
    write(vtk(rf)%u,'(A)',iostat=E_IO)trim(s_buffer)
    do n2=1,NC_NN
      write(vtk(rf)%u,'('//trim(str(.true.,N_COL+1))//'A)',iostat=E_IO)repeat(' ',vtk(rf)%indent),&
                                                                       (' '//str(n=var(n1,n2)),n1=1,N_COL)
    enddo
    write(vtk(rf)%u,'(A)',iostat=E_IO)repeat(' ',vtk(rf)%indent)//'</DataArray>'
  case(raw,bin_app)
    s_buffer = repeat(' ',vtk(rf)%indent)//'<DataArray type="Float64" Name="'//trim(varname)//'" NumberOfComponents="'// &
               trim(str(.true.,N_COL))//'" format="appended" offset="'//trim(str(.true.,vtk(rf)%ioffset))//'"/>'
    write(vtk(rf)%u,iostat=E_IO)trim(s_buffer)//end_rec
    call vtk(rf)%byte_update(N_Byte = N_COL*NC_NN*BYR8P)
    write(vtk(rf)%ua,iostat=E_IO)vtk(rf)%N_Byte,'R8',N_COL*NC_NN
    write(vtk(rf)%ua,iostat=E_IO)var
  case(binary)
    s_buffer = repeat(' ',vtk(rf)%indent)//'<DataArray type="Float64" Name="'//trim(varname)//'" NumberOfComponents="'// &
               trim(str(.true.,N_COL))//'" format="binary">'
    write(vtk(rf)%u,iostat=E_IO)trim(s_buffer)//end_rec
    call pack_data(a1=[int(N_COL*NC_NN*BYR8P,I4P)],a2=reshape(var,[N_COL*NC_NN]),packed=varp)
    call b64_encode(n=varp,code=var64) ; deallocate(varp)
    write(vtk(rf)%u,iostat=E_IO)repeat(' ',vtk(rf)%indent+2)//var64//end_rec ; deallocate(var64)
    write(vtk(rf)%u,iostat=E_IO)repeat(' ',vtk(rf)%indent)//'</DataArray>'//end_rec
  endselect
  return
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction VTK_VAR_XML_LIST_1DA_R8

  function VTK_VAR_XML_LIST_3DA_R8(NC_NN,N_COL,varname,var,cf) result(E_IO)
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
               trim(str(.true.,N_COL))//'" format="ascii">'
    write(vtk(rf)%u,'(A)',iostat=E_IO)trim(s_buffer)
    do nz=1,size(var,dim=4) ; do ny=1,size(var,dim=3) ; do nx=1,size(var,dim=2)
      write(vtk(rf)%u,'('//trim(str(.true.,N_COL+1))//'A)',iostat=E_IO)repeat(' ',vtk(rf)%indent),&
                                                                       (' '//str(n=var(n1,nx,ny,nz)),n1=1,N_COL)
    enddo ; enddo ; enddo
    write(vtk(rf)%u,'(A)',iostat=E_IO)repeat(' ',vtk(rf)%indent)//'</DataArray>'
  case(raw,bin_app)
    s_buffer = repeat(' ',vtk(rf)%indent)//'<DataArray type="Float64" Name="'//trim(varname)//'" NumberOfComponents="'// &
               trim(str(.true.,N_COL))//'" format="appended" offset="'//trim(str(.true.,vtk(rf)%ioffset))//'"/>'
    write(vtk(rf)%u,iostat=E_IO)trim(s_buffer)//end_rec
    call vtk(rf)%byte_update(N_Byte = N_COL*NC_NN*BYR8P)
    write(vtk(rf)%ua,iostat=E_IO)vtk(rf)%N_Byte,'R8',N_COL*NC_NN
    write(vtk(rf)%ua,iostat=E_IO)var
  case(binary)
    s_buffer = repeat(' ',vtk(rf)%indent)//'<DataArray type="Float64" Name="'//trim(varname)//'" NumberOfComponents="'// &
               trim(str(.true.,N_COL))//'" format="binary">'
    write(vtk(rf)%u,iostat=E_IO)trim(s_buffer)//end_rec
    call pack_data(a1=[int(N_COL*NC_NN*BYR8P,I4P)],a2=reshape(var,[N_COL*NC_NN]),packed=varp)
    call b64_encode(n=varp,code=var64) ; deallocate(varp)
    write(vtk(rf)%u,iostat=E_IO)repeat(' ',vtk(rf)%indent+2)//var64//end_rec ; deallocate(var64)
    write(vtk(rf)%u,iostat=E_IO)repeat(' ',vtk(rf)%indent)//'</DataArray>'//end_rec
  endselect
  return
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction VTK_VAR_XML_LIST_3DA_R8

  function VTK_VAR_XML_LIST_1DA_R4(NC_NN,N_COL,varname,var,cf) result(E_IO)
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
               trim(str(.true.,N_COL))//'" format="ascii">'
    write(vtk(rf)%u,'(A)',iostat=E_IO)trim(s_buffer)
    do n2=1,NC_NN
      write(vtk(rf)%u,'('//trim(str(.true.,N_COL+1))//'A)',iostat=E_IO)repeat(' ',vtk(rf)%indent),&
                                                                       (' '//str(n=var(n1,n2)),n1=1,N_COL)
    enddo
    write(vtk(rf)%u,'(A)',iostat=E_IO)repeat(' ',vtk(rf)%indent)//'</DataArray>'
  case(raw,bin_app)
    s_buffer = repeat(' ',vtk(rf)%indent)//'<DataArray type="Float32" Name="'//trim(varname)//'" NumberOfComponents="'// &
               trim(str(.true.,N_COL))//'" format="appended" offset="'//trim(str(.true.,vtk(rf)%ioffset))//'"/>'
    write(vtk(rf)%u,iostat=E_IO)trim(s_buffer)//end_rec
    call vtk(rf)%byte_update(N_Byte = N_COL*NC_NN*BYR4P)
    write(vtk(rf)%ua,iostat=E_IO)vtk(rf)%N_Byte,'R4',N_COL*NC_NN
    write(vtk(rf)%ua,iostat=E_IO)var
  case(binary)
    s_buffer = repeat(' ',vtk(rf)%indent)//'<DataArray type="Float32" Name="'//trim(varname)//'" NumberOfComponents="'// &
               trim(str(.true.,N_COL))//'" format="binary">'
    write(vtk(rf)%u,iostat=E_IO)trim(s_buffer)//end_rec
    call pack_data(a1=[int(N_COL*NC_NN*BYR4P,I4P)],a2=reshape(var,[N_COL*NC_NN]),packed=varp)
    call b64_encode(n=varp,code=var64) ; deallocate(varp)
    write(vtk(rf)%u,iostat=E_IO)repeat(' ',vtk(rf)%indent+2)//var64//end_rec ; deallocate(var64)
    write(vtk(rf)%u,iostat=E_IO)repeat(' ',vtk(rf)%indent)//'</DataArray>'//end_rec
  endselect
  return
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction VTK_VAR_XML_LIST_1DA_R4

  function VTK_VAR_XML_LIST_3DA_R4(NC_NN,N_COL,varname,var,cf) result(E_IO)
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
               trim(str(.true.,N_COL))//'" format="ascii">'
    write(vtk(rf)%u,'(A)',iostat=E_IO)trim(s_buffer)
    do nz=1,size(var,dim=4) ; do ny=1,size(var,dim=3) ; do nx=1,size(var,dim=2)
      write(vtk(rf)%u,'('//trim(str(.true.,N_COL+1))//'A)',iostat=E_IO)repeat(' ',vtk(rf)%indent),&
                                                                       (' '//str(n=var(n1,nx,ny,nz)),n1=1,N_COL)
    enddo ; enddo ; enddo
    write(vtk(rf)%u,'(A)',iostat=E_IO)repeat(' ',vtk(rf)%indent)//'</DataArray>'
  case(raw,bin_app)
    s_buffer = repeat(' ',vtk(rf)%indent)//'<DataArray type="Float32" Name="'//trim(varname)//'" NumberOfComponents="'// &
               trim(str(.true.,N_COL))//'" format="appended" offset="'//trim(str(.true.,vtk(rf)%ioffset))//'"/>'
    write(vtk(rf)%u,iostat=E_IO)trim(s_buffer)//end_rec
    call vtk(rf)%byte_update(N_Byte = N_COL*NC_NN*BYR4P)
    write(vtk(rf)%ua,iostat=E_IO)vtk(rf)%N_Byte,'R4',N_COL*NC_NN
    write(vtk(rf)%ua,iostat=E_IO)var
  case(binary)
    s_buffer = repeat(' ',vtk(rf)%indent)//'<DataArray type="Float32" Name="'//trim(varname)//'" NumberOfComponents="'// &
               trim(str(.true.,N_COL))//'" format="binary">'
    write(vtk(rf)%u,iostat=E_IO)trim(s_buffer)//end_rec
    call pack_data(a1=[int(N_COL*NC_NN*BYR4P,I4P)],a2=reshape(var,[N_COL*NC_NN]),packed=varp)
    call b64_encode(n=varp,code=var64) ; deallocate(varp)
    write(vtk(rf)%u,iostat=E_IO)repeat(' ',vtk(rf)%indent+2)//var64//end_rec ; deallocate(var64)
    write(vtk(rf)%u,iostat=E_IO)repeat(' ',vtk(rf)%indent)//'</DataArray>'//end_rec
  endselect
  return
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction VTK_VAR_XML_LIST_3DA_R4

  function VTK_VAR_XML_LIST_1DA_I8(NC_NN,N_COL,varname,var,cf) result(E_IO)
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
               trim(str(.true.,N_COL))//'" format="ascii">'
    write(vtk(rf)%u,'(A)',iostat=E_IO)trim(s_buffer)
    do n2=1,NC_NN
      write(vtk(rf)%u,'('//trim(str(.true.,N_COL+1))//'A)',iostat=E_IO)repeat(' ',vtk(rf)%indent),&
                                                                       (' '//str(n=var(n1,n2)),n1=1,N_COL)
    enddo
    write(vtk(rf)%u,'(A)',iostat=E_IO)repeat(' ',vtk(rf)%indent)//'</DataArray>'
  case(raw,bin_app)
    s_buffer = repeat(' ',vtk(rf)%indent)//'<DataArray type="Int64" Name="'//trim(varname)//'" NumberOfComponents="'// &
               trim(str(.true.,N_COL))//'" format="appended" offset="'//trim(str(.true.,vtk(rf)%ioffset))//'"/>'
    write(vtk(rf)%u,iostat=E_IO)trim(s_buffer)//end_rec
    call vtk(rf)%byte_update(N_Byte = int(N_COL*NC_NN*BYI8P,I4P))
    write(vtk(rf)%ua,iostat=E_IO)vtk(rf)%N_Byte,'I8',N_COL*NC_NN
    write(vtk(rf)%ua,iostat=E_IO)var
  case(binary)
    s_buffer = repeat(' ',vtk(rf)%indent)//'<DataArray type="Int64" Name="'//trim(varname)//'" NumberOfComponents="'// &
               trim(str(.true.,N_COL))//'" format="binary">'
    write(vtk(rf)%u,iostat=E_IO)trim(s_buffer)//end_rec
    call pack_data(a1=[int(N_COL*NC_NN*BYI8P,I4P)],a2=reshape(var,[N_COL*NC_NN]),packed=varp)
    call b64_encode(n=varp,code=var64) ; deallocate(varp)
    write(vtk(rf)%u,iostat=E_IO)repeat(' ',vtk(rf)%indent+2)//var64//end_rec ; deallocate(var64)
    write(vtk(rf)%u,iostat=E_IO)repeat(' ',vtk(rf)%indent)//'</DataArray>'//end_rec
  endselect
  return
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction VTK_VAR_XML_LIST_1DA_I8

  function VTK_VAR_XML_LIST_3DA_I8(NC_NN,N_COL,varname,var,cf) result(E_IO)
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
               trim(str(.true.,N_COL))//'" format="ascii">'
    write(vtk(rf)%u,'(A)',iostat=E_IO)trim(s_buffer)
    do nz=1,size(var,dim=4) ; do ny=1,size(var,dim=3) ; do nx=1,size(var,dim=2)
      write(vtk(rf)%u,'('//trim(str(.true.,N_COL+1))//'A)',iostat=E_IO)repeat(' ',vtk(rf)%indent),&
                                                                       (' '//str(n=var(n1,nx,ny,nz)),n1=1,N_COL)
    enddo ; enddo ; enddo
    write(vtk(rf)%u,'(A)',iostat=E_IO)repeat(' ',vtk(rf)%indent)//'</DataArray>'
  case(raw,bin_app)
    s_buffer = repeat(' ',vtk(rf)%indent)//'<DataArray type="Int64" Name="'//trim(varname)//'" NumberOfComponents="'// &
               trim(str(.true.,N_COL))//'" format="appended" offset="'//trim(str(.true.,vtk(rf)%ioffset))//'"/>'
    write(vtk(rf)%u,iostat=E_IO)trim(s_buffer)//end_rec
    call vtk(rf)%byte_update(N_Byte = int(N_COL*NC_NN*BYI8P,I4P))
    write(vtk(rf)%ua,iostat=E_IO)vtk(rf)%N_Byte,'I8',N_COL*NC_NN
    write(vtk(rf)%ua,iostat=E_IO)var
  case(binary)
    s_buffer = repeat(' ',vtk(rf)%indent)//'<DataArray type="Int64" Name="'//trim(varname)//'" NumberOfComponents="'// &
               trim(str(.true.,N_COL))//'" format="binary">'
    write(vtk(rf)%u,iostat=E_IO)trim(s_buffer)//end_rec
    call pack_data(a1=[int(N_COL*NC_NN*BYI8P,I4P)],a2=reshape(var,[N_COL*NC_NN]),packed=varp)
    call b64_encode(n=varp,code=var64) ; deallocate(varp)
    write(vtk(rf)%u,iostat=E_IO)repeat(' ',vtk(rf)%indent+2)//var64//end_rec ; deallocate(var64)
    write(vtk(rf)%u,iostat=E_IO)repeat(' ',vtk(rf)%indent)//'</DataArray>'//end_rec
  endselect
  return
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction VTK_VAR_XML_LIST_3DA_I8

  function VTK_VAR_XML_LIST_1DA_I4(NC_NN,N_COL,varname,var,cf) result(E_IO)
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
               trim(str(.true.,N_COL))//'" format="ascii">'
    write(vtk(rf)%u,'(A)',iostat=E_IO)trim(s_buffer)
    do n2=1,NC_NN
      write(vtk(rf)%u,'('//trim(str(.true.,N_COL+1))//'A)',iostat=E_IO)repeat(' ',vtk(rf)%indent),&
                                                                       (' '//str(n=var(n1,n2)),n1=1,N_COL)
    enddo
    write(vtk(rf)%u,'(A)',iostat=E_IO)repeat(' ',vtk(rf)%indent)//'</DataArray>'
  case(raw,bin_app)
    s_buffer = repeat(' ',vtk(rf)%indent)//'<DataArray type="Int32" Name="'//trim(varname)//'" NumberOfComponents="'// &
               trim(str(.true.,N_COL))//'" format="appended" offset="'//trim(str(.true.,vtk(rf)%ioffset))//'"/>'
    write(vtk(rf)%u,iostat=E_IO)trim(s_buffer)//end_rec
    call vtk(rf)%byte_update(N_Byte = N_COL*NC_NN*BYI4P)
    write(vtk(rf)%ua,iostat=E_IO)vtk(rf)%N_Byte,'I4',N_COL*NC_NN
    write(vtk(rf)%ua,iostat=E_IO)var
  case(binary)
    s_buffer = repeat(' ',vtk(rf)%indent)//'<DataArray type="Int32" Name="'//trim(varname)//'" NumberOfComponents="'// &
               trim(str(.true.,N_COL))//'" format="binary">'
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
  endfunction VTK_VAR_XML_LIST_1DA_I4

  function VTK_VAR_XML_LIST_3DA_I4(NC_NN,N_COL,varname,var,cf) result(E_IO)
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
               trim(str(.true.,N_COL))//'" format="ascii">'
    write(vtk(rf)%u,'(A)',iostat=E_IO)trim(s_buffer)
    do nz=1,size(var,dim=4) ; do ny=1,size(var,dim=3) ; do nx=1,size(var,dim=2)
      write(vtk(rf)%u,'('//trim(str(.true.,N_COL+1))//'A)',iostat=E_IO)repeat(' ',vtk(rf)%indent),&
                                                                       (' '//str(n=var(n1,nx,ny,nz)),n1=1,N_COL)
    enddo ; enddo ; enddo
    write(vtk(rf)%u,'(A)',iostat=E_IO)repeat(' ',vtk(rf)%indent)//'</DataArray>'
  case(raw,bin_app)
    s_buffer = repeat(' ',vtk(rf)%indent)//'<DataArray type="Int32" Name="'//trim(varname)//'" NumberOfComponents="'// &
               trim(str(.true.,N_COL))//'" format="appended" offset="'//trim(str(.true.,vtk(rf)%ioffset))//'"/>'
    write(vtk(rf)%u,iostat=E_IO)trim(s_buffer)//end_rec
    call vtk(rf)%byte_update(N_Byte = N_COL*NC_NN*BYI4P)
    write(vtk(rf)%ua,iostat=E_IO)vtk(rf)%N_Byte,'I4',N_COL*NC_NN
    write(vtk(rf)%ua,iostat=E_IO)var
  case(binary)
    s_buffer = repeat(' ',vtk(rf)%indent)//'<DataArray type="Int32" Name="'//trim(varname)//'" NumberOfComponents="'// &
               trim(str(.true.,N_COL))//'" format="binary">'
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
  endfunction VTK_VAR_XML_LIST_3DA_I4

  function VTK_VAR_XML_LIST_1DA_I2(NC_NN,N_COL,varname,var,cf) result(E_IO)
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
               trim(str(.true.,N_COL))//'" format="ascii">'
    write(vtk(rf)%u,'(A)',iostat=E_IO)trim(s_buffer)
    do n2=1,NC_NN
      write(vtk(rf)%u,'('//trim(str(.true.,N_COL+1))//'A)',iostat=E_IO)repeat(' ',vtk(rf)%indent),&
                                                                       (' '//str(n=var(n1,n2)),n1=1,N_COL)
    enddo
    write(vtk(rf)%u,'(A)',iostat=E_IO)repeat(' ',vtk(rf)%indent)//'</DataArray>'
  case(raw,bin_app)
    s_buffer = repeat(' ',vtk(rf)%indent)//'<DataArray type="Int16" Name="'//trim(varname)//'" NumberOfComponents="'// &
               trim(str(.true.,N_COL))//'" format="appended" offset="'//trim(str(.true.,vtk(rf)%ioffset))//'"/>'
    write(vtk(rf)%u,iostat=E_IO)trim(s_buffer)//end_rec
    call vtk(rf)%byte_update(N_Byte = N_COL*NC_NN*BYI2P)
    write(vtk(rf)%ua,iostat=E_IO)vtk(rf)%N_Byte,'I2',N_COL*NC_NN
    write(vtk(rf)%ua,iostat=E_IO)var
  case(binary)
    s_buffer = repeat(' ',vtk(rf)%indent)//'<DataArray type="Int16" Name="'//trim(varname)//'" NumberOfComponents="'// &
               trim(str(.true.,N_COL))//'" format="binary">'
    write(vtk(rf)%u,iostat=E_IO)trim(s_buffer)//end_rec
    call pack_data(a1=[int(N_COL*NC_NN*BYI2P,I4P)],a2=reshape(var,[N_COL*NC_NN]),packed=varp)
    call b64_encode(n=varp,code=var64) ; deallocate(varp)
    write(vtk(rf)%u,iostat=E_IO)repeat(' ',vtk(rf)%indent+2)//var64//end_rec ; deallocate(var64)
    write(vtk(rf)%u,iostat=E_IO)repeat(' ',vtk(rf)%indent)//'</DataArray>'//end_rec
  endselect
  return
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction VTK_VAR_XML_LIST_1DA_I2

  function VTK_VAR_XML_LIST_3DA_I2(NC_NN,N_COL,varname,var,cf) result(E_IO)
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
               trim(str(.true.,N_COL))//'" format="ascii">'
    write(vtk(rf)%u,'(A)',iostat=E_IO)trim(s_buffer)
    do nz=1,size(var,dim=4) ; do ny=1,size(var,dim=3) ; do nx=1,size(var,dim=2)
      write(vtk(rf)%u,'('//trim(str(.true.,N_COL+1))//'A)',iostat=E_IO)repeat(' ',vtk(rf)%indent),&
                                                                       (' '//str(n=var(n1,nx,ny,nz)),n1=1,N_COL)
    enddo ; enddo ; enddo
    write(vtk(rf)%u,'(A)',iostat=E_IO)repeat(' ',vtk(rf)%indent)//'</DataArray>'
  case(raw,bin_app)
    s_buffer = repeat(' ',vtk(rf)%indent)//'<DataArray type="Int16" Name="'//trim(varname)//'" NumberOfComponents="'// &
               trim(str(.true.,N_COL))//'" format="appended" offset="'//trim(str(.true.,vtk(rf)%ioffset))//'"/>'
    write(vtk(rf)%u,iostat=E_IO)trim(s_buffer)//end_rec
    call vtk(rf)%byte_update(N_Byte = N_COL*NC_NN*BYI2P)
    write(vtk(rf)%ua,iostat=E_IO)vtk(rf)%N_Byte,'I2',N_COL*NC_NN
    write(vtk(rf)%ua,iostat=E_IO)var
  case(binary)
    s_buffer = repeat(' ',vtk(rf)%indent)//'<DataArray type="Int16" Name="'//trim(varname)//'" NumberOfComponents="'// &
               trim(str(.true.,N_COL))//'" format="binary">'
    write(vtk(rf)%u,iostat=E_IO)trim(s_buffer)//end_rec
    call pack_data(a1=[int(N_COL*NC_NN*BYI2P,I4P)],a2=reshape(var,[N_COL*NC_NN]),packed=varp)
    call b64_encode(n=varp,code=var64) ; deallocate(varp)
    write(vtk(rf)%u,iostat=E_IO)repeat(' ',vtk(rf)%indent+2)//var64//end_rec ; deallocate(var64)
    write(vtk(rf)%u,iostat=E_IO)repeat(' ',vtk(rf)%indent)//'</DataArray>'//end_rec
  endselect
  return
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction VTK_VAR_XML_LIST_3DA_I2

  function VTK_VAR_XML_LIST_1DA_I1(NC_NN,N_COL,varname,var,cf) result(E_IO)
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
               trim(str(.true.,N_COL))//'" format="ascii">'
    write(vtk(rf)%u,'(A)',iostat=E_IO)trim(s_buffer)
    do n2=1,NC_NN
      write(vtk(rf)%u,'('//trim(str(.true.,N_COL+1))//'A)',iostat=E_IO)repeat(' ',vtk(rf)%indent),&
                                                                       (' '//str(n=var(n1,n2)),n1=1,N_COL)
    enddo
    write(vtk(rf)%u,'(A)',iostat=E_IO)repeat(' ',vtk(rf)%indent)//'</DataArray>'
  case(raw,bin_app)
    s_buffer = repeat(' ',vtk(rf)%indent)//'<DataArray type="Int8" Name="'//trim(varname)//'" NumberOfComponents="'// &
               trim(str(.true.,N_COL))//'" format="appended" offset="'//trim(str(.true.,vtk(rf)%ioffset))//'"/>'
    write(vtk(rf)%u,iostat=E_IO)trim(s_buffer)//end_rec
    call vtk(rf)%byte_update(N_Byte = N_COL*NC_NN*BYI1P)
    write(vtk(rf)%ua,iostat=E_IO)vtk(rf)%N_Byte,'I1',N_COL*NC_NN
    write(vtk(rf)%ua,iostat=E_IO)var
  case(binary)
    s_buffer = repeat(' ',vtk(rf)%indent)//'<DataArray type="Int8" Name="'//trim(varname)//'" NumberOfComponents="'// &
               trim(str(.true.,N_COL))//'" format="binary">'
    write(vtk(rf)%u,iostat=E_IO)trim(s_buffer)//end_rec
    call pack_data(a1=[int(N_COL*NC_NN*BYI1P,I4P)],a2=reshape(var,[N_COL*NC_NN]),packed=varp)
    call b64_encode(n=varp,code=var64) ; deallocate(varp)
    write(vtk(rf)%u,iostat=E_IO)repeat(' ',vtk(rf)%indent+2)//var64//end_rec ; deallocate(var64)
    write(vtk(rf)%u,iostat=E_IO)repeat(' ',vtk(rf)%indent)//'</DataArray>'//end_rec
  endselect
  return
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction VTK_VAR_XML_LIST_1DA_I1

  function VTK_VAR_XML_LIST_3DA_I1(NC_NN,N_COL,varname,var,cf) result(E_IO)
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
               trim(str(.true.,N_COL))//'" format="ascii">'
    write(vtk(rf)%u,'(A)',iostat=E_IO)trim(s_buffer)
    do nz=1,size(var,dim=4) ; do ny=1,size(var,dim=3) ; do nx=1,size(var,dim=2)
      write(vtk(rf)%u,'('//trim(str(.true.,N_COL+1))//'A)',iostat=E_IO)repeat(' ',vtk(rf)%indent),&
                                                                       (' '//str(n=var(n1,nx,ny,nz)),n1=1,N_COL)
    enddo ; enddo ; enddo
    write(vtk(rf)%u,'(A)',iostat=E_IO)repeat(' ',vtk(rf)%indent)//'</DataArray>'
  case(raw,bin_app)
    s_buffer = repeat(' ',vtk(rf)%indent)//'<DataArray type="Int8" Name="'//trim(varname)//'" NumberOfComponents="'// &
               trim(str(.true.,N_COL))//'" format="appended" offset="'//trim(str(.true.,vtk(rf)%ioffset))//'"/>'
    write(vtk(rf)%u,iostat=E_IO)trim(s_buffer)//end_rec
    call vtk(rf)%byte_update(N_Byte = N_COL*NC_NN*BYI1P)
    write(vtk(rf)%ua,iostat=E_IO)vtk(rf)%N_Byte,'I1',N_COL*NC_NN
    write(vtk(rf)%ua,iostat=E_IO)var
  case(binary)
    s_buffer = repeat(' ',vtk(rf)%indent)//'<DataArray type="Int8" Name="'//trim(varname)//'" NumberOfComponents="'// &
               trim(str(.true.,N_COL))//'" format="binary">'
    write(vtk(rf)%u,iostat=E_IO)trim(s_buffer)//end_rec
    call pack_data(a1=[int(N_COL*NC_NN*BYI1P,I4P)],a2=reshape(var,[N_COL*NC_NN]),packed=varp)
    call b64_encode(n=varp,code=var64) ; deallocate(varp)
    write(vtk(rf)%u,iostat=E_IO)repeat(' ',vtk(rf)%indent+2)//var64//end_rec ; deallocate(var64)
    write(vtk(rf)%u,iostat=E_IO)repeat(' ',vtk(rf)%indent)//'</DataArray>'//end_rec
  endselect
  return
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction VTK_VAR_XML_LIST_3DA_I1

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

  function PVTK_INI_XML(filename,mesh_topology,tp,cf,nx1,nx2,ny1,ny2,nz1,nz2) result(E_IO)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Function for initializing parallel (partitioned) VTK-XML file.
  !---------------------------------------------------------------------------------------------------------------------------------
  implicit none
  character(*), intent(IN)::            filename      !< File name.
  character(*), intent(IN)::            mesh_topology !< Mesh topology.
  character(*), intent(IN)::            tp            !< Type of geometry representation (Float32, Float64, ecc).
  integer(I4P), intent(OUT), optional:: cf            !< Current file index (for concurrent files IO).
  integer(I4P), intent(IN),  optional:: nx1           !< Initial node of x axis.
  integer(I4P), intent(IN),  optional:: nx2           !< Final node of x axis.
  integer(I4P), intent(IN),  optional:: ny1           !< Initial node of y axis.
  integer(I4P), intent(IN),  optional:: ny2           !< Final node of y axis.
  integer(I4P), intent(IN),  optional:: nz1           !< Initial node of z axis.
  integer(I4P), intent(IN),  optional:: nz2           !< Final node of z axis.
  integer(I4P)::                        E_IO          !< Input/Output inquiring flag: $0$ if IO is done, $> 0$ if IO is not done.
  character(len=maxlen)::               s_buffer      !< Buffer string.
  integer(I4P)::                        rf            !< Real file index.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  E_IO = -1_I4P
  if (.not.ir_initialized) call IR_Init
  if (.not.b64_initialized) call b64_init
  call vtk_update(act='add',cf=rf,Nvtk=Nvtk,vtk=vtk)
  f = rf
  if (present(cf)) cf = rf
  vtk(rf)%topology = trim(mesh_topology)
  open(unit=Get_Unit(vtk(rf)%u),file=trim(filename),&
       form='FORMATTED',access='SEQUENTIAL',action='WRITE',status='REPLACE',iostat=E_IO)
  write(unit=vtk(rf)%u,fmt='(A)',iostat=E_IO)'<?xml version="1.0"?>'
  if (endian==endianL) then
    s_buffer = '<VTKFile type="'//trim(vtk(rf)%topology)//'" version="0.1" byte_order="LittleEndian">'
  else
    s_buffer = '<VTKFile type="'//trim(vtk(rf)%topology)//'" version="0.1" byte_order="BigEndian">'
  endif
  write(unit=vtk(rf)%u,fmt='(A)',iostat=E_IO)trim(s_buffer) ; vtk(rf)%indent = 2
  select case(trim(vtk(rf)%topology))
  case('PRectilinearGrid')
    s_buffer = repeat(' ',vtk(rf)%indent)//'<'//trim(vtk(rf)%topology)//' WholeExtent="'//&
               trim(str(n=nx1))//' '//trim(str(n=nx2))//' '//                             &
               trim(str(n=ny1))//' '//trim(str(n=ny2))//' '//                             &
               trim(str(n=nz1))//' '//trim(str(n=nz2))//'" GhostLevel="#">'
    write(unit=vtk(rf)%u,fmt='(A)',iostat=E_IO)trim(s_buffer) ; vtk(rf)%indent = vtk(rf)%indent + 2
    write(unit=vtk(rf)%u,fmt='(A)',iostat=E_IO)repeat(' ',vtk(rf)%indent)//'<PCoordinates>' ; vtk(rf)%indent = vtk(rf)%indent + 2
    write(unit=vtk(rf)%u,fmt='(A)',iostat=E_IO)repeat(' ',vtk(rf)%indent)//'<PDataArray type="'//trim(tp)//'"/>'
    write(unit=vtk(rf)%u,fmt='(A)',iostat=E_IO)repeat(' ',vtk(rf)%indent)//'<PDataArray type="'//trim(tp)//'"/>'
    write(unit=vtk(rf)%u,fmt='(A)',iostat=E_IO)repeat(' ',vtk(rf)%indent)//'<PDataArray type="'//trim(tp)//'"/>'
    vtk(rf)%indent = vtk(rf)%indent - 2 ; write(unit=vtk(rf)%u,fmt='(A)',iostat=E_IO)repeat(' ',vtk(rf)%indent)//'</PCoordinates>'
  case('PStructuredGrid')
    s_buffer = repeat(' ',vtk(rf)%indent)//'<'//trim(vtk(rf)%topology)//' WholeExtent="'//&
               trim(str(n=nx1))//' '//trim(str(n=nx2))//' '//                             &
               trim(str(n=ny1))//' '//trim(str(n=ny2))//' '//                             &
               trim(str(n=nz1))//' '//trim(str(n=nz2))//'" GhostLevel="#">'
    write(unit=vtk(rf)%u,fmt='(A)',iostat=E_IO)trim(s_buffer) ; vtk(rf)%indent = vtk(rf)%indent + 2
    write(unit=vtk(rf)%u,fmt='(A)',iostat=E_IO)repeat(' ',vtk(rf)%indent)//'<PPoints>' ; vtk(rf)%indent = vtk(rf)%indent + 2
    s_buffer = repeat(' ',vtk(rf)%indent)//'<PDataArray type="'//trim(tp)//'" NumberOfComponents="3" Name="Points"/>'
    write(unit=vtk(rf)%u,fmt='(A)',iostat=E_IO)trim(s_buffer)
    vtk(rf)%indent = vtk(rf)%indent - 2 ; write(unit=vtk(rf)%u,fmt='(A)',iostat=E_IO)repeat(' ',vtk(rf)%indent)//'</PPoints>'
  case('PUnstructuredGrid')
    s_buffer = repeat(' ',vtk(rf)%indent)//'<'//trim(vtk(rf)%topology)//' GhostLevel="0">'
    write(unit=vtk(rf)%u,fmt='(A)',iostat=E_IO)trim(s_buffer) ; vtk(rf)%indent = vtk(rf)%indent + 2
    write(unit=vtk(rf)%u,fmt='(A)',iostat=E_IO)repeat(' ',vtk(rf)%indent)//'<PPoints>' ; vtk(rf)%indent = vtk(rf)%indent + 2
    s_buffer = repeat(' ',vtk(rf)%indent)//'<PDataArray type="'//trim(tp)//'" NumberOfComponents="3" Name="Points"/>'
    write(unit=vtk(rf)%u,fmt='(A)',iostat=E_IO)trim(s_buffer)
    vtk(rf)%indent = vtk(rf)%indent - 2 ; write(unit=vtk(rf)%u,fmt='(A)',iostat=E_IO)repeat(' ',vtk(rf)%indent)//'</PPoints>'
  endselect
  return
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction PVTK_INI_XML

  function PVTK_GEO_XML(source,cf,nx1,nx2,ny1,ny2,nz1,nz2) result(E_IO)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Function for saving piece geometry source for parallel (partitioned) VTK-XML file.
  !---------------------------------------------------------------------------------------------------------------------------------
  implicit none
  character(*), intent(IN)::           source   !< Source file name containing the piece data.
  integer(I4P), intent(IN), optional:: cf       !< Current file index (for concurrent files IO).
  integer(I4P), intent(IN), optional:: nx1      !< Initial node of x axis.
  integer(I4P), intent(IN), optional:: nx2      !< Final node of x axis.
  integer(I4P), intent(IN), optional:: ny1      !< Initial node of y axis.
  integer(I4P), intent(IN), optional:: ny2      !< Final node of y axis.
  integer(I4P), intent(IN), optional:: nz1      !< Initial node of z axis.
  integer(I4P), intent(IN), optional:: nz2      !< Final node of z axis.
  integer(I4P)::                       E_IO     !< Input/Output inquiring flag: $0$ if IO is done, $> 0$ if IO is not done.
  character(len=maxlen)::              s_buffer !< Buffer string.
  integer(I4P)::                       rf       !< Real file index.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  E_IO = -1_I4P
  rf = f
  if (present(cf)) then
    rf = cf ; f = cf
  endif
  select case (vtk(rf)%topology)
  case('PRectilinearGrid','PStructuredGrid')
    s_buffer = repeat(' ',vtk(rf)%indent)//'<Piece Extent="'// &
               trim(str(n=nx1))//' '//trim(str(n=nx2))//' '// &
               trim(str(n=ny1))//' '//trim(str(n=ny2))//' '// &
               trim(str(n=nz1))//' '//trim(str(n=nz2))//'" Source="'//trim(source)//'"/>'
    write(unit=vtk(rf)%u,fmt='(A)',iostat=E_IO)trim(s_buffer)
  case('PUnstructuredGrid')
    write(unit=vtk(rf)%u,fmt='(A)',iostat=E_IO)repeat(' ',vtk(rf)%indent)//'<Piece Source="'//trim(source)//'"/>'
  endselect
  return
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction PVTK_GEO_XML

  function PVTK_DAT_XML(var_location,var_block_action,cf) result(E_IO)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Function for initializing/finalizing the saving of data associated to the mesh.
  !<
  !< Function that **must** be called before saving the data related to geometric mesh, this function initializes the
  !< saving of data variables indicating the *type* (node or cell centered) of variables that will be saved.
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
  select case(trim(Upper_Case(var_location)))
  case('CELL')
    select case(trim(Upper_Case(var_block_action)))
    case('OPEN')
      write(unit=vtk(rf)%u,fmt='(A)',iostat=E_IO)repeat(' ',vtk(rf)%indent)//'<PCellData>' ; vtk(rf)%indent = vtk(rf)%indent + 2
    case('CLOSE')
      vtk(rf)%indent = vtk(rf)%indent - 2 ; write(unit=vtk(rf)%u,fmt='(A)',iostat=E_IO)repeat(' ',vtk(rf)%indent)//'</PCellData>'
    endselect
  case('NODE')
    select case(trim(Upper_Case(var_block_action)))
    case('OPEN')
      write(unit=vtk(rf)%u,fmt='(A)',iostat=E_IO)repeat(' ',vtk(rf)%indent)//'<PPointData>' ; vtk(rf)%indent = vtk(rf)%indent + 2
    case('CLOSE')
      vtk(rf)%indent = vtk(rf)%indent - 2 ; write(unit=vtk(rf)%u,fmt='(A)',iostat=E_IO)repeat(' ',vtk(rf)%indent)//'</PPointData>'
    endselect
  endselect
  return
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction PVTK_DAT_XML

  function PVTK_VAR_XML(varname,tp,cf,Nc) result(E_IO)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Function for saving variable associated to nodes or cells geometry.
  !---------------------------------------------------------------------------------------------------------------------------------
  implicit none
  character(*), intent(IN)::           varname  !< Variable name.
  character(*), intent(IN)::           tp       !< Type of data representation (Float32, Float64, ecc).
  integer(I4P), intent(IN), optional:: cf       !< Current file index (for concurrent files IO).
  integer(I4P), intent(IN), optional:: Nc       !< Number of components of variable.
  integer(I4P)::                       E_IO     !< Input/Output inquiring flag: $0$ if IO is done, $> 0$ if IO is not done.
  character(len=maxlen)::              s_buffer !< Buffer string.
  integer(I4P)::                       rf       !< Real file index.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  E_IO = -1_I4P
  rf = f
  if (present(cf)) then
    rf = cf ; f = cf
  endif
  if (present(Nc)) then
    s_buffer = repeat(' ',vtk(rf)%indent)//'<PDataArray type="'//trim(tp)//'" Name="'//trim(varname)//&
               '" NumberOfComponents="'//trim(str(.true.,Nc))//'"/>'
  else
    s_buffer = repeat(' ',vtk(rf)%indent)//'<PDataArray type="'//trim(tp)//'" Name="'//trim(varname)//'"/>'
  endif
  write(unit=vtk(rf)%u,fmt='(A)',iostat=E_IO)trim(s_buffer)
  return
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction PVTK_VAR_XML

  function PVTK_END_XML(cf) result(E_IO)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Function for finalizing the parallel (partitioned) VTK-XML file.
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
  write(unit=vtk(rf)%u,fmt='(A)',iostat=E_IO)repeat(' ',vtk(rf)%indent)//'</'//trim(vtk(rf)%topology)//'>'
  write(unit=vtk(rf)%u,fmt='(A)',iostat=E_IO)'</VTKFile>'
  close(unit=vtk(rf)%u,iostat=E_IO)
  call vtk_update(act='remove',cf=rf,Nvtk=Nvtk,vtk=vtk)
  f = rf
  if (present(cf)) cf = rf
  return
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction PVTK_END_XML

  function VTK_INI(output_format,filename,title,mesh_topology,cf) result(E_IO)
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
  character(*), intent(IN)::            output_format !< Output format: ASCII or RAW.
  character(*), intent(IN)::            filename      !< Name of file.
  character(*), intent(IN)::            title         !< Title.
  character(*), intent(IN)::            mesh_topology !< Mesh topology.
  integer(I4P), intent(OUT), optional:: cf            !< Current file index (for concurrent files IO).
  integer(I4P)::                        E_IO          !< Input/Output inquiring flag: $0$ if IO is done, $> 0$ if IO is not done.
  integer(I4P)::                        rf            !< Real file index.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  E_IO = -1_I4P
  if (.not.ir_initialized) call IR_Init
  if (.not.b64_initialized) call b64_init
  call vtk_update(act='add',cf=rf,Nvtk=Nvtk,vtk=vtk)
  f = rf
  if (present(cf)) cf = rf
  vtk(rf)%topology = trim(mesh_topology)
  select case(trim(Upper_Case(output_format)))
  case('ASCII')
    vtk(rf)%f = ascii
    open(unit=Get_Unit(vtk(rf)%u),file=trim(filename),form='FORMATTED',&
         access='SEQUENTIAL',action='WRITE',status='REPLACE',iostat=E_IO)
    ! writing header of file
    write(unit=vtk(rf)%u,fmt='(A)',iostat=E_IO)'# vtk DataFile Version 3.0'
    write(unit=vtk(rf)%u,fmt='(A)',iostat=E_IO)trim(title)
    write(unit=vtk(rf)%u,fmt='(A)',iostat=E_IO)trim(Upper_Case(output_format))
    write(unit=vtk(rf)%u,fmt='(A)',iostat=E_IO)'DATASET '//trim(vtk(rf)%topology)
  case('RAW')
    vtk(rf)%f = raw
    open(unit=Get_Unit(vtk(rf)%u),file=trim(filename),&
         form='UNFORMATTED',access='STREAM',action='WRITE',status='REPLACE',iostat=E_IO)
    ! writing header of file
    write(unit=vtk(rf)%u,iostat=E_IO)'# vtk DataFile Version 3.0'//end_rec
    write(unit=vtk(rf)%u,iostat=E_IO)trim(title)//end_rec
    write(unit=vtk(rf)%u,iostat=E_IO)trim(Upper_Case(output_format))//end_rec
    write(unit=vtk(rf)%u,iostat=E_IO)'DATASET '//trim(vtk(rf)%topology)//end_rec
  endselect
  return
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction VTK_INI

  function VTK_CON(NC,connect,cell_type,cf) result(E_IO)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Function for saving mesh connectivity.
  !<
  !< Function that **must** be used when unstructured grid is used, it saves the connectivity of the unstructured gird.
  !< @note The vector **connect** must follow the VTK-legacy standard. It is passed as *assumed-shape* array
  !< because its dimensions is related to the mesh dimensions in a complex way. Its dimensions can be calculated by the following
  !< equation: \(dc = NC + \sum\limits_{i = 1}^{NC} {nvertex_i }\)
  !< where \(dc\) is connectivity vector dimension and \(nvertex_i\) is the number of vertices of \(i^{th}\) cell. The VTK-
  !< legacy standard for the mesh connectivity is quite obscure at least at first sight. It is more simple analyzing an example.
  !< Suppose we have a mesh composed by 2 cells, one hexahedron (8 vertices) and one pyramid with square basis (5 vertices) and
  !< suppose that the basis of pyramid is constitute by a face of the hexahedron and so the two cells share 4 vertices.
  !< The above equation !> gives \(dc=2+8+5=15\). The connectivity vector for this mesh can be:
  !<
  !<##### first cell
  !<+ connect(1)  = 8 number of vertices of first cell
  !<+ connect(2)  = 0 identification flag of \(1^\circ\) vertex of first cell
  !<+ connect(3)  = 1 identification flag of \(2^\circ\) vertex of first cell
  !<+ connect(4)  = 2 identification flag of \(3^\circ\) vertex of first cell
  !<+ connect(5)  = 3 identification flag of \(4^\circ\) vertex of first cell
  !<+ connect(6)  = 4 identification flag of \(5^\circ\) vertex of first cell
  !<+ connect(7)  = 5 identification flag of \(6^\circ\) vertex of first cell
  !<+ connect(8)  = 6 identification flag of \(7^\circ\) vertex of first cell
  !<+ connect(9)  = 7 identification flag of \(8^\circ\) vertex of first cell
  !<
  !<##### second cell
  !<+ connect(10) = 5 number of vertices of first cell
  !<+ connect(11) = 0 identification flag of \(1^\circ\) vertex of second cell
  !<+ connect(12) = 1 identification flag of \(2^\circ\) vertex of second cell
  !<+ connect(13) = 2 identification flag of \(3^\circ\) vertex of second cell
  !<+ connect(14) = 3 identification flag of \(4^\circ\) vertex of second cell
  !<+ connect(15) = 8 identification flag of \(5^\circ\) vertex of second cell
  !<
  !< Note that the first 4 identification flags of pyramid vertices as the same of the first 4 identification flags of
  !< the hexahedron because the two cells share this face. It is also important to note that the identification flags start
  !< form $0$ value: this is impose to the VTK standard. The function VTK_CON does not calculate the connectivity vector: it
  !< writes the connectivity vector conforming the VTK standard, but does not calculate it.
  !< The vector variable *cell\_type* must conform the VTK-legacy standard (see the file VTK-Standard at the
  !< Kitware homepage). It contains the *type* of each cells. For the above example this vector is:
  !<
  !<##### first cell
  !< cell_type(1) = 12 hexahedron type of first cell
  !<
  !<##### second cell
  !< cell_type(2) = 14 pyramid type of second cell
  !---------------------------------------------------------------------------------------------------------------------------------
  implicit none
  integer(I4P), intent(IN)::           NC              !< Number of cells.
  integer(I4P), intent(IN)::           connect(:)      !< Mesh connectivity.
  integer(I4P), intent(IN)::           cell_type(1:NC) !< VTK cell type.
  integer(I4P), intent(IN), optional:: cf              !< Current file index (for concurrent files IO).
  integer(I4P)::                       E_IO            !< Input/Output inquiring flag: $0$ if IO is done, $> 0$ if IO is not done.
  character(len=maxlen)::              s_buffer        !< Buffer string.
  integer(I4P)::                       ncon            !< Dimension of connectivity vector.
  integer(I4P)::                       rf              !< Real file index.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  E_IO = -1_I4P
  rf = f
  if (present(cf)) then
    rf = cf ; f = cf
  endif
  ncon = size(connect,1)
  select case(vtk(rf)%f)
  case(ascii)
    write(unit=vtk(rf)%u,fmt='(A,2'//FI4P//')',iostat=E_IO)'CELLS ',NC,ncon
    write(unit=vtk(rf)%u,fmt=FI4P,             iostat=E_IO)connect
    write(unit=vtk(rf)%u,fmt='(A,'//FI4P//')', iostat=E_IO)'CELL_TYPES ',NC
    write(unit=vtk(rf)%u,fmt=FI4P,             iostat=E_IO)cell_type
  case(raw)
    write(s_buffer,      fmt='(A,2'//FI4P//')',iostat=E_IO)'CELLS ',NC,ncon
    write(unit=vtk(rf)%u,                      iostat=E_IO)trim(s_buffer)//end_rec
    write(unit=vtk(rf)%u,                      iostat=E_IO)connect
    write(unit=vtk(rf)%u,                      iostat=E_IO)end_rec
    write(s_buffer,      fmt='(A,'//FI4P//')', iostat=E_IO)'CELL_TYPES ',NC
    write(unit=vtk(rf)%u,                      iostat=E_IO)trim(s_buffer)//end_rec
    write(unit=vtk(rf)%u,                      iostat=E_IO)cell_type
    write(unit=vtk(rf)%u,                      iostat=E_IO)end_rec
  endselect
  return
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction VTK_CON

  function VTK_DAT(NC_NN,var_location,cf) result(E_IO)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Function for initializing/finalizing the saving of data associated to the mesh.
  !<
  !< Function that **must** be called before saving the data related to geometric mesh, this function initializes the
  !< saving of data variables indicating the *type* (node or cell centered) of variables that will be saved.
  !< @note A single file can contain both cell and node centered variables. In this case the VTK_DAT function must be
  !< called two times, before saving cell-centered variables and before saving node-centered variables.
  !<
  !<### Examples of usage
  !<
  !<#### Saving node data
  !<```fortran
  !< E_IO=VTK_DAT_XML(50,'node')
  !<```
  !<
  !<#### Saving cell data
  !<```fortran
  !< E_IO=VTK_DAT_XML(50,'cell')
  !<```
  !---------------------------------------------------------------------------------------------------------------------------------
  implicit none
  integer(I4P), intent(IN)::           NC_NN        !< Number of cells or nodes of field.
  character(*), intent(IN)::           var_location !< Location of saving variables: cell for cell-centered, node for node-centered.
  integer(I4P), intent(IN), optional:: cf           !< Current file index (for concurrent files IO).
  integer(I4P)::                       E_IO         !< Input/Output inquiring flag: $0$ if IO is done, $> 0$ if IO is not done.
  character(len=maxlen)::              s_buffer     !< Buffer string.
  integer(I4P)::                       rf           !< Real file index.
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
      write(unit=vtk(rf)%u,fmt='(A,'//FI4P//')',iostat=E_IO)'CELL_DATA ',NC_NN
    case('NODE')
      write(unit=vtk(rf)%u,fmt='(A,'//FI4P//')',iostat=E_IO)'POINT_DATA ',NC_NN
    endselect
  case(raw)
    select case(trim(Upper_Case(var_location)))
    case('CELL')
      write(s_buffer,fmt='(A,'//FI4P//')',iostat=E_IO)'CELL_DATA ',NC_NN
      write(unit=vtk(rf)%u,iostat=E_IO)trim(s_buffer)//end_rec
    case('NODE')
      write(s_buffer,fmt='(A,'//FI4P//')',iostat=E_IO)'POINT_DATA ',NC_NN
      write(unit=vtk(rf)%u,iostat=E_IO)trim(s_buffer)//end_rec
    endselect
  endselect
  return
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction VTK_DAT

  function VTK_VAR_SCAL_R8(NC_NN,varname,var,cf) result(E_IO)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Function for saving field of scalar variable (R8P).
  !---------------------------------------------------------------------------------------------------------------------------------
  implicit none
  integer(I4P), intent(IN)::           NC_NN        !< Number of nodes or cells.
  character(*), intent(IN)::           varname      !< Variable name.
  real(R8P),    intent(IN)::           var(1:NC_NN) !< Variable to be saved.
  integer(I4P), intent(IN), optional:: cf           !< Current file index (for concurrent files IO).
  integer(I4P)::                       E_IO         !< Input/Output inquiring flag: $0$ if IO is done, $> 0$ if IO is not done.
  integer(I4P)::                       rf           !< Real file index.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  E_IO = -1_I4P
  rf = f
  if (present(cf)) then
    rf = cf ; f = cf
  endif
  select case(vtk(rf)%f)
  case(ascii)
    write(unit=vtk(rf)%u,fmt='(A)',iostat=E_IO)'SCALARS '//trim(varname)//' double 1'
    write(unit=vtk(rf)%u,fmt='(A)',iostat=E_IO)'LOOKUP_TABLE default'
    write(unit=vtk(rf)%u,fmt=FR8P, iostat=E_IO)var
  case(raw)
    write(unit=vtk(rf)%u,iostat=E_IO)'SCALARS '//trim(varname)//' double 1'//end_rec
    write(unit=vtk(rf)%u,iostat=E_IO)'LOOKUP_TABLE default'//end_rec
    write(unit=vtk(rf)%u,iostat=E_IO)var
    write(unit=vtk(rf)%u,iostat=E_IO)end_rec
  endselect
  return
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction VTK_VAR_SCAL_R8

  function VTK_VAR_SCAL_R4(NC_NN,varname,var,cf) result(E_IO)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Function for saving field of scalar variable (R4P).
  !---------------------------------------------------------------------------------------------------------------------------------
  implicit none
  integer(I4P), intent(IN)::           NC_NN        !< Number of nodes or cells.
  character(*), intent(IN)::           varname      !< Variable name.
  real(R4P),    intent(IN)::           var(1:NC_NN) !< Variable to be saved.
  integer(I4P), intent(IN), optional:: cf           !< Current file index (for concurrent files IO).
  integer(I4P)::                       E_IO         !< Input/Output inquiring flag: $0$ if IO is done, $> 0$ if IO is not done.
  integer(I4P)::                       rf           !< Real file index.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  E_IO = -1_I4P
  rf = f
  if (present(cf)) then
    rf = cf ; f = cf
  endif
  select case(vtk(rf)%f)
  case(ascii)
    write(unit=vtk(rf)%u,fmt='(A)',iostat=E_IO)'SCALARS '//trim(varname)//' float 1'
    write(unit=vtk(rf)%u,fmt='(A)',iostat=E_IO)'LOOKUP_TABLE default'
    write(unit=vtk(rf)%u,fmt=FR4P, iostat=E_IO)var
  case(raw)
    write(unit=vtk(rf)%u,iostat=E_IO)'SCALARS '//trim(varname)//' float 1'//end_rec
    write(unit=vtk(rf)%u,iostat=E_IO)'LOOKUP_TABLE default'//end_rec
    write(unit=vtk(rf)%u,iostat=E_IO)var
    write(unit=vtk(rf)%u,iostat=E_IO)end_rec
  endselect
  return
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction VTK_VAR_SCAL_R4

  function VTK_VAR_SCAL_I4(NC_NN,varname,var,cf) result(E_IO)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Function for saving field of scalar variable (I4P).
  !---------------------------------------------------------------------------------------------------------------------------------
  implicit none
  integer(I4P), intent(IN)::           NC_NN        !< Number of nodes or cells.
  character(*), intent(IN)::           varname      !< Variable name.
  integer(I4P), intent(IN)::           var(1:NC_NN) !< Variable to be saved.
  integer(I4P), intent(IN), optional:: cf           !< Current file index (for concurrent files IO).
  integer(I4P)::                       E_IO         !< Input/Output inquiring flag: $0$ if IO is done, $> 0$ if IO is not done.
  integer(I4P)::                       rf           !< Real file index.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  E_IO = -1_I4P
  rf = f
  if (present(cf)) then
    rf = cf ; f = cf
  endif
  select case(vtk(rf)%f)
  case(ascii)
    write(unit=vtk(rf)%u,fmt='(A)',iostat=E_IO)'SCALARS '//trim(varname)//' int 1'
    write(unit=vtk(rf)%u,fmt='(A)',iostat=E_IO)'LOOKUP_TABLE default'
    write(unit=vtk(rf)%u,fmt=FI4P, iostat=E_IO)var
  case(raw)
    write(unit=vtk(rf)%u,iostat=E_IO)'SCALARS '//trim(varname)//' int 1'//end_rec
    write(unit=vtk(rf)%u,iostat=E_IO)'LOOKUP_TABLE default'//end_rec
    write(unit=vtk(rf)%u,iostat=E_IO)var
    write(unit=vtk(rf)%u,iostat=E_IO)end_rec
  endselect
  return
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction VTK_VAR_SCAL_I4

  function VTK_VAR_VECT_R8(vec_type,NC_NN,varname,varX,varY,varZ,cf) result(E_IO)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Function for saving field of vectorial variable (R8P).
  !---------------------------------------------------------------------------------------------------------------------------------
  implicit none
  character(*), intent(IN)::           vec_type      !< Vector type: vect = generic vector , norm = normal vector.
  integer(I4P), intent(IN)::           NC_NN         !< Number of nodes or cells.
  character(*), intent(IN)::           varname       !< Variable name.
  real(R8P),    intent(IN)::           varX(1:NC_NN) !< X component of vector.
  real(R8P),    intent(IN)::           varY(1:NC_NN) !< Y component of vector.
  real(R8P),    intent(IN)::           varZ(1:NC_NN) !< Z component of vector.
  integer(I4P), intent(IN), optional:: cf            !< Current file index (for concurrent files IO).
  integer(I4P)::                       E_IO          !< Input/Output inquiring flag: $0$ if IO is done, $> 0$ if IO is not done.
  integer(I4P)::                       rf            !< Real file index.
  integer(I4P)::                       n1            !< Counter.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  E_IO = -1_I4P
  rf = f
  if (present(cf)) then
    rf = cf ; f = cf
  endif
  select case(vtk(rf)%f)
  case(ascii)
    select case(Upper_Case(trim(vec_type)))
    case('VECT')
      write(unit=vtk(rf)%u,fmt='(A)',iostat=E_IO)'VECTORS '//trim(varname)//' double'
    case('NORM')
      write(unit=vtk(rf)%u,fmt='(A)',iostat=E_IO)'NORMALS '//trim(varname)//' double'
    endselect
    write(unit=vtk(rf)%u,fmt='(3'//FR8P//')',iostat=E_IO)(varX(n1),varY(n1),varZ(n1),n1=1,NC_NN)
  case(raw)
    select case(Upper_Case(trim(vec_type)))
    case('VECT')
      write(unit=vtk(rf)%u,iostat=E_IO)'VECTORS '//trim(varname)//' double'//end_rec
    case('NORM')
      write(unit=vtk(rf)%u,iostat=E_IO)'NORMALS '//trim(varname)//' double'//end_rec
    endselect
    write(unit=vtk(rf)%u,iostat=E_IO)(varX(n1),varY(n1),varZ(n1),n1=1,NC_NN)
    write(unit=vtk(rf)%u,iostat=E_IO)end_rec
  endselect
  return
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction VTK_VAR_VECT_R8

  function VTK_VAR_VECT_R4(vec_type,NC_NN,varname,varX,varY,varZ,cf) result(E_IO)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Function for saving field of vectorial variable (R4P).
  !---------------------------------------------------------------------------------------------------------------------------------
  implicit none
  character(*), intent(IN)::           vec_type      !< Vector type: vect = generic vector , norm = normal vector.
  integer(I4P), intent(IN)::           NC_NN         !< Number of nodes or cells.
  character(*), intent(IN)::           varname       !< Variable name.
  real(R4P),    intent(IN)::           varX(1:NC_NN) !< X component of vector.
  real(R4P),    intent(IN)::           varY(1:NC_NN) !< Y component of vector.
  real(R4P),    intent(IN)::           varZ(1:NC_NN) !< Z component of vector.
  integer(I4P), intent(IN), optional:: cf            !< Current file index (for concurrent files IO).
  integer(I4P)::                       E_IO          !< Input/Output inquiring flag: $0$ if IO is done, $> 0$ if IO is not done.
  integer(I4P)::                       rf            !< Real file index.
  integer(I4P)::                       n1            !< Counter.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  E_IO = -1_I4P
  rf = f
  if (present(cf)) then
    rf = cf ; f = cf
  endif
  select case(vtk(rf)%f)
  case(ascii)
    select case(Upper_Case(trim(vec_type)))
    case('vect')
      write(unit=vtk(rf)%u,fmt='(A)',          iostat=E_IO)'VECTORS '//trim(varname)//' float'
    case('norm')
      write(unit=vtk(rf)%u,fmt='(A)',          iostat=E_IO)'NORMALS '//trim(varname)//' float'
    endselect
    write(unit=vtk(rf)%u,fmt='(3'//FR4P//')',iostat=E_IO)(varX(n1),varY(n1),varZ(n1),n1=1,NC_NN)
  case(raw)
    select case(Upper_Case(trim(vec_type)))
    case('vect')
      write(unit=vtk(rf)%u,iostat=E_IO)'VECTORS '//trim(varname)//' float'//end_rec
    case('norm')
      write(unit=vtk(rf)%u,iostat=E_IO)'NORMALS '//trim(varname)//' float'//end_rec
    endselect
    write(unit=vtk(rf)%u,iostat=E_IO)(varX(n1),varY(n1),varZ(n1),n1=1,NC_NN)
    write(unit=vtk(rf)%u,iostat=E_IO)end_rec
  endselect
  return
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction VTK_VAR_VECT_R4

  function VTK_VAR_VECT_I4(NC_NN,varname,varX,varY,varZ,cf) result(E_IO)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Function for saving field of vectorial variable (I4P).
  !---------------------------------------------------------------------------------------------------------------------------------
  implicit none
  integer(I4P), intent(IN)::           NC_NN         !< Number of nodes or cells.
  character(*), intent(IN)::           varname       !< Variable name.
  integer(I4P), intent(IN)::           varX(1:NC_NN) !< X component of vector.
  integer(I4P), intent(IN)::           varY(1:NC_NN) !< Y component of vector.
  integer(I4P), intent(IN)::           varZ(1:NC_NN) !< Z component of vector.
  integer(I4P), intent(IN), optional:: cf            !< Current file index (for concurrent files IO).
  integer(I4P)::                       E_IO          !< Input/Output inquiring flag: $0$ if IO is done, $> 0$ if IO is not done.
  integer(I4P)::                       rf            !< Real file index.
  integer(I4P)::                       n1            !< Counter.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  E_IO = -1_I4P
  rf = f
  if (present(cf)) then
    rf = cf ; f = cf
  endif
  select case(vtk(rf)%f)
  case(ascii)
    write(unit=vtk(rf)%u,fmt='(A)',iostat=E_IO)'VECTORS '//trim(varname)//' int'
    write(unit=vtk(rf)%u,fmt='(3'//FI4P//')',iostat=E_IO)(varX(n1),varY(n1),varZ(n1),n1=1,NC_NN)
  case(raw)
    write(unit=vtk(rf)%u,iostat=E_IO)'VECTORS '//trim(varname)//' int'//end_rec
    write(unit=vtk(rf)%u,iostat=E_IO)(varX(n1),varY(n1),varZ(n1),n1=1,NC_NN)
    write(unit=vtk(rf)%u,iostat=E_IO)end_rec
  endselect
  return
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction VTK_VAR_VECT_I4

  function VTK_VAR_TEXT_R8(NC_NN,dimm,varname,textCoo,cf) result(E_IO)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Function for saving texture variable (R8P).
  !---------------------------------------------------------------------------------------------------------------------------------
  implicit none
  integer(I4P), intent(IN)::           NC_NN                   !< Number of nodes or cells.
  integer(I4P), intent(IN)::           dimm                    !< Texture dimensions.
  character(*), intent(IN)::           varname                 !< Variable name.
  real(R8P),    intent(IN)::           textCoo(1:NC_NN,1:dimm) !< Texture.
  integer(I4P), intent(IN), optional:: cf                      !< Current file index (for concurrent files IO).
  integer(I4P)::                       E_IO              !< Input/Output inquiring flag: $0$ if IO is done, $> 0$ if IO is not done.
  character(len=maxlen)::              s_buffer                !< Buffer string.
  integer(I4P)::                       rf                      !< Real file index.
  integer(I4P)::                       n1,n2                   !< Counters.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  E_IO = -1_I4P
  rf = f
  if (present(cf)) then
    rf = cf ; f = cf
  endif
  select case(vtk(rf)%f)
  case(ascii)
    write(unit=vtk(rf)%u,fmt='(A,1X,'//FI4P//',1X,A)',iostat=E_IO)'TEXTURE_COORDINATES '//trim(varname),dimm,' double'
    write(s_buffer,fmt='(I1)',iostat=E_IO)dimm
    s_buffer='('//trim(s_buffer)//FR4P//')'
    write(unit=vtk(rf)%u,fmt=trim(s_buffer),iostat=E_IO)((textCoo(n1,n2),n2=1,dimm),n1=1,NC_NN)
  case(raw)
    write(s_buffer,fmt='(A,1X,'//FI4P//',1X,A)',iostat=E_IO)'TEXTURE_COORDINATES '//trim(varname),dimm,' double'
    write(unit=vtk(rf)%u,iostat=E_IO)trim(s_buffer)//end_rec
    write(unit=vtk(rf)%u,iostat=E_IO)((textCoo(n1,n2),n2=1,dimm),n1=1,NC_NN)
    write(unit=vtk(rf)%u,iostat=E_IO)end_rec
  endselect
  return
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction VTK_VAR_TEXT_R8

  function VTK_VAR_TEXT_R4(NC_NN,dimm,varname,textCoo,cf) result(E_IO)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Function for saving texture variable (R4P).
  !---------------------------------------------------------------------------------------------------------------------------------
  implicit none
  integer(I4P), intent(IN)::           NC_NN                   !< Number of nodes or cells.
  integer(I4P), intent(IN)::           dimm                    !< Texture dimensions.
  character(*), intent(IN)::           varname                 !< Variable name.
  real(R4P),    intent(IN)::           textCoo(1:NC_NN,1:dimm) !< Texture.
  integer(I4P), intent(IN), optional:: cf                      !< Current file index (for concurrent files IO).
  integer(I4P)::                       E_IO              !< Input/Output inquiring flag: $0$ if IO is done, $> 0$ if IO is not done.
  character(len=maxlen)::              s_buffer                !< Buffer string.
  integer(I4P)::                       rf                      !< Real file index.
  integer(I4P)::                       n1,n2                   !< Counters.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  E_IO = -1_I4P
  rf = f
  if (present(cf)) then
    rf = cf ; f = cf
  endif
  select case(vtk(rf)%f)
  case(ascii)
    write(unit=vtk(rf)%u,fmt='(A,1X,'//FI4P//',1X,A)',iostat=E_IO)'TEXTURE_COORDINATES '//trim(varname),dimm,' float'
    write(s_buffer,fmt='(I1)',iostat=E_IO)dimm
    s_buffer='('//trim(s_buffer)//FR4P//')'
    write(unit=vtk(rf)%u,fmt=trim(s_buffer),iostat=E_IO)((textCoo(n1,n2),n2=1,dimm),n1=1,NC_NN)
  case(raw)
    write(s_buffer,fmt='(A,1X,'//FI4P//',1X,A)',iostat=E_IO)'TEXTURE_COORDINATES '//trim(varname),dimm,' float'
    write(unit=vtk(rf)%u,iostat=E_IO)trim(s_buffer)//end_rec
    write(unit=vtk(rf)%u,iostat=E_IO)((textCoo(n1,n2),n2=1,dimm),n1=1,NC_NN)
    write(unit=vtk(rf)%u,iostat=E_IO)end_rec
  endselect
  return
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction VTK_VAR_TEXT_R4

  function VTK_END(cf) result(E_IO)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Function for finalizing the VTK-XML file.
  !<
  !<### Usage
  !<```fortran
  !< E_IO = VTK_END()
  !<```
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
  close(unit=vtk(rf)%u,iostat=E_IO)
  call vtk_update(act='remove',cf=rf,Nvtk=Nvtk,vtk=vtk)
  f = rf
  if (present(cf)) cf = rf
  return
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction VTK_END
endmodule Lib_VTK_IO
