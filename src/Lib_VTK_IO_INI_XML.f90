!< INI_XML interface definition for Lib_VTK_IO.
module Lib_VTK_IO_INI_XML
!-----------------------------------------------------------------------------------------------------------------------------------
!< INI_XML interface definition for Lib_VTK_IO.
!-----------------------------------------------------------------------------------------------------------------------------------
USE IR_Precision        ! Integers and reals precision definition.
USE Lib_Base64          ! Base64 encoding/decoding procedures.
USE Lib_VTK_IO_Back_End ! Lib_VTK_IO back end module.
!-----------------------------------------------------------------------------------------------------------------------------------

!-----------------------------------------------------------------------------------------------------------------------------------
implicit none
private
save
public:: VTK_INI_XML
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
endmodule Lib_VTK_IO_INI_XML
