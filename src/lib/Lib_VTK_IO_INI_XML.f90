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
public:: VTK_INI_XML_WRITE
public:: VTK_INI_XML_READ
!-----------------------------------------------------------------------------------------------------------------------------------
contains
  function VTK_INI_XML_WRITE(fformat, filename, mesh_topology, nx1, nx2, ny1, ny2, nz1, nz2, cf) result(E_IO)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Procedure for initializing VTK-XML file (exporter).
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
  character(*), intent(IN)            :: fformat       !< File format: ASCII, BINARY, RAW or BINARY-APPENDED.
  character(*), intent(IN)            :: filename      !< File name.
  character(*), intent(IN)            :: mesh_topology !< Mesh topology.
  integer(I4P), intent(IN),  optional :: nx1           !< Initial node of x axis.
  integer(I4P), intent(IN),  optional :: nx2           !< Final node of x axis.
  integer(I4P), intent(IN),  optional :: ny1           !< Initial node of y axis.
  integer(I4P), intent(IN),  optional :: ny2           !< Final node of y axis.
  integer(I4P), intent(IN),  optional :: nz1           !< Initial node of z axis.
  integer(I4P), intent(IN),  optional :: nz2           !< Final node of z axis.
  integer(I4P), intent(OUT), optional :: cf            !< Current file index (for concurrent files IO).
  integer(I4P)                        :: E_IO          !< Input/Output inquiring flag: $0$ if IO is done, $> 0$ if IO is not done.
  character(len=maxlen)               :: s_buffer      !< Buffer string.
  integer(I4P)                        :: rf            !< Real file index.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  E_IO = -1_I4P
  if (.not.ir_initialized) call IR_Init
  if (.not.b64_initialized) call b64_init
  call vtk_update(act='add',cf=rf,Nvtk=Nvtk,vtk=vtk)
  f = rf
  if (present(cf)) cf = rf
  vtk(rf)%topology = trim(mesh_topology)
  select case(trim(Upper_Case(fformat)))
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
    if (trim(Upper_Case(fformat))=='BINARY-APPENDED') vtk(rf)%f = bin_app
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
  endfunction VTK_INI_XML_WRITE

  function VTK_INI_XML_READ(fformat, filename, mesh_topology, npieces, nx1, nx2, ny1, ny2, nz1, nz2, cf) result(E_IO)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Procedure for initializing VTK-XML file (importer).
  !<
  !< Supported input formats are (the passed specifier value is case insensitive):
  !<- ASCII: data are saved in ASCII format; (Not implemented!)
  !<- BINARY: data are saved in base64 encoded format; (Not tested!)
  !<- RAW: data are saved in raw-binary format in the appended tag of the XML file; (Not implemented!)
  !<- BINARY-APPENDED: data are saved in base64 encoded format in the appended tag of the XML file. (Not implemented!)
  !< Supported topologies are:
  !<- RectilinearGrid; (Not tested!)
  !<- StructuredGrid; (Not tested!)
  !<- UnstructuredGrid. (Not tested!)
  !<### Example of usage
  !<```fortran
  !< integer(I4P):: nx1,nx2,ny1,ny2,nz1,nz2
  !< ...
  !< E_IO = VTK_INI_XML_READ('BINARY','XML_RECT_BINARY.vtr','RectilinearGrid',nx1=nx1,nx2=nx2,ny1=ny1,ny2=ny2,nz1=nz1,nz2=nz2,cf=rf)
  !< ...
  !<```
  !< Note that the file extension is necessary in the file name. The XML standard has different extensions for each
  !< different topologies (e.g. *vtr* for rectilinear topology). See the VTK-standard file for more information.
  !---------------------------------------------------------------------------------------------------------------------------------
  character(*), intent(IN)            :: fformat        !< File format: ASCII,  BINARY or RAW
  character(*), intent(IN)            :: filename       !< File name
  character(*), intent(IN)            :: mesh_topology  !< Mesh topology
  integer(I4P), intent(OUT), optional :: npieces        !< Number of pieces stored in the file
  integer(I4P), intent(OUT), optional :: nx1            !< Initial node of x axis.
  integer(I4P), intent(OUT), optional :: nx2            !< Final node of x axis.
  integer(I4P), intent(OUT), optional :: ny1            !< Initial node of y axis.
  integer(I4P), intent(OUT), optional :: ny2            !< Final node of y axis.
  integer(I4P), intent(OUT), optional :: nz1            !< Initial node of z axis.
  integer(I4P), intent(OUT), optional :: nz2            !< Final node of z axis.
  integer(I4P), intent(OUT), optional :: cf             !< Current file index (for concurrent files IO).
  integer(I4P)                        :: E_IO           !< Input/Output inquiring flag: $0$ if IO is done, $> 0$ if IO is not done
  integer(I4P)                        :: rf             !< Real file index.
  integer(I4P)                        :: np             !< Real number of pieces.
  character(len=:), allocatable       :: s_buffer       !< Buffer string.
  character                           :: c1, c2         !< Characters dummies.
  character(len=:), allocatable       :: aux            !< Auxiliary string.
  integer(I4P), dimension(6)          :: rn             !< Real node ranges in WholeExtent [nx1,nx2,ny1,ny2,nz1,nz2].
  logical                             :: fexist         !< Flag for checking the existence of file to import.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  E_IO = -1_I4P
  if (.not.ir_initialized) call IR_Init
  if (.not.b64_initialized) call b64_init
  call vtk_update(act='add', cf=rf, Nvtk=Nvtk, vtk=vtk)
  f = rf
  if (present(cf)) cf = rf
  vtk(rf)%topology = trim(mesh_topology)
  inquire(file=trim(filename), exist=fexist); if(.not.fexist) return

  select case(trim(Upper_Case(fformat)))
  case('ASCII')
    vtk(rf)%f = ascii

    select case(trim(vtk(rf)%topology))
      case('RectilinearGrid', 'StructuredGrid', 'UnstructuredGrid')

        open(unit=Get_Unit(vtk(rf)%u), file=trim(filename), status='old', &
             form='UNFORMATTED', access='STREAM', action='READ',          &
             iostat=E_IO, position='REWIND')

        select case(trim(vtk(rf)%topology))
          case('RectilinearGrid', 'StructuredGrid')
            ! get WholeExtent
            E_IO = move(inside='VTKFile', to_find=trim(vtk(rf)%topology), cf=rf,buffer=s_buffer)
            call get_char(buffer=s_buffer, attrib='WholeExtent', val=aux, E_IO=E_IO)
            if (E_IO == 0) then
              read(aux,*) rn
              if (present(nx1)) nx1 = rn(1); if(present(nx2)) nx2 = rn(2)
              if (present(ny1)) ny1 = rn(3); if(present(ny2)) ny2 = rn(4)
              if (present(nz1)) nz1 = rn(5); if(present(nz2)) nz2 = rn(6)
            endif
        endselect

        ! count the pieces
        rewind(unit=vtk(rf)%u, iostat=E_IO)
        np = 0
        do
          E_IO = read_record(buffer=s_buffer, cf=rf); if (E_IO/=0) exit
          s_buffer = trim(adjustl(Upper_Case(s_buffer)))
          if (index(s_buffer, '</'//trim(Upper_Case(vtk(rf)%topology)))>0) exit ! end of ASCII header section found
          if (index(s_buffer, '<PIECE') > 0) np = np + 1
        enddo

    endselect

  case('BINARY')
    vtk(rf)%f = binary
    select case(trim(vtk(rf)%topology))
      case('RectilinearGrid', 'StructuredGrid', 'UnstructuredGrid')

        open(unit=Get_Unit(vtk(rf)%u), file=trim(filename), status='old', &
             form='UNFORMATTED', access='STREAM', action='READ',          &
             iostat=E_IO, position='REWIND')

        select case(trim(vtk(rf)%topology))
          case('RectilinearGrid', 'StructuredGrid')
            ! Get WholeExtent
            E_IO = move(inside='VTKFile', to_find=trim(vtk(rf)%topology), cf=rf,buffer=s_buffer)
            call get_char(buffer=s_buffer, attrib='WholeExtent', val=aux, E_IO=E_IO)
            if (E_IO == 0) then
              read(aux,*) rn
              if (present(nx1)) nx1 = rn(1); if (present(nx2)) nx2 = rn(2)
              if (present(ny1)) ny1 = rn(3); if (present(ny2)) ny2 = rn(4)
              if (present(nz1)) nz1 = rn(5); if (present(nz2)) nz2 = rn(6)
            endif
        endselect

        ! count the pieces
        rewind(unit=vtk(rf)%u, iostat=E_IO)
        np = 0
        do
          E_IO = read_record(buffer=s_buffer, cf=rf); if(E_IO /= 0) exit
          s_buffer = trim(adjustl(Upper_Case(s_buffer)))
          if (index(s_buffer, '</'//trim(Upper_Case(vtk(rf)%topology))) > 0) exit ! end of ASCII header section found
          if (index(s_buffer, '<PIECE') > 0) np = np + 1
        enddo

    endselect

  case('RAW')
    vtk(rf)%f = raw
    select case(trim(vtk(rf)%topology))
      case('RectilinearGrid', 'StructuredGrid', 'UnstructuredGrid')

        open(unit=Get_Unit(vtk(rf)%u), file=trim(filename), status='old', &
             form='UNFORMATTED', access='STREAM', action='READ',          &
             iostat=E_IO, position='REWIND')

        E_IO = move(inside='VTKFile', cf=rf, buffer=s_buffer)
        call get_char(buffer=s_buffer, attrib='byte_order', val=aux, E_IO=E_IO)

        ! check the file endianness
        if (index(trim(aux), 'LITTLEENDIAN')>0) then
          close(unit=vtk(rf)%u, iostat=E_IO)
          open(unit=Get_Unit(vtk(rf)%u), file=trim(filename), status='old', &
               form='UNFORMATTED', access='STREAM', action='READ',          &
               convert='LITTLE_ENDIAN', iostat=E_IO, position='REWIND')
        elseif (index(trim(aux), 'BIGENDIAN')>0) then
          close(unit=vtk(rf)%u, iostat=E_IO)
          open(unit=Get_Unit(vtk(rf)%u), file=trim(filename), status='old', &
               form='UNFORMATTED', access='STREAM', action='READ',          &
               convert='BIG_ENDIAN', iostat=E_IO, position='REWIND')
        else
          rewind(unit=vtk(rf)%u, iostat=E_IO)
        endif

        select case(trim(vtk(rf)%topology))
          case('RectilinearGrid', 'StructuredGrid')
            ! Get WholeExtent
            E_IO = move(inside='VTKFile', to_find=trim(vtk(rf)%topology), cf=rf, buffer=s_buffer)
            call get_char(buffer=s_buffer, attrib='WholeExtent', val=aux, E_IO=E_IO)
            if (E_IO == 0) then
              read(aux,*) rn
              if (present(nx1)) nx1 = rn(1); if (present(nx2)) nx2 = rn(2)
              if (present(ny1)) ny1 = rn(3); if (present(ny2)) ny2 = rn(4)
              if (present(nz1)) nz1 = rn(5); if (present(nz2)) nz2 = rn(6)
            endif
        endselect

        ! count the pieces
        rewind(unit=vtk(rf)%u, iostat=E_IO)
        np = 0
        do
          E_IO = read_record(buffer=s_buffer, cf=rf); if(E_IO /= 0) exit
          s_buffer = trim(adjustl(Upper_Case(s_buffer)))
          if (index(s_buffer, '</'//trim(Upper_Case(vtk(rf)%topology)))>0) exit ! end of ASCII header section found
          if (index(s_buffer, '<PIECE')>0) np = np + 1
        enddo

        ! calculate the offset to reach the appended data
        rewind(unit=vtk(rf)%u, iostat=E_IO)
        read(unit=vtk(rf)%u, iostat=E_IO) c1
        do
          read(unit=vtk(rf)%u, iostat=E_IO) c2; if (E_IO/=0) exit
          if (iachar(c1)==10.and.c2=='_') exit
          c1 = c2
        enddo
        inquire(unit=vtk(rf)%u, pos=vtk(rf)%ioffset)

    endselect
  endselect
  if (present(npieces)) npieces = np
  endfunction VTK_INI_XML_READ
endmodule Lib_VTK_IO_INI_XML
