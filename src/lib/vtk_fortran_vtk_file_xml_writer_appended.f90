!< VTK file XMl writer, appended.
module vtk_fortran_vtk_file_xml_writer_appended
!-----------------------------------------------------------------------------------------------------------------------------------
!< VTK file XMl writer, appended.
!-----------------------------------------------------------------------------------------------------------------------------------
use penf
use stringifor
use vtk_fortran_dataarray_encoder
use vtk_fortran_parameters
use vtk_fortran_vtk_file_xml_writer_abstract
!-----------------------------------------------------------------------------------------------------------------------------------

!-----------------------------------------------------------------------------------------------------------------------------------
implicit none
private
public :: xml_writer_appended
!-----------------------------------------------------------------------------------------------------------------------------------

!-----------------------------------------------------------------------------------------------------------------------------------
type, extends(xml_writer_abstract) :: xml_writer_appended
  !< VTK file XML writer, appended.
  type(string) :: encoding      !< Appended data encoding: "raw" or "base64".
  integer(I4P) :: scratch=0_I4P !< Scratch logical unit.
  contains
    ! deferred methods
    procedure, pass(self) :: initialize                 !< Initialize writer.
    procedure, pass(self) :: finalize                   !< Finalize writer.
    procedure, pass(self) :: write_dataarray1_rank1_R8P !< Write dataarray 1, rank 1, R8P.
    procedure, pass(self) :: write_dataarray1_rank1_R4P !< Write dataarray 1, rank 1, R4P.
    procedure, pass(self) :: write_dataarray1_rank1_I8P !< Write dataarray 1, rank 1, I8P.
    procedure, pass(self) :: write_dataarray1_rank1_I4P !< Write dataarray 1, rank 1, I4P.
    procedure, pass(self) :: write_dataarray1_rank1_I2P !< Write dataarray 1, rank 1, I2P.
    procedure, pass(self) :: write_dataarray1_rank1_I1P !< Write dataarray 1, rank 1, I1P.
    procedure, pass(self) :: write_dataarray1_rank2_R8P !< Write dataarray 1, rank 2, R8P.
    procedure, pass(self) :: write_dataarray1_rank2_R4P !< Write dataarray 1, rank 2, R4P.
    procedure, pass(self) :: write_dataarray1_rank2_I8P !< Write dataarray 1, rank 2, I8P.
    procedure, pass(self) :: write_dataarray1_rank2_I4P !< Write dataarray 1, rank 2, I4P.
    procedure, pass(self) :: write_dataarray1_rank2_I2P !< Write dataarray 1, rank 2, I2P.
    procedure, pass(self) :: write_dataarray1_rank2_I1P !< Write dataarray 1, rank 2, I1P.
    procedure, pass(self) :: write_dataarray1_rank3_R8P !< Write dataarray 1, rank 3, R8P.
    procedure, pass(self) :: write_dataarray1_rank3_R4P !< Write dataarray 1, rank 3, R4P.
    procedure, pass(self) :: write_dataarray1_rank3_I8P !< Write dataarray 1, rank 3, I8P.
    procedure, pass(self) :: write_dataarray1_rank3_I4P !< Write dataarray 1, rank 3, I4P.
    procedure, pass(self) :: write_dataarray1_rank3_I2P !< Write dataarray 1, rank 3, I2P.
    procedure, pass(self) :: write_dataarray1_rank3_I1P !< Write dataarray 1, rank 3, I1P.
    procedure, pass(self) :: write_dataarray1_rank4_R8P !< Write dataarray 1, rank 4, R8P.
    procedure, pass(self) :: write_dataarray1_rank4_R4P !< Write dataarray 1, rank 4, R4P.
    procedure, pass(self) :: write_dataarray1_rank4_I8P !< Write dataarray 1, rank 4, I8P.
    procedure, pass(self) :: write_dataarray1_rank4_I4P !< Write dataarray 1, rank 4, I4P.
    procedure, pass(self) :: write_dataarray1_rank4_I2P !< Write dataarray 1, rank 4, I2P.
    procedure, pass(self) :: write_dataarray1_rank4_I1P !< Write dataarray 1, rank 4, I1P.
    procedure, pass(self) :: write_dataarray3_rank1_R8P !< Write dataarray 3, rank 1, R8P.
    procedure, pass(self) :: write_dataarray3_rank1_R4P !< Write dataarray 3, rank 1, R4P.
    procedure, pass(self) :: write_dataarray3_rank1_I8P !< Write dataarray 3, rank 1, I8P.
    procedure, pass(self) :: write_dataarray3_rank1_I4P !< Write dataarray 3, rank 1, I4P.
    procedure, pass(self) :: write_dataarray3_rank1_I2P !< Write dataarray 3, rank 1, I2P.
    procedure, pass(self) :: write_dataarray3_rank1_I1P !< Write dataarray 3, rank 1, I1P.
    procedure, pass(self) :: write_dataarray3_rank3_R8P !< Write dataarray 3, rank 3, R8P.
    procedure, pass(self) :: write_dataarray3_rank3_R4P !< Write dataarray 3, rank 3, R4P.
    procedure, pass(self) :: write_dataarray3_rank3_I8P !< Write dataarray 3, rank 3, I8P.
    procedure, pass(self) :: write_dataarray3_rank3_I4P !< Write dataarray 3, rank 3, I4P.
    procedure, pass(self) :: write_dataarray3_rank3_I2P !< Write dataarray 3, rank 3, I2P.
    procedure, pass(self) :: write_dataarray3_rank3_I1P !< Write dataarray 3, rank 3, I1P.
    procedure, pass(self) :: write_dataarray_appended   !< Write appended.
    ! private methods
    procedure, pass(self), private :: ioffset_update     !< Update ioffset count.
    procedure, pass(self), private :: open_scratch_file  !< Open scratch file.
    procedure, pass(self), private :: close_scratch_file !< Close scratch file.
    generic, private :: write_on_scratch_dataarray =>          &
                        write_on_scratch_dataarray1_rank1,     &
                        write_on_scratch_dataarray1_rank2,     &
                        write_on_scratch_dataarray1_rank3,     &
                        write_on_scratch_dataarray1_rank4,     &
                        write_on_scratch_dataarray3_rank1_R8P, &
                        write_on_scratch_dataarray3_rank1_R4P, &
                        write_on_scratch_dataarray3_rank1_I8P, &
                        write_on_scratch_dataarray3_rank1_I4P, &
                        write_on_scratch_dataarray3_rank1_I2P, &
                        write_on_scratch_dataarray3_rank1_I1P, &
                        write_on_scratch_dataarray3_rank2_R8P, &
                        write_on_scratch_dataarray3_rank2_R4P, &
                        write_on_scratch_dataarray3_rank2_I8P, &
                        write_on_scratch_dataarray3_rank2_I4P, &
                        write_on_scratch_dataarray3_rank2_I2P, &
                        write_on_scratch_dataarray3_rank2_I1P, &
                        write_on_scratch_dataarray3_rank3_R8P, &
                        write_on_scratch_dataarray3_rank3_R4P, &
                        write_on_scratch_dataarray3_rank3_I8P, &
                        write_on_scratch_dataarray3_rank3_I4P, &
                        write_on_scratch_dataarray3_rank3_I2P, &
                        write_on_scratch_dataarray3_rank3_I1P !< Write dataarray.
    procedure, pass(self), private :: write_on_scratch_dataarray1_rank1     !< Write dataarray, data 1 rank 1.
    procedure, pass(self), private :: write_on_scratch_dataarray1_rank2     !< Write dataarray, data 1 rank 2.
    procedure, pass(self), private :: write_on_scratch_dataarray1_rank3     !< Write dataarray, data 1 rank 3.
    procedure, pass(self), private :: write_on_scratch_dataarray1_rank4     !< Write dataarray, data 1 rank 4.
    procedure, pass(self), private :: write_on_scratch_dataarray3_rank1_R8P !< Write dataarray, comp 3 rank 1, R8P.
    procedure, pass(self), private :: write_on_scratch_dataarray3_rank1_R4P !< Write dataarray, comp 3 rank 1, R4P.
    procedure, pass(self), private :: write_on_scratch_dataarray3_rank1_I8P !< Write dataarray, comp 3 rank 1, I8P.
    procedure, pass(self), private :: write_on_scratch_dataarray3_rank1_I4P !< Write dataarray, comp 3 rank 1, I4P.
    procedure, pass(self), private :: write_on_scratch_dataarray3_rank1_I2P !< Write dataarray, comp 3 rank 1, I2P.
    procedure, pass(self), private :: write_on_scratch_dataarray3_rank1_I1P !< Write dataarray, comp 3 rank 1, I1P.
    procedure, pass(self), private :: write_on_scratch_dataarray3_rank2_R8P !< Write dataarray, comp 3 rank 2, R8P.
    procedure, pass(self), private :: write_on_scratch_dataarray3_rank2_R4P !< Write dataarray, comp 3 rank 2, R4P.
    procedure, pass(self), private :: write_on_scratch_dataarray3_rank2_I8P !< Write dataarray, comp 3 rank 2, I8P.
    procedure, pass(self), private :: write_on_scratch_dataarray3_rank2_I4P !< Write dataarray, comp 3 rank 2, I4P.
    procedure, pass(self), private :: write_on_scratch_dataarray3_rank2_I2P !< Write dataarray, comp 3 rank 2, I2P.
    procedure, pass(self), private :: write_on_scratch_dataarray3_rank2_I1P !< Write dataarray, comp 3 rank 2, I1P.
    procedure, pass(self), private :: write_on_scratch_dataarray3_rank3_R8P !< Write dataarray, comp 3 rank 3, R8P.
    procedure, pass(self), private :: write_on_scratch_dataarray3_rank3_R4P !< Write dataarray, comp 3 rank 3, R4P.
    procedure, pass(self), private :: write_on_scratch_dataarray3_rank3_I8P !< Write dataarray, comp 3 rank 3, I8P.
    procedure, pass(self), private :: write_on_scratch_dataarray3_rank3_I4P !< Write dataarray, comp 3 rank 3, I4P.
    procedure, pass(self), private :: write_on_scratch_dataarray3_rank3_I2P !< Write dataarray, comp 3 rank 3, I2P.
    procedure, pass(self), private :: write_on_scratch_dataarray3_rank3_I1P !< Write dataarray, comp 3 rank 3, I1P.
endtype xml_writer_appended
!-----------------------------------------------------------------------------------------------------------------------------------
contains
  function initialize(self, format, filename, mesh_topology, nx1, nx2, ny1, ny2, nz1, nz2, mesh_kind) result(error)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Initialize writer.
  !---------------------------------------------------------------------------------------------------------------------------------
  class(xml_writer_appended), intent(inout)        :: self          !< Writer.
  character(*),               intent(in)           :: format        !< File format: ASCII.
  character(*),               intent(in)           :: filename      !< File name.
  character(*),               intent(in)           :: mesh_topology !< Mesh topology.
  integer(I4P),               intent(in), optional :: nx1           !< Initial node of x axis.
  integer(I4P),               intent(in), optional :: nx2           !< Final node of x axis.
  integer(I4P),               intent(in), optional :: ny1           !< Initial node of y axis.
  integer(I4P),               intent(in), optional :: ny2           !< Final node of y axis.
  integer(I4P),               intent(in), optional :: nz1           !< Initial node of z axis.
  integer(I4P),               intent(in), optional :: nz2           !< Final node of z axis.
  character(*),               intent(in), optional :: mesh_kind     !< Kind of mesh data: Float64, Float32, ecc.
  integer(I4P)                                     :: error         !< Error status.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  self%topology = trim(adjustl(mesh_topology))
  self%format_ch = 'appended'
  self%encoding = format
  self%encoding = self%encoding%upper()
  select case(self%encoding%chars())
  case('RAW')
    self%encoding = 'raw'
  case('BINARY-APPENDED')
    self%encoding = 'base64'
  endselect
  call self%open_xml_file(filename=filename)
  call self%write_header_tag
  call self%write_topology_tag(nx1=nx1, nx2=nx2, ny1=ny1, ny2=ny2, nz1=nz1, nz2=nz2, mesh_kind=mesh_kind)
  self%ioffset = 0
  call self%open_scratch_file
  error = self%error
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction initialize

  function finalize(self) result(error)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Finalize writer.
  !---------------------------------------------------------------------------------------------------------------------------------
  class(xml_writer_appended), intent(inout) :: self  !< Writer.
  integer(I4P)                              :: error !< Error status.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  call self%write_end_tag(name=self%topology%chars())
  call self%write_dataarray_appended
  call self%write_end_tag(name='VTKFile')
  call self%close_xml_file
  call self%close_scratch_file
  error = self%error
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction finalize

  elemental subroutine ioffset_update(self, n_byte)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Update ioffset count.
  !---------------------------------------------------------------------------------------------------------------------------------
  class(xml_writer_appended), intent(inout) :: self  !< Writer.
  integer(I4P),               intent(in)    :: n_byte !< Number of bytes saved.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  if (self%encoding=='raw') then
    self%ioffset = self%ioffset + BYI4P + n_byte
  else
    self%ioffset = self%ioffset + ((n_byte + BYI4P + 2_I4P)/3_I4P)*4_I4P
  endif
  !---------------------------------------------------------------------------------------------------------------------------------
  endsubroutine ioffset_update

  subroutine open_scratch_file(self)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Open scratch file.
  !---------------------------------------------------------------------------------------------------------------------------------
  class(xml_writer_appended), intent(inout) :: self  !< Writer.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  open(newunit=self%scratch, &
       form='UNFORMATTED',   &
       access='STREAM',      &
       action='READWRITE',   &
       status='SCRATCH',     &
       iostat=self%error)
  !---------------------------------------------------------------------------------------------------------------------------------
  endsubroutine open_scratch_file

  subroutine close_scratch_file(self)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Close scratch file.
  !---------------------------------------------------------------------------------------------------------------------------------
  class(xml_writer_appended), intent(inout) :: self  !< Writer.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  close(unit=self%scratch, iostat=self%error)
  !---------------------------------------------------------------------------------------------------------------------------------
  endsubroutine close_scratch_file

  ! write_dataarray methods
  function write_dataarray1_rank1_R8P(self, data_name, x, is_tuples) result(error)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Write `<DataArray... NumberOfComponents="1"...>...</DataArray>` tag (R8P).
  !---------------------------------------------------------------------------------------------------------------------------------
  class(xml_writer_appended), intent(inout)        :: self         !< Writer.
  character(*),               intent(in)           :: data_name    !< Data name.
  real(R8P),                  intent(in)           :: x(1:)        !< Data variable.
  logical,                    intent(in), optional :: is_tuples    !< Use "NumberOfTuples".
  integer(I4P)                                     :: error        !< Error status.
  character(len=:), allocatable                    :: data_type    !< Data type.
  integer(I4P)                                     :: n_components !< Number of components.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  data_type = 'Float64'
  n_components = 1
  call self%write_dataarray_tag_appended(data_type=data_type, number_of_components=n_components, data_name=data_name, &
                                         is_tuples=is_tuples)
  call self%ioffset_update(n_byte=self%write_on_scratch_dataarray(x=x))
  error = self%error
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction write_dataarray1_rank1_R8P

  function write_dataarray1_rank1_R4P(self, data_name, x, is_tuples) result(error)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Write `<DataArray... NumberOfComponents="1"...>...</DataArray>` tag (R4P).
  !---------------------------------------------------------------------------------------------------------------------------------
  class(xml_writer_appended), intent(inout)        :: self         !< Writer.
  character(*),               intent(in)           :: data_name    !< Data name.
  real(R4P),                  intent(in)           :: x(1:)        !< Data variable.
  logical,                    intent(in), optional :: is_tuples    !< Use "NumberOfTuples".
  integer(I4P)                                     :: error        !< Error status.
  character(len=:), allocatable                    :: data_type    !< Data type.
  integer(I4P)                                     :: n_components !< Number of components.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  data_type = 'Float32'
  n_components = 1
  call self%write_dataarray_tag_appended(data_type=data_type, number_of_components=n_components, data_name=data_name, &
                                         is_tuples=is_tuples)
  call self%ioffset_update(n_byte=self%write_on_scratch_dataarray(x=x))
  error = self%error
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction write_dataarray1_rank1_R4P

  function write_dataarray1_rank1_I8P(self, data_name, x, is_tuples) result(error)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Write `<DataArray... NumberOfComponents="1"...>...</DataArray>` tag (I8P).
  !---------------------------------------------------------------------------------------------------------------------------------
  class(xml_writer_appended), intent(inout)        :: self         !< Writer.
  character(*),               intent(in)           :: data_name    !< Data name.
  integer(I8P),               intent(in)           :: x(1:)        !< Data variable.
  logical,                    intent(in), optional :: is_tuples    !< Use "NumberOfTuples".
  integer(I4P)                                     :: error        !< Error status.
  character(len=:), allocatable                    :: data_type    !< Data type.
  integer(I4P)                                     :: n_components !< Number of components.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  data_type = 'Int64'
  n_components = 1
  call self%write_dataarray_tag_appended(data_type=data_type, number_of_components=n_components, data_name=data_name, &
                                         is_tuples=is_tuples)
  call self%ioffset_update(n_byte=self%write_on_scratch_dataarray(x=x))
  error = self%error
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction write_dataarray1_rank1_I8P

  function write_dataarray1_rank1_I4P(self, data_name, x, is_tuples) result(error)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Write `<DataArray... NumberOfComponents="1"...>...</DataArray>` tag (I4P).
  !---------------------------------------------------------------------------------------------------------------------------------
  class(xml_writer_appended), intent(inout)        :: self         !< Writer.
  character(*),               intent(in)           :: data_name    !< Data name.
  integer(I4P),               intent(in)           :: x(1:)        !< Data variable.
  logical,                    intent(in), optional :: is_tuples    !< Use "NumberOfTuples".
  integer(I4P)                                     :: error        !< Error status.
  character(len=:), allocatable                    :: data_type    !< Data type.
  integer(I4P)                                     :: n_components !< Number of components.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  data_type = 'Int32'
  n_components = 1
  call self%write_dataarray_tag_appended(data_type=data_type, number_of_components=n_components, data_name=data_name, &
                                         is_tuples=is_tuples)
  call self%ioffset_update(n_byte=self%write_on_scratch_dataarray(x=x))
  error = self%error
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction write_dataarray1_rank1_I4P

  function write_dataarray1_rank1_I2P(self, data_name, x, is_tuples) result(error)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Write `<DataArray... NumberOfComponents="1"...>...</DataArray>` tag (I2P).
  !---------------------------------------------------------------------------------------------------------------------------------
  class(xml_writer_appended), intent(inout)        :: self         !< Writer.
  character(*),               intent(in)           :: data_name    !< Data name.
  integer(I2P),               intent(in)           :: x(1:)        !< Data variable.
  logical,                    intent(in), optional :: is_tuples    !< Use "NumberOfTuples".
  integer(I4P)                                     :: error        !< Error status.
  character(len=:), allocatable                    :: data_type    !< Data type.
  integer(I4P)                                     :: n_components !< Number of components.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  data_type = 'Int16'
  n_components = 1
  call self%write_dataarray_tag_appended(data_type=data_type, number_of_components=n_components, data_name=data_name, &
                                         is_tuples=is_tuples)
  call self%ioffset_update(n_byte=self%write_on_scratch_dataarray(x=x))
  error = self%error
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction write_dataarray1_rank1_I2P

  function write_dataarray1_rank1_I1P(self, data_name, x, is_tuples) result(error)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Write `<DataArray... NumberOfComponents="1"...>...</DataArray>` tag (I1P).
  !---------------------------------------------------------------------------------------------------------------------------------
  class(xml_writer_appended), intent(inout)        :: self         !< Writer.
  character(*),               intent(in)           :: data_name    !< Data name.
  integer(I1P),               intent(in)           :: x(1:)        !< Data variable.
  logical,                    intent(in), optional :: is_tuples    !< Use "NumberOfTuples".
  integer(I4P)                                     :: error        !< Error status.
  character(len=:), allocatable                    :: data_type    !< Data type.
  integer(I4P)                                     :: n_components !< Number of components.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  data_type = 'Int8'
  n_components = 1
  call self%write_dataarray_tag_appended(data_type=data_type, number_of_components=n_components, data_name=data_name, &
                                         is_tuples=is_tuples)
  call self%ioffset_update(n_byte=self%write_on_scratch_dataarray(x=x))
  error = self%error
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction write_dataarray1_rank1_I1P

  function write_dataarray1_rank2_R8P(self, data_name, x, one_component, is_tuples) result(error)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Write `<DataArray... NumberOfComponents="n"...>...</DataArray>` tag (R8P).
  !---------------------------------------------------------------------------------------------------------------------------------
  class(xml_writer_appended), intent(inout)        :: self          !< Writer.
  character(*),               intent(in)           :: data_name     !< Data name.
  real(R8P),                  intent(in)           :: x(1:,1:)      !< Data variable.
  logical,                    intent(in), optional :: one_component !< Force one component.
  logical,                    intent(in), optional :: is_tuples     !< Use "NumberOfTuples".
  integer(I4P)                                     :: error         !< Error status.
  character(len=:), allocatable                    :: data_type     !< Data type.
  integer(I4P)                                     :: n_components  !< Number of components.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  data_type = 'Float64'
  n_components = size(x, dim=1)
  if (present(one_component)) then
    if (one_component) n_components = 1
  endif
  call self%write_dataarray_tag_appended(data_type=data_type, number_of_components=n_components, data_name=data_name, &
                                         is_tuples=is_tuples)
  call self%ioffset_update(n_byte=self%write_on_scratch_dataarray(x=x))
  error = self%error
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction write_dataarray1_rank2_R8P

  function write_dataarray1_rank2_R4P(self, data_name, x, one_component, is_tuples) result(error)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Write `<DataArray... NumberOfComponents="n"...>...</DataArray>` tag (R4P).
  !---------------------------------------------------------------------------------------------------------------------------------
  class(xml_writer_appended), intent(inout)        :: self          !< Writer.
  character(*),               intent(in)           :: data_name     !< Data name.
  real(R4P),                  intent(in)           :: x(1:,1:)      !< Data variable.
  logical,                    intent(in), optional :: one_component !< Force one component.
  logical,                    intent(in), optional :: is_tuples     !< Use "NumberOfTuples".
  integer(I4P)                                     :: error         !< Error status.
  character(len=:), allocatable                    :: data_type     !< Data type.
  integer(I4P)                                     :: n_components  !< Number of components.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  data_type = 'Float32'
  n_components = size(x, dim=1)
  if (present(one_component)) then
    if (one_component) n_components = 1
  endif
  call self%write_dataarray_tag_appended(data_type=data_type, number_of_components=n_components, data_name=data_name, &
                                         is_tuples=is_tuples)
  call self%ioffset_update(n_byte=self%write_on_scratch_dataarray(x=x))
  error = self%error
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction write_dataarray1_rank2_R4P

  function write_dataarray1_rank2_I8P(self, data_name, x, one_component, is_tuples) result(error)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Write `<DataArray... NumberOfComponents="n"...>...</DataArray>` tag (I8P).
  !---------------------------------------------------------------------------------------------------------------------------------
  class(xml_writer_appended), intent(inout)        :: self          !< Writer.
  character(*),               intent(in)           :: data_name     !< Data name.
  integer(I8P),               intent(in)           :: x(1:,1:)      !< Data variable.
  logical,                    intent(in), optional :: one_component !< Force one component.
  logical,                    intent(in), optional :: is_tuples     !< Use "NumberOfTuples".
  integer(I4P)                                     :: error         !< Error status.
  character(len=:), allocatable                    :: data_type     !< Data type.
  integer(I4P)                                     :: n_components  !< Number of components.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  data_type = 'Int64'
  n_components = size(x, dim=1)
  if (present(one_component)) then
    if (one_component) n_components = 1
  endif
  call self%write_dataarray_tag_appended(data_type=data_type, number_of_components=n_components, data_name=data_name, &
                                         is_tuples=is_tuples)
  call self%ioffset_update(n_byte=self%write_on_scratch_dataarray(x=x))
  error = self%error
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction write_dataarray1_rank2_I8P

  function write_dataarray1_rank2_I4P(self, data_name, x, one_component, is_tuples) result(error)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Write `<DataArray... NumberOfComponents="n"...>...</DataArray>` tag (I4P).
  !---------------------------------------------------------------------------------------------------------------------------------
  class(xml_writer_appended), intent(inout)        :: self          !< Writer.
  character(*),               intent(in)           :: data_name     !< Data name.
  integer(I4P),               intent(in)           :: x(1:,1:)      !< Data variable.
  logical,                    intent(in), optional :: one_component !< Force one component.
  logical,                    intent(in), optional :: is_tuples     !< Use "NumberOfTuples".
  integer(I4P)                                     :: error         !< Error status.
  character(len=:), allocatable                    :: data_type     !< Data type.
  integer(I4P)                                     :: n_components  !< Number of components.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  data_type = 'Int32'
  n_components = size(x, dim=1)
  if (present(one_component)) then
    if (one_component) n_components = 1
  endif
  call self%write_dataarray_tag_appended(data_type=data_type, number_of_components=n_components, data_name=data_name, &
                                         is_tuples=is_tuples)
  call self%ioffset_update(n_byte=self%write_on_scratch_dataarray(x=x))
  error = self%error
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction write_dataarray1_rank2_I4P

  function write_dataarray1_rank2_I2P(self, data_name, x, one_component, is_tuples) result(error)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Write `<DataArray... NumberOfComponents="n"...>...</DataArray>` tag (I2P).
  !---------------------------------------------------------------------------------------------------------------------------------
  class(xml_writer_appended), intent(inout)        :: self          !< Writer.
  character(*),               intent(in)           :: data_name     !< Data name.
  integer(I2P),               intent(in)           :: x(1:,1:)      !< Data variable.
  logical,                    intent(in), optional :: one_component !< Force one component.
  logical,                    intent(in), optional :: is_tuples     !< Use "NumberOfTuples".
  integer(I4P)                                     :: error         !< Error status.
  character(len=:), allocatable                    :: data_type     !< Data type.
  integer(I4P)                                     :: n_components  !< Number of components.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  data_type = 'Int16'
  n_components = size(x, dim=1)
  if (present(one_component)) then
    if (one_component) n_components = 1
  endif
  call self%write_dataarray_tag_appended(data_type=data_type, number_of_components=n_components, data_name=data_name, &
                                         is_tuples=is_tuples)
  call self%ioffset_update(n_byte=self%write_on_scratch_dataarray(x=x))
  error = self%error
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction write_dataarray1_rank2_I2P

  function write_dataarray1_rank2_I1P(self, data_name, x, one_component, is_tuples) result(error)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Write `<DataArray... NumberOfComponents="n"...>...</DataArray>` tag (I1P).
  !---------------------------------------------------------------------------------------------------------------------------------
  class(xml_writer_appended), intent(inout)        :: self          !< Writer.
  character(*),               intent(in)           :: data_name     !< Data name.
  integer(I1P),               intent(in)           :: x(1:,1:)      !< Data variable.
  logical,                    intent(in), optional :: one_component !< Force one component.
  logical,                    intent(in), optional :: is_tuples     !< Use "NumberOfTuples".
  integer(I4P)                                     :: error         !< Error status.
  character(len=:), allocatable                    :: data_type     !< Data type.
  integer(I4P)                                     :: n_components  !< Number of components.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  data_type = 'Int8'
  n_components = size(x, dim=1)
  if (present(one_component)) then
    if (one_component) n_components = 1
  endif
  call self%write_dataarray_tag_appended(data_type=data_type, number_of_components=n_components, data_name=data_name, &
                                         is_tuples=is_tuples)
  call self%ioffset_update(n_byte=self%write_on_scratch_dataarray(x=x))
  error = self%error
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction write_dataarray1_rank2_I1P

  function write_dataarray1_rank3_R8P(self, data_name, x, one_component, is_tuples) result(error)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Write `<DataArray... NumberOfComponents="n"...>...</DataArray>` tag (R8P).
  !---------------------------------------------------------------------------------------------------------------------------------
  class(xml_writer_appended), intent(inout)        :: self          !< Writer.
  character(*),               intent(in)           :: data_name     !< Data name.
  real(R8P),                  intent(in)           :: x(1:,1:,1:)   !< Data variable.
  logical,                    intent(in), optional :: one_component !< Force one component.
  logical,                    intent(in), optional :: is_tuples     !< Use "NumberOfTuples".
  integer(I4P)                                     :: error         !< Error status.
  character(len=:), allocatable                    :: data_type     !< Data type.
  integer(I4P)                                     :: n_components  !< Number of components.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  data_type = 'Float64'
  n_components = size(x, dim=1)
  if (present(one_component)) then
    if (one_component) n_components = 1
  endif
  call self%write_dataarray_tag_appended(data_type=data_type, number_of_components=n_components, data_name=data_name, &
                                         is_tuples=is_tuples)
  call self%ioffset_update(n_byte=self%write_on_scratch_dataarray(x=x))
  error = self%error
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction write_dataarray1_rank3_R8P

  function write_dataarray1_rank3_R4P(self, data_name, x, one_component, is_tuples) result(error)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Write `<DataArray... NumberOfComponents="n"...>...</DataArray>` tag (R4P).
  !---------------------------------------------------------------------------------------------------------------------------------
  class(xml_writer_appended), intent(inout)        :: self          !< Writer.
  character(*),               intent(in)           :: data_name     !< Data name.
  real(R4P),                  intent(in)           :: x(1:,1:,1:)   !< Data variable.
  logical,                    intent(in), optional :: one_component !< Force one component.
  logical,                    intent(in), optional :: is_tuples     !< Use "NumberOfTuples".
  integer(I4P)                                     :: error         !< Error status.
  character(len=:), allocatable                    :: data_type     !< Data type.
  integer(I4P)                                     :: n_components  !< Number of components.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  data_type = 'Float32'
  n_components = size(x, dim=1)
  if (present(one_component)) then
    if (one_component) n_components = 1
  endif
  call self%write_dataarray_tag_appended(data_type=data_type, number_of_components=n_components, data_name=data_name, &
                                         is_tuples=is_tuples)
  call self%ioffset_update(n_byte=self%write_on_scratch_dataarray(x=x))
  error = self%error
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction write_dataarray1_rank3_R4P

  function write_dataarray1_rank3_I8P(self, data_name, x, one_component, is_tuples) result(error)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Write `<DataArray... NumberOfComponents="n"...>...</DataArray>` tag (I8P).
  !---------------------------------------------------------------------------------------------------------------------------------
  class(xml_writer_appended), intent(inout)        :: self          !< Writer.
  character(*),               intent(in)           :: data_name     !< Data name.
  integer(I8P),               intent(in)           :: x(1:,1:,1:)   !< Data variable.
  logical,                    intent(in), optional :: one_component !< Force one component.
  logical,                    intent(in), optional :: is_tuples     !< Use "NumberOfTuples".
  integer(I4P)                                     :: error         !< Error status.
  character(len=:), allocatable                    :: data_type     !< Data type.
  integer(I4P)                                     :: n_components  !< Number of components.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  data_type = 'Int64'
  n_components = size(x, dim=1)
  if (present(one_component)) then
    if (one_component) n_components = 1
  endif
  call self%write_dataarray_tag_appended(data_type=data_type, number_of_components=n_components, data_name=data_name, &
                                         is_tuples=is_tuples)
  call self%ioffset_update(n_byte=self%write_on_scratch_dataarray(x=x))
  error = self%error
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction write_dataarray1_rank3_I8P

  function write_dataarray1_rank3_I4P(self, data_name, x, one_component, is_tuples) result(error)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Write `<DataArray... NumberOfComponents="n"...>...</DataArray>` tag (I4P).
  !---------------------------------------------------------------------------------------------------------------------------------
  class(xml_writer_appended), intent(inout)        :: self          !< Writer.
  character(*),               intent(in)           :: data_name     !< Data name.
  integer(I4P),               intent(in)           :: x(1:,1:,1:)   !< Data variable.
  logical,                    intent(in), optional :: one_component !< Force one component.
  logical,                    intent(in), optional :: is_tuples     !< Use "NumberOfTuples".
  integer(I4P)                                     :: error         !< Error status.
  character(len=:), allocatable                    :: data_type     !< Data type.
  integer(I4P)                                     :: n_components  !< Number of components.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  data_type = 'Int32'
  n_components = size(x, dim=1)
  if (present(one_component)) then
    if (one_component) n_components = 1
  endif
  call self%write_dataarray_tag_appended(data_type=data_type, number_of_components=n_components, data_name=data_name, &
                                         is_tuples=is_tuples)
  call self%ioffset_update(n_byte=self%write_on_scratch_dataarray(x=x))
  error = self%error
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction write_dataarray1_rank3_I4P

  function write_dataarray1_rank3_I2P(self, data_name, x, one_component, is_tuples) result(error)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Write `<DataArray... NumberOfComponents="n"...>...</DataArray>` tag (I2P).
  !---------------------------------------------------------------------------------------------------------------------------------
  class(xml_writer_appended), intent(inout)        :: self          !< Writer.
  character(*),               intent(in)           :: data_name     !< Data name.
  integer(I2P),               intent(in)           :: x(1:,1:,1:)   !< Data variable.
  logical,                    intent(in), optional :: one_component !< Force one component.
  logical,                    intent(in), optional :: is_tuples     !< Use "NumberOfTuples".
  integer(I4P)                                     :: error         !< Error status.
  character(len=:), allocatable                    :: data_type     !< Data type.
  integer(I4P)                                     :: n_components  !< Number of components.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  data_type = 'Int16'
  n_components = size(x, dim=1)
  if (present(one_component)) then
    if (one_component) n_components = 1
  endif
  call self%write_dataarray_tag_appended(data_type=data_type, number_of_components=n_components, data_name=data_name, &
                                         is_tuples=is_tuples)
  call self%ioffset_update(n_byte=self%write_on_scratch_dataarray(x=x))
  error = self%error
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction write_dataarray1_rank3_I2P

  function write_dataarray1_rank3_I1P(self, data_name, x, one_component, is_tuples) result(error)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Write `<DataArray... NumberOfComponents="n"...>...</DataArray>` tag (I1P).
  !---------------------------------------------------------------------------------------------------------------------------------
  class(xml_writer_appended), intent(inout)        :: self          !< Writer.
  character(*),               intent(in)           :: data_name     !< Data name.
  integer(I1P),               intent(in)           :: x(1:,1:,1:)   !< Data variable.
  logical,                    intent(in), optional :: one_component !< Force one component.
  logical,                    intent(in), optional :: is_tuples     !< Use "NumberOfTuples".
  integer(I4P)                                     :: error         !< Error status.
  character(len=:), allocatable                    :: data_type     !< Data type.
  integer(I4P)                                     :: n_components  !< Number of components.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  data_type = 'Int8'
  n_components = size(x, dim=1)
  if (present(one_component)) then
    if (one_component) n_components = 1
  endif
  call self%write_dataarray_tag_appended(data_type=data_type, number_of_components=n_components, data_name=data_name, &
                                         is_tuples=is_tuples)
  call self%ioffset_update(n_byte=self%write_on_scratch_dataarray(x=x))
  error = self%error
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction write_dataarray1_rank3_I1P

  function write_dataarray1_rank4_R8P(self, data_name, x, one_component, is_tuples) result(error)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Write `<DataArray... NumberOfComponents="n"...>...</DataArray>` tag (R8P).
  !---------------------------------------------------------------------------------------------------------------------------------
  class(xml_writer_appended), intent(inout)        :: self           !< Writer.
  character(*),               intent(in)           :: data_name      !< Data name.
  real(R8P),                  intent(in)           :: x(1:,1:,1:,1:) !< Data variable.
  logical,                    intent(in), optional :: one_component  !< Force one component.
  logical,                    intent(in), optional :: is_tuples      !< Use "NumberOfTuples".
  integer(I4P)                                     :: error          !< Error status.
  character(len=:), allocatable                    :: data_type      !< Data type.
  integer(I4P)                                     :: n_components   !< Number of components.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  data_type = 'Float64'
  n_components = size(x, dim=1)
  if (present(one_component)) then
    if (one_component) n_components = 1
  endif
  call self%write_dataarray_tag_appended(data_type=data_type, number_of_components=n_components, data_name=data_name, &
                                         is_tuples=is_tuples)
  call self%ioffset_update(n_byte=self%write_on_scratch_dataarray(x=x))
  error = self%error
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction write_dataarray1_rank4_R8P

  function write_dataarray1_rank4_R4P(self, data_name, x, one_component, is_tuples) result(error)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Write `<DataArray... NumberOfComponents="n"...>...</DataArray>` tag (R4P).
  !---------------------------------------------------------------------------------------------------------------------------------
  class(xml_writer_appended), intent(inout)        :: self           !< Writer.
  character(*),               intent(in)           :: data_name      !< Data name.
  real(R4P),                  intent(in)           :: x(1:,1:,1:,1:) !< Data variable.
  logical,                    intent(in), optional :: one_component  !< Force one component.
  logical,                    intent(in), optional :: is_tuples      !< Use "NumberOfTuples".
  integer(I4P)                                     :: error          !< Error status.
  character(len=:), allocatable                    :: data_type      !< Data type.
  integer(I4P)                                     :: n_components   !< Number of components.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  data_type = 'Float32'
  n_components = size(x, dim=1)
  if (present(one_component)) then
    if (one_component) n_components = 1
  endif
  call self%write_dataarray_tag_appended(data_type=data_type, number_of_components=n_components, data_name=data_name, &
                                         is_tuples=is_tuples)
  call self%ioffset_update(n_byte=self%write_on_scratch_dataarray(x=x))
  error = self%error
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction write_dataarray1_rank4_R4P

  function write_dataarray1_rank4_I8P(self, data_name, x, one_component, is_tuples) result(error)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Write `<DataArray... NumberOfComponents="n"...>...</DataArray>` tag (I8P).
  !---------------------------------------------------------------------------------------------------------------------------------
  class(xml_writer_appended), intent(inout)        :: self           !< Writer.
  character(*),               intent(in)           :: data_name      !< Data name.
  integer(I8P),               intent(in)           :: x(1:,1:,1:,1:) !< Data variable.
  logical,                    intent(in), optional :: one_component  !< Force one component.
  logical,                    intent(in), optional :: is_tuples      !< Use "NumberOfTuples".
  integer(I4P)                                     :: error          !< Error status.
  character(len=:), allocatable                    :: data_type      !< Data type.
  integer(I4P)                                     :: n_components   !< Number of components.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  data_type = 'Int64'
  n_components = size(x, dim=1)
  if (present(one_component)) then
    if (one_component) n_components = 1
  endif
  call self%write_dataarray_tag_appended(data_type=data_type, number_of_components=n_components, data_name=data_name, &
                                         is_tuples=is_tuples)
  call self%ioffset_update(n_byte=self%write_on_scratch_dataarray(x=x))
  error = self%error
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction write_dataarray1_rank4_I8P

  function write_dataarray1_rank4_I4P(self, data_name, x, one_component, is_tuples) result(error)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Write `<DataArray... NumberOfComponents="n"...>...</DataArray>` tag (I4P).
  !---------------------------------------------------------------------------------------------------------------------------------
  class(xml_writer_appended), intent(inout)        :: self           !< Writer.
  character(*),               intent(in)           :: data_name      !< Data name.
  integer(I4P),               intent(in)           :: x(1:,1:,1:,1:) !< Data variable.
  logical,                    intent(in), optional :: one_component  !< Force one component.
  logical,                    intent(in), optional :: is_tuples      !< Use "NumberOfTuples".
  integer(I4P)                                     :: error          !< Error status.
  character(len=:), allocatable                    :: data_type      !< Data type.
  integer(I4P)                                     :: n_components   !< Number of components.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  data_type = 'Int32'
  n_components = size(x, dim=1)
  if (present(one_component)) then
    if (one_component) n_components = 1
  endif
  call self%write_dataarray_tag_appended(data_type=data_type, number_of_components=n_components, data_name=data_name, &
                                         is_tuples=is_tuples)
  call self%ioffset_update(n_byte=self%write_on_scratch_dataarray(x=x))
  error = self%error
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction write_dataarray1_rank4_I4P

  function write_dataarray1_rank4_I2P(self, data_name, x, one_component, is_tuples) result(error)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Write `<DataArray... NumberOfComponents="n"...>...</DataArray>` tag (I2P).
  !---------------------------------------------------------------------------------------------------------------------------------
  class(xml_writer_appended), intent(inout)        :: self           !< Writer.
  character(*),               intent(in)           :: data_name      !< Data name.
  integer(I2P),               intent(in)           :: x(1:,1:,1:,1:) !< Data variable.
  logical,                    intent(in), optional :: one_component  !< Force one component.
  logical,                    intent(in), optional :: is_tuples      !< Use "NumberOfTuples".
  integer(I4P)                                     :: error          !< Error status.
  character(len=:), allocatable                    :: data_type      !< Data type.
  integer(I4P)                                     :: n_components   !< Number of components.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  data_type = 'Int16'
  n_components = size(x, dim=1)
  if (present(one_component)) then
    if (one_component) n_components = 1
  endif
  call self%write_dataarray_tag_appended(data_type=data_type, number_of_components=n_components, data_name=data_name, &
                                         is_tuples=is_tuples)
  call self%ioffset_update(n_byte=self%write_on_scratch_dataarray(x=x))
  error = self%error
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction write_dataarray1_rank4_I2P

  function write_dataarray1_rank4_I1P(self, data_name, x, one_component, is_tuples) result(error)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Write `<DataArray... NumberOfComponents="n"...>...</DataArray>` tag (I1P).
  !---------------------------------------------------------------------------------------------------------------------------------
  class(xml_writer_appended), intent(inout)        :: self           !< Writer.
  character(*),               intent(in)           :: data_name      !< Data name.
  integer(I1P),               intent(in)           :: x(1:,1:,1:,1:) !< Data variable.
  logical,                    intent(in), optional :: one_component  !< Force one component.
  logical,                    intent(in), optional :: is_tuples      !< Use "NumberOfTuples".
  integer(I4P)                                     :: error          !< Error status.
  character(len=:), allocatable                    :: data_type      !< Data type.
  integer(I4P)                                     :: n_components   !< Number of components.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  data_type = 'Int8'
  n_components = size(x, dim=1)
  if (present(one_component)) then
    if (one_component) n_components = 1
  endif
  call self%write_dataarray_tag_appended(data_type=data_type, number_of_components=n_components, data_name=data_name, &
                                         is_tuples=is_tuples)
  call self%ioffset_update(n_byte=self%write_on_scratch_dataarray(x=x))
  error = self%error
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction write_dataarray1_rank4_I1P

  function write_dataarray3_rank1_R8P(self, data_name, x, y, z, is_tuples) result(error)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Write `<DataArray... NumberOfComponents="3"...>...</DataArray>` tag (R8P).
  !---------------------------------------------------------------------------------------------------------------------------------
  class(xml_writer_appended), intent(inout)        :: self         !< Writer.
  character(*),               intent(in)           :: data_name    !< Data name.
  real(R8P),                  intent(in)           :: x(1:)        !< X component of data variable.
  real(R8P),                  intent(in)           :: y(1:)        !< Y component of data variable.
  real(R8P),                  intent(in)           :: z(1:)        !< Z component of data variable.
  logical,                    intent(in), optional :: is_tuples    !< Use "NumberOfTuples" instead "NumberOfComponents" attribute.
  integer(I4P)                                     :: error        !< Error status.
  character(len=:), allocatable                    :: data_type    !< Data type.
  integer(I4P)                                     :: n_components !< Number of components.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  data_type = 'Float64'
  n_components = 3
  call self%write_dataarray_tag_appended(data_type=data_type, number_of_components=n_components, data_name=data_name, &
                                         is_tuples=is_tuples)
  call self%ioffset_update(n_byte=self%write_on_scratch_dataarray(x=x, y=y, z=z))
  error = self%error
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction write_dataarray3_rank1_R8P

  function write_dataarray3_rank1_R4P(self, data_name, x, y, z, is_tuples) result(error)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Write `<DataArray... NumberOfComponents="3"...>...</DataArray>` tag (R4P).
  !---------------------------------------------------------------------------------------------------------------------------------
  class(xml_writer_appended), intent(inout)        :: self         !< Writer.
  character(*),               intent(in)           :: data_name    !< Data name.
  real(R4P),                  intent(in)           :: x(1:)        !< X component of data variable.
  real(R4P),                  intent(in)           :: y(1:)        !< Y component of data variable.
  real(R4P),                  intent(in)           :: z(1:)        !< Z component of data variable.
  logical,                    intent(in), optional :: is_tuples    !< Use "NumberOfTuples".
  integer(I4P)                                     :: error        !< Error status.
  character(len=:), allocatable                    :: data_type    !< Data type.
  integer(I4P)                                     :: n_components !< Number of components.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  data_type = 'Float32'
  n_components = 3
  call self%write_dataarray_tag_appended(data_type=data_type, number_of_components=n_components, data_name=data_name, &
                                         is_tuples=is_tuples)
  call self%ioffset_update(n_byte=self%write_on_scratch_dataarray(x=x, y=y, z=z))
  error = self%error
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction write_dataarray3_rank1_R4P

  function write_dataarray3_rank1_I8P(self, data_name, x, y, z, is_tuples) result(error)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Write `<DataArray... NumberOfComponents="3"...>...</DataArray>` tag (I8P).
  !---------------------------------------------------------------------------------------------------------------------------------
  class(xml_writer_appended), intent(inout)        :: self         !< Writer.
  character(*),               intent(in)           :: data_name    !< Data name.
  integer(I8P),               intent(in)           :: x(1:)        !< X component of data variable.
  integer(I8P),               intent(in)           :: y(1:)        !< Y component of data variable.
  integer(I8P),               intent(in)           :: z(1:)        !< Z component of data variable.
  logical,                    intent(in), optional :: is_tuples    !< Use "NumberOfTuples".
  integer(I4P)                                     :: error        !< Error status.
  character(len=:), allocatable                    :: data_type    !< Data type.
  integer(I4P)                                     :: n_components !< Number of components.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  data_type = 'Int64'
  n_components = 3
  call self%write_dataarray_tag_appended(data_type=data_type, number_of_components=n_components, data_name=data_name, &
                                         is_tuples=is_tuples)
  call self%ioffset_update(n_byte=self%write_on_scratch_dataarray(x=x, y=y, z=z))
  error = self%error
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction write_dataarray3_rank1_I8P

  function write_dataarray3_rank1_I4P(self, data_name, x, y, z, is_tuples) result(error)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Write `<DataArray... NumberOfComponents="3"...>...</DataArray>` tag (I4P).
  !---------------------------------------------------------------------------------------------------------------------------------
  class(xml_writer_appended), intent(inout)        :: self         !< Writer.
  character(*),               intent(in)           :: data_name    !< Data name.
  integer(I4P),               intent(in)           :: x(1:)        !< X component of data variable.
  integer(I4P),               intent(in)           :: y(1:)        !< Y component of data variable.
  integer(I4P),               intent(in)           :: z(1:)        !< Z component of data variable.
  logical,                    intent(in), optional :: is_tuples    !< Use "NumberOfTuples".
  integer(I4P)                                     :: error        !< Error status.
  character(len=:), allocatable                    :: data_type    !< Data type.
  integer(I4P)                                     :: n_components !< Number of components.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  data_type = 'Int32'
  n_components = 3
  call self%write_dataarray_tag_appended(data_type=data_type, number_of_components=n_components, data_name=data_name, &
                                         is_tuples=is_tuples)
  call self%ioffset_update(n_byte=self%write_on_scratch_dataarray(x=x, y=y, z=z))
  error = self%error
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction write_dataarray3_rank1_I4P

  function write_dataarray3_rank1_I2P(self, data_name, x, y, z, is_tuples) result(error)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Write `<DataArray... NumberOfComponents="3"...>...</DataArray>` tag (I2P).
  !---------------------------------------------------------------------------------------------------------------------------------
  class(xml_writer_appended), intent(inout)        :: self         !< Writer.
  character(*),               intent(in)           :: data_name    !< Data name.
  integer(I2P),               intent(in)           :: x(1:)        !< X component of data variable.
  integer(I2P),               intent(in)           :: y(1:)        !< Y component of data variable.
  integer(I2P),               intent(in)           :: z(1:)        !< Z component of data variable.
  logical,                    intent(in), optional :: is_tuples    !< Use "NumberOfTuples".
  integer(I4P)                                     :: error        !< Error status.
  character(len=:), allocatable                    :: data_type    !< Data type.
  integer(I4P)                                     :: n_components !< Number of components.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  data_type = 'Int16'
  n_components = 3
  call self%write_dataarray_tag_appended(data_type=data_type, number_of_components=n_components, data_name=data_name, &
                                         is_tuples=is_tuples)
  call self%ioffset_update(n_byte=self%write_on_scratch_dataarray(x=x, y=y, z=z))
  error = self%error
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction write_dataarray3_rank1_I2P

  function write_dataarray3_rank1_I1P(self, data_name, x, y, z, is_tuples) result(error)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Write `<DataArray... NumberOfComponents="3"...>...</DataArray>` tag (I1P).
  !---------------------------------------------------------------------------------------------------------------------------------
  class(xml_writer_appended), intent(inout)        :: self         !< Writer.
  character(*),               intent(in)           :: data_name    !< Data name.
  integer(I1P),               intent(in)           :: x(1:)        !< X component of data variable.
  integer(I1P),               intent(in)           :: y(1:)        !< Y component of data variable.
  integer(I1P),               intent(in)           :: z(1:)        !< Z component of data variable.
  logical,                    intent(in), optional :: is_tuples    !< Use "NumberOfTuples".
  integer(I4P)                                     :: error        !< Error status.
  character(len=:), allocatable                    :: data_type    !< Data type.
  integer(I4P)                                     :: n_components !< Number of components.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  data_type = 'Int8'
  n_components = 3
  call self%write_dataarray_tag_appended(data_type=data_type, number_of_components=n_components, data_name=data_name, &
                                         is_tuples=is_tuples)
  call self%ioffset_update(n_byte=self%write_on_scratch_dataarray(x=x, y=y, z=z))
  error = self%error
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction write_dataarray3_rank1_I1P

  function write_dataarray3_rank3_R8P(self, data_name, x, y, z, is_tuples) result(error)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Write `<DataArray... NumberOfComponents="3"...>...</DataArray>` tag (R8P).
  !---------------------------------------------------------------------------------------------------------------------------------
  class(xml_writer_appended), intent(inout)        :: self         !< Writer.
  character(*),               intent(in)           :: data_name    !< Data name.
  real(R8P),                  intent(in)           :: x(1:,1:,1:)  !< X component of data variable.
  real(R8P),                  intent(in)           :: y(1:,1:,1:)  !< Y component of data variable.
  real(R8P),                  intent(in)           :: z(1:,1:,1:)  !< Z component of data variable.
  logical,                    intent(in), optional :: is_tuples    !< Use "NumberOfTuples".
  integer(I4P)                                     :: error        !< Error status.
  character(len=:), allocatable                    :: data_type    !< Data type.
  integer(I4P)                                     :: n_components !< Number of components.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  data_type = 'Float64'
  n_components = 3
  call self%write_dataarray_tag_appended(data_type=data_type, number_of_components=n_components, data_name=data_name, &
                                         is_tuples=is_tuples)
  call self%ioffset_update(n_byte=self%write_on_scratch_dataarray(x=x, y=y, z=z))
  error = self%error
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction write_dataarray3_rank3_R8P

  function write_dataarray3_rank3_R4P(self, data_name, x, y, z, is_tuples) result(error)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Write `<DataArray... NumberOfComponents="3"...>...</DataArray>` tag (R4P).
  !---------------------------------------------------------------------------------------------------------------------------------
  class(xml_writer_appended), intent(inout)        :: self         !< Writer.
  character(*),               intent(in)           :: data_name    !< Data name.
  real(R4P),                  intent(in)           :: x(1:,1:,1:)  !< X component of data variable.
  real(R4P),                  intent(in)           :: y(1:,1:,1:)  !< Y component of data variable.
  real(R4P),                  intent(in)           :: z(1:,1:,1:)  !< Z component of data variable.
  logical,                    intent(in), optional :: is_tuples    !< Use "NumberOfTuples".
  integer(I4P)                                     :: error        !< Error status.
  character(len=:), allocatable                    :: data_type    !< Data type.
  integer(I4P)                                     :: n_components !< Number of components.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  data_type = 'Float32'
  n_components = 3
  call self%write_dataarray_tag_appended(data_type=data_type, number_of_components=n_components, data_name=data_name, &
                                         is_tuples=is_tuples)
  call self%ioffset_update(n_byte=self%write_on_scratch_dataarray(x=x, y=y, z=z))
  error = self%error
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction write_dataarray3_rank3_R4P

  function write_dataarray3_rank3_I8P(self, data_name, x, y, z, is_tuples) result(error)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Write `<DataArray... NumberOfComponents="3"...>...</DataArray>` tag (I8P).
  !---------------------------------------------------------------------------------------------------------------------------------
  class(xml_writer_appended), intent(inout)        :: self         !< Writer.
  character(*),               intent(in)           :: data_name    !< Data name.
  integer(I8P),               intent(in)           :: x(1:,1:,1:)  !< X component of data variable.
  integer(I8P),               intent(in)           :: y(1:,1:,1:)  !< Y component of data variable.
  integer(I8P),               intent(in)           :: z(1:,1:,1:)  !< Z component of data variable.
  logical,                    intent(in), optional :: is_tuples    !< Use "NumberOfTuples".
  integer(I4P)                                     :: error        !< Error status.
  character(len=:), allocatable                    :: data_type    !< Data type.
  integer(I4P)                                     :: n_components !< Number of components.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  data_type = 'Int64'
  n_components = 3
  call self%write_dataarray_tag_appended(data_type=data_type, number_of_components=n_components, data_name=data_name, &
                                         is_tuples=is_tuples)
  call self%ioffset_update(n_byte=self%write_on_scratch_dataarray(x=x, y=y, z=z))
  error = self%error
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction write_dataarray3_rank3_I8P

  function write_dataarray3_rank3_I4P(self, data_name, x, y, z, is_tuples) result(error)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Write `<DataArray... NumberOfComponents="3"...>...</DataArray>` tag (I4P).
  !---------------------------------------------------------------------------------------------------------------------------------
  class(xml_writer_appended), intent(inout)        :: self         !< Writer.
  character(*),               intent(in)           :: data_name    !< Data name.
  integer(I4P),               intent(in)           :: x(1:,1:,1:)  !< X component of data variable.
  integer(I4P),               intent(in)           :: y(1:,1:,1:)  !< Y component of data variable.
  integer(I4P),               intent(in)           :: z(1:,1:,1:)  !< Z component of data variable.
  logical,                    intent(in), optional :: is_tuples    !< Use "NumberOfTuples".
  integer(I4P)                                     :: error        !< Error status.
  character(len=:), allocatable                    :: data_type    !< Data type.
  integer(I4P)                                     :: n_components !< Number of components.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  data_type = 'Int32'
  n_components = 3
  call self%write_dataarray_tag_appended(data_type=data_type, number_of_components=n_components, data_name=data_name, &
                                         is_tuples=is_tuples)
  call self%ioffset_update(n_byte=self%write_on_scratch_dataarray(x=x, y=y, z=z))
  error = self%error
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction write_dataarray3_rank3_I4P

  function write_dataarray3_rank3_I2P(self, data_name, x, y, z, is_tuples) result(error)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Write `<DataArray... NumberOfComponents="3"...>...</DataArray>` tag (I2P).
  !---------------------------------------------------------------------------------------------------------------------------------
  class(xml_writer_appended), intent(inout)        :: self         !< Writer.
  character(*),               intent(in)           :: data_name    !< Data name.
  integer(I2P),               intent(in)           :: x(1:,1:,1:)  !< X component of data variable.
  integer(I2P),               intent(in)           :: y(1:,1:,1:)  !< Y component of data variable.
  integer(I2P),               intent(in)           :: z(1:,1:,1:)  !< Z component of data variable.
  logical,                    intent(in), optional :: is_tuples    !< Use "NumberOfTuples".
  integer(I4P)                                     :: error        !< Error status.
  character(len=:), allocatable                    :: data_type    !< Data type.
  integer(I4P)                                     :: n_components !< Number of components.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  data_type = 'Int16'
  n_components = 3
  call self%write_dataarray_tag_appended(data_type=data_type, number_of_components=n_components, data_name=data_name, &
                                         is_tuples=is_tuples)
  call self%ioffset_update(n_byte=self%write_on_scratch_dataarray(x=x, y=y, z=z))
  error = self%error
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction write_dataarray3_rank3_I2P

  function write_dataarray3_rank3_I1P(self, data_name, x, y, z, is_tuples) result(error)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Write `<DataArray... NumberOfComponents="3"...>...</DataArray>` tag (I1P).
  !---------------------------------------------------------------------------------------------------------------------------------
  class(xml_writer_appended), intent(inout)        :: self         !< Writer.
  character(*),               intent(in)           :: data_name    !< Data name.
  integer(I1P),               intent(in)           :: x(1:,1:,1:)  !< X component of data variable.
  integer(I1P),               intent(in)           :: y(1:,1:,1:)  !< Y component of data variable.
  integer(I1P),               intent(in)           :: z(1:,1:,1:)  !< Z component of data variable.
  logical,                    intent(in), optional :: is_tuples    !< Use "NumberOfTuples".
  integer(I4P)                                     :: error        !< Error status.
  character(len=:), allocatable                    :: data_type    !< Data type.
  integer(I4P)                                     :: n_components !< Number of components.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  data_type = 'Int8'
  n_components = 3
  call self%write_dataarray_tag_appended(data_type=data_type, number_of_components=n_components, data_name=data_name, &
                                         is_tuples=is_tuples)
  call self%ioffset_update(n_byte=self%write_on_scratch_dataarray(x=x, y=y, z=z))
  error = self%error
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction write_dataarray3_rank3_I1P

  subroutine write_dataarray_appended(self)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Do nothing, ascii data cannot be appended.
  !---------------------------------------------------------------------------------------------------------------------------------
  class(xml_writer_appended), intent(inout) :: self              !< Writer.
  type(string)                              :: tag_attributes    !< Tag attributes.
  integer(I4P)                              :: n_byte            !< Bytes count.
  character(len=2)                          :: dataarray_type    !< Dataarray type = R8,R4,I8,I4,I2,I1.
  integer(I4P)                              :: dataarray_dim     !< Dataarray dimension.
  real(R8P),    allocatable                 :: dataarray_R8P(:)  !< Dataarray buffer of R8P.
  real(R4P),    allocatable                 :: dataarray_R4P(:)  !< Dataarray buffer of R4P.
  integer(I8P), allocatable                 :: dataarray_I8P(:)  !< Dataarray buffer of I8P.
  integer(I4P), allocatable                 :: dataarray_I4P(:)  !< Dataarray buffer of I4P.
  integer(I2P), allocatable                 :: dataarray_I2P(:)  !< Dataarray buffer of I2P.
  integer(I1P), allocatable                 :: dataarray_I1P(:)  !< Dataarray buffer of I1P.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  call self%write_start_tag(name='AppendedData', attributes='encoding="'//self%encoding%chars()//'"')
  write(unit=self%xml, iostat=self%error)'_'
  endfile(unit=self%scratch, iostat=self%error)
  rewind(unit=self%scratch, iostat=self%error)
  do
    call read_dataarray_from_scratch
    if (self%error==0) call write_dataarray_on_xml
    if (is_iostat_end(self%error)) exit
  enddo
  close(unit=self%scratch, iostat=self%error)
  write(unit=self%xml, iostat=self%error)end_rec
  call self%write_end_tag(name='AppendedData')
  !---------------------------------------------------------------------------------------------------------------------------------
  contains
    subroutine read_dataarray_from_scratch
    !-------------------------------------------------------------------------------------------------------------------------------
    !< Read the current dataaray from scratch file.
    !-------------------------------------------------------------------------------------------------------------------------------

    !-------------------------------------------------------------------------------------------------------------------------------
    read(unit=self%scratch, iostat=self%error, end=10)n_byte, dataarray_type, dataarray_dim
    select case(dataarray_type)
    case('R8')
      if (allocated(dataarray_R8P)) deallocate(dataarray_R8P) ; allocate(dataarray_R8P(1:dataarray_dim))
      read(unit=self%scratch, iostat=self%error)dataarray_R8P
    case('R4')
      if (allocated(dataarray_R4P)) deallocate(dataarray_R4P) ; allocate(dataarray_R4P(1:dataarray_dim))
      read(unit=self%scratch, iostat=self%error)dataarray_R4P
    case('I8')
      if (allocated(dataarray_I8P)) deallocate(dataarray_I8P) ; allocate(dataarray_I8P(1:dataarray_dim))
      read(unit=self%scratch, iostat=self%error)dataarray_I8P
    case('I4')
      if (allocated(dataarray_I4P)) deallocate(dataarray_I4P) ; allocate(dataarray_I4P(1:dataarray_dim))
      read(unit=self%scratch, iostat=self%error)dataarray_I4P
    case('I2')
      if (allocated(dataarray_I2P)) deallocate(dataarray_I2P) ; allocate(dataarray_I2P(1:dataarray_dim))
      read(unit=self%scratch, iostat=self%error)dataarray_I2P
    case('I1')
      if (allocated(dataarray_I1P)) deallocate(dataarray_I1P) ; allocate(dataarray_I1P(1:dataarray_dim))
      read(unit=self%scratch, iostat=self%error)dataarray_I1P
    case default
      self%error = 1
      write (stderr,'(A)')' error: bad dataarray_type = '//dataarray_type
      write (stderr,'(A)')' bytes = '//trim(str(n=n_byte))
      write (stderr,'(A)')' dataarray dimension = '//trim(str(n=dataarray_dim))
    endselect
    10 return
    !-------------------------------------------------------------------------------------------------------------------------------
    endsubroutine read_dataarray_from_scratch

    subroutine write_dataarray_on_xml
    !-------------------------------------------------------------------------------------------------------------------------------
    !< Write the current dataaray on xml file.
    !-------------------------------------------------------------------------------------------------------------------------------
    character(len=:), allocatable  :: code !< Dataarray encoded with Base64 codec.
    !-------------------------------------------------------------------------------------------------------------------------------

    !-------------------------------------------------------------------------------------------------------------------------------
    if (self%encoding=='raw') then
      select case(dataarray_type)
      case('R8')
        write(unit=self%xml, iostat=self%error)n_byte, dataarray_R8P
        deallocate(dataarray_R8P)
      case('R4')
        write(unit=self%xml, iostat=self%error)n_byte, dataarray_R4P
        deallocate(dataarray_R4P)
      case('I8')
        write(unit=self%xml, iostat=self%error)n_byte, dataarray_I8P
        deallocate(dataarray_I8P)
      case('I4')
        write(unit=self%xml, iostat=self%error)n_byte, dataarray_I4P
        deallocate(dataarray_I4P)
      case('I2')
        write(unit=self%xml, iostat=self%error)n_byte, dataarray_I2P
        deallocate(dataarray_I2P)
      case('I1')
        write(unit=self%xml, iostat=self%error)n_byte, dataarray_I1P
        deallocate(dataarray_I1P)
      endselect
    else
      select case(dataarray_type)
      case('R8')
        code = encode_binary_dataarray(x=dataarray_R8P)
        write(unit=self%xml, iostat=self%error)code
      case('R4')
        code = encode_binary_dataarray(x=dataarray_R4P)
        write(unit=self%xml, iostat=self%error)code
      case('I8')
        code = encode_binary_dataarray(x=dataarray_I8P)
        write(unit=self%xml, iostat=self%error)code
      case('I4')
        code = encode_binary_dataarray(x=dataarray_I4P)
        write(unit=self%xml, iostat=self%error)code
      case('I2')
        code = encode_binary_dataarray(x=dataarray_I2P)
        write(unit=self%xml, iostat=self%error)code
      case('I1')
        code = encode_binary_dataarray(x=dataarray_I1P)
        write(unit=self%xml, iostat=self%error)code
      endselect
    endif
    !-------------------------------------------------------------------------------------------------------------------------------
    endsubroutine write_dataarray_on_xml
  endsubroutine write_dataarray_appended

  ! write_on_scratch_dataarray methods
  function write_on_scratch_dataarray1_rank1(self, x) result(n_byte)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Write a dataarray with 1 components of rank 1.
  !---------------------------------------------------------------------------------------------------------------------------------
  class(xml_writer_appended), intent(inout) :: self   !< Writer.
  class(*),                   intent(in)    :: x(1:)  !< Data variable.
  integer(I4P)                              :: n_byte !< Number of bytes
  integer(I4P)                              :: nn     !< Number of elements.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  nn = size(x, dim=1)
  select type(x)
  type is(real(R8P))
    n_byte = nn*BYR8P
    write(unit=self%scratch, iostat=self%error)n_byte, 'R8', nn
    write(unit=self%scratch, iostat=self%error)x
  type is(real(R4P))
    n_byte = nn*BYR4P
    write(unit=self%scratch, iostat=self%error)n_byte, 'R4', nn
    write(unit=self%scratch, iostat=self%error)x
  type is(integer(I8P))
    n_byte = nn*BYI8P
    write(unit=self%scratch, iostat=self%error)n_byte, 'I8', nn
    write(unit=self%scratch, iostat=self%error)x
  type is(integer(I4P))
    n_byte = nn*BYI4P
    write(unit=self%scratch, iostat=self%error)n_byte, 'I4', nn
    write(unit=self%scratch, iostat=self%error)x
  type is(integer(I2P))
    n_byte = nn*BYI2P
    write(unit=self%scratch, iostat=self%error)n_byte, 'I2', nn
    write(unit=self%scratch, iostat=self%error)x
  type is(integer(I1P))
    n_byte = nn*BYI1P
    write(unit=self%scratch, iostat=self%error)n_byte, 'I1', nn
    write(unit=self%scratch, iostat=self%error)x
  endselect
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction write_on_scratch_dataarray1_rank1

  function write_on_scratch_dataarray1_rank2(self, x) result(n_byte)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Write a dataarray with 1 components of rank 2.
  !---------------------------------------------------------------------------------------------------------------------------------
  class(xml_writer_appended), intent(inout) :: self     !< Writer.
  class(*),                   intent(in)    :: x(1:,1:) !< Data variable.
  integer(I4P)                              :: n_byte   !< Number of bytes
  integer(I4P)                              :: nn       !< Number of elements.
  !------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  nn = size(x, dim=1)*size(x, dim=2)
  select type(x)
  type is(real(R8P))
    n_byte = nn*BYR8P
    write(unit=self%scratch, iostat=self%error)n_byte, 'R8', nn
    write(unit=self%scratch, iostat=self%error)x
  type is(real(R4P))
    n_byte = nn*BYR4P
    write(unit=self%scratch, iostat=self%error)n_byte, 'R4', nn
    write(unit=self%scratch, iostat=self%error)x
  type is(integer(I8P))
    n_byte = nn*BYI8P
    write(unit=self%scratch, iostat=self%error)n_byte, 'I8', nn
    write(unit=self%scratch, iostat=self%error)x
  type is(integer(I4P))
    n_byte = nn*BYI4P
    write(unit=self%scratch, iostat=self%error)n_byte, 'I4', nn
    write(unit=self%scratch, iostat=self%error)x
  type is(integer(I2P))
    n_byte = nn*BYI2P
    write(unit=self%scratch, iostat=self%error)n_byte, 'I2', nn
    write(unit=self%scratch, iostat=self%error)x
  type is(integer(I1P))
    n_byte = nn*BYI1P
    write(unit=self%scratch, iostat=self%error)n_byte, 'I1', nn
    write(unit=self%scratch, iostat=self%error)x
  endselect
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction write_on_scratch_dataarray1_rank2

  function write_on_scratch_dataarray1_rank3(self, x) result(n_byte)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Write a dataarray with 1 components of rank 3.
  !---------------------------------------------------------------------------------------------------------------------------------
  class(xml_writer_appended), intent(inout) :: self        !< Writer.
  class(*),                   intent(in)    :: x(1:,1:,1:) !< Data variable.
  integer(I4P)                              :: n_byte      !< Number of bytes
  integer(I4P)                              :: nn          !< Number of elements.
  !------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  nn = size(x, dim=1)*size(x, dim=2)*size(x, dim=3)
  select type(x)
  type is(real(R8P))
    n_byte = nn*BYR8P
    write(unit=self%scratch, iostat=self%error)n_byte, 'R8', nn
    write(unit=self%scratch, iostat=self%error)x
  type is(real(R4P))
    n_byte = nn*BYR4P
    write(unit=self%scratch, iostat=self%error)n_byte, 'R4', nn
    write(unit=self%scratch, iostat=self%error)x
  type is(integer(I8P))
    n_byte = nn*BYI8P
    write(unit=self%scratch, iostat=self%error)n_byte, 'I8', nn
    write(unit=self%scratch, iostat=self%error)x
  type is(integer(I4P))
    n_byte = nn*BYI4P
    write(unit=self%scratch, iostat=self%error)n_byte, 'I4', nn
    write(unit=self%scratch, iostat=self%error)x
  type is(integer(I2P))
    n_byte = nn*BYI2P
    write(unit=self%scratch, iostat=self%error)n_byte, 'I2', nn
    write(unit=self%scratch, iostat=self%error)x
  type is(integer(I1P))
    n_byte = nn*BYI1P
    write(unit=self%scratch, iostat=self%error)n_byte, 'I1', nn
    write(unit=self%scratch, iostat=self%error)x
  endselect
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction write_on_scratch_dataarray1_rank3

  function write_on_scratch_dataarray1_rank4(self, x) result(n_byte)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Write a dataarray with 1 components of rank 4.
  !---------------------------------------------------------------------------------------------------------------------------------
  class(xml_writer_appended), intent(inout) :: self           !< Writer.
  class(*),                   intent(in)    :: x(1:,1:,1:,1:) !< Data variable.
  integer(I4P)                              :: n_byte         !< Number of bytes
  integer(I4P)                              :: nn             !< Number of elements.
  !------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  nn = size(x, dim=1)*size(x, dim=2)*size(x, dim=3)*size(x, dim=4)
  select type(x)
  type is(real(R8P))
    n_byte = nn*BYR8P
    write(unit=self%scratch, iostat=self%error)n_byte, 'R8', nn
    write(unit=self%scratch, iostat=self%error)x
  type is(real(R4P))
    n_byte = nn*BYR4P
    write(unit=self%scratch, iostat=self%error)n_byte, 'R4', nn
    write(unit=self%scratch, iostat=self%error)x
  type is(integer(I8P))
    n_byte = nn*BYI8P
    write(unit=self%scratch, iostat=self%error)n_byte, 'I8', nn
    write(unit=self%scratch, iostat=self%error)x
  type is(integer(I4P))
    n_byte = nn*BYI4P
    write(unit=self%scratch, iostat=self%error)n_byte, 'I4', nn
    write(unit=self%scratch, iostat=self%error)x
  type is(integer(I2P))
    n_byte = nn*BYI2P
    write(unit=self%scratch, iostat=self%error)n_byte, 'I2', nn
    write(unit=self%scratch, iostat=self%error)x
  type is(integer(I1P))
    n_byte = nn*BYI1P
    write(unit=self%scratch, iostat=self%error)n_byte, 'I1', nn
    write(unit=self%scratch, iostat=self%error)x
  endselect
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction write_on_scratch_dataarray1_rank4

  function write_on_scratch_dataarray3_rank1_R8P(self, x, y, z) result(n_byte)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Write a dataarray with 3 components of rank 1 (R8P).
  !---------------------------------------------------------------------------------------------------------------------------------
  class(xml_writer_appended), intent(inout) :: self   !< Writer.
  real(R8P),                  intent(in)    :: x(1:)  !< X component.
  real(R8P),                  intent(in)    :: y(1:)  !< Y component.
  real(R8P),                  intent(in)    :: z(1:)  !< Z component.
  integer(I4P)                              :: n_byte !< Number of bytes
  integer(I4P)                              :: n      !< Counter.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  n_byte = self%write_on_scratch_dataarray(x=[(x(n), y(n), z(n), n=1,size(x, dim=1))])
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction write_on_scratch_dataarray3_rank1_R8P

  function write_on_scratch_dataarray3_rank1_R4P(self, x, y, z) result(n_byte)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Write a dataarray with 3 components of rank 1 (R4P).
  !---------------------------------------------------------------------------------------------------------------------------------
  class(xml_writer_appended), intent(inout) :: self   !< Writer.
  real(R4P),                  intent(in)    :: x(1:)  !< X component.
  real(R4P),                  intent(in)    :: y(1:)  !< Y component.
  real(R4P),                  intent(in)    :: z(1:)  !< Z component.
  integer(I4P)                              :: n_byte !< Number of bytes
  integer(I4P)                              :: n      !< Counter.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  n_byte = self%write_on_scratch_dataarray(x=[(x(n), y(n), z(n), n=1,size(x, dim=1))])
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction write_on_scratch_dataarray3_rank1_R4P

  function write_on_scratch_dataarray3_rank1_I8P(self, x, y, z) result(n_byte)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Write a dataarray with 3 components of rank 1 (I8P).
  !---------------------------------------------------------------------------------------------------------------------------------
  class(xml_writer_appended), intent(inout) :: self   !< Writer.
  integer(I8P),               intent(in)    :: x(1:)  !< X component.
  integer(I8P),               intent(in)    :: y(1:)  !< Y component.
  integer(I8P),               intent(in)    :: z(1:)  !< Z component.
  integer(I4P)                              :: n_byte !< Number of bytes
  integer(I4P)                              :: n      !< Counter.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  n_byte = self%write_on_scratch_dataarray(x=[(x(n), y(n), z(n), n=1,size(x, dim=1))])
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction write_on_scratch_dataarray3_rank1_I8P

  function write_on_scratch_dataarray3_rank1_I4P(self, x, y, z) result(n_byte)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Write a dataarray with 3 components of rank 1 (I4P).
  !---------------------------------------------------------------------------------------------------------------------------------
  class(xml_writer_appended), intent(inout) :: self   !< Writer.
  integer(I4P),               intent(in)    :: x(1:)  !< X component.
  integer(I4P),               intent(in)    :: y(1:)  !< Y component.
  integer(I4P),               intent(in)    :: z(1:)  !< Z component.
  integer(I4P)                              :: n_byte !< Number of bytes
  integer(I4P)                              :: n      !< Counter.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  n_byte = self%write_on_scratch_dataarray(x=[(x(n), y(n), z(n), n=1,size(x, dim=1))])
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction write_on_scratch_dataarray3_rank1_I4P

  function write_on_scratch_dataarray3_rank1_I2P(self, x, y, z) result(n_byte)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Write a dataarray with 3 components of rank 1 (I2P).
  !---------------------------------------------------------------------------------------------------------------------------------
  class(xml_writer_appended), intent(inout) :: self   !< Writer.
  integer(I2P),               intent(in)    :: x(1:)  !< X component.
  integer(I2P),               intent(in)    :: y(1:)  !< Y component.
  integer(I2P),               intent(in)    :: z(1:)  !< Z component.
  integer(I4P)                              :: n_byte !< Number of bytes
  integer(I4P)                              :: n      !< Counter.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  n_byte = self%write_on_scratch_dataarray(x=[(x(n), y(n), z(n), n=1,size(x, dim=1))])
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction write_on_scratch_dataarray3_rank1_I2P

  function write_on_scratch_dataarray3_rank1_I1P(self, x, y, z) result(n_byte)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Write a dataarray with 3 components of rank 1 (I1P).
  !---------------------------------------------------------------------------------------------------------------------------------
  class(xml_writer_appended), intent(inout) :: self   !< Writer.
  integer(I1P),               intent(in)    :: x(1:)  !< X component.
  integer(I1P),               intent(in)    :: y(1:)  !< Y component.
  integer(I1P),               intent(in)    :: z(1:)  !< Z component.
  integer(I4P)                              :: n_byte !< Number of bytes
  integer(I4P)                              :: n      !< Counter.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  n_byte = self%write_on_scratch_dataarray(x=[(x(n), y(n), z(n), n=1,size(x, dim=1))])
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction write_on_scratch_dataarray3_rank1_I1P

  function write_on_scratch_dataarray3_rank2_R8P(self, x, y, z) result(n_byte)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Write a dataarray with 3 components of rank 2 (R8P).
  !---------------------------------------------------------------------------------------------------------------------------------
  class(xml_writer_appended), intent(inout) :: self     !< Writer.
  real(R8P),                  intent(in)    :: x(1:,1:) !< X component.
  real(R8P),                  intent(in)    :: y(1:,1:) !< Y component.
  real(R8P),                  intent(in)    :: z(1:,1:) !< Z component.
  integer(I4P)                              :: n_byte   !< Number of bytes
  integer(I4P)                              :: n1       !< Counter.
  integer(I4P)                              :: n2       !< Counter.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  n_byte = self%write_on_scratch_dataarray(x=[((x(n1,n2), y(n1,n2), z(n1,n2), n1=1,size(x, dim=1)),n2=1,size(x, dim=2))])
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction write_on_scratch_dataarray3_rank2_R8P

  function write_on_scratch_dataarray3_rank2_R4P(self, x, y, z) result(n_byte)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Write a dataarray with 3 components of rank 2 (R4P).
  !---------------------------------------------------------------------------------------------------------------------------------
  class(xml_writer_appended), intent(inout) :: self     !< Writer.
  real(R4P),                  intent(in)    :: x(1:,1:) !< X component.
  real(R4P),                  intent(in)    :: y(1:,1:) !< Y component.
  real(R4P),                  intent(in)    :: z(1:,1:) !< Z component.
  integer(I4P)                              :: n_byte   !< Number of bytes
  integer(I4P)                              :: n1       !< Counter.
  integer(I4P)                              :: n2       !< Counter.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  n_byte = self%write_on_scratch_dataarray(x=[((x(n1,n2), y(n1,n2), z(n1,n2), n1=1,size(x, dim=1)),n2=1,size(x, dim=2))])
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction write_on_scratch_dataarray3_rank2_R4P

  function write_on_scratch_dataarray3_rank2_I8P(self, x, y, z) result(n_byte)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Write a dataarray with 3 components of rank 2 (I8P).
  !---------------------------------------------------------------------------------------------------------------------------------
  class(xml_writer_appended), intent(inout) :: self     !< Writer.
  integer(I8P),               intent(in)    :: x(1:,1:) !< X component.
  integer(I8P),               intent(in)    :: y(1:,1:) !< Y component.
  integer(I8P),               intent(in)    :: z(1:,1:) !< Z component.
  integer(I4P)                              :: n_byte   !< Number of bytes
  integer(I4P)                              :: n1       !< Counter.
  integer(I4P)                              :: n2       !< Counter.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  n_byte = self%write_on_scratch_dataarray(x=[((x(n1,n2), y(n1,n2), z(n1,n2), n1=1,size(x, dim=1)),n2=1,size(x, dim=2))])
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction write_on_scratch_dataarray3_rank2_I8P

  function write_on_scratch_dataarray3_rank2_I4P(self, x, y, z) result(n_byte)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Write a dataarray with 3 components of rank 2 (I4P).
  !---------------------------------------------------------------------------------------------------------------------------------
  class(xml_writer_appended), intent(inout) :: self     !< Writer.
  integer(I4P),               intent(in)    :: x(1:,1:) !< X component.
  integer(I4P),               intent(in)    :: y(1:,1:) !< Y component.
  integer(I4P),               intent(in)    :: z(1:,1:) !< Z component.
  integer(I4P)                              :: n_byte   !< Number of bytes
  integer(I4P)                              :: n1       !< Counter.
  integer(I4P)                              :: n2       !< Counter.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  n_byte = self%write_on_scratch_dataarray(x=[((x(n1,n2), y(n1,n2), z(n1,n2), n1=1,size(x, dim=1)),n2=1,size(x, dim=2))])
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction write_on_scratch_dataarray3_rank2_I4P

  function write_on_scratch_dataarray3_rank2_I2P(self, x, y, z) result(n_byte)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Write a dataarray with 3 components of rank 2 (I2P).
  !---------------------------------------------------------------------------------------------------------------------------------
  class(xml_writer_appended), intent(inout) :: self     !< Writer.
  integer(I2P),               intent(in)    :: x(1:,1:) !< X component.
  integer(I2P),               intent(in)    :: y(1:,1:) !< Y component.
  integer(I2P),               intent(in)    :: z(1:,1:) !< Z component.
  integer(I4P)                              :: n_byte   !< Number of bytes
  integer(I4P)                              :: n1       !< Counter.
  integer(I4P)                              :: n2       !< Counter.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  n_byte = self%write_on_scratch_dataarray(x=[((x(n1,n2), y(n1,n2), z(n1,n2), n1=1,size(x, dim=1)),n2=1,size(x, dim=2))])
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction write_on_scratch_dataarray3_rank2_I2P

  function write_on_scratch_dataarray3_rank2_I1P(self, x, y, z) result(n_byte)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Write a dataarray with 3 components of rank 2 (I1P).
  !---------------------------------------------------------------------------------------------------------------------------------
  class(xml_writer_appended), intent(inout) :: self     !< Writer.
  integer(I1P),               intent(in)    :: x(1:,1:) !< X component.
  integer(I1P),               intent(in)    :: y(1:,1:) !< Y component.
  integer(I1P),               intent(in)    :: z(1:,1:) !< Z component.
  integer(I4P)                              :: n_byte   !< Number of bytes
  integer(I4P)                              :: n1       !< Counter.
  integer(I4P)                              :: n2       !< Counter.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  n_byte = self%write_on_scratch_dataarray(x=[((x(n1,n2), y(n1,n2), z(n1,n2), n1=1,size(x, dim=1)),n2=1,size(x, dim=2))])
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction write_on_scratch_dataarray3_rank2_I1P

  function write_on_scratch_dataarray3_rank3_R8P(self, x, y, z) result(n_byte)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Write a dataarray with 3 components of rank 3 (R8P).
  !---------------------------------------------------------------------------------------------------------------------------------
  class(xml_writer_appended), intent(inout) :: self        !< Writer.
  real(R8P),                  intent(in)    :: x(1:,1:,1:) !< X component.
  real(R8P),                  intent(in)    :: y(1:,1:,1:) !< Y component.
  real(R8P),                  intent(in)    :: z(1:,1:,1:) !< Z component.
  integer(I4P)                              :: n_byte      !< Number of bytes
  integer(I4P)                              :: n1          !< Counter.
  integer(I4P)                              :: n2          !< Counter.
  integer(I4P)                              :: n3          !< Counter.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  n_byte = self%write_on_scratch_dataarray(x=[(((x(n1,n2,n3), y(n1,n2,n3), z(n1,n2,n3), &
                                           n1=1,size(x, dim=1)),n2=1,size(x, dim=2)),n3=1,size(x, dim=3))])
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction write_on_scratch_dataarray3_rank3_R8P

  function write_on_scratch_dataarray3_rank3_R4P(self, x, y, z) result(n_byte)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Write a dataarray with 3 components of rank 3 (R4P).
  !---------------------------------------------------------------------------------------------------------------------------------
  class(xml_writer_appended), intent(inout) :: self        !< Writer.
  real(R4P),                  intent(in)    :: x(1:,1:,1:) !< X component.
  real(R4P),                  intent(in)    :: y(1:,1:,1:) !< Y component.
  real(R4P),                  intent(in)    :: z(1:,1:,1:) !< Z component.
  integer(I4P)                              :: n_byte      !< Number of bytes
  integer(I4P)                              :: n1          !< Counter.
  integer(I4P)                              :: n2          !< Counter.
  integer(I4P)                              :: n3          !< Counter.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  n_byte = self%write_on_scratch_dataarray(x=[(((x(n1,n2,n3), y(n1,n2,n3), z(n1,n2,n3), &
                                           n1=1,size(x, dim=1)),n2=1,size(x, dim=2)),n3=1,size(x, dim=3))])
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction write_on_scratch_dataarray3_rank3_R4P

  function write_on_scratch_dataarray3_rank3_I8P(self, x, y, z) result(n_byte)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Write a dataarray with 3 components of rank 3 (I8P).
  !---------------------------------------------------------------------------------------------------------------------------------
  class(xml_writer_appended), intent(inout) :: self        !< Writer.
  integer(I8P),               intent(in)    :: x(1:,1:,1:) !< X component.
  integer(I8P),               intent(in)    :: y(1:,1:,1:) !< Y component.
  integer(I8P),               intent(in)    :: z(1:,1:,1:) !< Z component.
  integer(I4P)                              :: n_byte      !< Number of bytes
  integer(I4P)                              :: n1          !< Counter.
  integer(I4P)                              :: n2          !< Counter.
  integer(I4P)                              :: n3          !< Counter.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  n_byte = self%write_on_scratch_dataarray(x=[(((x(n1,n2,n3), y(n1,n2,n3), z(n1,n2,n3), &
                                           n1=1,size(x, dim=1)),n2=1,size(x, dim=2)),n3=1,size(x, dim=3))])
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction write_on_scratch_dataarray3_rank3_I8P

  function write_on_scratch_dataarray3_rank3_I4P(self, x, y, z) result(n_byte)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Write a dataarray with 3 components of rank 3 (I4P).
  !---------------------------------------------------------------------------------------------------------------------------------
  class(xml_writer_appended), intent(inout) :: self        !< Writer.
  integer(I4P),               intent(in)    :: x(1:,1:,1:) !< X component.
  integer(I4P),               intent(in)    :: y(1:,1:,1:) !< Y component.
  integer(I4P),               intent(in)    :: z(1:,1:,1:) !< Z component.
  integer(I4P)                              :: n_byte      !< Number of bytes
  integer(I4P)                              :: n1          !< Counter.
  integer(I4P)                              :: n2          !< Counter.
  integer(I4P)                              :: n3          !< Counter.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  n_byte = self%write_on_scratch_dataarray(x=[(((x(n1,n2,n3), y(n1,n2,n3), z(n1,n2,n3), &
                                           n1=1,size(x, dim=1)),n2=1,size(x, dim=2)),n3=1,size(x, dim=3))])
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction write_on_scratch_dataarray3_rank3_I4P

  function write_on_scratch_dataarray3_rank3_I2P(self, x, y, z) result(n_byte)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Write a dataarray with 3 components of rank 3 (I2P).
  !---------------------------------------------------------------------------------------------------------------------------------
  class(xml_writer_appended), intent(inout) :: self        !< Writer.
  integer(I2P),               intent(in)    :: x(1:,1:,1:) !< X component.
  integer(I2P),               intent(in)    :: y(1:,1:,1:) !< Y component.
  integer(I2P),               intent(in)    :: z(1:,1:,1:) !< Z component.
  integer(I4P)                              :: n_byte      !< Number of bytes
  integer(I4P)                              :: n1          !< Counter.
  integer(I4P)                              :: n2          !< Counter.
  integer(I4P)                              :: n3          !< Counter.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  n_byte = self%write_on_scratch_dataarray(x=[(((x(n1,n2,n3), y(n1,n2,n3), z(n1,n2,n3), &
                                           n1=1,size(x, dim=1)),n2=1,size(x, dim=2)),n3=1,size(x, dim=3))])
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction write_on_scratch_dataarray3_rank3_I2P

  function write_on_scratch_dataarray3_rank3_I1P(self, x, y, z) result(n_byte)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Write a dataarray with 3 components of rank 3 (I1P).
  !---------------------------------------------------------------------------------------------------------------------------------
  class(xml_writer_appended), intent(inout) :: self        !< Writer.
  integer(I1P),               intent(in)    :: x(1:,1:,1:) !< X component.
  integer(I1P),               intent(in)    :: y(1:,1:,1:) !< Y component.
  integer(I1P),               intent(in)    :: z(1:,1:,1:) !< Z component.
  integer(I4P)                              :: n_byte      !< Number of bytes
  integer(I4P)                              :: n1          !< Counter.
  integer(I4P)                              :: n2          !< Counter.
  integer(I4P)                              :: n3          !< Counter.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  n_byte = self%write_on_scratch_dataarray(x=[(((x(n1,n2,n3), y(n1,n2,n3), z(n1,n2,n3), &
                                           n1=1,size(x, dim=1)),n2=1,size(x, dim=2)),n3=1,size(x, dim=3))])
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction write_on_scratch_dataarray3_rank3_I1P
endmodule vtk_fortran_vtk_file_xml_writer_appended
