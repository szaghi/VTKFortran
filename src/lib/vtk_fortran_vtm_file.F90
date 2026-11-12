!< VTM file class.
module vtk_fortran_vtm_file
!< VTM file class: a multi-block dataset, a `.vtm` (XML) whose blocks list VTK files, or one VTKHDF file
!< (`initialize(..., format='vtkhdf')`, a `MultiBlockDataSet`) into which the `.vtkhdf` files of the blocks are copied.
!<
!< In the VTKHDF format the same `write_block` calls build the file: each dataset file (a `.vtkhdf` written by `vtk_file`)
!< is copied as the group `Block_<n>` of `/VTKHDF`, and the tree of the blocks is the group `Assembly`, whose groups are
!< the blocks and whose soft links, named after the datasets (`names`, or the file names without directory and
!< extension), point to the copies.
!<
!< Time series: a file is either static or a time series, decided by its first block. The blocks of a time series must
!< all have the same steps (`NSteps`, and the times of `Steps/Values` compared exactly), as the VTKHDF specification
!< requires. ParaView 6.1.1 plays every step of every block (it reads no RectilinearGrid nor StructuredGrid VTKHDF file);
!< VTK 9.7.2 reads every step with `vtkHDFReader%SetStep` before its first update, but its pipeline time
!< (`UpdateTimeStep`) leaves an UnstructuredGrid or PolyData block at the first step and a grid block without arrays (its
!< own files too). Refused: a time-series Table block (VTK 9.7.2 reads it without rows), a
!< static block in a time series or the reverse (VTK fails on the mix, VTK issue 19963), a series with other steps; a
!< composite source (a MultiBlockDataSet or a PartitionedDataSetCollection: the blocks of a composite are not composite,
!< VTKHDF specification; its absolute Assembly links would point at this file's blocks, and VTK 9.7.2 crashes on it) and
!< an OverlappingAMR (a composite in VTK: VTK 9.7.2 reads the multi-block without that block).
!< A refused file leaves the block that `write_block` opened for it open (as a missing file does).
!< The names of the blocks and of the datasets (`names`, or the file names without directory and extension) cannot be
!< empty nor contain `/` or `.` (VTKHDF names): such a name is refused, never renamed.
!<
!< With `initialize(..., format='vtkhdf', composite='PartitionedDataSetCollection')` the file is a VTKHDF
!< `PartitionedDataSetCollection` (VTK's composite of choice) instead: each dataset is copied once as the group of its name
!< (`/VTKHDF/<name>`, the name VTK gives it), with the attribute `Index` (0, 1, ... in the order written); the blocks are
!< the nodes of `Assembly`, as for a multi-block file, and a dataset already copied can be placed in other blocks without a
!< new copy (`write_block(references='name ...')`, soft links to the copy). A dataset name is then unique in the file (a
!< second dataset of the same name is refused: reference the first one) and cannot be `Assembly`. The time series rules
!< are those of the multi-block file. A multi-block file places a dataset in other blocks the same way (`references`, the
!< name of one dataset written before; a name given to several datasets cannot be referenced): VTK reads it in each.
use befor64
use penf
use stringifor
use vtk_fortran_vtk_file_xml_writer_abstract
use vtk_fortran_vtk_file_xml_writer_ascii_local
use foxy, only : xml_index, xml_file, XML_MALFORMED
use vtk_fortran_hdf5
use vtk_fortran_vtk_file_vtkhdf_reader, only : is_hdf5_file
use vtk_fortran_vtk_file_xml_reader, only : xml_reader
use vtk_fortran_parameters, only : stderr

implicit none
private
public :: vtm_file

type :: vtm_file
   !< VTM file class.
   class(xml_writer_abstract), allocatable, public :: xml_writer      !< XML writer.
   integer(I4P), allocatable                       :: scratch_unit(:) !< Scratch units for very large list of named blocks.
   type(xml_index),            private             :: xml             !< Index of the file read (`action='read'`, FoXy).
   logical,                    private             :: is_reading=.false. !< The file is open for reading.
   logical,                    private             :: is_vtkhdf=.false.  !< The file is a VTKHDF one.
   integer(hid_kind),          private             :: file_id=-1_hid_kind !< VTKHDF file.
   integer(hid_kind),          private             :: root=-1_hid_kind    !< Its group /VTKHDF.
   character(len=:), allocatable, private          :: node               !< Path of the open block in the Assembly.
   integer(I4P), allocatable,  private             :: children(:)        !< Children written in each open level.
   integer(I4P),               private             :: n_leaves=0_I4P     !< Datasets copied.
   character(len=:), allocatable, private          :: leaf_links         !< Each dataset linked: its name, NUL, its group
                                                                          !< (Block_<n>), NUL (references of a multi-block).
   integer(I8P),               private             :: version_minor=1_I8P !< Minor version (the highest of the blocks).
   integer(I8P),               private             :: n_steps=-1_I8P     !< Steps of the blocks: -1 none yet, 0 static.
   real(R8P), allocatable,     private             :: times(:)           !< Times of the steps of a time series.
   logical,                    private             :: is_collection=.false. !< A PartitionedDataSetCollection.
   ! a VTKHDF composite read (action read): its entries, gathered from its Assembly when it is opened
   logical,                    private             :: is_reading_vtkhdf=.false. !< A VTKHDF composite is read.
   ! (plain integer arrays and one character buffer: nvfortran 26.9 corrupts arrays of strings grown by constructors)
   integer(I4P), allocatable,  private             :: entry_level(:)     !< Nesting level of the entries, from 1.
   logical,      allocatable,  private             :: entry_is_block(:)  !< The entry is a block (else a dataset).
   integer(I4P), allocatable,  private             :: entry_index(:)     !< Index of the entries among their siblings.
   integer(I4P), allocatable,  private             :: entry_text(:,:)    !< Start and length in entry_chars of the
                                                                          !< name, file (dataset) and Assembly path.
   character(len=:), allocatable, private          :: entry_chars        !< Names, files and paths of the entries.
   logical,      allocatable,  private             :: entry_is_piece(:)  !< The entry is a piece (XML .vtpc partitions).
   ! an XML PartitionedDataSetCollection (.vtpc) or PartitionedDataSet (.vtpd) written: its datasets and assembly, kept
   ! until finalize (each name, file and open node followed by NUL)
   logical,                    private             :: is_xml_collection=.false. !< An XML .vtpc is written.
   logical,                    private             :: is_xml_pds=.false.        !< An XML .vtpd is written.
   character(len=:), allocatable, private          :: xml_names          !< Names of the datasets written.
   character(len=:), allocatable, private          :: xml_files          !< Files of the datasets written.
   character(len=:), allocatable, private          :: xml_assembly       !< Body of the DataAssembly.
   character(len=:), allocatable, private          :: xml_open           !< Open nodes of the assembly.
   integer(I4P),               private             :: xml_ids=0_I4P      !< Assembly node ids given (the root is 0).
   ! an XML OverlappingAMR (.vthb) written: its levels and boxes, kept until finalize
   logical,                    private             :: is_xml_amr=.false. !< An XML .vthb is written.
   character(len=:), allocatable, private          :: amr_filename       !< Its file name.
   real(R8P),                  private             :: amr_origin(3)=0._R8P !< Origin (of every box).
   character(len=:), allocatable, private          :: amr_desc           !< Grid description: XYZ, XY, ...
   integer(I4P),               private             :: amr_levels=0_I4P   !< Levels written.
   real(R8P), allocatable,     private             :: amr_spacing(:,:)   !< Spacing of each level (3, levels).
   integer(I4P), allocatable,  private             :: amr_boxes(:,:)     !< Level and cells (lo hi per axis) of each box.
   character(len=:), allocatable, private          :: amr_files          !< File of each box, followed by NUL.
   contains
      ! public methods
      procedure, pass(self) :: initialize          !< Initialize file.
      procedure, pass(self) :: finalize            !< Finalize file.
      procedure, pass(self) :: get_entries         !< Return the blocks and datasets of a file read.
      procedure, pass(self) :: write_level         !< Write a level of an OverlappingAMR (.vthb).
      generic               :: write_block =>      &
                               write_block_array,  &
                               write_block_string, &
                               write_block_scratch !< Write one block dataset.
      ! private methods
      procedure, pass(self), private :: write_block_array  !< Write one block dataset (array input).
      procedure, pass(self), private :: write_block_string !< Write one block dataset (string input).
      ! scratch files methods``
      procedure, pass(self), private :: parse_scratch_files !< Parse scratch files.
      procedure, pass(self), private :: write_block_scratch !< Write one block dataset on scratch files.
      ! VTKHDF methods
      procedure, pass(self), private :: vtkhdf_close_block  !< Close the open block of a VTKHDF file.
      procedure, pass(self), private :: vtkhdf_entries      !< Gather the entries of a VTKHDF composite read.
      procedure, pass(self), private :: free_entries        !< Free the entries of a VTKHDF composite read.
      procedure, pass(self), private :: vtkhdf_add_entries  !< Gather the entries of a block of its Assembly.
      procedure, pass(self), private :: vtkhdf_unlinked_entries !< The datasets of a collection the Assembly misses.
      procedure, pass(self), private :: vtkhdf_dataset      !< Copy a dataset file into a VTKHDF file.
      procedure, pass(self), private :: vtkhdf_open_block   !< Open a block of a VTKHDF file.
      procedure, pass(self), private :: vtkhdf_reference    !< Place a dataset already copied in the open block.
      ! XML partitioned composites (.vtpc, .vtpd)
      procedure, pass(self), private :: xml_part_open_block   !< Open a node of the assembly of a .vtpc.
      procedure, pass(self), private :: xml_part_close_block  !< Close the open node of the assembly.
      procedure, pass(self), private :: xml_part_dataset      !< A dataset of a .vtpc (a Partitions) or .vtpd (a DataSet).
      procedure, pass(self), private :: xml_part_reference    !< Place a dataset already written in the open node.
      procedure, pass(self), private :: xml_part_finalize     !< Write the Partitions/DataSets and the DataAssembly.
      procedure, pass(self), private :: xml_part_entries      !< Gather the entries of a .vtpc or .vtpd read.
      procedure, pass(self), private :: add_entry             !< Append an entry to the entries read.
      procedure, pass(self), private :: add_partitions        !< Append the entries of a Partitions element.
      procedure, pass(self), private :: xml_amr_finalize      !< Write the levels and boxes of a .vthb.
      procedure, pass(self), private :: xml_amr_entries       !< Gather the entries of a .vthb read.
endtype vtm_file
contains
  ! public methods
  function initialize(self, filename, scratch_units_number, action, format, composite) result(error)
  !< Initialize file: open it for writing (default) or for reading.
  !<
  !< With `format='vtkhdf'` (case insensitive) the file is a VTKHDF `MultiBlockDataSet` (the library built with HDF5),
  !< built by the same `write_block` calls from `.vtkhdf` files, which are copied into it; scratch units are not used.
  !< Its blocks are all static or all time series of UnstructuredGrid and PolyData with the same steps (module header).
  !< `composite='PartitionedDataSetCollection'` (case insensitive; `MultiBlockDataSet` by default) writes a collection
  !< instead, whose datasets can be referenced from several blocks (module header): VTKHDF, or an XML `.vtpc` (its
  !< `Partitions` and `DataAssembly`); `composite='PartitionedDataSet'` an XML `.vtpd` (a flat list of DataSets).
  !<
  !< With `action='read'` (case insensitive) the file is indexed: its blocks and datasets are returned by `get_entries`, and
  !< each dataset file can then be read with `vtk_file` (or `pvtk_file`); `finalize` frees the index. The error is 1 if the
  !< file cannot be read, 2 if it is not a multi-block file (`vtkMultiBlockDataSet`) nor a partitioned one (`.vtpc`,
  !< `.vtpd`). A VTKHDF `MultiBlockDataSet` or
  !< `PartitionedDataSetCollection` is read too: its blocks are the groups of its `Assembly`, its datasets the links in
  !< them, the `file` of each the dataset path for `vtk_file%initialize(filename, action='read', dataset=file)`.
  class(vtm_file), intent(inout)        :: self                  !< VTM file.
  character(*),    intent(in)           :: filename              !< File name of output VTM file.
  integer(I4P),    intent(in), optional :: scratch_units_number  !< Number of scratch units for very large list of named blocks.
  character(*),    intent(in), optional :: action                !< Action: **write** (default) or **read**.
  character(*),    intent(in), optional :: format                !< Format: **xml** (default) or **vtkhdf**.
  character(*),    intent(in), optional :: composite             !< VTKHDF: **MultiBlockDataSet** (default) or
                                                                  !< **PartitionedDataSetCollection**.
  integer(hid_kind)                     :: assembly              !< Group Assembly.
  integer(I4P)                          :: scratch_units_number_ !< Number of scratch units for very large list of named blocks.
  integer(I4P)                          :: error                 !< Error status.
  character(len=:), allocatable         :: value                 !< Attribute value.
  integer(I4P)                          :: root                  !< Index of the VTKFile element.
  integer(I4P)                          :: iostat                !< IO status of the scan.
  type(string)                          :: action_               !< Action, upper case.
  logical                               :: is_pds                !< An XML PartitionedDataSet (.vtpd).

  if (.not.is_initialized) call penf_init
  if (.not.is_b64_initialized) call b64_init
  if (present(action)) then
     action_ = trim(adjustl(action)) ; action_ = action_%upper()
     select case(action_%chars())
     case('READ')
        error = self%finalize()
        if (is_hdf5_file(filename)) then ! a VTKHDF MultiBlockDataSet or PartitionedDataSetCollection
           error = self%vtkhdf_entries(filename)
           self%is_reading_vtkhdf = error == 0
           return
        endif
        call self%xml%scan(filename=filename, iostat=iostat)
        if (iostat /= 0) then ! 2 if malformed XML, 1 if it cannot be read
           error = merge(2_I4P, 1_I4P, iostat == XML_MALFORMED)
           return
        endif
        error = 2
        root = self%xml%find(name='VTKFile', parent=0)
        if (root == 0) return
        value = self%xml%attribute(id=root, name='type')
        if (value == 'vtkPartitionedDataSet' .or. value == 'vtkPartitionedDataSetCollection') then
           ! (its entries gathered at once, then served as those of a VTKHDF composite)
           error = self%xml_part_entries(root, value)
           call self%xml%free
           self%is_reading_vtkhdf = error == 0
           return
        endif
        if (value == 'vtkOverlappingAMR' .or. value == 'vtkHierarchicalBoxDataSet') then ! a .vthb (or a former .vth)
           error = self%xml_amr_entries(root, value)
           call self%xml%free
           self%is_reading_vtkhdf = error == 0
           return
        endif
        if (value /= 'vtkMultiBlockDataSet') return
        if (self%xml%find(name='vtkMultiBlockDataSet', parent=root) == 0) return
        self%is_reading = .true.
        error = 0
        return
     case('WRITE')
     case default
        error = 1
        return
     endselect
  endif
  scratch_units_number_ = 0_I4P ; if (present(scratch_units_number)) scratch_units_number_ = scratch_units_number
  self%is_collection = .false.
  is_pds = .false.
  if (present(composite)) then
     action_ = trim(adjustl(composite)) ; action_ = action_%upper()
     if (action_ == 'PARTITIONEDDATASETCOLLECTION') then
        self%is_collection = .true.
     elseif (action_ == 'PARTITIONEDDATASET') then
        is_pds = .true.
     elseif (action_ == 'OVERLAPPINGAMR') then ! an XML .vthb, written at finalize from the levels given
        error = 1
        if (scratch_units_number_ > 0_I4P) return
        if (present(format)) then
           action_ = trim(adjustl(format)) ; action_ = action_%upper()
           if (action_ /= 'XML') then
              write(stderr, '(A)') 'error: VTKFortran vtm_file: an OverlappingAMR composite is an XML .vthb; a VTKHDF '// &
                                   'OverlappingAMR is written by vtk_file (mesh_topology=''OverlappingAMR'')'
              return
           endif
        endif
        error = self%finalize()
        self%is_xml_amr = .true.
        self%amr_filename = trim(adjustl(filename))
        self%amr_origin = 0._R8P
        self%amr_desc = ''
        self%amr_levels = 0_I4P
        if (allocated(self%amr_spacing)) deallocate(self%amr_spacing)
        if (allocated(self%amr_boxes)) deallocate(self%amr_boxes)
        allocate(self%amr_spacing(3, 0), self%amr_boxes(7, 0))
        self%amr_files = ''
        error = 0
        return
     elseif (action_ /= 'MULTIBLOCKDATASET') then
        error = 1
        return
     endif
  endif
  if (is_pds .or. ((self%is_collection .or. is_pds) .and. scratch_units_number_ > 0_I4P)) then
     ! a partitioned dataset is XML only (a .vtkhdf has partitions of its own); no scratch units for them
     error = 1
     if (scratch_units_number_ > 0_I4P) return
     if (present(format)) then
        action_ = trim(adjustl(format)) ; action_ = action_%upper()
        if (action_ /= 'XML') return
     endif
  endif
  if (present(format)) then
     action_ = trim(adjustl(format)) ; action_ = action_%upper()
     if (action_ == 'VTKHDF') then
        error = self%finalize()
        error = 1
        if (scratch_units_number_ > 0_I4P) return
        if (.not.is_hdf5_enabled) then
           write(stderr, '(A)') 'error: VTKFortran vtm_file: VTKHDF needs the library built with HDF5 (VTKFORTRAN_USE_HDF5)'
           return
        endif
        error = h5_file_create(filename, self%file_id)
        if (error == 0) error = h5_group_create(self%file_id, 'VTKHDF', self%root, track_order=.true.)
        if (error == 0) error = h5_attribute_write(self%root, 'Type', &
                                                   trim(merge('PartitionedDataSetCollection', 'MultiBlockDataSet           ', &
                                                              self%is_collection)))
        if (error == 0) error = h5_group_create(self%root, 'Assembly', assembly, track_order=.true.)
        if (error == 0) error = h5_group_close(assembly)
        if (error /= 0) return
        self%is_vtkhdf = .true.
        self%node = 'Assembly'
        self%children = [0_I4P]
        self%n_leaves = 0_I4P
        self%leaf_links = ''
        self%version_minor = 1_I8P
        self%n_steps = -1_I8P
        if (allocated(self%times)) deallocate(self%times)
        return
     elseif (action_ /= 'XML') then
        error = 1
        return
     endif
  endif
  error = self%finalize()
  if (allocated(self%xml_writer)) deallocate(self%xml_writer)
  allocate(xml_writer_ascii_local :: self%xml_writer)
  self%is_xml_collection = self%is_collection
  self%is_xml_pds = is_pds
  self%xml_names = '' ; self%xml_files = '' ; self%xml_assembly = '' ; self%xml_open = '' ; self%xml_ids = 0_I4P
  if (self%is_xml_collection) then
     error = self%xml_writer%initialize(format='ascii', filename=filename, mesh_topology='vtkPartitionedDataSetCollection')
  elseif (self%is_xml_pds) then
     error = self%xml_writer%initialize(format='ascii', filename=filename, mesh_topology='vtkPartitionedDataSet')
  else
     error = self%xml_writer%initialize(format='ascii', filename=filename, mesh_topology='vtkMultiBlockDataSet')
  endif
  if (scratch_units_number_>0_I4P) allocate(self%scratch_unit(scratch_units_number_))
  endfunction initialize

  function finalize(self) result(error)
  !< Finalize file (writer).
  class(vtm_file), intent(inout) :: self  !< VTM file.
  integer(I4P)                   :: error !< Error status.
  integer(I4P)                   :: e     !< Error of the writer's finalize after a refusal.

  error = 1
  if (self%is_vtkhdf) then
     error = 0
     if (size(self%children) > 1) error = 1 ! a block left open
     if (h5_attribute_write(self%root, 'Version', [2_I8P, self%version_minor]) /= 0) error = 1
     if (h5_group_close(self%root) /= 0) error = 1
     if (h5_file_close(self%file_id) /= 0) error = 1
     self%root = -1_hid_kind ; self%file_id = -1_hid_kind
     self%is_vtkhdf = .false.
     return
  endif
  call self%free_entries ! (also after a refused read, which leaves them allocated)
  if (self%is_reading_vtkhdf) then
     self%is_reading_vtkhdf = .false.
     error = 0
     return
  endif
  if (self%is_reading) then
     call self%xml%free
     self%is_reading = .false.
     error = 0
     return
  endif
  if (self%is_xml_amr) then ! the .vthb: written now, from the levels given
     self%is_xml_amr = .false.
     error = self%xml_amr_finalize()
     return
  endif
  if (allocated(self%scratch_unit)) then
     error = self%parse_scratch_files()
     deallocate(self%scratch_unit)
  endif
  if (allocated(self%xml_writer) .and. (self%is_xml_collection .or. self%is_xml_pds)) then
     error = self%xml_part_finalize() ! its Partitions/DataSets and assembly, kept until now
     self%is_xml_collection = .false. ; self%is_xml_pds = .false.
     if (error /= 0) then
        e = self%xml_writer%finalize()
        deallocate(self%xml_writer)
        return
     endif
  endif
  if (allocated(self%xml_writer)) then
     error = self%xml_writer%finalize()
     ! a finalized writer is dropped: finalizing it again (e.g. by a new initialize) would write the closing tags twice
     deallocate(self%xml_writer)
  endif
  endfunction finalize

  function get_entries(self, level, kind, index, name, file) result(error)
  !< Return the blocks and datasets of a file read (`initialize(..., action='read')`), in the order of the file.
  !<
  !< The hierarchy is flattened depth first: each entry has its nesting level (1 for the children of the root), its kind
  !< (**block**, **dataset**, or **piece** for a `vtkMultiPieceDataSet`, written by VTK or by `write_block(...,
  !< action='pieces')`, whose `DataSet`s follow at the next level), its index among the
  !< children of its parent (-1 if the file has none), its name and, for datasets, its file (empty for blocks and pieces,
  !< or for datasets without file). The children of an entry are the following entries of the next level. The names and files
  !< are blank padded to the longest one; relative files are relative to the directory of the `.vtm` file. A VTKHDF
  !< composite: the groups (blocks) and links (datasets, `file` the dataset under /VTKHDF) of its `Assembly`, then, for a
  !< collection, its datasets (`Index`) that no link reaches, at level 1 in the order of their `Index`.
  !<
  !<```fortran
  !< type(vtm_file)                :: vtm
  !< integer(I4P),     allocatable :: level(:)
  !< character(len=:), allocatable :: kind(:), file(:)
  !< error = vtm%initialize(filename='assembly.vtm', action='read')
  !< error = vtm%get_entries(level=level, kind=kind, file=file)
  !< error = vtm%finalize()
  !<```
  class(vtm_file),               intent(in)            :: self     !< VTM file.
  integer(I4P),     allocatable, intent(out), optional :: level(:) !< Nesting level of the entries, from 1.
  character(len=:), allocatable, intent(out), optional :: kind(:)  !< Kind of the entries: block, dataset or piece.
  integer(I4P),     allocatable, intent(out), optional :: index(:) !< Index of the entries among their siblings.
  character(len=:), allocatable, intent(out), optional :: name(:)  !< Name of the entries.
  character(len=:), allocatable, intent(out), optional :: file(:)  !< File of the datasets.
  integer(I4P)                                         :: error    !< Error status: 0, or 4 if no file is open for reading.
  integer(I4P),     allocatable                        :: ids(:)   !< Indexes of the entry elements.
  character(len=:), allocatable                        :: value    !< Attribute value.
  integer(I4P)                                         :: base     !< Level of the multi-block element.
  integer(I4P)                                         :: n        !< Number of entries.
  integer(I4P)                                         :: e        !< Counter.
  integer(I4P)                                         :: ln       !< Length of the longest name.
  integer(I4P)                                         :: lf       !< Length of the longest file.
  integer(I4P)                                         :: iostat   !< IO status.

  error = 4
  if (self%is_reading_vtkhdf) then ! the entries gathered from the Assembly
     n = size(self%entry_level)
     ln = 0 ; lf = 0
     do e=1, n
        ln = max(ln, self%entry_text(2, e)) ; lf = max(lf, self%entry_text(4, e))
     enddo
     if (present(level)) level = self%entry_level
     if (present(kind)) then
        allocate(character(len=7) :: kind(1:n))
        do e=1, n
           kind(e) = trim(merge('block  ', 'dataset', self%entry_is_block(e)))
           if (allocated(self%entry_is_piece)) then
              if (self%entry_is_piece(e)) kind(e) = 'piece'
           endif
        enddo
     endif
     if (present(index)) index = self%entry_index
     if (present(name)) then
        allocate(character(len=ln) :: name(1:n))
        do e=1, n
           name(e) = self%entry_chars(self%entry_text(1, e):self%entry_text(1, e) + self%entry_text(2, e) - 1)
        enddo
     endif
     if (present(file)) then
        allocate(character(len=lf) :: file(1:n))
        do e=1, n
           file(e) = self%entry_chars(self%entry_text(3, e):self%entry_text(3, e) + self%entry_text(4, e) - 1)
        enddo
     endif
     error = 0
     return
  endif
  if (.not.self%is_reading) return
  base = self%xml%level(self%xml%find(name='vtkMultiBlockDataSet', parent=self%xml%find(name='VTKFile', parent=0)))
  allocate(ids(1:size(self%xml%find_all(name='Block'))+size(self%xml%find_all(name='DataSet'))+ &
               size(self%xml%find_all(name='Piece'))))
  n = 0 ; ln = 0 ; lf = 0
  e = 0
  do ! all the elements, in document order (the name of an element past the last one is empty)
     e = e + 1
     if (len(self%xml%name(e)) == 0) exit
     if (self%xml%level(e) <= base) cycle
     select case(self%xml%name(e))
     case('Block', 'DataSet', 'Piece')
        n = n + 1
        ids(n) = e
        value = self%xml%attribute(id=e, name='name')
        ln = max(ln, len(value))
        value = self%xml%attribute(id=e, name='file')
        lf = max(lf, len(value))
     endselect
  enddo
  if (present(level)) then
     allocate(level(1:n))
     do e=1, n
        level(e) = self%xml%level(ids(e)) - base
     enddo
  endif
  if (present(kind)) then
     allocate(character(len=7) :: kind(1:n))
     do e=1, n
        select case(self%xml%name(ids(e)))
        case('Block')
           kind(e) = 'block'
        case('DataSet')
           kind(e) = 'dataset'
        case default
           kind(e) = 'piece'
        endselect
     enddo
  endif
  if (present(index)) then
     allocate(index(1:n))
     do e=1, n
        index(e) = -1
        call self%xml%get_attribute(id=ids(e), name='index', value=index(e))
     enddo
  endif
  if (present(name)) then
     allocate(character(len=ln) :: name(1:n))
     do e=1, n
        value = self%xml%attribute(id=ids(e), name='name')
        name(e) = value
     enddo
  endif
  if (present(file)) then
     allocate(character(len=lf) :: file(1:n))
     do e=1, n
        value = self%xml%attribute(id=ids(e), name='file')
        file(e) = value
     enddo
  endif
  error = 0
  endfunction get_entries

  ! private methods
  function write_block_array(self, filenames, names, name, action) result(error)
  !< Write one block dataset (array input).
  !<
  !< By default the files are wrapped in a new block; with `action='write'` they are written as datasets of the current
  !< (open) block, without a new block; with `action='pieces'` (an XML `.vtm`) they are wrapped in a `Piece`, a
  !< vtkMultiPieceDataSet whose pieces they are (as VTK writes the partitions of a dataset in a `.vtm`).
  !<
  !<#### Example of usage: 3 files blocks
  !<```fortran
  !< error = vtm%write_block(filenames=['file_1.vts', 'file_2.vts', 'file_3.vtu'], name='my_block')
  !<```
  !<
  !<#### Example of usage: 3 files blocks with custom name
  !<```fortran
  !< error = vtm%write_block(filenames=['file_1.vts', 'file_2.vts', 'file_3.vtu'], &
  !<                         names=['block-bar', 'block-foo', 'block-baz'], name='my_block')
  !<```
  !<
  !<#### Example of usage: nested blocks
  !<```fortran
  !< error = vtm%write_block(action='open', name='assembly')
  !< error = vtm%write_block(filenames=['part_1.vtu', 'part_2.vtu'], action='write') ! datasets 0, 1 of assembly
  !< error = vtm%write_block(filenames=['bolt_1.vtu', 'bolt_2.vtu'], name='bolts')  ! block 2 of assembly
  !< error = vtm%write_block(action='close')
  !<```
  class(vtm_file), intent(inout)        :: self          !< VTM file.
  character(*),    intent(in)           :: filenames(1:) !< File names of VTK files grouped into current block.
  character(*),    intent(in), optional :: names(1:)     !< Auxiliary names attributed to each files.
  character(*),    intent(in), optional :: name          !< Block name
  character(*),    intent(in), optional :: action        !< Action: 'write' (datasets of the current block) or default.
  integer(I4P)                          :: error         !< Error status.
  logical                               :: is_new_block  !< Wrap the files in a new block.
  logical                               :: is_pieces     !< Wrap them in a Piece (a vtkMultiPieceDataSet).

  integer(I4P)                          :: f             !< Counter.

  if (self%is_xml_amr) then
     error = 1
     write(stderr, '(A)') 'error: VTKFortran vtm_file: the boxes of an OverlappingAMR are written by write_level'
     return
  endif
  is_new_block = .true.
  if (present(action)) is_new_block = trim(adjustl(action)) /= 'write' .and. trim(adjustl(action)) /= 'WRITE'
  is_pieces = .false.
  if (present(action)) is_pieces = trim(adjustl(action)) == 'pieces' .or. trim(adjustl(action)) == 'PIECES'
  if (is_pieces .and. (self%is_vtkhdf .or. self%is_xml_collection .or. self%is_xml_pds)) then
     error = 1
     write(stderr, '(A)') 'error: VTKFortran vtm_file: a block of pieces (action=pieces) is a vtkMultiPieceDataSet of an '// &
                          'XML .vtm only'
     return
  endif
  if (self%is_xml_collection .or. self%is_xml_pds) then
     error = 0
     if (is_new_block) error = self%xml_part_open_block(name=name)
     do f=1, size(filenames, dim=1)
        if (error /= 0) exit
        if (present(names)) then
           error = self%xml_part_dataset(filename=filenames(f), name=names(f))
        else
           error = self%xml_part_dataset(filename=filenames(f))
        endif
     enddo
     if (is_new_block .and. error == 0) error = self%xml_part_close_block()
     return
  endif
  if (self%is_vtkhdf) then
     error = 0
     if (is_new_block) error = self%vtkhdf_open_block(name=name)
     do f=1, size(filenames, dim=1)
        if (error /= 0) exit
        if (present(names)) then
           error = self%vtkhdf_dataset(filename=filenames(f), name=names(f))
        else
           error = self%vtkhdf_dataset(filename=filenames(f))
        endif
     enddo
     if (is_new_block .and. error == 0) error = self%vtkhdf_close_block()
     return
  endif
  if (is_new_block) error = self%xml_writer%write_parallel_open_block(name=name, pieces=is_pieces)
  error = self%xml_writer%write_parallel_block_files(filenames=filenames, names=names)
  if (is_new_block) error = self%xml_writer%write_parallel_close_block()
  endfunction write_block_array

   function write_block_string(self, action, filenames, names, name, references) result(error)
   !< Write one block dataset (string input).
   !<
   !< With `action` the block is written in steps, so blocks can be nested: `'open'` opens a (child) block, `'write'` writes the
   !< files as datasets of the current block, `'close'` closes it. The children of a block (blocks and datasets) are indexed
   !< from 0 in the order they are written. `'pieces'` (an XML `.vtm` only) wraps the files in a `Piece`, a
   !< vtkMultiPieceDataSet whose pieces they are.
   !<
   !<#### Example of usage: 3 files blocks
   !<```fortran
   !< error = vtm%write_block(filenames='file_1.vts file_2.vts file_3.vtu', name='my_block')
   !<```
   !<
   !<#### Example of usage: 3 files blocks with custom name
   !<```fortran
   !< error = vtm%write_block(filenames='file_1.vts file_2.vts file_3.vtu', names='block-bar block-foo block-baz', name='my_block')
   !<```
   !<
   !< In a VTKHDF file (`PartitionedDataSetCollection` or multi-block) or a `.vtpc`, `references` (blank separated) places
   !< datasets already written (by their names) in the block too, without a new copy: soft links to their copies (VTKHDF),
   !< after the datasets of `filenames`.
   !<
   !<#### Example of usage: a dataset in two blocks of a collection
   !<```fortran
   !< error = vtm%initialize(filename='plant.vtkhdf', format='vtkhdf', composite='PartitionedDataSetCollection')
   !< error = vtm%write_block(filenames='pump.vtkhdf pipe.vtkhdf', name='hydraulics')
   !< error = vtm%write_block(references='pump', name='rotating')
   !<```
   class(vtm_file), intent(inout)        :: self      !< VTM file.
   character(*),    intent(in), optional :: action    !< Action: [open, close, write, pieces].
   character(*),    intent(in), optional :: filenames !< File names of VTK files grouped into current block.
   character(*),    intent(in), optional :: names     !< Auxiliary names attributed to each files.
   character(*),    intent(in), optional :: name      !< Block name
   character(*),    intent(in), optional :: references !< Datasets already written, placed in the block.
   integer(I4P)                          :: error     !< Error status.
   type(string)                          :: action_   !< Action string.
   type(string)                          :: list      !< A list of names, blank separated.
   type(string), allocatable             :: files(:)  !< File names.
   type(string), allocatable             :: labels(:) !< Dataset names.
   integer(I4P)                          :: f         !< Counter.

   if (self%is_xml_amr) then
      error = 1
      write(stderr, '(A)') 'error: VTKFortran vtm_file: the boxes of an OverlappingAMR are written by write_level'
      return
   endif
   if (self%is_vtkhdf .or. self%is_xml_collection .or. self%is_xml_pds) then
      action_ = 'BLOCK'
      if (present(action)) then
         action_ = trim(adjustl(action)) ; action_ = action_%upper()
      endif
      error = 0
      if (action_ == 'PIECES') then
         error = 1
         write(stderr, '(A)') 'error: VTKFortran vtm_file: a block of pieces (action=pieces) is a vtkMultiPieceDataSet '// &
                              'of an XML .vtm only'
      endif
      if (action_ == 'OPEN' .or. action_ == 'BLOCK') then
         if (self%is_vtkhdf) then
            error = self%vtkhdf_open_block(name=name)
         else
            error = self%xml_part_open_block(name=name)
         endif
      endif
      if ((action_ == 'WRITE' .or. action_ == 'BLOCK') .and. present(filenames) .and. error == 0) then
         list = trim(adjustl(filenames)) ; call list%split(tokens=files, sep=' ')
         allocate(labels(size(files)))
         if (present(names)) then
            list = trim(adjustl(names)) ; call list%split(tokens=labels, sep=' ')
            error = merge(0, 1, size(labels) == size(files))
         endif
         do f=1, size(files)
            if (error /= 0) exit
            if (self%is_vtkhdf .and. present(names)) then
               error = self%vtkhdf_dataset(filename=files(f)%chars(), name=labels(f)%chars())
            elseif (self%is_vtkhdf) then
               error = self%vtkhdf_dataset(filename=files(f)%chars())
            elseif (present(names)) then
               error = self%xml_part_dataset(filename=files(f)%chars(), name=labels(f)%chars())
            else
               error = self%xml_part_dataset(filename=files(f)%chars())
            endif
         enddo
      endif
      if ((action_ == 'WRITE' .or. action_ == 'BLOCK') .and. present(references) .and. error == 0) then
         list = trim(adjustl(references)) ; call list%split(tokens=files, sep=' ')
         do f=1, size(files)
            if (error /= 0) exit
            if (self%is_vtkhdf) then
               error = self%vtkhdf_reference(files(f)%chars())
            else
               error = self%xml_part_reference(files(f)%chars())
            endif
         enddo
      endif
      if ((action_ == 'CLOSE' .or. action_ == 'BLOCK') .and. error == 0) then
         if (self%is_vtkhdf) then
            error = self%vtkhdf_close_block()
         else
            error = self%xml_part_close_block()
         endif
      endif
      return
   endif
   if (present(references)) then ! references: datasets of a collection only
      error = 1
      write(stderr, '(A)') 'error: VTKFortran vtm_file: references are for a VTKHDF file or a .vtpc (an XML .vtm '// &
                           'repeats the file)'
      return
   endif
   if (present(action)) then
      action_ = trim(adjustl(action)) ; action_ = action_%upper()
      select case(action_%chars())
      case('OPEN')
         error = self%xml_writer%write_parallel_open_block(name=name)
      case('CLOSE')
         error = self%xml_writer%write_parallel_close_block()
      case('WRITE')
         if (present(filenames)) error = self%xml_writer%write_parallel_block_files(filenames=filenames, names=names)
      case('PIECES')
         error = self%xml_writer%write_parallel_open_block(name=name, pieces=.true.)
         error = self%xml_writer%write_parallel_block_files(filenames=filenames, names=names)
         error = self%xml_writer%write_parallel_close_block()
      endselect
   else
      error = self%xml_writer%write_parallel_open_block(name=name)
      error = self%xml_writer%write_parallel_block_files(filenames=filenames, names=names)
      error = self%xml_writer%write_parallel_close_block()
   endif
   endfunction write_block_string

   ! scratch files methods
   function parse_scratch_files(self) result(error)
   !< Parse scratch files.
   class(vtm_file), intent(inout) :: self     !< VTM file.
   integer(I4P)                   :: error    !< Error status.
   character(9999)                :: filename !< File name of VTK file grouped into current block.
   character(9999)                :: name     !< Block name
   integer(I4P)                   :: s, f     !< Counter.

   if (allocated(self%scratch_unit)) then
      do s=1, size(self%scratch_unit, dim=1)
         ! rewind scratch file
         rewind(self%scratch_unit(s))
         ! write group name
         f = 0_I4P
         ! edit descriptor A, not list-directed: blanks, commas and slashes are part of names and paths
         read(self%scratch_unit(s), iostat=error, fmt='(A)') name
         error = self%write_block(action='open', name=trim(adjustl(name)))
         ! write group filenames
         parse_file_loop : do
            read(self%scratch_unit(s), iostat=error, fmt='(A)') filename
            if (is_iostat_end(error)) exit parse_file_loop
            error = self%xml_writer%write_parallel_block_files(file_index=f,                     &
                                                               filename=trim(adjustl(filename)), &
                                                               name=trim(adjustl(filename)))
            f = f + 1_I4P
         enddo parse_file_loop
         ! close group
         error = self%write_block(action='close')
         ! close scratch file
         close(self%scratch_unit(s))
      enddo
   endif
   endfunction parse_scratch_files

   function write_block_scratch(self, scratch, action, filename, name) result(error)
   !< Write one block dataset on scratch files.
   class(vtm_file), intent(inout)        :: self     !< VTM file.
   integer(I4P),    intent(in)           :: scratch  !< Scratch unit.
   character(*),    intent(in)           :: action   !< Action: [open, write].
   character(*),    intent(in), optional :: filename !< File name of VTK file grouped into current block.
   character(*),    intent(in), optional :: name     !< Block name
   integer(I4P)                          :: error    !< Error status.
   type(string)                          :: action_  !< Action string.
   type(string)                          :: name_    !< Block name, local variable

   error = 1
   if (self%is_vtkhdf) return ! no scratch units in the VTKHDF format: the blocks are written in order
   action_ = trim(adjustl(action)) ; action_ = action_%upper()
   select case(action_%chars())
   case('OPEN')
     open(newunit=self%scratch_unit(scratch), &
          form='FORMATTED',                   &
          action='READWRITE',                 &
          status='SCRATCH',                   &
          iostat=error)
      name_ = '' ; if (present(name)) name_ = trim(adjustl(name))
      write(self%scratch_unit(scratch), iostat=error, fmt='(A)') name_%chars()
   case('WRITE')
      if (present(filename)) write(self%scratch_unit(scratch), iostat=error, fmt='(A)') trim(filename)
   endselect
   endfunction write_block_scratch

   ! VTKHDF methods
   function vtkhdf_open_block(self, name) result(error)
   !< Open a block of a VTKHDF file: a group of the open block of the Assembly, named `name` or `block_<index>`.
   class(vtm_file), intent(inout)        :: self      !< VTM file.
   character(*),    intent(in), optional :: name      !< Block name.
   integer(I4P)                          :: error     !< Error status.
   character(len=:), allocatable         :: block     !< Block name.
   integer(hid_kind)                     :: group     !< Group of the block.
   integer(I4P), allocatable             :: stack(:)  !< Children of the open levels, one more level.

   block = 'block_'//trim(str(n=self%children(size(self%children)), no_sign=.true.))
   if (present(name)) then
      if (len_trim(name) > 0) block = trim(adjustl(name))
   endif
   if (len(h5_name_invalid(block)) > 0) then
      error = 1
      write(stderr, '(A)') 'error: VTKFortran vtm_file: the block name "'//block//'" '//h5_name_invalid(block)
      return
   endif
   self%children(size(self%children)) = self%children(size(self%children)) + 1_I4P
   error = h5_group_create(self%root, self%node//'/'//block, group, track_order=.true.)
   if (error == 0) error = h5_group_close(group)
   if (error /= 0) then
      write(stderr, '(A)') 'error: VTKFortran vtm_file: cannot create the block '//block//' (a name used twice?)'
      return
   endif
   self%node = self%node//'/'//block
   stack = [self%children, 0_I4P]
   call move_alloc(stack, self%children)
   endfunction vtkhdf_open_block

   pure subroutine free_entries(self)
   !< Free the entries of a VTKHDF composite read.
   class(vtm_file), intent(inout) :: self !< VTM file.

   if (allocated(self%entry_level)) deallocate(self%entry_level)
   if (allocated(self%entry_is_block)) deallocate(self%entry_is_block)
   if (allocated(self%entry_index)) deallocate(self%entry_index)
   if (allocated(self%entry_text)) deallocate(self%entry_text)
   if (allocated(self%entry_chars)) deallocate(self%entry_chars)
   if (allocated(self%entry_is_piece)) deallocate(self%entry_is_piece)
   endsubroutine free_entries

   function vtkhdf_entries(self, filename) result(error)
   !< Open a VTKHDF composite file (action read) and gather its entries from its `Assembly`: the groups are blocks, the
   !< soft links datasets (their file the dataset under `/VTKHDF` they point to, the `dataset` of
   !< `vtk_file%initialize(..., action='read', dataset=)`), in creation order, depth first: the members of a block are
   !< inserted right after it, and the list is walked forward. The file is closed after. Error 1 if it cannot be read,
   !< 2 if it is not a VTKHDF MultiBlockDataSet or PartitionedDataSetCollection.
   class(vtm_file), intent(inout) :: self     !< VTM file.
   character(*),    intent(in)    :: filename !< File name.
   integer(I4P)                   :: error    !< Error status.
   integer(hid_kind)              :: file_id  !< File.
   integer(hid_kind)              :: root     !< Group /VTKHDF.
   character(len=:), allocatable  :: text     !< Type of the file.
   character(len=:), allocatable  :: path     !< Path of a block.
   integer(I4P)                   :: e        !< Entry, error of a closing.

   call self%free_entries
   allocate(self%entry_level(0), self%entry_is_block(0), self%entry_index(0), self%entry_text(6, 0))
   self%entry_chars = ''
   error = 1
   if (h5_file_open(filename, file_id) /= 0) return
   error = 2
   if (h5_group_open(file_id, 'VTKHDF', root) == 0) then
      if (h5_attribute_read(root, 'Type', text) == 0) then
         if ((text == 'MultiBlockDataSet' .or. text == 'PartitionedDataSetCollection') .and. &
             h5_link_exists(root, 'Assembly')) then
            error = self%vtkhdf_add_entries(root, 'Assembly', 1, 0) ! the members of the Assembly, at the start
            e = 1
            do while (error == 0 .and. e <= size(self%entry_level)) ! each block expanded in place, in turn
               if (self%entry_is_block(e)) then ! (its path copied: entry_chars grows, an argument would alias it)
                  path = self%entry_chars(self%entry_text(5, e):self%entry_text(5, e) + self%entry_text(6, e) - 1)
                  error = self%vtkhdf_add_entries(root, path, self%entry_level(e) + 1, e)
               endif
               e = e + 1
            enddo
            ! a collection: its datasets are the groups with an Index; those the Assembly does not reach are listed too
            if (error == 0 .and. text == 'PartitionedDataSetCollection') error = self%vtkhdf_unlinked_entries(file_id, root)
         endif
      endif
      e = h5_group_close(root)
   endif
   e = h5_file_close(file_id)
   endfunction vtkhdf_entries

   function vtkhdf_unlinked_entries(self, file_id, root) result(error)
   !< Append, at the top level and in the order of their `Index`, the datasets of a collection (the groups of /VTKHDF with
   !< an `Index` attribute, the dataset list of the VTKHDF specification) that no soft link of its `Assembly` reaches
   !< (vtkHDFWriter writes such files: VTK reads every dataset of the list).
   class(vtm_file),   intent(inout) :: self    !< VTM file.
   integer(hid_kind), intent(in)    :: file_id !< File.
   integer(hid_kind), intent(in)    :: root    !< Group /VTKHDF.
   integer(I4P)                     :: error   !< Error status.
   character(len=:), allocatable    :: names   !< Members of /VTKHDF, each followed by NUL.
   character(len=:), allocatable    :: member  !< A member.
   character(len=:), allocatable    :: linked  !< The datasets the Assembly reaches, each between NULs.
   integer(I8P),     allocatable    :: values(:) !< Index attribute.
   integer(I8P),     allocatable    :: indexes(:) !< Index of each unlinked dataset.
   integer(I4P),     allocatable    :: firsts(:) !< First character of each unlinked dataset in names.
   integer(hid_kind)                :: group   !< A member group.
   integer(I4P)                     :: first, c, e, k, m, n, top, base !< Positions, counters.

   error = 2
   if (h5_group_names(file_id, 'VTKHDF', names) /= 0) return
   error = 0
   linked = achar(0)
   do e=1, size(self%entry_level)
      if (.not.self%entry_is_block(e)) linked = linked//self%entry_chars(self%entry_text(3, e):self%entry_text(3, e) + &
                                                                          self%entry_text(4, e) - 1)//achar(0)
   enddo
   allocate(indexes(0), firsts(0))
   first = 1
   do while (first <= len(names))
      c = first - 1 + index(names(first:), achar(0))
      member = names(first:c-1)
      if (member /= 'Assembly' .and. index(linked, achar(0)//member//achar(0)) == 0) then
         if (h5_group_open(root, member, group) == 0) then
            if (h5_attribute_exists(group, 'Index')) then
               ! one call per statement: the operands of .and. can be evaluated in any order (values is allocated
               ! by the read)
               if (h5_attribute_read(group, 'Index', values) == 0) then
                  if (size(values) > 0) then
                     indexes = [indexes, values(1)]
                     firsts = [firsts, first]
                  endif
               endif
            endif
            e = h5_group_close(group)
         endif
      endif
      first = c + 1
   enddo
   top = count(self%entry_level == 1)
   n = size(indexes)
   do k=1, n ! in Index order
      m = minloc(indexes, dim=1)
      first = firsts(m)
      c = first - 1 + index(names(first:), achar(0))
      member = names(first:c-1)
      base = len(self%entry_chars)
      self%entry_level = [self%entry_level, 1_I4P]
      self%entry_index = [self%entry_index, top + k - 1_I4P]
      self%entry_is_block = [self%entry_is_block, .false.]
      ! name, file (the group itself), path: the member's name each time
      self%entry_text = reshape([self%entry_text, [base + 1, len(member), base + 1, len(member), base + 1, len(member)]], &
                                [6, size(self%entry_level)])
      self%entry_chars = self%entry_chars//member
      indexes(m) = huge(1_I8P)
   enddo
   endfunction vtkhdf_unlinked_entries

   function vtkhdf_add_entries(self, root, path, level, after) result(error)
   !< Insert the members of the block `path` of the Assembly (nesting level `level`) after the entry `after` (0: first).
   class(vtm_file),   intent(inout) :: self    !< VTM file.
   integer(hid_kind), intent(in)    :: root    !< Group /VTKHDF.
   character(*),      intent(in)    :: path    !< Block, from /VTKHDF.
   integer(I4P),      intent(in)    :: level   !< Nesting level of its members.
   integer(I4P),      intent(in)    :: after   !< Entry after which they go.
   integer(I4P)                     :: error   !< Error status.
   character(len=:), allocatable    :: names   !< Its members, each followed by NUL.
   character(len=:), allocatable    :: target  !< Where a dataset links.
   integer(I4P), allocatable        :: lv(:), ix(:), tx(:,:) !< The members: levels, indices, texts.
   logical,      allocatable        :: bl(:)   !< The members: blocks.
   integer(I4P)                     :: first   !< First character of a member.
   integer(I4P)                     :: c       !< Its NUL.
   integer(I4P)                     :: m       !< Number of members.
   integer(I4P)                     :: base    !< Length of the text before.

   error = 2
   if (h5_group_names(root, path, names) /= 0) return
   error = 0
   m = count([(names(c:c) == achar(0), c=1, len(names))])
   allocate(lv(m), ix(m), bl(m), tx(6, m))
   first = 1
   do m=1, size(lv)
      c = first - 1 + index(names(first:), achar(0))
      lv(m) = level
      ix(m) = m - 1
      base = len(self%entry_chars)
      tx(1:2, m) = [base + 1, c - first] ! the name
      tx(5:6, m) = [base + c - first + 1, len(path) + 1 + c - first] ! the path, path/name
      self%entry_chars = self%entry_chars//names(first:c-1)//path//'/'//names(first:c-1)
      bl(m) = h5_link_target(root, path//'/'//names(first:c-1), target) /= 0 ! a group: no link target
      if (bl(m)) target = ''
      if (index(target, '/VTKHDF/') == 1) target = target(9:)
      if (.not.bl(m)) then ! an empty dataset (a group without Type, VTK's null block): no file, as in an XML .vtm
         if (.not.has_type(root, target)) target = ''
      endif
      tx(3:4, m) = [len(self%entry_chars) + 1, len(target)] ! the dataset
      self%entry_chars = self%entry_chars//target
      first = c + 1
   enddo
   self%entry_level = [self%entry_level(1:after), lv, self%entry_level(after+1:)]
   self%entry_index = [self%entry_index(1:after), ix, self%entry_index(after+1:)]
   self%entry_is_block = [self%entry_is_block(1:after), bl, self%entry_is_block(after+1:)]
   self%entry_text = reshape([self%entry_text(:, 1:after), tx, self%entry_text(:, after+1:)], &
                             [6, size(self%entry_level)])
   endfunction vtkhdf_add_entries

   function xml_part_open_block(self, name) result(error)
   !< Open a node of the assembly of an XML collection (.vtpc): an element named `name` (`block_<n>` by default), as VTK
   !< names the nodes of a vtkDataAssembly. Refused in a .vtpd (no blocks) and for a name that is not an XML element name.
   class(vtm_file), intent(inout)        :: self  !< VTM file.
   character(*),    intent(in), optional :: name  !< Block name.
   integer(I4P)                          :: error !< Error status.
   character(len=:), allocatable         :: node  !< Node name.

   error = 1
   if (self%is_xml_pds) then
      write(stderr, '(A)') 'error: VTKFortran vtm_file: a PartitionedDataSet (.vtpd) has no blocks: its datasets only'
      return
   endif
   node = 'block_'//trim(str(n=self%xml_ids + 1_I4P, no_sign=.true.))
   if (present(name)) then
      if (len_trim(name) > 0) node = trim(adjustl(name))
   endif
   if (.not.is_xml_name(node)) then
      write(stderr, '(A)') 'error: VTKFortran vtm_file: the block name "'//node//'" is not an XML element name (the '// &
                           'nodes of a vtkDataAssembly are)'
      return
   endif
   self%xml_ids = self%xml_ids + 1_I4P
   self%xml_assembly = self%xml_assembly//'<'//node//' id="'//trim(str(n=self%xml_ids, no_sign=.true.))//'">'//new_line('a')
   self%xml_open = self%xml_open//node//achar(0)
   error = 0
   endfunction xml_part_open_block

   function xml_part_close_block(self) result(error)
   !< Close the open node of the assembly of an XML collection.
   class(vtm_file), intent(inout) :: self  !< VTM file.
   integer(I4P)                   :: error !< Error status.
   integer(I4P)                   :: c     !< Start of the open node.

   error = 1
   if (len(self%xml_open) == 0) return
   c = index(self%xml_open(:len(self%xml_open)-1), achar(0), back=.true.)
   self%xml_assembly = self%xml_assembly//'</'//self%xml_open(c+1:len(self%xml_open)-1)//'>'//new_line('a')
   self%xml_open = self%xml_open(:c)
   error = 0
   endfunction xml_part_close_block

   function xml_part_dataset(self, filename, name) result(error)
   !< A dataset of an XML collection (a `Partitions` of one `DataSet`, named `name` or after the file, placed in the open
   !< node of the assembly) or of a .vtpd (a `DataSet`). An empty file name is an empty dataset (no `file`). A name is
   !< used once in a collection (place a dataset in other blocks with `references`).
   class(vtm_file), intent(inout)        :: self     !< VTM file.
   character(*),    intent(in)           :: filename !< File of the dataset.
   character(*),    intent(in), optional :: name     !< Dataset name.
   integer(I4P)                          :: error    !< Error status.
   character(len=:), allocatable         :: file     !< File, trimmed.
   character(len=:), allocatable         :: label    !< Dataset name.

   error = 1
   file = trim(adjustl(filename))
   label = file(index(file, '/', back=.true.)+1:)
   if (index(label, '.', back=.true.) > 1) label = label(1:index(label, '.', back=.true.)-1)
   if (present(name)) then
      if (len_trim(name) > 0) label = trim(adjustl(name))
   endif
   if (index(file, achar(0)) > 0 .or. index(label, achar(0)) > 0) return
   if (self%is_xml_collection .and. index(achar(0)//self%xml_names, achar(0)//label//achar(0)) > 0) then
      write(stderr, '(A)') 'error: VTKFortran vtm_file: the dataset name "'//label//'" is used twice (a collection '// &
                           'writes each dataset once: reference it with references=)'
      return
   endif
   if (self%is_xml_collection) self%xml_assembly = self%xml_assembly//'<dataset id="'// &
      trim(str(n=count_nul(self%xml_names), no_sign=.true.))//'"/>'//new_line('a')
   self%xml_names = self%xml_names//label//achar(0)
   self%xml_files = self%xml_files//file//achar(0)
   error = 0
   endfunction xml_part_dataset

   function xml_part_reference(self, label) result(error)
   !< Place a dataset already written into the open node of the assembly of an XML collection: a `dataset` element with
   !< its index, no new `Partitions`.
   class(vtm_file), intent(inout) :: self  !< VTM file.
   character(*),    intent(in)    :: label !< Name of the dataset.
   integer(I4P)                   :: error !< Error status.
   integer(I4P)                   :: c     !< Position of the name.

   error = 1
   c = index(achar(0)//self%xml_names, achar(0)//trim(label)//achar(0))
   if (.not.self%is_xml_collection .or. c == 0) then
      write(stderr, '(A)') 'error: VTKFortran vtm_file: '//trim(label)//' is not a dataset written in this collection'
      return
   endif
   self%xml_assembly = self%xml_assembly//'<dataset id="'//trim(str(n=count_nul(self%xml_names(:c-1)), no_sign=.true.))// &
                       '"/>'//new_line('a')
   error = 0
   endfunction xml_part_reference

   function xml_part_finalize(self) result(error)
   !< Write the datasets of an XML collection (`Partitions index name`, a `DataSet index file` each) and its assembly
   !< (`DataAssembly encoding="base64"`: the vtkDataAssembly XML, as VTK writes it), or the `DataSet`s of a .vtpd.
   class(vtm_file), intent(inout) :: self      !< VTM file.
   integer(I4P)                   :: error     !< Error status.
   character(len=:), allocatable  :: assembly  !< The vtkDataAssembly XML.
   character(len=:), allocatable  :: code      !< Its base64.
   character(len=:), allocatable  :: file      !< A file.
   integer(I4P)                   :: d, n, f1, f2, n1, n2 !< Counter, datasets, positions.

   error = 1
   if (len(self%xml_open) > 0) return ! a block left open
   n = count_nul(self%xml_names)
   f1 = 1 ; n1 = 1
   do d=0, n - 1
      f2 = f1 - 1 + index(self%xml_files(f1:), achar(0))
      n2 = n1 - 1 + index(self%xml_names(n1:), achar(0))
      file = self%xml_files(f1:f2-1)
      if (self%is_xml_collection) then
         call self%xml_writer%write_start_tag(name='Partitions', attributes='index="'//trim(str(n=d, no_sign=.true.))// &
                                              '" name="'//attr_text(self%xml_names(n1:n2-1))//'"')
         if (len(file) > 0) then
            call self%xml_writer%write_self_closing_tag(name='DataSet', attributes='index="0" file="'//attr_text(file)//'"')
         else
            call self%xml_writer%write_self_closing_tag(name='DataSet', attributes='index="0"')
         endif
         call self%xml_writer%write_end_tag(name='Partitions')
      else
         if (len(file) > 0) then
            call self%xml_writer%write_self_closing_tag(name='DataSet', attributes='index="'//trim(str(n=d, no_sign=.true.))// &
                                                        '" file="'//attr_text(file)//'"')
         else
            call self%xml_writer%write_self_closing_tag(name='DataSet', attributes='index="'//trim(str(n=d, no_sign=.true.))//'"')
         endif
      endif
      f1 = f2 + 1 ; n1 = n2 + 1
   enddo
   if (self%is_xml_collection) then
      assembly = '<?xml version="1.0"?>'//new_line('a')//'<root type="vtkDataAssembly" version="1.0" id="0">'// &
                 new_line('a')//self%xml_assembly//'</root>'//new_line('a')
      call b64_encode(s=assembly, code=code)
      call self%xml_writer%write_tag(name='DataAssembly', attributes='encoding="base64"', content=code)
   endif
   error = self%xml_writer%error
   endfunction xml_part_finalize

   function xml_part_entries(self, root, file_type) result(error)
   !< Gather the entries of an XML PartitionedDataSet (its `DataSet`s, datasets at level 1) or PartitionedDataSetCollection
   !< (the nodes of its `DataAssembly`, depth first: elements are blocks, `dataset` elements its `Partitions` of that
   !< index, then the `Partitions` the assembly does not reach, at level 1). A `Partitions` of one `DataSet` is a dataset
   !< with its file; of several, a dataset without file followed by its `DataSet`s as pieces.
   class(vtm_file), intent(inout)  :: self      !< VTM file.
   integer(I4P),    intent(in)     :: root      !< Index of the VTKFile element.
   character(*),    intent(in)     :: file_type !< vtkPartitionedDataSet or vtkPartitionedDataSetCollection.
   integer(I4P)                    :: error     !< Error status.
   integer(I4P), allocatable       :: parts(:)  !< The Partitions elements.
   integer(I4P), allocatable       :: kids(:)   !< Children of an element.
   integer(I4P), allocatable       :: stack(:), levels(:), sibling(:) !< Assembly nodes to visit: tag, level, index.
   logical,      allocatable       :: reached(:) !< The Partitions reached by the assembly.
   type(xml_file)                  :: assembly  !< The DataAssembly, parsed.
   character(len=:), allocatable   :: code      !< Its base64.
   character(len=:), allocatable   :: text      !< Its XML.
   character(len=:), allocatable   :: value     !< An attribute.
   integer(I4P)                    :: top, d, a, k, c, t, i, lv, iostat, top_index !< Elements, counters.

   call self%free_entries
   allocate(self%entry_level(0), self%entry_is_block(0), self%entry_index(0), self%entry_text(6, 0), &
            self%entry_is_piece(0))
   self%entry_chars = ''
   error = 2
   top = self%xml%find(name=file_type, parent=root)
   if (top == 0) return
   error = 0
   if (file_type == 'vtkPartitionedDataSet') then
      kids = self%xml%children(top)
      do c=1, size(kids)
         if (self%xml%name(kids(c)) /= 'DataSet') cycle
         call self%add_entry(1, .false., .false., index_of(self%xml%attribute(id=kids(c), name='index'), c - 1), &
                             '', self%xml%attribute(id=kids(c), name='file'))
      enddo
      return
   endif
   kids = self%xml%children(top)
   allocate(parts(0))
   do c=1, size(kids)
      if (self%xml%name(kids(c)) == 'Partitions') parts = [parts, kids(c)]
   enddo
   allocate(reached(size(parts)))
   reached = .false.
   top_index = 0
   a = self%xml%find(name='DataAssembly', parent=top)
   if (a > 0) then ! the assembly: base64 of the vtkDataAssembly XML
      code = self%xml%content(id=a)
      code = strip(code)
      error = 6
      if (len(code) == 0 .or. mod(len(code), 4) /= 0) return
      k = len(code) / 4 * 3
      if (code(len(code):len(code)) == '=') k = k - 1
      if (code(len(code)-1:len(code)-1) == '=') k = k - 1
      allocate(character(len=k) :: text)
      call b64_decode(code=code, s=text)
      call assembly%parse(string=text, iostat=iostat)
      if (iostat /= 0) return
      t = assembly%find(name='root', parent=0)
      if (t == 0) return
      error = 0
      ! depth first: a stack of the nodes to visit, the children of a node pushed in reverse order
      kids = assembly%children(t)
      stack = [(kids(c), c=size(kids), 1, -1)]
      levels = [(1, c=1, size(kids))]
      sibling = [(c - 1, c=size(kids), 1, -1)]
      do while (size(stack) > 0)
         t = stack(size(stack)) ; lv = levels(size(levels)) ; i = sibling(size(sibling))
         stack = stack(:size(stack)-1) ; levels = levels(:size(levels)-1) ; sibling = sibling(:size(sibling)-1)
         if (lv == 1) top_index = top_index + 1
         if (assembly%name(t) == 'dataset') then
            value = assembly%attribute(id=t, name='id')
            d = index_of(value, -1) + 1
            if (d >= 1 .and. d <= size(parts)) then
               reached(d) = .true.
               call self%add_partitions(parts(d), lv, i)
            endif
         else
            call self%add_entry(lv, .true., .false., i, assembly%name(t), '')
            kids = assembly%children(t)
            stack = [stack, (kids(c), c=size(kids), 1, -1)]
            levels = [levels, (lv + 1, c=1, size(kids))]
            sibling = [sibling, (c - 1, c=size(kids), 1, -1)]
         endif
      enddo
   endif
   do d=1, size(parts) ! the Partitions the assembly does not reach
      if (reached(d)) cycle
      call self%add_partitions(parts(d), 1, top_index)
      top_index = top_index + 1
   enddo
   endfunction xml_part_entries

   subroutine add_partitions(self, part, level, sibling)
   !< Append the entries of a `Partitions` element: a dataset with the file of its only `DataSet`, or without file and
   !< its `DataSet`s as pieces.
   class(vtm_file), intent(inout) :: self    !< VTM file.
   integer(I4P),    intent(in)    :: part    !< The Partitions element.
   integer(I4P),    intent(in)    :: level   !< Its level.
   integer(I4P),    intent(in)    :: sibling !< Its index among its siblings.
   integer(I4P), allocatable      :: kids(:), sets(:) !< Its children, its DataSets.
   integer(I4P)                   :: c        !< Counter.

   kids = self%xml%children(part)
   allocate(sets(0))
   do c=1, size(kids)
      if (self%xml%name(kids(c)) == 'DataSet') sets = [sets, kids(c)]
   enddo
   if (size(sets) == 1) then
      call self%add_entry(level, .false., .false., sibling, self%xml%attribute(id=part, name='name'), &
                          self%xml%attribute(id=sets(1), name='file'))
   else
      call self%add_entry(level, .false., .false., sibling, self%xml%attribute(id=part, name='name'), '')
      do c=1, size(sets)
         call self%add_entry(level + 1, .false., .true., c - 1, '', self%xml%attribute(id=sets(c), name='file'))
      enddo
   endif
   endsubroutine add_partitions

   subroutine add_entry(self, level, is_block, is_piece, sibling, name, file)
   !< Append an entry to the entries read (no path).
   class(vtm_file), intent(inout) :: self     !< VTM file.
   integer(I4P),    intent(in)    :: level    !< Nesting level.
   logical,         intent(in)    :: is_block !< A block.
   logical,         intent(in)    :: is_piece !< A piece.
   integer(I4P),    intent(in)    :: sibling  !< Index among its siblings.
   character(*),    intent(in)    :: name     !< Name.
   character(*),    intent(in)    :: file     !< File.
   integer(I4P)                   :: base     !< Length of the text before.
   integer(I4P), allocatable      :: text(:,:) !< The texts, one more.

   base = len(self%entry_chars)
   self%entry_level = [self%entry_level, level]
   self%entry_is_block = [self%entry_is_block, is_block]
   self%entry_is_piece = [self%entry_is_piece, is_piece]
   self%entry_index = [self%entry_index, sibling]
   allocate(text(6, size(self%entry_level)))
   text(:, :size(self%entry_level)-1) = self%entry_text
   text(:, size(self%entry_level)) = [base + 1, len(name), base + len(name) + 1, len(file), 1, 0]
   call move_alloc(text, self%entry_text)
   self%entry_chars = self%entry_chars//name//file
   endsubroutine add_entry

   pure function index_of(text, default) result(n)
   !< An integer attribute value, `default` if it is not one.
   character(*), intent(in) :: text    !< Attribute value.
   integer(I4P), intent(in) :: default !< Default.
   integer(I4P)             :: n       !< Value.
   integer                  :: iostat  !< IO status.

   n = default
   if (len_trim(text) == 0) return
   read(text, *, iostat=iostat) n
   if (iostat /= 0) n = default
   endfunction index_of

   pure function count_nul(text) result(n)
   !< The NUL characters of a text: the names of a NUL-ended list.
   character(*), intent(in) :: text !< Text.
   integer(I4P)             :: n    !< NUL characters.
   integer(I4P)             :: c    !< Counter.

   n = 0
   do c=1, len(text)
      if (text(c:c) == achar(0)) n = n + 1
   enddo
   endfunction count_nul

   pure function is_xml_name(name) result(is_name)
   !< True if `name` is an XML element name (ASCII: a letter or `_`, then letters, digits, `_`, `-`, `.`), as the nodes of a
   !< vtkDataAssembly are; `xml` is reserved.
   character(*), intent(in) :: name    !< Name.
   logical                  :: is_name !< Inquire result.
   character(*), parameter  :: FIRST = 'abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ_' !< First characters.

   is_name = .false.
   if (len(name) == 0) return
   if (index(FIRST, name(1:1)) == 0) return
   if (verify(name, FIRST//'0123456789-.') /= 0) return
   is_name = name(1:min(3, len(name))) /= 'xml' .and. name(1:min(3, len(name))) /= 'XML'
   endfunction is_xml_name

   pure function strip(text) result(stripped)
   !< A text without its white space (blanks, tabs, line ends).
   character(*), intent(in)      :: text     !< Text.
   character(len=:), allocatable :: stripped !< Text stripped.
   integer                       :: c, n     !< Counters.

   allocate(character(len=len(text)) :: stripped)
   n = 0
   do c=1, len(text)
      if (text(c:c) == ' ' .or. text(c:c) == achar(9) .or. text(c:c) == achar(10) .or. text(c:c) == achar(13)) cycle
      n = n + 1
      stripped(n:n) = text(c:c)
   enddo
   stripped = stripped(:n)
   endfunction strip

   function has_type(root, dataset) result(has)
   !< Return true if the dataset group `dataset` of /VTKHDF has a `Type` attribute (false for an empty dataset).
   integer(hid_kind), intent(in) :: root    !< Group /VTKHDF.
   character(*),      intent(in) :: dataset !< Dataset group, from /VTKHDF.
   logical                       :: has     !< Inquire result.
   integer(hid_kind)             :: group   !< The dataset group.
   integer(I4P)                  :: e       !< Error of a closing.

   has = .false.
   if (len(dataset) == 0) return
   if (h5_group_open(root, dataset, group) /= 0) return
   has = h5_attribute_exists(group, 'Type')
   e = h5_group_close(group)
   endfunction has_type

   function vtkhdf_close_block(self) result(error)
   !< Close the open block of a VTKHDF file.
   class(vtm_file), intent(inout) :: self     !< VTM file.
   integer(I4P)                   :: error    !< Error status.
   integer(I4P), allocatable      :: stack(:) !< Children of the open levels, one less level.

   error = 1
   if (size(self%children) < 2) return
   self%node = self%node(1:index(self%node, '/', back=.true.)-1)
   stack = self%children(1:size(self%children)-1) ! a new array: no shrinking in place (nvfortran 26.9)
   call move_alloc(stack, self%children)
   error = 0
   endfunction vtkhdf_close_block

   function vtkhdf_dataset(self, filename, name) result(error)
   !< Copy the dataset of a `.vtkhdf` file into the VTKHDF file, as `Block_<n>` (as `<name>` with the attribute `Index` in a
   !< collection), and link it in the open block, named `name` or after the file (without directory and extension). The
   !< time series rules are checked on the file before the copy: the first block decides whether the file is static or a
   !< time series; a series block has the steps of the first one (their number and times, compared exactly), and is not a
   !< Table (VTK 9.7.2 reads a Table-series block without rows). An empty `filename` is an empty dataset (VTK's null block)
   !< of a multi-block file: a group `Block_<n>` without attributes, linked as `name` (or `Block_<n>`); it does not take
   !< part in the time series rules. It is refused in a collection (VTK 9.7.2 then reads none of its datasets).
   class(vtm_file), intent(inout)        :: self      !< VTM file.
   character(*),    intent(in)           :: filename  !< File of the dataset.
   character(*),    intent(in), optional :: name      !< Dataset name.
   integer(I4P)                          :: error     !< Error status.
   character(len=:), allocatable         :: leaf      !< Group of the copy.
   character(len=:), allocatable         :: label     !< Name of the link.
   character(len=:), allocatable         :: file      !< File name, trimmed.
   character(len=:), allocatable         :: topology  !< Type of the dataset.
   character(len=:), allocatable         :: reason    !< Why the dataset is refused.
   integer(hid_kind)                     :: source    !< File of the dataset.
   integer(hid_kind)                     :: group     !< A group.
   integer(I8P), allocatable             :: values(:) !< Attribute values.
   real(R8P), allocatable                :: times(:)  !< Times of the steps of the dataset.
   integer(I8P)                          :: n_steps   !< Steps of the dataset.
   integer(I8P)                          :: s         !< Counter.
   integer(I4P)                          :: e         !< Error of a closing.

   error = 1
   file = trim(adjustl(filename))
   label = file(index(file, '/', back=.true.)+1:)
   if (index(label, '.', back=.true.) > 1) label = label(1:index(label, '.', back=.true.)-1)
   if (len(file) == 0) label = 'Block_'//trim(str(n=self%n_leaves + 1_I4P, no_sign=.true.)) ! an empty dataset
   if (present(name)) then
      if (len_trim(name) > 0) label = trim(adjustl(name))
   endif
   if (len(h5_name_invalid(label)) > 0) then
      write(stderr, '(A)') 'error: VTKFortran vtm_file: the dataset name "'//label//'" '//h5_name_invalid(label)// &
                           ' (give it a name with names=)'
      return
   endif
   if (self%is_collection) then ! the copy is the group of its name: unique, not Assembly
      if (label == 'Assembly' .or. h5_link_exists(self%root, label)) then
         write(stderr, '(A)') 'error: VTKFortran vtm_file: the dataset name "'//label//'" is Assembly or is used twice '// &
                              '(a collection copies each dataset once: reference it with references=)'
         return
      endif
   endif
   if (len(file) == 0 .and. self%is_collection) then
      write(stderr, '(A)') 'error: VTKFortran vtm_file: an empty dataset is refused in a collection (VTK 9.7.2 then '// &
                           'reads none of its partitioned datasets); a multi-block file accepts it'
      return
   endif
   if (len(file) == 0) then ! an empty dataset (a null block of VTK): a group without attributes, linked as a dataset
      self%n_leaves = self%n_leaves + 1_I4P
      self%children(size(self%children)) = self%children(size(self%children)) + 1_I4P
      leaf = 'Block_'//trim(str(n=self%n_leaves, no_sign=.true.))
      error = h5_group_create(self%root, leaf, group)
      if (error == 0) error = h5_group_close(group)
      if (error == 0) error = h5_group_open(self%root, self%node, group)
      if (error == 0) then
         error = h5_link_soft('/VTKHDF/'//leaf, group, label)
         e = h5_group_close(group)
      endif
      if (error == 0) self%leaf_links = self%leaf_links//label//achar(0)//leaf//achar(0)
      if (error /= 0) write(stderr, '(A)') 'error: VTKFortran vtm_file: cannot write the empty dataset '//label
      return
   endif
   if (h5_file_open(file, source) /= 0) then
      write(stderr, '(A)') 'error: VTKFortran vtm_file: cannot open '//file//' as an HDF5 file'
      return
   endif
   ! the type and steps of the dataset, read before it is copied
   reason = 'is not a VTKHDF file'
   topology = ''
   n_steps = 0_I8P
   if (h5_link_exists(source, 'VTKHDF')) then
      reason = ''
      if (h5_group_open(source, 'VTKHDF', group) == 0) then
         if (h5_attribute_read(group, 'Type', topology) /= 0) topology = ''
         e = h5_group_close(group)
      endif
      if (h5_link_exists(source, 'VTKHDF/Steps')) then
         reason = 'has unreadable steps'
         if (h5_group_open(source, 'VTKHDF/Steps', group) == 0) then
            if (h5_attribute_read(group, 'NSteps', values) == 0) then
               n_steps = values(1)
               if (h5_dataset_read(group, 'Values', times) == 0) reason = ''
            endif
            e = h5_group_close(group)
         endif
      endif
   endif
   if (len(reason) == 0 .and. n_steps > 0_I8P) then
      if (size(times) /= n_steps) reason = 'has unreadable steps'
   endif
   if (len(reason) == 0) then
      if (topology == 'MultiBlockDataSet' .or. topology == 'PartitionedDataSetCollection') then
         reason = 'is a '//topology//': the blocks of a composite cannot be composite (VTKHDF specification); its '// &
                  'own Assembly would point at the blocks of this file'
      elseif (topology == 'OverlappingAMR') then
         reason = 'is an OverlappingAMR, a composite in VTK: VTK 9.7.2 reads a multi-block with such a block without it'
      elseif (n_steps > 0_I8P .and. topology == 'Table') then
         reason = 'is a time series of a Table: VTK 9.7.2 reads such a block of a composite without rows'
      elseif (self%n_steps == 0_I8P .and. n_steps > 0_I8P) then
         reason = 'is a time series, the blocks before are not (a file is either static or a time series)'
      elseif (self%n_steps > 0_I8P .and. n_steps == 0_I8P) then
         reason = 'is not a time series, the blocks before are (a file is either static or a time series)'
      elseif (self%n_steps > 0_I8P .and. n_steps /= self%n_steps) then
         reason = 'has '//trim(str(n=n_steps, no_sign=.true.))//' steps, the blocks before '// &
                  trim(str(n=self%n_steps, no_sign=.true.))//' (every block has the same steps)'
      elseif (self%n_steps > 0_I8P) then
         do s=1_I8P, n_steps
            if (times(s) /= self%times(s)) then
               reason = 'has another time at step '//trim(str(n=s, no_sign=.true.))// &
                        ' than the blocks before (every block has the same steps)'
               exit
            endif
         enddo
      endif
   endif
   if (len(reason) > 0) then
      e = h5_file_close(source)
      write(stderr, '(A)') 'error: VTKFortran vtm_file: '//file//' '//reason
      return
   endif
   self%n_leaves = self%n_leaves + 1_I4P
   self%children(size(self%children)) = self%children(size(self%children)) + 1_I4P
   leaf = 'Block_'//trim(str(n=self%n_leaves, no_sign=.true.))
   if (self%is_collection) leaf = label
   error = h5_object_copy(source, 'VTKHDF', self%root, leaf)
   e = h5_file_close(source)
   if (error /= 0) then
      write(stderr, '(A)') 'error: VTKFortran vtm_file: cannot copy '//file
      return
   endif
   if (self%n_steps < 0_I8P) then ! the first block: the file is static or a time series of its steps
      self%n_steps = n_steps
      if (n_steps > 0_I8P) self%times = times
   endif
   ! the version: the highest of the blocks
   error = h5_group_open(self%root, leaf, group)
   if (error /= 0) return
   if (h5_attribute_read(group, 'Version', values) == 0) self%version_minor = max(self%version_minor, values(2))
   if (self%is_collection) error = h5_attribute_write(group, 'Index', [int(self%n_leaves - 1_I4P, I8P)])
   e = h5_group_close(group)
   if (error /= 0) return
   error = h5_group_open(self%root, self%node, group)
   if (error == 0) then
      error = h5_link_soft('/VTKHDF/'//leaf, group, label)
      e = h5_group_close(group)
   endif
   if (error == 0) self%leaf_links = self%leaf_links//label//achar(0)//leaf//achar(0)
   if (error /= 0) write(stderr, '(A)') 'error: VTKFortran vtm_file: cannot link '//label//' (a name used twice?)'
   endfunction vtkhdf_dataset

   function vtkhdf_reference(self, label) result(error)
   !< Place a dataset already copied in the open block: a soft link to the copy, named `label`, no new copy. In a collection
   !< the copy `/VTKHDF/<label>` (with its `Index`); in a multi-block the copy (`Block_<n>`) of the one dataset written
   !< before under that name (a name given to several datasets is refused: ambiguous).
   class(vtm_file), intent(inout) :: self    !< VTM file.
   character(*),    intent(in)    :: label   !< Name of the dataset.
   integer(I4P)                   :: error   !< Error status.
   character(len=:), allocatable  :: leaf    !< Group of the copy.
   integer(hid_kind)              :: group   !< A group.
   integer(I4P)                   :: e       !< Error of a closing.
   integer(I4P)                   :: matches !< Datasets of a multi-block written under the name.
   integer(I4P)                   :: i, j, k !< Positions in leaf_links.

   error = 1
   leaf = ''
   if (len(h5_name_invalid(label)) > 0 .or. label == 'Assembly') then
      continue
   elseif (self%is_collection) then
      if (h5_link_exists(self%root, label)) then
         if (h5_group_open(self%root, label, group) == 0) then
            if (h5_attribute_exists(group, 'Index')) leaf = label
            e = h5_group_close(group)
         endif
      endif
   else ! a multi-block: the pairs name NUL group NUL of the datasets linked
      matches = 0
      i = 1
      do while (i <= len(self%leaf_links))
         j = index(self%leaf_links(i:), achar(0)) + i - 1
         k = index(self%leaf_links(j+1:), achar(0)) + j
         if (self%leaf_links(i:j-1) == label) then
            matches = matches + 1
            leaf = self%leaf_links(j+1:k-1)
         endif
         i = k + 1
      enddo
      if (matches > 1) then
         write(stderr, '(A)') 'error: VTKFortran vtm_file: cannot reference "'//label//'": several datasets have that '// &
                              'name (give them unique names with names=)'
         return
      endif
   endif
   if (len(leaf) == 0) then
      write(stderr, '(A)') 'error: VTKFortran vtm_file: cannot reference "'//label//'": not a dataset written before '// &
                           'into this file'
      return
   endif
   error = h5_group_open(self%root, self%node, group)
   if (error == 0) then
      error = h5_link_soft('/VTKHDF/'//leaf, group, label)
      e = h5_group_close(group)
   endif
   if (error /= 0) then
      write(stderr, '(A)') 'error: VTKFortran vtm_file: cannot link '//label//' (a name used twice in the block?)'
      return
   endif
   self%children(size(self%children)) = self%children(size(self%children)) + 1_I4P
   endfunction vtkhdf_reference
   function write_level(self, level, filenames) result(error)
   !< Write the level `level` (0, 1, ... in order, each once) of an OverlappingAMR (`initialize(...,
   !< composite='OverlappingAMR')`, a `.vthb`): its boxes, each an ImageData file (`.vti`) written before with the origin
   !< of the AMR, the spacing of the level and its extent in the point indices of the level (`nx1...nz2`). The files are
   !< read: the origin (the same for every box), the spacing (the same in a level, the level's) and the extent (the
   !< `amr_box`, in cells: lo, hi of each axis; a flat axis 0 -1) are taken from them, so the `.vthb` agrees with its data.
   !< A relative file is relative to the directory of the `.vthb`, and written as given.
   !<
   !<```fortran
   !< error = amr%initialize(filename='amr.vthb', composite='OverlappingAMR')
   !< error = amr%write_level(level=0, filenames=['amr/amr_0.vti'])
   !< error = amr%write_level(level=1, filenames=['amr/amr_1.vti', 'amr/amr_2.vti'])
   !< error = amr%finalize()
   !<```
   class(vtm_file), intent(inout) :: self         !< VTM file.
   integer(I4P),    intent(in)    :: level        !< Level, from 0.
   character(*),    intent(in)    :: filenames(:) !< Files of its boxes.
   integer(I4P)                   :: error        !< Error status.
   type(xml_reader)               :: reader       !< A box read.
   character(len=:), allocatable  :: dir          !< Directory of the .vthb.
   character(len=:), allocatable  :: file         !< A file, as given.
   character(len=:), allocatable  :: topology     !< Its dataset type.
   character(len=:), allocatable  :: desc         !< Grid description of the box.
   character(len=:), allocatable  :: files        !< Files of the level, followed by NUL.
   integer(I4P), allocatable      :: boxes(:,:)   !< Boxes of the level.
   integer(I4P)                   :: ext(6)       !< Extent of a box (points).
   real(R8P)                      :: origin(3)    !< Origin of a box.
   real(R8P)                      :: spacing(3)   !< Spacing of a box.
   real(R8P)                      :: level_spacing(3) !< Spacing of the level.
   integer(I4P)                   :: f, a, e      !< Counters, error of a read.

   error = 1
   if (.not.self%is_xml_amr) then
      write(stderr, '(A)') 'error: VTKFortran vtm_file: write_level is for an OverlappingAMR (composite=''OverlappingAMR'')'
      return
   elseif (level /= self%amr_levels) then
      write(stderr, '(A)') 'error: VTKFortran vtm_file: the levels of an OverlappingAMR are 0, 1, ... in order, each once '// &
                           '(the next one is '//trim(str(n=self%amr_levels, no_sign=.true.))//')'
      return
   elseif (size(filenames) == 0) then
      write(stderr, '(A)') 'error: VTKFortran vtm_file: a level of an OverlappingAMR needs its boxes'
      return
   endif
   dir = ''
   if (index(self%amr_filename, '/', back=.true.) > 0) dir = self%amr_filename(:index(self%amr_filename, '/', back=.true.))
   allocate(boxes(7, size(filenames)))
   files = ''
   do f=1, size(filenames)
      file = trim(adjustl(filenames(f)))
      if (len(file) == 0) then
         write(stderr, '(A)') 'error: VTKFortran vtm_file: a box of an OverlappingAMR needs its file'
         return
      endif
      if (file(1:1) == '/') then
         e = reader%initialize(filename=file)
      else
         e = reader%initialize(filename=dir//file)
      endif
      if (e == 0) e = reader%get_info(mesh_topology=topology, nx1=ext(1), nx2=ext(2), ny1=ext(3), ny2=ext(4), nz1=ext(5), &
                                      nz2=ext(6), origin=origin, spacing=spacing)
      call reader%close
      if (e /= 0) then
         write(stderr, '(A,I0,A)') 'error: VTKFortran vtm_file: cannot read the box '//file//' (error ', e, ')'
         return
      elseif (topology /= 'ImageData') then
         write(stderr, '(A)') 'error: VTKFortran vtm_file: the box '//file//' is a '//topology//', not an ImageData'
         return
      endif
      desc = ''
      do a=1, 3
         if (ext(2*a) > ext(2*a-1)) desc = desc//'XYZ'(a:a)
      enddo
      if (self%amr_levels == 0 .and. f == 1) then ! the first box: the origin and grid description of the AMR
         self%amr_origin = origin
         self%amr_desc = desc
      endif
      if (f == 1) level_spacing = spacing
      if (any(origin /= self%amr_origin)) then
         write(stderr, '(A)') 'error: VTKFortran vtm_file: the box '//file//' has another origin than the first box (the '// &
                              'boxes of an OverlappingAMR share the origin; their extents place them)'
         return
      elseif (any(spacing /= level_spacing)) then
         write(stderr, '(A)') 'error: VTKFortran vtm_file: the box '//file//' has another spacing than the first box of '// &
                              'its level'
         return
      elseif (.not.all(spacing > 0._R8P)) then
         write(stderr, '(A)') 'error: VTKFortran vtm_file: the box '//file//' has a spacing not positive'
         return
      elseif (desc /= self%amr_desc) then
         write(stderr, '(A)') 'error: VTKFortran vtm_file: the box '//file//' spans the axes '//desc//', the first box '// &
                              self%amr_desc
         return
      endif
      boxes(:, f) = [level, ext(1), ext(2) - 1, ext(3), ext(4) - 1, ext(5), ext(6) - 1]
      files = files//file//achar(0)
   enddo
   self%amr_spacing = reshape([self%amr_spacing, level_spacing], [3, self%amr_levels + 1])
   self%amr_boxes = reshape([self%amr_boxes, boxes], [7, size(self%amr_boxes, 2) + size(filenames)])
   self%amr_files = self%amr_files//files
   self%amr_levels = self%amr_levels + 1_I4P
   error = 0
   endfunction write_level

   function xml_amr_finalize(self) result(error)
   !< Write the `.vthb` of an OverlappingAMR: a `Block level spacing` for each level, a `DataSet index amr_box file` for each
   !< of its boxes, as vtkXMLUniformGridAMRWriter writes them (version 1.1).
   class(vtm_file), intent(inout) :: self  !< VTM file.
   integer(I4P)                   :: error !< Error status.
   integer(I4P)                   :: e     !< Error of the writer's finalize.
   integer(I4P)                   :: l, b, i, f1, f2 !< Counters, positions.

   error = 1
   if (self%amr_levels == 0) then
      write(stderr, '(A)') 'error: VTKFortran vtm_file: an OverlappingAMR without levels is not written'
      return
   endif
   if (allocated(self%xml_writer)) deallocate(self%xml_writer)
   allocate(xml_writer_ascii_local :: self%xml_writer)
   self%xml_writer%origin = self%amr_origin
   error = self%xml_writer%initialize(format='ascii', filename=self%amr_filename, mesh_topology='vtkOverlappingAMR', &
                                      mesh_kind=self%amr_desc)
   f1 = 1
   b = 0
   do l=0, self%amr_levels - 1
      call self%xml_writer%write_start_tag(name='Block', attributes='level="'//trim(str(n=l, no_sign=.true.))// &
                                           '" spacing="'//real_values(self%amr_spacing(:, l+1))//'"')
      i = 0
      do while (b < size(self%amr_boxes, 2))
         if (self%amr_boxes(1, b+1) /= l) exit
         b = b + 1
         f2 = f1 - 1 + index(self%amr_files(f1:), achar(0))
         call self%xml_writer%write_self_closing_tag(name='DataSet', attributes='index="'// &
            trim(str(n=i, no_sign=.true.))//'" amr_box="'//int_values(self%amr_boxes(2:7, b))//'" file="'// &
            attr_text(self%amr_files(f1:f2-1))//'"')
         f1 = f2 + 1
         i = i + 1
      enddo
      call self%xml_writer%write_end_tag(name='Block')
   enddo
   if (self%xml_writer%error /= 0) error = self%xml_writer%error
   e = self%xml_writer%finalize()
   if (error == 0) error = e
   deallocate(self%xml_writer)
   contains
      function int_values(x) result(values)
      !< Space separated integers.
      integer(I4P), intent(in)      :: x(:)   !< Values.
      character(len=:), allocatable :: values !< Their text.
      integer                       :: k      !< Counter.

      values = trim(str(n=x(1), no_sign=.true.))
      if (x(1) < 0) values = trim(str(n=x(1)))
      do k=2, size(x)
         if (x(k) < 0) then
            values = values//' '//trim(str(n=x(k)))
         else
            values = values//' '//trim(str(n=x(k), no_sign=.true.))
         endif
      enddo
      endfunction int_values
   endfunction xml_amr_finalize

   function xml_amr_entries(self, root, file_type) result(error)
   !< Gather the entries of an XML OverlappingAMR read (`vtkOverlappingAMR`, `.vthb`; or `vtkHierarchicalBoxDataSet`, a
   !< former `.vth`): its `Block`s (the levels) are blocks at level 1 (index the `level`), their `DataSet`s (the boxes)
   !< datasets at level 2 with their files (index the `index`, or `dataset` of a former file).
   class(vtm_file), intent(inout) :: self      !< VTM file.
   integer(I4P),    intent(in)    :: root      !< Index of the VTKFile element.
   character(*),    intent(in)    :: file_type !< vtkOverlappingAMR or vtkHierarchicalBoxDataSet.
   integer(I4P)                   :: error     !< Error status.
   integer(I4P), allocatable      :: blocks(:) !< The Block elements.
   integer(I4P), allocatable      :: kids(:)   !< Children of a block.
   character(len=:), allocatable  :: value     !< An attribute.
   integer(I4P)                   :: top, b, c !< Elements, counters.

   call self%free_entries
   allocate(self%entry_level(0), self%entry_is_block(0), self%entry_index(0), self%entry_text(6, 0), &
            self%entry_is_piece(0))
   self%entry_chars = ''
   error = 2
   top = self%xml%find(name=file_type, parent=root)
   if (top == 0) return
   error = 0
   blocks = self%xml%children(top)
   do b=1, size(blocks)
      if (self%xml%name(blocks(b)) /= 'Block') cycle
      call self%add_entry(1, .true., .false., index_of(self%xml%attribute(id=blocks(b), name='level'), b - 1), '', '')
      kids = self%xml%children(blocks(b))
      do c=1, size(kids)
         if (self%xml%name(kids(c)) /= 'DataSet') cycle
         value = self%xml%attribute(id=kids(c), name='index')
         if (len(value) == 0) value = self%xml%attribute(id=kids(c), name='dataset')
         call self%add_entry(2, .false., .false., index_of(value, c - 1), '', self%xml%attribute(id=kids(c), name='file'))
      enddo
   enddo
   endfunction xml_amr_entries
endmodule vtk_fortran_vtm_file
