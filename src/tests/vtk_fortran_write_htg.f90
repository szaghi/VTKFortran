!< VTK_Fortran test: XML HyperTreeGrid (`.htg`).
program vtk_fortran_write_htg
!< VTK_Fortran test: XML HyperTreeGrid (`.htg`).
!<
!< The HyperTreeGrid of the VTKHDF test (a 2x2 root grid, branch factor 2: tree 0 of depth 3, tree 1 of depth 2, tree 3 a
!< masked leaf, tree 2 not written; cell data cd and depth, and 3-component Normals and Intercepts named by
!< `set_interface`) written in the four XML formats (ascii, binary, raw, binary-appended), as VTK 9.7.2 writes a `.htg`:
!< the HyperTreeGrid element, its Grid, its Trees (Descriptors and Mask as Bit arrays, NumberOfVerticesPerDepth,
!< TreeIds, DepthPerTree) and its CellData. The ASCII file is checked line by line (the element, the trees); VTK 9.7.2
!< reads every file outside this test (the same grid as the VTKHDF one). The refusals: point and field data, a second
!< piece, a piece that is not the root grid, coordinates of other sizes or after the trees, a tree outside the root grid
!< or written twice or after the cell data, a cell array of other cells, set_interface after the piece, a branch factor
!< of 4, write_tree in another topology. The four files read back (`get_info`, `read_piece`, `read_geo`, `get_trees` with
!< the interface names, `read_tree` of every tree, the cell data), and a `.htg` written by VTK 9.7.2 (embedded).
use penf
use vtk_fortran, only : vtk_file

implicit none
character(15), parameter :: FORMATS(4) = [character(15) :: 'ascii', 'binary', 'raw', 'binary-appended'] !< XML formats.
logical                  :: ok(5) !< Checks.
integer(I4P)             :: f     !< Counter.

ok(1) = .true.
do f=1, size(FORMATS)
  ok(1) = ok(1) .and. write_htg('vtkfortran_write_htg_'//trim(FORMATS(f))//'.htg', trim(FORMATS(f))) == 0
enddo
ok(2) = has_text('vtkfortran_write_htg_ascii.htg', &
                 [character(160) :: '<HyperTreeGrid BranchFactor="2" TransposedRootIndexing="0" Dimensions="3 3 1" '// &
                  'InterfaceNormalsName="Normals" InterfaceInterceptsName="Intercepts">', &
                  '<DataArray type="Bit" NumberOfTuples="6" Name="Descriptors" format="ascii">', &
                  '<DataArray type="Int64" NumberOfTuples="6" Name="NumberOfVerticesPerDepth" format="ascii">', &
                  '<DataArray type="Int64" NumberOfTuples="3" Name="TreeIds" format="ascii">', &
                  '<DataArray type="UInt32" NumberOfTuples="3" Name="DepthPerTree" format="ascii">', &
                  '<DataArray type="Bit" NumberOfTuples="15" Name="Mask" format="ascii">'])
ok(3) = refusals()
ok(4) = .true.
do f=1, size(FORMATS)
  ok(4) = ok(4) .and. read_htg('vtkfortran_write_htg_'//trim(FORMATS(f))//'.htg')
enddo
ok(5) = read_vtk_htg()
print '(A,5L2)', 'XML HyperTreeGrid: four formats, the ASCII text, refusals, read back, the .htg of VTK:', ok

print "(A,L1)", new_line('a')//'Are all tests passed? ', all(ok)
if (.not.all(ok)) error stop 'some tests failed'
stop
contains
  function write_htg(filename, format) result(error)
  !< The HyperTreeGrid of the test in one format.
  character(*), intent(in) :: filename !< File name.
  character(*), intent(in) :: format   !< XML format.
  integer(I4P)             :: error    !< Error status.
  logical, parameter       :: t=.true., f=.false.
  type(vtk_file)           :: w        !< Writer.
  real(R8P)                :: normals(3, 15) !< Interface normals.
  integer(I4P)             :: i        !< Counter.

  normals = 0._R8P ; normals(1,:) = 1._R8P
  error = w%initialize(format=format, filename=filename, mesh_topology='HyperTreeGrid', &
                       nx1=0, nx2=2, ny1=0, ny2=2, nz1=0, nz2=0, branch_factor=2)
  error = error + abs(w%xml_writer%set_interface(normals='Normals', intercepts='Intercepts'))
  error = error + abs(w%xml_writer%write_piece(nx1=0, nx2=2, ny1=0, ny2=2, nz1=0, nz2=0))
  error = error + abs(w%xml_writer%write_geo(x=[0._R8P, 1._R8P, 2._R8P], y=[0._R8P, 1._R8P, 2._R8P], z=[0._R8P]))
  ! one call per statement: the order of the calls of an expression is the processor's
  error = error + abs(w%xml_writer%write_tree(tree_id=0, descriptor=[t, t, f, f, f]))           ! depths of 1, 4, 4 cells
  error = error + abs(w%xml_writer%write_tree(tree_id=1, descriptor=[t]))                       ! depths of 1, 4 cells
  error = error + abs(w%xml_writer%write_tree(tree_id=3, descriptor=[logical ::], mask=[t]))   ! a masked leaf
  error = error + abs(w%xml_writer%write_dataarray(location='cell', action='open'))
  error = error + abs(w%xml_writer%write_dataarray(data_name='cd', x=[(real(i, R8P), i=0, 14)]))
  error = error + abs(w%xml_writer%write_dataarray(data_name='depth', x=[0, 1, 1, 1, 1, 2, 2, 2, 2, 0, 1, 1, 1, 1, 0]))
  error = error + abs(w%xml_writer%write_dataarray(data_name='Normals', x=normals))
  error = error + abs(w%xml_writer%write_dataarray(data_name='Intercepts', x=normals * 0.5_R8P))
  error = error + abs(w%xml_writer%write_dataarray(location='cell', action='close'))
  error = error + abs(w%xml_writer%write_piece())
  error = error + abs(w%finalize())
  if (error /= 0) print '(A,I0)', 'failed: '//filename//', error ', error
  endfunction write_htg

  logical function read_htg(filename)
  !< Read back a HyperTreeGrid of the test: every tree, the cell data, the root grid, the interface.
  character(*), intent(in)      :: filename !< File name.
  logical, parameter            :: t=.true., f=.false.
  type(vtk_file)                :: r        !< Reader.
  character(len=:), allocatable :: topology, normals, intercepts, names(:) !< Strings read.
  integer(I8P), allocatable     :: ids(:)   !< Tree ids.
  logical, allocatable          :: d(:), m(:) !< A tree.
  real(R8P), allocatable        :: x(:), y(:), z(:), cd(:), nv(:,:) !< Arrays read.
  integer(I4P), allocatable     :: depth(:) !< Cell data.
  integer(I8P)                  :: np, nc   !< Counts.
  integer(I4P)                  :: e(12), np_, bf, ext(6), i !< Errors, counts.

  e = 0
  e(1) = r%initialize(filename=filename, action='read')
  e(2) = r%xml_reader%get_info(mesh_topology=topology, npieces=np_, nx1=ext(1), nx2=ext(2), ny1=ext(3), ny2=ext(4), &
                               nz1=ext(5), nz2=ext(6))
  e(3) = r%xml_reader%read_piece(np=np, nc=nc)
  e(4) = r%xml_reader%read_geo(x=x, y=y, z=z)
  e(5) = r%xml_reader%get_trees(tree_ids=ids, branch_factor=bf, interface_normals=normals, interface_intercepts=intercepts)
  e(6) = r%xml_reader%read_tree(tree_id=0_I8P, descriptor=d, mask=m)
  if (e(6) == 0) e(6) = merge(0, 1, all(d .eqv. [t, t, f, f, f]) .and. size(d) == 5 .and. .not.any(m) .and. size(m) == 9)
  e(7) = r%xml_reader%read_tree(tree_id=1_I8P, descriptor=d, mask=m)
  if (e(7) == 0) e(7) = merge(0, 1, all(d .eqv. [t]) .and. size(d) == 1 .and. .not.any(m) .and. size(m) == 5)
  e(8) = r%xml_reader%read_tree(tree_id=3_I8P, descriptor=d, mask=m)
  if (e(8) == 0) e(8) = merge(0, 1, size(d) == 0 .and. all(m) .and. size(m) == 1)
  e(9) = merge(0, 1, r%xml_reader%read_tree(tree_id=2_I8P, descriptor=d) == 4)  ! a root cell without tree
  e(10) = r%xml_reader%read_dataarray(location='cell', data_name='cd', x=cd)
  e(10) = e(10) + r%xml_reader%read_dataarray(location='cell', data_name='depth', x=depth)
  e(11) = r%xml_reader%read_dataarray(location='cell', data_name='Normals', x=nv)
  e(12) = r%xml_reader%get_dataarray_names(location='cell', names=names)
  read_htg = all(e == 0)
  if (read_htg) read_htg = topology == 'HyperTreeGrid' .and. np_ == 1 .and. all(ext == [0, 2, 0, 2, 0, 0]) .and. &
                           np == 9_I8P .and. nc == 15_I8P .and. all(x == [0._R8P, 1._R8P, 2._R8P]) .and. size(z) == 1 .and. &
                           all(ids == [0_I8P, 1_I8P, 3_I8P]) .and. bf == 2 .and. normals == 'Normals' .and. &
                           intercepts == 'Intercepts' .and. size(names) == 4
  if (read_htg) read_htg = all(cd == [(real(i, R8P), i=0, 14)]) .and. &
                           all(depth == [0, 1, 1, 1, 1, 2, 2, 2, 2, 0, 1, 1, 1, 1, 0]) .and. size(nv, 1) == 3 .and. &
                           size(nv, 2) == 15 .and. all(nv(1,:) == 1._R8P)
  e(1) = r%finalize()
  if (.not.read_htg) print '(A,12(1X,I0))', 'failed: reading '//filename, e
  endfunction read_htg

  logical function read_vtk_htg()
  !< A `.htg` written by VTK 9.7.2 (vtkXMLHyperTreeGridWriter, ascii; vtkHyperTreeGridSource of 2x1 root cells, the first
  !< one refined once, a cell array v): read back.
  type(vtk_file)                :: r        !< Reader.
  integer(I8P), allocatable     :: ids(:)   !< Tree ids.
  logical, allocatable          :: d(:), m(:) !< A tree.
  real(R8P), allocatable        :: v(:)     !< Cell data.
  character(len=:), allocatable :: normals  !< Interface name.
  integer(I4P)                  :: u, e(5), bf, ext(6) !< Unit, errors, branch factor, extent.

  open(newunit=u, file='vtkfortran_write_htg_vtk.htg', status='replace', action='write')
  write(u, '(A)') '<?xml version="1.0"?>', &
    '<VTKFile type="HyperTreeGrid" version="2.0" byte_order="LittleEndian" header_type="UInt32" '// &
    'compressor="vtkZLibDataCompressor">', &
    '  <HyperTreeGrid BranchFactor="2" TransposedRootIndexing="0" Dimensions="3 2 1">', '    <Grid>', &
    '      <DataArray type="Float64" Name="XCoordinates" NumberOfTuples="3" format="ascii" RangeMin="0" RangeMax="2">', &
    '        0 1 2', '      </DataArray>', &
    '      <DataArray type="Float64" Name="YCoordinates" NumberOfTuples="2" format="ascii" RangeMin="0" RangeMax="1">', &
    '        0 1', '      </DataArray>', &
    '      <DataArray type="Float64" Name="ZCoordinates" NumberOfTuples="1" format="ascii" RangeMin="0" RangeMax="0">', &
    '        0', '      </DataArray>', '    </Grid>', '    <Trees>', &
    '        <DataArray type="Bit" Name="Descriptors" NumberOfTuples="1" format="ascii" RangeMin="1" RangeMax="1">', &
    '          1', '        </DataArray>', &
    '        <DataArray type="Int64" Name="NumberOfVerticesPerDepth" NumberOfTuples="3" format="ascii" RangeMin="1" '// &
    'RangeMax="4">', '          1 4 1', '        </DataArray>', &
    '        <DataArray type="Int64" Name="TreeIds" NumberOfTuples="2" format="ascii" RangeMin="0" RangeMax="1">', &
    '          0 1', '        </DataArray>', &
    '        <DataArray type="UInt32" Name="DepthPerTree" NumberOfTuples="2" format="ascii" RangeMin="1" RangeMax="2">', &
    '          2 1', '        </DataArray>', &
    '        <DataArray type="Bit" Name="Mask" NumberOfTuples="6" format="ascii" RangeMin="0" RangeMax="0">', &
    '          0 0 0 0 0 0', '        </DataArray>', '    </Trees>', '    <CellData>', &
    '        <DataArray type="Float64" Name="Depth" NumberOfTuples="6" format="ascii" RangeMin="0" RangeMax="1">', &
    '          0 1 1 1 1 0', '        </DataArray>', &
    '        <DataArray type="Float64" Name="v" NumberOfTuples="6" format="ascii" RangeMin="0" RangeMax="50">', &
    '          0 20 30 40 50 10', '        </DataArray>', '    </CellData>', '  </HyperTreeGrid>', '</VTKFile>'
  close(u)
  e = 0
  e(1) = r%initialize(filename='vtkfortran_write_htg_vtk.htg', action='read')
  e(2) = r%xml_reader%get_info(nx1=ext(1), nx2=ext(2), ny1=ext(3), ny2=ext(4), nz1=ext(5), nz2=ext(6))
  e(3) = r%xml_reader%get_trees(tree_ids=ids, branch_factor=bf, interface_normals=normals)
  e(4) = r%xml_reader%read_tree(tree_id=1_I8P, descriptor=d, mask=m)
  if (e(4) == 0) e(4) = merge(0, 1, size(d) == 0 .and. size(m) == 1 .and. .not.any(m))
  e(4) = e(4) + r%xml_reader%read_tree(tree_id=0_I8P, descriptor=d, mask=m)
  if (e(4) == 0) e(4) = merge(0, 1, size(d) == 1 .and. size(m) == 5 .and. d(1))
  e(5) = r%xml_reader%read_dataarray(location='cell', data_name='v', x=v)
  read_vtk_htg = all(e == 0)
  if (read_vtk_htg) read_vtk_htg = all(ext == [0, 2, 0, 1, 0, 0]) .and. all(ids == [0_I8P, 1_I8P]) .and. bf == 2 .and. &
                                   len(normals) == 0 .and. all(v == [0._R8P, 20._R8P, 30._R8P, 40._R8P, 50._R8P, 10._R8P])
  e(1) = r%finalize()
  if (.not.read_vtk_htg) print '(A,5(1X,I0))', 'failed: reading the .htg of VTK', e
  endfunction read_vtk_htg

  logical function refusals()
  !< The refusals of an XML HyperTreeGrid, each one a call returning an error.
  logical, parameter :: t=.true.
  type(vtk_file)     :: w     !< Writer.
  logical            :: c(14) !< Each refusal.

  c = .false.
  c(1) = w%initialize(format='ascii', filename='vtkfortran_write_htg_refused.htg', mesh_topology='HyperTreeGrid', &
                      nx1=0, nx2=2, ny1=0, ny2=2, nz1=0, nz2=0) == 0
  c(2) = w%xml_writer%write_piece(nx1=0, nx2=1, ny1=0, ny2=2, nz1=0, nz2=0) /= 0                ! not the root grid
  c(3) = w%xml_writer%write_tree(tree_id=0, descriptor=[t]) /= 0                                ! before the piece
  c(4) = w%xml_writer%write_piece(nx1=0, nx2=2, ny1=0, ny2=2, nz1=0, nz2=0) == 0
  c(5) = w%xml_writer%set_interface(normals='n', intercepts='i') /= 0                           ! after the piece
  c(6) = w%xml_writer%write_piece(nx1=0, nx2=2, ny1=0, ny2=2, nz1=0, nz2=0) /= 0                ! a second piece
  c(7) = w%xml_writer%write_geo(x=[0._R8P, 1._R8P], y=[0._R8P, 1._R8P, 2._R8P], z=[0._R8P]) /= 0 ! other sizes
  c(8) = w%xml_writer%write_tree(tree_id=4, descriptor=[t]) /= 0                                ! outside the grid
  c(9) = w%xml_writer%write_tree(tree_id=0, descriptor=[t]) == 0
  c(10) = w%xml_writer%write_tree(tree_id=0, descriptor=[t]) /= 0                               ! twice
  c(11) = w%xml_writer%write_dataarray(location='node', action='open') /= 0                     ! point data
  c(12) = w%xml_writer%write_dataarray(location='cell', action='open') == 0
  c(12) = c(12) .and. w%xml_writer%write_dataarray(data_name='cd', x=[1._R8P, 2._R8P]) /= 0     ! 2 values, 5 cells
  c(13) = w%xml_writer%write_tree(tree_id=1, descriptor=[t]) /= 0                               ! after the cell data
  c(13) = c(13) .and. w%xml_writer%write_fielddata(action='open') /= 0                          ! field data
  if (w%finalize() /= 0) continue
  c(14) = w%initialize(format='ascii', filename='vtkfortran_write_htg_refused.htg', mesh_topology='HyperTreeGrid', &
                       nx1=0, nx2=2, ny1=0, ny2=2, nz1=0, nz2=0, branch_factor=4) == 0
  c(14) = c(14) .and. w%xml_writer%write_piece(nx1=0, nx2=2, ny1=0, ny2=2, nz1=0, nz2=0) /= 0    ! branch factor 4
  if (w%finalize() /= 0) continue
  if (c(14)) then
    c(14) = w%initialize(format='ascii', filename='vtkfortran_write_htg_refused.vti', mesh_topology='ImageData', &
                         nx1=0, nx2=1, ny1=0, ny2=1, nz1=0, nz2=0, origin=[0._R8P, 0._R8P, 0._R8P], &
                         spacing=[1._R8P, 1._R8P, 1._R8P]) == 0
    c(14) = c(14) .and. w%xml_writer%write_tree(tree_id=0, descriptor=[t]) /= 0                 ! not a HyperTreeGrid
    if (w%finalize() /= 0) continue
  endif
  refusals = all(c)
  if (.not.refusals) print '(A,14L2)', 'refusals:', c
  endfunction refusals

  logical function has_text(filename, lines)
  !< The file holds each of the lines (leading blanks apart).
  character(*), intent(in) :: filename !< File name.
  character(*), intent(in) :: lines(:) !< Lines expected.
  character(len=1024)      :: line     !< A line.
  logical                  :: found(size(lines)) !< Each line found.
  integer(I4P)             :: u, iostat, k       !< Unit, IO status, counter.

  found = .false.
  open(newunit=u, file=filename, action='read')
  do
    read(u, '(A)', iostat=iostat) line
    if (iostat /= 0) exit
    do k=1, size(lines)
      if (trim(adjustl(line)) == trim(lines(k))) found(k) = .true.
    enddo
  enddo
  close(u)
  has_text = all(found)
  if (.not.has_text) print '(A,6L2)', 'lines of '//filename//' found:', found
  endfunction has_text
endprogram vtk_fortran_write_htg
