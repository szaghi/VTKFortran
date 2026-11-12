!< VTK_Fortran test: VTKHDF OverlappingAMR (`mesh_topology='OverlappingAMR'`), static and time series, read back.
program vtk_fortran_write_vtkhdf_amr
!< VTK_Fortran test: VTKHDF OverlappingAMR (`mesh_topology='OverlappingAMR'`), static and time series, read back.
!<
!< With HDF5 the test writes an OverlappingAMR of two levels: level 0 one box of 4x3x2 cells (spacing 1), level 1 two boxes
!< of 2x2x2 cells (spacing 0.5), with point data (R8P, and a vector) and cell data (I4P); checks the file with the HDF5
!< bindings (`Type`, `Origin`, the `Spacing` and `AMRBox` of each level) and reads it back through `vtk_file%xml_reader`
!< (`get_levels`, `set_level`, the boxes as pieces: their extents and arrays); then a time series of 2 steps, level 1 with
!< two boxes then one (the `Steps/Level<n>` entries checked, each step read back); then the refusals: a box outside the
!< boxes of level 0, a box of a finer level before those of level 0, a flat box, an array of the wrong size, an array
!< missing in a box or new in a later box, levels out of order, another spacing for a level, field data, a step without
!< boxes, a box left open, `write_level` in another topology, `action='append'`. Without HDF5 the writer refuses an OverlappingAMR; the XML
!< formats refuse it always.
use penf
use vtk_fortran, only : vtk_file
use vtk_fortran_hdf5

implicit none
type(vtk_file) :: w     !< Writer.
logical        :: ok(4) !< Checks.
integer(I4P)   :: e     !< Error.
integer(I4P), parameter :: boxes(6,3) = reshape([0, 4, 0, 3, 0, 2, & ! level 0
                                                 0, 2, 0, 2, 0, 2, & ! level 1, box 1
                                                 4, 6, 2, 4, 0, 2], [6, 3]) !< Point extents of the boxes.

ok = .false.
! the XML formats have no OverlappingAMR here (VTKHDF only)
ok(1) = w%initialize(format='ascii', filename='amr.vth', mesh_topology='OverlappingAMR', origin=[0._R8P, 0._R8P, 0._R8P]) /= 0
if (.not.is_hdf5_enabled) then
  e = w%initialize(format='vtkhdf', filename='amr.vtkhdf', mesh_topology='OverlappingAMR')
  ok(2:) = e /= 0
  print '(A)', 'HDF5 not enabled: an OverlappingAMR is refused'
else
  ok(2) = static_amr()
  ok(3) = series_amr()
  ok(4) = refusals()
endif
print '(A,4L2)', 'VTKHDF OverlappingAMR: XML refused, static, series, refusals:', ok

print "(A,L1)", new_line('a')//'Are all tests passed? ', all(ok)
if (.not.all(ok)) error stop 'some tests failed'
stop
contains
  function write_boxes(s, n_boxes) result(error)
  !< The levels of the step s (0 for a static file): level 0 its box, level 1 its first n_boxes boxes.
  integer(I4P), intent(in) :: s       !< Step.
  integer(I4P), intent(in) :: n_boxes !< Boxes of level 1.
  integer(I4P)             :: error   !< Error status.
  integer(I4P)             :: b       !< Counter.

  error = abs(w%xml_writer%write_level(0, [1._R8P, 1._R8P, 1._R8P]))
  error = error + box(s, 0, 1, boxes(:, 1))
  error = error + abs(w%xml_writer%write_level(1, [0.5_R8P, 0.5_R8P, 0.5_R8P]))
  do b=1, n_boxes
    error = error + box(s, 1, b, boxes(:, b + 1))
  enddo
  endfunction write_boxes

  function box(s, l, b, ext) result(error)
  !< A box: point data pd (and the vector v of pd, -pd, 2 pd), cell data cd, the values coding step, level and box.
  integer(I4P), intent(in) :: s, l, b, ext(6) !< Step, level, box, point extent.
  integer(I4P)             :: error           !< Error status.

  error = abs(w%xml_writer%write_piece(nx1=ext(1), nx2=ext(2), ny1=ext(3), ny2=ext(4), nz1=ext(5), nz2=ext(6)))
  error = error + abs(w%xml_writer%write_dataarray(location='node', action='open'))
  error = error + abs(w%xml_writer%write_dataarray(data_name='pd', x=point_values(s, l, b, ext)))
  error = error + abs(w%xml_writer%write_dataarray(data_name='v', x=point_values(s, l, b, ext), &
                                                   y=-point_values(s, l, b, ext), z=2 * point_values(s, l, b, ext)))
  error = error + abs(w%xml_writer%write_dataarray(location='node', action='close'))
  error = error + abs(w%xml_writer%write_dataarray(location='cell', action='open'))
  error = error + abs(w%xml_writer%write_dataarray(data_name='cd', x=cell_values(s, l, b, ext)))
  error = error + abs(w%xml_writer%write_dataarray(location='cell', action='close'))
  error = error + abs(w%xml_writer%write_piece())
  endfunction box

  pure function point_values(s, l, b, ext) result(x)
  !< The point data of a box, x fastest.
  integer(I4P), intent(in) :: s, l, b, ext(6) !< Step, level, box, point extent.
  real(R8P), allocatable   :: x(:)            !< Values.
  integer(I4P)             :: i               !< Counter.

  x = [(real(10000 * s + 1000 * l + 100 * b + i, R8P), i=0, product(ext(2::2) - ext(1::2) + 1) - 1)]
  endfunction point_values

  pure function cell_values(s, l, b, ext) result(x)
  !< The cell data of a box, x fastest.
  integer(I4P), intent(in)  :: s, l, b, ext(6) !< Step, level, box, point extent.
  integer(I4P), allocatable :: x(:)            !< Values.
  integer(I4P)              :: i               !< Counter.

  x = [(10000 * s + 1000 * l + 100 * b + i, i=0, product(ext(2::2) - ext(1::2)) - 1)]
  endfunction cell_values

  logical function read_level(r, s, l, n_boxes)
  !< The level l of the step read: its boxes (pieces), their extents and arrays.
  type(vtk_file), intent(inout) :: r       !< Reader.
  integer(I4P),   intent(in)    :: s, l    !< Step (of the values), level.
  integer(I4P),   intent(in)    :: n_boxes !< Boxes of the level.
  real(R8P),    allocatable     :: x(:), v(:,:)
  integer(I4P), allocatable     :: c(:)
  integer(I4P)                  :: np, b, ext(6), ee
  integer(I8P)                  :: n8
  real(R8P)                     :: spacing(3), origin(3)

  ee = r%xml_reader%set_level(l)
  ee = ee + r%xml_reader%get_info(npieces=np, spacing=spacing, origin=origin)
  read_level = ee == 0 .and. np == n_boxes .and. all(spacing == 1._R8P / (l + 1)) .and. all(origin == [1._R8P, -1._R8P, 0._R8P])
  do b=1, n_boxes
    ee = r%xml_reader%read_piece(piece=b, nx1=ext(1), nx2=ext(2), ny1=ext(3), ny2=ext(4), nz1=ext(5), nz2=ext(6))
    ee = ee + r%xml_reader%read_dataarray(location='node', data_name='pd', x=x, piece=b)
    ee = ee + r%xml_reader%read_dataarray(location='node', data_name='v', x=v, piece=b)
    ee = ee + r%xml_reader%read_dataarray(location='cell', data_name='cd', x=c, piece=b)
    read_level = read_level .and. ee == 0 .and. all(ext == boxes(:, l + b))
    if (read_level) read_level = all(x == point_values(s, l, b, ext)) .and. all(c == cell_values(s, l, b, ext)) .and. &
                                 all(shape(v) == [3, size(x)])
    if (read_level) read_level = all(v(1,:) == x) .and. all(v(2,:) == -x) .and. all(v(3,:) == 2 * x)
  enddo
  ee = r%xml_reader%read_piece(piece=n_boxes + 1, np=n8)
  read_level = read_level .and. ee /= 0 ! no more boxes
  endfunction read_level

  logical function static_amr()
  !< The static OverlappingAMR, checked with the bindings and read back.
  type(vtk_file)                :: r
  integer(hid_kind)             :: file_id, root
  integer(I8P),     allocatable :: ivalues(:)
  real(R8P),        allocatable :: rvalues(:)
  character(len=:), allocatable :: text, topology
  integer(I4P)                  :: ee(12), n_levels
  logical                       :: c(5)

  ee = 0 ; c = .false.
  ee(1) = w%initialize(format='vtkhdf', filename='amr.vtkhdf', mesh_topology='OverlappingAMR', &
                       origin=[1._R8P, -1._R8P, 0._R8P])
  ee(2) = write_boxes(0, 2)
  ee(3) = w%finalize()
  ee(4) = h5_file_open('amr.vtkhdf', file_id)
  ee(5) = h5_group_open(file_id, 'VTKHDF', root)
  ee(6) = h5_attribute_read(root, 'Type', text)
  ee(7) = h5_attribute_read(root, 'Origin', rvalues)
  c(1) = text == 'OverlappingAMR' .and. all(rvalues == [1._R8P, -1._R8P, 0._R8P])
  ee(8) = h5_attribute_read(root, 'Version', ivalues)
  c(1) = c(1) .and. all(ivalues == [2, 0])
  ee(9) = h5_dataset_read(root, 'Level1/AMRBox', ivalues)
  c(2) = all(ivalues == [0, 1, 0, 1, 0, 1, 4, 5, 2, 3, 0, 1]) ! the cells of each box
  ee(10) = h5_dataset_read(root, 'Level0/AMRBox', ivalues)
  c(2) = c(2) .and. all(ivalues == [0, 3, 0, 2, 0, 1])
  e = h5_group_close(root)
  e = h5_file_close(file_id)
  ee(11) = r%initialize(filename='amr.vtkhdf', action='read')
  ee(12) = r%xml_reader%get_info(mesh_topology=topology) + r%xml_reader%get_levels(n_levels)
  c(3) = topology == 'OverlappingAMR' .and. n_levels == 2
  c(4) = read_level(r, 0, 0, 1)
  c(5) = read_level(r, 0, 1, 2) .and. r%xml_reader%set_level(2) /= 0
  e = r%finalize()
  static_amr = all(c) .and. all(ee == 0)
  if (.not.static_amr) print '(A,12I3,A,5L2)', 'static AMR: errors ', ee, ' checks ', c
  endfunction static_amr

  logical function series_amr()
  !< A time series of 2 steps, level 1 with two boxes then one; each step read back.
  type(vtk_file)            :: r
  integer(hid_kind)         :: file_id
  integer(I8P), allocatable :: ivalues(:)
  real(R8P),    allocatable :: times(:)
  integer(I4P)              :: ee(8), s
  logical                   :: c(4)

  ee = 0 ; c = .false.
  ee(1) = w%initialize(format='vtkhdf', filename='amr_series.vtkhdf', mesh_topology='OverlappingAMR', &
                       origin=[1._R8P, -1._R8P, 0._R8P])
  do s=1, 2
    ee(2) = ee(2) + abs(w%xml_writer%write_step(time=0.5_R8P * s))
    ee(3) = ee(3) + write_boxes(s, 3 - s)
  enddo
  ee(4) = w%finalize()
  ee(5) = h5_file_open('amr_series.vtkhdf', file_id)
  ee(6) = h5_dataset_read(file_id, 'VTKHDF/Steps/Level1/NumberOfAMRBox', ivalues)
  c(1) = all(ivalues == [2, 1])
  ee(7) = h5_dataset_read(file_id, 'VTKHDF/Steps/Level1/AMRBoxOffset', ivalues)
  c(1) = c(1) .and. all(ivalues == [0, 2])
  ee(8) = h5_dataset_read(file_id, 'VTKHDF/Steps/Level1/PointDataOffset/pd', ivalues)
  c(2) = all(ivalues == [0, 54])
  e = h5_dataset_read(file_id, 'VTKHDF/Steps/Level0/CellDataOffset/cd', ivalues)
  c(2) = c(2) .and. all(ivalues == [0, 24])
  e = h5_file_close(file_id)
  e = r%initialize(filename='amr_series.vtkhdf', action='read')
  e = r%xml_reader%get_steps(times=times)
  c(3) = e == 0 .and. all(times == [0.5_R8P, 1._R8P])
  c(4) = .true.
  do s=1, 2
    e = r%xml_reader%set_step(s)
    c(4) = c(4) .and. e == 0 .and. read_level(r, s, 0, 1) .and. read_level(r, s, 1, 3 - s)
  enddo
  e = r%finalize()
  series_amr = all(c) .and. all(ee == 0)
  if (.not.series_amr) print '(A,8I3,A,4L2)', 'AMR series: errors ', ee, ' checks ', c
  endfunction series_amr

  logical function refusals()
  !< What an OverlappingAMR refuses (each a non-zero error). One call per statement: the order of the calls of an
  !< expression is the processor's.
  logical :: r(15)

  r = .false.
  e = w%initialize(format='vtkhdf', filename='amr_refused.vtkhdf', mesh_topology='OverlappingAMR')
  e = w%xml_writer%write_level(1, [0.5_R8P, 0.5_R8P, 0.5_R8P])
  r(1) = e /= 0                                                                            ! level 1 before level 0
  e = w%xml_writer%write_level(0, [1._R8P, 1._R8P, 0._R8P])
  r(2) = e /= 0                                                                            ! a zero spacing
  e = w%xml_writer%write_level(0, [1._R8P, 1._R8P, 1._R8P])
  e = w%xml_writer%write_piece(nx1=0, nx2=4, ny1=0, ny2=3, nz1=0, nz2=0)
  r(3) = e /= 0                                                                            ! a flat box
  e = w%xml_writer%write_piece(nx1=0, nx2=4, ny1=0, ny2=3, nz1=0, nz2=2)
  e = w%xml_writer%write_dataarray(location='node', action='open')
  e = w%xml_writer%write_dataarray(data_name='pd', x=[1._R8P, 2._R8P])
  r(4) = e /= 0                                                                            ! the wrong size
  e = w%xml_writer%write_dataarray(data_name='pd', x=point_values(0, 0, 1, boxes(:, 1)))
  e = w%xml_writer%write_dataarray(location='node', action='close')
  e = w%xml_writer%write_piece()
  e = w%xml_writer%write_piece(nx1=0, nx2=1, ny1=0, ny2=1, nz1=0, nz2=1)
  e = w%xml_writer%write_dataarray(location='node', action='open')
  e = w%xml_writer%write_dataarray(data_name='q', x=spread(1._R8P, 1, 8))
  r(5) = e /= 0                                                                            ! an array new in box 2
  e = w%xml_writer%write_dataarray(location='node', action='close')
  e = w%xml_writer%write_piece()
  r(6) = e /= 0                                                                            ! pd missing in box 2
  e = w%xml_writer%write_level(1, [0.5_R8P, 0.5_R8P, 0.5_R8P])
  e = w%xml_writer%write_piece(nx1=6, nx2=10, ny1=0, ny2=2, nz1=0, nz2=2)
  r(7) = e /= 0                                                                            ! outside level 0's boxes
  e = w%xml_writer%write_level(0, [2._R8P, 2._R8P, 2._R8P])
  r(8) = e /= 0                                                                            ! another spacing
  e = w%xml_writer%write_level(3, [0.1_R8P, 0.1_R8P, 0.1_R8P])
  r(9) = e /= 0                                                                            ! a level skipped
  e = w%xml_writer%write_fielddata(action='open')
  r(10) = e /= 0                                                                           ! field data
  e = w%finalize()
  ! a time series: a finer box before level 0, a step without boxes, a box left open
  e = w%initialize(format='vtkhdf', filename='amr_refused.vtkhdf', mesh_topology='OverlappingAMR')
  e = w%xml_writer%write_step(time=0._R8P)
  e = write_boxes(1, 2)
  e = w%xml_writer%write_step(time=1._R8P)
  e = w%xml_writer%write_level(0, [1._R8P, 1._R8P, 1._R8P])
  e = w%xml_writer%write_level(1, [0.5_R8P, 0.5_R8P, 0.5_R8P])
  e = w%xml_writer%write_piece(nx1=0, nx2=2, ny1=0, ny2=2, nz1=0, nz2=2)
  r(11) = e /= 0                                                                           ! no box of level 0 yet
  e = w%xml_writer%write_step(time=2._R8P)
  r(12) = e /= 0                                                                           ! step 2 without boxes
  e = w%xml_writer%write_level(0, [1._R8P, 1._R8P, 1._R8P])
  e = w%xml_writer%write_piece(nx1=0, nx2=4, ny1=0, ny2=3, nz1=0, nz2=2)
  e = w%finalize()
  r(13) = e /= 0                                                                           ! a box left open
  e = w%initialize(format='vtkhdf', filename='amr_refused.vtkhdf', mesh_topology='UnstructuredGrid')
  e = w%xml_writer%write_level(0, [1._R8P, 1._R8P, 1._R8P])
  r(14) = e /= 0                                                                           ! not an AMR
  e = w%finalize()
  e = w%initialize(format='vtkhdf', filename='amr_append.vtkhdf', mesh_topology='OverlappingAMR')
  e = w%xml_writer%write_step(time=0._R8P)
  e = write_boxes(1, 1)
  e = w%finalize()
  e = w%initialize(format='vtkhdf', filename='amr_append.vtkhdf', action='append')
  r(15) = e /= 0                                                                           ! not continued
  e = w%finalize()
  refusals = all(r)
  if (.not.refusals) print '(A,15L2)', 'AMR refusals: ', r
  endfunction refusals
endprogram vtk_fortran_write_vtkhdf_amr
