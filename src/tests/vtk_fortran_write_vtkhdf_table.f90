!< VTK_Fortran test: VTKHDF Table (`mesh_topology='Table'`), static and time series, read back.
program vtk_fortran_write_vtkhdf_table
!< VTK_Fortran test: VTKHDF Table (`mesh_topology='Table'`), static and time series, read back.
!<
!< With HDF5 the test writes a Table of columns of every kind (R8P, R4P, I8P, I4P, I2P, I1P, an unsigned one, a column
!< of 3 components), checks the file with the HDF5 bindings (`Type`, `Version` 2.8, `NumberOfRows`, the shape of the
!< columns) and reads it back through `vtk_file%xml_reader` (`location='row'`: names, types, components, values); then a
!< time series of 3 steps with different rows (`NumberOfRows` and `Steps/RowDataOffsets` checked, each step read back);
!< then the refusals: columns of different rows, a name with `/`, a column missing in a step or new in a later one, a step
!< without columns, a static Table then a step, a Table without columns, pieces and point data in a Table, columns in an
!< UnstructuredGrid, a refused close of the columns (finalize refuses too); field data left open (closed by finalize, the
!< columns ended once), without columns (refused). Without HDF5 the writer refuses a Table; the XML formats refuse it always.
use penf
use vtk_fortran, only : vtk_file
use vtk_fortran_hdf5

implicit none
integer(I4P), parameter :: rows(3) = [2, 3, 1] !< Rows of the steps of the series.
type(vtk_file) :: w       !< Writer.
logical        :: ok(4)   !< Checks.
integer(I4P)   :: e       !< Error.

ok = .false.
! the XML formats have no Table here (VTKHDF only)
ok(1) = w%initialize(format='ascii', filename='table.vtt', mesh_topology='Table') /= 0 .and. &
        w%initialize(format='raw', filename='table.vtt', mesh_topology='Table') /= 0
if (.not.is_hdf5_enabled) then
  e = w%initialize(format='vtkhdf', filename='table.vtkhdf', mesh_topology='Table')
  ok(2:) = e /= 0
  print '(A)', 'HDF5 not enabled: a Table is refused'
else
  ok(2) = static_table()
  ok(3) = series_table()
  ok(4) = refusals()
endif
print '(A,4L2)', 'VTKHDF Table: XML refused, static, series, refusals:', ok

print "(A,L1)", new_line('a')//'Are all tests passed? ', all(ok)
if (.not.all(ok)) error stop 'some tests failed'
stop
contains
  logical function static_table()
  !< A static Table of 5 rows: a column of each kind, an unsigned one, one of 3 components; read back.
  integer(I4P), parameter       :: n = 5
  real(R8P)                     :: time(n), velocity(3,n)
  type(vtk_file)                :: r
  real(R8P),    allocatable     :: x(:), v(:,:)
  real(R4P),    allocatable     :: x4(:)
  integer(I8P), allocatable     :: i8(:), ivalues(:), dims(:)
  integer(I4P), allocatable     :: i4(:)
  integer(I2P), allocatable     :: i2(:)
  integer(I1P), allocatable     :: i1(:)
  character(len=:), allocatable :: topology, names(:), dtype, text
  integer(hid_kind)             :: file_id, root
  integer(I4P)                  :: i, np, nc, ee(30)
  integer(I8P)                  :: nt
  logical                       :: c(0:13)
  character(8), parameter       :: expected(8) = [character(8) :: 'time', 'pressure', 'id', 'count', 'level', 'flag', &
                                                  'hits', 'velocity']

  time = [(0.1_R8P * i, i=1, n)]
  velocity = reshape([(real(i, R8P), i=1, 3 * n)], [3, n])
  ee = 0
  c = .false.
  ee(1) = w%initialize(format='vtkhdf', filename='table.vtkhdf', mesh_topology='Table')
  ee(2) = w%xml_writer%write_dataarray(location='row', action='open')
  ee(3) = w%xml_writer%write_dataarray(data_name='time', x=time)
  ee(4) = w%xml_writer%write_dataarray(data_name='pressure', x=real(time, R4P) * 2)
  ee(5) = w%xml_writer%write_dataarray(data_name='id', x=[(int(i, I8P) * 3000000000_I8P, i=1, n)])
  ee(6) = w%xml_writer%write_dataarray(data_name='count', x=[(i * 10, i=1, n)])
  ee(7) = w%xml_writer%write_dataarray(data_name='level', x=[(int(-i, I2P), i=1, n)])
  ee(8) = w%xml_writer%write_dataarray(data_name='flag', x=[(int(mod(i, 2), I1P), i=1, n)])
  ee(9) = w%xml_writer%write_dataarray_unsigned(data_name='hits', x=[(i * 7, i=1, n)])
  ee(10) = w%xml_writer%write_dataarray(data_name='velocity', x=velocity)
  ee(11) = w%xml_writer%write_dataarray(location='row', action='close')
  ee(12) = w%finalize()
  ! the file, with the bindings
  ee(13) = h5_file_open('table.vtkhdf', file_id)
  ee(14) = h5_group_open(file_id, 'VTKHDF', root)
  ee(15) = h5_attribute_read(root, 'Type', text)
  c(0) = text == 'Table'
  ee(16) = h5_attribute_read(root, 'Version', ivalues)
  c(1) = all(ivalues == [2, 8])
  ee(17) = h5_dataset_read(root, 'NumberOfRows', ivalues)
  c(2) = all(ivalues == [n])
  ee(18) = h5_dataset_info(root, 'RowData/velocity', dtype, dims)
  c(3) = all(dims == [3, n]) .and. dtype == 'Float64'
  ee(19) = h5_dataset_info(root, 'RowData/hits', dtype, dims)
  c(4) = all(dims == [n]) .and. dtype == 'UInt32'
  ee(20) = h5_group_close(root)
  ee(21) = h5_file_close(file_id)
  ! read back
  ee(22) = r%initialize(filename='table.vtkhdf', action='read')
  ee(23) = r%xml_reader%get_info(mesh_topology=topology, npieces=np)
  c(5) = topology == 'Table' .and. np == 0
  ee(24) = r%xml_reader%get_dataarray_names(location='row', names=names)
  c(6) = size(names) == 8 ! in the order written
  if (c(6)) c(6) = all(names == expected)
  ee(25) = r%xml_reader%get_dataarray_info(location='row', data_name='velocity', data_type=dtype, n_components=nc, n_tuples=nt)
  c(7) = dtype == 'Float64' .and. nc == 3 .and. nt == n
  ee(26) = r%xml_reader%read_dataarray(location='row', data_name='time', x=x)
  c(8) = all(x == time)
  ee(27) = r%xml_reader%read_dataarray(location='row', data_name='pressure', x=x4)
  c(9) = all(x4 == real(time, R4P) * 2)
  ee(28) = r%xml_reader%read_dataarray(location='row', data_name='id', x=i8)
  c(10) = all(i8 == [(int(i, I8P) * 3000000000_I8P, i=1, n)])
  ee(29) = r%xml_reader%read_dataarray(location='row', data_name='velocity', x=v)
  c(11) = all(v == velocity)
  ee(30) = r%xml_reader%read_dataarray(location='row', data_name='count', x=i4) + &
           r%xml_reader%read_dataarray(location='row', data_name='level', x=i2) + &
           r%xml_reader%read_dataarray(location='row', data_name='flag', x=i1)
  c(12) = all(i4 == [(i * 10, i=1, n)]) .and. all(i2 == [(int(-i, I2P), i=1, n)]) .and. &
          all(i1 == [(int(mod(i, 2), I1P), i=1, n)])
  ! no point data in a Table, no pieces
  c(13) = r%xml_reader%read_dataarray(location='node', data_name='time', x=x) /= 0 .and. &
          r%xml_reader%read_piece(piece=1, np=nt) /= 0
  e = r%finalize()
  static_table = all(c) .and. all(ee == 0)
  if (.not.static_table) print '(A,30I2)', 'static Table: errors ', ee
  if (.not.static_table) print '(A,14L2)', 'static Table: checks ', c
  endfunction static_table

  logical function series_table()
  !< A time series of 3 steps of 2, 3 and 1 rows: a column of 2 components and one integer, each step read back.
  type(vtk_file)                :: r
  real(R8P),    allocatable     :: v(:,:), times(:)
  integer(I4P), allocatable     :: i4(:)
  integer(I8P), allocatable     :: ivalues(:)
  integer(hid_kind)             :: file_id
  integer(I4P)                  :: s, i, ee(12)

  ee = 0
  series_table = .true.
  ee(1) = w%initialize(format='vtkhdf', filename='table_series.vtkhdf', mesh_topology='Table')
  do s=1, 3
    ee(2) = ee(2) + w%xml_writer%write_step(time=0.5_R8P * s)
    ee(3) = ee(3) + w%xml_writer%write_dataarray(location='row', action='open')
    ee(4) = ee(4) + w%xml_writer%write_dataarray(data_name='sample', x=[(s * 100 + i, i=1, rows(s))])
    ee(5) = ee(5) + w%xml_writer%write_dataarray(data_name='value', x=values(s))
    ee(6) = ee(6) + w%xml_writer%write_dataarray(location='row', action='close')
  enddo
  ee(7) = w%finalize()
  ee(8) = h5_file_open('table_series.vtkhdf', file_id)
  ee(9) = h5_dataset_read(file_id, 'VTKHDF/NumberOfRows', ivalues)
  series_table = series_table .and. all(ivalues == rows)
  ee(10) = h5_dataset_read(file_id, 'VTKHDF/Steps/RowDataOffsets/value', ivalues)
  series_table = series_table .and. all(ivalues == [0, 2, 5])
  ee(11) = h5_dataset_read(file_id, 'VTKHDF/Steps/RowDataOffsets/sample', ivalues)
  series_table = series_table .and. all(ivalues == [0, 2, 5])
  e = h5_file_close(file_id)
  ee(12) = r%initialize(filename='table_series.vtkhdf', action='read')
  e = r%xml_reader%get_steps(times=times)
  series_table = series_table .and. e == 0 .and. all(times == [0.5_R8P, 1._R8P, 1.5_R8P])
  do s=1, 3
    e = r%xml_reader%set_step(s)
    e = e + r%xml_reader%read_dataarray(location='row', data_name='value', x=v)
    e = e + r%xml_reader%read_dataarray(location='row', data_name='sample', x=i4)
    series_table = series_table .and. e == 0 .and. all(shape(v) == [2, rows(s)]) .and. all(i4 == [(s * 100 + i, i=1, rows(s))])
    if (e == 0 .and. all(shape(v) == [2, rows(s)])) series_table = series_table .and. all(v == values(s))
  enddo
  e = r%finalize()
  series_table = series_table .and. all(ee == 0)
  if (.not.series_table) print '(A,12I2)', 'Table series: errors ', ee
  endfunction series_table

  function values(s) result(v)
  !< The column of 2 components of the step s.
  integer(I4P), intent(in) :: s
  real(R8P), allocatable   :: v(:,:)
  integer(I4P)             :: i

  allocate(v(2, rows(s)))
  do i=1, size(v, 2)
    v(:,i) = [real(s, R8P), 0.25_R8P * i]
  enddo
  endfunction values

  logical function refusals()
  !< What a Table refuses (each a non-zero error).
  logical :: r(13)
  integer(hid_kind)         :: file_id !< A file read back.
  integer(I8P), allocatable :: dims(:) !< The shape of its NumberOfRows.

  r = .false.
  ! columns of different rows
  e = w%initialize(format='vtkhdf', filename='table_refused.vtkhdf', mesh_topology='Table')
  e = w%xml_writer%write_dataarray(location='row', action='open')
  e = w%xml_writer%write_dataarray(data_name='a', x=[1._R8P, 2._R8P, 3._R8P])
  r(1) = w%xml_writer%write_dataarray(data_name='b', x=[1._R8P, 2._R8P]) /= 0
  ! a name with /
  r(2) = w%xml_writer%write_dataarray(data_name='c/d', x=[1._R8P, 2._R8P, 3._R8P]) /= 0
  e = w%xml_writer%write_dataarray(location='row', action='close')
  ! the columns written once, then no step
  ! (one call per statement: the order of the calls in an expression is the processor's)
  r(3) = w%xml_writer%write_dataarray(location='row', action='open') /= 0
  e = w%xml_writer%write_step(time=0._R8P)
  r(3) = r(3) .and. e /= 0
  ! pieces and point data in a Table
  r(4) = w%xml_writer%write_piece(np=1, nc=1) /= 0
  e = w%xml_writer%write_dataarray(location='node', action='open')
  r(4) = r(4) .and. e /= 0
  e = w%finalize()
  ! a column missing in a step, a new one in a later step, a step without columns
  e = w%initialize(format='vtkhdf', filename='table_refused.vtkhdf', mesh_topology='Table')
  e = w%xml_writer%write_step(time=0._R8P)
  e = w%xml_writer%write_dataarray(location='row', action='open')
  e = w%xml_writer%write_dataarray(data_name='a', x=[1._R8P, 2._R8P])
  e = w%xml_writer%write_dataarray(data_name='b', x=[1_I4P, 2_I4P])
  e = w%xml_writer%write_dataarray(location='row', action='close')
  e = w%xml_writer%write_step(time=1._R8P)
  e = w%xml_writer%write_dataarray(location='row', action='open')
  e = w%xml_writer%write_dataarray(data_name='a', x=[3._R8P])
  r(5) = w%xml_writer%write_dataarray(data_name='c', x=[3._R8P]) /= 0
  r(6) = w%xml_writer%write_dataarray(location='row', action='close') /= 0 ! b missing
  r(7) = w%xml_writer%write_step(time=2._R8P) == 0
  e = w%xml_writer%write_step(time=3._R8P)
  r(7) = r(7) .and. e /= 0
  r(8) = w%finalize() /= 0 ! the last step has no columns
  ! a Table without columns
  e = w%initialize(format='vtkhdf', filename='table_refused.vtkhdf', mesh_topology='Table')
  r(9) = w%finalize() /= 0
  ! columns in an UnstructuredGrid
  e = w%initialize(format='vtkhdf', filename='table_refused.vtkhdf', mesh_topology='UnstructuredGrid')
  r(10) = w%xml_writer%write_dataarray(location='row', action='open') /= 0
  e = w%finalize()
  ! field data left open: finalize closes it, the columns ended once (one NumberOfRows entry)
  e = w%initialize(format='vtkhdf', filename='table_field.vtkhdf', mesh_topology='Table')
  e = w%xml_writer%write_dataarray(location='row', action='open')
  e = w%xml_writer%write_dataarray(data_name='a', x=[1._R8P, 2._R8P, 3._R8P])
  e = w%xml_writer%write_dataarray(location='row', action='close')
  e = w%xml_writer%write_fielddata(action='open')
  e = w%finalize()
  r(11) = e == 0 .and. h5_file_open('table_field.vtkhdf', file_id) == 0
  if (r(11)) then
    r(11) = h5_dataset_dims(file_id, 'VTKHDF/NumberOfRows', dims) == 0
    if (r(11)) r(11) = all(dims == [1])
    e = h5_file_close(file_id)
  endif
  ! no columns, field data left open: refused
  e = w%initialize(format='vtkhdf', filename='table_refused.vtkhdf', mesh_topology='Table')
  e = w%xml_writer%write_fielddata(action='open')
  e = w%finalize()
  r(12) = e /= 0
  ! the close of the columns refused (a column written twice): finalize refuses too (the file is not complete)
  e = w%initialize(format='vtkhdf', filename='table_refused.vtkhdf', mesh_topology='Table')
  e = w%xml_writer%write_dataarray(location='row', action='open')
  e = w%xml_writer%write_dataarray(data_name='a', x=[1._R8P, 2._R8P])
  e = w%xml_writer%write_dataarray(data_name='a', x=[3._R8P, 4._R8P])
  e = w%xml_writer%write_dataarray(location='row', action='close')
  e = w%finalize()
  r(13) = e /= 0
  refusals = all(r)
  if (.not.refusals) print '(A,13L2)', 'Table refusals: ', r
  endfunction refusals
endprogram vtk_fortran_write_vtkhdf_table
