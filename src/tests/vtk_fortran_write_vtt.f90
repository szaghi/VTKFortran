!< VTK_Fortran test: XML Table (`.vtt`, `mesh_topology='Table'`), every XML format, read back.
program vtk_fortran_write_vtt
!< VTK_Fortran test: XML Table (`.vtt`, `mesh_topology='Table'`), every XML format, read back.
!<
!< The test writes a Table of 5 rows (columns of every kind: R8P, R4P, I8P, I4P, I2P, I1P, an unsigned one, one of 3
!< components given as an array and one given as 3 arrays) in the ASCII, BINARY, RAW and BINARY-APPENDED formats, and
!< RAW zlib-compressed when the library has zlib (`vtkfortran_vtt_<format>.vtt`); reads each back with
!< `vtk_file%xml_reader` (`location='row'`: names in order, types, components, `NumberOfRows` as the tuples, values,
!< `npieces` 0); writes the same Table as VTKHDF when the library has HDF5 (`vtkfortran_vtt_table.vtkhdf`: VTK readers
!< compare the two outside this test). Then the refusals: columns of different rows, pieces, point and cell data in a
!< Table, the columns opened twice, closed without being opened, location row in an UnstructuredGrid. Last a time series
!< of two Tables listed in a `.pvd` (`vtkfortran_vtt_series.pvd`, read back with `get_datasets`; ParaView plays it).
use penf
use vtk_fortran, only : vtk_file, pvd_file
use vtk_fortran_hdf5, only : is_hdf5_enabled
use vtk_fortran_zlib, only : is_zlib_enabled

implicit none
integer(I4P), parameter :: n = 5                    !< Rows.
character(15), parameter :: FORMATS(5) = [character(15) :: 'ascii', 'binary', 'raw', 'binary-appended', 'raw-zlib']
character(8), parameter :: EXPECTED(9) = [character(8) :: 'time', 'pressure', 'id', 'count', 'level', 'flag', 'hits', &
                                          'velocity', 'force'] !< Columns, in order.
real(R8P)               :: time(n)                  !< Column time.
real(R8P)               :: velocity(3,n)            !< Column velocity (3 components).
type(vtk_file)          :: w                        !< Writer.
logical                 :: ok(8)                    !< Checks.
integer(I4P)            :: f, i, e                  !< Counters, error.

time = [(0.1_R8P * i, i=1, n)]
velocity = reshape([(real(i, R8P), i=1, 3 * n)], [3, n])
ok = .false.
do f=1, size(FORMATS)
  if (trim(FORMATS(f)) == 'raw-zlib' .and. .not.is_zlib_enabled) then
    ok(f) = .true. ! zlib refused without the library built with it, as for every dataset
    print '(A)', 'zlib not enabled: the compressed Table is skipped'
    cycle
  endif
  ok(f) = table(trim(FORMATS(f)), 'vtkfortran_vtt_'//trim(FORMATS(f))//'.vtt') == 0
  if (ok(f)) ok(f) = read_back('vtkfortran_vtt_'//trim(FORMATS(f))//'.vtt')
  if (.not.ok(f)) print '(A)', 'format '//trim(FORMATS(f))//': failed'
enddo
ok(6) = .true.
if (is_hdf5_enabled) ok(6) = table('vtkhdf', 'vtkfortran_vtt_table.vtkhdf') == 0
ok(7) = refusals()
ok(8) = series()
print '(A,8L2)', 'XML Table: ascii, binary, raw, binary-appended, raw zlib, VTKHDF twin, refusals, series:', ok

print "(A,L1)", new_line('a')//'Are all tests passed? ', all(ok)
if (.not.all(ok)) error stop 'some tests failed'
stop
contains
  function table(format, filename) result(error)
  !< Write the Table in a format.
  character(*), intent(in) :: format   !< Format.
  character(*), intent(in) :: filename !< File name.
  integer(I4P)             :: error    !< Error status.
  integer(I4P)             :: ee(13)   !< Errors.

  if (format == 'raw-zlib') then
    ee(1) = w%initialize(format='raw', filename=filename, mesh_topology='Table', compressor='zlib')
  else
    ee(1) = w%initialize(format=format, filename=filename, mesh_topology='Table')
  endif
  ee(2) = w%xml_writer%write_dataarray(location='row', action='open')
  ee(3) = w%xml_writer%write_dataarray(data_name='time', x=time)
  ee(4) = w%xml_writer%write_dataarray(data_name='pressure', x=real(time, R4P) * 2)
  ee(5) = w%xml_writer%write_dataarray(data_name='id', x=[(int(i, I8P) * 3000000000_I8P, i=1, n)])
  ee(6) = w%xml_writer%write_dataarray(data_name='count', x=[(i * 10, i=1, n)])
  ee(7) = w%xml_writer%write_dataarray(data_name='level', x=[(int(-i, I2P), i=1, n)])
  ee(8) = w%xml_writer%write_dataarray(data_name='flag', x=[(int(mod(i, 2), I1P), i=1, n)])
  ee(9) = w%xml_writer%write_dataarray_unsigned(data_name='hits', x=[(i * 7, i=1, n)])
  ee(10) = w%xml_writer%write_dataarray(data_name='velocity', x=velocity)
  ee(11) = w%xml_writer%write_dataarray(data_name='force', x=velocity(1,:), y=velocity(2,:), z=-velocity(3,:))
  ee(12) = w%xml_writer%write_dataarray(location='row', action='close')
  ee(13) = w%finalize()
  error = maxval(abs(ee))
  if (error /= 0) print '(A,13I2)', filename//': errors ', ee
  endfunction table

  logical function read_back(filename)
  !< Read the Table back: its type, no pieces, the names of its columns in order, types, components, rows, values.
  character(*), intent(in)      :: filename !< File name.
  type(vtk_file)                :: r
  real(R8P),    allocatable     :: x(:), v(:,:)
  real(R4P),    allocatable     :: x4(:)
  integer(I8P), allocatable     :: i8(:)
  integer(I4P), allocatable     :: i4(:)
  integer(I2P), allocatable     :: i2(:)
  integer(I1P), allocatable     :: i1(:)
  character(len=:), allocatable :: topology, names(:), dtype
  integer(I4P)                  :: np, nc, ee(14)
  integer(I8P)                  :: nt
  logical                       :: c(12)

  ee = 0
  c = .false.
  ee(1) = r%initialize(filename=filename, action='read')
  ee(2) = r%xml_reader%get_info(mesh_topology=topology, npieces=np)
  c(1) = topology == 'Table' .and. np == 0
  ee(3) = r%xml_reader%get_dataarray_names(location='row', names=names)
  c(2) = size(names) == size(EXPECTED)
  if (c(2)) c(2) = all(names == EXPECTED)
  ee(4) = r%xml_reader%get_dataarray_info(location='row', data_name='velocity', data_type=dtype, n_components=nc, &
                                          n_tuples=nt)
  c(3) = dtype == 'Float64' .and. nc == 3 .and. nt == n
  ee(5) = r%xml_reader%get_dataarray_info(location='row', data_name='hits', data_type=dtype, n_components=nc, n_tuples=nt)
  c(4) = dtype == 'UInt32' .and. nc == 1 .and. nt == n
  ee(6) = r%xml_reader%read_dataarray(location='row', data_name='time', x=x)
  c(5) = all(x == time)
  ee(7) = r%xml_reader%read_dataarray(location='row', data_name='pressure', x=x4)
  c(6) = all(x4 == real(time, R4P) * 2)
  ee(8) = r%xml_reader%read_dataarray(location='row', data_name='id', x=i8)
  c(7) = all(i8 == [(int(i, I8P) * 3000000000_I8P, i=1, n)])
  ee(9) = r%xml_reader%read_dataarray(location='row', data_name='velocity', x=v)
  c(8) = all(v == velocity)
  ee(10) = r%xml_reader%read_dataarray(location='row', data_name='force', x=v)
  c(9) = all(v(1:2,:) == velocity(1:2,:)) .and. all(v(3,:) == -velocity(3,:))
  ee(11) = r%xml_reader%read_dataarray(location='row', data_name='count', x=i4)
  ee(12) = r%xml_reader%read_dataarray(location='row', data_name='level', x=i2)
  ee(13) = r%xml_reader%read_dataarray(location='row', data_name='flag', x=i1)
  c(10) = all(i4 == [(i * 10, i=1, n)]) .and. all(i2 == [(int(-i, I2P), i=1, n)]) .and. &
          all(i1 == [(int(mod(i, 2), I1P), i=1, n)])
  ee(14) = r%xml_reader%read_dataarray(location='row', data_name='hits', x=i8)
  c(11) = all(i8 == [(int(i * 7, I8P), i=1, n)])
  ! no point data, no pieces
  c(12) = r%xml_reader%read_dataarray(location='node', data_name='time', x=x) /= 0
  e = r%finalize()
  read_back = all(c) .and. all(ee == 0)
  if (.not.read_back) print '(A,14I2)', filename//': read errors ', ee
  if (.not.read_back) print '(A,12L2)', filename//': checks ', c
  endfunction read_back

  logical function series()
  !< A time series of two Tables of 2 and 4 rows, one `.vtt` per step listed in a `.pvd`, read back.
  type(pvd_file)                :: pvd
  type(vtk_file)                :: r
  real(R8P),    allocatable     :: steps(:), x(:)
  character(len=:), allocatable :: files(:)
  integer(I4P)                  :: s, ee(9)
  character(27)                 :: filename

  ee = 0
  ee(1) = pvd%initialize(filename='vtkfortran_vtt_series.pvd')
  do s=1, 2
    write(filename, '(A,I0,A)') 'vtkfortran_vtt_series_', s, '.vtt'
    ee(2) = ee(2) + w%initialize(format='binary', filename=trim(filename), mesh_topology='Table')
    ee(3) = ee(3) + w%xml_writer%write_dataarray(location='row', action='open')
    ee(4) = ee(4) + w%xml_writer%write_dataarray(data_name='probe', x=[(real(10 * s + i, R8P), i=1, 2 * s)])
    ee(5) = ee(5) + w%xml_writer%write_dataarray(location='row', action='close')
    ee(6) = ee(6) + w%finalize()
    ee(7) = ee(7) + pvd%write_dataset(filename=trim(filename), timestep=real(s, R8P))
  enddo
  ee(8) = pvd%finalize()
  ee(9) = pvd%initialize(filename='vtkfortran_vtt_series.pvd', action='read')
  e = pvd%get_datasets(timestep=steps, file=files)
  series = all(ee == 0) .and. e == 0 .and. size(files) == 2
  if (series) series = all(steps == [1._R8P, 2._R8P])
  do s=1, 2
    if (.not.series) exit
    e = r%initialize(filename=trim(files(s)), action='read')
    e = e + r%xml_reader%read_dataarray(location='row', data_name='probe', x=x)
    series = series .and. e == 0 .and. size(x) == 2 * s
    if (series) series = all(x == [(real(10 * s + i, R8P), i=1, 2 * s)])
    e = r%finalize()
  enddo
  e = pvd%finalize()
  if (.not.series) print '(A,9I2)', 'series: errors ', ee
  endfunction series

  logical function refusals()
  !< What an XML Table refuses (each a non-zero error).
  logical :: r(6)

  r = .false.
  e = w%initialize(format='ascii', filename='vtkfortran_vtt_refused.vtt', mesh_topology='Table')
  r(1) = w%xml_writer%write_piece(np=1, nc=1) /= 0                          ! pieces
  r(2) = w%xml_writer%write_dataarray(location='node', action='open') /= 0  ! point data
  e = w%xml_writer%write_dataarray(location='cell', action='open')        ! (one call per statement)
  r(2) = r(2) .and. e /= 0
  r(3) = w%xml_writer%write_dataarray(location='row', action='close') /= 0  ! closed before being opened
  e = w%xml_writer%write_dataarray(location='row', action='open')
  e = w%xml_writer%write_dataarray(data_name='a', x=[1._R8P, 2._R8P, 3._R8P])
  r(4) = w%xml_writer%write_dataarray(data_name='b', x=[1._R8P, 2._R8P]) /= 0 ! columns of different rows
  e = w%xml_writer%write_dataarray(location='row', action='close')
  r(5) = w%xml_writer%write_dataarray(location='row', action='open') /= 0   ! the columns once
  e = w%finalize()
  e = w%initialize(format='ascii', filename='vtkfortran_vtt_refused.vtu', mesh_topology='UnstructuredGrid')
  r(6) = w%xml_writer%write_dataarray(location='row', action='open') /= 0   ! columns in an UnstructuredGrid
  e = w%finalize()
  refusals = all(r)
  if (.not.refusals) print '(A,6L2)', 'refusals: ', r
  endfunction refusals
endprogram vtk_fortran_write_vtt
