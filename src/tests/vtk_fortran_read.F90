!< VTK_Fortran test: read VTK XML files (vtk_file, action='read').
program vtk_fortran_read
!< VTK_Fortran test: read VTK XML files (vtk_file, action='read').
!<
!< Small files are written and read back, and the values compared exactly: an unstructured grid (geometry, cells, point,
!< cell and field data, strings) in every format, with UInt32 and UInt64 headers and zlib compressed when available; an
!< image data; a polydata with 64-bit ids; a rectilinear grid of two pieces; unsigned arrays. Two files written by VTK
!< 9.7 (embedded below; one zlib compressed) and an ASCII file with values not separated are read too. The errors of
!< missing files, arrays and of kinds that cannot hold the values are checked.
use penf
use vtk_fortran, only : vtk_file

implicit none
character(*), parameter :: formats(4)=['ascii          ', 'binary         ', 'raw            ', 'binary-appended'] !< Formats.
real(R8P),    parameter :: x(4)=[0._R8P, 1._R8P, 0._R8P, 0._R8P] !< X coordinates of the tetrahedron.
real(R8P),    parameter :: y(4)=[0._R8P, 0._R8P, 1._R8P, 0._R8P] !< Y coordinates of the tetrahedron.
real(R8P),    parameter :: z(4)=[0._R8P, 0._R8P, 0._R8P, 1._R8P] !< Z coordinates of the tetrahedron.
real(R8P),    parameter :: p(4)=[1.5_R8P, -2.25_R8P, 1.e-300_R8P, 4._R8P/3._R8P] !< Point data.
real(R4P),    parameter :: v(3,4)=reshape([1,2,3, 4,5,6, 7,8,9, 10,11,12], [3,4]) * 0.1_R4P !< Point vectors.
type(vtk_file)          :: a_vtk_file      !< A VTK file.
integer(I4P)            :: f               !< Counter.
logical                 :: test_passed(10) !< List of passed tests.

test_passed = .true.
do f=1, size(formats)
  test_passed(1) = test_passed(1) .and. check_vtu(format=trim(formats(f)), header_type='UInt32', compressor='none')
enddo
test_passed(2) = check_vtu(format='binary', header_type='UInt64', compressor='none') .and. &
                 check_vtu(format='raw', header_type='UInt64', compressor='none')
#ifdef VTKFORTRAN_USE_ZLIB
do f=2, size(formats)
  test_passed(3) = test_passed(3) .and. check_vtu(format=trim(formats(f)), header_type='UInt32', compressor='zlib') .and. &
                                        check_vtu(format=trim(formats(f)), header_type='UInt64', compressor='zlib')
enddo
#endif
test_passed(4) = check_vti()
test_passed(5) = check_vtp()
test_passed(6) = check_vtr_pieces()
test_passed(7) = check_unsigned()
test_passed(8) = check_errors()
test_passed(9) = check_vtk_written()
test_passed(10) = check_ascii_not_separated()

print "(A,L1)", new_line('a')//'Are all tests passed? ', all(test_passed)
if (.not.all(test_passed)) error stop 'some tests failed'
stop
contains
  function check_vtu(format, header_type, compressor) result(is_passed)
  !< Write an unstructured grid (a tetrahedron) and read it back.
  character(*), intent(in)      :: format         !< File format.
  character(*), intent(in)      :: header_type    !< Header type.
  character(*), intent(in)      :: compressor     !< Compressor.
  logical                       :: is_passed      !< Check result.
  character(len=:), allocatable :: filename       !< File name.
  character(len=:), allocatable :: topology       !< Mesh topology.
  character(len=:), allocatable :: names(:)       !< Names of dataarrays.
  character(len=:), allocatable :: strings(:)     !< Strings.
  character(len=:), allocatable :: data_type      !< VTK type.
  real(R8P),    allocatable     :: xr(:)          !< X coordinates read.
  real(R8P),    allocatable     :: yr(:)          !< Y coordinates read.
  real(R8P),    allocatable     :: zr(:)          !< Z coordinates read.
  real(R8P),    allocatable     :: pr(:)          !< Point data read.
  real(R4P),    allocatable     :: vr(:,:)        !< Point vectors read.
  integer(I4P), allocatable     :: connect4(:)    !< Connectivity read (I4P).
  integer(I4P), allocatable     :: offset4(:)     !< Offsets read (I4P).
  integer(I8P), allocatable     :: connect8(:)    !< Connectivity read (I8P).
  integer(I8P), allocatable     :: offset8(:)     !< Offsets read (I8P).
  integer(I1P), allocatable     :: cell_type(:)   !< Cell types read.
  integer(I8P), allocatable     :: cell_id(:)     !< Cell data read, widened to I8P.
  real(R8P),    allocatable     :: time(:)        !< Field data read.
  integer(I8P), allocatable     :: cycle(:)       !< Field data read.
  integer(I4P), allocatable     :: wrong(:)       !< Array of a kind that cannot hold the data.
  integer(I8P)                  :: np             !< Number of points.
  integer(I8P)                  :: nc             !< Number of cells.
  integer(I8P)                  :: n_tuples       !< Number of tuples.
  integer(I4P)                  :: n_components   !< Number of components.
  integer(I4P)                  :: npieces        !< Number of pieces.
  integer(I4P)                  :: e(16)          !< Errors.

  filename = 'vtkfortran_read-'//format//'-'//header_type//'-'//compressor//'.vtu'
  e = 0
  e(1) = a_vtk_file%initialize(format=format, filename=filename, mesh_topology='UnstructuredGrid', &
                               header_type=header_type, compressor=compressor)
  e(1) = e(1) + a_vtk_file%xml_writer%write_fielddata(action='open')
  e(1) = e(1) + a_vtk_file%xml_writer%write_fielddata(data_name='TIME', x=0.5_R8P)
  e(1) = e(1) + a_vtk_file%xml_writer%write_fielddata(data_name='CYCLE', x=7_I8P)
  e(1) = e(1) + a_vtk_file%xml_writer%write_fielddata(data_name='species', x=['N2 ', 'O2 '])
  e(1) = e(1) + a_vtk_file%xml_writer%write_fielddata(action='close')
  e(1) = e(1) + a_vtk_file%xml_writer%write_piece(np=4, nc=1)
  e(1) = e(1) + a_vtk_file%xml_writer%write_geo(np=4, nc=1, x=x, y=y, z=z)
  e(1) = e(1) + a_vtk_file%xml_writer%write_connectivity(nc=1, connectivity=[0_I4P, 1_I4P, 2_I4P, 3_I4P], offset=[4_I4P], &
                                                         cell_type=[10_I1P])
  e(1) = e(1) + a_vtk_file%xml_writer%write_dataarray(location='node', action='open')
  e(1) = e(1) + a_vtk_file%xml_writer%write_dataarray(data_name='p', x=p)
  e(1) = e(1) + a_vtk_file%xml_writer%write_dataarray(data_name='v', x=v(1,:), y=v(2,:), z=v(3,:))
  e(1) = e(1) + a_vtk_file%xml_writer%write_dataarray(location='node', action='close')
  e(1) = e(1) + a_vtk_file%xml_writer%write_dataarray(location='cell', action='open')
  e(1) = e(1) + a_vtk_file%xml_writer%write_dataarray(data_name='id', x=[7_I4P])
  e(1) = e(1) + a_vtk_file%xml_writer%write_dataarray(location='cell', action='close')
  e(1) = e(1) + a_vtk_file%xml_writer%write_piece()
  e(1) = e(1) + a_vtk_file%finalize()

  e(2) = a_vtk_file%initialize(filename=filename, action='read')
  e(3) = a_vtk_file%xml_reader%get_info(mesh_topology=topology, npieces=npieces)
  e(4) = a_vtk_file%xml_reader%read_piece(np=np, nc=nc)
  e(5) = a_vtk_file%xml_reader%read_geo(x=xr, y=yr, z=zr)
  e(6) = a_vtk_file%xml_reader%read_connectivity(connectivity=connect4, offset=offset4, cell_type=cell_type)
  e(7) = a_vtk_file%xml_reader%read_connectivity(connectivity=connect8, offset=offset8)
  e(8) = a_vtk_file%xml_reader%get_dataarray_names(location='node', names=names)
  e(9) = a_vtk_file%xml_reader%read_dataarray(location='node', data_name='p', x=pr)
  e(10) = a_vtk_file%xml_reader%read_dataarray(location='node', data_name='v', x=vr)
  e(11) = a_vtk_file%xml_reader%get_dataarray_info(location='node', data_name='v', data_type=data_type, &
                                                   n_components=n_components, n_tuples=n_tuples)
  e(12) = a_vtk_file%xml_reader%read_dataarray(location='cell', data_name='id', x=cell_id)
  e(13) = a_vtk_file%xml_reader%read_dataarray(location='field', data_name='TIME', x=time)
  e(14) = a_vtk_file%xml_reader%read_dataarray(location='field', data_name='CYCLE', x=cycle)
  e(15) = a_vtk_file%xml_reader%read_dataarray(location='field', data_name='species', x=strings)
  ! a Float64 array does not fit I4P
  e(16) = merge(0, 1, a_vtk_file%xml_reader%read_dataarray(location='node', data_name='p', x=wrong) == 5)
  e(1) = e(1) + a_vtk_file%finalize()

  is_passed = all(e == 0)
  if (.not.is_passed) return
  is_passed = topology == 'UnstructuredGrid' .and. npieces == 1 .and. np == 4_I8P .and. nc == 1_I8P .and. &
              all(xr == x) .and. all(yr == y) .and. all(zr == z) .and.                                    &
              all(connect4 == [0, 1, 2, 3]) .and. all(offset4 == [4]) .and. all(cell_type == [10_I1P]) .and. &
              all(connect8 == [0, 1, 2, 3]) .and. all(offset8 == [4]) .and.                                 &
              size(names) == 2 .and. names(1) == 'p' .and. names(2) == 'v' .and.                            &
              all(pr == p) .and. all(shape(vr) == [3, 4]) .and. all(vr == v) .and.                          &
              data_type == 'Float32' .and. n_components == 3 .and. n_tuples == 4_I8P .and.                  &
              all(cell_id == [7_I8P]) .and. all(time == [0.5_R8P]) .and. all(cycle == [7_I8P]) .and.        &
              size(strings) == 2 .and. strings(1) == 'N2' .and. strings(2) == 'O2'
  if (.not.is_passed) print '(A)', 'failed: '//filename
  endfunction check_vtu

  function check_vti() result(is_passed)
  !< Write an image data and read back its extent, origin, spacing, direction and data.
  logical                   :: is_passed    !< Check result.
  real(R8P), allocatable    :: phi(:)       !< Point data read.
  integer(I4P)              :: ext(6)       !< Whole extent read.
  integer(I4P)              :: pext(6)      !< Piece extent read.
  real(R8P)                 :: origin(3)    !< Origin read.
  real(R8P)                 :: spacing(3)   !< Spacing read.
  real(R8P)                 :: direction(9) !< Direction read.
  integer(I8P)              :: np           !< Number of points.
  integer(I8P)              :: nc           !< Number of cells.
  integer(I4P)              :: e(6)         !< Errors.

  e = 0
  e(1) = a_vtk_file%initialize(format='raw', filename='vtkfortran_read.vti', mesh_topology='ImageData', &
                               nx1=0, nx2=2, ny1=0, ny2=1, nz1=0, nz2=0,                            &
                               origin=[1._R8P, 2._R8P, 3._R8P], spacing=[0.5_R8P, 0.25_R8P, 1._R8P])
  e(1) = e(1) + a_vtk_file%xml_writer%write_piece(nx1=0, nx2=2, ny1=0, ny2=1, nz1=0, nz2=0)
  e(1) = e(1) + a_vtk_file%xml_writer%write_dataarray(location='node', action='open')
  e(1) = e(1) + a_vtk_file%xml_writer%write_dataarray(data_name='phi', x=[0._R8P, 1._R8P, 2._R8P, 3._R8P, 4._R8P, 5._R8P])
  e(1) = e(1) + a_vtk_file%xml_writer%write_dataarray(location='node', action='close')
  e(1) = e(1) + a_vtk_file%xml_writer%write_piece()
  e(1) = e(1) + a_vtk_file%finalize()
  e(2) = a_vtk_file%initialize(filename='vtkfortran_read.vti', action='read')
  e(3) = a_vtk_file%xml_reader%get_info(nx1=ext(1), nx2=ext(2), ny1=ext(3), ny2=ext(4), nz1=ext(5), nz2=ext(6), &
                                        origin=origin, spacing=spacing, direction=direction)
  e(4) = a_vtk_file%xml_reader%read_piece(np=np, nc=nc, nx1=pext(1), nx2=pext(2), ny1=pext(3), ny2=pext(4), &
                                          nz1=pext(5), nz2=pext(6))
  e(5) = a_vtk_file%xml_reader%read_dataarray(location='node', data_name='phi', x=phi)
  e(6) = a_vtk_file%finalize()
  is_passed = all(e == 0) .and. all(ext == [0, 2, 0, 1, 0, 0]) .and. all(pext == ext) .and. np == 6_I8P .and. &
              nc == 2_I8P .and. all(origin == [1._R8P, 2._R8P, 3._R8P]) .and. all(spacing == [0.5_R8P, 0.25_R8P, 1._R8P]) &
              .and. all(direction == [1._R8P, 0._R8P, 0._R8P, 0._R8P, 1._R8P, 0._R8P, 0._R8P, 0._R8P, 1._R8P])
  if (is_passed) is_passed = all(phi == [0._R8P, 1._R8P, 2._R8P, 3._R8P, 4._R8P, 5._R8P])
  if (.not.is_passed) print '(A)', 'failed: vtkfortran_read.vti'
  endfunction check_vti

  function check_vtp() result(is_passed)
  !< Write a polydata with I8P ids (a polyline and a triangle) and read back its cells.
  logical                   :: is_passed    !< Check result.
  integer(I8P), allocatable :: connect8(:)  !< Connectivity read (I8P).
  integer(I8P), allocatable :: offset8(:)   !< Offsets read (I8P).
  integer(I4P), allocatable :: connect4(:)  !< Connectivity read (I4P).
  integer(I4P), allocatable :: offset4(:)   !< Offsets read (I4P).
  integer(I8P), allocatable :: vconnect(:)  !< Connectivity of an empty block.
  integer(I8P), allocatable :: voffset(:)   !< Offsets of an empty block.
  integer(I8P)              :: counts(5)    !< Number of points, verts, lines, strips, polys.
  integer(I4P)              :: e(6)         !< Errors.

  e = 0
  e(1) = a_vtk_file%initialize(format='binary', filename='vtkfortran_read.vtp', mesh_topology='PolyData')
  e(1) = e(1) + a_vtk_file%xml_writer%write_piece(np=6_I8P, nverts=0_I8P, nlines=1_I8P, nstrips=0_I8P, npolys=1_I8P)
  e(1) = e(1) + a_vtk_file%xml_writer%write_geo(np=6_I8P, nc=2_I8P, x=[0._R8P, 1._R8P, 2._R8P, 0._R8P, 1._R8P, 0._R8P], &
                                                y=[0._R8P, 0._R8P, 0._R8P, 1._R8P, 1._R8P, 2._R8P], z=[0._R8P, 0._R8P, &
                                                0._R8P, 0._R8P, 0._R8P, 0._R8P])
  e(1) = e(1) + a_vtk_file%xml_writer%write_polydata_cells(lines_connectivity=[0_I8P, 1_I8P, 2_I8P], lines_offset=[3_I8P], &
                                                           polys_connectivity=[3_I8P, 4_I8P, 5_I8P], polys_offset=[3_I8P])
  e(1) = e(1) + a_vtk_file%xml_writer%write_piece()
  e(1) = e(1) + a_vtk_file%finalize()
  e(2) = a_vtk_file%initialize(filename='vtkfortran_read.vtp', action='read')
  e(3) = a_vtk_file%xml_reader%read_piece(np=counts(1), nverts=counts(2), nlines=counts(3), nstrips=counts(4), &
                                          npolys=counts(5))
  e(4) = a_vtk_file%xml_reader%read_polydata_cells(block='lines', connectivity=connect8, offset=offset8)
  e(5) = a_vtk_file%xml_reader%read_polydata_cells(block='polys', connectivity=connect4, offset=offset4)
  e(6) = a_vtk_file%xml_reader%read_polydata_cells(block='verts', connectivity=vconnect, offset=voffset)
  e(1) = e(1) + a_vtk_file%finalize()
  is_passed = all(e == 0) .and. all(counts == [6, 0, 1, 0, 1]) .and. all(connect8 == [0, 1, 2]) .and. &
              all(offset8 == [3]) .and. all(connect4 == [3, 4, 5]) .and. all(offset4 == [3]) .and.    &
              size(vconnect) == 0 .and. size(voffset) == 0
  if (.not.is_passed) print '(A)', 'failed: vtkfortran_read.vtp'
  endfunction check_vtp

  function check_vtr_pieces() result(is_passed)
  !< Write a rectilinear grid of two pieces and read back the coordinates and the data of the second one.
  logical                   :: is_passed !< Check result.
  real(R8P),    allocatable :: xr(:)     !< X coordinates read.
  real(R8P),    allocatable :: yr(:)     !< Y coordinates read.
  real(R8P),    allocatable :: zr(:)     !< Z coordinates read.
  real(R4P),    allocatable :: x4(:)     !< X coordinates read (R4P).
  real(R4P),    allocatable :: y4(:)     !< Y coordinates read (R4P).
  real(R4P),    allocatable :: z4(:)     !< Z coordinates read (R4P).
  integer(I4P), allocatable :: cp(:)     !< Cell data read.
  integer(I4P)              :: npieces   !< Number of pieces.
  integer(I4P)              :: nx1       !< Initial node of x axis of the piece.
  integer(I4P)              :: nx2       !< Final node of x axis of the piece.
  integer(I4P)              :: k         !< Counter.
  integer(I4P)              :: e(7)      !< Errors.

  e = 0
  e(1) = a_vtk_file%initialize(format='ascii', filename='vtkfortran_read.vtr', mesh_topology='RectilinearGrid', &
                               nx1=0, nx2=4, ny1=0, ny2=1, nz1=0, nz2=1)
  do k=1, 2
    e(1) = e(1) + a_vtk_file%xml_writer%write_piece(nx1=2*(k-1), nx2=2*k, ny1=0, ny2=1, nz1=0, nz2=1)
    e(1) = e(1) + a_vtk_file%xml_writer%write_geo(x=[real(2*(k-1), R4P), real(2*k-1, R4P), real(2*k, R4P)], &
                                                  y=[0._R4P, 1._R4P], z=[0._R4P, 1._R4P])
    e(1) = e(1) + a_vtk_file%xml_writer%write_dataarray(location='cell', action='open')
    e(1) = e(1) + a_vtk_file%xml_writer%write_dataarray(data_name='cell_piece', x=[k, k])
    e(1) = e(1) + a_vtk_file%xml_writer%write_dataarray(location='cell', action='close')
    e(1) = e(1) + a_vtk_file%xml_writer%write_piece()
  enddo
  e(1) = e(1) + a_vtk_file%finalize()
  e(2) = a_vtk_file%initialize(filename='vtkfortran_read.vtr', action='read')
  e(3) = a_vtk_file%xml_reader%get_info(npieces=npieces)
  e(4) = a_vtk_file%xml_reader%read_piece(piece=2, nx1=nx1, nx2=nx2)
  e(5) = a_vtk_file%xml_reader%read_geo(x=x4, y=y4, z=z4, piece=2)
  e(6) = a_vtk_file%xml_reader%read_dataarray(location='cell', data_name='cell_piece', x=cp, piece=2)
  ! Float32 coordinates widened to R8P
  e(7) = a_vtk_file%xml_reader%read_geo(x=xr, y=yr, z=zr, piece=2)
  e(1) = e(1) + a_vtk_file%finalize()
  is_passed = all(e == 0) .and. npieces == 2 .and. nx1 == 2 .and. nx2 == 4 .and. all(x4 == [2._R4P, 3._R4P, 4._R4P]) .and. &
              all(y4 == [0._R4P, 1._R4P]) .and. all(z4 == [0._R4P, 1._R4P]) .and. all(cp == [2, 2]) .and.              &
              all(xr == [2._R8P, 3._R8P, 4._R8P])
  if (.not.is_passed) print '(A)', 'failed: vtkfortran_read.vtr'
  endfunction check_vtr_pieces

  function check_unsigned() result(is_passed)
  !< Write an UInt8 array and read it back into I1P (same bits) and into I2P (values).
  logical                   :: is_passed !< Check result.
  integer(I1P), allocatable :: u1(:)     !< Values read into I1P.
  integer(I2P), allocatable :: u2(:)     !< Values read into I2P.
  integer(I4P)              :: e(4)      !< Errors.

  e = 0
  e(1) = a_vtk_file%initialize(format='raw', filename='vtkfortran_read-unsigned.vtu', mesh_topology='UnstructuredGrid')
  e(1) = e(1) + a_vtk_file%xml_writer%write_piece(np=4, nc=1)
  e(1) = e(1) + a_vtk_file%xml_writer%write_geo(np=4, nc=1, x=x, y=y, z=z)
  e(1) = e(1) + a_vtk_file%xml_writer%write_connectivity(nc=1, connectivity=[0_I4P, 1_I4P, 2_I4P, 3_I4P], offset=[4_I4P], &
                                                         cell_type=[10_I1P])
  e(1) = e(1) + a_vtk_file%xml_writer%write_dataarray(location='node', action='open')
  e(1) = e(1) + a_vtk_file%xml_writer%write_dataarray_unsigned(data_name='u8', x=[0_I1P, 1_I1P, -56_I1P, -1_I1P])
  e(1) = e(1) + a_vtk_file%xml_writer%write_dataarray(location='node', action='close')
  e(1) = e(1) + a_vtk_file%xml_writer%write_piece()
  e(1) = e(1) + a_vtk_file%finalize()
  e(2) = a_vtk_file%initialize(filename='vtkfortran_read-unsigned.vtu', action='read')
  e(3) = a_vtk_file%xml_reader%read_dataarray(location='node', data_name='u8', x=u1)
  e(4) = a_vtk_file%xml_reader%read_dataarray(location='node', data_name='u8', x=u2)
  e(1) = e(1) + a_vtk_file%finalize()
  is_passed = all(e == 0) .and. all(u1 == [0_I1P, 1_I1P, -56_I1P, -1_I1P]) .and. all(u2 == [0_I2P, 1_I2P, 200_I2P, 255_I2P])
  if (.not.is_passed) print '(A)', 'failed: vtkfortran_read-unsigned.vtu'
  endfunction check_unsigned

  function check_errors() result(is_passed)
  !< Check the errors: missing file, not a VTK file, missing piece and array, reader not initialized.
  logical                       :: is_passed !< Check result.
  real(R8P), allocatable        :: r(:)      !< Values read.
  character(len=:), allocatable :: names(:)  !< Names of dataarrays.
  integer(I4P)                  :: u         !< File unit.
  integer(I8P)                  :: np        !< Number of points.

  is_passed = a_vtk_file%initialize(filename='vtkfortran_read-missing.vtu', action='read') == 1
  open(newunit=u, file='vtkfortran_read-not-vtk.xml', action='write', status='replace')
  write(u, '(A)') '<?xml version="1.0"?>'
  write(u, '(A)') '<Root><Child/></Root>'
  close(u)
  is_passed = is_passed .and. a_vtk_file%initialize(filename='vtkfortran_read-not-vtk.xml', action='read') == 2
  is_passed = is_passed .and. a_vtk_file%xml_reader%read_piece(np=np) == 4
  is_passed = is_passed .and. a_vtk_file%initialize(filename='vtkfortran_read-ascii-UInt32-none.vtu', action='read') == 0
  is_passed = is_passed .and. a_vtk_file%xml_reader%read_piece(piece=2, np=np) == 4
  is_passed = is_passed .and. a_vtk_file%xml_reader%read_dataarray(location='node', data_name='q', x=r) == 4
  is_passed = is_passed .and. a_vtk_file%xml_reader%read_dataarray(location='edge', data_name='p', x=r) == 4
  is_passed = is_passed .and. a_vtk_file%xml_reader%get_dataarray_names(location='node', names=names, piece=2) == 4
  is_passed = is_passed .and. a_vtk_file%finalize() == 0
  if (.not.is_passed) print '(A)', 'failed: errors'
  endfunction check_errors

  function check_vtk_written() result(is_passed)
  !< Read files written by VTK 9.7 (vtkXMLUnstructuredGridWriter): inline binary with a UInt64 header and, with zlib,
  !< zlib compressed base64 appended data. They hold a tetrahedron with Int64 cells and a Float32 point array.
  logical                       :: is_passed    !< Check result.
  character(len=:), allocatable :: text         !< File content.
  character(len=1), parameter   :: nl=new_line('a') !< New line.

#ifdef VTKFORTRAN_USE_ZLIB
  logical                       :: is_passed_zlib !< Check result of the compressed file.
#endif
  text = &
    '<?xml version="1.0"?>'//nl//&
    '<VTKFile type="UnstructuredGrid" version="1.0" byte_order="LittleEndian" header_type="UInt64">'//nl//&
    '  <UnstructuredGrid>'//nl//&
    '    <Piece NumberOfPoints="4" NumberOfCells="1">'//nl//&
    '      <PointData>'//nl//&
    '        <DataArray type="Float32" Name="p" format="binary" RangeMin="1.5" RangeMax="4.5">'//nl//&
    '          EAAAAAAAAAAAAMA/AAAgQAAAYEAAAJBA'//nl//&
    '        </DataArray>'//nl//&
    '      </PointData>'//nl//&
    '      <CellData>'//nl//&
    '      </CellData>'//nl//&
    '      <Points>'//nl//&
    '        <DataArray type="Float64" Name="Points" NumberOfComponents="3" format="binary" RangeMin='//&
    '"0" RangeMax="1">'//nl//&
    '          YAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAADwPwAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA'//&
    'AAAAAAAPA/AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA8D8='//nl//&
    '          <InformationKey name="L2_NORM_RANGE" location="vtkDataArray" length="2">'//nl//&
    '            <Value index="0">'//nl//&
    '              0'//nl//&
    '            </Value>'//nl//&
    '            <Value index="1">'//nl//&
    '              1'//nl//&
    '            </Value>'//nl//&
    '          </InformationKey>'//nl//&
    '        </DataArray>'//nl//&
    '      </Points>'//nl//&
    '      <Cells>'//nl//&
    '        <DataArray type="Int64" Name="connectivity" format="binary" RangeMin="0" RangeMax="3">'//nl//&
    '          IAAAAAAAAAAAAAAAAAAAAAEAAAAAAAAAAgAAAAAAAAADAAAAAAAAAA=='//nl//&
    '        </DataArray>'//nl//&
    '        <DataArray type="Int64" Name="offsets" format="binary" RangeMin="4" RangeMax="4">'//nl//&
    '          CAAAAAAAAAAEAAAAAAAAAA=='//nl//&
    '        </DataArray>'//nl//&
    '        <DataArray type="UInt8" Name="types" format="binary" RangeMin="10" RangeMax="10">'//nl//&
    '          AQAAAAAAAAAK'//nl//&
    '        </DataArray>'//nl//&
    '      </Cells>'//nl//&
    '    </Piece>'//nl//&
    '  </UnstructuredGrid>'//nl//&
    '</VTKFile>'
  is_passed = check_vtk_file(filename='vtkfortran_read-vtk-binary.vtu', text=text)
#ifdef VTKFORTRAN_USE_ZLIB
  text = &
    '<?xml version="1.0"?>'//nl//&
    '<VTKFile type="UnstructuredGrid" version="1.0" byte_order="LittleEndian" header_type="UInt64" co'//&
    'mpressor="vtkZLibDataCompressor">'//nl//&
    '  <UnstructuredGrid>'//nl//&
    '    <Piece NumberOfPoints="4"                    NumberOfCells="1"                   >'//nl//&
    '      <PointData>'//nl//&
    '        <DataArray type="Float32" Name="p" format="appended" RangeMin="1.5"                  Ran'//&
    'geMax="4.5"                  offset="0"                   >'//nl//&
    '        </DataArray>'//nl//&
    '      </PointData>'//nl//&
    '      <CellData>'//nl//&
    '      </CellData>'//nl//&
    '      <Points>'//nl//&
    '        <DataArray type="Float64" Name="Points" NumberOfComponents="3" format="appended" RangeMi'//&
    'n="0"                    RangeMax="1"                    offset="76"                  >'//nl//&
    '          <InformationKey name="L2_NORM_RANGE" location="vtkDataArray" length="2">'//nl//&
    '            <Value index="0">'//nl//&
    '              0'//nl//&
    '            </Value>'//nl//&
    '            <Value index="1">'//nl//&
    '              1'//nl//&
    '            </Value>'//nl//&
    '          </InformationKey>'//nl//&
    '        </DataArray>'//nl//&
    '      </Points>'//nl//&
    '      <Cells>'//nl//&
    '        <DataArray type="Int64" Name="connectivity" format="appended" RangeMin=""               '//&
    '      RangeMax=""                     offset="144"                 />'//nl//&
    '        <DataArray type="Int64" Name="offsets" format="appended" RangeMin=""                    '//&
    ' RangeMax=""                     offset="216"                 />'//nl//&
    '        <DataArray type="UInt8" Name="types" format="appended" RangeMin=""                     R'//&
    'angeMax=""                     offset="276"                 >'//nl//&
    '        </DataArray>'//nl//&
    '      </Cells>'//nl//&
    '    </Piece>'//nl//&
    '  </UnstructuredGrid>'//nl//&
    '  <AppendedData encoding="base64">'//nl//&
    '   _AQAAAAAAAAAAgAAAAAAAABAAAAAAAAAAFgAAAAAAAAA=eF5jYDhgz8Cg4MDAkADEExwAFiMC0A==AQAAAAAAAAAAgAAA'//&
    'AAAAAGAAAAAAAAAAEgAAAAAAAAA=eF5jYMAHPtjjlSZCHgB4XQOOAQAAAAAAAAAAgAAAAAAAACAAAAAAAAAAEwAAAAAAAAA='//&
    'eF5jYIAARijNBKWZoTQAAHAABw==AQAAAAAAAAAAgAAAAAAAAAgAAAAAAAAACwAAAAAAAAA=eF5jYYAAAAAoAAU=AQAAAAAA'//&
    'AAAAgAAAAAAAAAEAAAAAAAAACQAAAAAAAAA=eF7jAgAACwAL'//nl//&
    '  </AppendedData>'//nl//&
    '</VTKFile>'
  is_passed_zlib = check_vtk_file(filename='vtkfortran_read-vtk-appended-zlib.vtu', text=text)
  is_passed = is_passed .and. is_passed_zlib
#endif
  endfunction check_vtk_written

  function check_vtk_file(filename, text) result(is_passed)
  !< Write a file written by VTK and read it back.
  character(*), intent(in)      :: filename  !< File name.
  character(*), intent(in)      :: text      !< File content.
  logical                       :: is_passed !< Check result.
  real(R8P),    allocatable     :: xr(:)     !< X coordinates read.
  real(R8P),    allocatable     :: yr(:)     !< Y coordinates read.
  real(R8P),    allocatable     :: zr(:)     !< Z coordinates read.
  real(R4P),    allocatable     :: pr(:)     !< Point data read.
  integer(I4P), allocatable     :: connect(:) !< Connectivity read.
  integer(I4P), allocatable     :: offset(:) !< Offsets read.
  integer(I1P), allocatable     :: cell_type(:) !< Cell types read.
  integer(I4P)                  :: u         !< File unit.
  integer(I4P)                  :: e(5)      !< Errors.

  open(newunit=u, file=filename, access='stream', form='unformatted', action='write', status='replace')
  write(u) text//new_line('a')
  close(u)
  e(1) = a_vtk_file%initialize(filename=filename, action='read')
  e(2) = a_vtk_file%xml_reader%read_geo(x=xr, y=yr, z=zr)
  e(3) = a_vtk_file%xml_reader%read_connectivity(connectivity=connect, offset=offset, cell_type=cell_type)
  e(4) = a_vtk_file%xml_reader%read_dataarray(location='node', data_name='p', x=pr)
  e(5) = a_vtk_file%finalize()
  is_passed = all(e == 0) .and. all(xr == x) .and. all(yr == y) .and. all(zr == z) .and. all(connect == [0, 1, 2, 3]) .and. &
              all(offset == [4]) .and. all(cell_type == [10_I1P]) .and. all(pr == [1.5_R4P, 2.5_R4P, 3.5_R4P, 4.5_R4P])
  if (.not.is_passed) print '(A)', 'failed: '//filename
  endfunction check_vtk_file

  function check_ascii_not_separated() result(is_passed)
  !< Read an ASCII array whose values are not separated (written so by VTKFortran up to v2.0.10).
  logical                :: is_passed !< Check result.
  real(R8P), allocatable :: phi(:)    !< Point data read.
  integer(I4P)           :: u         !< File unit.
  integer(I4P)           :: e(3)      !< Errors.

  open(newunit=u, file='vtkfortran_read-not-separated.vti', action='write', status='replace')
  write(u, '(A)') '<?xml version="1.0"?>'
  write(u, '(A)') '<VTKFile type="ImageData" version="1.0" byte_order="LittleEndian" header_type="UInt32">'
  write(u, '(A)') '  <ImageData WholeExtent="+0 +2 +0 +0 +0 +0" Origin="0.0 0.0 0.0" Spacing="1.0 1.0 1.0">'
  write(u, '(A)') '    <Piece Extent="+0 +2 +0 +0 +0 +0">'
  write(u, '(A)') '      <PointData>'
  write(u, '(A)') '        <DataArray type="Float64" NumberOfComponents="1" Name="phi" format="ascii">'
  write(u, '(A)') '          +0.10000000000000000E+001+0.20000000000000000E-001  -0.30000000000000000E+003'
  write(u, '(A)') '        </DataArray>'
  write(u, '(A)') '      </PointData>'
  write(u, '(A)') '    </Piece>'
  write(u, '(A)') '  </ImageData>'
  write(u, '(A)') '</VTKFile>'
  close(u)
  e(1) = a_vtk_file%initialize(filename='vtkfortran_read-not-separated.vti', action='read')
  e(2) = a_vtk_file%xml_reader%read_dataarray(location='node', data_name='phi', x=phi)
  e(3) = a_vtk_file%finalize()
  is_passed = all(e == 0) .and. all(phi == [1._R8P, 0.02_R8P, -300._R8P])
  if (.not.is_passed) print '(A)', 'failed: vtkfortran_read-not-separated.vti'
  endfunction check_ascii_not_separated
endprogram vtk_fortran_read
