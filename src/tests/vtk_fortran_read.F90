!< VTK_Fortran test: read VTK XML files (vtk_file, action='read').
program vtk_fortran_read
!< VTK_Fortran test: read VTK XML files (vtk_file, action='read').
!<
!< Small files are written and read back, and the values compared exactly: an unstructured grid (geometry, cells, point,
!< cell and field data, strings) in every format, with UInt32 and UInt64 headers and zlib compressed when available; an
!< image data; a polydata with 64-bit ids; a rectilinear grid of two pieces; unsigned arrays. Two files written by VTK
!< 9.7 (embedded below; one zlib compressed) and an ASCII file with values not separated are read too. The errors of
!< missing files, arrays and of kinds that cannot hold the values are checked. Polyhedra in the layout of VTK 9.7.2 (XML
!< 2.3, a file written by VTK embedded below, and a shared face stored once, 2.5); ASCII non-finite reals (VTK's
!< `nan inf -inf`, VTKFortran's former `+    Infinity`) and UInt64 values above huge(I8P); names holding `"&<>'` and
!< non-finite values written in every format and read back, and in a `.pvd`; a 6-component ASCII array (written
!< u v w x y z, w and x joined) with non-finite values where w and x meet; `type="Bit"` arrays of two files written by
!< VTK 9.7.2 (ascii, binary), embedded below; string arrays written as point and cell data in every format and as a column
!< of a `.vtt` Table (a column of other rows refused, the next column unaffected), read back; component names
!< (`set_component_names`, the next array only) and field data of several components in every format, read back. Four
!< BigEndian files written by VTK 9.7.2 (inline binary with UInt32 headers, appended raw with UInt64 headers; with zlib,
!< appended base64 and appended raw), embedded below: every array, the points and the cells. A binary Bit array of an
!< ImageData (its tuples from the extent, not 8 per byte) and component names not carried over a refused Table column.
use penf
use, intrinsic :: ieee_arithmetic, only : ieee_is_nan, ieee_is_finite, ieee_value, ieee_quiet_nan, &
                                          ieee_positive_inf, ieee_negative_inf
use vtk_fortran, only : vtk_file, pvd_file

implicit none
character(*), parameter :: formats(4)=['ascii          ', 'binary         ', 'raw            ', 'binary-appended'] !< Formats.
real(R8P),    parameter :: x(4)=[0._R8P, 1._R8P, 0._R8P, 0._R8P] !< X coordinates of the tetrahedron.
real(R8P),    parameter :: y(4)=[0._R8P, 0._R8P, 1._R8P, 0._R8P] !< Y coordinates of the tetrahedron.
real(R8P),    parameter :: z(4)=[0._R8P, 0._R8P, 0._R8P, 1._R8P] !< Z coordinates of the tetrahedron.
real(R8P),    parameter :: p(4)=[1.5_R8P, -2.25_R8P, 1.e-300_R8P, 4._R8P/3._R8P] !< Point data.
real(R4P),    parameter :: v(3,4)=reshape([1,2,3, 4,5,6, 7,8,9, 10,11,12], [3,4]) * 0.1_R4P !< Point vectors.
type(vtk_file)          :: a_vtk_file      !< A VTK file.
integer(I4P)            :: f               !< Counter.
logical                 :: test_passed(19) !< List of passed tests.

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
test_passed(11) = check_vtk_polyhedra()
test_passed(12) = check_ascii_special()
test_passed(13) = check_names_and_non_finite()
test_passed(14) = check_six_non_finite()
test_passed(15) = check_vtk_bits()
test_passed(16) = check_string_arrays()
test_passed(17) = check_components()
test_passed(18) = check_big_endian()
test_passed(19) = check_review_fixes()

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
  function check_vtk_polyhedra() result(is_passed)
  !< Read the polyhedra of files in the layout VTK writes since XML 2.3 (`face_connectivity`, `face_offsets`,
  !< `polyhedron_to_faces`, `polyhedron_offsets`): the file written by VTK 9.7.2 (vtkXMLUnstructuredGridWriter, ascii; two
  !< hexahedral polyhedra and a tetrahedron between them), then the same with the face shared by the two polyhedra stored
  !< once (XML 2.5: `-(n+1)` for the face `n` traversed in reverse).
  logical                       :: is_passed      !< Check result.
  character(len=:), allocatable :: text           !< File content.
  character(len=:), allocatable :: shared         !< File content, the shared face stored once.
  character(len=1), parameter   :: nl=new_line('a') !< New line.
  integer(I8P),     parameter   :: cube0(31)=[6, 4,0,3,4,1, 4,6,7,10,9, 4,0,1,7,6, 4,1,4,10,7, 4,4,3,9,10, 4,3,0,6,9] !< Faces.
  integer(I8P),     parameter   :: cube1(31)=[6, 4,1,4,5,2, 4,7,8,11,10, 4,1,2,8,7, 4,2,5,11,8, 4,5,4,10,11, 4,4,1,7,10] !< Faces.
  integer(I8P),     parameter   :: cube1r(31)=[6, 4,1,4,5,2, 4,7,8,11,10, 4,1,2,8,7, 4,2,5,11,8, 4,5,4,10,11, &
                                               4,7,10,4,1] !< Faces of the second polyhedron, the shared one reversed.
  integer(I8P), allocatable     :: face(:)        !< Faces read (I8P).
  integer(I8P), allocatable     :: faceoffset(:)  !< Face offsets read (I8P).
  integer(I8P), allocatable     :: connect(:)     !< Connectivity read (I8P).
  integer(I8P), allocatable     :: offset(:)      !< Offsets read (I8P).
  integer(I4P), allocatable     :: face4(:)       !< Faces read (I4P).
  integer(I4P), allocatable     :: faceoffset4(:) !< Face offsets read (I4P).
  integer(I4P), allocatable     :: connect4(:)    !< Connectivity read (I4P).
  integer(I4P), allocatable     :: offset4(:)     !< Offsets read (I4P).
  integer(I1P), allocatable     :: cell_type(:)   !< Cell types read.
  character(len=:), allocatable :: filename       !< File name.
  integer(I4P)                  :: c              !< Counter.
  integer(I4P)                  :: u              !< File unit.
  integer(I4P)                  :: e(4)           !< Errors.

  text = &
    '<?xml version="1.0"?>'//nl//&
    '<VTKFile type="UnstructuredGrid" version="2.3" byte_order="LittleEndian" header_type="UInt32">'//nl//&
    '  <UnstructuredGrid>'//nl//&
    '    <Piece NumberOfPoints="12" NumberOfCells="3">'//nl//&
    '      <PointData>'//nl//&
    '      </PointData>'//nl//&
    '      <CellData>'//nl//&
    '      </CellData>'//nl//&
    '      <Points>'//nl//&
    '        <DataArray type="Float32" Name="Points" NumberOfComponents="3" format="ascii" RangeMin="0" R'//&
    'angeMax="2.449489742783178">'//nl//&
    '          0 0 0 1 0 0'//nl//&
    '          2 0 0 0 1 0'//nl//&
    '          1 1 0 2 1 0'//nl//&
    '          0 0 1 1 0 1'//nl//&
    '          2 0 1 0 1 1'//nl//&
    '          1 1 1 2 1 1'//nl//&
    '          <InformationKey name="L2_NORM_RANGE" location="vtkDataArray" length="2">'//nl//&
    '            <Value index="0">'//nl//&
    '              0'//nl//&
    '            </Value>'//nl//&
    '            <Value index="1">'//nl//&
    '              2.4494897428'//nl//&
    '            </Value>'//nl//&
    '          </InformationKey>'//nl//&
    '        </DataArray>'//nl//&
    '      </Points>'//nl//&
    '      <Cells>'//nl//&
    '        <DataArray type="Int64" Name="connectivity" format="ascii" RangeMin="0" RangeMax="11">'//nl//&
    '          0 1 3 4 6 7'//nl//&
    '          9 10 0 1 3 6'//nl//&
    '          1 2 4 5 7 8'//nl//&
    '          10 11'//nl//&
    '        </DataArray>'//nl//&
    '        <DataArray type="Int64" Name="offsets" format="ascii" RangeMin="8" RangeMax="20">'//nl//&
    '          8 12 20'//nl//&
    '        </DataArray>'//nl//&
    '        <DataArray type="UInt8" Name="types" format="ascii" RangeMin="10" RangeMax="42">'//nl//&
    '          42 10 42'//nl//&
    '        </DataArray>'//nl//&
    '        <DataArray type="Int64" Name="face_connectivity" format="ascii" RangeMin="0" RangeMax="11">'//nl//&
    '          0 3 4 1 6 7'//nl//&
    '          10 9 0 1 7 6'//nl//&
    '          1 4 10 7 4 3'//nl//&
    '          9 10 3 0 6 9'//nl//&
    '          1 4 5 2 7 8'//nl//&
    '          11 10 1 2 8 7'//nl//&
    '          2 5 11 8 5 4'//nl//&
    '          10 11 4 1 7 10'//nl//&
    '        </DataArray>'//nl//&
    '        <DataArray type="Int64" Name="face_offsets" format="ascii" RangeMin="4" RangeMax="48">'//nl//&
    '          4 8 12 16 20 24'//nl//&
    '          28 32 36 40 44 48'//nl//&
    '        </DataArray>'//nl//&
    '        <DataArray type="Int64" Name="polyhedron_to_faces" format="ascii" RangeMin="0" RangeMax="11"'//&
    '>'//nl//&
    '          0 1 2 3 4 5'//nl//&
    '          6 7 8 9 10 11'//nl//&
    '        </DataArray>'//nl//&
    '        <DataArray type="Int64" Name="polyhedron_offsets" format="ascii" RangeMin="6" RangeMax="12">'//nl//&
    '          6 6 12'//nl//&
    '        </DataArray>'//nl//&
    '      </Cells>'//nl//&
    '    </Piece>'//nl//&
    '  </UnstructuredGrid>'//nl//&
    '</VTKFile>'
  ! the shared face (the 4th of the first polyhedron, the 6th of the second, reversed) stored once
  shared = text
  call replace(shared, '          10 11 4 1 7 10'//nl, '          10 11'//nl)
  call replace(shared, '28 32 36 40 44 48', '28 32 36 40 44')
  call replace(shared, '6 7 8 9 10 11', '6 7 8 9 10 -4')
  call replace(shared, 'version="2.3"', 'version="2.5"')
  is_passed = .true.
  do c=1, 2
    filename = 'vtkfortran_read-vtk-polyhedra-'//trim(str(c, no_sign=.true.))//'.vtu'
    open(newunit=u, file=filename, access='stream', form='unformatted', action='write', status='replace')
    if (c == 1) then
      write(u) text//nl
    else
      write(u) shared//nl
    endif
    close(u)
    e(1) = a_vtk_file%initialize(filename=filename, action='read')
    e(2) = a_vtk_file%xml_reader%read_connectivity(connectivity=connect, offset=offset, cell_type=cell_type, face=face, &
                                                   faceoffset=faceoffset)
    e(3) = a_vtk_file%xml_reader%read_connectivity(connectivity=connect4, offset=offset4, face=face4, &
                                                   faceoffset=faceoffset4)
    e(4) = a_vtk_file%finalize()
    is_passed = is_passed .and. all(e == 0)
    if (.not.is_passed) exit
    is_passed = all(cell_type == [42_I1P, 10_I1P, 42_I1P]) .and. all(offset == [8, 12, 20]) .and. &
                all(offset4 == [8, 12, 20]) .and. all(faceoffset == [31, -1, 62]) .and. all(faceoffset4 == [31, -1, 62])
    if (.not.is_passed) exit
    if (c == 1) then
      is_passed = all(face == [cube0, cube1]) .and. all(face4 == [cube0, cube1])
    else
      is_passed = all(face == [cube0, cube1r]) .and. all(face4 == [cube0, cube1r])
    endif
    if (.not.is_passed) exit
  enddo
  if (.not.is_passed) print '(A)', 'failed: polyhedra in the layout of VTK 9.7.2'
  endfunction check_vtk_polyhedra

  subroutine replace(text, old, new)
  !< Replace the first occurrence of a substring.
  character(len=:), allocatable, intent(inout) :: text !< Text.
  character(*),                  intent(in)    :: old  !< Substring.
  character(*),                  intent(in)    :: new  !< Replacement.
  integer                                      :: i    !< Position.

  i = index(text, old)
  if (i == 0) error stop 'replace: substring not found'
  text = text(:i-1)//new//text(i+len(old):)
  endsubroutine replace

  function check_ascii_special() result(is_passed)
  !< Read ASCII values that need care: non-finite reals as VTK writes them (`nan inf -inf`) and as VTKFortran wrote them
  !< up to v3.x (`+                Infinity`, the sign apart); UInt64 values above huge(I8P), read with their bits.
  logical                   :: is_passed !< Check result.
  real(R8P),    allocatable :: phi(:)    !< Float64 read.
  real(R4P),    allocatable :: psi(:)    !< Float32 read.
  integer(I8P), allocatable :: big(:)    !< UInt64 read.
  integer(I4P)              :: u         !< File unit.
  integer(I4P)              :: e(5)      !< Errors.

  open(newunit=u, file='vtkfortran_read-ascii-special.vti', action='write', status='replace')
  write(u, '(A)') '<?xml version="1.0"?>'
  write(u, '(A)') '<VTKFile type="ImageData" version="1.0" byte_order="LittleEndian" header_type="UInt32">'
  write(u, '(A)') '  <ImageData WholeExtent="0 5 0 0 0 0" Origin="0 0 0" Spacing="1 1 1">'
  write(u, '(A)') '    <Piece Extent="0 5 0 0 0 0">'
  write(u, '(A)') '      <PointData>'
  write(u, '(A)') '        <DataArray type="Float64" Name="phi" format="ascii">'
  write(u, '(A)') '          1.5 nan inf -inf +                Infinity -                Infinity'
  write(u, '(A)') '        </DataArray>'
  write(u, '(A)') '        <DataArray type="Float32" Name="psi" format="ascii">'
  write(u, '(A)') '          -2 NaN Infinity -Infinity +       Infinity    -       Infinity'
  write(u, '(A)') '        </DataArray>'
  write(u, '(A)') '        <DataArray type="UInt64" Name="big" format="ascii">'
  write(u, '(A)') '          9223372036854775807 9223372036854775808 9223372036854775809 18446744073709551614'
  write(u, '(A)') '          +18446744073709551615 0'
  write(u, '(A)') '        </DataArray>'
  write(u, '(A)') '      </PointData>'
  write(u, '(A)') '    </Piece>'
  write(u, '(A)') '  </ImageData>'
  write(u, '(A)') '</VTKFile>'
  close(u)
  e(1) = a_vtk_file%initialize(filename='vtkfortran_read-ascii-special.vti', action='read')
  e(2) = a_vtk_file%xml_reader%read_dataarray(location='node', data_name='phi', x=phi)
  e(3) = a_vtk_file%xml_reader%read_dataarray(location='node', data_name='psi', x=psi)
  e(4) = a_vtk_file%xml_reader%read_dataarray(location='node', data_name='big', x=big)
  e(5) = a_vtk_file%finalize()
  is_passed = all(e == 0)
  if (is_passed) is_passed = size(phi) == 6 .and. size(psi) == 6 .and. size(big) == 6
  if (is_passed) is_passed = phi(1) == 1.5_R8P .and. ieee_is_nan(phi(2)) .and. is_inf(phi(3), 1) .and. &
                             is_inf(phi(4), -1) .and. is_inf(phi(5), 1) .and. is_inf(phi(6), -1) .and. &
                             psi(1) == -2._R4P .and. ieee_is_nan(psi(2)) .and. is_inf(real(psi(3), R8P), 1) .and. &
                             is_inf(real(psi(4), R8P), -1) .and. is_inf(real(psi(5), R8P), 1) .and. &
                             is_inf(real(psi(6), R8P), -1) .and. &
                             all(big == [huge(1_I8P), -huge(1_I8P) - 1_I8P, -huge(1_I8P), -2_I8P, -1_I8P, 0_I8P])
  if (.not.is_passed) print '(A,*(1X,I0))', 'failed: vtkfortran_read-ascii-special.vti', e
  endfunction check_ascii_special

  function check_names_and_non_finite() result(is_passed)
  !< Write, in every format, an array whose name holds the characters XML escapes (`"`, `&`, `<`, `>`, `'`) and
  !< non-finite values (NaN, +Inf, -Inf; ASCII: `nan inf -inf`, as VTK writes them), with the name as the active
  !< scalars; read them back. A `.pvd` listing a file whose name holds them too.
  logical                       :: is_passed  !< Check result.
  character(*), parameter       :: name='a"b&c<d>e''f' !< Array name.
  real(R8P)                     :: s(3)       !< Values.
  real(R8P),    allocatable     :: sr(:)      !< Values read.
  real(R4P),    allocatable     :: sr4(:)     !< Values read (Float32).
  character(len=:), allocatable :: names(:)   !< Names read.
  character(len=:), allocatable :: active     !< Active scalars read.
  character(len=:), allocatable :: files(:)   !< Files of the .pvd read.
  character(len=:), allocatable :: groups(:)  !< Groups of the .pvd read.
  character(len=:), allocatable :: labels(:)  !< Names of the .pvd read.
  type(pvd_file)                :: pvd        !< PVD file.
  integer(I4P)                  :: f          !< Counter.
  integer(I4P)                  :: e(10)      !< Errors.

  s = [ieee_value(1._R8P, ieee_quiet_nan), ieee_value(1._R8P, ieee_positive_inf), ieee_value(1._R8P, ieee_negative_inf)]
  is_passed = .true.
  do f=1, size(formats)
    e = 0
    e(1) = a_vtk_file%initialize(format=trim(formats(f)), filename='vtkfortran_read-names-'//trim(formats(f))//'.vtu', &
                                 mesh_topology='UnstructuredGrid')
    e(2) = a_vtk_file%xml_writer%write_piece(np=3, nc=1)
    e(3) = a_vtk_file%xml_writer%write_geo(np=3, nc=1, x=x(1:3), y=y(1:3), z=z(1:3))
    e(4) = a_vtk_file%xml_writer%write_connectivity(nc=1, connectivity=[0_I4P, 1_I4P, 2_I4P], offset=[3_I4P], &
                                                    cell_type=[5_I1P])
    e(5) = a_vtk_file%xml_writer%write_dataarray(location='node', action='open', scalars=name)
    e(6) = a_vtk_file%xml_writer%write_dataarray(data_name=name, x=s)
    e(7) = a_vtk_file%xml_writer%write_dataarray(data_name='s4', x=real(s, R4P))
    e(8) = a_vtk_file%xml_writer%write_dataarray(location='node', action='close')
    e(9) = a_vtk_file%xml_writer%write_piece()
    e(10) = a_vtk_file%finalize()
    is_passed = is_passed .and. all(e == 0)
    if (any(e /= 0)) print '(A,*(1X,I0))', 'write '//trim(formats(f)), e
    e = 0
    e(1) = a_vtk_file%initialize(filename='vtkfortran_read-names-'//trim(formats(f))//'.vtu', action='read')
    e(2) = a_vtk_file%xml_reader%get_dataarray_names(location='node', names=names)
    e(3) = a_vtk_file%xml_reader%read_dataarray(location='node', data_name=name, x=sr)
    e(4) = a_vtk_file%xml_reader%read_dataarray(location='node', data_name='s4', x=sr4)
    e(5) = a_vtk_file%xml_reader%get_active(location='node', role='scalars', data_name=active)
    e(6) = a_vtk_file%finalize()
    is_passed = is_passed .and. all(e == 0)
    if (is_passed) is_passed = size(names) == 2 .and. size(sr) == 3 .and. size(sr4) == 3
    if (is_passed) is_passed = trim(names(1)) == name .and. active == name
    if (is_passed) is_passed = ieee_is_nan(sr(1)) .and. is_inf(sr(2), 1) .and. is_inf(sr(3), -1) .and. &
                               ieee_is_nan(sr4(1)) .and. is_inf(real(sr4(2), R8P), 1) .and. is_inf(real(sr4(3), R8P), -1)
    if (.not.is_passed) then
      print '(A,*(1X,I0))', 'failed: vtkfortran_read-names-'//trim(formats(f))//'.vtu', e
      return
    endif
  enddo
  e = 0
  e(1) = pvd%initialize(filename='vtkfortran_read-names.pvd')
  e(2) = pvd%write_dataset(filename=name//'.vtu', timestep=0._R8P, name=name, group=name)
  e(3) = pvd%finalize()
  e(4) = pvd%initialize(filename='vtkfortran_read-names.pvd', action='read')
  e(5) = pvd%get_datasets(group=groups, name=labels, file=files)
  e(6) = pvd%finalize()
  is_passed = all(e == 0)
  if (is_passed) is_passed = size(files) == 1 .and. size(groups) == 1 .and. size(labels) == 1
  if (is_passed) is_passed = trim(files(1)) == name//'.vtu' .and. trim(groups(1)) == name .and. trim(labels(1)) == name
  if (.not.is_passed) print '(A,*(1X,I0))', 'failed: vtkfortran_read-names.pvd', e
  endfunction check_names_and_non_finite

  function check_six_non_finite() result(is_passed)
  !< A 6-component array in ascii (`write_dataarray(u, v, w, x, y, z)`: w and x are joined, the sign of x separating
  !< them) with non-finite values at the join, read back: a blank separates them then.
  logical                :: is_passed !< Check result.
  real(R8P)              :: c(3)      !< Components u, v, y, z.
  real(R8P)              :: cw(3)     !< Component w.
  real(R8P)              :: cx(3)     !< Component x.
  real(R8P), allocatable :: t(:,:)    !< Values read (components, tuples).
  integer(I4P)           :: e(9)      !< Errors.

  c = [1._R8P, 2._R8P, 3._R8P]
  cw = [ieee_value(1._R8P, ieee_quiet_nan), ieee_value(1._R8P, ieee_negative_inf), 3._R8P]
  cx = [ieee_value(1._R8P, ieee_positive_inf), ieee_value(1._R8P, ieee_quiet_nan), -4._R8P]
  e = 0
  e(1) = a_vtk_file%initialize(format='ascii', filename='vtkfortran_read-six.vtu', mesh_topology='UnstructuredGrid')
  e(2) = a_vtk_file%xml_writer%write_piece(np=3, nc=1)
  e(3) = a_vtk_file%xml_writer%write_geo(np=3, nc=1, x=x(1:3), y=y(1:3), z=z(1:3))
  e(4) = a_vtk_file%xml_writer%write_connectivity(nc=1, connectivity=[0_I4P, 1_I4P, 2_I4P], offset=[3_I4P], &
                                                  cell_type=[5_I1P])
  e(5) = a_vtk_file%xml_writer%write_dataarray(location='node', action='open')
  e(6) = a_vtk_file%xml_writer%write_dataarray(data_name='six', u=c, v=c, w=cw, x=cx, y=c, z=c)
  e(7) = a_vtk_file%xml_writer%write_dataarray(location='node', action='close')
  e(8) = a_vtk_file%xml_writer%write_piece()
  e(9) = a_vtk_file%finalize()
  is_passed = all(e == 0)
  e = 0
  e(1) = a_vtk_file%initialize(filename='vtkfortran_read-six.vtu', action='read')
  e(2) = a_vtk_file%xml_reader%read_dataarray(location='node', data_name='six', x=t)
  e(3) = a_vtk_file%finalize()
  is_passed = is_passed .and. all(e == 0)
  if (is_passed) is_passed = all(shape(t) == [6, 3])
  if (is_passed) is_passed = all(t(1:2,:) == spread(c, 1, 2)) .and. all(t(5:6,:) == spread(c, 1, 2)) .and. &
                             ieee_is_nan(t(3,1)) .and. is_inf(t(4,1), 1) .and. is_inf(t(3,2), -1) .and. &
                             ieee_is_nan(t(4,2)) .and. t(3,3) == 3._R8P .and. t(4,3) == -4._R8P
  if (.not.is_passed) print '(A,*(1X,I0))', 'failed: vtkfortran_read-six.vtu', e
  endfunction check_six_non_finite

  function check_components() result(is_passed)
  !< Component names (`set_component_names`: `ComponentName<i>` of the next array only) of a 3-component point array, and
  !< a field data array of 3 components and 2 tuples, in every format; read back (names, values, counts).
  logical                       :: is_passed !< Check result.
  character(len=:), allocatable :: names(:)  !< Component names read.
  real(R8P),        allocatable :: fd(:,:)   !< Field data read.
  integer(I4P)                  :: nc, f     !< Components, counter.
  integer(I8P)                  :: nt        !< Tuples.
  integer(I4P)                  :: e(12)     !< Errors.

  is_passed = .true.
  do f=1, size(formats)
    e = 0
    e(1) = a_vtk_file%initialize(format=trim(formats(f)), filename='vtkfortran_read-components-'//trim(formats(f))//'.vtu', &
                                 mesh_topology='UnstructuredGrid')
    e(2) = a_vtk_file%xml_writer%write_fielddata(action='open')
    e(2) = e(2) + abs(a_vtk_file%xml_writer%write_fielddata(data_name='bounds', &
                                                            x=reshape([0._R8P, 1._R8P, 2._R8P, 3._R8P, 4._R8P, 5._R8P], [3, 2])))
    e(2) = e(2) + abs(a_vtk_file%xml_writer%write_fielddata(action='close'))
    e(3) = a_vtk_file%xml_writer%write_piece(np=4, nc=1)
    e(4) = a_vtk_file%xml_writer%write_geo(np=4, nc=1, x=x, y=y, z=z)
    e(5) = a_vtk_file%xml_writer%write_connectivity(nc=1, connectivity=[0_I4P, 1_I4P, 2_I4P, 3_I4P], offset=[4_I4P], &
                                                    cell_type=[10_I1P])
    e(6) = a_vtk_file%xml_writer%write_dataarray(location='node', action='open')
    e(7) = a_vtk_file%xml_writer%set_component_names([character(4) :: 'u', 'v', 'wall'])
    e(8) = a_vtk_file%xml_writer%write_dataarray(data_name='vel', x=v)
    e(9) = a_vtk_file%xml_writer%write_dataarray(data_name='p', x=p) ! no names: they apply to the next array only
    e(10) = a_vtk_file%xml_writer%write_dataarray(location='node', action='close')
    e(11) = a_vtk_file%xml_writer%write_piece()
    e(12) = a_vtk_file%finalize()
    is_passed = is_passed .and. all(e == 0)
    e = 0
    e(1) = a_vtk_file%initialize(filename='vtkfortran_read-components-'//trim(formats(f))//'.vtu', action='read')
    e(2) = a_vtk_file%xml_reader%get_dataarray_info(location='node', data_name='vel', n_components=nc, component_names=names)
    is_passed = is_passed .and. e(2) == 0 .and. nc == 3
    if (is_passed) is_passed = size(names) == 3
    if (is_passed) is_passed = names(1) == 'u' .and. names(2) == 'v' .and. names(3) == 'wall'
    e(3) = a_vtk_file%xml_reader%get_dataarray_info(location='node', data_name='p', component_names=names)
    is_passed = is_passed .and. e(3) == 0
    if (is_passed) is_passed = size(names) == 1
    if (is_passed) is_passed = len_trim(names(1)) == 0
    e(4) = a_vtk_file%xml_reader%get_dataarray_info(location='field', data_name='bounds', n_components=nc, n_tuples=nt)
    e(5) = a_vtk_file%xml_reader%read_dataarray(location='field', data_name='bounds', x=fd)
    e(6) = a_vtk_file%finalize()
    is_passed = is_passed .and. all(e == 0) .and. nc == 3 .and. nt == 2_I8P
    if (is_passed) is_passed = all(shape(fd) == [3, 2])
    if (is_passed) is_passed = all(fd == reshape([0._R8P, 1._R8P, 2._R8P, 3._R8P, 4._R8P, 5._R8P], [3, 2]))
    if (.not.is_passed) then
      print '(A,*(1X,I0))', 'failed: vtkfortran_read-components-'//trim(formats(f))//'.vtu', e
      return
    endif
  enddo
  endfunction check_components

  function check_string_arrays() result(is_passed)
  !< String arrays (VTK's String): point and cell data written in every format, a column of a Table (`.vtt`) next to a
  !< numeric one, read back; a string column of other rows refused, the numeric column after it written as usual.
  logical                       :: is_passed !< Check result.
  character(len=:), allocatable :: pl(:)     !< Point strings read.
  character(len=:), allocatable :: cl(:)     !< Cell strings read.
  character(len=:), allocatable :: data_type !< Type read.
  real(R8P),        allocatable :: v(:)      !< Numeric column read.
  integer(I4P)                  :: f         !< Counter.
  integer(I4P)                  :: e(12)     !< Errors.

  is_passed = .true.
  do f=1, size(formats)
    e = 0
    e(1) = a_vtk_file%initialize(format=trim(formats(f)), filename='vtkfortran_read-strings-'//trim(formats(f))//'.vtu', &
                                 mesh_topology='UnstructuredGrid')
    e(2) = a_vtk_file%xml_writer%write_piece(np=3, nc=1)
    e(3) = a_vtk_file%xml_writer%write_geo(np=3, nc=1, x=x(1:3), y=y(1:3), z=z(1:3))
    e(4) = a_vtk_file%xml_writer%write_connectivity(nc=1, connectivity=[0_I4P, 1_I4P, 2_I4P], offset=[3_I4P], &
                                                    cell_type=[5_I1P])
    e(5) = a_vtk_file%xml_writer%write_dataarray(location='node', action='open')
    e(6) = a_vtk_file%xml_writer%write_dataarray(data_name='pl', x=[character(7) :: 'inlet', 'wall', 'out let'])
    e(7) = a_vtk_file%xml_writer%write_dataarray(location='node', action='close')
    e(8) = a_vtk_file%xml_writer%write_dataarray(location='cell', action='open')
    e(9) = a_vtk_file%xml_writer%write_dataarray(data_name='cl', x=['tri'])
    e(10) = a_vtk_file%xml_writer%write_dataarray(location='cell', action='close')
    e(11) = a_vtk_file%xml_writer%write_piece()
    e(12) = a_vtk_file%finalize()
    is_passed = is_passed .and. all(e == 0)
    e = 0
    e(1) = a_vtk_file%initialize(filename='vtkfortran_read-strings-'//trim(formats(f))//'.vtu', action='read')
    e(2) = a_vtk_file%xml_reader%read_dataarray(location='node', data_name='pl', x=pl)
    e(3) = a_vtk_file%xml_reader%read_dataarray(location='cell', data_name='cl', x=cl)
    e(4) = a_vtk_file%xml_reader%get_dataarray_info(location='node', data_name='pl', data_type=data_type)
    e(5) = a_vtk_file%finalize()
    is_passed = is_passed .and. all(e == 0)
    if (is_passed) is_passed = size(pl) == 3 .and. size(cl) == 1 .and. data_type == 'String'
    if (is_passed) is_passed = pl(1) == 'inlet' .and. pl(2) == 'wall' .and. pl(3) == 'out let' .and. cl(1) == 'tri'
    if (.not.is_passed) then
      print '(A,*(1X,I0))', 'failed: vtkfortran_read-strings-'//trim(formats(f))//'.vtu', e
      return
    endif
  enddo
  ! a Table: a string column next to a numeric one; a string column of 2 rows refused, the next column written
  e = 0
  e(1) = a_vtk_file%initialize(format='binary', filename='vtkfortran_read-strings.vtt', mesh_topology='Table')
  e(2) = a_vtk_file%xml_writer%write_dataarray(location='row', action='open')
  e(3) = a_vtk_file%xml_writer%write_dataarray(data_name='name', x=[character(5) :: 'alpha', 'beta', 'gamma'])
  e(4) = merge(0, 1, a_vtk_file%xml_writer%write_dataarray(data_name='short', x=['a', 'b']) /= 0) ! 2 rows: refused
  e(5) = a_vtk_file%xml_writer%write_dataarray(data_name='value', x=[1._R8P, 2._R8P, 3._R8P])
  e(6) = a_vtk_file%xml_writer%write_dataarray(location='row', action='close')
  e(7) = a_vtk_file%finalize()
  e(8) = a_vtk_file%initialize(filename='vtkfortran_read-strings.vtt', action='read')
  e(9) = a_vtk_file%xml_reader%read_dataarray(location='row', data_name='name', x=pl)
  e(10) = a_vtk_file%xml_reader%read_dataarray(location='row', data_name='value', x=v)
  e(11) = merge(0, 1, a_vtk_file%xml_reader%read_dataarray(location='row', data_name='short', x=cl) == 4) ! not written
  e(12) = a_vtk_file%finalize()
  is_passed = all(e == 0)
  if (is_passed) is_passed = size(pl) == 3 .and. size(v) == 3
  if (is_passed) is_passed = pl(1) == 'alpha' .and. pl(3) == 'gamma' .and. all(v == [1._R8P, 2._R8P, 3._R8P])
  if (.not.is_passed) print '(A,*(1X,I0))', 'failed: vtkfortran_read-strings.vtt', e
  endfunction check_string_arrays

  function check_vtk_bits() result(is_passed)
  !< Read `type="Bit"` arrays (vtkBitArray) of files written by VTK 9.7.2 (vtkXMLUnstructuredGridWriter, ascii and
  !< binary: one value per bit, MSB first, the last byte padded): 11 points, `bits` (1 component) and `b3` (3 components),
  !< read as 0/1 integers; `get_dataarray_info` gives the type `Bit`.
  logical                       :: is_passed !< Check result.
  character(len=:), allocatable :: text      !< File content.
  character(len=1), parameter   :: nl=new_line('a') !< New line.
  integer(I1P),     parameter   :: pattern(11) = [1, 0, 1, 1, 0, 0, 0, 1, 1, 1, 0] !< The bits written.
  integer(I1P),     allocatable :: b(:)      !< bits read.
  integer(I4P),     allocatable :: b3(:)     !< b3 read.
  character(len=:), allocatable :: data_type !< Type of b3.
  integer(I4P)                  :: nc, u, f  !< Components, file unit, counter.
  integer(I4P)                  :: e(5)      !< Errors.

  is_passed = .true.
  do f=1, 2
    if (f == 1) then
      text = &
        '<?xml version="1.0"?>'//nl//&
        '<VTKFile type="UnstructuredGrid" version="0.1" byte_order="LittleEndian" header_type="UInt32">'//nl//&
        '  <UnstructuredGrid>'//nl//&
        '    <Piece NumberOfPoints="11" NumberOfCells="0">'//nl//&
        '      <PointData>'//nl//&
        '        <DataArray type="Bit" Name="bits" format="ascii" RangeMin="0" RangeMax="1">'//nl//&
        '          1 0 1 1 0 0'//nl//&
        '          0 1 1 1 0'//nl//&
        '        </DataArray>'//nl//&
        '        <DataArray type="Bit" Name="b3" NumberOfComponents="3" format="ascii" RangeMin="0" RangeMax='//&
        '"1.7320508075688772">'//nl//&
        '          1 0 1 1 0 0'//nl//&
        '          0 1 1 1 0 1'//nl//&
        '          0 1 1 0 0 0'//nl//&
        '          1 1 1 0 1 0'//nl//&
        '          1 1 0 0 0 1'//nl//&
        '          1 1 0'//nl//&
        '          <InformationKey name="L2_NORM_RANGE" location="vtkDataArray" length="2">'//nl//&
        '            <Value index="0">'//nl//&
        '              0'//nl//&
        '            </Value>'//nl//&
        '            <Value index="1">'//nl//&
        '              1.7320508076'//nl//&
        '            </Value>'//nl//&
        '          </InformationKey>'//nl//&
        '        </DataArray>'//nl//&
        '      </PointData>'//nl//&
        '      <CellData>'//nl//&
        '      </CellData>'//nl//&
        '      <Points>'//nl//&
        '        <DataArray type="Float32" Name="Points" NumberOfComponents="3" format="ascii" RangeMin="0" R'//&
        'angeMax="10">'//nl//&
        '          0 0 0 1 0 0'//nl//&
        '          2 0 0 3 0 0'//nl//&
        '          4 0 0 5 0 0'//nl//&
        '          6 0 0 7 0 0'//nl//&
        '          8 0 0 9 0 0'//nl//&
        '          10 0 0'//nl//&
        '          <InformationKey name="L2_NORM_RANGE" location="vtkDataArray" length="2">'//nl//&
        '            <Value index="0">'//nl//&
        '              0'//nl//&
        '            </Value>'//nl//&
        '            <Value index="1">'//nl//&
        '              10'//nl//&
        '            </Value>'//nl//&
        '          </InformationKey>'//nl//&
        '        </DataArray>'//nl//&
        '      </Points>'//nl//&
        '      <Cells>'//nl//&
        '        <DataArray type="Int64" Name="connectivity" format="ascii" RangeMin="1e+299" RangeMax="-1e+2'//&
        '99">'//nl//&
        '        </DataArray>'//nl//&
        '        <DataArray type="Int64" Name="offsets" format="ascii" RangeMin="1e+299" RangeMax="-1e+299">'//nl//&
        '        </DataArray>'//nl//&
        '        <DataArray type="UInt8" Name="types" format="ascii" RangeMin="1e+299" RangeMax="-1e+299">'//nl//&
        '        </DataArray>'//nl//&
        '      </Cells>'//nl//&
        '    </Piece>'//nl//&
        '  </UnstructuredGrid>'//nl//&
        '</VTKFile>'
    else
      text = &
        '<?xml version="1.0"?>'//nl//&
        '<VTKFile type="UnstructuredGrid" version="0.1" byte_order="LittleEndian" header_type="UInt32">'//nl//&
        '  <UnstructuredGrid>'//nl//&
        '    <Piece NumberOfPoints="11" NumberOfCells="0">'//nl//&
        '      <PointData>'//nl//&
        '        <DataArray type="Bit" Name="bits" format="binary" RangeMin="0" RangeMax="1">'//nl//&
        '          AgAAALHA'//nl//&
        '        </DataArray>'//nl//&
        '        <DataArray type="Bit" Name="b3" NumberOfComponents="3" format="binary" RangeMin="0" RangeMax'//&
        '="1.7320508075688772">'//nl//&
        '          BQAAALHWOscA'//nl//&
        '          <InformationKey name="L2_NORM_RANGE" location="vtkDataArray" length="2">'//nl//&
        '            <Value index="0">'//nl//&
        '              0'//nl//&
        '            </Value>'//nl//&
        '            <Value index="1">'//nl//&
        '              1.7320508076'//nl//&
        '            </Value>'//nl//&
        '          </InformationKey>'//nl//&
        '        </DataArray>'//nl//&
        '      </PointData>'//nl//&
        '      <CellData>'//nl//&
        '      </CellData>'//nl//&
        '      <Points>'//nl//&
        '        <DataArray type="Float32" Name="Points" NumberOfComponents="3" format="binary" RangeMin="0" '//&
        'RangeMax="10">'//nl//&
        '          hAAAAAAAAAAAAAAAAAAAAAAAgD8AAAAAAAAAAAAAAEAAAAAAAAAAAAAAQEAAAAAAAAAAAAAAgEAAAAAAAAAAAAAAoE'//&
        'AAAAAAAAAAAAAAwEAAAAAAAAAAAAAA4EAAAAAAAAAAAAAAAEEAAAAAAAAAAAAAEEEAAAAAAAAAAAAAIEEAAAAAAAAAAA=='//nl//&
        '          <InformationKey name="L2_NORM_RANGE" location="vtkDataArray" length="2">'//nl//&
        '            <Value index="0">'//nl//&
        '              0'//nl//&
        '            </Value>'//nl//&
        '            <Value index="1">'//nl//&
        '              10'//nl//&
        '            </Value>'//nl//&
        '          </InformationKey>'//nl//&
        '        </DataArray>'//nl//&
        '      </Points>'//nl//&
        '      <Cells>'//nl//&
        '        <DataArray type="Int64" Name="connectivity" format="binary" RangeMin="1e+299" RangeMax="-1e+'//&
        '299">'//nl//&
        '          AAAAAA=='//nl//&
        '        </DataArray>'//nl//&
        '        <DataArray type="Int64" Name="offsets" format="binary" RangeMin="1e+299" RangeMax="-1e+299">'//nl//&
        '          AAAAAA=='//nl//&
        '        </DataArray>'//nl//&
        '        <DataArray type="UInt8" Name="types" format="binary" RangeMin="1e+299" RangeMax="-1e+299">'//nl//&
        '          AAAAAA=='//nl//&
        '        </DataArray>'//nl//&
        '      </Cells>'//nl//&
        '    </Piece>'//nl//&
        '  </UnstructuredGrid>'//nl//&
        '</VTKFile>'
    endif
    open(newunit=u, file='vtkfortran_read-vtk-bits-'//trim(str(f, no_sign=.true.))//'.vtu', access='stream', &
         form='unformatted', action='write', status='replace')
    write(u) text//nl
    close(u)
    e(1) = a_vtk_file%initialize(filename='vtkfortran_read-vtk-bits-'//trim(str(f, no_sign=.true.))//'.vtu', action='read')
    e(2) = a_vtk_file%xml_reader%read_dataarray(location='node', data_name='bits', x=b)
    e(3) = a_vtk_file%xml_reader%read_dataarray(location='node', data_name='b3', x=b3)
    e(4) = a_vtk_file%xml_reader%get_dataarray_info(location='node', data_name='b3', data_type=data_type, n_components=nc)
    e(5) = a_vtk_file%finalize()
    is_passed = is_passed .and. all(e == 0)
    if (.not.is_passed) exit
    is_passed = all(b == pattern) .and. size(b3) == 33 .and. data_type == 'Bit' .and. nc == 3
    if (is_passed) is_passed = all(b3 == int([pattern, pattern, pattern], I4P))
    if (.not.is_passed) exit
  enddo
  if (.not.is_passed) print '(A,*(1X,I0))', 'failed: Bit arrays written by VTK 9.7.2', e
  endfunction check_vtk_bits

  elemental function is_inf(v, sign) result(is_it)
  !< Return .true. if a value is an infinity of a sign.
  real(R8P),    intent(in) :: v     !< Value.
  integer,      intent(in) :: sign  !< Sign: 1 or -1.
  logical                  :: is_it !< Inquire result.

  is_it = (.not.ieee_is_finite(v)) .and. (.not.ieee_is_nan(v)) .and. ((v > 0._R8P) .eqv. (sign > 0))
  endfunction is_inf
  logical function check_review_fixes()
  !< A binary `type="Bit"` point array of an ImageData without NumberOfTuples: its tuples are the points of the extent (5),
  !< not the 8 bits of its byte; component names set before a refused Table column are not given to the next column.
  type(vtk_file)                :: w, r     !< Writer, reader.
  integer(I1P), allocatable     :: bits(:)  !< Bit array read.
  character(len=:), allocatable :: names(:) !< Component names read.
  integer(I8P)                  :: n        !< Tuples read.
  integer(I4P)                  :: u, e(4)  !< Unit, errors.

  open(newunit=u, file='vtkfortran_read_bit_image.vti', status='replace', action='write')
  write(u, '(A)') '<?xml version="1.0"?>', &
    '<VTKFile type="ImageData" version="1.0" byte_order="LittleEndian" header_type="UInt32">', &
    '  <ImageData WholeExtent="0 4 0 0 0 0" Origin="0 0 0" Spacing="1 1 1">', &
    '    <Piece Extent="0 4 0 0 0 0">', '      <PointData>', &
    '        <DataArray type="Bit" Name="b" format="binary">AQAAAKg=</DataArray>', &
    '      </PointData>', '      <CellData>', '      </CellData>', '    </Piece>', '  </ImageData>', '</VTKFile>'
  close(u)
  e = 0
  e(1) = r%initialize(filename='vtkfortran_read_bit_image.vti', action='read')
  e(1) = e(1) + abs(r%xml_reader%read_dataarray(location='node', data_name='b', x=bits))
  e(1) = e(1) + abs(r%xml_reader%get_dataarray_info(location='node', data_name='b', n_tuples=n))
  if (e(1) == 0) e(1) = merge(0, 1, size(bits) == 5 .and. n == 5_I8P)
  if (e(1) == 0) e(1) = merge(0, 1, all(bits == [1_I1P, 0_I1P, 1_I1P, 0_I1P, 1_I1P]))
  e(2) = r%finalize()
  e(3) = w%initialize(format='ascii', filename='vtkfortran_read_names_refused.vtt', mesh_topology='Table')
  e(3) = e(3) + abs(w%xml_writer%write_dataarray(location='row', action='open'))
  e(3) = e(3) + abs(w%xml_writer%write_dataarray(data_name='a', x=[1._R8P, 2._R8P, 3._R8P]))
  e(3) = e(3) + abs(w%xml_writer%set_component_names(['u', 'v']))
  e(3) = e(3) + merge(0, 1, w%xml_writer%write_dataarray(data_name='r', x=[1._R8P, 2._R8P]) /= 0) ! 2 rows: refused
  e(3) = e(3) + abs(w%xml_writer%write_dataarray(data_name='c', x=[4._R8P, 5._R8P, 6._R8P]))
  e(3) = e(3) + abs(w%xml_writer%write_dataarray(location='row', action='close'))
  if (w%finalize() /= 0) continue ! the refusal is reported again
  e(4) = r%initialize(filename='vtkfortran_read_names_refused.vtt', action='read')
  e(4) = e(4) + abs(r%xml_reader%get_dataarray_info(location='row', data_name='c', component_names=names))
  if (e(4) == 0) e(4) = merge(0, 1, size(names) == 1 .and. len(names) == 0)
  if (r%finalize() /= 0) continue
  check_review_fixes = all(e == 0)
  if (.not.check_review_fixes) print '(A,4(1X,I0))', 'failed: review fixes (bits of an image, component names)', e
  endfunction check_review_fixes

  logical function check_big_endian()
  !< BigEndian files written by VTK 9.7.2 (vtkXMLUnstructuredGridWriter, SetByteOrderToBigEndian): a tetrahedron with
  !< Float64, Int32 and 3-component UInt16 point data and Float32 cell data; inline binary (UInt32 headers), appended raw
  !< (UInt64 headers) and, with zlib, appended base64 and appended raw (UInt32). Their information keys and ranges are
  !< left out; the appended raw data are written as bytes (hex).
  character(*), parameter       :: nl = new_line('a')
  character(32)                 :: files(4) !< The files.
  type(vtk_file)                :: r        !< Reader.
  real(R8P),    allocatable     :: pd(:), xx(:), yy(:), zz(:) !< Arrays read.
  real(R4P),    allocatable     :: cf(:)    !< Cell data read.
  integer(I4P), allocatable     :: ip(:), us(:,:) !< Arrays read.
  integer(I8P), allocatable     :: conn(:), offs(:) !< Cells read.
  integer(I1P), allocatable     :: types(:) !< Cell types read.
  integer(I4P)                  :: u, k, n, e !< Unit, counters, error.

  ! be_binary_none_32.vtu
  open(newunit=u, file='vtkfortran_read_be_binary.vtu', status='replace', action='write')
  write(u, '(A)') &
    '<?xml version="1.0"?>', &
    '<VTKFile type="UnstructuredGrid" version="0.1" byte_order="BigEndian" header_type="UInt32"' // &
    '>', &
    '  <UnstructuredGrid>', &
    '    <Piece NumberOfPoints="4" NumberOfCells="1">', &
    '      <PointData>', &
    '        <DataArray type="Float64" Name="pd" format="binary">', &
    '          AAAAID/4AAAAAAAAwAIAAAAAAABCG/COsAAAAL37fN/Z1727', &
    '        </DataArray>', &
    '        <DataArray type="Int32" Name="ip" format="binary">', &
    '          AAAAEAAAAAf////4AAAACf////Y=', &
    '        </DataArray>', &
    '        <DataArray type="UInt16" Name="us" NumberOfComponents="3" format="binary">', &
    '          AAAAGAABAAIAAwAEAAUABgAHAAgACf//AAAAAQ==', &
    '        </DataArray>', &
    '      </PointData>', &
    '      <CellData>', &
    '        <DataArray type="Float32" Name="cf" format="binary">', &
    '          AAAABEAgAAA=', &
    '        </DataArray>', &
    '      </CellData>', &
    '      <Points>', &
    '        <DataArray type="Float64" Name="Points" NumberOfComponents="3" format="binary">', &
    '          AAAAYAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAD/wAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA' // &
    'P/AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA/8AAAAAAAAA==', &
    '        </DataArray>', &
    '      </Points>', &
    '      <Cells>', &
    '        <DataArray type="Int64" Name="connectivity" format="binary">', &
    '          AAAAIAAAAAAAAAAAAAAAAAAAAAEAAAAAAAAAAgAAAAAAAAAD', &
    '        </DataArray>', &
    '        <DataArray type="Int64" Name="offsets" format="binary">', &
    '          AAAACAAAAAAAAAAE', &
    '        </DataArray>', &
    '        <DataArray type="UInt8" Name="types" format="binary">', &
    '          AAAAAQo=', &
    '        </DataArray>', &
    '      </Cells>', &
    '    </Piece>', &
    '  </UnstructuredGrid>', &
    '</VTKFile>'
  close(u)
  ! be_appended-b64_zlib_32.vtu
  open(newunit=u, file='vtkfortran_read_be_b64zlib.vtu', status='replace', action='write')
  write(u, '(A)') &
    '<?xml version="1.0"?>', &
    '<VTKFile type="UnstructuredGrid" version="0.1" byte_order="BigEndian" header_type="UInt32"' // &
    ' compressor="vtkZLibDataCompressor">', &
    '  <UnstructuredGrid>', &
    '    <Piece NumberOfPoints="4"                    NumberOfCells="1"                   >', &
    '      <PointData>', &
    '        <DataArray type="Float64" Name="pd" format="appended" RangeMin="-2.25"            ' // &
    '    RangeMax="30000000000"          offset="0"                   >', &
    '        </DataArray>', &
    '        <DataArray type="Int32" Name="ip" format="appended" RangeMin="-10"                ' // &
    '  RangeMax="9"                    offset="68"                  >', &
    '        </DataArray>', &
    '        <DataArray type="UInt16" Name="us" NumberOfComponents="3" format="appended" RangeM' // &
    'in="3.7416573868"         RangeMax="65535.000008"         offset="124"                 >', &
    '        </DataArray>', &
    '      </PointData>', &
    '      <CellData>', &
    '        <DataArray type="Float32" Name="cf" format="appended" RangeMin="2.5"              ' // &
    '    RangeMax="2.5"                  offset="188"                 >', &
    '        </DataArray>', &
    '      </CellData>', &
    '      <Points>', &
    '        <DataArray type="Float64" Name="Points" NumberOfComponents="3" format="appended" R' // &
    'angeMin="0"                    RangeMax="1"                    offset="228"               ' // &
    '  >', &
    '        </DataArray>', &
    '      </Points>', &
    '      <Cells>', &
    '        <DataArray type="Int64" Name="connectivity" format="appended" RangeMin=""         ' // &
    '            RangeMax=""                     offset="276"                 />', &
    '        <DataArray type="Int64" Name="offsets" format="appended" RangeMin=""              ' // &
    '       RangeMax=""                     offset="324"                 />', &
    '        <DataArray type="UInt8" Name="types" format="appended" RangeMin=""                ' // &
    '     RangeMax=""                     offset="364"                 >', &
    '        </DataArray>', &
    '      </Cells>', &
    '    </Piece>', &
    '  </UnstructuredGrid>', &
    '  <AppendedData encoding="base64">', &
    '   _AAAAAQAAgAAAAAAgAAAAIA==eJyz/8EABgeYILST9Ie+DUB67++a+zev790NAHanCsA=AAAAAQAAgAAAAAAQAA' // &
    'AAFw==eJxjYGBg/////w8GBgZOIP0NADQcB/k=AAAAAQAAgAAAAAAYAAAAHg==eJwFwYEBABAIALCFiv8PziYs25FK' // &
    'u94M4gMNOQItAAAAAQAAgAAAAAAEAAAADA==eJxzUGBgAAABZABhAAAAAQAAgAAAAABgAAAAEg==eJxjYMAO7D/gkC' // &
    'BRHgCLmAOOAAAAAQAAgAAAAAAgAAAAEg==eJxjYEABjFCaCUozAwAARgAHAAAAAQAAgAAAAAAIAAAADA==eJxjYAAD' // &
    'FgAADAAFAAAAAQAAgAAAAAABAAAACQ==eJzjAgAACwAL', &
    '  </AppendedData>', &
    '</VTKFile>'
  close(u)
  ! be_appended-raw_none_64.vtu: the XML, the raw appended data (hex), the end
  open(newunit=u, file='vtkfortran_read_be_raw64.vtu', status='replace', action='write', access='stream', &
       form='unformatted')
  write(u) &
    '<VTKFile type="UnstructuredGrid" version="1.0" byte_order="BigEndian" header_type="UInt64"' // &
    '>' // nl // &
    '  <UnstructuredGrid>' // nl // &
    '    <Piece NumberOfPoints="4"                    NumberOfCells="1"                   >' // nl // &
    '      <PointData>' // nl // &
    '        <DataArray type="Float64" Name="pd" format="appended" RangeMin="-2.25"            ' // &
    '    RangeMax="30000000000"          offset="0"                   >' // nl // &
    '        </DataArray>' // nl // &
    '        <DataArray type="Int32" Name="ip" format="appended" RangeMin="-10"                ' // &
    '  RangeMax="9"                    offset="40"                  >' // nl // &
    '        </DataArray>' // nl // &
    '        <DataArray type="UInt16" Name="us" NumberOfComponents="3" format="appended" RangeM' // &
    'in="3.7416573868"         RangeMax="65535.000008"         offset="64"                  >' // nl // &
    '        </DataArray>' // nl // &
    '      </PointData>' // nl // &
    '      <CellData>' // nl // &
    '        <DataArray type="Float32" Name="cf" format="appended" RangeMin="2.5"              ' // &
    '    RangeMax="2.5"                  offset="96"                  >' // nl // &
    '        </DataArray>' // nl // &
    '      </CellData>' // nl // &
    '      <Points>' // nl // &
    '        <DataArray type="Float64" Name="Points" NumberOfComponents="3" format="appended" R' // &
    'angeMin="0"                    RangeMax="1"                    offset="108"               ' // &
    '  >' // nl // &
    '        </DataArray>' // nl // &
    '      </Points>' // nl // &
    '      <Cells>' // nl // &
    '        <DataArray type="Int64" Name="connectivity" format="appended" RangeMin=""         ' // &
    '            RangeMax=""                     offset="212"                 />' // nl // &
    '        <DataArray type="Int64" Name="offsets" format="appended" RangeMin=""              ' // &
    '       RangeMax=""                     offset="252"                 />' // nl // &
    '        <DataArray type="UInt8" Name="types" format="appended" RangeMin=""                ' // &
    '     RangeMax=""                     offset="268"                 >' // nl // &
    '        </DataArray>' // nl // &
    '      </Cells>' // nl // &
    '    </Piece>' // nl // &
    '  </UnstructuredGrid>' // nl // &
    '  <AppendedData encoding="raw">' // nl // &
    '   _'
  call write_hex(u, &
    '00000000000000203ff8000000000000c002000000000000421bf08eb0000000bdfb7cdfd9d7bdbb0000000000000010' // &
    '00000007fffffff800000009fffffff60000000000000018000100020003000400050006000700080009ffff00000001' // &
    '00000000000000044020000000000000000000600000000000000000000000000000000000000000000000003ff00000' // &
    '000000000000000000000000000000000000000000000000000000003ff0000000000000000000000000000000000000' // &
    '0000000000000000000000003ff000000000000000000000000000200000000000000000000000000000000100000000' // &
    '0000000200000000000000030000000000000008000000000000000400000000000000010a0a2020')
  write(u) nl // '  </AppendedData>' // nl // '</VTKFile>' // nl
  close(u)
  ! be_appended-raw_zlib_32.vtu: the XML, the raw appended data (hex), the end
  open(newunit=u, file='vtkfortran_read_be_rawzlib.vtu', status='replace', action='write', access='stream', &
       form='unformatted')
  write(u) &
    '<VTKFile type="UnstructuredGrid" version="0.1" byte_order="BigEndian" header_type="UInt32"' // &
    ' compressor="vtkZLibDataCompressor">' // nl // &
    '  <UnstructuredGrid>' // nl // &
    '    <Piece NumberOfPoints="4"                    NumberOfCells="1"                   >' // nl // &
    '      <PointData>' // nl // &
    '        <DataArray type="Float64" Name="pd" format="appended" RangeMin="-2.25"            ' // &
    '    RangeMax="30000000000"          offset="0"                   >' // nl // &
    '        </DataArray>' // nl // &
    '        <DataArray type="Int32" Name="ip" format="appended" RangeMin="-10"                ' // &
    '  RangeMax="9"                    offset="48"                  >' // nl // &
    '        </DataArray>' // nl // &
    '        <DataArray type="UInt16" Name="us" NumberOfComponents="3" format="appended" RangeM' // &
    'in="3.7416573868"         RangeMax="65535.000008"         offset="87"                  >' // nl // &
    '        </DataArray>' // nl // &
    '      </PointData>' // nl // &
    '      <CellData>' // nl // &
    '        <DataArray type="Float32" Name="cf" format="appended" RangeMin="2.5"              ' // &
    '    RangeMax="2.5"                  offset="133"                 >' // nl // &
    '        </DataArray>' // nl // &
    '      </CellData>' // nl // &
    '      <Points>' // nl // &
    '        <DataArray type="Float64" Name="Points" NumberOfComponents="3" format="appended" R' // &
    'angeMin="0"                    RangeMax="1"                    offset="161"               ' // &
    '  >' // nl // &
    '        </DataArray>' // nl // &
    '      </Points>' // nl // &
    '      <Cells>' // nl // &
    '        <DataArray type="Int64" Name="connectivity" format="appended" RangeMin=""         ' // &
    '            RangeMax=""                     offset="195"                 />' // nl // &
    '        <DataArray type="Int64" Name="offsets" format="appended" RangeMin=""              ' // &
    '       RangeMax=""                     offset="229"                 />' // nl // &
    '        <DataArray type="UInt8" Name="types" format="appended" RangeMin=""                ' // &
    '     RangeMax=""                     offset="257"                 >' // nl // &
    '        </DataArray>' // nl // &
    '      </Cells>' // nl // &
    '    </Piece>' // nl // &
    '  </UnstructuredGrid>' // nl // &
    '  <AppendedData encoding="raw">' // nl // &
    '   _'
  call write_hex(u, &
    '00000001000080000000002000000020789cb3ffc10006079820b493f487be0d407aefef9afb37afefdd0d0076a70ac0' // &
    '00000001000080000000001000000017789c63606060ffffffff0f0606064e20fd0d00341c07f9000000010000800000' // &
    '0000180000001e789c05c1810100100800b0858aff0fce262cdb914abbde0ce2030d39022d0000000100008000000000' // &
    '040000000c789c7350606000000164006100000001000080000000006000000012789c6360c00eec3fe09020511e008b' // &
    '98038e00000001000080000000002000000012789c636040018c509a094a330300004600070000000100008000000000' // &
    '080000000c789c636000031600000c000500000001000080000000000100000009789ce30200000b000b0a2020')
  write(u) nl // '  </AppendedData>' // nl // '</VTKFile>' // nl
  close(u)
  files = [character(32) :: 'vtkfortran_read_be_binary.vtu', 'vtkfortran_read_be_raw64.vtu', &
           'vtkfortran_read_be_b64zlib.vtu', 'vtkfortran_read_be_rawzlib.vtu']
  n = 2
#ifdef VTKFORTRAN_USE_ZLIB
  n = 4
#endif
  check_big_endian = .true.
  do k=1, n
    e = r%initialize(filename=trim(files(k)), action='read')
    e = e + abs(r%xml_reader%read_dataarray(location='node', data_name='pd', x=pd))
    e = e + abs(r%xml_reader%read_dataarray(location='node', data_name='ip', x=ip))
    e = e + abs(r%xml_reader%read_dataarray(location='node', data_name='us', x=us))
    e = e + abs(r%xml_reader%read_dataarray(location='cell', data_name='cf', x=cf))
    e = e + abs(r%xml_reader%read_geo(x=xx, y=yy, z=zz))
    e = e + abs(r%xml_reader%read_connectivity(connectivity=conn, offset=offs, cell_type=types))
    if (e == 0) then
      if (.not.(all(pd == [1.5_R8P, -2.25_R8P, 3.e10_R8P, -4.e-10_R8P]) .and. all(ip == [7, -8, 9, -10]) .and. &
                all(us(:,1) == [1, 2, 3]) .and. all(us(:,4) == [65535, 0, 1]) .and. all(cf == [2.5_R4P]) .and. &
                all(xx == x) .and. all(yy == y) .and. all(zz == z) .and. all(conn == [0_I8P, 1_I8P, 2_I8P, 3_I8P]) .and. &
                all(offs == [4_I8P]) .and. all(types == [10_I1P]))) e = 1
    endif
    if (e /= 0) print '(A,I0)', 'failed: '//trim(files(k))//', error ', e
    check_big_endian = check_big_endian .and. e == 0
    e = r%finalize()
  enddo
  endfunction check_big_endian

  subroutine write_hex(unit, hex)
  !< Write the bytes of a hexadecimal string.
  integer(I4P), intent(in) :: unit     !< Unit, stream access.
  character(*), intent(in) :: hex      !< Bytes, two hexadecimal digits each.
  integer(I4P)             :: b, value !< Counter, a byte.

  do b=1, len(hex) - 1, 2
    read(hex(b:b+1), '(Z2)') value
    write(unit) int(merge(value - 256, value, value > 127), I1P)
  enddo
  endsubroutine write_hex
endprogram vtk_fortran_read
