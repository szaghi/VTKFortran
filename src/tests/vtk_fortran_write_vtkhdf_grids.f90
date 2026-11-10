!< VTK_Fortran test: write RectilinearGrid and StructuredGrid in the VTKHDF format (`format='vtkhdf'`).
program vtk_fortran_write_vtkhdf_grids
!< VTK_Fortran test: write RectilinearGrid and StructuredGrid in the VTKHDF format (`format='vtkhdf'`).
!<
!< With HDF5 the test writes six grids, each one also as a binary `.vtr` or `.vts` by the same calls (VTK readers compare
!< the two outside this test):
!<
!<- `vtkfortran_rect_3d`: a RectilinearGrid of non-uniform R8P coordinates, point (scalar and vector) and cell data, active
!<  arrays, field data;
!<- `vtkfortran_rect_2d`: a 2D RectilinearGrid (one point along z) of R4P coordinates, whose extent does not start at 0,
!<  compressed if HDF5 has deflate;
!<- `vtkfortran_strg_rank4`: a 3D StructuredGrid, the points as `xyz(3,0:ni,0:nj,0:nk)` (R8P), point and cell data;
!<- `vtkfortran_strg_rank1`: a 2D StructuredGrid, the points as `n, x(n), y(n), z(n)` (R4P);
!<- `vtkfortran_strg_rank3`: a 1D StructuredGrid (a curve), the points as `n, x(:,:,:), y(:,:,:), z(:,:,:)` (R8P);
!<- `vtkfortran_strg_rank2`: a 3D StructuredGrid, the points as `xyz(3,n)` (R4P).
!<
!< Two files are read back with the HDF5 bindings (Type, Version, Dimensions, coordinates, points, shapes of the arrays).
!< Refusals are checked too. Without HDF5 the writer refuses to start.
use penf
use vtk_fortran, only : vtk_file
use vtk_fortran_hdf5

implicit none
type(vtk_file)                :: a_vtk_file                 !< VTK file.
integer(hid_kind)             :: file_id, root              !< HDF5 identifiers.
character(len=:), allocatable :: text                       !< String read back.
integer(I8P),     allocatable :: ivalues(:), dims(:)        !< Values read back.
real(R8P),        allocatable :: rvalues(:)                 !< Values read back.
integer(I4P)                  :: e(40)                      !< Errors of each call.
integer(I4P)                  :: f                          !< Counter.
logical                       :: test_passed(6)             !< List of passed tests.
character(16), parameter      :: formats(2) = ['vtkhdf          ', 'binary          '] !< Formats of each grid.

test_passed = .false.
if (.not.is_hdf5_enabled) then
  test_passed = a_vtk_file%initialize(format='vtkhdf', filename='refused.vtkhdf', mesh_topology='StructuredGrid', &
                                      nx1=0, nx2=1, ny1=0, ny2=1, nz1=0, nz2=1) /= 0
  print '(A)', 'HDF5 not enabled: the VTKHDF writer refuses to start'
else
  e = 0
  do f=1, 2
    e(f)    = write_rect_3d(trim(formats(f)), 'vtkfortran_rect_3d'//trim(extension(f, '.vtr')))
    e(2+f)  = write_rect_2d(trim(formats(f)), 'vtkfortran_rect_2d'//trim(extension(f, '.vtr')))
    e(4+f)  = write_strg_rank4(trim(formats(f)), 'vtkfortran_strg_rank4'//trim(extension(f, '.vts')))
    e(6+f)  = write_strg_rank1(trim(formats(f)), 'vtkfortran_strg_rank1'//trim(extension(f, '.vts')))
    e(8+f)  = write_strg_rank3(trim(formats(f)), 'vtkfortran_strg_rank3'//trim(extension(f, '.vts')))
    e(10+f) = write_strg_rank2(trim(formats(f)), 'vtkfortran_strg_rank2'//trim(extension(f, '.vts')))
  enddo
  test_passed(1) = all(e(1:12) == 0)
  if (.not.test_passed(1)) print '(A,12I2)', 'errors of the grids (vtkhdf, xml for each): ', e(1:12)
  test_passed(2) = refusals()

  ! read back the 3D RectilinearGrid
  e = 0
  e(1) = h5_file_open('vtkfortran_rect_3d.vtkhdf', file_id)
  e(2) = h5_group_open(file_id, 'VTKHDF', root)
  e(3) = h5_attribute_read(root, 'Type', text)
  e(4) = h5_attribute_read(root, 'Version', ivalues)
  test_passed(3) = text == 'RectilinearGrid' .and. all(ivalues == [2_I8P, 7_I8P]) .and. .not.h5_link_exists(root, 'Points')
  e(5) = h5_attribute_read(root, 'Dimensions', ivalues)
  test_passed(3) = test_passed(3) .and. all(ivalues == [4_I8P, 3_I8P, 2_I8P])
  e(6) = h5_dataset_read(root, 'XCoordinates', rvalues)
  test_passed(4) = all(rvalues == [0._R8P, 1._R8P, 3._R8P, 6._R8P])
  e(7) = h5_dataset_read(root, 'ZCoordinates', rvalues)
  test_passed(4) = test_passed(4) .and. all(rvalues == [0._R8P, 0.5_R8P])
  e(8) = h5_dataset_dims(root, 'PointData/velocity', dims)
  test_passed(4) = test_passed(4) .and. all(dims == [3_I8P, 4_I8P, 3_I8P, 2_I8P])
  e(9) = h5_dataset_dims(root, 'CellData/cid', dims)
  test_passed(4) = test_passed(4) .and. all(dims == [3_I8P, 2_I8P, 1_I8P])
  e(10) = h5_group_close(root)
  e(11) = h5_file_close(file_id)
  ! read back the 2D StructuredGrid: Points keeps its axis of one point, the arrays drop it
  e(12) = h5_file_open('vtkfortran_strg_rank1.vtkhdf', file_id)
  e(13) = h5_group_open(file_id, 'VTKHDF', root)
  e(14) = h5_attribute_read(root, 'Type', text)
  e(15) = h5_attribute_read(root, 'Dimensions', ivalues)
  test_passed(5) = text == 'StructuredGrid' .and. all(ivalues == [5_I8P, 4_I8P, 1_I8P])
  e(16) = h5_dataset_dims(root, 'Points', dims)
  test_passed(5) = test_passed(5) .and. all(dims == [3_I8P, 5_I8P, 4_I8P, 1_I8P])
  e(17) = h5_dataset_dims(root, 'PointData/pd', dims)
  test_passed(5) = test_passed(5) .and. all(dims == [5_I8P, 4_I8P])
  e(18) = h5_dataset_read(root, 'Points', rvalues)
  test_passed(5) = test_passed(5) .and. all(abs(rvalues(4:6) - [1.2_R8P, 0.2_R8P, 0._R8P]) < 1.e-6_R8P)
  e(19) = h5_group_close(root)
  e(20) = h5_file_close(file_id)
  test_passed(6) = all(e(1:20) == 0)
  print '(A,6L2)', 'VTKHDF RectilinearGrid, StructuredGrid: grids, refusals, header, rect, strg, reads:', test_passed
endif

print "(A,L1)", new_line('a')//'Are all tests passed? ', all(test_passed)
if (.not.all(test_passed)) error stop 'some tests failed'
stop
contains
  pure function extension(f, xml) result(ext)
  !< The extension of the format `f`: .vtkhdf, or the XML one.
  integer(I4P), intent(in) :: f   !< Format index.
  character(*), intent(in) :: xml !< Extension of the XML format.
  character(7)             :: ext !< Extension.

  ext = '.vtkhdf'
  if (f == 2) ext = xml
  endfunction extension

  function write_rect_3d(format, filename) result(error)
  !< A 3D RectilinearGrid, 4x3x2 points of non-uniform coordinates.
  character(*), intent(in) :: format   !< Format.
  character(*), intent(in) :: filename !< File name.
  integer(I4P)             :: error    !< Error status.
  real(R8P)                :: velocity(3,0:3,0:2,0:1)
  integer(I4P)             :: i, j, k

  do k=0, 1 ; do j=0, 2 ; do i=0, 3
    velocity(:,i,j,k) = [real(i, R8P), real(j, R8P), real(k, R8P)]
  enddo ; enddo ; enddo
  error = a_vtk_file%initialize(format=format, filename=filename, mesh_topology='RectilinearGrid', &
                                nx1=0, nx2=3, ny1=0, ny2=2, nz1=0, nz2=1)
  error = max(error, abs(a_vtk_file%xml_writer%write_fielddata(action='open')))
  error = max(error, abs(a_vtk_file%xml_writer%write_fielddata(data_name='time', x=2.5_R8P)))
  error = max(error, abs(a_vtk_file%xml_writer%write_fielddata(action='close')))
  error = max(error, abs(a_vtk_file%xml_writer%write_piece(nx1=0, nx2=3, ny1=0, ny2=2, nz1=0, nz2=1)))
  error = max(error, abs(a_vtk_file%xml_writer%write_geo(x=[0._R8P, 1._R8P, 3._R8P, 6._R8P], y=[0._R8P, 1._R8P, 2._R8P], &
                                                         z=[0._R8P, 0.5_R8P])))
  error = max(error, abs(a_vtk_file%xml_writer%write_dataarray(location='node', action='open', scalars='pd', &
                                                               vectors='velocity')))
  error = max(error, abs(a_vtk_file%xml_writer%write_dataarray(data_name='pd', x=[(real(i, R8P), i=1, 24)])))
  error = max(error, abs(a_vtk_file%xml_writer%write_dataarray(data_name='velocity', x=velocity)))
  error = max(error, abs(a_vtk_file%xml_writer%write_dataarray(location='node', action='close')))
  error = max(error, abs(a_vtk_file%xml_writer%write_dataarray(location='cell', action='open')))
  error = max(error, abs(a_vtk_file%xml_writer%write_dataarray(data_name='cid', x=[(i, i=1, 6)])))
  error = max(error, abs(a_vtk_file%xml_writer%write_dataarray(location='cell', action='close')))
  error = max(error, abs(a_vtk_file%xml_writer%write_piece()))
  error = max(error, abs(a_vtk_file%finalize()))
  endfunction write_rect_3d

  function write_rect_2d(format, filename) result(error)
  !< A 2D RectilinearGrid (one point along z), extent 2:5 x 0:2 x 0:0, R4P coordinates, compressed if possible.
  character(*), intent(in) :: format   !< Format.
  character(*), intent(in) :: filename !< File name.
  integer(I4P)             :: error    !< Error status.
  integer(I4P)             :: i

  if (format == 'vtkhdf' .and. h5_deflate_available()) then
    error = a_vtk_file%initialize(format=format, filename=filename, mesh_topology='RectilinearGrid', &
                                  nx1=2, nx2=5, ny1=0, ny2=2, nz1=0, nz2=0, compressor='zlib')
  else
    error = a_vtk_file%initialize(format=format, filename=filename, mesh_topology='RectilinearGrid', &
                                  nx1=2, nx2=5, ny1=0, ny2=2, nz1=0, nz2=0)
  endif
  error = max(error, abs(a_vtk_file%xml_writer%write_piece(nx1=2, nx2=5, ny1=0, ny2=2, nz1=0, nz2=0)))
  error = max(error, abs(a_vtk_file%xml_writer%write_geo(x=[2., 2.5, 3.5, 5.], y=[0., 1., 4.], z=[1.])))
  error = max(error, abs(a_vtk_file%xml_writer%write_dataarray(location='node', action='open')))
  error = max(error, abs(a_vtk_file%xml_writer%write_dataarray(data_name='pd', x=[(real(i, R4P), i=1, 12)])))
  error = max(error, abs(a_vtk_file%xml_writer%write_dataarray(location='node', action='close')))
  error = max(error, abs(a_vtk_file%xml_writer%write_dataarray(location='cell', action='open')))
  error = max(error, abs(a_vtk_file%xml_writer%write_dataarray(data_name='cid', x=[(i, i=1, 6)])))
  error = max(error, abs(a_vtk_file%xml_writer%write_dataarray(location='cell', action='close')))
  error = max(error, abs(a_vtk_file%xml_writer%write_piece()))
  error = max(error, abs(a_vtk_file%finalize()))
  endfunction write_rect_2d

  function write_strg_rank4(format, filename) result(error)
  !< A 3D StructuredGrid (a sheared box, 4x3x2 points), points as xyz(3,0:3,0:2,0:1), point and cell data.
  character(*), intent(in) :: format   !< Format.
  character(*), intent(in) :: filename !< File name.
  integer(I4P)             :: error    !< Error status.
  real(R8P)                :: xyz(3,0:3,0:2,0:1)
  integer(I4P)             :: i, j, k

  do k=0, 1 ; do j=0, 2 ; do i=0, 3
    xyz(:,i,j,k) = [i + 0.25_R8P * j, real(j, R8P), k + 0.1_R8P * i]
  enddo ; enddo ; enddo
  error = a_vtk_file%initialize(format=format, filename=filename, mesh_topology='StructuredGrid', &
                                nx1=0, nx2=3, ny1=0, ny2=2, nz1=0, nz2=1)
  error = max(error, abs(a_vtk_file%xml_writer%write_piece(nx1=0, nx2=3, ny1=0, ny2=2, nz1=0, nz2=1)))
  error = max(error, abs(a_vtk_file%xml_writer%write_geo(xyz=xyz)))
  error = max(error, abs(a_vtk_file%xml_writer%write_dataarray(location='node', action='open', scalars='height')))
  error = max(error, abs(a_vtk_file%xml_writer%write_dataarray(data_name='height', x=xyz(3,:,:,:), one_component=.true.)))
  error = max(error, abs(a_vtk_file%xml_writer%write_dataarray(location='node', action='close')))
  error = max(error, abs(a_vtk_file%xml_writer%write_dataarray(location='cell', action='open')))
  error = max(error, abs(a_vtk_file%xml_writer%write_dataarray(data_name='cid', x=[(i, i=1, 6)])))
  error = max(error, abs(a_vtk_file%xml_writer%write_dataarray(location='cell', action='close')))
  error = max(error, abs(a_vtk_file%xml_writer%write_piece()))
  error = max(error, abs(a_vtk_file%finalize()))
  endfunction write_strg_rank4

  function write_strg_rank1(format, filename) result(error)
  !< A 2D StructuredGrid (5x4x1 points, a quarter of an annulus), points as n, x(n), y(n), z(n) (R4P).
  character(*), intent(in) :: format   !< Format.
  character(*), intent(in) :: filename !< File name.
  integer(I4P)             :: error    !< Error status.
  real(R4P)                :: x(20), y(20), z(20), r, t
  integer(I4P)             :: i, j, p

  p = 0
  do j=0, 3 ; do i=0, 4
    p = p + 1
    r = 1. + 0.2 * i ; t = 0.5 * acos(-1.) * j / 3.
    x(p) = r * cos(t) ; y(p) = r * sin(t) ; z(p) = 0.
  enddo ; enddo
  x(2) = 1.2 ; y(2) = 0.2 ! the second point moved off the circle, checked when read back
  error = a_vtk_file%initialize(format=format, filename=filename, mesh_topology='StructuredGrid', &
                                nx1=0, nx2=4, ny1=0, ny2=3, nz1=0, nz2=0)
  error = max(error, abs(a_vtk_file%xml_writer%write_piece(nx1=0, nx2=4, ny1=0, ny2=3, nz1=0, nz2=0)))
  error = max(error, abs(a_vtk_file%xml_writer%write_geo(n=20, x=x, y=y, z=z)))
  error = max(error, abs(a_vtk_file%xml_writer%write_dataarray(location='node', action='open')))
  error = max(error, abs(a_vtk_file%xml_writer%write_dataarray(data_name='pd', x=[(real(i, R4P), i=1, 20)])))
  error = max(error, abs(a_vtk_file%xml_writer%write_dataarray(location='node', action='close')))
  error = max(error, abs(a_vtk_file%xml_writer%write_piece()))
  error = max(error, abs(a_vtk_file%finalize()))
  endfunction write_strg_rank1

  function write_strg_rank3(format, filename) result(error)
  !< A 1D StructuredGrid (a curve of 6 points), points as n, x(:,:,:), y(:,:,:), z(:,:,:) (R8P).
  character(*), intent(in) :: format   !< Format.
  character(*), intent(in) :: filename !< File name.
  integer(I4P)             :: error    !< Error status.
  real(R8P)                :: x(0:5,0:0,0:0), y(0:5,0:0,0:0), z(0:5,0:0,0:0)
  integer(I4P)             :: i

  do i=0, 5
    x(i,0,0) = real(i, R8P) ; y(i,0,0) = real(i * i, R8P) / 5._R8P ; z(i,0,0) = 0._R8P
  enddo
  error = a_vtk_file%initialize(format=format, filename=filename, mesh_topology='StructuredGrid', &
                                nx1=0, nx2=5, ny1=0, ny2=0, nz1=0, nz2=0)
  error = max(error, abs(a_vtk_file%xml_writer%write_piece(nx1=0, nx2=5, ny1=0, ny2=0, nz1=0, nz2=0)))
  error = max(error, abs(a_vtk_file%xml_writer%write_geo(n=6, x=x, y=y, z=z)))
  error = max(error, abs(a_vtk_file%xml_writer%write_dataarray(location='cell', action='open')))
  error = max(error, abs(a_vtk_file%xml_writer%write_dataarray(data_name='cid', x=[(i, i=1, 5)])))
  error = max(error, abs(a_vtk_file%xml_writer%write_dataarray(location='cell', action='close')))
  error = max(error, abs(a_vtk_file%xml_writer%write_piece()))
  error = max(error, abs(a_vtk_file%finalize()))
  endfunction write_strg_rank3

  function write_strg_rank2(format, filename) result(error)
  !< A 3D StructuredGrid (a unit cube of 2x2x2 points), points as xyz(3,n) (R4P), x fastest.
  character(*), intent(in) :: format   !< Format.
  character(*), intent(in) :: filename !< File name.
  integer(I4P)             :: error    !< Error status.
  real(R4P)                :: xyz(3,8)

  xyz = reshape([0., 0., 0.,  1., 0., 0.,  0., 1., 0.,  1., 1., 0.,  0., 0., 1.,  1., 0., 1.,  0., 1., 1.,  1., 1., 1.], [3, 8])
  error = a_vtk_file%initialize(format=format, filename=filename, mesh_topology='StructuredGrid', &
                                nx1=0, nx2=1, ny1=0, ny2=1, nz1=0, nz2=1)
  error = max(error, abs(a_vtk_file%xml_writer%write_piece(nx1=0, nx2=1, ny1=0, ny2=1, nz1=0, nz2=1)))
  error = max(error, abs(a_vtk_file%xml_writer%write_geo(xyz=xyz)))
  error = max(error, abs(a_vtk_file%xml_writer%write_dataarray(location='cell', action='open')))
  error = max(error, abs(a_vtk_file%xml_writer%write_dataarray(data_name='cid', x=[7_I4P])))
  error = max(error, abs(a_vtk_file%xml_writer%write_dataarray(location='cell', action='close')))
  error = max(error, abs(a_vtk_file%xml_writer%write_piece()))
  error = max(error, abs(a_vtk_file%finalize()))
  endfunction write_strg_rank2

  logical function refusals()
  !< The refusals of the RectilinearGrid and StructuredGrid writers, each one a non-zero error.
  integer(I4P) :: error
  logical      :: ok(8)
  real(R8P)    :: two(2) = [0._R8P, 1._R8P]

  ok = .false.
  error = a_vtk_file%initialize(format='vtkhdf', filename='refused_rect.vtkhdf', mesh_topology='RectilinearGrid', &
                                nx1=0, nx2=1, ny1=0, ny2=1, nz1=0, nz2=1)
  ok(1) = a_vtk_file%xml_writer%write_piece(nx1=0, nx2=0, ny1=0, ny2=1, nz1=0, nz2=1) /= 0 ! not the whole extent
  error = a_vtk_file%xml_writer%write_piece(nx1=0, nx2=1, ny1=0, ny2=1, nz1=0, nz2=1)
  ok(2) = a_vtk_file%xml_writer%write_geo(x=two, y=two, z=[0._R8P]) /= 0                  ! z of the wrong size
  ok(3) = a_vtk_file%xml_writer%write_geo(xyz=reshape([two, two, two, two], [2, 4])) /= 0 ! the points of a StructuredGrid
  ok(4) = a_vtk_file%xml_writer%write_piece() /= 0                                       ! no geometry
  error = a_vtk_file%finalize()
  error = a_vtk_file%initialize(format='vtkhdf', filename='refused_strg.vtkhdf', mesh_topology='StructuredGrid', &
                                nx1=0, nx2=1, ny1=0, ny2=1, nz1=0, nz2=0)
  error = a_vtk_file%xml_writer%write_piece(nx1=0, nx2=1, ny1=0, ny2=1, nz1=0, nz2=0)
  ok(5) = a_vtk_file%xml_writer%write_geo(x=two, y=two, z=two) /= 0                       ! coordinates of a RectilinearGrid
  ok(6) = a_vtk_file%xml_writer%write_geo(n=2, x=two, y=two, z=two) /= 0                  ! 2 points, the grid has 4
  error = a_vtk_file%xml_writer%write_geo(n=4, x=[two, two], y=[0._R8P, 0._R8P, 1._R8P, 1._R8P], z=[two, two] * 0._R8P)
  ok(7) = a_vtk_file%xml_writer%write_geo(n=4, x=[two, two], y=[two, two], z=[two, two]) /= 0 ! written twice
  error = a_vtk_file%xml_writer%write_piece()
  ok(8) = a_vtk_file%xml_writer%write_piece(nx1=0, nx2=1, ny1=0, ny2=1, nz1=0, nz2=0) /= 0 ! a second piece
  error = a_vtk_file%finalize()
  refusals = all(ok)
  if (.not.refusals) print '(A,8L2)', 'refusals: ', ok
  endfunction refusals
endprogram vtk_fortran_write_vtkhdf_grids
