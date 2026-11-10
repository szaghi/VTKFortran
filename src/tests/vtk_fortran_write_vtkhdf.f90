!< VTK_Fortran test: write ImageData in the VTKHDF format (`format='vtkhdf'`).
program vtk_fortran_write_vtkhdf
!< VTK_Fortran test: write ImageData in the VTKHDF format (`format='vtkhdf'`).
!<
!< With HDF5 the test writes two VTKHDF files through `vtk_file`, the same calls as the XML formats, and reads them back with
!< the HDF5 bindings:
!<
!<- `vtkfortran_write_vtkhdf.vtkhdf`: a 4x3x2 points grid, rotated (direction), field data, point data (one component as a
!<  rank-3 array, 3 components given one by one, 2 components as a rank-2 array, unsigned bytes), active arrays, cell data;
!<- `vtkfortran_write_vtkhdf_2d.vtkhdf`: a 3x2x1 points grid (2D: the z axis is dropped from the datasets, as VTK
!<  requires), compressed if HDF5 has deflate.
!<
!< Refusals are checked too: string field data, an array of the wrong size, a second piece, other topologies, a volatile
!< file. Without HDF5 the writer refuses to start. VTK readers open both files outside this test.
use penf
use vtk_fortran, only : vtk_file
use vtk_fortran_hdf5

implicit none
character(*), parameter       :: filename='vtkfortran_write_vtkhdf.vtkhdf'       !< 3D file.
character(*), parameter       :: filename_2d='vtkfortran_write_vtkhdf_2d.vtkhdf' !< 2D file.
integer(I4P), parameter       :: ni=4, nj=3, nk=2                                !< Points along each axis.
real(R8P),    parameter       :: direction(9)=[0._R8P, -1._R8P, 0._R8P, &
                                               1._R8P,  0._R8P, 0._R8P, &
                                               0._R8P,  0._R8P, 1._R8P]          !< 90 degrees about z.
type(vtk_file)                :: a_vtk_file                                      !< VTK file.
real(R8P)                     :: f(ni,nj,nk)                                     !< Point data: f(i,j,k) = i + 10j + 100k.
real(R4P)                     :: vx(ni*nj*nk), vy(ni*nj*nk), vz(ni*nj*nk)        !< Point data: a vector by components.
integer(I4P)                  :: g(2,ni*nj*nk)                                   !< Point data: 2 components.
integer(I1P)                  :: u(ni*nj*nk)                                     !< Point data: unsigned bytes.
integer(I8P)                  :: c((ni-1)*(nj-1)*(nk-1))                         !< Cell data.
integer(hid_kind)             :: file_id, root, dset                             !< HDF5 identifiers.
character(len=:), allocatable :: text                                            !< String read back.
integer(I8P),     allocatable :: ivalues(:), dims(:)                             !< Values read back.
real(R8P),        allocatable :: rvalues(:)                                      !< Values read back.
real(R4P),        allocatable :: svalues(:)                                      !< Values read back.
integer(I2P),     allocatable :: bvalues(:)                                      !< Values read back.
integer(I4P),     allocatable :: gvalues(:)                                      !< Values read back.
integer(I4P)                  :: e(40)                                           !< Errors of each call.
integer(I4P)                  :: i, j, k, p                                      !< Counters.
logical                       :: test_passed(8)                                  !< List of passed tests.

test_passed = .false.
if (.not.is_hdf5_enabled) then
  ! without HDF5 the writer refuses to start
  test_passed = a_vtk_file%initialize(format='vtkhdf', filename=filename, mesh_topology='ImageData', &
                                      nx1=0, nx2=ni-1, ny1=0, ny2=nj-1, nz1=0, nz2=nk-1,          &
                                      origin=[0._R8P, 0._R8P, 0._R8P], spacing=[1._R8P, 1._R8P, 1._R8P]) /= 0
  print '(A)', 'HDF5 not enabled: the VTKHDF writer refuses to start'
else
  p = 0
  do k=1, nk ; do j=1, nj ; do i=1, ni
    p = p + 1
    f(i,j,k) = i + 10*j + 100*k
    vx(p) = real(i, R4P) ; vy(p) = real(j, R4P) ; vz(p) = real(k, R4P)
    g(:,p) = [p, -p]
    u(p) = int(merge(p * 10, p * 10 - 256, p * 10 < 128), I1P) ! the bits of p*10 (up to 240) as unsigned
  enddo ; enddo ; enddo
  c = [(int(p, I8P) * 1000000000_I8P, p=1, size(c))]

  ! 3D file
  e = 0
  e(1) = a_vtk_file%initialize(format='VTKHDF', filename=filename, mesh_topology='ImageData', &
                               nx1=0, nx2=ni-1, ny1=0, ny2=nj-1, nz1=0, nz2=nk-1,           &
                               origin=[1._R8P, 2._R8P, 3._R8P], spacing=[0.5_R8P, 0.25_R8P, 1._R8P], direction=direction)
  e(2) = a_vtk_file%xml_writer%write_fielddata(action='open')
  e(3) = a_vtk_file%xml_writer%write_fielddata(data_name='time', x=1.5_R8P)
  e(4) = a_vtk_file%xml_writer%write_fielddata(data_name='cycle', x=[7_I4P, 8_I4P])
  e(5) = merge(0, 1, a_vtk_file%xml_writer%write_fielddata(data_name='label', x='refused') /= 0) ! strings are refused
  e(6) = a_vtk_file%xml_writer%write_fielddata(action='close')
  e(7) = a_vtk_file%xml_writer%write_piece(nx1=0, nx2=ni-1, ny1=0, ny2=nj-1, nz1=0, nz2=nk-1)
  e(8) = a_vtk_file%xml_writer%write_dataarray(location='node', action='open', scalars='f', vectors='v')
  e(9) = a_vtk_file%xml_writer%write_dataarray(data_name='f', x=f, one_component=.true.)
  e(10) = a_vtk_file%xml_writer%write_dataarray(data_name='v', x=vx, y=vy, z=vz)
  e(11) = a_vtk_file%xml_writer%write_dataarray(data_name='g', x=g)
  e(12) = a_vtk_file%xml_writer%write_dataarray_unsigned(data_name='u', x=u)
  e(13) = merge(0, 1, a_vtk_file%xml_writer%write_dataarray(data_name='short', x=vx(2:)) /= 0) ! wrong size: refused
  e(14) = a_vtk_file%xml_writer%write_dataarray(location='node', action='close')
  e(15) = a_vtk_file%xml_writer%write_dataarray(location='cell', action='open', scalars='c')
  e(16) = a_vtk_file%xml_writer%write_dataarray(data_name='c', x=c)
  e(17) = a_vtk_file%xml_writer%write_dataarray(location='cell', action='close')
  e(18) = a_vtk_file%xml_writer%write_piece()
  e(19) = merge(0, 1, a_vtk_file%xml_writer%write_piece(nx1=0, nx2=ni-1, ny1=0, ny2=nj-1, nz1=0, nz2=nk-1) /= 0) ! one piece
  e(20) = a_vtk_file%finalize()
  test_passed(1) = all(e(1:20) == 0)
  if (.not.test_passed(1)) print '(A,20I2)', 'errors of the 3D file: ', e(1:20)

  ! 2D file, compressed if HDF5 has deflate (refused otherwise)
  e = 0
  e(1) = a_vtk_file%initialize(format='vtkhdf', filename=filename_2d, mesh_topology='ImageData', &
                               nx1=0, nx2=2, ny1=0, ny2=1, nz1=0, nz2=0,                       &
                               origin=[0._R8P, 0._R8P, 0._R8P], spacing=[1._R8P, 1._R8P, 1._R8P], compressor='zlib')
  if (h5_deflate_available()) then
    e(2) = a_vtk_file%xml_writer%write_piece(nx1=0, nx2=2, ny1=0, ny2=1, nz1=0, nz2=0)
    e(3) = a_vtk_file%xml_writer%write_dataarray(location='node', action='open')
    e(4) = a_vtk_file%xml_writer%write_dataarray(data_name='p', x=[(real(p, R8P), p=1, 6)])
    e(5) = a_vtk_file%xml_writer%write_dataarray(location='node', action='close')
    e(6) = a_vtk_file%xml_writer%write_dataarray(location='cell', action='open')
    e(7) = a_vtk_file%xml_writer%write_dataarray(data_name='c', x=[1._R4P, 2._R4P])
    e(8) = a_vtk_file%xml_writer%write_dataarray(location='cell', action='close')
    e(9) = a_vtk_file%xml_writer%write_piece()
    e(10) = a_vtk_file%finalize()
    test_passed(2) = all(e(1:10) == 0)
  else
    test_passed(2) = e(1) /= 0
    print '(A)', 'HDF5 without deflate: compressor zlib refused'
  endif

  ! refusals
  test_passed(3) = a_vtk_file%initialize(format='vtkhdf', filename='refused.vtkhdf', mesh_topology='StructuredGrid')   /= 0 &
             .and. a_vtk_file%initialize(format='vtkhdf', filename='refused.vtkhdf', mesh_topology='ImageData',             &
                                         nx1=0, nx2=1, ny1=0, ny2=1, nz1=0, nz2=1, origin=[0._R8P, 0._R8P, 0._R8P],        &
                                         spacing=[1._R8P, 1._R8P, 1._R8P], is_volatile=.true.) /= 0

  ! read back the 3D file
  e = 0
  e(1) = h5_file_open(filename, file_id)
  e(2) = h5_group_open(file_id, 'VTKHDF', root)
  e(3) = h5_attribute_read(root, 'Version', ivalues)
  test_passed(4) = all(ivalues == [2_I8P, 6_I8P])
  e(4) = h5_attribute_read(root, 'Type', text)
  e(5) = h5_attribute_read(root, 'WholeExtent', ivalues)
  test_passed(4) = test_passed(4) .and. text == 'ImageData' .and. all(ivalues == [0_I8P, 3_I8P, 0_I8P, 2_I8P, 0_I8P, 1_I8P])
  e(6) = h5_attribute_read(root, 'Origin', rvalues)
  test_passed(4) = test_passed(4) .and. all(rvalues == [1._R8P, 2._R8P, 3._R8P])
  e(7) = h5_attribute_read(root, 'Spacing', rvalues)
  test_passed(4) = test_passed(4) .and. all(rvalues == [0.5_R8P, 0.25_R8P, 1._R8P])
  e(8) = h5_attribute_read(root, 'Direction', rvalues)
  test_passed(4) = test_passed(4) .and. all(rvalues == direction)
  ! point data: the grid shape (and the components first), the values in memory order
  e(9) = h5_dataset_dims(root, 'PointData/f', dims)
  e(10) = h5_dataset_read(root, 'PointData/f', rvalues)
  test_passed(5) = all(dims == [4_I8P, 3_I8P, 2_I8P]) .and. all(rvalues == reshape(f, [size(f)]))
  e(11) = h5_dataset_dims(root, 'PointData/v', dims)
  e(12) = h5_dataset_read(root, 'PointData/v', svalues)
  test_passed(5) = test_passed(5) .and. all(dims == [3_I8P, 4_I8P, 3_I8P, 2_I8P]) .and. &
                   all(svalues(1::3) == vx) .and. all(svalues(2::3) == vy) .and. all(svalues(3::3) == vz)
  e(13) = h5_dataset_dims(root, 'PointData/g', dims)
  e(14) = h5_dataset_read(root, 'PointData/g', gvalues)
  test_passed(5) = test_passed(5) .and. all(dims == [2_I8P, 4_I8P, 3_I8P, 2_I8P]) .and. all(gvalues == reshape(g, [size(g)]))
  e(15) = h5_dataset_read(root, 'PointData/u', bvalues)
  test_passed(5) = test_passed(5) .and. all(bvalues == [(int(p * 10, I2P), p=1, size(u))]) .and. &
                   .not.h5_link_exists(root, 'PointData/short')
  ! active arrays
  e(16) = h5_dataset_open(root, 'PointData/f', dset)
  e(17) = h5_attribute_read(dset, 'Attribute', text)
  test_passed(6) = text == 'Scalars'
  e(18) = h5_dataset_close(dset)
  e(19) = h5_dataset_open(root, 'PointData/v', dset)
  e(20) = h5_attribute_read(dset, 'Attribute', text)
  test_passed(6) = test_passed(6) .and. text == 'Vectors'
  e(21) = h5_dataset_close(dset)
  e(22) = h5_dataset_open(root, 'PointData/g', dset)
  test_passed(6) = test_passed(6) .and. .not.h5_attribute_exists(dset, 'Attribute')
  e(23) = h5_dataset_close(dset)
  ! cell and field data
  e(24) = h5_dataset_dims(root, 'CellData/c', dims)
  e(25) = h5_dataset_read(root, 'CellData/c', ivalues)
  test_passed(7) = all(dims == [3_I8P, 2_I8P, 1_I8P]) .and. all(ivalues == c)
  e(26) = h5_dataset_read(root, 'FieldData/time', rvalues)
  e(27) = h5_dataset_read(root, 'FieldData/cycle', gvalues)
  test_passed(7) = test_passed(7) .and. all(rvalues == [1.5_R8P]) .and. all(gvalues == [7_I4P, 8_I4P]) .and. &
                   .not.h5_link_exists(root, 'FieldData/label')
  e(28) = h5_group_close(root)
  e(29) = h5_file_close(file_id)
  test_passed(4:7) = test_passed(4:7) .and. all(e(1:29) == 0)
  ! read back the 2D file: no z axis, version 2.0 (no active array)
  test_passed(8) = .true.
  if (h5_deflate_available()) then
    e = 0
    e(1) = h5_file_open(filename_2d, file_id)
    e(2) = h5_group_open(file_id, 'VTKHDF', root)
    e(3) = h5_attribute_read(root, 'Version', ivalues)
    test_passed(8) = all(ivalues == [2_I8P, 0_I8P])
    e(4) = h5_dataset_dims(root, 'CellData/c', dims)
    e(5) = h5_dataset_read(root, 'CellData/c', svalues)
    test_passed(8) = test_passed(8) .and. all(dims == [2_I8P, 1_I8P]) .and. all(svalues == [1._R4P, 2._R4P])
    e(6) = h5_dataset_dims(root, 'PointData/p', dims)
    test_passed(8) = test_passed(8) .and. all(dims == [3_I8P, 2_I8P])
    e(7) = h5_group_close(root)
    e(8) = h5_file_close(file_id)
    test_passed(8) = test_passed(8) .and. all(e(1:8) == 0)
  endif
  print '(A,8L2)', 'VTKHDF: 3D, 2D zlib, refusals, header, point data, active arrays, cell+field data, 2D:', test_passed
endif

print "(A,L1)", new_line('a')//'Are all tests passed? ', all(test_passed)
if (.not.all(test_passed)) error stop 'some tests failed'
stop
endprogram vtk_fortran_write_vtkhdf
