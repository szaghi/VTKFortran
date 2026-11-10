!< VTK_Fortran test: the HDF5 bindings used by the VTKHDF format.
program vtk_fortran_hdf5_bindings
!< VTK_Fortran test: the HDF5 bindings used by the VTKHDF format.
!<
!< With HDF5 (`VTKFORTRAN_USE_HDF5`) the test writes a small VTKHDF ImageData through the bindings (string and numeric
!< attributes, datasets of rank 3 in the Fortran order, deflate if HDF5 has it), an appendable dataset grown twice, an
!< unsigned dataset,
!< a group with tracked creation order and a soft link, then reads everything back. The ImageData is a valid VTKHDF
!< file (`vtkfortran_hdf5_bindings.vtkhdf`), opened by VTK readers outside this test. Without HDF5 it checks that the
!< procedures are stubs returning an error.
use penf
use vtk_fortran_hdf5

implicit none
character(*), parameter :: filename='vtkfortran_hdf5_bindings.vtkhdf' !< File written.
integer(I4P),  parameter :: ni=3, nj=2, nk=2                         !< Points along each axis.
integer(hid_kind)       :: file_id                                   !< File.
integer(hid_kind)       :: root                                      !< Group /VTKHDF.
integer(hid_kind)       :: group                                     !< A group.
integer(hid_kind)       :: dset                                      !< A dataset.
real(R8P)               :: f(ni,nj,nk)                               !< Point data: the index of each point.
real(R4P)               :: c(ni-1,nj-1,nk-1)                         !< Cell data.
integer(I8P)            :: ids(5)                                    !< Values appended.
integer(I1P)            :: bytes(3)                                  !< Unsigned values.
character(len=:), allocatable :: text                                !< String read back.
integer(I8P), allocatable :: version(:), dims(:), ids_read(:)        !< Values read back.
real(R8P),    allocatable :: f_read(:)                               !< Values read back.
real(R4P),    allocatable :: c_read(:)                               !< Values read back.
integer(I2P), allocatable :: bytes_read(:)                           !< Values read back.
integer(I4P)            :: e(32)                                     !< Errors of each call.
integer(I4P)            :: i, j, k                                   !< Counters.
logical                 :: test_passed(6)                            !< List of passed tests.

if (.not.is_hdf5_enabled) then
  ! without HDF5: every procedure is a stub returning an error
  test_passed = .false.
  test_passed(1) = h5_file_create(filename, file_id) /= 0
  test_passed(2:) = .not.h5_deflate_available()
  print '(A)', 'HDF5 not enabled: stubs checked'
else
  do k=1, nk ; do j=1, nj ; do i=1, ni
    f(i,j,k) = i + 10*j + 100*k
  enddo ; enddo ; enddo
  c = 7.5_R4P
  ids = [1_I8P, 2_I8P, 3_I8P, 4_I8P, 5_I8P]
  bytes = [0_I1P, 1_I1P, -56_I1P] ! the last one is 200 as unsigned
  e = 0
  ! write
  e(1) = h5_file_create(filename, file_id)
  e(2) = h5_group_create(file_id, 'VTKHDF', root)
  e(3) = h5_attribute_write(root, 'Version', [2_I8P, 0_I8P])
  e(4) = h5_attribute_write(root, 'Type', 'ImageData')
  e(5) = h5_attribute_write(root, 'WholeExtent', [0_I8P, int(ni-1, I8P), 0_I8P, int(nj-1, I8P), 0_I8P, int(nk-1, I8P)])
  e(6) = h5_attribute_write(root, 'Origin', [0._R8P, 0._R8P, 0._R8P])
  e(7) = h5_attribute_write(root, 'Spacing', [1._R8P, 1._R8P, 1._R8P])
  e(8) = h5_attribute_write(root, 'Direction', [1._R8P, 0._R8P, 0._R8P, 0._R8P, 1._R8P, 0._R8P, 0._R8P, 0._R8P, 1._R8P])
  e(9) = h5_group_create(root, 'PointData', group)
  e(10) = h5_dataset_write_R8P(group, 'f', f, [int(ni, I8P), int(nj, I8P), int(nk, I8P)])
  e(11) = h5_dataset_open(group, 'f', dset)
  e(12) = h5_attribute_write(dset, 'Attribute', 'Scalars')
  e(13) = h5_dataset_close(dset)
  e(14) = h5_group_close(group)
  e(15) = h5_group_create(root, 'CellData', group)
  e(16) = h5_dataset_write_R4P(group, 'c', c, [int(ni-1, I8P), int(nj-1, I8P), int(nk-1, I8P)], deflate=6)
  e(17) = h5_group_close(group)
  e(18) = h5_group_close(root)
  ! outside /VTKHDF (ignored by VTK readers): an appendable dataset, unsigned bytes, tracked order, a soft link
  e(19) = h5_group_create(file_id, 'extra', group, track_order=.true.)
  e(20) = h5_dataset_write_I8P(group, 'ids', ids(1:2), [2_I8P], appendable=.true.)
  e(21) = h5_dataset_append_I8P(group, 'ids', ids(3:5), [3_I8P])
  e(22) = h5_dataset_write_I1P(group, 'bytes', bytes, [3_I8P], unsigned=.true.)
  e(23) = h5_link_soft('/VTKHDF/PointData/f', group, 'link_to_f')
  e(24) = h5_group_close(group)
  e(25) = h5_file_close(file_id)
  test_passed(1) = all(e(1:25) == 0)

  ! read back
  e = 0
  e(1) = h5_file_open(filename, file_id)
  e(2) = h5_group_open(file_id, 'VTKHDF', root)
  e(3) = h5_attribute_read(root, 'Version', version)
  e(4) = h5_attribute_read(root, 'Type', text)
  test_passed(2) = all(e(1:4) == 0) .and. all(version == [2_I8P, 0_I8P]) .and. text == 'ImageData' .and. &
                   h5_attribute_exists(root, 'WholeExtent') .and. .not.h5_attribute_exists(root, 'Missing')
  e(5) = h5_dataset_dims(root, 'PointData/f', dims)
  e(6) = h5_dataset_read(root, 'PointData/f', f_read)
  test_passed(3) = all(e(5:6) == 0) .and. all(dims == [int(ni, I8P), int(nj, I8P), int(nk, I8P)]) .and. &
                   all(f_read == reshape(f, [ni*nj*nk]))
  e(7) = h5_dataset_read(root, 'CellData/c', c_read)
  ! deflate is used if HDF5 has it (an HDF5 built without zlib writes the data uncompressed): the values must match
  test_passed(4) = e(7) == 0 .and. all(c_read == 7.5_R4P)
  print '(A,L1)', 'HDF5 deflate filter available: ', h5_deflate_available()
  e(8) = h5_dataset_dims(file_id, 'extra/ids', dims)
  e(9) = h5_dataset_read(file_id, 'extra/ids', ids_read)
  e(10) = h5_dataset_read(file_id, 'extra/bytes', bytes_read)
  test_passed(5) = all(e(8:10) == 0) .and. all(dims == [5_I8P]) .and. all(ids_read == ids) .and. &
                   all(bytes_read == [0_I2P, 1_I2P, 200_I2P])
  e(11) = h5_dataset_read(file_id, 'extra/link_to_f', f_read)
  test_passed(6) = e(11) == 0 .and. all(f_read == reshape(f, [ni*nj*nk])) .and. h5_link_exists(file_id, 'extra') .and. &
                   .not.h5_link_exists(file_id, 'missing')
  e(12) = h5_group_close(root)
  e(13) = h5_file_close(file_id)
  print '(A,6L2)', 'HDF5 bindings: write, attributes, rank 3, deflate, append+unsigned, soft link:', test_passed
endif

print "(A,L1)", new_line('a')//'Are all tests passed? ', all(test_passed)
if (.not.all(test_passed)) error stop 'some tests failed'
stop
endprogram vtk_fortran_hdf5_bindings
