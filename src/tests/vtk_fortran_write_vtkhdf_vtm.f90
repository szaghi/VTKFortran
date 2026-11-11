!< VTK_Fortran test: write a multi-block dataset in one VTKHDF file (`vtm_file`, `format='vtkhdf'`).
program vtk_fortran_write_vtkhdf_vtm
!< VTK_Fortran test: write a multi-block dataset in one VTKHDF file (`vtm_file`, `format='vtkhdf'`).
!<
!< With HDF5 the test writes `.vtkhdf` datasets with `vtk_file`, then builds two VTKHDF multi-block files from them with
!< the calls of the XML `.vtm`, and lists for each one, in `<name>.txt`, the path of each dataset in the tree of the blocks
!< and its file (VTK readers compare each dataset of the multi-block file with its file outside this test):
!<
!<- `vtkfortran_vtm_nested`: the nested assembly of the docs: a block `assembly` with two parts and a nested block `bolts`
!<  of two bolts, and a block `domain` with the fluid (an ImageData), written with every form of `write_block`;
!<
!< A time series block is refused (VTK 9.7.2 reads only the first step of a multi-block time series, its own files too).
!<
!< The first file is read back with the HDF5 bindings (its type, the links of its tree). Refusals are checked too.
!< Without HDF5 the VTKHDF format of `vtm_file` is refused.
use penf
use vtk_fortran, only : vtk_file, vtm_file
use vtk_fortran_hdf5

implicit none
type(vtk_file)                :: a_vtk_file                 !< VTK file.
type(vtm_file)                :: a_vtm_file                 !< VTM file.
integer(hid_kind)             :: file_id, root              !< HDF5 identifiers.
character(len=:), allocatable :: text                       !< String read back.
integer(I4P)                  :: e(20)                      !< Errors.
integer(I4P)                  :: u                          !< Unit.
logical                       :: test_passed(4)             !< List of passed tests.

test_passed = .false.
if (.not.is_hdf5_enabled) then
  test_passed = a_vtm_file%initialize(filename='refused.vtkhdf', format='vtkhdf') /= 0
  print '(A)', 'HDF5 not enabled: the VTKHDF format of vtm_file is refused'
else
  e = 0
  ! the datasets
  e(1) = tetra('part_1.vtkhdf', 0._R8P, 0, 0._R8P)
  e(2) = tetra('part_2.vtkhdf', 2._R8P, 0, 0._R8P)
  e(3) = tetra('bolt_1.vtkhdf', 0._R8P, 0, 3._R8P)
  e(4) = tetra('bolt_2.vtkhdf', 2._R8P, 0, 3._R8P)
  e(5) = image('fluid.vtkhdf', 0)
  e(6) = tetra('series_tetra.vtkhdf', 0._R8P, 2, 0._R8P)
  e(7) = image('series_fluid.vtkhdf', 2)
  ! the nested assembly
  e(8) = a_vtm_file%initialize(filename='vtkfortran_vtm_nested.vtkhdf', format='vtkhdf')
  e(8) = max(e(8), abs(a_vtm_file%write_block(action='open', name='assembly')))
  e(8) = max(e(8), abs(a_vtm_file%write_block(filenames=['part_1.vtkhdf', 'part_2.vtkhdf'], action='write')))
  e(8) = max(e(8), abs(a_vtm_file%write_block(filenames=['bolt_1.vtkhdf', 'bolt_2.vtkhdf'], names=['bolt-1', 'bolt-2'], &
                                              name='bolts')))
  e(8) = max(e(8), abs(a_vtm_file%write_block(action='close')))
  e(8) = max(e(8), abs(a_vtm_file%write_block(filenames='fluid.vtkhdf', names='fluid', name='domain')))
  e(8) = max(e(8), abs(a_vtm_file%finalize()))
  open(newunit=u, file='vtkfortran_vtm_nested.txt', status='replace')
  write(u, '(A)') 'assembly/part_1 part_1.vtkhdf', 'assembly/part_2 part_2.vtkhdf', 'assembly/bolts/bolt-1 bolt_1.vtkhdf', &
                  'assembly/bolts/bolt-2 bolt_2.vtkhdf', 'domain/fluid fluid.vtkhdf'
  close(u)
  test_passed(1) = all(e(1:8) == 0)
  if (.not.test_passed(1)) print '(A,8I2)', 'errors of the files: ', e(1:8)
  test_passed(2) = refusals()

  ! read back the nested assembly
  e = 0
  e(1) = h5_file_open('vtkfortran_vtm_nested.vtkhdf', file_id)
  e(2) = h5_group_open(file_id, 'VTKHDF', root)
  e(3) = h5_attribute_read(root, 'Type', text)
  test_passed(3) = text == 'MultiBlockDataSet' .and. h5_link_exists(root, 'Block_5')                         .and. &
                   h5_link_exists(root, 'Assembly/assembly/part_1') .and. h5_link_exists(root, 'Assembly/domain/fluid') .and. &
                   h5_link_exists(root, 'Assembly/assembly/bolts/bolt-2')
  e(4) = h5_group_close(root)
  e(5) = h5_file_close(file_id)
  test_passed(4) = all(e(1:5) == 0)
  print '(A,4L2)', 'VTKHDF multi-block: files, refusals, tree, reads:', test_passed
endif

print "(A,L1)", new_line('a')//'Are all tests passed? ', all(test_passed)
if (.not.all(test_passed)) error stop 'some tests failed'
stop
contains
  function tetra(filename, shift, n_steps, height) result(error)
  !< A tetrahedron moved by shift along x and height along z, with point data; a time series of n_steps steps if n_steps > 0.
  character(*), intent(in) :: filename  !< File name.
  real(R8P),    intent(in) :: shift     !< Shift along x.
  integer(I4P), intent(in) :: n_steps   !< Steps, 0 for a dataset that is not a time series.
  real(R8P),    intent(in) :: height    !< Shift along z.
  integer(I4P)             :: error     !< Error status.
  integer(I4P)             :: s, i      !< Counters.

  error = a_vtk_file%initialize(format='vtkhdf', filename=filename, mesh_topology='UnstructuredGrid')
  do s=1, max(1, n_steps)
    if (n_steps > 0) error = max(error, abs(a_vtk_file%xml_writer%write_step(time=0.5_R8P * s)))
    error = max(error, abs(a_vtk_file%xml_writer%write_piece(np=4, nc=1)))
    error = max(error, abs(a_vtk_file%xml_writer%write_geo(np=4, nc=1, x=[0._R8P, 1._R8P, 0._R8P, 0._R8P] + shift + s, &
                                                           y=[0._R8P, 0._R8P, 1._R8P, 0._R8P],                         &
                                                           z=[0._R8P, 0._R8P, 0._R8P, 1._R8P] + height)))
    error = max(error, abs(a_vtk_file%xml_writer%write_connectivity(nc=1, connectivity=[0, 1, 2, 3], offset=[4], &
                                                                      cell_type=[10_I1P])))
    error = max(error, abs(a_vtk_file%xml_writer%write_dataarray(location='node', action='open')))
    error = max(error, abs(a_vtk_file%xml_writer%write_dataarray(data_name='pd', x=[(real(10 * s + i, R8P), i=1, 4)])))
    error = max(error, abs(a_vtk_file%xml_writer%write_dataarray(location='node', action='close')))
    error = max(error, abs(a_vtk_file%xml_writer%write_piece()))
  enddo
  error = max(error, abs(a_vtk_file%finalize()))
  endfunction tetra

  function image(filename, n_steps) result(error)
  !< A 2D ImageData with point data; a time series of n_steps steps if n_steps > 0.
  character(*), intent(in) :: filename  !< File name.
  integer(I4P), intent(in) :: n_steps   !< Steps, 0 for a dataset that is not a time series.
  integer(I4P)             :: error     !< Error status.
  integer(I4P)             :: s, i      !< Counters.

  error = a_vtk_file%initialize(format='vtkhdf', filename=filename, mesh_topology='ImageData', &
                                nx1=0, nx2=2, ny1=0, ny2=1, nz1=0, nz2=0,                       &
                                origin=[0._R8P, 0._R8P, -1._R8P], spacing=[1._R8P, 1._R8P, 1._R8P])
  do s=1, max(1, n_steps)
    if (n_steps > 0) error = max(error, abs(a_vtk_file%xml_writer%write_step(time=0.5_R8P * s)))
    error = max(error, abs(a_vtk_file%xml_writer%write_piece(nx1=0, nx2=2, ny1=0, ny2=1, nz1=0, nz2=0)))
    error = max(error, abs(a_vtk_file%xml_writer%write_dataarray(location='node', action='open')))
    error = max(error, abs(a_vtk_file%xml_writer%write_dataarray(data_name='pd', x=[(real(10 * s + i, R8P), i=1, 6)])))
    error = max(error, abs(a_vtk_file%xml_writer%write_dataarray(location='node', action='close')))
    error = max(error, abs(a_vtk_file%xml_writer%write_piece()))
  enddo
  error = max(error, abs(a_vtk_file%finalize()))
  endfunction image

  logical function refusals()
  !< The refusals of the VTKHDF format of vtm_file, each one a non-zero error.
  integer(I4P) :: error
  logical      :: ok(5)

  ok = .false.
  error = a_vtm_file%initialize(filename='refused_vtm.vtkhdf', format='vtkhdf')
  ok(1) = a_vtm_file%write_block(filenames=['missing.vtkhdf'], name='a') /= 0             ! a missing file
  ok(2) = a_vtm_file%write_block(filenames='series_tetra.vtkhdf series_fluid.vtkhdf', name='b') /= 0 ! time series
  error = a_vtm_file%write_block(action='open', name='c')
  error = a_vtm_file%write_block(filenames=['part_1.vtkhdf'], names=['p'], action='write')
  ok(3) = a_vtm_file%write_block(filenames=['part_2.vtkhdf'], names=['p'], action='write') /= 0 ! a name used twice
  error = a_vtm_file%write_block(action='close')
  ok(4) = a_vtm_file%write_block(scratch=1, action='open', name='d') /= 0                  ! no scratch units
  error = a_vtm_file%write_block(action='open', name='e')
  ok(5) = a_vtm_file%finalize() /= 0                                                       ! a block left open
  refusals = all(ok)
  if (.not.refusals) print '(A,5L2)', 'refusals: ', ok
  endfunction refusals
endprogram vtk_fortran_write_vtkhdf_vtm
