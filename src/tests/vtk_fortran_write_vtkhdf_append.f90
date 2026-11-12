!< VTK_Fortran test: continue a VTKHDF time series (`action='append'`), after a restart or an interrupted run.
program vtk_fortran_write_vtkhdf_append
!< VTK_Fortran test: continue a VTKHDF time series (`action='append'`), after a restart or an interrupted run.
!<
!< With HDF5 the test writes time series in two runs, the second one reopening the file with `action='append'`, and each
!< step also as one binary XML file `<name>_<step>.vt?` (VTK readers compare each step with its XML file outside this test):
!<
!<- `vtkfortran_steps_restartunst`: an UnstructuredGrid, steps 1-2 then 3; the same series written in one run,
!<  `vtkfortran_restart_onerun.vtkhdf`, has the same datasets (checked here);
!<- `vtkfortran_steps_restartimage`: a 2D ImageData, steps 1-2 then 3;
!<- `vtkfortran_steps_restartrect`: a RectilinearGrid of extent 2:4 x 0:1 x 0:0 (its start is not in the file), 1 then 2;
!<- `vtkfortran_steps_restartpoly`: a PolyData, step 1 then 2;
!<- `vtkfortran_steps_restartstrg`: a 2D StructuredGrid with point data only (no cell data group), step 1 then 2;
!<- `vtkfortran_steps_restartinterrupted`: an UnstructuredGrid whose first run ends in the second piece of step 3: the
!<  second run drops that piece, step 3 keeps its first one, and step 4 follows;
!<- `vtkfortran_steps_restartunst_fixed`, `_restartimage_fixed`, `_restartrect_fixed`, `_restartstrg_fixed`: series that
!<  store once what does not change (the mesh written at step 1 only, arrays reused), continued after the restart with
!<  the mesh of the first run (and, for the RectilinearGrid, new coordinates at step 3); their offsets are checked here;
!<- `vtkfortran_restart_orphan.vtkhdf`: a series reusing the mesh, steps 1-2, then a run that died after writing an
!<  offset of a third step (an entry past NSteps, added here with the HDF5 bindings): the restart ignores it, and its
!<  step 3, reusing the point data of step 2, gets the offset of step 2.
!<
!< Refusals are checked too. Without HDF5 the writer refuses to start.
use penf
use vtk_fortran, only : vtk_file
use vtk_fortran_hdf5

implicit none
type(vtk_file)                :: a_vtk_file                 !< VTK file.
integer(hid_kind)             :: file_id(2), root(2)        !< HDF5 identifiers.
integer(I8P),     allocatable :: ivalues(:), jvalues(:)     !< Values read back.
real(R8P),        allocatable :: rvalues(:), svalues(:)     !< Values read back.
integer(I4P)                  :: e(40)                      !< Errors of each call.
integer(I4P)                  :: f                          !< Counter.
logical                       :: test_passed(5)             !< List of passed tests.
logical                       :: hdf=.false.                !< The run writes the VTKHDF file (not the XML files).
real(R8P),        parameter   :: times(4) = [0._R8P, 0.5_R8P, 1._R8P, 1.5_R8P] !< Times of the steps.
character(24),    parameter   :: same(6) = ['NumberOfPoints          ', 'Points                  ', &
                                            'Steps/PointOffsets      ', 'Steps/Values            ', &
                                            'CellData/cid            ', 'FieldData/time          '] !< Datasets compared.

test_passed = .false.
if (.not.is_hdf5_enabled) then
  test_passed = a_vtk_file%initialize(format='vtkhdf', filename='refused.vtkhdf', action='append') /= 0
  print '(A)', 'HDF5 not enabled: the VTKHDF writer refuses to start'
else
  e = 0
  e(1) = run('vtkfortran_restart_onerun.vtkhdf', 'UnstructuredGrid', .false., 1, 3, unst_step)
  e(2) = run('vtkfortran_steps_restartunst.vtkhdf', 'UnstructuredGrid', .false., 1, 2, unst_step)
  e(3) = run('vtkfortran_steps_restartunst.vtkhdf', 'UnstructuredGrid', .true., 3, 3, unst_step)
  e(4) = xml('vtkfortran_steps_restartunst', 'UnstructuredGrid', '.vtu', 3, unst_step)
  e(5) = run('vtkfortran_steps_restartimage.vtkhdf', 'ImageData', .false., 1, 2, image_step)
  e(6) = run('vtkfortran_steps_restartimage.vtkhdf', 'ImageData', .true., 3, 3, image_step)
  e(7) = xml('vtkfortran_steps_restartimage', 'ImageData', '.vti', 3, image_step)
  e(8) = run('vtkfortran_steps_restartrect.vtkhdf', 'RectilinearGrid', .false., 1, 1, rect_step)
  e(9) = run('vtkfortran_steps_restartrect.vtkhdf', 'RectilinearGrid', .true., 2, 2, rect_step)
  e(10) = xml('vtkfortran_steps_restartrect', 'RectilinearGrid', '.vtr', 2, rect_step)
  e(11) = run('vtkfortran_steps_restartpoly.vtkhdf', 'PolyData', .false., 1, 1, poly_step)
  e(12) = run('vtkfortran_steps_restartpoly.vtkhdf', 'PolyData', .true., 2, 2, poly_step)
  e(13) = xml('vtkfortran_steps_restartpoly', 'PolyData', '.vtp', 2, poly_step)
  e(19) = run('vtkfortran_steps_restartstrg.vtkhdf', 'StructuredGrid', .false., 1, 1, strg_step)
  e(20) = run('vtkfortran_steps_restartstrg.vtkhdf', 'StructuredGrid', .true., 2, 2, strg_step)
  e(21) = xml('vtkfortran_steps_restartstrg', 'StructuredGrid', '.vts', 2, strg_step)
  ! interrupted: steps 1-2, then step 3 with its first piece only and a second piece left open
  e(14) = run('vtkfortran_steps_restartinterrupted.vtkhdf', 'UnstructuredGrid', .false., 1, 2, unst_step)
  e(15) = a_vtk_file%initialize(format='vtkhdf', filename='vtkfortran_steps_restartinterrupted.vtkhdf', action='append')
  e(15) = max(e(15), abs(a_vtk_file%xml_writer%write_step(time=times(3))))
  e(15) = max(e(15), abs(field_data(3)))
  e(15) = max(e(15), abs(unst_step(3)))
  e(15) = max(e(15), abs(a_vtk_file%xml_writer%write_piece(np=8, nc=1)))
  e(15) = max(e(15), abs(a_vtk_file%xml_writer%write_geo(np=8, nc=1, x=[(real(f, R8P), f=1, 8)], y=[(0._R8P, f=1, 8)], &
                                                         z=[(0._R8P, f=1, 8)])))
  e(16) = a_vtk_file%finalize() ! the second piece of step 3 never ends: an interrupted run
  e(17) = run('vtkfortran_steps_restartinterrupted.vtkhdf', 'UnstructuredGrid', .true., 4, 4, unst_step)
  e(18) = xml('vtkfortran_steps_restartinterrupted', 'UnstructuredGrid', '.vtu', 4, unst_step)
  ! what does not change stored once, across the restart
  e(22) = run('vtkfortran_steps_restartunst_fixed.vtkhdf', 'UnstructuredGrid', .false., 1, 2, unst_fixed_step)
  e(23) = run('vtkfortran_steps_restartunst_fixed.vtkhdf', 'UnstructuredGrid', .true., 3, 4, unst_fixed_step)
  e(24) = xml('vtkfortran_steps_restartunst_fixed', 'UnstructuredGrid', '.vtu', 4, unst_fixed_step)
  e(25) = run('vtkfortran_steps_restartimage_fixed.vtkhdf', 'ImageData', .false., 1, 2, image_fixed_step)
  e(26) = run('vtkfortran_steps_restartimage_fixed.vtkhdf', 'ImageData', .true., 3, 3, image_fixed_step)
  e(27) = xml('vtkfortran_steps_restartimage_fixed', 'ImageData', '.vti', 3, image_fixed_step)
  e(28) = run('vtkfortran_steps_restartrect_fixed.vtkhdf', 'RectilinearGrid', .false., 1, 1, rect_fixed_step)
  e(29) = run('vtkfortran_steps_restartrect_fixed.vtkhdf', 'RectilinearGrid', .true., 2, 3, rect_fixed_step)
  e(30) = xml('vtkfortran_steps_restartrect_fixed', 'RectilinearGrid', '.vtr', 3, rect_fixed_step)
  e(31) = run('vtkfortran_steps_restartstrg_fixed.vtkhdf', 'StructuredGrid', .false., 1, 1, strg_fixed_step)
  e(32) = run('vtkfortran_steps_restartstrg_fixed.vtkhdf', 'StructuredGrid', .true., 2, 2, strg_fixed_step)
  e(33) = xml('vtkfortran_steps_restartstrg_fixed', 'StructuredGrid', '.vts', 2, strg_fixed_step)
  e(34) = run('vtkfortran_restart_orphan.vtkhdf', 'UnstructuredGrid', .false., 1, 2, unst_fixed_step)
  e(35) = h5_file_open('vtkfortran_restart_orphan.vtkhdf', file_id(1), readwrite=.true.)
  e(36) = h5_dataset_append_I8P(file_id(1), 'VTKHDF/Steps/PointDataOffsets/pd', [99_I8P], [1_I8P]) ! the orphan entry
  e(37) = h5_file_close(file_id(1))
  e(38) = a_vtk_file%initialize(format='vtkhdf', filename='vtkfortran_restart_orphan.vtkhdf', action='append')
  e(38) = max(e(38), abs(a_vtk_file%xml_writer%write_step(time=times(3)))) ! no piece, no array: all of step 2
  e(38) = max(e(38), abs(a_vtk_file%finalize()))
  test_passed(1) = all(e(1:38) == 0)
  if (.not.test_passed(1)) print '(A,38I2)', 'errors of the runs: ', e(1:38)
  test_passed(2) = refusals()

  ! the series continued is the series written in one run
  e = 0
  e(1) = h5_file_open('vtkfortran_restart_onerun.vtkhdf', file_id(1))
  e(2) = h5_file_open('vtkfortran_steps_restartunst.vtkhdf', file_id(2))
  e(3) = h5_group_open(file_id(1), 'VTKHDF', root(1))
  e(4) = h5_group_open(file_id(2), 'VTKHDF', root(2))
  test_passed(3) = .true.
  do f=1, size(same)
    if (index(same(f), 'Points ') > 0 .or. index(same(f), 'Values') > 0 .or. index(same(f), 'time') > 0) then
      e(5) = max(e(5), abs(h5_dataset_read(root(1), trim(same(f)), rvalues)))
      e(5) = max(e(5), abs(h5_dataset_read(root(2), trim(same(f)), svalues)))
      if (size(rvalues) /= size(svalues)) then
        test_passed(3) = .false.
      elseif (any(rvalues /= svalues)) then
        test_passed(3) = .false.
      endif
    else
      e(5) = max(e(5), abs(h5_dataset_read(root(1), trim(same(f)), ivalues)))
      e(5) = max(e(5), abs(h5_dataset_read(root(2), trim(same(f)), jvalues)))
      if (size(ivalues) /= size(jvalues)) then
        test_passed(3) = .false.
      elseif (any(ivalues /= jvalues)) then
        test_passed(3) = .false.
      endif
    endif
    if (.not.test_passed(3)) then
      print '(A)', 'different after the restart: '//trim(same(f))
      exit
    endif
  enddo
  e(6) = h5_group_close(root(1)) ; e(7) = h5_group_close(root(2))
  e(8) = h5_file_close(file_id(1)) ; e(9) = h5_file_close(file_id(2))
  test_passed(4) = all(e(1:9) == 0)
  test_passed(5) = stored_once()
  print '(A,5L2)', 'VTKHDF time series continued: runs, refusals, same as one run, reads, stored once:', test_passed
endif

print "(A,L1)", new_line('a')//'Are all tests passed? ', all(test_passed)
if (.not.all(test_passed)) error stop 'some tests failed'
stop
contains
  function run(filename, topology, is_append, first, last, step) result(error)
  !< One run of a series: a new file, or the file reopened (`action='append'`), with the steps `first` to `last`.
  character(*), intent(in) :: filename  !< File name.
  character(*), intent(in) :: topology  !< Mesh topology.
  logical,      intent(in) :: is_append !< Reopen the file.
  integer(I4P), intent(in) :: first     !< First step.
  integer(I4P), intent(in) :: last      !< Last step.
  interface
    function step(s) result(error)
    import :: I4P
    integer(I4P), intent(in) :: s     !< Step, from 1.
    integer(I4P)             :: error !< Error status.
    endfunction step
  endinterface
  integer(I4P)             :: error     !< Error status.
  integer(I4P)             :: s         !< Step.

  hdf = .true.
  if (is_append) then
    error = a_vtk_file%initialize(format='vtkhdf', filename=filename, action='append')
  else
    error = initialize(filename, 'vtkhdf', topology)
  endif
  do s=first, last
    error = max(error, abs(a_vtk_file%xml_writer%write_step(time=times(s))))
    error = max(error, abs(field_data(s)))
    error = max(error, abs(step(s)))
  enddo
  error = max(error, abs(a_vtk_file%finalize()))
  endfunction run

  function xml(base, topology, ext, n_steps, step) result(error)
  !< The XML files of the steps of a series, `<base>_<step><ext>`.
  character(*), intent(in) :: base     !< Base name.
  character(*), intent(in) :: topology !< Mesh topology.
  character(*), intent(in) :: ext      !< Extension.
  integer(I4P), intent(in) :: n_steps  !< Steps.
  interface
    function step(s) result(error)
    import :: I4P
    integer(I4P), intent(in) :: s     !< Step, from 1.
    integer(I4P)             :: error !< Error status.
    endfunction step
  endinterface
  integer(I4P)             :: error    !< Error status.
  integer(I4P)             :: s        !< Step.

  error = 0
  hdf = .false.
  do s=1, n_steps
    error = max(error, abs(initialize(base//'_'//trim(str(n=s, no_sign=.true.))//ext, 'binary', topology)))
    error = max(error, abs(field_data(s)))
    error = max(error, abs(step(s)))
    error = max(error, abs(a_vtk_file%finalize()))
  enddo
  endfunction xml

  function initialize(filename, format, topology) result(error)
  !< Initialize a file of the topology (the grids: 2D, 0:2 x 0:1, the RectilinearGrid 2:4 x 0:1).
  character(*), intent(in) :: filename !< File name.
  character(*), intent(in) :: format   !< Format.
  character(*), intent(in) :: topology !< Mesh topology.
  integer(I4P)             :: error    !< Error status.

  select case(topology)
  case('ImageData')
    error = a_vtk_file%initialize(format=format, filename=filename, mesh_topology=topology, &
                                  nx1=0, nx2=2, ny1=0, ny2=1, nz1=0, nz2=0,                 &
                                  origin=[0._R8P, 0._R8P, 0._R8P], spacing=[1._R8P, 1._R8P, 1._R8P])
  case('RectilinearGrid')
    error = a_vtk_file%initialize(format=format, filename=filename, mesh_topology=topology, &
                                  nx1=2, nx2=4, ny1=0, ny2=1, nz1=0, nz2=0)
  case('StructuredGrid')
    error = a_vtk_file%initialize(format=format, filename=filename, mesh_topology=topology, &
                                  nx1=0, nx2=2, ny1=0, ny2=1, nz1=0, nz2=0)
  case default
    error = a_vtk_file%initialize(format=format, filename=filename, mesh_topology=topology)
  endselect
  endfunction initialize

  function field_data(s) result(error)
  !< The field data of the step: its time.
  integer(I4P), intent(in) :: s     !< Step.
  integer(I4P)             :: error !< Error status.

  error = a_vtk_file%xml_writer%write_fielddata(action='open')
  error = max(error, abs(a_vtk_file%xml_writer%write_fielddata(data_name='time', x=times(s))))
  error = max(error, abs(a_vtk_file%xml_writer%write_fielddata(action='close')))
  endfunction field_data

  function cell_point_data(np, nc, s, cell_step) result(error)
  !< Point data pd (100*s + point) and cell data cid (100*cell_step + cell, cell_step = s by default) of a piece, then the
  !< end of the piece.
  integer(I4P), intent(in)           :: np, nc, s
  integer(I4P), intent(in), optional :: cell_step
  integer(I4P)                       :: error, i, cs

  cs = s ; if (present(cell_step)) cs = cell_step
  error = a_vtk_file%xml_writer%write_dataarray(location='node', action='open')
  error = max(error, abs(a_vtk_file%xml_writer%write_dataarray(data_name='pd', x=[(real(100 * s + i, R8P), i=1, np)])))
  error = max(error, abs(a_vtk_file%xml_writer%write_dataarray(location='node', action='close')))
  error = max(error, abs(a_vtk_file%xml_writer%write_dataarray(location='cell', action='open')))
  error = max(error, abs(a_vtk_file%xml_writer%write_dataarray(data_name='cid', x=[(100 * cs + i, i=1, nc)])))
  error = max(error, abs(a_vtk_file%xml_writer%write_dataarray(location='cell', action='close')))
  error = max(error, abs(a_vtk_file%xml_writer%write_piece()))
  endfunction cell_point_data

  function unst_step(s) result(error)
  !< A tetrahedron and s more points (the mesh changes at each step).
  integer(I4P), intent(in) :: s     !< Step.
  integer(I4P)             :: error !< Error status.
  integer(I4P)             :: np, i

  np = 4 + s
  error = a_vtk_file%xml_writer%write_piece(np=np, nc=1)
  error = max(error, abs(a_vtk_file%xml_writer%write_geo(np=np, nc=1, x=[(real(i, R8P), i=0, np-1)],           &
                                                         y=[(real(mod(i, 2), R8P), i=0, np-1)],                   &
                                                         z=[(real(i / 3, R8P) + 0.1_R8P * s, i=0, np-1)])))
  error = max(error, abs(a_vtk_file%xml_writer%write_connectivity(nc=1, connectivity=[0, 1, 2, 3], offset=[4], &
                                                                    cell_type=[10_I1P])))
  error = max(error, abs(cell_point_data(np, 1, s)))
  endfunction unst_step

  function image_step(s) result(error)
  !< The 3x2 points (2x1 cells) of the 2D ImageData, its data changing at each step.
  integer(I4P), intent(in) :: s     !< Step.
  integer(I4P)             :: error !< Error status.

  error = a_vtk_file%xml_writer%write_piece(nx1=0, nx2=2, ny1=0, ny2=1, nz1=0, nz2=0)
  error = max(error, abs(cell_point_data(6, 2, s)))
  endfunction image_step

  function rect_step(s) result(error)
  !< The 3x2 points of the RectilinearGrid (extent 2:4 x 0:1), its x coordinates stretched at each step.
  integer(I4P), intent(in) :: s     !< Step.
  integer(I4P)             :: error !< Error status.

  error = a_vtk_file%xml_writer%write_piece(nx1=2, nx2=4, ny1=0, ny2=1, nz1=0, nz2=0)
  error = max(error, abs(a_vtk_file%xml_writer%write_geo(x=[2._R8P, 3._R8P, 4._R8P] * s, y=[0._R8P, 1._R8P], z=[0._R8P])))
  error = max(error, abs(cell_point_data(6, 2, s)))
  endfunction rect_step

  function strg_step(s) result(error)
  !< The 3x2 points of the 2D StructuredGrid, moved along x at each step, with point data only.
  integer(I4P), intent(in) :: s     !< Step.
  integer(I4P)             :: error !< Error status.
  integer(I4P)             :: i

  error = a_vtk_file%xml_writer%write_piece(nx1=0, nx2=2, ny1=0, ny2=1, nz1=0, nz2=0)
  error = max(error, abs(a_vtk_file%xml_writer%write_geo(n=6, x=[0._R8P, 1._R8P, 2._R8P, 0._R8P, 1._R8P, 2._R8P] + s, &
                                                         y=[0._R8P, 0._R8P, 0._R8P, 1._R8P, 1._R8P, 1._R8P],           &
                                                         z=[0._R8P, 0._R8P, 0._R8P, 0._R8P, 0._R8P, 0._R8P])))
  error = max(error, abs(a_vtk_file%xml_writer%write_dataarray(location='node', action='open')))
  error = max(error, abs(a_vtk_file%xml_writer%write_dataarray(data_name='pd', x=[(real(100 * s + i, R8P), i=1, 6)])))
  error = max(error, abs(a_vtk_file%xml_writer%write_dataarray(location='node', action='close')))
  error = max(error, abs(a_vtk_file%xml_writer%write_piece()))
  endfunction strg_step

  function poly_step(s) result(error)
  !< The four blocks at step 1, then a polyline and a triangle strip.
  integer(I4P), intent(in) :: s     !< Step.
  integer(I4P)             :: error !< Error status.

  if (s == 1) then
    error = a_vtk_file%xml_writer%write_piece(np=6, nverts=1, nlines=1, nstrips=1, npolys=1)
    error = max(error, abs(a_vtk_file%xml_writer%write_geo(np=6, nc=4, x=[0._R8P, 1._R8P, 2._R8P, 0._R8P, 1._R8P, 2._R8P], &
                                                           y=[0._R8P, 0._R8P, 0._R8P, 1._R8P, 1._R8P, 1._R8P],           &
                                                           z=[0._R8P, 0._R8P, 0._R8P, 0._R8P, 0._R8P, 0._R8P])))
    error = max(error, abs(a_vtk_file%xml_writer%write_polydata_cells(verts_connectivity=[5], verts_offset=[1],          &
                                                                      lines_connectivity=[0, 1, 2], lines_offset=[3],   &
                                                                      strips_connectivity=[0, 3, 1, 4], strips_offset=[4], &
                                                                      polys_connectivity=[1, 2, 5, 4], polys_offset=[4])))
    error = max(error, abs(cell_point_data(6, 4, s)))
  else
    error = a_vtk_file%xml_writer%write_piece(np=4, nverts=0, nlines=1, nstrips=1, npolys=0)
    error = max(error, abs(a_vtk_file%xml_writer%write_geo(np=4, nc=2, x=[0._R8P, 1._R8P, 0._R8P, 1._R8P], &
                                                           y=[2._R8P, 2._R8P, 3._R8P, 3._R8P], z=[0._R8P, 0._R8P, 0._R8P, 0._R8P])))
    error = max(error, abs(a_vtk_file%xml_writer%write_polydata_cells(lines_connectivity=[0, 1, 2], lines_offset=[3],      &
                                                                      strips_connectivity=[0, 1, 2, 3], strips_offset=[4])))
    error = max(error, abs(cell_point_data(4, 2, s)))
  endif
  endfunction poly_step

  function unst_fixed_step(s) result(error)
  !< A tetrahedron and a fifth point written at step 1, reused at the next steps (step 3: point data only).
  integer(I4P), intent(in) :: s     !< Step.
  integer(I4P)             :: error !< Error status.
  integer(I4P)             :: i

  if (hdf .and. s > 1) then ! no piece: the mesh of step 1
    error = a_vtk_file%xml_writer%write_dataarray(location='node', action='open')
    error = max(error, abs(a_vtk_file%xml_writer%write_dataarray(data_name='pd', x=[(real(100 * s + i, R8P), i=1, 5)])))
    error = max(error, abs(a_vtk_file%xml_writer%write_dataarray(location='node', action='close')))
    if (s /= 3) then
      error = max(error, abs(a_vtk_file%xml_writer%write_dataarray(location='cell', action='open')))
      error = max(error, abs(a_vtk_file%xml_writer%write_dataarray(data_name='cid', x=[100 * s + 1])))
      error = max(error, abs(a_vtk_file%xml_writer%write_dataarray(location='cell', action='close')))
    endif
    return
  endif
  error = a_vtk_file%xml_writer%write_piece(np=5, nc=1)
  error = max(error, abs(a_vtk_file%xml_writer%write_geo(np=5, nc=1, x=[0._R8P, 1._R8P, 0._R8P, 0._R8P, 1._R8P], &
                                                         y=[0._R8P, 0._R8P, 1._R8P, 0._R8P, 1._R8P],              &
                                                         z=[0._R8P, 0._R8P, 0._R8P, 1._R8P, 1._R8P])))
  error = max(error, abs(a_vtk_file%xml_writer%write_connectivity(nc=1, connectivity=[0, 1, 2, 3], offset=[4], &
                                                                    cell_type=[10_I1P])))
  error = max(error, abs(cell_point_data(5, 1, s, cell_step=merge(2, s, s == 3))))
  endfunction unst_fixed_step

  function image_fixed_step(s) result(error)
  !< The 2D ImageData: step 2 writes its point data only, step 3 nothing (no piece).
  integer(I4P), intent(in) :: s     !< Step.
  integer(I4P)             :: error !< Error status.
  integer(I4P)             :: i

  error = 0
  if (hdf .and. s == 3) return
  error = a_vtk_file%xml_writer%write_piece(nx1=0, nx2=2, ny1=0, ny2=1, nz1=0, nz2=0)
  if (hdf .and. s == 2) then
    error = max(error, abs(a_vtk_file%xml_writer%write_dataarray(location='node', action='open')))
    error = max(error, abs(a_vtk_file%xml_writer%write_dataarray(data_name='pd', x=[(real(100 * s + i, R8P), i=1, 6)])))
    error = max(error, abs(a_vtk_file%xml_writer%write_dataarray(location='node', action='close')))
    error = max(error, abs(a_vtk_file%xml_writer%write_piece()))
  else
    error = max(error, abs(cell_point_data(6, 2, min(s, 2), cell_step=1)))
  endif
  endfunction image_fixed_step

  function rect_fixed_step(s) result(error)
  !< The RectilinearGrid (extent 2:4 x 0:1): coordinates at step 1, reused at step 2, new ones at step 3.
  integer(I4P), intent(in) :: s     !< Step.
  integer(I4P)             :: error !< Error status.

  error = a_vtk_file%xml_writer%write_piece(nx1=2, nx2=4, ny1=0, ny2=1, nz1=0, nz2=0)
  if (.not.(hdf .and. s == 2)) &
    error = max(error, abs(a_vtk_file%xml_writer%write_geo(x=[2._R8P, 3._R8P, 4._R8P] * merge(3, 1, s == 3), &
                                                           y=[0._R8P, 1._R8P], z=[0._R8P])))
  error = max(error, abs(cell_point_data(6, 2, s)))
  endfunction rect_fixed_step

  function strg_fixed_step(s) result(error)
  !< The 2D StructuredGrid: its points at step 1 only, point data only.
  integer(I4P), intent(in) :: s     !< Step.
  integer(I4P)             :: error !< Error status.
  integer(I4P)             :: i

  error = a_vtk_file%xml_writer%write_piece(nx1=0, nx2=2, ny1=0, ny2=1, nz1=0, nz2=0)
  if (.not.hdf .or. s == 1) &
    error = max(error, abs(a_vtk_file%xml_writer%write_geo(n=6, x=[0._R8P, 1._R8P, 2._R8P, 0._R8P, 1._R8P, 2._R8P], &
                                                           y=[0._R8P, 0._R8P, 0._R8P, 1._R8P, 1._R8P, 1._R8P],     &
                                                           z=[0._R8P, 0._R8P, 0._R8P, 0._R8P, 0._R8P, 0._R8P])))
  error = max(error, abs(a_vtk_file%xml_writer%write_dataarray(location='node', action='open')))
  error = max(error, abs(a_vtk_file%xml_writer%write_dataarray(data_name='pd', x=[(real(100 * s + i, R8P), i=1, 6)])))
  error = max(error, abs(a_vtk_file%xml_writer%write_dataarray(location='node', action='close')))
  error = max(error, abs(a_vtk_file%xml_writer%write_piece()))
  endfunction strg_fixed_step

  logical function stored_once()
  !< The offsets of the series continued with what does not change stored once.
  integer(I8P), allocatable :: dims(:)
  logical                   :: ok(10)

  ok = .false.
  e = 0
  e(1) = h5_file_open('vtkfortran_steps_restartunst_fixed.vtkhdf', file_id(1))
  e(2) = h5_group_open(file_id(1), 'VTKHDF', root(1))
  e(3) = h5_dataset_dims(root(1), 'Points', dims) ; ok(1) = all(dims == [3_I8P, 5_I8P])
  e(4) = h5_dataset_read(root(1), 'Steps/PointOffsets', ivalues) ; ok(2) = all(ivalues == 0_I8P) .and. size(ivalues) == 4
  e(5) = h5_dataset_read(root(1), 'Steps/PointDataOffsets/pd', ivalues) ; ok(3) = all(ivalues == [0_I8P, 5_I8P, 10_I8P, 15_I8P])
  e(6) = h5_dataset_read(root(1), 'Steps/CellDataOffsets/cid', ivalues) ; ok(4) = all(ivalues == [0_I8P, 1_I8P, 1_I8P, 2_I8P])
  e(7) = max(h5_group_close(root(1)), h5_file_close(file_id(1)))
  e(8) = h5_file_open('vtkfortran_steps_restartrect_fixed.vtkhdf', file_id(1))
  e(9) = h5_group_open(file_id(1), 'VTKHDF', root(1))
  e(10) = h5_dataset_read(root(1), 'Steps/XCoordinatesOffsets', ivalues) ; ok(5) = all(ivalues == [0_I8P, 0_I8P, 3_I8P])
  e(11) = h5_dataset_dims(root(1), 'XCoordinates', dims) ; ok(6) = all(dims == [6_I8P])
  e(12) = max(h5_group_close(root(1)), h5_file_close(file_id(1)))
  e(13) = h5_file_open('vtkfortran_steps_restartimage_fixed.vtkhdf', file_id(1))
  e(14) = h5_group_open(file_id(1), 'VTKHDF', root(1))
  e(15) = h5_dataset_read(root(1), 'Steps/PointDataOffsets/pd', ivalues) ; ok(7) = all(ivalues == [0_I8P, 1_I8P, 1_I8P])
  e(16) = h5_dataset_read(root(1), 'Steps/CellDataOffsets/cid', ivalues) ; ok(8) = all(ivalues == 0_I8P)
  e(17) = max(h5_group_close(root(1)), h5_file_close(file_id(1)))
  e(18) = h5_file_open('vtkfortran_restart_orphan.vtkhdf', file_id(1))
  e(19) = h5_group_open(file_id(1), 'VTKHDF', root(1))
  e(20) = h5_dataset_read(root(1), 'Steps/PointDataOffsets/pd', ivalues) ; ok(9) = all(ivalues == [0_I8P, 5_I8P, 5_I8P])
  e(21) = h5_dataset_dims(root(1), 'PointData/pd', dims) ; ok(10) = all(dims == [10_I8P])
  e(22) = max(h5_group_close(root(1)), h5_file_close(file_id(1)))
  stored_once = all(ok) .and. all(e(1:22) == 0)
  if (.not.stored_once) print '(A,10L2)', 'stored once: ', ok
  endfunction stored_once

  logical function refusals()
  !< The refusals of `action='append'`, each one a non-zero error.
  integer(I4P) :: error
  logical      :: ok(4)

  ok = .false.
  ! a file that is not a time series; an XML format; a missing file
  error = a_vtk_file%initialize(format='vtkhdf', filename='refused_static.vtkhdf', mesh_topology='UnstructuredGrid')
  error = unst_step(1)
  error = a_vtk_file%finalize()
  ok(1) = a_vtk_file%initialize(format='vtkhdf', filename='refused_static.vtkhdf', action='append') /= 0
  ok(2) = a_vtk_file%initialize(format='binary', filename='refused_static.vtu', action='append') /= 0
  ok(3) = a_vtk_file%initialize(format='vtkhdf', filename='refused_missing.vtkhdf', action='append') /= 0
  ! a step not after the last one of the file
  error = a_vtk_file%initialize(format='vtkhdf', filename='vtkfortran_steps_restartunst.vtkhdf', action='append')
  ok(4) = a_vtk_file%xml_writer%write_step(time=times(3)) /= 0
  error = a_vtk_file%finalize()
  refusals = all(ok)
  if (.not.refusals) print '(A,4L2)', 'refusals: ', ok
  endfunction refusals
endprogram vtk_fortran_write_vtkhdf_append
