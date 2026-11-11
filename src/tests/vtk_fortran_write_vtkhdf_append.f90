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
!<  second run drops that piece, step 3 keeps its first one, and step 4 follows.
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
integer(I4P)                  :: e(30)                      !< Errors of each call.
integer(I4P)                  :: f                          !< Counter.
logical                       :: test_passed(4)             !< List of passed tests.
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
  test_passed(1) = all(e(1:21) == 0)
  if (.not.test_passed(1)) print '(A,21I2)', 'errors of the runs: ', e(1:21)
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
  print '(A,4L2)', 'VTKHDF time series continued: runs, refusals, same as one run, reads:', test_passed
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

  function cell_point_data(np, nc, s) result(error)
  !< Point data pd (100*s + point) and cell data cid (100*s + cell) of a piece, then the end of the piece.
  integer(I4P), intent(in) :: np, nc, s
  integer(I4P)             :: error, i

  error = a_vtk_file%xml_writer%write_dataarray(location='node', action='open')
  error = max(error, abs(a_vtk_file%xml_writer%write_dataarray(data_name='pd', x=[(real(100 * s + i, R8P), i=1, np)])))
  error = max(error, abs(a_vtk_file%xml_writer%write_dataarray(location='node', action='close')))
  error = max(error, abs(a_vtk_file%xml_writer%write_dataarray(location='cell', action='open')))
  error = max(error, abs(a_vtk_file%xml_writer%write_dataarray(data_name='cid', x=[(100 * s + i, i=1, nc)])))
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
