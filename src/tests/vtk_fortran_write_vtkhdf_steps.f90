!< VTK_Fortran test: write time series in the VTKHDF format (`format='vtkhdf'`, `write_step`).
program vtk_fortran_write_vtkhdf_steps
!< VTK_Fortran test: write time series in the VTKHDF format (`format='vtkhdf'`, `write_step`).
!<
!< With HDF5 the test writes six time series, each one as one `.vtkhdf` file and, by the same calls, as one binary XML
!< file per step, `<name>_<step>.vt?` (VTK readers compare each step with its XML file outside this test):
!<
!<- `vtkfortran_steps_unst`: an UnstructuredGrid whose mesh changes (one more point at each step), two pieces, then one,
!<  then two, with point, cell and field data (two at step 1: vtkHDFReader 9.7.2 reads a series of one piece at step 1 as
!<  one dataset, of which it keeps only the last piece of a later step with more);
!<- `vtkfortran_steps_polyh`: an UnstructuredGrid with a hexahedron, then a cube as a polyhedron and a tetrahedron;
!<- `vtkfortran_steps_poly`: a PolyData with the four blocks, then lines and strips only;
!<- `vtkfortran_steps_image`: a 2D ImageData, its point and cell data and its field data changing at each step;
!<- `vtkfortran_steps_rect`: a RectilinearGrid whose coordinates stretch at each step;
!<- `vtkfortran_steps_strg`: a 2D StructuredGrid whose points move at each step.
!<
!< Five more store once what does not change (the VTKHDF file only; the XML files hold every step in full):
!<
!<- `vtkfortran_steps_unst_fixed`: an UnstructuredGrid of two parts at step 1, its mesh reused by step 2 (a step without
!<  pieces, its arrays written for all the points and cells of the two parts), a new mesh of one part at step 3, reused
!<  by step 4, which writes its point data only (the cell data of step 3 reused);
!<- `vtkfortran_steps_poly_fixed`: a PolyData whose mesh is written at step 1 only, its cell data reused at step 3;
!<- `vtkfortran_steps_image_reuse`: an ImageData whose cell data are reused at step 2, and whose step 3 has no piece (all
!<  its arrays reused) and no `cycle` field data (reused);
!<- `vtkfortran_steps_rect_fixed`: a RectilinearGrid whose coordinates are reused at step 2 and written again at step 3;
!<- `vtkfortran_steps_strg_fixed`: a StructuredGrid whose points are written at step 1 only.
!<
!< The UnstructuredGrid series are read back with the HDF5 bindings (NSteps, Values, the parts and offsets of each step,
!< the points stored once), and the offsets of the grids. Refusals are checked too. The chunks (`chunk_bytes`): the
!< UnstructuredGrid and RectilinearGrid series written again with small chunks (`vtkfortran_steps_chunks_unst`,
!< `vtkfortran_steps_chunks_rect`), their chunk shapes read back against those of the default; the UnstructuredGrid one
!< continued (`action='append'`) with another `chunk_bytes` by a step with a polyhedron: its datasets keep their chunks,
!< the face datasets created after the reopen get the new one; a deflated ImageData
!< (`vtkfortran_steps_chunks_image`, if HDF5 has deflate); the refusals of `chunk_bytes`. Without HDF5 the writer refuses
!< to start.
use penf
use vtk_fortran, only : vtk_file
use vtk_fortran_hdf5

implicit none
type(vtk_file)                :: a_vtk_file                 !< VTK file.
integer(hid_kind)             :: file_id, root, steps       !< HDF5 identifiers.
integer(I8P),     allocatable :: ivalues(:)                 !< Values read back.
real(R8P),        allocatable :: rvalues(:)                 !< Values read back.
integer(I4P)                  :: e(30)                      !< Errors of each call.
integer(I4P)                  :: f                          !< Counter.
logical                       :: test_passed(7)             !< List of passed tests.
real(R8P),        parameter   :: times(4) = [0._R8P, 0.5_R8P, 1._R8P, 1.5_R8P] !< Times of the steps.
logical                       :: hdf=.false.                !< The series written is the VTKHDF file (not the XML files).
integer(I4P)                  :: skip_cycle=0               !< Step whose VTKHDF file does not write the cycle (reused).
integer(I4P)                  :: chunk_set=0                !< chunk_bytes of the VTKHDF files written, 0 for the default.

test_passed = .false.
if (.not.is_hdf5_enabled) then
  test_passed = a_vtk_file%initialize(format='vtkhdf', filename='refused.vtkhdf', mesh_topology='UnstructuredGrid') /= 0
  print '(A)', 'HDF5 not enabled: the VTKHDF writer refuses to start'
else
  e = 0
  do f=1, 2
    e(f)    = series('vtkfortran_steps_unst',  'UnstructuredGrid', '.vtu', 3, f == 1, unst_step)
    e(2+f)  = series('vtkfortran_steps_polyh', 'UnstructuredGrid', '.vtu', 2, f == 1, polyh_step)
    e(4+f)  = series('vtkfortran_steps_poly',  'PolyData',         '.vtp', 2, f == 1, poly_step)
    e(6+f)  = series('vtkfortran_steps_image', 'ImageData',        '.vti', 3, f == 1, image_step)
    e(8+f)  = series('vtkfortran_steps_rect',  'RectilinearGrid',  '.vtr', 2, f == 1, rect_step)
    e(10+f) = series('vtkfortran_steps_strg',  'StructuredGrid',   '.vts', 2, f == 1, strg_step)
    e(12+f) = series('vtkfortran_steps_unst_fixed',  'UnstructuredGrid', '.vtu', 4, f == 1, unst_fixed_step)
    e(14+f) = series('vtkfortran_steps_poly_fixed',  'PolyData',         '.vtp', 3, f == 1, poly_fixed_step)
    skip_cycle = 3
    e(16+f) = series('vtkfortran_steps_image_reuse', 'ImageData',        '.vti', 3, f == 1, image_reuse_step)
    skip_cycle = 0
    e(18+f) = series('vtkfortran_steps_rect_fixed',  'RectilinearGrid',  '.vtr', 3, f == 1, rect_fixed_step)
    e(20+f) = series('vtkfortran_steps_strg_fixed',  'StructuredGrid',   '.vts', 3, f == 1, strg_fixed_step)
  enddo
  test_passed(1) = all(e(1:22) == 0)
  if (.not.test_passed(1)) print '(A,22I2)', 'errors of the series (vtkhdf, xml for each): ', e(1:22)
  test_passed(2) = refusals()

  ! read back the UnstructuredGrid series
  e = 0
  e(1) = h5_file_open('vtkfortran_steps_unst.vtkhdf', file_id)
  e(2) = h5_group_open(file_id, 'VTKHDF', root)
  e(3) = h5_group_open(root, 'Steps', steps)
  e(4) = h5_attribute_read(steps, 'NSteps', ivalues)
  test_passed(3) = all(ivalues == [3_I8P])
  e(5) = h5_dataset_read(steps, 'Values', rvalues)
  test_passed(3) = test_passed(3) .and. all(rvalues == times(1:3))
  e(6) = h5_dataset_read(steps, 'NumberOfParts', ivalues)
  test_passed(4) = all(ivalues == [2_I8P, 1_I8P, 2_I8P])
  e(7) = h5_dataset_read(steps, 'PartOffsets', ivalues)
  test_passed(4) = test_passed(4) .and. all(ivalues == [0_I8P, 2_I8P, 3_I8P])
  e(8) = h5_dataset_read(steps, 'PointOffsets', ivalues)
  test_passed(4) = test_passed(4) .and. all(ivalues == [0_I8P, 13_I8P, 19_I8P])
  e(9) = h5_dataset_read(steps, 'FieldDataSizes/time', ivalues)
  test_passed(4) = test_passed(4) .and. all(ivalues == [1_I8P, 1_I8P, 1_I8P, 1_I8P, 1_I8P, 1_I8P])
  e(10) = h5_group_close(steps)
  e(11) = h5_group_close(root)
  e(12) = h5_file_close(file_id)
  test_passed(5) = all(e(1:12) == 0)
  test_passed(6) = stored_once()
  test_passed(7) = chunks()
  print '(A,7L2)', 'VTKHDF time series: series, refusals, steps, offsets, reads, stored once, chunks:', test_passed
endif

print "(A,L1)", new_line('a')//'Are all tests passed? ', all(test_passed)
if (.not.all(test_passed)) error stop 'some tests failed'
stop
contains
  function series(base, topology, xml, n_steps, is_vtkhdf, step) result(error)
  !< Write a time series: one `.vtkhdf` file with a `write_step` per step, or one XML file per step, by the same calls.
  character(*), intent(in) :: base       !< Base name of the files.
  character(*), intent(in) :: topology   !< Mesh topology.
  character(*), intent(in) :: xml        !< Extension of the XML files.
  integer(I4P), intent(in) :: n_steps    !< Number of steps.
  logical,      intent(in) :: is_vtkhdf  !< One VTKHDF file, otherwise XML files.
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
  hdf = is_vtkhdf
  if (is_vtkhdf) error = initialize(base//'.vtkhdf', 'vtkhdf', topology)
  do s=1, n_steps
    if (is_vtkhdf) then
      error = max(error, abs(a_vtk_file%xml_writer%write_step(time=times(s))))
    else
      error = max(error, abs(initialize(base//'_'//trim(str(n=s, no_sign=.true.))//xml, 'binary', topology)))
    endif
    error = max(error, abs(field_data(s)))
    error = max(error, abs(step(s)))
    if (.not.is_vtkhdf) error = max(error, abs(a_vtk_file%finalize()))
  enddo
  if (is_vtkhdf) error = max(error, abs(a_vtk_file%finalize()))
  endfunction series

  function initialize(filename, format, topology) result(error)
  !< Initialize a file of the topology (the extents of the grids of the test: 0:2 x 0:1 x 0:0, 2D).
  character(*), intent(in) :: filename !< File name.
  character(*), intent(in) :: format   !< Format.
  character(*), intent(in) :: topology !< Mesh topology.
  integer(I4P)             :: error    !< Error status.

  select case(topology)
  case('ImageData')
    error = a_vtk_file%initialize(format=format, filename=filename, mesh_topology=topology, &
                                  nx1=0, nx2=2, ny1=0, ny2=1, nz1=0, nz2=0,                 &
                                  origin=[0._R8P, 0._R8P, 0._R8P], spacing=[1._R8P, 1._R8P, 1._R8P])
  case('RectilinearGrid', 'StructuredGrid')
    if (chunk_set > 0) then
      error = a_vtk_file%initialize(format=format, filename=filename, mesh_topology=topology, &
                                    nx1=0, nx2=2, ny1=0, ny2=1, nz1=0, nz2=0, chunk_bytes=chunk_set)
    else
      error = a_vtk_file%initialize(format=format, filename=filename, mesh_topology=topology, &
                                    nx1=0, nx2=2, ny1=0, ny2=1, nz1=0, nz2=0)
    endif
  case default
    if (chunk_set > 0) then
      error = a_vtk_file%initialize(format=format, filename=filename, mesh_topology=topology, chunk_bytes=chunk_set)
    else
      error = a_vtk_file%initialize(format=format, filename=filename, mesh_topology=topology)
    endif
  endselect
  endfunction initialize

  function field_data(s) result(error)
  !< The field data of the step: its time and its cycle.
  integer(I4P), intent(in) :: s     !< Step.
  integer(I4P)             :: error !< Error status.

  error = a_vtk_file%xml_writer%write_fielddata(action='open')
  error = max(error, abs(a_vtk_file%xml_writer%write_fielddata(data_name='time', x=times(s))))
  if (s /= skip_cycle) then
    error = max(error, abs(a_vtk_file%xml_writer%write_fielddata(data_name='cycle', x=10_I4P * s)))
  elseif (.not.hdf) then ! the cycle of the step before, which the VTKHDF file reuses
    error = max(error, abs(a_vtk_file%xml_writer%write_fielddata(data_name='cycle', x=10_I4P * (s - 1))))
  endif
  error = max(error, abs(a_vtk_file%xml_writer%write_fielddata(action='close')))
  endfunction field_data

  function cell_point_data(np, nc, s, shift, cell_step) result(error)
  !< Point data pd (100*s + shift + point) and cell data cid (100*cell_step + shift + cell, cell_step = s by default) of a
  !< piece, then the end of the piece.
  integer(I4P), intent(in)           :: np, nc, s
  integer(I4P), intent(in), optional :: shift, cell_step
  integer(I4P)                       :: error, i, sh, cs

  sh = 0 ; if (present(shift)) sh = shift
  cs = s ; if (present(cell_step)) cs = cell_step
  error = a_vtk_file%xml_writer%write_dataarray(location='node', action='open')
  error = max(error, abs(a_vtk_file%xml_writer%write_dataarray(data_name='pd', x=[(real(100 * s + sh + i, R8P), i=1, np)])))
  error = max(error, abs(a_vtk_file%xml_writer%write_dataarray(location='node', action='close')))
  error = max(error, abs(a_vtk_file%xml_writer%write_dataarray(location='cell', action='open')))
  error = max(error, abs(a_vtk_file%xml_writer%write_dataarray(data_name='cid', x=[(100 * cs + sh + i, i=1, nc)])))
  error = max(error, abs(a_vtk_file%xml_writer%write_dataarray(location='cell', action='close')))
  error = max(error, abs(a_vtk_file%xml_writer%write_piece()))
  endfunction cell_point_data

  function unst_step(s) result(error)
  !< A tetrahedron and 0 to 2 more points (the mesh changes); at steps 1 and 3 a second piece, a hexahedron.
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
  if (s == 1 .or. s == 3) then
    error = max(error, abs(a_vtk_file%xml_writer%write_piece(np=8, nc=1)))
    error = max(error, abs(a_vtk_file%xml_writer%write_geo(np=8, nc=1, x=[0._R8P, 1._R8P, 1._R8P, 0._R8P, 0._R8P, 1._R8P, &
                                                                         1._R8P, 0._R8P] + 10._R8P,                       &
                                                           y=[0._R8P, 0._R8P, 1._R8P, 1._R8P, 0._R8P, 0._R8P, 1._R8P, 1._R8P], &
                                                           z=[0._R8P, 0._R8P, 0._R8P, 0._R8P, 1._R8P, 1._R8P, 1._R8P, 1._R8P])))
    error = max(error, abs(a_vtk_file%xml_writer%write_connectivity(nc=1, connectivity=[0, 1, 2, 3, 4, 5, 6, 7], &
                                                                      offset=[8], cell_type=[12_I1P])))
    error = max(error, abs(cell_point_data(8, 1, 50)))
  endif
  endfunction unst_step

  function polyh_step(s) result(error)
  !< A hexahedron (step 1), then a cube as a polyhedron and a tetrahedron (step 2), moved up at each step.
  integer(I4P), intent(in) :: s     !< Step.
  integer(I4P)             :: error !< Error status.
  integer(I4P), parameter  :: cube(31) = [6, 4, 0, 3, 2, 1,  4, 4, 5, 6, 7,  4, 0, 1, 5, 4,  4, 1, 2, 6, 5, &
                                          4, 2, 3, 7, 6,  4, 3, 0, 4, 7]
  real(R8P)                :: x(12), y(12), z(12)
  integer(I4P)             :: i

  x = [0, 1, 1, 0, 0, 1, 1, 0,  2, 3, 2, 2] ; y = [0, 0, 1, 1, 0, 0, 1, 1,  0, 0, 1, 0]
  z = [0, 0, 0, 0, 1, 1, 1, 1,  0, 0, 0, 1] ; z = z + s
  if (s == 1) then
    error = a_vtk_file%xml_writer%write_piece(np=8, nc=1)
    error = max(error, abs(a_vtk_file%xml_writer%write_geo(np=8, nc=1, x=x(1:8), y=y(1:8), z=z(1:8))))
    error = max(error, abs(a_vtk_file%xml_writer%write_connectivity(nc=1, connectivity=[(i, i=0, 7)], offset=[8], &
                                                                      cell_type=[12_I1P])))
    error = max(error, abs(cell_point_data(8, 1, s)))
  else
    error = a_vtk_file%xml_writer%write_piece(np=12, nc=2)
    error = max(error, abs(a_vtk_file%xml_writer%write_geo(np=12, nc=2, x=x, y=y, z=z)))
    error = max(error, abs(a_vtk_file%xml_writer%write_connectivity(nc=2, connectivity=[(i, i=0, 11)], offset=[8, 12],    &
                                                                      cell_type=[42_I1P, 10_I1P], face=cube, faceoffset=[31, -1])))
    error = max(error, abs(cell_point_data(12, 2, s)))
  endif
  endfunction polyh_step

  function poly_step(s) result(error)
  !< The four blocks (a vertex, a polyline, a triangle strip, a quad) at step 1, then a polyline and a triangle strip only.
  !< (Not a block first appearing at a later step: vtkHDFReader 9.7.2 reads it wrongly at that step, in VTK's own files too.)
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

  function image_step(s) result(error)
  !< The 3x2 points (2x1 cells) of the 2D ImageData, its data changing at each step.
  integer(I4P), intent(in) :: s     !< Step.
  integer(I4P)             :: error !< Error status.

  error = a_vtk_file%xml_writer%write_piece(nx1=0, nx2=2, ny1=0, ny2=1, nz1=0, nz2=0)
  error = max(error, abs(cell_point_data(6, 2, s)))
  endfunction image_step

  function rect_step(s) result(error)
  !< The 3x2 points of the RectilinearGrid, its x coordinates stretched at each step.
  integer(I4P), intent(in) :: s     !< Step.
  integer(I4P)             :: error !< Error status.

  error = a_vtk_file%xml_writer%write_piece(nx1=0, nx2=2, ny1=0, ny2=1, nz1=0, nz2=0)
  error = max(error, abs(a_vtk_file%xml_writer%write_geo(x=[0._R8P, 1._R8P, 2._R8P] * s, y=[0._R8P, 1._R8P], z=[0._R8P])))
  error = max(error, abs(cell_point_data(6, 2, s)))
  endfunction rect_step

  function strg_step(s) result(error)
  !< The 3x2 points of the StructuredGrid, moved along x at each step.
  integer(I4P), intent(in) :: s     !< Step.
  integer(I4P)             :: error !< Error status.

  error = a_vtk_file%xml_writer%write_piece(nx1=0, nx2=2, ny1=0, ny2=1, nz1=0, nz2=0)
  error = max(error, abs(a_vtk_file%xml_writer%write_geo(n=6, x=[0._R8P, 1._R8P, 2._R8P, 0._R8P, 1._R8P, 2._R8P] + s, &
                                                         y=[0._R8P, 0._R8P, 0._R8P, 1._R8P, 1._R8P, 1._R8P],           &
                                                         z=[0._R8P, 0._R8P, 0._R8P, 0._R8P, 0._R8P, 0._R8P])))
  error = max(error, abs(cell_point_data(6, 2, s)))
  endfunction strg_step

  function unst_fixed_step(s) result(error)
  !< Mesh A (a tetrahedron and a hexahedron, two parts) at step 1, reused at step 2; mesh B (a tetrahedron and a fifth
  !< point, one part) at step 3, reused at step 4 with its point data only (the cell data of step 3).
  integer(I4P), intent(in) :: s     !< Step.
  integer(I4P)             :: error !< Error status.
  integer(I4P)             :: i

  if (s <= 2) then
    if (hdf .and. s == 2) then ! the mesh of step 1: the arrays of its two parts, in their order
      error = point_cell_data([(real(100 * s + i, R8P), i=1, 4), (real(100 * s + 50 + i, R8P), i=1, 8)], &
                              [100 * s + 1, 100 * s + 51])
      return
    endif
    error = a_vtk_file%xml_writer%write_piece(np=4, nc=1)
    error = max(error, abs(a_vtk_file%xml_writer%write_geo(np=4, nc=1, x=[0._R8P, 1._R8P, 0._R8P, 0._R8P], &
                                                           y=[0._R8P, 0._R8P, 1._R8P, 0._R8P], z=[0._R8P, 0._R8P, 0._R8P, 1._R8P])))
    error = max(error, abs(a_vtk_file%xml_writer%write_connectivity(nc=1, connectivity=[0, 1, 2, 3], offset=[4], &
                                                                      cell_type=[10_I1P])))
    error = max(error, abs(cell_point_data(4, 1, s)))
    error = max(error, abs(a_vtk_file%xml_writer%write_piece(np=8, nc=1)))
    error = max(error, abs(a_vtk_file%xml_writer%write_geo(np=8, nc=1, x=[0._R8P, 1._R8P, 1._R8P, 0._R8P, 0._R8P, 1._R8P, &
                                                                         1._R8P, 0._R8P] + 3._R8P,                        &
                                                           y=[0._R8P, 0._R8P, 1._R8P, 1._R8P, 0._R8P, 0._R8P, 1._R8P, 1._R8P], &
                                                           z=[0._R8P, 0._R8P, 0._R8P, 0._R8P, 1._R8P, 1._R8P, 1._R8P, 1._R8P])))
    error = max(error, abs(a_vtk_file%xml_writer%write_connectivity(nc=1, connectivity=[0, 1, 2, 3, 4, 5, 6, 7], &
                                                                      offset=[8], cell_type=[12_I1P])))
    error = max(error, abs(cell_point_data(8, 1, s, 50)))
  else
    if (hdf .and. s == 4) then ! the mesh of step 3, its point data only
      error = point_cell_data([(real(100 * s + i, R8P), i=1, 5)])
      return
    endif
    error = a_vtk_file%xml_writer%write_piece(np=5, nc=1)
    error = max(error, abs(a_vtk_file%xml_writer%write_geo(np=5, nc=1, x=[0._R8P, 2._R8P, 0._R8P, 0._R8P, 1._R8P], &
                                                           y=[0._R8P, 0._R8P, 2._R8P, 0._R8P, 1._R8P],              &
                                                           z=[0._R8P, 0._R8P, 0._R8P, 2._R8P, 1._R8P])))
    error = max(error, abs(a_vtk_file%xml_writer%write_connectivity(nc=1, connectivity=[0, 1, 2, 3], offset=[4], &
                                                                      cell_type=[10_I1P])))
    error = max(error, abs(cell_point_data(5, 1, s, cell_step=3)))
  endif
  endfunction unst_fixed_step

  function poly_fixed_step(s) result(error)
  !< A PolyData (a polyline and a quad) written at step 1, its mesh reused at steps 2 and 3 (step 3: point data only).
  integer(I4P), intent(in) :: s     !< Step.
  integer(I4P)             :: error !< Error status.
  integer(I4P)             :: i

  if (hdf .and. s > 1) then
    if (s == 2) then
      error = point_cell_data([(real(100 * s + i, R8P), i=1, 4)], [100 * s + 1, 100 * s + 2])
    else
      error = point_cell_data([(real(100 * s + i, R8P), i=1, 4)])
    endif
    return
  endif
  error = a_vtk_file%xml_writer%write_piece(np=4, nverts=0, nlines=1, nstrips=0, npolys=1)
  error = max(error, abs(a_vtk_file%xml_writer%write_geo(np=4, nc=2, x=[0._R8P, 1._R8P, 1._R8P, 0._R8P], &
                                                         y=[0._R8P, 0._R8P, 1._R8P, 1._R8P], z=[0._R8P, 0._R8P, 0._R8P, 0._R8P])))
  error = max(error, abs(a_vtk_file%xml_writer%write_polydata_cells(lines_connectivity=[0, 1, 2], lines_offset=[3], &
                                                                    polys_connectivity=[0, 1, 2, 3], polys_offset=[4])))
  error = max(error, abs(cell_point_data(4, 2, s, cell_step=min(s, 2))))
  endfunction poly_fixed_step

  function image_reuse_step(s) result(error)
  !< The 2D ImageData: step 2 writes its point data only (its cell data of step 1 reused), step 3 has no piece (both).
  integer(I4P), intent(in) :: s     !< Step.
  integer(I4P)             :: error !< Error status.
  integer(I4P)             :: i

  error = 0
  if (hdf .and. s == 3) return ! nothing changes: no piece
  error = a_vtk_file%xml_writer%write_piece(nx1=0, nx2=2, ny1=0, ny2=1, nz1=0, nz2=0)
  if (hdf .and. s == 2) then
    error = max(error, abs(a_vtk_file%xml_writer%write_dataarray(location='node', action='open')))
    error = max(error, abs(a_vtk_file%xml_writer%write_dataarray(data_name='pd', x=[(real(100 * s + i, R8P), i=1, 6)])))
    error = max(error, abs(a_vtk_file%xml_writer%write_dataarray(location='node', action='close')))
    error = max(error, abs(a_vtk_file%xml_writer%write_piece()))
  else ! the XML files: the values reused by the VTKHDF file
    error = max(error, abs(cell_point_data(6, 2, min(s, 2), cell_step=1)))
  endif
  endfunction image_reuse_step

  function rect_fixed_step(s) result(error)
  !< The RectilinearGrid: its coordinates of step 1 reused at step 2, new ones at step 3 (cell data of step 2 reused).
  integer(I4P), intent(in) :: s     !< Step.
  integer(I4P)             :: error !< Error status.

  error = a_vtk_file%xml_writer%write_piece(nx1=0, nx2=2, ny1=0, ny2=1, nz1=0, nz2=0)
  if (.not.(hdf .and. s == 2)) then
    error = max(error, abs(a_vtk_file%xml_writer%write_geo(x=[0._R8P, 1._R8P, 2._R8P] * merge(3, 1, s == 3), &
                                                           y=[0._R8P, 1._R8P], z=[0._R8P])))
  endif
  if (hdf .and. s == 3) then
    error = max(error, abs(grid_point_data(s)))
  else
    error = max(error, abs(cell_point_data(6, 2, s, cell_step=min(s, 2))))
  endif
  endfunction rect_fixed_step

  function strg_fixed_step(s) result(error)
  !< The StructuredGrid: its points written at step 1 only; step 3 writes its point data only.
  integer(I4P), intent(in) :: s     !< Step.
  integer(I4P)             :: error !< Error status.

  error = a_vtk_file%xml_writer%write_piece(nx1=0, nx2=2, ny1=0, ny2=1, nz1=0, nz2=0)
  if (.not.hdf .or. s == 1) then
    error = max(error, abs(a_vtk_file%xml_writer%write_geo(n=6, x=[0._R8P, 1._R8P, 2._R8P, 0._R8P, 1._R8P, 2._R8P], &
                                                           y=[0._R8P, 0._R8P, 0.5_R8P, 1._R8P, 1._R8P, 1.5_R8P],     &
                                                           z=[0._R8P, 0._R8P, 0._R8P, 0._R8P, 0._R8P, 0._R8P])))
  endif
  if (hdf .and. s == 3) then
    error = max(error, abs(grid_point_data(s)))
  else
    error = max(error, abs(cell_point_data(6, 2, s, cell_step=min(s, 2))))
  endif
  endfunction strg_fixed_step

  function grid_point_data(s) result(error)
  !< The point data pd of the 3x2 points of a grid only, then the end of the piece.
  integer(I4P), intent(in) :: s     !< Step.
  integer(I4P)             :: error !< Error status.
  integer(I4P)             :: i

  error = a_vtk_file%xml_writer%write_dataarray(location='node', action='open')
  error = max(error, abs(a_vtk_file%xml_writer%write_dataarray(data_name='pd', x=[(real(100 * s + i, R8P), i=1, 6)])))
  error = max(error, abs(a_vtk_file%xml_writer%write_dataarray(location='node', action='close')))
  error = max(error, abs(a_vtk_file%xml_writer%write_piece()))
  endfunction grid_point_data

  function point_cell_data(pd, cid) result(error)
  !< A step without pieces: the point data pd and, if given, the cell data cid of all the parts of the mesh reused.
  real(R8P),    intent(in)           :: pd(:)  !< Point data.
  integer(I4P), intent(in), optional :: cid(:) !< Cell data.
  integer(I4P)                       :: error  !< Error status.

  error = a_vtk_file%xml_writer%write_dataarray(location='node', action='open')
  error = max(error, abs(a_vtk_file%xml_writer%write_dataarray(data_name='pd', x=pd)))
  error = max(error, abs(a_vtk_file%xml_writer%write_dataarray(location='node', action='close')))
  if (present(cid)) then
    error = max(error, abs(a_vtk_file%xml_writer%write_dataarray(location='cell', action='open')))
    error = max(error, abs(a_vtk_file%xml_writer%write_dataarray(data_name='cid', x=cid)))
    error = max(error, abs(a_vtk_file%xml_writer%write_dataarray(location='cell', action='close')))
  endif
  endfunction point_cell_data

  logical function stored_once()
  !< Read back what the reusing series store: the points of the two meshes of the UnstructuredGrid once, the offsets
  !< repeated by the steps that reuse the mesh or an array, those of the grids, the cycle reused.
  integer(I8P), allocatable :: dims(:)
  logical                   :: ok(12)

  ok = .false.
  e = 0
  e(1) = h5_file_open('vtkfortran_steps_unst_fixed.vtkhdf', file_id)
  e(2) = h5_group_open(file_id, 'VTKHDF', root)
  e(3) = h5_dataset_dims(root, 'Points', dims)
  ok(1) = all(dims == [3_I8P, 17_I8P]) ! 12 points of mesh A, 5 of mesh B
  e(4) = h5_dataset_read(root, 'Steps/PartOffsets', ivalues) ; ok(2) = all(ivalues == [0_I8P, 0_I8P, 2_I8P, 2_I8P])
  e(5) = h5_dataset_read(root, 'Steps/NumberOfParts', ivalues) ; ok(3) = all(ivalues == [2_I8P, 2_I8P, 1_I8P, 1_I8P])
  e(6) = h5_dataset_read(root, 'Steps/PointOffsets', ivalues) ; ok(4) = all(ivalues == [0_I8P, 0_I8P, 12_I8P, 12_I8P])
  e(7) = h5_dataset_read(root, 'Steps/PointDataOffsets/pd', ivalues) ; ok(5) = all(ivalues == [0_I8P, 12_I8P, 24_I8P, 29_I8P])
  e(8) = h5_dataset_read(root, 'Steps/CellDataOffsets/cid', ivalues) ; ok(6) = all(ivalues == [0_I8P, 2_I8P, 4_I8P, 4_I8P])
  e(9) = max(h5_group_close(root), h5_file_close(file_id))
  e(10) = h5_file_open('vtkfortran_steps_image_reuse.vtkhdf', file_id)
  e(11) = h5_group_open(file_id, 'VTKHDF', root)
  e(12) = h5_dataset_read(root, 'Steps/PointDataOffsets/pd', ivalues) ; ok(7) = all(ivalues == [0_I8P, 1_I8P, 1_I8P])
  e(13) = h5_dataset_read(root, 'Steps/CellDataOffsets/cid', ivalues) ; ok(8) = all(ivalues == [0_I8P, 0_I8P, 0_I8P])
  e(14) = h5_dataset_read(root, 'Steps/FieldDataOffsets/cycle', ivalues) ; ok(9) = all(ivalues == [0_I8P, 1_I8P, 1_I8P])
  e(15) = max(h5_group_close(root), h5_file_close(file_id))
  e(16) = h5_file_open('vtkfortran_steps_rect_fixed.vtkhdf', file_id)
  e(17) = h5_group_open(file_id, 'VTKHDF', root)
  e(18) = h5_dataset_read(root, 'Steps/XCoordinatesOffsets', ivalues) ; ok(10) = all(ivalues == [0_I8P, 0_I8P, 3_I8P])
  e(19) = max(h5_group_close(root), h5_file_close(file_id))
  e(20) = h5_file_open('vtkfortran_steps_strg_fixed.vtkhdf', file_id)
  e(21) = h5_group_open(file_id, 'VTKHDF', root)
  e(22) = h5_dataset_read(root, 'Steps/PointOffsets', ivalues) ; ok(11) = all(ivalues == [0_I8P, 0_I8P, 0_I8P])
  e(23) = h5_dataset_dims(root, 'Points', dims) ; ok(12) = dims(size(dims)) == 1_I8P ! one time slice of points
  e(24) = max(h5_group_close(root), h5_file_close(file_id))
  stored_once = all(ok) .and. all(e(1:24) == 0)
  if (.not.stored_once) print '(A,12L2)', 'stored once: ', ok
  endfunction stored_once

  logical function chunks()
  !< The chunks of the datasets: whole rows, at most `chunk_bytes` (default 1 MiB), at least one row, at most the rows of
  !< the first write; kept by a series continued with another `chunk_bytes`; the refusals of `chunk_bytes`.
  integer(I4P) :: error, i
  logical      :: ok(12)

  ok = .false.
  ! 64 bytes: 2 points of 24 bytes (the first piece has 5); 16 bytes: 2 x coordinates (3 per step)
  chunk_set = 64
  ok(1) = series('vtkfortran_steps_chunks_unst', 'UnstructuredGrid', '.vtu', 3, .true., unst_step) == 0
  chunk_set = 16
  ok(2) = series('vtkfortran_steps_chunks_rect', 'RectilinearGrid', '.vtr', 2, .true., rect_step) == 0
  chunk_set = 0
  ok(3) = shape_of('vtkfortran_steps_unst.vtkhdf', 'Points', [3_I8P, 5_I8P])               ! default: the rows written
  ok(4) = shape_of('vtkfortran_steps_chunks_unst.vtkhdf', 'Points', [3_I8P, 2_I8P])        ! 64 bytes
  ok(5) = shape_of('vtkfortran_steps_chunks_unst.vtkhdf', 'Steps/Values', [1_I8P])         ! one row at least
  ok(6) = shape_of('vtkfortran_steps_rect.vtkhdf', 'XCoordinates', [3_I8P])
  ok(7) = shape_of('vtkfortran_steps_chunks_rect.vtkhdf', 'XCoordinates', [2_I8P])         ! 16 bytes
  ! continued with chunk_bytes=16: the datasets of the file keep their chunks, those created after the reopen with rows
  ! get 16 bytes, 2 int64 (the offsets of the polyhedra of step 4, with zeros for the parts and steps before; a
  ! dataset created empty, as FaceConnectivity, has chunks of one row whatever chunk_bytes)
  error = a_vtk_file%initialize(format='vtkhdf', filename='vtkfortran_steps_chunks_unst.vtkhdf', action='append', &
                                chunk_bytes=16)
  error = max(error, abs(a_vtk_file%xml_writer%write_step(time=times(4))))
  error = max(error, abs(field_data(4)))
  error = max(error, abs(polyh_step(2)))
  error = max(error, abs(a_vtk_file%finalize()))
  ok(8) = error == 0 .and. shape_of('vtkfortran_steps_chunks_unst.vtkhdf', 'Points', [3_I8P, 2_I8P]) .and. &
          shape_of('vtkfortran_steps_chunks_unst.vtkhdf', 'PolyhedronOffsets', [2_I8P]) .and. &
          shape_of('vtkfortran_steps_chunks_unst.vtkhdf', 'Steps/FaceOffsetsOffsets', [2_I8P])
  ! a deflated ImageData: planes of 3x4 points (96 bytes), 200 bytes = 2 planes; contiguous without deflate
  error = a_vtk_file%initialize(format='vtkhdf', filename='vtkfortran_steps_chunks_image.vtkhdf', &
                                mesh_topology='ImageData', nx1=0, nx2=2, ny1=0, ny2=3, nz1=0, nz2=4, &
                                origin=[0._R8P, 0._R8P, 0._R8P], spacing=[1._R8P, 1._R8P, 1._R8P], &
                                compressor='zlib', chunk_bytes=200)
  if (h5_deflate_available()) then
    error = max(error, abs(a_vtk_file%xml_writer%write_piece(nx1=0, nx2=2, ny1=0, ny2=3, nz1=0, nz2=4)))
    error = max(error, abs(a_vtk_file%xml_writer%write_dataarray(location='node', action='open')))
    error = max(error, abs(a_vtk_file%xml_writer%write_dataarray(data_name='pd', x=[(real(i, R8P), i=1, 60)])))
    error = max(error, abs(a_vtk_file%xml_writer%write_dataarray(location='node', action='close')))
    error = max(error, abs(a_vtk_file%xml_writer%write_piece()))
    error = max(error, abs(a_vtk_file%finalize()))
    ok(9) = error == 0 .and. shape_of('vtkfortran_steps_chunks_image.vtkhdf', 'PointData/pd', [3_I8P, 4_I8P, 2_I8P])
  else
    ok(9) = error /= 0 ! zlib refused without the deflate filter
  endif
  ! refused: not positive, an XML format, a file read
  ok(10) = a_vtk_file%initialize(format='vtkhdf', filename='refused_chunks.vtkhdf', mesh_topology='UnstructuredGrid', &
                                 chunk_bytes=0) /= 0
  ok(11) = a_vtk_file%initialize(format='binary', filename='refused_chunks.vtu', mesh_topology='UnstructuredGrid', &
                                 chunk_bytes=4096) /= 0
  ok(12) = a_vtk_file%initialize(format='vtkhdf', filename='vtkfortran_steps_unst.vtkhdf', action='read', &
                                 chunk_bytes=4096) /= 0
  chunks = all(ok)
  if (.not.chunks) print '(A,12L2)', 'chunks: ', ok
  endfunction chunks

  logical function shape_of(filename, name, expected)
  !< The chunk shape of the dataset `name` of `/VTKHDF` in the file is `expected`.
  character(*), intent(in)  :: filename    !< File name.
  character(*), intent(in)  :: name        !< Dataset, in /VTKHDF.
  integer(I8P), intent(in)  :: expected(:) !< Chunk shape expected, Fortran order.
  integer(I8P), allocatable :: chunk(:)    !< Chunk shape read.
  integer(hid_kind)         :: file_id     !< File.
  integer(I4P)              :: error       !< Error status.

  shape_of = .false.
  if (h5_file_open(filename, file_id) /= 0) return
  error = h5_dataset_chunk(file_id, 'VTKHDF/'//name, chunk)
  shape_of = error == 0 .and. size(chunk) == size(expected)
  if (shape_of) shape_of = all(chunk == expected)
  if (.not.shape_of) print '(A,10I8)', filename//' '//name//' chunk: ', chunk
  error = h5_file_close(file_id)
  endfunction shape_of

  logical function refusals()
  !< The refusals of the time series, each one a non-zero error.
  integer(I4P) :: error
  logical      :: ok(10)

  ok = .false.
  ! write_step in an XML format
  error = a_vtk_file%initialize(format='binary', filename='refused_steps.vti', mesh_topology='ImageData', &
                                nx1=0, nx2=2, ny1=0, ny2=1, nz1=0, nz2=0,                                &
                                origin=[0._R8P, 0._R8P, 0._R8P], spacing=[1._R8P, 1._R8P, 1._R8P])
  ok(1) = a_vtk_file%xml_writer%write_step(time=0._R8P) /= 0
  error = a_vtk_file%finalize()
  ! write_step after the first piece of a file that is not a time series
  error = initialize('refused_steps_1.vtkhdf', 'vtkhdf', 'ImageData')
  error = image_step(1)
  ok(2) = a_vtk_file%xml_writer%write_step(time=0._R8P) /= 0
  error = a_vtk_file%finalize()
  ! times not increasing; a step without piece; two pieces of a grid in a step; a new array; a last step without piece
  error = initialize('refused_steps_2.vtkhdf', 'vtkhdf', 'ImageData')
  error = a_vtk_file%xml_writer%write_step(time=1._R8P)
  ok(3) = a_vtk_file%xml_writer%write_step(time=2._R8P) /= 0                             ! the step before has no piece
  error = image_step(1)
  ok(4) = a_vtk_file%xml_writer%write_step(time=1._R8P) /= 0                             ! not increasing
  ok(5) = a_vtk_file%xml_writer%write_piece(nx1=0, nx2=2, ny1=0, ny2=1, nz1=0, nz2=0) /= 0 ! a second piece in the step
  error = a_vtk_file%xml_writer%write_step(time=2._R8P)
  error = a_vtk_file%xml_writer%write_piece(nx1=0, nx2=2, ny1=0, ny2=1, nz1=0, nz2=0)
  error = a_vtk_file%xml_writer%write_dataarray(location='node', action='open')
  ok(6) = a_vtk_file%xml_writer%write_dataarray(data_name='new', x=[(1._R8P, f=1, 6)]) /= 0 ! not in the first step
  error = a_vtk_file%xml_writer%write_dataarray(location='node', action='close')
  error = a_vtk_file%xml_writer%write_piece()
  error = a_vtk_file%xml_writer%write_step(time=3._R8P)
  ok(7) = a_vtk_file%finalize() == 0                                         ! accepted: a last step without piece (reused)
  ! an UnstructuredGrid: a step with pieces leaves out an array; a step without pieces writes an array of other size, then
  ! a piece
  error = initialize('refused_steps_3.vtkhdf', 'vtkhdf', 'UnstructuredGrid')
  error = a_vtk_file%xml_writer%write_step(time=1._R8P)
  error = unst_step(1)
  error = a_vtk_file%xml_writer%write_step(time=2._R8P)
  error = a_vtk_file%xml_writer%write_piece(np=4, nc=1)
  error = a_vtk_file%xml_writer%write_geo(np=4, nc=1, x=[(real(f, R8P), f=0, 3)], y=[(0._R8P, f=0, 3)], z=[(0._R8P, f=0, 3)])
  error = a_vtk_file%xml_writer%write_connectivity(nc=1, connectivity=[0, 1, 2, 3], offset=[4], cell_type=[10_I1P])
  ok(8) = a_vtk_file%xml_writer%write_piece() /= 0                                     ! pd and cid left out of a piece
  error = a_vtk_file%finalize()
  error = initialize('refused_steps_4.vtkhdf', 'vtkhdf', 'UnstructuredGrid')
  error = a_vtk_file%xml_writer%write_step(time=1._R8P)
  error = unst_step(1)
  error = a_vtk_file%xml_writer%write_step(time=2._R8P)
  error = a_vtk_file%xml_writer%write_dataarray(location='node', action='open')
  ok(9) = a_vtk_file%xml_writer%write_dataarray(data_name='pd', x=[(1._R8P, f=1, 3)]) /= 0 ! 3 values, not the 13 points
  error = a_vtk_file%xml_writer%write_dataarray(location='node', action='close')
  error = a_vtk_file%finalize()
  error = initialize('refused_steps_5.vtkhdf', 'vtkhdf', 'UnstructuredGrid')
  error = a_vtk_file%xml_writer%write_step(time=1._R8P)
  error = unst_step(1)
  error = a_vtk_file%xml_writer%write_step(time=2._R8P)
  error = a_vtk_file%xml_writer%write_dataarray(location='cell', action='open')
  error = a_vtk_file%xml_writer%write_dataarray(data_name='cid', x=[1, 2])
  error = a_vtk_file%xml_writer%write_dataarray(location='cell', action='close')
  ok(10) = a_vtk_file%xml_writer%write_piece(np=4, nc=1) /= 0                           ! a piece after the arrays
  error = a_vtk_file%finalize()
  refusals = all(ok)
  if (.not.refusals) print '(A,10L2)', 'refusals: ', ok
  endfunction refusals
endprogram vtk_fortran_write_vtkhdf_steps
