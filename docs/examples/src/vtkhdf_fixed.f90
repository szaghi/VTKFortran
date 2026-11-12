!hdf5
!run vtkhdf_fixed vtkhdf_fixed
!render vtkhdf_fixed plate.vtkhdf array=temperature range=0,1 edges=1 camera=xy size=560x300 zoom=1.1 fps=3 time=1
program vtkhdf_fixed
!< A time series on a fixed mesh in one VTKHDF file, the mesh stored once: the first step writes the plate (an
!< UnstructuredGrid of quadrilaterals) and its temperature, the next steps write the temperature only (steps without
!< pieces, which reuse the mesh of the step before). The same series with the mesh written at every step is written too,
!< and the points stored by each file are printed, read back with the HDF5 bindings of the library.
use penf, only : I1P, I4P, I8P, R8P
use vtk_fortran, only : vtk_file
use vtk_fortran_hdf5
implicit none
integer(I4P), parameter   :: nx=16, ny=8, n_steps=6, np=(nx + 1) * (ny + 1), nc=nx * ny
type(vtk_file)            :: a_vtk_file
real(R8P)                 :: x(np), y(np), z(np)
integer(I4P)              :: connectivity(4 * nc), offset(nc), i, j, s, error
integer(I1P)              :: cell_type(nc)
integer(hid_kind)         :: file_id, root
integer(I8P), allocatable :: dims(:), offsets(:)

do j=0, ny ; do i=0, nx
  x(1 + i + j * (nx + 1)) = i ; y(1 + i + j * (nx + 1)) = j ; z(1 + i + j * (nx + 1)) = 0
enddo ; enddo
do j=0, ny - 1 ; do i=0, nx - 1 ! the corners of each quadrilateral, counter-clockwise
  connectivity(4 * (i + j * nx) + 1:4 * (i + j * nx) + 4) = [i + j * (nx + 1), i + 1 + j * (nx + 1), &
                                                             i + 1 + (j + 1) * (nx + 1), i + (j + 1) * (nx + 1)]
enddo ; enddo
offset = [(4 * i, i=1, nc)]
cell_type = 9_I1P ! VTK_QUAD

!region write
error = a_vtk_file%initialize(format='vtkhdf', filename='plate.vtkhdf', mesh_topology='UnstructuredGrid')
do s=0, n_steps - 1
  error = a_vtk_file%xml_writer%write_step(time=0.25_R8P * s)
  if (s == 0) then ! the first step: the mesh and the temperature
    error = a_vtk_file%xml_writer%write_piece(np=np, nc=nc)
    error = a_vtk_file%xml_writer%write_geo(np=np, nc=nc, x=x, y=y, z=z)
    error = a_vtk_file%xml_writer%write_connectivity(nc=nc, connectivity=connectivity, offset=offset, cell_type=cell_type)
    error = temperature(time=0._R8P)
    error = a_vtk_file%xml_writer%write_piece()
  else             ! the next steps: no piece, the temperature only (the mesh of the step before)
    error = temperature(time=0.25_R8P * s)
  endif
enddo
error = a_vtk_file%finalize()
!endregion write
if (error /= 0) error stop 'write error'

! the same series, the mesh written at every step
error = a_vtk_file%initialize(format='vtkhdf', filename='every_step.vtkhdf', mesh_topology='UnstructuredGrid')
do s=0, n_steps - 1
  error = a_vtk_file%xml_writer%write_step(time=0.25_R8P * s)
  error = a_vtk_file%xml_writer%write_piece(np=np, nc=nc)
  error = a_vtk_file%xml_writer%write_geo(np=np, nc=nc, x=x, y=y, z=z)
  error = a_vtk_file%xml_writer%write_connectivity(nc=nc, connectivity=connectivity, offset=offset, cell_type=cell_type)
  error = temperature(time=0.25_R8P * s)
  error = a_vtk_file%xml_writer%write_piece()
enddo
error = a_vtk_file%finalize()

do i=1, 2
  error = h5_file_open(trim(merge('plate.vtkhdf     ', 'every_step.vtkhdf', i == 1)), file_id)
  error = h5_group_open(file_id, 'VTKHDF', root)
  error = h5_dataset_dims(root, 'Points', dims)
  print '(A,I5,A,I5,A)', trim(merge('plate.vtkhdf     ', 'every_step.vtkhdf', i == 1))//': ', dims(2), &
                         ' points stored, ', dims(2) * 3 * 8, ' bytes of coordinates'
  error = h5_dataset_read(root, 'Steps/PointOffsets', offsets)
  print '(A,*(1X,I3))', '  PointOffsets of the steps:', offsets
  error = h5_group_close(root)
  error = h5_file_close(file_id)
enddo
contains
  !region temperature
  function temperature(time) result(error)
  !< The temperature of the plate at a time: a hot spot moving along x, as point data.
  real(R8P), intent(in) :: time  !< Time.
  integer(I4P)          :: error !< Error status.

  error = a_vtk_file%xml_writer%write_dataarray(location='node', action='open')
  error = a_vtk_file%xml_writer%write_dataarray(data_name='temperature', &
                                                x=exp(-((x - 2 - 8 * time)**2 + (y - ny / 2._R8P)**2) / 8))
  error = a_vtk_file%xml_writer%write_dataarray(location='node', action='close')
  endfunction temperature
  !endregion temperature
endprogram vtkhdf_fixed
