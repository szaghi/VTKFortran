program vtkhdf_motion
!< A time series of several meshes in one VTKHDF file: a wave (a PolyData surface) and a buoy riding it (an
!< UnstructuredGrid of hexahedra), each written as its own time series, then copied as the blocks of one multi-block
!< file by `vtm_file`. A static block is refused in a time series. The file is read back with the library: its entries
!< with `vtm_file`, the steps of each dataset with `vtk_file` (`dataset=`).
use penf, only : I1P, I4P, R8P
use vtk_fortran, only : vtk_file, vtm_file
implicit none
integer(I4P), parameter   :: nx=16, ny=8, n_steps=6
real(R8P),    parameter   :: pi = acos(-1._R8P)
type(vtk_file)            :: a_vtk_file
type(vtm_file)            :: a_vtm_file
integer(I4P)              :: s, e, error
integer(I4P), allocatable :: level(:)
character(len=:), allocatable :: kind(:), name(:), file(:), topology
real(R8P),    allocatable :: times(:)

! each mesh is a time series of its own, with the same times
error = a_vtk_file%initialize(format='vtkhdf', filename='sea.vtkhdf', mesh_topology='PolyData')
do s=0, n_steps - 1
  error = sea(time=0.25_R8P * s)
enddo
error = a_vtk_file%finalize()
error = a_vtk_file%initialize(format='vtkhdf', filename='buoy.vtkhdf', mesh_topology='UnstructuredGrid')
do s=0, n_steps - 1
  error = buoy(time=0.25_R8P * s)
enddo
error = a_vtk_file%finalize()
! the multi-block file: the two series copied as its blocks
error = a_vtm_file%initialize(filename='harbour.vtkhdf', format='vtkhdf')
error = a_vtm_file%write_block(filenames='sea.vtkhdf', name='water')
error = a_vtm_file%write_block(filenames='buoy.vtkhdf', name='bodies')
error = a_vtm_file%finalize()
if (error /= 0) error stop 'write error'

! a static block (one mesh, no steps) is refused in a time series
error = a_vtk_file%initialize(format='vtkhdf', filename='pier.vtkhdf', mesh_topology='UnstructuredGrid')
error = a_vtk_file%xml_writer%write_piece(np=4, nc=1)
error = a_vtk_file%xml_writer%write_geo(np=4, nc=1, x=[0._R8P, 1._R8P, 0._R8P, 0._R8P], y=[0._R8P, 0._R8P, 1._R8P, 0._R8P], &
                                        z=[0._R8P, 0._R8P, 0._R8P, 1._R8P])
error = a_vtk_file%xml_writer%write_connectivity(nc=1, connectivity=[0, 1, 2, 3], offset=[4], cell_type=[10_I1P])
error = a_vtk_file%xml_writer%write_piece()
error = a_vtk_file%finalize()
error = a_vtm_file%initialize(filename='refused.vtkhdf', format='vtkhdf')
error = a_vtm_file%write_block(filenames='buoy.vtkhdf', name='bodies')
error = a_vtm_file%write_block(filenames='pier.vtkhdf', name='pier')
print '(A,I0)', 'the static block: error ', error

! the entries of the multi-block file, then each dataset (its path in the file) read as a .vtkhdf of its own
error = a_vtm_file%initialize(filename='harbour.vtkhdf', action='read')
error = a_vtm_file%get_entries(level=level, kind=kind, name=name, file=file)
error = a_vtm_file%finalize()
print '(A)', 'harbour.vtkhdf, its blocks and datasets:'
do e=1, size(level)
  if (trim(kind(e)) == 'block') then
    print '(A)', repeat('  ', level(e))//'block '//trim(name(e))
  else
    error = a_vtk_file%initialize(filename='harbour.vtkhdf', action='read', dataset=trim(file(e)))
    error = a_vtk_file%xml_reader%get_info(mesh_topology=topology)
    error = a_vtk_file%xml_reader%get_steps(times=times)
    print '(A,I0,A,*(F5.2))', repeat('  ', level(e))//'dataset '//trim(name(e))//' ('//trim(file(e))//', '// &
                              topology//'): ', size(times), ' steps, at', times
    error = a_vtk_file%finalize()
  endif
enddo
if (error /= 0) error stop 'read error'
contains
  function sea(time) result(error)
  !< One step of the sea: a surface of nx x ny quadrilaterals, its height a wave travelling along x.
  real(R8P), intent(in) :: time  !< Time.
  integer(I4P)          :: error !< Error status.
  real(R8P)             :: x(0:nx,0:ny), y(0:nx,0:ny), z(0:nx,0:ny)
  integer(I4P)          :: i, j, c(4 * nx * ny)

  do j=0, ny ; do i=0, nx
    x(i,j) = i ; y(i,j) = j ; z(i,j) = wave(real(i, R8P), time)
  enddo ; enddo
  do j=0, ny - 1 ; do i=0, nx - 1 ! the corners of each quadrilateral, counter-clockwise
    c(4 * (j * nx + i) + 1:4 * (j * nx + i) + 4) = [i + j * (nx + 1), i + 1 + j * (nx + 1), i + 1 + (j + 1) * (nx + 1), &
                                                    i + (j + 1) * (nx + 1)]
  enddo ; enddo
  error = a_vtk_file%xml_writer%write_step(time=time)
  error = a_vtk_file%xml_writer%write_piece(np=(nx + 1) * (ny + 1), nverts=0, nlines=0, nstrips=0, npolys=nx * ny)
  error = a_vtk_file%xml_writer%write_geo(np=(nx + 1) * (ny + 1), nc=nx * ny, x=reshape(x, [size(x)]), &
                                          y=reshape(y, [size(y)]), z=reshape(z, [size(z)]))
  error = a_vtk_file%xml_writer%write_polydata_cells(polys_connectivity=c, polys_offset=[(4 * i, i=1, nx * ny)])
  error = a_vtk_file%xml_writer%write_dataarray(location='node', action='open')
  error = a_vtk_file%xml_writer%write_dataarray(data_name='height', x=reshape(z, [size(z)]))
  error = a_vtk_file%xml_writer%write_dataarray(location='node', action='close')
  error = a_vtk_file%xml_writer%write_piece()
  endfunction sea

  function buoy(time) result(error)
  !< One step of the buoy: two hexahedra (a 2 x 2 x 0.4 box) at x = 6...8, lifted by the wave at its centre.
  real(R8P), intent(in) :: time  !< Time.
  integer(I4P)          :: error !< Error status.
  real(R8P)             :: x(12), y(12), z(12)
  integer(I4P)          :: p

  do p=0, 11 ! x = 6 + mod(p, 3), y = 3 + 2 mod(p / 3, 2), z = the wave at x = 7, -0.2 or +0.2
    x(p + 1) = 6 + mod(p, 3) ; y(p + 1) = 3 + 2 * mod(p / 3, 2) ; z(p + 1) = wave(7._R8P, time) - 0.2_R8P + 0.4_R8P * (p / 6)
  enddo
  error = a_vtk_file%xml_writer%write_step(time=time)
  error = a_vtk_file%xml_writer%write_piece(np=12, nc=2)
  error = a_vtk_file%xml_writer%write_geo(np=12, nc=2, x=x, y=y, z=z)
  error = a_vtk_file%xml_writer%write_connectivity(nc=2, connectivity=[0, 1, 4, 3, 6, 7, 10, 9, 1, 2, 5, 4, 7, 8, 11, 10], &
                                                   offset=[8, 16], cell_type=[12_I1P, 12_I1P])
  error = a_vtk_file%xml_writer%write_dataarray(location='node', action='open')
  error = a_vtk_file%xml_writer%write_dataarray(data_name='height', x=z)
  error = a_vtk_file%xml_writer%write_dataarray(location='node', action='close')
  error = a_vtk_file%xml_writer%write_piece()
  endfunction buoy

  pure function wave(x, time) result(z)
  !< The height of the wave at x and time.
  real(R8P), intent(in) :: x, time
  real(R8P)             :: z

  z = 0.6_R8P * sin(2._R8P * pi * (x / nx - 0.5_R8P * time))
  endfunction wave
endprogram vtkhdf_motion
