program ref_reading
!< The examples of the reference page "Reading files": a serial file, a parallel header, multi-block and time series files
!< (written by ref_parallel, which runs before).
use penf, only : I1P, I4P, R8P
use vtk_fortran, only : vtk_file
implicit none

call write_mesh
call read_serial
call read_parallel
call read_composite
contains
  subroutine write_mesh
  !< Write ref_mesh.vtu: one tetrahedron with the point arrays pressure and velocity.
  type(vtk_file) :: a_vtk_file
  integer(I4P)   :: error

  error = a_vtk_file%initialize(format='raw', filename='ref_mesh.vtu', mesh_topology='UnstructuredGrid')
  error = a_vtk_file%xml_writer%write_piece(np=4, nc=1)
  error = a_vtk_file%xml_writer%write_geo(np=4, nc=1, x=[0._R8P, 1._R8P, 0._R8P, 0._R8P], &
                                          y=[0._R8P, 0._R8P, 1._R8P, 0._R8P], z=[0._R8P, 0._R8P, 0._R8P, 1._R8P])
  error = a_vtk_file%xml_writer%write_connectivity(nc=1, connectivity=[0,1,2,3], offset=[4], cell_type=[10_I1P])
  error = a_vtk_file%xml_writer%write_dataarray(location='node', action='open')
  error = a_vtk_file%xml_writer%write_dataarray(data_name='pressure', x=[1._R8P, 2._R8P, 3._R8P, 4._R8P])
  error = a_vtk_file%xml_writer%write_dataarray(data_name='velocity', x=[1._R8P, 1._R8P, 1._R8P, 1._R8P], &
                                                y=[0._R8P, 0._R8P, 0._R8P, 0._R8P], z=[2._R8P, 2._R8P, 2._R8P, 2._R8P])
  error = a_vtk_file%xml_writer%write_dataarray(location='node', action='close')
  error = a_vtk_file%xml_writer%write_piece()
  error = a_vtk_file%finalize()
  endsubroutine write_mesh

  subroutine read_serial
  use vtk_fortran, only : vtk_file
  use penf,        only : I1P, I4P, I8P, R8P

  type(vtk_file)                :: a_vtk_file
  character(len=:), allocatable :: topology, names(:)
  real(R8P),        allocatable :: x(:), y(:), z(:), pressure(:), velocity(:,:)
  integer(I4P),     allocatable :: connectivity(:), offset(:)
  integer(I1P),     allocatable :: cell_type(:)
  integer(I8P)                  :: np, nc
  integer(I4P)                  :: error

  error = a_vtk_file%initialize(filename='ref_mesh.vtu', action='read')
  error = a_vtk_file%xml_reader%get_info(mesh_topology=topology)
  error = a_vtk_file%xml_reader%read_piece(np=np, nc=nc)
  error = a_vtk_file%xml_reader%read_geo(x=x, y=y, z=z)
  error = a_vtk_file%xml_reader%read_connectivity(connectivity=connectivity, offset=offset, cell_type=cell_type)
  error = a_vtk_file%xml_reader%get_dataarray_names(location='node', names=names)
  error = a_vtk_file%xml_reader%read_dataarray(location='node', data_name='pressure', x=pressure)
  error = a_vtk_file%xml_reader%read_dataarray(location='node', data_name='velocity', x=velocity) ! shape (3, np)
  error = a_vtk_file%finalize()
  if (error /= 0 .or. size(velocity, dim=1) /= 3 .or. size(pressure) /= np) error stop 'ref_mesh.vtu: read error'
  print '(A,I0,A,I0,A)', 'ref_mesh.vtu: '//topology//', ', np, ' points, ', nc, ' cells, arrays: '// &
                         trim(names(1))//', '//trim(names(2))
  endsubroutine read_serial

  subroutine read_parallel
  use vtk_fortran, only : pvtk_file
  type(pvtk_file)               :: a_pvtk_file
  character(len=:), allocatable :: sources(:), names(:), message
  integer(I4P)                  :: error

  error = a_pvtk_file%initialize(filename='ref.pvtu', action='read')
  error = a_pvtk_file%xml_reader%get_sources(sources)                              ! files of the pieces
  error = a_pvtk_file%xml_reader%get_dataarray_names(location='node', names=names) ! declared point arrays
  error = a_pvtk_file%xml_reader%check_pieces(message=message)                     ! 0, or 7 and the first mismatch
  error = a_pvtk_file%finalize()
  if (error /= 0 .or. size(sources) /= 2) error stop 'ref.pvtu: read error'
  print '(A)', 'ref.pvtu: pieces '//trim(sources(1))//', '//trim(sources(2))//'; point array '//trim(names(1))
  endsubroutine read_parallel

  subroutine read_composite
  use vtk_fortran, only : vtm_file, pvd_file
  type(vtm_file)                :: a_vtm_file
  type(pvd_file)                :: a_pvd_file
  integer(I4P),     allocatable :: level(:), index(:)
  character(len=:), allocatable :: kind(:), name(:), file(:)
  real(R8P),        allocatable :: timestep(:)
  integer(I4P)                  :: error

  error = a_vtm_file%initialize(filename='ref_machine.vtm', action='read')
  error = a_vtm_file%get_entries(level=level, kind=kind, index=index, name=name, file=file)
  error = a_vtm_file%finalize()

  error = a_pvd_file%initialize(filename='ref_simulation.pvd', action='read')
  error = a_pvd_file%get_datasets(timestep=timestep, file=file)  ! also part, group, name
  error = a_pvd_file%finalize()
  if (error /= 0 .or. size(level) == 0 .or. size(timestep) == 0) error stop 'ref_machine.vtm, ref_simulation.pvd: read error'
  print '(A,I0,A,I0,A)', 'ref_machine.vtm: ', size(level), ' entries; ref_simulation.pvd: ', size(timestep), ' datasets'
  endsubroutine read_composite
endprogram ref_reading
