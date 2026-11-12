!run ref_data ref_data
!run ref_data-trap grep NumberOfComponents ref_trap.vts
program ref_data
!< The examples of the reference page "Data arrays": every shape write_dataarray accepts, read back to show the number of
!< components and tuples each one is written with; field data, active arrays, unsigned arrays, partitions with global
!< ids and ghost points (their roles read back).
use penf, only : I1P, I4P, I8P, R8P
use vtk_fortran, only : vtk_file
implicit none

call shapes
call trap
call field_data
call active_arrays
call unsigned_arrays
call partitions
call report('ref_shapes.vts', 'node', show=.true.)
call report('ref_fielddata.vtu', 'field')
call report('ref_active.vtu', 'node')
call report('ref_unsigned.vtu', 'cell')
contains
  subroutine shapes
  !< Every shape of write_dataarray, on a grid of 2x2x2 points.
  type(vtk_file) :: a_vtk_file
  integer(I4P)   :: error
  !region shapes
  real(R8P) :: s(8)                        ! rank 1: one value per point
  real(R8P) :: t(2,2,2)                    ! rank 3, (i,j,k): a scalar field, needs one_component=.true.
  real(R8P) :: vec(3,8)                    ! rank 2, (components, points)
  real(R8P) :: vec4(3,2,2,2)               ! rank 4, (components, i, j, k)
  real(R8P) :: vx(8), vy(8), vz(8)         ! the 3 components as separate rank-1 arrays
  real(R8P) :: wx(2,2,2), wy(2,2,2), wz(2,2,2) ! the 3 components as separate rank-3 arrays
  real(R8P) :: txx(8), tyy(8), tzz(8), txy(8), tyz(8), txz(8) ! a symmetric tensor as 6 separate arrays
  real(R8P) :: full(9,8)                   ! rank 2: a full tensor, 9 components
  !endregion shapes

  s = 1 ; t = 2 ; vec = 3 ; vec4 = 4 ; vx = 5 ; vy = 5 ; vz = 5 ; wx = 6 ; wy = 6 ; wz = 6
  txx = 7 ; tyy = 7 ; tzz = 7 ; txy = 0 ; tyz = 0 ; txz = 0 ; full = 8
  error = a_vtk_file%initialize(format='ascii', filename='ref_shapes.vts', mesh_topology='StructuredGrid', &
                                nx1=1, nx2=2, ny1=1, ny2=2, nz1=1, nz2=2)
  error = a_vtk_file%xml_writer%write_piece(nx1=1, nx2=2, ny1=1, ny2=2, nz1=1, nz2=2)
  error = a_vtk_file%xml_writer%write_geo(n=8, x=reshape([1,2,1,2,1,2,1,2], [2,2,2]) + 0._R8P, &
                                               y=reshape([1,1,2,2,1,1,2,2], [2,2,2]) + 0._R8P, &
                                               z=reshape([1,1,1,1,2,2,2,2], [2,2,2]) + 0._R8P)
  error = a_vtk_file%xml_writer%write_dataarray(location='node', action='open')
  !region shapes-write
  error = a_vtk_file%xml_writer%write_dataarray(data_name='s',    x=s)
  error = a_vtk_file%xml_writer%write_dataarray(data_name='t',    x=t, one_component=.true.)
  error = a_vtk_file%xml_writer%write_dataarray(data_name='vec',  x=vec)
  error = a_vtk_file%xml_writer%write_dataarray(data_name='vec4', x=vec4)
  error = a_vtk_file%xml_writer%write_dataarray(data_name='v',    x=vx, y=vy, z=vz)
  error = a_vtk_file%xml_writer%write_dataarray(data_name='w',    x=wx, y=wy, z=wz)
  error = a_vtk_file%xml_writer%write_dataarray(data_name='sym',  u=txx, v=tyy, w=tzz, x=txy, y=tyz, z=txz)
  error = a_vtk_file%xml_writer%write_dataarray(data_name='full', x=full)
  !endregion shapes-write
  error = a_vtk_file%xml_writer%write_dataarray(location='node', action='close')
  error = a_vtk_file%xml_writer%write_piece()
  error = a_vtk_file%finalize()
  endsubroutine shapes

  subroutine trap
  !< The rank-3 scalar field written without one_component: the first dimension is taken as the components.
  type(vtk_file) :: a_vtk_file
  real(R8P)      :: t(2,2,2)
  integer(I4P)   :: error

  t = 2
  error = a_vtk_file%initialize(format='ascii', filename='ref_trap.vts', mesh_topology='StructuredGrid', &
                                nx1=1, nx2=2, ny1=1, ny2=2, nz1=1, nz2=2)
  error = a_vtk_file%xml_writer%write_piece(nx1=1, nx2=2, ny1=1, ny2=2, nz1=1, nz2=2)
  error = a_vtk_file%xml_writer%write_dataarray(location='node', action='open')
  !region trap
  error = a_vtk_file%xml_writer%write_dataarray(data_name='t_wrong', x=t)   ! WRONG: 2 components, 4 tuples
  !endregion trap
  error = a_vtk_file%xml_writer%write_dataarray(location='node', action='close')
  error = a_vtk_file%xml_writer%write_piece()
  error = a_vtk_file%finalize()
  endsubroutine trap

  subroutine field_data
  type(vtk_file) :: a_vtk_file
  integer(I4P)   :: error

  error = a_vtk_file%initialize(format='binary', filename='ref_fielddata.vtu', mesh_topology='UnstructuredGrid')
  !region fielddata
  error = a_vtk_file%xml_writer%write_fielddata(action='open')
  error = a_vtk_file%xml_writer%write_fielddata(data_name='TIME',      x=0.5_R8P)                     ! scalar
  error = a_vtk_file%xml_writer%write_fielddata(data_name='CYCLE',     x=7_I8P)                       ! scalar
  error = a_vtk_file%xml_writer%write_fielddata(data_name='residuals', x=[1.e-3_R8P, 1.e-4_R8P])      ! rank-1 array
  error = a_vtk_file%xml_writer%write_fielddata(data_name='solver',    x='my solver v1.2')            ! string
  error = a_vtk_file%xml_writer%write_fielddata(data_name='species',   x=['N2', 'O2'])                ! array of strings
  error = a_vtk_file%xml_writer%write_fielddata(action='close')
  !endregion fielddata
  call one_tetrahedron(a_vtk_file)
  error = a_vtk_file%finalize()
  endsubroutine field_data

  subroutine active_arrays
  type(vtk_file) :: a_vtk_file
  real(R8P)      :: t(4), p(4), u(4), v(4), w(4)
  integer(I4P)   :: error

  t = 300 ; p = 1.e5_R8P ; u = 1 ; v = 0 ; w = 0
  error = a_vtk_file%initialize(format='binary', filename='ref_active.vtu', mesh_topology='UnstructuredGrid')
  call one_tetrahedron(a_vtk_file, open_piece_only=.true.)
  !region active
  error = a_vtk_file%xml_writer%write_dataarray(location='node', action='open', scalars='pressure', vectors='velocity')
  error = a_vtk_file%xml_writer%write_dataarray(data_name='temperature', x=t)              ! not active
  error = a_vtk_file%xml_writer%write_dataarray(data_name='pressure', x=p)                 ! active scalars
  error = a_vtk_file%xml_writer%write_dataarray(data_name='velocity', x=u, y=v, z=w)       ! active vectors
  error = a_vtk_file%xml_writer%write_dataarray(location='node', action='close')
  !endregion active
  error = a_vtk_file%xml_writer%write_piece()
  error = a_vtk_file%finalize()
  endsubroutine active_arrays

  subroutine unsigned_arrays
  type(vtk_file) :: a_vtk_file
  integer(I4P)   :: error
  integer(I1P)   :: ghost(1)

  ghost = 1_I1P
  error = a_vtk_file%initialize(format='binary', filename='ref_unsigned.vtu', mesh_topology='UnstructuredGrid')
  call one_tetrahedron(a_vtk_file, open_piece_only=.true.)
  !region unsigned
  error = a_vtk_file%xml_writer%write_dataarray(location='cell', action='open')
  error = a_vtk_file%xml_writer%write_dataarray_unsigned(data_name='vtkGhostType', x=ghost)
  error = a_vtk_file%xml_writer%write_dataarray(location='cell', action='close')
  !endregion unsigned
  error = a_vtk_file%xml_writer%write_piece()
  error = a_vtk_file%finalize()
  endsubroutine unsigned_arrays

  subroutine partitions
  !< Two tetrahedra sharing a face, a piece each: the points of the face are in both, with the same global ids, ghosts
  !< (duplicates) in the second; the active arrays of the roles read back.
  type(vtk_file)                :: a_vtk_file
  character(len=:), allocatable :: name
  integer(I4P)                  :: error

  error = a_vtk_file%initialize(format='binary', filename='ref_partitions.vtu', mesh_topology='UnstructuredGrid')
  call piece(a_vtk_file, x=[0._R8P, 1._R8P, 0._R8P, 0._R8P], y=[0._R8P, 0._R8P, 1._R8P, 0._R8P], z=[0._R8P, 0._R8P, 0._R8P, 1._R8P], &
             ids=[0_I8P, 1_I8P, 2_I8P, 3_I8P], ghost=[0_I1P, 0_I1P, 0_I1P, 0_I1P], cell_id=0_I8P)
  call piece(a_vtk_file, x=[1._R8P, 0._R8P, 0._R8P, 1._R8P], y=[0._R8P, 1._R8P, 0._R8P, 1._R8P], z=[0._R8P, 0._R8P, 1._R8P, 1._R8P], &
             ids=[1_I8P, 2_I8P, 3_I8P, 4_I8P], ghost=[1_I1P, 1_I1P, 1_I1P, 0_I1P], cell_id=1_I8P)
  error = a_vtk_file%finalize()
  error = a_vtk_file%initialize(filename='ref_partitions.vtu', action='read')
  error = max(error, a_vtk_file%xml_reader%get_active(location='node', role='GlobalIds', data_name=name, piece=2))
  if (error /= 0 .or. name /= 'id') error stop 'GlobalIds not read back'
  error = a_vtk_file%finalize()
  endsubroutine partitions

  subroutine piece(a_vtk_file, x, y, z, ids, ghost, cell_id)
  !< A tetrahedron as a piece, its global ids and ghost points, its cell global id.
  type(vtk_file), intent(inout) :: a_vtk_file
  real(R8P),    intent(in) :: x(4), y(4), z(4)
  integer(I8P), intent(in) :: ids(4), cell_id
  integer(I1P), intent(in) :: ghost(4)
  integer(I4P)              :: error

  !region partitions
  error = a_vtk_file%xml_writer%write_piece(np=4, nc=1)
  error = a_vtk_file%xml_writer%write_geo(np=4, nc=1, x=x, y=y, z=z)
  error = a_vtk_file%xml_writer%write_connectivity(nc=1, connectivity=[0,1,2,3], offset=[4], cell_type=[10_I1P])
  error = a_vtk_file%xml_writer%write_dataarray(location='node', action='open', global_ids='id')
  error = a_vtk_file%xml_writer%write_dataarray(data_name='id', x=ids)                       ! the same id in both
  error = a_vtk_file%xml_writer%write_dataarray_unsigned(data_name='vtkGhostType', x=ghost)  ! 1: a duplicate
  error = a_vtk_file%xml_writer%write_dataarray(location='node', action='close')
  error = a_vtk_file%xml_writer%write_dataarray(location='cell', action='open', global_ids='cell_id')
  error = a_vtk_file%xml_writer%write_dataarray(data_name='cell_id', x=[cell_id])
  error = a_vtk_file%xml_writer%write_dataarray(location='cell', action='close')
  error = a_vtk_file%xml_writer%write_piece()
  !endregion partitions
  endsubroutine piece

  subroutine one_tetrahedron(a_vtk_file, open_piece_only)
  !< Write a piece of one tetrahedron: closed, or left open for its data.
  type(vtk_file), intent(inout)        :: a_vtk_file
  logical,        intent(in), optional :: open_piece_only
  integer(I4P)                         :: error

  error = a_vtk_file%xml_writer%write_piece(np=4, nc=1)
  error = a_vtk_file%xml_writer%write_geo(np=4, nc=1, x=[0._R8P, 1._R8P, 0._R8P, 0._R8P], &
                                          y=[0._R8P, 0._R8P, 1._R8P, 0._R8P], z=[0._R8P, 0._R8P, 0._R8P, 1._R8P])
  error = a_vtk_file%xml_writer%write_connectivity(nc=1, connectivity=[0,1,2,3], offset=[4], cell_type=[10_I1P])
  if (present(open_piece_only)) then
    if (open_piece_only) return
  endif
  error = a_vtk_file%xml_writer%write_piece()
  endsubroutine one_tetrahedron

  subroutine report(filename, location, show)
  !< Read back every array of a location: stop on an error; print their type, components and tuples if show.
  character(*), intent(in)           :: filename, location
  logical,      intent(in), optional :: show
  type(vtk_file)                :: a_vtk_file
  character(len=:), allocatable :: names(:), data_type
  character(12)                 :: name
  integer(I4P)                  :: a, n_components, error
  integer(I8P)                  :: n_tuples

  error = a_vtk_file%initialize(filename=filename, action='read')
  if (error == 0) error = a_vtk_file%xml_reader%get_dataarray_names(location=location, names=names)
  if (error /= 0) then
    print '(A,I0)', filename//': read error ', error
    error stop
  endif
  if (present(show)) print '(A)', filename//', '//location//' data:'
  do a=1, size(names)
    error = a_vtk_file%xml_reader%get_dataarray_info(location=location, data_name=trim(names(a)), data_type=data_type, &
                                                     n_components=n_components, n_tuples=n_tuples)
    if (error /= 0) error stop 'get_dataarray_info error'
    name = names(a)
    if (present(show)) print '(2X,A,A8,I3,A,I3,A)', name, data_type, n_components, ' components', n_tuples, ' tuples'
  enddo
  error = a_vtk_file%finalize()
  endsubroutine report
endprogram ref_data
