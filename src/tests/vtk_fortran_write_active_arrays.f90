!< VTK_Fortran test: designate the active arrays of PointData/CellData (every attribute role of VTK).
program vtk_fortran_write_active_arrays
!< VTK_Fortran test: designate the active arrays of PointData/CellData (every attribute role of VTK).
!<
!< One hexahedron with an array for each of the roles Scalars, Vectors, Normals, Tensors and TCoords, written in a `.vtu`
!< file and declared in a `.pvtu` header: the test checks the attributes written in the data tags of both files.
!<
!< Then the other roles: two hexahedra sharing a face, each a piece (a partition), with point data of global ids (the
!< shared points with the same id in both pieces), pedigree ids, edge flags, tangents, rational weights, higher order
!< degrees, process ids and the ghost points (`vtkGhostType`: the shared points of the second piece are duplicates), and
!< cell global ids: `vtkfortran_active_roles.vtu` and, with HDF5, `vtkfortran_active_roles.vtkhdf`. The tag of the `.vtu`
!< is checked, and both files are read back: the active array of each role (`get_active`). VTK readers check the roles
!< outside this test.
use penf
use vtk_fortran, only : vtk_file, pvtk_file
use vtk_fortran_hdf5, only : is_hdf5_enabled

implicit none
integer(I4P), parameter :: np=8_I4P                                     !< Number of points.
integer(I4P), parameter :: nc=1_I4P                                     !< Number of cells.
real(R8P),    parameter :: x(np)=[0,1,1,0,0,1,1,0]                      !< X coordinates.
real(R8P),    parameter :: y(np)=[0,0,1,1,0,0,1,1]                      !< Y coordinates.
real(R8P),    parameter :: z(np)=[0,0,0,0,1,1,1,1]                      !< Z coordinates.
integer(I4P), parameter :: connect(np)=[0,1,2,3,4,5,6,7]                !< Connectivity.
integer(I4P), parameter :: offset(nc)=[8]                               !< Cells offset.
integer(I1P), parameter :: cell_type(nc)=[12_I1P]                       !< Cells type: hexahedron.
character(*), parameter :: point_tag='<PointData Scalars="pressure" Vectors="velocity" Normals="normal" Tensors="stress" '// &
                                     'TCoords="uv">'                    !< Expected PointData tag.
character(*), parameter :: cell_tag='<CellData Scalars="part">'         !< Expected CellData tag.
character(*), parameter :: ppoint_tag='<PPointData Scalars="pressure" Vectors="velocity">' !< Expected PPointData tag.
character(*), parameter :: roles_tag='<PointData GlobalIds="gid" PedigreeIds="ped" EdgeFlag="edge" Tangents="tan" '// &
                                     'RationalWeights="w" HigherOrderDegrees="hod" ProcessIds="pid">' !< Expected tag.
real(R8P)               :: stress(9,np)                                 !< Tensor at points (9 components).
integer(I4P)            :: error                                        !< Status error.
logical                 :: test_passed(7)                               !< List of passed tests.

stress = 1._R8P
call write_vtu(filename='vtkfortran_write_active_arrays.vtu')
call write_pvtu(filename='vtkfortran_write_active_arrays.pvtu', source='vtkfortran_write_active_arrays.vtu')
test_passed(1) = has_line(filename='vtkfortran_write_active_arrays.vtu', tag=point_tag)
test_passed(2) = has_line(filename='vtkfortran_write_active_arrays.vtu', tag=cell_tag)
test_passed(3) = has_line(filename='vtkfortran_write_active_arrays.pvtu', tag=ppoint_tag)
test_passed(4) = error == 0
call write_roles(format='ascii', filename='vtkfortran_active_roles.vtu')
test_passed(5) = has_line(filename='vtkfortran_active_roles.vtu', tag=roles_tag) .and. error == 0
test_passed(6) = read_roles(filename='vtkfortran_active_roles.vtu')
test_passed(7) = .true.
if (is_hdf5_enabled) then
  call write_roles(format='vtkhdf', filename='vtkfortran_active_roles.vtkhdf')
  test_passed(7) = read_roles(filename='vtkfortran_active_roles.vtkhdf') .and. error == 0
endif
print '(A,7L2)', 'active arrays: vtu, cells, pvtu, errors, roles tag, roles read (vtu), roles (vtkhdf):', test_passed

print "(A,L1)", new_line('a')//'Are all tests passed? ', all(test_passed)
if (.not.all(test_passed)) error stop 'some tests failed'
stop
contains
  subroutine write_vtu(filename)
  !< Write the serial file, an array for each role.
  character(*), intent(in) :: filename   !< Output file name.
  type(vtk_file)           :: a_vtk_file !< A VTK file.

  error = a_vtk_file%initialize(format='ascii', filename=filename, mesh_topology='UnstructuredGrid')
  error = a_vtk_file%xml_writer%write_piece(np=np, nc=nc)
  error = a_vtk_file%xml_writer%write_geo(np=np, nc=nc, x=x, y=y, z=z)
  error = a_vtk_file%xml_writer%write_connectivity(nc=nc, connectivity=connect, offset=offset, cell_type=cell_type)
  error = a_vtk_file%xml_writer%write_dataarray(location='node', action='open', scalars='pressure', vectors='velocity', &
                                                normals='normal', tensors='stress', tcoords='uv')
  error = a_vtk_file%xml_writer%write_dataarray(data_name='temperature', x=x)  ! not active: first array, on purpose
  error = a_vtk_file%xml_writer%write_dataarray(data_name='pressure', x=x+y)
  error = a_vtk_file%xml_writer%write_dataarray(data_name='velocity', x=x, y=y, z=z)
  error = a_vtk_file%xml_writer%write_dataarray(data_name='normal', x=0*x, y=0*y, z=1+0*z)
  error = a_vtk_file%xml_writer%write_dataarray(data_name='stress', x=stress)
  error = a_vtk_file%xml_writer%write_dataarray(data_name='uv', x=x, y=y, z=0*z)
  error = a_vtk_file%xml_writer%write_dataarray(location='node', action='close')
  error = a_vtk_file%xml_writer%write_dataarray(location='cell', action='open', scalars='part')
  error = a_vtk_file%xml_writer%write_dataarray(data_name='part', x=[1_I4P])
  error = a_vtk_file%xml_writer%write_dataarray(location='cell', action='close')
  error = a_vtk_file%xml_writer%write_piece()
  error = a_vtk_file%finalize()
  endsubroutine write_vtu

  subroutine write_pvtu(filename, source)
  !< Write the parallel header, designating the active arrays of the pieces.
  character(*), intent(in) :: filename    !< Output file name.
  character(*), intent(in) :: source      !< Piece file name.
  type(pvtk_file)          :: a_pvtk_file !< A parallel (partitioned) VTK file.

  error = a_pvtk_file%initialize(filename=filename, mesh_topology='PUnstructuredGrid', mesh_kind='Float64')
  error = a_pvtk_file%xml_writer%write_dataarray(location='node', action='open', scalars='pressure', vectors='velocity')
  error = a_pvtk_file%xml_writer%write_parallel_dataarray(data_name='temperature', data_type='Float64', number_of_components=1)
  error = a_pvtk_file%xml_writer%write_parallel_dataarray(data_name='pressure', data_type='Float64', number_of_components=1)
  error = a_pvtk_file%xml_writer%write_parallel_dataarray(data_name='velocity', data_type='Float64', number_of_components=3)
  error = a_pvtk_file%xml_writer%write_parallel_dataarray(data_name='normal', data_type='Float64', number_of_components=3)
  error = a_pvtk_file%xml_writer%write_parallel_dataarray(data_name='stress', data_type='Float64', number_of_components=9)
  error = a_pvtk_file%xml_writer%write_parallel_dataarray(data_name='uv', data_type='Float64', number_of_components=3)
  error = a_pvtk_file%xml_writer%write_dataarray(location='node', action='close')
  error = a_pvtk_file%xml_writer%write_dataarray(location='cell', action='open')
  error = a_pvtk_file%xml_writer%write_parallel_dataarray(data_name='part', data_type='Int32', number_of_components=1)
  error = a_pvtk_file%xml_writer%write_dataarray(location='cell', action='close')
  error = a_pvtk_file%xml_writer%write_parallel_geo(source=source)
  error = a_pvtk_file%finalize()
  endsubroutine write_pvtu

  subroutine write_roles(format, filename)
  !< Two hexahedra sharing the face x=1, a piece each, with an array for each of the other roles and the ghost points.
  character(*), intent(in) :: format     !< Format.
  character(*), intent(in) :: filename   !< Output file name.
  type(vtk_file)           :: a_vtk_file !< A VTK file.
  integer(I8P)             :: gid(np)    !< Global ids of the points of a piece.
  integer(I1P)             :: ghost(np)  !< Ghost points (1: a duplicate point) of a piece.
  integer(I4P)             :: p, i       !< Piece, counter.

  error = a_vtk_file%initialize(format=format, filename=filename, mesh_topology='UnstructuredGrid')
  do p=1, 2
    if (p == 1) then
      gid = [0, 1, 2, 3, 4, 5, 6, 7] ; ghost = 0_I1P
    else ! the points of x=0 are those of x=1 of the first piece
      gid = [1, 8, 9, 2, 5, 10, 11, 6] ; ghost = [1, 0, 0, 1, 1, 0, 0, 1]
    endif
    error = max(error, abs(a_vtk_file%xml_writer%write_piece(np=np, nc=nc)))
    error = max(error, abs(a_vtk_file%xml_writer%write_geo(np=np, nc=nc, x=x + p - 1, y=y, z=z)))
    error = max(error, abs(a_vtk_file%xml_writer%write_connectivity(nc=nc, connectivity=connect, offset=offset, &
                                                                      cell_type=cell_type)))
    error = max(error, abs(a_vtk_file%xml_writer%write_dataarray(location='node', action='open', global_ids='gid',     &
                                                                   pedigree_ids='ped', edge_flags='edge', tangents='tan', &
                                                                   rational_weights='w', higher_order_degrees='hod',     &
                                                                   process_ids='pid')))
    error = max(error, abs(a_vtk_file%xml_writer%write_dataarray(data_name='gid', x=gid)))
    error = max(error, abs(a_vtk_file%xml_writer%write_dataarray(data_name='ped', x=int(gid + 100_I8P, I4P))))
    error = max(error, abs(a_vtk_file%xml_writer%write_dataarray_unsigned(data_name='edge', x=[(1_I1P, i=1, np)])))
    error = max(error, abs(a_vtk_file%xml_writer%write_dataarray(data_name='tan', x=y, y=-x, z=0*z)))
    error = max(error, abs(a_vtk_file%xml_writer%write_dataarray(data_name='w', x=1 + 0*x)))
    error = max(error, abs(a_vtk_file%xml_writer%write_dataarray(data_name='hod', x=1 + 0*x, y=1 + 0*y, z=1 + 0*z)))
    error = max(error, abs(a_vtk_file%xml_writer%write_dataarray(data_name='pid', x=[(p - 1, i=1, np)])))
    error = max(error, abs(a_vtk_file%xml_writer%write_dataarray_unsigned(data_name='vtkGhostType', x=ghost)))
    error = max(error, abs(a_vtk_file%xml_writer%write_dataarray(location='node', action='close')))
    error = max(error, abs(a_vtk_file%xml_writer%write_dataarray(location='cell', action='open', global_ids='cgid')))
    error = max(error, abs(a_vtk_file%xml_writer%write_dataarray(data_name='cgid', x=[int(p - 1, I8P)])))
    error = max(error, abs(a_vtk_file%xml_writer%write_dataarray(location='cell', action='close')))
    error = max(error, abs(a_vtk_file%xml_writer%write_piece()))
  enddo
  error = max(error, abs(a_vtk_file%finalize()))
  endsubroutine write_roles

  logical function read_roles(filename)
  !< Read back the active array of each role of the second piece; a role without array is empty, an unknown one error 4.
  character(*), intent(in)      :: filename   !< File name.
  type(vtk_file)                :: a_vtk_file !< A VTK file.
  character(len=:), allocatable :: name       !< Name of an active array.
  character(18), parameter      :: roles(7) = ['GlobalIds         ', 'PedigreeIds       ', 'EdgeFlag          ', &
                                               'Tangents          ', 'RationalWeights   ', 'HigherOrderDegrees', &
                                               'ProcessIds        '] !< Roles.
  character(4), parameter       :: names(7) = ['gid ', 'ped ', 'edge', 'tan ', 'w   ', 'hod ', 'pid '] !< Their arrays.
  integer(I4P)                  :: r, e       !< Counter, error.

  read_roles = a_vtk_file%initialize(filename=filename, action='read') == 0
  do r=1, size(roles)
    e = a_vtk_file%xml_reader%get_active(location='node', role=trim(roles(r)), data_name=name, piece=2)
    read_roles = read_roles .and. e == 0 .and. name == trim(names(r))
  enddo
  e = a_vtk_file%xml_reader%get_active(location='cell', role='globalids', data_name=name, piece=2) ! case insensitive
  read_roles = read_roles .and. e == 0 .and. name == 'cgid'
  e = a_vtk_file%xml_reader%get_active(location='node', role='Scalars', data_name=name, piece=2)  ! no array
  read_roles = read_roles .and. e == 0 .and. len(name) == 0
  read_roles = read_roles .and. a_vtk_file%xml_reader%get_active(location='node', role='Speed', data_name=name) == 4
  e = a_vtk_file%finalize()
  if (.not.read_roles) print '(A)', 'roles of '//filename//': different'
  endfunction read_roles

  function has_line(filename, tag) result(is_found)
  !< Check that the file contains a line made of the given tag (leading indentation apart).
  character(*), intent(in) :: filename !< File name.
  character(*), intent(in) :: tag      !< Expected tag.
  logical                  :: is_found !< Check result.
  character(len=1024)      :: line     !< Line buffer.
  integer(I4P)             :: u        !< File unit.
  integer(I4P)             :: iostat   !< IO status.

  is_found = .false.
  open(newunit=u, file=filename, action='read')
  do
    read(u, '(A)', iostat=iostat) line
    if (iostat /= 0) exit
    if (trim(adjustl(line)) == tag) is_found = .true.
  enddo
  close(u)
  endfunction has_line
endprogram vtk_fortran_write_active_arrays
