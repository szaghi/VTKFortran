!< VTK_Fortran test: designate the active arrays of PointData/CellData (Scalars, Vectors, Normals, Tensors, TCoords).
program vtk_fortran_write_active_arrays
!< VTK_Fortran test: designate the active arrays of PointData/CellData (Scalars, Vectors, Normals, Tensors, TCoords).
!<
!< One hexahedron with an array for each role, written in a `.vtu` file and declared in a `.pvtu` header: the test checks
!< the attributes written in the data tags of both files.
use penf
use vtk_fortran, only : vtk_file, pvtk_file

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
real(R8P)               :: stress(9,np)                                 !< Tensor at points (9 components).
integer(I4P)            :: error                                        !< Status error.
logical                 :: test_passed(4)                               !< List of passed tests.

stress = 1._R8P
call write_vtu(filename='vtkfortran_write_active_arrays.vtu')
call write_pvtu(filename='vtkfortran_write_active_arrays.pvtu', source='vtkfortran_write_active_arrays.vtu')
test_passed(1) = has_line(filename='vtkfortran_write_active_arrays.vtu', tag=point_tag)
test_passed(2) = has_line(filename='vtkfortran_write_active_arrays.vtu', tag=cell_tag)
test_passed(3) = has_line(filename='vtkfortran_write_active_arrays.pvtu', tag=ppoint_tag)
test_passed(4) = error == 0

print "(A,L1)", new_line('a')//'Are all tests passed? ', all(test_passed)
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
