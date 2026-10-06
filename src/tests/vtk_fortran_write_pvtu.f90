!< VTK_Fortran test: write PVTU file (parallel, partitioned unstructured grid, issue #32).
program vtk_fortran_write_pvtu
!< VTK_Fortran test: write PVTU file (parallel, partitioned unstructured grid, issue #32).
!<
!< A mesh of two hexahedra split in two pieces, as two processes of a parallel code would write it: each piece is a complete
!< `.vtu` file with its own (local) points and connectivity, the `.pvtu` file only declares the data layout (PPoints, PPointData,
!< PCellData) and lists the pieces. Point ids in each piece are local to that piece.
use penf
use vtk_fortran, only : vtk_file, pvtk_file

implicit none
integer(I4P), parameter :: np=8_I4P                                         !< Number of points of each piece.
integer(I4P), parameter :: nc=1_I4P                                         !< Number of cells of each piece.
integer(I4P), parameter :: connect(8)=[0,1,2,3,4,5,6,7]                     !< Connectivity of each piece (local ids).
integer(I4P), parameter :: offset(nc)=[8]                                   !< Cells offset.
integer(I1P), parameter :: cell_type(nc)=[12_I1P]                           !< Cells type: hexahedron.
character(*), parameter :: pieces(2)=['vtkfortran_write_pvtu_01.vtu', &
                                      'vtkfortran_write_pvtu_02.vtu']       !< Pieces file name.
integer(I4P)            :: p                                                !< Counter.
integer(I4P)            :: error                                            !< Status error.

do p=1, size(pieces)
  call write_piece(part=p, filename=pieces(p))
enddo
call write_pvtu(filename='vtkfortran_write_pvtu.pvtu')

print "(A,L1)", new_line('a')//'Are all tests passed? ', error==0
stop
contains
  subroutine write_piece(part, filename)
  !< Write one piece: hexahedron number `part`, shifted along x.
  integer(I4P), intent(in) :: part         !< Piece number.
  character(*), intent(in) :: filename     !< Output file name.
  type(vtk_file)           :: a_vtk_file   !< A VTK file.
  real(R8P)                :: x(np)        !< X coordinates.
  real(R8P)                :: y(np)        !< Y coordinates.
  real(R8P)                :: z(np)        !< Z coordinates.
  real(R8P)                :: temp(np)     !< Point-centered variable.
  integer(I4P)             :: part_id(nc)  !< Cell-centered variable.

  x = real([0,1,1,0,0,1,1,0] + (part - 1), R8P)
  y = real([0,0,1,1,0,0,1,1], R8P)
  z = real([0,0,0,0,1,1,1,1], R8P)
  temp = x + y + z
  part_id = part
  error = a_vtk_file%initialize(format='binary', filename=filename, mesh_topology='UnstructuredGrid')
  error = a_vtk_file%xml_writer%write_piece(np=np, nc=nc)
  error = a_vtk_file%xml_writer%write_geo(np=np, nc=nc, x=x, y=y, z=z)
  error = a_vtk_file%xml_writer%write_connectivity(nc=nc, connectivity=connect, offset=offset, cell_type=cell_type)
  error = a_vtk_file%xml_writer%write_dataarray(location='node', action='open')
  error = a_vtk_file%xml_writer%write_dataarray(data_name='temperature', x=temp)
  error = a_vtk_file%xml_writer%write_dataarray(location='node', action='close')
  error = a_vtk_file%xml_writer%write_dataarray(location='cell', action='open')
  error = a_vtk_file%xml_writer%write_dataarray(data_name='part', x=part_id)
  error = a_vtk_file%xml_writer%write_dataarray(location='cell', action='close')
  error = a_vtk_file%xml_writer%write_piece()
  error = a_vtk_file%finalize()
  endsubroutine write_piece

  subroutine write_pvtu(filename)
  !< Write the parallel file: data layout of the pieces and the list of pieces.
  character(*), intent(in) :: filename    !< Output file name.
  type(pvtk_file)          :: a_pvtk_file !< A parallel (partitioned) VTK file.
  integer(I4P)             :: p           !< Counter.

  ! mesh_kind is the type of the points coordinates (PPoints) of the pieces
  error = a_pvtk_file%initialize(filename=filename, mesh_topology='PUnstructuredGrid', mesh_kind='Float64')
  ! each data array of the pieces must be declared with name, type and number of components
  error = a_pvtk_file%xml_writer%write_dataarray(location='node', action='open')
  error = a_pvtk_file%xml_writer%write_parallel_dataarray(data_name='temperature', data_type='Float64', number_of_components=1)
  error = a_pvtk_file%xml_writer%write_dataarray(location='node', action='close')
  error = a_pvtk_file%xml_writer%write_dataarray(location='cell', action='open')
  error = a_pvtk_file%xml_writer%write_parallel_dataarray(data_name='part', data_type='Int32', number_of_components=1)
  error = a_pvtk_file%xml_writer%write_dataarray(location='cell', action='close')
  ! unstructured pieces have no extents: only their file names are listed
  do p=1, size(pieces)
    error = a_pvtk_file%xml_writer%write_parallel_geo(source=pieces(p))
  enddo
  error = a_pvtk_file%finalize()
  endsubroutine write_pvtu
endprogram vtk_fortran_write_pvtu
