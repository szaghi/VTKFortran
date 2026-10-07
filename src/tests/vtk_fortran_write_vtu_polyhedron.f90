!< VTK_Fortran test: write VTU file with polyhedron cells (regression of issue #31).
program vtk_fortran_write_vtu_polyhedron
!< VTK_Fortran test: write VTU file with polyhedron cells (regression of issue #31).
!<
!< A unit cube described as a general polyhedron (VTK_POLYHEDRON = 42) and a tetrahedron (VTK_TETRA = 10). The `faces` and
!< `faceoffsets` DataArrays describing the polyhedra **must** be children of the `Cells` element: the test checks it in the ASCII
!< output, then writes the binary and raw files too. It also checks that the cell types are written as UInt8.
use penf
use vtk_fortran, only : vtk_file

implicit none
type(vtk_file)          :: a_vtk_file                                       !< A VTK file.
integer(I4P), parameter :: np=12_I4P                                        !< Number of points.
integer(I4P), parameter :: nc=2_I4P                                         !< Number of cells.
real(R4P),    parameter :: x(np)=[0,1,1,0,0,1,1,0,2,3,2,2]                  !< X coordinates.
real(R4P),    parameter :: y(np)=[0,0,1,1,0,0,1,1,0,0,1,0]                  !< Y coordinates.
real(R4P),    parameter :: z(np)=[0,0,0,0,1,1,1,1,0,0,0,1]                  !< Z coordinates.
integer(I4P), parameter :: connect(12)=[0,1,2,3,4,5,6,7, 8,9,10,11]         !< Connectivity (points of each cell).
integer(I4P), parameter :: offset(nc)=[8,12]                                !< Cells offset.
integer(I1P), parameter :: cell_type(nc)=[42_I1P,10_I1P]                    !< Cells type: polyhedron, tetrahedron.
integer(I4P), parameter :: face(31)=[6,                    &                !< Faces stream: number of faces, then
                                     4, 0,1,2,3,           &                !< for each face its number of points and
                                     4, 4,5,6,7,           &                !< the points ids.
                                     4, 0,1,5,4,           &
                                     4, 1,2,6,5,           &
                                     4, 2,3,7,6,           &
                                     4, 3,0,4,7]
integer(I4P), parameter :: faceoffset(nc)=[31,-1]                           !< End of each cell faces, -1 if not a polyhedron.
real(R8P)               :: v(nc)=[1._R8P,2._R8P]                            !< Cell-centered variable.
logical                 :: test_passed(2)                                   !< List of passed tests.

call write_file(format='ascii', filename='XML_UNST-polyhedron-ascii.vtu')
test_passed(1) = faces_inside_cells(filename='XML_UNST-polyhedron-ascii.vtu')
call write_file(format='binary', filename='XML_UNST-polyhedron-binary.vtu')
call write_file(format='raw', filename='XML_UNST-polyhedron-raw.vtu')
test_passed(2) = types_are_uint8(filename='XML_UNST-polyhedron-ascii.vtu')

print "(A,L1)", new_line('a')//'Are all tests passed? ', all(test_passed)
if (.not.all(test_passed)) error stop 'some tests failed'
stop
contains
  subroutine write_file(format, filename)
  !< Write the polyhedron file.
  character(*), intent(in) :: format   !< File format.
  character(*), intent(in) :: filename !< File name.
  integer(I4P)             :: error    !< Status error.

  error = a_vtk_file%initialize(format=format, filename=filename, mesh_topology='UnstructuredGrid')
  error = a_vtk_file%xml_writer%write_piece(np=np, nc=nc)
  error = a_vtk_file%xml_writer%write_geo(np=np, nc=nc, x=x, y=y, z=z)
  error = a_vtk_file%xml_writer%write_connectivity(nc=nc, connectivity=connect, offset=offset, cell_type=cell_type, &
                                                   face=face, faceoffset=faceoffset)
  error = a_vtk_file%xml_writer%write_dataarray(location='cell', action='open')
  error = a_vtk_file%xml_writer%write_dataarray(data_name='cell_value', x=v)
  error = a_vtk_file%xml_writer%write_dataarray(location='cell', action='close')
  error = a_vtk_file%xml_writer%write_piece()
  error = a_vtk_file%finalize()
  endsubroutine write_file

  function faces_inside_cells(filename) result(is_inside)
  !< Check that faces and faceoffsets DataArrays are written between `<Cells>` and `</Cells>`.
  character(*), intent(in) :: filename  !< File name.
  logical                  :: is_inside !< Check result.
  character(len=1024)      :: line      !< Line buffer.
  logical                  :: in_cells  !< Flag: inside Cells element.
  integer(I4P)             :: found     !< Number of polyhedra DataArrays found inside Cells.
  integer(I4P)             :: u         !< File unit.
  integer(I4P)             :: iostat    !< IO status.

  in_cells = .false.
  found = 0
  open(newunit=u, file=filename, action='read')
  do
    read(u, '(A)', iostat=iostat) line
    if (iostat /= 0) exit
    if (index(line, '<Cells>') > 0) in_cells = .true.
    if (index(line, '</Cells>') > 0) in_cells = .false.
    if (in_cells .and. (index(line, 'Name="faces"') > 0 .or. index(line, 'Name="faceoffsets"') > 0)) found = found + 1
  enddo
  close(u)
  is_inside = found == 2
  endfunction faces_inside_cells

  function types_are_uint8(filename) result(is_uint8)
  !< Check that the cell types DataArray is written as UInt8, as the VTK XML format specifies.
  character(*), intent(in) :: filename !< File name.
  logical                  :: is_uint8 !< Check result.
  character(len=1024)      :: line     !< Line buffer.
  integer(I4P)             :: u        !< File unit.
  integer(I4P)             :: iostat   !< IO status.

  is_uint8 = .false.
  open(newunit=u, file=filename, action='read')
  do
    read(u, '(A)', iostat=iostat) line
    if (iostat /= 0) exit
    if (index(line, 'Name="types"') > 0) is_uint8 = index(line, 'type="UInt8"') > 0
  enddo
  close(u)
  endfunction types_are_uint8
endprogram vtk_fortran_write_vtu_polyhedron
