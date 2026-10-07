!< VTK_Fortran test: write meshes with 64-bit (I8P) counts, connectivity and offsets.
program vtk_fortran_write_i8p
!< VTK_Fortran test: write meshes with 64-bit (I8P) counts, connectivity and offsets.
!<
!< The same small meshes are written with I4P and with I8P counts and ids, in every format: an unstructured grid of a
!< polyhedron and a tetrahedron (faces included), and a polydata with vertices, a polyline and a polygon. The I8P files
!< declare Int64 connectivity and offsets (as VTK writes them). The test checks the errors, the declared types and the error
!< for ids of a wrong type; the meshes are compared by VTK readers, outside this test.
use penf
use vtk_fortran, only : vtk_file

implicit none
character(*), parameter :: formats(4)=['ascii          ', 'binary         ', 'raw            ', 'binary-appended'] !< Formats.
real(R8P),    parameter :: x(12)=[0,1,1,0,0,1,1,0,2,3,2,2]                  !< X coordinates.
real(R8P),    parameter :: y(12)=[0,0,1,1,0,0,1,1,0,0,1,0]                  !< Y coordinates.
real(R8P),    parameter :: z(12)=[0,0,0,0,1,1,1,1,0,0,0,1]                  !< Z coordinates.
integer(I4P), parameter :: connect(12)=[0,1,2,3,4,5,6,7, 8,9,10,11]         !< Connectivity (points of each cell).
integer(I4P), parameter :: offset(2)=[8,12]                                 !< Cells offset.
integer(I1P), parameter :: cell_type(2)=[42_I1P,10_I1P]                     !< Cells type: polyhedron, tetrahedron.
integer(I4P), parameter :: face(31)=[6, 4,0,1,2,3, 4,4,5,6,7, 4,0,1,5,4, &
                                     4,1,2,6,5, 4,2,3,7,6, 4,3,0,4,7]       !< Faces stream of the polyhedron.
integer(I4P), parameter :: faceoffset(2)=[31,-1]                            !< End of each cell faces, -1 if not a polyhedron.
type(vtk_file)          :: a_vtk_file                                       !< A VTK file.
integer(I4P)            :: error                                            !< Status error: the largest error of all calls.
integer(I4P)            :: f                                                !< Counter.
logical                 :: test_passed(4)                                   !< List of passed tests.

error = 0
do f=1, size(formats)
  error = max(error, write_vtu(format=trim(formats(f)), suffix='I4P'))
  error = max(error, write_vtu(format=trim(formats(f)), suffix='I8P'))
  error = max(error, write_vtp(format=trim(formats(f)), suffix='I4P'))
  error = max(error, write_vtp(format=trim(formats(f)), suffix='I8P'))
enddo
test_passed(1) = error == 0
test_passed(2) = has_text('vtkfortran_write_i8p-ascii-I8P.vtu', 'type="Int64" NumberOfComponents="1" Name="connectivity"')
test_passed(3) = has_text('vtkfortran_write_i8p-ascii-I4P.vtu', 'type="Int32" NumberOfComponents="1" Name="connectivity"')
! polydata ids of a type other than I4P/I8P are refused
error = a_vtk_file%initialize(format='ascii', filename='vtkfortran_write_i8p-error.vtp', mesh_topology='PolyData')
error = a_vtk_file%xml_writer%write_piece(np=3_I8P, nverts=0_I8P, nlines=0_I8P, nstrips=0_I8P, npolys=1_I8P)
test_passed(4) = a_vtk_file%xml_writer%write_polydata_cells(polys_connectivity=[0._R8P, 1._R8P, 2._R8P], &
                                                            polys_offset=[3._R8P]) /= 0
error = a_vtk_file%xml_writer%write_piece()
error = a_vtk_file%finalize()

print "(A,L1)", new_line('a')//'Are all tests passed? ', all(test_passed)
if (.not.all(test_passed)) error stop 'some tests failed'
stop
contains
  function write_vtu(format, suffix) result(error)
  !< Write the unstructured grid with I4P or I8P counts and ids.
  character(*), intent(in) :: format !< File format.
  character(*), intent(in) :: suffix !< Kind of counts and ids: I4P or I8P.
  integer(I4P)             :: error  !< Status error: the largest error of all calls.

  error = abs(a_vtk_file%initialize(format=format, filename='vtkfortran_write_i8p-'//format//'-'//suffix//'.vtu', &
                                    mesh_topology='UnstructuredGrid'))
  if (suffix == 'I8P') then
    error = max(error, abs(a_vtk_file%xml_writer%write_piece(np=12_I8P, nc=2_I8P)))
    error = max(error, abs(a_vtk_file%xml_writer%write_geo(np=12_I8P, nc=2_I8P, x=x, y=y, z=z)))
    error = max(error, abs(a_vtk_file%xml_writer%write_connectivity(nc=2_I8P, connectivity=int(connect, I8P),      &
                                                                    offset=int(offset, I8P), cell_type=cell_type, &
                                                                    face=int(face, I8P), faceoffset=int(faceoffset, I8P))))
  else
    error = max(error, abs(a_vtk_file%xml_writer%write_piece(np=12, nc=2)))
    error = max(error, abs(a_vtk_file%xml_writer%write_geo(np=12, nc=2, x=x, y=y, z=z)))
    error = max(error, abs(a_vtk_file%xml_writer%write_connectivity(nc=2, connectivity=connect, offset=offset, &
                                                                    cell_type=cell_type, face=face, faceoffset=faceoffset)))
  endif
  error = max(error, abs(a_vtk_file%xml_writer%write_dataarray(location='cell', action='open')))
  error = max(error, abs(a_vtk_file%xml_writer%write_dataarray(data_name='cell_id', x=[1_I4P, 2_I4P])))
  error = max(error, abs(a_vtk_file%xml_writer%write_dataarray(location='cell', action='close')))
  error = max(error, abs(a_vtk_file%xml_writer%write_piece()))
  error = max(error, abs(a_vtk_file%finalize()))
  endfunction write_vtu

  function write_vtp(format, suffix) result(error)
  !< Write a polydata (2 vertices, a polyline of 3 points, a triangle) with I4P or I8P counts and ids.
  character(*), intent(in) :: format !< File format.
  character(*), intent(in) :: suffix !< Kind of counts and ids: I4P or I8P.
  integer(I4P)             :: error  !< Status error: the largest error of all calls.

  error = abs(a_vtk_file%initialize(format=format, filename='vtkfortran_write_i8p-'//format//'-'//suffix//'.vtp', &
                                    mesh_topology='PolyData'))
  if (suffix == 'I8P') then
    error = max(error, abs(a_vtk_file%xml_writer%write_piece(np=8_I8P, nverts=2_I8P, nlines=1_I8P, &
                                                             nstrips=0_I8P, npolys=1_I8P)))
    error = max(error, abs(a_vtk_file%xml_writer%write_geo(np=8_I8P, nc=4_I8P, x=x(1:8), y=y(1:8), z=z(1:8))))
    error = max(error, abs(a_vtk_file%xml_writer%write_polydata_cells(                          &
                             verts_connectivity=[0_I8P, 1_I8P], verts_offset=[1_I8P, 2_I8P],   &
                             lines_connectivity=[2_I8P, 3_I8P, 4_I8P], lines_offset=[3_I8P],   &
                             polys_connectivity=[5_I8P, 6_I8P, 7_I8P], polys_offset=[3_I8P])))
  else
    error = max(error, abs(a_vtk_file%xml_writer%write_piece(np=8, nverts=2, nlines=1, nstrips=0, npolys=1)))
    error = max(error, abs(a_vtk_file%xml_writer%write_geo(np=8, nc=4, x=x(1:8), y=y(1:8), z=z(1:8))))
    error = max(error, abs(a_vtk_file%xml_writer%write_polydata_cells(              &
                             verts_connectivity=[0, 1], verts_offset=[1, 2],       &
                             lines_connectivity=[2, 3, 4], lines_offset=[3],       &
                             polys_connectivity=[5, 6, 7], polys_offset=[3])))
  endif
  error = max(error, abs(a_vtk_file%xml_writer%write_piece()))
  error = max(error, abs(a_vtk_file%finalize()))
  endfunction write_vtp

  function has_text(filename, text) result(is_found)
  !< Check that the file contains the text in one of its lines.
  character(*), intent(in) :: filename !< File name.
  character(*), intent(in) :: text     !< Expected text.
  logical                  :: is_found !< Check result.
  character(len=1024)      :: line     !< Line buffer.
  integer(I4P)             :: u        !< File unit.
  integer(I4P)             :: iostat   !< IO status.

  is_found = .false.
  open(newunit=u, file=filename, action='read')
  do
    read(u, '(A)', iostat=iostat) line
    if (iostat /= 0) exit
    if (index(line, text) > 0) is_found = .true.
  enddo
  close(u)
  endfunction has_text
endprogram vtk_fortran_write_i8p
