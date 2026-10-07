!< VTK_Fortran test: write VTP (PolyData) and PVTP (parallel PolyData) files.
program vtk_fortran_write_vtp
!< VTK_Fortran test: write VTP (PolyData) and PVTP (parallel PolyData) files.
!<
!< Seven points and the four cell blocks of polydata: a vertex, a polyline, a triangle strip and two triangles, with point
!< and cell data (cell data ordered by block: verts, lines, strips, polys). Written in every format, then split in two pieces
!< collected by a `.pvtp` file.
use penf
use vtk_fortran, only : pvtk_file, vtk_file

implicit none
integer(I4P), parameter :: np=7                                   !< Number of points.
real(R8P),    parameter :: x(np)=[0,1,1,0,2,3,3]                  !< X coordinates.
real(R8P),    parameter :: y(np)=[0,0,1,1,0,0,1]                  !< Y coordinates.
real(R8P),    parameter :: z(np)=[0,0,0,0,0,0,0]                  !< Z coordinates.
character(*), parameter :: formats(4)=['ascii          ', 'binary         ', 'raw            ', 'binary-appended'] !< Formats.
type(vtk_file)          :: a_vtk_file                             !< A VTK file.
integer(I4P)            :: error                                  !< Status error.
integer(I4P)            :: f                                      !< Counter.
logical                 :: test_passed(4)                         !< List of passed tests.

do f=1, size(formats)
  call write_vtp(format=trim(formats(f)), filename='vtkfortran_write_vtp-'//trim(formats(f))//'.vtp')
enddo
test_passed(1) = has_text('vtkfortran_write_vtp-ascii.vtp', &
                 '<Piece NumberOfPoints="7" NumberOfVerts="1" NumberOfLines="1" NumberOfStrips="1" NumberOfPolys="2">')
test_passed(2) = error == 0

! parallel: the surface (strip and triangles) and the curve (vertex and polyline) as two pieces
call write_surface(filename='vtkfortran_write_vtp_01.vtp')
call write_curve(filename='vtkfortran_write_vtp_02.vtp')
call write_pvtp(filename='vtkfortran_write_vtp.pvtp')
test_passed(3) = has_text('vtkfortran_write_vtp.pvtp', '<PPolyData GhostLevel="0">') .and. error == 0

! error: a block needs both its connectivity and its offsets
error = a_vtk_file%initialize(format='ascii', filename='vtkfortran_write_vtp-error.vtp', mesh_topology='PolyData')
error = a_vtk_file%xml_writer%write_piece(np=np, nverts=0, nlines=1, nstrips=0, npolys=0)
test_passed(4) = a_vtk_file%xml_writer%write_polydata_cells(lines_connectivity=[4_I4P,5_I4P,6_I4P]) /= 0
error = a_vtk_file%finalize()

print "(A,L1)", new_line('a')//'Are all tests passed? ', all(test_passed)
if (.not.all(test_passed)) error stop 'some tests failed'
stop
contains
  subroutine write_vtp(format, filename)
  !< Write all the cell blocks in one file.
  character(*), intent(in) :: format   !< File format.
  character(*), intent(in) :: filename !< Output file name.

  error = a_vtk_file%initialize(format=format, filename=filename, mesh_topology='PolyData')
  error = a_vtk_file%xml_writer%write_piece(np=np, nverts=1, nlines=1, nstrips=1, npolys=2)
  error = a_vtk_file%xml_writer%write_geo(np=np, nc=5, x=x, y=y, z=z)
  error = a_vtk_file%xml_writer%write_polydata_cells(verts_connectivity=[4_I4P], verts_offset=[1_I4P],                     &
                                                     lines_connectivity=[4_I4P,5_I4P,6_I4P], lines_offset=[3_I4P],         &
                                                     strips_connectivity=[0_I4P,1_I4P,3_I4P,2_I4P], strips_offset=[4_I4P], &
                                                     polys_connectivity=[0_I4P,1_I4P,2_I4P, 0_I4P,2_I4P,3_I4P],            &
                                                     polys_offset=[3_I4P,6_I4P])
  error = a_vtk_file%xml_writer%write_dataarray(location='node', action='open')
  error = a_vtk_file%xml_writer%write_dataarray(data_name='h', x=x+y)
  error = a_vtk_file%xml_writer%write_dataarray(location='node', action='close')
  error = a_vtk_file%xml_writer%write_dataarray(location='cell', action='open')
  error = a_vtk_file%xml_writer%write_dataarray(data_name='block', x=[1_I4P,2_I4P,3_I4P,4_I4P,4_I4P]) ! verts, lines, strips, polys
  error = a_vtk_file%xml_writer%write_dataarray(location='cell', action='close')
  error = a_vtk_file%xml_writer%write_piece()
  error = a_vtk_file%finalize()
  endsubroutine write_vtp

  subroutine write_surface(filename)
  !< Write the surface piece: the 4 points of the square, a triangle strip and two triangles.
  character(*), intent(in) :: filename !< Output file name.

  error = a_vtk_file%initialize(format='raw', filename=filename, mesh_topology='PolyData')
  error = a_vtk_file%xml_writer%write_piece(np=4, nverts=0, nlines=0, nstrips=1, npolys=2)
  error = a_vtk_file%xml_writer%write_geo(np=4, nc=3, x=x(1:4), y=y(1:4), z=z(1:4))
  error = a_vtk_file%xml_writer%write_polydata_cells(strips_connectivity=[0_I4P,1_I4P,3_I4P,2_I4P], strips_offset=[4_I4P], &
                                                     polys_connectivity=[0_I4P,1_I4P,2_I4P, 0_I4P,2_I4P,3_I4P],            &
                                                     polys_offset=[3_I4P,6_I4P])
  error = a_vtk_file%xml_writer%write_dataarray(location='node', action='open')
  error = a_vtk_file%xml_writer%write_dataarray(data_name='h', x=x(1:4)+y(1:4))
  error = a_vtk_file%xml_writer%write_dataarray(location='node', action='close')
  error = a_vtk_file%xml_writer%write_dataarray(location='cell', action='open')
  error = a_vtk_file%xml_writer%write_dataarray(data_name='block', x=[3_I4P,4_I4P,4_I4P])
  error = a_vtk_file%xml_writer%write_dataarray(location='cell', action='close')
  error = a_vtk_file%xml_writer%write_piece()
  error = a_vtk_file%finalize()
  endsubroutine write_surface

  subroutine write_curve(filename)
  !< Write the curve piece: the 3 points of the polyline (local ids), a vertex and the polyline.
  character(*), intent(in) :: filename !< Output file name.

  error = a_vtk_file%initialize(format='raw', filename=filename, mesh_topology='PolyData')
  error = a_vtk_file%xml_writer%write_piece(np=3, nverts=1, nlines=1, nstrips=0, npolys=0)
  error = a_vtk_file%xml_writer%write_geo(np=3, nc=2, x=x(5:7), y=y(5:7), z=z(5:7))
  error = a_vtk_file%xml_writer%write_polydata_cells(verts_connectivity=[0_I4P], verts_offset=[1_I4P], &
                                                     lines_connectivity=[0_I4P,1_I4P,2_I4P], lines_offset=[3_I4P])
  error = a_vtk_file%xml_writer%write_dataarray(location='node', action='open')
  error = a_vtk_file%xml_writer%write_dataarray(data_name='h', x=x(5:7)+y(5:7))
  error = a_vtk_file%xml_writer%write_dataarray(location='node', action='close')
  error = a_vtk_file%xml_writer%write_dataarray(location='cell', action='open')
  error = a_vtk_file%xml_writer%write_dataarray(data_name='block', x=[1_I4P,2_I4P])
  error = a_vtk_file%xml_writer%write_dataarray(location='cell', action='close')
  error = a_vtk_file%xml_writer%write_piece()
  error = a_vtk_file%finalize()
  endsubroutine write_curve

  subroutine write_pvtp(filename)
  !< Write the parallel file collecting the two pieces.
  character(*), intent(in) :: filename    !< Output file name.
  type(pvtk_file)          :: a_pvtk_file !< A parallel (partitioned) VTK file.

  error = a_pvtk_file%initialize(filename=filename, mesh_topology='PPolyData', mesh_kind='Float64')
  error = a_pvtk_file%xml_writer%write_dataarray(location='node', action='open')
  error = a_pvtk_file%xml_writer%write_parallel_dataarray(data_name='h', data_type='Float64', number_of_components=1)
  error = a_pvtk_file%xml_writer%write_dataarray(location='node', action='close')
  error = a_pvtk_file%xml_writer%write_dataarray(location='cell', action='open')
  error = a_pvtk_file%xml_writer%write_parallel_dataarray(data_name='block', data_type='Int32', number_of_components=1)
  error = a_pvtk_file%xml_writer%write_dataarray(location='cell', action='close')
  error = a_pvtk_file%xml_writer%write_parallel_geo(source='vtkfortran_write_vtp_01.vtp')
  error = a_pvtk_file%xml_writer%write_parallel_geo(source='vtkfortran_write_vtp_02.vtp')
  error = a_pvtk_file%finalize()
  endsubroutine write_pvtp

  function has_text(filename, text) result(is_found)
  !< Check that the file contains the text.
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
endprogram vtk_fortran_write_vtp
