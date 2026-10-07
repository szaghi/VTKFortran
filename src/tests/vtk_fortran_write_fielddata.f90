!< VTK_Fortran test: write FieldData (global metadata): scalars, arrays and strings.
program vtk_fortran_write_fielddata
!< VTK_Fortran test: write FieldData (global metadata): scalars, arrays and strings.
!<
!< A one-cell mesh with FieldData made of scalars, rank-1 arrays and strings, written in every format: the test checks the
!< tags of the ASCII file (strings as VTK writes them: `<Array type="String" ...>`, arrays with their tuples count).
use penf
use vtk_fortran, only : vtk_file

implicit none
character(*), parameter :: formats(4)=['ascii          ', 'binary         ', 'raw            ', 'binary-appended'] !< Formats.
integer(I4P)            :: error          !< Status error.
integer(I4P)            :: f              !< Counter.
logical                 :: test_passed(5) !< List of passed tests.

do f=1, size(formats)
  call write_file(format=trim(formats(f)), filename='vtkfortran_write_fielddata-'//trim(formats(f))//'.vtu')
enddo
test_passed(1) = has_line('vtkfortran_write_fielddata-ascii.vtu', &
                          '<DataArray type="Float64" NumberOfTuples="1" Name="TIME" format="ascii">')
test_passed(2) = has_line('vtkfortran_write_fielddata-ascii.vtu', &
                          '<DataArray type="Float64" NumberOfTuples="3" Name="residuals" format="ascii">')
test_passed(3) = has_line('vtkfortran_write_fielddata-ascii.vtu', &
                          '<Array type="String" NumberOfTuples="1" Name="solver" format="ascii">')
test_passed(4) = has_line('vtkfortran_write_fielddata-ascii.vtu', &
                          '<Array type="String" NumberOfTuples="2" Name="species" format="ascii">')
test_passed(5) = error == 0

print "(A,L1)", new_line('a')//'Are all tests passed? ', all(test_passed)
stop
contains
  subroutine write_file(format, filename)
  !< Write a tetrahedron with FieldData.
  character(*), intent(in) :: format     !< File format.
  character(*), intent(in) :: filename   !< Output file name.
  type(vtk_file)           :: a_vtk_file !< A VTK file.

  error = a_vtk_file%initialize(format=format, filename=filename, mesh_topology='UnstructuredGrid')
  error = a_vtk_file%xml_writer%write_fielddata(action='open')
  error = a_vtk_file%xml_writer%write_fielddata(data_name='TIME', x=0.5_R8P)
  error = a_vtk_file%xml_writer%write_fielddata(data_name='CYCLE', x=7_I8P)
  error = a_vtk_file%xml_writer%write_fielddata(data_name='residuals', x=[1.e-3_R8P, 1.e-4_R8P, 1.e-5_R8P])
  error = a_vtk_file%xml_writer%write_fielddata(data_name='counts', x=[3_I4P, -2_I4P])
  error = a_vtk_file%xml_writer%write_fielddata(data_name='solver', x='VTKFortran test v1')
  error = a_vtk_file%xml_writer%write_fielddata(data_name='species', x=['N2 ', 'O2 '])
  error = a_vtk_file%xml_writer%write_fielddata(action='close')
  error = a_vtk_file%xml_writer%write_piece(np=4, nc=1)
  error = a_vtk_file%xml_writer%write_geo(np=4, nc=1, x=[0._R8P,1._R8P,0._R8P,0._R8P], y=[0._R8P,0._R8P,1._R8P,0._R8P], &
                                          z=[0._R8P,0._R8P,0._R8P,1._R8P])
  error = a_vtk_file%xml_writer%write_connectivity(nc=1, connectivity=[0_I4P,1_I4P,2_I4P,3_I4P], offset=[4_I4P], &
                                                   cell_type=[10_I1P])
  error = a_vtk_file%xml_writer%write_piece()
  error = a_vtk_file%finalize()
  endsubroutine write_file

  function has_line(filename, expected) result(is_found)
  !< Check that the file contains a line made of the expected text (leading indentation apart).
  character(*), intent(in) :: filename !< File name.
  character(*), intent(in) :: expected !< Expected line.
  logical                  :: is_found !< Check result.
  character(len=1024)      :: line     !< Line buffer.
  integer(I4P)             :: u        !< File unit.
  integer(I4P)             :: iostat   !< IO status.

  is_found = .false.
  open(newunit=u, file=filename, action='read')
  do
    read(u, '(A)', iostat=iostat) line
    if (iostat /= 0) exit
    if (trim(adjustl(line)) == expected) is_found = .true.
  enddo
  close(u)
  endfunction has_line
endprogram vtk_fortran_write_fielddata
