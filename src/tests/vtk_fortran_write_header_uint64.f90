!< VTK_Fortran test: write binary data with UInt64 bytes count headers (header_type='UInt64').
program vtk_fortran_write_header_uint64
!< VTK_Fortran test: write binary data with UInt64 bytes count headers (header_type='UInt64').
!<
!< A tetrahedron with point data of every kind, written in the binary formats with 8-byte bytes count headers: the test checks
!< the declared header type and the error on an unknown one. The values are checked by VTK readers, outside this test.
use penf
use vtk_fortran, only : vtk_file

implicit none
character(*), parameter :: formats(3)=['binary         ', 'raw            ', 'binary-appended'] !< Binary formats.
real(R8P),    parameter :: x(4)=[0._R8P, 1._R8P, 0._R8P, 0._R8P]   !< X coordinates.
real(R8P),    parameter :: y(4)=[0._R8P, 0._R8P, 1._R8P, 0._R8P]   !< Y coordinates.
real(R8P),    parameter :: z(4)=[0._R8P, 0._R8P, 0._R8P, 1._R8P]   !< Z coordinates.
type(vtk_file)          :: a_vtk_file                              !< A VTK file.
integer(I4P)            :: error                                   !< Status error.
integer(I4P)            :: f                                       !< Counter.
logical                 :: test_passed(3)                          !< List of passed tests.

do f=1, size(formats)
  call write_file(format=trim(formats(f)), filename='vtkfortran_write_header_uint64-'//trim(formats(f))//'.vtu')
enddo
test_passed(1) = has_text('vtkfortran_write_header_uint64-binary.vtu', 'header_type="UInt64"')
test_passed(2) = error == 0
test_passed(3) = a_vtk_file%initialize(format='raw', filename='vtkfortran_write_header_uint64-error.vtu', &
                                       mesh_topology='UnstructuredGrid', header_type='UInt16') /= 0

print "(A,L1)", new_line('a')//'Are all tests passed? ', all(test_passed)
stop
contains
  subroutine write_file(format, filename)
  !< Write a tetrahedron with point data of every kind.
  character(*), intent(in) :: format   !< File format.
  character(*), intent(in) :: filename !< Output file name.

  error = a_vtk_file%initialize(format=format, filename=filename, mesh_topology='UnstructuredGrid', header_type='UInt64')
  error = a_vtk_file%xml_writer%write_piece(np=4, nc=1)
  error = a_vtk_file%xml_writer%write_geo(np=4, nc=1, x=x, y=y, z=z)
  error = a_vtk_file%xml_writer%write_connectivity(nc=1, connectivity=[0_I4P,1_I4P,2_I4P,3_I4P], offset=[4_I4P], &
                                                   cell_type=[10_I1P])
  error = a_vtk_file%xml_writer%write_dataarray(location='node', action='open')
  error = a_vtk_file%xml_writer%write_dataarray(data_name='r8', x=x+2*y+3*z)
  error = a_vtk_file%xml_writer%write_dataarray(data_name='r4', x=real(x-y, R4P))
  error = a_vtk_file%xml_writer%write_dataarray(data_name='i8', x=[10_I8P, -20_I8P, 30_I8P, -40_I8P])
  error = a_vtk_file%xml_writer%write_dataarray(data_name='i4', x=[1_I4P, -2_I4P, 3_I4P, -4_I4P])
  error = a_vtk_file%xml_writer%write_dataarray(data_name='i2', x=[5_I2P, 6_I2P, 7_I2P, 8_I2P])
  error = a_vtk_file%xml_writer%write_dataarray(data_name='i1', x=[-1_I1P, 2_I1P, -3_I1P, 4_I1P])
  error = a_vtk_file%xml_writer%write_dataarray(data_name='v', x=x, y=y, z=z)
  error = a_vtk_file%xml_writer%write_dataarray(location='node', action='close')
  error = a_vtk_file%xml_writer%write_piece()
  error = a_vtk_file%finalize()
  endsubroutine write_file

  function has_text(filename, text) result(is_found)
  !< Check that the (first lines of the) file contains the text.
  character(*), intent(in) :: filename !< File name.
  character(*), intent(in) :: text     !< Expected text.
  logical                  :: is_found !< Check result.
  character(len=1024)      :: line     !< Line buffer.
  integer(I4P)             :: u        !< File unit.
  integer(I4P)             :: iostat   !< IO status.
  integer(I4P)             :: l        !< Counter.

  is_found = .false.
  open(newunit=u, file=filename, action='read')
  do l=1, 3
    read(u, '(A)', iostat=iostat) line
    if (iostat /= 0) exit
    if (index(line, text) > 0) is_found = .true.
  enddo
  close(u)
  endfunction has_text
endprogram vtk_fortran_write_header_uint64
