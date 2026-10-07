!< VTK_Fortran test: write unsigned integer data arrays (write_dataarray_unsigned).
program vtk_fortran_write_unsigned
!< VTK_Fortran test: write unsigned integer data arrays (write_dataarray_unsigned).
!<
!< A tetrahedron with unsigned point data of every width and a `vtkGhostType` (UInt8) cell array, in every format. Fortran
!< has no unsigned integers: the arrays hold the bits of the unsigned values, e.g. 200 is stored as -56_I1P (200-256). The
!< test checks the declared types and the ASCII values, and the error for a UInt64 value beyond 2^63-1 in ASCII; the values
!< of the binary formats are checked by VTK readers, outside this test.
use penf
use vtk_fortran, only : vtk_file

implicit none
character(*), parameter :: formats(4)=['ascii          ', 'binary         ', 'raw            ', 'binary-appended'] !< Formats.
real(R8P),    parameter :: x(4)=[0._R8P, 1._R8P, 0._R8P, 0._R8P]           !< X coordinates.
real(R8P),    parameter :: y(4)=[0._R8P, 0._R8P, 1._R8P, 0._R8P]           !< Y coordinates.
real(R8P),    parameter :: z(4)=[0._R8P, 0._R8P, 0._R8P, 1._R8P]           !< Z coordinates.
integer(I1P), parameter :: u8(4)=[0_I1P, 1_I1P, -56_I1P, -1_I1P]           !< UInt8:  0, 1, 200, 255.
integer(I2P), parameter :: u16(4)=[0_I2P, 1_I2P, -25536_I2P, -1_I2P]       !< UInt16: 0, 1, 40000, 65535.
integer(I4P), parameter :: u32(4)=[0_I4P, 1_I4P, -1294967296_I4P, -1_I4P]  !< UInt32: 0, 1, 3000000000, 4294967295.
integer(I8P), parameter :: u64(4)=[0_I8P, 1_I8P, 4611686018427387904_I8P, &
                                   9223372036854775807_I8P]                !< UInt64: 0, 1, 2^62, 2^63-1.
type(vtk_file)          :: a_vtk_file                                      !< A VTK file.
integer(I4P)            :: error                                           !< Status error: the largest error of all calls.
integer(I4P)            :: f                                               !< Counter.
logical                 :: test_passed(4)                                  !< List of passed tests.

error = 0
do f=1, size(formats)
  error = max(error, write_file(format=trim(formats(f)), filename='vtkfortran_write_unsigned-'//trim(formats(f))//'.vtu'))
enddo
test_passed(1) = error == 0
test_passed(2) = has_text('vtkfortran_write_unsigned-ascii.vtu', 'type="UInt8"')
test_passed(3) = has_values('vtkfortran_write_unsigned-ascii.vtu', 'Name="u32"', &
                            [0_I8P, 1_I8P, 3000000000_I8P, 4294967295_I8P])
! a UInt64 value of 2^63 or more cannot be printed in ASCII
error = a_vtk_file%initialize(format='ascii', filename='vtkfortran_write_unsigned-error.vtu', mesh_topology='UnstructuredGrid')
error = a_vtk_file%xml_writer%write_piece(np=4, nc=1)
error = a_vtk_file%xml_writer%write_dataarray(location='node', action='open')
test_passed(4) = a_vtk_file%xml_writer%write_dataarray_unsigned(data_name='u64', x=[0_I8P, -1_I8P, 0_I8P, 0_I8P]) /= 0
error = a_vtk_file%xml_writer%write_dataarray(location='node', action='close')
error = a_vtk_file%xml_writer%write_piece()
error = a_vtk_file%finalize()

print "(A,L1)", new_line('a')//'Are all tests passed? ', all(test_passed)
if (.not.all(test_passed)) error stop 'some tests failed'
stop
contains
  function write_file(format, filename) result(error)
  !< Write a tetrahedron with unsigned point data of every width and a vtkGhostType cell array.
  character(*), intent(in) :: format   !< File format.
  character(*), intent(in) :: filename !< Output file name.
  integer(I4P)             :: error    !< Status error: the largest error of all calls.

  error = abs(a_vtk_file%initialize(format=format, filename=filename, mesh_topology='UnstructuredGrid'))
  error = max(error, abs(a_vtk_file%xml_writer%write_piece(np=4, nc=1)))
  error = max(error, abs(a_vtk_file%xml_writer%write_geo(np=4, nc=1, x=x, y=y, z=z)))
  error = max(error, abs(a_vtk_file%xml_writer%write_connectivity(nc=1, connectivity=[0_I4P, 1_I4P, 2_I4P, 3_I4P], &
                                                                  offset=[4_I4P], cell_type=[10_I1P])))
  error = max(error, abs(a_vtk_file%xml_writer%write_dataarray(location='node', action='open')))
  error = max(error, abs(a_vtk_file%xml_writer%write_dataarray_unsigned(data_name='u8', x=u8)))
  error = max(error, abs(a_vtk_file%xml_writer%write_dataarray_unsigned(data_name='u16', x=u16)))
  error = max(error, abs(a_vtk_file%xml_writer%write_dataarray_unsigned(data_name='u32', x=u32)))
  error = max(error, abs(a_vtk_file%xml_writer%write_dataarray_unsigned(data_name='u64', x=u64)))
  error = max(error, abs(a_vtk_file%xml_writer%write_dataarray(data_name='i8', x=u8))) ! the same bits, signed
  error = max(error, abs(a_vtk_file%xml_writer%write_dataarray(location='node', action='close')))
  error = max(error, abs(a_vtk_file%xml_writer%write_dataarray(location='cell', action='open')))
  error = max(error, abs(a_vtk_file%xml_writer%write_dataarray_unsigned(data_name='vtkGhostType', x=[0_I1P])))
  error = max(error, abs(a_vtk_file%xml_writer%write_dataarray(location='cell', action='close')))
  error = max(error, abs(a_vtk_file%xml_writer%write_piece()))
  error = max(error, abs(a_vtk_file%finalize()))
  endfunction write_file

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

  function has_values(filename, tag, values) result(is_found)
  !< Check that the (ASCII) DataArray of the tag containing `tag` holds `values` (read after the tag).
  character(*), intent(in) :: filename                  !< File name.
  character(*), intent(in) :: tag                       !< Text identifying the DataArray tag.
  integer(I8P), intent(in) :: values(:)                 !< Expected values.
  logical                  :: is_found                  !< Check result.
  character(len=1024)      :: line                      !< Line buffer.
  integer(I8P)             :: read_values(size(values)) !< Values read.
  integer(I4P)             :: u                         !< File unit.
  integer(I4P)             :: iostat                    !< IO status.

  is_found = .false.
  open(newunit=u, file=filename, action='read')
  do
    read(u, '(A)', iostat=iostat) line
    if (iostat /= 0) exit
    if (index(line, tag) > 0) then
      read(u, *, iostat=iostat) read_values
      is_found = iostat == 0 .and. all(read_values == values)
      exit
    endif
  enddo
  close(u)
  endfunction has_values
endprogram vtk_fortran_write_unsigned
