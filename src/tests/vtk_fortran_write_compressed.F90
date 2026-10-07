!< VTK_Fortran test: write zlib compressed binary data (compressor='zlib').
program vtk_fortran_write_compressed
!< VTK_Fortran test: write zlib compressed binary data (compressor='zlib').
!<
!< A tetrahedron with point data of every kind and field data, written in the binary formats (binary, raw, binary-appended)
!< with zlib compression and both header types. The field data arrays span several compressed blocks of 32 KiB: one fills its
!< blocks exactly (64 KiB), one ends with a partial block. The test checks the declared compressor and the errors of the
!< compressor argument; the values are checked by VTK readers, outside this test. Without VTKFORTRAN_USE_ZLIB, the test checks
!< that compressor='zlib' is refused.
use penf
use vtk_fortran, only : vtk_file

implicit none
character(*), parameter :: formats(3)=['binary         ', 'raw            ', 'binary-appended'] !< Binary formats.
character(*), parameter :: headers(2)=['UInt32', 'UInt64']           !< Header types.
real(R8P),    parameter :: x(4)=[0._R8P, 1._R8P, 0._R8P, 0._R8P]   !< X coordinates.
real(R8P),    parameter :: y(4)=[0._R8P, 0._R8P, 1._R8P, 0._R8P]   !< Y coordinates.
real(R8P),    parameter :: z(4)=[0._R8P, 0._R8P, 0._R8P, 1._R8P]   !< Z coordinates.
type(vtk_file)          :: a_vtk_file                              !< A VTK file.
integer(I4P)            :: error                                   !< Status error.
integer(I4P)            :: f                                       !< Counter.
integer(I4P)            :: h                                       !< Counter.
logical                 :: test_passed(6)                          !< List of passed tests.

#ifdef VTKFORTRAN_USE_ZLIB
test_passed(2) = .true.
do h=1, size(headers)
  do f=1, size(formats)
    call write_file(format=trim(formats(f)), header_type=headers(h), &
                    filename='vtkfortran_write_compressed-'//trim(formats(f))//'-'//headers(h)//'.vtu')
    test_passed(2) = test_passed(2) .and. error == 0
  enddo
enddo
test_passed(1) = has_text('vtkfortran_write_compressed-binary-UInt32.vtu', &
                          'compressor="vtkZLibDataCompressor" header_type="UInt32"')
! ASCII ignores the compressor
error = a_vtk_file%initialize(format='ascii', filename='vtkfortran_write_compressed-ascii.vtu', &
                              mesh_topology='UnstructuredGrid', compressor='zlib')
test_passed(3) = error == 0
error = a_vtk_file%finalize()
test_passed(4) = .not.has_text('vtkfortran_write_compressed-ascii.vtu', 'compressor=')
#else
! zlib not available: the compressor is refused
test_passed(1:3) = .true.
test_passed(4) = a_vtk_file%initialize(format='binary', filename='vtkfortran_write_compressed-error.vtu', &
                                       mesh_topology='UnstructuredGrid', compressor='zlib') /= 0
#endif
test_passed(5) = a_vtk_file%initialize(format='binary', filename='vtkfortran_write_compressed-error.vtu', &
                                       mesh_topology='UnstructuredGrid', compressor='lz4') /= 0
test_passed(6) = a_vtk_file%initialize(format='raw-zlib', filename='vtkfortran_write_compressed-error.vtu', &
                                       mesh_topology='UnstructuredGrid', compressor='none') /= 0

print "(A,L1)", new_line('a')//'Are all tests passed? ', all(test_passed)
if (.not.all(test_passed)) error stop 'some tests failed'
stop
contains
  subroutine write_file(format, header_type, filename)
  !< Write a tetrahedron with point data of every kind and field data spanning several compressed blocks.
  character(*), intent(in) :: format      !< File format.
  character(*), intent(in) :: header_type !< Header type.
  character(*), intent(in) :: filename    !< Output file name.
  integer(I4P)             :: i           !< Counter.

  error = a_vtk_file%initialize(format=format, filename=filename, mesh_topology='UnstructuredGrid', &
                                header_type=header_type, compressor='zlib')
  error = a_vtk_file%xml_writer%write_fielddata(action='open')
  error = a_vtk_file%xml_writer%write_fielddata(data_name='full_blocks', x=[(0.5_R8P*i, i=1, 8192)])
  error = a_vtk_file%xml_writer%write_fielddata(data_name='partial_block', x=[(i, i=1, 9000)])
  error = a_vtk_file%xml_writer%write_fielddata(action='close')
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
endprogram vtk_fortran_write_compressed
