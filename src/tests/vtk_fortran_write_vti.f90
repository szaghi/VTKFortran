!< VTK_Fortran test: write VTI (ImageData) and PVTI (parallel ImageData) files.
program vtk_fortran_write_vti
!< VTK_Fortran test: write VTI (ImageData) and PVTI (parallel ImageData) files.
!<
!< A regular grid of 4x3x2 points, defined by extents, origin and spacing only (no geometry), with point and cell data:
!< written in every format (the ascii one with a rotated direction), then split in two pieces collected by a `.pvti` file.
use penf
use vtk_fortran, only : pvtk_file, vtk_file

implicit none
integer(I4P), parameter :: nx1=0, nx2=3, ny1=0, ny2=2, nz1=0, nz2=1   !< Whole extents.
integer(I4P), parameter :: nx2_p(2)=[2, 3]                            !< Upper x extent of the pieces.
integer(I4P), parameter :: nx1_p(2)=[0, 2]                            !< Lower x extent of the pieces.
real(R8P),    parameter :: origin(3)=[-1._R8P, 0._R8P, 0.5_R8P]       !< Origin.
real(R8P),    parameter :: spacing(3)=[0.5_R8P, 0.25_R8P, 1._R8P]     !< Spacing.
real(R8P),    parameter :: rotated(9)=[0._R8P, -1._R8P, 0._R8P, &
                                       1._R8P,  0._R8P, 0._R8P, &
                                       0._R8P,  0._R8P, 1._R8P]       !< Direction: 90 degrees about z.
character(*), parameter :: formats(4)=['ascii          ', 'binary         ', 'raw            ', 'binary-appended'] !< Formats.
real(R8P)               :: phi(nx1:nx2,ny1:ny2,nz1:nz2)               !< Point data.
integer(I4P)            :: cid(nx1:nx2-1,ny1:ny2-1,nz1:nz2-1)         !< Cell data.
type(vtk_file)          :: a_vtk_file                                 !< A VTK file.
integer(I4P)            :: i, j, k, f, p                              !< Counters.
integer(I4P)            :: error                                      !< Status error.
logical                 :: test_passed(4)                             !< List of passed tests.

do k=nz1, nz2 ; do j=ny1, ny2 ; do i=nx1, nx2
  phi(i,j,k) = real(i + 10*j + 100*k, R8P)
enddo ; enddo ; enddo
do k=nz1, nz2-1 ; do j=ny1, ny2-1 ; do i=nx1, nx2-1
  cid(i,j,k) = i + 10*j
enddo ; enddo ; enddo

do f=1, size(formats)
  if (f == 1) then
    call write_vti(format=trim(formats(f)), filename='vtkfortran_write_vti-'//trim(formats(f))//'.vti', &
                   x1=nx1, x2=nx2, direction=rotated)
  else
    call write_vti(format=trim(formats(f)), filename='vtkfortran_write_vti-'//trim(formats(f))//'.vti', x1=nx1, x2=nx2)
  endif
enddo
test_passed(1) = has_text('vtkfortran_write_vti-ascii.vti', 'Origin="-1.0 0.0 0.5" Spacing="0.5 0.25 1.0" '// &
                          'Direction="0.0 -1.0 0.0 1.0 0.0 0.0 0.0 0.0 1.0"')
test_passed(2) = error == 0

! parallel: two pieces sharing the boundary x index, collected by a pvti file
do p=1, 2
  call write_vti(format='raw', filename='vtkfortran_write_vti_0'//trim(str(p, .true.))//'.vti', x1=nx1_p(p), x2=nx2_p(p))
enddo
call write_pvti(filename='vtkfortran_write_vti.pvti')
test_passed(3) = has_text('vtkfortran_write_vti.pvti', '<PImageData ') .and. error == 0

! error: ImageData requires origin and spacing
test_passed(4) = a_vtk_file%initialize(format='ascii', filename='vtkfortran_write_vti-error.vti', mesh_topology='ImageData', &
                                       nx1=nx1, nx2=nx2, ny1=ny1, ny2=ny2, nz1=nz1, nz2=nz2, origin=origin) /= 0

print "(A,L1)", new_line('a')//'Are all tests passed? ', all(test_passed)
if (.not.all(test_passed)) error stop 'some tests failed'
stop
contains
  subroutine write_vti(format, filename, x1, x2, direction)
  !< Write the part of the grid between x indexes x1 and x2.
  character(*), intent(in)           :: format       !< File format.
  character(*), intent(in)           :: filename     !< Output file name.
  integer(I4P), intent(in)           :: x1, x2       !< X extents of the part.
  real(R8P),    intent(in), optional :: direction(9) !< Axes directions.

  error = a_vtk_file%initialize(format=format, filename=filename, mesh_topology='ImageData', &
                                nx1=x1, nx2=x2, ny1=ny1, ny2=ny2, nz1=nz1, nz2=nz2,         &
                                origin=origin, spacing=spacing, direction=direction)
  error = a_vtk_file%xml_writer%write_piece(nx1=x1, nx2=x2, ny1=ny1, ny2=ny2, nz1=nz1, nz2=nz2)
  error = a_vtk_file%xml_writer%write_dataarray(location='node', action='open', scalars='phi')
  error = a_vtk_file%xml_writer%write_dataarray(data_name='phi', x=phi(x1:x2,:,:), one_component=.true.)
  error = a_vtk_file%xml_writer%write_dataarray(location='node', action='close')
  error = a_vtk_file%xml_writer%write_dataarray(location='cell', action='open')
  error = a_vtk_file%xml_writer%write_dataarray(data_name='cid', x=cid(x1:x2-1,:,:), one_component=.true.)
  error = a_vtk_file%xml_writer%write_dataarray(location='cell', action='close')
  error = a_vtk_file%xml_writer%write_piece()
  error = a_vtk_file%finalize()
  endsubroutine write_vti

  subroutine write_pvti(filename)
  !< Write the parallel file collecting the two pieces.
  character(*), intent(in) :: filename    !< Output file name.
  type(pvtk_file)          :: a_pvtk_file !< A parallel (partitioned) VTK file.

  error = a_pvtk_file%initialize(filename=filename, mesh_topology='PImageData', &
                                 nx1=nx1, nx2=nx2, ny1=ny1, ny2=ny2, nz1=nz1, nz2=nz2, origin=origin, spacing=spacing)
  error = a_pvtk_file%xml_writer%write_dataarray(location='node', action='open', scalars='phi')
  error = a_pvtk_file%xml_writer%write_parallel_dataarray(data_name='phi', data_type='Float64', number_of_components=1)
  error = a_pvtk_file%xml_writer%write_dataarray(location='node', action='close')
  error = a_pvtk_file%xml_writer%write_dataarray(location='cell', action='open')
  error = a_pvtk_file%xml_writer%write_parallel_dataarray(data_name='cid', data_type='Int32', number_of_components=1)
  error = a_pvtk_file%xml_writer%write_dataarray(location='cell', action='close')
  do p=1, 2
    error = a_pvtk_file%xml_writer%write_parallel_geo(source='vtkfortran_write_vti_0'//trim(str(p, .true.))//'.vti', &
                                                      nx1=nx1_p(p), nx2=nx2_p(p), ny1=ny1, ny2=ny2, nz1=nz1, nz2=nz2)
  enddo
  error = a_pvtk_file%finalize()
  endsubroutine write_pvti

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
endprogram vtk_fortran_write_vti
