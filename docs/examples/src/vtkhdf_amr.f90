!hdf5
!run vtkhdf_amr vtkhdf_amr
program vtkhdf_amr
!< A block-structured AMR in one VTKHDF file (an OverlappingAMR): level 0 covers the domain with one box of 16x8 cells,
!< level 1 refines two boxes (half the spacing) around a hot spot, each box with its temperature at the points. The domain
!< is one cell thick (VTK reads every AMR box in 3D). Then the file is read back with the reader of the library: the
!< levels, the boxes of each level and the hottest point of each box.
use penf, only : I4P, R8P
use vtk_fortran, only : vtk_file
implicit none
type(vtk_file)         :: a_vtk_file, reader
real(R8P), allocatable :: temperature(:)
real(R8P)              :: spacing(3)
integer(I4P)           :: n_levels, n_boxes, level, b, nx1, nx2, ny1, ny2, error

!region write
error = a_vtk_file%initialize(format='vtkhdf', filename='amr.vtkhdf', mesh_topology='OverlappingAMR', &
                              origin=[0._R8P, 0._R8P, 0._R8P])
! level 0: the whole domain, spacing 1
error = a_vtk_file%xml_writer%write_level(level=0, spacing=[1._R8P, 1._R8P, 1._R8P])
error = box(nx1=0, nx2=16, ny1=0, ny2=8, h=1._R8P)
! level 1: spacing 0.5, two boxes around the hot spot (their point indices are those of level 1)
error = a_vtk_file%xml_writer%write_level(level=1, spacing=[0.5_R8P, 0.5_R8P, 0.5_R8P])
error = box(nx1=8, nx2=16, ny1=4, ny2=12, h=0.5_R8P)
error = box(nx1=16, nx2=24, ny1=4, ny2=12, h=0.5_R8P)
error = a_vtk_file%finalize()
!endregion write
if (error /= 0) error stop 'write error'

!region read
error = reader%initialize(filename='amr.vtkhdf', action='read')
error = reader%xml_reader%get_levels(n_levels)
do level=0, n_levels - 1
  error = reader%xml_reader%set_level(level)
  error = reader%xml_reader%get_info(npieces=n_boxes, spacing=spacing)
  print '(A,I0,A,F4.2,A,I0)', 'level ', level, ': spacing ', spacing(1), ', boxes ', n_boxes
  do b=1, n_boxes
    error = reader%xml_reader%read_piece(piece=b, nx1=nx1, nx2=nx2, ny1=ny1, ny2=ny2)
    error = reader%xml_reader%read_dataarray(location='node', data_name='temperature', x=temperature, piece=b)
    print '(A,I0,A,4(1X,I2),A,F6.3)', '  box ', b, ': points', nx1, nx2, ny1, ny2, ', hottest', maxval(temperature)
  enddo
enddo
error = reader%finalize()
!endregion read
if (error /= 0) error stop 'read error'
contains
  function box(nx1, nx2, ny1, ny2, h) result(error)
  !< A box of the open level, one cell thick (z from 0 to h), and its temperature at the points.
  integer(I4P), intent(in) :: nx1, nx2, ny1, ny2 !< Point extent in x, y.
  real(R8P),    intent(in) :: h                  !< Spacing of the level.
  integer(I4P)             :: error              !< Error status.
  real(R8P)                :: t(nx1:nx2,ny1:ny2,0:1) !< Temperature, x fastest.
  integer(I4P)             :: i, j               !< Counters.

  do j=ny1, ny2 ; do i=nx1, nx2 ! a hot spot at (10, 4)
    t(i,j,:) = exp(-((i * h - 10)**2 + (j * h - 4)**2) / 4)
  enddo ; enddo
  error = a_vtk_file%xml_writer%write_piece(nx1=nx1, nx2=nx2, ny1=ny1, ny2=ny2, nz1=0, nz2=1)
  error = a_vtk_file%xml_writer%write_dataarray(location='node', action='open')
  error = a_vtk_file%xml_writer%write_dataarray(data_name='temperature', x=reshape(t, [size(t)]))
  error = a_vtk_file%xml_writer%write_dataarray(location='node', action='close')
  error = a_vtk_file%xml_writer%write_piece()
  endfunction box
endprogram vtkhdf_amr
