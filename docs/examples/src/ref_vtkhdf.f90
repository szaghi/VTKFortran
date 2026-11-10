!hdf5
!run ref_vtkhdf ref_vtkhdf
!render ref_vtkhdf wave.vtkhdf array=wave glyph=velocity size=560x340 zoom=1.05
program ref_vtkhdf
!< The example of the VTKHDF section of the reference page "Formats and large data": an ImageData in the VTKHDF format,
!< then its layout, read back with the HDF5 bindings of the library (VTKFortran does not read VTKHDF files yet).
use penf, only : I4P, I8P, R8P
use vtk_fortran, only : vtk_file
use vtk_fortran_hdf5
implicit none
integer(I4P), parameter       :: nx=40, ny=30, nz=10
real(R8P)                     :: wave(0:nx,0:ny,0:nz), velocity(3,0:nx,0:ny,0:nz)
type(vtk_file)                :: a_vtk_file
integer(hid_kind)             :: file_id, root
integer(I8P), allocatable     :: ints(:)
real(R8P),    allocatable     :: reals(:)
character(len=:), allocatable :: text
integer(I4P)                  :: i, j, k, error

do k=0, nz ; do j=0, ny ; do i=0, nx
  wave(i,j,k) = sin(0.3_R8P*i)*cos(0.4_R8P*j) + 0.1_R8P*k
  velocity(:,i,j,k) = [-cos(0.4_R8P*j), sin(0.3_R8P*i), 0._R8P]
enddo ; enddo ; enddo
!region write
error = a_vtk_file%initialize(format='vtkhdf', filename='wave.vtkhdf', mesh_topology='ImageData',     &
                              nx1=0, nx2=nx, ny1=0, ny2=ny, nz1=0, nz2=nz, compressor='zlib',      &
                              origin=[0._R8P, 0._R8P, 0._R8P], spacing=[0.1_R8P, 0.1_R8P, 0.1_R8P])
error = a_vtk_file%xml_writer%write_fielddata(action='open')
error = a_vtk_file%xml_writer%write_fielddata(data_name='time', x=0.5_R8P)
error = a_vtk_file%xml_writer%write_fielddata(action='close')
error = a_vtk_file%xml_writer%write_piece(nx1=0, nx2=nx, ny1=0, ny2=ny, nz1=0, nz2=nz)
error = a_vtk_file%xml_writer%write_dataarray(location='node', action='open', scalars='wave', vectors='velocity')
error = a_vtk_file%xml_writer%write_dataarray(data_name='wave', x=wave, one_component=.true.)
error = a_vtk_file%xml_writer%write_dataarray(data_name='velocity', x=velocity)
error = a_vtk_file%xml_writer%write_dataarray(location='node', action='close')
error = a_vtk_file%xml_writer%write_piece()
error = a_vtk_file%finalize()
!endregion write
if (error /= 0) error stop 'write error'
print '(A)', 'wave.vtkhdf written; its layout, shapes in C order (as h5dump and h5py show them):'

error = h5_file_open('wave.vtkhdf', file_id)
if (error == 0) error = h5_group_open(file_id, 'VTKHDF', root)
if (error == 0) error = h5_attribute_read(root, 'Version', ints)
if (error /= 0) error stop 'read error'
print '(A,I0,A,I0)', '/VTKHDF                    Version     ', ints(1), '.', ints(2)
error = h5_attribute_read(root, 'Type', text)
print '(A)', '                           Type        '//text
error = h5_attribute_read(root, 'WholeExtent', ints)
print '(A,6(I0,:,1X))', '                           WholeExtent ', ints
error = h5_attribute_read(root, 'Spacing', reals)
print '(A,3(F3.1,:,1X))', '                           Spacing     ', reals
call show('PointData/wave')
call show('PointData/velocity')
call show('FieldData/time')
! the values read back are the values written, in the same (Fortran) order
error = h5_dataset_read(root, 'PointData/velocity', reals)
if (error /= 0 .or. any(reals /= reshape(velocity, [size(velocity)]))) error stop 'velocity read back differs'
error = h5_dataset_read(root, 'PointData/wave', reals)
if (error /= 0 .or. any(reals /= reshape(wave, [size(wave)]))) error stop 'wave read back differs'
error = h5_group_close(root)
error = h5_file_close(file_id)
contains
  subroutine show(path)
  !< Print a dataset of /VTKHDF: its shape in C order, its active role or its value.
  character(*), intent(in)      :: path
  integer(I8P), allocatable     :: dims(:)
  integer(hid_kind)             :: dset
  character(len=:), allocatable :: role

  error = h5_dataset_dims(root, path, dims)
  if (error /= 0) error stop 'read error'
  role = ''
  error = h5_dataset_open(root, path, dset)
  if (h5_attribute_exists(dset, 'Attribute')) error = h5_attribute_read(dset, 'Attribute', role)
  error = h5_dataset_close(dset)
  if (size(dims) == 1 .and. dims(1) == 1) then
    error = h5_dataset_read(root, path, reals)
    role = 'value '//trim(adjustl(str_value(reals(1))))
  else
    role = 'Attribute '//role
  endif
  write(*, '(A,T28,A,*(I0,:,","))', advance='no') '/VTKHDF/'//path, 'shape [', dims(size(dims):1:-1)
  print '(A)', ']  '//role
  endsubroutine show

  function str_value(x) result(s)
  !< A real as a short string.
  real(R8P), intent(in) :: x
  character(16)         :: s

  write(s, '(F6.2)') x
  endfunction str_value
endprogram ref_vtkhdf
