!hdf5
!run vtkhdf_grid vtkhdf_grid
program vtkhdf_grid
!< Write a StructuredGrid (a thick quarter of an annulus) into a VTKHDF file, then print its layout, read back with the HDF5
!< bindings of the library (VTKFortran does not read VTKHDF yet).
use penf, only : I4P, I8P, R8P
use vtk_fortran, only : vtk_file
use vtk_fortran_hdf5
implicit none
integer(I4P), parameter       :: ni=12, nj=6, nk=2
real(R8P),    parameter       :: pi = acos(-1._R8P)
type(vtk_file)                :: a_vtk_file
real(R8P)                     :: xyz(3,0:ni,0:nj,0:nk), radius(0:ni,0:nj,0:nk), r, t
integer(I4P)                  :: i, j, k, error
integer(hid_kind)             :: file_id, root
integer(I8P), allocatable     :: ints(:), dims(:)
character(len=:), allocatable :: text

do k=0, nk ; do j=0, nj ; do i=0, ni
  r = 1._R8P + 0.1_R8P * j ; t = 0.5_R8P * pi * i / ni
  xyz(:,i,j,k) = [r * cos(t), r * sin(t), 0.1_R8P * k]
  radius(i,j,k) = r
enddo ; enddo ; enddo
!region write
error = a_vtk_file%initialize(format='vtkhdf', filename='annulus.vtkhdf', mesh_topology='StructuredGrid', &
                              nx1=0, nx2=ni, ny1=0, ny2=nj, nz1=0, nz2=nk)
error = a_vtk_file%xml_writer%write_piece(nx1=0, nx2=ni, ny1=0, ny2=nj, nz1=0, nz2=nk)
error = a_vtk_file%xml_writer%write_geo(xyz=xyz)
error = a_vtk_file%xml_writer%write_dataarray(location='node', action='open')
error = a_vtk_file%xml_writer%write_dataarray(data_name='radius', x=radius, one_component=.true.)
error = a_vtk_file%xml_writer%write_dataarray(location='node', action='close')
error = a_vtk_file%xml_writer%write_piece()
error = a_vtk_file%finalize()
!endregion write
if (error /= 0) error stop 'write error'

error = h5_file_open('annulus.vtkhdf', file_id)
if (error == 0) error = h5_group_open(file_id, 'VTKHDF', root)
if (error /= 0) error stop 'read error'
print '(A)', 'annulus.vtkhdf, shapes in C order (as h5dump and h5py show them):'
error = h5_attribute_read(root, 'Type', text)
print '(A)', '/VTKHDF                  Type       '//text
error = h5_attribute_read(root, 'Version', ints)
print '(A,I0,A,I0)', '                         Version    ', ints(1), '.', ints(2)
error = h5_attribute_read(root, 'Dimensions', ints)
print '(A,3(I0,:,1X))', '                         Dimensions ', ints
error = h5_dataset_dims(root, 'Points', dims)
write(*, '(A,*(I0,:,","))', advance='no') '/VTKHDF/Points           shape [', dims(size(dims):1:-1)
print '(A)', ']'
error = h5_dataset_dims(root, 'PointData/radius', dims)
write(*, '(A,*(I0,:,","))', advance='no') '/VTKHDF/PointData/radius shape [', dims(size(dims):1:-1)
print '(A)', ']'
error = h5_group_close(root)
error = h5_file_close(file_id)
endprogram vtkhdf_grid
