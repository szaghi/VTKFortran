!run compress compress
program compress
!< Compress the binary data with zlib, and use 64-bit headers for arrays beyond 2 GiB; the reader finds both by itself.
use penf, only : I4P, R8P
use vtk_fortran, only : vtk_file
implicit none
real(R8P)                     :: v(1000)
type(vtk_file)                :: a_vtk_file
character(len=:), allocatable :: header_type, compressor
integer(I4P)                  :: i, error

v = [(sin(i/50._R8P), i=1, size(v))]
error = a_vtk_file%initialize(format='raw', filename='big.vtr', mesh_topology='RectilinearGrid', &
                              nx1=1, nx2=1000, ny1=1, ny2=1, nz1=1, nz2=1,                       &
                              compressor='zlib', header_type='UInt64')
if (error /= 0) error stop 'zlib is not available: build the library with VTKFORTRAN_USE_ZLIB'
error = a_vtk_file%xml_writer%write_piece(nx1=1, nx2=1000, ny1=1, ny2=1, nz1=1, nz2=1)
error = a_vtk_file%xml_writer%write_geo(x=[(real(i, R8P), i=1, 1000)], y=[0._R8P], z=[0._R8P])
error = a_vtk_file%xml_writer%write_dataarray(location='node', action='open')
error = a_vtk_file%xml_writer%write_dataarray(data_name='v', x=v)
error = a_vtk_file%xml_writer%write_dataarray(location='node', action='close')
error = a_vtk_file%xml_writer%write_piece()
error = a_vtk_file%finalize()

error = a_vtk_file%initialize(filename='big.vtr', action='read')
error = a_vtk_file%xml_reader%get_info(header_type=header_type, compressor=compressor)
print '(A,A,A,A)', 'big.vtr: header ', header_type, ', compressor ', compressor
error = a_vtk_file%finalize()
endprogram compress
