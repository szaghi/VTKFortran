program heat
!< Tutorial, chapter 2: the same temperature in every format, and the size of each file.
use penf, only : I4P, I8P, R8P
use vtk_fortran, only : vtk_file
implicit none
integer(I4P), parameter :: n=24
character(*), parameter :: formats(5)=['ascii          ', 'binary         ', 'raw            ', 'raw            ', &
                                       'binary-appended']
character(*), parameter :: compressors(5)=['none', 'none', 'none', 'zlib', 'zlib']
real(R8P)               :: x(n), t(n,n,n)
integer(I8P)            :: bytes
integer(I4P)            :: i, j, k, f, error
character(len=:), allocatable :: filename

x = [(real(i - 1, R8P)/(n - 1), i=1, n)]
do k=1, n ; do j=1, n ; do i=1, n
  t(i,j,k) = blob(x(i), x(j), x(k), [0.35_R8P, 0.4_R8P, 0.5_R8P]) + 0.6_R8P*blob(x(i), x(j), x(k), [0.7_R8P, 0.65_R8P, 0.45_R8P])
enddo ; enddo ; enddo
t(1,:,:) = 0 ; t(n,:,:) = 0 ; t(:,1,:) = 0 ; t(:,n,:) = 0 ; t(:,:,1) = 0 ; t(:,:,n) = 0

print '(A,T32,A,T49,A,T63,A)', 'file', 'format', 'compressor', 'bytes'
do f=1, size(formats)
  filename = 'heat-'//trim(formats(f))//'-'//compressors(f)//'.vtr'
  error = write_file(filename=filename, format=trim(formats(f)), compressor=compressors(f))
  inquire(file=filename, size=bytes)
  print '(A,T32,A,T49,A,T58,I10)', filename, formats(f), compressors(f), bytes
enddo
contains
  function write_file(filename, format, compressor) result(error)
  !< Write the temperature in the given format, with the given compressor.
  character(*), intent(in) :: filename, format, compressor
  integer(I4P)             :: error
  type(vtk_file)           :: a_vtk_file

  error = a_vtk_file%initialize(format=format, filename=filename, mesh_topology='RectilinearGrid', &
                                nx1=1, nx2=n, ny1=1, ny2=n, nz1=1, nz2=n, compressor=compressor)
  error = a_vtk_file%xml_writer%write_piece(nx1=1, nx2=n, ny1=1, ny2=n, nz1=1, nz2=n)
  error = a_vtk_file%xml_writer%write_geo(x=x, y=x, z=x)
  error = a_vtk_file%xml_writer%write_dataarray(location='node', action='open')
  error = a_vtk_file%xml_writer%write_dataarray(data_name='temperature', x=t, one_component=.true.)
  error = a_vtk_file%xml_writer%write_dataarray(location='node', action='close')
  error = a_vtk_file%xml_writer%write_piece()
  error = a_vtk_file%finalize()
  endfunction write_file

  pure function blob(x, y, z, centre) result(t)
  !< A hot blob: a Gaussian bump of temperature 1 at its centre.
  real(R8P), intent(in) :: x, y, z, centre(3)
  real(R8P)             :: t

  t = exp(-((x - centre(1))**2 + (y - centre(2))**2 + (z - centre(3))**2)/0.04_R8P)
  endfunction blob
endprogram heat
