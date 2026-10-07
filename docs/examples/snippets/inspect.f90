program inspect
!< Print what a VTK file holds: its dataset, pieces and arrays, with the range of each array.
use penf, only : I4P, I8P, R8P
use vtk_fortran, only : vtk_file
implicit none
character(len=256)            :: filename
character(len=:), allocatable :: topology, compressor, names(:), data_type
character(len=5), parameter   :: locations(3)=['node ', 'cell ', 'field']
real(R8P),        allocatable :: values(:)
integer(I8P),     allocatable :: ids(:)
type(vtk_file)                :: a_file
integer(I8P)                  :: np, nc
integer(I4P)                  :: npieces, ncomp, p, l, a, error

call get_command_argument(1, filename)
error = a_file%initialize(filename=trim(filename), action='read')
if (error /= 0) error stop 'cannot read the file'
error = a_file%xml_reader%get_info(mesh_topology=topology, npieces=npieces, compressor=compressor)
print '(A,A,I0,A,A)', trim(filename)//': ', topology//', pieces: ', npieces, ', compressor: ', compressor
do p=1, npieces
  error = a_file%xml_reader%read_piece(piece=p, np=np, nc=nc)
  print '(A,I0,A,I0,A,I0,A)', '  piece ', p, ': ', np, ' points, ', nc, ' cells'
  do l=1, size(locations)
    if (l == 3 .and. p > 1) cycle ! field data belong to the dataset
    error = a_file%xml_reader%get_dataarray_names(location=trim(locations(l)), names=names, piece=p)
    do a=1, size(names)
      error = a_file%xml_reader%get_dataarray_info(location=trim(locations(l)), data_name=trim(names(a)), piece=p, &
                                                   data_type=data_type, n_components=ncomp)
      if (data_type(1:1) == 'F') then
        error = a_file%xml_reader%read_dataarray(location=trim(locations(l)), data_name=trim(names(a)), x=values, piece=p)
      elseif (data_type /= 'String') then
        error = a_file%xml_reader%read_dataarray(location=trim(locations(l)), data_name=trim(names(a)), x=ids, piece=p)
        values = real(ids, R8P)
      else
        cycle
      endif
      print '(4X,A6,1X,A16,1X,A8,I2,A,2(1X,ES11.4))', locations(l), names(a), data_type, ncomp, ' comp., range', &
            minval(values), maxval(values)
    enddo
  enddo
enddo
error = a_file%finalize()
endprogram inspect
