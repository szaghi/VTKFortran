program read_array
!< Read one array of a file (here a file written by VTK): ask its type and size, then read it into a kind that holds it.
use penf, only : I4P, I8P, R4P, R8P
use vtk_fortran, only : vtk_file
implicit none
type(vtk_file)                :: a_vtk_file
character(len=:), allocatable :: data_type
real(R8P),        allocatable :: p(:)
integer(I4P),     allocatable :: wrong(:)
integer(I8P)                  :: n_tuples
integer(I4P)                  :: n_components, error

error = a_vtk_file%initialize(filename='vtk_tetra.vtu', action='read')
error = a_vtk_file%xml_reader%get_dataarray_info(location='node', data_name='p', data_type=data_type, &
                                                 n_components=n_components, n_tuples=n_tuples)
print '(A,A,A,I0,A,I0,A)', 'p: ', data_type, ', ', n_components, ' component, ', n_tuples, ' tuples'
! Float32 reads into R4P or R8P
error = a_vtk_file%xml_reader%read_dataarray(location='node', data_name='p', x=p)
print '(A,4F5.1)', 'p:', p
! not into an integer: error 5
error = a_vtk_file%xml_reader%read_dataarray(location='node', data_name='p', x=wrong)
print '(A,I0)', 'p into I4P: error ', error
error = a_vtk_file%finalize()
endprogram read_array
