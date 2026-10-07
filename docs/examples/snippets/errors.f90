program errors
!< Every procedure returns an error status: 0 on success; the readers say what went wrong.
use penf, only : I4P, R8P
use vtk_fortran, only : vtk_file
implicit none
type(vtk_file)         :: a_vtk_file
real(R8P), allocatable :: x(:)
integer(I4P)           :: error

error = a_vtk_file%initialize(filename='no_such_file.vtu', action='read')
print '(A,I0)', 'a missing file:        ', error  ! 1
error = a_vtk_file%initialize(filename='vtk_tetra.vtu', action='read')
print '(A,I0)', 'a good file:           ', error  ! 0
error = a_vtk_file%xml_reader%read_dataarray(location='node', data_name='no_such_array', x=x)
print '(A,I0)', 'a missing array:       ', error  ! 4
error = a_vtk_file%xml_reader%read_dataarray(location='node', data_name='p', x=x, piece=2)
print '(A,I0)', 'a missing piece:       ', error  ! 4
error = a_vtk_file%finalize()
error = a_vtk_file%initialize(format='rawest', filename='x.vtu', mesh_topology='UnstructuredGrid')
print '(A,I0)', 'an unknown format:     ', error  ! not 0
endprogram errors
