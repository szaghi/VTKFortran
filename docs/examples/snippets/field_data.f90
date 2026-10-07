program field_data
!< Attach global data to a dataset: numbers, arrays, strings; then read them back.
use penf, only : I4P, I8P, R8P
use vtk_fortran, only : vtk_file
implicit none
type(vtk_file)                :: a_vtk_file
real(R8P),        allocatable :: residuals(:)
character(len=:), allocatable :: species(:), names(:)
integer(I4P)                  :: error

error = a_vtk_file%initialize(format='binary', filename='case.vtr', mesh_topology='RectilinearGrid', &
                              nx1=1, nx2=2, ny1=1, ny2=2, nz1=1, nz2=2)
error = a_vtk_file%xml_writer%write_fielddata(action='open')
error = a_vtk_file%xml_writer%write_fielddata(data_name='TIME', x=0.25_R8P)
error = a_vtk_file%xml_writer%write_fielddata(data_name='CYCLE', x=100_I8P)
error = a_vtk_file%xml_writer%write_fielddata(data_name='residuals', x=[1.e-2_R8P, 1.e-4_R8P, 1.e-6_R8P])
error = a_vtk_file%xml_writer%write_fielddata(data_name='solver', x='my solver v2.1')
error = a_vtk_file%xml_writer%write_fielddata(data_name='species', x=['N2 ', 'O2 ', 'CO2'])
error = a_vtk_file%xml_writer%write_fielddata(action='close')
error = a_vtk_file%xml_writer%write_piece(nx1=1, nx2=2, ny1=1, ny2=2, nz1=1, nz2=2)
error = a_vtk_file%xml_writer%write_geo(x=[0._R8P, 1._R8P], y=[0._R8P, 1._R8P], z=[0._R8P, 1._R8P])
error = a_vtk_file%xml_writer%write_piece()
error = a_vtk_file%finalize()

error = a_vtk_file%initialize(filename='case.vtr', action='read')
error = a_vtk_file%xml_reader%get_dataarray_names(location='field', names=names)
print '(A,*(1X,A))', 'field data:', (trim(names(error)), error=1, size(names))
error = a_vtk_file%xml_reader%read_dataarray(location='field', data_name='residuals', x=residuals)
error = a_vtk_file%xml_reader%read_dataarray(location='field', data_name='species', x=species)
print '(A,*(1X,ES8.1))', 'residuals:', residuals
print '(A,*(1X,A))', 'species:', species
error = a_vtk_file%finalize()
endprogram field_data
