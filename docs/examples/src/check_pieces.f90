!run check_pieces check_pieces
program check_pieces
!< Find the mismatch between a parallel header and its pieces before ParaView does.
use penf, only : I1P, I4P, R4P, R8P
use vtk_fortran, only : pvtk_file, vtk_file
implicit none
type(pvtk_file)               :: header
type(vtk_file)                :: piece
character(len=:), allocatable :: message
integer(I4P)                  :: error

! a piece with a Float32 pressure
error = piece%initialize(format='raw', filename='p_1.vtu', mesh_topology='UnstructuredGrid')
error = piece%xml_writer%write_piece(np=4, nc=1)
error = piece%xml_writer%write_geo(np=4, nc=1, x=[0._R8P, 1._R8P, 0._R8P, 0._R8P], y=[0._R8P, 0._R8P, 1._R8P, 0._R8P], &
                                   z=[0._R8P, 0._R8P, 0._R8P, 1._R8P])
error = piece%xml_writer%write_connectivity(nc=1, connectivity=[0, 1, 2, 3], offset=[4], cell_type=[10_I1P])
error = piece%xml_writer%write_dataarray(location='node', action='open')
error = piece%xml_writer%write_dataarray(data_name='pressure', x=[1._R4P, 2._R4P, 3._R4P, 4._R4P])
error = piece%xml_writer%write_dataarray(location='node', action='close')
error = piece%xml_writer%write_piece()
error = piece%finalize()
! a header declaring it Float64
error = header%initialize(filename='p.pvtu', mesh_topology='PUnstructuredGrid', mesh_kind='Float64')
error = header%xml_writer%write_dataarray(location='node', action='open')
error = header%xml_writer%write_parallel_dataarray(data_name='pressure', data_type='Float64', number_of_components=1)
error = header%xml_writer%write_dataarray(location='node', action='close')
error = header%xml_writer%write_parallel_geo(source='p_1.vtu')
error = header%finalize()

error = header%initialize(filename='p.pvtu', action='read')
error = header%xml_reader%check_pieces(message=message)
print '(A,I0)', 'check_pieces error ', error
print '(A)', message
error = header%finalize()
endprogram check_pieces
