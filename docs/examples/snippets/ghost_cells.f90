program ghost_cells
!< Mark ghost cells, the duplicates a piece keeps of its neighbours, so that readers do not draw them twice.
use penf, only : I1P, I2P, I4P, R8P
use vtk_fortran, only : vtk_file
implicit none
type(vtk_file)            :: a_vtk_file
integer(I1P), allocatable :: flags(:)
integer(I2P), allocatable :: values(:)
integer(I4P)              :: error

error = a_vtk_file%initialize(format='raw', filename='piece.vtr', mesh_topology='RectilinearGrid', &
                              nx1=1, nx2=5, ny1=1, ny2=2, nz1=1, nz2=2)
error = a_vtk_file%xml_writer%write_piece(nx1=1, nx2=5, ny1=1, ny2=2, nz1=1, nz2=2)
error = a_vtk_file%xml_writer%write_geo(x=[0._R8P, 1._R8P, 2._R8P, 3._R8P, 4._R8P], y=[0._R8P, 1._R8P], &
                                        z=[0._R8P, 1._R8P])
error = a_vtk_file%xml_writer%write_dataarray(location='cell', action='open')
! vtkGhostType is an unsigned byte: 0 an owned cell, 1 a duplicate (ghost) cell; here the last cell is a ghost
error = a_vtk_file%xml_writer%write_dataarray_unsigned(data_name='vtkGhostType', x=[0_I1P, 0_I1P, 0_I1P, 1_I1P])
error = a_vtk_file%xml_writer%write_dataarray(location='cell', action='close')
error = a_vtk_file%xml_writer%write_piece()
error = a_vtk_file%finalize()

! unsigned arrays read back into the signed kind of the same width (same bits) or into a wider one (same values)
error = a_vtk_file%initialize(filename='piece.vtr', action='read')
error = a_vtk_file%xml_reader%read_dataarray(location='cell', data_name='vtkGhostType', x=flags)
error = a_vtk_file%xml_reader%read_dataarray(location='cell', data_name='vtkGhostType', x=values)
print '(A,4I4,A,4I4)', 'vtkGhostType as I1P:', flags, ', as I2P:', values
error = a_vtk_file%finalize()
endprogram ghost_cells
