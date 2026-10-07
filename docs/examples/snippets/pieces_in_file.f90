program pieces_in_file
!< Write several pieces in one file: e.g. the blocks of a solver, written by one process.
use penf, only : I4P, R8P
use vtk_fortran, only : vtk_file
implicit none
type(vtk_file) :: a_vtk_file
integer(I4P)   :: b, error

! the whole extent, then each piece with its own extent inside it; adjacent pieces share their boundary
error = a_vtk_file%initialize(format='raw', filename='blocks.vtr', mesh_topology='RectilinearGrid', &
                              nx1=0, nx2=6, ny1=0, ny2=1, nz1=0, nz2=1)
do b=1, 3
  error = a_vtk_file%xml_writer%write_piece(nx1=2*(b-1), nx2=2*b, ny1=0, ny2=1, nz1=0, nz2=1)
  error = a_vtk_file%xml_writer%write_geo(x=[real(2*b-2, R8P), real(2*b-1, R8P), real(2*b, R8P)], y=[0._R8P, 1._R8P], &
                                          z=[0._R8P, 1._R8P])
  error = a_vtk_file%xml_writer%write_dataarray(location='cell', action='open')
  error = a_vtk_file%xml_writer%write_dataarray(data_name='block', x=[b, b])
  error = a_vtk_file%xml_writer%write_dataarray(location='cell', action='close')
  error = a_vtk_file%xml_writer%write_piece()
enddo
error = a_vtk_file%finalize()
endprogram pieces_in_file
