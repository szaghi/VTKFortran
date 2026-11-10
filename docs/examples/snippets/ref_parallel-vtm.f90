use vtk_fortran, only : vtm_file, vtk_file
use penf,        only : I4P, R8P

type(vtk_file)  :: a_vtk_file
type(vtm_file)  :: a_vtm_file
character(16)   :: filenames(4)
real(R8P)       :: x(0:3), y(0:2), z(0:2)
integer(I4P)    :: f, error

filenames = ['ref_block_1.vtr', 'ref_block_2.vtr', 'ref_block_3.vtr', 'ref_block_4.vtr']
y = [0, 1, 2] ; z = [0, 1, 2]
! write each block as an independent file
do f=1, size(filenames, dim=1)
  x = [0, 1, 2, 3] + 4._R8P*(f - 1)
  error = a_vtk_file%initialize(format='raw', filename=trim(filenames(f)), mesh_topology='RectilinearGrid', &
                                nx1=0, nx2=3, ny1=0, ny2=2, nz1=0, nz2=2)
  error = a_vtk_file%xml_writer%write_piece(nx1=0, nx2=3, ny1=0, ny2=2, nz1=0, nz2=2)
  error = a_vtk_file%xml_writer%write_geo(x=x, y=y, z=z)
  error = a_vtk_file%xml_writer%write_piece()
  error = a_vtk_file%finalize()
enddo

! assemble the multi-block file
error = a_vtm_file%initialize(filename='ref.vtm')
error = a_vtm_file%write_block(filenames=[filenames(1), filenames(2)], names=['1','2'], name='first block')
error = a_vtm_file%write_block(filenames=[filenames(3), filenames(4)], names=['3','4'], name='second block')
error = a_vtm_file%finalize()
