!run ids_64bit ids_64bit
!run ids_64bit-grep grep -o 'type="Int64"[^>]*Name="[a-z]*"' tet64.vtu
program ids_64bit
!< Write the counts and the connectivity in 64 bits, for meshes with more than 2^31 points or cells.
use penf, only : I1P, I8P, R8P
use vtk_fortran, only : vtk_file
implicit none
type(vtk_file) :: a_vtk_file
integer        :: error

! the kind of the arguments selects the version: I8P counts and ids are written as Int64
error = a_vtk_file%initialize(format='ascii', filename='tet64.vtu', mesh_topology='UnstructuredGrid')
error = a_vtk_file%xml_writer%write_piece(np=4_I8P, nc=1_I8P)
error = a_vtk_file%xml_writer%write_geo(np=4_I8P, nc=1_I8P, x=[0._R8P, 1._R8P, 0._R8P, 0._R8P], &
                                        y=[0._R8P, 0._R8P, 1._R8P, 0._R8P], z=[0._R8P, 0._R8P, 0._R8P, 1._R8P])
error = a_vtk_file%xml_writer%write_connectivity(nc=1_I8P, connectivity=[0_I8P, 1_I8P, 2_I8P, 3_I8P], offset=[4_I8P], &
                                                 cell_type=[10_I1P])
error = a_vtk_file%xml_writer%write_piece()
error = a_vtk_file%finalize()
endprogram ids_64bit
