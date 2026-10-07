!run volatile volatile
program volatile
!< Write a file into memory, as a process without access to the file system would; another one saves it.
use penf, only : I4P, R8P
use vtk_fortran, only : vtk_file, write_xml_volatile
implicit none
type(vtk_file)                :: a_vtk_file
character(len=:), allocatable :: xml
integer(I4P)                  :: error

error = a_vtk_file%initialize(format='binary', filename='part.vtr', mesh_topology='RectilinearGrid', &
                              nx1=1, nx2=2, ny1=1, ny2=2, nz1=1, nz2=2, is_volatile=.true.)
error = a_vtk_file%xml_writer%write_piece(nx1=1, nx2=2, ny1=1, ny2=2, nz1=1, nz2=2)
error = a_vtk_file%xml_writer%write_geo(x=[0._R8P, 1._R8P], y=[0._R8P, 1._R8P], z=[0._R8P, 1._R8P])
error = a_vtk_file%xml_writer%write_piece()
error = a_vtk_file%finalize()
call a_vtk_file%get_xml_volatile(xml) ! the whole file, as a string: send it to the process that writes
call a_vtk_file%free
print '(A,I0,A)', 'the file is in memory: ', len(xml), ' characters'
! ... on the process that accesses the file system
error = write_xml_volatile(xml_volatile=xml, filename='part.vtr')
print '(A,I0)', 'part.vtr written, error ', error
endprogram volatile
