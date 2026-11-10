use vtk_fortran, only : vtk_file, write_xml_volatile
use penf,        only : I4P, R8P

type(vtk_file)                :: a_vtk_file
character(len=:), allocatable :: xml_volatile
integer(I4P)                  :: error

! on a process without access to the file system
error = a_vtk_file%initialize(format='binary', filename='ref_volatile.vtr', mesh_topology='RectilinearGrid', &
                              is_volatile=.true., nx1=0, nx2=1, ny1=0, ny2=1, nz1=0, nz2=1)
error = a_vtk_file%xml_writer%write_piece(nx1=0, nx2=1, ny1=0, ny2=1, nz1=0, nz2=1)
error = a_vtk_file%xml_writer%write_geo(x=[0._R8P, 1._R8P], y=[0._R8P, 1._R8P], z=[0._R8P, 1._R8P])
error = a_vtk_file%xml_writer%write_piece()
error = a_vtk_file%finalize()
call a_vtk_file%get_xml_volatile(xml_volatile) ! the whole file, as a string
call a_vtk_file%free                           ! free the memory of the volatile file
! ... send xml_volatile to the master ...

! on the master
error = write_xml_volatile(xml_volatile=xml_volatile, filename='ref_volatile.vtr')
