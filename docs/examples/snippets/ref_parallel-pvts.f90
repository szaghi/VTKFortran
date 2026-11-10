use vtk_fortran, only : pvtk_file
use penf,        only : I4P

type(pvtk_file)         :: a_pvtk_file
integer(I4P), parameter :: nx1=0, nx2=9, ny1=0, ny2=5, nz1=0, nz2=5 ! whole extent
integer(I4P), parameter :: nx1_p(2) = [0, 4]                        ! extents of the pieces along x:
integer(I4P), parameter :: nx2_p(2) = [4, 9]                        ! adjacent pieces share the plane i=4
integer(I4P)            :: error

! --- each rank writes its own piece: a complete .vts file (a few lines of the program, see the cookbook) ---
call write_piece(filename='ref_part_1.vts', i1=nx1_p(1), i2=nx2_p(1))
call write_piece(filename='ref_part_2.vts', i1=nx1_p(2), i2=nx2_p(2))

! --- one rank writes the parallel header ---
error = a_pvtk_file%initialize(filename='ref.pvts', mesh_topology='PStructuredGrid', mesh_kind='Float64', &
                               nx1=nx1, nx2=nx2, ny1=ny1, ny2=ny2, nz1=nz1, nz2=nz2)
error = a_pvtk_file%xml_writer%write_dataarray(location='node', action='open')
error = a_pvtk_file%xml_writer%write_parallel_dataarray(data_name='pressure', data_type='Float64', number_of_components=1)
error = a_pvtk_file%xml_writer%write_dataarray(location='node', action='close')
error = a_pvtk_file%xml_writer%write_parallel_geo(source='ref_part_1.vts', &
                                                  nx1=nx1_p(1), nx2=nx2_p(1), ny1=ny1, ny2=ny2, nz1=nz1, nz2=nz2)
error = a_pvtk_file%xml_writer%write_parallel_geo(source='ref_part_2.vts', &
                                                  nx1=nx1_p(2), nx2=nx2_p(2), ny1=ny1, ny2=ny2, nz1=nz1, nz2=nz2)
error = a_pvtk_file%finalize()
