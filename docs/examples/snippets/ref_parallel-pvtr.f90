error = a_pvtk_file%initialize(filename='ref.pvtr', mesh_topology='PRectilinearGrid', mesh_kind='Float64', &
                               nx1=0, nx2=4, ny1=0, ny2=2, nz1=0, nz2=2)
! ... PPointData / PCellData as for PVTS ...
error = a_pvtk_file%xml_writer%write_parallel_geo(source='ref_part_1.vtr', nx1=0, nx2=2, ny1=0, ny2=2, nz1=0, nz2=2)
error = a_pvtk_file%xml_writer%write_parallel_geo(source='ref_part_2.vtr', nx1=2, nx2=4, ny1=0, ny2=2, nz1=0, nz2=2)
error = a_pvtk_file%finalize()
