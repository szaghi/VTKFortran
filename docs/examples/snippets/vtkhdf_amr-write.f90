error = a_vtk_file%initialize(format='vtkhdf', filename='amr.vtkhdf', mesh_topology='OverlappingAMR', &
                              origin=[0._R8P, 0._R8P, 0._R8P])
! level 0: the whole domain, spacing 1
error = a_vtk_file%xml_writer%write_level(level=0, spacing=[1._R8P, 1._R8P, 1._R8P])
error = box(nx1=0, nx2=16, ny1=0, ny2=8, h=1._R8P)
! level 1: spacing 0.5, two boxes around the hot spot (their point indices are those of level 1)
error = a_vtk_file%xml_writer%write_level(level=1, spacing=[0.5_R8P, 0.5_R8P, 0.5_R8P])
error = box(nx1=8, nx2=16, ny1=4, ny2=12, h=0.5_R8P)
error = box(nx1=16, nx2=24, ny1=4, ny2=12, h=0.5_R8P)
error = a_vtk_file%finalize()
