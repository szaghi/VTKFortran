integer(I4P), parameter :: connect(12)  = [0,1,2,3,4,5,6,7, 8,9,10,11]
integer(I4P), parameter :: offset(2)    = [8, 12]
integer(I1P), parameter :: cell_type(2) = [42_I1P, 10_I1P]  ! polyhedron, tetrahedron
integer(I4P), parameter :: face(31)     = [6,                &  ! the cube has 6 faces,
                                           4, 0,1,2,3,       &  ! each made of 4 points
                                           4, 4,5,6,7,       &
                                           4, 0,1,5,4,       &
                                           4, 1,2,6,5,       &
                                           4, 2,3,7,6,       &
                                           4, 3,0,4,7]
integer(I4P), parameter :: faceoffset(2) = [31, -1]          ! the tetrahedron is not a polyhedron

error = a_vtk_file%xml_writer%write_connectivity(nc=2, connectivity=connect, offset=offset, cell_type=cell_type, &
                                                  face=face, faceoffset=faceoffset)
