!< VTK_Fortran test for Parallel (partioned) Structured Grid.
program vtk_fortran_write_pvts
!< VTK_Fortran test for Parallel (partioned) Structured Grid.
use penf
use vtk_fortran, only : vtk_file, pvtk_file

implicit none
!< The mesh is a simple prism partitioned into two pieces along x direction at ordinate i=nx2_p(1).
!<```
!< y ^
!<   |               ny2 +-----------------+--------------+
!<   |                   |                 |              |
!<   |                   |                 |              |
!<   |                   |                 |              |
!<   |                   |                 |              |
!<   o-------->      ny1 +-----------------+--------------+
!<            x         nx1               i=nx2_p(1)     nx2
!<```
integer(I4P), parameter :: nx1=0_I4P, nx2=9_I4P, ny1=0_I4P, ny2=5_I4P, nz1=0_I4P, nz2=5_I4P !< Whole extents.
integer(I4P), parameter :: nx1_p(1:2)=[nx1, 4_I4P]                                          !< Lower x extents.
integer(I4P), parameter :: nx2_p(1:2)=[4_I4P, nx2]                                          !< Upper x extents.
integer(I4P), parameter :: nn       = (nx2     -nx1     +1)*(ny2-ny1+1)*(nz2-nz1+1)         !< Whole number of points.
integer(I4P), parameter :: nn_p(1:2)=[(nx2_p(1)-nx1     +1)*(ny2-ny1+1)*(nz2-nz1+1),&
                                      (nx2_p(2)-nx2_p(1)+1)*(ny2-ny1+1)*(nz2-nz1+1)]        !< Partinoned number of points.

real(R8P),    dimension(nx1:nx2,ny1:ny2,nz1:nz2) :: x, y, z !< Mesh coordinates.
integer(I4P), dimension(nx1:nx2,ny1:ny2,nz1:nz2) :: v       !< Noces-centered value.
integer(I4P)                                     :: i       !< Counter.
integer(I4P)                                     :: j       !< Counter.
integer(I4P)                                     :: k       !< Counter.

! inizialize data
do k=nz1, nz2
   do j=ny1, ny2
      do i=nx1, nx2
         x(i, j, k) = i*1._R8P
         y(i, j, k) = j*1._R8P
         z(i, j, k) = k*1._R8P
         v(i, j, k) = i*j*k
      enddo
   enddo
enddo

call write_vts(part=1, filename='vtkfortran_write_pvts_01.vts')
call write_vts(part=2, filename='vtkfortran_write_pvts_02.vts')
call write_pvts(filename='vtkfortran_write_pvts.pvts', parts_filename=['vtkfortran_write_pvts_01.vts', &
                                                                       'vtkfortran_write_pvts_02.vts'])
contains
   subroutine write_vts(part, filename)
   !< Write VTS parts.
   integer(I4P), intent(in) :: part         !< Part to be saved [1,2].
   character(*), intent(in) :: filename     !< Output file name.
   type(vtk_file)           :: a_vtk_file   !< A VTK file.
   integer(I4P)             :: error        !< Status error.

   error = a_vtk_file%initialize(format='binary', filename=filename, mesh_topology='StructuredGrid', &
                                 nx1=nx1_p(part), nx2=nx2_p(part), ny1=ny1, ny2=ny2, nz1=nz1, nz2=nz2)
   error = a_vtk_file%xml_writer%write_piece(nx1=nx1_p(part), nx2=nx2_p(part), ny1=ny1, ny2=ny2, nz1=nz1, nz2=nz2)
   error = a_vtk_file%xml_writer%write_geo(n=nn_p(part), x=x(nx1_p(part):nx2_p(part),:,:), &
                                                         y=y(nx1_p(part):nx2_p(part),:,:), &
                                                         z=z(nx1_p(part):nx2_p(part),:,:))
   error = a_vtk_file%xml_writer%write_dataarray(location='node', action='open')
   error = a_vtk_file%xml_writer%write_dataarray(data_name='int32_scalar', x=v(nx1_p(part):nx2_p(part),:,:), &
                                                 one_component=.true.)
   error = a_vtk_file%xml_writer%write_dataarray(location='node', action='close')
   error = a_vtk_file%xml_writer%write_piece()
   error = a_vtk_file%finalize()
   endsubroutine write_vts

   subroutine write_pvts(filename, parts_filename)
   !< Write PVTS.

   character(*), intent(in) :: filename          !< Output file name.
   character(*), intent(in) :: parts_filename(:) !< Parts file name.
   type(pvtk_file)          :: a_pvtk_file       !< A parallel (partioned) VTK file.
   integer(I4P)             :: error             !< Status error.

   error = a_pvtk_file%initialize(filename=filename, mesh_topology='PStructuredGrid', mesh_kind="Float64", &
                                  nx1=nx1, nx2=nx2, ny1=ny1, ny2=ny2, nz1=nz1, nz2=nz2)
   error = a_pvtk_file%xml_writer%write_dataarray(location='node', action='open')
   error = a_pvtk_file%xml_writer%write_parallel_dataarray(data_name='int32_scalar', &
                                                           data_type='Int32', &
                                                           number_of_components=1)
   error = a_pvtk_file%xml_writer%write_dataarray(location='node', action='close')
   error = a_pvtk_file%xml_writer%write_parallel_geo(source=parts_filename(1), &
                                                     nx1=nx1, nx2=nx2_p(1), ny1=ny1, ny2=ny2, nz1=nz1, nz2=nz2)
   error = a_pvtk_file%xml_writer%write_parallel_geo(source=parts_filename(2), &
                                                     nx1=nx2_p(1), nx2=nx2_p(2), ny1=ny1, ny2=ny2, nz1=nz1, nz2=nz2)
   error = a_pvtk_file%finalize()
   endsubroutine write_pvts
endprogram vtk_fortran_write_pvts
