!< VTK_Fortran test.
program write_vtu
!-----------------------------------------------------------------------------------------------------------------------------------
!< VTK_Fortran test.
!-----------------------------------------------------------------------------------------------------------------------------------
use penf
use vtk_fortran, only : vtk_file
!-----------------------------------------------------------------------------------------------------------------------------------

!-----------------------------------------------------------------------------------------------------------------------------------
implicit none
type(vtk_file)                :: a_vtk_file                                                   !< A VTK file.
integer(I4P), parameter       :: np = 27_I4P                                                  !< Number of points.
integer(I4P), parameter       :: nc = 11_I4P                                                  !< Number of cells.
real(R4P),    dimension(1:np) :: x = [0,1,2,0,1,2,0,1,2,0,1,2,0,1,2,0,1,2,0,1,2,0,1,2,0,1,2]  !< X coordinates of points.
real(R4P),    dimension(1:np) :: y = [0,0,0,1,1,1,0,0,0,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1]  !< Y coordinates of points.
real(R4P),    dimension(1:np) :: z = [0,0,0,0,0,0,1,1,1,1,1,1,2,2,2,3,3,3,4,4,4,5,5,5,6,6,6]  !< Z coordinates of points.
integer(I1P), dimension(1:nc) :: cell_type = [12_I1P,12_I1P,10_I1P,10_I1P,&
                                              7_I1P,6_I1P,9_I1P,5_I1P,5_I1P,3_I1P,1_I1P]      !< Cells type.
integer(I4P), dimension(1:nc) :: offset = [8_I4P,16_I4P,20_I4P,24_I4P,30_I4P,&
                                           36_I4P,40_I4P,43_I4P,46_I4P,48_I4P,49_I4P]         !< Cells offset.
integer(I4P), dimension(1:49) :: connect                                                      !< Connectivity.
real(R8P),    dimension(1:np) :: v                                                            !< One component points-variable.
integer(I4P), dimension(1:np) :: v_x                                                          !< X component points-variable.
integer(I4P), dimension(1:np) :: v_y                                                          !< Y component points-variable.
integer(I4P), dimension(1:np) :: v_z                                                          !< Z component points-variable.
integer(I4P)                  :: error                                                        !< Status error.
logical                       :: test_passed(1)                                               !< List of passed tests.
!-----------------------------------------------------------------------------------------------------------------------------------

!-----------------------------------------------------------------------------------------------------------------------------------
connect = [0 ,1 ,4 ,3 ,6 ,7 ,10,9 , &
           1 ,2 ,5 ,4 ,7 ,8 ,11,10, &
           6 ,10,9 ,12,             &
           5 ,11,10,14,             &
           15,16,17,14,13,12,       &
           18,15,19,16,20,17,       &
           22,23,20,19,             &
           21,22,18,                &
           22,19,18,                &
           26,25,                   &
           24]
v = [0.0,1.0,2.0,3.0,4.0,5.0,6.0,7.0,8.0,9.0,10.0,11.0,12.0,13.0,14.0,15.0,16.0,17.0, &
     18.0,19.0,20.0,21.0,22.0,23.0,24.0,25.0,26.0]
v_X = [1,1,0,1,1,0,1,1,0,1,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0]
v_Y = [0,1,2,0,1,2,0,1,2,0,1,2,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0]
v_Z = [0,0,0,0,0,0,0,0,0,0,0,0,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1]
! ascii
error = a_vtk_file%initialize(format='ascii', filename='XML_UNST-ascii.vtu', mesh_topology='UnstructuredGrid')
call write_data
error = a_vtk_file%finalize()
! raw
error = a_vtk_file%initialize(format='raw', filename='XML_UNST-raw.vtu', mesh_topology='UnstructuredGrid')
call write_data
error = a_vtk_file%finalize()
! binary
error = a_vtk_file%initialize(format='binary', filename='XML_UNST-binary.vtu', mesh_topology='UnstructuredGrid')
call write_data
error = a_vtk_file%finalize()

test_passed = .true. ! nothing to test yet

print "(A,L1)", new_line('a')//'Are all tests passed? ', all(test_passed)
stop
!-----------------------------------------------------------------------------------------------------------------------------------
contains
  subroutine write_data
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Write data.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  error = a_vtk_file%xml_writer%write_piece(np=np, nc=nc)
  error = a_vtk_file%xml_writer%write_geo(np=np, nc=nc, x=x, y=y, z=z)
  error = a_vtk_file%xml_writer%write_connectivity(nc=nc, connectivity=connect, offset=offset, cell_type=cell_type)
  error = a_vtk_file%xml_writer%write_dataarray(location='node', action='open')
  error = a_vtk_file%xml_writer%write_dataarray(data_name='scalars', x=v)
  error = a_vtk_file%xml_writer%write_dataarray(data_name='vector', x=v_x, y=v_y, z=v_z)
  error = a_vtk_file%xml_writer%write_dataarray(location='node', action='close')
  error = a_vtk_file%xml_writer%write_piece()
  !---------------------------------------------------------------------------------------------------------------------------------
  endsubroutine write_data
endprogram write_vtu
