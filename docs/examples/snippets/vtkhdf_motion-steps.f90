function sea(time) result(error)
!< One step of the sea: a surface of nx x ny quadrilaterals, its height a wave travelling along x.
real(R8P), intent(in) :: time  !< Time.
integer(I4P)          :: error !< Error status.
real(R8P)             :: x(0:nx,0:ny), y(0:nx,0:ny), z(0:nx,0:ny)
integer(I4P)          :: i, j, c(4 * nx * ny)

do j=0, ny ; do i=0, nx
  x(i,j) = i ; y(i,j) = j ; z(i,j) = wave(real(i, R8P), time)
enddo ; enddo
do j=0, ny - 1 ; do i=0, nx - 1 ! the corners of each quadrilateral, counter-clockwise
  c(4 * (j * nx + i) + 1:4 * (j * nx + i) + 4) = [i + j * (nx + 1), i + 1 + j * (nx + 1), i + 1 + (j + 1) * (nx + 1), &
                                                  i + (j + 1) * (nx + 1)]
enddo ; enddo
error = a_vtk_file%xml_writer%write_step(time=time)
error = a_vtk_file%xml_writer%write_piece(np=(nx + 1) * (ny + 1), nverts=0, nlines=0, nstrips=0, npolys=nx * ny)
error = a_vtk_file%xml_writer%write_geo(np=(nx + 1) * (ny + 1), nc=nx * ny, x=reshape(x, [size(x)]), &
                                        y=reshape(y, [size(y)]), z=reshape(z, [size(z)]))
error = a_vtk_file%xml_writer%write_polydata_cells(polys_connectivity=c, polys_offset=[(4 * i, i=1, nx * ny)])
error = a_vtk_file%xml_writer%write_dataarray(location='node', action='open')
error = a_vtk_file%xml_writer%write_dataarray(data_name='height', x=reshape(z, [size(z)]))
error = a_vtk_file%xml_writer%write_dataarray(location='node', action='close')
error = a_vtk_file%xml_writer%write_piece()
endfunction sea

function buoy(time) result(error)
!< One step of the buoy: two hexahedra (a 2 x 2 x 0.4 box) at x = 6...8, lifted by the wave at its centre.
real(R8P), intent(in) :: time  !< Time.
integer(I4P)          :: error !< Error status.
real(R8P)             :: x(12), y(12), z(12)
integer(I4P)          :: p

do p=0, 11 ! x = 6 + mod(p, 3), y = 3 + 2 mod(p / 3, 2), z = the wave at x = 7, -0.2 or +0.2
  x(p + 1) = 6 + mod(p, 3) ; y(p + 1) = 3 + 2 * mod(p / 3, 2) ; z(p + 1) = wave(7._R8P, time) - 0.2_R8P + 0.4_R8P * (p / 6)
enddo
error = a_vtk_file%xml_writer%write_step(time=time)
error = a_vtk_file%xml_writer%write_piece(np=12, nc=2)
error = a_vtk_file%xml_writer%write_geo(np=12, nc=2, x=x, y=y, z=z)
error = a_vtk_file%xml_writer%write_connectivity(nc=2, connectivity=[0, 1, 4, 3, 6, 7, 10, 9, 1, 2, 5, 4, 7, 8, 11, 10], &
                                                 offset=[8, 16], cell_type=[12_I1P, 12_I1P])
error = a_vtk_file%xml_writer%write_dataarray(location='node', action='open')
error = a_vtk_file%xml_writer%write_dataarray(data_name='height', x=z)
error = a_vtk_file%xml_writer%write_dataarray(location='node', action='close')
error = a_vtk_file%xml_writer%write_piece()
endfunction buoy
