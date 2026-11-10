program ref_topologies
!< The examples of the reference page "Topologies": each subroutine writes one file, then every file is read back.
use penf, only : I4P
use vtk_fortran, only : vtk_file
implicit none

call image_data
call rectilinear_grid
call structured_grid
call unstructured_grid
call polyhedron
call polydata
call packed_coordinates
call several_pieces
call check([character(20) :: 'ref_image.vti', 'ref_rectilinear.vtr', 'ref_structured.vts', 'ref_unstructured.vtu', &
            'ref_polyhedron.vtu', 'ref_polydata.vtp', 'ref_packed.vts', 'ref_packed.vtu', 'ref_blocks.vtu'])
contains
  subroutine image_data
  use vtk_fortran, only : vtk_file
  use penf,        only : I4P, R8P

  type(vtk_file) :: a_vtk_file
  integer(I4P)   :: i, j, k, error
  real(R8P)      :: phi(0:15,0:15,0:7)   ! point data, one value per grid point

  do k=0, 7 ; do j=0, 15 ; do i=0, 15
    phi(i,j,k) = sin(0.4_R8P*i) * cos(0.4_R8P*j) + 0.1_R8P*k
  enddo ; enddo ; enddo
  error = a_vtk_file%initialize(format='raw', filename='ref_image.vti', mesh_topology='ImageData', &
                                nx1=0, nx2=15, ny1=0, ny2=15, nz1=0, nz2=7,                     &
                                origin=[0._R8P, 0._R8P, 0._R8P], spacing=[0.1_R8P, 0.1_R8P, 0.2_R8P])
  error = a_vtk_file%xml_writer%write_piece(nx1=0, nx2=15, ny1=0, ny2=15, nz1=0, nz2=7)
  error = a_vtk_file%xml_writer%write_dataarray(location='node', action='open')
  error = a_vtk_file%xml_writer%write_dataarray(data_name='phi', x=phi, one_component=.true.)
  error = a_vtk_file%xml_writer%write_dataarray(location='node', action='close')
  error = a_vtk_file%xml_writer%write_piece()
  error = a_vtk_file%finalize()
  endsubroutine image_data

  subroutine rectilinear_grid
  use vtk_fortran, only : vtk_file
  use penf,        only : I4P, I8P, R8P

  type(vtk_file)          :: a_vtk_file
  integer(I4P), parameter :: nx1=0, nx2=8, ny1=0, ny2=4, nz1=0, nz2=4
  integer(I4P), parameter :: nn=(nx2-nx1+1)*(ny2-ny1+1)*(nz2-nz1+1)
  real(R8P)               :: x(nx1:nx2), y(ny1:ny2), z(nz1:nz2)
  integer(I4P)            :: v(1:nn)   ! one value per point
  integer(I4P)            :: i, error

  x = [(0.5_R8P*i, i=nx1, nx2)] ; y = [(0.5_R8P*i, i=ny1, ny2)] ; z = [(0.5_R8P*i, i=nz1, nz2)]
  v = [(i, i=1, nn)]
  error = a_vtk_file%initialize(format='binary', filename='ref_rectilinear.vtr', &
                                mesh_topology='RectilinearGrid',                  &
                                nx1=nx1, nx2=nx2, ny1=ny1, ny2=ny2, nz1=nz1, nz2=nz2)
  ! optional global field data
  error = a_vtk_file%xml_writer%write_fielddata(action='open')
  error = a_vtk_file%xml_writer%write_fielddata(x=0._R8P, data_name='TIME')
  error = a_vtk_file%xml_writer%write_fielddata(x=1_I8P,  data_name='CYCLE')
  error = a_vtk_file%xml_writer%write_fielddata(action='close')

  error = a_vtk_file%xml_writer%write_piece(nx1=nx1, nx2=nx2, ny1=ny1, ny2=ny2, nz1=nz1, nz2=nz2)
  error = a_vtk_file%xml_writer%write_geo(x=x, y=y, z=z)
  error = a_vtk_file%xml_writer%write_dataarray(location='node', action='open')
  error = a_vtk_file%xml_writer%write_dataarray(data_name='node_value', x=v)
  error = a_vtk_file%xml_writer%write_dataarray(location='node', action='close')
  error = a_vtk_file%xml_writer%write_piece()
  error = a_vtk_file%finalize()
  endsubroutine rectilinear_grid

  subroutine structured_grid
  use vtk_fortran, only : vtk_file
  use penf,        only : I4P, R8P

  type(vtk_file)          :: a_vtk_file
  integer(I4P), parameter :: nx1=0, nx2=9, ny1=0, ny2=5, nz1=0, nz2=5
  integer(I4P), parameter :: nn=(nx2-nx1+1)*(ny2-ny1+1)*(nz2-nz1+1)
  real(R8P)               :: x(nx1:nx2,ny1:ny2,nz1:nz2)
  real(R8P)               :: y(nx1:nx2,ny1:ny2,nz1:nz2)
  real(R8P)               :: z(nx1:nx2,ny1:ny2,nz1:nz2)
  real(R8P)               :: v(nx1:nx2,ny1:ny2,nz1:nz2)
  integer(I4P)            :: i, j, k, error

  do k=nz1, nz2 ; do j=ny1, ny2 ; do i=nx1, nx2   ! a sheared box
    x(i,j,k) = i + 0.3_R8P*j ; y(i,j,k) = j ; z(i,j,k) = k ; v(i,j,k) = x(i,j,k)**2
  enddo ; enddo ; enddo
  error = a_vtk_file%initialize(format='binary', filename='ref_structured.vts', &
                                mesh_topology='StructuredGrid',                  &
                                nx1=nx1, nx2=nx2, ny1=ny1, ny2=ny2, nz1=nz1, nz2=nz2)
  error = a_vtk_file%xml_writer%write_piece(nx1=nx1, nx2=nx2, ny1=ny1, ny2=ny2, nz1=nz1, nz2=nz2)
  error = a_vtk_file%xml_writer%write_geo(n=nn, x=x, y=y, z=z)
  error = a_vtk_file%xml_writer%write_dataarray(location='node', action='open')
  error = a_vtk_file%xml_writer%write_dataarray(data_name='pressure', x=v, one_component=.true.)
  error = a_vtk_file%xml_writer%write_dataarray(location='node', action='close')
  error = a_vtk_file%xml_writer%write_piece()
  error = a_vtk_file%finalize()
  endsubroutine structured_grid

  subroutine unstructured_grid
  use vtk_fortran, only : vtk_file
  use penf,        only : I1P, I4P, R4P, R8P

  type(vtk_file)          :: a_vtk_file
  integer(I4P), parameter :: np = 9    ! number of points: a unit cube and the apex of a pyramid on top of it
  integer(I4P), parameter :: nc = 2    ! number of cells: a hexahedron and a pyramid
  real(R4P),    parameter :: x(np) = [0, 1, 1, 0, 0, 1, 1, 0, 0]
  real(R4P),    parameter :: y(np) = [0, 0, 1, 1, 0, 0, 1, 1, 0]
  real(R4P),    parameter :: z(np) = [0, 0, 0, 0, 1, 1, 1, 1, 2]
  integer(I4P), parameter :: connect(13)  = [0,1,2,3,4,5,6,7, 4,5,6,7,8]   ! the point ids of each cell, 0-based
  integer(I4P), parameter :: offset(nc)   = [8, 13]                        ! the end of each cell in connect
  integer(I1P), parameter :: cell_type(nc) = [12_I1P, 14_I1P]               ! VTK_HEXAHEDRON, VTK_PYRAMID
  real(R8P)               :: v(np)              ! scalar at the points
  real(R8P)               :: vx(np), vy(np), vz(np) ! vector at the points
  integer(I4P)            :: error

  v = real(z, R8P) ; vx = 0._R8P ; vy = 0._R8P ; vz = 1._R8P
  error = a_vtk_file%initialize(format='binary', filename='ref_unstructured.vtu', mesh_topology='UnstructuredGrid')
  error = a_vtk_file%xml_writer%write_piece(np=np, nc=nc)
  error = a_vtk_file%xml_writer%write_geo(np=np, nc=nc, x=x, y=y, z=z)
  error = a_vtk_file%xml_writer%write_connectivity(nc=nc, connectivity=connect, offset=offset, cell_type=cell_type)
  error = a_vtk_file%xml_writer%write_dataarray(location='node', action='open')
  error = a_vtk_file%xml_writer%write_dataarray(data_name='scalars', x=v)
  error = a_vtk_file%xml_writer%write_dataarray(data_name='vector', x=vx, y=vy, z=vz)
  error = a_vtk_file%xml_writer%write_dataarray(location='node', action='close')
  error = a_vtk_file%xml_writer%write_piece()
  error = a_vtk_file%finalize()
  endsubroutine unstructured_grid

  subroutine polyhedron
  use vtk_fortran, only : vtk_file
  use penf,        only : I1P, I4P, R8P
  type(vtk_file) :: a_vtk_file
  real(R8P)      :: x(12), y(12), z(12)
  integer(I4P)   :: error

  x = [0, 1, 1, 0, 0, 1, 1, 0, 2, 3, 2, 2] ; y = [0, 0, 1, 1, 0, 0, 1, 1, 0, 0, 1, 0] ; z = [0, 0, 0, 0, 1, 1, 1, 1, 0, 0, 0, 1]
  error = a_vtk_file%initialize(format='raw', filename='ref_polyhedron.vtu', mesh_topology='UnstructuredGrid')
  error = a_vtk_file%xml_writer%write_piece(np=12, nc=2)
  error = a_vtk_file%xml_writer%write_geo(np=12, nc=2, x=x, y=y, z=z)
  block
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
  endblock
  error = a_vtk_file%xml_writer%write_piece()
  error = a_vtk_file%finalize()
  endsubroutine polyhedron

  subroutine polydata
  use vtk_fortran, only : vtk_file
  use penf,        only : I4P, R8P

  type(vtk_file) :: a_vtk_file
  integer(I4P)   :: error
  real(R8P)      :: x(7), y(7), z(7)   ! a square (points 0-3) and a polyline (points 4-6)

  x = [0, 1, 1, 0, 0, 1, 2] ; y = [0, 0, 1, 1, 2, 3, 2] ; z = 0
  error = a_vtk_file%initialize(format='raw', filename='ref_polydata.vtp', mesh_topology='PolyData')
  error = a_vtk_file%xml_writer%write_piece(np=7, nverts=0, nlines=1, nstrips=0, npolys=2)
  error = a_vtk_file%xml_writer%write_geo(np=7, nc=3, x=x, y=y, z=z)
  error = a_vtk_file%xml_writer%write_polydata_cells(lines_connectivity=[4,5,6], lines_offset=[3],          &
                                                      polys_connectivity=[0,1,2, 0,2,3], polys_offset=[3,6])
  error = a_vtk_file%xml_writer%write_dataarray(location='cell', action='open')
  error = a_vtk_file%xml_writer%write_dataarray(data_name='id', x=[1,2,3])   ! the polyline, then the two triangles
  error = a_vtk_file%xml_writer%write_dataarray(location='cell', action='close')
  error = a_vtk_file%xml_writer%write_piece()
  error = a_vtk_file%finalize()
  endsubroutine polydata

  subroutine packed_coordinates
  !< The coordinates packed in one array, (3, points) or (3, i, j, k).
  use vtk_fortran, only : vtk_file
  use penf,        only : I1P, I4P, R8P
  type(vtk_file) :: a_vtk_file
  integer(I4P)   :: i, j, k, error
  real(R8P) :: xyz(3,0:2,0:1,0:1)   ! StructuredGrid: (x,y,z) of each point, the points in the order i, j, k
  real(R8P) :: pts(3,4)             ! UnstructuredGrid and PolyData: (x,y,z) of each point

  do k=0, 1 ; do j=0, 1 ; do i=0, 2
    xyz(:,i,j,k) = [real(i, R8P), real(j, R8P), real(k, R8P)]
  enddo ; enddo ; enddo
  pts = reshape([0,0,0, 1,0,0, 0,1,0, 0,0,1], [3,4])
  error = a_vtk_file%initialize(format='ascii', filename='ref_packed.vts', mesh_topology='StructuredGrid', &
                                nx1=0, nx2=2, ny1=0, ny2=1, nz1=0, nz2=1)
  error = a_vtk_file%xml_writer%write_piece(nx1=0, nx2=2, ny1=0, ny2=1, nz1=0, nz2=1)
  error = a_vtk_file%xml_writer%write_geo(xyz=xyz)
  error = a_vtk_file%xml_writer%write_piece()
  error = a_vtk_file%finalize()
  error = a_vtk_file%initialize(format='ascii', filename='ref_packed.vtu', mesh_topology='UnstructuredGrid')
  error = a_vtk_file%xml_writer%write_piece(np=4, nc=1)
  error = a_vtk_file%xml_writer%write_geo(np=4, nc=1, xyz=pts)
  error = a_vtk_file%xml_writer%write_connectivity(nc=1, connectivity=[0,1,2,3], offset=[4], cell_type=[10_I1P])
  error = a_vtk_file%xml_writer%write_piece()
  error = a_vtk_file%finalize()
  endsubroutine packed_coordinates

  subroutine several_pieces
  use vtk_fortran, only : vtk_file
  use penf,        only : I1P, I4P, R8P
  type(vtk_file)          :: a_vtk_file
  integer(I4P), parameter :: nblocks=2, np(nblocks)=[4, 4], nc(nblocks)=[1, 1]
  real(R8P)               :: x(4), y(4), z(4), p(4)
  integer(I4P)            :: b, error

  y = [0, 0, 1, 0] ; z = [0, 0, 0, 1]
  error = a_vtk_file%initialize(format='raw', filename='ref_blocks.vtu', mesh_topology='UnstructuredGrid')
  do b=1, nblocks
    x = [0, 1, 0, 0] + 2._R8P*b ; p = b   ! the points and the data of block b
    error = a_vtk_file%xml_writer%write_piece(np=np(b), nc=nc(b))
    error = a_vtk_file%xml_writer%write_geo(np=np(b), nc=nc(b), x=x, y=y, z=z)
    error = a_vtk_file%xml_writer%write_connectivity(nc=nc(b), connectivity=[0,1,2,3], offset=[4], cell_type=[10_I1P])
    error = a_vtk_file%xml_writer%write_dataarray(location='node', action='open')
    error = a_vtk_file%xml_writer%write_dataarray(data_name='pressure', x=p)
    error = a_vtk_file%xml_writer%write_dataarray(location='node', action='close')
    error = a_vtk_file%xml_writer%write_piece()
  enddo
  error = a_vtk_file%finalize()
  endsubroutine several_pieces

  subroutine check(files)
  !< Read every file back: the program stops with an error if one cannot be read.
  character(*), intent(in)      :: files(:)
  type(vtk_file)                :: a_vtk_file
  character(len=:), allocatable :: topology
  integer(I4P)                  :: f, npieces, error

  do f=1, size(files)
    error = a_vtk_file%initialize(filename=trim(files(f)), action='read')
    if (error == 0) error = a_vtk_file%xml_reader%get_info(mesh_topology=topology, npieces=npieces)
    if (error /= 0) then
      print '(A,I0)', trim(files(f))//': read error ', error
      error stop
    endif
    print '(A,I0,A)', trim(files(f))//': '//topology//', ', npieces, ' piece(s)'
    error = a_vtk_file%finalize()
  enddo
  endsubroutine check
endprogram ref_topologies
