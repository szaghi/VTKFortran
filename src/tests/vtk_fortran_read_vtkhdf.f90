!< VTK_Fortran test: read VTKHDF files back (`vtk_file%initialize(filename, action='read')` on a `.vtkhdf`).
program vtk_fortran_read_vtkhdf
!< VTK_Fortran test: read VTKHDF files back (`vtk_file%initialize(filename, action='read')` on a `.vtkhdf`).
!<
!< With HDF5 the test writes VTKHDF files with the writer of the library, then reads them through `vtk_file%xml_reader`,
!< as an XML file is read, and compares what it reads with what was written: an ImageData (extent, origin, spacing, the
!< names, types and values of its arrays, field data), a RectilinearGrid and a StructuredGrid (geometry), an
!< UnstructuredGrid of two pieces with a polyhedron (counts, points, cells, faces, cell data of each piece), a PolyData
!< (its blocks), a time series of an UnstructuredGrid (two parts, then one) and of an ImageData (steps, times, the data
!< of each step), I8P ids read as I4P; the error codes; two polyhedra sharing a face (VTKHDF 2.9: the face stored once,
!< referenced reversed by the second polyhedron), the file built with the HDF5 bindings, read back as the face stream of
!< each polyhedron, and a face id out of range (error 6); time series storing once what does not change (an
!< UnstructuredGrid whose mesh is reused by steps without pieces, an ImageData whose cell data are reused), each step
!< read as written. Without HDF5 the reader refuses a VTKHDF file.
use penf
use vtk_fortran, only : vtk_file
use vtk_fortran_hdf5

implicit none
type(vtk_file)                :: w, r                    !< Writer, reader.
logical                       :: ok(11)                  !< Checks.
integer(I4P)                  :: e, i                    !< Error, counter.
integer(I4P), parameter       :: cube(31) = [6, 4, 0, 3, 2, 1,  4, 4, 5, 6, 7,  4, 0, 1, 5, 4,  4, 1, 2, 6, 5, &
                                             4, 2, 3, 7, 6,  4, 3, 0, 4, 7] !< Faces of a unit cube.

ok = .false.
if (.not.is_hdf5_enabled) then
  open(newunit=i, file='not_read.vtkhdf', access='stream', form='unformatted', status='replace')
  write(i) achar(137)//'HDF'//achar(13)//achar(10)//achar(26)//achar(10)
  close(i)
  ok = r%initialize(filename='not_read.vtkhdf', action='read') /= 0
  print '(A)', 'HDF5 not enabled: a VTKHDF file is not read'
else
  ok(1) = image()
  ok(2) = rect()
  ok(3) = strg()
  ok(4) = unst()
  ok(5) = poly()
  ok(6) = series()
  ok(7) = image_series()
  ok(8) = ids_i4p()
  ok(9) = errors()
  ok(10) = shared_faces()
  ok(11) = reused()
  print '(A,11L2)', 'VTKHDF read: image, rect, strg, unst, poly, series, image series, I4P, errors, shared faces, reused:', ok
endif

print "(A,L1)", new_line('a')//'Are all tests passed? ', all(ok)
if (.not.all(ok)) error stop 'some tests failed'
stop
contains
  logical function image()
  !< A 3D ImageData: extent, origin, spacing; point data (R8P, a R4P vector), cell data (I4P), field data.
  real(R8P)                     :: pd(0:3,0:2,0:1), origin(3), spacing(3)
  real(R4P)                     :: pv(3,24)
  real(R8P), allocatable        :: x(:)
  real(R4P), allocatable        :: v(:,:)
  integer(I4P), allocatable     :: c(:)
  character(len=:), allocatable :: names(:), dtype, topology
  integer(I4P)                  :: ext(6), nc, np
  integer(I8P)                  :: nt

  pd = reshape([(real(i, R8P), i=1, 24)], [4, 3, 2])
  pv = reshape([(real(i, R4P), i=1, 72)], [3, 24])
  e = w%initialize(format='vtkhdf', filename='read_image.vtkhdf', mesh_topology='ImageData', &
                   nx1=0, nx2=3, ny1=0, ny2=2, nz1=0, nz2=1, origin=[1._R8P, 2._R8P, 3._R8P], spacing=[0.5_R8P, 1._R8P, 2._R8P])
  e = w%xml_writer%write_fielddata(action='open')
  e = w%xml_writer%write_fielddata(data_name='time', x=7.5_R8P)
  e = w%xml_writer%write_fielddata(action='close')
  e = w%xml_writer%write_piece(nx1=0, nx2=3, ny1=0, ny2=2, nz1=0, nz2=1)
  e = w%xml_writer%write_dataarray(location='node', action='open')
  e = w%xml_writer%write_dataarray(data_name='pd', x=pd, one_component=.true.)
  e = w%xml_writer%write_dataarray(data_name='pv', x=pv)
  e = w%xml_writer%write_dataarray(location='node', action='close')
  e = w%xml_writer%write_dataarray(location='cell', action='open')
  e = w%xml_writer%write_dataarray(data_name='cid', x=[(i, i=1, 6)])
  e = w%xml_writer%write_dataarray(location='cell', action='close')
  e = w%xml_writer%write_piece()
  e = w%finalize()
  image = r%initialize(filename='read_image.vtkhdf', action='read') == 0
  e = r%xml_reader%get_info(mesh_topology=topology, npieces=np, nx1=ext(1), nx2=ext(2), ny1=ext(3), ny2=ext(4), &
                            nz1=ext(5), nz2=ext(6), origin=origin, spacing=spacing)
  image = image .and. e == 0 .and. topology == 'ImageData' .and. np == 1 .and. all(ext == [0, 3, 0, 2, 0, 1]) .and. &
          all(origin == [1._R8P, 2._R8P, 3._R8P]) .and. all(spacing == [0.5_R8P, 1._R8P, 2._R8P])
  e = r%xml_reader%get_dataarray_names(location='node', names=names)
  image = image .and. e == 0 .and. size(names) == 2
  if (image) image = trim(names(1)) == 'pd' .and. trim(names(2)) == 'pv'
  e = r%xml_reader%get_dataarray_info(location='node', data_name='pv', data_type=dtype, n_components=nc, n_tuples=nt)
  image = image .and. e == 0 .and. dtype == 'Float32' .and. nc == 3 .and. nt == 24_I8P
  e = r%xml_reader%read_dataarray(location='node', data_name='pd', x=x)
  image = image .and. e == 0 .and. all(x == reshape(pd, [24]))
  e = r%xml_reader%read_dataarray(location='node', data_name='pv', x=v)
  image = image .and. e == 0 .and. all(shape(v) == [3, 24])
  if (image) image = all(v == pv)
  e = r%xml_reader%read_dataarray(location='cell', data_name='cid', x=c)
  image = image .and. e == 0 .and. all(c == [(i, i=1, 6)])
  e = r%xml_reader%read_dataarray(location='field', data_name='time', x=x)
  image = image .and. e == 0 .and. all(x == [7.5_R8P])
  e = r%finalize()
  if (.not.image) print '(A)', 'image: different'
  endfunction image

  logical function rect()
  !< A RectilinearGrid: its coordinates.
  real(R8P), allocatable :: x(:), y(:), z(:)

  e = w%initialize(format='vtkhdf', filename='read_rect.vtkhdf', mesh_topology='RectilinearGrid', &
                   nx1=0, nx2=3, ny1=0, ny2=1, nz1=0, nz2=0)
  e = w%xml_writer%write_piece(nx1=0, nx2=3, ny1=0, ny2=1, nz1=0, nz2=0)
  e = w%xml_writer%write_geo(x=[0._R8P, 1._R8P, 3._R8P, 6._R8P], y=[0._R8P, 2._R8P], z=[5._R8P])
  e = w%xml_writer%write_piece()
  e = w%finalize()
  rect = r%initialize(filename='read_rect.vtkhdf', action='read') == 0
  e = r%xml_reader%read_geo(x=x, y=y, z=z)
  rect = rect .and. e == 0 .and. all(x == [0._R8P, 1._R8P, 3._R8P, 6._R8P]) .and. all(y == [0._R8P, 2._R8P]) .and. &
         all(z == [5._R8P])
  e = r%finalize()
  if (.not.rect) print '(A)', 'rect: different'
  endfunction rect

  logical function strg()
  !< A 2D StructuredGrid: its points (R4P read as R8P).
  real(R4P)              :: px(6), py(6), pz(6)
  real(R8P), allocatable :: x(:), y(:), z(:)

  px = [0., 1., 2., 0.5, 1.5, 2.5] ; py = [0., 0., 0., 1., 1., 1.] ; pz = 0.
  e = w%initialize(format='vtkhdf', filename='read_strg.vtkhdf', mesh_topology='StructuredGrid', &
                   nx1=0, nx2=2, ny1=0, ny2=1, nz1=0, nz2=0)
  e = w%xml_writer%write_piece(nx1=0, nx2=2, ny1=0, ny2=1, nz1=0, nz2=0)
  e = w%xml_writer%write_geo(n=6, x=px, y=py, z=pz)
  e = w%xml_writer%write_piece()
  e = w%finalize()
  strg = r%initialize(filename='read_strg.vtkhdf', action='read') == 0
  e = r%xml_reader%read_geo(x=x, y=y, z=z)
  strg = strg .and. e == 0 .and. all(x == real(px, R8P)) .and. all(y == real(py, R8P)) .and. all(z == real(pz, R8P))
  e = r%finalize()
  if (.not.strg) print '(A)', 'strg: different'
  endfunction strg

  logical function unst()
  !< An UnstructuredGrid of two pieces: a tetrahedron; a cube as a polyhedron and a tetrahedron.
  real(R8P)                 :: x(12), y(12), z(12)
  real(R8P), allocatable    :: rx(:), ry(:), rz(:)
  integer(I4P), allocatable :: connect(:), offset(:), face(:), faceoffset(:), c(:)
  integer(I1P), allocatable :: types(:)
  integer(I8P)              :: np, nc
  integer(I4P)              :: npieces

  x = [0, 1, 1, 0, 0, 1, 1, 0,  2, 3, 2, 2] ; y = [0, 0, 1, 1, 0, 0, 1, 1,  0, 0, 1, 0]
  z = [0, 0, 0, 0, 1, 1, 1, 1,  0, 0, 0, 1]
  e = w%initialize(format='vtkhdf', filename='read_unst.vtkhdf', mesh_topology='UnstructuredGrid')
  e = w%xml_writer%write_piece(np=4, nc=1)
  e = w%xml_writer%write_geo(np=4, nc=1, x=x(9:12), y=y(9:12), z=z(9:12))
  e = w%xml_writer%write_connectivity(nc=1, connectivity=[0, 1, 2, 3], offset=[4], cell_type=[10_I1P])
  e = cell_data([1])
  e = w%xml_writer%write_piece(np=12, nc=2)
  e = w%xml_writer%write_geo(np=12, nc=2, x=x, y=y, z=z)
  e = w%xml_writer%write_connectivity(nc=2, connectivity=[(i, i=0, 11)], offset=[8, 12], cell_type=[42_I1P, 10_I1P], &
                                      face=cube, faceoffset=[31, -1])
  e = cell_data([2, 3])
  e = w%finalize()
  unst = r%initialize(filename='read_unst.vtkhdf', action='read') == 0
  e = r%xml_reader%get_info(npieces=npieces)
  unst = unst .and. npieces == 2
  e = r%xml_reader%read_piece(piece=2, np=np, nc=nc)
  unst = unst .and. e == 0 .and. np == 12_I8P .and. nc == 2_I8P
  e = r%xml_reader%read_geo(x=rx, y=ry, z=rz, piece=2)
  unst = unst .and. e == 0 .and. all(rx == x) .and. all(ry == y) .and. all(rz == z)
  e = r%xml_reader%read_connectivity(connectivity=connect, offset=offset, cell_type=types, face=face, &
                                     faceoffset=faceoffset, piece=2)
  unst = unst .and. e == 0 .and. all(connect == [(i, i=0, 11)]) .and. all(offset == [8, 12]) .and. &
         all(types == [42_I1P, 10_I1P])
  if (unst) unst = all(face == cube) .and. all(faceoffset == [31, -1])
  e = r%xml_reader%read_connectivity(connectivity=connect, offset=offset, piece=1)
  unst = unst .and. e == 0 .and. all(connect == [0, 1, 2, 3]) .and. all(offset == [4])
  e = r%xml_reader%read_dataarray(location='cell', data_name='cid', x=c, piece=2)
  unst = unst .and. e == 0 .and. all(c == [2, 3])
  e = r%finalize()
  if (.not.unst) print '(A)', 'unst: different'
  endfunction unst

  logical function poly()
  !< A PolyData: a vertex, a polyline and a triangle.
  integer(I4P), allocatable :: connect(:), offset(:)
  integer(I8P)              :: nverts, nlines, nstrips, npolys

  e = w%initialize(format='vtkhdf', filename='read_poly.vtkhdf', mesh_topology='PolyData')
  e = w%xml_writer%write_piece(np=6, nverts=1, nlines=1, nstrips=0, npolys=1)
  e = w%xml_writer%write_geo(np=6, nc=3, x=[(real(i, R8P), i=0, 5)], y=[(0._R8P, i=0, 5)], z=[(0._R8P, i=0, 5)])
  e = w%xml_writer%write_polydata_cells(verts_connectivity=[0], verts_offset=[1], lines_connectivity=[1, 2, 3], &
                                        lines_offset=[3], polys_connectivity=[3, 4, 5], polys_offset=[3])
  e = w%xml_writer%write_piece()
  e = w%finalize()
  poly = r%initialize(filename='read_poly.vtkhdf', action='read') == 0
  e = r%xml_reader%read_piece(nverts=nverts, nlines=nlines, nstrips=nstrips, npolys=npolys)
  poly = poly .and. e == 0 .and. all([nverts, nlines, nstrips, npolys] == [1_I8P, 1_I8P, 0_I8P, 1_I8P])
  e = r%xml_reader%read_polydata_cells(block='lines', connectivity=connect, offset=offset)
  poly = poly .and. e == 0 .and. all(connect == [1, 2, 3]) .and. all(offset == [3])
  e = r%xml_reader%read_polydata_cells(block='strips', connectivity=connect, offset=offset)
  poly = poly .and. e == 0 .and. size(connect) == 0 .and. size(offset) == 0
  e = r%xml_reader%read_polydata_cells(block='polys', connectivity=connect, offset=offset)
  poly = poly .and. e == 0 .and. all(connect == [3, 4, 5]) .and. all(offset == [3])
  e = r%finalize()
  if (.not.poly) print '(A)', 'poly: different'
  endfunction poly

  logical function series()
  !< A time series of an UnstructuredGrid: two parts (4 and 8 points) at step 1, one (5 points) at step 2.
  real(R8P), allocatable    :: times(:), rx(:), ry(:), rz(:), pd(:), t(:)
  integer(I4P), allocatable :: connect(:), offset(:)
  integer(I4P)              :: npieces
  integer(I8P)              :: np

  e = w%initialize(format='vtkhdf', filename='read_series.vtkhdf', mesh_topology='UnstructuredGrid')
  e = w%xml_writer%write_step(time=0._R8P)
  e = field_data(0._R8P)
  e = tetra(4, 0._R8P) ; e = tetra(8, 10._R8P)
  e = w%xml_writer%write_step(time=0.5_R8P)
  e = field_data(0.5_R8P)
  e = tetra(5, 20._R8P)
  e = w%finalize()
  series = r%initialize(filename='read_series.vtkhdf', action='read') == 0
  e = r%xml_reader%get_steps(times=times)
  series = series .and. e == 0 .and. all(times == [0._R8P, 0.5_R8P])
  e = r%xml_reader%get_info(npieces=npieces)
  series = series .and. npieces == 2
  e = r%xml_reader%read_piece(piece=2, np=np)
  series = series .and. e == 0 .and. np == 8_I8P
  e = r%xml_reader%read_dataarray(location='node', data_name='pd', x=pd, piece=2)
  series = series .and. e == 0 .and. all(pd == [(10._R8P + i, i=1, 8)])
  e = r%xml_reader%set_step(step=2)
  e = r%xml_reader%get_info(npieces=npieces)
  series = series .and. e == 0 .and. npieces == 1
  e = r%xml_reader%read_geo(x=rx, y=ry, z=rz)
  series = series .and. e == 0 .and. size(rx) == 5
  if (series) series = all(rx == [(20._R8P + i, i=0, 4)])
  e = r%xml_reader%read_connectivity(connectivity=connect, offset=offset)
  series = series .and. e == 0 .and. all(connect == [0, 1, 2, 3]) .and. all(offset == [4])
  e = r%xml_reader%read_dataarray(location='node', data_name='pd', x=pd)
  series = series .and. e == 0 .and. all(pd == [(20._R8P + i, i=1, 5)])
  e = r%xml_reader%read_dataarray(location='field', data_name='TIME', x=t)
  series = series .and. e == 0 .and. all(t == [0.5_R8P])
  e = r%finalize()
  if (.not.series) print '(A)', 'series: different'
  endfunction series

  logical function image_series()
  !< A time series of a 2D ImageData: the point data of each step.
  real(R8P), allocatable :: pd(:), times(:)
  integer(I4P)           :: s

  e = w%initialize(format='vtkhdf', filename='read_image_series.vtkhdf', mesh_topology='ImageData', &
                   nx1=0, nx2=2, ny1=0, ny2=1, nz1=0, nz2=0, origin=[0._R8P, 0._R8P, 0._R8P], spacing=[1._R8P, 1._R8P, 1._R8P])
  do s=1, 3
    e = w%xml_writer%write_step(time=real(s, R8P))
    e = w%xml_writer%write_piece(nx1=0, nx2=2, ny1=0, ny2=1, nz1=0, nz2=0)
    e = w%xml_writer%write_dataarray(location='node', action='open')
    e = w%xml_writer%write_dataarray(data_name='pd', x=[(real(10 * s + i, R8P), i=1, 6)])
    e = w%xml_writer%write_dataarray(location='node', action='close')
    e = w%xml_writer%write_piece()
  enddo
  e = w%finalize()
  image_series = r%initialize(filename='read_image_series.vtkhdf', action='read') == 0
  e = r%xml_reader%get_steps(times=times)
  image_series = image_series .and. e == 0 .and. size(times) == 3
  do s=1, 3
    e = r%xml_reader%set_step(step=s)
    e = max(e, abs(r%xml_reader%read_dataarray(location='node', data_name='pd', x=pd)))
    image_series = image_series .and. e == 0 .and. size(pd) == 6
    if (image_series) image_series = all(pd == [(real(10 * s + i, R8P), i=1, 6)])
  enddo
  e = r%finalize()
  if (.not.image_series) print '(A)', 'image series: different'
  endfunction image_series

  logical function ids_i4p()
  !< I8P ids and counts, read as I4P.
  integer(I4P), allocatable :: connect(:), offset(:)

  e = w%initialize(format='vtkhdf', filename='read_ids.vtkhdf', mesh_topology='UnstructuredGrid')
  e = w%xml_writer%write_piece(np=4_I8P, nc=1_I8P)
  e = w%xml_writer%write_geo(np=4_I8P, nc=1_I8P, x=[0._R8P, 1._R8P, 0._R8P, 0._R8P], y=[0._R8P, 0._R8P, 1._R8P, 0._R8P], &
                             z=[0._R8P, 0._R8P, 0._R8P, 1._R8P])
  e = w%xml_writer%write_connectivity(nc=1_I8P, connectivity=[0_I8P, 1_I8P, 2_I8P, 3_I8P], offset=[4_I8P], &
                                      cell_type=[10_I1P])
  e = w%xml_writer%write_piece()
  e = w%finalize()
  ids_i4p = r%initialize(filename='read_ids.vtkhdf', action='read') == 0
  e = r%xml_reader%read_connectivity(connectivity=connect, offset=offset)
  ids_i4p = ids_i4p .and. e == 0 .and. all(connect == [0, 1, 2, 3]) .and. all(offset == [4])
  e = r%finalize()
  if (.not.ids_i4p) print '(A)', 'I4P ids: different'
  endfunction ids_i4p

  logical function errors()
  !< The error codes: a missing array 4, a step not in the file 4, the geometry of an ImageData 4, the sources 3, a
  !< piece not in the step 4.
  real(R8P), allocatable              :: x(:), y(:), z(:)
  character(len=:), allocatable       :: sources(:)
  logical                             :: is(5)

  is = .false.
  e = r%initialize(filename='read_image.vtkhdf', action='read')
  is(1) = r%xml_reader%read_dataarray(location='node', data_name='missing', x=x) == 4
  is(2) = r%xml_reader%set_step(step=2) == 4
  is(3) = r%xml_reader%read_geo(x=x, y=y, z=z) == 4
  is(4) = r%xml_reader%get_sources(sources=sources) == 3
  e = r%finalize()
  e = r%initialize(filename='read_unst.vtkhdf', action='read')
  is(5) = r%xml_reader%read_geo(x=x, y=y, z=z, piece=3) == 4
  e = r%finalize()
  errors = all(is)
  if (.not.errors) print '(A,5L2)', 'errors: ', is
  endfunction errors

  logical function shared_faces()
  !< Two hexahedra as polyhedra sharing the face x=1 (VTKHDF 2.9): 11 faces, the shared one (5) stored once, oriented
  !< outwards for the first cell, referenced as -(5+1) by the second; read back, each polyhedron has its own copy of the
  !< face, reversed in the second. A copy of the file with a face id out of range is error 6.
  integer(I4P), parameter   :: stream(62) = [6, 4, 0, 3, 9, 6,  4, 0, 6, 7, 1,  4, 3, 4, 10, 9,  4, 0, 1, 4, 3, &
                                             4, 6, 9, 10, 7,  4, 1, 4, 10, 7, &
                                             6, 4, 7, 10, 4, 1,  4, 2, 8, 11, 5,  4, 1, 7, 8, 2,  4, 4, 5, 11, 10, &
                                             4, 1, 2, 5, 4,  4, 7, 10, 11, 8] !< Expected face stream.
  integer(I4P), allocatable :: connect(:), offset(:), face(:), faceoffset(:)
  integer(I1P), allocatable :: types(:)

  shared_faces = polyhedra_file('read_shared_faces.vtkhdf', [0_I8P, 1_I8P, 2_I8P, 3_I8P, 4_I8P, 5_I8P, &
                                                             -6_I8P, 6_I8P, 7_I8P, 8_I8P, 9_I8P, 10_I8P])
  shared_faces = shared_faces .and. polyhedra_file('read_shared_faces_bad.vtkhdf', [0_I8P, 1_I8P, 2_I8P, 3_I8P, 4_I8P, &
                                                   5_I8P, -6_I8P, 6_I8P, 7_I8P, 8_I8P, 9_I8P, 11_I8P])
  shared_faces = shared_faces .and. r%initialize(filename='read_shared_faces.vtkhdf', action='read') == 0
  e = r%xml_reader%read_connectivity(connectivity=connect, offset=offset, cell_type=types, face=face, faceoffset=faceoffset)
  shared_faces = shared_faces .and. e == 0 .and. all(offset == [8, 16]) .and. all(types == [42_I1P, 42_I1P])
  if (shared_faces) shared_faces = size(face) == 62 .and. all(faceoffset == [31, 62])
  if (shared_faces) shared_faces = all(face == stream)
  e = r%finalize()
  shared_faces = shared_faces .and. r%initialize(filename='read_shared_faces_bad.vtkhdf', action='read') == 0
  e = r%xml_reader%read_connectivity(connectivity=connect, offset=offset, face=face, faceoffset=faceoffset)
  shared_faces = shared_faces .and. e == 6
  e = r%finalize()
  if (.not.shared_faces) print '(A)', 'shared faces: different'
  endfunction shared_faces

  logical function polyhedra_file(filename, faces_of) result(ok)
  !< The file of the two hexahedra, `faces_of` their faces (PolyhedronToFaces), written with the HDF5 bindings.
  character(*), intent(in)  :: filename    !< File name.
  integer(I8P), intent(in)  :: faces_of(:) !< Faces of the polyhedra.
  integer(I8P), parameter   :: fc(44) = [0, 3, 9, 6,  0, 6, 7, 1,  3, 4, 10, 9,  0, 1, 4, 3,  6, 9, 10, 7, &
                                         1, 4, 10, 7, &
                                         2, 8, 11, 5,  1, 7, 8, 2,  4, 5, 11, 10,  1, 2, 5, 4,  7, 10, 11, 8] !< Faces.
  integer(hid_kind)         :: file_id, root
  real(R8P)                 :: points(3,0:11)
  integer(I4P)              :: p

  do p=0, 11 ! x = mod(p, 3), y = mod(p / 3, 2), z = p / 6
    points(:,p) = [real(mod(p, 3), R8P), real(mod(p / 3, 2), R8P), real(p / 6, R8P)]
  enddo
  ok = h5_file_create(filename, file_id) == 0
  ok = ok .and. h5_group_create(file_id, 'VTKHDF', root) == 0
  if (.not.ok) return
  e = h5_attribute_write(root, 'Version', [2_I8P, 9_I8P])
  e = max(e, h5_attribute_write(root, 'Type', 'UnstructuredGrid'))
  e = max(e, h5_dataset_write_I8P(root, 'NumberOfPoints', [12_I8P], [1_I8P]))
  e = max(e, h5_dataset_write_I8P(root, 'NumberOfCells', [2_I8P], [1_I8P]))
  e = max(e, h5_dataset_write_I8P(root, 'NumberOfConnectivityIds', [16_I8P], [1_I8P]))
  e = max(e, h5_dataset_write_R8P(root, 'Points', points, [3_I8P, 12_I8P]))
  e = max(e, h5_dataset_write_I8P(root, 'Connectivity', [0_I8P, 1_I8P, 3_I8P, 4_I8P, 6_I8P, 7_I8P, 9_I8P, 10_I8P, &
                                                         1_I8P, 2_I8P, 4_I8P, 5_I8P, 7_I8P, 8_I8P, 10_I8P, 11_I8P], [16_I8P]))
  e = max(e, h5_dataset_write_I8P(root, 'Offsets', [0_I8P, 8_I8P, 16_I8P], [3_I8P]))
  e = max(e, h5_dataset_write_I1P(root, 'Types', [42_I1P, 42_I1P], [2_I8P], unsigned=.true.))
  e = max(e, h5_dataset_write_I8P(root, 'NumberOfFaces', [11_I8P], [1_I8P]))
  e = max(e, h5_dataset_write_I8P(root, 'NumberOfFaceConnectivityIds', [44_I8P], [1_I8P]))
  e = max(e, h5_dataset_write_I8P(root, 'NumberOfPolyhedronToFaceIds', [12_I8P], [1_I8P]))
  e = max(e, h5_dataset_write_I8P(root, 'FaceConnectivity', fc, [44_I8P]))
  e = max(e, h5_dataset_write_I8P(root, 'FaceOffsets', [(4_I8P * p, p=0, 11)], [12_I8P]))
  e = max(e, h5_dataset_write_I8P(root, 'PolyhedronToFaces', faces_of, [12_I8P]))
  e = max(e, h5_dataset_write_I8P(root, 'PolyhedronOffsets', [0_I8P, 6_I8P, 12_I8P], [3_I8P]))
  ok = e == 0 .and. h5_group_close(root) == 0 .and. h5_file_close(file_id) == 0
  endfunction polyhedra_file

  logical function reused()
  !< Series that store once what does not change, read step by step: an UnstructuredGrid (a tetrahedron of 4 points at
  !< step 1; step 2 without pieces, new point data; step 3 without pieces nor arrays), an ImageData (step 2: point data
  !< only, the cell data of step 1 reused).
  real(R8P), allocatable    :: rx(:), ry(:), rz(:), pd(:)
  integer(I4P), allocatable :: connect(:), offset(:), c(:)
  integer(I4P)              :: s

  e = w%initialize(format='vtkhdf', filename='read_reused.vtkhdf', mesh_topology='UnstructuredGrid')
  e = w%xml_writer%write_step(time=0._R8P)
  e = max(e, tetra(4, 0._R8P))
  e = max(e, w%xml_writer%write_step(time=1._R8P))
  e = max(e, w%xml_writer%write_dataarray(location='node', action='open'))
  e = max(e, w%xml_writer%write_dataarray(data_name='pd', x=[(30._R8P + i, i=1, 4)]))
  e = max(e, w%xml_writer%write_dataarray(location='node', action='close'))
  e = max(e, w%xml_writer%write_step(time=2._R8P))
  e = max(e, w%finalize())
  reused = e == 0 .and. r%initialize(filename='read_reused.vtkhdf', action='read') == 0
  do s=1, 3
    e = r%xml_reader%set_step(step=s)
    e = max(e, abs(r%xml_reader%read_geo(x=rx, y=ry, z=rz)))
    e = max(e, abs(r%xml_reader%read_connectivity(connectivity=connect, offset=offset)))
    e = max(e, abs(r%xml_reader%read_dataarray(location='node', data_name='pd', x=pd)))
    reused = reused .and. e == 0 .and. size(rx) == 4 .and. size(pd) == 4
    if (reused) reused = all(rx == [(real(i, R8P), i=0, 3)]) .and. all(connect == [0, 1, 2, 3]) .and. all(offset == [4])
    if (reused .and. s == 1) reused = all(pd == [(real(i, R8P), i=1, 4)])
    if (reused .and. s > 1) reused = all(pd == [(30._R8P + i, i=1, 4)])
  enddo
  e = r%finalize()
  e = w%initialize(format='vtkhdf', filename='read_reused_image.vtkhdf', mesh_topology='ImageData', &
                   nx1=0, nx2=2, ny1=0, ny2=1, nz1=0, nz2=0, origin=[0._R8P, 0._R8P, 0._R8P], spacing=[1._R8P, 1._R8P, 1._R8P])
  do s=1, 2
    e = max(e, w%xml_writer%write_step(time=real(s, R8P)))
    e = max(e, w%xml_writer%write_piece(nx1=0, nx2=2, ny1=0, ny2=1, nz1=0, nz2=0))
    e = max(e, w%xml_writer%write_dataarray(location='node', action='open'))
    e = max(e, w%xml_writer%write_dataarray(data_name='pd', x=[(real(10 * s + i, R8P), i=1, 6)]))
    e = max(e, w%xml_writer%write_dataarray(location='node', action='close'))
    if (s == 1) then
      e = max(e, w%xml_writer%write_dataarray(location='cell', action='open'))
      e = max(e, w%xml_writer%write_dataarray(data_name='cd', x=[7, 8]))
      e = max(e, w%xml_writer%write_dataarray(location='cell', action='close'))
    endif
    e = max(e, w%xml_writer%write_piece())
  enddo
  e = max(e, w%finalize())
  reused = reused .and. e == 0 .and. r%initialize(filename='read_reused_image.vtkhdf', action='read') == 0
  e = r%xml_reader%set_step(step=2)
  e = max(e, abs(r%xml_reader%read_dataarray(location='node', data_name='pd', x=pd)))
  e = max(e, abs(r%xml_reader%read_dataarray(location='cell', data_name='cd', x=c)))
  reused = reused .and. e == 0 .and. size(pd) == 6 .and. size(c) == 2
  if (reused) reused = all(pd == [(real(20 + i, R8P), i=1, 6)]) .and. all(c == [7, 8])
  e = r%finalize()
  if (.not.reused) print '(A)', 'reused: different'
  endfunction reused

  function cell_data(cid) result(error)
  !< The cell data cid of a piece, then the end of the piece.
  integer(I4P), intent(in) :: cid(:)
  integer(I4P)             :: error

  error = w%xml_writer%write_dataarray(location='cell', action='open')
  error = max(error, abs(w%xml_writer%write_dataarray(data_name='cid', x=cid)))
  error = max(error, abs(w%xml_writer%write_dataarray(location='cell', action='close')))
  error = max(error, abs(w%xml_writer%write_piece()))
  endfunction cell_data

  function field_data(time) result(error)
  !< The field data of a step: its time.
  real(R8P), intent(in) :: time
  integer(I4P)          :: error

  error = w%xml_writer%write_fielddata(action='open')
  error = max(error, abs(w%xml_writer%write_fielddata(data_name='TIME', x=time)))
  error = max(error, abs(w%xml_writer%write_fielddata(action='close')))
  endfunction field_data

  function tetra(np, shift) result(error)
  !< A piece of np points along x from shift, a tetrahedron on the first four, point data pd = shift + point number.
  integer(I4P), intent(in) :: np
  real(R8P),    intent(in) :: shift
  integer(I4P)             :: error

  error = w%xml_writer%write_piece(np=np, nc=1)
  error = max(error, abs(w%xml_writer%write_geo(np=np, nc=1, x=[(shift + i, i=0, np-1)], y=[(real(mod(i, 2), R8P), i=0, np-1)], &
                                                z=[(real(i / 2, R8P), i=0, np-1)])))
  error = max(error, abs(w%xml_writer%write_connectivity(nc=1, connectivity=[0, 1, 2, 3], offset=[4], cell_type=[10_I1P])))
  error = max(error, abs(w%xml_writer%write_dataarray(location='node', action='open')))
  error = max(error, abs(w%xml_writer%write_dataarray(data_name='pd', x=[(shift + i, i=1, np)])))
  error = max(error, abs(w%xml_writer%write_dataarray(location='node', action='close')))
  error = max(error, abs(w%xml_writer%write_piece()))
  endfunction tetra
endprogram vtk_fortran_read_vtkhdf
