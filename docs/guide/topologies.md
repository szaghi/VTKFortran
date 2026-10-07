---
title: Topologies
---

# Topologies

Write each kind of VTK XML dataset: the serial topologies, several pieces in one file. Complete programs: the
[cookbook](/manual/cookbook#write-each-kind-of-dataset) and the [tutorial](/manual/tutorial/01-first-file).

All examples use `use vtk_fortran` and `type(vtk_file)`. The general workflow for writing a VTK XML file is:

1. **Initialize** — open the file and select the format and mesh topology
2. **Write field data** *(optional)* — attach global metadata (time, cycle, etc.)
3. **Open a piece** — declare the extent or node/cell counts for the current piece
4. **Write geometry** — coordinates (and connectivity for unstructured grids)
5. **Write data arrays** — node-centered or cell-centered variables
6. **Close the piece**
7. **Finalize** — flush and close the file

All procedures return an integer error code. Zero means success. To read a file back, see [Reading files](/guide/reading).

## Image Data (VTI)

A regular grid with uniform spacing along each axis needs no geometry at all: it is defined by its extents, the coordinates of
its origin and its spacing. The point of indexes `(i,j,k)` is at `origin + [i,j,k] * spacing` (the origin is the point of
indexes `(0,0,0)`, also when the extents do not start at 0).

```fortran
use vtk_fortran, only : vtk_file
use penf,        only : I4P, R8P

type(vtk_file) :: a_vtk_file
integer(I4P)   :: error
real(R8P)      :: phi(0:63,0:63,0:31)   ! point data, one value per grid point

error = a_vtk_file%initialize(format='raw', filename='output.vti', mesh_topology='ImageData', &
                              nx1=0, nx2=63, ny1=0, ny2=63, nz1=0, nz2=31,                   &
                              origin=[0._R8P, 0._R8P, 0._R8P], spacing=[0.1_R8P, 0.1_R8P, 0.2_R8P])
error = a_vtk_file%xml_writer%write_piece(nx1=0, nx2=63, ny1=0, ny2=63, nz1=0, nz2=31)
error = a_vtk_file%xml_writer%write_dataarray(location='node', action='open')
error = a_vtk_file%xml_writer%write_dataarray(data_name='phi', x=phi, one_component=.true.)
error = a_vtk_file%xml_writer%write_dataarray(location='node', action='close')
error = a_vtk_file%xml_writer%write_piece()
error = a_vtk_file%finalize()
```

- `origin` and `spacing` are required for `ImageData`: without them `initialize` returns a non-zero error (and no file is
  written).
- `direction` (optional) orients the grid axes, as a row-major 3x3 matrix: the point `(i,j,k)` is at
  `origin + direction . ([i,j,k] * spacing)`. Default: identity.
- `write_geo` is not used. Point data have one value per grid point, cell data one value per cell, written with the usual
  `write_dataarray` calls in any format.

## Rectilinear Grid (VTR)

A rectilinear grid has independent 1-D coordinate arrays along each axis.

```fortran
use vtk_fortran, only : vtk_file
use penf,        only : I4P, I8P, R8P

type(vtk_file)          :: a_vtk_file
integer(I4P), parameter :: nx1=0, nx2=16, ny1=0, ny2=16, nz1=0, nz2=16
integer(I4P), parameter :: nn=(nx2-nx1+1)*(ny2-ny1+1)*(nz2-nz1+1)
real(R8P)               :: x(nx1:nx2), y(ny1:ny2), z(nz1:nz2)
integer(I4P)            :: v(1:nn)   ! one value per point
integer(I4P)            :: error

! ... fill x, y, z, v ...

error = a_vtk_file%initialize(format='binary', filename='output.vtr', &
                              mesh_topology='RectilinearGrid',         &
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
```

## Structured Grid (VTS)

A structured grid has full 3-D coordinate arrays — each grid point has its own `(x, y, z)` position.

```fortran
use vtk_fortran, only : vtk_file
use penf,        only : I4P, R8P

type(vtk_file)          :: a_vtk_file
integer(I4P), parameter :: nx1=0, nx2=9, ny1=0, ny2=5, nz1=0, nz2=5
integer(I4P), parameter :: nn=(nx2-nx1+1)*(ny2-ny1+1)*(nz2-nz1+1)
real(R8P)               :: x(nx1:nx2,ny1:ny2,nz1:nz2)
real(R8P)               :: y(nx1:nx2,ny1:ny2,nz1:nz2)
real(R8P)               :: z(nx1:nx2,ny1:ny2,nz1:nz2)
real(R8P)               :: v(nx1:nx2,ny1:ny2,nz1:nz2)
integer(I4P)            :: error

! ... fill x, y, z, v ...

error = a_vtk_file%initialize(format='binary', filename='output.vts', &
                              mesh_topology='StructuredGrid',          &
                              nx1=nx1, nx2=nx2, ny1=ny1, ny2=ny2, nz1=nz1, nz2=nz2)
error = a_vtk_file%xml_writer%write_piece(nx1=nx1, nx2=nx2, ny1=ny1, ny2=ny2, nz1=nz1, nz2=nz2)
error = a_vtk_file%xml_writer%write_geo(n=nn, x=x, y=y, z=z)
error = a_vtk_file%xml_writer%write_dataarray(location='node', action='open')
error = a_vtk_file%xml_writer%write_dataarray(data_name='pressure', x=v, one_component=.true.)
error = a_vtk_file%xml_writer%write_dataarray(location='node', action='close')
error = a_vtk_file%xml_writer%write_piece()
error = a_vtk_file%finalize()
```

The `one_component=.true.` flag ensures a scalar is written as a 1-component array, which ParaView renders correctly.

## Unstructured Grid (VTU)

An unstructured grid requires explicit node coordinates, a connectivity table, cell offsets, and cell types.

```fortran
use vtk_fortran, only : vtk_file
use penf,        only : I1P, I4P, R4P, R8P

type(vtk_file)                :: a_vtk_file
integer(I4P), parameter       :: np = 27   ! number of points
integer(I4P), parameter       :: nc = 11   ! number of cells
real(R4P),    dimension(1:np) :: x, y, z   ! node coordinates
integer(I1P), dimension(1:nc) :: cell_type ! VTK cell type code per cell
integer(I4P), dimension(1:nc) :: offset    ! cumulative connectivity offset per cell
integer(I4P), dimension(1:49) :: connect   ! flat connectivity list
real(R8P),    dimension(1:np) :: v         ! scalar at nodes
integer(I4P), dimension(1:np) :: vx, vy, vz ! vector components at nodes
integer(I4P)                  :: error

! ... fill arrays ...

error = a_vtk_file%initialize(format='binary', filename='output.vtu', &
                              mesh_topology='UnstructuredGrid')
error = a_vtk_file%xml_writer%write_piece(np=np, nc=nc)
error = a_vtk_file%xml_writer%write_geo(np=np, nc=nc, x=x, y=y, z=z)
error = a_vtk_file%xml_writer%write_connectivity(nc=nc, connectivity=connect, &
                                                  offset=offset, cell_type=cell_type)
error = a_vtk_file%xml_writer%write_dataarray(location='node', action='open')
error = a_vtk_file%xml_writer%write_dataarray(data_name='scalars', x=v)
error = a_vtk_file%xml_writer%write_dataarray(data_name='vector', x=vx, y=vy, z=vz)
error = a_vtk_file%xml_writer%write_dataarray(location='node', action='close')
error = a_vtk_file%xml_writer%write_piece()
error = a_vtk_file%finalize()
```

Unstructured grids are written in every format, compressed or not, as the other topologies.

Cell types are passed as `integer(I1P)` and written as a `UInt8` DataArray, as the VTK XML format specifies (VTK cell type
codes are all below 128, so the bytes are the same).

### Polyhedron cells

General polyhedra (VTK cell type `42`) need two more arrays, passed to `write_connectivity` as the optional `face` and
`faceoffset` arguments:

- `face`: for each polyhedron, the number of its faces followed, for each face, by the number of its points and the point ids;
- `faceoffset`: for each cell, the position in `face` where the description of that cell ends, or `-1` if the cell is not a
  polyhedron.

The `connectivity` of a polyhedron lists the (unique) ids of its points, as for any other cell. For example, a unit cube written
as a polyhedron (points `0`–`7`) followed by a tetrahedron (points `8`–`11`):

```fortran
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
```

See `src/tests/vtk_fortran_write_vtu_polyhedron.f90` for the complete program.

## Polygonal Data (VTP)

PolyData describe points and up to four blocks of cells: **vertices**, **lines** (polylines), **triangle strips** and
**polygons**, typical of surfaces, curves and point clouds. Open the piece with the number of points and of cells of each block,
write the points with `write_geo`, then the blocks with `write_polydata_cells`:

```fortran
use vtk_fortran, only : vtk_file
use penf,        only : I4P, R8P

type(vtk_file) :: a_vtk_file
integer(I4P)   :: error
real(R8P)      :: x(7), y(7), z(7)   ! a square (points 0-3) and a polyline (points 4-6)

error = a_vtk_file%initialize(format='raw', filename='output.vtp', mesh_topology='PolyData')
error = a_vtk_file%xml_writer%write_piece(np=7, nverts=0, nlines=1, nstrips=0, npolys=2)
error = a_vtk_file%xml_writer%write_geo(np=7, nc=3, x=x, y=y, z=z)
error = a_vtk_file%xml_writer%write_polydata_cells(lines_connectivity=[4,5,6], lines_offset=[3],          &
                                                    polys_connectivity=[0,1,2, 0,2,3], polys_offset=[3,6])
error = a_vtk_file%xml_writer%write_dataarray(location='cell', action='open')
error = a_vtk_file%xml_writer%write_dataarray(data_name='id', x=[1,2,3])   ! the polyline, then the two triangles
error = a_vtk_file%xml_writer%write_dataarray(location='cell', action='close')
error = a_vtk_file%xml_writer%write_piece()
error = a_vtk_file%finalize()
```

- Each block is a pair of arrays, as in `write_connectivity`: the point ids (0-based) of its cells one after the other, and the
  cumulative offset of the end of each cell. Pass only the blocks you have; a block with only one of its two arrays is an
  error. The number of cells of each block must match the counts given to `write_piece`.
- **Cell data follow the VTK order of the blocks**: vertices, then lines, then strips, then polygons, whatever the order of the
  arguments.
- The `nc` argument of `write_geo` is not used for PolyData (it writes the points only).

## Multiple pieces in one file

A single file can hold several pieces, e.g. the blocks of a multi-block solver written by one process without a parallel
header: open and close each piece with `write_piece`, and write its geometry, connectivity and data in between, exactly as
for a single-piece file. Readers (VTK, ParaView) merge the pieces into one dataset.

```fortran
error = a_vtk_file%initialize(format='raw', filename='blocks.vtu', mesh_topology='UnstructuredGrid')
do b=1, nblocks
  error = a_vtk_file%xml_writer%write_piece(np=np(b), nc=nc(b))
  error = a_vtk_file%xml_writer%write_geo(np=np(b), nc=nc(b), x=..., y=..., z=...)
  error = a_vtk_file%xml_writer%write_connectivity(nc=nc(b), connectivity=..., offset=..., cell_type=...)
  error = a_vtk_file%xml_writer%write_dataarray(location='node', action='open')
  error = a_vtk_file%xml_writer%write_dataarray(data_name='pressure', x=...)
  error = a_vtk_file%xml_writer%write_dataarray(location='node', action='close')
  error = a_vtk_file%xml_writer%write_piece()
enddo
error = a_vtk_file%finalize()
```

- Each piece has its own points: the point ids of its connectivity (and offsets) are local to the piece, starting from 0.
- Every piece must carry the same data arrays (same names, types and components): VTK takes the arrays of the first piece,
  so an array missing in the first piece is dropped, and one missing in a later piece is silently filled with zeros there.
- For structured topologies (`RectilinearGrid`, `StructuredGrid`, `ImageData`), `initialize` sets the whole extent and each
  `write_piece(nx1=..., nx2=..., ...)` sets the extent of the piece inside it; adjacent pieces share their boundary plane.
- It works with every format; the test `src/tests/vtk_fortran_write_multipiece.f90` writes both kinds of grids.

## Mesh topology strings

The `mesh_topology` argument is case-sensitive:

| Value | Produces |
|-------|---------|
| `ImageData` | `.vti` file (requires `origin` and `spacing`) |
| `RectilinearGrid` | `.vtr` file |
| `StructuredGrid` | `.vts` file |
| `UnstructuredGrid` | `.vtu` file |
| `PolyData` | `.vtp` file |
| `PRectilinearGrid` | `.pvtr` file (pvtk_file only) |
| `PStructuredGrid` | `.pvts` file (pvtk_file only) |
| `PUnstructuredGrid` | `.pvtu` file (pvtk_file only) |
| `PImageData` | `.pvti` file (pvtk_file only, requires `origin` and `spacing`) |
| `PPolyData` | `.pvtp` file (pvtk_file only) |
