---
title: Usage
---

# Usage

All examples use the modern VTKFortran API: `use vtk_fortran` and `type(vtk_file)`.

The general workflow for writing a VTK XML file is:

1. **Initialize** — open the file and select the format and mesh topology
2. **Write field data** *(optional)* — attach global metadata (time, cycle, etc.)
3. **Open a piece** — declare the extent or node/cell counts for the current piece
4. **Write geometry** — coordinates (and connectivity for unstructured grids)
5. **Write data arrays** — node-centered or cell-centered variables
6. **Close the piece**
7. **Finalize** — flush and close the file

All procedures return an integer error code. Zero means success.

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
integer(I4P)            :: v(1:nn)
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
error = a_vtk_file%xml_writer%write_dataarray(location='cell', action='open')
error = a_vtk_file%xml_writer%write_dataarray(data_name='cell_value', x=v)
error = a_vtk_file%xml_writer%write_dataarray(location='cell', action='close')
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

Supported formats for unstructured grids: `ascii`, `raw`, and `binary`.

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

## Multi-block Dataset (VTM)

A VTM file is a composite wrapper that references multiple individual VTK files organised into named blocks.

```fortran
use vtk_fortran, only : vtm_file, vtk_file
use penf,        only : I4P, R8P

type(vtk_file)  :: a_vtk_file
type(vtm_file)  :: a_vtm_file
character(15)   :: filenames(4)
integer(I4P)    :: error, f

filenames = ['block_01.vts', 'block_02.vts', 'block_03.vts', 'block_04.vts']

! write each partition as an independent VTS file
do f = 1, size(filenames, dim=1)
  error = a_vtk_file%initialize(format='raw', filename=filenames(f), &
                                mesh_topology='StructuredGrid',        &
                                nx1=0, nx2=9, ny1=0, ny2=5, nz1=0, nz2=5)
  ! ... write_piece / write_geo / write_dataarray / write_piece ...
  error = a_vtk_file%finalize()
enddo

! assemble the multi-block wrapper
error = a_vtm_file%initialize(filename='output.vtm')
error = a_vtm_file%write_block(filenames=[filenames(1), filenames(2)], &
                               names=['1','2'], name='first block')
error = a_vtm_file%write_block(filenames=[filenames(3), filenames(4)], &
                               names=['3','4'], name='second block')
error = a_vtm_file%finalize()
```

## Parallel Structured Grid (PVTS)

For MPI-parallel codes, each rank writes its own partition as a regular VTS file, then a single PVTS header file references all partitions.

```fortran
use vtk_fortran, only : vtk_file, pvtk_file
use penf,        only : I4P, R8P

! whole extent
integer(I4P), parameter :: nx1=0, nx2=9, ny1=0, ny2=5, nz1=0, nz2=5
! partition boundaries along x
integer(I4P), parameter :: nx1_p(2) = [0,  4]
integer(I4P), parameter :: nx2_p(2) = [4,  9]

! --- each rank writes its own partition ---
call write_partition(part=1, filename='part_01.vts')
call write_partition(part=2, filename='part_02.vts')

! --- one rank writes the parallel header ---
block
  type(pvtk_file) :: a_pvtk_file
  integer(I4P)    :: error

  error = a_pvtk_file%initialize(filename='output.pvts',              &
                                  mesh_topology='PStructuredGrid',      &
                                  mesh_kind='Float64',                  &
                                  nx1=nx1, nx2=nx2, ny1=ny1, ny2=ny2, nz1=nz1, nz2=nz2)
  error = a_pvtk_file%xml_writer%write_dataarray(location='node', action='open')
  error = a_pvtk_file%xml_writer%write_parallel_dataarray(data_name='pressure', &
                                                           data_type='Float64',  &
                                                           number_of_components=1)
  error = a_pvtk_file%xml_writer%write_dataarray(location='node', action='close')
  error = a_pvtk_file%xml_writer%write_parallel_geo(source='part_01.vts', &
            nx1=nx1, nx2=nx2_p(1), ny1=ny1, ny2=ny2, nz1=nz1, nz2=nz2)
  error = a_pvtk_file%xml_writer%write_parallel_geo(source='part_02.vts', &
            nx1=nx2_p(1), nx2=nx2_p(2), ny1=ny1, ny2=ny2, nz1=nz1, nz2=nz2)
  error = a_pvtk_file%finalize()
end block
```

::: tip Adjacent partition extents
Adjacent pieces must share the boundary ordinate: `nx2_p(1)` of piece 1 must equal `nx1_p(2)` of piece 2. This is required for correct rendering in ParaView.
:::

## Parallel Unstructured Grid (PVTU)

The same approach works for unstructured grids: each rank writes its partition as a complete `.vtu` file, with its own points
and a connectivity using **local** point ids (`0` to `np-1` of that piece), then one rank writes the `.pvtu` file. The `.pvtu`
file contains no data: it declares the type of the points coordinates (`mesh_kind`) and of every data array of the pieces, and
lists the pieces. Unstructured pieces have no extents, so `write_parallel_geo` takes only the piece file name.

```fortran
use vtk_fortran, only : pvtk_file
use penf,        only : I4P

type(pvtk_file) :: a_pvtk_file
integer(I4P)    :: error

! --- after every rank has written its own part_NN.vtu ---
error = a_pvtk_file%initialize(filename='output.pvtu', mesh_topology='PUnstructuredGrid', mesh_kind='Float64')
error = a_pvtk_file%xml_writer%write_dataarray(location='node', action='open')
error = a_pvtk_file%xml_writer%write_parallel_dataarray(data_name='temperature', data_type='Float64', &
                                                         number_of_components=1)
error = a_pvtk_file%xml_writer%write_dataarray(location='node', action='close')
error = a_pvtk_file%xml_writer%write_dataarray(location='cell', action='open')
error = a_pvtk_file%xml_writer%write_parallel_dataarray(data_name='part', data_type='Int32', number_of_components=1)
error = a_pvtk_file%xml_writer%write_dataarray(location='cell', action='close')
error = a_pvtk_file%xml_writer%write_parallel_geo(source='part_01.vtu')
error = a_pvtk_file%xml_writer%write_parallel_geo(source='part_02.vtu')
error = a_pvtk_file%finalize()
```

The names, types and numbers of components declared in the `.pvtu` file must match the data arrays written in every piece. See
`src/tests/vtk_fortran_write_pvtu.f90` for the complete program, pieces included.

## Field data (global metadata)

FieldData holds values that belong to the whole dataset rather than to points or cells: time, cycle number, solver name and
version, a history of residuals, etc. Write it right after `initialize`, before the first piece, between an `open` and a
`close` call:

```fortran
error = a_vtk_file%xml_writer%write_fielddata(action='open')
error = a_vtk_file%xml_writer%write_fielddata(data_name='TIME',      x=0.5_R8P)                     ! scalar
error = a_vtk_file%xml_writer%write_fielddata(data_name='CYCLE',     x=7_I8P)                       ! scalar
error = a_vtk_file%xml_writer%write_fielddata(data_name='residuals', x=[1.e-3_R8P, 1.e-4_R8P])      ! rank-1 array
error = a_vtk_file%xml_writer%write_fielddata(data_name='solver',    x='my solver v1.2')            ! string
error = a_vtk_file%xml_writer%write_fielddata(data_name='species',   x=['N2', 'O2'])                ! array of strings
error = a_vtk_file%xml_writer%write_fielddata(action='close')
```

| `x` | Written as |
|-----|------------|
| scalar of any PENF kind (`R8P`, `R4P`, `I8P`, `I4P`, `I2P`, `I1P`) | `DataArray` with 1 tuple |
| rank-1 array of any PENF kind | `DataArray` with `size(x)` tuples, one component each |
| `character` scalar | `Array type="String"` with 1 tuple |
| `character` rank-1 array | `Array type="String"` with `size(x)` tuples, one string each |

Strings are written as VTK writes them (each string followed by a NUL character), in every output format, and readers return
them as string arrays. Trailing blanks of each string are trimmed, so a Fortran array of fixed-length strings can be passed as
is. In ParaView, field data are listed in the Spreadsheet view (attribute *Field Data*) and are available to filters and Python.

## Active arrays

Readers such as ParaView color a dataset by its *active* scalars and use its *active* vectors, normals, tensors and texture
coordinates by default. Without a designation they pick the first suitable array, which is often not the one wanted. Designate
the active array of each role when opening the node or cell data, with the optional `scalars`, `vectors`, `normals`, `tensors`
and `tcoords` arguments: each one is the `data_name` of an array written inside that tag.

```fortran
error = a_vtk_file%xml_writer%write_dataarray(location='node', action='open', scalars='pressure', vectors='velocity')
error = a_vtk_file%xml_writer%write_dataarray(data_name='temperature', x=t)              ! not active
error = a_vtk_file%xml_writer%write_dataarray(data_name='pressure', x=p)                 ! active scalars
error = a_vtk_file%xml_writer%write_dataarray(data_name='velocity', x=u, y=v, z=w)       ! active vectors
error = a_vtk_file%xml_writer%write_dataarray(location='node', action='close')
```

writes `<PointData Scalars="pressure" Vectors="velocity">` (`<CellData ...>` for `location='cell'`).

| Argument | Attribute | Array components expected by readers |
|----------|-----------|--------------------------------------|
| `scalars` | `Scalars` | 1 (up to 4) |
| `vectors` | `Vectors` | 3 |
| `normals` | `Normals` | 3 |
| `tensors` | `Tensors` | 9 (or 6, symmetric) |
| `tcoords` | `TCoords` | 1 to 3 |

- Each argument is optional and independent: omit them all and the tag is written as before, without attributes.
- The names are written as given, not checked: make sure an array with that name is written inside the same tag.
- The arguments apply to `action='open'` only, and are ignored when closing.
- In parallel files the same arguments designate the active arrays in `<PPointData>`/`<PCellData>`: pass them to the
  `pvtk_file` writer with the arrays declared by `write_parallel_dataarray`.

See `src/tests/vtk_fortran_write_active_arrays.f90` for a complete program, parallel header included.

## Parallel Image Data (PVTI)

Each rank writes its piece as a `.vti` file with the **same** `origin` and `spacing` of the whole grid and the extents of the
piece (adjacent pieces share the boundary index, as for PVTS); one rank writes the `.pvti` file with the whole extents, the
same `origin` and `spacing`, and the extents of each piece. `mesh_kind` is not needed: image pieces have no points coordinates.

```fortran
error = a_pvtk_file%initialize(filename='output.pvti', mesh_topology='PImageData',        &
                                nx1=0, nx2=63, ny1=0, ny2=63, nz1=0, nz2=31,               &
                                origin=[0._R8P, 0._R8P, 0._R8P], spacing=[0.1_R8P, 0.1_R8P, 0.2_R8P])
error = a_pvtk_file%xml_writer%write_dataarray(location='node', action='open')
error = a_pvtk_file%xml_writer%write_parallel_dataarray(data_name='phi', data_type='Float64', number_of_components=1)
error = a_pvtk_file%xml_writer%write_dataarray(location='node', action='close')
error = a_pvtk_file%xml_writer%write_parallel_geo(source='part_01.vti', nx1=0,  nx2=32, ny1=0, ny2=63, nz1=0, nz2=31)
error = a_pvtk_file%xml_writer%write_parallel_geo(source='part_02.vti', nx1=32, nx2=63, ny1=0, ny2=63, nz1=0, nz2=31)
error = a_pvtk_file%finalize()
```

See `src/tests/vtk_fortran_write_vti.f90` for a complete program, serial and parallel.

## Parallel Polygonal Data (PVTP)

As for PVTU: each rank writes its piece as a complete `.vtp` file (local point ids), one rank writes the `.pvtp` file with the
type of the points coordinates (`mesh_kind`, required) and of each data array, and the list of the pieces:

```fortran
error = a_pvtk_file%initialize(filename='output.pvtp', mesh_topology='PPolyData', mesh_kind='Float64')
error = a_pvtk_file%xml_writer%write_dataarray(location='cell', action='open')
error = a_pvtk_file%xml_writer%write_parallel_dataarray(data_name='id', data_type='Int32', number_of_components=1)
error = a_pvtk_file%xml_writer%write_dataarray(location='cell', action='close')
error = a_pvtk_file%xml_writer%write_parallel_geo(source='part_01.vtp')
error = a_pvtk_file%xml_writer%write_parallel_geo(source='part_02.vtp')
error = a_pvtk_file%finalize()
```

See `src/tests/vtk_fortran_write_vtp.f90` for a complete program with the four blocks, serial and parallel.

## Time series (PVD)

A `.pvd` file is a *collection*: it lists the files of a simulation (any VTK XML file written by `vtk_file`, `pvtk_file` or
`vtm_file`) with their time, and ParaView loads them as a single time series. Write each time step as usual, then add it to the
collection with `pvd_file`:

```fortran
use vtk_fortran, only : pvd_file, vtk_file
use penf,        only : I4P, R8P, strz

type(pvd_file) :: pvd
integer(I4P)   :: error, step
real(R8P)      :: time

error = pvd%initialize(filename='simulation.pvd')
do step = 0, nsteps
  ! ... advance the solution to `time`, write simulation_NNNN.vtu with a vtk_file ...
  error = pvd%write_dataset(filename='simulation_'//trim(strz(step, 4))//'.vtu', timestep=time)
enddo
error = pvd%finalize()
```

writes

```xml
<?xml version="1.0"?>
<VTKFile type="Collection" version="1.0" byte_order="LittleEndian">
  <Collection>
    <DataSet timestep="0.0" part="0" file="simulation_0000.vtu"/>
    <DataSet timestep="0.1" part="0" file="simulation_0001.vtu"/>
  </Collection>
</VTKFile>
```

- **The file is valid at any time.** Each `write_dataset` writes its entry and the closing tags, and flushes the file: a run
  that stops before `finalize` (crash, job killed by the scheduler) still leaves a collection that ParaView opens, with all the
  steps written so far.
- **Restart.** `pvd%initialize(filename='simulation.pvd', action='append')` reopens an existing collection and adds the new
  steps after the ones already listed (the default `action='new'` replaces the file). The file must exist and be a collection,
  otherwise `initialize` returns a non-zero error.
- **Optional attributes.** `write_dataset` also accepts `part` (default `0`; e.g. the rank, when each process writes its own
  file per step), `group` and `name`.
- `timestep` is a `real(R8P)`, written with the shortest representation that reads back exactly. `filename` is written as
  given: relative paths are relative to the directory of the `.pvd` file.

See `src/tests/vtk_fortran_write_pvd.f90` for a complete program, restart included.

## Volatile XML output

`write_xml_volatile` returns the XML content as an in-memory string instead of writing to disk. This is useful when the calling code controls I/O (e.g., HDF5-backed parallel I/O or MPI-IO).

```fortran
use vtk_fortran, only : write_xml_volatile

character(len=:), allocatable :: xml_string
integer                       :: error

xml_string = write_xml_volatile(format='binary', mesh_topology='UnstructuredGrid', &
                                 np=np, nc=nc, x=x, y=y, z=z, &
                                 connectivity=connect, offset=offset, cell_type=cell_type, &
                                 error=error)
```

## Output format selection

The `format` argument to `initialize` is case-insensitive:

| Value | Description |
|-------|-------------|
| `ascii` | Text inside XML elements |
| `binary` | Base64-encoded binary inside XML elements |
| `raw` | Raw binary in the appended section |
| `binary-appended` | Base64-encoded binary in the appended section |
| `raw-zlib` | Raw binary in the appended section, zlib-compressed; requires building with `VTKFORTRAN_USE_ZLIB` (CMake option or `-DVTKFORTRAN_USE_ZLIB`), otherwise `initialize` returns a non-zero error |

## Mesh topology strings

The `mesh_topology` argument is case-sensitive:

| Value | Produces |
|-------|---------|
| `ImageData` | `.vti` file (requires `origin` and `spacing`) |
| `RectilinearGrid` | `.vtr` file |
| `StructuredGrid` | `.vts` file |
| `UnstructuredGrid` | `.vtu` file |
| `PolyData` | `.vtp` file |
| `PStructuredGrid` | `.pvts` file (pvtk_file only) |
| `PUnstructuredGrid` | `.pvtu` file (pvtk_file only) |
| `PImageData` | `.pvti` file (pvtk_file only, requires `origin` and `spacing`) |
| `PPolyData` | `.pvtp` file (pvtk_file only) |
