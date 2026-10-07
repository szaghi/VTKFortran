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
| `RectilinearGrid` | `.vtr` file |
| `StructuredGrid` | `.vts` file |
| `UnstructuredGrid` | `.vtu` file |
| `PStructuredGrid` | `.pvts` file (pvtk_file only) |
| `PUnstructuredGrid` | `.pvtu` file (pvtk_file only) |
