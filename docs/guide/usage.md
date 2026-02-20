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

## Mesh topology strings

The `mesh_topology` argument is case-sensitive:

| Value | Produces |
|-------|---------|
| `RectilinearGrid` | `.vtr` file |
| `StructuredGrid` | `.vts` file |
| `UnstructuredGrid` | `.vtu` file |
| `PStructuredGrid` | `.pvts` file (pvtk_file only) |
| `PUnstructuredGrid` | `.pvtu` file (pvtk_file only) |
