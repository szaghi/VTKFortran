---
title: Parallel and composite files
---

# Parallel and composite files

Files that list other files: parallel (partitioned) headers, multi-block assemblies, time series; files written into
memory. Complete programs: the [cookbook](/manual/cookbook#parallel-and-composite-files) and chapters
[4](/manual/tutorial/04-time-series), [6](/manual/tutorial/06-parallel) and [7](/manual/tutorial/07-assembly) of the tutorial.

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

## Parallel Rectilinear Grid (PVTR)

A `.pvtr` header is written as a `.pvts` one, with `mesh_topology='PRectilinearGrid'`: each piece is a regular `.vtr` file
holding the coordinates of its own extent, and the header declares the coordinates type through `mesh_kind` (written as
`PCoordinates`). The test `src/tests/vtk_fortran_write_pvtr.f90` is a complete example with point and cell data.

```fortran
error = a_pvtk_file%initialize(filename='output.pvtr', mesh_topology='PRectilinearGrid', mesh_kind='Float64', &
                               nx1=0, nx2=4, ny1=0, ny2=2, nz1=0, nz2=2)
! ... PPointData / PCellData as for PVTS ...
error = a_pvtk_file%xml_writer%write_parallel_geo(source='part_01.vtr', nx1=0, nx2=2, ny1=0, ny2=2, nz1=0, nz2=2)
error = a_pvtk_file%xml_writer%write_parallel_geo(source='part_02.vtr', nx1=2, nx2=4, ny1=0, ny2=2, nz1=0, nz2=2)
error = a_pvtk_file%finalize()
```

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

The names, types and numbers of components declared in the `.pvtu` file must match the data arrays written in every piece:
`check_pieces` verifies it, see [Parallel headers](/guide/reading#parallel-headers). See `src/tests/vtk_fortran_write_pvtu.f90` for the
complete program, pieces included.

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

### Nested blocks

Blocks can contain both datasets and other blocks, to any depth, e.g. to mirror the parts and assemblies of a CAD model.
Build the hierarchy step by step with the `action` argument of `write_block`:

| Call | Effect |
|------|--------|
| `write_block(action='open', name=...)` | open a child block of the current block (or a top-level block) |
| `write_block(filenames=[...], names=[...], action='write')` | write the files as datasets of the current block |
| `write_block(filenames=[...], names=[...], name=...)` | write the files wrapped in a new child block |
| `write_block(action='close')` | close the current block |

```fortran
error = a_vtm_file%initialize(filename='machine.vtm')
error = a_vtm_file%write_block(action='open', name='assembly')                              ! Block 0
error = a_vtm_file%write_block(filenames=['a.vtu', 'b.vtu'], names=['a', 'b'], action='write') !   DataSet 0, 1
error = a_vtm_file%write_block(filenames=['b1.vtu', 'b2.vtu'], name='bolts')                  !   Block 2: DataSet 0, 1
error = a_vtm_file%write_block(action='open', name='sub-assembly')                          !   Block 3
error = a_vtm_file%write_block(filenames=['n.vtu'], name='nuts')                              !     Block 0: DataSet 0
error = a_vtm_file%write_block(action='close')                                              !   (end of Block 3)
error = a_vtm_file%write_block(action='close')                                              ! (end of Block 0)
error = a_vtm_file%finalize()
```

- The children of a block (blocks and datasets together) get the indexes 0, 1, 2, … in the order they are written, as VTK
  numbers the children of a multi-block dataset. Every `action='open'` needs its `action='close'`.
- The string form (`filenames='a.vtu b.vtu'`, names separated by blanks) accepts the same `action` values.
- The test `src/tests/vtk_fortran_write_vtm_nested.f90` writes the hierarchy of issue #25, checked with VTK's reader.

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

In some parallel setups only one process (the master) can access the file system. The other processes can write their files
into memory instead: initialize the file with `is_volatile=.true.`, write it as usual, then get its content as a string,
send it to the master, which writes it to disk with `write_xml_volatile`.

```fortran
use vtk_fortran, only : vtk_file, write_xml_volatile

type(vtk_file)                :: a_vtk_file
character(len=:), allocatable :: xml_volatile
integer(I4P)                  :: error

! on a process without access to the file system
error = a_vtk_file%initialize(format='binary', filename='part_01.vtr', mesh_topology='RectilinearGrid', &
                              is_volatile=.true., nx1=nx1, nx2=nx2, ny1=ny1, ny2=ny2, nz1=nz1, nz2=nz2)
! ... write_piece / write_geo / write_dataarray / write_piece, as usual ...
error = a_vtk_file%finalize()
call a_vtk_file%get_xml_volatile(xml_volatile) ! the whole file, as a string
call a_vtk_file%free                           ! free the memory of the volatile file
! ... send xml_volatile to the master ...

! on the master
error = write_xml_volatile(xml_volatile=xml_volatile, filename='part_01.vtr')
```

- The `binary` and `ascii` formats support volatile files. The appended formats (`raw`, `raw-zlib`, `binary-appended`)
  write their data to the file directly: with `is_volatile=.true.`, `initialize` returns a non-zero error.
- The string is the exact content of the file: written by the master, it is identical to the file written directly (the
  test `src/tests/vtk_fortran_write_volatile.f90` checks it).
