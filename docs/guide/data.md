---
title: Data arrays
---

# Data arrays

What goes with the mesh: global field data, the active arrays of the readers, unsigned arrays. Complete programs: the
[cookbook](/manual/cookbook#write-data) and [chapter 3](/manual/tutorial/03-more-data) of the tutorial.

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
VTKFortran reads them back with `read_dataarray(location='field', ...)`, see [Reading files](/guide/reading).

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

## Unsigned integer arrays

The VTK XML format has unsigned integer types (`UInt8`, `UInt16`, `UInt32`, `UInt64`), which Fortran lacks. Write a
rank-1, one-component integer array as unsigned with `write_dataarray_unsigned`: the kind of the array selects the type of
the same width, and its bits are written as they are.

| Fortran kind | VTK type | Stored value for unsigned `v` |
|--------------|----------|-------------------------------|
| `I1P` | `UInt8` | `v` if `v < 128`, else `v - 256` |
| `I2P` | `UInt16` | `v` if `v < 32768`, else `v - 65536` |
| `I4P` | `UInt32` | `v` if `v < 2^31`, else `v - 2^32` |
| `I8P` | `UInt64` | `v` if `v < 2^63`, else `v - 2^64` |

The typical use is the ghost array of VTK, `vtkGhostType` (`UInt8` bit flags: `1` marks a duplicate, i.e. ghost, cell or
point, `32` a hidden cell). Readers still load the ghost cells, but VTK and ParaView skip them when they extract the
surfaces to render, so the cells a parallel piece shares with its neighbours are not drawn twice:

```fortran
integer(I1P) :: ghost(nc) ! 0 for the cells owned by this piece, 1 for the ghost cells
...
error = a_vtk_file%xml_writer%write_dataarray(location='cell', action='open')
error = a_vtk_file%xml_writer%write_dataarray_unsigned(data_name='vtkGhostType', x=ghost)
error = a_vtk_file%xml_writer%write_dataarray(location='cell', action='close')
```

- It works with every format, compression included. The binary formats write the bytes of the array; the ASCII format
  prints the unsigned values (e.g. `200` for `-56_I1P`).
- With the ASCII format a `UInt64` value of `2^63` or more (a negative `I8P`) cannot be printed: nothing is written and
  the function returns a non-zero error. The binary formats write any value.
- Only rank-1 arrays with one component; in a parallel header declare it with
  `write_parallel_dataarray(data_name=..., data_type='UInt8', number_of_components=1)`.
