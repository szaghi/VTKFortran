---
title: Data arrays
---

# Data arrays

What goes with the mesh: point and cell data of every shape and kind, global field data, the active arrays of the
readers, unsigned arrays. Complete programs: the [cookbook](/manual/cookbook#write-data) and
[chapter 3](/manual/tutorial/03-more-data) of the tutorial. The examples of this page are taken from
[`ref_data.f90`](https://github.com/szaghi/VTKFortran/tree/master/docs/examples/src/ref_data.f90), compiled and run with
the documentation.

## Writing arrays

Point data and cell data are written between an `open` and a `close` of their location,
`write_dataarray(location='node', action='open')` (`<PointData>`; `location='cell'` for `<CellData>`) and
`write_dataarray(location='node', action='close')`, with any number of `write_dataarray(data_name=..., x=...)` in between.

An array has one **tuple** per point (`node`) or per cell (`cell`) of the piece, and each tuple has one or more
**components**: 1 for a scalar, 3 for a vector, 6 or 9 for a tensor. `write_dataarray` takes the components from the shape
of what it is given:

| Arguments | Components | Tuples |
|-----------|------------|--------|
| `x(n)`, rank 1 | 1 | `n` |
| `x(nc,n)`, rank 2 | `nc` = `size(x,1)` | `n` |
| `x(nc,ni,nj,nk)`, rank 4 | `nc` = `size(x,1)` | `ni*nj*nk` |
| `x` of rank 2, 3 or 4 with `one_component=.true.` | 1 | `size(x)` |
| `x, y, z`, three arrays of rank 1 or 3 | 3, interleaved: `x(1), y(1), z(1), x(2), ...` | `size(x)` |
| `u, v, w, x, y, z`, six arrays of rank 1 or 3 | 6, interleaved in this order | `size(x)` |

The kind of the arrays selects the VTK type: `R8P` `Float64`, `R4P` `Float32`, `I8P` `Int64`, `I4P` `Int32`, `I2P` `Int16`,
`I1P` `Int8` (for the unsigned types see [below](#unsigned-integer-arrays)). The tuples are taken in the Fortran order of
the array elements: for a structured grid `i` varies first, then `j`, then `k`, which is the order of the points (and of
the cells) of VTK. For a symmetric tensor VTK expects the components `XX, YY, ZZ, XY, YZ, XZ`: pass them in this order
as `u, v, w, x, y, z`, or as the first dimension of a rank-2 array.

<<< @/examples/snippets/ref_data-shapes.f90

<<< @/examples/snippets/ref_data-shapes-write.f90

Read back, each array has the components and tuples of the table (a grid of 2x2x2 points):

<<< @/examples/output/ref_data.ansi{ansi}

::: warning A scalar field stored in an array of rank 2 to 4 needs `one_component=.true.`
Without it the first dimension is taken as the components: the rank-3 `t(2,2,2)` becomes an array of 2 components,
a file that readers reject or misread.

<<< @/examples/snippets/ref_data-trap.f90

<<< @/examples/output/ref_data-trap.ansi{ansi}
:::

## Field data (global metadata)

FieldData holds values that belong to the whole dataset rather than to points or cells: time, cycle number, solver name and
version, a history of residuals, etc. Write it right after `initialize`, before the first piece, between an `open` and a
`close` call:

<<< @/examples/snippets/ref_data-fielddata.f90

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

<<< @/examples/snippets/ref_data-active.f90

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

`ghost` is an `integer(I1P)` array, one value per cell: 0 for the cells owned by the piece, 1 for the ghost cells.

<<< @/examples/snippets/ref_data-unsigned.f90

- It works with every format, compression included. The binary formats write the bytes of the array; the ASCII format
  prints the unsigned values (e.g. `200` for `-56_I1P`).
- With the ASCII format a `UInt64` value of `2^63` or more (a negative `I8P`) cannot be printed: nothing is written and
  the function returns a non-zero error. The binary formats write any value.
- Only rank-1 arrays with one component; in a parallel header declare it with
  `write_parallel_dataarray(data_name=..., data_type='UInt8', number_of_components=1)`.
