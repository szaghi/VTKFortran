---
title: Formats and large data
---

# Formats and large data

How the data are stored: the formats, zlib compression, arrays and meshes beyond the 32-bit limits. Complete programs:
the [cookbook](/manual/cookbook#choose-the-format) and [chapter 2](/manual/tutorial/02-formats) of the tutorial.

## Output format selection

The `format` argument to `initialize` is case-insensitive:

| Value | Description |
|-------|-------------|
| `ascii` | Text inside XML elements |
| `binary` | Base64-encoded binary inside XML elements |
| `raw` | Raw binary in the appended section |
| `binary-appended` | Base64-encoded binary in the appended section |
| `raw-zlib` | Shorthand for `raw` with `compressor='zlib'`, see [Compressed binary data](#compressed-binary-data-zlib) |

The binary formats (`binary`, `raw`, `binary-appended`) can be zlib-compressed with `compressor='zlib'`, see
[Compressed binary data](#compressed-binary-data-zlib). Binary arrays larger than 2 GiB need `header_type='UInt64'`, see
[Large data arrays](#large-data-arrays-uint64-headers).

The appended formats (`raw`, `raw-zlib`, `binary-appended`) write the XML metadata of each array first and its data only at
`finalize`, in the appended section after all the metadata. Until then the data are held in a **scratch file**, so memory use
does not grow with the file size and the arrays can be deallocated after each `write_dataarray`. The cost is writing the data
once more to the scratch file and reading them back. The scratch file is opened in the temporary directory of the Fortran
runtime: set `TMPDIR` (or `GFORTRAN_TMPDIR` for gfortran, `FORT_TMPDIR` for Intel ifx) to move it to a disk with enough free
space, e.g. off a small `/tmp` on HPC nodes. The `binary` format writes inline, with no scratch file.

## Compressed binary data (zlib)

The binary formats can compress their data with zlib, as VTK and ParaView do (`vtkZLibDataCompressor`). Select the
compressor when initializing the file:

```fortran
error = a_vtk_file%initialize(format='binary', filename='mesh.vtu', mesh_topology='UnstructuredGrid', compressor='zlib')
```

| `format` | with `compressor='zlib'` |
|----------|--------------------------|
| `binary` | compressed data, base64-encoded inside each `DataArray` |
| `raw` | compressed data, raw in the appended section (the same as `format='raw-zlib'`) |
| `binary-appended` | compressed data, base64-encoded in the appended section |
| `ascii` | the compressor is ignored: ASCII data are never compressed |

- `compressor` is `'none'` (default) or `'zlib'`, case insensitive. Any other value makes `initialize` return a non-zero
  error, and so does `format='raw-zlib'` with `compressor='none'`.
- zlib must be enabled when the library is built (`VTKFORTRAN_USE_ZLIB`, see
  [Installation](/guide/installation#optional-zlib-compression)). Without it, `compressor='zlib'` and `raw-zlib` make
  `initialize` return a non-zero error: check it to fall back to an uncompressed format.
- The compression applies to every DataArray of the file: points, connectivity, point, cell and field data. The file
  declares `compressor="vtkZLibDataCompressor"` in its `VTKFile` element. Parallel (`P*`), multi-block and `.pvd` files
  contain no binary data: each piece selects its own compressor.
- Each array is split into blocks of 32 KiB, each compressed independently (zlib level 6), and is preceded by a header with
  the number of blocks, the block size, the size of the last partial block and the compressed size of each block. The
  header words are UInt32 or UInt64, as the `header_type` of the file. With base64, VTK encodes the header and the
  compressed blocks as two separate base64 streams, and so does VTKFortran: the encoded arrays are byte-identical to the
  output of VTK's own writer with the same compression level.
- How much the files shrink depends on the data: integer arrays (connectivity, offsets, cell types, ids) and fields with
  repeated values compress well, generic floating point fields much less. Measure on your own data.
- The cost is CPU time, and memory: while an array is written, its bytes are copied and compressed into temporary buffers
  that together take up to about twice the size of the array.
- With `header_type='UInt32'`, the 2 GiB limit applies to the uncompressed size of each array, as without compression.

## Large data arrays (UInt64 headers)

In the binary formats (`binary`, `raw`, `raw-zlib`, `binary-appended`) each DataArray is prefixed by its size in bytes (by
the header of its compressed blocks, when compressed). By
default the prefix is a 32-bit integer (`header_type="UInt32"`), which limits **each DataArray to 2 GiB**: for example a
3-component `R8P` field of more than about 89 million points. A larger array stops the execution with an explicit error that
suggests the fix. Select 64-bit prefixes when initializing the file:

```fortran
error = a_vtk_file%initialize(format='raw', filename='large.vtu', mesh_topology='UnstructuredGrid', header_type='UInt64')
```

- `header_type` is `'UInt32'` (default) or `'UInt64'`, case insensitive; any other value makes `initialize` return a
  non-zero error. It applies to the whole file, so choose it before writing any array; the ASCII format ignores it.
- With `'UInt64'` the file declares `header_type="UInt64"` and the size prefixes (and, for compressed data, the compressed
  blocks headers) are 8 bytes wide, as written by VTK itself; ParaView and VTK read these files.
- The default keeps the files exactly as before. Parallel (`P*`) and multi-block files contain no binary data: their pieces
  select their own header type.
- The number of elements of an array is not limited to 32 bits: an array can hold more than 2^31 values (e.g. the
  connectivity of more than about 270 million hexahedra) in every format, provided its bytes fit the header type.
- The limit concerns the bytes of a single array; for pieces with more than 2^31-1 points or cells, see
  [Large meshes](#large-meshes-64-bit-counts-and-connectivity).

## Large meshes (64-bit counts and connectivity)

A piece with more than 2^31-1 points or cells, or whose point ids exceed `huge(1_I4P)`, needs 64-bit counts and ids. The
mesh procedures accept `I8P` arguments: the kind passed selects the version, so existing `I4P` calls are unchanged.

```fortran
integer(I8P) :: np, nc, connect(:), offset(:), face(:), faceoffset(:) ! allocatable in a real code
...
error = a_vtk_file%xml_writer%write_piece(np=np, nc=nc)
error = a_vtk_file%xml_writer%write_geo(np=np, nc=nc, x=x, y=y, z=z)
error = a_vtk_file%xml_writer%write_connectivity(nc=nc, connectivity=connect, offset=offset, cell_type=cell_type)
```

| Procedure | 64-bit version |
|-----------|----------------|
| `write_piece(np, nc)` | `np`, `nc` as `I8P` |
| `write_piece(np, nverts, nlines, nstrips, npolys)` | all counts as `I8P` |
| `write_geo(np, nc, x, y, z)`, `write_geo(np, nc, xyz)` | `np`, `nc` as `I8P` |
| `write_connectivity(nc, connectivity, offset, cell_type, face, faceoffset)` | `nc`, ids, offsets and faces as `I8P`; `cell_type` stays `I1P` |
| `write_polydata_cells(...)` | the arrays of each block as `I8P` (each block can use `I4P` or `I8P`) |

- `I8P` connectivity, offsets and faces are written as `Int64`, as VTK's own writer does; `I4P` ones as `Int32`. Readers
  accept both, and the mesh is the same.
- Counts and ids can be mixed: e.g. `I4P` counts in `write_piece` with `I8P` connectivity.
- An array with more than 2^31 elements works in either kind (e.g. an `I4P` connectivity of more than about 270 million
  hexahedra); its bytes must fit the header type (`header_type='UInt64'` beyond 2 GiB).
- The extents of structured grids (`RectilinearGrid`, `StructuredGrid`, `ImageData`) stay 32-bit per axis.
- The test `src/tests/vtk_fortran_write_i8p.f90` writes the same meshes with both kinds, in every format.
