---
title: Reading files
---

# Reading files

Read back every file VTKFortran writes, and the same files written by VTK. Complete programs: the
[cookbook](/manual/cookbook#read-files) and [chapter 8](/manual/tutorial/08-restart) of the tutorial.

## Serial files

A serial file (`.vti`, `.vtr`, `.vts`, `.vtu`, `.vtp`) is read by initializing a `vtk_file` with `action='read'`: the file is
indexed, and its data are then read, array by array, through the `xml_reader` component.

<<< @/examples/snippets/ref_reading-serial.f90

| Procedure | Returns |
|-----------|---------|
| `get_info(mesh_topology, npieces, header_type, compressor, nx1...nz2, origin, spacing, direction)` | dataset type, number of pieces, header type and compressor of the binary data; whole extent of structured grids; origin, spacing and direction of `ImageData` |
| `read_piece(piece, np, nc, nx1...nz2, nverts, nlines, nstrips, npolys)` | counts (`I8P`) and extent of a piece |
| `read_geo(x, y, z, piece)` | point coordinates (`StructuredGrid`, `UnstructuredGrid`, `PolyData`) or the coordinates along each axis (`RectilinearGrid`), `R8P` or `R4P`; `ImageData` has no stored geometry, see `get_info` |
| `read_connectivity(connectivity, offset, cell_type, face, faceoffset, piece)` | cells of an `UnstructuredGrid`, ids `I4P` or `I8P`, cell types `I1P`, polyhedra faces if present, as the face stream of `write_connectivity` |
| `read_polydata_cells(block, connectivity, offset, piece)` | one block of `PolyData` cells: `block` is `'verts'`, `'lines'`, `'strips'` or `'polys'`; ids `I4P` or `I8P` |
| `get_dataarray_names(location, names, piece)` | names of the arrays of `location`, in the order of the file |
| `get_dataarray_info(location, data_name, piece, data_type, n_components, n_tuples, component_names)` | VTK type, components, tuples and component names (`ComponentName<i>`, empty if none) of an array, without reading it |
| `get_active(location, role, data_name, piece)` | name of the active array of a role (`Scalars`, ..., `GlobalIds`, ..., `ProcessIds`, case insensitive), empty if none |
| `read_dataarray(location, data_name, x, piece)` | the values of an array |

- `location` is `'node'`, `'cell'` or `'field'` (field data), case insensitive (`'row'` for the columns of a Table). `piece` counts from 1 and defaults to 1; field
  data belong to the dataset and ignore it. The outputs are allocatable: they are allocated by the reader.
- `read_dataarray` returns the values flattened, rank 1 with the components interleaved, or with shape
  `(n_components, n_tuples)` for a rank-2 output; strings (type `String`, field, point or cell data) go into a
  `character(len=:), allocatable :: x(:)` output. `Bit` arrays (vtkBitArray: one value per bit, packed MSB first in the
  binary formats) are read as integers 0 and 1, into any integer kind; `get_dataarray_info` gives their type `Bit`.
- The output kind must hold every value of the array type: `Int8` reads into `I1P`...`I8P`, `Int16` into `I2P`...`I8P`,
  `Int32` into `I4P` or `I8P`, `Int64` into `I8P`, `Float32` into `R4P` or `R8P`, `Float64` into `R8P`. The unsigned types
  read into the signed kind of the same width with the same bits (the inverse of
  [`write_dataarray_unsigned`](/guide/data#unsigned-integer-arrays)), or into any wider kind with their values; in `ascii`
  too (`UInt64` values above `huge(I8P)` included). Any other kind is an
  error: query the type first with `get_dataarray_info`.
- The cells ids (`read_connectivity`, `read_polydata_cells`) can be of any integer type in the file: VTK writes `Int64` ids,
  which read into `I4P` as long as the values fit.
- The faces of the polyhedra are read from the `faces`/`faceoffsets` arrays VTKFortran writes, or from the four arrays VTK
  9.7.2 writes (`version="2.3"`: `face_connectivity`, `face_offsets`, `polyhedron_to_faces`,
  `polyhedron_offsets`; a face shared by two polyhedra stored once, `-(n+1)` for the face `n` reversed, `version="2.5"`);
  either way `read_connectivity` returns them as the face stream of `write_connectivity`.
- The format of each array (`ascii`, `binary`, raw or base64 appended), the header type and the compressor are taken from
  the file. Files written by VTK and ParaView are read as well as those written by VTKFortran.

**Errors.** As every function of the library, the reader returns 0 on success, otherwise:

| Error | Meaning |
|-------|---------|
| 1 | the file cannot be read |
| 2 | not a VTK XML file: malformed XML, no `VTKFile` element or no dataset element |
| 3 | unsupported: `BigEndian` byte order, a compressor other than zlib, a parallel or unknown dataset type |
| 4 | not found: piece, array, geometry or cells; or the file is not open for reading |
| 5 | the output kind cannot hold the values of the array |
| 6 | the data do not decode: inconsistent sizes, malformed base64, zlib failure |
| 7 | the pieces of a parallel header do not match it (`check_pieces`) |

**Memory.** `initialize` scans the file once and records where each array is; it stops at the appended data, and keeps no
data in memory. Each read then loads and decodes only the requested array: memory use is a small multiple of the size of
that array (while decoding, its text, its decoded bytes and the output coexist), whatever the size of the file.

**Not supported:** `BigEndian` files, the LZ4 and LZMA compressors, and the legacy `.vtk` format. Compressed files
need the library built with zlib, see [Installation](/guide/installation#optional-zlib-compression). The test
`src/tests/vtk_fortran_read.F90` reads every topology and format, and two files written by VTK.

### VTKHDF files

A `.vtkhdf` file (the library built with HDF5, see [Installation](/guide/installation#optional-hdf5-vtkhdf)) is read
with the same calls: `initialize(filename, action='read')` recognizes it by its HDF5 signature, and every call of
`xml_reader` answers from it. Every topology is read, polyhedra included; the pieces of an UnstructuredGrid or PolyData
are the partitions of the file. The offsets are returned as VTKFortran writes them (the end of each cell), the faces of
the polyhedra as the face stream of `write_connectivity`; `get_info` returns `header_type='HDF5'`, `compressor='none'`
(HDF5 decompresses its datasets itself). Files written by VTK's `vtkHDFWriter` are read too, string arrays included
(string datasets, variable or fixed length, as `String`: VTK 9.7.2 writes and reads them as field data only). The string
attributes
(`Type`, the `Attribute` of an active array) can be of fixed or variable length (h5py writes variable-length ones); the
temporal field data can omit `Steps/FieldDataSizes` (one tuple per step, as the specification says). The arrays of an
ImageData, RectilinearGrid or StructuredGrid have the axes of more than one point, as VTK writes and reads them; an array
keeping the axis of one point is refused (error 6), as VTK refuses it.

- **Polyhedra with shared faces** (VTKHDF 2.9): a face shared by two polyhedra is stored once, and the polyhedron that
  traverses it in reverse lists the face `n` as `-(n+1)` in `PolyhedronToFaces`. The reader gives the face in the stream
  of each polyhedron that uses it, its points in reverse order where it is marked so: the stream is the one
  `write_connectivity` takes, valid for every format. A face id out of the faces of the piece is error 6 (the data do
  not decode).

- **Time series**: `get_steps(times)` returns the times of the steps (none for a file that is not a time series), and
  `set_step(step)` selects the step that the following calls read (the step 1 by default). The pieces are those of the
  step. A step that stores nothing new for its mesh or an array (the offsets of the step before repeated) reads them
  as if written in full.
- **Table** (a `.vtt` reads the same way: its columns those of its `RowData`, their tuples its `NumberOfRows`):
  `get_info` returns `mesh_topology='Table'` and `npieces=0` (`read_piece` returns 4); its columns are the
  arrays of `location='row'` (`get_dataarray_names`, `get_dataarray_info`, `read_dataarray`), the rows of the step read
  in a time series. A column of a time series without its `Steps/RowDataOffsets` entry is error 6 (VTK 9.7.2 cannot read
  it either). A complete program: [A time history (Table)](/manual/cookbook#a-time-history-table).
- **OverlappingAMR**: `get_levels(n_levels)` returns its levels and `set_level(level)` (from 0, level 0 by default)
  selects the one the following calls read: `get_info` returns the number of its boxes (`npieces`), its `spacing` and
  the `origin`; `read_piece(piece, nx1, ..., nz2, np, nc)` the point extent and counts of a box; `read_dataarray(...,
  piece)` its arrays (of the step read in a time series). A complete program:
  [A block-structured AMR](/manual/cookbook#a-block-structured-amr).
- **HyperTreeGrid**: its partitions are the pieces (`get_info` `npieces`: one for the files VTKFortran writes; VTK
  writes several for a `vtkPartitionedDataSet` of HyperTreeGrids). `get_info` gives the extent of the root grid (its
  points), `read_geo(..., piece)` the coordinates of the partition (as a RectilinearGrid);
  `get_trees(tree_ids, branch_factor, piece, interface_normals, interface_intercepts)` the ids of its trees (their root
  cells, i + (nx-1)(j + (ny-1)k) from 0; a file with `TransposedRootIndexing`, z fastest, is converted to these ids, as
  VTK places its trees) in the order of the file, the branch factor and the names of its interface arrays (empty
  without); `read_tree(tree_id, descriptor, mask, piece)` a tree as `write_tree` takes it (the
  refinement of its cells breadth first, every depth but the deepest; `mask` one value per cell, all false when the file
  has none), error 4 for a root cell without tree; `read_dataarray(location='cell', ..., piece)` the cells of the trees of
  the partition, in their order, breadth first in each (no point data); `get_dataarray_names`, `get_active` and
  `read_piece` (`nc` the cells of the partition, `np` the points of the root grid) as for any dataset. In a time series,
  those of the step read. A complete program: [A tree-based AMR](/manual/cookbook#a-tree-based-amr).
- **A dataset of a composite file** (a VTKHDF `MultiBlockDataSet` or `PartitionedDataSetCollection`):
  `initialize(filename, action='read', dataset=path)`, `path` the group of the dataset under `/VTKHDF` (`Block_3`, or
  its name in a collection), as `vtm_file%get_entries` gives it in `file` (see
  [Multi-block and time series files](#multi-block-and-time-series-files)); the dataset is then read as a `.vtkhdf` of
  its own, time series included. Without `dataset` a composite is error 3, a missing dataset error 4; `dataset` with an
  XML file is error 3.
- **Memory**: each read is an HDF5 hyperslab of the piece and step asked for: memory use is about the size of the array
  read.
- `finalize` closes the file (`free` alone forgets it without closing it: call `finalize`).
- **Not supported**: a multi-block VTKHDF file (`vtm_file` with `format='vtkhdf'`), and `get_sources` /
  `check_pieces` (a VTKHDF file is not a parallel header). The test `src/tests/vtk_fortran_read_vtkhdf.f90` reads every
  topology and time series back, and a file of two polyhedra sharing a face; `src/tests/vtk_fortran_write_vtkhdf_table.f90`
  the Tables, `src/tests/vtk_fortran_write_vtkhdf_amr.f90` the OverlappingAMRs.

### Parallel headers

A parallel header (`.pvti`, `.pvtr`, `.pvts`, `.pvtu`, `.pvtp`, `.pvtt`) is read with `pvtk_file`, through the same `xml_reader`: it
holds no data, only the declaration of the arrays and the list of the pieces, each one a serial file read with `vtk_file`.

<<< @/examples/snippets/ref_reading-parallel.f90

- `get_info` returns the dataset type (e.g. `PUnstructuredGrid`), the number of pieces, the whole extent and `ghost_level`;
  `read_piece(piece, nx1, ...)` the extent of a piece of a structured grid; `get_dataarray_info` the type and components of
  a declared array. `read_dataarray`, `read_geo` and the cells readers return 4: the data are in the pieces.
- `get_sources` returns the files as written in the header; relative paths are relative to the directory of the header.
- `check_pieces` reads the index of every piece (not its data) and checks that it is a dataset of the type of the header,
  with coordinates of the declared type, and that each of its pieces holds every declared point and cell array with the
  same type and number of components (a piece can hold more arrays; the columns of a `.pvtt`, `location='row'`). It
  returns 7 and describes the first mismatch in
  `message`, e.g. `piece 2 (part_02.vtu): node array "pressure" declared but missing`: VTK readers would otherwise drop the
  array, fill it with zeros or fail. A piece that cannot be read returns its own error (1, 2, 3).

### Multi-block and time series files

The entries of a `.vtm` file and the datasets of a `.pvd` file are read with `vtm_file` and `pvd_file`; each dataset file is
then read with `vtk_file` (or `pvtk_file`). Relative files are relative to the directory of the `.vtm` or `.pvd` file.

<<< @/examples/snippets/ref_reading-composite.f90

- `get_entries` flattens the hierarchy depth first: `level` is the nesting level (1 for the children of the root), `kind` is
  `block`, `dataset`, or `piece` (a `vtkMultiPieceDataSet`, written by VTK or by `write_block(..., action='pieces')`, its
  `DataSet`s at the next level), `index` the index among the
  siblings, `file` empty for blocks. The children of an entry are the entries that follow it with the next level.
- `get_datasets` returns the attributes of each `DataSet` of the collection; missing ones are 0 (`timestep`, `part`) or empty.
- An XML `.vtpc` (vtkPartitionedDataSetCollection) is read by `vtm_file` too: the nodes of its `DataAssembly` are the
  blocks, its `dataset` nodes the `Partitions` of that index (the `file` of its `DataSet`; a `Partitions` of several
  `DataSet`s is a dataset without file followed by its files as `piece`s), then the `Partitions` the assembly does not
  reach, at the top level; a `.vtpd` (vtkPartitionedDataSet) gives its `DataSet`s as datasets.
- A VTKHDF `MultiBlockDataSet` or `PartitionedDataSetCollection` (`.vtkhdf`) is read by `vtm_file` too: its entries are
  the groups of its `Assembly` (`block`) and the links in them (`dataset`, a collection's references included), in the
  order written, then, for a collection, the datasets (groups of `/VTKHDF` with an `Index`) that no link of the
  `Assembly` reaches, at the top level in the order of their `Index` (`vtkHDFWriter` writes such files); the `file` of a
  dataset is its group under `/VTKHDF` (empty for an empty dataset, a group without `Type`: VTK's null block), read with
  `vtk_file%initialize(filename, action='read', dataset=file)`. A complete program:
  [A time series of several meshes in one .vtkhdf file](/manual/cookbook#a-time-series-of-several-meshes-in-one-vtkhdf-file).
- All outputs are optional and allocatable; strings are blank padded to the longest one. `initialize` returns 1 if the file
  cannot be read, 2 if it is not a multi-block file or a collection.
