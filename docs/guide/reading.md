---
title: Reading files
---

# Reading files

Read back every file VTKFortran writes, and the same files written by VTK. Complete programs: the
[cookbook](/manual/cookbook#read-files) and [chapter 8](/manual/tutorial/08-restart) of the tutorial.

## Serial files

A serial file (`.vti`, `.vtr`, `.vts`, `.vtu`, `.vtp`) is read by initializing a `vtk_file` with `action='read'`: the file is
indexed, and its data are then read, array by array, through the `xml_reader` component.

```fortran
use vtk_fortran, only : vtk_file
use penf
type(vtk_file)                :: a_vtk_file
character(len=:), allocatable :: topology, names(:)
real(R8P),        allocatable :: x(:), y(:), z(:), pressure(:), velocity(:,:)
integer(I4P),     allocatable :: connectivity(:), offset(:)
integer(I1P),     allocatable :: cell_type(:)
integer(I8P)                  :: np, nc
integer(I4P)                  :: error

error = a_vtk_file%initialize(filename='mesh.vtu', action='read')
error = a_vtk_file%xml_reader%get_info(mesh_topology=topology)
error = a_vtk_file%xml_reader%read_piece(np=np, nc=nc)
error = a_vtk_file%xml_reader%read_geo(x=x, y=y, z=z)
error = a_vtk_file%xml_reader%read_connectivity(connectivity=connectivity, offset=offset, cell_type=cell_type)
error = a_vtk_file%xml_reader%get_dataarray_names(location='node', names=names)
error = a_vtk_file%xml_reader%read_dataarray(location='node', data_name='pressure', x=pressure)
error = a_vtk_file%xml_reader%read_dataarray(location='node', data_name='velocity', x=velocity) ! shape (3, np)
error = a_vtk_file%finalize()
```

| Procedure | Returns |
|-----------|---------|
| `get_info(mesh_topology, npieces, header_type, compressor, nx1...nz2, origin, spacing, direction)` | dataset type, number of pieces, header type and compressor of the binary data; whole extent of structured grids; origin, spacing and direction of `ImageData` |
| `read_piece(piece, np, nc, nx1...nz2, nverts, nlines, nstrips, npolys)` | counts (`I8P`) and extent of a piece |
| `read_geo(x, y, z, piece)` | point coordinates (`StructuredGrid`, `UnstructuredGrid`, `PolyData`) or the coordinates along each axis (`RectilinearGrid`), `R8P` or `R4P`; `ImageData` has no stored geometry, see `get_info` |
| `read_connectivity(connectivity, offset, cell_type, face, faceoffset, piece)` | cells of an `UnstructuredGrid`, ids `I4P` or `I8P`, cell types `I1P`, polyhedra faces if present |
| `read_polydata_cells(block, connectivity, offset, piece)` | one block of `PolyData` cells: `block` is `'verts'`, `'lines'`, `'strips'` or `'polys'`; ids `I4P` or `I8P` |
| `get_dataarray_names(location, names, piece)` | names of the arrays of `location`, in the order of the file |
| `get_dataarray_info(location, data_name, piece, data_type, n_components, n_tuples)` | VTK type, components and tuples of an array, without reading it |
| `read_dataarray(location, data_name, x, piece)` | the values of an array |

- `location` is `'node'`, `'cell'` or `'field'` (field data), case insensitive. `piece` counts from 1 and defaults to 1; field
  data belong to the dataset and ignore it. The outputs are allocatable: they are allocated by the reader.
- `read_dataarray` returns the values flattened, rank 1 with the components interleaved, or with shape
  `(n_components, n_tuples)` for a rank-2 output; field data strings (type `String`) go into a
  `character(len=:), allocatable :: x(:)` output.
- The output kind must hold every value of the array type: `Int8` reads into `I1P`...`I8P`, `Int16` into `I2P`...`I8P`,
  `Int32` into `I4P` or `I8P`, `Int64` into `I8P`, `Float32` into `R4P` or `R8P`, `Float64` into `R8P`. The unsigned types
  read into the signed kind of the same width with the same bits (the inverse of
  [`write_dataarray_unsigned`](/guide/data#unsigned-integer-arrays)), or into any wider kind with their values. Any other kind is an
  error: query the type first with `get_dataarray_info`.
- The cells ids (`read_connectivity`, `read_polydata_cells`) can be of any integer type in the file: VTK writes `Int64` ids,
  which read into `I4P` as long as the values fit.
- The format of each array (`ascii`, `binary`, raw or base64 appended), the header type and the compressor are taken from
  the file. Files written by VTK and ParaView are read as well as those written by VTKFortran.

**Errors.** As every procedure, the reader returns 0 on success, otherwise:

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

**Not supported:** `BigEndian` files, the LZ4 and LZMA compressors, the legacy `.vtk` format, and VTKHDF. Compressed files
need the library built with zlib, see [Installation](/guide/installation#optional-zlib-compression). The test
`src/tests/vtk_fortran_read.F90` reads every topology and format, and two files written by VTK.

### Parallel headers

A parallel header (`.pvti`, `.pvtr`, `.pvts`, `.pvtu`, `.pvtp`) is read with `pvtk_file`, through the same `xml_reader`: it
holds no data, only the declaration of the arrays and the list of the pieces, each one a serial file read with `vtk_file`.

```fortran
type(pvtk_file)               :: a_pvtk_file
character(len=:), allocatable :: sources(:), names(:), message

error = a_pvtk_file%initialize(filename='mesh.pvtu', action='read')
error = a_pvtk_file%xml_reader%get_sources(sources)                          ! files of the pieces
error = a_pvtk_file%xml_reader%get_dataarray_names(location='node', names=names) ! declared point arrays
error = a_pvtk_file%xml_reader%check_pieces(message=message)                 ! 0, or 7 and the first mismatch
error = a_pvtk_file%finalize()
```

- `get_info` returns the dataset type (e.g. `PUnstructuredGrid`), the number of pieces, the whole extent and `ghost_level`;
  `read_piece(piece, nx1, ...)` the extent of a piece of a structured grid; `get_dataarray_info` the type and components of
  a declared array. `read_dataarray`, `read_geo` and the cells readers return 4: the data are in the pieces.
- `get_sources` returns the files as written in the header; relative paths are relative to the directory of the header.
- `check_pieces` reads the index of every piece (not its data) and checks that it is a dataset of the type of the header,
  with coordinates of the declared type, and that each of its pieces holds every declared point and cell array with the
  same type and number of components (a piece can hold more arrays). It returns 7 and describes the first mismatch in
  `message`, e.g. `piece 2 (part_02.vtu): node array "pressure" declared but missing`: VTK readers would otherwise drop the
  array, fill it with zeros or fail. A piece that cannot be read returns its own error (1, 2, 3).

### Multi-block and time series files

The entries of a `.vtm` file and the datasets of a `.pvd` file are read with `vtm_file` and `pvd_file`; each dataset file is
then read with `vtk_file` (or `pvtk_file`). Relative files are relative to the directory of the `.vtm` or `.pvd` file.

```fortran
type(vtm_file)                :: a_vtm_file
type(pvd_file)                :: a_pvd_file
integer(I4P),     allocatable :: level(:), index(:)
character(len=:), allocatable :: kind(:), name(:), file(:)
real(R8P),        allocatable :: timestep(:)

error = a_vtm_file%initialize(filename='assembly.vtm', action='read')
error = a_vtm_file%get_entries(level=level, kind=kind, index=index, name=name, file=file)
error = a_vtm_file%finalize()

error = a_pvd_file%initialize(filename='simulation.pvd', action='read')
error = a_pvd_file%get_datasets(timestep=timestep, file=file)  ! also part, group, name
error = a_pvd_file%finalize()
```

- `get_entries` flattens the hierarchy depth first: `level` is the nesting level (1 for the children of the root), `kind` is
  `block`, `dataset`, or `piece` (the pieces of a `vtkMultiPieceDataSet` written by VTK), `index` the index among the
  siblings, `file` empty for blocks. The children of an entry are the entries that follow it with the next level.
- `get_datasets` returns the attributes of each `DataSet` of the collection; missing ones are 0 (`timestep`, `part`) or empty.
- All outputs are optional and allocatable; strings are blank padded to the longest one. `initialize` returns 1 if the file
  cannot be read, 2 if it is not a multi-block file or a collection.
