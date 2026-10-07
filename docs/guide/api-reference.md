---
title: API Reference
---

# API Reference

Auto-generated from Fortran source doc comments using [FORMAL](https://github.com/szaghi/formal).

## src/lib

- [vtk_fortran](/api/src/lib/vtk_fortran) — main module; re-exports `vtk_file`, `pvtk_file`, `vtm_file`, `pvd_file`, `write_xml_volatile`
- [vtk_fortran_vtk_file](/api/src/lib/vtk_fortran_vtk_file) — `vtk_file` derived type (serial writer and reader)
- [vtk_fortran_pvtk_file](/api/src/lib/vtk_fortran_pvtk_file) — `pvtk_file` derived type (parallel/partitioned writer)
- [vtk_fortran_vtm_file](/api/src/lib/vtk_fortran_vtm_file) — `vtm_file` derived type (multi-block composite writer)
- [vtk_fortran_pvd_file](/api/src/lib/vtk_fortran_pvd_file) — `pvd_file` derived type (time series collection writer)
- [vtk_fortran_vtk_file_xml_writer_abstract](/api/src/lib/vtk_fortran_vtk_file_xml_writer_abstract) — abstract base class defining the common writer interface
- [vtk_fortran_vtk_file_xml_writer_ascii_local](/api/src/lib/vtk_fortran_vtk_file_xml_writer_ascii_local) — ASCII writer
- [vtk_fortran_vtk_file_xml_writer_binary_local](/api/src/lib/vtk_fortran_vtk_file_xml_writer_binary_local) — Base64-encoded binary writer
- [vtk_fortran_vtk_file_xml_writer_appended](/api/src/lib/vtk_fortran_vtk_file_xml_writer_appended) — raw binary appended writer
- [vtk_fortran_vtk_file_xml_reader](/api/src/lib/vtk_fortran_vtk_file_xml_reader) — reader of serial files (`vtk_file%xml_reader`), with its error codes
- [vtk_fortran_xml_scanner](/api/src/lib/vtk_fortran_xml_scanner) — XML scanner: indexes the elements of a file without loading it
- [vtk_fortran_dataarray_decoder](/api/src/lib/vtk_fortran_dataarray_decoder) — decoding routines for ASCII and Base64 data arrays (optionally zlib-compressed)
- [vtk_fortran_dataarray_encoder](/api/src/lib/vtk_fortran_dataarray_encoder) — encoding routines for ASCII and Base64 data arrays (optionally zlib-compressed)
- [vtk_fortran_zlib](/api/src/lib/vtk_fortran_zlib) — zlib bindings, VTK block compression and decompression (stubs when built without `VTKFORTRAN_USE_ZLIB`)
- [vtk_fortran_parameters](/api/src/lib/vtk_fortran_parameters) — shared constants

## Key type-bound procedures

### `vtk_file` / `pvtk_file`

| Procedure | Description |
|-----------|-------------|
| `%initialize(format, filename, mesh_topology, ...)` | Open the file, select the writer, write the XML header; for `ImageData`/`PImageData` also `origin`, `spacing` (required) and `direction`; `header_type='UInt64'` for binary arrays larger than 2 GiB; `compressor='zlib'` to compress binary data (`vtk_file` only) |
| `%initialize(filename=..., action='read')` | Open a file for reading through `%xml_reader`: a serial file (`vtk_file`) or a parallel header (`pvtk_file`), see below |
| `%finalize()` | Flush and close the file (or free the reader) |
| `%get_xml_volatile(xml_volatile)`, `%free()` | Return the file written with `is_volatile=.true.` as a string, then free its memory (`vtk_file` only) |
| `%xml_writer%write_fielddata(...)` | Write global FieldData: scalars, rank-1 arrays (all kinds), strings and arrays of strings |
| `%xml_writer%write_piece(...)` | Open or close a Piece element (extents, `np`/`nc`, or `np`/`nverts`/`nlines`/`nstrips`/`npolys` for PolyData; counts `I4P` or `I8P`) |
| `%xml_writer%write_geo(...)` | Write geometry (coordinates; unstructured `np`/`nc` counts `I4P` or `I8P`) |
| `%xml_writer%write_connectivity(...)` | Write unstructured connectivity, offsets, and cell types (ids `I4P` or `I8P`) |
| `%xml_writer%write_polydata_cells(...)` | Write the cell blocks of PolyData: vertices, lines, triangle strips, polygons (ids `I4P` or `I8P`) |
| `%xml_writer%write_dataarray(...)` | Write a data array (overloaded for all kinds and ranks) |
| `%xml_writer%write_dataarray_unsigned(data_name, x)` | Write a rank-1 integer array as unsigned (`I1P`…`I8P` as `UInt8`…`UInt64`) |
| `%xml_writer%write_parallel_geo(...)` | Write a `<P*>` geometry piece reference (pvtk_file only) |
| `%xml_writer%write_parallel_dataarray(...)` | Write a parallel data array descriptor (pvtk_file only) |

### `vtk_file` reading (`%xml_reader`)

| Procedure | Description |
|-----------|-------------|
| `%xml_reader%get_info(...)` | Dataset type, number of pieces, header type, compressor; whole extent; `ImageData` origin, spacing, direction |
| `%xml_reader%read_piece(...)` | Counts (`I8P`) and extent of a piece |
| `%xml_reader%read_geo(x, y, z, piece)` | Point coordinates, or the axes coordinates of a `RectilinearGrid` (`R8P` or `R4P`) |
| `%xml_reader%read_connectivity(...)` | Connectivity, offsets, cell types and polyhedra faces of an `UnstructuredGrid` (ids `I4P` or `I8P`) |
| `%xml_reader%read_polydata_cells(block, connectivity, offset, piece)` | One block of `PolyData` cells: verts, lines, strips or polys (ids `I4P` or `I8P`) |
| `%xml_reader%get_dataarray_names(location, names, piece)` | Names of the arrays of a location (node, cell, field) |
| `%xml_reader%get_dataarray_info(...)` | VTK type, components and tuples of an array |
| `%xml_reader%read_dataarray(location, data_name, x, piece)` | Values of an array: rank 1 or `(components, tuples)`, all kinds; strings |
| `%xml_reader%get_sources(sources)` | Files of the pieces of a parallel header |
| `%xml_reader%check_pieces(message)` | Check the pieces of a parallel header against it: type, coordinates, declared arrays (error 7 and `message` on mismatch) |

### `vtm_file`

| Procedure | Description |
|-----------|-------------|
| `%initialize(filename)` | Create the `.vtm` wrapper file |
| `%initialize(filename, action='read')` | Open a `.vtm` file for reading |
| `%get_entries(level, kind, index, name, file)` | Blocks and datasets of a file read, flattened depth first |
| `%write_block(filenames, names, name, action)` | Add a named block of files; `action='open'`/`'close'` to nest blocks, `'write'` to add datasets to the current block (filenames and names as arrays or blank-separated strings) |
| `%finalize()` | Close the `.vtm` file |

### `pvd_file`

| Procedure | Description |
|-----------|-------------|
| `%initialize(filename, action)` | Create the `.pvd` collection (`action='new'`, default), reopen it to add steps (`action='append'`) or read it (`action='read'`) |
| `%get_datasets(timestep, part, group, name, file)` | Datasets of a collection read |
| `%write_dataset(filename, timestep, part, group, name)` | Add a dataset (file) with its time step; the collection is valid after each call |
| `%finalize()` | Close the `.pvd` file |

### `write_xml_volatile`

A module-level function (not a type-bound procedure), `write_xml_volatile(xml_volatile, filename)`: it writes to disk a file
held in memory, as returned by `get_xml_volatile`. Useful for parallel workflows where only one process accesses the file
system, see [Volatile XML output](/guide/parallel#volatile-xml-output).
