---
title: API Reference
---

# API Reference

Auto-generated from Fortran source doc comments using [FORMAL](https://github.com/szaghi/formal).

## src/lib

- [vtk_fortran](/api/src/lib/vtk_fortran) — main module; re-exports `vtk_file`, `pvtk_file`, `vtm_file`, `pvd_file`, `write_xml_volatile`
- [vtk_fortran_vtk_file](/api/src/lib/vtk_fortran_vtk_file) — `vtk_file` derived type (serial writer)
- [vtk_fortran_pvtk_file](/api/src/lib/vtk_fortran_pvtk_file) — `pvtk_file` derived type (parallel/partitioned writer)
- [vtk_fortran_vtm_file](/api/src/lib/vtk_fortran_vtm_file) — `vtm_file` derived type (multi-block composite writer)
- [vtk_fortran_pvd_file](/api/src/lib/vtk_fortran_pvd_file) — `pvd_file` derived type (time series collection writer)
- [vtk_fortran_vtk_file_xml_writer_abstract](/api/src/lib/vtk_fortran_vtk_file_xml_writer_abstract) — abstract base class defining the common writer interface
- [vtk_fortran_vtk_file_xml_writer_ascii_local](/api/src/lib/vtk_fortran_vtk_file_xml_writer_ascii_local) — ASCII writer
- [vtk_fortran_vtk_file_xml_writer_binary_local](/api/src/lib/vtk_fortran_vtk_file_xml_writer_binary_local) — Base64-encoded binary writer
- [vtk_fortran_vtk_file_xml_writer_appended](/api/src/lib/vtk_fortran_vtk_file_xml_writer_appended) — raw binary appended writer
- [vtk_fortran_dataarray_encoder](/api/src/lib/vtk_fortran_dataarray_encoder) — encoding routines for ASCII and Base64 data arrays (optionally zlib-compressed)
- [vtk_fortran_zlib](/api/src/lib/vtk_fortran_zlib) — zlib bindings, VTK block compression and decompression (stubs when built without `VTKFORTRAN_USE_ZLIB`)
- [vtk_fortran_parameters](/api/src/lib/vtk_fortran_parameters) — shared constants

## Key type-bound procedures

### `vtk_file` / `pvtk_file`

| Procedure | Description |
|-----------|-------------|
| `%initialize(format, filename, mesh_topology, ...)` | Open the file, select the writer, write the XML header; for `ImageData`/`PImageData` also `origin`, `spacing` (required) and `direction`; `header_type='UInt64'` for binary arrays larger than 2 GiB; `compressor='zlib'` to compress binary data (`vtk_file` only) |
| `%finalize()` | Flush and close the file |
| `%xml_writer%write_fielddata(...)` | Write global FieldData: scalars, rank-1 arrays (all kinds), strings and arrays of strings |
| `%xml_writer%write_piece(...)` | Open or close a Piece element (extents, `np`/`nc`, or `np`/`nverts`/`nlines`/`nstrips`/`npolys` for PolyData; counts `I4P` or `I8P`) |
| `%xml_writer%write_geo(...)` | Write geometry (coordinates; unstructured `np`/`nc` counts `I4P` or `I8P`) |
| `%xml_writer%write_connectivity(...)` | Write unstructured connectivity, offsets, and cell types (ids `I4P` or `I8P`) |
| `%xml_writer%write_polydata_cells(...)` | Write the cell blocks of PolyData: vertices, lines, triangle strips, polygons (ids `I4P` or `I8P`) |
| `%xml_writer%write_dataarray(...)` | Write a data array (overloaded for all kinds and ranks) |
| `%xml_writer%write_dataarray_unsigned(data_name, x)` | Write a rank-1 integer array as unsigned (`I1P`…`I8P` as `UInt8`…`UInt64`) |
| `%xml_writer%write_parallel_geo(...)` | Write a `<P*>` geometry piece reference (pvtk_file only) |
| `%xml_writer%write_parallel_dataarray(...)` | Write a parallel data array descriptor (pvtk_file only) |

### `vtm_file`

| Procedure | Description |
|-----------|-------------|
| `%initialize(filename)` | Create the `.vtm` wrapper file |
| `%write_block(filenames, names, name)` | Add a named block referencing one or more partition files |
| `%finalize()` | Close the `.vtm` file |

### `pvd_file`

| Procedure | Description |
|-----------|-------------|
| `%initialize(filename, action)` | Create the `.pvd` collection (`action='new'`, default) or reopen it to add steps (`action='append'`) |
| `%write_dataset(filename, timestep, part, group, name)` | Add a dataset (file) with its time step; the collection is valid after each call |
| `%finalize()` | Close the `.pvd` file |

### `write_xml_volatile`

A module-level function (not a type-bound procedure) that returns the full XML output as an allocatable character string instead of writing to disk. Useful for parallel I/O workflows where the calling code controls file access.
