---
title: API Reference
---

# API Reference

Auto-generated from Fortran source doc comments using [FORMAL](https://github.com/szaghi/formal).

## src/lib

- [vtk_fortran](/api/src/lib/vtk_fortran) — main module; re-exports `vtk_file`, `pvtk_file`, `vtm_file`, `write_xml_volatile`
- [vtk_fortran_vtk_file](/api/src/lib/vtk_fortran_vtk_file) — `vtk_file` derived type (serial writer)
- [vtk_fortran_pvtk_file](/api/src/lib/vtk_fortran_pvtk_file) — `pvtk_file` derived type (parallel/partitioned writer)
- [vtk_fortran_vtm_file](/api/src/lib/vtk_fortran_vtm_file) — `vtm_file` derived type (multi-block composite writer)
- [vtk_fortran_vtk_file_xml_writer_abstract](/api/src/lib/vtk_fortran_vtk_file_xml_writer_abstract) — abstract base class defining the common writer interface
- [vtk_fortran_vtk_file_xml_writer_ascii_local](/api/src/lib/vtk_fortran_vtk_file_xml_writer_ascii_local) — ASCII writer
- [vtk_fortran_vtk_file_xml_writer_binary_local](/api/src/lib/vtk_fortran_vtk_file_xml_writer_binary_local) — Base64-encoded binary writer
- [vtk_fortran_vtk_file_xml_writer_appended](/api/src/lib/vtk_fortran_vtk_file_xml_writer_appended) — raw binary appended writer
- [vtk_fortran_dataarray_encoder](/api/src/lib/vtk_fortran_dataarray_encoder) — encoding routines for ASCII and Base64 data arrays
- [vtk_fortran_parameters](/api/src/lib/vtk_fortran_parameters) — shared constants

## Key type-bound procedures

### `vtk_file` / `pvtk_file`

| Procedure | Description |
|-----------|-------------|
| `%initialize(format, filename, mesh_topology, ...)` | Open the file, select the writer, write the XML header |
| `%finalize()` | Flush and close the file |
| `%xml_writer%write_fielddata(...)` | Write global FieldData (time, cycle, etc.) |
| `%xml_writer%write_piece(...)` | Open or close a Piece element |
| `%xml_writer%write_geo(...)` | Write geometry (coordinates) |
| `%xml_writer%write_connectivity(...)` | Write unstructured connectivity, offsets, and cell types |
| `%xml_writer%write_dataarray(...)` | Write a data array (overloaded for all kinds and ranks) |
| `%xml_writer%write_parallel_geo(...)` | Write a `<P*>` geometry piece reference (pvtk_file only) |
| `%xml_writer%write_parallel_dataarray(...)` | Write a parallel data array descriptor (pvtk_file only) |

### `vtm_file`

| Procedure | Description |
|-----------|-------------|
| `%initialize(filename)` | Create the `.vtm` wrapper file |
| `%write_block(filenames, names, name)` | Add a named block referencing one or more partition files |
| `%finalize()` | Close the `.vtm` file |

### `write_xml_volatile`

A module-level function (not a type-bound procedure) that returns the full XML output as an allocatable character string instead of writing to disk. Useful for parallel I/O workflows where the calling code controls file access.
