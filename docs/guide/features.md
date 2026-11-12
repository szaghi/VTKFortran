---
title: Features
---

# Features

## Feature map

| Topic | Reference | Complete programs |
|---|---|---|
| Image, rectilinear, structured, unstructured grids, polydata, polyhedra | [Topologies](/guide/topologies) | [cookbook](/manual/cookbook#write-each-kind-of-dataset), [tutorial 1](/manual/tutorial/01-first-file), [5](/manual/tutorial/05-unstructured) |
| Field data, vectors, tensors, active arrays, unsigned and ghost arrays | [Data arrays](/guide/data) | [cookbook](/manual/cookbook#write-data), [tutorial 3](/manual/tutorial/03-more-data) |
| ASCII, binary, raw, zlib, VTKHDF; UInt64 headers, 64-bit ids | [Formats and large data](/guide/formats) | [cookbook](/manual/cookbook#choose-the-format), [tutorial 2](/manual/tutorial/02-formats) |
| Parallel headers, multi-block, time series, volatile files | [Parallel and composite files](/guide/parallel) | [cookbook](/manual/cookbook#parallel-and-composite-files), [tutorial 4](/manual/tutorial/04-time-series), [6](/manual/tutorial/06-parallel), [7](/manual/tutorial/07-assembly) |
| Reading every file back, checking parallel pieces | [Reading files](/guide/reading) | [cookbook](/manual/cookbook#read-files), [tutorial 8](/manual/tutorial/08-restart) |
| Every procedure and its arguments | [Procedures summary](/guide/api-reference), [API](/api/) | |

## VTK XML Exporters

### Serial datasets

| Topology | Extension | Status |
|----------|-----------|--------|
| Image Data | `.vti` | ✅ |
| Polydata | `.vtp` | ✅ |
| Rectilinear Grid | `.vtr` | ✅ |
| Structured Grid | `.vts` | ✅ |
| Unstructured Grid | `.vtu` | ✅ |

### Parallel (partitioned) datasets

| Topology | Extension | Status |
|----------|-----------|--------|
| Parallel Image Data | `.pvti` | ✅ |
| Parallel Polydata | `.pvtp` | ✅ |
| Parallel Rectilinear Grid | `.pvtr` | ✅ |
| Parallel Structured Grid | `.pvts` | ✅ |
| Parallel Unstructured Grid | `.pvtu` | ✅ |

### Composite datasets

| Type | Extension | Status |
|------|-----------|--------|
| vtkMultiBlockDataSet | `.vtm` | ✅ |
| Time series (collection) | `.pvd` | ✅ |

## VTK XML Importers

Files are read with `initialize(filename=..., action='read')`, see [Reading files](/guide/reading): every
format (`ascii`, `binary`, `raw`, `binary-appended`), UInt32 and UInt64 headers, zlib compressed or not, as written by
VTKFortran or by VTK and ParaView. Only the array asked for is loaded and decoded.

| Type | Extension | Status |
|------|-----------|--------|
| Serial datasets | `.vti`, `.vtp`, `.vtr`, `.vts`, `.vtu` | ✅ |
| Parallel (partitioned) headers, with the check of the pieces | `.pvti`, `.pvtp`, `.pvtr`, `.pvts`, `.pvtu` | ✅ |
| Multi-block entries and time series datasets | `.vtm`, `.pvd` | ✅ |

Not supported: `BigEndian` files and the LZ4 and LZMA compressors.

## VTKHDF Exporters

The [VTKHDF](/guide/formats#vtkhdf-hdf5) format (`format='vtkhdf'`, one HDF5 file, extension `.vtkhdf`) needs the library
built with HDF5 ([Installation](/guide/installation#optional-hdf5-vtkhdf)). It is being added step by step
([issue #51](https://github.com/szaghi/VTKFortran/issues/51)).

| Type | Status |
|------|--------|
| Image Data, one piece: point, cell and field data, active arrays, unsigned arrays, deflate | ✅ |
| Unstructured Grid, each piece a partition: polyhedra, I4P and I8P ids, point, cell and field data, active arrays, deflate | ✅ |
| Polydata, each piece a partition: the four blocks of cells, I4P and I8P ids, point, cell and field data | ✅ |
| Rectilinear and Structured Grids, one piece: coordinates or points, point, cell and field data (read by VTK 9.7, not ParaView 6.1) | ✅ |
| Time series in one file (`Steps`): `write_step`, valid after every piece, restart with `action='append'`; a fixed mesh, unchanged geometry or arrays stored once | ✅ |
| Multi-block (`vtm_file` with `format='vtkhdf'`): the `.vtkhdf` datasets copied in, the tree of the blocks as `Assembly`; static, or time series of Unstructured Grids and Polydata | ✅ |
| Reading VTKHDF files back (`action='read'`): every topology, time series step by step | ✅ |
| One file from MPI ranks (`comm`, a parallel HDF5): Unstructured Grid and Polydata, one partition per rank, time series too (restart with `action='append'`); Image Data, Rectilinear and Structured Grids, one region per rank | ✅ |

## VTK Legacy Exporters

The legacy (`.vtk`) format is not supported: VTKFortran writes the VTK XML formats only. Legacy writers were part of the old
`Lib_VTK_IO` (VTKFortran 1.x) and were dropped with the OOP refactoring.

## Formats and data

The formats `ascii`, `binary`, `raw` and `binary-appended` (and `raw-zlib`), zlib compression of the binary ones, UInt64
headers and 64-bit ids, and `vtkhdf`: see [Formats and large data](/guide/formats). Point, cell and field data of every PENF kind
(`I1P` to `R8P`), ranks 1 to 4, scalars, vectors and tensors, unsigned arrays, active arrays: see
[Data arrays](/guide/data), and [Writing arrays](/guide/data#writing-arrays) for the shape each array is given in.

## Parallel Support

Each `vtk_file` (writer or reader) keeps its state in its own components. MPI processes write their files at the same
time with no limitation. OpenMP threads do too with the library built by ifx or nvfortran (the builds add the flags they need), but not
by gfortran, whose static variables for deferred-length string results are shared by the threads (GCC bug 113797): see
[Threads and processes](/guide/parallel#threads-and-processes).

## Compiler Support

| Compiler | Status |
|----------|--------|
| GNU gfortran 12.5, 13.4, 14.3, 16.2 | ✅ Every test passes (the CI uses gfortran 14) |
| GNU gfortran 11.5 | ⚠️ Builds; the two `.vtm` tests crash |
| Intel ifx 2025.3 | ✅ Every test passes (the discontinued ifort is not tested) |
| NVIDIA nvfortran 26.9 | ✅ Every test passes (built with CMake) |
| NAG, Cray, LLVM Flang | Not tested |

See [Installation](/guide/installation#prerequisites) for how the table is measured.

## Design Principles

- **Pure Fortran** — no system calls beyond standard I/O; the only C libraries are zlib, for compressed data, and HDF5,
  for VTKHDF, both optional
- **OOP** — polymorphic `xml_writer` allocated at runtime, an `xml_reader` to read files back; `vtk_file`, `pvtk_file`,
  `vtm_file`, `pvd_file` expose type-bound procedures
- **KISS** — simple, focused API without unnecessary abstractions
- **Error codes** — every function returns an integer status, zero on success; only a binary array overflowing its
  UInt32 header and a zlib failure stop the program, with a message
- **Free & Open Source** — multi-licensed for FOSS and commercial use
