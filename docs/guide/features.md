---
title: Features
---

# Features

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

Files are read with `initialize(filename=..., action='read')`, see [Usage](/guide/usage#reading-files): every
format (`ascii`, `binary`, `raw`, `binary-appended`), UInt32 and UInt64 headers, zlib compressed or not, as written by
VTKFortran or by VTK and ParaView. Only the array asked for is loaded and decoded.

| Type | Extension | Status |
|------|-----------|--------|
| Serial datasets | `.vti`, `.vtp`, `.vtr`, `.vts`, `.vtu` | ✅ |
| Parallel (partitioned) headers, with the check of the pieces | `.pvti`, `.pvtp`, `.pvtr`, `.pvts`, `.pvtu` | ✅ |
| Multi-block entries and time series datasets | `.vtm`, `.pvd` | ✅ |

Not supported: `BigEndian` files and the LZ4 and LZMA compressors.

## VTK Legacy Exporters

The legacy (`.vtk`) format is not supported: VTKFortran writes the VTK XML formats only. Legacy writers were part of the old
`Lib_VTK_IO` (VTKFortran 1.x) and were dropped with the OOP refactoring.

## Output Formats

| Format | Description |
|--------|-------------|
| `ascii` | Human-readable text inside XML elements |
| `binary` | Base64-encoded binary inside XML elements |
| `raw` | Raw binary in the XML appended section (with byte offsets) |
| `binary-appended` | Base64-encoded binary in the XML appended section |
| `raw-zlib` | Shorthand for `raw` with `compressor='zlib'` |

The format string passed to `initialize` is case-insensitive. Binary arrays are prefixed by a UInt32 bytes count by default
(2 GiB per array); `header_type='UInt64'` lifts the limit, see [Usage](/guide/usage#large-data-arrays-uint64-headers).

### Compression

The three binary formats (`binary`, `raw`, `binary-appended`) can be zlib-compressed with `compressor='zlib'`, the layout
VTK and ParaView write (`vtkZLibDataCompressor`, 32 KiB blocks): the encoded arrays are byte-identical to VTK's own writer at
the same compression level. zlib is optional: it needs the library built with `VTKFORTRAN_USE_ZLIB`, see
[Usage](/guide/usage#compressed-binary-data-zlib) and [Installation](/guide/installation#optional-zlib-compression).

## Global Field Data

Optional simulation metadata (time, cycle number, solver name, residuals history, etc.) can be attached before the first piece
via `write_fielddata`: scalars and rank-1 arrays of all PENF kinds, strings and arrays of strings.

```fortran
error = a_vtk_file%xml_writer%write_fielddata(action='open')
error = a_vtk_file%xml_writer%write_fielddata(x=0._R8P, data_name='TIME')
error = a_vtk_file%xml_writer%write_fielddata(x=1_I8P,  data_name='CYCLE')
error = a_vtk_file%xml_writer%write_fielddata(x=residuals, data_name='residuals')  ! rank-1 array
error = a_vtk_file%xml_writer%write_fielddata(x='my solver v1.2', data_name='solver')
error = a_vtk_file%xml_writer%write_fielddata(action='close')
```

See [Usage](/guide/usage#field-data-global-metadata) for details.

## Data Arrays

`write_dataarray` is a heavily overloaded interface that accepts:

- **All PENF numeric kinds**: `R8P`, `R4P`, `I8P`, `I4P`, `I2P`, `I1P`
- **Ranks 1–4** for dense arrays
- **Scalar, 1-component, 3-component (vector), and 6-component (symmetric tensor)** layouts
- **Node-centered or cell-centered** placement (`location='node'` or `location='cell'`)
- **Active arrays**: the arrays readers use by default for each role (`Scalars`, `Vectors`, `Normals`, `Tensors`, `TCoords`) can be
  designated when opening the node/cell data, see [Usage](/guide/usage#active-arrays)
- **Unsigned integers** (`UInt8`, `UInt16`, `UInt32`, `UInt64`, e.g. `vtkGhostType`) with `write_dataarray_unsigned`, see
  [Usage](/guide/usage#unsigned-integer-arrays)

## Parallel Support

VTKFortran can safely manage multiple concurrent open files: each `vtk_file` (writer or reader) keeps its state in its own
components, so files can be written or read concurrently within OpenMP parallel regions and by MPI programs where each rank
writes its own partition file. The dependencies PENF and BeFoR64 initialize their constant tables at the first
`initialize`; in OpenMP, initialize one file (or call `penf_init` and `b64_init`) before the parallel region, so that two
threads never run that initialization at the same time.

## Compiler Support

| Compiler | Status |
|----------|--------|
| GNU gfortran 12, 13, 14, 16 | ✅ Tested (the CI uses gfortran 14) |
| GNU gfortran 11 | ⚠️ Builds; the string form of `vtm_file%write_block` crashes (in StringiFor `split`) |
| Intel ifx 2025.3 | ✅ Tested (the discontinued ifort is not tested) |
| NAG, NVIDIA, Cray, LLVM Flang | Not tested |

## Design Principles

- **Pure Fortran** — no system calls beyond standard I/O; the only C library is zlib, optional, for compressed data
- **OOP** — polymorphic `xml_writer` allocated at runtime, an `xml_reader` to read files back; `vtk_file`, `pvtk_file`,
  `vtm_file`, `pvd_file` expose type-bound procedures
- **KISS** — simple, focused API without unnecessary abstractions
- **Error codes** — every procedure returns an integer; zero means success
- **Free & Open Source** — multi-licensed for FOSS and commercial use
