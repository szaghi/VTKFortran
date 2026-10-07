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

## Parallel Support

VTKFortran can safely manage multiple concurrent open files. It is thread/processor safe, suitable for use within OpenMP parallel regions and MPI programs where each rank writes its own partition file.

## Compiler Support

| Compiler | Status |
|----------|--------|
| GNU gfortran ≥ 6.0.1 | ✅ Supported |
| Intel Fortran ≥ 16.x | ✅ Supported |
| IBM XL Fortran | Not tested |
| g95 | Not tested |
| NAG Fortran | Not tested |
| PGI / NVIDIA | Not tested |

## Design Principles

- **Pure Fortran** — no external C libraries or system calls beyond standard I/O
- **OOP** — polymorphic `xml_writer` allocated at runtime; `vtk_file`, `pvtk_file`, `vtm_file` expose type-bound procedures
- **KISS** — simple, focused API without unnecessary abstractions
- **Error codes** — every procedure returns an integer; zero means success
- **Free & Open Source** — multi-licensed for FOSS and commercial use
