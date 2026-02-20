---
title: Features
---

# Features

## VTK XML Exporters

### Serial datasets

| Topology | Extension | Status |
|----------|-----------|--------|
| Image Data | `.vti` | — |
| Polydata | `.vtp` | — |
| Rectilinear Grid | `.vtr` | ✅ |
| Structured Grid | `.vts` | ✅ |
| Unstructured Grid | `.vtu` | ✅ |

### Parallel (partitioned) datasets

| Topology | Extension | Status |
|----------|-----------|--------|
| Parallel Image Data | `.pvti` | — |
| Parallel Polydata | `.pvtp` | — |
| Parallel Rectilinear Grid | `.pvtr` | — |
| Parallel Structured Grid | `.pvts` | ✅ |
| Parallel Unstructured Grid | `.pvtu` | — |

### Composite datasets

| Type | Extension | Status |
|------|-----------|--------|
| vtkMultiBlockDataSet | `.vtm` | ✅ |

## VTK Legacy Exporters

| Topology | Status |
|----------|--------|
| Structured Points | ✅ |
| Structured Grid | ✅ |
| Unstructured Grid | ✅ |
| Rectilinear Grid | ✅ |
| Polydata | — |
| Field | — |

## Output Formats

| Format | Description |
|--------|-------------|
| `ascii` | Human-readable text inside XML elements |
| `binary` | Base64-encoded binary inside XML elements |
| `raw` | Raw binary in the XML appended section (with byte offsets) |
| `binary-appended` | Base64-encoded binary in the XML appended section |

The format string passed to `initialize` is case-insensitive.

## Global Field Data

Optional simulation metadata (time, cycle number, dataset name, etc.) can be attached before the first piece via `write_fielddata`:

```fortran
error = a_vtk_file%xml_writer%write_fielddata(action='open')
error = a_vtk_file%xml_writer%write_fielddata(x=0._R8P, data_name='TIME')
error = a_vtk_file%xml_writer%write_fielddata(x=1_I8P,  data_name='CYCLE')
error = a_vtk_file%xml_writer%write_fielddata(action='close')
```

## Data Arrays

`write_dataarray` is a heavily overloaded interface that accepts:

- **All PENF numeric kinds**: `R8P`, `R4P`, `I8P`, `I4P`, `I2P`, `I1P`
- **Ranks 1–4** for dense arrays
- **Scalar, 1-component, 3-component (vector), and 6-component (symmetric tensor)** layouts
- **Node-centered or cell-centered** placement (`location='node'` or `location='cell'`)

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
