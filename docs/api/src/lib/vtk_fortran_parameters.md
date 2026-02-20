---
title: vtk_fortran_parameters
---

# vtk_fortran_parameters

> VTK_Fortran parameters.

**Source**: `src/lib/vtk_fortran_parameters.f90`

**Dependencies**

```mermaid
graph LR
  vtk_fortran_parameters["vtk_fortran_parameters"] --> iso_fortran_env["iso_fortran_env"]
```

## Variables

| Name | Type | Attributes | Description |
|------|------|------------|-------------|
| `stderr` | integer(kind=I4P) | parameter | Standard error unit. |
| `stdout` | integer(kind=I4P) | parameter | Standard output unit. |
| `end_rec` | character(len=1) | parameter | End-character for binary-record finalize. |
