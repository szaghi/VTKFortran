---
title: About VTKFortran
---

# About VTKFortran

**VTKFortran** is a pure Fortran 2003+ library for writing (and reading) data files conforming the [VTK standard](http://www.vtk.org/). It is not a wrapper around the VTK C++ source — it is an independent, pure-Fortran exporter/importer of the VTK data format, designed for Fortran programmers who want to use VTK-based visualization tools (such as [ParaView](https://www.paraview.org/)) without mixing languages.

Key properties:

- **Pure Fortran 2003+** — no C or C++ dependencies
- **Thread and processor safe** — multiple files can be written concurrently under OpenMP or MPI
- **Multiple output formats** — ASCII, binary (Base64), and raw binary appended
- **OOP design** — a polymorphic `xml_writer` component is selected at runtime based on the chosen format

## Why VTKFortran?

Fortran remains the language of choice for high-performance scientific computing. Many HPC codes produce large multi-dimensional arrays that need post-processing and visualization. The VTK XML format is the native input format for ParaView, one of the most widely used open-source visualization tools. VTKFortran provides a direct path from Fortran arrays to VTK XML files without leaving the Fortran ecosystem.

## Authors

- Stefano Zaghi — [@szaghi](https://github.com/szaghi)

Contributions are welcome — see the [Contributing](contributing) page.

## Copyrights

VTKFortran is distributed under a multi-licensing system:

| Use case | License |
|----------|---------|
| FOSS projects | [GPL v3](http://www.gnu.org/licenses/gpl-3.0.html) |
| Closed source / commercial | [BSD 2-Clause](http://opensource.org/licenses/BSD-2-Clause) |
| Closed source / commercial | [BSD 3-Clause](http://opensource.org/licenses/BSD-3-Clause) |
| Closed source / commercial | [MIT](http://opensource.org/licenses/MIT) |

> Anyone interested in using, developing, or contributing to VTKFortran is welcome — pick the license that best fits your needs.
