---
title: About VTKFortran
---

# About VTKFortran

**VTKFortran** is a pure Fortran 2008 library for writing and reading data files conforming the [VTK standard](http://www.vtk.org/). It is not a wrapper around the VTK C++ source — it is an independent, pure-Fortran exporter/importer of the VTK data format, designed for Fortran programmers who want to use VTK-based visualization tools (such as [ParaView](https://www.paraview.org/)) without mixing languages.

Key properties:

- **Pure Fortran 2008** — no C or C++ dependencies, except zlib (optional) for compressed data
- **Parallel writers** — MPI ranks write their own files at the same time; OpenMP threads too with ifx or nvfortran, not
  gfortran (see [Threads and processes](/guide/parallel#threads-and-processes))
- **Multiple output formats** — ASCII, binary (Base64), and raw binary appended, optionally zlib compressed
- **Reading** — every file it writes (and the same files written by VTK) can be read back
- **OOP design** — a polymorphic `xml_writer` component is selected at runtime based on the chosen format

## Why VTKFortran?

Fortran remains the language of choice for high-performance scientific computing. Many HPC codes produce large multi-dimensional arrays that need post-processing and visualization. The VTK XML format is the native input format for ParaView, one of the most widely used open-source visualization tools. VTKFortran provides a direct path from Fortran arrays to VTK XML files without leaving the Fortran ecosystem.

## Authors and licenses

VTKFortran is written by Stefano Zaghi ([@szaghi](https://github.com/szaghi)) and its contributors, see
[Contributing](contributing). It is multi-licensed, GPL v3 for FOSS projects and BSD 2-Clause, BSD 3-Clause or MIT for
closed source and commercial ones, see [Copyrights](/#copyrights).
