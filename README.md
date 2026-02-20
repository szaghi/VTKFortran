# VTKFortran

**Pure Fortran VTK XML API** — a pure Fortran 2003+ OOP library for writing and reading files conforming the [VTK](http://www.vtk.org/) XML standard.

[![CI](https://github.com/szaghi/VTKFortran/actions/workflows/ci.yml/badge.svg)](https://github.com/szaghi/VTKFortran/actions)
[![Coverage](https://img.shields.io/codecov/c/github/szaghi/VTKFortran.svg)](https://app.codecov.io/gh/szaghi/VTKFortran)
[![GitHub tag](https://img.shields.io/github/tag/szaghi/VTKFortran.svg)](https://github.com/szaghi/VTKFortran/releases)
[![License](https://img.shields.io/badge/license-GPLv3%20%7C%20BSD%20%7C%20MIT-blue.svg)](#copyrights)

---

## Features

- Write Rectilinear, Structured, and Unstructured grids in the VTK XML format
- Composite multi-block datasets (`.vtm`) and parallel partitioned files (`.pvts`) supported
- Output formats: ASCII, binary (Base64-encoded), and raw binary appended
- Thread and processor safe — multiple files can be written concurrently under OpenMP or MPI
- OOP design — polymorphic `xml_writer` allocated at runtime; `vtk_file`, `pvtk_file`, `vtm_file` types expose a clean type-bound-procedure API
- All procedures return an integer error code — zero means success

**[Documentation](https://szaghi.github.io/VTKFortran/)** | **[API Reference](https://szaghi.github.io/VTKFortran/api/)**

---

## Authors

- Stefano Zaghi — [@szaghi](https://github.com/szaghi)

Contributions are welcome — see the [Contributing](https://szaghi.github.io/VTKFortran/guide/contributing) page.

## Copyrights

This project is distributed under a multi-licensing system:

- **FOSS projects**: [GPL v3](http://www.gnu.org/licenses/gpl-3.0.html)
- **Closed source / commercial**: [BSD 2-Clause](http://opensource.org/licenses/BSD-2-Clause), [BSD 3-Clause](http://opensource.org/licenses/BSD-3-Clause), or [MIT](http://opensource.org/licenses/MIT)

> Anyone interested in using, developing, or contributing to this project is welcome — pick the license that best fits your needs.

---

## Quick start

Write a structured grid in binary XML format:

```fortran
use vtk_fortran, only : vtk_file
use penf,        only : I4P, R8P
implicit none
type(vtk_file)     :: a_vtk_file
integer, parameter :: nx1=0, nx2=9, ny1=0, ny2=5, nz1=0, nz2=5
integer, parameter :: nn=(nx2-nx1+1)*(ny2-ny1+1)*(nz2-nz1+1)
real(R8P)          :: x(nx1:nx2,ny1:ny2,nz1:nz2)
real(R8P)          :: y(nx1:nx2,ny1:ny2,nz1:nz2)
real(R8P)          :: z(nx1:nx2,ny1:ny2,nz1:nz2)
real(R8P)          :: v(nx1:nx2,ny1:ny2,nz1:nz2)
integer            :: error

error = a_vtk_file%initialize(format='binary', filename='output.vts', &
                              mesh_topology='StructuredGrid',          &
                              nx1=nx1, nx2=nx2, ny1=ny1, ny2=ny2, nz1=nz1, nz2=nz2)
error = a_vtk_file%xml_writer%write_piece(nx1=nx1, nx2=nx2, ny1=ny1, ny2=ny2, nz1=nz1, nz2=nz2)
error = a_vtk_file%xml_writer%write_geo(n=nn, x=x, y=y, z=z)
error = a_vtk_file%xml_writer%write_dataarray(location='node', action='open')
error = a_vtk_file%xml_writer%write_dataarray(data_name='pressure', x=v, one_component=.true.)
error = a_vtk_file%xml_writer%write_dataarray(location='node', action='close')
error = a_vtk_file%xml_writer%write_piece()
error = a_vtk_file%finalize()
```

---

## Install

### Clone and build with CMake

```sh
git clone https://github.com/szaghi/VTKFortran --recursive
cd VTKFortran
cmake -S . -B build -DBUILD_TESTING=ON
cmake --build build && ctest --test-dir build
```

### CMake subdirectory integration

```cmake
add_subdirectory(VTKFortran)
target_link_libraries(your_target VTKFortran::VTKFortran)
```

| Tool | Command |
|------|---------|
| CMake | `cmake -S . -B build && cmake --build build` |
| FoBiS.py | `FoBiS.py build -mode static-gnu` |
| FPM | `fpm build` |
