# VTKFortran

>#### Pure Fortran VTK XML API
>a pure Fortran 2003+ OOP library for writing and reading files conforming the [VTK](http://www.vtk.org/) XML standard.

[![GitHub tag](https://img.shields.io/github/v/tag/szaghi/VTKFortran)](https://github.com/szaghi/VTKFortran/tags)
[![GitHub issues](https://img.shields.io/github/issues/szaghi/VTKFortran)](https://github.com/szaghi/VTKFortran/issues)
[![CI](https://github.com/szaghi/VTKFortran/actions/workflows/ci.yml/badge.svg)](https://github.com/szaghi/VTKFortran/actions/workflows/ci.yml)
[![coverage](https://img.shields.io/endpoint?url=https://szaghi.github.io/VTKFortran/coverage.json)](https://github.com/szaghi/VTKFortran/actions/workflows/ci.yml)
[![License](https://img.shields.io/badge/license-GPLv3%20%7C%20BSD%20%7C%20MIT-blue.svg)](#copyrights)

| 📐 **VTK Topologies**<br>Rectilinear (`.vtr`), Structured (`.vts`), and Unstructured (`.vtu`) grids; composite multi-block (`.vtm`) and parallel partitioned (`.pvts`) datasets | 🗜️ **Output Formats**<br>ASCII, Base64-encoded binary, and raw binary appended | ⚡ **Parallel Safe**<br>Thread and processor safe — multiple files can be written concurrently under OpenMP or MPI | 🎨 **OOP Design**<br>Polymorphic `xml_writer` allocated at runtime; `vtk_file`, `pvtk_file`, `vtm_file` expose a clean type-bound-procedure API |
|:---:|:---:|:---:|:---:|
| 🔢 **All Numeric Kinds**<br>All PENF kinds (`I1P`–`I8P`, `R4P`–`R8P`) and array ranks 1–4 supported in data arrays | ✅ **Error Codes**<br>Every procedure returns an integer error code — zero means success | 🔓 **Multi-licensed**<br>GPL v3 · BSD 2/3-Clause · MIT | 📦 **Multiple build systems**<br>CMake, FoBiS.py, fpm, Make |

>#### [Documentation](https://szaghi.github.io/VTKFortran/)
> For full documentation (guide, API reference, usage examples, etc.) see the [VTKFortran website](https://szaghi.github.io/VTKFortran/).

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

See [`src/tests/`](src/tests/) for more examples covering rectilinear, unstructured, multi-block, parallel, and volatile XML output.

---

## Install

### CMake

**Clone, build, and test:**

```bash
git clone https://github.com/szaghi/VTKFortran --recursive
cd VTKFortran
cmake -S . -B build -DBUILD_TESTING=ON
cmake --build build && ctest --test-dir build
```

**As a subdirectory dependency** — place a recursive clone alongside your sources and add to your `CMakeLists.txt`:

```cmake
add_subdirectory(VTKFortran)
target_link_libraries(your_target VTKFortran::VTKFortran)
```

### FoBiS.py

```bash
git clone https://github.com/szaghi/VTKFortran --recursive && cd VTKFortran
FoBiS.py build -mode static-gnu   # static library
FoBiS.py build -mode tests-gnu    # build and place tests in ./exe/
bash scripts/run_tests.sh         # run tests
```

### fpm

```bash
fpm build
fpm test
```

### GNU Make

```bash
make
```
