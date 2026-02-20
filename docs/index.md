---
layout: home

hero:
  name: VTKFortran
  text: VTK XML API
  tagline: A pure Fortran 2003+ library to parse and emit files conforming the VTK XML standard.
  actions:
    - theme: brand
      text: Guide
      link: /guide/
    - theme: alt
      text: API Reference
      link: /api/
    - theme: alt
      text: View on GitHub
      link: https://github.com/szaghi/VTKFortran

features:
  - icon: 📐
    title: VTK XML Support
    details: Write Rectilinear, Structured, and Unstructured grids in the VTK XML format. Composite multi-block datasets (VTM) and parallel partitioned files (PVTS) are also supported.
  - icon: 🗜️
    title: Multiple Output Formats
    details: Choose between ASCII, binary (Base64-encoded), and raw binary appended formats. All procedures return an integer error code for flexible error trapping.
  - icon: ⚡
    title: Parallel Safe
    details: Handle multiple concurrent files safely. Thread and processor safe — works with OpenMP and MPI paradigms without coordination overhead.
  - icon: 🛠️
    title: Multi Build System
    details: Build with CMake (preferred), FoBiS.py, FPM, or GNU Make. Integrate into CMake projects via add_subdirectory and target_link_libraries.
  - icon: 🧱
    title: OOP Design
    details: A polymorphic xml_writer component is allocated at runtime based on the chosen format. The vtk_file, pvtk_file, and vtm_file types expose a clean, consistent type-bound-procedure API.
  - icon: 🆓
    title: Free & Open Source
    details: Multi-licensed — GPLv3 for FOSS projects, BSD 2/3-Clause or MIT for commercial use. Fortran 2003+ standard compliant.
---

## Quick start

Write a structured grid in binary XML format:

```fortran
use vtk_fortran, only : vtk_file
use penf,        only : I4P, R8P

type(vtk_file)     :: a_vtk_file
integer, parameter :: nx1=0, nx2=9, ny1=0, ny2=5, nz1=0, nz2=5
integer, parameter :: nn=(nx2-nx1+1)*(ny2-ny1+1)*(nz2-nz1+1)
real(R8P)          :: x(nx1:nx2,ny1:ny2,nz1:nz2)
real(R8P)          :: y(nx1:nx2,ny1:ny2,nz1:nz2)
real(R8P)          :: z(nx1:nx2,ny1:ny2,nz1:nz2)
real(R8P)          :: v(nx1:nx2,ny1:ny2,nz1:nz2)
integer            :: error

! ... fill x, y, z, v ...

error = a_vtk_file%initialize(format='binary', filename='output.vts', &
                              mesh_topology='StructuredGrid',          &
                              nx1=nx1, nx2=nx2, ny1=ny1, ny2=ny2, nz1=nz1, nz2=nz2)
error = a_vtk_file%xml_writer%write_piece(nx1=nx1, nx2=nx2, ny1=ny1, ny2=ny2, nz1=nz1, nz2=nz2)
error = a_vtk_file%xml_writer%write_geo(n=nn, x=x, y=y, z=z)
error = a_vtk_file%xml_writer%write_dataarray(location='node', action='open')
error = a_vtk_file%xml_writer%write_dataarray(data_name='velocity', x=v, one_component=.true.)
error = a_vtk_file%xml_writer%write_dataarray(location='node', action='close')
error = a_vtk_file%xml_writer%write_piece()
error = a_vtk_file%finalize()
```

All procedures return an integer error code — zero means success.

## Authors

- Stefano Zaghi — [@szaghi](https://github.com/szaghi)

Contributions are welcome — see the [Contributing](/guide/contributing) page.

## Copyrights

VTKFortran is distributed under a multi-licensing system:

| Use case | License |
|----------|---------|
| FOSS projects | [GPL v3](http://www.gnu.org/licenses/gpl-3.0.html) |
| Closed source / commercial | [BSD 2-Clause](http://opensource.org/licenses/BSD-2-Clause) |
| Closed source / commercial | [BSD 3-Clause](http://opensource.org/licenses/BSD-3-Clause) |
| Closed source / commercial | [MIT](http://opensource.org/licenses/MIT) |

> Anyone interested in using, developing, or contributing to VTKFortran is welcome — pick the license that best fits your needs.
