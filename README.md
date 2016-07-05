<a name="top"></a>

# VTKFortran [![GitHub tag](https://img.shields.io/github/tag/szaghi/VTKFortran.svg)]()

[![Join the chat at https://gitter.im/szaghi/VTKFortran](https://badges.gitter.im/Join%20Chat.svg)](https://gitter.im/szaghi/VTKFortran?utm_source=badge&utm_medium=badge&utm_campaign=pr-badge&utm_content=badge)

[![License](https://img.shields.io/badge/license-GNU%20GeneraL%20Public%20License%20v3,%20GPLv3-blue.svg)]()
[![License](https://img.shields.io/badge/license-BSD2-red.svg)]()
[![License](https://img.shields.io/badge/license-BSD3-red.svg)]()
[![License](https://img.shields.io/badge/license-MIT-red.svg)]()

[![Status](https://img.shields.io/badge/status-stable-brightgreen.svg)]()
[![Build Status](https://travis-ci.org/szaghi/VTKFortran.svg?branch=master)](https://travis-ci.org/szaghi/VTKFortran)
[![Coverage Status](https://codecov.io/gh/szaghi/VTKFortran/branch/master/graph/badge.svg)](https://codecov.io/gh/szaghi/VTKFortran)

### VTKFortran, pure Fortran VTK (XML) API

A KISS pure Fortran Library to parse and emit files conforming VTK (XML) standard

+ VTKFortran is a pure Fortran library to parse and emit VTK files, [VTK standard](http://www.vtk.org/);
+ VTKFortran is Fortran 2003+ standard compliant;
+ VTKFortran supports parallel architectures, it being threads-safe;
+ VTKFortran supports _ascii_, _binary_ (Base64 encoding) and _raw_ file formats;
+ VTKFortran is a Free, Open Source Project.

#### Issues

[![GitHub issues](https://img.shields.io/github/issues/szaghi/VTKFortran.svg)]()
[![Ready in backlog](https://badge.waffle.io/szaghi/VTKFortran.png?label=ready&title=Ready)](https://waffle.io/szaghi/VTKFortran)
[![In Progress](https://badge.waffle.io/szaghi/VTKFortran.png?label=in%20progress&title=In%20Progress)](https://waffle.io/szaghi/VTKFortran)
[![Open bugs](https://badge.waffle.io/szaghi/VTKFortran.png?label=bug&title=Open%20Bugs)](https://waffle.io/szaghi/VTKFortran)

#### Compiler Support

[![Compiler](https://img.shields.io/badge/GNU-pass%20(v6.0.1+)-brightgreen.svg)]()
[![Compiler](https://img.shields.io/badge/Intel-pass%20(v16.x+)-brightgreen.svg)]()
[![Compiler](https://img.shields.io/badge/IBM%20XL-not%20tested-yellow.svg)]()
[![Compiler](https://img.shields.io/badge/g95-not%20tested-yellow.svg)]()
[![Compiler](https://img.shields.io/badge/NAG-not%20tested-yellow.svg)]()
[![Compiler](https://img.shields.io/badge/PGI-not%20tested-yellow.svg)]()

---

[Main features](#main-features) | [Copyrights](#copyrights) | [Documentation](#documentation) | [A Taste of VTKFortran](#a-taste-of-vtkfortran)

---

## Main features

### VTK features

#### Exporters

##### Legacy standard
* [x] Structured Points;
* [x] Structured Grid;
* [x] Unstructured Grid;
* [ ] Polydata;
* [x] Rectilinear Grid;
* [ ] Field;

##### XML standard
* [ ] serial dataset:
    * [ ] Image Data;
    * [ ] Polydata;
    * [x] Rectilinear Grid;
    * [x] Structured Grid;
    * [x] Unstructured Grid;
* [ ] parallel (partitioned) dataset:
    * [ ] Image Data;
    * [ ] Polydata;
    * [x] Rectilinear Grid;
    * [x] Structured Grid;
    * [x] Unstructured Grid;
* [x] composite dataset:
    * [x] vtkMultiBlockDataSet.

#### Importers
The importers are under developing.

### Parallel Support

VTKFortran can be safely used in parallel *environments*, handling multiple concurrent files: it is **thread/processor-safe**, meaning that it can be safely used into parallel architectures using OpenMP and/or MPI paradigms.

## Copyrights

VTKFortran is an open source project, it is distributed under a multi-licensing system:

+ for FOSS projects:
  - [GPL v3](http://www.gnu.org/licenses/gpl-3.0.html);
+ for closed source/commercial projects:
  - [BSD 2-Clause](http://opensource.org/licenses/BSD-2-Clause);
  - [BSD 3-Clause](http://opensource.org/licenses/BSD-3-Clause);
  - [MIT](http://opensource.org/licenses/MIT).

Anyone is interest to use, to develop or to contribute to VTKFortran is welcome, feel free to select the license that best matches your soul!

More details can be found on [wiki](https://github.com/szaghi/VTKFortran/wiki/Copyrights).

Go to [Top](#top)

## Documentation

Besides this README file the VTKFortran documentation is contained into its own [wiki](https://github.com/szaghi/VTKFortran/wiki). Detailed documentation of the API is contained into the [GitHub Pages](http://szaghi.github.io/VTKFortran/index.html) that can also be created locally by means of [ford tool](https://github.com/cmacmackin/ford).

Go to [Top](#top)

### A taste of VTKFortran

Let us assume our aim being to save our pure Fortran data into a VTK structured grid file in binary XML form. This is simple as

```fortran
use vtk_fortran, only : vtk_file

type(vtk_file)     :: a_vtk_file                             ! A VTK file.
integer, parameter :: nx1=0_I4P                              ! X lower bound extent.
integer, parameter :: nx2=9_I4P                              ! X upper bound extent.
integer, parameter :: ny1=0_I4P                              ! Y lower bound extent.
integer, parameter :: ny2=5_I4P                              ! Y upper bound extent.
integer, parameter :: nz1=0_I4P                              ! Z lower bound extent.
integer, parameter :: nz2=5_I4P                              ! Z upper bound extent.
integer, parameter :: nn=(nx2-nx1+1)*(ny2-ny1+1)*(nz2-nz1+1) ! Number of elements.
real               :: x(nx1:nx2,ny1:ny2,nz1:nz2)             ! X coordinates.
real               :: y(nx1:nx2,ny1:ny2,nz1:nz2)             ! Y coordinates.
real               :: z(nx1:nx2,ny1:ny2,nz1:nz2)             ! Z coordinates.
real               :: v(nx1:nx2,ny1:ny2,nz1:nz2)             ! Variable at coordinates.
integer            :: error                                  ! Error status.

! initialize the data...

error = a_vtk_file%initialize(format='binary', filename='XML_STRG-binary.vts', &
                              mesh_topology='StructuredGrid',                  &
                              nx1=nx1, nx2=nx2, ny1=ny1, ny2=ny2, nz1=nz1, nz2=nz2)
error = a_vtk_file%xml_writer%write_piece(nx1=nx1, nx2=nx2, ny1=ny1, ny2=ny2, nz1=nz1, nz2=nz2)
error = a_vtk_file%xml_writer%write_geo(n=nn, x=x, y=y, z=z)
error = a_vtk_file%xml_writer%write_dataarray(location='node', action='open')
error = a_vtk_file%xml_writer%write_dataarray(data_name='float64_scalar', x=v, one_component=.true.)
error = a_vtk_file%xml_writer%write_dataarray(location='node', action='close')
error = a_vtk_file%xml_writer%write_piece()
error = a_vtk_file%finalize()
```

Note that all VTKFortran functions return an error code that can be used for sophisticated error trapping algorithms.
