<a name="top"></a>

# Lib\_VTK\_IO [![GitHub tag](https://img.shields.io/github/tag/szaghi/Lib_VTK_IO.svg)]()

[![Join the chat at https://gitter.im/szaghi/Lib_VTK_IO](https://badges.gitter.im/Join%20Chat.svg)](https://gitter.im/szaghi/Lib_VTK_IO?utm_source=badge&utm_medium=badge&utm_campaign=pr-badge&utm_content=badge)

[![License](https://img.shields.io/badge/license-GNU%20GeneraL%20Public%20License%20v3,%20GPLv3-blue.svg)]()
[![License](https://img.shields.io/badge/license-BSD2-red.svg)]()
[![License](https://img.shields.io/badge/license-BSD3-red.svg)]()
[![License](https://img.shields.io/badge/license-MIT-red.svg)]()

[![Status](https://img.shields.io/badge/status-stable-brightgreen.svg)]()
[![Build Status](https://travis-ci.org/szaghi/Lib_VTK_IO.svg?branch=master)](https://travis-ci.org/szaghi/Lib_VTK_IO)
[![Coverage Status](https://coveralls.io/repos/szaghi/Lib_VTK_IO/badge.svg?branch=master)](https://coveralls.io/r/szaghi/Lib_VTK_IO?branch=master)

### Lib\_VTK\_IO, VTK IO in pure Fortran (2003+)

A KISS pure Fortran Library to IO data conforming the VTK standard

+ Lib\_VTK\_IO is a pure Fortran library to write and read data conforming the [VTK standard](http://www.vtk.org/);
+ Lib\_VTK\_IO is Fortran 2003+ standard compliant;
+ Lib\_VTK\_IO supports parallel architectures by means OpenMP and MPI paradigms;
+ Lib\_VTK\_IO supports _ascii_, _binary_ and _base64_ file formats;
+ Lib\_VTK\_IO is a Free, Open Source Project.

#### Table of Contents

- [Main features](#main-features)
- [Copyrights](#copyrights)
- [Documentation](#documentation)
  - [A Taste of Lib_VTK_IO](#a-taste-of-lib_vtk_io)

#### Issues

[![GitHub issues](https://img.shields.io/github/issues/szaghi/Lib_VTK_IO.svg)]()
[![Ready in backlog](https://badge.waffle.io/szaghi/Lib_VTK_IO.png?label=ready&title=Ready)](https://waffle.io/szaghi/Lib_VTK_IO)
[![In Progress](https://badge.waffle.io/szaghi/Lib_VTK_IO.png?label=in%20progress&title=In%20Progress)](https://waffle.io/szaghi/Lib_VTK_IO)
[![Open bugs](https://badge.waffle.io/szaghi/Lib_VTK_IO.png?label=bug&title=Open%20Bugs)](https://waffle.io/szaghi/Lib_VTK_IO)

#### Compiler Support

[![Compiler](https://img.shields.io/badge/GNU-pass%20(v4.9.2+)-brightgreen.svg)]()
[![Compiler](https://img.shields.io/badge/Intel-pass%20(v12.x+)-brightgreen.svg)]()
[![Compiler](https://img.shields.io/badge/IBM%20XL-not%20tested-yellow.svg)]()
[![Compiler](https://img.shields.io/badge/g95-not%20tested-yellow.svg)]()
[![Compiler](https://img.shields.io/badge/NAG-not%20tested-yellow.svg)]()
[![Compiler](https://img.shields.io/badge/PGI-not%20tested-yellow.svg)]()

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

### Parallel Architectures

Lib\_VTK\_IO can handle multiple concurrent files and it is \b thread/processor-safe, meaning that it can be safely used into parallel architectures using OpenMP and/or MPI paradigms. Into section [[Parallel-benchmarks]] some more details can be found.

## Copyrights

Lib_VTK_IO is an open source project, it is distributed under a multi-licensing system:

+ for FOSS projects:
  - [GPL v3](http://www.gnu.org/licenses/gpl-3.0.html);
+ for closed source/commercial projects:
  - [BSD 2-Clause](http://opensource.org/licenses/BSD-2-Clause);
  - [BSD 3-Clause](http://opensource.org/licenses/BSD-3-Clause);
  - [MIT](http://opensource.org/licenses/MIT).

Anyone is interest to use, to develop or to contribute to Lib_VTK_IO is welcome, feel free to select the license that best matches your soul!

More details can be found on [wiki](https://github.com/szaghi/Lib_VTK_IO/wiki/Copyrights).

Go to [Top](#top)

## Documentation

Besides this README file the Lib_VTK_IO documentation is contained into its own [wiki](https://github.com/szaghi/Lib_VTK_IO/wiki). Detailed documentation of the API is contained into the [GitHub Pages](http://szaghi.github.io/Lib_VTK_IO/index.html) that can also be created locally by means of [ford tool](https://github.com/cmacmackin/ford).

Go to [Top](#top)

### A taste of Lib\_VTK\_IO

Let us assume our aim being to save our pure Fortran data into a VTK structured grid file in binary XML form. This is simple as

```fortran
USE Lib_VTK_IO
...
! dataset dimensions
integer, parameter:: nx1=0,nx2=9,ny1=0,ny2=5,nz1=0,nz2=5
integer, parameter:: nn=(nx2-nx1+1)*(ny2-ny1+1)*(nz2-nz1+1)
! grid coordinates
real, dimension(nx1:nx2,ny1:ny2,nz1:nz2):: x,y,z
! variables associated at grid nodes
real, dimension(nx1:nx2,ny1:ny2,nz1:nz2):: v_R
! auxiliary variables
integer:: E_IO
...
E_IO = VTK_INI_XML(output_format='binary',filename='XML_STRG.vts',mesh_topology='StructuredGrid',nx1=nx1,nx2=nx2,ny1=ny1,ny2=ny2,nz1=nz1,nz2=nz2)
E_IO = VTK_GEO_XML(nx1=nx1,nx2=nx2,ny1=ny1,ny2=ny2,nz1=nz1,nz2=nz2,NN=nn,X=x,Y=y,Z=z)
E_IO = VTK_DAT_XML(var_location='node',var_block_action='open')
E_IO = VTK_VAR_XML(NC_NN=nn,varname='real scalar',var=v_R)
E_IO = VTK_DAT_XML(var_location='node',var_block_action='close')
E_IO = VTK_GEO_XML()
E_IO = VTK_END_XML()
```

Note that all Lib\_VTK\_IO functions return an error code (`E_IO`) that can be used for sophisticated error trapping algorithms.
