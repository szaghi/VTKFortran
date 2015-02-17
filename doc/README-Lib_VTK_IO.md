<a name="top"></a>

# Lib\_VTK\_IO [![GitHub tag](https://img.shields.io/github/tag/szaghi/Lib_VTK_IO.svg)]()

[![License](https://img.shields.io/badge/license-GNU%20GeneraL%20Public%20License%20v3%20,%20GPLv3-blue.svg)]()

[![Status](https://img.shields.io/badge/status-stable-brightgreen.svg)]()
[![Build Status](https://travis-ci.org/szaghi/Lib_VTK_IO.svg?branch=master)](https://travis-ci.org/szaghi/Lib_VTK_IO)

### Lib\_VTK\_IO, VTK IO in pure Fortran (2003+)
A KISS pure Fortran Library to IO data conforming the VTK standard

+ Lib\_VTK\_IO is a pure Fortran library to write and read data conforming the [VTK standard](http://www.vtk.org/);
+ Lib\_VTK\_IO is Fortran 2003+ standard compliant;
+ Lib\_VTK\_IO supports parallel architectures by means OpenMP and MPI paradigms;
+ Lib\_VTK\_IO supports _ascii_, _binary_ and _base64_ file formats;
+ Lib\_VTK\_IO is a Free, Open Source Project.

#### Issues
[![GitHub issues](https://img.shields.io/github/issues/szaghi/Lib_VTK_IO.svg)]()
[![Ready in backlog](https://badge.waffle.io/szaghi/Lib_VTK_IO.png?label=ready&title=Ready)](https://waffle.io/szaghi/Lib_VTK_IO)
[![In Progress](https://badge.waffle.io/szaghi/Lib_VTK_IO.png?label=in%20progress&title=In%20Progress)](https://waffle.io/szaghi/Lib_VTK_IO)
[![Open bugs](https://badge.waffle.io/szaghi/Lib_VTK_IO.png?label=bug&title=Open%20Bugs)](https://waffle.io/szaghi/Lib_VTK_IO)

#### Compiler Support
[![Compiler](https://img.shields.io/badge/GNU%20Gfortran%20Compiler-build%20pass%20with%20v4.9.2+-brightgreen.svg)]()

[![Compiler](https://img.shields.io/badge/Intel%20Fortran%20Compiler-build%20pass%20with%20v12.x+-brightgreen.svg)]()

[![Compiler](https://img.shields.io/badge/IBM%20XL%20Fortran%20Compiler-not%20tested-yellow.svg)]()

[![Compiler](https://img.shields.io/badge/g95%20Fortran%20Compiler-not%20tested-yellow.svg)]()

[![Compiler](https://img.shields.io/badge/NAG%20Fortran%20Compiler-not%20tested-yellow.svg)]()

[![Compiler](https://img.shields.io/badge/PGI%20Fortran%20Compiler-not%20tested-yellow.svg)]()

## Copyrights

The Lib\_VTK\_IO is an open source project, it is distributed under the [GPL v3](http://www.gnu.org/licenses/gpl-3.0.html). Anyone is interest to use, to develop or to contribute to Lib\_VTK\_IO is welcome. Take a look at the [contributing guidelines](CONTRIBUTING.md) for starting to contribute to the project.

Go to [Top](#top)

## Documentation

Lib\_VTK\_IO has a comprehensive [wiki](https://github.com/szaghi/Lib_VTK_IO/wiki). Moreover, the API is well documented, on the [GitHub pages](http://szaghi.github.com/Lib_VTK_IO/index.html) of the project.

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
