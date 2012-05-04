=======
Lib\_VTK\_IO
==========

Lib\_VTK\_IO is a Fortran library to write and read data conforming the [VTK standard](http://www.vtk.org/).

Even though there are many wrappers/porting of the VTK source code (C++ code), there is not a Fortran one. This library is not a porting or a wrapper of the VTK code, but it is only an exporter/importer of VTK data format written in pure Fortran language (standard Fortran 2003) that can be used by Fortran coders (yes, there are still a lot of these brave coders...) without mixing Fortran with C++ language.


The library is still in developing and testing, this is first usable release, but there are not all the features of the stable release (the importer is totally absent and the exporter is not complete). Surely there are a lot of bugs and the programming style is not the best, but the exporters are far-complete.

The supported VTK features are:
* Exporters:
  * Legacy standard:
    * Structured Points;
    * Structured Grid;
    * Unstructured Grid;
    * Polydata (__missing__);
    * Rectilinear Grid;
    * Field (__missing__);
  * XML standard:
    * Image Data (__missing__);
    * Polydata (__missing__);
    * Rectilinear Grid;
    * Structured Grid;
    * Unstructured Grid;
    * Parallel Image Data (__missing__);
    * Parallel Polydata (__missing__);
    * Parallel Rectilinear Grid (__missing__);
    * Parallel Structured Grid (__missing__);
    * Parallel Unstructured Grid (__missing__);
* Importers are __missing__.

Copyrights
----------

The Lib\_VTK\_IO is an open source project, it is distributed under the [GPL v3](http://www.gnu.org/licenses/gpl-3.0.html). Anyone is interest to use, to develop or to contribute to Lib\_VTK\_IO is welcome.

Documentation
----------

Detailed documentation can be found on the [GitHub pages](http://szaghi.github.com/Lib_VTK_IO/index.html) of the project.
