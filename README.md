=======
Lib\_VTK\_IO
==========

Lib\_VTK\_IO is a Fortran library to write and read data conforming the VTK standard.

Even though there are many wrappers/porting of the VTK source code (C++ code), there is not a Fortran one. This library is not a porting or a wrapper of the VTK code, but it is only an exporter/importer of VTK data format written in pure Fortran language (standard Fortran 2003) that can be used by Fortran coders (yes, there are still a lot of these brave coders...) without mixing Fortran with C++ language.

The library is still in developing and testing, this is first usable release, but there are not all the features of the stable release (the importer is totaly absent and the exporter is not complete). Surely there are a lot of bugs and the progamming style is not the best, but the exporter is usable for the 90\% of the VTK data format.

The Lib\_VTK\_IO is an open source project, it is distribuited under the GPL v3. Anyone is interest to use, to develop or to contribuite to Lib\_VTK\_IO is welcome.

Detailed documentation can be found on the [GitHub pages](http://szaghi.github.com/Lib_VTK_IO/index.html) of the project.
