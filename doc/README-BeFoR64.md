<a name="top"></a>

# BeFoR64 [![GitHub tag](https://img.shields.io/github/tag/szaghi/BeFoR64.svg)]()

[![License](https://img.shields.io/badge/license-GNU%20GeneraL%20Public%20License%20v3%20,%20GPLv3-blue.svg)]()

[![Status](https://img.shields.io/badge/status-stable-brightgreen.svg)]()
[![Build Status](https://travis-ci.org/szaghi/BeFoR64.svg?branch=master)](https://travis-ci.org/szaghi/BeFoR64)
[![Coverage Status](https://coveralls.io/repos/szaghi/BeFoR64/badge.svg?branch=master)](https://coveralls.io/r/szaghi/BeFoR64?branch=master)

### BeFoR64, Base64 encoding/decoding library for FoRtran poor men

+ BeFoR64 is a pure Fortran (KISS) library for base64 encoding/decoding for modern (2003+) Fortran projects;
+ BeFoR64 is Fortran 2003+ standard compliant;
+ BeFoR64 is a Free, Open Source Project.

#### Issues
[![GitHub issues](https://img.shields.io/github/issues/szaghi/BeFoR64.svg)]()
[![Ready in backlog](https://badge.waffle.io/szaghi/BeFoR64.png?label=ready&title=Ready)](https://waffle.io/szaghi/BeFoR64)
[![In Progress](https://badge.waffle.io/szaghi/BeFoR64.png?label=in%20progress&title=In%20Progress)](https://waffle.io/szaghi/BeFoR64)
[![Open bugs](https://badge.waffle.io/szaghi/BeFoR64.png?label=bug&title=Open%20Bugs)](https://waffle.io/szaghi/BeFoR64)

#### Compiler Support
[![Compiler](https://img.shields.io/badge/GNU%20Gfortran%20Compiler-build%20pass%20with%20v4.9.2+-brightgreen.svg)]()

[![Compiler](https://img.shields.io/badge/Intel%20Fortran%20Compiler-build%20pass%20with%20v12.x+-brightgreen.svg)]()

[![Compiler](https://img.shields.io/badge/IBM%20XL%20Fortran%20Compiler-not%20tested-yellow.svg)]()

[![Compiler](https://img.shields.io/badge/g95%20Fortran%20Compiler-not%20tested-yellow.svg)]()

[![Compiler](https://img.shields.io/badge/NAG%20Fortran%20Compiler-not%20tested-yellow.svg)]()

[![Compiler](https://img.shields.io/badge/PGI%20Fortran%20Compiler-not%20tested-yellow.svg)]()

## What is BeFoR64?

Modern Fortran standards (2003+) have introduced better support for strings manipulations. Exploiting such new Fortran capabilities, BeFoR64 provides an easy to use module library for encoding and decoding Fortran types (binary internal representation) in ascii-[base64](http://en.wikipedia.org/wiki/Base64)-encoded string.

Go to [Top](#top)

## Main features

* [X] User-friendly methods for encoding/decoding in base64:
    * [x] encode real/integer scalar variables;
    * [X] decode real/integer scalar variables;
    * [x] encode real/integer array variables;
    * [X] decode real/integer array variables;
    * [X] encode character scalar variables;
    * [X] decode character scalar variables;
    * [X] encode character array variables;
    * [X] decode character array variables;
    * [X] encode unlimited polymorphic scalar variables;
    * [X] decode unlimited polymorphic scalar variables;
    * [X] encode unlimited polymorphic array variables;
    * [X] decode unlimited polymorphic array variables;
* [ ] user-friendly methods for packing heterogeneous data:
    * [ ] pack integer/integer (different kinds) scalars;
    * [x] pack integer/integer (different kinds) arrays;
    * [ ] pack real/real (different kinds) scalars;
    * [x] pack real/real (different kinds) arrays;
    * [ ] pack integer/real scalars;
    * [x] pack integer/real arrays;
* [ ] errors trapping mechanism.

Any feature request is welcome.

Go to [Top](#top)

## Copyrights

BeFoR64 is an open source project, it is distributed under the [GPL v3](http://www.gnu.org/licenses/gpl-3.0.html). Anyone is interest to use, to develop or to contribute to BeFoR64 is welcome.

Go to [Top](#top)

## Documentation

Besides this README file the BeFoR64 documentation is contained into its own [wiki](https://github.com/szaghi/BeFoR64/wiki). Detailed documentation of the API is contained into the [GitHub Pages](http://szaghi.github.io/BeFoR64/index.html) that can also be created locally by means of [ford tool](https://github.com/cmacmackin/ford).

### A Taste of BeFoR64
Let us assume our goal is encoding a binary integer. It is as simple as
```fortran
USE Lib_Base64
...
character(len=:), allocatable:: code64 ! base64 encoded string
...
call b64_encode(n=12._R8P,code=code64)
print "(A)", code64
```

But you are not limited to a simple integer scalar, you can encode real, integer, characters scalar or arrays, and by means of the auxiliary `Lib_Pack` library also mixed types. See the [wiki](https://github.com/szaghi/BeFoR64/wiki).

Go to [Top](#top)
