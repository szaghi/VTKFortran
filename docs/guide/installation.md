---
title: Installation
---

# Installation

## Prerequisites

A Fortran 2003+ compliant compiler is required:

| Compiler | Minimum version |
|----------|----------------|
| GNU gfortran | ≥ 6.0.1 |
| Intel Fortran (ifort) | ≥ 16.x |

## Download

Clone the repository, then fetch its third-party dependencies with [FoBiS.py](https://github.com/szaghi/FoBiS) (they are
listed in the `[dependencies]` section of `fobos`):

```bash
git clone https://github.com/szaghi/VTKFortran
cd VTKFortran
pip install FoBiS.py
fobis fetch
```

CMake and FoBiS.py builds need the fetched dependencies; fpm fetches its own (see `fpm.toml`). The release assets include an
`install.sh` script that downloads a release, fetches the dependencies and builds it (`install.sh --help`).

### Third-Party Dependencies

`fobis fetch` places them under `src/third_party/`:

| Library | Purpose |
|---------|---------|
| [PENF](https://github.com/szaghi/PENF) | Portable numeric kind parameters (`I4P`, `R8P`, etc.) — used everywhere |
| [BeFoR64](https://github.com/szaghi/BeFoR64) | Base64 encode/decode for binary XML data |
| [StringiFor](https://github.com/szaghi/StringiFor) | OOP `string` type used throughout the writer classes |
| [FoXy](https://github.com/Fortran-FOSS-Programmers/FoXy) | XML tag parsing and emitting |
| [FACE](https://github.com/szaghi/FACE) | ANSI terminal colour output |

## Build with CMake (preferred)

CMake is the recommended build system for library use and integration into other projects.

```bash
cmake -S . -B build
cmake --build build
```

### Build the test programs

```bash
cmake -S . -B build -DBUILD_TESTING=ON
cmake --build build
```

The test programs are built in `build/src/tests/`; run one from a scratch directory (it writes its VTK files in the current
directory):

```bash
mkdir -p run && cd run
../build/src/tests/vtk_fortran_write_vtu
```

Each test program prints `Are all tests passed? T` (or `F`). The tests are not registered with CTest: to run the whole suite,
use the FoBiS.py build below.

### CMake subdirectory integration

To embed VTKFortran in an existing CMake project, place a clone (with its dependencies fetched) alongside your sources and add
to your `CMakeLists.txt`:

```cmake
add_subdirectory(VTKFortran)

target_link_libraries(your_target VTKFortran::VTKFortran)
```

## Build with FoBiS.py

[FoBiS.py](https://github.com/szaghi/FoBiS) (CLI `fobis`) is also used for coverage analysis and documentation generation.

### List all build modes

```bash
fobis build --lmodes
```

### Build and run tests

```bash
fobis build --mode tests-gnu
scripts/run_tests.sh
```

Compiled test executables are placed in `./exe/`. `scripts/run_tests.sh` runs each executable and reports pass/fail from
its exit status.

### Build the library

```bash
# Static library (GNU gfortran)
fobis build --mode static-gnu

# Shared library (GNU gfortran)
fobis build --mode shared-gnu

# Intel Fortran variants
fobis build --mode static-intel
fobis build --mode shared-intel
```

### Debug builds

```bash
fobis build --mode static-gnu-debug
fobis build --mode tests-gnu-debug   # with zlib, as the CI
```

### Coverage and documentation

```bash
fobis rule --ex makecoverage   # build + run tests + gcov report
fobis rule --ex makedoc        # build the API reference (formal) and the VitePress site
```

`makecoverage` calls `scripts/compute-coverage.sh`, which automatically selects the `gcov-N` binary that matches the installed `gfortran` version. If you run the script directly, ensure that `gfortran` is on `$PATH` so the version is detected correctly.

## Build with FPM

```bash
fpm build
```

fpm builds the library (and fetches its dependencies); the test programs are built with CMake or FoBiS.py.

## Optional zlib compression

The compression of binary data (`compressor='zlib'`, `format='raw-zlib'`, see
[Usage](/guide/usage#compressed-binary-data-zlib)) needs [zlib](https://zlib.net) and the preprocessor macro
`VTKFORTRAN_USE_ZLIB` at build time. Without it the library builds and works as usual, and requesting zlib makes
`initialize` return a non-zero error. Install the zlib development files first (e.g. `apt install zlib1g-dev`).

**CMake**: enable the option; zlib is found with `find_package(ZLIB)`.

```bash
cmake -S . -B build -DVTKFORTRAN_USE_ZLIB=ON
cmake --build build
```

**FoBiS.py**: the `tests-gnu-debug` mode (the CI one) is built with zlib. For other modes, or your own fobos, add
`-DVTKFORTRAN_USE_ZLIB` to `cflags` and link zlib with `ext_libs = z`.

**fpm**: pass the macro and link zlib. With GNU ld, `--no-as-needed` is needed because zlib is listed before the library
that uses it:

```bash
fpm build --flag "-DVTKFORTRAN_USE_ZLIB" --link-flag "-Wl,--no-as-needed -lz"
```
