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

VTKFortran uses **git submodules** for its third-party dependencies. Clone recursively:

```bash
git clone https://github.com/szaghi/VTKFortran --recursive
cd VTKFortran
```

If you already have a non-recursive clone:

```bash
git submodule update --init --recursive
```

### Third-Party Dependencies

The submodules live under `src/third_party/`:

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

### Run the test suite

```bash
cmake -S . -B build -DBUILD_TESTING=ON
cmake --build build
ctest --test-dir build
```

Run a single named test:

```bash
ctest --test-dir build -R vtk_fortran_write_vtu
```

### CMake subdirectory integration

To embed VTKFortran in an existing CMake project, place a recursive clone alongside your sources and add to your `CMakeLists.txt`:

```cmake
add_subdirectory(VTKFortran)

target_link_libraries(your_target VTKFortran::VTKFortran)
```

## Build with FoBiS.py

[FoBiS.py](https://github.com/szaghi/FoBiS) is used for coverage analysis and documentation generation.

```bash
pip install FoBiS.py
```

### List all build modes

```bash
FoBiS.py build -lmodes
```

### Build and run tests

```bash
FoBiS.py build -mode tests-gnu
bash run_tests.sh
```

Compiled test executables are placed in `./exe/`. `run_tests.sh` runs each executable and reports pass/fail based on the `"Are all tests passed? T/F"` output line.

### Build the library

```bash
# Static library (GNU gfortran)
FoBiS.py build -mode static-gnu

# Shared library (GNU gfortran)
FoBiS.py build -mode shared-gnu

# Intel Fortran variants
FoBiS.py build -mode static-intel
FoBiS.py build -mode shared-intel
```

### Debug builds

```bash
FoBiS.py build -mode static-gnu-debug
FoBiS.py build -mode tests-gnu-debug
```

### Coverage and documentation

```bash
FoBiS.py rule -ex makecoverage   # build + run tests + gcov report
FoBiS.py rule -ex makedoc        # build FORD API documentation
```

`makecoverage` calls `scripts/compute-coverage.sh`, which automatically selects the `gcov-N` binary that matches the installed `gfortran` version. If you run the script directly, ensure that `gfortran` is on `$PATH` so the version is detected correctly.

## Build with FPM

```bash
fpm build
fpm test
```

## Build with GNU Make

```bash
make                   # default build
make TESTS=yes         # build with tests
make DEBUG=yes         # debug mode
make COMPILER=intel    # use Intel Fortran
make OPTIMIZE=yes      # enable -O3
make OPENMP=yes        # enable OpenMP
make MPI=yes           # enable MPI
```
