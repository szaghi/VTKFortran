# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Build Commands

### CMake (recommended)
```bash
cmake -S . -B build -DBUILD_TESTING=ON
cmake --build build
ctest --test-dir build
```

### FoBiS.py
```bash
FoBiS.py build -mode tests-gnu       # Build and place test executables in ./exe/
FoBiS.py build -mode static-gnu      # Build static library
FoBiS.py build -lmodes               # List all available modes
bash run_tests.sh                     # Run all executables in ./exe/ and check pass/fail
```

### FPM
```bash
fpm build
fpm test
```

## Architecture

VTKFortran is a Fortran 2003+ library for reading/writing VTK XML format files. It uses a **polymorphic writer pattern**: the user-facing `vtk_file` type holds a polymorphic `xml_writer` component that is allocated at runtime based on the requested format.

### Module hierarchy

- `vtk_fortran` — main API module (re-exports `vtk_file`, `pvtk_file`, `vtm_file`, `write_xml_volatile`)
- `vtk_fortran_vtk_file` — `vtk_file` type: single-file serial writer; selects and allocates the appropriate `xml_writer` concrete type
- `vtk_fortran_pvtk_file` — `pvtk_file` type: parallel/partitioned VTK files (ASCII only)
- `vtk_fortran_vtm_file` — `vtm_file` type: multi-block composite datasets (`.vtm`)
- `vtk_fortran_vtk_file_xml_writer_abstract` — abstract base class defining the common interface (`initialize`, `finalize`, `write_piece`, `write_geo`, `write_connectivity`, `write_dataarray`, `get_xml_volatile`)
- Three concrete writer implementations:
  - `vtk_fortran_vtk_file_xml_writer_ascii_local` — human-readable ASCII
  - `vtk_fortran_vtk_file_xml_writer_binary_local` — Base64-encoded binary inside XML elements
  - `vtk_fortran_vtk_file_xml_writer_appended` — raw binary in appended section with offsets
- `vtk_fortran_dataarray_encoder` — overloaded encoding routines for ASCII and Base64, covering all PENF numeric kinds and ranks 1–4
- `vtk_fortran_parameters` — shared constants (`stderr`, `stdout`, `end_rec`)

Source lives in `src/lib/` (library) and `src/tests/` (8 integration test programs).

### Third-party dependencies (git submodules in `src/third_party/`)

| Library | Purpose |
|---------|---------|
| **PENF** | Portable numeric kind parameters (`I1P`, `I4P`, `R8P`, etc.) — used everywhere |
| **BeFoR64** | Base64 encode/decode for binary XML data |
| **StringiFor** | OOP `string` type used throughout the writer classes |
| **FoXy** | XML tag parsing/emitting |
| **FACE** | ANSI terminal colour output |

CMake pulls all submodules via `add_subdirectory()` and centralises `.mod` files under `${PROJECT_BINARY_DIR}/src/third_party/<LIB>/modules/`.

## Coding Conventions (from CONTRIBUTING.md)

- `implicit none` in every program unit
- Explicit `intent` on all dummy arguments
- 2-space indentation, no tabs
- Modern relational operators (`>`, `<`, `==`, not `.gt.`, `.lt.`, `.eq.`)
- No trailing whitespace; Unix line endings

## Test Infrastructure

Each test program in `src/tests/` writes actual VTK XML files, then prints `"Are all tests passed? T"` or `"F"`. `run_tests.sh` collects these results and exits non-zero if any test fails. Tests cover major topologies: VTR (rectilinear), VTS (structured), VTU (unstructured), VTM (multi-block), PVTS (parallel structured), and volatile XML output.
