# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Build Commands

### CMake (recommended)
```bash
cmake -S . -B build -DBUILD_TESTING=ON   # add -DVTKFORTRAN_USE_ZLIB=ON for zlib compression
cmake --build build
ctest --test-dir build                   # each test runs in build/src/tests/run/<name>/
```

### FoBiS.py
```bash
fobis fetch                          # Fetch the dependencies into src/third_party/ (needed by CMake and FoBiS builds)
fobis build --mode tests-gnu         # Build and place test executables in ./exe/
fobis build --mode tests-gnu-debug   # As the CI: debug, with zlib (VTKFORTRAN_USE_ZLIB)
fobis build --mode static-gnu        # Build static library
fobis build --lmodes                 # List all available modes
scripts/run_tests.sh                 # Run all executables in ./exe/: pass/fail from the exit status
```

### Documentation examples
```bash
FC=gfortran-14 PVPYTHON=<paraview>/bin/pvpython bash scripts/docs_examples.sh   # regenerate docs/examples (commit them)
```
Programs in `docs/examples/src/` with markers (`!run`, `!region`, `!cast`, `!render`, ...); the pages include the
generated snippets, outputs, terminal casts (`scripts/ansi2svg.py`) and ParaView renders (`scripts/render_vtk.py`, PNG or
GIF for a `.pvd`). The *Docs examples* workflow checks all but the renders (no ParaView in CI).
Generated, git-ignored, rebuilt by the Docs workflow: `docs/api/` (`formal generate ...`, rule `makedoc`), the coverage
pages `docs/guide/coverage-analysis.md`, `docs/guide/*.gcov.md` and `docs/public/coverage.json` (rule
`makecoverage-analysis`); the site builds without them.

### FPM
```bash
fpm build   # library only; zlib: --flag "-DVTKFORTRAN_USE_ZLIB" --link-flag "-Wl,--no-as-needed -lz"
```

zlib compression (`compressor='zlib'`, `raw-zlib`) is optional: CMake `-DVTKFORTRAN_USE_ZLIB=ON`, FoBiS
`-DVTKFORTRAN_USE_ZLIB` + `ext_libs = z`. Without it `vtk_fortran_zlib` holds stubs and requesting zlib is an error.

## Architecture

VTKFortran is a Fortran 2008 library for reading/writing VTK XML format files. It uses a **polymorphic writer pattern**: the user-facing `vtk_file` type holds a polymorphic `xml_writer` component that is allocated at runtime based on the requested format.

### Module hierarchy

- `vtk_fortran` — main API module (re-exports `vtk_file`, `pvtk_file`, `vtm_file`, `pvd_file`, `write_xml_volatile`)
- `vtk_fortran_vtk_file` — `vtk_file` type: single-file serial writer; selects and allocates the appropriate `xml_writer` concrete type; with `initialize(filename, action='read')` it reads instead, through its `xml_reader` component
- `vtk_fortran_pvtk_file` — `pvtk_file` type: parallel/partitioned VTK files (`.pvtr`, `.pvts`, `.pvtu`); the header is plain XML metadata, the pieces can use any format
- `vtk_fortran_vtm_file` — `vtm_file` type: multi-block composite datasets (`.vtm`)
- `vtk_fortran_pvd_file` — `pvd_file` type: time series collections (`.pvd`), valid after each `write_dataset`
- `vtk_fortran_vtk_file_xml_writer_abstract` — abstract base class defining the common interface (`initialize`, `finalize`, `write_piece`, `write_geo`, `write_connectivity`, `write_dataarray`, `get_xml_volatile`)
- Three concrete writer implementations:
  - `vtk_fortran_vtk_file_xml_writer_ascii_local` — human-readable ASCII
  - `vtk_fortran_vtk_file_xml_writer_binary_local` — Base64-encoded binary inside XML elements
  - `vtk_fortran_vtk_file_xml_writer_appended` — raw binary in appended section with offsets
- `vtk_fortran_vtk_file_xml_reader` — `xml_reader` type: reader of serial files (any format, header type, zlib) and parallel headers (`check_pieces`); indexes the file once, then decodes only the arrays asked for; error codes 0–7 documented in the module. `pvtk_file` uses it too (`action='read'`); `vtm_file` and `pvd_file` read with `get_entries`/`get_datasets`
- `vtk_fortran_xml_scanner` — XML scanner indexing elements, attributes and content positions without loading the file (depends only on PENF: meant to move into FoXy)
- `vtk_fortran_dataarray_decoder` — inverse of the encoder: ASCII/Base64/zlib data into bytes, bytes into arrays of the requested kind
- `vtk_fortran_dataarray_encoder` — overloaded encoding routines for ASCII and Base64 (optionally zlib-compressed), covering all PENF numeric kinds and ranks 1–4
- `vtk_fortran_zlib` — zlib bindings, VTK block compression and decompression (`zlib_compress_blocks`, `zlib_uncompress_blocks`); always compiled, stubs without `VTKFORTRAN_USE_ZLIB`
- `vtk_fortran_parameters` — shared constants (`stderr`, `stdout`, `end_rec`)

Source lives in `src/lib/` (library) and `src/tests/` (integration test programs).

### Third-party dependencies (fetched by `fobis fetch` into `src/third_party/`)

| Library | Purpose |
|---------|---------|
| **PENF** | Portable numeric kind parameters (`I1P`, `I4P`, `R8P`, etc.) — used everywhere |
| **BeFoR64** | Base64 encode/decode for binary XML data |
| **StringiFor** | OOP `string` type used throughout the writer classes |
| **FoXy** | XML tag parsing/emitting |
| **FACE** | ANSI terminal colour output |

CMake pulls all dependencies via `add_subdirectory()` and centralises `.mod` files under `${PROJECT_BINARY_DIR}/src/third_party/<LIB>/modules/`.

## Coding Conventions (from CONTRIBUTING.md)

- `implicit none` in every program unit
- Explicit `intent` on all dummy arguments
- 2-space indentation, no tabs
- Modern relational operators (`>`, `<`, `==`, not `.gt.`, `.lt.`, `.eq.`)
- No trailing whitespace; Unix line endings

## Test Infrastructure

Each test program in `src/tests/` writes actual VTK XML files, then prints `"Are all tests passed? T"` or `"F"`. `scripts/run_tests.sh` and CTest judge each test by its exit status only, so every test ends with `if (.not.all(test_passed)) error stop 'some tests failed'`; a new test must be added to `src/tests/CMakeLists.txt` too. Tests cover major topologies: VTI and PVTI (image data), VTR and PVTR (rectilinear), VTS (structured), VTU (unstructured, polyhedra), VTP and PVTP (polydata), VTM (multi-block, nested blocks), PVTS and PVTU (parallel), files with several pieces, unsigned arrays, PVD (time series), active arrays, large arrays, UInt64 headers, 64-bit (I8P) counts and connectivity, zlib compressed binary data, and volatile XML output; `vtk_fortran_read.F90` reads files back (every topology and format, and two files written by VTK, embedded in the test), `vtk_fortran_read_composite.f90` parallel headers (and their check), multi-block and time series files.
