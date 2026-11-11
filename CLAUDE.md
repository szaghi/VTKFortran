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
FC=gfortran-14 PVPYTHON=<paraview>/bin/pvpython MPIFC=<openmpi>/bin/mpifort bash scripts/docs_examples.sh   # regenerate docs/examples
```
Programs in `docs/examples/src/` with markers (`!run`, `!region`, `!cast`, `!render`, ...); the pages include the
generated snippets, outputs, terminal casts (`scripts/ansi2svg.py`) and ParaView renders (`scripts/render_vtk.py`, PNG or
GIF for a `.pvd`; `!mpi` programs need `MPIFC`, `!hdf5` ones the HDF5 C library in `HDF5_LIB`, default Ubuntu's
libhdf5-dev, `!mpi` + `!hdf5` ones `MPIFC` and a parallel HDF5 installed in `HDF5_MPI` (e.g. ADAM's
`~/fortran/adam/lib/hdf5/develop/gnu/16.2.0` with Open MPI 5.0.11's mpifort), otherwise their committed outputs are
kept). The *Docs examples* workflow (with libhdf5-dev) checks all but
the renders and the MPI runs (no ParaView, no MPI in CI). The reference pages
(`docs/guide/*.md`) hold no code of their own: their snippets are regions of `ref_<page>.f90`, programs that also read
back what they write and stop with an error otherwise.
Generated, git-ignored, rebuilt by the Docs workflow: `docs/api/` (`formal generate ...`, rule `makedoc`), the coverage
pages `docs/guide/coverage-analysis.md`, `docs/guide/*.gcov.md` and `docs/public/coverage.json` (rule
`makecoverage-analysis`); the site builds without them.

### FPM
```bash
fpm build   # library only; zlib: --flag "-DVTKFORTRAN_USE_ZLIB" --link-flag "-Wl,--no-as-needed -lz"
```

zlib compression (`compressor='zlib'`, `raw-zlib`) is optional: CMake `-DVTKFORTRAN_USE_ZLIB=ON`, FoBiS
`-DVTKFORTRAN_USE_ZLIB` + `ext_libs = z`. Without it `vtk_fortran_zlib` holds stubs and requesting zlib is an error.

HDF5 (VTKHDF format, `format='vtkhdf'`, issue #51: every topology done, the rest in progress) is optional
too: CMake
`-DVTKFORTRAN_USE_HDF5=ON` (`find_package(HDF5 COMPONENTS C)`), FoBiS modes `static-gnu-hdf5`, `tests-gnu-debug-hdf5`
(varset `ubuntu` = libhdf5-dev, add one for another HDF5), workflow `hdf5.yml`. Only the HDF5 C library is used, bound
with iso_c_binding (no `hdf5.mod`). Without it `format='vtkhdf'` is an error.

One VTKHDF file from MPI ranks (`vtk_file%initialize(..., comm=)`) needs `VTKFORTRAN_USE_MPI` too and a parallel HDF5:
CMake `-DVTKFORTRAN_USE_MPI=ON` (MPI C and Fortran found at the top level; the test runs on 4 ranks, add
`-DMPIEXEC_PREFLAGS=--oversubscribe` on fewer cores), FoBiS modes `static-gnu-hdf5-mpi`, `tests-gnu-debug-hdf5-mpi`
(mpif90 + mpicc; the varset gives `$HDF5_INC` too; `ubuntu-openmpi` = libhdf5-openmpi-dev); no fpm. The C source
`src/mpi/vtk_fortran_mpi.c` (`MPI_Comm_f2c` + `H5Pset_fapl_mpio`, C only) sits outside `src/lib` (fpm's source dir) and
is excluded by the non-MPI FoBiS modes (`exclude_dirs = $EXDIRS src/mpi/`): they need no C compiler (ifx's family
default, `icx`, is often missing). CI: job `hdf5-mpi` of `hdf5.yml` in the toolchain image (FoBiS with the varset
`toolchain-image`, and CMake; the MPI test on 4 ranks); locally ADAM's HDF5 2.3.0 + Open MPI 5.0.11, the same stack.

CI toolchain: `ci.yml` (tests, coverage) and the build job of `docs.yml` run in the container
`ghcr.io/szaghi/fortran-toolchains:gnu-16.2` (repo `~/fortran/fortran-toolchains`: gfortran 16.2, Open MPI 5.0.11, HDF5
2.3.0 parallel with deflate, FoBiS, formal), set by `[scaffold] toolchain_image` in `fobos` (FoBiS scaffold: run the
sync with a FoBiS that has it, e.g. `~/python/FoBiS/.venv/bin/fobis` until a release). On the bare runner: the serial
`hdf5` job (gfortran 14, Ubuntu's HDF5 1.10, the oldest supported), *Docs examples* and `install.yml`.

## Architecture

VTKFortran is a Fortran 2008 library for reading/writing VTK XML format files. It uses a **polymorphic writer pattern**: the user-facing `vtk_file` type holds a polymorphic `xml_writer` component that is allocated at runtime based on the requested format.

### Module hierarchy

- `vtk_fortran` — main API module (re-exports `vtk_file`, `pvtk_file`, `vtm_file`, `pvd_file`, `write_xml_volatile`)
- `vtk_fortran_vtk_file` — `vtk_file` type: single-file serial writer; selects and allocates the appropriate `xml_writer` concrete type; with `initialize(filename, action='read')` it reads instead, through its `xml_reader` component
- `vtk_fortran_pvtk_file` — `pvtk_file` type: parallel/partitioned VTK files (`.pvtr`, `.pvts`, `.pvtu`); the header is plain XML metadata, the pieces can use any format
- `vtk_fortran_vtm_file` — `vtm_file` type: multi-block composite datasets (`.vtm`); with `initialize(...,
  format='vtkhdf')` one VTKHDF
  `MultiBlockDataSet`: the `.vtkhdf` datasets copied in (`H5Ocopy`, `Block_<n>`), the tree as `Assembly` soft links;
  time series blocks refused (VTK 9.7.2 reads only their first step)
- `vtk_fortran_pvd_file` — `pvd_file` type: time series collections (`.pvd`), valid after each `write_dataset`
- `vtk_fortran_vtk_writer_abstract` — format-neutral abstract writer (`vtk_writer_abstract`): the state `vtk_file` sets
  (origin, spacing, direction, header type, compression) and every procedure `vtk_file%xml_writer` exposes (`initialize`,
  `finalize`, `write_piece`, `write_geo`, `write_connectivity`, `write_polydata_cells`, `write_dataarray`,
  `write_dataarray_unsigned`, `write_fielddata`, `write_step`, `free`, `get_xml_volatile`), all deferred and PUBLIC (ifx does not
  override a private binding from another module). `vtk_file%xml_writer` is of this type (the name is kept); the XML
  writers and the VTKHDF writer extend it
- `vtk_fortran_vtk_file_xml_writer_abstract` — abstract XML writer, extends `vtk_writer_abstract`: implements the
  format-neutral procedures as XML tags, plus the XML-only ones (`write_parallel_*`, used by `pvtk_file` and `vtm_file`,
  whose `xml_writer` stays `class(xml_writer_abstract)`)
- Three concrete writer implementations:
  - `vtk_fortran_vtk_file_xml_writer_ascii_local` — human-readable ASCII
  - `vtk_fortran_vtk_file_xml_writer_binary_local` — Base64-encoded binary inside XML elements
  - `vtk_fortran_vtk_file_xml_writer_appended` — raw binary in appended section with offsets
- `vtk_fortran_vtk_file_vtkhdf_writer` — `vtkhdf_writer` (`format='vtkhdf'`), extends `vtk_writer_abstract`: the same
  calls write an HDF5 file in the VTKHDF layout. ImageData, RectilinearGrid and StructuredGrid: one piece, extent =
  whole extent (RG/SG: `Dimensions`, `X/Y/ZCoordinates` or a `Points` always 4D, Version 2.7). UnstructuredGrid: each
  piece a partition (counts appended to `NumberOf*`, points, cells and arrays appended to datasets shared by the pieces,
  offsets from 0, I4P/I8P ids, the same arrays in every piece; polyhedra as `FaceConnectivity`, `FaceOffsets`,
  `PolyhedronToFaces`, `PolyhedronOffsets` and their counts, zero entries for the pieces without, Version 2.5).
  PolyData: each piece a partition too, the four blocks of `write_polydata_cells` in the groups `Vertices`, `Lines`,
  `Strips`, `Polygons`, all four always written (VTK reads nothing without one), a block not given gets zero entries,
  `nc` of `write_geo` unused as in XML. Point/cell/field data, active arrays as the `Attribute` attribute, unsigned,
  deflate for `compressor='zlib'`; refusals return an error with a message on stderr. Arrays stored as in memory
  (`x(nc,i,j,k)` = VTKHDF `[k,j,i,nc]`), the axes of one point of a grid dropped (VTK requires it); `Version` written by
  `finalize` (2.0, 2.5 with polyhedra, 2.6 with active arrays, 2.7 for RG/SG). Its procedures repeat the abstract
  interfaces, one per kind and layout, all funnelled into the private `put_<kind>` (and `store_<kind>` for the appended
  datasets of an UnstructuredGrid and PolyData): change the kinds together. Time series (`write_step`, `Steps`): the
  `Steps` entries of the open step rewritten at the end of every piece (file valid after it); `reopen`
  (`vtk_file%initialize(..., action='append')`, reached by `select type`) reads the state back and trims every dataset
  to the last complete piece. vtkHDFReader 9.7.2 misreads a PolyData block first appearing at a later step, and a series
  of one part at step 1 (its own files too): the tests avoid both. MPI (`set_comm`, called by
  `vtk_file%initialize(..., comm=)` through `select type` before `initialize`, forgotten by `finalize`): every
  `store_<kind>` collective, totals summed over the ranks, one piece per rank; refused: grids, `write_step`, polyhedra,
  zlib, a second piece; a PolyData piece without cells calls `write_polydata_cells()` (the implicit empty blocks of
  `end_partition` would come out of the collective order)
- `vtk_fortran_vtk_file_vtkhdf_reader` — `vtkhdf_reader`: VTKHDF files read through `xml_reader` (dispatched when
  `initialize` sees the HDF5 signature): every topology, partitions as pieces, time series (`get_steps`, `set_step`),
  values as HDF5 hyperslabs of the piece/step converted by the decoder (`read_dataarray_bytes`), offsets back to end
  offsets, polyhedra back to the face stream. `xml_reader%finalize` stays pure (forgets the file); `close` closes it
- `vtk_fortran_vtk_file_xml_reader` — `xml_reader` type: reader of serial files (any format, header type, zlib) and parallel headers (`check_pieces`); indexes the file once with FoXy `xml_index` (tags and content positions, scan stopped at `AppendedData`), then decodes only the arrays asked for; error codes 0–7 documented in the module. `pvtk_file` uses it too (`action='read'`); `vtm_file` and `pvd_file` read with `get_entries`/`get_datasets`
- `vtk_fortran_dataarray_decoder` — inverse of the encoder: ASCII/Base64/zlib data into bytes, bytes into arrays of the requested kind
- `vtk_fortran_dataarray_encoder` — overloaded encoding routines for ASCII and Base64 (optionally zlib-compressed), covering all PENF numeric kinds and ranks 1–4
- `vtk_fortran_hdf5` — HDF5 C API bindings (files, groups and their members, links, attributes (overwritten),
  datasets of every kind, appendable, resized, deflate, read back); always compiled, stubs without
  `VTKFORTRAN_USE_HDF5`. With `comm` (`VTKFORTRAN_USE_MPI`): `h5_file_create` opens a parallel file, an appendable
  dataset is created by every rank (empty, chunks sized by all the rows, the widest kind the ranks give) and appended to
  collectively (`MPI_Allgather` of the rows, every rank extends by the total, writes its hyperslab after the ranks
  before; mpi_f08 under the macro). The `*_g` type globals must stay PUBLIC:
  gfortran gives private module variables hidden visibility, even with bind(C), and they would never bind to HDF5's
- `vtk_fortran_zlib` — zlib bindings, VTK block compression and decompression (`zlib_compress_blocks`, `zlib_uncompress_blocks`); always compiled, stubs without `VTKFORTRAN_USE_ZLIB`
- `vtk_fortran_parameters` — shared constants (`stderr`, `stdout`, `end_rec`)

Source lives in `src/lib/` (library) and `src/tests/` (integration test programs).

### Third-party dependencies (fetched by `fobis fetch` into `src/third_party/`)

| Library | Purpose |
|---------|---------|
| **PENF** | Portable numeric kind parameters (`I1P`, `I4P`, `R8P`, etc.) — used everywhere |
| **BeFoR64** | Base64 encode/decode for binary XML data |
| **StringiFor** | OOP `string` type used throughout the writer classes |
| **FoXy** | XML streaming writer (`xml_writer`, writers) and file indexing (`xml_index`, readers) |
| **FACE** | ANSI terminal colour output |

CMake pulls all dependencies via `add_subdirectory()` and centralises `.mod` files under `${PROJECT_BINARY_DIR}/src/third_party/<LIB>/modules/`.

## Coding Conventions (from CONTRIBUTING.md)

- `implicit none` in every program unit
- Explicit `intent` on all dummy arguments
- 2-space indentation, no tabs
- Modern relational operators (`>`, `<`, `==`, not `.gt.`, `.lt.`, `.eq.`)
- No trailing whitespace; Unix line endings

## Test Infrastructure

Each test program in `src/tests/` writes actual VTK XML files, then prints `"Are all tests passed? T"` or `"F"`. `scripts/run_tests.sh` and CTest judge each test by its exit status only, so every test ends with `if (.not.all(test_passed)) error stop 'some tests failed'`; a new test must be added to `src/tests/CMakeLists.txt` too. Tests cover major topologies: VTI and PVTI (image data), VTR and PVTR (rectilinear), VTS (structured), VTU (unstructured, polyhedra), VTP and PVTP (polydata), VTM (multi-block, nested blocks, blocks filled through scratch units), PVTS and PVTU (parallel), files with several pieces, files from OpenMP threads (CMake `-DVTKFORTRAN_TEST_OPENMP=ON`, 25 rounds of concurrent writing and reading, since a race shows now and then; passes with ifx and nvfortran, whose builds add `-recursive` and `-mp` to the library and its dependencies; gfortran fails, GCC bug 113797), unsigned arrays, PVD (time series), active arrays, large arrays, UInt64 headers, 64-bit (I8P) counts and connectivity, zlib compressed binary data, volatile XML output, the HDF5 bindings (`vtk_fortran_hdf5_bindings.F90`: a small
VTKHDF ImageData written and read back; stubs checked without HDF5) and the VTKHDF writer (`vtk_fortran_write_vtkhdf.f90`:
3D and 2D ImageData through `vtk_file`, read back with the bindings, refusals; `vtk_fortran_write_vtkhdf_unst.f90`:
UnstructuredGrid, every cell type but polyhedra, two pieces, I8P ids, each also written as a `.vtu` to compare in VTK;
`vtk_fortran_write_vtkhdf_steps.f90`: time series of every topology, each step also written as an XML file;
`vtk_fortran_write_vtkhdf_append.f90`: series continued with `action='append'` (restart, interrupted run);
`vtk_fortran_write_vtkhdf_vtm.f90`: a VTKHDF multi-block file (`vtm_file`, `format='vtkhdf'`);
`vtk_fortran_read_vtkhdf.f90`: VTKHDF files read back (every topology, time series);
`vtk_fortran_write_vtkhdf_polyh.f90`: polyhedra (one piece; three pieces with polyhedra in the second), each also
written as a `.vtu`; `vtk_fortran_write_vtkhdf_grids.f90`: RectilinearGrid and StructuredGrid (3D/2D/1D, the four StructuredGrid
`write_geo` layouts), each also written as a `.vtr`/`.vts`; `vtk_fortran_write_vtkhdf_poly.f90`: PolyData, the four
blocks, three pieces with different blocks (one without cells),
I8P, each also written as a `.vtp`; `vtk_fortran_write_vtkhdf_ranks.F90`: an UnstructuredGrid and a PolyData from MPI
ranks (one rank empty, I4P and I8P ids by rank), compared with the same pieces written serially and as `.vtu`/`.vtp`,
the refusals of `comm`; without MPI `comm` refused, its name without "mpi" since `scripts/run_tests.sh` runs those with
mpirun; without HDF5 the writer refuses to start); `vtk_fortran_read.F90` reads files back (every topology and format, and two files written by VTK, embedded in the test), `vtk_fortran_read_composite.f90` parallel headers (and their check), multi-block and time series files.
