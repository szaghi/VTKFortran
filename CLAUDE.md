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
2.3.0 parallel with deflate, FoBiS, formal), set by `[scaffold] toolchain_image` in `fobos` (FoBiS >= 3.16.5 renders it;
an older `fobis scaffold sync` would revert both workflows to the bare runner). On the bare runner: the serial
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
  all static or all time series (the first block decides), series blocks UnstructuredGrid or PolyData with the same
  `NSteps` and times (grid series refused: VTK 9.7.2 reads them without arrays), checked on the file before the copy.
  ParaView 6.1.1 plays every step; VTK 9.7.2 reads each with `SetStep`, its pipeline time stays at step 1 on a
  multi-block (fine on a single dataset). `composite='PartitionedDataSetCollection'` (`is_collection`): each copy named
  after its dataset (VTK names a partitioned dataset after its group) with `Index`, unique names (not `Assembly`),
  `write_block(references='name ...')` (string form) soft-links a copy into the open block (`vtkhdf_reference`);
  references refused outside a collection; the same time series rules (VTK 9.7.2 pipeline time likewise at step 1)
- `vtk_fortran_pvd_file` — `pvd_file` type: time series collections (`.pvd`), valid after each `write_dataset`
- `vtk_fortran_vtk_writer_abstract` — format-neutral abstract writer (`vtk_writer_abstract`): the state `vtk_file` sets
  (origin, spacing, direction, header type, compression, `branch_factor`, `chunk_bytes`) and every procedure `vtk_file%xml_writer` exposes (`initialize`,
  `finalize`, `write_piece`, `write_geo`, `write_connectivity`, `write_polydata_cells`, `write_dataarray`,
  `write_dataarray_unsigned`, `write_fielddata`, `write_step`, `free`, `get_xml_volatile`; `write_dataarray` opening a
  location takes the 12 attribute roles of VTK, `scalars` ... `tcoords`, `global_ids` ... `process_ids`), all deferred and PUBLIC (ifx does not
  override a private binding from another module). `vtk_file%xml_writer` is of this type (the name is kept); the XML
  writers and the VTKHDF writer extend it
- `vtk_fortran_vtk_file_xml_writer_abstract` — abstract XML writer, extends `vtk_writer_abstract`: implements the
  format-neutral procedures as XML tags, plus the XML-only ones (`write_parallel_*`, used by `pvtk_file` and `vtm_file`,
  whose `xml_writer` stays `class(xml_writer_abstract)`). Table (`.vtt`, #86): `location='row'` (`write_table_rows`)
  swaps the FoXy writer for one on memory (`table_writer` keeps the file writer) holding `RowData`; at the close the
  `Piece NumberOfCols NumberOfRows` tag is written, then the buffered columns (indented by 6) with `raw`: FoXy keeps a
  start tag pending only until its first content, so the counts cannot be patched later. Rows: every one of the 144
  `write_dataarray<N>_rank<M>_<kind>` of the three concrete writers sets `n_tuples` before its tag, checked by
  `is_table_column` (the first column's rows; a mismatch writes no tag). VTK 9.7.2 needs `NumberOfRows` (absent: 0
  rows), ignores `NumberOfCols`. Refused for a Table: pieces, node/cell locations, the rows opened twice
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
  `nc` of `write_geo` unused as in XML. Table (`mesh_topology='Table'`, Version 2.8): no pieces, columns in `RowData`
  written with `write_dataarray(location='row', action='open'|'close')` (every kind, components; the same rows,
  `table_rows`), `NumberOfRows` written at the close (`end_table`); a series appends the rows of each step
  (`NumberOfRows` one entry per step, `Steps/RowDataOffsets/<col>` = `row_total`, VTK's spelling, not the spec page's
  `RowDataOffets`); refused: `comm`, a step without columns (`step_pieces` is the
  "columns opened" flag); VTK 9.7.2 reads it, ParaView 6.1.1 does not. OverlappingAMR (`mesh_topology='OverlappingAMR'`,
  `TOPOLOGY_AMR`): `write_level(level, spacing)` (the only AMR binding, deferred in `vtk_writer_abstract`, XML writers
  refuse) opens `Level<n>` (`open_level`, `level_group`; `open_group` puts PointData/CellData in it), each box a
  `write_piece(nx1..nz2)` (`start_box`: `AMRBox` row = cells, inside the level-0 bounds `domain`, >= 1 cell per axis)
  closed by `write_piece()` (`end_box`: every recorded array holds the level's tuples, `level_counts`); series:
  `Steps/Level<n>/AMRBoxOffset`, `NumberOfAMRBox`, `PointDataOffset/`, `CellDataOffset/` (VTK 9.7.2's singular names);
  no field data, comm, reopen; VTK 9.7.2 reads it (pipeline time too), ParaView 6.1.1 not (vtkGroupDataSetsFilter).
  HyperTreeGrid (`TOPOLOGY_HTG`, Version 2.4): `branch_factor` (vtk_writer_abstract state, set by vtk_file),
  one piece + `write_geo(x, y, z)` (rect coordinates path), `write_tree(tree_id, descriptor, mask)` (new deferred
  binding; depths derived from the descriptor, appended with capacity doubling `append_I8P`/`append_bits`), cell data
  after the trees (`is_tree_data`), `end_trees` writes the datasets at the piece end (`pack_bits`, MSB first, as VTK);
  series: every step its piece, the trees appended (`store_*`, the mask always), `tree_totals` = the trees, depth
  entries, bytes, cells of the steps before, the `Steps` entries VTK writes (TreeIdsOffsets, DescriptorsOffsets in
  bytes, ...); refused: point/field data, a step without its piece, comm, append; read back by the reader (#87).
  Point/cell/field data, active arrays as the `Attribute` attribute, unsigned,
  deflate for `compressor='zlib'`; refusals return an error with a message on stderr (a data name empty or with `/` or
  `.`: `h5_name_invalid`, also used by `vtm_file` for block and dataset names; never renamed; HDF5 has no length limit). Arrays stored as in memory
  (`x(nc,i,j,k)` = VTKHDF `[k,j,i,nc]`), the axes of one point of a grid dropped (VTK requires it); every chunked
  dataset (appendable or deflated) created with `chunk_bytes=self%chunk_bytes` (`vtk_file%initialize(..., chunk_bytes=)`,
  #85; refused not positive, with XML or `action='read'`, different on the ranks (`h5_mpi_max` in `vtk_file`; the
  binding takes the ranks' maximum); with `append` only the new datasets; created empty: one-row chunks); `Version` written by
  `finalize` (2.0, 2.5 with polyhedra, 2.6 with active arrays, 2.7 for RG/SG, 2.8 Table). Its procedures repeat the abstract
  interfaces, one per kind and layout, all funnelled into the private `put_<kind>` (and `store_<kind>` for the appended
  datasets of an UnstructuredGrid and PolyData): change the kinds together. Time series (`write_step`, `Steps`): the
  `Steps` entries of the open step rewritten at the end of every piece (file valid after it); what a step does not
  write is stored once, its offsets repeating the step before (`mesh_start`/`mesh_blocks`/`mesh_pieces` = the last step
  with pieces, reused by a step without pieces whose data groups open directly; `geo_slices` for RG/SG; per-array
  `point_start`/`point_prev`, `cell_start`/`cell_prev` in the order of the name lists, `roll_arrays` at `write_step`;
  `PointDataOffsets`/`CellDataOffsets` written for every array, time slices for grids, as VTK 9.7.2 reads them); a
  step with pieces writes every point/cell array; no `StaticMesh` (no reader reads it). `reopen`
  (`vtk_file%initialize(..., action='append')`, reached by `select type`) reads the state back (the reuse state too) and
  trims every dataset to the last complete piece (`reopen_file`; a refusal closes the file and resets the writer, so the
  caller's `finalize` writes nothing: it used to rewrite `Version` as 2.0). vtkHDFReader 9.7.2 misreads a PolyData block first appearing at a later step, and a series
  of one part at step 1 (its own files too): the tests avoid both. MPI (`set_comm`, called by
  `vtk_file%initialize(..., comm=)` through `select type` before `initialize`, forgotten by `finalize`): every
  `store_<kind>` collective, totals summed over the ranks, one piece per rank; grids by regions (`write_piece` with the
  rank's sub-extent, `start_region` gathers and checks them: inside, no cell overlap, cover; empty = upper < lower;
  `region_block`/`points_block`: datasets created with the whole extent by every rank, each writes its block
  (`h5_dataset_slab_<kind>`), the points of an upper boundary inside the extent left to the next region; RG coordinates
  by `axis_runs`, the entries no lower rank has, as many writes on every rank); a check of a rank's own values before
  a collective call goes through `agree` (`h5_mpi_max` of the errors: every rank refuses, none hangs; also
  `start_partition`, `start_region`, `write_connectivity`); every parallel `put_<kind>` compares a hash of the array
  name over the ranks (`agree_name`): different arrays in a call refused on every rank; a rank leaving a call out
  hangs the others (documented, not detectable); grid time series (#88): a region per rank per step (`start_region`
  each step), every dataset one time slice more (`h5_dataset_slab_<kind>(..., append=.true.)`: created appendable,
  chunked as `dataset_write`, or extended by `dims(last)` collectively, the block after the old rows; RG coordinates
  offset by `geo_slices * grid(a)`), Steps entries as serial (rank 0), reopen with comm generic; refused:
  zlib for a grid, a second piece or region in a step. Polyhedra with comm: decided at
  `write_connectivity` (`h5_mpi_max` of a flag), `start_polyhedra` writes the zero entries of the earlier parts once
  (rank 0), ranks without faces `no_faces` (the same store sequence as `put_faces`, `add_face_totals` after it); zlib with
  comm: `h5_parallel_filters_available` (HDF5 >= 1.10.2 + deflate), appends collective (`vtkfortran_h5_dxpl_mpio_collective`
  of `src/mpi`, an empty selection for a rank without rows, values converted in memory to the dataset type: a
  conversion breaks the collective write), static parallel field data not deflated. Time series (UG/PolyData) with comm:
  `write_step` checks the same time (max/min of its bits), `n_pieces`/`step_pieces` global (sums), `local_pieces`/
  `step_local` per process, `Steps` entries and temporal field data written by rank 0 (others append 0 rows), totals of
  ids/blocks summed; a reusing step: each rank its part (`np_piece`); `reopen` with comm (`h5_file_open(comm=)`; the
  rank's part of the last mesh if the parts = ranks); reopen trims each array to the largest offset+rows over the
  complete steps (`step_tuples`); a PolyData piece without cells calls `write_polydata_cells()` (the implicit empty blocks of
  `end_partition` would come out of the collective order)
- `vtk_fortran_vtk_file_vtkhdf_reader` — `vtkhdf_reader`: VTKHDF files read through `xml_reader` (dispatched when
  `initialize` sees the HDF5 signature): every topology, partitions as pieces, time series (`get_steps`, `set_step`),
  values as HDF5 hyperslabs of the piece/step converted by the decoder (`read_dataarray_bytes`), offsets back to end
  offsets, a Table's columns as `location='row'` (`npieces` 0; a series column needs its `RowDataOffsets`, error 6
  otherwise, as VTK), an OverlappingAMR level by level (`get_levels`, `set_level` from 0; its boxes as pieces, `amr_box`,
  `prefix` = `Level<n>/`), polyhedra back to the face stream (VTKHDF 2.9 shared faces, `-(n+1)` in `PolyhedronToFaces`, given to each
  polyhedron, reversed where marked; a face id out of range is error 6; VTK 9.7.2 cannot read such files, the nightly
  of 2026-11-08 misreads the reversed faces), a HyperTreeGrid (#87: `get_trees(tree_ids, branch_factor)`,
  `read_tree(tree_id, descriptor, mask)` via `tree_layout` (ids, depths, cells per depth, first bytes of the step) and
  `read_bits` (MSB first, `skip` bits from a byte); cell data from `NumberOfCells`/`Steps/CellDataOffsets` in
  `array_slice`; `step_entry` = the step's entry of a one-per-step dataset), a dataset of a composite
  (`initialize(filename, dataset)`: root `/VTKHDF/<dataset>`, through the Assembly's soft links too).
  `xml_reader%finalize` stays pure (forgets the file); `close` closes it
- `vtk_fortran_vtk_file_xml_reader` — `xml_reader` type: reader of serial files (any format, header type, zlib; a `.vtt` Table: `npieces` 0, `location='row'` = `Piece/RowData`, tuples from `NumberOfRows`) and parallel headers (`check_pieces`); indexes the file once with FoXy `xml_index` (tags and content positions, scan stopped at `AppendedData`), then decodes only the arrays asked for; `get_active(location, role, data_name)` the active array of a role (the
roles list `ACTIVE_ROLES` and `active_role` in `vtk_fortran_parameters`, shared with the VTKHDF reader); error codes 0–7 documented in the module. `pvtk_file` uses it too (`action='read'`); `vtm_file` and `pvd_file` read with `get_entries`/`get_datasets` (`vtm_file` a VTKHDF composite too:
`vtkhdf_entries` walks its `Assembly` once at `initialize`, groups = blocks, soft links = datasets (`h5_link_target`,
H5Lget_val: a hard link fails silently), `file` = the link target under /VTKHDF)
- `vtk_fortran_dataarray_decoder` — inverse of the encoder: ASCII/Base64/zlib data into bytes, bytes into arrays of the requested kind
- `vtk_fortran_dataarray_encoder` — overloaded encoding routines for ASCII and Base64 (optionally zlib-compressed), covering all PENF numeric kinds and ranks 1–4
- `vtk_fortran_hdf5` — HDF5 C API bindings (files, groups and their members (in creation order when tracked), links,
  attributes (overwritten), datasets of every kind, appendable, resized, deflate, read back; chunks of whole rows up to
  the optional `chunk_bytes` of `h5_dataset_write_<kind>` (default `DEFAULT_CHUNK_BYTES` = 1 MiB; never named
  `CHUNK_BYTES`: Fortran is case insensitive, it would be the dummy), at most the rows of the creation, read back by
  `h5_dataset_chunk`); always compiled, stubs without
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

Each test program in `src/tests/` writes actual VTK XML files, then prints `"Are all tests passed? T"` or `"F"`. `scripts/run_tests.sh` and CTest judge each test by its exit status only, so every test ends with `if (.not.all(test_passed)) error stop 'some tests failed'`; a new test must be added to `src/tests/CMakeLists.txt` too. Tests cover major topologies: VTI and PVTI (image data), VTR and PVTR (rectilinear), VTS (structured), VTU (unstructured, polyhedra), VTP and PVTP (polydata), VTM (multi-block, nested blocks, blocks filled through scratch units), PVTS and PVTU (parallel), files with several pieces, files from OpenMP threads (CMake `-DVTKFORTRAN_TEST_OPENMP=ON`, 25 rounds of concurrent writing and reading, since a race shows now and then; passes with ifx and nvfortran, whose builds add `-recursive` and `-mp` to the library and its dependencies; gfortran fails, GCC bug 113797), unsigned arrays, PVD (time series), active arrays (every role of VTK, XML and VTKHDF, read back with `get_active`; partitions with global ids and ghosts), large arrays, UInt64 headers, 64-bit (I8P) counts and connectivity, zlib compressed binary data, volatile XML output, the HDF5 bindings (`vtk_fortran_hdf5_bindings.F90`: a small
VTKHDF ImageData written and read back, chunk shapes (default, `chunk_bytes`, one row at least, the rows written at
most, a refusal); stubs checked without HDF5) and the VTKHDF writer (`vtk_fortran_write_vtkhdf.f90`:
3D and 2D ImageData through `vtk_file`, read back with the bindings, refusals; `vtk_fortran_write_vtkhdf_unst.f90`:
UnstructuredGrid, every cell type but polyhedra, two pieces, I8P ids, each also written as a `.vtu` to compare in VTK;
`vtk_fortran_write_vtkhdf_steps.f90`: time series of every topology, each step also written as an XML file, and five
storing once what does not change (a mesh reused by steps without pieces, grid geometry and arrays reused), their
offsets read back, the reuse refusals; `chunk_bytes`: chunk shapes of a UG and an RG series with small chunks, kept by
an append with another value (the polyhedron offsets created after it get the new one), a deflated ImageData, the
refusals;
`vtk_fortran_write_vtkhdf_table.f90`: Tables (a column of each kind, unsigned, 3 components; a 3-step series), read
back with the bindings and the reader, the refusals (rows, names, missing/new columns, steps without columns, pieces,
locations; the XML formats refuse an OverlappingAMR);
`vtk_fortran_write_vtt.f90`: XML Tables (`.vtt`) in ascii, binary, raw, binary-appended and raw zlib (columns of every
kind, unsigned, 3 components as an array and as x, y, z), read back (names in order, types, `NumberOfRows` tuples,
values), the same Table as VTKHDF (VTK compares them outside the test), the refusals, a time series as a `.pvd` of two
`.vtt` read back;
`vtk_fortran_write_vtkhdf_htg.f90`: HyperTreeGrids (the 2x2 root grid of the VTK probe: trees of depth 3, 2, a
masked leaf, a root cell without tree; branch factor 3; a 2-step series), checked with the bindings (descriptors and
mask unpacked, the Steps entries; append refused), 16 refusals, the three files read back with the reader (every
tree and mask as written, cell data, coordinates, the series step by step, a missing tree);
`vtk_fortran_write_vtkhdf_amr.f90`: OverlappingAMRs (two levels, three boxes, point/cell data and a vector; a
2-step series with level 1 going from two boxes to one), read back with the bindings and the reader (`set_level`), 15
refusals (levels, boxes outside or flat or out of order, array sizes, missing/new arrays, field data, steps, append);
`vtk_fortran_write_vtkhdf_append.f90`: series continued with `action='append'` (restart, interrupted run), reuse across
the restart, an orphan offset entry, a restart after a step with an empty piece;
`vtk_fortran_write_vtkhdf_vtm.f90`: VTKHDF multi-block files (`vtm_file`, `format='vtkhdf'`): the nested assembly, a
time series of two UnstructuredGrid series (one of two parts) and a PolyData series, both also as
PartitionedDataSetCollections with a block of references, the refusals (grid series, static and series mixed, other
steps or times; collection names, references, composites); the four files read back (`vtm_file` read: the entries;
every dataset with `dataset=` compared with its source step by step, integer arrays as I8P), the refusals of `dataset`;
`vtk_fortran_read_vtkhdf.f90`: VTKHDF files read back (every topology, time series, two polyhedra sharing a face
(Version 2.9, the file built with the bindings), a face id out of range, series storing once the mesh and arrays);
`vtk_fortran_write_vtkhdf_polyh.f90`: polyhedra (one piece; three pieces with polyhedra in the second), each also
written as a `.vtu`; `vtk_fortran_write_vtkhdf_grids.f90`: RectilinearGrid and StructuredGrid (3D/2D/1D, the four StructuredGrid
`write_geo` layouts), each also written as a `.vtr`/`.vts`; `vtk_fortran_write_vtkhdf_poly.f90`: PolyData, the four
blocks, three pieces with different blocks (one without cells),
I8P, each also written as a `.vtp`; `vtk_fortran_write_vtkhdf_ranks.F90`: an UnstructuredGrid and a PolyData from MPI
ranks (one rank empty, I4P and I8P ids by rank), an ImageData, a RectilinearGrid and a StructuredGrid by regions (on 4
ranks two along x, one across, one empty; otherwise rank 0 whole), an UnstructuredGrid series (pieces changing size,
continued by the ranks after a restart, a step reusing the mesh), a PolyData series, polyhedra appearing at step 2 on the odd ranks,
the UG and PolyData compressed (with parallel filtered writes, skipped otherwise), compared with the same files written serially and
as `.vtu`/`.vtp`/`.vti`/`.vtr`/`.vts`, the refusals of `comm` (grid regions outside, overlapping, not covering, two; a Table, an OverlappingAMR, a
HyperTreeGrid), an UnstructuredGrid with `chunk_bytes` (chunks sized from all the ranks' rows, its `.vtu` twin), `chunk_bytes`
different on the ranks refused; ImageData, RectilinearGrid and StructuredGrid series of 4 steps by regions (geometry at
steps 1 and 3, cell data reused at step 4, a parallel restart at step 3), compared with the serial series dataset by
dataset and, by VTK outside the test, each step with its XML file (`vtkfortran_steps_<grid>_mpi_<s>.vt?`); ranks
writing different arrays refused; without MPI `comm` refused, its name without "mpi" since `scripts/run_tests.sh` runs those with
mpirun; without HDF5 the writer refuses to start); `vtk_fortran_read.F90` reads files back (every topology and format, and two files written by VTK, embedded in the test), `vtk_fortran_read_composite.f90` parallel headers (and their check), multi-block and time series files.
