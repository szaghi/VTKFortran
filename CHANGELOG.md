# Changelog

All notable changes to this project are documented here.
Versions follow [Semantic Versioning](https://semver.org/).
Format follows [Keep a Changelog](https://keepachangelog.com/).

## [3.1.0] — 2026-11-12
### Added
- **vtkhdf**: Write ImageData in the VTKHDF format

- **vtkhdf**: Write UnstructuredGrid in the VTKHDF format

- **vtkhdf**: Write PolyData in the VTKHDF format

- **vtkhdf**: Write RectilinearGrid and StructuredGrid in the VTKHDF format

- **vtkhdf**: Write the polyhedra of an UnstructuredGrid in the VTKHDF format

- **vtkhdf**: Time series in one VTKHDF file, write_step

- **vtkhdf**: Continue a VTKHDF time series, action='append'

- **vtkhdf**: Read VTKHDF files with action='read'

- **vtkhdf**: Multi-block datasets in one VTKHDF file, vtm_file format='vtkhdf'

- **vtkhdf**: One .vtkhdf written by MPI ranks, vtk_file%initialize(..., comm=)

- **vtkhdf**: Multi-block files of time-series UnstructuredGrid and PolyData blocks

- **vtkhdf**: Time series store an unchanged mesh and unchanged arrays once

- **vtkhdf**: ImageData, RectilinearGrid and StructuredGrid from MPI ranks, a region per rank

- **active-arrays**: Every attribute role of VTK, written and read back

- **vtkhdf**: Time series from MPI ranks, and a parallel restart

- **vtkhdf**: Polyhedra and compression from MPI ranks

- **vtkhdf**: Table, columns of values in a VTKHDF file, static or a time series

- **vtkhdf**: PartitionedDataSetCollection, datasets referenced from several blocks

- **vtkhdf**: OverlappingAMR, the levels of a block-structured AMR in one file

- **vtkhdf**: HyperTreeGrid, the trees of a tree-based AMR in one file

- **vtkhdf**: HyperTreeGrid time series

- **vtkhdf**: Configurable chunk size, vtk_file%initialize(..., chunk_bytes=)

- **vtkhdf**: Grid time series from MPI ranks

- **xml**: Table as a .vtt file, written and read back

- **vtkhdf**: Read back HyperTreeGrids and composite files

- **read**: String arrays of VTKHDF files, Bit arrays of XML files

- **composite**: Empty datasets in VTKHDF multi-block files; PArray in parallel headers

- **write**: String arrays as point, cell and row data (XML); string field data in VTKHDF

- **data**: Component names; multi-component field data

- **composite**: XML partitioned composites (.vtpc, .vtpd)

- **parallel**: Parallel Tables (.pvtt); blocks of pieces in .vtm files

- **vtkhdf**: HyperTreeGrid interface arrays; references in multi-block files

- **composite**: Grid time series as blocks of a VTKHDF multi-block file

- **pvd**: ParaView file series (.series)

- **composite**: XML OverlappingAMR files (.vthb)

- **xml**: HyperTreeGrid files (.htg)

- **read**: HyperTreeGrid files (.htg)

- **read**: BigEndian files; docs of the compression level and the .pvti direction


### Changed
- **writers**: A format-neutral abstract writer for vtk_file


### Documentation
- **vtkhdf**: The VTKHDF format, its reference program and the HDF5 examples

- **vtkhdf**: UnstructuredGrid in VTKHDF, and the recipe "Write a .vtkhdf file"

- **vtkhdf**: PolyData in VTKHDF, and the recipe "A PolyData in a .vtkhdf file"

- **vtkhdf**: RectilinearGrid and StructuredGrid in VTKHDF, and the recipe "A grid in a .vtkhdf file"

- **vtkhdf**: Polyhedra in VTKHDF

- **vtkhdf**: Time series in one .vtkhdf file, and the VTKHDF anatomy diagram

- **vtkhdf**: Reading VTKHDF files and multi-block VTKHDF files

- VTKHDF in the README, the landing page and the formats chapter

- **vtkhdf**: One .vtkhdf from MPI ranks

- **claude**: The toolchain image needs FoBiS >= 3.16.5, released


### Fixed
- **appended**: Close the scratch file once, and stop on a read error

- **vtkhdf**: Read polyhedra with shared, reversed faces (VTKHDF 2.9)

- **vtkhdf**: A restart reads the array offsets of the last complete step

- **vtkhdf**: Refuse array, block and dataset names with '/' or '.'

- **vtkhdf**: Refuse on every rank the wrong arguments of one rank

- **vtkhdf**: List the members of a group in the order they were written

- **vtkhdf**: Leave the file of a refused append as it was

- **vtkhdf**: Finalize refuses what it would lose; malformed AMR files read safely

- Review of the grid series from ranks, the .vtt Table and the reading of composites

- **xml**: Read VTK 9.7.2 polyhedra, non-finite ASCII values, quotes in names, large UInt64

- **vtkhdf**: AMR series Steps names of VTKHDF 2.3; refuse composite and AMR blocks

- **vtkhdf**: Read collections, vlen strings, partitioned and transposed HyperTreeGrids

- **fpm**: Evaluate the #ifdef blocks when fpm scans the module dependencies

- Read AMR series of the former layout; separate non-finite 6-component ASCII values

- Review of the #90 work


## [3.0.1] — 2026-11-10
### Changed
- **reader**: Index files with FoXy xml_index, drop the private scanner

- **writers**: Stream the XML through FoXy xml_writer

- **reader**: Read numeric attributes with FoXy typed accessors


### Documentation
- Adopt the Circuit Moss identity, the vortex dipole mark

- **readme**: Show the vortex dipole mark

- Correct stale and wrong statements, measure the compilers again

- Compile the code of the reference pages, document what was missing

- Draw the structure of the files, the cells and the pieces

- List the faces of a polyhedron outward, as VTK requires

- Render in the site palette, add the cell gallery and animations

- One place for each thing, the quick start in the sidebar

- MPI ranks and OpenMP threads, as measured


### Fixed
- **docs**: Pin mermaid to 11, vitepress-plugin-mermaid rejects 12

- **xml-writer-appended**: Compare the encoding as characters, nvfortran 25.5 rejects the string ==

- Work around three nvfortran 26.9 bugs, every test passes with it

- **vtm**: Keep blanks and slashes in the names and files of scratch blocks


## [3.0.0] — 2026-10-07
### Added
- **writers**: Designate active arrays of point and cell data

- **pvd**: Add pvd_file writer for time series collections

- **writers**: Write FieldData arrays and strings

- **writers**: Add ImageData (vti) and PImageData (pvti) topologies

- **writers**: Add PolyData (vtp) and PPolyData (pvtp) topologies

- **writers**: Opt-in UInt64 bytes count headers for large arrays

- **writers**: Zlib compression for binary and binary-appended data

- **writers**: Write unsigned integer arrays

- **writers**: 64-bit counts and connectivity for large meshes

- **zlib**: Decompress VTK zlib blocks

- **readers**: Read serial VTK XML files

- **readers**: Read parallel headers, multi-block and time series files


### Documentation
- **usage**: Document the scratch file of the appended formats

- **contributing**: Document release.sh as the only release tool

- Fix stale and missing documentation

- Build the documentation examples, new landing page

- Add the heat tutorial, in eight chapters

- Add the cookbook

- Split the usage page into reference topics


### Fixed
- **writers**: Write polyhedron faces inside the Cells element

- **writers**: Write a valid GhostLevel and declare header_type

- **writers**: Write cell types as UInt8

- **vtm**: Index nested blocks and datasets per level

- **writers**: Count array elements in 64 bits

- **writers**: Separate the last value of each row of ASCII arrays

- **tests**: Write the volatile test variable as point data

- **writers**: Close write_xml_volatile, volatile files in ascii too


## [2.0.10] — 2026-10-06
### Fixed
- **ci**: Drop stale coverage page, pin fpm for the install smoke test


## [2.0.9] — 2026-10-06
### Fixed
- **build**: Exclude dependency examples from tests, drop stale makefile


## [2.0.8] — 2026-10-06
### Fixed
- **zlib**: Compile when disabled

- **ci**: Drop extra arg from FoBiS.py -gcov_analyzer call

- **fobos**: Move -lz to ext_libs to fix linker order issue

- **docs**: Pin markdown-it-mathjax3 to ^4 to restore docs build

- **docs**: Untrack package-lock and pin esbuild for lock-free vite build

- **writers**: Stop stack overflow when writing large dataarrays

- **build**: Preprocess zlib-conditional sources in cmake builds


## [2.0.5] — 2026-02-20
### Documentation
- **coverage**: Update coverage stats and fix page frontmatter


## [2.0.4] — 2026-02-20
### Added
- Add vitepress docs site and modernize project infrastructure


## [2.0.3] — 2026-02-18
### Fixed
- Update submodules

- Fix issue #45


## [2.0.2] — 2022-03-03
### Fixed
- Makefile compilation



