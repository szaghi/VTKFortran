# Changelog

All notable changes to this project are documented here.
Versions follow [Semantic Versioning](https://semver.org/).
Format follows [Keep a Changelog](https://keepachangelog.com/).

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



