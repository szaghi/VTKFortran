# Changelog

All notable changes to this project are documented here.
Versions follow [Semantic Versioning](https://semver.org/).
Format follows [Keep a Changelog](https://keepachangelog.com/).

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



