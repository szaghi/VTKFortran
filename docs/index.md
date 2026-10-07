---
layout: home

hero:
  name: VTKFortran
  text: VTK files from pure Fortran
  tagline: "Write the VTK XML formats that ParaView and VisIt read: image, rectilinear, structured and unstructured grids, polygonal data, parallel pieces, multi-block assemblies, time series. ASCII, binary, raw or zlib compressed. Then read them back. Pure Fortran 2008, no VTK to install."
  actions:
    - theme: brand
      text: Tutorial
      link: /manual/tutorial/01-first-file
    - theme: alt
      text: Quick start
      link: "#quick-start"
    - theme: alt
      text: Reference
      link: /guide/features
    - theme: alt
      text: API
      link: /api/
    - theme: alt
      text: View on GitHub
      link: https://github.com/szaghi/VTKFortran

features:
  - icon: 📐
    title: Every VTK XML dataset
    details: "Image data, rectilinear, structured and unstructured grids (polyhedra included), polygonal data with vertices, lines, strips and polygons."
    link: /guide/usage#image-data-vti
    linkText: Topologies
  - icon: 🗜️
    title: Any format, zlib too
    details: "ASCII to read by eye, base64 binary inline, raw binary appended for speed; the binary formats compressed as VTK does, byte for byte."
    link: /guide/usage#output-format-selection
    linkText: Formats
  - icon: 🧩
    title: Parallel pieces
    details: "Each rank writes its own piece, one rank writes the .pvtu, .pvts, .pvtr, .pvti or .pvtp header; ghost cells marked as ParaView expects."
    link: /guide/usage#parallel-structured-grid-pvts
    linkText: Parallel files
  - icon: 🎞️
    title: Time series
    details: "A .pvd collection valid after every step, so a crashed run still opens in ParaView; restarts append to it."
    link: /guide/usage#time-series-pvd
    linkText: Time series
  - icon: 🗂️
    title: Assemblies
    details: "Multi-block .vtm files with blocks nested to any depth, mirroring the parts of a model."
    link: /guide/usage#nested-blocks
    linkText: Multi-block
  - icon: 📖
    title: Read them back
    details: "Every file VTKFortran writes, and the same files written by VTK, read array by array without loading the whole file; parallel headers checked against their pieces."
    link: /guide/usage#reading-files
    linkText: Reading
  - icon: 🏷️
    title: Field data and metadata
    details: "Time, cycle, solver name and any global array or string attached to the dataset; active scalars and vectors chosen for the reader."
    link: /guide/usage#field-data-global-metadata
    linkText: Field data
  - icon: 🔢
    title: All kinds, all ranks
    details: "Every PENF kind from I1P to R8P, ranks 1 to 4, scalars, vectors and tensors; unsigned arrays such as vtkGhostType."
    link: /guide/features#data-arrays
    linkText: Data arrays
  - icon: 🐘
    title: Big data
    details: "Arrays beyond 2 GiB with UInt64 headers, more than 2^31 elements, 64-bit connectivity; appended data kept on disk while writing."
    link: /guide/usage#large-meshes-64-bit-counts-and-connectivity
    linkText: Large meshes
  - icon: ⚡
    title: Thread and process safe
    details: "Every file keeps its own state: write them concurrently from OpenMP threads or MPI ranks."
    link: /guide/features#parallel-support
    linkText: Parallel support
  - icon: 🛠️
    title: Any build
    details: "CMake, FoBiS.py or fpm; gfortran and Intel ifx. Small dependencies, fetched for you; zlib optional."
    link: /guide/installation
    linkText: Installation
  - icon: 🔓
    title: Multi-licensed
    details: "GPL v3 for FOSS projects; BSD 2-Clause, BSD 3-Clause or MIT for closed source and commercial ones."
    link: "#copyrights"
    linkText: Copyrights
---

<p align="center"><img src="./examples/images/quickstart.png" alt="a torus, written by the quick start program, rendered by ParaView: a structured grid coloured by its temperature"></p>

## Quick start

A real session: a short program writes a torus as a structured grid with a temperature and a velocity field, then a
second one reads the file back and prints what it holds.

<p align="center"><img src="./examples/images/quickstart-cast.svg" alt="a terminal session: the quick start program writes torus.vts, the head of the file is shown, the inspect program lists its arrays"></p>

This is the whole program: compute the points and the fields, then one call for each part of the file. The torus above
is `torus.vts` opened in ParaView.

<<< @/examples/snippets/quickstart.f90

Reading is as short: `initialize` with `action='read'`, then ask for what you need. The `inspect` program of the session:

<<< @/examples/snippets/inspect.f90

Every example on these pages is a program compiled and run to produce the output and the images shown.

## Grows with your simulation

From a first file to a parallel, restartable time series: the [tutorial](/manual/tutorial/01-first-file) builds `heat`, a
small solver of the heat equation, chapter by chapter. This is its time series played in ParaView: two hot blobs merging
and cooling down, on the middle plane of the cube.

<p align="center"><img src="./examples/images/heat_4.gif" alt="an animation of the temperature on the middle plane of a cube: two peaks merging and decaying"></p>

| | |
|---|---|
| <img src="./examples/images/heat_6.png" alt="the cube split in four pieces"> | <img src="./examples/images/heat_7.png" alt="the cube cut at the height of eight probes"> |
| [Parallel pieces](/manual/tutorial/06-parallel), ghost cells and a checked header | [An assembly](/manual/tutorial/07-assembly) of the domain and its probes |

## Authors

- Stefano Zaghi — [@szaghi](https://github.com/szaghi)

Contributions are welcome — see the [Contributing](/guide/contributing) page.

## Copyrights

VTKFortran is distributed under a multi-licensing system:

| Use case | License |
|----------|---------|
| FOSS projects | [GPL v3](http://www.gnu.org/licenses/gpl-3.0.html) |
| Closed source / commercial | [BSD 2-Clause](http://opensource.org/licenses/BSD-2-Clause) |
| Closed source / commercial | [BSD 3-Clause](http://opensource.org/licenses/BSD-3-Clause) |
| Closed source / commercial | [MIT](http://opensource.org/licenses/MIT) |

> Anyone interested in using, developing, or contributing to VTKFortran is welcome — pick the license that best fits your needs.
