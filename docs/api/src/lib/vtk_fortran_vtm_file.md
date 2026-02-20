---
title: vtk_fortran_vtm_file
---

# vtk_fortran_vtm_file

> VTM file class.

**Source**: `src/lib/vtk_fortran_vtm_file.F90`

**Dependencies**

```mermaid
graph LR
  vtk_fortran_vtm_file["vtk_fortran_vtm_file"] --> vtk_fortran_vtk_file_xml_writer_abstract["vtk_fortran_vtk_file_xml_writer_abstract"]
  vtk_fortran_vtm_file["vtk_fortran_vtm_file"] --> vtk_fortran_vtk_file_xml_writer_ascii_local["vtk_fortran_vtk_file_xml_writer_ascii_local"]
```

## Contents

- [vtm_file](#vtm-file)
- [initialize](#initialize)
- [finalize](#finalize)
- [write_block_array](#write-block-array)
- [write_block_string](#write-block-string)
- [parse_scratch_files](#parse-scratch-files)
- [write_block_scratch](#write-block-scratch)

## Derived Types

### vtm_file

VTM file class.

#### Components

| Name | Type | Attributes | Description |
|------|------|------------|-------------|
| `xml_writer` | class([xml_writer_abstract](/api/src/lib/vtk_fortran_vtk_file_xml_writer_abstract#xml-writer-abstract)) | allocatable | XML writer. |
| `scratch_unit` | integer(kind=I4P) | allocatable | Scratch units for very large list of named blocks. |

#### Type-Bound Procedures

| Name | Attributes | Description |
|------|------------|-------------|
| `initialize` | pass(self) | Initialize file. |
| `finalize` | pass(self) | Finalize file. |
| `write_block` |  | Write one block dataset. |
| `write_block_array` | pass(self) | Write one block dataset (array input). |
| `write_block_string` | pass(self) | Write one block dataset (string input). |
| `parse_scratch_files` | pass(self) | Parse scratch files. |
| `write_block_scratch` | pass(self) | Write one block dataset on scratch files. |

## Functions

### initialize

Initialize file (writer).

**Returns**: `integer(kind=I4P)`

```fortran
function initialize(self, filename, scratch_units_number) result(error)
```

**Arguments**

| Name | Type | Intent | Attributes | Description |
|------|------|--------|------------|-------------|
| `self` | class([vtm_file](/api/src/lib/vtk_fortran_vtm_file#vtm-file)) | inout |  | VTM file. |
| `filename` | character(len=*) | in |  | File name of output VTM file. |
| `scratch_units_number` | integer(kind=I4P) | in | optional | Number of scratch units for very large list of named blocks. |

**Call graph**

```mermaid
flowchart TD
  initialize["initialize"] --> initialize["initialize"]
  initialize["initialize"] --> initialize["initialize"]
  initialize["initialize"] --> initialize["initialize"]
  initialize["initialize"] --> finalize["finalize"]
  initialize["initialize"] --> initialize["initialize"]
  style initialize fill:#3e63dd,stroke:#99b,stroke-width:2px
```

### finalize

Finalize file (writer).

**Returns**: `integer(kind=I4P)`

```fortran
function finalize(self) result(error)
```

**Arguments**

| Name | Type | Intent | Attributes | Description |
|------|------|--------|------------|-------------|
| `self` | class([vtm_file](/api/src/lib/vtk_fortran_vtm_file#vtm-file)) | inout |  | VTM file. |

**Call graph**

```mermaid
flowchart TD
  finalize["finalize"] --> finalize["finalize"]
  finalize["finalize"] --> finalize["finalize"]
  finalize["finalize"] --> finalize["finalize"]
  initialize["initialize"] --> finalize["finalize"]
  finalize["finalize"] --> finalize["finalize"]
  finalize["finalize"] --> parse_scratch_files["parse_scratch_files"]
  style finalize fill:#3e63dd,stroke:#99b,stroke-width:2px
```

### write_block_array

Write one block dataset (array input).

#### Example of usage: 3 files blocks
```fortran
 error = vtm%write_block(filenames=['file_1.vts', 'file_2.vts', 'file_3.vtu'], name='my_block')
```

#### Example of usage: 3 files blocks with custom name
```fortran
 error = vtm%write_block(filenames=['file_1.vts', 'file_2.vts', 'file_3.vtu'], &
                         names=['block-bar', 'block-foo', 'block-baz'], name='my_block')
```

**Returns**: `integer(kind=I4P)`

```fortran
function write_block_array(self, filenames, names, name) result(error)
```

**Arguments**

| Name | Type | Intent | Attributes | Description |
|------|------|--------|------------|-------------|
| `self` | class([vtm_file](/api/src/lib/vtk_fortran_vtm_file#vtm-file)) | inout |  | VTM file. |
| `filenames` | character(len=*) | in |  | File names of VTK files grouped into current block. |
| `names` | character(len=*) | in | optional | Auxiliary names attributed to each files. |
| `name` | character(len=*) | in | optional | Block name |

**Call graph**

```mermaid
flowchart TD
  write_block_array["write_block_array"] --> write_parallel_block_files["write_parallel_block_files"]
  write_block_array["write_block_array"] --> write_parallel_close_block["write_parallel_close_block"]
  write_block_array["write_block_array"] --> write_parallel_open_block["write_parallel_open_block"]
  style write_block_array fill:#3e63dd,stroke:#99b,stroke-width:2px
```

### write_block_string

Write one block dataset (string input).

#### Example of usage: 3 files blocks
```fortran
 error = vtm%write_block(filenames='file_1.vts file_2.vts file_3.vtu', name='my_block')
```

#### Example of usage: 3 files blocks with custom name
```fortran
 error = vtm%write_block(filenames='file_1.vts file_2.vts file_3.vtu', names='block-bar block-foo block-baz', name='my_block')
```

**Returns**: `integer(kind=I4P)`

```fortran
function write_block_string(self, action, filenames, names, name) result(error)
```

**Arguments**

| Name | Type | Intent | Attributes | Description |
|------|------|--------|------------|-------------|
| `self` | class([vtm_file](/api/src/lib/vtk_fortran_vtm_file#vtm-file)) | inout |  | VTM file. |
| `action` | character(len=*) | in | optional | Action: [open, close, write]. |
| `filenames` | character(len=*) | in | optional | File names of VTK files grouped into current block. |
| `names` | character(len=*) | in | optional | Auxiliary names attributed to each files. |
| `name` | character(len=*) | in | optional | Block name |

**Call graph**

```mermaid
flowchart TD
  write_block_string["write_block_string"] --> write_parallel_block_files["write_parallel_block_files"]
  write_block_string["write_block_string"] --> write_parallel_close_block["write_parallel_close_block"]
  write_block_string["write_block_string"] --> write_parallel_open_block["write_parallel_open_block"]
  style write_block_string fill:#3e63dd,stroke:#99b,stroke-width:2px
```

### parse_scratch_files

Parse scratch files.

**Returns**: `integer(kind=I4P)`

```fortran
function parse_scratch_files(self) result(error)
```

**Arguments**

| Name | Type | Intent | Attributes | Description |
|------|------|--------|------------|-------------|
| `self` | class([vtm_file](/api/src/lib/vtk_fortran_vtm_file#vtm-file)) | inout |  | VTM file. |

**Call graph**

```mermaid
flowchart TD
  finalize["finalize"] --> parse_scratch_files["parse_scratch_files"]
  parse_scratch_files["parse_scratch_files"] --> write_block["write_block"]
  parse_scratch_files["parse_scratch_files"] --> write_parallel_block_files["write_parallel_block_files"]
  style parse_scratch_files fill:#3e63dd,stroke:#99b,stroke-width:2px
```

### write_block_scratch

Write one block dataset on scratch files.

**Returns**: `integer(kind=I4P)`

```fortran
function write_block_scratch(self, scratch, action, filename, name) result(error)
```

**Arguments**

| Name | Type | Intent | Attributes | Description |
|------|------|--------|------------|-------------|
| `self` | class([vtm_file](/api/src/lib/vtk_fortran_vtm_file#vtm-file)) | inout |  | VTM file. |
| `scratch` | integer(kind=I4P) | in |  | Scratch unit. |
| `action` | character(len=*) | in |  | Action: [open, write]. |
| `filename` | character(len=*) | in | optional | File name of VTK file grouped into current block. |
| `name` | character(len=*) | in | optional | Block name |
