# 2. Formats

ASCII is easy to read and slow to write and read back: a number takes 25 characters instead of its 8 bytes. The format is
an argument of `initialize`, and the rest of the program does not change. `heat` now writes the same temperature in every
format:

<<< @/examples/snippets/heat_2-function.f90

## Running it

<<< @/examples/output/heat_2.ansi{ansi}

| Format | Where the data are | Notes |
|---|---|---|
| `ascii` | inside each `DataArray`, as text | readable, about 3 times the binary size |
| `binary` | inside each `DataArray`, base64 encoded | 4/3 of the raw size; the file is still valid XML text |
| `raw` | after the XML, in the appended section, as bytes | the smallest and the fastest to read |
| `binary-appended` | in the appended section, base64 encoded | text, as `binary`, with the metadata first |
| `vtkhdf` | one HDF5 file, a dataset per array | read by HDF5 tools too; the library built with HDF5, see [VTKHDF](/guide/formats#vtkhdf-hdf5) |

The sizes printed above, drawn:

<!--@include: ../../examples/images/heat_2-sizes.svg-->

- `compressor='zlib'` compresses the binary formats as VTK does (blocks of 32 KiB): smooth fields such as this one shrink,
  noisy ones much less. It needs the library built with zlib, see [Installation](/guide/installation#optional-zlib-compression).
- The appended formats keep the data in a scratch file until `finalize`: memory does not grow with the file.
- `vtkhdf` writes an HDF5 file: about the size of `raw`, and `compressor='zlib'` uses the deflate filter of HDF5. A
  RectilinearGrid in VTKHDF needs VTK 9.7 (ParaView 6.1 does not read it); `heat` writes it only when the library is
  built with HDF5.
- Arrays larger than 2 GiB need `header_type='UInt64'`, see [Large data arrays](/guide/formats#large-data-arrays-uint64-headers).

From now on `heat` writes `raw` data compressed with zlib.

::: details heat_2.f90
<<< @/examples/snippets/heat_2.f90
:::

::: tip What you learned
The format is one argument of `initialize`: `ascii` to look at the data, `raw` (with `compressor='zlib'`) for real runs.
Reference: [Output format selection](/guide/formats#output-format-selection), [Compressed binary data](/guide/formats#compressed-binary-data-zlib).
:::

Next: [3. More data](./03-more-data).
