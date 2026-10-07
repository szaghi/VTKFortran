# 7. An assembly

A simulation often has more than one dataset: the domain, the probes that record the temperature at a few points, a
boundary, a body. A **multi-block** `.vtm` file gathers them in blocks, nested as you like, and ParaView opens it as one
object whose parts can be shown or hidden.

The probes are a **polydata** of 8 points, one vertex each, with the temperature read from the pieces of chapter 6:

<<< @/examples/snippets/heat_7-probes.f90

The assembly: a `solver` block holding the `domain` (the 4 pieces) and the `sensors`:

<<< @/examples/snippets/heat_7-assembly.f90

Read back, the tree of blocks and datasets:

<<< @/examples/snippets/heat_7-tree.f90

::: details heat_7.f90
<<< @/examples/snippets/heat_7.f90
:::

## Running it

<<< @/examples/output/heat_7.ansi{ansi}

The assembly in ParaView, cut at the height of the probes:

<p align="center"><img src="../../examples/images/heat_7.png" alt="the cube cut horizontally, coloured by temperature, with eight probes"></p>

::: tip What you learned
`write_block` with `action='open'`/`'close'` nests blocks; `get_entries` reads the tree back; a piece is read with
`action='read'` and `read_geo`, `read_dataarray`. Reference: [Nested blocks](/guide/parallel#nested-blocks),
[Reading files](/guide/reading).
:::

Next: [8. Restart](./08-restart).
