# 4. A time series

A simulation writes its solution every few time steps: one file each time. A `.pvd` **collection** lists them with their
time, and ParaView opens them as one time series, with a play button.

<<< @/examples/snippets/heat_4-series.f90

Each output is written by the function of chapter 3, with its time and cycle:

<<< @/examples/snippets/heat_4-write.f90

- `pvd_file%write_dataset` adds a file and its time step to the collection.
- The collection is a valid file after every call: if the run crashes or is killed, everything written so far still opens
  in ParaView.
- The time and the cycle are written in each file too, as field data: chapter 8 restarts from them.

::: details heat_4.f90
<<< @/examples/snippets/heat_4.f90
:::

## Running it

<<< @/examples/output/heat_4.ansi{ansi}

<<< @/examples/output/heat_4-pvd.ansi{ansi}

`heat.pvd` played in ParaView: the temperature on the horizontal middle plane, raised as a surface, while the blobs merge
and the cube cools down.

<p align="center"><img src="../../examples/images/heat_4.gif" alt="an animation of the temperature on the middle plane of the cube, two peaks merging and decaying"></p>

::: tip What you learned
One file per output, listed by a `pvd_file` with its time: the collection is always valid. Reference:
[Time series](/guide/usage#time-series-pvd).
:::

Next: [5. An unstructured mesh](./05-unstructured), or jump to [8. Restart](./08-restart).
