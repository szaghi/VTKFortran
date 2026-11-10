#!/usr/bin/env bash
# Build and run the documentation examples, regenerating everything the pages include from them.
#
#   docs/examples/src/*.f90    the example programs (hand-written), with marker comments:
#                                !run ID COMMAND        a run shown in the pages (!run -s: with its exit status)
#                                !region NAME ... !endregion NAME   a part of the program included on its own
#                                !as NAME               its runs call it NAME (the chapters of the tutorial are all heat)
#                                !image ID              the output of the run ID also as an image, images/ID.svg
#                                !cast NAME ID ID ...   the runs ID as one animated image, images/NAME.svg: a
#                                                       terminal session typing each command, then showing its output
#                                !mpi                   an MPI program: built by $MPIFC, its runs call mpirun
#                                !render ID FILE [KEY=VALUE ...]   a ParaView render of FILE (written by the runs):
#                                                       images/ID.png, or images/ID.gif for a .pvd time series
#                                                       (scripts/render_vtk.py, where the keys are described)
#   docs/examples/files/       input files, copied into the directory where the runs happen
#   docs/examples/snippets/    generated: <program>.f90 without the markers, and <program>-<region>.f90
#   docs/examples/images/      generated: <ID>.svg, a terminal window showing the run ID, and the casts (scripts/ansi2svg.py);
#                              <ID>.png and <ID>.gif, the renders
#   docs/examples/output/      generated: <ID>.ansi, "$ COMMAND" then its output (standard output and error, colours
#                              kept), the scratch run directory shown as /home/user
#
# The library is rebuilt from scratch by FoBiS (mode static-gnu-zlib) and the examples are built by the same compiler,
# gfortran or $FC: the outputs must not depend on the compiler (use explicit formats). A run happens in a scratch
# directory, with HOME pointing there and a minimal environment, so nothing outside it is read or written. The examples
# must stay small: their meshes are rendered, not used for measurements.
#
# The renders need ParaView: they are regenerated only when $PVPYTHON (or pvpython on the PATH) is found, otherwise the
# committed ones are kept. The MPI programs (!mpi) need an MPI compiler wrapper, $MPIFC: the library is built again with
# it (in its own directory) and their runs call mpirun from the same installation, with the current LD_LIBRARY_PATH;
# without $MPIFC they are not built and their committed outputs are kept. The Docs examples workflow, without ParaView
# and MPI, checks the snippets, outputs and terminal images of all the others.
#
# Usage: bash scripts/docs_examples.sh            (FC=gfortran-14 bash scripts/docs_examples.sh: another compiler)
#        PVPYTHON=/path/to/pvpython MPIFC=/path/to/mpifort bash scripts/docs_examples.sh
set -euo pipefail

root=$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)
ex=$root/docs/examples
build=$root/build/docs-examples # git-ignored
run_dir=$build/run
home=/home/user                 # how the run directory is shown
pvpython=${PVPYTHON:-$(command -v pvpython || true)}
mpifc=${MPIFC:-}
mpi_programs=$(grep -l '^ *!mpi *$' "$ex"/src/*.f90 || true)

fc=${FC:-gfortran}
mkdir -p "$root/build"
(cd "$root" && fobis clean --mode static-gnu-zlib && fobis build --mode static-gnu-zlib --fc "$fc") \
  > "$root/build/docs-examples.log" 2>&1 || {
  cat "$root/build/docs-examples.log"; echo "docs_examples: library build failed" >&2; exit 1; }
if [ -n "$mpifc" ]; then
  (cd "$root" && fobis build --mode static-gnu-zlib --fc "$mpifc" --dbld "$root/build/docs-examples-mpi") \
    >> "$root/build/docs-examples.log" 2>&1 || {
    cat "$root/build/docs-examples.log"; echo "docs_examples: library build with $mpifc failed" >&2; exit 1; }
fi
rm -rf -- "$build"
mkdir -p "$build/bin" "$build/keep" "$run_dir" "$ex/snippets" "$ex/output" "$ex/images"
if [ -z "$mpifc" ]; then # the outputs of the MPI programs are kept
  for src in $mpi_programs; do
    for id in $(sed -n 's/^ *!run \(-s \)\{0,1\}\([A-Za-z0-9_-]*\) .*/\2/p' "$src"); do
      cp "$ex/output/$id.ansi" "$build/keep/" 2>/dev/null || true
    done
  done
fi
rm -f -- "$ex"/snippets/*.f90 "$ex"/output/*.ansi "$ex"/images/*.svg
cp "$build"/keep/*.ansi "$ex/output/" 2>/dev/null || true

markers='run|region|endregion|as|image|cast|render|mpi'
# snippets: the whole program and each region, without the markers, dedented
dedent() { awk '{l[NR]=$0; if ($0 ~ /[^ ]/) {match($0, /^ */); if (m == "" || RLENGTH < m) m = RLENGTH}}
                END {for (i = 1; i <= NR; i++) print substr(l[i], m + 1)}' "$1"; }
for src in "$ex"/src/*.f90; do
  name=$(basename "$src" .f90)
  grep -Ev "^ *!($markers) " "$src" > "$ex/snippets/$name.f90" || true
  for region in $(sed -n 's/^ *!region \([A-Za-z0-9_-]*\).*/\1/p' "$src"); do
    awk -v r="$region" -v m="^ *!($markers) " '$1 == "!endregion" && $2 == r {on = 0}
                        on && $0 !~ m {print}
                        $1 == "!region" && $2 == r {on = 1}' "$src" > "$build/region.f90"
    dedent "$build/region.f90" > "$ex/snippets/$name-$region.f90"
  done
done

# programs
is_mpi() { grep -q '^ *!mpi *$' "$1"; }
for src in "$ex"/src/*.f90; do
  if is_mpi "$src"; then
    if [ -n "$mpifc" ]; then
      "$mpifc" -I"$root/build/docs-examples-mpi/mod" -o "$build/bin/$(basename "$src" .f90)" "$src" \
               "$root/build/docs-examples-mpi/libvtkfortran.a" -lz
    fi
  else
    "$fc" -I"$root/static/mod" -o "$build/bin/$(basename "$src" .f90)" "$src" "$root/static/libvtkfortran.a" -lz
  fi
done

# runs, in the order of the files and of the lines; the renders of a program follow its runs (a later program can
# change the files: the restart of the tutorial appends to the time series)
renders=0
render() { # render ID FILE [OPTIONS]
  local id=$1 file=$2; shift 2
  # shellcheck disable=SC2068 # the options are KEY=VALUE words
  "$pvpython" --force-offscreen-rendering "$root/scripts/render_vtk.py" "$run_dir/$file" "$ex/images/$id" $@ < /dev/null \
    > "$build/render-$id.log" 2>&1 || { cat "$build/render-$id.log"; echo "docs_examples: render $id failed" >&2; exit 1; }
  renders=$((renders + 1))
}
if [ -d "$ex/files" ]; then cp -R "$ex/files/." "$run_dir/"; fi
path=$build/bin
run() { # run [-s] ID COMMAND
  local show=0 status=0
  if [ "$1" = -s ]; then show=1; shift; fi
  local id=$1; shift
  local cmd="$*"
  {
    printf '$ %s\n' "$cmd"
    (cd "$run_dir" && env -i HOME="$run_dir" PATH="$path:/usr/bin:/bin" SHELL=/bin/bash LC_ALL=C \
                     LD_LIBRARY_PATH="$ld_path" GFORTRAN_UNBUFFERED_PRECONNECTED=y bash -c "$cmd" 2>&1) || status=$?
    if [ $show = 1 ]; then printf '[exit status %d]\n' "$status"; fi
  } | sed -e "s|$run_dir|$home|g" > "$ex/output/$id.ansi"
}
ld_path=
for src in "$ex"/src/*.f90; do
  path=$build/bin
  ld_path=
  if is_mpi "$src"; then
    [ -n "$mpifc" ] || continue # not built: its committed outputs are kept
    path=$build/bin:$(dirname "$(command -v "$mpifc")")
    ld_path=${LD_LIBRARY_PATH:-}
  fi
  as=$(sed -n 's/^ *!as \([A-Za-z0-9_-]*\).*/\1/p' "$src" | head -n 1)
  if [ -n "$as" ]; then
    path=$build/as/$(basename "$src" .f90)
    mkdir -p "$path" && ln -sf "$build/bin/$(basename "$src" .f90)" "$path/$as"
    path=$path:$build/bin
  fi
  while IFS= read -r line; do
    line=${line#*!run }
    if [ "${line%% *}" = -s ]; then line=${line#-s }; run -s "${line%% *}" "${line#* }"
    else run "${line%% *}" "${line#* }"; fi
  done < <(grep -E '^ *!run ' "$src" || true)
  if [ -n "$pvpython" ]; then
    while read -r id file options; do
      # shellcheck disable=SC2086 # the options are KEY=VALUE words
      render "$id" "$file" $options
    done < <(sed -n 's/^ *!render \(.*\)/\1/p' "$src")
  fi
done
# terminal images
for id in $(sed -n 's/^ *!image \([A-Za-z0-9_-]*\).*/\1/p' "$ex"/src/*.f90); do
  python3 "$root/scripts/ansi2svg.py" "$ex/output/$id.ansi" "$ex/images/$id.svg"
done
while read -r name ids; do
  captures=()
  for id in $ids; do captures+=("$ex/output/$id.ansi"); done
  python3 "$root/scripts/ansi2svg.py" --cast "$ex/images/$name.svg" "${captures[@]}"
done < <(sed -n 's/^ *!cast \(.*\)/\1/p' "$ex"/src/*.f90)
# charts of the outputs, inlined in the pages: drawn with the theme classes, as the diagrams
python3 "$root/scripts/docs_diagrams.py" --charts
if [ -z "$pvpython" ]; then echo "docs_examples: no pvpython (set PVPYTHON): the renders are not regenerated" >&2; fi
echo "docs_examples: $(ls "$ex"/src/*.f90 | wc -l) programs, $(ls "$ex"/output/*.ansi | wc -l) runs, $renders renders"
