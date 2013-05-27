#!/bin/bash
# simple bash script for assessing the speedup of the library into a parallel OpenMP framework
function print_usage {
  echo "Usage: `basename $0` -ni number_of_iteration -nf number_of_files -nt maximum_number_of_threads"
  echo "Examples:"
  echo "  `basename $0` -ni 10 -nf 32 -nt 8"
  echo "  `basename $0` -ni 10 -nf 32 -nt 4"
}
# parsing command line arguments
if [ $# -eq 0 ] ; then
  print_usage
  exit
fi
while [ "$1" != "" ]; do
  case $1 in
    -ni)
      shift; Ni=$1; shift
      ;;
    -nf)
      shift; Nf=$1; shift
      ;;
    -nt)
      shift; Nt=$1; shift
      ;;
    \?)
      print_usage
      exit 1
      ;;
  esac
done
echo "Varying the number of OpenMP threads ('t') up to $Nt, $Ni benchmarks are repeated for each 't'"
# serial benchmark
tmaxS=0.
tminS=0.
tmeanS=0.
export OMP_NUM_THREADS=1
for i in $( seq 1 $Ni ); do
  ./Test_Driver -openmp $Nf > openmp.log
  tmax=`grep -i maximum openmp.log | awk '{print $7}'`
  tmin=`grep -i minimum openmp.log | awk '{print $7}'`
  tmean=`grep -i average openmp.log | awk '{print $4}'`
  tmaxS=`echo "scale=10; $tmaxS+$tmax" | bc -l`
  tminS=`echo "scale=10; $tminS+$tmin" | bc -l`
  tmeanS=`echo "scale=10; $tmeanS+$tmean" | bc -l`
done
tmaxS=`echo "scale=10; $tmaxS/$Ni" | bc -l`
tminS=`echo "scale=10; $tminS/$Ni" | bc -l`
tmeanS=`echo "scale=10; $tmeanS/$Ni" | bc -l`
rm -f openmp.log
echo "Serial results"
echo "Maximum elapsed time $tmaxS"
echo "Minimum elapsed time $tminS"
echo "Average elapsed time $tmeanS"
echo "1 $tmeanS $tmaxS $tminS 1 1 1" > openmp_speedup.dat
# parallel benchmarks: the number of OpenMp threads is increased from 2 to Nt with an increment of 2
for p in $( seq 2 2 $Nt ); do
  tmaxP=0.
  tminP=0.
  tmeanP=0.
  export OMP_NUM_THREADS=$p
  for i in $( seq 1 $Ni ); do
    ./Test_Driver -openmp $Nf > openmp.log
    tmax=`grep -i maximum openmp.log | awk '{print $7}'`
    tmin=`grep -i minimum openmp.log | awk '{print $7}'`
    tmean=`grep -i average openmp.log | awk '{print $4}'`
    tmaxP=`echo "scale=10; $tmaxP+$tmax" | bc -l`
    tminP=`echo "scale=10; $tminP+$tmin" | bc -l`
    tmeanP=`echo "scale=10; $tmeanP+$tmean" | bc -l`
  done
  tmaxP=`echo "scale=10; $tmaxP/$Ni" | bc -l`
  tminP=`echo "scale=10; $tminP/$Ni" | bc -l`
  tmeanP=`echo "scale=10; $tmeanP/$Ni" | bc -l`
  rm -f openmp.log
  echo "openmp processes $p"
  echo "Maximum elapsed time $tmaxP"
  echo "Minimum elapsed time $tminP"
  echo "Average elapsed time $tmeanP"
  tmaxN=`echo "scale=10; $tmaxP/$tmaxS" | bc -l`
  tminN=`echo "scale=10; $tminP/$tminS" | bc -l`
  tmeanN=`echo "scale=10; $tmeanP/$tmeanS" | bc -l`
  echo "$p $tmeanP $tmaxP $tminP $tmeanN $tmaxN $tminN" >> openmp_speedup.dat
done
