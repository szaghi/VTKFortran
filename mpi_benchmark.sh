#!/bin/bash
# simple bash script for assessing the speedup of the library into a parallel MPI framework
function print_usage {
  echo "Usage: `basename $0` -ni number_of_iteration -nf number_of_files -np maximum_number_of_processes"
  echo "Examples:"
  echo "  `basename $0` -ni 10 -nf 32 -np 8"
  echo "  `basename $0` -ni 10 -nf 32 -np 4"
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
    -np)
      shift; Np=$1; shift
      ;;
    \?)
      print_usage
      exit 1
      ;;
  esac
done
echo "Varying the number of MPI processes ('p') up to $Np, $Ni benchmarks are repeated for each 'p'"
# serial benchmark
tmaxS=0.
tminS=0.
tmeanS=0.
for i in $( seq 1 $Ni ); do
  mpiexec -n 1 ./Test_Driver -mpi $Nf > mpi.log
  tmax=`grep -i maximum mpi.log | awk '{print $7}'`
  tmin=`grep -i minimum mpi.log | awk '{print $7}'`
  tmean=`grep -i average mpi.log | awk '{print $4}'`
  tmaxS=`echo "scale=10; $tmaxS+$tmax" | bc -l`
  tminS=`echo "scale=10; $tminS+$tmin" | bc -l`
  tmeanS=`echo "scale=10; $tmeanS+$tmean" | bc -l`
done
tmaxS=`echo "scale=10; $tmaxS/$Ni" | bc -l`
tminS=`echo "scale=10; $tminS/$Ni" | bc -l`
tmeanS=`echo "scale=10; $tmeanS/$Ni" | bc -l`
rm -f mpi.log
echo "Serial results"
echo "Maximum elapsed time $tmaxS"
echo "Minimum elapsed time $tminS"
echo "Average elapsed time $tmeanS"
echo "1 $tmeanS $tmaxS $tminS 1 1 1" > mpi_speedup.dat
# parallel benchmarks: the number of MPI processes is increased from 2 to Np with an increment of 2
for p in $( seq 2 2 $Np ); do
  tmaxP=0.
  tminP=0.
  tmeanP=0.
  for i in $( seq 1 $Ni ); do
    mpiexec -n $p ./Test_Driver -mpi $Nf > mpi.log
    tmax=`grep -i maximum mpi.log | awk '{print $7}'`
    tmin=`grep -i minimum mpi.log | awk '{print $7}'`
    tmean=`grep -i average mpi.log | awk '{print $4}'`
    tmaxP=`echo "scale=10; $tmaxP+$tmax" | bc -l`
    tminP=`echo "scale=10; $tminP+$tmin" | bc -l`
    tmeanP=`echo "scale=10; $tmeanP+$tmean" | bc -l`
  done
  tmaxP=`echo "scale=10; $tmaxP/$Ni" | bc -l`
  tminP=`echo "scale=10; $tminP/$Ni" | bc -l`
  tmeanP=`echo "scale=10; $tmeanP/$Ni" | bc -l`
  rm -f mpi.log
  echo "MPI processes $p"
  echo "Maximum elapsed time $tmaxP"
  echo "Minimum elapsed time $tminP"
  echo "Average elapsed time $tmeanP"
  tmaxN=`echo "scale=10; $tmaxP/$tmaxS" | bc -l`
  tminN=`echo "scale=10; $tminP/$tminS" | bc -l`
  tmeanN=`echo "scale=10; $tmeanP/$tmeanS" | bc -l`
  echo "$p $tmeanP $tmaxP $tminP $tmeanN $tmaxN $tminN" >> mpi_speedup.dat
done
