#!/bin/sh
cpp -lang-fortran -traditional-cpp -D_LANGUAGE_FORTRAN -DDOXYGEN_SKIP -DOPENMP -DMPI2 -I.. $*
