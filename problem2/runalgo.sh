#!/bin/bash

perl /home/shubhangi/nomad.3.5.1/examples/interfaces/FORTRAN/MultiMADS/problem2/popbeta.pl
/home/shubhangi/nomad.3.5.1/examples/interfaces/FORTRAN/MultiMADS/problem2/test.exe
cp RSMInTest.dat input.dat
perl ./paretofront.pl
/home/shubhangi/nomad.3.5.1/examples/interfaces/FORTRAN/MultiMADS/problem2/pfront
cp blank.dat RSMInTest.dat
