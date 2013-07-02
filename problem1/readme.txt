This example shows how to write a FORTRAN program calling NOMAD,
with a black-box routine written in FORTRAN.

It has been tested on a X system with g++ and gfortran.

Type 'make' to compile and './test.exe' to execute.


rm *.o *.mod *.exe *.*~ *~

g++ -ansi -Wall -O3  -I/home/shubhangi/nomad.3.5.1/src -I. -c nomad.cpp 

g95 -c REAL_PRECISION.f TestObj.f95 test.f

g++ -o test.exe *.o /home/shubhangi/nomad.3.5.1/lib/nomad.a -lm -ansi -Wall -O3 /home/shubhangi/g95-install/lib/gcc-lib/i686-pc-linux-gnu/4.0.3/libf95.a



