#!/bin/bash

perl ./popbeta.pl
./test.exe
cp RSMInTest.dat input.dat
perl ./paretofront.pl
./pfront
cp blank.dat RSMInTest.dat
