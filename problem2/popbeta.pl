#!/usr/local/bin/perl
use Tie::File;
use File::Copy;

tie @array, 'Tie::File', 'betas.txt' or die;
$temp = shift(@array);
@b = split(" ",$temp);

open OUT, ">", "beta.dat" or die;
print OUT $b[0],' ',$b[1],' ',$b[2];
close OUT;

untie @array;


