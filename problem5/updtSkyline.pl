#!/usr/local/bin/perl
use Tie::File;
use POSIX;
$ndv=12;
$m=6;
$c=0;
tie @array, 'Tie::File', 'skyline.dat' or die;
for $i (0..$#array){
	@X=split(" ",$array[$i]);
	for $j ($ndv..$ndv+5){
		if($X[$j]<=0.0000001){
#			print "in if\n";
			$c = $c+1;
			$X[$j]=0;
		}
	}
	$array[$i]=join(' ',@X);
}
print $c,"\n";
untie @array;
close OUT;
