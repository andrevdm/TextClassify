#!/usr/bin/awk -f
BEGIN {FS = ",";}
NR<4 {next}
NF { print } 
#NF { print $1, ",", $4} 
END { }
