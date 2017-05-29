#!/bin/bash

fname="datafiles/spanish_urls"
List=$(cat $fname)
arr=($List)
nlines=$(cat $fname | wc -l)

n=$1
while (( $n < $nlines )) ; do
    firefox ${arr[n]}
    let "n+=1"
    if [ $((n%5)) == 0 ] ; then
	echo $n, ${arr[n]}
	cat
    fi
done
