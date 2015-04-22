#!/bin/bash

c=0
for f in $(find ./src/ -name *.scala);
do
 a=$(cat $f | sed '/^\s*$/d' | wc -l)
 c=$(($c+$a))
done

echo $c
