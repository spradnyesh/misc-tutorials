#!/bin/bash

cd ~/.flickInt
i=`ls | wc -l`
j=`expr $RANDOM % $i`
k=`ls | head -${j} | tail -1`
feh --bg-center ${k}
