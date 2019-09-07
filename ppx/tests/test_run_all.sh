#!/bin/bash

for i in *.ml
do
    echo $i
    if head $i | grep SCaml > /dev/null 2>&1; then
	./test_run.sh $i
    fi
done

	 
