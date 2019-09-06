#!/bin/bash

for i in *.ml
do
    echo $i
    if head $i | grep SCaml; then
	./test_run.sh $i
    fi
done

	 
