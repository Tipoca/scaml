#!/bin/bash
set -e

COMMIT=$1

if [ -z $COMMIT ]; then
    echo Using master
    COMMIT=master
fi

sed -e "s/@COMMIT@/$COMMIT/g" Dockerfile.in > Dockerfile
echo docker build --no-cache --squash=true -t dailambda/scaml:$COMMIT .
docker build --no-cache --squash=true -t dailambda/scaml:$COMMIT .
