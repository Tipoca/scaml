#!/bin/bash
set -e

COMMIT=$1

if [ -z $COMMIT ]; then
    echo "Must specify the commit"
    exit 2
fi

sed -e "s/@COMMIT@/$COMMIT/g" Dockerfile.in > Dockerfile
docker build --squash=true -t dailambda/scaml:$COMMIT .
