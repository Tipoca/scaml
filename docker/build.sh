#!/bin/bash
set -e

COMMIT=$1

if [ -z $COMMIT ]; then
    echo Using master
    COMMIT=master
fi

sed -e "s/@COMMIT@/$COMMIT/g" Dockerfile.in > Dockerfile
echo docker build -t dailambda/scaml:$COMMIT .
docker build --squash --no-cache -t dailambda/scaml:$COMMIT .
cp ../src/tests/app_vote.ml .
docker run --rm -v `pwd`:/work dailambda/scaml:$COMMIT /root/.opam/4.07.1/bin/scamlc app_vote.ml
