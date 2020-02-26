#!/bin/sh
docker run --rm		 \
  -v `pwd`:/work	 \
  dailambda/scaml:master \
  scamlc "$*"
