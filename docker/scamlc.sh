#!/bin/sh
docker run --rm		 \
  -v `pwd`:/work	 \
  dailambda/scaml:1.1.0  \
  scamlc "$*"
