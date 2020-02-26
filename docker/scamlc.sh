#!/bin/sh
docker run -v `pwd`:/work dailambda/scaml:master scamlc "$*"
