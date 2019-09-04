#!/bin/sh
set -e

ml=$1
tz=`echo $ml | sed -e 's/\.ml$/.tz/'`
rm -f "$tz"
../../../../_build/install/default/lib/ppx_scaml/ppx.exe $ml

if [ -f "$tz" ]; then
    cp "$tz" /Users/jun/.share/projects/dailambda/docker/docker-tezos-hands-on/contracts/
    (cd /Users/jun/.share/projects/dailambda/docker/docker-tezos-hands-on; ./tezos-client run script contracts/"$tz" on storage 'Unit' and input 'Unit')
fi


