#!/bin/bash
set -e

for i in $*
do
  echo "----- $i"    
  case "$i" in
  *.tz)
      tz="$i"
      ;;
  *)
      if [ ! -d _build ]; then mkdir _build; fi
      cp $i _build/$i
      ml=_build/"$i"
      tz=`echo $ml | sed -e 's/\.ml$/.tz/'`
      rm -f "$tz"
      ../../../../_build/install/default/lib/ppx_scaml/ppx.exe $ml
      ;;
  esac
  
  if [ -f "$tz" ]; then
      cp "$tz" /Users/jun/.share/projects/dailambda/docker/docker-tezos-hands-on/contracts/
      tz=`basename $tz`
      (cd /Users/jun/.share/projects/dailambda/docker/docker-tezos-hands-on; ./tezos-client run script "contracts/$tz" on storage 'Unit' and input 'Unit' )
  fi
done

