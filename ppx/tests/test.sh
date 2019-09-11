#!/bin/bash
set -e

script_dir="$(cd "$(dirname "$0")" && echo "$(pwd -P)/")"
build_dir=$script_dir/_build

(cd $script_dir; ocamlfind ocamlc -package zarith -c SCaml.ml)

d=$(dirname $script_dir)

while [ ! -x $d/_build/install/default/lib/ppx_scaml/ppx.exe ]; do
    if [ "$d" = "/" ]; then
	echo "SCaml compiler binary not found"
	exit 2
    fi
    d=$(dirname $d)
done

ppx=$d/_build/install/default/lib/ppx_scaml/ppx.exe
echo ppx=$ppx

for i in $*
do
  echo "----- $i"    
  case "$i" in
  *.tz)
      tz="$i"
      ;;
  *)
      if [ ! -d $build_dir ]; then mkdir $build_dir; fi
      cp $i $build_dir/$(basename $i)
      ml=$build_dir/$(basename $i)
      tz=`echo $ml | sed -e 's/\.ml$/.tz/'`
      rm -f "$tz"
      echo $ppx $ml
      (cd $script_dir; $ppx $ml)
      ;;
  esac

  tezos_client=`which tezos-client || true`
  if [ -f "$tz" -a -n "$tezos_client" ]; then
      echo Executing $tezos_client run script $tz on storage 'Unit' and input 'Unit'
      TEZOS_CLIENT_UNSAFE_DISABLE_DISCLAIMER=Y $tezos_client run script $tz on storage 'Unit' and input 'Unit'
  fi
done
