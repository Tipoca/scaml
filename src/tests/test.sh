#!/bin/bash
set -e

script_dir="$(cd "$(dirname "$0")" && echo "$(pwd -P)/")"
build_dir=$script_dir/_build

(cd $script_dir; ocamlfind ocamlc -package zarith -c SCaml.ml)

d=$(dirname $script_dir)

comp="dune exec ../main.exe --"
echo comp=$comp

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
      echo $comp $ml
      (cd $script_dir; $comp $ml)
      ;;
  esac

  # If tz compilation is successful, and if there is tezos-client in the PATH,
  # let's try to execute it.

  tezos_client=`which tezos-client || true`
  if [ -f "$tz" -a -n "$tezos_client" ]; then
      echo Executing $tezos_client run script $tz on storage 'Unit' and input 'Unit'
      TEZOS_CLIENT_UNSAFE_DISABLE_DISCLAIMER=Y $tezos_client run script $tz on storage 'Unit' and input 'Unit'
  fi
done
