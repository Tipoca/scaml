#!/bin/bash
set -e

# Where am I?
script_dir="$(cd "$(dirname "$0")" && echo "$(pwd -P)/")"

# Where to work?
build_dir=$script_dir/_build

# Make sure the library module is compiled
(cp $script_dir/../lib/SCaml.mli $build_dir; ocamlfind ocamlc -package zarith -c $build_dir/SCaml.mli)

# Compilation command
comp="dune exec ../main.exe -- --scaml-noscamlib -I $build_dir"
echo comp=$comp

for i in $*
do
  echo "----- $i"    
  case "$i" in
  *.tz)
      # Do nothing if it is *.tz
      tz="$i"
      ;;
  *)
      # Compile it under $build_dir
      if [ ! -d $build_dir ]; then mkdir $build_dir; fi
      cp $i $build_dir/$(basename $i)
      ml=$build_dir/$(basename $i)
      # Remove old output files
      iml=`echo $ml | sed -e 's/\.ml$/.iml/'`
      tz=`echo $ml | sed -e 's/\.ml$/.tz/'`
      rm -f "$iml" "$tz"
      # Compile!
      echo $comp $ml
      (cd $script_dir; $comp $ml)
      ;;
  esac

  # If tz compilation is successful, and if there is tezos-client in the PATH,
  # let's try to execute it.

  tezos_client=`which tezos-client || true`
  if [ -f "$tz" -a -n "$tezos_client" ]; then
      echo Executing $tezos_client run script $tz on storage 'Unit' and input 'Unit'

      # Must this test fail ?
      MUST_FAIL=$(grep MUST_FAIL $i)

      if [ "$MUST_FAIL" = "" ]; then
	  TEZOS_CLIENT_UNSAFE_DISABLE_DISCLAIMER=Y $tezos_client run script $tz on storage 'Unit' and input 'Unit'
      else
	  echo THIS TEST MUST FAIL
	  if
	      TEZOS_CLIENT_UNSAFE_DISABLE_DISCLAIMER=Y $tezos_client run script $tz on storage 'Unit' and input 'Unit'
	  then
	      echo "Error: TEST UNEXPECTEDLY SUCCEEEDED"; exit 2
	  else
	      echo "Ok: Test failed expectedly"
	  fi
      fi
  fi
done
