#!/bin/bash
set -e

# Disable the disclaimer message of tezos-node
export TEZOS_CLIENT_UNSAFE_DISABLE_DISCLAIMER=Y 

# Where am I?
SCRIPT_DIR="$(cd "$(dirname "$0")" && echo "$(pwd -P)/")"

# Where to work?
BUILD_DIR=$SCRIPT_DIR/_build
if [ ! -d $BUILD_DIR ]; then mkdir $BUILD_DIR; fi

# Make sure the library module is COMPiled
(if [ ! -d $BUILD_DIR ]; then mkdir $BUILD_DIR; fi; \
 cp $SCRIPT_DIR/../lib/SCaml.mli $BUILD_DIR; \
 ocamlfind ocamlc -package zarith -c $BUILD_DIR/SCaml.mli)

# Compilation command
COMP="dune exec ../main.exe -- --scaml-noscamlib --scaml-dump-iml0 --scaml-dump-iml -I $BUILD_DIR"

# Optional: tezos-client
TEZOS_CLIENT=`which tezos-client || true`

# Input <ML>
# Output TZ
function compile () {
    # Compile it under $BUILD_DIR
    if [ ! -d $BUILD_DIR ]; then mkdir $BUILD_DIR; fi
    local ml=$BUILD_DIR/$(basename $1)
    cp $1 $ml
    # Remove old output files
    local iml=`echo $ml | sed -e 's/\.ml$/.iml/'`
    TZ=`echo $ml | sed -e 's/\.ml$/.tz/'`
    rm -f "$iml" "$TZ"
    # Compile!
    echo $COMP $ml
    (cd $SCRIPT_DIR; $COMP $ml)
}

# Input: <code>
# Output: CONVERSION
function convert () {
    echo "converting $1 ..."
    tmp=`mktemp`
    echo "open SCaml" > $tmp
    echo "let x = $1" >> $tmp
    cat $tmp
    CONVERSION=$($COMP --scaml-convert -impl $tmp | sed -e 's/^x: //')
    echo "converted to $CONVERSION"
}

# Input <ML> <TZ>
# Output: none
function run () {

    local ml=$1
    local tz=$2

    # Must this test fail ?
    local must_fail=$(grep MUST_FAIL $ml || true)

    # STORAGE=.*$
    local storage=$(grep STORAGE= $i || true)
    if [ -z "$storage" ]; then
	storage='Unit'
    else
	storage=`echo "$storage" | sed -e 's/.*STORAGE=//'`
	convert "$storage"
	storage=$CONVERSION
    fi

    # INPUT=.*$
    local input=$(grep INPUT= $i || true)
    if [ -z "$input" ]; then
	input='Unit'
    else
	input=`echo $input | sed -e 's/.*INPUT=//'`
	convert "$input"
	input=$CONVERSION
    fi

    echo Executing $TEZOS_CLIENT typecheck script $tz
    
    $TEZOS_CLIENT typecheck script $tz

    # Really weird but --source is to set SENDER and --payer to set SOURCE
    echo Executing $TEZOS_CLIENT run script $tz on storage $storage and input $input --source bootstrap1 --payer bootstrap2

    if [ -z "$must_fail" ]; then
	$TEZOS_CLIENT run script $tz on storage "$storage" and input "$input" --source bootstrap1 --payer bootstrap2
    else
	echo THIS TEST MUST FAIL
	if
    	    $TEZOS_CLIENT run script $tz on storage "$storage" and input "$input" --source bootstrap1 --payer bootstrap2
	then
	    echo "Error: TEST UNEXPECTEDLY SUCCEEEDED"; exit 2
	else
	    echo "Ok: Test failed expectedly"
	fi
    fi
}

for i in $*
do

  echo "----- $i"    
  case "$i" in
  *.tz)
      # Do nothing if it is *.tz
      TZ="$i"
      ;;
  *)
      compile "$i"
      ;;
  esac

  # If tz Compilation is successful, and if there is tezos-client in the PATH,
  # let's try to execute it.
  if [ -f "$TZ" -a -n "$TEZOS_CLIENT" ]; then
      run "$i" "$TZ"
  fi
done
