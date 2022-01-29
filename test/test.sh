#!/bin/bash

for f in ./test/good/*.lat; do 
	echo "--" $f "--"
  DIRNAME=`dirname $f`
  BASENAME=`basename $f .lat`
  OUTPUT="$DIRNAME/${BASENAME}.output"
  BCFILE="$DIRNAME/${BASENAME}.bc"
  ./latc_llvm "$f" 
  lli $BCFILE > moje.out
  diff moje.out $OUTPUT
  echo 
  echo 
done;

echo "!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!"
echo "!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!"
echo "!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!"
echo

for f in ./test/extensions/arrays1/*.lat; do 
	echo "--" $f "--"
  DIRNAME=`dirname $f`
  BASENAME=`basename $f .lat`
  OUTPUT="$DIRNAME/${BASENAME}.output"
  BCFILE="$DIRNAME/${BASENAME}.bc"
  ./latc_llvm "$f" 
  lli $BCFILE > moje.out
  diff moje.out $OUTPUT
  echo 
  echo 
done;

echo "!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!"
echo "!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!"
echo "!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!"
echo

for f in ./test/bad/*.lat; do 
	echo "--" $f "--"
  cabal run compiler -- "$f" 
done;
