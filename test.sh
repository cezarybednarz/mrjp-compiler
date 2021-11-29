#!/bin/bash
for f in ./test/good/*.lat; do 
	echo "-----------------------------------"
	echo "---"   $f   "---"
  echo "-----------------------------------"
  cabal run compiler -- "$f" > test/out
  #echo "--"
	#cat ${f::24}output
  cat test/out 
done;

echo "!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!"
echo "!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!"
echo "!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!"
echo "!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!"
echo "!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!"

for f in ./test/bad/*.lat; do 
	echo "-----------------------------------"
	echo "---"   $f   "---"
  echo "-----------------------------------"
  cabal run compiler -- "$f" > test/out
  #echo "--"
	#cat ${f::24}output
  cat test/out 
done;