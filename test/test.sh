#!/bin/bash

for f in ./test/bad2/*.lat; do 
	echo "--" $f "--"
  cabal run compiler -- "$f" 
done;

echo "!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!"
echo "!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!"
echo "!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!"
echo

for f in ./test/bad/*.lat; do 
	echo "--" $f "--"
  cabal run compiler -- "$f" 
done;
