#!/bin/bash

rm -rf bin lib

mkdir bin
cp $(which ceta-postproc) bin/

mkdir lib
cp  /usr/lib/libgmp.so.3 lib

zip -r ceta-postproc.zip process bin/ lib/
