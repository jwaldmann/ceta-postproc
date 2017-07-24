#!/bin/bash

rm -rf bin lib

mkdir bin
cp $(which ceta-postproc) bin/

mkdir lib
cp  /lib64/libgmp.so.10 lib

zip -r ceta-postproc.zip process bin/ lib/
