#!/bin/bash

N=${N:-1}
S=$(head -n $N mosaic-run/scores.txt | tail -n 1 | sed "s/.*(//" | sed "s/)//")
sbcl --noinform \
     --eval '(ql:quickload "png-read")' \
     --eval '(load "main.lisp")' \
     --eval "(top-level $N $S $S)"
