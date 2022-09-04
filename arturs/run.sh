#!/bin/bash

SCORES=mosaic-run/scores.txt
if [[ $1 -le 0 || $1 -gt 40 ]]; then
    echo ARG Please!
    exit
fi
if [[ -z $SIZE && $1 -le 25 ]]; then
    SIZE=$(head -n $1 $SCORES | tail -n 1 | sed "s/.*(//" | sed "s/)//")
elif [[ -z $SIZE ]]; then
    SIZE=8
fi
sbcl --noinform \
     --eval '(load "main.lisp")' \
     --eval "(top-level $1 $SIZE $SIZE)"

# sbcl --load main.lisp --eval "(build)"
