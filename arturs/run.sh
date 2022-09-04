#!/bin/bash

SCORES=mosaic-run/scores.txt
if [[ $1 -le 0 || $1 -gt 35 ]]; then
    echo ARG Please!
    exit
fi
if [[ -z $SIZE ]]; then
    SIZE=$(head -n $1 $SCORES | tail -n 1 | sed "s/.*(//" | sed "s/)//")
fi
sbcl --noinform \
     --eval '(load "main.lisp")' \
     --eval "(top-level $1 $SIZE $SIZE)"
