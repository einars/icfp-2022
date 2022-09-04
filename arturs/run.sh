#!/bin/bash

if [[ $1 -le 0 || $1 -gt 35 ]]; then
    echo ARG Please!
    exit
fi
S=$(head -n $1 mosaic-run/scores.txt | tail -n 1 | sed "s/.*(//" | sed "s/)//")
sbcl --noinform \
     --eval '(load "main.lisp")' \
     --eval "(top-level $1 $S $S)"
