#!/bin/bash

grep -v "^#" \
| sed "s/^/(process '/" \
| sed "s/$/)/" \
| sed "s/\[x\]/'x/g" \
| sed "s/\[y\]/'y/g" \
| sed "s/\[X\]/'x/g" \
| sed "s/\[Y\]/'y/g" \
| sed "s/\]/)/g" \
| sed "s/\[/(list /g" \
| sed "s/\./ /g" \
| sed "s/\,/ /g"


