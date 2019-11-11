#!/bin/bash
set -e

./main.native atprograms/own/svg/parse.oat -S --print-ll > output/svg.ll
clang output/svg.ll runtime.c -o output/svg