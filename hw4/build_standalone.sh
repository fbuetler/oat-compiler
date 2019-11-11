#!/bin/bash
set -e
cd "${0%/*}"

./main.native "$1" -S --print-ll | tail -n +2 > output/standalone.ll
clang output/standalone.ll runtime.c -o "$2" -Wno-override-module 