#!/bin/bash
set -e
cd "${0%/*}"

# Usage: ./render_svg.sh <input_svg_file_path> <output_bmp_file_path>

./build_standalone.sh atprograms/own/svg/parse.oat output/svg
./output/svg "$(cat "$1")" | node decode_bytes.js > "$2"