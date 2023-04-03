#!/bin/sh

set -x
this_dir="$(dirname "$0")"
(cd "$this_dir" && wasm-pack build --release --target web -d "$this_dir/pkg")
