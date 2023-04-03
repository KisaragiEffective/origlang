#!/bin/sh

this_dir="$(dirname "$0")"
wasm-pack build --release --target web -d "$this_dir"
