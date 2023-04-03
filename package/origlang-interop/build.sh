#!/bin/sh

this_dir="$(basename "$0")"
wasm-pack build --release --target web -d "$this_dir"
