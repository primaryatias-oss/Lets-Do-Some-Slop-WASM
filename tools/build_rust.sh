#!/bin/sh
set -e
cd "$(dirname "$0")/../core/rust"
cargo build --release --target wasm32-unknown-unknown 2>&1 | tail -20
mkdir -p ../../wasm && cp target/wasm32-unknown-unknown/release/slop_core.wasm ../../wasm/rust.wasm
ls -l ../../wasm/rust.wasm
