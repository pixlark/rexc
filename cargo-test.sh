#!/usr/bin/env sh
cargo test $1 -- --nocapture --test-threads 1
