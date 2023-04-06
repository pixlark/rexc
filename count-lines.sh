#!/usr/bin/env sh
cloc --force-lang=text,rx --exclude-dir=bdwgc,bdwgc-build,target --categorized=temp .
