#!/usr/bin/env sh

# For Windows MSYS2
# ./build-libgc <configuration=debug|release>

mkdir -p bdwgc-build/
cd bdwgc-build
cmake -Dbuild_tests=OFF -DBUILD_SHARED_LIBS=OFF -G "MSYS Makefiles" ../bdwgc
cmake --build .

mkdir -p ../target/$1
cp libgc.a ../target/$1/
