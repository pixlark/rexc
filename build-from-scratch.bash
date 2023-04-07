#!/usr/bin/env bash

#
# Utility
#

# https://stackoverflow.com/a/29436423
function yes_or_no {
    while true; do
        read -rp "$* [y/n]: " yn
        case $yn in
            [Yy]*) return 0 ;;
            [Nn]*) return 1 ;;
        esac
    done
}

#
# Informational message
#

printf -- "- build-from-scratch.bash -\n\n"
printf "This script performs *all* the necessary steps to build the compiler from scratch.\n"
printf "Once this is run once, all that needs to be done for further builds is to run \"cargo build\"!\n\n"

#
# Check that BDWGC submodule is cloned
#

if [[ ! -e "bdwgc/CMakeLists.txt" ]]
then {
    printf "Error!\n bdwgc submodule is empty. Have you initialized the git submodules?\n"
    exit 1
}
fi

#
# Ask for platform and check for required binaries
#

if ! type -p cargo > /dev/null
then {
    printf "Error!\n Could not find cargo on your PATH - do you have Rust installed?\n"
    exit 1
}
fi

if ! type -p cmake > /dev/null
then {
    printf "Error!\n Could not find cmake on your PATH - do you have CMake installed?\n"
    exit 1
}
fi

python_path=
if type -p python3 > /dev/null
then python_path=$(which python3)
elif type -p python > /dev/null
then python_path=$(which python)
else {
    printf "Error!\n Could not find python on your PATH - do you have Python installed?\n"
    exit 1
}
fi

python_version=$($python_path -c 'import sys; print(sys.version_info[0])')

if [[ $python_version != 3 ]]
then {
    printf "Error!\n Could not find Python 3 - one of python/python3 points to a Python 2 installation.\n"
    exit 1
}
fi

platform=

if yes_or_no "Are you building on Windows MSYS2?"
then platform="msys2"
elif yes_or_no "Are you building on *nix?"
then platform="nix"
else {
    printf "Error!\n Did not select a supported plaform.\n"
    exit 1
}
fi

if [[ $platform = "msys2" ]]
then {
    if ! type -p cygpath > /dev/null
    then {
        printf "Error!\n Building on MSYS2 but could not find cygpath on your PATH.\n"
        exit 1
    }
    fi
}
fi

#
# Fetch lesen requirements
#

pushd lesen
$python_path -m pip install -r requirements.txt || {
    "Error!\n Could not install lesen requirements via \"python -m pip\"\n"
    exit 1
}
popd

#
# Build rust binaries
#

cargo build || {
    printf "Error!\n cargo build failed.\n"
    exit 1
}

#
# Build bdwgc library
#

# Get a project data directory for storing the libgc.a library

data_dir=$(cargo run --bin rexc_data_directory) || {
    printf "Error!\n Could not generate project data directory path.\n"
    exit 1
}

if [[ $platform = "msys2" ]]
then data_dir=$(cygpath "$data_dir")
fi

mkdir -p "$data_dir" || {
    printf "Error!\n Could not create data directory $data_dir.\n"
    exit 1
}

# Run the cmake build

generator=

if [[ $platform = "msys2" ]]
then generator="-G MSYS Makefiles"
fi

mkdir -p bdwgc-build
pushd bdwgc-build || {
    printf "Error!\n Could not create/enter bdwgc-build directory.\n"
    exit 1
}
cmake -Dbuild_tests=OFF -DBUILD_SHARED_LIBS=OFF "$generator" ../bdwgc || {
    printf "Error!\n BDWGC build preparation failed.\n"
    exit 1
}
cmake --build . || {
    printf "Error!\n BDWGC build step failed.\n"
    exit 1
}

# Copy the library into our data directory

cp libgc.a "$data_dir/"
popd
