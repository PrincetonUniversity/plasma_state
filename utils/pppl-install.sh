#!/bin/bash

rm -rf builds *.out

export MAKE_SO=1

echo "GCC 11.2.0"
export COMPILER=GCC ; export COMPILER_VER=11.2.0 ; source environ
make --no-print-directory | tee gcc11.out
mv builds/gcc/11.2.0 builds/gcc/11.2

echo "GCC 9.3.0"
export COMPILER=GCC ; export COMPILER_VER=9.3.0 ; source environ
make --no-print-directory | tee gcc9.out
mv builds/gcc/9.3.0 builds/gcc/9.3

echo "ICC 2019"
export COMPILER=ICC ; export COMPILER_VER=2019 ; source environ
make --no-print-directory | tee icc19.out

echo "ICC 2022"
export COMPILER=ICC ; export COMPILER_VER=2022 ; source environ
make --no-print-directory | tee icc22.out

echo "PGC 19.10"
export COMPILER=PGC ; export COMPILER_VER=19.10 ; source environ
make --no-print-directory | tee pgc19.out
