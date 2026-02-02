#!/bin/bash

cd "$(dirname "$0")"

mkdir -p _out/log
cd _out
vivado -mode batch -source ../build.tcl
shopt -s nullglob
mv *.backup.jou log
mv *.backup.log log
shopt -u nullglob
