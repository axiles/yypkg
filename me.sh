#!/bin/sh
export OCAMLRUNPARAM="b"
export PREFIX="."

tar cf test.tar -C test_package . &&
rm -rf prefix db &&
ocamlbuild yaxmpkg.native -- -install test.tar &&
./yaxmpkg.native -uninstall test
