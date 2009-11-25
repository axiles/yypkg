#!/bin/sh
export OCAMLRUNPARAM="b"
export PREFIX="./prefix"

tar cf test.tar -C test_package . &&
tar cf juju.tar -C juju . &&
rm -rf prefix db &&
ocamlbuild yaxmpkg.native &&
./yaxmpkg.native -install test.tar &&
./yaxmpkg.native -install juju.tar &&
./yaxmpkg.native -uninstall juju &&
./yaxmpkg.native -uninstall test
