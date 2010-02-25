#!/bin/sh
export OCAMLRUNPARAM="b"
export PREFIX="./prefix"

tar cf test.tar -C test_package . &&
tar cf juju.tar -C juju . &&
rm -rf prefix db &&
ocamlbuild yypkg.native makeypkg.native &&
./yypkg.native -install test.tar &&
./yypkg.native -install juju.tar &&
./yypkg.native -uninstall juju &&
./yypkg.native -uninstall test
