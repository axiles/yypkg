#!/bin/sh
export OCAMLRUNPARAM="b"
export PREFIX="."

tar cf test.tar -C test_package . &&
tar cf juju.tar -C juju . &&
rm -rf prefix db &&
ocamlbuild yaxmpkg.native -- -install test.tar &&
ocamlbuild yaxmpkg.native -- -install juju.tar &&
ocamlbuild yaxmpkg.native -- -uninstall juju
ocamlbuild yaxmpkg.native -- -uninstall test
