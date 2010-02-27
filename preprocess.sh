#!/bin/sh

# ocamlfind query -format "-I %d %a" -predicates syntax,preprocessor -r mikmatch_str -separator ' '
# -I /ocaml/lib/ocaml/pkg-lib/type-conv pa_type_conv.cmo -I /ocaml/lib/ocaml/pkg-lib/sexplib pa_sexp_conv.cmo

# ocamlfind query -format "-I %d %a" -predicates syntax,preprocessor -r sexplib.syntax -separator ' '
# -I /ocaml/lib/ocaml/site-lib/mikmatch_str pa_mikmatch_str.cma

O=preprocessed_src
mkdir -p $O/{makeypkg,yypkg}
cp src/{_tags,myocamlbuild.ml} $O

PKGS='mikmatch_str sexplib.syntax'
CMD='ocamlfind query -format "-I %d %a" -predicates syntax,preprocessor -r -separator " "'
PARAMS=$(eval "$CMD $PKGS")

find src -iname "*.ml" \! -name "*myocamlbuild.ml*" \! -name "*_build*" -print0 | while read -d "$(echo -en '\0')" A; do camlp4o $PARAMS $A > ${A/src/preprocessed_src}; done

