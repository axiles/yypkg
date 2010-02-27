#!/bin/sh

# ocamlfind query -format "-I %d %a" -predicates syntax,preprocessor -r mikmatch_str -separator ' '
# -I /ocaml/lib/ocaml/pkg-lib/type-conv pa_type_conv.cmo -I /ocaml/lib/ocaml/pkg-lib/sexplib pa_sexp_conv.cmo

# ocamlfind query -format "-I %d %a" -predicates syntax,preprocessor -r sexplib.syntax -separator ' '
# -I /ocaml/lib/ocaml/site-lib/mikmatch_str pa_mikmatch_str.cma

O=preprocessed_src
mkdir -p $O/{makeypkg,yypkg}

PKGS='mikmatch_str sexplib.syntax'
I_FLAGS='ocamlfind query -i-format -predicates syntax,preprocessor -r -separator " "'
A_FLAGS='ocamlfind query -a-format -predicates syntax,preprocessor -r -separator " "'
I_PARAMS=$(eval "$I_FLAGS $PKGS")
A_PARAMS=$(eval "$A_FLAGS $PKGS")

find src -iname "*.ml" \! -name "*myocamlbuild.ml*" \! -path "*_build*" -print0 | while read -d "$(echo -en '\0')" A; do camlp4o $I_PARAMS $A_PARAMS $A -o ${A/src/preprocessed_src}; done

