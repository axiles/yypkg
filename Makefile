all:
	ocamlbuild yaxmpkg.native makeypkg.native

install:
	cp yaxmpkg.native makeypkg.native /opt/mgw64/bin
