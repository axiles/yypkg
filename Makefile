all:
	ocamlbuild yaxmpkg.native makeypkg.native

install:
	cp yaxmpkg.native /opt/mgw64/bin/yaxmpkg
	cp makeypkg.native /opt/mgw64/bin/makeypkg
