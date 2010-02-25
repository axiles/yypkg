all:
	ocamlbuild yypkg.native makeypkg.native

install:
	cp yypkg.native /opt/mgw64/bin/yypkg
	cp makeypkg.native /opt/mgw64/bin/makeypkg
