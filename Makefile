all:
	cd src && ocamlbuild yypkg.native makeypkg.native

install:
	cp src/yypkg.native /opt/mgw64/bin/yypkg
	cp src/makeypkg.native /opt/mgw64/bin/makeypkg
