all:
	cd src && ocamlbuild -cflag -rectypes yypkg.native makeypkg.native

install:
	cp src/yypkg.native /opt/mgw64/sbin/yypkg
	cp src/makeypkg.native /opt/mgw64/sbin/makeypkg
