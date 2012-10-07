all:
	cd src && ocamlbuild -cflag -rectypes yypkg.native makeypkg.native sherpa.native sherpa_gen.native

clean:
	cd src && ocamlbuild -clean

install:
	cp src/yypkg.native /opt/mgw64/sbin/yypkg
	cp src/makeypkg.native /opt/mgw64/sbin/makeypkg
	cp src/sherpa.native /opt/mgw64/sbin/sherpa
