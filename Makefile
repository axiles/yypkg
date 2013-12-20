KIND=native
TARGETS=win.o yypkg.$(KIND) makeypkg.$(KIND) sherpa.$(KIND) sherpa_gen.$(KIND)

all:
	cd src && ocamlbuild -use-ocamlfind -cflag -rectypes -cflags -ccopt,-Wall,-ccopt,-Wextra,-ccopt,-O2 -lflags yypkg/win.o $(TARGETS)

clean:
	cd src && ocamlbuild -clean
	rm -f src/*.$(KIND)

install:
	cp src/yypkg.native /opt/mgw64/sbin/yypkg
	cp src/makeypkg.native /opt/mgw64/sbin/makeypkg
	cp src/sherpa.native /opt/mgw64/sbin/sherpa
