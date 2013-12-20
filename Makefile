KIND=native
TARGETS=win.o yypkg.$(KIND) makeypkg.$(KIND) sherpa.$(KIND) sherpa_gen.$(KIND)

all:
	cd src && ocamlbuild -use-ocamlfind -cflag -rectypes -cflags -ccopt,-Wall,-ccopt,-Wextra,-ccopt,-O2 -lflags yypkg/win.o $(TARGETS)

clean:
	cd src && ocamlbuild -clean
	rm -f src/*.$(KIND)
