KIND=native
TARGETS=yypkg.$(KIND) makeypkg.$(KIND) sherpa.$(KIND) sherpa_gen.$(KIND)

all:
	cd src && ocamlbuild -use-ocamlfind -cflags -ccopt,-Wall,-ccopt,-Wextra,-ccopt,-O2 -lflags yypkg/win.o win.o $(TARGETS)

clean:
	cd src && ocamlbuild -clean
	cd src && rm -f $(TARGETS)
