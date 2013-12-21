KIND=native
TARGETS=yypkg.$(KIND) makeypkg.$(KIND) sherpa.$(KIND) sherpa_gen.$(KIND)

all:
	cd src && OCAMLFIND_COMMANDS="ocamlc=ocamlopt" ocamlbuild -use-ocamlfind -cflags -passopt,-compact -cflags -ccopt,-Wall,-ccopt,-Wextra,-ccopt,-O2 -lflags yypkg/win.o win.o $(TARGETS)

clean:
	cd src && ocamlbuild -clean
	cd src && rm -f $(TARGETS)
