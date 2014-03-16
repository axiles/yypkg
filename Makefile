KIND=native
TARGETS=yypkg.$(KIND)

all:
	cd src && OCAMLFIND_COMMANDS="ocamlc=ocamlopt" ocamlbuild -use-ocamlfind -cflags -passopt,-compact,-passopt,-nodynlink -cflags -ccopt,-Wall,-ccopt,-Wextra,-ccopt,-O2 -lflags yypkg/win.o win.o $(TARGETS)

release-tarball:
	git archive --prefix=yypkg-$(VERSION)/ $(VERSION) --format=tar \
	  | gzip -9 \
	  > yypkg-$(VERSION).tar.gz

clean:
	cd src && ocamlbuild -clean
	cd src && rm -f $(TARGETS)
