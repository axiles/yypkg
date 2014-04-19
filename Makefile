test:
	cd src && ocamlbuild -use-ocamlfind http_get_repro.native

all:
	cd src && ocamlbuild -use-ocamlfind \
		-cflags -ccopt,-Wall,-ccopt,-Wextra,-ccopt,-O2 -lflags yypkg/win.o \
		win.o yypkg.native

release-tarball:
	git archive --prefix=yypkg-$(VERSION)/ $(VERSION) --format=tar \
	  | gzip -9 \
	  > yypkg-$(VERSION).tar.gz

clean:
	cd src && ocamlbuild -clean && rm -f yypkg.native
