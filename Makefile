build:
	dune build

install:
	dune install

uninstall:
	dune uninstall

doc:
	ocamlbuild -I lib lib.docdir/index.html
	cp style.css lib.docdir/

clean:
	rm -rf _build/ lib.docdir clarity.install lib/.merlin

tests: clean
	dune runtest --no-buffer

.PHONY: build install uninstall doc clean tests
