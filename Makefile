build:
	dune build

install:
	dune install

uninstall:
	dune uninstall

doc:
	ocamlbuild -I lib -I lib/classes -I lib/types -I lib/vector lib.docdir/index.html

clean:
	rm -rf _build/ lib.docdir clarity.install lib/.merlin

tests: clean
	dune runtest --no-buffer

.PHONY: build install uninstall doc clean tests
