build:
	jbuilder build

install:
	jbuilder install

uninstall:
	jbuilder uninstall

doc:
	ocamlbuild -I lib lib.docdir/index.html
	cp style.css lib.docdir/

clean:
	rm -rf _build/ lib.docdir clarity.install lib/.merlin

tests: clean
	jbuilder runtest

.PHONY: build install uninstall doc clean tests

