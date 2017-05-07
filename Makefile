build:
	jbuilder build

install:
	jbuilder install

uninstall:
	jbuilder uninstall

clean:
	rm -r _build/
	rm clarity.install

.PHONY: build install uninstall clean

