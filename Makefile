# This Makefile is mostly a wrapper around Setup.hs for people who
# just want to type make.

all: build

configure: .setup-config
.setup-config:
	runhaskell ./Setup.hs configure

build: configure
	runhaskell ./Setup.hs build

install: build
	runhaskell ./Setup.hs install

test:
	runhaskell tests/Main.hs

dist:
	runhaskell ./Setup.hs sdist

haddock doc:
	runhaskell ./Setup.hs haddock

clean:
	-runhaskell ./Setup.hs clean
	-rm -rf dist
#	$(MAKE) -C test clean

.PHONY: all configure build install dist haddock doc clean
