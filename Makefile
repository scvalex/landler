.PHONY: all build test dist install clean doc

all: build

build: dist/setup-config
	cabal build

test: install
	OK=true &&\
	{ runhaskell Test/Unit.hs || OK=false ; } &&\
	{ runhaskell Test/QC.hs || OK=false ; } &&\
	$$OK

dist: test
	cabal sdist

install: build
	cabal install

clean:
	cabal clean

dist/setup-config: landler.cabal
	cabal configure

doc: build
	cabal haddock
