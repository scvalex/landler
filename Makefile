.PHONY: all build test dist install clean doc

all: build

build: dist/setup-config
	cabal build

test: build
	runhaskell Test/Unit.hs
	runhaskell Test/QC.hs

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
