
all: build

clean:
	runhaskell Setup.hs clean

configure: Setup.hs rdf4h.cabal *.hs
	runhaskell Setup.hs configure --user --prefix=${HOME} \
		--docdir=dist/doc \
	    --haddock-options="-v \
		--source-base=http://protempore.net/rdf4h/ \
		--source-module=http://protempore.net/rdf4h/doc/html/rdf4h/src/%M.hs"

build: configure
	runhaskell Setup.hs build

haddock: configure build
	runhaskell Setup.hs haddock --hyperlink-source

install:  configure build
	runhaskell Setup.hs install

# The test function compiles, so no need to depend on configure or compile.
test:
	runhaskell Setup.hs test
