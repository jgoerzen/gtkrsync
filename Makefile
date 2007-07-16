# arch-tag: Primary makefile
# Copyright (c) 2004-2006 John Goerzen
#

DESTDIR ?= /usr/local
all: setup			# GHC build
	./setup configure --prefix=$(DESTDIR)
	./setup build

install:
	./setup install
	@echo "Please manually install docs/gtkrsync.1"

hugsbuild: setup
	./setup configure --hugs
	./setup build

setup: Setup.lhs
	ghc -package Cabal Setup.lhs -o setup

clean: clean-code clean-doc

clean-code:
	-./setup clean
	-cd libsrc && ../setup clean
	-rm -rf dist libsrc/dist *.ho *.hi *.o *.a setup *~
	-rm -f `find . -name "*~"` `find . -name "*.o"`
	-rm -f `find . -name "*.cm*"`

clean-doc:
	-cd doc && scons -c && scons -c html pdf text ps gtkrsync.1
	-rm -rf docs/.sconsign* .depend test
	-rm -f docs/manpage*
