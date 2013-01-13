#!/usr/bin/make -f

.PHONY: clean mount umount

MAIN_SOURCES := \
	Main.hs     \
	Route.hs    \
	TagFS.hs    \
	TagSet.hs   \
	Stat.hs

Main: $(MAIN_SOURCES)
	ghc -O2 -threaded Main.hs

clean:
	rm -f *.hi *.o Main

mount: Main
	./Main test

umount:
	fusermount -u test

debug: Main
	./Main -d test

remount: Main
	fusermount -u test
	./Main test
