#!/usr/bin/make -f

.PHONY: clean mount umount

MAIN_SOURCES :=   \
	FSMain.hs       \
	Route.hs      \
	TagFS.hs      \
	TagSet.hs     \
	Stat.hs       \
	Config.hs

Main: $(MAIN_SOURCES)
	ghc -O2 -threaded FSMain.hs

clean:
	rm -f *.hi *.o Main

mount: FSMain
	./FSMain test

umount:
	fusermount -u test

debug: FSMain
	./FSMain -d test

foreground: FSMain
	./FSMain -f test

remount: FSMain
	fusermount -u test
	./FSMain test
