# $Id$

on_windows = $(shell if test -d c:/; then echo yes; else echo nol fi)

ifeq ($(on_windows),yes)
mlisp = "//c/Program Files/ACL60/mlisp.exe" +B +cn
else
mlisp = mlisp
endif

all:	build install

build:	FORCE
	rm -fr dist
	mlisp -L buildit.cl -kill

install: FORCE
ifeq ($(on_windows),yes)
	cp -p dist/*.* c:/bin
else
	-mkdir /usr/local/pubpics
	cp -p dist/* /usr/local/pubpics
	rm -f /usr/local/bin/pubpics
	ln -s /usr/local/pubpics/pubpics /usr/local/bin/pubpics
endif

FORCE:

