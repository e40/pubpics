# $Id$

on_windows = $(shell if test -d "c:/"; then echo yes; else echo no; fi)

ifeq ($(on_windows),yes)
acldir = //c/Program Files/ACL60
mlisp = "$(acldir)/mlisp.exe" +B +cn
else
mlisp = mlisp
endif

all:	build install

build:	FORCE
	rm -fr dist
	$(mlisp) -L buildit.cl -kill
ifeq ($(on_windows),yes)
	"$(acldir)/bin/setcmd -o dist/pubpics.exe --
endif

install: FORCE
ifeq ($(on_windows),yes)
	cp -p dist/*.* c:/bin
else
	rm -fr /usr/local/pubpics
	mkdir /usr/local/pubpics
	cp -p dist/* /usr/local/pubpics
	rm -f /usr/local/bin/pubpics
	ln -s /usr/local/pubpics/pubpics /usr/local/bin/pubpics
endif

clean: FORCE
	rm -fr *.fasl dist

FORCE:
