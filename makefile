# $Id$

on_windows = $(shell if test -d "c:/"; then echo yes; else echo no; fi)

ifeq ($(on_windows),yes)
acldir = //c/Program Files/ACL60
mlisp = "$(acldir)/mlisp.exe" +B +cn
else
mlisp = mlisp
endif

default:	build

all:	build install

build:	FORCE
	rm -fr pubpics
	$(mlisp) -L buildit.cl -kill -batch

install: FORCE
ifeq ($(on_windows),yes)
	cp -p pubpics/*.* c:/bin
else
	rm -fr /usr/local/pubpics
	mkdir /usr/local/pubpics
	cp -p pubpics/* /usr/local/pubpics
	rm -f /usr/local/bin/pubpics
	ln -s /usr/local/pubpics/pubpics /usr/local/bin/pubpics
endif

dist-files = ChangeLog *.cl exif-utils/*.cl *.gif makefile
dist-version = \
 $(shell grep Revision: pubpics.cl | sed 's/.*Revision: \([0-9.]*\).*/\1/')

dist-tar = pubpics-$(dist-version)-linux-glibc-2.1.tar
dist-gz = $(dist-tar).gz
dist-bz2 = $(dist-tar).bz2
dist-src = pubpics-$(dist-version)-src.tar.gz

dist:	FORCE
	rm -fr pubpics-$(dist-version) *.gz *.bz2
	cp -rp pubpics pubpics-$(dist-version)
	tar cf $(dist-tar) pubpics-$(dist-version)
	gzip -c9 < $(dist-tar) > $(dist-gz)
	bzip2 -c9 < $(dist-tar) > $(dist-bz2)
	rm -f $(dist-tar)
	tar zcf $(dist-src) $(dist-files)
	rm -fr pubpics-$(dist-version)

clean: FORCE
	rm -fr *.fasl pubpics *.gz *.bz2 */*.fasl

FORCE:
