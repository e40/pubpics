# $Id$

on_windows = $(shell if test -d "c:/"; then echo yes; else echo no; fi)

ifndef mlisp
ifeq ($(on_windows),yes)
acldir = /c/Program Files/acl62
mlisp = "$(acldir)/mlisp.exe" +B +cn
else
mlisp = mlisp
endif
endif

default:	build

all:	clean build install

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

dist-version = \
 $(shell grep Revision: pubpics.cl | sed 's/.*Revision: \([0-9.]*\).*/\1/')

src_files = readme.txt ChangeLog *.cl exif-utils/*.cl *.gif makefile

bin_dir = pubpics-$(dist-version)
src_dir = pubpics-$(dist-version)-src
bin_tar = $(bin_dir)-linux-glibc-2.1.tar

bin_gz = $(bin_tar).gz
bin_bz2 = $(bin_tar).bz2
src_gz = $(src_dir).tar.gz

dist:	FORCE
	rm -fr $(bin_dir) $(bin_tar) $(src_dir) *.gz *.bz2
## binary distribution
	cp -rp pubpics $(bin_dir)
	cp -p readme.txt $(bin_dir)
	tar cf $(bin_tar) $(bin_dir)
	gzip -c9 < $(bin_tar) > $(bin_gz)
	bzip2 -c9 < $(bin_tar) > $(bin_bz2)
	rm -f $(bin_tar)
	rm -fr $(bin_dir)
## source distribution
	mkdir $(src_dir)
	cp -p $(src_files) $(src_dir)
	tar zcf $(src_gz) $(src_dir)
	rm -fr $(src_dir)

clean: FORCE
	rm -fr *.fasl pubpics testout *.gz *.bz2 */*.fasl

test: FORCE
	rm -fr testout
	pubpics/pubpics test/ testout/

FORCE:
