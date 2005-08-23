# $Id$

on_windows = $(shell if test -d "c:/"; then echo yes; else echo no; fi)

ifndef mlisp
ifeq ($(on_windows),yes)
acldir = /c/Program Files/acl70
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

version = \
 $(shell grep Revision: pubpics.cl | sed 's/.*Revision: \([0-9.]*\).*/\1/')

src_files = readme.txt ChangeLog *.cl exif-utils/*.cl *.gif makefile

bin_dir = pubpics-$(version)

ifeq ($(on_windows),yes)
bin_zip = DIST/$(bin_dir)-windows.zip
else
bin_tar = $(bin_dir)-linux-glibc-2.2.tar
bin_gz  = DIST/$(bin_tar).gz
bin_bz2 = DIST/$(bin_tar).bz2
endif

src_dir = pubpics-$(version)-src
src_gz  = DIST/$(src_dir).tar.gz
src_zip = DIST/$(src_dir).zip

readme = DIST/readme-$(version).txt

src-dist: FORCE
	rm -fr $(src_dir) $(src_gz)
	mkdir $(src_dir)
	tar cf - $(src_files) | (cd $(src_dir); tar xf -)
	tar zcf $(src_gz) $(src_dir)
	find $(src_dir) -type f -print | zip -q $(src_zip) -@9
	rm -fr $(src_dir)

dist:	FORCE
	rm -fr $(bin_dir) $(bin_zip) $(bin_tar) $(bin_gz) $(bin_bz2) $(readme)
	cp -rp pubpics $(bin_dir)
	cp -p readme.txt $(bin_dir)
	cp -p readme.txt $(readme)
ifeq ($(on_windows),yes)
	find $(bin_dir) -type f -print | zip -q $(bin_zip) -@9
else
	tar cf $(bin_tar) $(bin_dir)
	gzip -c9 < $(bin_tar) > $(bin_gz)
	bzip2 -c9 < $(bin_tar) > $(bin_bz2)
	rm -f $(bin_tar)
endif
	rm -fr $(bin_dir)

clean: FORCE
	rm -fr *.fasl */*.fasl pubpics testout *.gz *.bz2

test: FORCE
	rm -fr testout
	pubpics/pubpics test/ testout/

FORCE:
