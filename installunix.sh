#! /bin/sh -x

if test ! -d /usr/local/pubpics; then
	mkdir /usr/local/pubpics
fi

cp -p dist/* /usr/local/pubpics

rm -f /usr/local/bin/pubpics
ln -s /usr/local/pubpics/pubpics /usr/local/bin/pubpics

