@echo off

rm -fr dist
"c:\Program Files\ACL60\mlisp.exe" +B +cn -L buildit.cl -kill
sh installwin.sh
