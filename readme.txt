pubpics -- -t "our Xmas walk" -d "A walk in Berkeley on Xmas day"
   c:/pictures/_TMP/_work/to-publish/ c:/tmp/wwwtmp/


test:

windows (PII 450, 256MB):
  commands:
    time pubpics -- -t x -d y c:/pictures/family/adrian/birth c:/tmp/testxxx
    rm -fr c:/tmp/testxxx
  time: 7m31.018s

linux (Celeron 500, 128MB):
  commands:
    time pubpics -- -t x -d y /c/pictures/family/adrian/birth /tmp/testxxx
    rm -fr c:/tmp/testxxx
  time: 
