pubpics -- -t "our Xmas walk" -d "A walk in Berkeley on Xmas day"
   c:/pictures/_TMP/_work/to-publish/ c:/tmp/wwwtmp/


test:

windows (PII 450MHz, 256MB):
  commands:
    time pubpics c:/pictures/family/adrian/birth c:/tmp/testxxx
  time: 7m31.018s

linux (Celeron 500Mhz, 128MB):
  commands:
    time pubpics /c/pictures/family/adrian/birth /tmp/testxxx
  time: 6m56.37s

linux (Athlon 1.0GHz, 256MB):
  commands:
    time pubpics /c/pictures/family/adrian/birth /tmp/testxxx
  time: 6m56.37s
