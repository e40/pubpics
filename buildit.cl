
(in-package :user)

(setq excl::*break-on-warnings* t)

(declaim (optimize (speed 3)))
(compile-file "pubpics.cl")

(generate-application
 "pubpics"
 "pubpics/"
 '(:trace #-mswindows :process "pubpics.fasl")
 :restart-init-function 'pubpics-init-function
 :include-ide nil
 :include-compiler nil
 :us-government nil
 :presto nil
 :discard-local-name-info t
 :discard-source-file-info t
 :discard-xref-info t
 :load-xref-info nil
 :load-source-file-info nil
 :record-xref-info nil
 :record-source-file-info nil
 :include-devel-env nil
 :include-tpl nil
 :newspace 6144
 :oldspace 256000
 :show-window :normal
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 :autoload-warning nil
 :purify nil
 :runtime :standard
 :suppress-allegro-cl-banner t
 )

#+mswindows
(progn
  (delete-file "pubpics/pubpics.exe")
  (sys:copy-file "sys:buildi.exe" "pubpics/pubpics.exe"))

(with-open-file (s "pubpics/pubpics.rc" :direction :output)
  (format s ".command-line: --~%"))

(sys:copy-file "home.gif" "pubpics/home.gif")
(sys:copy-file "next.gif" "pubpics/next.gif")
(sys:copy-file "previous.gif" "pubpics/previous.gif")
(sys:copy-file "blank.gif" "pubpics/blank.gif")
