
(in-package :user)

(setq excl::*break-on-warnings* t)

(declaim (optimize (speed 3)))
(compile-file "pubpics.cl")

(generate-application
 "pubpics"
 "dist/"
 '(#-mswindows :process "pubpics.fasl")
 :restart-init-function 'pubpics-init-function
 :include-ide nil
 :include-compiler nil
 :us-government nil
 :presto nil
 :debug-on-error t
 :discard-local-name-info t
 :discard-source-file-info t
 :discard-xref-info t
 :load-xref-info nil
 :load-source-file-info nil
 :record-xref-info nil
 :record-source-file-info nil
 :exit-after-image-build t
 :include-common-graphics nil
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
;;;:internal-debug "build.in"
 )

#+mswindows
(progn
  (delete-file "dist/pubpics.exe")
  (sys:copy-file "sys:buildi.exe" "dist/pubpics.exe"))

(with-open-file (s "dist/pubpics.rc" :direction :output)
  (format s ".command-line: --~%"))

(sys:copy-file "home.gif" "dist/home.gif")
(sys:copy-file "next.gif" "dist/next.gif")
(sys:copy-file "previous.gif" "dist/previous.gif")
