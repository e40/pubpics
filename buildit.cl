
(in-package :user)

(delete-directory-and-files "c:/src/pubpics/dist/" :if-does-not-exist :ignore)

(declaim (optimize (speed 3)))
(compile-file "pubpics.cl")

(generate-application
 "pubpics"
 "dist/"
 '("pubpics.fasl")
 :restart-init-function 'pubpics-init-function
 ;;:build-executable "build.exe"
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

(delete-file "dist/pubpics.exe")
(sys:copy-file "c:/Program files/ACL60/buildi.exe" "dist/pubpics.exe")

(sys:copy-file "home.gif" "dist/home.gif")
(sys:copy-file "next.gif" "dist/next.gif")
(sys:copy-file "previous.gif" "dist/previous.gif")

