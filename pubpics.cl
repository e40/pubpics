;; $Id$

(in-package :user)

(eval-when (compile)
  (compile-file-if-needed
   (or (probe-file "exif-utils/exifdump.cl")
       #+mswindows (probe-file "c:/src/exif-utils/exifdump.cl")
       (error "Could not find exifdump.cl"))))

(eval-when (compile eval load)
  (require :exifdump
	   (or (probe-file "exif-utils/exifdump.fasl")
	       #+mswindows (probe-file "c:/src/exif-utils/exifdump.fasl")))
  (require :aserve)
  (use-package :net.html.generator)
  (require :aclwin)
  (require :fileutil))

(setq *read-init-files* nil)
(setq excl::*internal-read-init-files* nil)
(setq *print-startup-message* nil)
(setq excl::.dump-lisp-suppress-allegro-cl-banner. t)
(setq excl::*force-quiet-exit* t) ; 6.0
(setq sys::.ignore-command-line-arguments. t)

(defvar *version* "$Revision$")

(defvar *quiet* nil)
(defvar *debug* nil)
(defvar *usage*
    "Usage: [-V] [-q] [-t title] [-d description] source-dir dest-dir
-V -- print version info and exit
-q -- quiet mode (do not print informative messages)
-t title -- set title of generated pages to 'title', and use this title
   at the top of each photo page
-t description -- addition information for the index page
source-dir -- directory containing images
dest-dir -- non-existent directory for web pages
")

(defvar *image-magick-root*
    #+mswindows "c:/ImageMagick/"
    #-mswindows "/usr/local/bin/")

(defvar *large-divisor* 1.5)
(defvar *medium-divisor* 2)
(defvar *small-divisor* 3)

(defvar *cleanup-forms* nil)

(defun pubpics-init-function ()
  (handler-case
      (sys:with-command-line-arguments
	  ("d:I:fqt:V"
	   description image-file force-flag *quiet* title
	   print-version-and-exit)
	  (rest)
	(declare (ignore image-file))
	(when print-version-and-exit
	  (format t "pubpics: ~a~%" *version*)
	  (exit 0 :quiet t))
	(when (/= 2 (length rest)) (error-die *usage*))
	(pubpics (first rest) (second rest) :force-flag force-flag
		 :title title :description description)
	(exit 0 :quiet t))
    (excl::asynchronous-operating-system-signal (c)
      (dolist (form *cleanup-forms*)
	(funcall form c))
      (exit 1 :quiet t))
    (error (c) (error-die "An error occurred: ~a." c))))

(defun pubpics (source-dir dest-dir
		&key force-flag title description
		&aux (pictures '())
		     (sizes '(:small :medium :large))
		     i npics)
  
  (setq source-dir (pathname-as-directory (pathname source-dir)))
  (setq dest-dir (pathname-as-directory (pathname dest-dir)))
  
  (when (not (probe-file source-dir))
    (error-die "~a does not exist." source-dir))

  ;; Make sure dest-dir doesn't exist, and remove it if it does and
  ;; force-flag is non-nil.
  (when (probe-file dest-dir)
    (if* force-flag
       then (when (not (file-directory-p dest-dir))
	      (error-die "~a is not a directory." dest-dir))
	    (my-delete-directory-and-files dest-dir)
       else (error-die "~a already exists." dest-dir)))

  (when (null title)
    (setq title (car (last (pathname-directory source-dir)))))

  (when (null description)
    (setq description
      (multiple-value-bind (second minute hour day month year)
	  (decode-universal-time (file-write-date source-dir))
	(declare (ignore second minute hour))
	(format nil "~d~a~d"
		day
		(aref #(0 "Jan" "Feb" "Mar" "Apr" "May" "Jun" "Jul" "Aug"
			"Sep" "Oct" "Nov" "Dec")
		      month)
		year))))

  (push #'(lambda (condition)
	    (format t "~&~a~%Cleaning up...~%" condition)
	    (force-output)
	    (when (probe-file dest-dir)
	      (my-delete-directory-and-files dest-dir)))
	*cleanup-forms*)

  (dolist (dir '("large/" "medium/" "small/" "thumbs/"
		 "pages/" "pages/small/" "pages/medium/" "pages/large/"))
    (ensure-directories-exist (merge-pathnames dir dest-dir)))

  (when (not *quiet*)
    (format t "~%collecting information:")
    (force-output))
  (setq i 0)
  (map-over-directory
   #'(lambda (p)
       (when (equalp "jpg" (pathname-type p))
	 (incf i)
	 (let ((info (image-info p)))
	   (push (list p info) pictures))))
   source-dir
   :recurse nil)
  (setq npics i)
  (when (not *quiet*) (format t " ~d pictures~%" i))
  
  ;; Put the pictures in "numerical" order.
  (setq pictures
    (sort pictures
	  #'(lambda (item1 item2)
	      ;; return true, iff item1 < item2
	      (let* ((item1
		      (mapcar #'read-from-string
			      (delimited-string-to-list
			       (pathname-name (car item1))
			       "-")))
		     (item2
		      (mapcar #'read-from-string
			      (delimited-string-to-list
			       (pathname-name (car item2))
			       "-"))))
		(do* ((x1 item1 (cdr x1))
		      (x2 item2 (cdr x2)))
		    ((or (null x1) (null x2)) nil)
		  (when (< (car x1) (car x2)) (return t))
		  (when (> (car x1) (car x2)) (return nil)))))))
  
  (when (not *quiet*)
    (format t "~%make html pages:")
    (force-output))
  (dolist (image '("home.gif" "next.gif" "previous.gif"))
    (sys:copy-file (merge-pathnames image "sys:")
		   (merge-pathnames image dest-dir)))
  (dolist (size sizes)
    (when (not *quiet*) (format t " ~a" size)(force-output))
    (do* ((pictures-info pictures (cdr pictures-info))
	  (current (car pictures-info) (car pictures-info))
	  (prev nil picture)
	  (picture (car current) (car current))
	  (next #1=(caar (cdr pictures-info)) #1#)
	  (htm (make-pathname :type "htm"))
	  page)
	((null picture))
      (setq page (merge-pathnames
		  htm
		  (merge-pathnames
		   (file-namestring picture)
		   (merge-pathnames (format nil "pages/~a/" size)
				    dest-dir))))
      (with-open-file (s page :direction :output)
	(make-page size
		   s
		   (format nil "../../~a/~a" size (file-namestring picture))
		   (when prev
		     (format nil "~a.htm" (pathname-name prev)))
		   (when next
		     (format nil "~a.htm" (pathname-name next)))
		   title
		   (remove size sizes)))))
  (when (not *quiet*) (format t "~%"))
  
  (when (not *quiet*) (format t "~%make index pages:"))
  (let ((pictures (mapcar #'car pictures)))
    (when (not *quiet*) (format t " index.htm")(force-output))
    (make-index :medium (merge-pathnames "index.htm" dest-dir)
		pictures title description)
    (when (not *quiet*) (format t " index_small.htm")(force-output))
    (make-index :small (merge-pathnames "index_small.htm" dest-dir)
		pictures title description)
    (when (not *quiet*) (format t " index_large.htm")(force-output))
    (make-index :large (merge-pathnames "index_large.htm" dest-dir)
		pictures title description))
  (when (not *quiet*) (format t "~%"))
  
  (when (not *quiet*) (format t "~%generate images:~%"))
  (setq i 1)
  (dolist (stuff pictures)
    (destructuring-bind (picture picture-info) stuff
      (when (not *quiet*)
	(format t "  [~d of ~d] ~a:" i npics (file-namestring picture))
	(format t " thumbnail")(force-output))
      (make-image
       :thumbnail picture-info picture
       (merge-pathnames (file-namestring picture)
			(merge-pathnames "thumbs/" dest-dir)))
      (when (not *quiet*) (format t " large")(force-output))
      (make-image
       :large picture-info picture
       (merge-pathnames (file-namestring picture)
			(merge-pathnames "large/" dest-dir)))
      (when (not *quiet*) (format t " medium")(force-output))
      (make-image
       :medium picture-info picture
       (merge-pathnames (file-namestring picture)
			(merge-pathnames "medium/" dest-dir)))
      (when (not *quiet*) (format t " small")(force-output))
      (make-image
       :small picture-info picture
       (merge-pathnames (file-namestring picture)
			(merge-pathnames "small/" dest-dir)))
      (when (not *quiet*) (format t "~%")))
    (incf i))

  (values))

(defun image-info (file)
  (let ((ed (parse-exif-data file)))
    (when (null ed) (error "No exif info in ~a!" file))
    `((:dimensions ,(exif-info-image-width ed)
		   ,(exif-info-image-length ed)))))

(defun make-image (type info from to)
  (let* ((dimensions
	  (or (cdr (assoc :dimensions info))
	      (error-die "Could not determine dimensions of ~a." from)))
	 (convert-command
	  (if* (eq :large type)
	     then ;; Only put a copyright notice on the large one, since
		  ;; that's the one that would be useful to someone wanting
		  ;; to steal one of my images (hey, it's possible!).
		  
		  (format nil "~
~aconvert ~
     -border 20x20 -bordercolor black ~
     -geometry \"~ax~a>\" ~
     -font ~a ~
     -pointsize 12 ~
     -quality 50 ~
     -fill white ~
     -draw \"text 3,9 '~a 2000 D. Kevin Layer'\" ~
     \"~a\" \"~a\""
			  *image-magick-root*
			  (truncate (first dimensions)
				    *large-divisor*)
			  (truncate (second dimensions)
				    *large-divisor*)
			  #+mswindows "C:/Winnt/fonts/arialbd.ttf"
			  #-mswindows "arialbd"
			  #+mswindows "\\0x00a9" ;; Unicode copyright symbol
			  #-mswindows "Copyright"
			  (forward-slashify (namestring from))
			  (forward-slashify (namestring to)))
	     else (let* ((landscape (> (first dimensions)
				       (second dimensions)))
			 (tn-max 100)
			 (tn-height
			  (if* landscape
			     then (round (* tn-max
					    (/ (second dimensions)
					       (first dimensions))))
			     else tn-max))
			 (tn-width
			  (if* landscape
			     then tn-max
			     else (round (* tn-max
					    (/ (first dimensions)
					       (second dimensions)))))))
		    (format nil
			    "~
~aconvert -quality 50 -geometry \"~ax~a>\" \"~a\" \"~a\""
			    *image-magick-root*
			    (if* (eq :thumbnail type)
			       then tn-width
			       else (truncate
				     (first dimensions)
				     (ecase type
				       (:medium *medium-divisor*)
				       (:small *small-divisor*))))
			    (if* (eq :thumbnail type)
			       then tn-height
			       else (truncate
				     (second dimensions)
				     (ecase type
				       (:medium *medium-divisor*)
				       (:small *small-divisor*))))
			    (forward-slashify (namestring from))
			    (forward-slashify (namestring to))))))
	 (status
	  (progn
	    (when *debug* (format t "~a~%~%" convert-command))
	    (run-shell-command convert-command :show-window :hide))))
    (when (/= 0 status)
      (error-die "Convert command failed on ~a with status ~d." from status))
    t))

(defun make-page (size s image previous-page next-page title other-sizes)
  (html-stream
   s
   (:head (:title (:princ-safe (file-namestring image))))
   :newline
   ((:body :bgcolor "#ffffff" :link "#ff0000" :vlink "#52188C")
    :newline
    ((:table :border "0" :cellpadding "5" :cellspacing "2" :width "100%"
	     :bgcolor "#f0f0f0")
     :newline
     (:tr
      :newline
      (:td 
       :newline
       (:h2 (:princ-safe
	     (format nil "~a/~a" title (file-namestring image)))))))
    :newline
    (:p
     :newline
     (:center
      :newline
      ((:table :border "0" :cellpadding "0" :cellspacing "2" :width "200")
       :newline
       (:tr
	:newline
	(when previous-page
	  (html
	   ((:td :width "80" :align "center")
	    :newline
	    ((:a :href (namestring previous-page))
	     ((:img :src "../../previous.gif" :height "30" :width "30"
		    :border "0" :alt "Previous"))))))
	:newline
	((:td :width "80" :align "center")
	 :newline
	 ((:a :href (format nil "../../index~a.htm"
			    (case size
			      (:small "_small")
			      (:medium "")
			      (:large "_large"))))
	  ((:img :src "../../home.gif" :height "30" :width "30"
		 :border "0" :alt "Home"))))
	:newline
	(when next-page
	  (html
	   ((:td :width "80" :align "center")
	    :newline
	    ((:a :href (namestring next-page))
	     :newline
	     ((:img :src "../../next.gif" :height "30" :width "30"
		    :border "0" :alt "Next"))))))))))
    
    (:p
     (:center
      ((:img :src image :border "0" :alt (file-namestring image)))))
    (:p
     (dolist (size other-sizes)
       (html
	((:a :href (format nil "../~a/~a.htm" size (pathname-name image)))
	 (:princ-safe (format nil "A ~a image." size)))
	:br))))))

(defun make-index (size index pictures title description)
  (with-open-file (s index :direction :output)
    (html-stream
     s
     (:head
      :newline
      (:title (:princ-safe title))
      :newline
      ((:body :bgcolor "#ffffff" :link "#ff0000" :vlink "#52188C")
       :newline
       ((:table :border "0" :cellpadding "5" :cellspacing "2"
		:width "100%" :bgcolor "#f0f0f0")
	:newline
	(:tr
	 :newline
	 (:td
	  :newline
	  (:h2 (:princ-safe title))
	  :newline
	  (:p (:princ-safe description)))))
       :newline
       (:center
	:newline
	((:table :cellspacing 10 :cellpadding 0 :border 0)
	 :newline
	 (dolist (row (chop-list pictures 3))
	   (make-index-row size row)))))))))

(defun make-index-row (size pictures)
  (html
   (:tr
    (dolist (picture pictures)
      (let ((picture (format nil "pages/~a/~a.htm" size
			     (pathname-name picture)))
	    (tn (format nil "thumbs/~a" (file-namestring picture)))
	    (name (pathname-name picture)))
	(html
	 ((:td :align "center")
	  ((:a :href picture) ((:img :src tn :border 0 :alt name))))
	 :newline))))
   :newline
   (:tr
    (dolist (picture pictures)
      (let ((picture (format nil "pages/~a/~a.htm" size
			     (pathname-name picture)))
	    (name (pathname-name picture)))
	(html
	 ((:td :align "center" :valign "top")
	  ((:a :href picture) ((:font :size "2") (:princ-safe name))))
	 :newline))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; utils which should be in allegro?????

(defun error-die (format &rest args)
  (if* *debug*
     then (apply #'break format args)
     else (apply #'format t format args)
	  (format t "~%")
	  (force-output)
	  (exit 0 :no-unwind t :quiet t)))

(defun my-read-line (stream &optional terminate-on-space)
  (let ((term-chars
	 `(#\return #\newline ,@(when terminate-on-space '(#\space))))
	(line '())
	(eof nil)
	c)
    (loop
      (setq c (read-char stream nil stream))
      (when (or (and (char= c stream) (setq eof t))
		(and (member c term-chars :test #'char=)
		     ;; read-char until no more term-chars or EOF
		     (loop
		       (setq c (peek-char nil stream nil stream))
		       (when (or (and (char= c stream) (setq eof t))
				 (not (member c term-chars :test #'char=)))
			 (return t))
		       ;; toss it
		       (read-char stream))))
	(return
	  (if* (and eof (null line))
	     then stream
	     else (concatenate 'simple-string (nreverse line)))))
      (push c line))))

(defun read-lines (stream)
  (let ((lines '())
	line)
    (loop
      (setq line (my-read-line stream))
      (when (eq stream line) (return (nreverse lines)))
      (push line lines))))

(defun my-delete-directory-and-files (directory)
  (handler-case (my-delete-directory-and-files-1 directory)
    (error (c) (error-die "~a: ~a" directory c))))

(defun my-delete-directory-and-files-1 (directory)
  (setq directory (pathname-as-directory (pathname directory)))
  (when (not (probe-file directory))
    (return-from my-delete-directory-and-files-1 nil))
  (when (not *quiet*) (format t "Removing ~a~%" directory))
  (let ((directories '()))
    (map-over-directory #'(lambda (p)
			    (if* (file-directory-p p)
			       then (push p directories)
			       else (force-delete-file p)))
			directory
			:include-directories t)
    (setq directories
      ;; Sort "longest directory first", which will allow them to be
      ;; removed in the correct order.
      (sort directories
	    #'(lambda (p1 p2)
		(> (length (pathname-directory p1))
		   (length (pathname-directory p2))))))
    (dolist (p directories)
      (ignore-errors (delete-directory p))))
  t)

(defun my-delete-directory (directory)
  (when (probe-file directory)
    (handler-case (delete-directory directory)
      (error-dir (c) (error "~a: ~a" directory c)))))

(defun force-delete-file (file)
  (let ((p (pathname file)))
    (when (aclwin:file-read-only-p p)
;;;; there should be a better way:
      (excl::filesys-chmod (namestring p) #o777))
    (delete-file p)))

(defun chop-list (list n)
  (do* ((l list (cdr l))
	(i 2 (1+ i))
	(tmp '())
	(res '()))
      ((null l)
       (when tmp (push (nreverse tmp) res))
       (nreverse res))
    (push (car l) tmp)
    (when (> i n)
      (setq i 1)
      (push (nreverse tmp) res)
      (setq tmp nil))))

(defun forward-slashify (string)
  (substitute #\/ #\\ string)) 
