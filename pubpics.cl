;; Web publisher for digital photos, written by Kevin Layer.
;; This code is in the public domain.  You may do with it what you
;; want.
;;
;; What exactly does it do?  It takes as input a set of jpg's from a
;; digital camera and produces nice looking web pages, with index pages
;; containing thumbnails.  The original digital pictures are resized into
;; three sets of pictures, allowing viewers to sequentially or randomly
;; browse in any size.  The largest images can optionally have a copyright
;; notice added to them.
;;
;; Required software:
;; - Allegro CL: probably any version will do, but I used ACL 6.0.
;;   http://www.franz.com
;; - ImageMagick 5.2.7: other later versions may work, but I'm pretty sure
;;   some earlier versions have problems.  It works on Windows (at least
;;   2000) and Linux (at least RedHat 6.1).
;;   http://www.imagemagick.org
;;
;; This program works on Windows (at least 2000) and Linux (at least RedHat
;; 6.1).
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; $Id$

(in-package :user)

(eval-when (compile)
  ;; An experimental run-shell-command scheduler that allows multiple,
  ;; concurrent run-shell-command's, in order to take advantage of multiple
  ;; processors.  Because ImageMagik takes so much memory, this doesn't
  ;; turn out to save a lot of time (unless you have LOTS of RAM).
  #+ignore (push :rsc-scheduler *features*)
  
  ;;(push :debug-pubpics *features*)
  
  (compile-file-if-needed
   (or (probe-file "exif-utils/exifinfo.cl")
       #+mswindows (probe-file "c:/src/exif-utils/exifinfo.cl")
       (error "Could not find exifinfo.cl"))))

(eval-when (compile eval load)
  (require :exifinfo
	   (or (probe-file "exif-utils/exifinfo.fasl")
	       #+mswindows (probe-file "c:/src/exif-utils/exifinfo.fasl")))
  (use-package :util.exif)
  (require :aserve
	   (or (probe-file "sys:aserve;aserve.fasl")
	       (probe-file "sys:code;aserve.fasl")))
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
    "~
Usage: [-p] [-c name] [-n size] [-V] [-q] [-t title] [-d description]
       source-dir dest-dir
-c name        - for the largest image size, add copyright in a border for
                 `name' 
-d description - addition information for the index page
-n size        - max number of images on each index page--probably should
                 be a multiple of 3, since there are three thumbnails per
                 row on each index page
-p             - pause after completion
-V             - print version info and exit
-q             - quiet mode--do not print informative messages
-r             - recurse on source-dir
-t title       - set title of generated pages to `title', and use this
                 title at the top of each photo page
source-dir     - directory containing images
dest-dir       - non-existent directory for web pages
")

(defvar *image-magick-root*
    #+mswindows "c:/Program Files/ImageMagick-5.4.9-Q16/"
    #-mswindows "/usr/bin/X11/")

(defvar *large-divisor* 1.5 "How much `large' images are scaled down.")
(defvar *medium-divisor* 2  "How much `medium' images are scaled down.")
(defvar *small-divisor* 3   "How much `small' images are scaled down.")
(defvar .cleanup-forms. nil)

(defun pubpics-init-function ()
  ;; The following makes run-shell-command interruptable.  Why, I have no
  ;; idea.  It appears that /bin/csh fixes something in how our signal
  ;; handling works.
  #+linux (setf (sys:getenv "SHELL") "/bin/csh")
  (flet ((doit ()
	   (sys:with-command-line-arguments
	       ("c:d:fI:n:pqrt:V"
		copyright description force-flag image-file index-size
		pause *quiet* recurse title print-version-and-exit)
	       (rest)
	     (declare (ignore image-file))
	     (when print-version-and-exit
	       (format t "pubpics: ~a~%" *version*)
	       (exit 0 :quiet t))
	     (when (/= 2 (length rest)) (error *usage*))
	     #+rsc-scheduler (setq *quiet* t)
	     (pubpics (first rest) (second rest) :force-flag force-flag
		      :title title :description description
		      :copyright copyright
		      :recurse recurse
		      :index-size (when index-size
				    (read-from-string index-size)))
	     #+rsc-scheduler (rsc-finalize)
	     (when pause
	       (format t "hit ENTER to continue:")
	       (force-output)
	       (read-line))
	     (exit 0 :quiet t))))
    #+debug-pubpics (doit)
    #-debug-pubpics
    (handler-case
	(doit)
      (error (c)
	(format t "~a~%" c)
	(dolist (form .cleanup-forms.) (funcall form c))
	(exit 1 :quiet t)))))

(defun pubpics (source-dir dest-dir
		&key force-flag title description copyright index-size recurse
		&aux (pictures '())
		     (sizes '(:small :medium :large))
		     i npics)
  (setq source-dir (pathname-as-directory (pathname source-dir)))
  (setq dest-dir (pathname-as-directory (pathname dest-dir)))
  
  (when (not (probe-file source-dir))
    (error "~a does not exist." source-dir))

  ;; Make sure dest-dir doesn't exist, and remove it if it does and
  ;; force-flag is non-nil.
  (when (probe-file dest-dir)
    (if* force-flag
       then (when (not (file-directory-p dest-dir))
	      (error "~a is not a directory." dest-dir))
	    (delete-directory-and-files dest-dir :force t)
       else (error "~a already exists." dest-dir)))

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

  (let ((cleanup-form
	 #'(lambda (&optional condition)
	    (format t "~&~@[~a~%~]Cleaning up...~%" condition)
	    (force-output)
	    (when (probe-file dest-dir)
	      (delete-directory-and-files dest-dir :force t)))))
    (push cleanup-form .cleanup-forms.)
    (add-signal-handler
     2
     #'(lambda (sig cont)
	 (format t "In SIGINT cleanup handler...~%")
	 (force-output)
	 (dolist (form .cleanup-forms.) (funcall form))
	 (excl::sig-handler-exit sig cont))))

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
   :recurse recurse)
  (when (= i 0)
    (error "There are no .jpg (or .JPG) files in ~a." source-dir))
  (setq npics i)
  (when (not *quiet*) (format t " ~d pictures~%" i))
  
  ;; Put the pictures in "numerical" order.
  (setq pictures
    (handler-case
	(let ((sample-file (pathname-name (car (car pictures)))))
	  (sort (copy-list pictures)
		(if* (date-based-filename-p sample-file)
		   then #'date-based-filename-sort-function
		 elseif (numbered-filename-p sample-file)
		   then #'numbered-filename-sort-function
		   else (error "can't figure out naming scheme"))))
      (error (c)
	;; they might not have the new names, so don't sort if an error
	;; occurs.
	(format t "Error sorting files; ~a" c)
	pictures)))
  
  (when (not *quiet*)
    (format t "~%make html pages:")
    (force-output))
  (dolist (image '("home.gif" "next.gif" "previous.gif" "blank.gif"))
    (sys:copy-file
     (if* (probe-file image)
	thenret
	else (merge-pathnames image "sys:"))
     (merge-pathnames image dest-dir)))
  (dolist (size sizes)
    (when (not *quiet*) (format t " ~a" size)(force-output))
    (do* ((pictures-info pictures (cdr pictures-info))
	  (current (car pictures-info) (car pictures-info))
	  (prev nil picture)
	  (picture (car current) (car current))
	  (next #1=(caar (cdr pictures-info)) #1#)
	  (htm (make-pathname :type "htm"))
	  (image-number 1 (1+ image-number))
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
		   image-number
		   index-size
		   (when prev (format nil "~a.htm" (pathname-name prev)))
		   (when next (format nil "~a.htm" (pathname-name next)))
		   title
		   (remove size sizes)))))
  (when (not *quiet*) (format t "~%"))
  
  (when (not *quiet*) (format t "~%make index pages:"))
  (let ((pictures (mapcar #'car pictures)))
    (when (not *quiet*) (format t " index.htm")(force-output))
    (make-index :medium "index" dest-dir
		pictures title description index-size)
    (when (not *quiet*) (format t " index_small.htm")(force-output))
    (make-index :small "index_small" dest-dir
		pictures title description index-size)
    (when (not *quiet*) (format t " index_large.htm")(force-output))
    (make-index :large "index_large" dest-dir
		pictures title description index-size))
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
			(merge-pathnames "large/" dest-dir))
       :copyright copyright)
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

(defun date-based-filename-p (filename)
  (match-regexp
   (load-time-value (compile-regexp "^[0-9]+-[0-9]+"))
   filename
   :return nil))

(defun numbered-filename-p (filename)
  (match-regexp
   (load-time-value (compile-regexp "^[a-zA-Z]+[0-9]+"))
   filename
   :return nil))

(defun date-based-filename-sort-function (item1 item2)
  ;; return true, iff item1 < item2
  (let* ((item1 (mapcar #'read-from-string
			(delimited-string-to-list
			 (pathname-name (car item1))
			 "-")))
	 (item2 (mapcar #'read-from-string
			(delimited-string-to-list
			 (pathname-name (car item2))
			 "-"))))
    (do* ((x1 item1 (cdr x1))
	  (x2 item2 (cdr x2)))
	((or (null x1) (null x2)) nil)
      (when (< (car x1) (car x2)) (return t))
      (when (> (car x1) (car x2)) (return nil)))))

(defun numbered-filename-sort-function (item1 item2)
  ;; return true, iff item1 < item2
  (let* ((re (load-time-value (compile-regexp "[-a-zA-Z_]+\\([0-9]+\\)")))
	 (n1 (multiple-value-bind (found ignore i)
		 (match-regexp re (pathname-name (car item1)))
	       (declare (ignore ignore))
	       (when (not found) (error "can't parse filename1"))
	       (read-from-string i)))
	 (n2 (multiple-value-bind (found ignore i)
		 (match-regexp re (pathname-name (car item2)))
	       (declare (ignore ignore))
	       (when (not found) (error "can't parse filename2"))
	       (read-from-string i))))
    (< n1 n2)))

(defun image-info (file)
  (let ((ed (parse-exif-data file)))
    (when (null ed) (error "No exif info in ~a!" file))
    `((:dimensions ,(exif-info-image-width ed)
		   ,(exif-info-image-length ed)))))

(defun make-image (type info from to &key copyright)
  (let* ((dimensions
	  (or (cdr (assoc :dimensions info))
	      (error "Could not determine dimensions of ~a." from)))
	 (convert-command
	  ;; The -size argument causes the convert command to operate
	  ;; significantly faster on the thumbnail, medium and small
	  ;; images.
	  (if* (eq :large type)
	     then ;; Only put a copyright notice on the large one, since
		  ;; that's the one that would be useful to someone wanting
		  ;; to steal one of my images (hey, it's possible!).
		  (let ((x (truncate (first dimensions) *large-divisor*))
			(y (truncate (second dimensions) *large-divisor*)))
		    (with-output-to-string (s)
		      (format s "~
~aconvert ~
     -size ~ax~a ~
     -geometry \"~ax~a>\" ~
     -quality 50 "
			      *image-magick-root*
			      x y x y)
		      (when copyright
			(format
			 s "~
     -border 20x20 -bordercolor black ~
     -pointsize 12 ~
     -fill white ~
     -font ~a ~
     -draw \"text 3,9 '~a ~d ~a'\" "
			 #+mswindows "C:/Winnt/fonts/arialbd.ttf"
			 #-mswindows "arialbd"
			 #+mswindows "\\0x00a9" ;; Unicode copyright symbol
			 #-mswindows "Copyright"
			 (nth-value 5 (decode-universal-time
				       (get-universal-time)))
			 copyright))
		      (format s "\"~a\" \"~a\""
			      (forward-slashify (namestring from))
			      (forward-slashify (namestring to)))))
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
					       (second dimensions))))))
			 (x (if* (eq :thumbnail type)
			       then tn-width
			       else (truncate
				     (first dimensions)
				     (ecase type
				       (:medium *medium-divisor*)
				       (:small *small-divisor*)))))
			 (y (if* (eq :thumbnail type)
			       then tn-height
			       else (truncate
				     (second dimensions)
				     (ecase type
				       (:medium *medium-divisor*)
				       (:small *small-divisor*))))))
		    (format nil
			    "~
~aconvert -quality 50 -size ~ax~a -geometry \"~ax~a>\" \"~a\" \"~a\""
			    *image-magick-root* x y x y
			    (forward-slashify (namestring from))
			    (forward-slashify (namestring to))))))
	 (status
	  (progn
	    (when *debug* (format t "~a~%~%" convert-command))
	    #+rsc-scheduler (rsc-scheduler convert-command)
	    #-rsc-scheduler
	    (run-shell-command convert-command :show-window :hide))))
    #+rsc-scheduler
    status
    #-rsc-scheduler
    (when (/= 0 status)
      (error "Convert command failed on ~a with status ~d."
	     (file-namestring from) status))
    t))

(defun make-page (size s image image-number index-size previous-page
		  next-page title other-sizes)
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
       ((:a :href
	    (forward-slashify
	     (namestring
	      (merge-pathnames
	       (page-number-to-index image-number index-size size)
	       "../../"))))
	((:img :src "../../home.gif" :height "30" :width "30"
	       :border "0" :alt "Home"))))
      :newline
      (:td
       :newline
       (if* previous-page
	 then (html
	       ((:a :href (forward-slashify (namestring previous-page)))
		((:img :src "../../previous.gif" :height "30" :width "30"
		       :border "0" :alt "Previous"))))
	  else (html
		((:img :src "../../blank.gif" :height "30" :width "30"
		       :border "0" :alt "")))))
      :newline
      (:td
       :newline
       (if* next-page
	  then (html
		((:a :href (forward-slashify (namestring next-page)))
		 :newline
		 ((:img :src "../../next.gif" :height "30" :width "30"
			:border "0" :alt "Next"))))
	  else (html
		((:img :src "../../blank.gif" :height "30" :width "30"
		       :border "0" :alt "")))))
      :newline
      (:td 
       :newline
       (:h2
	(:princ-safe (format nil "~a (~a) " title (file-namestring image)))
	
	(dolist (size other-sizes)
	  (html
	   " / "
	   ((:a :href (format nil "../~a/~a.htm" size (pathname-name image)))
	    (:princ-safe (format nil "~a image" size)))))))))
    :newline
    (:center
     :newline
     ((:img :src image :border "0" :alt (file-namestring image)))))))

(defun make-index (size index-name dest-dir pictures title description
		   index-size)
  (let* ((n-pictures (length pictures))
	 (n-pages (ceiling n-pictures index-size)))
    (flet
	((gen-index (s size title description pictures
		     &optional indicies prev-index next-index)
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
;;;; the index buttons:
	      (when indicies
		(html
		 (:p
		  :newline
		  (:center
		   :newline
		   ((:table :border "0" :cellpadding "0" :cellspacing "2")
		    :newline
		    (:tr
		     :newline
		     ((:td :colspan "2" :align "center")
		      (do* ((index indicies (cdr index))
			    (i 1 (1+ i)))
			  ((null index))
			(html ((:a :href (car index)) (:princ i)))
			(when (cdr index)
			  (html " | ")))))
		    :newline
		    (:tr
		     :newline
		     (if* prev-index
			then (html
			      ((:td :align "right"  :width "50%")
			       :newline
			       ((:a :href (file-namestring prev-index))
				((:img :src "previous.gif"
				       :height "30" :width "30"
				       :border "0"
				       :alt "Previous index page")))))
			else (html ((:td :width "50%") " ")))
		     :newline
		     (if* next-index
			then (html
			      ((:td :align "left"  :width "50%")
			       :newline
			       ((:a :href (file-namestring next-index))
				:newline
				((:img :src "next.gif" :height "30" :width "30"
				       :border "0" :alt "Next index page")))))
			else (html ((:td :width "50%") " ")))))))))
	     
;;;; the thumbnails:
	      :newline
	      (:center
	       :newline
	       ((:table :cellspacing 10 :cellpadding 0 :border 0)
		:newline
		(dolist (row (chop-list pictures 3))
		  (make-index-row size row))))))))
	 (index (index-name page dest-dir)
	   (merge-pathnames
	    (merge-pathnames
	     (if* (= page 1)
		then index-name
		else (format nil "~a~a" index-name page))
	     (make-pathname :type "htm"))
	    dest-dir)))
      (if* (and index-size (> n-pages 1))
	 then (do* ((indicies
		     (do* ((res '())
			   (page n-pages (1- page)))
			 ((= page 0) res)
		       (push (file-namestring
			      (index index-name page dest-dir))
			     res)))
		    (page 1 (1+ page))
		    (prev-index nil index)
		    (index #1=(index index-name page dest-dir)
			   #1#)
		    (next-index
		     #2=(when (< page n-pages)
			  (index index-name (1+ page) dest-dir))
		     #2#))
		  ((> page n-pages))
		(with-open-file (s index :direction :output)
		  (gen-index s size title description
			     (list-section pictures page index-size)
			     indicies
			     prev-index next-index)))
	 else (with-open-file (s (index index-name 1 dest-dir)
			       :direction :output)
		(gen-index s size title description pictures))))))

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

(defun page-number-to-index (image-number index-size size)
  (let ((suffix (case size
		  (:small "_small")
		  (:medium "")
		  (:large "_large"))))
    (forward-slashify
     (namestring
      (merge-pathnames
       (make-pathname :type "htm")
       (if* index-size
	  then (let ((page-number
		      (1+ (truncate (1- image-number) index-size))))
		 (format nil "index~a~a"
			 suffix
			 (if* (= page-number 1)
			    then ""
			    else page-number)))
	  else (format nil "index~a" suffix)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; utils

(defun list-section (list section-index section-size)
  ;; LIST is composed of SECTION-SIZE segments of elements.  Return the
  ;; SECTION-INDEX subsequence of list elements.
  (let* ((start (* (1- section-index) section-size))
	 (end (+ start section-size)))
    (subseq list start end)))

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
  #+mswindows (substitute #\/ #\\ string)
  #-mswindows string)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; An experimental run-shell-command scheduler.

#+rsc-scheduler
(defparameter *rsc-number-of-processors* 3)

#+rsc-scheduler
(defparameter *rsc-status*
    ;; nil or (status . command)
    nil)

#+rsc-scheduler
(defparameter *rsc-procs* nil)

#+rsc-scheduler
(defun rsc-scheduler (command)
  (when (null *rsc-procs*) (rsc-initialize-processors))
  
  (mp:without-scheduling
    (let ((p (pop *rsc-procs*)))
      (setq *rsc-procs* (append *rsc-procs* (list p)))
      (push command (getf (mp:process-property-list p) :rsc-queue)))))

#+rsc-scheduler
(defun rsc-initialize-processors ()
  (setq *rsc-procs* nil)
  (setq *rsc-status* nil)
  (dotimes (i *rsc-number-of-processors*)
    (push
     (mp:process-run-function (format nil "rsc processor ~d" i)
			      #'rsc-process-queue)
     *rsc-procs*)))

#+rsc-scheduler
(defun rsc-process-queue (&aux (p mp:*current-process*))
  ;; our queue is stored on our plist
  (loop
    (mp:process-wait
     "waiting for something to do"
     (lambda ()
       (or *rsc-status*
	   (getf (mp:process-property-list p) :rsc-queue))))

    (when *rsc-status*
      (mp:process-kill p)
      (sleep 100000))

    (let* ((command (pop (getf (mp:process-property-list p) :rsc-queue)))
	   status)
      (when (eq :die command)
	(mp:process-kill p)
	(sleep 100000))
      (multiple-value-bind (ignore1 ignore2 pid)
	  (run-shell-command command :show-window :hide :wait nil)
	(declare (ignore ignore1 ignore2))
	(format t "~a: ~a~%" (mp:process-name p) command)
	(mp:process-wait
	 (format nil "waiting for pid ~d to finish" pid)
	 (lambda (pid)
	   (multiple-value-bind (exit-status xpid)
	       (sys:reap-os-subprocess :wait nil :pid pid)
	     (if* (not (eql pid xpid))
		then ;; something is wrong, the process isn't running
		     (setq status (or exit-status -1))
		     t
	      elseif exit-status
		then ;; process finished
		     (setq status exit-status)
		     t
		else ;; still running
		     nil)))
	 pid)
	(when (/= 0 status) (setq *rsc-status* (cons status command)))))))

#+rsc-scheduler
(defun rsc-finalize ()
  (mp:without-scheduling
    (dolist (p *rsc-procs*)
      (setf (getf (mp:process-property-list p) :rsc-queue)
	(append (getf (mp:process-property-list p) :rsc-queue)
		(list :die)))))
  (mp:process-wait
   "waiting for rsc processors to finish"
   (lambda ()
     (dolist (p *rsc-procs* t)
       (when (mp:process-active-p p) (return nil)))))
  (if* *rsc-status*
     then (format t "Error command ~s exited with status ~d."
		  (cdr *rsc-status*) (car *rsc-status*))
	  nil
     else t)
  (setq *rsc-procs* nil))
