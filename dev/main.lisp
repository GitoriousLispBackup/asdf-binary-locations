(in-package asdf)

;(defpackage asdf-binary-locations
;  (:use "COMMON-LISP" "ASDF"))
;(in-package asdf-binary-locations)

(export '(*system-configuration-paths*))

;;; ---------------------------------------------------------------------------
;;; this next bit of code stolen from SWANK / SLIME
;;; ---------------------------------------------------------------------------

(defparameter *implementation-features*
  '(:allegro :lispworks :sbcl :openmcl :cmu :clisp :ccl
    :corman :cormanlisp :armedbear :gcl :ecl))

(defparameter *os-features*
  '(:macosx :linux :windows :mswindows :win32 :solaris :darwin :sunos :unix :apple))

(defparameter *architecture-features*
  '(:powerpc :ppc :x86 :x86-64 :i686 :pc386 :iapx386 :sparc))

(defun lisp-version-string ()
  #+cmu       (substitute #\- #\/ (lisp-implementation-version))
  #+sbcl      (lisp-implementation-version)
  #+ecl       (lisp-implementation-version)
  #+gcl       (let ((s (lisp-implementation-version))) (subseq s 4))
  #+openmcl   (format nil "~d.~d"
                      ccl::*openmcl-major-version* 
                      ccl::*openmcl-minor-version*)
  #+lispworks (lisp-implementation-version)
  #+allegro   (concatenate 'string (if (eq 'h 'H) "A" "M") 
                           excl::*common-lisp-version-number*)
  #+clisp     (let ((s (lisp-implementation-version)))
                (subseq s 0 (position #\space s)))
  #+armedbear (lisp-implementation-version)
  #+cormanlisp (lisp-implementation-version)
  #+digitool   (subseq (lisp-implementation-version) 8))

(defparameter *implementation-specific-directory-name* nil)

(defun implementation-specific-directory-name ()
  "Return a name that can be used as a directory name that is
unique to a Lisp implementation, Lisp implementation version,
operating system, and hardware architecture."
  (or *implementation-specific-directory-name*
      (setf *implementation-specific-directory-name*
            (flet ((first-of (features)
                     (loop for f in features
                           when (find f *features*) return it))
                   (maybe-warn (value fstring &rest args)
                     (cond (value)
                           (t (apply #'warn fstring args)
                              "unknown"))))
              (let ((lisp (maybe-warn (first-of *implementation-features*)
                                      "No implementation feature found in ~a." 
                                      *implementation-features*))
                    (os   (maybe-warn (first-of *os-features*)
                                      "No os feature found in ~a." *os-features*))
                    (arch (maybe-warn (first-of *architecture-features*)
                                      "No architecture feature found in ~a."
                                      *architecture-features*))
                    (version (maybe-warn (lisp-version-string)
                                         "Don't know how to get Lisp ~
                                          implementation version.")))
                (format nil "~(~@{~a~^-~}~)" lisp version os arch))))))

;;; ---------------------------------------------------------------------------
;;; and this bit of code mostly stolen from Bjorn Lindberg
;;; see http://www.cliki.net/asdf%20binary%20locations
;;; and http://groups.google.com/group/comp.lang.lisp/msg/bd5ea9d2008ab9fd
;;; ---------------------------------------------------------------------------

(defvar *system-configuration-paths* 
  nil
  #+Example
  '(("/nfs/home/compbio/d95-bli/share/common-lisp/src/" 
     "/nfs/home/compbio/d95-bli/lib/common-lisp/cmucl/"))
  "The *system-configuration-paths* variable specifies mappings from source to target.
If the target is nil, then it means to not map the source to anything. I.e., to leave 
it as is. This has the effect of turning off ASDF-Binary-Locations for the given source
directory.") 

(defun pathname-prefix-p (prefix pathname) 
  (not (equal (enough-namestring pathname prefix) (namestring pathname)))) 

;; Instead of just returning the path when we don't find a mapping, we
;; stick stuff into the appropriate binary directory based on the implementation
;;
(defun determine-mapping (source possible-paths)
  (mapcar (lambda (path) 
	    (loop for (from to) in *system-configuration-paths* 
	       when (pathname-prefix-p from source) 
	       do (return 
		    (if to
			(merge-pathnames 
			 (make-pathname :type (pathname-type path)) 
			 (merge-pathnames (enough-namestring source from) 
					  to))
			path))
		 
	       finally (return 
			 (make-pathname 
			  :type (pathname-type path)
			  :directory (append (pathname-directory path)
					     (list (implementation-specific-directory-name)))
			  :defaults path)))) 
	  possible-paths))

(defmethod output-files :around ((operation compile-op) (c source-file)) 
  (let ((source (component-pathname c)) 
        (paths (call-next-method))) 
    (determine-mapping source paths)))

