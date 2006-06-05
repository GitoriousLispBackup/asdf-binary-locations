;;; ---------------------------------------------------------------------------
;;; and this bit of code mostly stolen from Bjorn Lindberg
;;; see http://www.cliki.net/asdf%20binary%20locations
;;; and http://groups.google.com/group/comp.lang.lisp/msg/bd5ea9d2008ab9fd
;;; ---------------------------------------------------------------------------
;;; Portions of this code are from SWANK / SLIME

(in-package #:asdf)

(export '(*source-to-target-mappings*
          *default-toplevel-directory*
          *centralize-lisp-binaries*
          output-files-for-system-and-operation))

(defparameter *centralize-lisp-binaries*
  nil
  "If true, compiled lisp files without an explicit mapping (see *source-to-target-mappings*) will be placed in subdirectories of *default-toplevel-directory*. If false, then compiled lisp files without an explicitly mapping will be placed in subdirectories of their sources.")

(defparameter *default-toplevel-directory*
  (merge-pathnames
   (make-pathname :directory '(:relative ".fasls"))
   (user-homedir-pathname))
  "If *centralize-lisp-binaries* is true, then compiled lisp files without an explicit mapping \(see *source-to-target-mappings*\) will be placed in subdirectories of *default-toplevel-directory*.")

(defvar *source-to-target-mappings* 
  nil
  #+Example
  '(("/nfs/home/compbio/d95-bli/share/common-lisp/src/" 
     "/nfs/home/compbio/d95-bli/lib/common-lisp/cmucl/"))
  "The *source-to-target-mappings* variable specifies mappings from source to target.
If the target is nil, then it means to not map the source to anything. I.e., to leave 
it as is. This has the effect of turning off ASDF-Binary-Locations for the given source
directory.")

;; obsolete variable check
(when (boundp '*system-configuration-paths*)
  (warn "The variable *system-configuration-paths* has been renamed to *source-to-target-mappings*. Please update your configuration files.")
  (setf *source-to-target-mappings* (symbol-value '*system-configuration-paths*)))


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

(defun pathname-prefix-p (prefix pathname) 
  (not (equal (enough-namestring pathname prefix) (namestring pathname)))) 

(defgeneric output-files-for-system-and-operation
  (system operation component source possible-paths)
  (:documentation "Returns the directory where the componets output files should be placed. This may depends on the system, the operation and the component. The ASDF default input and outputs are provided in the source and possible-paths parameters."))

(defmethod output-files-for-system-and-operation
           ((system system) operation component source possible-paths)
  (declare (ignore operation component))
  (output-files-using-mappings
   source possible-paths *source-to-target-mappings*))

(defgeneric output-files-using-mappings (source possible-paths path-mappings)
  (:documentation "Use the variable *system-configuration-mappings* to find an output path for the source. The algorithm transforms each entry in possible-paths as follows: If there is a mapping whose source starts with the path of possible-path, then replace possible-path with a pathname that starts with the target of the mapping and continues with the rest of possible-path. If no such mapping is found, then use the default mapping. 

If *centralize-lisp-binaries* is false, then the default mapping is to place the output in a subdirectory of the source. The subdirectory is named using the Lisp implementation \(see implementation-specific-directory-name\). If *centralize-lisp-binaries* is true, then the default mapping is to place the output in subdirectories of *default-toplevel-directory* where the subdirectory structure will mirror that of the source."))

(defmethod output-files-using-mappings (source possible-paths path-mappings)
  (mapcar (lambda (path) 
	    (loop for (from to) in path-mappings 
	          when (pathname-prefix-p from source) 
	          do (return 
		      (if to
			(merge-pathnames 
			 (make-pathname :type (pathname-type path)) 
			 (merge-pathnames (enough-namestring source from) 
					  to))
			path))
		  
	          finally (return 
			   ;; Instead of just returning the path when we 
                           ;; don't find a mapping, we stick stuff into 
                           ;; the appropriate binary directory based on 
                           ;; the implementation
                           (if *centralize-lisp-binaries*
                             (merge-pathnames
                              (make-pathname
                               :type (pathname-type path)
                               :directory `(:relative
                                            ,(implementation-specific-directory-name)
                                            ,@(rest (pathname-directory path)))
                               :defaults path)
                              *default-toplevel-directory*)
                             (make-pathname 
			      :type (pathname-type path)
			      :directory (append
                                          (pathname-directory path)
                                          (list (implementation-specific-directory-name)))
			      :defaults path))))) 
	  possible-paths))

(defmethod output-files :around ((operation compile-op) (component source-file)) 
  (let ((source (component-pathname component )) 
        (paths (call-next-method))) 
    (output-files-for-system-and-operation 
     (component-system component) operation component source paths)))

