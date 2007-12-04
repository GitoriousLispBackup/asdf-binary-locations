;;;-*- Mode: Lisp; Package: common-lisp-user -*-

#| simple-header

Author: Gary King

|#

(in-package #:common-lisp-user)
(defpackage #:asdf-binary-locations-system (:use #:asdf #:cl))
(in-package #:asdf-binary-locations-system)

(defclass load-only-file-mixin ()
  ())

(defclass load-only-cl-source-file (load-only-file-mixin cl-source-file)
  ())

(defmethod perform ((op compile-op) (component load-only-file-mixin))
  nil)

(defmethod perform ((op load-op) (component load-only-cl-source-file))
  (load (component-pathname component)))

(defsystem asdf-binary-locations
  :version "0.2.5"
  :author "Gary Warren King <gwking@metabang.com>"
  :maintainer "Gary Warren King <gwking@metabang.com>"
  :licence "MIT Style License"
  :description "Put compiled Lisp files in their places."
  
  :components ((:module 
                "dev"
                :components
		((:load-only-cl-source-file "main")
		 
		 (:static-file "notes.text")))
               
               (:module
                "website"
                :components
		((:module "source"
			  :components ((:static-file "index.lml")))))))
