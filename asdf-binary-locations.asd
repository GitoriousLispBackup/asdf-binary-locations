;;;-*- Mode: Lisp; Package: COMMON-LISP-USER -*-

#| simple-header

Author: Gary King

DISCUSSION

|#

(in-package :common-lisp-user)
(defpackage :asdf-binary-locations-system (:use #:asdf #:cl))
(in-package :asdf-binary-locations-system)

(defclass load-only-cl-source-file (cl-source-file)
  ())

(defmethod perform ((op compile-op) (component load-only-cl-source-file))
  nil)

(defmethod perform ((op load-op) (component load-only-cl-source-file))
  (load (component-pathname component)))

(defsystem asdf-binary-locations
  :version "0.1"
  :author "Gary Warren King <gwking@metabang.com>"
  :maintainer "Gary Warren King <gwking@metabang.com>"
  :licence "MIT Style License"
  :description "Put compiled Lisp files in their places."
  
  :components ((:module 
                "dev"
                :components ((:load-only-cl-source-file "main")
                             
                             (:static-file "notes.text")))))
