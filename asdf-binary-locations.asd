;;;-*- Mode: Lisp; Package: COMMON-LISP-USER -*-

#| simple-header

Author: Gary King

DISCUSSION

|#

(in-package :common-lisp-user)
(defpackage :asdf-binary-locations-system (:use #:asdf #:cl))
(in-package :asdf-binary-locations-system)

(defsystem asdf-binary-locations
  :version "0.1"
  :author "Gary Warren King <gwking@metabang.com>"
  :maintainer "Gary Warren King <gwking@metabang.com>"
  :licence "MIT Style License"

  :components ((:module "dev"
                        :components ((:file "asdf-binary-locations")
                                     
                                     (:static-file "notes.text")))))