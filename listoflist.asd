(defpackage #:listoflist-asd
  (:use :cl :asdf))

(in-package #:listoflist-asd)

(defsystem #:listoflist
  :description "makes listoflist an xarray'able data structure." 
  :author "AJ Rossini"
  :license "LLGPL"
  ;; :serial t
  :depends-on (:xarray :lift)
  :components ((:file "package")
	       (:file "listoflist" :depends-on ("package")))) ; depends-on needed if we remove serial.
