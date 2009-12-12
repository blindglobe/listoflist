(defpackage #:listoflist-asd
  (:use :cl :asdf))

(in-package #:listoflist-asd)

(defsystem #:listoflist
  :description "makes listoflist an xarray'able data structure." 
  :author "AJ Rossini"
  :license "LLGPL"
  :serial t
  :components ((:file "package")
	       (:file "listoflist"))
  :depends-on (:xarray))
