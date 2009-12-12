(defpackage #:listoflist-asd
  (:use :cl :asdf))

(in-package #:listoflist-asd)

(defsystem #:listoflist
  :description "makes listoflist an xarray'able data structure." 
  :author "AJ Rossini"
  :license "MIT"
  ;; :serial t
  :depends-on (:xarray :lift)
  :components
  ((:module
    "src"
    :pathname #p"src/"
    :components 
    ((:file "package")
     (:file "listoflist" :depends-on ("package")))) ; depends-on needed if we remove serial.
#|
   (:module
    "unittest"
    :pathname #p"src/"
    :depends-on "src"
    :components
    ((:file "unittests")))
|#
   ))
