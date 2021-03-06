;;; -*- mode: lisp -*-

;;; Time-stamp: <2014-07-26 17:44:43 tony>
;;; Creation:   <2009-12-10 08:10:39 tony>
;;; File:       listoflist.asd
;;; Author:     AJ Rossini <blindglobe@gmail.com>
;;; Copyright:  (c)2009--, AJ Rossini.  See file LICENSE.mit in
;;;             top-level directory for information.
;;; Purpose:    ASDF definition for LISTOFLIST

;;; What is this talk of 'release'? Klingons do not make software
;;; 'releases'.  Our software 'escapes', leaving a bloody trail of
;;; designers and quality assurance people in its wake.

;;; This organization and structure is new to the 21st Century
;;; version..   Think, "21st Century Schizoid Man".

(in-package #:cl-user)

(defpackage #:listoflist-asd
  (:use :cl :asdf))

(in-package #:listoflist-asd)

(defsystem #:listoflist
  :description "makes listoflist an xarray'able data structure." 
  :author "AJ Rossini"
  :license "MIT"
  :depends-on (:xarray :clunit)
  :components
  ((:module
    "src"
    :pathname #p"src/"
    :components 
    ((:file "package")
     (:file "listoflist" :depends-on ("package"))))
   (:module
    "unittest"
    :pathname #p"src/"
    :depends-on ("src")
    :components
    ((:file "unittests")))))
