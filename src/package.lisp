;;; -*- mode: lisp -*-

;;; Time-stamp: <2009-12-20 21:45:45 tony>
;;; Creation:   <2009-12-10 08:10:39 tony>
;;; File:       package.lisp
;;; Author:     AJ Rossini <blindglobe@gmail.com>
;;; Copyright:  (c)2009--, AJ Rossini.  See file LICENSE.mit in
;;;             top-level directory for information.
;;; Purpose:    package specification for LISTOFLIST

;;; What is this talk of 'release'? Klingons do not make software
;;; 'releases'.  Our software 'escapes', leaving a bloody trail of
;;; designers and quality assurance people in its wake.

;;; This organization and structure is new to the 21st Century
;;; version..   Think, "21st Century Schizoid Man".

(defpackage :listoflist
  (:documentation "XARRAY support for list-of-list data structures.")
  (:nicknames :lol)
  (:use :common-lisp
	:xarray)
  (:export
   ;; core functions
   listoflistp transpose-listoflist equal-listoflist
   sublists-of-same-size-p

   ;; xarray
   xeltype xtype xrank xdims xref-writeable-p xref listoflist->array ))

(defpackage :listoflist-user
  (:documentation "verification, sandbox, and illustration package for LISTOFLIST.")
  (:nicknames :lol-user)
  (:use :common-lisp
	:xarray
	:listoflist))

(defpackage :listoflist-unittests
  (:documentation "unit-testing structure for LISTOFLIST using LIFT.")
  (:nicknames :lol-unittests)
  (:use :common-lisp
	:xarray
	:listoflist
	:lift)
  (:export lol-ut))
