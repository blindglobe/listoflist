;;; -*- mode: lisp -*-

;;; Time-stamp: <2009-12-10 08:15:19 tony>
;;; Creation:   <2009-12-10 08:10:39 tony>
;;; File:       package.lisp
;;; Author:     AJ Rossini <blindglobe@gmail.com>
;;; Copyright:  (c)2009--, AJ Rossini.  Currently licensed under MIT
;;;             license.  See file LICENSE.mit in top-level directory
;;;             for information.
;;; Purpose:    package specification for LISTOFLIST

;;; What is this talk of 'release'? Klingons do not make software
;;; 'releases'.  Our software 'escapes', leaving a bloody trail of
;;; designers and quality assurance people in its wake.

;;; This organization and structure is new to the 21st Century
;;; version..   Think, "21st Century Schizoid Man".

(defpackage :listoflist
  (:documentation "xarray support for listoflist data structures.")
  (:use :common-lisp
	:xarray)
  (:export xref ))

(defpackage :listoflist-user
  (:documentation "xarray support for listoflist data structures.")
  (:nicknames :lol-user)
  (:use :common-lisp
	:xarray
	:listoflist))
;; no exports, just for verification, illustration, and tinkering

(defpackage :listoflist-test
  (:documentation "unit-testing structure for LISTOFLIST.")
  (:nicknames :lol-user)
  (:use :common-lisp
	:xarray
	:listoflist
	:lift))
