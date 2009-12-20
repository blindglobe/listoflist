;;; -*- mode: lisp -*-

;;; Time-stamp: <2009-12-12 08:33:24 tony>
;;; Creation:   <2009-12-10 08:10:39 tony>
;;; File:       TODO.lisp
;;; Author:     AJ Rossini <blindglobe@gmail.com>
;;; Copyright:  (c)2009--, AJ Rossini.  See file LICENSE.mit in
;;;             top-level directory for information.
;;; Purpose:    development support and short-term memory. 

;;; What is this talk of 'release'? Klingons do not make software
;;; 'releases'.  Our software 'escapes', leaving a bloody trail of
;;; designers and quality assurance people in its wake.

;;; This organization and structure is new to the 21st Century
;;; version..   Think, "21st Century Schizoid Man".

;;(asdf:oos 'asdf:compile-op 'listoflist :force t)
;;(asdf:oos 'asdf:load-op 'listoflist)

(in-package :lisp-stat-unittests)

;; tests = 2, failures = 0, errors = 2
(run-tests :suite 'lol-ut)
(describe (run-tests :suite 'lol-ut))


;;;; 
(in-package :lol-user)

(listoflistp '((0d0  1d0  2d0  3d0)
	       (10d0 11d0 12d0 13d0)))
(listoflistp (list (list 0d0  1d0  2d0  3d0)
		   (list 10d0 11d0 12d0 13d0)))



