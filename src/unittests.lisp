;;; -*- mode: lisp -*-

;;; Time-stamp: <2009-12-12 08:00:43 tony>
;;; Creation:   <2009-04-15 08:43:02 tony>
;;; File:       unittests-listoflist.lisp
;;; Author:     AJ Rossini <blindglobe@gmail.com>
;;; Copyright:  (c)2009--, AJ Rossini.  MIT License.  See LICENSE.mit in top 
;;; Purpose:    unittests in LIFT for the listoflist handling.

;;; What is this talk of 'release'? Klingons do not make software
;;; 'releases'.  Our software 'escapes', leaving a bloody trail of
;;; designers and quality assurance people in its wake.

(in-package :listoflist-unittests)

(deftestsuite lol-ut ()
  ((my-lol-1 '((0d0  1d0  2d0  3d0)
	       (10d0 11d0 12d0 13d0)))))

;;; Listoflist tests

(addtest (lol-ut) lol-equalp
	 (ensure
	  (equalp (dataset (make-instance 'dataframe-array
					  :storage #2A(('a 'b)
						       ('c 'd))))
		  #2A(('a 'b)
		      ('c 'd)))))

(addtest (lol-ut) lol-consdata
	 (ensure 
	  (consistent-dataframe-p my-df-1)))

;;;
;; (run-tests)
;; (describe (run-tests))
