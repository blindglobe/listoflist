;;; -*- mode: lisp -*-

;;; Time-stamp: <2009-12-20 12:50:39 tony>
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
	       (10d0 11d0 12d0 13d0)))
   (my-lol-2 (list (list 0d0  1d0  2d0  3d0)
		   (list 10d0 11d0 12d0 13d0)))))

;;; Listoflist tests

(addtest (lol-ut) lol-consdata-1
	 (ensure 
	  (listoflistp my-lol-1)))

(addtest (lol-ut) lol-consdata-2
	 (ensure 
	  (listoflistp my-lol-2)))

(addtest (lol-ut) lol-equalp
	 (ensure
	  (equalp (dataset (make-instance 'dataframe-array
					  :storage #2A(('a 'b)
						       ('c 'd))))
		  #2A(('a 'b)
		      ('c 'd)))))



#|
;; Test cases:
 (and T T nil T)
 (and T T T)
 (defparameter *x1* (list 1 2 3))
 (defparameter *x2* (list 1 2 3))
 (defparameter *x3* (list 1 2 3 4))
 (defparameter *x4* (list 1 2 3))
 (reduce #'(lambda (x y)
	      (if (= x y) y -1))
	  (mapcar #'length (list *x1* *x2* *x3*)))
 (reduce #'(lambda (x y)
	      (if (= x y) y -1))  (list 2 3 2))
 (lists-of-same-size *x1* *x2* *x4*) ; => T
 (lists-of-same-size *x1* *x3* *x4*) ; => F
 (lists-of-same-size *x1* *x2* *x3*) ; => F
 (lists-of-same-size *x3* *x1* *x3*) ; => F
|#

;;;
;; (run-tests)
;; (describe (run-tests))

