;;; -*- mode: lisp -*-

;;; Time-stamp: <2009-12-20 21:57:13 tony>
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

;;; Need to move to CLS, since XARRAY and LISTOFLIST are there.
(addtest (lol-ut) lol-equalp
	 (ensure
	  (equalp (dataset (make-instance 'dataframe-array
					  :storage #2A(('a 'b)
						       ('c 'd))))
		  #2A(('a 'b)
		      ('c 'd)))))

(addtest (lol-ut) lol-same-size-1
	 (ensure
	  (let ((*mdfl-test* (list (list 'a 1 2.1)
				   (list 'b 2 1.1)
				   (list 'c 1 2.0)
				   (list 'd 2 3.0))))
	    (sublists-of-same-size-p *mdfl-test*))))

(addtest (lol-ut) lol-same-size-2
	 (ensure
	  (not
	   (let ((*mdfl-test2*
		  (list (list 'a 1 2.1)
			(list 'b 2 1.1)
			(list 'c 1 )
			(list 'd 2 3.0))))
	     (sublists-of-same-size-p *mdfl-test2*)))))


;;;
;; (run-tests)
;; #<Results for LOL-UT 5 Tests, 2 Failures, 1 Error>
;; (describe (run-tests))


