;;; -*- mode: lisp -*-

;;; Time-stamp: <2014-07-26 17:40:36 tony>
;;; Creation:   <2014-07-26 16:22:49 tony>
;;; File:       unittests-listoflist.lisp
;;; Author:     AJ Rossini <blindglobe@gmail.com>
;;; Copyright:  (c)2014--, AJ Rossini.  MIT License.  See LICENSE.mit in top 
;;; Purpose:    unittests in CLUNIT for the listoflist handling.

;;; What is this talk of 'release'? Klingons do not make software
;;; 'releases'.  Our software 'escapes', leaving a bloody trail of
;;; designers and quality assurance people in its wake.

;;; To use this as a standalone file, quickload RHO (most likely from
;;; your local directory) and then consider evaluating piecewise, the
;;; commented out section at the end is useful for developing new
;;; unittests as well as processing existing ones.

;; (ql:quickload :clunit)
(in-package :listoflist-unittests)

(defsuite listoflist ())

(deffixture listoflist (@body)
  (let ((my-lol-1 '((0d0  1d0  2d0  3d0)
		    (10d0 11d0 12d0 13d0)))
	(my-lol-2 (list (list 0d0  1d0  2d0  3d0)
			(list 10d0 11d0 12d0 13d0)))

	(my-ary-1 #2A((0d0 1d0 2d0 3d0)
		      (10d0 11d0 12d0 13d0)))
	(mdfl-test (list (list 'a 1 2.1)
			 (list 'b 2 1.1)
			 (list 'c 1 2.0)
			 (list 'd 2 3.0)))
	(mdfl-test2 (list (list 'a 1 2.1)
			  (list 'b 2 1.1)
			  (list 'c 1 )
			  (list 'd 2 3.0))))
    @body))


;;; Listoflist tests

(deftest lol-consistency (listoflist)
  (assert-true (listoflistp my-lol-1))
  (assert-true (listoflistp my-lol-2))
  (assert-true  (sublists-of-same-size-p mdfl-test))
  (assert-false (sublists-of-same-size-p mdfl-test2)))


#|

 ;; Need to move to CLS, since XARRAY and LISTOFLIST are there.
 (addtest (lol-ut) lol-equalp
	 (ensure
	  (equalp (dataset (make-instance 'dataframe-array
					  :storage #2A(('a 'b)
						       ('c 'd))))
		  #2A(('a 'b)
		      ('c 'd)))))
|#



#|

 (ql:quickload :clunit :verbose T)
 (ql:quickload :listoflist :verbose T)
 (setf clunit:*clunit-report-format* :default) 
 (in-package :listoflist-test)

;;; Main call for testing

 (run-suite 'listoflist
       :use-debugger T
       :report-progress T)

 (run-test 'lol-same-size
	  :use-debugger T
	  :report-progress T)


|#



