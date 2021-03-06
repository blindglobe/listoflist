;;; -*- mode: lisp -*-

;;; Time-stamp: <2014-07-26 17:48:23 tony>
;;; Creation:   <2008-09-08 08:06:30 tony>
;;; File:       listoflist.lisp
;;; Author:     AJ Rossini <blindglobe@gmail.com>
;;; Copyright:  (c) 2007-2013, AJ Rossini <blindglobe@gmail.com>.  BSD.
;;; Purpose:    Manipulating structures which are lists of lists
;;;             rather than arrays or matrix-likes.  Providing an
;;;             xarray interface to these structures

;;; What is this talk of 'release'? Klingons do not make software
;;; 'releases'.  Our software 'escapes', leaving a bloody trail of
;;; designers and quality assurance people in its wake.

(in-package :listoflist)

;;; LISTOFLIST is another useful xrefable object.  The interface does
;;; NOT map to CL functions in a straightforward manner, unlike
;;; arrays.  So it makes a good example of a non-standard underlying
;;; data structure that we'd like for XARRAY.

;;; Thoughts for organization: there are 2 general flavors of
;;; activities.  The first is that we do list-of-list to list-of-list
;;; transforms, and these do not rely on external packages being in
;;; existence.  The second is that we do transformations from them
;;; into other similar rectangular or ragged data structures.
;;; Within-structure should include item-selection and subsetting,
;;; while between-structure should include copying and ideally,
;;; "pass-through" referential structures.  The latter is probably
;;; going to take a performance hit, but should allow for maximal
;;; memory use.

;; Currently, we assume that the list-of-list representation is in
;; row-major form, i.e. that lists represent rows and not columns.
;; The original lisp-stat had the other way around.  We could augment
;; the top-level list with a property to check orientation
;; (row-major/column-major), but this hasn't been done yet.

;;; IN PROGRESS!

(defun listoflistp (lists  ;; &key (ragged T) (vartypes nil)
				)
  "Test for conformance of structure: list whose sublists are of the
same size (if ragged is T, then just test that list has elements of
type list).

FIXME: should have a flag to verify equivalence of types in case
needed to map to a RHO:DATA-FRAME.
"
 ;;  (declare (ignore ragged vartypes)
 ;;	   (type list lists))
  (and (sublists-of-same-size-p lists)
       T))

(defun transpose-listoflist (listoflist)
  "This function does the moral-equivalent of a matrix transpose on a
rectangular list-of-lists data structure.

Assumes listoflist is rectangular, need to see what happens if
ragged (or at least check).  Could use the listoflistp predicate to
confirm."
  (apply #'mapcar #'list listoflist))


(defun equal-listoflist (x y)
  "FIXME: This function, when written, should walk through 2 listoflists and
return T/nil based on equality."
  (and (= (list-length x) ;; top-level length same
	  (list-length y))
       ;; FIXME: within-level lengths same
       ;; FIXME: flattened values same, walking through
       ;; (loop over x and verify same tree as y)
       ))

(defun sublists-of-same-size-p (lists)
  "Take a list of list, and verify that the sublists are all of the same size.
returns size-of-sublist if all sublists same size, otherwise nil"
  (declare (type list lists))
  (if (apply #'= (map 'list #'length lists))
      (length (nth 0 lists))
      NIL))


(defun listsoflists-consistent-dimensions-p (&rest list-of-list-names)
  "Check if the lengths of the lists are equal (T, otherwise NIL), to
justify further processing and initial conditions."
  (if (< 0  (reduce #'(lambda (x y) (if  (= x y) y -1))
		    (mapcar #'length list-of-list-names)))
      T nil))



;;;; XARRAY INTERFACE

#|
 (defmethod xtype ((object list))
  ;; collect and rationalize all types into the most specific covering all.
  (loop for sublist in object
     collect (loop do i in (length sublist)
		collect (type-of (elt sublist 0))))
  )
|#


(defmethod xrank ((object list))
  "Basically, assuming coherently sized object, return number of
nested lists in first object."
  (length (xdims object)))

(defun listoflists-dimensions (lol)
  "We assume row-major, that is each sublist denotes a row, and columns are formed by taking the jth element from each list to form the jth column"
  (list (length lol)
	(length (map 'list #'(lambda (x) (declare (ignore x)) 1) lol))))

#|
 (defparameter *lol-1*  (list (list 1 2 3 4)
			     (list 5 6 7 8)))
 (map 'list #'list *lol-1*)
 (map 'list (lambda (x) x) *lol-1*)
 (listoflists-dimensions *lol-1*)



 (defun listoflists->matrix-like (lol &optional (coerce-to 'double-float))
  (let ((dims (loop lol counting number of subunits, first number is
		 number in inside, second is nil if they don't all
		 equal each other))
	)
    (let ((result (apply #'make-matrix dims))
	  )
      (loop-over-lol counting i j and setting value, and
	   (setf (mref result i j) value)))  )  )

 (defun matrix-like->listoflists ())
 (defun matrix-like->array ())
 (defun array->matrix-like ())
|#

(defmethod xdims ((object list))
#|
  (if (coherent-list-of-list-p object)
      (n-nested-lists object)
      nil)
|#
  )

#|
 (defmethod xdim ((object list) axis-number)
  (array-dimension object axis-number))

 (defmethod xsize ((object list))
  (array-total-size object))
|#

#|
 (defmethod xref-writable-p ((object list) &rest subscripts)
  "Lists always can be written to -- until we read-only it?!"
  (declare (ignore subscripts))
  t)
|#


(defmethod xref ((object list) &rest subscripts)
  (apply #'aref object subscripts))

(defmethod (setf xref) (value (object list) &rest subscripts)
  (setf (apply #'aref object subscripts) value))

;;;; HOW TO TREAT THE FOLLOWING FOR LOL data structures?  (I think we
;;;; move the work in lisp-matrix and cls to this package, or factor
;;;; out into a list-of-list package?  but it is only the xref'able
;;;; stuff we want?).


;;;; Convenience functions for vector and list construction.  All
;;;;  return simple-lists of the specified type, the versions with *
;;;;  use numeric-type-classifier.
#|
 (defun cvector-lol (element-type &rest elements)
  "Return a (simple-list element-type (*)) containing elements,
coerced to element-type."
  (let ((vector (make-list (length elements) :element-type element-type)))
    (fill-list-with-list vector elements)))
|#


(defun fill-array-with-list (array list)
  "Fills array with elements from list, coerced to the appropriate
  type.  No error checking, meant to be used internally.  Return array."
  (let ((i 0)
	(type (array-element-type array)))
    (dolist (l list)
      (setf (row-major-aref array i) (coerce l type))
      (incf i)))
  array)

(defun numeric-type-classifier (list)
  "Numeric type classifier, finds the smallest subtype that can
  accomodate the elements of list, in the ordering fixnum < integer <
  float < complex < t.  Rational, float (any kind) are classified as
  double-float, and complex numbers as (complex double-float).  Meant
  to be used by simple array-constructing functions.
  Upgraded-array-element-type is called on end result."
  (upgraded-array-element-type 
   (case (reduce #'max list
		 :key (lambda (x)
			(typecase x
			  (fixnum 0)
			  (integer 1)
			  ((or rational float) 2)
			  (complex 3)
			  (t 4))))
     (0 'fixnum)
     (1 'integer)
     (2 'double-float)
     (3 '(complex double-float))
     (4 t))))

(defun carray-lol (element-type dimensions &rest elements)
  "Return a (simple-array element-type dimensions) containing elements,
coerced to element-type."
  (unless (= (length elements) (reduce #'* dimensions))
    (error "incorrect number of elements provided"))
  (let ((vector (make-array dimensions :element-type element-type)))
    (fill-array-with-list vector elements)))

(defun cvector*-lol (&rest elements)
  "Return a (simple-array element-type (*)) containing elements,
coerced to element-type, where the elemen-type is obtained using
numeric-type-classifier."
  (apply #'cvector (numeric-type-classifier elements) elements))

(defun carray*-lol (dimensions &rest elements)
  "Return a (simple-array element-type dimensions) containing elements,
coerced to element-type, where the elemen-type is obtained using
numeric-type-classifier."
  (apply #'carray (numeric-type-classifier elements) dimensions elements))

(defun array->listoflist (arr &key (type 'rowmajor))
  "FIXME: need to write this."
  (declare (ignore arr type))
  (assert nil))


(defun listoflist->array (lol &key (type 'row-major))
  "From a listoflists structure, make an array.

FIXME: need to verify that the listoflists is a valid structure (same
size rows, typing if required, etc.

<example>
  (defparameter *mdfl-test*
      (list (list 'a 1 2.1)
            (list 'b 2 1.1)
            (list 'c 1 2.0)
            (list 'd 2 3.0)))
  (length *mdfl-test*)
  (length (elt *mdfl-test* 0))

  (defparameter *mdfl-test-dt* (make-datatable-from-listoflists *mdfl-test*))
  (array-dimensions *mdfl-test-dt*)
</example>"
  (let ((n (length lol))
	(p (length (elt lol 0))))
    (let ((result (make-array (list n p))))
      (dotimes (i n)
	(dotimes (j p)
	  (if (equal  type 'row-major)
	      (setf (aref result i j) (elt (elt lol i) j))
	      (setf (aref result i j) (elt (elt lol j) i)))))
      result)))
