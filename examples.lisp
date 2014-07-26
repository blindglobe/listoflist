
(ql:quickload :listoflist)

(in-package :listoflist-user)

(defparameter *my-lol-1*
  '((0d0  1d0  2d0  3d0)
    (10d0 11d0 12d0 13d0)))


(defparameter *my-lol-2*
  (list (list 0d0  1d0  2d0  3d0)
	(list 10d0 11d0 12d0 13d0)))

(defparameter *my-ary-1*
  #2A((0d0 1d0 2d0 3d0)
      (10d0 11d0 12d0 13d0)))

(defparameter *mdfl-test*
  (list (list 'a 1 2.1)
	(list 'b 2 1.1)
	(list 'c 1 2.0)
	(list 'd 2 3.0)))

(defparameter *mdfl-test2*
  (list (list 'a 1 2.1)
	(list 'b 2 1.1)
	(list 'c 1 )
	(list 'd 2 3.0)))


(listoflistp *my-lol-1*)
(listoflistp *my-lol-2*)


(apply #'= (map 'list #'length *my-lol-1*))
(apply #'= (map 'list #'length *mdfl-test2*))

(sublists-of-same-size-p *my-lol-1*)
(sublists-of-same-size-p *mdfl-test*)
(sublists-of-same-size-p *mdfl-test2*)



 (defparameter LOL-2by3 (list (list 1 2) (list 3 4) (list 5 6)))
 (defparameter LOL-3by2 (list (list 1 3 5) (list 2 4 6)))
 (transpose-listoflists (transpose-listoflists LOL-2by3))
 (transpose-listoflists (transpose-listoflists LOL-3by2))

