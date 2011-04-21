;;; -*- mode: lisp -*-

;;; Time-stamp: <2010-07-01 08:26:36 tony>
;;; Creation:   <2009-03-12 17:14:56 tony>
;;; File:       transpose.lisp
;;; Author:     AJ Rossini <blindglobe@gmail.com>
;;; Copyright:  (c)2010--, AJ Rossini.  Currently licensed under MIT
;;;             license.  See file LICENSE.mit in top-level directory
;;;             for information.

;;; Purpose:    examples from transposing in the list-of-list
;;;             array-like structure.

;;; What is this talk of 'release'? Klingons do not make software
;;; 'releases'.  Our software 'escapes', leaving a bloody trail of
;;; designers and quality assurance people in its wake.

(in-package :listoflist)

#|
Hello, recently I saw this code on the maxima list for computing the
transpose of a matrix,
can you do it better (less computing time)?  How this speed compares
to C?

 B. Willis version:
|#

(defun transpose-wb (ll)
  (let ((acc nil))
    (loop while (car ll) do
      (push (mapcar #'car ll) acc)
      (setq ll (mapcar #'cdr ll)))
    (nreverse acc)))
#|
---------------------------------------------------

My version. This version is destructive and avoid consing.
|#

(defvar l1 nil)
(defvar l2 nil)

(defun transpose-pg(m)
  (let (m1)
    (setq m1 (loop for x on (car m) collect x))
    (transposeaux m)
    m1))

(defun transposeaux(ll)
  (let (next)
    (loop for l1 in ll
       for l2 in (cdr ll) do
           (loop for ai = l1 then next
              for bi on l2 do
                (setf next (cdr ai))
                (rplacd ai bi))
         finally
           (loop for bi = l2 then next
                while (cdr bi) do
                (setf next (cdr bi)
                      (cdr bi) nil)))
    ll))

(defun a-matrix(i j)
  "construct a rectangular matrix"
  (loop repeat i collect
       (loop repeat j collect (random 20))))

(defun transpose-wb (ll)
  (let ((acc nil))
    (loop while (car ll) do
      (push (mapcar #'car ll) acc)
      (setq ll (mapcar #'cdr ll)))
    (nreverse acc)))

#|
Now a benchmark:

CL-USER> (progn (setq l2 (copy-tree (setq l1 (a-matrix 1000 1000))))
nil)
NIL
CL-USER> (time (progn (transpose-wb l1) 'done))
Evaluation took:
  0.184 seconds of real time
  0.190000 seconds of total run time (0.130000 user, 0.060000 system)
  [ Run times consist of 0.140 seconds GC time, and 0.050 seconds non-
GC time. ]
  103.26% CPU
  431,324,341 processor cycles
  32,046,400 bytes consed

DONE
CL-USER> (time (progn (transpose-pg l2) 'done))
Evaluation took:
  0.028 seconds of real time
  0.030000 seconds of total run time (0.020000 user, 0.010000 system)
  107.14% CPU
  67,007,521 processor cycles
  16,384 bytes consed


From: "jos...@corporate-world.lisp.de" <jos...@lisp.de>
Date: Tue, 29 Jun 2010 05:46:39 -0700 (PDT)
Local: Tues, Jun 29 2010 2:46 pm
Subject: Re: transpose of a matrix

On 29 Jun., 14:23, pero <perogarridogo...@yahoo.es> wrote:

a style remark:

> (defvar l1 nil)
> (defvar l2 nil)

L1 and L2 are now defined to be SPECIAL (dynamic variables).

...

> (defun transposeaux(ll)
>   (let (next)
>     (loop for l1 in ll
>        for l2 in (cdr ll) do

Here L1 and L2 are local variables introduced by LOOP.
Unfortunately both are also SPECIAL variables, because
of the DEFVAR above.

Because of that there is a convention to
write SPECIAL variables as
*l1* and *l2*. That way your local variables
can be named l1 and l2 and will not be special.

Often one may not see a difference. Sometimes
it introduces hard to find bugs and it
even may have performance implications in some
cases...

		

From: Marc Mientki <mien...@nonet.com>
Date: Tue, 29 Jun 2010 15:10:01 +0200
Local: Tues, Jun 29 2010 3:10 pm
Subject: Re: transpose of a matrix

Am 29.06.2010 14:23, schrieb pero:

> Hello, recently I saw this code on the maxima list for computing the
> transpose of a matrix,
> can you do it better (less computing time)?  How this speed compares
> to C?

>   B. Willis version:

> (defun transpose-wb (ll)
>    (let ((acc nil))
>      (loop while (car ll) do
>        (push (mapcar #'car ll) acc)
>        (setq ll (mapcar #'cdr ll)))
>      (nreverse acc)))

And how about this version?
|#

(defun transpose-m (matrix)
   (apply 'mapcar 'list matrix))
#|
regards
Marc


From: Norbert_Paul <norbertpauls_spam...@yahoo.com>
Date: Tue, 29 Jun 2010 15:22:46 +0200
Local: Tues, Jun 29 2010 3:22 pm
Subject: Re: transpose of a matrix


pero wrote:
> Hello, recently I saw this code on the maxima list for computing the
> transpose of a matrix,
> can you do it better (less computing time)?  How this speed compares
> to C?

>   B. Willis version:

> (defun transpose-wb (ll)
>    (let ((acc nil))
>      (loop while (car ll) do
>        (push (mapcar #'car ll) acc)
>        (setq ll (mapcar #'cdr ll)))
>      (nreverse acc)))

I'd avoid (mapcar #'cdr ll) and use map-into:
Something like:
...
   (let ((acc nil) (ll (copy-list ll))) ; conses only once
...
      (setf ll (map-into ll #'cdr ll))  ; re-use conses.
...

gc now recycles only n conses, not (roughly) n^2
(not tested)

Norbert


From: p...@informatimago.com (Pascal J. Bourguignon)
Date: Tue, 29 Jun 2010 16:30:38 +0200
Local: Tues, Jun 29 2010 4:30 pm
Subject: Re: transpose of a matrix

pero <perogarridogo...@yahoo.es> writes:
> Hello, recently I saw this code on the maxima list for computing the
> transpose of a matrix,
> can you do it better (less computing time)?  How this speed compares
> to C?

You are talking of matrices, not of list of lists, therefore I will
boldly ignore the code you've pasted and show you how to transpose a
matrix in O(1):
|#

(defun transpose-matrix (matrix)
  (lambda (i j &rest args)
     (apply matrix j i args)))


;; That is, with the right representation.

(defun make-matrix (r c)
  (let ((storage (make-array (list r c))))
    (lambda (i j &optional (new-value nil setp))
       (if setp
          (setf (aref storage i j) new-value)
          (aref storage i j)))))

(let ((m (make-matrix 2 2)))
  (funcall m 0 0  3)
  (funcall m 0 1  2)
  (funcall m 1 0 -2)
  (funcall m 1 1  3)
  (flet ((print-matrix (m)
            (loop for i from 0 to 1 do
               (terpri)
               (loop for j from 0 to 1 do
                  (prin1 (funcall m i j))  (princ " ")))
            (terpri)))
    (print-matrix m)
    (print-matrix (transpose-matrix m))))

#|

3 2
-2 3

3 -2
2 3

-- 
__Pascal Bourguignon__                     http://www.informatimago.com/
	
		
From: "Thomas M. Hermann" <tmh.pub...@gmail.com>
Date: Tue, 29 Jun 2010 08:42:37 -0700 (PDT)

Subject: Re: transpose of a matrix

On Jun 29, 9:30 am, p...@informatimago.com (Pascal J. Bourguignon)
wrote:

> You are talking of matrices, not of list of lists, therefore I will
> boldly ignore the code you've pasted and show you how to transpose a
> matrix in O(1):

I, too, will boldly ignore the original code and paste something that
may or may not be apropos. I needed something to do while I was
waiting on my utility company to verify my email address.
|#

(defun transpose-matrix (matrix)
  "Transpose the matrix represented by a 2D array."
  (if (= 2 (array-rank matrix))
      (destructuring-bind (numrows numcols)
          (array-dimensions matrix)
        (let ((new-matrix (make-array (list numcols numrows)
                                      :element-type
                                      (array-element-type matrix))))
          (dotimes (i0 numrows new-matrix)
            (dotimes (i1 numcols)
              (setf (aref new-matrix i1 i0) (aref matrix i0 i1))))))
      (error "The matrix is not a 2D array.")))

#|
Hope that helps or at least confuses,

~ Tom


From: pero <perogarridogo...@yahoo.es>
Date: Tue, 29 Jun 2010 13:05:47 -0700 (PDT)
Subject: Re: transpose of a matrix

On 29 jun, 17:42, "Thomas M. Hermann" <tmh.pub...@gmail.com> wrote:

> On Jun 29, 9:30 am, p...@informatimago.com (Pascal J. Bourguignon)
> wrote:

> > You are talking of matrices, not of list of lists, therefore I will
> > boldly ignore the code you've pasted and show you how to transpose a
> > matrix in O(1):

> I, too, will boldly ignore the original code and paste something that
> may or may not be apropos. I needed something to do while I was
> waiting on my utility company to verify my email address.

> (defun transpose-matrix (matrix)
>   "Transpose the matrix represented by a 2D array."
>   (if (= 2 (array-rank matrix))
>       (destructuring-bind (numrows numcols)
>           (array-dimensions matrix)
>         (let ((new-matrix (make-array (list numcols numrows)
>                                       :element-type
>                                       (array-element-type matrix))))
>           (dotimes (i0 numrows new-matrix)
>             (dotimes (i1 numcols)
>               (setf (aref new-matrix i1 i0) (aref matrix i0 i1))))))
>       (error "The matrix is not a 2D array.")))

> Hope that helps or at least confuses,

> ~ Tom

You boldly lose: 0.050 > 0.028, better continue waiting for email.

CL-USER> (progn (setq m1 (make-array (list 1000 1000))) nil)
NIL
CL-USER> (progn (time (transpose-matrix m1)) nil)
Evaluation took:
  0.050 seconds of real time
  0.050000 seconds of total run time (0.050000 user, 0.000000 system)
  100.00% CPU
  117,341,497 processor cycles
  8,000,016 bytes consed

NIL
		
From: Raffael Cavallaro <raffaelcavall...@pas.despam.s.il.vous.plait.mac.com>
Date: Tue, 29 Jun 2010 16:33:21 -0400
Subject: Re: transpose of a matrix

On 2010-06-29 16:05:47 -0400, pero said:

> You boldly lose: 0.050 > 0.028, better continue waiting for email.

You've boldly missed the point of the previous posts entirely.

Common Lisp has an array data type. Anyone interested in speed,
especially with larger matrices, is going to use arrays, not lists for
matrix code since array element access is O(1) and list element access
is O(n). Their code operates on this array data type; yours does not.
In addition, as Rainer pointed out, your code uses special variable
names as locals!

Faulty code is still faulty, even if fast on toy examples.
Specifically, your array representation (i.e., a list) will show poor
performance for anything that requires random access (as opposed to
merely sequential access) of array elements:

? (let* ((list-matrix (loop for i below 10000 collect i)))
    (time (loop repeat 10000
            for index = (random 10000)
            always (= (nth index list-matrix) index))))
(LOOP REPEAT 10000 FOR INDEX = (RANDOM 10000) ALWAYS (= (NTH INDEX
LIST-MATRIX) INDEX)) took 301 milliseconds (0.301 seconds) to run
                    with 2 available CPU cores.
During that period, 304 milliseconds (0.304 seconds) were spent in user mode
                    0 milliseconds (0.000 seconds) were spent in system mode
T
? (let* ((real-matrix (make-array 10000 :element-type 'fixnum)))
    (loop for i below 10000 do (setf (aref real-matrix i) i))
    (time (loop repeat 10000
            for index = (random 10000)
            always (= (aref real-matrix index) index))))
(LOOP REPEAT 10000 FOR INDEX = (RANDOM 10000) ALWAYS (= (AREF
REAL-MATRIX INDEX) INDEX)) took 0 milliseconds (0.000 seconds) to run
                    with 2 available CPU cores.
During that period, 0 milliseconds (0.000 seconds) were spent in user mode
                    1 milliseconds (0.001 seconds) were spent in system mode
T

warmest regards,

Ralph

-- 
Raffael Cavallaro


From: pero <perogarridogo...@yahoo.es>
Date: Tue, 29 Jun 2010 14:17:01 -0700 (PDT)
Subject: Re: transpose of a matrix

On 29 jun, 22:33, Raffael Cavallaro

- Hide quoted text -
- Show quoted text -
<raffaelcavall...@pas.despam.s.il.vous.plait.mac.com> wrote:
> On 2010-06-29 16:05:47 -0400, pero said:

> > You boldly lose: 0.050 > 0.028, better continue waiting for email.

> You've boldly missed the point of the previous posts entirely.

> Common Lisp has an array data type. Anyone interested in speed,
> especially with larger matrices, is going to use arrays, not lists for
> matrix code since array element access is O(1) and list element access
> is O(n). Their code operates on this array data type; yours does not.
> In addition, as Rainer pointed out, your code uses special variable
> names as locals!

> Faulty code is still faulty, even if fast on toy examples.
> Specifically, your array representation (i.e., a list) will show poor
> performance for anything that requires random access (as opposed to
> merely sequential access) of array elements:

> ? (let* ((list-matrix (loop for i below 10000 collect i)))
>     (time (loop repeat 10000
>             for index = (random 10000)
>             always (= (nth index list-matrix) index))))
> (LOOP REPEAT 10000 FOR INDEX = (RANDOM 10000) ALWAYS (= (NTH INDEX
> LIST-MATRIX) INDEX)) took 301 milliseconds (0.301 seconds) to run
>                     with 2 available CPU cores.
> During that period, 304 milliseconds (0.304 seconds) were spent in user mode
>                     0 milliseconds (0.000 seconds) were spent in system mode
> T
> ? (let* ((real-matrix (make-array 10000 :element-type 'fixnum)))
>     (loop for i below 10000 do (setf (aref real-matrix i) i))
>     (time (loop repeat 10000
>             for index = (random 10000)
>             always (= (aref real-matrix index) index))))
> (LOOP REPEAT 10000 FOR INDEX = (RANDOM 10000) ALWAYS (= (AREF
> REAL-MATRIX INDEX) INDEX)) took 0 milliseconds (0.000 seconds) to run
>                     with 2 available CPU cores.
> During that period, 0 milliseconds (0.000 seconds) were spent in user mode
>                     1 milliseconds (0.001 seconds) were spent in system mode
> T


Well, now I ask: Can you find a way to calculate the transpose of a
matrix (2d-array) that is faster than the algorithm I gave for
calculating the transpose a list of lists that contains the same data?

 Do you really think that a person that can write this kind of code
doesn't know the difference between a list and an array? really?

	
From: Raffael Cavallaro <raffaelcavall...@pas.despam.s.il.vous.plait.mac.com>
Date: Tue, 29 Jun 2010 19:38:48 -0400
Subject: Re: transpose of a matrix

On 2010-06-29 17:17:01 -0400, pero said:

> Well, now I ask: Can you find a way to calculate the transpose of a
> matrix (2d-array) that is faster than the algorithm I gave for
> calculating the transpose a list of lists that contains the same data?

A destructive matrix transpose which optimizes for array element type:
|#


(defun 2d-matrix-transpose! (matrix)
  (declare (type (array fixnum (* *)) matrix))
  (let* ((dimensions (array-dimensions matrix))
         (rows (first dimensions))
         (cols (second dimensions)))
    (loop for i from 0 below rows do
      (loop for j from  (1+ i) below cols
        do
        (rotatef (aref matrix j i) (aref matrix i j))))
    matrix))

#|
runs in the same time your tortured list of lists transpose does, while
random access for your list of lists matrix is 2 orders of magnitude
(100x) slower!

If I'm using arrays for their intended O(1) random access, why would I
possibly care how fast a list representation with its O(n) access can
be transposed?

>  Do you really think that a person that can write this kind of code
> doesn't know the difference between a list and an array? really?

I think that a person that would bother to write such code (i.e.,
representing large arrays as lists) might not have a good grasp of why
people interested in performance don't use lists to represent arrays,
yes.
warmest regards,

Ralph

-- 
Raffael Cavallaro

		
On 6/29/10 7:38 PM, Raffael Cavallaro wrote:

> runs in the same time your tortured list of lists transpose does, while
> random access for your list of lists matrix is 2 orders of magnitude
> (100x) slower!

> If I'm using arrays for their intended O(1) random access, why would I
> possibly care how fast a list representation with its O(n) access can be
> transposed?

Perhaps we should put this all back into perspective.  The original
question concerned maxima, which represents matrices as lists of lists.

You may think this stupid, but consider that maxima is a SYMBOLIC math
system, so large symbolic matrices quickly become unusable and the cost
of matrix operations is probably swamped with the work needed to
simplify the terms resulting from the matrix operation.



From: Raffael Cavallaro <raffaelcavall...@pas.despam.s.il.vous.plait.mac.com>
Date: Tue, 29 Jun 2010 21:51:16 -0400
Local: Wed, Jun 30 2010 3:51 am
Subject: Re: transpose of a matrix
Reply | Reply to author | Forward | Print | Individual message | Show original | Report this message | Find messages by this author
On 2010-06-29 20:47:55 -0400, Raymond Toy said:

> Perhaps we should put this all back into perspective.  The original
> question concerned maxima, which represents matrices as lists of lists.

> You may think this stupid,

No, I don't; I think crowing about the speed of a particular operation
on such an otherwise sub-optimal representation is silly.

>  but consider that maxima is a SYMBOLIC math
> system, so large symbolic matrices quickly become unusable and the cost
> of matrix operations is probably swamped with the work needed to
> simplify the terms resulting from the matrix operation.

As you say, this representation was chosen in maxima for ease of
symbolic manipulation, not for raw speed, so, optimizing matrix
transpose is fairly pointless here.

warmest regards,

Ralph

-- 
Raffael Cavallaro


From: pero <perogarridogo...@yahoo.es>
Date: Wed, 30 Jun 2010 02:59:43 -0700 (PDT)
Local: Wed, Jun 30 2010 11:59 am
Subject: Re: transpose of a matrix
Reply | Reply to author | Forward | Print | Individual message | Show original | Report this message | Find messages by this author
On 30 jun, 01:38, Raffael Cavallaro

- Hide quoted text -
- Show quoted text -
<raffaelcavall...@pas.despam.s.il.vous.plait.mac.com> wrote:
> On 2010-06-29 17:17:01 -0400, pero said:

> > Well, now I ask: Can you find a way to calculate the transpose of a
> > matrix (2d-array) that is faster than the algorithm I gave for
> > calculating the transpose a list of lists that contains the same data?

> A destructive matrix transpose which optimizes for array element type:

> (defun 2d-matrix-transpose! (matrix)
>   (declare (type (array fixnum (* *)) matrix))
>   (let* ((dimensions (array-dimensions matrix))
>          (rows (first dimensions))
>          (cols (second dimensions)))
>     (loop for i from 0 below rows do
>       (loop for j from  (1+ i) below cols
>         do
>         (rotatef (aref matrix j i) (aref matrix i j))))
>     matrix))

> runs in the same time your tortured list of lists transpose does, while
> random access for your list of lists matrix is 2 orders of magnitude
> (100x) slower!

> If I'm using arrays for their intended O(1) random access, why would I
> possibly care how fast a list representation with its O(n) access can
> be transposed?

> >  Do you really think that a person that can write this kind of code
> > doesn't know the difference between a list and an array? really?

> I think that a person that would bother to write such code (i.e.,
> representing large arrays as lists) might not have a good grasp of why
> people interested in performance don't use lists to represent arrays,
> yes.

> warmest regards,

> Ralph

> --
> Raffael Cavallaro

Sorry Raffael, but you lose: 0.40 > 0.28, also your transpose-rc is
only for array of fixnum, and this is a severe restriction.

(defun transpose-rc (matrix)
  (declare (type (array fixnum (* *)) matrix))
  (let* ((dimensions (array-dimensions matrix))
         (rows (first dimensions))
         (cols (second dimensions)))
    (loop for i from 0 below rows do
      (loop for j from  (1+ i) below cols

CL-USER> (defparameter l1 (make-array (list 1000 1000) :element-type
'fixnum :initial-element 0))

CL-USER> (progn (time (transpose-rc l1)) 'done)
Evaluation took:
  0.040 seconds of real time
  0.040000 seconds of total run time (0.040000 user, 0.000000 system)
  100.00% CPU
  92,120,462 processor cycles
  0 bytes consed

DONE

 Well, I think my hidden motivation for this post was about football,
as I am not a big fan of football but here is Spain there is a
football fever. I think I have got an "Optimization Fever" (tm).
Anyway, I am glad I have beaten every attempt done by Lispers  to
improve the time of this operation in sbcl.


From: Raymond Toy <toy.raym...@gmail.com>
Date: Wed, 30 Jun 2010 09:31:11 -0400
Local: Wed, Jun 30 2010 3:31 pm
Subject: Re: transpose of a matrix
Reply | Reply to author | Forward | Print | Individual message | Show original | Report this message | Find messages by this author
On 6/29/10 9:51 PM, Raffael Cavallaro wrote:

- Hide quoted text -
- Show quoted text -
> On 2010-06-29 20:47:55 -0400, Raymond Toy said:

>> Perhaps we should put this all back into perspective.  The original
>> question concerned maxima, which represents matrices as lists of lists.

>> You may think this stupid,

> No, I don't; I think crowing about the speed of a particular operation
> on such an otherwise sub-optimal representation is silly.

>>  but consider that maxima is a SYMBOLIC math
>> system, so large symbolic matrices quickly become unusable and the cost
>> of matrix operations is probably swamped with the work needed to
>> simplify the terms resulting from the matrix operation.

> As you say, this representation was chosen in maxima for ease of
> symbolic manipulation, not for raw speed, so, optimizing matrix
> transpose is fairly pointless here.

But aren't we all guilty of premature optimization sometimes? :-)

Sometimes it's just fun to do.

Ray


From: Raffael Cavallaro <raffaelcavall...@pas.despam.s.il.vous.plait.mac.com>
Date: Wed, 30 Jun 2010 15:44:11 -0700
Local: Thurs, Jul 1 2010 12:44 am
Subject: Re: transpose of a matrix
Reply | Reply to author | Forward | Print | Individual message | Show original | Report this message | Find messages by this author
On 2010-06-30 02:59:43 -0700, pero said:

> Anyway, I am glad I have beaten every attempt done by Lispers  to
> improve the time of this operation in sbcl.

Again, no one cares because you've microoptimized one operation on a
matrix - transpose - at the expense of random access which is the
primary reason arrays exist. To repeat, the random access for list of
list matrices of the size you test is 2 orders of magnitude - 100 times
slower!

In the implementation I use, CCL, the in-place array transpose runs in
the exact same time as your highly convoluted list of lists
implementation.

To summarize:

* You've gained no speed advantage in transpose
* Your code is much less clear
* Your random access times, the whole reason arrays exist in the first
place, are slower by 100 times

The only thing you've triumphed over here is common sense. At least
Spain actually beat Portugal.

warmest regards,

Ralph

-- 
Raffael Cavallaro

|#
