;; Spork Factory Copyright (C) 2012 David Griffiths
;; 
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU Affero General Public License as
;; published by the Free Software Foundation, either version 3 of the
;; License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU Affero General Public License for more details.
;;
;; You should have received a copy of the GNU Affero General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

(define (square x) 
  (* x x))

(define (vx v) (vector-ref v 0))
(define (vy v) (vector-ref v 1))
(define (vz v) (vector-ref v 2))

(define (vadd a b)
  (vector (+ (vx a) (vx b)) 
          (+ (vy a) (vy b))
          (+ (vz a) (vz b)))) 

(define (vmag v)
  (sqrt (+ (square (vx v))
           (square (vy v))
           (square (vz v)))))
         
(define (vsub a b)
  (vector (- (vx a) (vx b)) 
          (- (vy a) (vy b))
          (- (vz a) (vz b)))) 

(define (vmul v a)
  (vector (* (vx v) a) (* (vy v) a) (* (vz v) a))) 

(define (vdiv v a)
  (vector (/ (vx v) a) (/ (vy v) a) (/ (vz v) a))) 

(define (vdist a b)
  (vmag (vsub a b)))

(define (vlerp v1 v2 t)
	(vadd v1 (vmul (vsub v2 v1) t)))

(define (mscale v)
  (vector (vx v) 0 0 0 
          0 (vy v) 0 0 
          0 0 (vz v) 0 
          0 0 0 1))

; 0  1  2  3 
; 4  5  6  7
; 8  9 10 11
;12 13 14 15

(define (mtranspose m)
  (vector (vector-ref m 0) (vector-ref m 4) (vector-ref m 8) (vector-ref m 12)
          (vector-ref m 1) (vector-ref m 5) (vector-ref m 9) (vector-ref m 13)
          (vector-ref m 2) (vector-ref m 6) (vector-ref m 10) (vector-ref m 14)
          (vector-ref m 3) (vector-ref m 7) (vector-ref m 11) (vector-ref m 15)))


(define (vtransform v m)
  (let ((m m));(mtranspose m)))
    (let ((w (+ (* (vx v) (vector-ref m 3)) 
                (* (vy v) (vector-ref m 7)) 
                (* (vz v) (vector-ref m 11)) 
                (vector-ref m 15))))
   (vdiv
    (vector
     (+ (* (vx v) (vector-ref m 0)) 
        (* (vy v) (vector-ref m 4))
        (* (vz v) (vector-ref m 8))
        (vector-ref m 12))
     (+ (* (vx v) (vector-ref m 1))
        (* (vy v) (vector-ref m 5))
        (* (vz v) (vector-ref m 9))
        (vector-ref m 13))
     (+ (* (vx v) (vector-ref m 2)) 
        (* (vy v) (vector-ref m 6))
        (* (vz v) (vector-ref m 10))
        (vector-ref m 14)))
    w))))

;------------------------------------------------------------

(define random-maker
  (let* ((multiplier 48271)
         (modulus 2147483647)
         (apply-congruence
          (lambda (current-seed)
            (let ((candidate (modulo (* current-seed multiplier)
                                     modulus)))
              (if (zero? candidate)
                  modulus
                  candidate))))
         (coerce
          (lambda (proposed-seed)
            (if (integer? proposed-seed)
                (- modulus (modulo proposed-seed modulus))
                19860617))))  ;; an arbitrarily chosen birthday
  (lambda (initial-seed)
    (let ((seed (coerce initial-seed)))
      (lambda args
        (cond ((null? args)
               (set! seed (apply-congruence seed))
               (/ (- modulus seed) modulus))
              ((null? (cdr args))
               (let* ((proposed-top
                       (ceiling (abs (car args))))
                      (exact-top
                       (if (inexact? proposed-top)
                           (inexact->exact proposed-top)
                           proposed-top))
                      (top
                       (if (zero? exact-top)
                           1
                           exact-top)))
                 (set! seed (apply-congruence seed))
                 (inexact->exact (floor (* top (/ seed modulus))))))
              ((eq? (cadr args) 'reset)
               (set! seed (coerce (car args))))
              (else
               (display "random: unrecognized message")
               (newline))))))))

(define random
  (random-maker 19781116))  ;; another arbitrarily chosen birthday

(define rndf random)

(define (rndvec) (vector (rndf) (rndf) (rndf)))
   
(define (crndf)
  (* (- (rndf) 0.5) 2))

(define (crndvec)
  (vector (crndf) (crndf) (crndf)))

(define (srndvec)
  (let loop ((v (crndvec)))
    (if (> (vmag v) 1) ; todo: use non sqrt version
        (loop (crndvec))
        v)))

(define (hsrndvec)
  (let loop ((v (crndvec)))
    (let ((l (vmag v)))
      (if (or (> l 1) (eq? l 0))
          (loop (crndvec))
          (vdiv v l)))))

(define (grndf)
  (let loop ((x (crndf)) (y (crndf)))
    (let ((l (+ (* x x) (* y y))))
      (if (or (>= l 1) (eq? l 0))
          (loop (crndf) (crndf))
          (* (sqrt (/ (* -2 (log l)) l)) x)))))

(define (grndvec)
  (vector (grndf) (grndf) (grndf)))

(define (rndbary)
	(let* 
		((a (- 1.0 (sqrt (rndf))))
		 (b (* (rndf) (- 1.0 a)))
		 (c (- 1.0 (+ a b))))
		(vector a b c)))

; return a line on the hemisphere 
(define (rndhemi n)
  (let loop ((v (srndvec)))
    (if (> (vdot n v) 0)
        v
        (loop (srndvec)))))

(define (hrndhemi n)
  (let loop ((v (hsrndvec)))
    (if (> (vdot n v) 0)
        v
        (loop (hsrndvec)))))
