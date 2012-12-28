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

;; visualiser of core code for fluxus

(define (build-row x)
    (cond 
        ((zero? x) '())
        (else
            (cons (list
                    (with-state
                        (translate (vector (- x) 0 0))
                        (scale (vector 0.5 1 1))
                        (build-plane))
                    (with-state
                        (translate (vector (+ (- x) 0.5) 0 0))
                        (scale (vector 0.5 1 1))
                        (build-plane)))
                (build-row (- x 1))))))

(define (build-grid x y)
    (cond 
        ((zero? y) '())
        (else
            (cons (with-state
                    (translate (vector 0 y 0))
                    (build-row x))
                (build-grid x (- y 1))))))

(define (get-tile grid x y)
    (list-ref (list-ref grid y) x))

(define (set-value grid x y v)
    (if (< v 24)
        (let ((p (get-tile grid x y)))
            (with-primitive (car p)
                (texture (load-texture "textures/ops.png"))
                (let* ((vx (modulo v 8))
                        (vy (quotient v 8))
                        (tw (/ 1 8))
                        (th (- (/ 1 3)))
                        (tx (* tw vx))
                        (ty (* th vy)))
                    (pdata-set! "t" 3 (vector tx ty 0))
                    (pdata-set! "t" 0 (vector tx (+ ty th) 0))
                    (pdata-set! "t" 2 (vector (+ (* tw 0.5) tx) ty 0))
                    (pdata-set! "t" 1 (vector (+ (* tw 0.5) tx) (+ ty th) 0))))
            (with-primitive (cadr p)
                (texture (load-texture "textures/ops.png"))
                (let* ((vx (modulo v 8))
                        (vy (quotient v 8))
                        (tw (/ 1 8))
                        (th (- (/ 1 3)))
                        (tx (* tw vx))
                        (ty (* th vy)))
                    (pdata-set! "t" 3 (vector (+ tx (* tw 0.5)) ty 0))
                    (pdata-set! "t" 0 (vector (+ tx (* tw 0.5)) (+ ty th) 0))
                    (pdata-set! "t" 2 (vector (+ tw tx) ty 0))
                    (pdata-set! "t" 1 (vector (+ tw tx) (+ ty th) 0)))))
        (let ((p (get-tile grid x y)))
            (with-primitive (car p)
                (texture (load-texture "textures/nums.png"))
                (let* ((vx (modulo (quotient v 16) 8))
                        (vy (quotient (quotient v 16) 2))
                        (tw (/ 1 8))
                        (th (- (/ 1 2)))
                        (tx (* tw vx))
                        (ty (* th vy)))
                    (pdata-set! "t" 3 (vector tx ty 0))
                    (pdata-set! "t" 0 (vector tx (+ ty th) 0))
                    (pdata-set! "t" 2 (vector (+ (* tw 0.5) tx) ty 0))
                    (pdata-set! "t" 1 (vector (+ (* tw 0.5) tx) (+ ty th) 0))))
            (with-primitive (cadr p)
                (texture (load-texture "textures/nums.png"))
                (let* ((vx (modulo (modulo v 16) 8))
                        (vy (quotient v 8))
                        (tw (/ 1 8))
                        (th (- (/ 1 2)))
                        (tx (* tw vx))
                        (ty (* th vy)))
                    (pdata-set! "t" 3 (vector (+ tx (* tw 0.5)) ty 0))
                    (pdata-set! "t" 0 (vector (+ tx (* tw 0.5)) (+ ty th) 0))
                    (pdata-set! "t" 2 (vector (+ tw tx) ty 0))
                    (pdata-set! "t" 1 (vector (+ tw tx) (+ ty th) 0)))))))

(define (upload grid code)
    (foldl
        (lambda (i r)
            (set-value grid (modulo r 16) (quotient r 16) i)
            (+ r 1))
        0
        code))

(clear)
(define grid (with-state
        (translate (vector 8.5 -8.5 -1))
        (hint-unlit)
        (build-grid 16 16)))

(define (animate d)
    (let ((prog (let* ((f (open-input-file 
                                (string-append "recording/machine0-" (number->string d) ".txt")))
                        (r (read f)))
                    (close-input-port f) r)))
        (upload grid prog)))

(define d 1)

(every-frame
 (begin (animate d)
    (set! d (+ d 1))))





