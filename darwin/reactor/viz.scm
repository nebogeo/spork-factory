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

;; visualiser for fluxus

(define (build d)
    (let ((p (build-particles (length d))))
        (with-primitive p
        (for-each
            (lambda (n i)
                (pdata-set! "p" i (vmul (car n) 0.1)))
            d
            (build-list (length d) (lambda (i) i)))
        p)))

(define (hash-col data)
    (car (foldl
            (lambda (byte r)
                (let ((col (car r)) (i (cadr r)))
                    (list
                        (vector (fmod (+ (vx col) (* byte 0.0013)) 1)
                                (fmod (+ (vy col) (* byte 0.0037)) 1)
                                (fmod (+ (vz col) (* byte 0.00021)) 1))
                        (+ i 1))))
            (list (vector 1 1 1) 0)
            data)))

(define (animate d)
    (for-each
        (lambda (dma)
            (let ((hash (hash-col (cadr (car dma)))))
            (pdata-set! "c" (car (car dma)) hash)
            #;(for-each
                (lambda (dst)
                    (pdata-set! "c" dst hash))
                (cadr dma))))
        (let* ((f (open-input-file 
            (string-append "recording/record-" (number->string d) ".txt")))
            (r (read f)))
            (close-input-port f) r)))
(clear)

(define d
    (let* ((f (open-input-file "recording/record-0.txt"))
           (r (read f)))
        (close-input-port f) r))

(define p (with-state
    ;(hint-points)
    (point-width 16)
    (hint-ignore-depth)
    (texture (load-texture "core.png"))
    (build (reverse d))))
        
(with-primitive p
    (pdata-map! (lambda (s) 1) "s")
    (pdata-map! (lambda (c) (vector 1 1 1)) "c"))

(define f 0)

(every-frame
    (with-primitive p
        (set! f (+ f 1))
        (animate f)))