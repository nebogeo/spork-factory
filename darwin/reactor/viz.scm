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

;; description of the instruction set
(define opcodes 
  (list "nop" "org" "equ" "jmp" "jmpz" "pshl"
        "psh" "pshi" "pop" "popi" "add" "sub"
        "inc" "dec" "and" "or" "xor" "not" "ror"
        "rol" "pip" "pdp" "dup" "dma"))

(define (opcode c) (list-ref opcodes c))

;; does it need an operand?
(define (operand? c)
  (list-ref
   (list #f #f #f #t #t #t
         #t #t #t #t #f #f
         #f #f #f #f #f #f #t
         #t #t #t #f #f #f #f #f)
   c))

(define (safe-nth l i)
  (if (< i (length l))
    (list-ref l i)
    0))

;; make a string of bytecode human readable
(define (disassemble code)
  (define (_ r pos)
    (let ((op (safe-nth code pos)))
      (cond
       ((>= pos (length code)) r)
       ((< op (length opcodes))
        (if (operand? op)
            (_ (string-append r (opcode op) " " (number->string (safe-nth code (+ pos 1))) "\n")
               (+ pos 2))
            (_ (string-append r (opcode op) "\n")
               (+ pos 1))))
       (else
        (_ (string-append r (number->string op) "\n") (+ pos 1))))))
  (display 
   (_ "" 0))(newline))





(define (build d)
    (let ((p (build-particles (length d))))
        (with-primitive p
        (for-each
            (lambda (n i)
                (pdata-set! "p" i (vmul (car n) 0.1)))
            d
            (build-list (length d) (lambda (i) i)))
        p)))


(define (hash-col1 data)
    (car (foldl
            (lambda (byte r)
                (let ((col (car r)) (i (cadr r)))
                    (list
                        (vector (fmod (+ (vy col) (* byte 0.001)) 1)
                                (fmod (+ (vx col) (* byte 0.002)) 1)
                                (fmod (+ (vz col) (* byte 0.003)) 1))
                        (+ i 1))))
            (list (vector 1 1 1) 0)
            data)))

(define (blend l v)
    (let* ((vl (* v (- (length l) 1)))
           (li (inexact->exact (floor vl)))
           (lt (fmod vl 1)))
      (vmix (list-ref l li)
            (list-ref l (+ li 1)) lt)))

(define (hash-col data)
    (blend (list (vector 1 0 0) (vector 0 0 0) (vector 1 0 1) (vector 1 1 1) (vector 0 1 0) (vector 0 0 1))
        (/ (foldl + 0 data) (length data) 256)))

(define (animate d)
  (let ((dmas (let* ((f (open-input-file 
                         (string-append "recording/record-" (number->string d) ".txt")))
                     (r (read f)))
                (close-input-port f) r)))
    
    (when (not (null? dmas))
          (display (disassemble (cadr (car (car dmas)))))(newline))

    (for-each
        (lambda (dma)
            (let ((hash (hash-col (cadr (car dma)))))
            (pdata-set! "c" (car (car dma)) hash)
            (pdata-set! "s" (car (car dma)) 
                (+ (vx (pdata-get "s" (car (car dma)))) 0.3)) 
            #;(for-each
                (lambda (dst)
                    (pdata-set! "c" dst (vmul hash 0.3)))
                (cadr dma))))
        dmas)

    (pdata-map!
        (lambda (s)
            (if (> (vx s) 0.2) (* (vx s) 0.8) s))
        "s")))

(clear)
;(clear-colour 1)
(define d
    (let* ((f (open-input-file "recording/record-0.txt"))
           (r (read f)))
        (close-input-port f) r))

(define p (with-state
    ;(hint-points)
    (point-width 16)
    (hint-ignore-depth)
    (texture (load-texture "textures/core.png"))
    (build (reverse d))))
        
(with-primitive p
    (pdata-map! (lambda (s) 1) "s")
    (pdata-map! (lambda (c) (vector 1 1 1)) "c"))

(define f 0)

(every-frame
    (with-primitive p
        (set! f (+ f 1))
        (animate f)))

