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

; --------------------------------------------------------
; swarms of processors, amorphous computing style
; --------------------------------------------------------

(load "math.scm")

(define nop 0)
(define org 1)
(define equ 2)
(define jmp 3)
(define jmpz 4)
(define pshl 5)
(define psh 6)
(define pshi 7)
(define pop 8)
(define popi 9)
(define add 10)
(define sub 11)
(define inc 12)
(define dec 13)
(define and 14)
(define or 15)
(define xor 16)
(define nnot 17)
(define ror 18)
(define rol 19)
(define pip 20)
(define pdp 21)
(define dup 22)
(define say 23)

(define (filter fn l)
  (foldr
   (lambda (r i)
     (if (fn i) (cons i r) r))
   '()
   l))

(define (println . args)
  (display (apply string-append args)) (newline))

(define (build-list fn n)
  (define (_ fn n l)
    (cond ((zero? n) l)  
          (else 

           (_ fn (- n 1) (cons (fn (- n 1)) l)))))
  (_ fn n '()))   

(define (make-entity pos)
  (list 
   pos
   (machine-create)))

(define (entity-pos e) (list-ref e 0))
(define (entity-machine e) (list-ref e 1))

(define (make-cluster-pos)
  (let ((g (grndvec)))
    (vmul (vector (vx g) (vy g) 0) 100)))

(define (make-line-pos)
  (let ((line-pos (vector (* (crndf) 300) 0 0))
        (g (grndvec)))
    (vadd (vmul (vector (vx g) (vy g) 0) 10) line-pos)))

(define (make-donut-pos)
  (let* ((g (srndvec))
         (pos (vmul (vector (vx g) (vy g) 0) 150)))
    (if (> (vmag pos) 75)
        pos
        (make-donut-pos))))

(define (make-swarm n)
  (build-list
   (lambda (i)
     (let ((g (grndvec)))
       (make-entity (make-donut-pos))))
   n))

(define (mutate prog mutation)
  (map
   (lambda (i)
     (if (< (random 1000) mutation)
         (random 256)
         i))
   prog))

(define (entity-program e a prog)
  (let ((m (entity-machine e)))
    (cond
     ((null? prog) 0)
     (else
      ;; write relative to the current thread reference
      (machine-poke m (+ a (machine-start m)) (car prog))
      (entity-program e (+ a 1) (cdr prog))))))

(define (entity-fuzz e c)
  (cond
   ((zero? c) 0)
   (else
    (machine-poke (entity-machine e) c (random 256))
    (entity-fuzz e (- c 1)))))

(define (swarm-program s a prog)
  (cond
   ((null? s) 0)
   (else
    (entity-program (car s) a prog 0)
    (swarm-program (cdr s) a prog))))

(define (swarm-fuzz s)
  (cond
   ((null? s) 0)
   (else
    (entity-fuzz (car s) 256)
    (swarm-fuzz (cdr s)))))

(define (entity-read e a l)
  (define (_ e a l c)
    (cond
     ((eqv? c l) ())
     (else
      (cons (machine-peek (entity-machine e) a) 
            (_ e (+ a 1) l (+ c 1))))))
  (_ e a l 0))

(define (entity-dump m a l)
  (define (_ m a l c)
    (cond
     ((eqv? c l) 0)
     (else
      (display (machine-peek (entity-machine m) a))
      (display " ")
      (_ m (+ a 1) l (+ c 1)))))
  (_ m a l 0))

(define (swarm-dump s a l)
  (for-each 
   (lambda (e)
     (display "machine ")(display e)(newline)
     (entity-dump e a l))(newline)
   s))

(define (swarm-disassemble s a l)
  (for-each 
   (lambda (e)
     (display "entity ")(display e)(newline)
     (disassemble (entity-read e a l)))
   s))

(define (swarm-run s c)
  (for-each
   (lambda (e)
     (machine-run (entity-machine e) c))
   s))

(define (make-dma src pos addr data) (list src pos addr data))
(define (dma-src d) (list-ref d 0))
(define (dma-pos d) (list-ref d 1))
(define (dma-addr d) (list-ref d 2))
(define (dma-data d) (list-ref d 3))

(define (swarm-listen s fn)
  (let* ((speakers 
          (filter
           (lambda (e)
             (not (zero? (machine-say (entity-machine e)))))
           s))
         (dma
          (map
           (lambda (e)
             (let ((m (entity-machine e)))
               (make-dma
                m
                (entity-pos e)
                (machine-say-addr m)
                (entity-read 
                 e 
                 (machine-say-addr m)
                 (machine-say-size m)))))
           speakers)))
    (foldr
     (lambda (r dma)
       (fn s dma r))
     '()
     dma)))

;---------------------------------------

;; description of the instruction set
(define opcodes 
  (list "nop" "org" "equ" "jmp" "jmpz" "pshl"
        "psh" "pshi" "pop" "popi" "add" "sub"
        "inc" "dec" "and" "or" "xor" "not" "ror"
        "rol" "pip" "pdp" "dup" "say"))

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

