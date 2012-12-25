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

(define (make-swarm n)
  (build-list
   (lambda (i)
     (let ((g (grndvec)))
       (make-entity (vmul (vector (vx g) (vy g) 0) 100))))
   n))

(define (entity-program e a prog mutation)
  (cond
   ((null? prog) 0)
   (else
    (machine-poke (entity-machine e) a 
                  (if (< (random 100) mutation)
                      (+ (car prog) (* (crndf) 2))
                      (car prog)))
    (entity-program e (+ a 1) (cdr prog) mutation))))

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

(define (make-dma pos addr data) (list pos addr data))
(define (dma-pos d) (list-ref d 0))
(define (dma-addr d) (list-ref d 1))
(define (dma-data d) (list-ref d 2))

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
               ;(display (entity-read e 0 10))(newline)
               ;(disassemble (entity-read e 0 10))
               (display (entity-machine e))
               (display " sending ")
               (disassemble (entity-read 
                             e 
                             (machine-say-addr m)
                             (machine-say-size m)))
               (newline)
               (make-dma
                (entity-pos e)
                (machine-say-addr m)
                (entity-read 
                 e 
                 (machine-say-addr m)
                 (machine-say-size m)))))
           speakers)))
    (for-each
     (lambda (dma)
       (fn s dma))
     dma)))

(define (broadcast s dma)
  (cond
   ((null? s) 0)
   (else
    (when (< (vdist (dma-pos dma) 
                    (entity-pos (car s)))
             20)
        ;;  (println "recieved by " (number->string (entity-machine (car s))))
          (entity-program (car s) (dma-addr dma) (dma-data dma) 2))
    (broadcast (cdr s) dma))))

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

