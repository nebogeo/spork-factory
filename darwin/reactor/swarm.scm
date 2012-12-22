
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
(define not 17)
(define ror 18)
(define rol 19)
(define pip 20)
(define pdp 21)
(define dup 22)
(define say 23)

(define (println . args)
  (display (apply string-append args)) (newline))

(define (build-list fn n)
  (define (_ fn n l)
    (cond ((zero? n) l)  
          (else 
           (_ fn (- n 1) (cons (fn (- n 1)) l)))))
  (_ fn n '()))   

(define (make-swarm n)
  (build-list
   (lambda (i)
     (machine-create))
   n))

(define (machine-program m a prog)
  (cond
   ((null? prog) 0)
   (else
    (machine-poke m a (car prog))
    (machine-program m (+ a 1) (cdr prog)))))

(define (swarm-program s a prog)
  (cond
   ((null? s) 0)
   (else
    (machine-program (car s) a prog)
    (swarm-program (cdr s) a prog))))

(define (machine-read m a l)
  (define (_ m a l c)
    (cond
     ((eqv? c l) ())
     (else
      (cons (machine-peek m a) 
            (_ m (+ a 1) l (+ c 1))))))
  (_ m a l 0))

(define (machine-dump m a l)
  (define (_ m a l c)
    (cond
     ((eqv? c l) 0)
     (else
      (display (machine-peek m a))
      (display " ")
      (_ m (+ a 1) l (+ c 1)))))
  (_ m a l 0))

(define (swarm-dump s a l)
  (for-each 
   (lambda (m)
     (display "machine ")(display m)(newline)
     (machine-dump m a l))(newline)
   s))

(define (swarm-disassemble s a l)
  (for-each 
   (lambda (m)
     (display "machine ")(display m)(newline)
     (disassemble (machine-read m a l)))
   s))

(define (swarm-run s c)
  (for-each
   (lambda (m)
     (machine-run m c))
   s))

(define (filter fn l)
  (foldr
   (lambda (r i)
     (if (fn i) (cons i r) r))
   '()
   l))

(define (swarm-listen s fn)
  (let* ((speakers 
          (filter
           (lambda (m)
             (not (zero? (machine-say m))))
           s))
         (dma
          (map
           (lambda (s)
             (list
              (machine-say-addr m)
              (machine-read 
               s (machine-say-addr m)
               (machine-say-size m))))
           speakers)))
    (for-each
     (lambda (dma)
       (fn s dma))
     dma)))

(define (broadcast s dma)
  (swarm-program s (car dma) (cadr dma)))

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
        (_ (string-append r op " ") (+ pos 1))))))
  (display 
   (_ "" 0))(newline))

