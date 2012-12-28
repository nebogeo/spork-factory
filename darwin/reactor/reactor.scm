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

(load "swarm.scm")
(load "log.scm")

(open-log "recording.txt")

(define s (make-swarm 500))

(swarm-fuzz s)

(recn 0 s)

(define (broadcast s dma l)
  (define (_ s l)
    (cond
     ((null? s) l)
     (else
      (_
       (cdr s)
       (if (and
            (not (eqv? (dma-src dma) (entity-machine (car s))))
            (< (vdist (dma-pos dma) 
                      (entity-pos (car s)))
               30))
           (let ((program (mutate (dma-data dma) 5)))
             (entity-program 
              (car s) 
              (dma-addr dma) 
              program)
             (cons (entity-machine (car s)) l))
           l)))))
  (cons (list (list (dma-src dma) (dma-data dma)) (_ s '())) l))

;(disassemble (list 1 23 22 3 1 1 23 22 1 23 22 3 1 1 1 23 22 3 1 1))
;(disassemble (list 1 233 230 254 254 235 23 24 230 1 233 230 254 254 235 23 235 0 234 1))
;(disassemble (list 1 210 23 108 254 64 235 0 22 3 1 1 210 23 108 254 64 235 0 22))

(define (run g)
  (swarm-run s 5) ;; run for some cycles
  ;; output the first machine data
  (output "machine0" g (entity-read (car s) 0 256))
  (let ((dmas (swarm-listen s broadcast)))
    ;;        (swarm-disassemble s 0 20)
    (display (length dmas))(newline)
    (recn g dmas))
  (run (+ g 1)))

(run 1)