(load "swarm.scm")

(define s (make-swarm 3))

(machine-program (car s)
                 0 (list 
                    org 
                    pshl 8
                    pshl 0
                    say
                    jmp 0))

(define (run n)
  (println "hhhh")
  (if (> n 0)
      (begin
        (println "hellowewe")
        (swarm-run s 1)
        (println "foo")
        (swarm-listen s broadcast)
        (println "arr")
        (swarm-disassemble s 0 20)
        (run (- n 1)))))

(println "hello")
(run 10)