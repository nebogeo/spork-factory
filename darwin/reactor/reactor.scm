(load "swarm.scm")

(define s (make-swarm 200))

(swarm-fuzz s)

(define (run)
  (println ".........................")
  (swarm-run s 1)
  (swarm-listen s broadcast)
  ;;        (swarm-disassemble s 0 20)
  (run))

(run)