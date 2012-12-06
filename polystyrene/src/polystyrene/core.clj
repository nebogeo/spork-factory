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

(ns polystyrene.core
  (:use clojure.java.shell)
  (:import 
   org.jgap.Chromosome
   org.jgap.FitnessFunction
   org.jgap.Genotype
   org.jgap.impl.DefaultConfiguration
   org.jgap.impl.IntegerGene
   ))

(def cycles 1000)
(def fft-chunk-size 50)
(def population-size 1000)
(def code-size 32)

(defn make-csv [l]
  (reduce
   (fn [r i]
     (str r "," i))
   "" l))

(defn parse-number [s]
  (try (Float/parseFloat (.trim s))
       (catch NumberFormatException e nil)))

;; description of the instruction set
(def opcodes (list "nop" "org" "equ" "jmp" "jmpz" "pshl"
                   "psh" "pshi" "pop" "popi" "add" "sub"
                   "inc" "dec" "and" "or" "xor" "not" "ror"
                   "rol" "pip" "pdp" "dup" "out"))
  
(defn opcode [c] (nth opcodes c))

;; does it need an operand?
(defn operand? [c]
  (nth
   (list false false false true true true
         true true true true false false
         false false false false false false true
         true true true false false false)
   c))

(defn safe-nth [l i]
  (if (< i (count l))
    (nth l i)
    0))

;; make a string of bytecode human readable
(defn disassemble [code]
  (defn _ [r pos]
    (let [op (safe-nth code pos)]
      (cond
       (>= pos (count code)) r
       (< op (count opcodes))
       (if (operand? op)
         (_ (str r (opcode op) " " (safe-nth code (+ pos 1)) "\n")
            (+ pos 2))
         (_ (str r (opcode op) "\n")
            (+ pos 1)))
       :else
       (_ (str r op "\n") (+ pos 1)))))
  (println code)
  (_ "" 0))

;; run the processor and pass in code
(defn run-core [cycles fft-chunk code]
  (map
   parse-number
   (.split
    (:out
     (sh "../emu/emu"
         (str cycles)
         (str fft-chunk)
         :in (make-csv code)))
    " ")))

;; run the processor and pass in code
(defn run-robot [cycles code]
  (map
   parse-number
   (.split
    (:out
     (sh "../robotgen/robotgen"
         (str cycles)
         :in (make-csv code)))
    " ")))

;; make a normal list from the allele values
(defn chromo->list [chromo]
  (defn _ [c]
    (cond
     (= (.size chromo) c) '()
     :else (cons (.getAllele (.getGene chromo c))
                 (_ (+ c 1)))))
  (_ 0))

;; writes program.h and sends to atmel chip
(defn send-to-device [code]
  (spit "../core/program.h"
        (apply str
               "// autogenerated by polystyrene\n"
               "u32 program_size=" code-size ";\n"
               "u8 program[]={\n"
               (let [s (reduce (fn [r i] (str r i ", ")) "" code)]
                 (subs s 0 (- (count s) 2))) "\n"
                 "};\n"))
  ;; uncomment to upload (takes a while)
  ;;(sh "../core/make")
  )

(defn loop-evolve-forever [population]
  (.evolve population)
  (let [fittest (.getFittestChromosome population)
        program (chromo->list fittest)
        output (run-robot cycles program)] 
    (println fittest)
    (send-to-device program)
    (println (disassemble program))
    ;(apply println (partition fft-chunk-size output))
                                        ;(println (count output))
    )
  (recur population))

;; ----------------------------------------------------------
;; simple ways to gauge fitness from fft

;; boring average of the list
(defn avg [l]
  (/ (reduce + 0 l) (count l)))

;; flips list of lists sideways:
;; (flip (list (list 1 2) (list 3 4) (list 5 6))) => ((5 3 1) (6 4 2))
(defn flip [l]
  (cond
   (empty? l) ()
   (empty? (first l)) ()
   :else
   (cons
    (reduce (fn [r l] (cons (first l) r)) () l)
    (flip (map (fn [l] (rest l)) l)))))

;; returns the closest distance from an ordered list
(defn closest [l c]
  (cond
   (empty? l) c
   (empty? (rest l)) c
   (< (- (second l) (first l)) c)
   (closest (rest l) (- (second l) (first l)))
   :else (closest (rest l) c)))

;; adds up the total distance from an ordered list
(defn total-dist [l]
  (cond
   (empty? l) 0
   (empty? (rest l)) 0
   :else (+ (- (second l) (first l))
            (total-dist (rest l)))))

;; average distance from an ordered list
(defn average-dist [l]
  (/ (total-dist l) (count l)))

;; compare each list of elements by looking at difference between closest
(defn compare-chopped [l]
  (if (> (count l) 1) ;; only if we have more than one fft 
    (let [sorted (sort < l)]
      (+ (average-dist sorted) ;; add the average dist (encourage spread between highest and lowest)
         (closest sorted 9999999))) ;; to the closest distance (encourage spread between each other)
    0))

;; chop frequencies into length s 
;; and compare each against the others
;; in this way, we compare the sound at different times
(defn compare-sublists [l s]
  (reduce
   (fn [r els]
     (+ (compare-chopped els) r))
   0
   (flip (partition s l))))

;; the fitness function, return value from results of program
(defn fitness [res]
  (+ 
   (* 2 (count res)) ;; more samples is good
   (avg res) ;; high average fft frequencies are good
   (compare-sublists res 50) ;; compare frequency over time to encourage changing sounds
   ))

;; the fitness function, return value from results of program
(defn robot-fitness [res]
  (first res))

;; setup jgap and go!
(let [conf (DefaultConfiguration.)
      init (proxy [FitnessFunction] []
             (evaluate [chromo]
                       (let [res (run-robot cycles (chromo->list chromo))]
                         (if (and (> (count res) 0)
                                  (not (nil? (first res))))
                           (+ 1 (robot-fitness res))
                           0))))
      sample-genes (IntegerGene. conf 0 256) ;; 8bit genes
      sample-chromosome (Chromosome. conf sample-genes code-size)]
  
  (.setFitnessFunction conf init)
  (.setSampleChromosome conf sample-chromosome)
  (.setPopulationSize conf population-size)
  
  (loop-evolve-forever (Genotype/randomInitialGenotype conf)))
