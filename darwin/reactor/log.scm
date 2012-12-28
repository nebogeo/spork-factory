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

(define log-filename "")
(define log-file 0)
(define log-contents "")

(define (open-log filename)
  (set! log-filename filename)
  (set! log-file (open-output-file log-filename))
  (write '() log-file)
  (close-output-port log-file))

(define (rec l)
  (set! log-file (open-input-file log-filename))
  (set! log-contents (read log-file))
  (close-input-port log-file)
  (set! log-file (open-output-file log-filename))
  (write (append log-contents (list l)) log-file)
  (close-output-port log-file))

(define (recn d l)
  (set! log-file (open-output-file (string-append "recording/record-" 
                                                  (number->string d)
                                                  ".txt")))
  (write l log-file)
  (close-output-port log-file))

(define (output fn d l)
  (set! log-file (open-output-file (string-append "recording/" fn "-" 
                                                  (number->string d)
                                                  ".txt")))
  (write l log-file)
  (close-output-port log-file))

