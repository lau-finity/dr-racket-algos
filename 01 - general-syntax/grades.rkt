;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname grades) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; Definitions:
(define part-weight 5/100)
(define assign-weight 20/100)
(define mid-weight 25/100)
(define final-weight 50/100)

;; 3A
;; Examples:
(check-expect (cs135-grade-sofar 97 95 80) 90)
(check-expect (cs135-grade-sofar 80 60 75) 76)

(define total-weight (+ mid-weight part-weight assign-weight))

(define (cs135-grade-sofar mid part assign)
  (/ (+ (* mid mid-weight)
        (* part part-weight)
        (* assign assign-weight))
     total-weight))

;; Tests:
(check-expect (cs135-grade-sofar 100 100 100) 100)
(check-expect (cs135-grade-sofar 0 0 0) 0)

;; 3B
;; Examples:
(check-expect (cs135-final-exam 95 65) 35)
(check-expect (cs135-final-exam 55 75) 95)

(define (cs135-final-exam sofar-weight final)
  (/ (- final (* sofar-weight total-weight))
     final-weight))

;; Tests:
(check-expect (cs135-final-exam 100 100) 100)
(check-expect (cs135-final-exam 0 0) 0)