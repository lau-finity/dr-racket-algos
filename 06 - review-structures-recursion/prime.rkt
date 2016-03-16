;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname prime) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(define counter 2)

;; 1A
(define (prime? num)
  (prime-ornot num counter))

(define (prime-ornot num counter)
  (cond
    [(= num counter) true]
    [(= (remainder num counter) 0) false]
    [else (prime-ornot num (add1 counter))]))

;; 1B
(define (next-prime? num)
  (next-primeis (add1 num) counter))

(define (next-primeis num counter)
  (cond
    [(equal? (prime? num) false)(next-prime? num)]
    [else num]))

;; 1C
(define (prime-range lownat highnat)
  (cond
    [(> lownat highnat) empty]
    [else (prime-rangeis (next-prime? lownat) highnat)]))

(define (prime-rangeis lownat highnat)
  (cond
    [(and (<= lownat highnat) 
          (prime-range lownat highnat)) (list lownat)]
    [else empty])) 

(prime-range 1 10)
  