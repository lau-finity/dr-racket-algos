;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname conversion) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(define m 1609.344)
(define ft 1.8288)
(define fn 14)
(define h 24)
(define ftn (/ 1.8288 14))
(define s 1.7018)
(define sec 3600)
(define nc 3.15576)

;; 2A
(define (m/s->mph m/s)
  (/ (/ m/s m)(/ 1 3600)))
(check-expect (m/s->mph 1609.344) 3600)

;; 2B
(define (fpf->mph fpf)
  (* fpf (/ ft (* fn h m))))
(check-expect (fpf->mph 1)(/ 18288 5407395840))

;; 2C
(define (mph->S/nc mph)
  (* mph (/ (* m nc)(* sec s))))
(check-expect (mph->S/nc 1)(/ 507870342144 612648000000))