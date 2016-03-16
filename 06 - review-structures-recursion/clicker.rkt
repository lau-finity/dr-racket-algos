;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname clicker) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #t)))
(define acc 0)
(define total 0)
(define count 0)

;; (grade acc total count) produces the grade based on the student's
;;    responses and the correct answer key list, if none have been
;;    answered correctly produce 0, if at least 75% of the questions
;;    have been answered correctly produce 100, otherwise produce
;;    the grade
;; grade: Num Num Num -> Num
;; Examples:
(check-expect (grade 8 8 8) 100)
(check-expect (grade 0 12 0) 0)

(define (grade acc total count)
  (cond
    [(= count 0) 0]
    [(>= (/ count total) 0.75) 100]
    [else
     (* (/ acc total) 100)]))

;; Tests:
(check-expect (grade 6 8 6) 100)
(check-expect (grade 7 8 3) 87.5)

;; (clicker-grade studentlst correctlst) produces a Num, the resulting
;;    clicker participation grade
;; requires: two consumed lists have equal length and should be
;;           consumed in the order given above
;; clicker-grade: (listof (anyof Sym)) (list (anyof Sym)) -> Num
;; Examples:
(check-expect (clicker-grade '(A C B E)
                             '(E B D B)) 0)
(check-expect (clicker-grade '(A B C D)
                             '(A B C D)) 100)
(check-expect (clicker-grade '(A B C none)
                             '(A B C D)) 100)

(define (clicker/acc studentlst correctlst acc total count)
  (cond
    [(empty? studentlst) (grade acc total count)]
    [(symbol=? (first studentlst)(first correctlst))
     (clicker/acc (rest studentlst)
                  (rest correctlst)
                  (add1 (add1 acc))
                  (add1 (add1 total))
                  (add1 (add1 count)))]
    [(symbol=? (first studentlst) 'none)
     (clicker/acc (rest studentlst)
                  (rest correctlst)
                  acc
                  (add1 (add1 total))
                  count)]
    [else
     (clicker/acc (rest studentlst)
                  (rest correctlst)
                  (add1 acc)
                  (add1 (add1 total))
                  count)]))

(define (clicker-grade studentlst correctlst)
  (clicker/acc studentlst correctlst acc total count))

;; Tests:
(check-expect (clicker-grade '(A B none none)
                             '(A B C D)) 50)
(check-expect (clicker-grade '(A B D E)
                             '(B E D E)) 75)
(check-expect (clicker-grade '(A B C E D A A C)
                             '(A B E none D A A C)) 100)