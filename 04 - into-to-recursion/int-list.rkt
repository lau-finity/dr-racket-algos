;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname int-list) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #t)))
;; 2A
;; (elements-more-than lst num) produces a list of integers from
;;   the original list that are strictly greater than the integer
;;   given in the second parameter
;; elements-more-than: (listof Int) Int -> (listof Int)
;; requires: only integers are allowed in both parameters
;; Examples:
(check-expect (elements-more-than (cons 1 (cons 2 (cons 3 empty))) 2)
              (cons 3 empty))
(check-expect (elements-more-than (cons 5 (cons -1 (cons 3 empty))) 0)
              (cons 5 (cons 3 empty)))
(check-expect (elements-more-than (cons 2 (cons 2 empty)) 2)
              empty)
 
(define (elements-more-than lst num)
  (cond
    [(empty? lst) empty]
    [(> (first lst) num)
     (cons (first lst)(elements-more-than (rest lst) num))]
    [else
     (elements-more-than (rest lst) num)]))

;; Tests:
(check-expect (elements-more-than empty 10) empty)
(check-expect (elements-more-than (cons 1 (cons 3 empty)) -1)
              (cons 1 (cons 3 empty)))
(check-expect (elements-more-than (cons 1 (cons 2 (cons 2 empty))) 1)
              (cons 2 (cons 2 empty)))

;; 2B
;; (arthimetic-sequence? lst) produces true if the list is an
;;    arthimetic sequence and false otherwise
;; arthimetic-sequence?: (listof Int) -> Bool
;; requires: input parameter must be a list of integers
;; Examples:
(check-expect (arithmetic-sequence?
               (cons 1 (cons 2 (cons 3 (cons 4 empty)))))
              true)
(check-expect (arithmetic-sequence?
               (cons 4 (cons 2 (cons 0 (cons -2 (cons -4 empty))))))
              true)

(define (arithmetic-sequence? lst)
  (cond
    [(empty? lst) true]
    [(empty? (rest lst)) true]
    [(empty? (rest (rest lst))) true]
    [(= (- (first lst)(first (rest lst)))
        (- (first (rest lst))(first (rest (rest lst)))))
     (arithmetic-sequence? (rest lst))]
    [else false]))     

;; Tests:
(check-expect (arithmetic-sequence? empty)
              true)              
(check-expect (arithmetic-sequence?
               (cons 1 (cons 3 (cons 4 empty))))
              false)
(check-expect (arithmetic-sequence?
               (cons 5 (cons 5 (cons 5 empty))))
              true)
(check-expect (arithmetic-sequence?
               (cons 2 (cons 3 empty)))
              true)

;; 2C
;; (digits->integer lst) produces the integer represented by those
;;   digits, where the least significant digit is at the beginning
;;   of the list and the most significant digit is at the end of the
;;   list
;; digits->integer: (listof Int) -> Any
;; Examples:
(check-expect (digits->integer (cons 0 empty)) 0)
(check-expect (digits->integer (cons 24 (cons 54 (cons 64 empty)))) 'error)
(check-expect (digits->integer (cons 9 (cons 0 (cons 0 empty)))) 9)
(check-expect (digits->integer (cons 0 (cons 0 (cons 9 empty)))) 900) 

(define (helper lst lngth)
  (cond
    [(empty? lst) 0]
    [(or (< (first lst) 0) 
         (>= (first lst) 10)) 'error] 
    [else (+ (helper (rest lst) lngth)
          (* (first lst)(expt 10 (- lngth (length lst)))))]))

(define (digits->integer lst)
  (helper lst (length lst)))

;; Tests:
(check-expect (digits->integer (cons 4 (cons 5 (cons 6 empty)))) 654)
(check-expect (digits->integer (cons 0 (cons 1 (cons 2 (cons 0 empty))))) 210)
(check-expect (digits->integer (cons -7 (cons -8 (cons -9 empty)))) 'error)
(check-expect (digits->integer (cons 2398 (cons 283723 (cons -209323 empty)))) 'error)