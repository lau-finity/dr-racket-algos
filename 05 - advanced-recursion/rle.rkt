;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname rle) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #t)))
;; 2A
;; An RlePair is a (list Any Nat)
;; requires: a non-zero natural number
;; An RleList is one of:
;; * empty
;; * (cons RlePair RleList)

;; 2B
;; (rle-decode rlst) produces the list converted from the run-length
;;   encoding to its original list
;; rle-decode: RleList -> (listof Any)
;; Examples:
(check-expect (rle-decode (list (list 'red 4)
                                (list 'blue 2)))
                          (list 'red 'red 'red 'red 'blue 'blue))
(check-expect (rle-decode (list (list 'a 3)
                                (list 'b 2)))
              (list 'a 'a 'a 'b 'b))
(check-expect (rle-decode (list))
              empty)

(define (repeat sym nat)
  (cond
    [(zero? nat) empty]
    [else (cons sym (repeat sym (sub1 nat)))]))


(define (rle-decode rlst)
  (cond
    [(empty? rlst) empty]
    [else
     (append (repeat (first (first rlst))
                     (first (rest (first rlst))))
             (rle-decode (rest rlst)))]))

;; Tests:
(check-expect (rle-decode (list (list 'pepsi 2)))
              (list 'pepsi 'pepsi))
(check-expect (rle-decode (list (list 'c 0)
                                (list 'd 5)))
              (list 'd 'd 'd 'd 'd))
(check-expect (rle-decode (list (list 'e 0)
                                (list 'f 0)
                                (list 'z 0)))
              empty)