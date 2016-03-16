;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname mapfn) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #t)))
;; 3A
;; (mapfn lof lon) produces the list of the results of applying
;;   each function in turn to the given two numbers
;; requires: - lof must have only five elements in the list (each of
;;           which is a function that can take two numbers)
;;           - resulting list must be also a length of five
;; mapfn: (listof (Num Num -> Any))(listof Num) -> (listof Any)
;; Examples:
(check-expect (mapfn (list) '(3 2)) empty)
(check-expect (mapfn (list +) '(3 2)) '(5))

(define (mapfn lof lon)
  (cond
    [(empty? lof) empty]
    [else
     (cons ((first lof)(first lon)(second lon))
           (mapfn (rest lof) lon))]))

;; Tests:
(check-expect (mapfn (list + - * / list) '(3 2))
              '(5 1 6 1.5 (3 2))) 
(check-expect (mapfn (list + = expt remainder < > =) '(4 5))
              (list 9 false 1024 4 true false false))

;; 3B
;; (is-in-order? lof looper) produces a boolean result if the second
;;   list satisfies all the predicate function, binary relational
;;   operators in the first list then output true, otherwise false
;; is-in-order?: (listof (list (Any-> Bool)(X X -> Boolean)))
;;               (listof Any) -> (anyof Bool 'error)
;; requires: first list is non-empty
;; Examples:
(check-expect (is-in-order? (list (list integer? <)) empty) true)
(check-expect (is-in-order? (list (list integer? <))(list 1)) true)
(check-expect (is-in-order? (list (list integer? >)
                            (list string? string>?))
                            (list 'a 'b 'c)) 'error)
(check-expect (is-in-order? (list (list integer? <))
                            (list 1 2 7)) true)
(check-expect (is-in-order? (list (list integer? <))
                            (list 1 2 8 10)) true)

(define (is-in-order? lof looper)
  (cond
    [(empty? lof) 'error]
    [(empty? looper) true]
    [(empty? (rest looper)) true]
    [(boolean=? (check-fn (first lof) looper looper) true)
     (cond
       [(boolean=? (relat-fn (second (first lof)) looper) true)
        true]
       [else false])]
    [(boolean=? (check-fn (first lof) looper looper) false)
     (is-in-order? (rest lof) looper)]))
          
(define (check-fn lof looper1 looper2)
  (cond
    [(empty? looper1) true] 
    [(boolean=? ((first lof)(first looper1)) true)
     (check-fn lof (rest looper1) looper2)]
    [else
     false]))

(define (relat-fn operator lon)
  (cond
    [(empty? lon) true]
    [(empty? (rest lon)) true]
    [(boolean=? (operator (first lon)(second lon)) true)
     (relat-fn operator (rest lon))]
    [else
     false])) 

;; Tests:
(check-expect (is-in-order? (list (list integer? >))
                            (list 1 2 7)) false)
(check-expect (is-in-order? (list (list integer? >))
                            (list 2 1 7)) false)
(check-expect (is-in-order? (list (list string? string<?))
                            (list "1" "2" "7")) true)
(check-expect (is-in-order? (list (list symbol? symbol=?) 
                            (list integer? =))
                            (list 1 1 1)) true)
(check-expect (is-in-order? (list (list integer? >)
                                  (list integer? <))
                            (list 1 1 1)) false)