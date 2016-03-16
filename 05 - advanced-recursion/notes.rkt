;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname notes) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #t)))
;; 1A
(define-struct note (letter modifier))
;; A Note is a (make-note Sym Sym)
;; requires: fields correspond to a valid note
;;           one of 'A through 'G inclusive
;;           one of 'sharp, 'flat, or 'natural

;; 1B
;; (normalize-note nt) produces an integer between 1 and 12
;;   inclusive depending on the given note position on the
;;   chromatic scale
;; normalize-note: Struct -> Num
;; Examples:
(check-expect (normalize-note (make-note 'C 'sharp)) 2)
(check-expect (normalize-note (make-note 'B 'sharp)) 1)
(check-expect (normalize-note (make-note 'E 'natural))  5)

(define (normalize-note nt)
  (cond
    [(or (and (symbol=? (note-letter nt) 'C)
              (symbol=? (note-modifier nt) 'natural))
         (and (symbol=? (note-letter nt) 'B)
              (symbol=? (note-modifier nt) 'sharp))) 1]
    [(or (and (symbol=? (note-letter nt) 'C)
              (symbol=? (note-modifier nt) 'sharp))
         (and (symbol=? (note-letter nt) 'D)
              (symbol=? (note-modifier nt) 'flat))) 2]
    [(and (symbol=? (note-letter nt) 'D)
          (symbol=? (note-modifier nt) 'natural)) 3]
    [(or (and (symbol=? (note-letter nt) 'D)
              (symbol=? (note-modifier nt) 'sharp))
         (and (symbol=? (note-letter nt) 'E)
              (symbol=? (note-modifier nt) 'flat))) 4]
    [(or (and (symbol=? (note-letter nt) 'E)
              (symbol=? (note-modifier nt) 'natural))
         (and (symbol=? (note-letter nt) 'F) 
              (symbol=? (note-modifier nt) 'flat))) 5]
    [(or (and (symbol=? (note-letter nt) 'F)
              (symbol=? (note-modifier nt) 'natural))
         (and (symbol=? (note-letter nt) 'E)
              (symbol=? (note-modifier nt) 'sharp))) 6]
    [(or (and (symbol=? (note-letter nt) 'F)
              (symbol=? (note-modifier nt) 'sharp))
         (and (symbol=? (note-letter nt) 'G)
              (symbol=? (note-modifier nt) 'flat))) 7]
    [(and (symbol=? (note-letter nt) 'G)
          (symbol=? (note-modifier nt) 'natural)) 8]
    [(or (and (symbol=? (note-letter nt) 'G)
              (symbol=? (note-modifier nt) 'sharp))
         (and (symbol=? (note-letter nt) 'A)
              (symbol=? (note-modifier nt) 'flat))) 9]
    [(and (symbol=? (note-letter nt) 'A)
          (symbol=? (note-modifier nt) 'natural)) 10]
    [(or (and (symbol=? (note-letter nt) 'A)
              (symbol=? (note-modifier nt) 'sharp))
         (and (symbol=? (note-letter nt) 'B)
              (symbol=? (note-modifier nt) 'flat))) 11]
    [(or (and (symbol=? (note-letter nt) 'B)
              (symbol=? (note-modifier nt) 'natural))
         (and (symbol=? (note-letter nt) 'C)
              (symbol=? (note-modifier nt) 'flat))) 12]))

;; Tests:
(check-expect (normalize-note (make-note 'G 'natural)) 8)
(check-expect (normalize-note (make-note 'C 'flat)) 12)
(check-expect (normalize-note (make-note 'A 'sharp))  11)

;; 1C
;; (normalize-note-list ntlst) produces the corresponding list
;;   of numbers that indicate the positions on the chromatic scale
;; normalize-note-list: (listof Struct) -> (listof Num)
;; Examples:
(check-expect (normalize-note-list
               (list (make-note 'C 'sharp)
                     (make-note 'G 'flat)))(list 2 7))
(check-expect (normalize-note-list
               (list (make-note 'C 'sharp)))(list 2))

(define (normalize-note-list ntlst)
  (cond
    [(empty? ntlst) empty]
    [else
     (cons (normalize-note (first ntlst))
           (normalize-note-list (rest ntlst)))]))

;; Tests:
(check-expect (normalize-note-list
               (list (make-note 'C 'sharp)
                     (make-note 'A 'natural)
                     (make-note 'E 'flat)
                     (make-note 'G 'natural)
                     (make-note 'G 'sharp)))(list 2 10 4 8 9))
(check-expect (normalize-note-list
               (list)) empty)

;; 1D
;; (interval nst1 nst2) produces the number of increasing steps
;;   needed to get from the first note to the second note on the
;;   chromatic scale
;; interval: Struct Struct -> Num
;; Examples:
(check-expect (interval
               (make-note 'C 'sharp)
               (make-note 'G 'flat)) 5)
(check-expect (interval
               (make-note 'C 'natural)
               (make-note 'C 'flat)) 11)
(check-expect (interval
               (make-note 'C 'natural)
               (make-note 'C 'natural)) 0)
(check-expect (interval
               (make-note 'F 'natural)
               (make-note 'A 'sharp)) 5)

(define (interval nst1 nst2)
  (cond
    [(< (normalize-note nst1)
        (normalize-note nst2))(- (normalize-note nst2)
                                 (normalize-note nst1))]
    [(= (normalize-note nst1)
        (normalize-note nst2)) 0]
    [(> (normalize-note nst1)
        (normalize-note nst2))(- 12 (- (normalize-note nst1)
                                       (normalize-note nst2)))]))                              

;; Tests:
(check-expect (interval
               (make-note 'C 'sharp)
               (make-note 'D 'flat)) 0)
(check-expect (interval
               (make-note 'G 'natural)
               (make-note 'F 'sharp)) 11)
(check-expect (interval
               (make-note 'D 'natural)
               (make-note 'C 'natural)) 10)
(check-expect (interval
               (make-note 'C 'natural)
               (make-note 'D 'natural)) 2)

;; 1E
;; (slider pholder olst) is a helper function for the note-list-to
;;   -interval-list function
;; slider: Struct (listof Struct) -> (listof Num)

(define (slider pholder olst)
  (cond
    [(empty? (rest olst))(cons (interval (first olst) pholder) empty)]
    [else
     (cons (interval (first olst)(second olst))
           (slider pholder (rest olst)))]))

;; (note-list-to-interval-list nstlst) produces a list of intervals
;;   between every adjacent pair of notes, followed by the interval
;;   between the last note in the list and the first note in the list
;; note-list-to-interval-list: (listof Struct) -> (list Num)
;; Examples:
(check-expect (note-list-to-interval-list
              (list (make-note 'C 'natural)
                    (make-note 'D 'natural)
                    (make-note 'E 'natural)))(list 2 2 8))
(check-expect (note-list-to-interval-list
              (list (make-note 'C 'natural)
                    (make-note 'D 'natural)
                    (make-note 'E 'natural)
                    (make-note 'E 'natural)))(list 2 2 0 8))

(define (note-list-to-interval-list nstlst)
  (cond
    [(empty? nstlst) empty] 
    [else (slider (first nstlst) nstlst)]))

;; Tests:
(check-expect (note-list-to-interval-list
              (list (make-note 'C 'natural)
                    (make-note 'B 'natural)))(list 11 1))
(check-expect (note-list-to-interval-list
              (list)) empty)