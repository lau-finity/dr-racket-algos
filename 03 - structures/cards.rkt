;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname cards) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; 1A
;; A Card structure has the fields rank and suit:
;; A Card is a (make-card Nat Sym)
;; requires: rank: 1<=Nat<=13 
;;           suit: {'club, 'diamonds, 'hearts, or 'spades}
(define-struct card (rank suit))

;; A Hand structure has the fields c1, c2, and c3:
;; A Hand is a (make-hand Card Card Card)
;; requires: appropriate field values for each card structure
(define-struct hand (c1 c2 c3))

;; 1B
;; (better-card c1 c2) produces the best Card out of the two,
;;   depending on the ranking and/or suit
;; better-card: Struct Struct -> Struct
;; Examples:
(check-expect (better-card (make-card 1 'clubs)
                           (make-card 10 'clubs))
              (make-card 10 'clubs))
(check-expect (better-card (make-card 2 'hearts)
                           (make-card 2 'hearts))
              (make-card 2 'hearts))

(define (better-card c1 c2) 
  (cond
    [(symbol=? (card-suit c1)(card-suit c2))
     (cond
       [(< (card-rank c1)(card-rank c2))
        (make-card (card-rank c2)(card-suit c2))]
       [(> (card-rank c1)(card-rank c2))
        (make-card (card-rank c1)(card-suit c1))]
       [(= (card-rank c1)(card-rank c2))
        (make-card (card-rank c1)(card-suit c1))])]
    [(symbol=? (card-suit c1) 'clubs)
     (cond
       [(symbol=? (card-suit c2) 'diamonds)
        (make-card (card-rank c2)(card-suit c2))]
       [(symbol=? (card-suit c2) 'hearts)
        (make-card (card-rank c2)(card-suit c2))]
       [(symbol=? (card-suit c2) 'spades)
        (make-card (card-rank c2)(card-suit c2))])]
    [(symbol=? (card-suit c1) 'diamonds)
     (cond
       [(symbol=? (card-suit c2) 'hearts)
        (make-card (card-rank c2)(card-suit c2))]
       [(symbol=? (card-suit c2) 'spades)
        (make-card (card-rank c2)(card-suit c2))]
       [(symbol=? (card-suit c2) 'clubs)
        (make-card (card-rank c1)(card-suit c1))])]
    [(symbol=? (card-suit c1) 'hearts)
     (cond
       [(symbol=? (card-suit c2) 'spades)
        (make-card (card-rank c2)(card-suit c2))]
       [(symbol=? (card-suit c2) 'clubs)
        (make-card (card-rank c1)(card-suit c1))]
       [(symbol=? (card-suit c2) 'diamonds)
        (make-card (card-rank c1)(card-suit c1))])]
    [(symbol=? (card-suit c1) 'spades)
     (cond
       [(symbol=? (card-suit c2) 'clubs)
        (make-card (card-rank c1)(card-suit c1))]
       [(symbol=? (card-suit c2) 'diamonds)
        (make-card (card-rank c1)(card-suit c1))]
       [(symbol=? (card-suit c2) 'hearts)
        (make-card (card-rank c1)(card-suit c1))])]))

;; Tests:
(check-expect (better-card (make-card 2 'spades)
                           (make-card 2 'hearts))
              (make-card 2 'spades))
(check-expect (better-card (make-card 2 'spades)
                           (make-card 2 'spades))
              (make-card 2 'spades))
(check-expect (better-card (make-card 10 'diamonds)
                           (make-card 11 'clubs))
              (make-card 10 'diamonds))
(check-expect (better-card (make-card 11 'clubs)
                           (make-card 2 'diamonds))
              (make-card 2 'diamonds))                    

;; 1C
;; (hand-value hd) produces a symbol indicating the best
;;   hand-value of the given Hand(were the arrangement/order of the
;;   three cards in the Hand can be altered). The outputs of the
;;   hand-values are described in the following decreasing order
;;  (best to worst):
;;           - 'straight-flush
;;           - 'flush
;;           - 'straight
;;           - 'three-of-a-kind
;;           - 'pair
;;           - 'high-card
;; hand-value: Struct -> Sym
;; Examples:
(check-expect (hand-value (make-hand (make-card 10 'spades)
                                     (make-card 10 'hearts)
                                     (make-card 10 'diamonds)))
                          'three-of-a-kind)
(check-expect (hand-value (make-hand (make-card 10 'spades)
                                     (make-card 9 'hearts)
                                     (make-card 11 'clubs)))
                          'straight)
                                     
(define (hand-value hd)
  (cond
    [(and (and (symbol=? (card-suit (hand-c1 hd))
                         (card-suit (hand-c2 hd)))
               (symbol=? (card-suit (hand-c2 hd)) 
                         (card-suit (hand-c3 hd))))
          (and (= (card-rank (hand-c1 hd)) 1)     
               (or (= (card-rank (hand-c2 hd)) 2) 
                   (= (card-rank (hand-c3 hd)) 3)
                   (= (card-rank (hand-c2 hd)) 3)
                   (= (card-rank (hand-c3 hd)) 2))))
     'straight-flush]
    [(and (and (symbol=? (card-suit (hand-c1 hd))
                         (card-suit (hand-c2 hd)))
               (symbol=? (card-suit (hand-c2 hd))
                         (card-suit (hand-c3 hd))))
          (and (= (card-rank (hand-c1 hd)) 2)     
               (or (= (card-rank (hand-c2 hd)) 3) 
                   (= (card-rank (hand-c3 hd)) 4)
                   (= (card-rank (hand-c2 hd)) 4)
                   (= (card-rank (hand-c3 hd)) 3))))
     'straight-flush]
    [(and (and (symbol=? (card-suit (hand-c1 hd))
                         (card-suit (hand-c2 hd)))
               (symbol=? (card-suit (hand-c2 hd))
                         (card-suit (hand-c3 hd))))
          (and (= (card-rank (hand-c1 hd)) 3)     
               (or (= (card-rank (hand-c2 hd)) 4) 
                   (= (card-rank (hand-c3 hd)) 5)
                   (= (card-rank (hand-c2 hd)) 5)
                   (= (card-rank (hand-c3 hd)) 4))))
     'straight-flush]
    [(and (and (symbol=? (card-suit (hand-c1 hd))
                         (card-suit (hand-c2 hd)))
               (symbol=? (card-suit (hand-c2 hd))
                         (card-suit (hand-c3 hd))))
          (and (= (card-rank (hand-c1 hd)) 4)     
               (or (= (card-rank (hand-c2 hd)) 5) 
                   (= (card-rank (hand-c3 hd)) 6)
                   (= (card-rank (hand-c2 hd)) 6)
                   (= (card-rank (hand-c3 hd)) 5))))
     'straight-flush]
    [(and (and (symbol=? (card-suit (hand-c1 hd))
                         (card-suit (hand-c2 hd)))
               (symbol=? (card-suit (hand-c2 hd))
                         (card-suit (hand-c3 hd))))
          (and (= (card-rank (hand-c1 hd)) 5)     
               (or (= (card-rank (hand-c2 hd)) 6) 
                   (= (card-rank (hand-c3 hd)) 7)
                   (= (card-rank (hand-c2 hd)) 7)
                   (= (card-rank (hand-c3 hd)) 6))))
     'straight-flush]
    [(and (and (symbol=? (card-suit (hand-c1 hd))
                         (card-suit (hand-c2 hd)))
               (symbol=? (card-suit (hand-c2 hd))
                         (card-suit (hand-c3 hd))))
          (and (= (card-rank (hand-c1 hd)) 6)     
               (or (= (card-rank (hand-c2 hd)) 7) 
                   (= (card-rank (hand-c3 hd)) 8)
                   (= (card-rank (hand-c2 hd)) 8)
                   (= (card-rank (hand-c3 hd)) 7))))
     'straight-flush]
    [(and (and (symbol=? (card-suit (hand-c1 hd))
                         (card-suit (hand-c2 hd)))
               (symbol=? (card-suit (hand-c2 hd))
                         (card-suit (hand-c3 hd))))
          (and (= (card-rank (hand-c1 hd)) 7)     
               (or (= (card-rank (hand-c2 hd)) 8) 
                   (= (card-rank (hand-c3 hd)) 9)
                   (= (card-rank (hand-c2 hd)) 9)
                   (= (card-rank (hand-c3 hd)) 8))))
     'straight-flush]
    [(and (and (symbol=? (card-suit (hand-c1 hd))
                         (card-suit (hand-c2 hd)))
               (symbol=? (card-suit (hand-c2 hd))
                         (card-suit (hand-c3 hd))))
          (and (= (card-rank (hand-c1 hd)) 8)     
               (or (= (card-rank (hand-c2 hd)) 9) 
                   (= (card-rank (hand-c3 hd)) 10)
                   (= (card-rank (hand-c2 hd)) 10)
                   (= (card-rank (hand-c3 hd)) 9))))
     'straight-flush]
    [(and (and (symbol=? (card-suit (hand-c1 hd))
                         (card-suit (hand-c2 hd)))
               (symbol=? (card-suit (hand-c2 hd))
                         (card-suit (hand-c3 hd))))
          (and (= (card-rank (hand-c1 hd)) 9)     
               (or (= (card-rank (hand-c2 hd)) 10) 
                   (= (card-rank (hand-c3 hd)) 11)
                   (= (card-rank (hand-c2 hd)) 11)
                   (= (card-rank (hand-c3 hd)) 10))))
     'straight-flush]
    [(and (and (symbol=? (card-suit (hand-c1 hd))
                         (card-suit (hand-c2 hd)))
               (symbol=? (card-suit (hand-c2 hd))
                         (card-suit (hand-c3 hd))))
          (and (= (card-rank (hand-c1 hd)) 10)     
               (or (= (card-rank (hand-c2 hd)) 11) 
                   (= (card-rank (hand-c3 hd)) 12)
                   (= (card-rank (hand-c2 hd)) 12)
                   (= (card-rank (hand-c3 hd)) 11))))
     'straight-flush]
    [(and (and (symbol=? (card-suit (hand-c1 hd))
                         (card-suit (hand-c2 hd)))
               (symbol=? (card-suit (hand-c2 hd))
                         (card-suit (hand-c3 hd))))
          (and (= (card-rank (hand-c1 hd)) 11)     
               (or (= (card-rank (hand-c2 hd)) 12) 
                   (= (card-rank (hand-c3 hd)) 13)
                   (= (card-rank (hand-c2 hd)) 13)
                   (= (card-rank (hand-c3 hd)) 12))))
     'straight-flush]
    [(and (and (symbol=? (card-suit (hand-c1 hd))
                         (card-suit (hand-c2 hd)))
               (symbol=? (card-suit (hand-c2 hd))
                         (card-suit (hand-c3 hd))))
          (and (= (card-rank (hand-c1 hd)) 12)     
               (or (= (card-rank (hand-c2 hd)) 13) 
                   (= (card-rank (hand-c3 hd)) 11)
                   (= (card-rank (hand-c2 hd)) 11)
                   (= (card-rank (hand-c3 hd)) 13))))
     'straight-flush]
    [(and (and (symbol=? (card-suit (hand-c1 hd))
                         (card-suit (hand-c2 hd)))
               (symbol=? (card-suit (hand-c2 hd))
                         (card-suit (hand-c3 hd))))
          (and (= (card-rank (hand-c1 hd)) 13)     
               (or (= (card-rank (hand-c2 hd)) 12)  
                   (= (card-rank (hand-c3 hd)) 11)
                   (= (card-rank (hand-c2 hd)) 11)
                   (= (card-rank (hand-c3 hd)) 12))))
     'straight-flush]    
    [(and (symbol=? (card-suit (hand-c1 hd))
                    (card-suit (hand-c2 hd)))
          (symbol=? (card-suit (hand-c2 hd))
                    (card-suit (hand-c3 hd)))) 'flush]
    [(and (= (card-rank (hand-c1 hd)) 1)     
          (or (= (card-rank (hand-c2 hd)) 2) 
              (= (card-rank (hand-c3 hd)) 3)
              (= (card-rank (hand-c2 hd)) 3)
              (= (card-rank (hand-c3 hd)) 2))) 'straight]
    [(and (= (card-rank (hand-c1 hd)) 2)     
          (or (= (card-rank (hand-c2 hd)) 3)
              (= (card-rank (hand-c3 hd)) 4)
              (= (card-rank (hand-c2 hd)) 4)
              (= (card-rank (hand-c3 hd)) 3))) 'straight]
    [(and (= (card-rank (hand-c1 hd)) 3)     
          (or (= (card-rank (hand-c2 hd)) 4)
              (= (card-rank (hand-c3 hd)) 5)
              (= (card-rank (hand-c2 hd)) 5)
              (= (card-rank (hand-c3 hd)) 4))) 'straight]
    [(and (= (card-rank (hand-c1 hd)) 4)     
          (or (= (card-rank (hand-c2 hd)) 5)
              (= (card-rank (hand-c3 hd)) 6)
              (= (card-rank (hand-c2 hd)) 6)
              (= (card-rank (hand-c3 hd)) 5))) 'straight]
    [(and (= (card-rank (hand-c1 hd)) 5)     
          (or (= (card-rank (hand-c2 hd)) 6)
              (= (card-rank (hand-c3 hd)) 7)
              (= (card-rank (hand-c2 hd)) 7)
              (= (card-rank (hand-c3 hd)) 6))) 'straight]
    [(and (= (card-rank (hand-c1 hd)) 6)     
          (or (= (card-rank (hand-c2 hd)) 7)
              (= (card-rank (hand-c3 hd)) 8)
              (= (card-rank (hand-c2 hd)) 8)
              (= (card-rank (hand-c3 hd)) 7))) 'straight]
    [(and (= (card-rank (hand-c1 hd)) 7)     
          (or (= (card-rank (hand-c2 hd)) 8)
              (= (card-rank (hand-c3 hd)) 9)
              (= (card-rank (hand-c2 hd)) 9)
              (= (card-rank (hand-c3 hd)) 8))) 'straight]
    [(and (= (card-rank (hand-c1 hd)) 8)     
          (or (= (card-rank (hand-c2 hd)) 9)
              (= (card-rank (hand-c3 hd)) 10)
              (= (card-rank (hand-c2 hd)) 10)
              (= (card-rank (hand-c3 hd)) 9))) 'straight]
    [(and (= (card-rank (hand-c1 hd)) 9)     
          (or (= (card-rank (hand-c2 hd)) 10) 
              (= (card-rank (hand-c3 hd)) 11)
              (= (card-rank (hand-c2 hd)) 11)
              (= (card-rank (hand-c3 hd)) 10))) 'straight]
    [(and (= (card-rank (hand-c1 hd)) 10)     
          (or (= (card-rank (hand-c2 hd)) 11)
              (= (card-rank (hand-c3 hd)) 12)
              (= (card-rank (hand-c2 hd)) 12)
              (= (card-rank (hand-c3 hd)) 11))) 'straight]
    [(and (= (card-rank (hand-c1 hd)) 11)     
          (or (= (card-rank (hand-c2 hd)) 12)
              (= (card-rank (hand-c3 hd)) 13)
              (= (card-rank (hand-c2 hd)) 13)
              (= (card-rank (hand-c3 hd)) 12))) 'straight]
    [(and (= (card-rank (hand-c1 hd)) 12)     
          (or (= (card-rank (hand-c2 hd)) 13) 
              (= (card-rank (hand-c3 hd)) 11)
              (= (card-rank (hand-c2 hd)) 11)
              (= (card-rank (hand-c3 hd)) 13))) 'straight]
    [(and (= (card-rank (hand-c1 hd)) 13)     
          (or (= (card-rank (hand-c2 hd)) 12)
              (= (card-rank (hand-c3 hd)) 11)
              (= (card-rank (hand-c2 hd)) 11)
              (= (card-rank (hand-c3 hd)) 12))) 'straight]
    [(= (card-rank (hand-c1 hd))(card-rank (hand-c2 hd))
        (card-rank (hand-c3 hd))) 'three-of-a-kind] 
    [(= (card-rank (hand-c1 hd))(card-rank (hand-c2 hd))) 'pair]
    [(= (card-rank (hand-c1 hd))(card-rank (hand-c3 hd))) 'pair]
    [(= (card-rank (hand-c2 hd))(card-rank (hand-c3 hd))) 'pair]
    [else 'high-card]))  

;; Tests:
(check-expect (hand-value (make-hand (make-card 1 'spades)
                       (make-card 2 'spades)
                       (make-card 3 'spades))) 'straight-flush) 
(check-expect (hand-value (make-hand (make-card 12 'spades)
                       (make-card 1 'hearts)
                       (make-card 9 'clubs))) 'high-card)
(check-expect (hand-value (make-hand (make-card 12 'spades)
                       (make-card 1 'spades)
                       (make-card 9 'spades))) 'flush)
(check-expect (hand-value (make-hand (make-card 11 'spades)
                       (make-card 11 'hearts)
                       (make-card 11 'clubs))) 'three-of-a-kind)
(check-expect (hand-value (make-hand (make-card 11 'spades)
                       (make-card 11 'hearts)
                       (make-card 10 'clubs))) 'pair)
(check-expect (hand-value (make-hand (make-card 11 'spades)
                       (make-card 10 'hearts)
                       (make-card 11 'clubs))) 'pair)
(check-expect (hand-value (make-hand (make-card 10 'spades)
                       (make-card 11 'hearts)
                       (make-card 11 'clubs))) 'straight)
(check-expect (hand-value (make-hand (make-card 1 'spades)
                       (make-card 2 'hearts)
                       (make-card 3 'clubs))) 'straight)
(check-expect (hand-value (make-hand (make-card 1 'spades)
                       (make-card 2 'spades)
                       (make-card 3 'spades))) 'straight-flush)