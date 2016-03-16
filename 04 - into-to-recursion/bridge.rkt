;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname bridge) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; 3A
;; A BridgeHand is one of:
;; * empty
;; * (cons bhe BridgeHand), where bhe is a BridgeHandElement

;; A BridgeHandElement is one of:
;; * a Str
;; * a Sym, one of 'Ace, 'King, 'Queen, or 'Jack
;; * a Nat in the range 2 to 10 inclusive.

(define (bridge-hand bhlst)
  (cond
    [(empty? bhlst) ...]
    [(not (symbol? (first bhlst)))
     (count-points (rest bhlst))]
    [(symbol=? 'Ace (first bhlst))
     (... (count-points (rest bhlst)) ...)]
    [(symbol=? 'King (first bhlst))
     (... (count-points (rest bhlst)) ...)]
    [(symbol=? 'Queen (first bhlst))
     (... (count-points (rest bhlst)) ...)]
    [(symbol=? 'Jack (first bhlst))
     (... (count-points (rest bhlst)) ...)]))      

;; 3B
;; (count-points bhlst) produces the number of points that the
;;    BridgeHand is worth, points for a hand are based only on the
;;    total of the high cards in your hand
;; count-points: (listof Any) -> Num
;; Examples:
(check-expect (count-points (cons "Diamonds Start" (cons 8
                            (cons 'King (cons 9 (cons 'Jack
                            (cons "Diamonds End" empty))))))) 4)
(check-expect (count-points (cons "Clubs Start" (cons "Clubs End"
                            empty))) 0)
(check-expect (count-points (cons "Clubs Start" (cons "Diamonds End"
                            empty))) 0)                            

(define (count-points bhlst) 
  (cond
    [(empty? bhlst) 0]
    [(not (symbol? (first bhlst)))
     (count-points (rest bhlst))]
    [(symbol=? 'Ace (first bhlst))
     (+ 4 (count-points (rest bhlst)))]
    [(symbol=? 'King (first bhlst))
     (+ 3 (count-points (rest bhlst)))]
    [(symbol=? 'Queen (first bhlst))
     (+ 2 (count-points (rest bhlst)))]
    [(symbol=? 'Jack (first bhlst))
     (+ 1 (count-points (rest bhlst)))]))
    
;; Tests:
(check-expect (count-points (cons "Clubs Start" (cons 8
                            (cons 'King (cons 'Ace
                            (cons "Clubs End" (cons "Hearts Start"
                            (cons 4 (cons "Hearts End"
                            (cons "Diamonds Start" (cons 4
                            (cons 'Jack (cons "Diamonds End"
                            empty))))))))))))) 8)
(check-expect (count-points (cons "Hearts Start" (cons 9
                            (cons 'Queen (cons 'Ace
                            (cons 'Jack (cons 'King
                            (cons "Hearts End" empty)))))))) 10)
(check-expect (count-points (cons "Hearts Start" (cons 1
                            (cons 2 (cons 3 (cons 5 (cons 9
                            empty))))))) 0)