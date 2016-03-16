;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname rpsls) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; (rpsls p1 p2) produces a result of either {'tie, 'player1, or
;;   'player2} corresponding to the winner of the game of RPSLS
;;   {'rock, 'paper, 'scissors, 'lizard, 'spock} 
;; rpsls: Sym Sym -> Sym
;; requires: p1 and p2 be one of the following symbols:
;;            - rock
;;            - paper
;;            - scissors
;;            - lizard
;;            - spock
;; Examples:
(check-expect (rpsls 'lizard 'lizard) 'tie)
(check-expect (rpsls 'spock 'rock) 'player1)

(define (rpsls p1 p2)
  (cond
    [(symbol=? p1 'scissors)
     (cond
       [(symbol=? p2 'scissors) 'tie]
       [(symbol=? p2 'paper) 'player1]
       [(symbol=? p2 'lizard) 'player1]
       [(symbol=? p2 'spock) 'player2]
       [(symbol=? p2 'rock) 'player2])]
    [(symbol=? p1 'paper) 
     (cond
       [(symbol=? p2 'paper) 'tie]
       [(symbol=? p2 'rock) 'player1]
       [(symbol=? p2 'spock) 'player1]
       [(symbol=? p2 'scissors) 'player2]
       [(symbol=? p2 'lizard) 'player2])]
    [(symbol=? p1 'rock)
     (cond
       [(symbol=? p2 'rock) 'tie]
       [(symbol=? p2 'lizard) 'player1] 
       [(symbol=? p2 'scissors) 'player1]
       [(symbol=? p2 'paper) 'player2]
       [(symbol=? p2 'spock) 'player2])]
    [(symbol=? p1 'lizard) 
     (cond
       [(symbol=? p2 'lizard) 'tie]
       [(symbol=? p2 'spock) 'player1]
       [(symbol=? p2 'paper) 'player1]
       [(symbol=? p2 'scissors) 'player2]
       [(symbol=? p2 'rock) 'player2])]
    [(symbol=? p1 'spock)
     (cond
       [(symbol=? p2 'spock) 'tie]
       [(symbol=? p2 'scissors) 'player1]
       [(symbol=? p2 'rock) 'player1]
       [(symbol=? p2 'lizard) 'player2]
       [(symbol=? p2 'paper) 'player2])])) 

;; Tests:
(check-expect (rpsls 'lizard 'lizard) 'tie)
(check-expect (rpsls 'lizard 'scissors) 'player2)
(check-expect (rpsls 'scissors 'paper) 'player1)
(check-expect (rpsls 'rock 'scissors) 'player1)