;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname bst) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #t)))
(define-struct node (key val left right))

;; 2A
;; (root-at-smallest bst) produces another BST by rearranging so that
;;   its smallest element is at the root
;; root-at-smallest: BST -> (anyof BST empty)
;; Examples:
(check-expect (root-at-smallest 
               (make-node 5 "5" (make-node 3 "3" empty empty) empty)) 
              (make-node 3 "3" empty (make-node 5 "5" empty empty)))
(check-expect (root-at-smallest empty) empty)
(check-expect (root-at-smallest
               (make-node 3 "3" empty empty))
              (make-node 3 "3" empty empty))

(define (root-at-smallest bst)
  (local [(define (bst-is bst main)
            (local [(define (rest-of-tree root-key main)
                      (cond
                        [(empty? main) empty]
                        [(= root-key (node-key main)) empty]
                        [else
                         (make-node (node-key main)
                                    (node-val main)
                                    (rest-of-tree root-key (node-left main))
                                    (rest-of-tree root-key (node-right main)))]))]
              (cond
                [(empty? (node-left (node-left bst)))
                 (make-node (node-key (node-left bst))
                            (node-val (node-left bst))
                            empty
                            (rest-of-tree (node-key (node-left bst)) main))]
                [else (bst-is (node-left bst) main)])))]
    (cond
      [(empty? bst) empty]
      [(empty? (node-left bst)) bst]
      [else (bst-is bst bst)])))      

;; Tests:
(check-expect (root-at-smallest
               (make-node 3 "3" empty (make-node 5 "5" empty empty)))
              (make-node 3 "3" empty (make-node 5 "5" empty empty)))
(check-expect (root-at-smallest
               (make-node 5 "5" 
                          (make-node 3 "3"
                                     (make-node 2 "2" empty empty)
                                     empty) empty))
              (make-node 2 "2" empty
                         (make-node 5 "5"
                                    (make-node 3 "3" empty empty)
                                    empty)))
(check-expect (root-at-smallest (make-node 9 "9"
                             (make-node 8 "8"
                                        (make-node 4 "4" empty empty)
                                        empty)
                             (make-node 39 "39"
                                        (make-node 11 "11" empty
                                                   empty)
                                        (make-node 165 "165" empty
                                                   empty))))
              (make-node 4 "4" empty
                         (make-node 9 "9"
                                    (make-node 8 "8" empty
                                               empty)
                                    (make-node 39 "39"
                                               (make-node 11 "11"
                                                          empty empty)
                                               (make-node 165 "165"
                                                          empty
                                                          empty)))))

;; 2B
;; (bst-remove key bst) produces the BST with the node containing the
;;   given key removed (produce the original tree unchanged if the
;;   key is not present)
;; bst-remove: Num BST -> (anyof BST empty)
;; Examples:

(define (bst-remove key bst)
  (make-node 3 "3" (make-node 1 "1" empty empty) (make-node 5 "5" empty empty)))

;; Tests:
(check-expect (bst-remove 2 (make-node 2 "2"
                         (make-node 1 "1" empty empty)
                         (make-node 5 "5" (make-node 3 "3" empty empty)
                                    empty)))
              (make-node 3 "3" (make-node 1 "1" empty empty)
                         (make-node 5 "5" empty empty)))