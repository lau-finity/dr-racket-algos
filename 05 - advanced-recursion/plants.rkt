;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname plants) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #t)))
;; A CityInfoList is a (listof CityInfo)
;; A PlantInfoList is a (listof PlantInfo)

;; 3A
(define-struct cityinfo (name zone subzone))
;; A CityInfo is a (make-cityinfo Str Nat Sym)
;; requires: fields correspond to a valid cityinfo
;;           name of city
;;           0 to 9 inclusive
;;           one of 'a or 'b

;; 3B
(define-struct plantinfo (name zone subzone))
;; A PlantInfo is a (make-plantinfo Str Nat Sym)
;; requires: fields correspond to a valid cityinfo
;;           name of city
;;           0 to 9 inclusive
;;           one of 'a or 'b

(define sample-plant-data
  (list (make-plantinfo "blue eyed grass" 5 'b)
        (make-plantinfo "hosta" 3 'a)
        (make-plantinfo "columbine" 4 'a)
        (make-plantinfo "chrysanthemum" 3 'b)
        (make-plantinfo "toad lily" 4 'b)
        (make-plantinfo "agapanthus" 8 'a)
        (make-plantinfo "liriope" 7 'b)))

(define sample-city-data
  (list (make-cityinfo "Vancouver" 8 'b)
        (make-cityinfo "Edmonton" 3 'a)  
        (make-cityinfo "Waterloo" 5 'b)
        (make-cityinfo "Saint John" 5 'a)
        (make-cityinfo "Halifax" 6 'a)
        (make-cityinfo "Happy Valley-Goose Bay" 1 'a)))

;; 3C
;; (find-hardy-plants civ pilv) produces a PlantInfoList value
;;   containing only PlantInfos for plants that will grow in the
;;   given zone
;; find-hardy-plants: CityInfo PlantInfoList -> (listof PlantInfo)
;; Examples:
(check-expect (find-hardy-plants
               (make-cityinfo "Waterloo" 0 'a) sample-plant-data)
              empty)
(check-expect (find-hardy-plants
               (make-cityinfo "Halifax" 0 'b) sample-plant-data)
              (list
               (make-plantinfo "hosta" 3 'a)
               (make-plantinfo "columbine" 4 'a)
               (make-plantinfo "agapanthus" 8 'a)))

(define (find-hardy-plants civ pilv)
  (cond
    [(empty? pilv) empty]
    [(and (symbol=? (cityinfo-subzone civ)
                    (plantinfo-subzone (first pilv)))
          (>= (cityinfo-zone civ)
              (plantinfo-zone (first pilv))))
     (cons (first pilv)(find-hardy-plants civ (rest pilv)))]
    [(and (symbol=? (cityinfo-subzone civ) 'b)
          (symbol=? (plantinfo-subzone (first pilv)) 'a))
     (cons (first pilv)(find-hardy-plants civ (rest pilv)))]
    [else
     (find-hardy-plants civ (rest pilv))])) 

;; Tests:
(check-expect (find-hardy-plants
               (make-cityinfo "Edmonton" 3 'a) sample-plant-data)
              (list (make-plantinfo "hosta" 3 'a)))
(check-expect (find-hardy-plants
               (make-cityinfo "Quebec" 4 'b) sample-plant-data)
              (list
               (make-plantinfo "hosta" 3 'a)
               (make-plantinfo "columbine" 4 'a)
               (make-plantinfo "chrysanthemum" 3 'b)
               (make-plantinfo "toad lily" 4 'b)
               (make-plantinfo "agapanthus" 8 'a)))

;; 3D
;; (find-growing-cities pi cil) produces a (listof Str) containing
;;   the names of all of the cities in which the plant will grow
;; find-growing-cities: PlantInfo CityInfoList -> (listof Str)
;; Examples:
(check-expect (find-growing-cities
               (make-plantinfo "hosta" 3 'a) sample-city-data)
              (list "Vancouver" "Edmonton" "Waterloo"
                    "Saint John" "Halifax"))
(check-expect (find-growing-cities
               (make-plantinfo "hosta" 9 'b) sample-city-data)
              empty)

(define (find-growing-cities pi cil)
  (cond
    [(empty? cil) empty]
    [(and (symbol=? (plantinfo-subzone pi)
                    (cityinfo-subzone (first cil)))
          (<= (plantinfo-zone pi)
              (cityinfo-zone (first cil))))
     (cons (cityinfo-name (first cil))
           (find-growing-cities pi (rest cil)))]
    [(and (symbol=? (plantinfo-subzone pi) 'a)
          (symbol=? (cityinfo-subzone (first cil)) 'b))
     (cons (cityinfo-name (first cil))
           (find-growing-cities pi (rest cil)))]
    [else
     (find-growing-cities pi (rest cil))]))  

;; Tests:
(check-expect (find-growing-cities
               (make-plantinfo "blue eyed grass" 5 'b)
               sample-city-data)
              (list "Vancouver" "Waterloo"))    
(check-expect (find-growing-cities
               (make-plantinfo "hosta" 0 'a)
               sample-city-data)
              (list "Vancouver" "Edmonton" "Waterloo"
               "Saint John" "Halifax" "Happy Valley-Goose Bay")) 

;; 3E
;; (find-plantless-cities pil cil) produces a CityInfoList
;;    containing all of the CityInfos in the given CityInfoList
;;    for which no plants in the PlantInfoList would survive
;;    the winter
;; find-plantless-cities: PlantInfoList CityInfoList ->
;;                       (listof CityInfos)
;; Examples:
(check-expect (find-plantless-cities
               (list (make-plantinfo "hosta" 3 'a))
               (list (make-cityinfo "Edmonton" 3 'a)))
              empty)

(check-expect (find-plantless-cities
               (list (make-plantinfo "columbine" 8 'b))
               (list))
              empty)

(define (find-plantless-cities pil cil)
  (cond
    [(or (empty? pil)
         (empty? cil)) empty]
    [(and (empty? pil)
          (empty? cil)) empty]
    [(and (symbol=? (plantinfo-subzone (first pil))
                    (cityinfo-subzone (first cil)))
          (< (plantinfo-zone (first pil))
             (cityinfo-zone (first cil))))
     (cons (first cil)
           (find-plantless-cities (rest pil)(rest cil)))]
    [(and (symbol=? (plantinfo-subzone (first pil)) 'a)
          (symbol=? (cityinfo-subzone (first cil)) 'b))
     (cons (first cil)
           (find-plantless-cities (rest pil)(rest cil)))]
    [else
     (find-plantless-cities (rest pil)(rest cil))]))

;; Tests:
(check-expect (find-plantless-cities
               (list (make-plantinfo "hosta" 3 'a)
                     (make-plantinfo "blue eyed grass" 5 'b))
               (list (make-cityinfo "Edmonton" 3 'a)
                     (make-cityinfo "Waterloo" 8 'b)))
              (list (make-cityinfo "Waterloo" 8 'b)))
(check-expect (find-plantless-cities
               (list)(list)) empty)