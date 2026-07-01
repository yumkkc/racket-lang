#lang plait

;; determinte the representation
;; write examples (test)
;; create a template for the implementation
;; finish body implementation case by case
;; run tests

;; keep track of the number of cookies in a cookie jar
(define (eat-cookie [n : Number])
  (if (> n 0)
      (- n 1)
      0))

(test (eat-cookie 10)
      9)

(test (eat-cookie 00)
      0)
(test (eat-cookie 1)
      0)

 ;;track a position on the screen
(define-type Posn
  (posn [x : Number]
        [y : Number]))

;; flip : (Posn -> Posn)

(define (flip [p : Posn])
  (type-case Posn p
    [(posn x y) (posn y x)]))

(test (flip (posn 1 17))
            (posn 17 1)) 

(test (flip (posn -3 4))
            (posn 4 -3))

