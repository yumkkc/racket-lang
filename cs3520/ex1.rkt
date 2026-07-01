#lang plait

;; part 1
(define (3rd-power (n : Number))
  (* n (* n n)))

(test (3rd-power 17)
      4913)

;; part 2

(define (42nd-power [n : Number])
  (local [(define (helper [counter : Number])
            (if (equal? counter 0)
                1
                (* (3rd-power n) (helper (- counter 1)))))]
    (helper 14)
    ))

(test (42nd-power 17)
      4773695331839566234818968439734627784374274207965089)

;; part 3
(define (plural [s : String])
  (local [(define chs (reverse(string->list s)))]
    (cond
      [(equal? (first chs) #\y) (list->string (reverse (append (string->list "sei") (rest chs))))]
      [else (list->string (reverse (cons #\s chs)))])))

;; part 4
(define-type Light
  (bulb [watts : Number]
        [technology : Symbol])
  (candle [inches : Number]))

(define (energy-usage [source : Light])
  (type-case Light source
    [(bulb w t) (/ (* 24 w) 1000)]
    [(candle i) 0.0]))

(test (energy-usage (bulb 100.0 'halogen))
      2.4)

(test (energy-usage (candle 10.0))
      0.0)


;; 5
(define (use-for-one-hour [source : Light])
  (type-case Light source
    [(bulb w t) (bulb w t)]
    [(candle i) (candle (- i 1))]))

(test (use-for-one-hour (bulb 100.0 'halogen))
      (bulb 100.0 'halogen))

(test (use-for-one-hour (candle 10.0))
      (candle 9.0))
    