#lang plait

(print-only-errors #t)

(define-type Exp
  (numE [n : Number])
  (plusE [l : Exp]
         [r : Exp])
  (multE [l : Exp]
         [r : Exp]))

(define (interp [e : Exp]) : Number
  (type-case Exp e
    [(numE n) n]
    [(plusE l r) ( + (interp l) (interp r))]
    [(multE l r) ( * (interp l) (interp r) )]))


;; 2
(test (interp (numE 2))
      2)

;; {+ 1 2}
(test (interp (plusE (numE 1) (numE 2)))
     3)

;; {* 3 4}
(test (interp (multE (numE 3) (numE 4)))
      12)

(test (interp (plusE (numE 1) (multE (numE 3) (numE 4))))
      13)

;; parsing

(define (parse (s : S-Exp))
   (cond
    [(s-exp-match? `{* ANY ANY} s)
     (multE
      (parse (second (s-exp->list s)))
      (parse (third (s-exp->list s))))]
    [(s-exp-match? `{+ ANY ANY} s)
     (plusE
      (parse (second (s-exp->list s)))
      (parse (third (s-exp->list s))))]
    [(s-exp-match? `NUMBER s) (numE (s-exp->number s))]
    [else (error 'parse "invalid input")]
    ))


;; tests
(test (parse `{+ 1 3})
      (plusE (numE 1) (numE 3)))

(test/exn (parse `{+ 1})
          "parse: invalid input")


;; overall tests
(test (interp (parse `{+ 1 2}))
      3)

(test (interp (parse `{+ 3 {* 3 3}}))
      12)

























