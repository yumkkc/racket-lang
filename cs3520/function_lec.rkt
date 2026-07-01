#lang plait

(define-type Exp
  (numE [n : Number])
  (idE [s : Symbol])
  (plusE [l : Exp] 
         [r : Exp])
  (multE [l : Exp]
         [r : Exp])
  (lamE [n : Symbol]
        [body : Exp])
  (appE [fun : Exp]
        [arg : Exp])
  (letE [n : Symbol] 
        [rhs : Exp]
        [body : Exp]))

(define-type Value
  (numV [n : Number])
  (closV [arg : Symbol]
         [body : Exp]
         [env : Env]))

(define-type Func-Defn
  (fd [name : Symbol] 
      [arg : Symbol] 
      [body : Exp]))

(define-type Binding
  (bind [name : Symbol]
        [val : Value]))

(define-type-alias Env (Listof Binding))  

;; An EXP is either
;; - `NUMBER
;; - `SYMBOL
;; - `{+ EXP EXP}
;; - `{* EXP EXP}
;; - `{SYMBOL EXP)
;; - `{let {[SYMBOL EXP]} EXP}

(module+ test
  (print-only-errors #t))

;; parse ----------------------------------------
(define (parse [s : S-Exp]) : Exp
  (cond
    [(s-exp-match? `NUMBER s) (numE (s-exp->number s))]
    [(s-exp-match? `SYMBOL s) (idE (s-exp->symbol s))]
    [(s-exp-match? `{+ ANY ANY} s)
     (plusE (parse (second (s-exp->list s)))
            (parse (third (s-exp->list s))))]
    [(s-exp-match? `{* ANY ANY} s)
     (multE (parse (second (s-exp->list s)))
            (parse (third (s-exp->list s))))]
    ;; function application
    [(s-exp-match? `{ANY ANY} s)
     (appE (parse (first (s-exp->list s)))
           (parse (second (s-exp->list s))))]
    [(s-exp-match? `{lambda {SYMBOL} ANY} s)
     (lamE (s-exp->symbol (first (s-exp->list
                                  (second (s-exp->list s)))))
           (parse (third (s-exp->list s))))]
    [(s-exp-match? `{let {[SYMBOL ANY]} ANY} s)
     (let ([bs (s-exp->list (first
                             (s-exp->list (second
                                           (s-exp->list s)))))])
       (letE (s-exp->symbol (first bs))
             (parse (second bs))
             (parse (third (s-exp->list s)))))]
    [else (error 'parse "invalid input")]))

(define (parse-fundef [s : S-Exp]) : Func-Defn
  (cond
    [(s-exp-match? `{define {SYMBOL SYMBOL} ANY} s)
     (fd (s-exp->symbol (first (s-exp->list (second (s-exp->list s)))))
         (s-exp->symbol (second (s-exp->list (second (s-exp->list s)))))
         (parse (third (s-exp->list s))))]
    [else (error 'parse-fundef "invalid input")]))


(module+ test
  (test (parse `2)
        (numE 2))
  (test (parse `x)
        (idE 'x))
  (test (parse `{+ 2 1})
        (plusE (numE 2) (numE 1)))
  (test (parse `{* 3 4})
        (multE (numE 3) (numE 4)))
  (test (parse `{+ {* 3 4} 8})
        (plusE (multE (numE 3) (numE 4))
               (numE 8)))
  (test (parse `{double 9})
        (appE (idE 'double) (numE 9)))
  (test (parse `{let {[x {+ 1 2}]}
                  y})
        (letE 'x (plusE (numE 1) (numE 2))
              (idE 'y)))
  (test/exn (parse `{{+ 1 2}})
            "invalid input")

  
  (test (parse  `{lambda {x} {+ x 1}})
        (lamE 'x (plusE (idE 'x) (numE 1))))
  

  (test (parse-fundef `{define {double x} {+ x x}})
        (fd 'double 'x (plusE (idE 'x) (idE 'x))))
  (test/exn (parse-fundef `{def {f x} x})
            "invalid input")

  (define double-def
    (parse-fundef `{define {double x} {+ x x}}))
  (define quadruple-def
    (parse-fundef `{define {quadruple x} {double {double x}}})))


;; interp ----------------------------------------
(define (interp [a : Exp] [env :  Env]) : Value
  (type-case Exp a
    [(numE n) (numV n)]
    [(idE s) (lookup s env)]
    [(plusE l r) (num+ (interp l env) (interp r env))]
    [(multE l r) (num* (interp l env) (interp r env))]
    [(letE n rhs body)
     (interp body (extend-env
      (bind n (interp rhs env))
      env))]
    [(lamE n body) (closV n body env)] ;; created a closure
    [(appE fun arg)
     (type-case Value (interp fun env)
       [(closV n body c-env)
        (interp body
                (extend-env
                (bind n (interp arg env))
                c-env))]
      [else (error 'interp "not a function")])]))

(module+ test
  (test (interp (parse `2)  mt-env)
        (numV 2))
  
  (test/exn (interp (parse `x) mt-env)
            "free variable")

  (test (interp (parse `x)
                (extend-env (bind 'x (numV 1)) mt-env)
                )
        (numV 1))
  
  (test (interp (parse `{+ 2 1}) mt-env)
        (numV 3))
  
  (test (interp (parse `{* 2 1}) mt-env)
        (numV 2))
  
  (test (interp (parse `{+ {* 2 3} {+ 5 8}})
                mt-env)
        (numV 19))
  )

;; helperse
(define (num+ [l : Value] [r : Value]) : Value
  (cond
    [(and (numV? l) (numV? r))
     (numV (+ (numV-n l) (numV-n r)))]
    [else
     (error 'interp "not a number")]))

(define (num* [l : Value] [r : Value]) : Value
  (cond
    [(and (numV? l) (numV? r))
     (numV (* (numV-n l) (numV-n r)))]
    [else
     (error 'interp "not a number")]))

;; Environment definition
(define mt-env empty)
(define extend-env cons)

;; lookup
(define (lookup [n : Symbol] [env : Env]) : Value
  (type-case Env env
    [empty (error 'lookup "free variable")]
    [(cons b rst-env) (cond
                        [(symbol=? (bind-name b) n)
                          (bind-val b)]
                          [else (lookup n rst-env)])]))


(module+ test
  (test/exn (lookup 'x mt-env)
            "free variable")
  (test (lookup 'x (extend-env (bind 'x (numV 1)) mt-env))
        (numV 1))
  (test (lookup 'x (extend-env (bind 'y (numV 1))
                               (extend-env (bind 'x (numV 2)) mt-env)))
        (numV 2))

  (test (lookup 'y (extend-env (bind 'y (numV 2))
                               (extend-env (bind 'y (numV 1)) mt-env)))
        (numV 2)))  

