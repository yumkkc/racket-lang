#lang plait

(define-type Exp
  (numE [n : Number])
  (idE [s : Symbol])
  (plusE [l : Exp] 
         [r : Exp])
  (multE [l : Exp]
         [r : Exp])
  (maxE [l : Exp]
        [r : Exp])
  (appE [s : Symbol]
        [arg : (Listof Exp)]))

(define-type Func-Defn
  (fd [name : Symbol] 
      [arg : (Listof Symbol)] 
      [body : Exp]))

(module+ test
  (print-only-errors #t))

;; An EXP is either
;; - `NUMBER
;; - `SYMBOL
;; - `{+ EXP EXP}
;; - `{* EXP EXP}
;; - `{SYMBOL EXP)

;; A FUNC-DEFN is
;; - `{define {SYMBOL SYMBOL} EXP}

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
    [(s-exp-match? `{max ANY ANY} s)
     (maxE (parse (second (s-exp->list s)))
            (parse (third (s-exp->list s))))]
    [(s-exp-match? `{SYMBOL ANY ...} s)
     (appE (s-exp->symbol (first (s-exp->list s)))
           (map parse (rest (s-exp->list s))))]
    [else (error 'parse "invalid input")]))

(define (exists? [x : S-Exp] [xs : (Listof Symbol)])
  (type-case (Listof Symbol) xs
    [empty #f]
    [(cons y rst) (or (equal? (s-exp->symbol x) y)
                      (exists? x rst))]))

(define (parse-fundef [s : S-Exp]) : Func-Defn
  (cond
    [(s-exp-match? `{define {SYMBOL SYMBOL ...} ANY} s)
     (fd (s-exp->symbol (first (s-exp->list (second (s-exp->list s)))))
         (foldl (λ (arg args) (if (exists? arg args)
                                  (error 'parse-fundef "same arguments repeated")
                                  (cons (s-exp->symbol arg) args)))
                  empty
                  (reverse (rest (s-exp->list (second (s-exp->list s))))))
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
        (appE 'double (list (numE 9))))
  (test (parse `{max 9 10})
        (maxE (numE 9) (numE 10)))

  (test (parse `{max {+ 1 10} {* 9 11}})
        (maxE (plusE (numE 1) (numE 10))
              (multE (numE 9) (numE 11))))
  
  (test/exn (parse `{{+ 1 2}})
            "invalid input")

  (test (parse-fundef `{define {double x} {+ x x}})
        (fd 'double (list 'x) (plusE (idE 'x) (idE 'x))))
  (test/exn (parse-fundef `{def {f x} x})
            "invalid input")

  (test/exn (parse-fundef `{define {f x x} {+ x x}})
            "same arguments repeated")

  (define double-def
    (parse-fundef `{define {double x} {+ x x}}))
  (define quadruple-def
    (parse-fundef `{define {quadruple x} {double {double x}}})))

;; interp ----------------------------------------
(define (interp [a : Exp] [defs : (Listof Func-Defn)]) : Number
  (type-case Exp a
    [(numE n) n]
    [(idE s) (error 'interp "free variable")]
    [(plusE l r) (+ (interp l defs) (interp r defs))]
    [(multE l r) (* (interp l defs) (interp r defs))]
    [(maxE l r) (local [(define l-exp (interp l defs))
                  (define r-exp (interp r defs))]
                  (if (> l-exp r-exp)
                      l-exp
                      r-exp))]
    [(appE s args) (local [
                           (define fd (get-fundef s defs))
                           (define new-args (map (λ (x) (numE (interp x defs))) args))
                           (define zipped-args (if (equal? (length new-args) (length (fd-arg fd)))
                                                (map2 (λ (x y) (pair x y)) new-args (fd-arg fd))
                                                (error 'interp "wrong arity")))
                           (define new-body (foldl (λ (x y)
                                                     (subst (fst x) (snd x) y))
                                                   (fd-body fd)
                                                   zipped-args))]
                    (interp new-body defs))]))

(module+ test
  (test (interp (parse `2) empty)
        2)
  (test/exn (interp (parse `x) empty)
            "free variable")
  (test (interp (parse `{+ 2 1}) empty)
        3)
  (test (interp (parse `{* 2 1}) empty)
        2)
  (test (interp (parse `{+ {* 2 3}
                           {+ 5 8}})
                empty)
        19)
  (test (interp (parse `{double 8})
                (list double-def))
        16)
  (test (interp (parse `{quadruple 8})
                (list double-def quadruple-def))
        32)
  (test (interp (parse `{max 1 2})
                (list))
        2)

  (test (interp (parse `{max {+ 4 5} {+ 2 3}})
                (list))
        9)
  )

;; get-fundef ----------------------------------------
(define (get-fundef [s : Symbol] [defs : (Listof Func-Defn)]) : Func-Defn
  (type-case (Listof Func-Defn) defs
    [empty (error 'get-fundef "undefined function")]
    [(cons def rst-defs) (if (eq? s (fd-name def))
                             def
                             (get-fundef s rst-defs))]))

(module+ test
  (test (get-fundef 'double (list double-def))
        double-def)
  (test (get-fundef 'double (list double-def quadruple-def))
        double-def)
  (test (get-fundef 'double (list quadruple-def double-def))
        double-def)
  (test (get-fundef 'quadruple (list quadruple-def double-def))
        quadruple-def)
  (test/exn (get-fundef 'double empty)
            "undefined function"))

;; subst ----------------------------------------
(define (subst [what : Exp] [for : Symbol] [in : Exp])
  (type-case Exp in
    [(numE n) in]
    [(idE s) (if (eq? for s)
                 what
                 in)]
    [(plusE l r) (plusE (subst what for l)
                        (subst what for r))]
    [(multE l r) (multE (subst what for l)
                        (subst what for r))]
    [(maxE l r) (maxE (subst what for l)
                      (subst what for r))]
    [(appE s args) (appE s (map (λ (arg) (subst what for arg)) args))]))

(module+ test
  (test (subst (parse `8) 'x (parse `9))
        (numE 9))
  (test (subst (parse `8) 'x (parse `x))
        (numE 8))
  (test (subst (parse `8) 'x (parse `y))
        (idE 'y))
  (test (subst (parse `8) 'x (parse `{+ x y}))
        (parse `{+ 8 y}))
  (test (subst (parse `8) 'x (parse `{* y x}))
        (parse `{* y 8}))
  (test (subst (parse `8) 'x (parse `{double x}))
        (parse `{double 8}))

  (test (subst (parse `10) 'x (parse `{max x 0}))
        (parse `{max 10 0}))

  (test (interp (parse `{f 1 2})
                (list (parse-fundef `{define {f x y} {+ x y}})))
        3)

  (test (interp (parse `{+ {f} {f}})
                (list (parse-fundef `{define {f} 5})))
        10)
  (test (interp (parse `{f 2 3})
                (list (parse-fundef `{define {f x y} {+ y {+ x x}}})))
        7)
  (test/exn (interp (parse `{f 1})
                    (list (parse-fundef `{define {f x y} {+ x y}})))
            "wrong arity")
  
  )

