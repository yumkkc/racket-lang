#lang plait

(define-type Tree
  (leaf [val : Number])
  (node [val : Number]
        [left : Tree]
        [right : Tree]))


;; 1
(define (sum [tree : Tree])
  (type-case Tree tree
    [(leaf val) val]
    [(node val left right)
     (+ val (+ (sum left) (sum right)))]))

(test (sum (node 5 (leaf 6) (leaf 7)))
      18)
(test (sum (node 2 (leaf 8) (node 3 (leaf 10) (leaf 10))))
      33)

;; 2
(define (flip-sign [n : Number])
  (* -1 n))

(test (flip-sign 1)
      -1)

(test (flip-sign 2)
      -2)

(define (negate [tree : Tree])
  (type-case Tree tree
    [(leaf val) (leaf (flip-sign val))]
    [(node val left right)
     (node (flip-sign val) (negate left) (negate right))]))

(test (negate (node 6 (leaf 6) (leaf 7)))
      (node -6 (leaf -6) (leaf -7)))


;;3
(define (contains? [tree : Tree] [n : Number])
  (type-case Tree tree
    [(leaf val) (equal? val n)]
    [(node val left right) (or (equal? val n)
                                (or (contains? left n) (contains? right n)))]))

(test (contains? (node 5 (leaf 6) (leaf 7)) 6)
      #t)


(test (contains? (node 10 (leaf 6) (leaf 7)) 9)
      #f)


;; 4
(define (big-leaves? [tree : Tree])
  (local [(define (helper [t : Tree] [total : Number])
            (type-case Tree t
              [(leaf val) (> val total)]
              [(node val left right)
               (and
                (helper left (+ total val))
                (helper right (+ total val)))]))]
    (helper tree 0)))


(test (big-leaves? (node 5 (leaf 6) (leaf 7)))
      #t)

(test (big-leaves? (node 5 (node 2 (leaf 8) (leaf 6)) (leaf 7)))
      #f)


;; 5
(define (positive-trees? [trees : (Listof Tree)])
  (type-case (Listof Tree) trees
    [empty #t]
    [(cons tree rest-trees) (and (> (sum tree) 0)
                                 (positive-trees? rest-trees))]))


(test (positive-trees? empty)
      #t)

(test (positive-trees? (cons (leaf 6)
                             empty))
      #t)

(test (positive-trees? (cons (node 1 (leaf 6) (leaf -6))
                               empty))
        #t)

  (test (positive-trees? (cons (node 1 (leaf 6) (leaf -6))
                               (cons (node 0 (leaf 0) (leaf 1))
                                      empty)))
        #t)

  (test (positive-trees? (cons (node -1 (leaf 6) (leaf -6))
                               (cons (node 0 (leaf 0) (leaf 1))
                                      empty)))
        #f)

;; 6
(define (flatten-helper [tree : Tree] [acc : (Listof Number)])
  (type-case Tree tree
    [(leaf val) (cons val acc)]
    [(node val left right) (local
                             [(define right-acc (flatten-helper right acc))
                              (define new-acc (cons val right-acc))]
                             (flatten-helper left new-acc))]))


(define (flatten [tree : Tree])
  (flatten-helper tree empty))

(test (flatten (node 0 (leaf 1) (leaf 2)))
      '(1 0 2))


(test (flatten (node 1 (node 2 (leaf 3) (leaf 4)) (leaf 5)))
      '(3 2 4 1 5))





