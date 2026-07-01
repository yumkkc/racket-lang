#lang plait

(define-type GUI
  (label [text : String])
  (button [text : String]
          [enabled? : Boolean])
  (choice [items : (Listof String)]
                 [selected : Number]))

(define (read-screen [g : GUI]): (Listof String)
  (type-case GUI g
    [(label t) (list t)]
    [(button t e?) (list t)]
    [(choice i s) i]))


(test (read-screen (label "Pick a fruit:"))
      (list "Pick a fruit:"))

(test (read-screen (button "Ok" #t))
      (list "Ok"))

(test (read-screen (choice (list "Apple" "Banana" "Coconut") 0))
      (list "Apple" "Banana" "Coconut"))




