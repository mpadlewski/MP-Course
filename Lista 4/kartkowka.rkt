#lang racket

;;zaimplementuj drzewo binarne
;;konstruktory, selektory, predykaty ktore przechowuja wartosci w lisciach
;;funkcja zliczajaca ile w drzewie znajduje sie zer

(define (make-bt left right)
  (list 'node left right))

(define (bt-left tree)
  (second tree))

(define (bt-right tree)
  (third tree))

(define (btree? t)
  (or (leaf? t)
      (and (list? t)
           (= (length t) 3)
           (eq? (first t) 'node)
           (btree? (bt-left t))
           (btree? (bt-right t)))))

(define (make-leaf val)
  (list 'leaf val))

(define (leaf-val l)
  (second l))

(define (leaf? l)
  (and (list? l)
       (= (length l) 2)
       (eq? (first l) 'leaf)))

(define (how-many-0 t)
    (cond [(and (leaf? t) (= (leaf-val t) 0)) 1]
          [(and (leaf? t) (not (= (leaf-val t) 0))) 0]
          [else (+ (how-many-0 (bt-left t))
                   (how-many-0 (bt-right t)))]))

;;TESTS
(define tree1 (make-bt (make-bt (make-leaf 3) (make-leaf 0)) (make-bt (make-leaf 1) (make-leaf 1))))
(define tree2 (make-bt (make-bt (make-leaf 0) (make-leaf 0)) (make-bt (make-leaf 2) (make-leaf 9))))
(define tree3 (make-bt (make-bt tree1 (make-leaf 0)) (make-bt (make-leaf 42) tree2)))
;;RESULTS
(how-many-0 tree1)
(how-many-0 tree2)
(how-many-0 tree3)