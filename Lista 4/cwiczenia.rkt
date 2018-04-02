#lang racket

(define (btree? t)
  (or (eq? t 'leaf)
      (and (list? t)
           (= 4 (length t))
           (eq? (car t) 'node)
           (btree? (caddr t))
           (btree? (cadddr t)))))
#|zad3
(define (mirror t)
  (if (leaf? t)
      leaf
      (make-tree tree-val (mirror (btree-left t)) (mirror (btree-right t)))))|#

(define (flatten t)
  (define (flat acc t)
    (if (leaf? t)
        (cons (value t) acc)
        (flat (flat acc (right-node t)) (left-node t))))
  (flat null t))
        

;;insert
;;flatten
;;isc po liscie i kazdy element wstawic do drzewa binarnego
;; flatten po drzewie

;;zad5

(define (treesort l)
  (define (sort acc l)
    (if (= (cdr l) null)
        acc
        (sort (insert (car t)) (cdr l))))
  (flatten (sort null l)))

(define (treesort2 l)
  (flatten (foldl insert leaf l)))
  
  
      