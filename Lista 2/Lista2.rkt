#lang racket

(define (cont-frac-rec num den k)
  (if (= k 1)
      (/ (num k) (den k))
      (/ num (+ den (cont-frac-rec num den (- k 1))))))

;;zad 4
(define (product term next s e acc)
  (define (suma s acc)
    (if (> s e)
        acc
        (suma (next s) (+ acc (term s)))))
  (suma s 0))

;;zad 5
(define (acc comb term next s e nullv)
  (define (acca s temp)
    (if (> s e)
        temp
        (acca (next s) (comb (+ temp (term s))))))
  (acca s nullv))

;;zad 6-7
(define (cont-frac num den k)
  (define (cont acc i)
  (if (= i 0)
      acc
      (cont (/ (num i) (+ (den i) acc)) (- i 1))))
  (cont 0 k))


  










  
