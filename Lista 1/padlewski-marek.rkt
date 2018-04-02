#lang racket

(define (dist x y)
  (abs (- x y)))

(define (cube x)
  (* x x x))

(define (square x)
  (* x x))

(define (cube-root x)
  (define (improve approx)
    ;;lepsze przyblizenie
    (/ (+ (/ x (square approx)) (* 2 approx)) 3))
  (define (good-enough? approx)
    (< (dist x (cube approx)) 0.0001))
  (define (iter approx)
    ;; główna procedura
    (cond
      [(good-enough? approx) approx]
      [else                  (iter (improve approx))]))
  
  (iter 1.0))

;;testy
;; x    (cube-root x)
;; 0    0.03901844231062338
;; 1    1.0
;; 8    2.000004911675504
;; 15   2.466213896188235
;; 27   3.0000005410641766
;; 64   4.000000000076121
;; 1000 10.000000145265767