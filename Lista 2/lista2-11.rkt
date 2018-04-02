#lang racket

;;ZAD 11

(define (identity x) x)

(define (inc x)
  (+ x 1))

(define (dec x)
  (- x 1))

(define (dist x y)
  (abs (- x y)))

(define (compose f g)
  (lambda (x) (f (g x))))

(define (repeated p n)
  (if (= 0 n)
      (identity p)
      (average-damp (repeated p (- n 1)))))

(define (close-enough? x y)
  (< (dist x y) 0.00001))

;;obliczanie podłogi z log2(x)
(define (log2-floor x)
  (define (log-iter i)
    (if (> (expt 2 i) x)
        (- i 1)
        (log-iter (inc i))))
  (log-iter 0))

;; obliczanie (przybliżonego) punktu stałego funkcji f
(define (fix-point f x0)
  (let ((x1 (f x0)))
    (if (close-enough? x0 x1)
        x0
        (fix-point f x1))))

;; tlumienie z uśrednieniem
(define (average-damp f)
  (lambda (x) (/ (+ x (f x)) 2)))

;; obliczanie pierwiastka n stopnia
(define (nth-root x n)
  (cond [(= x 0) 0]
        [(or (and (< x 0) (= (modulo n 2) 0)) (< n 1)) (print "WRONG INPUT")]
        [else (fix-point (repeated (average-damp (lambda (y) (/ x (expt y (dec n)))))
                                   (- (log2-floor n) 1)) 1.0)]))

;;TESTY

#|
(define (sqrt-ad x)
  (fix-point (repeated (average-damp (lambda (y) (/ x (expt y (- n 1))))) 0) 1.0))

Testując powyzsza procedure dla kolejnych poteg y i kolejnych powtorzen tlumienia
otrzymałem m.in. wyniki:
4-7th root : 1 złożenie - zapętlenie
             2 złożenia - działa
8-15th root: 2 złożenia - zapętlenie
             3 złożenia - działa
16th root  : 3 złożenia - zapętlenie
             4 złożenia - działa

Po tych testach miałem podejrzenie, że ilość złożeń jest równa podłodze z log2(n),
co potwierdziły także informacje znalezione na internecie.|#

(display "Testy:\n")
(nth-root 0 2)
(nth-root 1 3)
(nth-root 9 2)
(nth-root 8 3)
(nth-root -8 3)
(nth-root 256 4)
(nth-root -32 5)
(nth-root 77799 17)
