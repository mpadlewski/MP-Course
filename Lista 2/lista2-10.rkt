#lang racket

;;PRACOWNIA 10

(define (square x)
  (* x x))

(define (inc x)
  (+ x 1))

(define (dist x y)
  (abs (- x y)))

(define (fractions num den)
  ;;obliczanie itego wyrazu ciagu
  (define (expr C1 C2 i)
    (+ (* (den i) C1) (* (num i) C2)))
  ;;sprawdzanie odleglosci miedzy Fk i Fk+1
  (define (good-enough? x y)
    (< (dist x y) 0.00001))
  
  ;;glowna procedura iteracyjna
  (define (fra-iter i Fk Fk1 Ak A1 A2 Bk B1 B2)
    (if (good-enough? Fk Fk1)
        Fk
        (let ([Aknext (expr Ak A1 (inc i))] ;;aby uzyskac przejrzystosc kodu def nowe zmienne
              [Bknext (expr Bk B1 (inc i))])
          (let ([Fknext (/ Aknext Bknext)])
            (fra-iter (inc i) Fknext Fk Aknext Ak A1 Bknext Bk B1)))))

  (fra-iter 1 (/ (expr 0 1.0 1) (expr 1.0 0 1)) 0 (expr 0 1.0 1) 0 1.0 (expr 1.0 0 1) 1.0 0))


;;TESTY

;;Złoty środek
(fractions (lambda (x) 1.0) (lambda (x) 1.0))

;;Obliczanie PI
(+ 3 (fractions (lambda (x) (square (- (* x 2) 1))) (lambda (x) 6.0)))

;;Obliczanie arctan dla x = {sqrt(3), sqrt(3)/3, 1}
(/ 1.73205 (+ 1 (fractions (lambda (x) (square (* x 1.73205))) (lambda (x) (+ x x 1)))))
(/ 0.57735 (+ 1 (fractions (lambda (x) (square (* x 0.57735))) (lambda (x) (+ x x 1)))))
(/ 1.0 (+ 1 (fractions (lambda (x) (square (* x 1.0))) (lambda (x) (+ x x 1)))))