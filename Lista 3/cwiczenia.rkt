#lang racket

;;--- ZAD 1 ---

(define (make-rat n d)
  (let ((c (gcd n d)))
    (cons (/ n c) (cons (/ d c) null))))
  
(define (rat-numer l)
  (car l))

(define (rat-denum l)
  (cadr l))

(define (rat? l)
  (and(pair? l)
      (not (= (rat-denum l) 0))
      (= 1 (gcd (rat-numer l) (rat-denum l)))))

;;--- ZAD2 ---

(define (square x) (* x x))

(define (make-point x y)
  (cons x y))

(define (point-x p)
  (car p))

(define (point-y p)
  (cdr p))

(define (point? p)
  (and (pair? p) (number? (point-x p)) (number? (point-y p))))

( define ( display-point p )
   (display "(" )
   (display (point-x p)) 
   (display " , ")
   (display (point-y p))
   (display ")"))


(define (make-vect a b)
  (cons a b))

(define (vect-begin v)
  (car v))

(define (vect-end v)
  (cdr v))

(define (vect? v)
  (and (point? (vect-begin v)) (point? (vect-end v))))

(define (vect-len v)
  (sqrt (+ (square (- (point-x (vect-end v)) (point-x (vect-begin v))))
           (square (- (point-y (vect-end v)) (point-y (vect-begin v)))))))

(define (vect-scale v k)
  (define x (+ (car (vect-begin v))
               (* k (- (car (vect-end v)) (car (vect-begin v))))))
  (define y (+ (cdr (vect-begin v))
               (* k (- (cdr (vect-end v)) (cdr (vect-begin v))))))
  (make-vect (vect-begin v) (make-point x y)))

(define (vect-translate v p)
  (define x (+ (point-x p) (- (car (vect-end v)) (car (vect-begin v)))))
  (define y (+ (point-y p) (- (cdr (vect-end v)) (cdr (vect-begin v)))))

  (make-vect p (make-point x y)))
  

(define (display-vect v)
   (display "[")
   (display-point (vect-begin v))
   (display " , ")
   (display-point (vect-end v))
   (display "]"))

;;tests

(define a (make-point 2 4))
(define b (make-point 1 7))
(define v (make-vect a b))

(define c (make-point 10 10))

;;--- ZAD 3 ---

(define (make-vect2 p dir len)
  (list p dir len))

(define (vect2-begin v)
  (car v))

(define (vect2-dir v)
  (cadr v))

(define (vect2-len v)
  (caddr v))

(define (vect2-end v)
  (let ((x (* (vect2-len v) (cos (vect2-dir v))))
        (y (* (vect2-len v) (sin (vect2-dir v)))))
    (make-point x y)))

(define (vect2-scale v k)
  (make-vect2 (vect2-begin v) (vect2-dir v) (* (vect2-len v) k)))

(define (vect2-translate v p)
  (let ([x (- (point-x p) (point-x (vect2-begin v)))]
        [y (- (point-y p) (point-y (vect2-begin v)))])
    (make-vect2 (make-point x y) (vect2-dir v) (vect2-len v))))
 
(define (display-vect2 v)
  (display "[")
  (display-point (vect2-begin v))
  (display ", ")
  (display-point (vect2-end v))
  (display "]"))

;;TESTS
(define p (make-point 0 0))
(define u (make-vect2 p 0 1))
;;(vect2-end u)
;;(vect2-scale u 2)
;;(display-vect2 (vect2-translate u (make-point 3 4)))


;;--- ZAD 4 ---

(define (rev xs)
  (if (null? xs)
      null
      (append (rev (cdr xs)) (list (car xs)))))
  

;;TESTS
(define seq (list 1 3 5 7 9))

;;--- ZAD 5 ---

;;bierze liste i x ma zwrocic liste
(define (insert seq n)
  (define (ins seq acc)
    (if (null? seq)
        (append acc (list n))
    ;;jeszcze warunek gdy lista pusta to zwroc list n
    (if (and (< (car seq) n) (> (cadr seq) n))
        (append acc (list (car seq)) (list n) (cdr seq))
        (ins (cdr seq) (append acc (list (car seq)))))))
  (ins seq '()))


(fold + 0 '(1 2 3))
;;(fold insert (lista) '())









  