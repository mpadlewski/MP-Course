#lang racket

(define (var? t)
  (symbol? t))

(define (neg? t)
  (and (list? t)
       (= 2 (length t))
       (eq? 'neg (car t))))

(define (conj? t)
  (and (list? t)
        (= 3 (length t))
        (eq? 'conj (car t))))

(define (disj? t)
  (and (list? t)
       (= 3 (length t))
       (eq? 'disj (car t))))

(define (prop? f)
  (or (var? f)
      (and (neg? f)
           (prop? (neg-subf f)))
      (and (disj? f)
            (prop? (disj-left f))
            (prop? (disj-rght f)))
      (and (conj? f)
            (prop? (conj-left f))
            (prop? (conj-rght f)))))


;;zad1
(define (make-neg t)
  (list 'neg t))

(define (neg-subf f)
  (second f))

(define (make-conj l p)
  (list 'conj l p))

(define (conj-left f)
  (second f))

(define (conj-rght f)
  (third f))

(define (make-disj l p)
  (list 'disj l p))

(define (disj-left f)
  (second f))

(define (disj-rght f)
  (third f))


;;ZAD2

(define (free-vars formula)
  (define (in-list? x l)
  (if (equal? l null)
      false
      (if (equal? (car l) x)
        true
        (in-list? x (cdr l)))))
  
  (define (fv f xs)
    (cond [(neg? f) (fv (neg-subf f) xs)]
          [(disj? f) (fv (disj-rght f) (fv (disj-left f) xs))]
          [(conj? f) (fv (conj-rght f) (fv (conj-left f) xs))]
          [(not (in-list? f xs)) (append xs (list f))]
          [else xs]))
  (fv formula null))


(define ff (make-disj (make-conj 'p 'r) (make-disj (make-neg 's) 'u)))
;;(free-vars ff)

;;ewentualnie remove-duplicates

;;ZAD3

(define (gen-vals xs)
  (if (null? xs)
      (list null)
      (let*
          ((vss (gen-vals (cdr xs)))
           (x (car xs))
           (vst (map (lambda (vs) (cons (list x true) vs)) vss))
           (vsf (map (lambda (vs) (cons (list x false) vs)) vss)))
        (append vst vsf))))

(define (get-value s eval)
  (define l (filter (lambda (x) (if (equal? (car x) s) #t #f)) eval))
  (if (equal? l null)
      l
      (cadar l)))

(define (sigma formula eval)
  (cond [(var? formula) (get-value formula eval)]
        [(disj? formula) (or (sigma (disj-rght formula) eval) (sigma (disj-left formula) eval))]
        [(conj? formula) (and (sigma (conj-rght formula) eval) (sigma (conj-left formula) eval))]
        [(neg? formula) (if (equal? (sigma (neg-subf formula) eval) #t) #f #t)]))

(define (fals-eval? form)
  (define x (filter (lambda (l) (if (eq? (sigma form l) #f) #t #f)) (gen-vals (free-vars form))))
  (if (eq? x null)
      "tautologia"
      x))

;;zad4
(define (nnf? f)
  (or (var? f)
      (and (neg? f)
           (var? (neg-subf f)))
      (and (disj? f)
            (nnf? (disj-left f))
            (nnf? (disj-rght f)))
      (and (conj? f)
            (nnf? (conj-left f))
            (nnf? (conj-rght f)))))

(define f1 (make-neg (make-disj (make-conj 'p 'r) (make-disj (make-neg 's) 'u))))
(define f2 (make-disj (make-conj 'p 'r) (make-neg (make-conj 's 'u))))

;;zad 5
