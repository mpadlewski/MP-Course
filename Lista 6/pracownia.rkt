#lang racket

(define (const? t)
  (number? t))

(define (binop? t)
  (and (list? t)
       (= (length t) 3)
       (member (car t) '(+ - * /))))

(define (binop-op e)
  (car e))

(define (binop-left e)
  (cadr e))

(define (binop-right e)
  (caddr e))

(define (binop-cons op l r)
  (list op l r))

(define (op->proc op)
  (cond [(eq? op '+) +]
        [(eq? op '*) *]
        [(eq? op '-) -]
        [(eq? op '/) /]))

(define (let-def? t)
  (and (list? t)
       (= (length t) 2)
       (symbol? (car t))))

(define (let-def-var e)
  (car e))

(define (let-def-expr e)
  (cadr e))

(define (let-def-cons x e)
  (list x e))

(define (let? t)
  (and (list? t)
       (= (length t) 3)
       (eq? (car t) 'let)
       (let-def? (cadr t))))

(define (let-def e)
  (cadr e))

(define (let-expr e)
  (caddr e))

(define (let-cons def e)
  (list 'let def e))

(define (var? t)
  (symbol? t))

(define (var-var e)
  e)

(define (var-cons x)
  x)

(define (hole? t)
  (eq? t 'hole))

(define (arith/let/holes? t)
  (or (hole? t)
      (const? t)
      (and (binop? t)
           (arith/let/holes? (binop-left  t))
           (arith/let/holes? (binop-right t)))
      (and (let? t)
           (arith/let/holes? (let-expr t))
           (arith/let/holes? (let-def-expr (let-def t))))
      (var? t)))

(define (num-of-holes t)
  (cond [(hole? t) 1]
        [(const? t) 0]
        [(binop? t)
         (+ (num-of-holes (binop-left  t))
            (num-of-holes (binop-right t)))]
        [(let? t)
         (+ (num-of-holes (let-expr t))
            (num-of-holes (let-def-expr (let-def t))))]
        [(var? t) 0]))

(define (arith/let/hole-expr? t)
  (and (arith/let/holes? t)
       (= (num-of-holes t) 1)))

(define (hole-context e)
  ;; glowna procedura przechodzaca po wyrazeniu
  (define (find-context e vars)
    (cond [(hole? e) vars] ;; jesli trafimy na dziure to wyrzucamy liste zmiennych
          [(let? e) ;; jesli trafimy na let wyrazenie
           (if (arith/let/hole-expr? (let-def-expr (let-def e))) 
               (find-context (let-def-expr (let-def e)) vars)
               (find-context (let-expr e)
                             (let* ((new-var (let-def-var (let-def e))))
                               (cons new-var vars))))]
          [(binop? e) ;; jesli trafimy na operacje
           (if (arith/let/hole-expr? (binop-left e))
               (find-context (binop-left e) vars)
               (find-context (binop-right e) vars))]))

  ;; wywolujemy procedure i usuwamy duplikaty z listy
  (remove-duplicates (find-context e null)))

(define (test)
  (define tests-list ;; lista testow
    ;; kazdy test sklada sie z wyrazenia do sprawdzenia i poprawnego wyniku
    (list
     (list '(+ 3 hole) null)
     (list '(let (x 17) hole) '(x))
     (list '(let (z (let (y (let (x 10) (+ y 10))) x)) hole) '(z))
     (list '(let (a 1) (let (c 7) (let (a 9) (let (b 5) hole)))) '(a b c))
     (list '(let (x (let (y hole) 42)) (+ 2 3)) null)
     (list '(let (x 123) (let (y hole) (let (z 2) z))) '(x))
     (list '(let (x 7) (let (y (+ 3 x)) hole)) '(x y))
     (list '(let (x hole) (let (y 10 y))) null)
     (list '(+ (let (x (let (y 0) hole)) 6) 7) '(y))
     (list '(* (let (z 42) (- 4 2)) hole) null)
     (list '(/ hole (let (x 12) x)) null)))
     
  ;; iteracja po wszystkich testach i sprawdzanie poprawnosci wynikow
  (define (tests-iter t i)
    (cond [(null? t) (displayln "All tests passed")]
          [(let*
               ((expr (first (first t)))
                (good-res (second (first t)))
                (test-res  (sort (hole-context expr) symbol<?))) ;; sortujemy wynik testu aby kolejnosc zmiennych nie miala znaczenia
             (equal? good-res test-res)) ;; jesli test poprawny
           (tests-iter (cdr t) (+ i 1))] ;; przechodzimy do kolejnego
          [else (displayln (list 'Test i 'failed))])) ;; jesli nie poprawny, wypisujemy nr testu

  (tests-iter tests-list 1))
             

(test)