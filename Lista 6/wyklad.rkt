#lang racket

;; arithmetic expressions

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

(define (arith-expr? t)
  (or (const? t)
      (and (binop? t)
           (arith-expr? (binop-left  t))
           (arith-expr? (binop-right t)))))

;; calculator

(define (op->proc op)
  (cond [(eq? op '+) +]
        [(eq? op '*) *]
        [(eq? op '-) -]
        [(eq? op '/) /]))

(define (eval-arith e)
  (cond [(const? e) e]
        [(binop? e)
         ((op->proc (binop-op e))
            (eval-arith (binop-left  e))
            (eval-arith (binop-right e)))]))

;; let expressions

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

(define (arith/let-expr? t)
  (or (const? t)
      (and (binop? t)
           (arith/let-expr? (binop-left  t))
           (arith/let-expr? (binop-right t)))
      (and (let? t)
           (arith/let-expr? (let-expr t))
           (arith/let-expr? (let-def (let-def-expr t))))
      (var? t)))

;; evalation via substitution

(define (subst e x f)
  (cond [(const? e) e]
        [(binop? e)
         (binop-cons
           (binop-op e)
           (subst (binop-left  e) x f)
           (subst (binop-right e) x f))]
        [(let? e)
         (let-cons
           (let-def-cons
             (let-def-var (let-def e))
             (subst (let-def-expr (let-def e)) x f))
           (if (eq? x (let-def-var (let-def e)))
               (let-expr e)
               (subst (let-expr e) x f)))]
        [(var? e)
         (if (eq? x (var-var e))
             f
             (var-var e))]))

(define (eval-subst e)
  (cond [(const? e) e]
        [(binop? e)
         ((op->proc (binop-op e))
            (eval-subst (binop-left  e))
            (eval-subst (binop-right e)))]
        [(let? e)
         (eval-subst
           (subst
             (let-expr e)
             (let-def-var (let-def e))
             (eval-subst (let-def-expr (let-def e)))))]
        [(var? e)
         (error "undefined variable" (var-var e))]))

;; evaluation via environments

(define empty-env
  null)

(define (add-to-env x v env)
  (cons (list x v) env))

(define (find-in-env x env)
  (cond [(null? env) (error "undefined variable" x)]
        [(eq? x (caar env)) (cadar env)]
        [else (find-in-env x (cdr env))]))

(define (if-zero? e)
  (and (list? e)
       (= 4 (length e))
       (eq? (first e) 'if-zero)))

(define (if-zero-cond e)
  (second e))

(define (if-zero-true e)
  (third e))

(define (if-zero-false e)
  (fourth e))

(define (eval-env e env)
  (cond [(if-zero? e)
         (if (= (eval-env (if-zero-cond e) env) 0)
             (eval-env (if-zero-true e) env)
             (eval-env (if-zero-false e) env))]
        [(const? e) e]
        [(binop? e)
         ((op->proc (binop-op e))
            (eval-env (binop-left  e) env)
            (eval-env (binop-right e) env))]
        [(let? e)
         (eval-env
           (let-expr e)
           (env-for-let (let-def e) env))]
        [(var? e) (find-in-env (var-var e) env)]))

(define (env-for-let def env)
  (add-to-env
    (let-def-var def)
    (eval-env (let-def-expr def) env)
    env))

(define (eval e)
  (eval-env e empty-env))


;;zad1
(define (arith-to-rpn e)
  (define (helper e acc)
    (cond [(const? e) (cons e acc)]
          [(binop? e) (helper (binop-left e) (helper (binop-right e) (cons (binop-op e) acc)))]
          [else (error " ")]))
  (helper e null))


;;zad2
(define (stack? s)
 (or (null? s)
     (and (pair? s)
          (stack? (car s)))))

(define (push elem stck)
  (cons stck elem))
  
(define (pop x)
  (cons (cdr x) (car x)))

(define empty-stack null)

(define (empty-stack? s)
  (null? s))

(define (oper? o)
  (or (eq? o '+)
      (eq? o '*)
      (eq? o '-)
      (eq? o '/)))
;;zad3
(define (eval-rpn expr)
  (define (help expr stack)
    (cond [(null? expr) (cdr stack)]
          [(number? (car expr))
           (help (cdr expr) (push (car expr) stack))]
          [(oper? (car expr))
           (let*
               ((fst (car (pop stack)))
                (scd (car (pop (cdr (pop stack)))))
                (elem ((op->proc (car expr)) scd fst)))
             (help (cdr expr) (push elem (cdr (pop (cdr (pop stack)))))))]))
  (help expr null))

(define e '(4 3 + 9 /))


(define (if-zero x t f)
  (if (= (eval x) 0)
      (eval t)
      (eval f)))





  
