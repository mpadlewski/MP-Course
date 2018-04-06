#lang racket

;; pomocnicza funkcja dla list tagowanych o określonej długości
(define (tagged-tuple? tag len p)
  (and (list? p)
       (= (length p) len)
       (eq? (car p) tag)))

(define (tagged-list? tag p)
  (and (pair? p)
       (eq? (car p) tag)
       (list? (cdr p))))

;; reprezentacja danych wejściowych (z ćwiczeń)
(define (var? x)
  (symbol? x))

(define (var x)
  x)

(define (var-name x)
  x)

;; przydatne predykaty na zmiennych
(define (var<? x y)
  (symbol<? x y))

(define (var=? x y)
  (eq? x y))

(define (literal? x)
  (and (tagged-tuple? 'literal 3 x)
       (boolean? (cadr x))
       (var? (caddr x))))

(define (literal pol x)
  (list 'literal pol x))

(define (literal-pol x)
  (cadr x))

(define (literal-var x)
  (caddr x))

(define (clause? x)
  (and (tagged-list? 'clause x)
       (andmap literal? (cdr x))))

(define (clause . lits)
  (cons 'clause lits))

(define (clause-lits c)
  (cdr c))

(define (cnf? x)
  (and (tagged-list? 'cnf x)
       (andmap clause? (cdr x))))

(define (cnf . cs)
    (cons 'cnf cs))

(define (cnf-clauses x)
  (cdr x))

;; oblicza wartość formuły w CNF z częściowym wartościowaniem. jeśli zmienna nie jest
;; zwartościowana, literał jest uznawany za fałszywy.
(define (valuate-partial val form)
  (define (val-lit l)
    (let ((r (assoc (literal-var l) val)))
      (cond
       [(not r)  false]
       [(cadr r) (literal-pol l)]
       [else     (not (literal-pol l))])))
  (define (val-clause c)
    (ormap val-lit (clause-lits c)))
  (andmap val-clause (cnf-clauses form)))

;; reprezentacja dowodów sprzeczności

(define (axiom? p)
  (tagged-tuple? 'axiom 2 p))

(define (proof-axiom c)
  (list 'axiom c))

(define (axiom-clause p)
  (cadr p))

(define (res? p)
  (tagged-tuple? 'resolve 4 p))

(define (proof-res x pp pn)
  (list 'resolve x pp pn))

(define (res-var p)
  (cadr p))

(define (res-proof-pos p)
  (caddr p))

(define (res-proof-neg p)
  (cadddr p))

;; sprawdza strukturę, ale nie poprawność dowodu
(define (proof? p)
  (or (and (axiom? p)
           (clause? (axiom-clause p)))
      (and (res? p)
           (var? (res-var p))
           (proof? (res-proof-pos p))
           (proof? (res-proof-neg p)))))

;; procedura sprawdzająca poprawność dowodu
(define (check-proof pf form)
  (define (run-axiom c)
    (displayln (list 'checking 'axiom c))
    (and (member c (cnf-clauses form))
         (clause-lits c)))
  (define (run-res x cpos cneg)
    (displayln (list 'checking 'resolution 'of x 'for cpos 'and cneg))
    (and (findf (lambda (l) (and (literal-pol l)
                                 (eq? x (literal-var l))))
                cpos)
         (findf (lambda (l) (and (not (literal-pol l))
                                 (eq? x (literal-var l))))
                cneg)
         (append (remove* (list (literal true x))  cpos)
                 (remove* (list (literal false x)) cneg))))
  (define (run-proof pf)
    (cond
     [(axiom? pf) (run-axiom (axiom-clause pf))]
     [(res? pf)   (run-res (res-var pf)
                           (run-proof (res-proof-pos pf))
                           (run-proof (res-proof-neg pf)))]
     [else        false]))
  (null? (run-proof pf)))


;; reprezentacja wewnętrzna

;; sprawdza posortowanie w porządku ściśle rosnącym, bez duplikatów
(define (sorted? vs)
  (or (null? vs)
      (null? (cdr vs))
      (and (var<? (car vs) (cadr vs))
           (sorted? (cdr vs)))))

(define (sorted-varlist? x)
  (and (list? x)
       (andmap var? x)
       (sorted? x)))

;; klauzulę reprezentujemy jako parę list — osobno wystąpienia pozytywne i negatywne. Dodatkowo
;; pamiętamy wyprowadzenie tej klauzuli (dowód) i jej rozmiar.
(define (res-clause? x)
  (and (tagged-tuple? 'res-int 5 x)
       (sorted-varlist? (second x))
       (sorted-varlist? (third x))
       (= (fourth x) (+ (length (second x)) (length (third x))))
       (proof? (fifth x))))

(define (res-clause pos neg proof)
  (list 'res-int pos neg (+ (length pos) (length neg)) proof))

(define (res-clause-pos c)
  (second c))

(define (res-clause-neg c)
  (third c))

(define (res-clause-size c)
  (fourth c))

(define (res-clause-proof c)
  (fifth c))

;; przedstawia klauzulę jako parę list zmiennych występujących odpowiednio pozytywnie i negatywnie
(define (print-res-clause c)
  (list (res-clause-pos c) (res-clause-neg c)))

;; sprawdzanie klauzuli sprzecznej
(define (clause-false? c)
  (and (null? (res-clause-pos c))
       (null? (res-clause-neg c))))

;; pomocnicze procedury: scalanie i usuwanie duplikatów z list posortowanych
(define (merge-vars xs ys)
  (cond [(null? xs) ys]
        [(null? ys) xs]
        [(var<? (car xs) (car ys))
         (cons (car xs) (merge-vars (cdr xs) ys))]
        [(var<? (car ys) (car xs))
         (cons (car ys) (merge-vars xs (cdr ys)))]
        [else (cons (car xs) (merge-vars (cdr xs) (cdr ys)))]))

(define (remove-duplicates-vars xs)
  (cond [(null? xs) xs]
        [(null? (cdr xs)) xs]
        [(var=? (car xs) (cadr xs)) (remove-duplicates-vars (cdr xs))]
        [else (cons (car xs) (remove-duplicates-vars (cdr xs)))]))

(define (rev-append xs ys)
  (if (null? xs) ys
      (rev-append (cdr xs) (cons (car xs) ys))))

;; miejsce na uzupełnienie własnych funkcji pomocniczych

;; Procedura sprawdzajaca czy w posortowanej liscie występuje dana zmienna
(define (var-on-sorted-list? var l)
  (cond [(null? l) false]
        [(eq? var (car l)) true]
        [(symbol<? (car l) var) (var-on-sorted-list? var (cdr l))]
        [else false]))

;; Procedura zwraca pierwsza zmienna ktora wystepuje w obu listach lub null wpp
(define (first-var-on-both l1 l2)
  (cond [(null? l1) null]
        [(var-on-sorted-list? (car l1) l2) (car l1)]
        [else (first-var-on-both (cdr l1) l2)]))

;; w resolve zmienna reprezentuje jako pare (wartosc, nazwa)
;; gdzie nazwa to nazwa klauzuli w ktorej wystepuje ona pozytywnie
;; tworze konstruktory i selektor do uzyskania wartosci i nazwy

(define (var-pair val key)
  (cons val key))

(define (var-val p)
  (car p))

(define (var-key p)
  (cdr p))

;; sprawdzanie trywialnosci klauzuli
(define (clause-trivial? c)
  (define v (first-var-on-both (res-clause-pos c) (res-clause-neg c)))
  (not (null? v)))

#|;;test trywialnosci
(define c3 (res-clause '(p q r) '(q s) null))
(display "Czy trywialna? ")
(clause-trivial? c3)|#

(define (resolve c1 c2)
  (let*
      ((var1 (var-pair (first-var-on-both (res-clause-pos c1) (res-clause-neg c2)) 'c1))
       (var2 (var-pair (first-var-on-both (res-clause-pos c2) (res-clause-neg c1)) 'c2))
       (var-res (if (null? (var-val var1)) var2 var1))
       (pos-res (remove (var-val var-res) (merge-vars (res-clause-pos c1) (res-clause-pos c2))))
       (neg-res (remove (var-val var-res) (merge-vars (res-clause-neg c1) (res-clause-neg c2)))))
    (if (null? (var-val var-res))
        false
        (res-clause pos-res
                    neg-res
                    (proof-res (var-val var-res)
                               (if (eq? (var-key var-res) 'c1)
                                   (res-clause-proof c1)
                                   (res-clause-proof c2))
                               (if (eq? (var-key var-res) 'c1)
                                   (res-clause-proof c2)
                                   (res-clause-proof c1)))))))


(define (resolve-single-prove s-clause checked pending)
  ;; TODO: zaimplementuj!
  ;; Poniższa implementacja działa w ten sam sposób co dla większych klauzul — łatwo ją poprawić!
  (let* ((resolvents   (filter-map (lambda (c) (resolve c s-clause))
                                     checked))
         (sorted-rs    (sort resolvents < #:key res-clause-size)))
    (subsume-add-prove (cons s-clause checked) pending sorted-rs)))

;; wstawianie klauzuli w posortowaną względem rozmiaru listę klauzul
(define (insert nc ncs)
  (cond
   [(null? ncs)                     (list nc)]
   [(< (res-clause-size nc)
       (res-clause-size (car ncs))) (cons nc ncs)]
   [else                            (cons (car ncs) (insert nc (cdr ncs)))]))

;; sortowanie klauzul względem rozmiaru (funkcja biblioteczna sort)
(define (sort-clauses cs)
  (sort cs < #:key res-clause-size))

;; główna procedura szukająca dowodu sprzeczności
;; zakładamy że w checked i pending nigdy nie ma klauzuli sprzecznej
(define (resolve-prove checked pending)
  (cond
   ;; jeśli lista pending jest pusta, to checked jest zamknięta na rezolucję czyli spełnialna
   [(null? pending) (generate-valuation (sort-clauses checked))]
   ;; jeśli klauzula ma jeden literał, to możemy traktować łatwo i efektywnie ją przetworzyć
   [(= 1 (res-clause-size (car pending)))
    (resolve-single-prove (car pending) checked (cdr pending))]
   ;; w przeciwnym wypadku wykonujemy rezolucję z wszystkimi klauzulami już sprawdzonymi, a
   ;; następnie dodajemy otrzymane klauzule do zbioru i kontynuujemy obliczenia
   [else
    (let* ((next-clause  (car pending))
           (rest-pending (cdr pending))
           (resolvents   (filter-map (lambda (c) (resolve c next-clause))
                                     checked))
           (sorted-rs    (sort-clauses resolvents)))
      (subsume-add-prove (cons next-clause checked) rest-pending sorted-rs))]))

;; procedura upraszczająca stan obliczeń biorąc pod uwagę świeżo wygenerowane klauzule i
;; kontynuująca obliczenia. Do uzupełnienia.
(define (subsume-add-prove checked pending new)
  (cond
   [(null? new)                 (resolve-prove checked pending)]
   ;; jeśli klauzula do przetworzenia jest sprzeczna to jej wyprowadzenie jest dowodem sprzeczności
   ;; początkowej formuły
   [(clause-false? (car new))   (list 'unsat (res-clause-proof (car new)))]
   ;; jeśli klauzula jest trywialna to nie ma potrzeby jej przetwarzać
   [(clause-trivial? (car new)) (subsume-add-prove checked pending (cdr new))]
   [else
    ;; TODO: zaimplementuj!
    ;; Poniższa implementacja nie sprawdza czy nowa klauzula nie jest lepsza (bądź gorsza) od już
    ;; rozpatrzonych; popraw to!
    (subsume-add-prove checked (insert (car new) pending) (cdr new))
    ]))

;; Pomocnicza procedura sprawdzajaca czy zmienna jest w liscie par (zmienna, wart)
(define (var-in-list-of-pairs? x l)
  (if (equal? l null)
      false
      (if (equal? (caar l) x)
        true
        (var-in-list-of-pairs? x (cdr l)))))


(define (generate-valuation resolved)
  ;; Ta implementacja mówi tylko że formuła może być spełniona, ale nie mówi jak.
  ;; resolved to lista klauzul w postaci res-clause
  ;; trzeba obliczyc wartosciowanie spelniajace je wszystkie 
  ;; (list 'sat (list (list 'x #t) ...))  
  (define (add-new-var clauses curr-vals)
  ;;wchodzi do pierwszej klauzuli
  ;;bierze pierwsza zmienna i sprawdza czy nie ma juz jej na liscie wartosci
  ;;jesli tak to bierze kolejna, jesli nie to bierze te zmienna
  ;;jak jest w pozytywnych to daje jej #t, wpp #f i appenduje do old-list (list var value)
    (define (var-without-val l)
    ;; procedura zwraca zmienna z listy l ktorej nie ma na liscie wartosciowan
      (cond [(null? l) #f]
            [(not (var-in-list-of-pairs? (car l) curr-vals)) (car l)]
            [else (var-without-val (cdr l))]))

  ;;iterujemy sie po wszystkich klauzulach az nie spotkamy zmiennej bez wartosciowania
    (define (iter cls)
      (if (null? cls)
          #f
          (let*
              ((temp-var-pos (var-without-val (res-clause-pos (car cls))))
               (temp-var-neg (var-without-val (res-clause-neg (car cls)))))
            (if (eq? #f temp-var-pos)
                (if (eq? #f temp-var-neg)
                    (iter (cdr cls))
                    (cons (list temp-var-neg #f) curr-vals))
                (cons (list temp-var-pos #t) curr-vals)))))
    (iter clauses))

  (define (simplify-clause c var)
    ;; bierze klauzule i pare (zmienna, wartosc)
    ;; jesli np zmienna wystepuje w pozytywnych i jest #t to zwracasz null bo ją wywalimy w ogole
    ;; jesli zmienna nie wystepuje w ogole to zwracam ta sama klauzule
    ;; jesli np x = #t a x jest w negatywnych to usuwam x z negatywnych i odwrtownie z pos
    (cond [(and (not (var-on-sorted-list? (car var) (res-clause-pos c))) (not (var-on-sorted-list? (car var) (res-clause-neg c)))) c]
          [(or (and (var-on-sorted-list? (car var) (res-clause-pos c)) (eq? (cdr var) #t))
               (and (var-on-sorted-list? (car var) (res-clause-neg c)) (eq? (cdr var) #f))) null]
          
          [(and (var-on-sorted-list? (car var) (res-clause-pos c)) (eq? (cdr var) #f)) (res-clause (remove (car var) (res-clause-pos c))
                                                                                  (res-clause-neg c)
                                                                                  (res-clause-proof c))]
          [(and (var-on-sorted-list? (car var) (res-clause-neg c)) (eq? (cdr var) #t)) (res-clause (res-clause-pos c)
                                                                                  (remove (car var) (res-clause-neg c))
                                                                                  (res-clause-proof c))]))
  (define (simplify-clauses-list l var)
    (define (iter l acc)
      (cond [(null? l) acc]
            [(null? (simplify-clause (car l) var)) (iter (cdr l) acc)]
            [else (iter (cdr l) (append acc (simplify-clause (car l) var)))]))
    (iter l null))

  (define (main-iter clauses vals)
    (if (null? clauses)
        (list 'sat vals)
        (let*
            ((c-vals (add-new-var clauses vals)) ;;aktualne wartosciowania
             (new-val (car c-vals)) ;;nowe wartosciowanie
             (new-clauses (simplify-clauses-list clauses new-val)))
          (main-iter new-clauses c-vals))))

  (main-iter resolved null)) 
  


(define clss (list (res-clause '(p r) '(q s) null) (res-clause '(p s) '(r t) null)))
;;(add-new-var clss '((p #t)))



(define lval (list (list 'p #t) (list 'b #f) (list 'a #t)))
;(var-without-val '(p q s) lval)

(define (simplify-clause c var)
    ;; bierze klauzule i pare (zmienna, wartosc)
    ;; jesli np zmienna wystepuje w pozytywnych i jest #t to zwracasz null bo ją wywalimy w ogole
    ;; jesli zmienna nie wystepuje w ogole to zwracam ta sama klauzule
    ;; jesli np x = #t a x jest w negatywnych to usuwam x z negatywnych i odwrtownie z pos
    (cond [(and (not (var-on-sorted-list? (car var) (res-clause-pos c))) (not (var-on-sorted-list? (car var) (res-clause-neg c)))) c]
          [(or (and (var-on-sorted-list? (car var) (res-clause-pos c)) (eq? (cdr var) #t))
               (and (var-on-sorted-list? (car var) (res-clause-neg c)) (eq? (cdr var) #f))) null]
          
          [(and (var-on-sorted-list? (car var) (res-clause-pos c)) (eq? (cdr var) #f)) (res-clause (remove (car var) (res-clause-pos c))
                                                                                  (res-clause-neg c)
                                                                                  (res-clause-proof c))]
          [(and (var-on-sorted-list? (car var) (res-clause-neg c)) (eq? (cdr var) #t)) (res-clause (res-clause-pos c)
                                                                                  (remove (car var) (res-clause-neg c))
                                                                                  (res-clause-proof c))]))
  (define (simplify-clauses-list l var)
    (define (iter l acc)
      (cond [(null? l) acc]
            [(null? (simplify-clause (car l) var)) (iter (cdr l) acc)]
            [else (iter (cdr l) (append acc (simplify-clause (car l) var)))]))
    (iter l null))
     

  



(define cl1 (res-clause '(p q r) '(s t) '(axiom p)))
(define cl2 (res-clause '(q r) '(p t) '(axiom s)))
(define cl3 (res-clause '(s t) '(q r) '(axiom r)))
(simplify-clauses-list (list cl1 cl2 cl3) (cons 'p #t))


;; procedura przetwarzające wejściowy CNF na wewnętrzną reprezentację klauzul
(define (form->clauses f)
  (define (conv-clause c)
    (define (aux ls pos neg)
      (cond
       [(null? ls)
        (res-clause (remove-duplicates-vars (sort pos var<?))
                    (remove-duplicates-vars (sort neg var<?))
                    (proof-axiom c))]
       [(literal-pol (car ls))
        (aux (cdr ls)
             (cons (literal-var (car ls)) pos)
             neg)]
       [else
        (aux (cdr ls)
             pos
             (cons (literal-var (car ls)) neg))]))
    (aux (clause-lits c) null null))
  (map conv-clause (cnf-clauses f)))

(define (prove form)
  (let* ((clauses (form->clauses form)))
    (subsume-add-prove '() '() clauses)))

;; procedura testująca: próbuje dowieść sprzeczność formuły i sprawdza czy wygenerowany
;; dowód/waluacja są poprawne. Uwaga: żeby działała dla formuł spełnialnych trzeba umieć wygenerować
;; poprawną waluację.
(define (prove-and-check form)
  (let* ((res (prove form))
         (sat (car res))
         (pf-val (cadr res)))
    (if (eq? sat 'sat)
        (valuate-partial pf-val form)
        (check-proof pf-val form))))

;;; TODO: poniżej wpisz swoje testy

;;TEST RESOLVE
#|(define cla1 (clause (literal #t 'p) (literal #t 'q) (literal #f 'r)))
(define cla2 (clause (literal #t 'p) (literal #t 'r) (literal #f 's)))
(define f (form->clauses (cnf cla1 cla2)))
(define rc (resolve (car f) (cadr f)))
rc
(display "Dowód poprawny? ")
(res-clause? rc)
;; poprawny wynik: p v q v ~s|#



(define cnf1 (cnf (clause (literal #t 'p) (literal #t 'q))
                  (clause (literal #f 'p) (literal #t 'r))
                  (clause (literal #f 'q))
                  (clause (literal #f 'r))))

(define cnf1-clause (form->clauses cnf1))

(define cnf2 (cnf (clause (literal #t 'p) (literal #t 'q) (literal #t 'r))
                  (clause (literal #f 'r) (literal #f 'g) (literal #f 'p))
                  (clause (literal #f 'q) (literal #t 'r))
                  (clause (literal #f 'r) (literal #t 'p))))

(define cnf2-clause (form->clauses cnf2)) ;;klauzula prawdziwa sie zapetla

(define cnf4 (cnf (clause (literal #t 'p) (literal #t 'q))
                  (clause (literal #t 'r) (literal #f 'p))
                  (clause (literal #f 'q) (literal #t 'r))
                  (clause (literal #f 'r) (literal #t 's))))

(define cnf4-clause (form->clauses cnf4)) ;;klauzula prawdziwa

(define cnf3 (cnf (clause (literal #f 'p) (literal #t 'q))
                  (clause (literal #f 'p) (literal #f 'r) (literal #t 's))
                  (clause (literal #f 'q) (literal #t 'r))
                  (clause (literal #t 'p))
                  (clause (literal #f 's))))


;;TEST DOWODU
;;(resolve-prove null cnf1-clause)
;;(resolve-prove null cnf3-clause)
;;(resolve-prove null cnf4-clause)
;;(prove-and-check cnf1)