#lang scheme

(define rember-f1
  (lambda (test? a l)
    (cond
      ((null? l) l)
      ((test? (car l) a) (cdr l))
      (else (cons (car l) (rember-f1 test? a (cdr l)))))))

(rember-f1 = 5 '(6 2 5 3))
(rember-f1 eq? 'jelly '(jelly beans are good))
(rember-f1 equal? '(pop corn) '(lemonade (pop corn) and (cake)))

(define eq?-c
  (lambda (a)
    (lambda (x)
      (eq? x a))))

(define eq?-salad
  (eq?-c 'salad))

(eq?-salad 'salad)
(eq?-salad 'tuna)

(define rember-f
  (lambda (test?)
    (lambda (a l)
      (cond
        ((null? l) l)
        ((test? (car l) a) (cdr l))
        (else (cons (car l) ((rember-f test?) a (cdr l))))))))

(define rember-eq? (rember-f eq?))

(rember-eq? 'tuna '(tuna salad is good))
((rember-f eq?) 'tuna '(shrimp salad and tuna salad))
((rember-f eq?) 'eq? '(equal? eq? eqan? eqlist? eqpair?))

(define insertL-f
  (lambda (test?)
    (lambda (new old l)
      (cond
        ((null? l) l)
        ((test? (car l) old) (cons new (cons old (cdr l))))
        (else (cons (car l) ((insertL-f test?) new old (cdr l))))))))

(define insertR-f
  (lambda (test?)
    (lambda (new old l)
      (cond
        ((null? l) l)
        ((test? (car l) old) (cons old (cons new (cdr l))))
        (else (cons (car l) ((insertR-f test?) new old (cdr l))))))))

(define insert-g-my
  (lambda (test? consord)
    (lambda (new old l)
      (cond
        ((null? l) l)
        ((test? (car l) old) (consord new (cons old (cdr l))))
        (else (cons (car l) ((insert-g-my test? consord) new old l)))))))

(define seqL
  (lambda (new old l)
    (cons new (cons old l))))
(define seqR
  (lambda (new old l)
    (cons old (cons new l))))

(define insert-g
  (lambda (seq)
    (lambda (new old l)
      (cond
        ((null? l) l)
        ((eq? (car l) old) (seq new old l))
        (else (cons (car l) ((insert-g seq) new old (cdr l))))))))

(define insertL1 (insert-g seqL))
(define insertR1 (insert-g seqR))

(define insertL
  (insert-g
   (lambda (new old l)
     (cons new (cons old l)))))

(define subst1
  (lambda (new old l)
    (cond
      ((null? l) '())
      ((eq? (car l) old) (cons new (cdr l)))
      (else (cons (car l) (subst new old (cdr l)))))))

(define seqS
  (lambda (new old l)
    (cons new l)))

(define subst (insert-g seqS))

(define pow
  (lambda (a b)
    (cond
      ((zero? b) 1)
      (else (* a (pow a (- b 1)))))))
(pow 5 2)
(pow 2 8)

(define atom?
  (lambda (x)
    (and (not (pair? x)) (not (null? x)))))

(define atom-to-function
  (lambda (a)
    (cond
      ((eq? a '+) +)
      ((eq? a 'x) *)
      (else pow))))

(define operator
  (lambda (nexp)
    (car nexp)))
(define 1st-sub-exp
  (lambda (nexp)
    (car (cdr nexp))))
(define 2nd-sub-exp
  (lambda (nexp)
    (car (cdr (cdr nexp)))))

(atom-to-function (operator '(+ 5 3)))

(define value
  (lambda (nexp)
    (cond
      ((atom? nexp) nexp)
      (else ((atom-to-function (operator nexp))
             (value (1st-sub-exp nexp))
             (value (2nd-sub-exp nexp)))))))

(define multirember
  (lambda (a lat)
    (cond
      ((null? lat) '())
      ((eq? (car lat) a) (multirember a (cdr lat)))
      (else (cons (car lat) (multirember a (cdr lat)))))))

(define multirember-f
  (lambda (f)
    (lambda (a lat)
      (cond
        ((null? lat) '())
        ((f (car lat) a) ((multirember-f f) a (cdr lat)))
        (else (cons (car lat) ((multirember-f f) a (cdr lat))))))))

(define multirember-eq? (multirember-f eq?))

