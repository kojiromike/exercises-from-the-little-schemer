#lang scheme

(define atom?
  (lambda (x)
    (and (not (pair? x))
         (not (null? x)))))

(define ×
  (lambda (a b)
    (cond
      ((zero? a) '0)
      (else (+ b (× (sub1 a) b))))))

(define pow
  (lambda (x y)
    (cond
      ((zero? y) 1)
      (else (× x (pow x (sub1 y)))))))

(define numbered?
  (lambda (s)
    (cond
      ((atom? s) (number? s))
      ((eq? (car (cdr s)) '+) (and (numbered? (car s))
                                   (numbered? (car (cdr (cdr s))))))
      ((eq? (car (cdr s)) 'x) (and (numbered? (car s))
                                   (numbered? (car (cdr (cdr s))))))
      ((eq? (car (cdr s)) 't) (and (numbered? (car s))
                                   (numbered? (car (cdr (cdr s))))))
      (else (and (numbered? (car s))
                 (numbered? (car (cdr (cdr s)))))))))

; infix operations
(define value-infix
  (lambda (nexp)
    (cond
      ((atom? nexp) nexp)
      ((eq? (car (cdr nexp)) '+) (+ (value (car nexp))
                                 (value (cdr nexp))))
      ((eq? (car (cdr nexp)) 'x) (* (value (car nexp))
                                 (value (cdr nexp))))
      (else (pow (value (car nexp))
                 (value (cdr nexp)))))))

; prefix operations
(define value
  (lambda (nexp)
    (cond
      ((atom? nexp) nexp)
      ((eq? (car nexp) '+) (+ (value (car (cdr nexp)))
                              (value (car (cdr (cdr nexp))))))
      ((eq? (car nexp) 'x) (* (value (car (cdr nexp)))
                              (value (car (cdr (cdr nexp))))))
      (else (pow (value (car (cdr nexp)))
                 (value (car (cdr (cdr nexp)))))))))

(define sero?
  (lambda (n)
    (null? n)))

(define edd1
  (lambda (n)
    (cons '() n)))

(define zub1
  (lambda (n)
    (cdr n)))

(define edd
  (lambda (n m)
    (cond
      ((sero? (n)) m)
      ((sero? (m)) n)
      (else (edd1 (edd (zub1 n) m))))))


