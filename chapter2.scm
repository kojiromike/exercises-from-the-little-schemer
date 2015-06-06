#lang scheme

(define anatom 'a)
(define alat '(a b c))
(define ansexp '(a (a b c)))

(define atom?
  (lambda (x)
    (and (not (pair? x)) (not (null? x)))))

(atom? anatom)
(atom? alat)
(atom? ansexp)

(define lat?
  (lambda (l)
    (cond
      ((null? l) #t)
      ((atom? (car l)) (lat? (cdr l)))
      (else #f))))

(lat? alat)
(lat? ansexp)

(define member?
  (lambda (a l)
    (cond
      ((null? l) #f)
      ((eq? (car l) a) #t)
      (else (member? a (cdr l))))))

(member? anatom alat)
(member? anatom ansexp)
(member? 'a '(b c d))