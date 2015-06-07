#lang scheme

(define atom?
  (lambda (a)
    (and (not (pair? a)) (not (null? a)))))

(define rember*
  (lambda (a l)
    (cond
      ((null? l) '())
      ((atom? (car l)) (cond
                         ((eq? (car l) a) (rember* a (cdr l)))
                         (else (cons (car l) (rember* a (cdr l))))))
      (else (cons (rember* a (car l))
                  (rember* a (cdr l)))))))

(define insertR*
  (lambda (new old l)
    (cond
      ((null? l) '())
      ((atom? (car l)) (cond
                         ((eq? (car l) old) (cons old (cons new (insertR* new old (cdr l)))))
                         (else (cons (car l) (insertR* new old (cdr l))))))
      (else (cons (insertR* new old (car l))
                  (insertR* new old (cdr l)))))))

(define occur*
  (lambda (a l)
    (cond
      ((null? l) 0)
      ((atom? (car l))
       (cond
         ((eq? (car l) a) (+ 1 (occur* a (cdr l))))
         (else (occur* a (cdr l)))))
      (else (+ (occur* a (car l))
               (occur* a (cdr l)))))))

(define subst*
  (lambda (new old l)
    (cond
      ((null? l) '())
      ((atom? (car l))
       (cond
         ((eq? (car l) old) (cons new (subst* new old (cdr l))))
         (else (cons (car l) (subst* new old (cdr l))))))
      (else (cons (subst* new old (car l))
                  (subst* new old (cdr l)))))))

(define insertL*
  (lambda (new old l)
    (cond
      ((null? l) '())
      ((atom? (car l))
       (cond
         ((eq? (car l) old) (cons new (cons old (insertL* new old (cdr l)))))
         (else (cons (car l) (insertL* new old (cdr l))))))
      (else (cons (insertL* new old (car l))
                  (insertL* new old (cdr l)))))))

(define member*
  (lambda (a l)
    (cond
      ((null? l) #f)
      ((atom? (car l))
       (or (eq? (car l) a)
           (member* a (cdr l))))
      (else (or (member* a (car l))
                (member* a (cdr l)))))))

(member* 'chips '((potato) (chips ((with) fish) (chips))))
(member* 'chips '((fish) (sticks ((with) fish) (broth))))

; frame 0: (null? l) | #f
;          (car l) | (potato)
;          (atom? (car l)) | #f
;          (member* a (car l)) | downframe
; frame 1: l | (potato)
;          (null? l) | #f
;          (car l) | potato
;          (atom? (car l)) | #t
;          (eq? (car l) a) | #f
;          (cdr l) | ()
;          (member* a (cdr l)) | downframe
; frame 2: l | ()
;          (null? l) | #t
;          (not (null? l)) | #f -> return
; frame 1: (member* a (cdr l)) | #f
;          (or (eq? (car l) a) (member* a (cdr l)))) | #f
;          (and (atom? (car l)) (or (eq? (car l) a) (member* a (cdr l)))) | #f
;          
