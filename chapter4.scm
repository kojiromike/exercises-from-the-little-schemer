#lang scheme

(define add1
  (lambda (n)
    (+ 1 n)))

(add1 67)

(define sub1
  (lambda (n)
    (- n 1)))

(sub1 5)
(sub1 0)

(zero? 0)
(zero? 1492)

(define o+
  (lambda (n m)
    (cond
      ((zero? n) m)
      (else (o+ (sub1 n) (add1 m))))))

(o+ 56 100)

(define o-
  (lambda (n m)
    (cond
      ((zero? n) m)
      (else (o- (sub1 n) (sub1 m))))))

(o- 5 15)

(define addtup
  (lambda (tup)
    (cond
      ((null? tup) 0)
      (else (+ (car tup) (addtup (cdr tup)))))))

(addtup '(1 2 3 4 5 6 7 8 9 10))

(define ×
  (lambda (a b)
    (cond
      ((zero? a) '0)
      (else (+ b (× (sub1 a) b))))))

(× 10 10)
(× 0 10)
(× 10 0)

(define tup+
  (lambda (tup1 tup2)
    (cond
      ((null? tup1) tup2)
      ((null? tup2) tup1)
      (else (cons (+ (car tup1) (car tup2))
                  (tup+ (cdr tup1) (cdr tup2)))))))

(tup+ '(3 6 9 11 4) '(8 5 2 0 7))
(tup+ '(2 3) '(4 6))
(tup+ '(3 7) '(4 6 8 1))
(tup+ '(3 7 8 1) '(4 6))

(define gt?
  (lambda (x y)
    (cond
      ((zero? x) #f)
      ((zero? y) #t)
      (else (gt? (sub1 x) (sub1 y))))))

(gt? 12 133)
(gt? 120 11)
(gt? 3 3)

(define eq?
  (lambda (x y)
    (cond
      ((zero? x) (zero? y))
      ((zero? y) #f)
      (else (eq? (sub1 x) (sub1 y))))))

(define lt?
  (lambda (x y)
    (not (or (eq? x y) (gt? x y)))))

(lt? 4 6)
(lt? 8 3)
(lt? 6 6)

(define pow
  (lambda (x y)
    (cond
      ((zero? y) 1)
      (else (× x (pow x (sub1 y)))))))

(pow 1 1)
(pow 2 3)
(pow 5 3)

(define div
  (lambda (n m)
    (cond
      ((< n m) 0)
      (else (add1 (div (- n m) m))))))

(define length
  (lambda (l)
    (cond
      ((null? l) '0)
      (else (+ 1 (length (cdr l)))))))

(length '(hotdogs with mustard sauerkraut and pickles))

(define pick
  (lambda (n lat)
    (cond
      ((zero? (sub1 n)) (car lat))
      (else (pick (sub1 n) (cdr lat))))))

(pick 4 '(lasagna spaghetti ravioli macaroni meatball))

(define rempick
  (lambda (n lat)
    (cond
      ((zero? (sub1 n)) (cdr lat))
      (else (cons (car lat) (rempick (sub1 n) (cdr lat)))))))

(rempick 3 '(hotdogs with hot mustard))

(define no-nums
  (lambda (lat)
    (cond
      ((null? lat) '())
      ((number? (car lat)) (no-nums (cdr lat)))
      (else (cons (car lat) (no-nums (cdr lat)))))))

(no-nums '(5 pears 6 prunes 9 dates))

(define all-nums
  (lambda (lat)
    (cond
      ((null? lat) '())
      ((number? (car lat)) (cons (car lat) (all-nums (cdr lat))))
      (else (all-nums (cdr lat))))))

(all-nums '(5 pears 6 prunes 9 dates))

(define eqan?
  (lambda (x y)
    (cond
      ((and (number? x) (number? y)) (= x y))
      ((or (number? x) (number? y)) #f)
      (else (eq? x y)))))

(define occur
  (lambda (a lat)
    (cond
      ((null? lat) 0)
      ((eqan? (car lat) a) (add1 (occur a (cdr lat))))
      (else (occur a (cdr lat))))))

(define one?
  (lambda (n)
    (= n 1)))

(define rempick
  (lambda (n lat)
    (cond
      ((one? n) (car lat))
      (else (cons (car lat) (rempick (sub1 n) (cdr lat)))))))

