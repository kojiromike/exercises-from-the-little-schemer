#lang scheme

(define member?
  (lambda (a lat)
    (cond
      ((null? lat) #f)
      ((eq? (car lat) a) #t)
      (else (member? a (cdr lat))))))

(define set?
  (lambda (l)
    (cond
      ((null? l) #t)
      ((member? (car l) (cdr l)) #f)
      (else (set? (cdr l))))))

(set? '(apple peaches apple plum))
(set? '(apple peaches pears plum))
(set? '())
(set? '(apple 3 pear 4 9 apple 3 4))

(define makeset-orig
  (lambda (lat)
    (cond
      ((null? lat) '())
      ((member? (car lat) (cdr lat))
       (makeset-orig (cdr lat)))
      (else (cons (car lat)
                  (makeset-orig (cdr lat)))))))

(define floop
  '(apple peach pear peach plum apple lemon peach))

(makeset-orig floop)

(define multirember
  (lambda (a lat)
    (cond
      ((null? lat) '())
      ((eq? (car lat) a)
       (multirember a (cdr lat)))
      (else (cons (car lat)
                  (multirember a (cdr lat)))))))

(define makeset-multirember
  (lambda (lat)
    (cond
      ((null? lat) '())
      (else (cons (car lat)
                  (makeset-multirember (multirember (car lat) (cdr lat))))))))

(makeset-multirember floop)

(define subset?
  (lambda (s1 s2)
    (cond
      ((null? s1) #t)
      (else (and (member? (car s1) s2)
                 (subset? (cdr s1) s2))))))

(subset? '(5 chicken wings) '(5 hamburgers 2 pices fried chicken and light duckling wings))
(subset? '(4 pounds of horseradish) '(four pounds chicken and 5 ounces horseradish))

(define eqset?
  (lambda (s1 s2)
    (and (subset? s1 s2) (subset? s2 s1))))

(eqset? '(6 large chickens with wings) '(6 chickens with large wings))

(define intersect?
  (lambda (s1 s2)
    (and (not (null? s1))
         (or (member? (car s1) s2)
             (intersect? (cdr s1) s2)))))

(intersect? '(stewed tomatoes and macaroni) '(macaroni and cheese))
(intersect? '(rhombus) '(macaroni rhododendron))
(intersect? '(fish) '())
(intersect? '() '(wild man))

(define intersect
  (lambda (s1 s2)
    (cond
      ((null? s1) '())
      ((member? (car s1) s2)
       (cons (car s1) (intersect (cdr s1) s2)))
      (else
       (intersect (cdr s1) s2)))))

(intersect '(stewed tomatoes and macaroni) '(macaroni and cheese))

(define union
  (lambda (s1 s2)
    (cond
      ((null? s1) s2)
      (else (makeset-multirember (cons (car s1)
                                       (union (cdr s1) s2)))))))

(union '(stewed tomatoes and macaroni casserole) '(macaroni and cheese))

(define intersectall
  (lambda (l-set)
    (cond
      ((null? (cdr l-set)) (car l-set))
      (else (intersect (car l-set) (intersectall (cdr l-set)))))))

(intersectall '((a b c) (c a d e) (e f g h a b)))
(intersectall '((6 pears and)
                (3 peaches and 6 peppers)
                (8 pears and 6 plums)
                (and 6 prunes with some apples)))

(define atom?
  (lambda (x)
    (and (not (pair? x)) (not (null? x)))))

(define a-pair?
  (lambda (s-exp)
    (and (not (or (atom? s-exp)
                  (null? s-exp)
                  (null? (cdr s-exp))))
         (null? (cdr (cdr s-exp))))))

(a-pair? '(pear pear))
(a-pair? '(3 7))
(a-pair? '((2) (pair)))
(a-pair? '(full (house)))
(a-pair? '())
(a-pair? '(a))
(a-pair? '(() ()))
(a-pair? '(() () ()))
(a-pair? 'abc)

(define first
  (lambda (p)
    (car p)))

(define second
  (lambda (p)
    (car (cdr p))))

(define build
  (lambda (s1 s2)
    (cons s1 (cons s2 '()))))

(define third
  (lambda (p)
    (car (cdr (cdr p)))))

(define firsts
  (lambda (l)
    (cond
      ((null? l) '(car (car l)))
      (else (cons (car (car l)) (firsts (cdr l)))))))

(define fun?
  (lambda (rel)
    (set? (firsts rel))))

(fun? '((8 3) (4 2) (7 6) (6 2) (3 4)))
(fun? '((d 4) (b 0) (b 9) (e 5) (g 4)))

(define revrel-orig
  (lambda (rel)
    (cond
      ((null? rel) '())
      (else (cons (build (second (car rel)) (first (car rel)))
                  (revrel-orig (cdr rel)))))))

(revrel-orig '((8 a) (pumpkin pie) (got sick)))

(define revpair
  (lambda (pair)
    (build (second pair) (first pair))))

(define revrel-revpair
  (lambda (rel)
    (cond
      ((null? rel) '())
      (else (cons (revpair (car rel))
                  (revrel-revpair (cdr rel)))))))

(define seconds
  (lambda (rel)
    (cond
      ((null? rel) '())
      (else (cons (second (car rel))
                  (seconds (cdr rel)))))))

(define fullfun?
  (lambda (fun)
    (set? (seconds fun))))

(fullfun? '((8 3) (4 2) (7 6) (6 2) (3 4)))
(fullfun? '((8 3) (4 8) (7 6) (6 2) (3 4)))
(fullfun? '((grape raisin) (plum prune) (stewed prune)))
(fullfun? '((grape raisin) (plum prune) (stewed grape)))