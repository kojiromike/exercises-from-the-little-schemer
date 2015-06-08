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

